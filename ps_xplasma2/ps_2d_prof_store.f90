subroutine ps_2d_prof_store(iobj, profname, gridname1, gridname2, &
     units, iauthor, label, isplini, bc_opt, &
     n1, n2, k_ccw1, k_ccw2, profdata, id, imod, iout, ierr)

  !------------------------------------------------------------
  ! D. McCune 8 Oct 2006

  ! XPLASMA2 implementation of plasma_state subroutine
  ! store a 2d profile, return its ID, perform error checking

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)  ! opaque container for xplasma object

  character*(*), intent(in) :: profname    ! NAME of profile to store
  character*(*), intent(in) :: gridname1,gridname2
                                           ! NAMEs of profile's grids
  character*(*), intent(in) :: units       ! units label
  integer, intent(in) :: iauthor           ! author ID

  character*(*), intent(in) :: label       ! profile label
  integer, intent(in) :: isplini           ! profile interpolation method

  integer, intent(in) :: bc_opt            ! spline Bdy Cond option

  integer, intent(in) :: n1,n2             ! profile data dimensions
  integer, intent(in) :: k_ccw1,k_ccw2     ! CCW flags

  REAL(KIND=rspec) :: profdata(n1,n2)      ! profile data to be stored

  integer, intent(out) :: id               ! profile ID (interpolation handle)

  integer, intent(out) :: imod             ! =1 if profile is new or modified
  !  =0 on output if profile was not changed.

  integer, intent(in) :: iout              ! LUN for error messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: irank,idg1,idg2,icoord,iertmp,diff_code
  integer :: ispline
  logical :: ccwflag,ccwflag1,ccwflag2
  logical :: rewrite
  integer :: ibc1a,ibc1b,ibc2a,ibc2b
  REAL(KIND=rspec), dimension(:,:), allocatable :: zprof
  character*32 zunits
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  imod = 0

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_2d_prof_store: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_gridId(s,gridname1,idg1)
  if(idg1.eq.0) then
     ierr=300
     write(iout,*) ' ?ps_2d_prof_store: grid name 1 not recognized: "', &
          trim(gridname1),'".'
     write(iout,*) '  error on call to store profile "',profname,'".'
  endif

  call xplasma_gridId(s,gridname2,idg2)
  if(idg2.eq.0) then
     ierr=300
     write(iout,*) ' ?ps_2d_prof_store: grid name 2 not recognized: "', &
          trim(gridname2),'".'
     write(iout,*) '  error on call to store profile "',profname,'".'
  endif
  if(ierr.ne.0) return

  ccwflag1 = .TRUE.
  ccwflag2 = .TRUE.

  ccwflag = (k_ccw1.ne.ps_ccw_reverse)

  call xplasma_grid_info(s,idg1,iertmp, coord=icoord)

  if(icoord.eq.xplasma_theta_coord) then
     ccwflag1=ccwflag
  endif

  ibc1a=0
  ibc1b=0

  if(bc_opt.ne.ps_bc_dflt) then
     if(icoord.eq.xplasma_rho_coord) then
        if(bc_opt.eq.ps_bc_grad00) then
           ibc1a=1  ! 1st derivative to -> 0 as rho -> 0
           ibc1b=0  ! still the default at bdy
        endif
     endif
  endif

  ccwflag = (k_ccw2.ne.ps_ccw_reverse)

  call xplasma_grid_info(s,idg2,iertmp, coord=icoord)

  if(icoord.eq.xplasma_theta_coord) then
     ccwflag2=ccwflag
  endif

  ibc2a=0
  ibc2b=0

  if(bc_opt.ne.ps_bc_dflt) then
     if(icoord.eq.xplasma_rho_coord) then
        if(bc_opt.eq.ps_bc_grad00) then
           ibc2a=1  ! 1st derivative to -> 0 as rho -> 0
           ibc2b=0  ! still the default at bdy
        endif
     endif
  endif

  allocate(zprof(n1,n2))

  ispline=max(ps_stepfun,isplini)
  if(ispline.ne.isplini) then
     call norm2(isplini,profdata,zprof)
     if(ierr.ne.0) return
     if(isplini.eq.ps_stepdv) then
        zunits=trim(units)//'/m^3'
     else if(isplini.eq.ps_stepda) then
        zunits=trim(units)//'/m^2'
     endif
  else
     zprof=profdata
     zunits=units
  endif

  !--------------------------------------------------
  !  find if profile ID already defined

  call xplasma_profId(s,profname,id)
  if(id.gt.0) then
     !  profile exists -- see if it is the same...
     call xplasma_prof_info(s,id,ierr, rank=irank)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_2d_prof_store: unexpected XPLASMA2 error:'
        call xplasma_error(s,ierr,iout)
        ierr=301
        rewrite=.FALSE.
     else
        if(irank.ne.2) then
           rewrite=.TRUE.
        else
           call xplasma_diff_prof(s,id,zprof,diff_code,ierr, &
                splineType=ispline, units=zunits, &
                ccwflag1=ccwflag1, ccwflag2=ccwflag2)
           if(ierr.ne.0) then
              write(iout,*) ' ?ps_2d_prof_store: unexpected XPLASMA2 error:'
              call xplasma_error(s,ierr,iout)
              ierr=301
              rewrite=.FALSE.
           else
              if(diff_code.eq.0) then
                 rewrite=.FALSE.
              else
                 rewrite=.TRUE.
              endif
           endif
        endif
     endif
  else
     rewrite=.TRUE.  ! actually, write for first time...
  endif

  if(.not.rewrite) then
     deallocate(zprof)
     return
  endif

  !--------------------------------------------------
  !  store the modified profile...

  !  set author...

  imod = 1

  call xplasma_author_set(s,ps_cnames(iauthor),ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_2d_prof_store: unexpected xplasma_author_set error:'
     call xplasma_error(s,ierr,iout)
     ierr=301
     deallocate(zprof)
     return
  endif

  !  write profile...

  call xplasma_create_2dprof(s,profname,idg1,idg2,zprof,id,ierr, &
       ispline=ispline,ibcx1a=ibc1a,ibcx1b=ibc1b,ibcx2a=ibc2a,ibcx2b=ibc2b, &
       ccwflag1=ccwflag1,ccwflag2=ccwflag2, &
       label=label,units=zunits)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_2d_prof_store: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=302
  endif

  !  clear author...

  call xplasma_author_clear(s,ps_cnames(iauthor),ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_2d_prof_store: unexpected xplasma_author_clear error:'
     call xplasma_error(s,ierr,iout)
     ierr=301
  endif

  deallocate(zprof)

  contains
    subroutine norm2(itype,zfdv,zf)

      integer, intent(in) :: itype
      REAL(KIND=rspec), intent(in) :: zfdv(n1,n2)   ! input f*dV or f*dA
      REAL(KIND=rspec), intent(out) :: zf(n1,n2)    ! output f

      ! divide out area or volume element, depending on "itype"

      REAL(KIND=rspec), dimension(:,:), allocatable :: zdv

      allocate(zdv(n1,n2))

      ierr=0

      if(itype.eq.ps_stepdv) then
         call xplasma_rhoth_grid_dva(s,idg1,idg2,ierr, dvol=zdv, &
              ccwflag=ccwflag)
         if(ierr.ne.0) then
            write(iout,*) ' ?ps_2d_prof_store: xplasma_rhoth_grid_dva error:'
            call xplasma_error(s,ierr,iout)
            zf=0.0d0
         endif

      else if(itype.eq.ps_stepda) then
         call xplasma_rhoth_grid_dva(s,idg1,idg2,ierr, darea=zdv, &
              ccwflag=ccwflag)
         if(ierr.ne.0) then
            write(iout,*) ' ?ps_2d_prof_store: xplasma_rhoth_grid_dva error:'
            call xplasma_error(s,ierr,iout)
            zf=0.0d0
         endif

      else
         ierr=302
         zf=0.0d0
         write(iout,*) ' ?ps_2d_prof_store internal error: bad norm2 call.'
      endif

      if(ierr.eq.0) then
         zf = zfdv/zdv
      endif

      deallocate(zdv)

    end subroutine norm2

end subroutine ps_2d_prof_store
