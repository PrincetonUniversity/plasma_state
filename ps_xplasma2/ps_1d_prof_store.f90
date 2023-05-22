subroutine ps_1d_prof_store(iobj, profname, gridname, &
     units, iauthor, label, isplini, bc_opt, &
     n, k_ccw, profdata, id, imod, iout, ierr)

  !------------------------------------------------------------
  ! D. McCune 8 Oct 2006

  ! XPLASMA2 implementation of plasma_state subroutine
  ! store a 1d profile, return its ID, perform error checking

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions

  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)  ! opaque container for xplasma object

  character*(*), intent(in) :: profname    ! NAME of profile to fetch
  character*(*), intent(in) :: gridname    ! NAME of profile's grid
  character*(*), intent(in) :: units       ! units label
  integer, intent(in) :: iauthor           ! author ID

  character*(*), intent(in) :: label       ! profile label
  integer, intent(in) :: isplini           ! profile interpolation method

  integer, intent(in) :: bc_opt            ! spline Bdy Cond option

  integer, intent(in) :: n                 ! profile data size
  integer, intent(in) :: k_ccw             ! CCW flag

  REAL(KIND=rspec) :: profdata(n)          ! profile data to be stored

  integer, intent(out) :: id               ! profile ID (interpolation handle)
  integer, intent(out) :: imod             ! =1 if profile is new or modified
  !  =0 on output if profile was not changed.

  integer, intent(in) :: iout              ! LUN for error messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: irank,idg,icoord,diff_code,iertmp,ispline,ing,inx
  logical :: ccwflag,ccwflag1
  logical :: rewrite
  integer :: ibca,ibcb
  REAL(KIND=rspec) :: zprof(n)
  character*32 :: zunits
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  imod=0

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_1d_prof_store: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_gridId(s,gridname,idg)
  if(idg.eq.0) then
     ierr=300
     write(iout,*) ' ?ps_1d_prof_store: grid name not recognized: "', &
          trim(gridname),'".'
     write(iout,*) '  error on call to store profile "',profname,'".'
     return
  endif

  call xplasma_grid_size(s,idg,ing,ierr)
  inx=ing
  if(isplini.lt.0) inx=inx-1

  if(n.ne.inx) then
     ierr=301
     write(iout,*) ' ?ps_1d_prof_store: profile size: ',n
     write(iout,*) '  does not match expected size from grid '// &
          trim(gridname)//': ',inx
     write(iout,*) '  for spline type: ',isplini
     return
  endif

  ispline=max(isplini,ps_stepfun)
  if(ispline.ne.isplini) then
     call norm1(isplini,profdata,zprof)
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

  call xplasma_grid_info(s,idg,iertmp, coord=icoord)

  ccwflag = (k_ccw.ne.ps_ccw_reverse)
  ccwflag1 = .TRUE.

  if(icoord.eq.xplasma_theta_coord) then
     ccwflag1=ccwflag
  endif

  ibca=0
  ibcb=0

  if(bc_opt.ne.ps_bc_dflt) then
     if(icoord.eq.xplasma_rho_coord) then
        if(bc_opt.eq.ps_bc_grad00) then
           ibca=1  ! 1st derivative to -> 0 as rho -> 0
           ibcb=0  ! still the default at bdy
        endif
     endif
  endif

  !--------------------------------------------------
  !  find if profile ID already defined

  call xplasma_profId(s,profname,id)
  if(id.gt.0) then
     !  profile exists -- see if it is the same...
     call xplasma_prof_info(s,id,ierr, rank=irank)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_1d_prof_store: unexpected XPLASMA2 error:'
        call xplasma_error(s,ierr,iout)
        ierr=301
        rewrite=.FALSE.
     else
        if(irank.ne.1) then
           rewrite=.TRUE.
        else
           call xplasma_diff_prof(s,id,zprof,diff_code,ierr, &
                splineType=ispline, units=zunits, ccwflag1=ccwflag1)
           if(diff_code.eq.0) then
              rewrite=.FALSE.
           else
              rewrite=.TRUE.
           endif
        endif
     endif
  else
     rewrite=.TRUE.   ! actually, write for first time
  endif

  if(.not.rewrite) return

  !--------------------------------------------------
  !  store the modified profile...

  !  set author...

  imod = 1

  call xplasma_author_set(s,ps_cnames(iauthor),ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_1d_prof_store: unexpected xplasma_author_set error:'
     call xplasma_error(s,ierr,iout)
     ierr=301
  endif
  if(ierr.ne.0) return

  !  write profile...

  call xplasma_create_1dprof(s,profname,idg,zprof,id,ierr, &
       ispline=ispline,ibca=ibca,ibcb=ibcb, &
       ccwflag=ccwflag1,label=label,units=zunits)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_1d_prof_store: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=302
  endif

  !  clear author...

  call xplasma_author_clear(s,ps_cnames(iauthor),ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_1d_prof_store: unexpected xplasma_author_clear error:'
     call xplasma_error(s,ierr,iout)
     ierr=301
  endif

  contains
    subroutine norm1(itype,zfdv,zf)

      integer, intent(in) :: itype
      REAL(KIND=rspec), intent(in) :: zfdv(n)   ! input f*dV or f*dA
      REAL(KIND=rspec), intent(out) :: zf(n)    ! output f

      ! if {R,Z} surfaces are available:
      !   divide out area or volume element, depending on "itype"
      ! if not:  look for VOL(rho) and AREA(rho) functions and use
      !   these go get area and volume elements.
      ! if this too fails: 
      !   write a warning and create a black box marker s.t. the warning
      !   appears only once...

      REAL(KIND=rspec) :: zdv(n)

      integer :: id_vol,id_area,id_R,id_Z,id_blkbx,iii,iexec
      integer :: iarray(1),iertmp
      REAL(KIND=rspec) :: r8array(1)
      character*32, parameter :: bbname = '_1d_prof_store_warn_marker'
      real*8, dimension(:), allocatable :: zx,zva
      real*8 :: zmaxa
      real*8, parameter :: ZERO = 0.0d0
      !-----------------------------

      ierr=0
      iexec=0

      zmaxa=ZERO
      do iii=1,n
         zmaxa=max(zmaxa,abs(zfdv(iii)))
      enddo
      if(zmaxa.eq.ZERO) then
         zf=ZERO
         return
      endif

      call xplasma_common_ids(s,ierr,id_R=id_R,id_Z=id_Z)
      if(ierr.ne.0) then
         write(iout,*) ' ?ps_1d_prof_store: xplasma_common_ids error:'
         call xplasma_error(s,ierr,iout)
         zf=0.0d0
         return
      endif

      do

         if((id_R.eq.0).or.(id_Z.eq.0)) then
            !  state has not retained full flux surface geometry; look for
            !  Vol(rho) and Area(rho) instead...

            allocate(zx(ing),zva(ing))
            call xplasma_grid(s,idg,zx,iertmp)
            if(iertmp.ne.0) then
               write(iout,*) ' ?ps_1d_prof_store: grid extraction error.'
               id_vol=0
               id_area=0
            else
               call xplasma_profId(s,'VOL',id_vol)
               call xplasma_profId(s,'AREA',id_area)
            endif

            if((id_vol.eq.0).or.(id_area.eq.0)) then
               call xplasma_blkbxID(s,bbname,id_blkbx)
               if(id_blkbx.eq.0) then
                  write(iout,*) &
                       ' %ps_1d_prof_store: ZEROed step*dV & step*dA interpolants:'
                  write(iout,*) &
                       '  neither {R,Z} surfaces nor Vol(rho) & Area(rho) were found.'

                  ! set warning marker, owned by xplasma_xmhd s.t. erased when
                  ! an equilibrium is established.
                  call xplasma_author_set(s,xplasma_xmhd,iertmp)
                  iarray=0
                  r8array=0.0_rspec

                  call xplasma_create_blackBox(s,bbname,-12345,id_blkbx,ierr, &
                       iarray=iarray,r8array=r8array)
                  if(ierr.ne.0) then
                     write(iout,*) ' ?ps_1d_prof_store: blackbox warning marker error:'
                     call xplasma_error(s,ierr,iout)
                     exit
                  endif

                  call xplasma_author_clear(s,xplasma_xmhd,iertmp)
               else
                  ierr=0  ! warning already issued: OK
               endif

               zf=0.0d0
               
            else if(itype.eq.ps_stepdv) then
               ! OK step*dV & there appears to be a volume profile available
               call xplasma_eval_prof(s,id_vol,zx,zva,ierr)
               if(ierr.ne.0) then
                  write(iout,*) ' ?ps_1d_prof_store: VOL interpolation error:'
                  call xplasma_error(s,ierr,iout)
                  zf=0.0d0
               endif
               iexec=2

            else if(itype.eq.ps_stepda) then
               ! OK step*dA & there appears to be an area profile available
               call xplasma_eval_prof(s,id_area,zx,zva,ierr)
               if(ierr.ne.0) then
                  write(iout,*) ' ?ps_1d_prof_store: AREA interpolation error:'
                  call xplasma_error(s,ierr,iout)
                  zf=0.0d0
               endif
               iexec=2

            else
               ierr=302
               zf=0.0d0
               write(iout,*) &
                    ' ?ps_1d_prof_store internal error: bad norm1 call.'
            endif

            exit

         else if(itype.eq.ps_stepdv) then
            ! OK step*dV & there appears to be a geometry available
            call xplasma_rhogrid_dva(s,idg,ierr, dvol=zdv)
            if(ierr.ne.0) then
               write(iout,*) ' ?ps_1d_prof_store: xplasma_rhogrid_dva error:'
               call xplasma_error(s,ierr,iout)
               zf=0.0d0
            else
               iexec=1
            endif

         else if(itype.eq.ps_stepda) then
            ! OK step*dA & there appears to be a geometry available
            call xplasma_rhogrid_dva(s,idg,ierr, darea=zdv)
            if(ierr.ne.0) then
               write(iout,*) ' ?ps_1d_prof_store: xplasma_rhogrid_dva error:'
               call xplasma_error(s,ierr,iout)
               zf=0.0d0
            else
               iexec=1
            endif

         else
            ierr=302
            zf=0.0d0
            write(iout,*) ' ?ps_1d_prof_store internal error: bad norm1 call.'
         endif

         exit
      enddo

      if(iexec.eq.1) then
         zf = zfdv/zdv

      else if(iexec.eq.2) then
         do iii=1,inx
            zdv(iii)=zva(iii+1)-zva(iii)
            zf(iii) = zfdv(iii)/zdv(iii)
         enddo
      endif

      if(allocated(zx)) deallocate(zx,zva)

    end subroutine norm1

end subroutine ps_1d_prof_store
