subroutine ps_2dx_prof_store(iobj, profname, gridname1, gridname2, &
     units, iauthor, label, &
     n1, n2, k_ccw1, k_ccw2, &
     profdata, dprofdx1, dprofdx2, d2profdx1dx2, &
     id, imod, iout, ierr)

  !------------------------------------------------------------
  ! D. McCune 8 Oct 2006

  ! XPLASMA2 implementation of plasma_state subroutine
  ! store a 2d profile, return its ID, perform error checking

  ! Here, store a bi-Hermite profile with derivatives provided as input.

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

  integer, intent(in) :: n1,n2             ! profile data dimensions
  integer, intent(in) :: k_ccw1,k_ccw2     ! CCW flags

  REAL(KIND=rspec) :: profdata(n1,n2)      ! profile data to be stored
  REAL(KIND=rspec) :: dprofdx1(n1,n2)      ! profile derivative to be stored
  REAL(KIND=rspec) :: dprofdx2(n1,n2)      ! profile derivative to be stored
  REAL(KIND=rspec) :: d2profdx1dx2(n1,n2)  ! profile derivative to be stored

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
  character*32 zunits
  REAL(KIND=rspec), dimension(:,:,:), allocatable :: coeffs
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  zunits = ' '
  imod = 0

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_2dx_prof_store: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_gridId(s,gridname1,idg1)
  if(idg1.eq.0) then
     ierr=300
     write(iout,*) ' ?ps_2dx_prof_store: grid name 1 not recognized: "', &
          trim(gridname1),'".'
     write(iout,*) '  error on call to store profile "',profname,'".'
  endif

  call xplasma_gridId(s,gridname2,idg2)
  if(idg2.eq.0) then
     ierr=300
     write(iout,*) ' ?ps_2dx_prof_store: grid name 2 not recognized: "', &
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

  ccwflag = (k_ccw2.ne.ps_ccw_reverse)

  call xplasma_grid_info(s,idg2,iertmp, coord=icoord)

  if(icoord.eq.xplasma_theta_coord) then
     ccwflag2=ccwflag
  endif

  ispline=1

  !--------------------------------------------------
  !  with derivatives provided: must assume profile data
  !  has changed...
  !--------------------------------------------------
  !  store the modified profile...

  !  set author...

  imod = 1

  call xplasma_author_set(s,ps_cnames(iauthor),ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_2dx_prof_store: unexpected xplasma_author_set error:'
     call xplasma_error(s,ierr,iout)
     ierr=301
     return
  endif

  !  write profile...

  allocate(coeffs(3,n1,n2))
  coeffs(1,:,:) = dprofdx1
  coeffs(2,:,:) = dprofdx2
  coeffs(3,:,:) = d2profdx1dx2

  call xplasma_create_2dprof(s,profname,idg1,idg2,profdata,id,ierr, &
       ispline=ispline, coeffs=coeffs, &
       ccwflag1=ccwflag1,ccwflag2=ccwflag2, &
       label=label,units=zunits)

  deallocate(coeffs)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_2dx_prof_store: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=302
  endif

  !  clear author...

  call xplasma_author_clear(s,ps_cnames(iauthor),ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_2dx_prof_store: unexpected xplasma_author_clear error:'
     call xplasma_error(s,ierr,iout)
     ierr=301
  endif

end subroutine ps_2dx_prof_store
