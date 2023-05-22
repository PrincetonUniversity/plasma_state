subroutine ps_protmp(iobj,idrho,npro,pro,itmp,intrp,id,iout,ierr)

  !------------------------------------------------------------
  ! create a temporary f(rho) profile.
  ! use ps_freetmp(id,iout) to remove it when done
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer :: iobj(ps_container_size)     ! xplasma opaque container

  integer, intent(in) :: idrho    ! ID of rho grid over which data is defined

  integer, intent(in) :: npro     ! profile data size (must be consistent with
  !                                 idrho and intrp choices).

  REAL(KIND=rspec), intent(in) :: pro(npro)   ! the profile values

  integer, intent(in) :: itmp     ! number to use for temporary name generation
  !  if multiple temporary profiles are created, differing values of itmp are
  !  needed to separate them (i.e. give them different names in xplasma).
  !    abs(itmp) is used.

  integer, intent(in) :: intrp    ! interpolation method for profile
  !                       -1: zonal step function
  !                        0: piecewise linear
  !                        1: C1 cubic Hermite
  !                        2: C2 cubic Spline

  integer, intent(out) :: id      ! ID returned (0 if error occurs)

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  !-----------------------
  !  local:
  integer :: iertmp,jtmp
  character*32 :: tmpname
  character*32, parameter :: authname = "PS_XPLASMA2"

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  id = 0

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_protmp: invalid or unitialized xplasma object.'
     return
  endif

  tmpname = ' '
  jtmp=abs(itmp)
  write(tmpname,'("__TMPF_",i5.5)') jtmp

  call xplasma_author_set(s,authname,iertmp)
  
  call xplasma_create_prof(s,tmpname,idrho,pro,id,ierr, ispline=intrp)

  call xplasma_author_clear(s,authname,iertmp)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_rhotmp: xplasma error creating temporary profile:'
     call xplasma_error(s,ierr,iout)
  endif

end subroutine ps_protmp

subroutine ps_wprotmp(iobj,idrho,npro,pro,itmp,intrp,id,iout,ierr)

  !------------------------------------------------------------
  ! create a temporary f(rho) "weight" profile (should be positive definite).
  ! use ps_freetmp(id,iout) to remove it when done
  !   arguments same is ps_protmp; this function returns an error
  !   if pro(1:npro) changes sign; it returns ierr=ID=0 if pro(1:npro)
  !   is identically zero everywhere.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer :: iobj(ps_container_size)     ! xplasma opaque container

  integer, intent(in) :: idrho    ! ID of rho grid over which data is defined

  integer, intent(in) :: npro     ! profile data size (must be consistent with
  !                                 idrho and intrp choices).

  REAL(KIND=rspec), intent(in) :: pro(npro)   ! the profile values

  integer, intent(in) :: itmp     ! number to use for temporary name generation
  !  if multiple temporary profiles are created, differing values of itmp are
  !  needed to separate them (i.e. give them different names in xplasma).
  !    abs(itmp) is used.

  integer, intent(in) :: intrp    ! interpolation method for profile
  !                       -1: zonal step function
  !                        0: piecewise linear
  !                        1: C1 cubic Hermite
  !                        2: C2 cubic Spline

  integer, intent(out) :: id      ! ID returned (0 if error occurs)

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  !-----------------------
  !  local:
  REAL(KIND=rspec) :: promin,promax
  REAL(KIND=rspec), parameter :: ZERO = 0.0_rspec
  !-----------------------

  id = 0

  promin = minval(pro)
  promax = maxval(pro)

  if((promin.eq.ZERO).and.(promax.eq.ZERO)) then
     ierr = 0
     return    ! no weighting profile (identically ZERO)
  endif

  if(promin*promax.lt.ZERO) then
     ierr = 999
     write(iout,*) ' ?ps_wprotmp: profile weight data changes sign:'
     write(iout,*) '   minval(pro) = ',promin
     write(iout,*) '   maxval(pro) = ',promax
     return
  endif

  !  OK -- use standard routine to set up temporary profile

  call ps_protmp(iobj,idrho,npro,pro,itmp,intrp,id,iout,ierr)

end subroutine ps_wprotmp
