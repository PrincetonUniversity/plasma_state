subroutine ps_rhotmp(iobj,nrho,rho,itmp,id,iout,ierr)

  !------------------------------------------------------------
  ! create a temporary rho grid
  ! use ps_freetmp(id,iout) to remove it when done
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer :: iobj(ps_container_size)     ! xplasma opaque container

  integer, intent(in) :: nrho     ! rho grid size
  REAL(KIND=rspec), intent(in) :: rho(nrho)   ! the grid values

  integer, intent(in) :: itmp     ! number to use for temporary name generation
  !  if multiple temporary grids are created, differing values of itmp are
  !  needed to separate them (i.e. give them different names in xplasma).
  !    abs(itmp) is used.

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
     write(iout,*) ' %ps_rhotmp: invalid or unitialized xplasma object.'
     return
  endif

  tmpname = ' '
  jtmp=abs(itmp)
  write(tmpname,'("__TMPRHO_",i5.5)') jtmp

  call xplasma_author_set(s,authname,iertmp)
  
  call xplasma_create_grid(s,tmpname,xplasma_rho_coord,rho,id,ierr)

  call xplasma_author_clear(s,authname,iertmp)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_rhotmp: xplasma error creating temporary rho grid:'
     call xplasma_error(s,ierr,iout)
  endif

end subroutine ps_rhotmp
