subroutine ps_psimap_sub(nvec,psi,rhopsi,tol,jwarn,iobj,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 4 Apr 2007

  ! XPLASMA2 implementation of Psi -> rho(Psi) mapper
  ! Find rho values such that spline of Psi(rho(j)) = psi(j) input

  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer, intent(in) :: nvec              ! size of arrays

  real(kind=rspec), intent(in) :: psi(nvec)      ! Psi values
  real(kind=rspec), intent(out) :: rhopsi(nvec)  ! rho(Psi) values returned

  real(kind=rspec), intent(in) :: tol      ! accuracy tolerance
  integer, intent(out) :: jwarn            ! out-of-bounds warning (0 for none)

  integer :: iobj(ps_container_size)     ! xplasma opaque container
  integer, intent(in) :: iout              ! LUN for error messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_limsize: invalid or unitialized xplasma object.'
     jwarn = 99
     rhopsi = 0.0_rspec
     return
  endif

  !--------------------------------------------------
  !  perform the integration with error checking...

  call xplasma_rhopsi_find(s,psi,rhopsi,ierr, tol=tol, iwarn=jwarn)
  if(ierr.ne.0) then
     write(iout,*) &
          ' ?ps_psimap_sub: Psi -> rho(Psi) mapping failed in xplasma:'
     call xplasma_error(s,ierr,iout)
     rhopsi = 0.0_rspec
     return
  endif

end subroutine ps_psimap_sub
