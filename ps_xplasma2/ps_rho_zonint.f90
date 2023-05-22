subroutine ps_rho_zonint(iobj, idi, integ_name, nvec, ansv, iout, ierr)

  !------------------------------------------------------------
  ! D. McCune 4 Apr 2007

  ! XPLASMA2 implementation of plasma_state subroutine to get numerically
  !   integrated metric quantities

  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)     ! xplasma opaque container
  integer, intent(in) :: idi   ! handle for integrator (0 if fail)

  character*(*), intent(in) :: integ_name  ! NAME of integration
  !  e.g. "dVol" for zone volumes

  integer, intent(in) :: nvec              ! size of answer array
  !  this size must = [integrator grid size] or [integrator grid size] - 1,
  !  depending on the type of integration
  !    the integrator grid size is known to the xplasma software via "idi".

  real(kind=rspec) :: ansv(nvec)           ! result of integration

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
     write(iout,*) ' %ps_rho_zonint: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  perform the integration with error checking...

  call xplasma_rho_zonint(s,idi,integ_name,ansv,ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_rho_zonint: integration "', &
          trim(integ_name),'" failed in xplasma:'
     call xplasma_error(s,ierr,iout)
     ansv = 0.0_rspec
     return
  endif

end subroutine ps_rho_zonint
