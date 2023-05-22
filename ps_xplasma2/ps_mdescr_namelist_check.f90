subroutine ps_mdescr_namelist_check(ss,iout,ierr)

  !------------------------------------------------------------
  ! (hand maintained) namelist input checks -- machine description namelist
  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions

  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ss       ! plasma state object
  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-----------------------

  ierr = 0

  write(iout,*) ' %ps_mdescr_namelist_check:'
  write(iout,*) '  machine description namelist checks: not implemented.'

end subroutine ps_mdescr_namelist_check
