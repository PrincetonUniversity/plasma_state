subroutine ccps_set_debug_level(idebug,ierr)

  ! Set global debug level

  use plasma_state_mod
  implicit NONE
  integer, intent(in) :: idebug ! 0, 1, 2, or 3
  integer, intent(out) :: ierr    ! status code returned, 0=normal

  ierr = 0
  ps_debug = idebug

end subroutine ccps_set_debug_level
