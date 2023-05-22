subroutine ccps_get_version_id(version, size, ierr)

  ! Set global debug level

  use plasma_state_mod
  implicit NONE
  character(len=*), intent(out) :: version
  integer, intent(out) :: size
  integer, intent(out) :: ierr    ! status code returned, 0=normal

  ierr = 0
  size    = len(ps_version_id)
  version = ps_version_id

end subroutine ccps_get_version_id
