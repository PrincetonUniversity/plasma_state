subroutine ccps_xload_check(iobj,iout,icheck,ierr)
  ! load xplasma from state structure; only load items that changed
  ! or are not found in the xplasma object (small items may be loaded
  ! regardless).  Check for rebuild of equilibrium in xplasma.

  use plasma_state_mod
  implicit NONE

  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  INTEGER, intent(in) :: iout   ! I/O channel for messages
  INTEGER, intent(in) :: icheck ! =0 to SKIP equilibrium check
  INTEGER, intent(out) :: ierr  ! completion status (0=OK)

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_xload_check',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_xload_check(ss, iout, icheck, ierr)
end subroutine ccps_xload_check
