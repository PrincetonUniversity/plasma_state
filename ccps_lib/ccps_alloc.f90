subroutine ccps_alloc(iobj,ierr)

  ! allocate a portion of a state object
  !    state elements for which all array dimensions have been defined (are
  !    non-zero) are allocated.

  use plasma_state_mod
  implicit NONE

  integer :: iobj(*)   ! (ps_container_size) words are used
  integer :: ierr

  !--------------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !--------------------------------

  call ccps_argcheck('ccps_alloc',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_alloc_plasma_state(ierr, state=ss)

end subroutine ccps_alloc
