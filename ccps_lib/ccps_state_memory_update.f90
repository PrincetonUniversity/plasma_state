subroutine ccps_state_memory_update(iobj,ierr)

  ! update hash table and interpolation data associated with state

  use plasma_state_mod
  implicit NONE

  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  integer,intent(out) :: ierr      ! exit status 0=OK

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_state_memory_update',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_state_memory_update(ierr, state=ss)

end subroutine ccps_state_memory_update
