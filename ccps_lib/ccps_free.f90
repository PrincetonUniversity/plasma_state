subroutine ccps_free(iobj,ierr)

  ! free a state object -- this routine to be called by C/C++ destructor

  use plasma_state_mod
  implicit NONE

  integer :: iobj(*)   ! (ps_container_size) words are used
  integer :: ierr

  !--------------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !--------------------------------

  call ccps_argcheck('ccps_free',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_free_user_state(ss,ierr)

  deallocate(ptr%ps_ptr)

  iobj(1:ps_container_size)=0

end subroutine ccps_free
