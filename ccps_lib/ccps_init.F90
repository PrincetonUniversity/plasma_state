subroutine ccps_init(iobj,bytename,ierr)

  ! construct a state object with passed, null-terminated tag name byte string
  ! this routine to be called from C/C++ constructor

  use iso_c_binding, only: c_char
  use plasma_state_mod
  implicit NONE


  integer :: iobj(*)   ! (ps_container_size) words are used
  character(kind=c_char), intent(in) :: bytename(*)
  integer :: ierr

  !--------------------------------
  type (plasma_state_container) :: ptr
  character*32 tagname
  type (plasma_state), pointer :: ss
  !--------------------------------

  call cstring(tagname,bytename,'2F')  ! fill fortran char string (tagname)

  ! allocate state object

  allocate(ptr%ps_ptr)
  ss => ptr%ps_ptr

  ! initialize with tagname

  call ps_init_user_state(ss,tagname,ierr)

  ! transfer pointer

  if(ierr.eq.0) then
     call unload_ps_container(ptr,iobj)
  else
     iobj(1:ps_container_size)=0
  endif

end subroutine ccps_init
