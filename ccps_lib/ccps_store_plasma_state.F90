subroutine ccps_store_plasma_state(iobj,filepath,ierr)

  ! write state object contents to file

  use iso_c_binding, only: c_char
  use plasma_state_mod
  implicit NONE


  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  character(kind=c_char), intent(in) :: filepath(*)  ! null terminated file path string (max 256 chars)
  integer,intent(out) :: ierr      ! exit status 0=OK

  !-------------------------
  character*256 filename
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_store_plasma_state',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  ! convert filepath to fortran string
  call cstring(filename,filepath,'2F')  ! convert file path to fortran string

  call ps_store_plasma_state(ierr, filename=filename, state=ss)

end subroutine ccps_store_plasma_state
