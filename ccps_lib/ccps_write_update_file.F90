subroutine ccps_write_update_file(iobj,filepath,hashflag,ierr)

  ! write file containing updates-- i.e. state elements that have
  ! changed since the last time the file was read or written.

  ! if hashflag.NE.0 is set, update hash table, so that a subsequent
  ! call to this routine won't rewrite the same elements (unless they
  ! change value again).  If hashflag=0, hash table is not updated.

  use iso_c_binding, only: c_char
  use plasma_state_mod
  implicit NONE


  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  character(kind=c_char), intent(in) :: filepath(*)  ! null terminated file path string (max 256 chars)
  integer,intent(in) :: hashflag   ! .NE.0 to update I/O hash table
  integer,intent(out) :: ierr      ! exit status 0=OK

  !-------------------------
  character*256 filename
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  logical :: hash_update
  !-------------------------

  call ccps_argcheck('ccps_write_update_file',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  ! convert filepath to fortran string
  call cstring(filename,filepath,'2F')  ! convert file path to fortran string

  hash_update = (hashflag.NE.0)
  call ps_write_update_file(filename, ierr, state=ss, hash_update=hash_update)

end subroutine ccps_write_update_file
