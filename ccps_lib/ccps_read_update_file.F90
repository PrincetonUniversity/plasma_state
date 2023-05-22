subroutine ccps_read_update_file(iobj,filepath,finalflag,ierr)

  ! read file containing updates-- i.e. state element value changes.

  ! if finalflag.NE.0 is set, perform internal updates to state (update
  ! hash table and interpolation data).  If hashflag=0, hash table is not 
  ! updated.  If a series of update files are to be read, the most efficient
  ! thing to do is, set finalflag=0 for all but the last of the reads.

  use iso_c_binding, only: c_char
  use plasma_state_mod
  implicit NONE


  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  character(kind=c_char), intent(in) :: filepath(*)  ! null terminated file path string (max 256 chars)
  integer,intent(in) :: finalflag  ! .NE.0 to update state internal tables
  integer,intent(out) :: ierr      ! exit status 0=OK

  !-------------------------
  character*256 filename
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  logical :: fflag
  !-------------------------

  call ccps_argcheck('ccps_read_update_file',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  ! convert filepath to fortran string
  call cstring(filename,filepath,'2F')  ! convert file path to fortran string

  fflag = (finalflag.NE.0)
  call ps_read_update_file(filename, ierr, state=ss, update_complete=fflag)

end subroutine ccps_read_update_file
