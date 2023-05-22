subroutine ccps_mdescr_read(iobj, filepath, caction, g_filepath, ierr)

  ! read file containing machine description
  ! optionally, get limiter and (R,Z) simulation limits from G-eqdsk file.

  use iso_c_binding, only: c_char
  use plasma_state_mod
  implicit NONE


  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  character(kind=c_char), intent(in) :: filepath(*)  ! null terminated file path string (max 256 chars)
  character(kind=c_char), intent(in) :: caction(*)    ! null terminated string (max 32 chars)
  character(kind=c_char), intent(in) :: g_filepath(*) ! null terminated file path string (max 256 chars)
  ! if g_filename is blank or null, ignore.

  integer,intent(out) :: ierr      ! exit status 0=OK

  !-------------------------
  character*256 filename, g_filename
  character*32 action
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_mdescr_read',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  ! convert filepath to fortran string
  call cstring(filename,filepath,'2F')  ! convert file path to fortran string
  call cstring(action, caction, '2F')
  call cstring(g_filename,g_filepath,'2F')

  if(g_filename.eq.' ') then
     call ps_mdescr_read(filename, ierr, state=ss, action=action)
  else
     call ps_mdescr_read(filename, ierr, state=ss, &
          & action=action, g_filename=g_filename)
  endif

end subroutine ccps_mdescr_read
