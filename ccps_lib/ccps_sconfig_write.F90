subroutine ccps_sconfig_write(iobj,filename,ierr)

  ! write file containing shot configuration data

  use iso_c_binding, only: c_char
  use plasma_state_mod
  implicit NONE


  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  character(kind=c_char), intent(in) :: filename(*)  ! null terminated file name (max 256 chars)
  integer,intent(out) :: ierr      ! exit status 0=OK

  !-------------------------
  character*256 filename_ftn
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_sconfig_write',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  ! convert filepath to fortran string
  call cstring(filename_ftn,filename,'2F')  ! convert filename to fortran string

  call ps_sconfig_write(filename_ftn, ierr, state=ss)

end subroutine ccps_sconfig_write
