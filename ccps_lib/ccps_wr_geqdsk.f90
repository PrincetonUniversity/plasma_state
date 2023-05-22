subroutine ccps_wr_geqdsk(iobj,filename,ierr)

  ! write file containing machine description

  use iso_c_binding, only: c_char
  use plasma_state_mod
  implicit NONE


  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  character(kind=c_char), intent(in) :: filename(*)  ! null terminated file name
  integer,intent(out) :: ierr      ! exit status 0=OK

  !-------------------------
  character*256 filename_ftn
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_wr_geqdsk',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  ! convert filename to fortran string
  call cstring(filename_ftn,filename,'2F')  ! convert filename => fortran string

  call ps_wr_geqdsk(g_filepath=filename_ftn, ierr=ierr, state=ss)

end subroutine ccps_wr_geqdsk
