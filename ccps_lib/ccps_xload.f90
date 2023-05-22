subroutine ccps_xload(iobj,ierr)

  ! store plasma state data in xplasma
  ! enable interpolation services

  ! scalar data is written if section hashes indicate change, or if tests
  ! indicate t
  
  ! grid data items are written if hashes indicate change, or if grid IDs
  ! cannot be found in xplasma

  ! profile data is written if hashes indicate change of id ss%id_<profile>
  ! variables are zero.

  use plasma_state_mod
  implicit NONE

  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  integer,intent(out) :: ierr      ! exit status 0=OK

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_xload',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call plasma_state_xload(ss, ierr)

end subroutine ccps_xload
