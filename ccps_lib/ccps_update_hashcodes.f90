subroutine ccps_update_hashcodes(iobj, &
     mdescr_iflag,sconfig_iflag,simInit_iflag,ierr)

  ! update hash table; lock (or unlock) initialization sections of state

  use plasma_state_mod
  implicit NONE

  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  integer :: mdescr_iflag   ! .NE. 0 => lock machine description section
  integer :: sconfig_iflag  ! .NE. 0 => lock shot configuration section
  integer :: simInit_iflag  ! .NE. 0 => lock simulation initialization section
  integer,intent(out) :: ierr      ! exit status 0=OK

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_update_hashcodes',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_update_hashcodes(ierr, state=ss, &
       mdescr_lock = (mdescr_iflag.NE.0), &
       sconfig_lock = (sconfig_iflag.NE.0), &
       simInit_lock = (simInit_iflag.NE.0) )

end subroutine ccps_update_hashcodes

subroutine ccps_save_hash_codes(iobj, ierr)

  use plasma_state_mod
  implicit none
  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  integer,intent(out) :: ierr ! exit status 0=OK

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_save_hash_codes',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_save_hash_codes(iwarn=ierr, state=ss)

end subroutine ccps_save_hash_codes

subroutine ccps_restore_hash_codes(iobj, ierr)

  use plasma_state_mod
  implicit none
  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  integer,intent(out) :: ierr  ! exit status 0=OK

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_restore_hash_codes',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_restore_hash_codes(ierr=ierr, state=ss)

end subroutine ccps_restore_hash_codes
