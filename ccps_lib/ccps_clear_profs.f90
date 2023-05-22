SUBROUTINE ccps_clear_profs(iobj, cclist, save_hash, ierr)
  use plasma_state_mod
  implicit NONE
  integer :: iobj(*)   ! (ps_container_size) words : state object
  INTEGER, INTENT(IN) :: cclist(ps_ccount)  ! component list
  !  default: clear profiles of ALL components; presence of this list
  !  allows selection of subset.

  integer, INTENT(IN) :: save_hash  ! 1 to save hash codes
  !  set in the state prior to this call; by default they are not saved.
  integer, intent(out) :: ierr  ! exit status code (0=OK)

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  logical :: save_hash_log
  integer :: iersum
  !-------------------------

  iersum = 0

  call ccps_argcheck('ccps_clear_profs ',iobj,ierr)
  if(ierr.ne.0) iersum=iersum+1
  
  if(iersum.gt.0) then
     ierr=1
     return
  endif

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  save_hash_log = .FALSE.
  if (save_hash == 1) save_hash_log = .TRUE.

  call ps_clear_profs(state=ss, cclist=cclist, &
       & save_hash=save_hash_log, ierr=ierr)

end SUBROUTINE ccps_clear_profs
