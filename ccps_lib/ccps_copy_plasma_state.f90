subroutine ccps_copy_plasma_state(iobj_from, iobj_to, &
     cclist, inodims, iwant_1deq, ierr)

  ! Copy state objects

  !  All of the data is physically copied; memory is not shared between
  !  states; modification to values of state A never effects state B.

  !  default is to copy ALL components

  !  unless optional argument inodims=1 is set, the to_state is erased
  !  prior to copy; to_state ends up containing ONLY the copied data

  !  if inodims=1 is set, to_state must have its array dimensions already
  !  defined; its contents are NOT erased.  Its non-zero dimensions MUST
  !  match the non-zero dimensions of from_state, or an error will be
  !  reported when the copy is attempted.

  !  (inodims=1 & cclist settings allow a portion of a state to be copied
  !  into another, established state).

  !  the optional control cclist allows selection of components for
  !  which grids and profiles should NOT be copied:

  !     cclist(ps_cnum_<component>) = 1 to copy this component's profiles
  !     cclist(ps_cnum_<component>) = 0 not to copy

  !  NOTE: machine_description and other scalars associated with
  !  components are still copied; only the copying of grids and profiles
  !  is affected by the cclist(...) control.

  !  the user can build a cclist, refering to components by name, using
  !  the ps_cclist_add and ps_cclist_remove subroutines...

  use plasma_state_mod
  implicit NONE

  integer :: iobj_from(*)   ! (ps_container_size) words : state object "from"
  integer :: iobj_to(*)     ! (ps_container_size) words : state object "to"
  INTEGER, INTENT(IN) :: cclist(ps_ccount)  ! component list
  INTEGER, INTENT(IN) :: inodims  ! =1 to copy data ONLY (no alloc)
  INTEGER, INTENT(IN) :: iwant_1deq  ! =1 to copy (nrho_eq) dim
  integer,intent(out) :: ierr      ! exit status 0=OK

  !-------------------------
  type (plasma_state_container) :: ptr_from,ptr_to
  type (plasma_state), pointer :: ss_from,ss_to
  integer :: iersum
  !-------------------------

  iersum = 0

  call ccps_argcheck('ccps_copy_plasma_state (from argument)',iobj_from,ierr)
  if(ierr.ne.0) iersum=iersum+1

  call ccps_argcheck('ccps_copy_plasma_state (to argument)',iobj_to,ierr)
  if(ierr.ne.0) iersum=iersum+1

  if(iersum.gt.0) then
     ierr=1
     return
  endif

  ! proceed...

  call load_ps_container(iobj_from,ptr_from)
  ss_from => ptr_from%ps_ptr

  call load_ps_container(iobj_to,ptr_to)
  ss_to => ptr_to%ps_ptr

  call ps_copy_plasma_state(from_state=ss_from, to_state=ss_to, &
       & cclist=cclist, inodims=inodims, iwant_1deq=iwant_1deq, ierr=ierr)

end subroutine ccps_copy_plasma_state
