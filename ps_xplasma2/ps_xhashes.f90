subroutine ps_xhashes_get(iobj,xhash,iout,ierr)

  ! fetch the xplasma hashes

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !================================
  integer :: iobj(ps_container_size)     ! xplasma opaque container
  integer :: xhash(ps_hash_total) ! xplasma data hash codes
  integer, intent(in) :: iout     ! LUN for messages
  integer, intent(out) :: ierr    ! completion status, 0=OK
  !================================

  ! local:
  logical :: inew  ! flag if hash entry is new in xplasma
  integer :: id
  integer, dimension(:), pointer :: ia_ptr
  real*8, dimension(:), pointer :: r8a_ptr
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------

  ierr = 0
  xhash = ps_unInit

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_xhashes_get: invalid or unitialized xplasma object.'
     return
  endif

  call xpobj_blkbx(xobj,xpobj_hash_name,ps_hash_total,r8hash_size, &
       inew,id,ia_ptr,r8a_ptr,iout,ierr)
  if(ierr.ne.0) return

  xhash = ia_ptr

  nullify(ia_ptr)
  nullify(r8a_ptr)

end subroutine ps_xhashes_get

subroutine ps_xhashes_put(iobj,xhash,iout,ierr)

  ! replace the xplasma hashes with updated values

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !================================
  integer :: iobj(ps_container_size)     ! xplasma opaque container
  integer :: xhash(ps_hash_total) ! xplasma data hash codes
  integer, intent(in) :: iout     ! LUN for messages
  integer, intent(out) :: ierr    ! completion status, 0=OK
  !================================

  ! local:
  logical :: inew  ! flag if hash entry is new in xplasma
  integer :: id
  integer, dimension(:), pointer :: ia_ptr
  real*8, dimension(:), pointer :: r8a_ptr
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------

  ierr = 0

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_xhashes_put: invalid or unitialized xplasma object.'
     return
  endif

  call xpobj_blkbx(xobj,xpobj_hash_name,ps_hash_total,r8hash_size, &
       inew,id,ia_ptr,r8a_ptr,iout,ierr)
  if(ierr.ne.0) return

  ia_ptr = xhash

  nullify(ia_ptr)
  nullify(r8a_ptr)

end subroutine ps_xhashes_put
