subroutine ps_freetmp(iobj,id,iout)

  !------------------------------------------------------------
  ! delate a temporary object
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer :: iobj(ps_container_size)     ! xplasma opaque container
  integer, intent(in) :: id       ! ID of object to free, if non-zero
  !  if id<=0 this routine silently does nothing.

  integer, intent(in) :: iout     ! LUN for messages

  !-----------------------
  integer :: iertmp
  character*32, parameter :: authname = "PS_XPLASMA2"

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  if(id.le.0) return

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,iertmp, init_check=.TRUE.)
  if(iertmp.ne.0) then
     write(iout,*) ' %ps_freetmp: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_author_set(s,authname,iertmp)

  call xplasma_remove_item(s,id,iertmp)
  if(iertmp.ne.0) then
     write(iout,*) ' %ps_freetmp: error removing item:'
     call xplasma_error(s,iertmp,iout)
  endif

  call xplasma_author_clear(s,authname,iertmp)

end subroutine ps_freetmp
