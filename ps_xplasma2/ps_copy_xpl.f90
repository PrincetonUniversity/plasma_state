subroutine ps_copy_xpl(iobjp,iobj,iout,ierr)

  !-----------------------------------------------------
  ! D. McCune 26 Nov 2007
  !
  ! Copy xplasma object associated with state (iobjp) to that
  ! associated with state (iobj)
  !-----------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer :: iobjp(ps_container_size)    ! xplasma opaque container (copy from...)
  integer :: iobj(ps_container_size)     ! xplasma opaque container (copy to...)

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)
  
  type (interp_object) :: xobjp
  type (xplasma), pointer :: sp

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  xobjp = transfer(iobjp,xobjp)
  sp => xobjp%s_xpobj

  call xplasma_global_info(sp,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_copy_xpl: invalid or unitialized xplasma object (iobjp).'
     return
  endif

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_copy_xpl: invalid or unitialized xplasma object (iobj).'
     return
  endif

  call xplasma_copy(sp,s,ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_copy_xpl: xplasma_copy error:'
     call xplasma_error(s,ierr,iout)
  endif

end subroutine ps_copy_xpl
