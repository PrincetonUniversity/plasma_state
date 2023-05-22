subroutine ps_purge_equilibrium(iobj,iout,ierr)

  !  Remove equilibrium-related data from a plasma state
  !  XPLASMA2 implementation -- DMC Jul 18 2007

  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed:

  integer :: iobj(ps_container_size)     ! xplasma opaque container
  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-------------------------------------------------------------------------
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-------------------------------------------------------------------------
  ! executable code...

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_purge_equilibrium: invalid or unitialized xplasma object.'
     return
  endif

  !----------------------

  call xplasma_remove_author(s,xplasma_xmhd,ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_purge_equilibrium: xplasma error:'
     call xplasma_error(s,iout,ierr)
  endif

  call xplasma_remove_author(s,'EQ',ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_purge_equilibrium: xplasma error:'
     call xplasma_error(s,iout,ierr)
  endif

end subroutine ps_purge_equilibrium
