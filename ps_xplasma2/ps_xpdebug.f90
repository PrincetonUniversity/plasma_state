subroutine ps_xpdebug(iobj)

  !------------------------------------------------------------
  ! D. McCune May 11 2009

  ! access xplasma debug plots -- DEBUG COMPILE to get this access
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
#ifdef __DEBUG
  use eqi_intg_module     ! sp_debug pointer
#endif

  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)       ! xplasma opaque container
  !--------------------------------------------------
  type (interp_object) :: xobj
  !--------------------------------------------------

#ifdef __DEBUG
  xobj = transfer(iobj,xobj)
  sp_debug => xobj%s_xpobj

  call xplasma_debug
#else
  write(6,*) ' ?ps_xpdebug -- dummy code called.'
#endif

end subroutine ps_xpdebug
