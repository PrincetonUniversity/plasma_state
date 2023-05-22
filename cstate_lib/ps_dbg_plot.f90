subroutine ps_dbg_plot(iobj,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune Aug 2007
  ! Debug plotting of xplasma contents
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use xplasma_definitions
  use eqi_intg_module
  implicit NONE

  type :: interp_object

     type (xplasma), pointer :: s_xpobj => NULL()

  end type interp_object

  !-----------------------
  !  passed arguments:

  integer :: iobj(ps_container_size)  ! opaque container, xplasma object

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  !-----------------------
  ! local:
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj
  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_dbg_plot: xplasma object invalid or uninitialized.'
     return
  endif

  sp_debug => xobj%s_xpobj
  call xplasma_debug

end subroutine ps_dbg_plot
