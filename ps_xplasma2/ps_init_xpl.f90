subroutine ps_init_xpl(iobj,iout,ierr)

  !-----------------------------------------------------
  ! D. McCune 26 Nov 2007
  !
  ! Re-initialize xplasma object associated with state
  ! In version 1.xxx of the software this was done when xplasma_read(...)
  ! was invoked; with the I/O now done using NetCDF directly, this must
  ! be done as a separate step.
  !-----------------------------------------------------


  use plasma_state_definition_mod  ! plasma state object definitions
  use plasma_state_xpobj           ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer :: iobj(ps_container_size)              ! opaque handle => xplasma
  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  character*32 sname
  !-----------------------

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj
  call xplasma_global_info(s,ierr, init_check=.TRUE., initLabel=sname)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_init_xpl: xplasma object invalid or uninitialized.'
     return
  endif

  call xplasma_init(s,sname,ierr,total=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_init_xpl: xplasma_init error:'
     call xplasma_error(s,ierr,iout)
  endif

end subroutine ps_init_xpl
