subroutine ps_remove_grid(iobj,iauthor,gname,iout,ierr)

  !  Update plasma state time
  !  XPLASMA2 implementation -- DMC Oct 10 2006

  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed:

  integer :: iobj(ps_container_size)     ! xplasma opaque container
  integer, intent(in) :: iauthor  ! component or author index
  character*(*), intent(in) :: gname   ! name of grid
  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-------------------------------------------------------------------------
  !  local:
  
  integer :: id_grid,iertmp

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-------------------------------------------------------------------------
  ! executable code...

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_remove_grid: invalid or unitialized xplasma object.'
     return
  endif

  !----------------------
  call xplasma_gridId(s,gname,id_grid)
  if(id_grid.eq.0) return

  !----------------------

  call xplasma_author_set(s, ps_cnames(iauthor), ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_remove_grid: xplasma_author_set error:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  call xplasma_remove_grid(s, id_grid, ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_remove_grid: xplasma_remove_grid error:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  call xplasma_author_clear(s, ps_cnames(iauthor), iertmp)
  if(iertmp.ne.0) then
     write(iout,*) ' ?ps_remove_grid: xplasma_author_clear error:'
     call xplasma_error(s,iertmp,iout)
     ierr = iertmp
  endif

end subroutine ps_remove_grid
