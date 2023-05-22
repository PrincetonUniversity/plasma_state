subroutine ps_time_set(iobj,curtime,iout,ierr)

  !  Update plasma state time
  !  XPLASMA2 implementation -- DMC Oct 10 2006

  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed:

  integer :: iobj(ps_container_size)     ! xplasma opaque container
  REAL(KIND=rspec), intent(in) :: curtime  ! time to assign
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
     write(iout,*) ' %ps_time_set: invalid or unitialized xplasma object.'
     return
  endif

  !----------------------

  call xplasma_time_set(s,curtime,ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_time_set: unexpected error in xplasma_time_set:'
     call xplasma_error(s,iout,ierr)
  endif

end subroutine ps_time_set
