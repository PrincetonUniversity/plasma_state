subroutine ps_wrgeq_chk(ps,lbl_sub,iout,ierr)

  !---------------------------------------------------------------------
  ! check if PS object contains data needed to generate geqdsk output
  !---------------------------------------------------------------------

  use plasma_state_definition_mod
  implicit NONE

  !-----------------------
  ! arguments:

  type (plasma_state) :: ps            ! plasma state instance
  character*(*), intent(in) :: lbl_sub ! label to identify caller subroutine
  integer, intent(in) :: iout          ! I/O unit for messages
  integer, intent(out) :: ierr         ! status code output

  !-----------------------

  ierr = 0
  if(.not.allocated(ps%psirz)) then
     ierr = ierr + 1
     write(iout,*) ' ?'//trim(lbl_sub)//': not found: Psi(R,Z)'
  endif

  if(.not.allocated(ps%R_geo)) then
     ierr = ierr + 1
     write(iout,*) ' ?'//trim(lbl_sub)//': not found: R_geo'
  endif

  if(.not.allocated(ps%Z_geo)) then
     ierr = ierr + 1
     write(iout,*) ' ?'//trim(lbl_sub)//': not found: Z_geo'
  endif

  if(.not.allocated(ps%Rlim)) then
     ierr = ierr + 1
     write(iout,*) ' ?'//trim(lbl_sub)//': not found: limiter [R,Z] contour.'
  endif

end subroutine ps_wrgeq_chk
