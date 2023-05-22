subroutine ps_wr_geqdsk_file(ps,gfile,iout,ierr)

  ! write G-eqdsk file from contents of plasma state, if possible.
  ! Psi(R,Z), [R,Z](rho,theta), g(rho), q(rho), and P(rho) must be there.
  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions

  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  ! arguments:

  type (plasma_state) :: ps            ! plasma state instance 
  character*(*), intent(in) :: gfile   ! name of file to write
  integer, intent(in) :: iout          ! Fortran LUN for messages
  integer, intent(out) :: ierr         ! status code, 0=OK

  !------------------------
  ! local:

  integer :: ilun

  !------------------------

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-------------------------------------------------------------------------
  ! executable code...

  call ps_tagchk('ps_wr_geqdsk_file',ps%itag,iout,ierr)
  if(ierr.ne.0) return

  ! check contents

  call ps_wrgeq_chk(ps,'ps_wr_geqdsk_file',iout,ierr)

  if(ierr.ne.0) return

  ! OK everything seems to be there...

  ! get xplasma pointer

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  call find_io_unit(ilun)
  call xplasma_wr_geqdsk(s,ierr, &
       lun_geqdsk=ilun, filename=gfile, &
       label=ps%Global_label, &
       id_qprof=ps%id_q_eq, id_Pprof=ps%id_P_eq)

end subroutine ps_wr_geqdsk_file
