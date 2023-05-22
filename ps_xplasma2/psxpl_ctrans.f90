subroutine psxpl_rhoth2rz(ss,isize,rho,th,R,Z,iout,ierr)

  !  coordinate transform: (rho,th) -> (R,Z)
  !  in xplasma contained in state "ss"

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod
  implicit NONE

  !---------------------------------------
  !  passed arguments:

  type (plasma_state) :: ss
  integer, intent(in) :: isize  ! vector size

  REAL(KIND=rspec), intent(in) :: rho(isize),th(isize)

  REAL(KIND=rspec), intent(out) :: R(isize),Z(isize)

  integer, intent(in) :: iout   ! LUN for messages
  integer, intent(out) :: ierr  ! exit status 0=OK

  !---------------------------------------
  !  local:

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  xobj = transfer(ss%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_ctrans(s, .TRUE., ierr, &
       rho_in = rho, theta_in = th, &
       R_out = R, Z_out = Z)

  if(ierr.ne.0) then
     write(iout,*) ' ?psxpl_rhoth2RZ: xplasma error:'
     call xplasma_error(s,ierr,iout)
  endif

end subroutine psxpl_rhoth2rz

subroutine psxpl_rz2rhoth(ss,isize,R,Z,rho,th,iout,imap,rtol,ierr)

  !  coordinate transform: (R,Z) -> (rho,th)
  !  in xplasma contained in state "ss"

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod
  implicit NONE

  !---------------------------------------
  !  passed arguments:

  type (plasma_state) :: ss
  integer, intent(in) :: isize  ! vector size

  REAL(KIND=rspec), intent(in) :: R(isize),Z(isize)

  REAL(KIND=rspec), intent(out) :: rho(isize),th(isize)

  integer, intent(in) :: iout   ! LUN for messages

  integer, intent(in) :: imap   ! inverse map selection code

  REAL(KIND=rspec), intent(in) :: rtol  ! accuracy tolerance, if imap=1
  !  (Newton map only)

  integer, intent(out) :: ierr  ! exit status 0=OK

  !---------------------------------------
  !  local:

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  xobj = transfer(ss%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_ctrans(s, .TRUE., ierr, &
       R_in = R, Z_in = Z, &
       rho_out = rho, theta_out = th, &
       maptype = imap, tol = rtol)

  if(ierr.ne.0) then
     write(iout,*) ' ?psxpl_rhoth2RZ: xplasma error:'
     call xplasma_error(s,ierr,iout)
  endif

end subroutine psxpl_rz2rhoth
