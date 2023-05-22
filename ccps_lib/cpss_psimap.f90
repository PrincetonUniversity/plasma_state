subroutine ccps_psimap(iobj,tol,npsi,psi,rhopsi,n_outside,ierr)

  !  Given a set of poloidal flux (Psi) values, Wb/rad
  !  find the corresponding rho values (normalized sqrt toroidal flux, 
  !  btw 0, the magnetic axis, and 1, the boundary at/near the last closed
  !  flux surface).  This inverts the mapping Psipol(rho) corresponding to
  !  the spline element (psipol) in the plasma state.  Psipol(0)=Psimin;
  !  Psipol(1)=Psimax; input psi values should normally fall in this range.
  !     To see these values, read or interpolate the Psipol state element.

  use plasma_state_mod
  implicit NONE

  integer :: iobj(*)        ! (ps_container_size) words are used
  REAL(kind=rspec), intent(in) :: tol   ! tolerance for Psi->rho map.
  !  constrained to be between 1.0d-4 and 1.0d-14; if 0.0 is specified
  !  a default value of 1.0d-10 is used.
  !  On output, abs(Psi(rhopsi(i))-Psi(i)) <= tol*Psimax

  integer, intent(in) :: npsi  ! number of psi values in & rhopsi values out
  REAL(kind=rspec), intent(in) :: psi(npsi)     ! psi values in
  REAL(kind=rspec), intent(out) :: rhopsi(npsi) ! rho values out
  !     For psi(j) <= Psimin, rhopsi(j)=0.0 is returned;
  !     For psi(j) >= Psimax, rhopsi(j)=1.0 is returned.

  integer, intent(out) :: n_outside   ! number of values not in range
  !   [Psimin:Psimax]

  integer :: ierr

  !--------------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  REAL(kind=rspec) :: ztol
  !--------------------------------

  call ccps_argcheck('ccps_psimap',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  if(tol.eq.0.0_rspec) then
     ztol=1.0e-10_rspec
  else
     ztol=max(1.0e-14_rspec,min(1.0e-4_rspec,tol))
  endif

  call ps_psimap(psi,rhopsi,ierr, state=ss, tol=ztol, &
       iwarn=n_outside)

end subroutine ccps_psimap
