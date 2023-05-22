subroutine ccps_rho_rezone(iobj,id,ngrid,rho_grid,result, &
     curflag, normflag, smooflag, ierr)

  ! REZONE a 1d profile f(rho), rho=sqrt(tor.flux) flux label
  !   Densities are rezoned to conserve volume integral
  !   Temperatures are rezoned to conserve density weighted volume integral
  !   Current densities are rezoned to conserve cross-sect. area integral

  !   The user specifies the target rho_grid boundaries -- strict ascending
  !   sequence spanning [0.0_rspec:1.0_rspec] -- ngrid points

  !   The result returned is "zone oriented" -- between the boundary points --
  !   (ngrid-1) values returned.

  !   curflag.NE.0 indicates quantity has area normalization (i.e. a
  !   current density).

  !   normflag.NE.0 means: return integrated quantities, e.g. 
  !     (#ptcls/zone), (Amps), (Watts),...
  !   normflag.EQ.0 means: return normalized quantities, e.g.
  !     (#ptcls/m^3), (Amps/m^2), (Watts/m^3)

  !   ...See subroutine ccps_rho_metric to get normaling areas or volumes
  !   ***normflag has no effect on temperatures or other weighted-averaged 
  !   quantities.

  !   smooflag.NE.0 means: smooth the result; total integrated over entire
  !   range [0:1] is still conserved.

  !   Note on smoothing: if underlying profile being rezoned is a step 
  !   function, rezoning to a finer grid without smoothing will reveal
  !   the underlying step structure.  If smoothing is specified, the
  !   results are blended over a zone width with a convolution.  This
  !   removes the "step-function-like" behavior in the result.  However,
  !   any smoothing can produce small artifacts...

  use plasma_state_mod
  implicit NONE

  integer :: iobj(*)        ! (ps_container_size) words are used: state object

  integer, intent(in) :: ID ! profile to be rezoned.  If id references a
  !  quantity that is specified as weighted in the plasma state specification
  !  then the rezoning here will be weighted (e.g. the case with temperature
  !  profiles).

  integer :: ngrid          ! size of RHO grid provided
  REAL(kind=rspec) :: rho_grid(ngrid)  ! the RHO grid
          ! *** strict ascending order ***
          ! *** in range [0.0_rspec:1.0_rspec] ***

  REAL(kind=rspec) :: result(ngrid-1) ! results returned ***(ngrid-1) values***

  integer, intent(in) :: curflag      ! set .NE.0 for current density rezone
  integer, intent(in) :: normflag     ! set .NE.0 for area or volume norm.
  integer, intent(in) :: smooflag     ! set .NE.0 for smoothing

  integer,intent(out) :: ierr   ! exit status 0=OK

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_rho_rezone',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_rho_rezone(rho_grid, id, result, ierr, state=ss, &
       curdens = (curflag.NE.0), &
       nonorm = (normflag.EQ.0), &
       zonesmoo = (smooflag.Ne.0))

end subroutine ccps_rho_rezone
