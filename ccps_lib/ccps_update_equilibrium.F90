subroutine ccps_update_equilibrium(iobj,g_filepath,bdy_crat,kcur_option, &
     & rho_curbrk, npprime, rho_psmoo, delrho_psmoo, ierr)

  ! update MHD equilibrium stored in state object, from G-eqdsk file.

  use iso_c_binding, only: c_char
  use plasma_state_mod
  implicit NONE


  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  character(kind=c_char), intent(in) :: g_filepath(*) ! null terminated file path string (max 256 chars)
  REAL(kind=rspec) :: bdy_crat  ! curvature ratio limit for boundary
  ! bdy_crat controls selection of a boundary surface somewhat in from
  ! the separatrix Last Closed Flux Surface (LCFS) found in the G-eqdsk
  ! Psi(R,Z) data.  Typical value: 0.075 -- this says the local ratio
  ! of curvature anywhere along the chosen boundary surface must be
  ! greater than equal to 0.075*[midplane half radius of equilibrium].
  ! At the exact LCFS this ratio is zero; for a limiter bound plasma,
  ! the ratio is typically >> bdy_crat=0.075.  So, a limiter bound
  ! plasma uses the G-eqdsk boundary; a separatrix bound plasma has
  ! a boundary chosen which backs off sufficiently to meet this 
  ! geometrical constraint.
  integer, intent(in) :: kcur_option ! how to set q(rho) and I(rho)
                         ! kcur_option control to select method
                         ! for obtaining q(rho) and I(rho) enclosed
                         ! toroidal current function (details below).
                         ! Default = -1: choose based on file label;
                         !   =0: use EFIT file data for total current
                         !       and q(psi) profile
                         !   =1: compute total current and q(psi)
                         !       profile from Psi(R,Z); supersede
                         !       EFIT file data for these quantities.  
  real(kind=rspec), intent(in) :: rho_curbrk  ! break location for
                         ! I(rho) -- point at which to transition
                         ! from Psi(R,Z) Ampere's Law integration 
                         ! to a parabolic interpolation to reach
                         ! the total enclosed current (boundary 
                         ! value). Default = 0.9_rspec.
  integer, intent(in) :: npprime  ! > 0 to request 
                         ! recomputation of pressure profile using
                         ! PPRIME data from G-eqdsk input file;
                         ! a value > 0 indicates that npprime rho_psmoo
                         ! and delrho_psmoo are expected (see following)
                         ! default is 0 
  real(kind=rspec), dimension(npprime), intent(in) :: rho_psmoo
                                  ! rho location(s) for pressure profile
                                  ! smoothing array delrho_psmoo; if this
                                  ! is present, delrho_psmoo(...) must
                                  ! also be present, and, sizes must match.

  real(kind=rspec), dimension(npprime), intent(in) :: delrho_psmoo
                                  ! array of smoothing parameters for pressure
                                  ! profile; together with rho_psmoo this
                                  ! defines a profile to be piecewise linearly
                                  ! interpolated to produce local delta_rho
                                  ! values for smoothing.  At each rho location
                                  ! the pressure profile is smoothed with a 
                                  ! triangular weighted average of neighboring
                                  ! data, from rho-delta_rho to rho+delta_rho.
                                  ! if delrho_psmoo(...) is present, the array
                                  ! rho_psmoo(...) must also be present, and, 
                                  ! sizes must match.
  integer,intent(out) :: ierr   ! exit status 0=OK

  !-------------------------
  character*256 g_filename
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_update_equilibrium',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  ! convert filepath to fortran string
  call cstring(g_filename,g_filepath,'2F')  ! convert file path to 
                                            ! fortran string (g_filename)

  if (npprime > 0) then
     call ps_update_equilibrium(ierr, g_filepath=g_filename, bdy_crat=bdy_crat, &
       kcur_option=kcur_option, pprime_option=.TRUE., &
       rho_psmoo=rho_psmoo, delrho_psmoo=delrho_psmoo, &
       state=ss)
  else
     call ps_update_equilibrium(ierr, g_filepath=g_filename, bdy_crat=bdy_crat, &
       kcur_option=kcur_option, pprime_option=.FALSE., &
       state=ss)
  endif

end subroutine ccps_update_equilibrium
