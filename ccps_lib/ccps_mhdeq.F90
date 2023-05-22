! $Id: ccps_mhdeq.F90,v 1.3 2009-01-20 02:13:57 xshare Exp $

subroutine ccps_mhdeq_verify(iobj, reltol, nrhowk, rhowk, &
       update_phit, update_psi, update_q, check_phit, check_q,  &
       relerr_max, ierr)

    ! Verify that profile of enclosed toroidal flux state%phit(:) is 
    ! consistent with definition of rho coordinate, i.e. that the relation
    !
    !     [Tor.flux] = rho**2*[Tor.flux.at.bdy]
    !
    ! is satisfied to reasonable accuracy (2% or value of optional argument
    ! reltol).
    !
    ! If flags are set, actually update the profiles state%phit(:),
    ! state%psipol(:), and/or state%q_eq(:).
    !    Note#1: all update controls if defaulted are taken as .FALSE.
    !    Note#2: update of Psipol depends on q_eq; update of q_eq depends
    !            on Psipol.  Therefore update_psi and update_q cannot BOTH
    !            be .TRUE. (an error would be reported).
    !
    ! if (check_q) is set, check consistency of q_eq and Psipol profiles.
    !              this is the default.
    !
    ! Note: consistency of MHDEQ pressure profile NOT checked!  This routine
    !       does not check the self-consistency of the Grad-Shafranov MHD
    !       equilibrium in the state!  Only the fluxes are checked.
    !
    ! If the check reveals no error, after any requested profiles are 
    ! updated, internal splines representing equilibrium field quantities
    ! are updated; this allows all PS_MHDEQ_DERIVE actions to be carried
    ! out.
 

  use plasma_state_mod
  implicit NONE

   integer :: iobj(*)        ! (ps_container_size) words are used: state object
    REAL(kind=rspec), intent(IN) :: reltol ! error tolerance for
                                  ! toroidal flux check (default: 2%)

    integer, intent(in)                             :: nrhowk   				  
    REAL(kind=rspec), dimension(nrhowk), intent(IN) :: rhowk
                                  ! working grid for <1/R^2> integration
                                  ! if defaulted use state%rho_eq_geo.
                                  ! some codes may want a coarser grid for
                                  ! performance reasons.

    ! all LOGICAL optional controls default to 0:

    integer, intent(IN) :: update_phit ! update toroidal flux profile
                                                 ! state%phit(:)

    integer, intent(IN) :: update_psi  ! update poloidal flux profile
                                                 ! state%psipol(:)

    integer, intent(IN) :: update_q    ! update q profile
                                                 ! state%q_eq(:)

    integer, intent(IN) :: check_phit  ! check consistency of
                                                 ! toroidal flux profile
                                                 ! default: .TRUE.

    integer, intent(IN) :: check_q     ! check consistency of q
                                                 ! and psipol profiles
                                                 ! default: .TRUE.

    REAL(kind=rspec), intent(out) :: relerr_max  ! maximum rel. error
                                                 ! in phit or q tests
    integer, intent(out)           :: ierr       ! error code (0=OK)

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss

  logical :: update_phit_bool = .FALSE.
  logical :: update_psi_bool = .FALSE.
  logical :: update_q_bool = .FALSE.
  logical :: check_phit_bool = .TRUE.
  logical :: check_q_bool = .TRUE.
  !-------------------------

  if (update_phit /= 0) update_phit_bool = .TRUE.
  if (update_psi /= 0) update_psi_bool = .TRUE.
  if (update_q /= 0) update_q_bool = .TRUE.

  if (check_phit == 0) check_phit_bool = .FALSE.
  if (check_q == 0) check_q_bool = .FALSE.

  call ccps_argcheck('ccps_mhd_verify',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  if (nrhowk /= 0) then 
     call ps_mhdeq_verify(ierr=ierr, state=ss, reltol=reltol, rhowk=rhowk, &
       update_phit=update_phit_bool, update_psi=update_psi_bool, update_q=update_q_bool, &
       check_phit=check_phit_bool, check_q=check_q_bool, &
       relerr_max=relerr_max)
  else
     call ps_mhdeq_verify(ierr=ierr, state=ss, reltol=reltol, &
       update_phit=update_phit_bool, update_psi=update_psi_bool, update_q=update_q_bool, &
       check_phit=check_phit_bool, check_q=check_q_bool, &
       relerr_max=relerr_max)
  endif

end subroutine ccps_mhdeq_verify

subroutine ccps_mhdeq_derive(iobj, action, nrhowk, rhowk, ierr) 

    ! compute flux surface averages and other profiles derived from
    ! the MHD equilibrium geometry and fields

  use iso_c_binding, only: c_char
  use plasma_state_mod
  implicit NONE


    integer :: iobj(*)        ! (ps_container_size) words are used: state object
    character(kind=c_char), intent(in) :: action      ! specify action(s) to be taken (null termined)
    integer, intent(in)  :: nrhowk  ! size of rhowk
    REAL(kind=rspec), intent(IN) :: rhowk(nrhowk)
                                  ! working grid for flux surface averages
                                  ! if defaulted use state%rho_eq_geo.
                                  ! some codes may want a coarser grid for
                                  ! performance reasons
    integer, intent(out) :: ierr             ! exit status code (0=OK)

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  character(len=256)           :: action_ftn
  !-------------------------

  call ccps_argcheck('ccps_mhd_derive',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  ! convert to fortran string
  call cstring(action_ftn, action, '2F')  ! convert file path to 
                                            ! fortran string (g_filename)

  if (nrhowk == 0) then 
     call ps_mhdeq_derive(action=action_ftn, ierr=ierr, state=ss)
  else
     call ps_mhdeq_derive(action=action_ftn, ierr=ierr, state=ss, rhowk=rhowk)
  endif

end subroutine ccps_mhdeq_derive
