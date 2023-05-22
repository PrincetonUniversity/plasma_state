
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 2.056
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_e2_dummy
     return
  end subroutine ccps_getset_e2_dummy

subroutine psget_einj_max(iobj, n1, data, ierr)
    !! maximum injection energy, maximum energy of the full energy
    !! fractional component of each beam, Some codes use
    !! 1.5*maxval(Einj_max) as an upper limit on a lab frame, energy
    !! grid for beam ions slowing down distribution functions.
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(out)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_einj_max', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % Einj_max
end subroutine psget_einj_max


subroutine psset_einj_max(iobj, n1, data, ierr)
    !! maximum injection energy, maximum energy of the full energy
    !! fractional component of each beam, Some codes use
    !! 1.5*maxval(Einj_max) as an upper limit on a lab frame, energy
    !! grid for beam ions slowing down distribution functions.
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(in)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_einj_max', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % Einj_max = data(1:n1)
end subroutine psset_einj_max


subroutine psgetrnk_einj_max(iobj, ndims, ierr)
    !! maximum injection energy, maximum energy of the full energy
    !! fractional component of each beam, Some codes use
    !! 1.5*maxval(Einj_max) as an upper limit on a lab frame, energy
    !! grid for beam ions slowing down distribution functions.
    use plasma_state_mod, only :  plasma_state_container, plasma_state, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(out) :: ndims  ! number of dimensions
    integer, intent(out) :: ierr   ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    ndims = 1
end subroutine psgetrnk_einj_max


subroutine psgetdim_einj_max(iobj, ndims, dims, ierr)
    !! maximum injection energy, maximum energy of the full energy
    !! fractional component of each beam, Some codes use
    !! 1.5*maxval(Einj_max) as an upper limit on a lab frame, energy
    !! grid for beam ions slowing down distribution functions.
    use plasma_state_mod, only :  plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(in) :: ndims   ! number of dimensions
    integer, intent(out) :: dims(ndims) ! sizes
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    if (ndims /= 1) ierr = 1
    call ccps_argcheck('psgetdim_einj_max', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % Einj_max ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % Einj_max)
    endif
end subroutine psgetdim_einj_max


subroutine psget_ec_half_power_angle(iobj, n1, data, ierr)
    !! Divergence of EC beam, beam at half-power, this half-angle
    !! away from, direction of beam centerline which is started, at
    !! the launcher (treated as a point source);, See
    !! EC_theta_aim(necrf_src) & EC_phi_aim(necrf_src), in the shot
    !! configuration section.
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(out)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_ec_half_power_angle', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % EC_Half_Power_Angle
end subroutine psget_ec_half_power_angle


subroutine psset_ec_half_power_angle(iobj, n1, data, ierr)
    !! Divergence of EC beam, beam at half-power, this half-angle
    !! away from, direction of beam centerline which is started, at
    !! the launcher (treated as a point source);, See
    !! EC_theta_aim(necrf_src) & EC_phi_aim(necrf_src), in the shot
    !! configuration section.
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(in)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_ec_half_power_angle', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % EC_Half_Power_Angle = data(1:n1)
end subroutine psset_ec_half_power_angle


subroutine psgetrnk_ec_half_power_angle(iobj, ndims, ierr)
    !! Divergence of EC beam, beam at half-power, this half-angle
    !! away from, direction of beam centerline which is started, at
    !! the launcher (treated as a point source);, See
    !! EC_theta_aim(necrf_src) & EC_phi_aim(necrf_src), in the shot
    !! configuration section.
    use plasma_state_mod, only :  plasma_state_container, plasma_state, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(out) :: ndims  ! number of dimensions
    integer, intent(out) :: ierr   ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    ndims = 1
end subroutine psgetrnk_ec_half_power_angle


subroutine psgetdim_ec_half_power_angle(iobj, ndims, dims, ierr)
    !! Divergence of EC beam, beam at half-power, this half-angle
    !! away from, direction of beam centerline which is started, at
    !! the launcher (treated as a point source);, See
    !! EC_theta_aim(necrf_src) & EC_phi_aim(necrf_src), in the shot
    !! configuration section.
    use plasma_state_mod, only :  plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(in) :: ndims   ! number of dimensions
    integer, intent(out) :: dims(ndims) ! sizes
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    if (ndims /= 1) ierr = 1
    call ccps_argcheck('psgetdim_ec_half_power_angle', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % EC_Half_Power_Angle ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % EC_Half_Power_Angle)
    endif
end subroutine psgetdim_ec_half_power_angle


subroutine psget_ec_theta_aim(iobj, n1, data, ierr)
    !! poloidal aiming angle, = angle from +Z direction:, 0
    !! degrees-> straight up;, +/-  45 degrees-> on a cone in an
    !! upward direction, +/-  90 degrees-> in a horizontal
    !! direction..., +/- 135 degrees-> on a cone in a downward
    !! direction, +/- 180 degrees-> straight down, (for values other
    !! than 0,+/- 180, see alse EC_phi_aim).
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(out)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_ec_theta_aim', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % EC_theta_aim
end subroutine psget_ec_theta_aim


subroutine psset_ec_theta_aim(iobj, n1, data, ierr)
    !! poloidal aiming angle, = angle from +Z direction:, 0
    !! degrees-> straight up;, +/-  45 degrees-> on a cone in an
    !! upward direction, +/-  90 degrees-> in a horizontal
    !! direction..., +/- 135 degrees-> on a cone in a downward
    !! direction, +/- 180 degrees-> straight down, (for values other
    !! than 0,+/- 180, see alse EC_phi_aim).
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(in)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_ec_theta_aim', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % EC_theta_aim = data(1:n1)
end subroutine psset_ec_theta_aim


subroutine psgetrnk_ec_theta_aim(iobj, ndims, ierr)
    !! poloidal aiming angle, = angle from +Z direction:, 0
    !! degrees-> straight up;, +/-  45 degrees-> on a cone in an
    !! upward direction, +/-  90 degrees-> in a horizontal
    !! direction..., +/- 135 degrees-> on a cone in a downward
    !! direction, +/- 180 degrees-> straight down, (for values other
    !! than 0,+/- 180, see alse EC_phi_aim).
    use plasma_state_mod, only :  plasma_state_container, plasma_state, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(out) :: ndims  ! number of dimensions
    integer, intent(out) :: ierr   ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    ndims = 1
end subroutine psgetrnk_ec_theta_aim


subroutine psgetdim_ec_theta_aim(iobj, ndims, dims, ierr)
    !! poloidal aiming angle, = angle from +Z direction:, 0
    !! degrees-> straight up;, +/-  45 degrees-> on a cone in an
    !! upward direction, +/-  90 degrees-> in a horizontal
    !! direction..., +/- 135 degrees-> on a cone in a downward
    !! direction, +/- 180 degrees-> straight down, (for values other
    !! than 0,+/- 180, see alse EC_phi_aim).
    use plasma_state_mod, only :  plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(in) :: ndims   ! number of dimensions
    integer, intent(out) :: dims(ndims) ! sizes
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    if (ndims /= 1) ierr = 1
    call ccps_argcheck('psgetdim_ec_theta_aim', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % EC_theta_aim ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % EC_theta_aim)
    endif
end subroutine psgetdim_ec_theta_aim


subroutine psget_eperp_rw(iobj, n1, data, ierr)
    !! runaway electron <Eperp>
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(out)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_eperp_rw', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % eperp_rw
end subroutine psget_eperp_rw


subroutine psset_eperp_rw(iobj, n1, data, ierr)
    !! runaway electron <Eperp>
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(in)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_eperp_rw', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % eperp_rw = data(1:n1)
end subroutine psset_eperp_rw


subroutine psgetrnk_eperp_rw(iobj, ndims, ierr)
    !! runaway electron <Eperp>
    use plasma_state_mod, only :  plasma_state_container, plasma_state, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(out) :: ndims  ! number of dimensions
    integer, intent(out) :: ierr   ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    ndims = 1
end subroutine psgetrnk_eperp_rw


subroutine psgetdim_eperp_rw(iobj, ndims, dims, ierr)
    !! runaway electron <Eperp>
    use plasma_state_mod, only :  plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(in) :: ndims   ! number of dimensions
    integer, intent(out) :: dims(ndims) ! sizes
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    if (ndims /= 1) ierr = 1
    call ccps_argcheck('psgetdim_eperp_rw', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % eperp_rw ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % eperp_rw)
    endif
end subroutine psgetdim_eperp_rw

