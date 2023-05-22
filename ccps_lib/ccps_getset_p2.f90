
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_p2_dummy
     return
  end subroutine ccps_getset_p2_dummy

subroutine psget_psi_to_machine_axis(iobj, data, ierr)
    !! delta(Psi) from magnetic axis to machine axis, subtract this
    !! from Psi(R,Z) to get a Psi value, that approaches zero as R->
    !! zero: Psi ~ R*A_phi, (as in the EFIT free boundary
    !! representation)., Note: prescribed boundary code runs (e.g.,
    !! traditional TRANSP runs) cannot know this, number; so Plasma
    !! States created from such run, data will leave this at zero. 
    !! Plasma States, using free boundary MHD equilibrium information,
    !! will always have a non-zero value here.
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    REAL(KIND=rspec), intent(out)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_psi_to_machine_axis', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % Psi_to_machine_axis
end subroutine psget_psi_to_machine_axis


subroutine psset_psi_to_machine_axis(iobj, data, ierr)
    !! delta(Psi) from magnetic axis to machine axis, subtract this
    !! from Psi(R,Z) to get a Psi value, that approaches zero as R->
    !! zero: Psi ~ R*A_phi, (as in the EFIT free boundary
    !! representation)., Note: prescribed boundary code runs (e.g.,
    !! traditional TRANSP runs) cannot know this, number; so Plasma
    !! States created from such run, data will leave this at zero. 
    !! Plasma States, using free boundary MHD equilibrium information,
    !! will always have a non-zero value here.
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    REAL(KIND=rspec), intent(in)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_psi_to_machine_axis', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % Psi_to_machine_axis = data
end subroutine psset_psi_to_machine_axis


subroutine psget_picrf_nphi_srcs(iobj, n1, n2, n3, n4, data, ierr)
    !! direct ICRF power deposition, contribution from each n_phi
    !! on each antenna
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1, n2, n3, n4 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(out)  :: data(n1, n2, n3, n4) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_picrf_nphi_srcs', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2, 1: n3, 1: n4) = ss % picrf_nphi_srcs
end subroutine psget_picrf_nphi_srcs


subroutine psset_picrf_nphi_srcs(iobj, n1, n2, n3, n4, data, ierr)
    !! direct ICRF power deposition, contribution from each n_phi
    !! on each antenna
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1, n2, n3, n4 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(in)  :: data(n1, n2, n3, n4) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_picrf_nphi_srcs', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % picrf_nphi_srcs = data(1:n1, 1: n2, 1: n3, 1: n4)
end subroutine psset_picrf_nphi_srcs


subroutine psgetrnk_picrf_nphi_srcs(iobj, ndims, ierr)
    !! direct ICRF power deposition, contribution from each n_phi
    !! on each antenna
    use plasma_state_mod, only :  plasma_state_container, plasma_state, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(out) :: ndims  ! number of dimensions
    integer, intent(out) :: ierr   ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    ndims = 4
end subroutine psgetrnk_picrf_nphi_srcs


subroutine psgetdim_picrf_nphi_srcs(iobj, ndims, dims, ierr)
    !! direct ICRF power deposition, contribution from each n_phi
    !! on each antenna
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
    if (ndims /= 4) ierr = 1
    call ccps_argcheck('psgetdim_picrf_nphi_srcs', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % picrf_nphi_srcs ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % picrf_nphi_srcs)
    endif
end subroutine psgetdim_picrf_nphi_srcs


subroutine psget_pbth(iobj, n1, data, ierr)
    !! thermalization of beam ions
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
    call ccps_argcheck('psget_pbth', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % pbth
end subroutine psget_pbth


subroutine psset_pbth(iobj, n1, data, ierr)
    !! thermalization of beam ions
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
    call ccps_argcheck('psset_pbth', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % pbth = data(1:n1)
end subroutine psset_pbth


subroutine psgetrnk_pbth(iobj, ndims, ierr)
    !! thermalization of beam ions
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
end subroutine psgetrnk_pbth


subroutine psgetdim_pbth(iobj, ndims, dims, ierr)
    !! thermalization of beam ions
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
    call ccps_argcheck('psgetdim_pbth', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % pbth ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % pbth)
    endif
end subroutine psgetdim_pbth


subroutine psget_peech(iobj, n1, data, ierr)
    !! electron heating by ECH
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
    call ccps_argcheck('psget_peech', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % peech
end subroutine psget_peech


subroutine psset_peech(iobj, n1, data, ierr)
    !! electron heating by ECH
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
    call ccps_argcheck('psset_peech', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % peech = data(1:n1)
end subroutine psset_peech


subroutine psgetrnk_peech(iobj, ndims, ierr)
    !! electron heating by ECH
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
end subroutine psgetrnk_peech


subroutine psgetdim_peech(iobj, ndims, dims, ierr)
    !! electron heating by ECH
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
    call ccps_argcheck('psgetdim_peech', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % peech ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % peech)
    endif
end subroutine psgetdim_peech


subroutine psget_phiflux_ky_tglf(iobj, n1, n2, data, ierr)
    !! momentum fluctuation spectrum
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1, n2 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(out)  :: data(n1, n2) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_phiflux_ky_tglf', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % phiflux_ky_tglf
end subroutine psget_phiflux_ky_tglf


subroutine psset_phiflux_ky_tglf(iobj, n1, n2, data, ierr)
    !! momentum fluctuation spectrum
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1, n2 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(in)  :: data(n1, n2) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_phiflux_ky_tglf', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % phiflux_ky_tglf = data(1:n1, 1: n2)
end subroutine psset_phiflux_ky_tglf


subroutine psgetrnk_phiflux_ky_tglf(iobj, ndims, ierr)
    !! momentum fluctuation spectrum
    use plasma_state_mod, only :  plasma_state_container, plasma_state, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(out) :: ndims  ! number of dimensions
    integer, intent(out) :: ierr   ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    ndims = 2
end subroutine psgetrnk_phiflux_ky_tglf


subroutine psgetdim_phiflux_ky_tglf(iobj, ndims, dims, ierr)
    !! momentum fluctuation spectrum
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
    if (ndims /= 2) ierr = 1
    call ccps_argcheck('psgetdim_phiflux_ky_tglf', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % phiflux_ky_tglf ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % phiflux_ky_tglf)
    endif
end subroutine psgetdim_phiflux_ky_tglf

