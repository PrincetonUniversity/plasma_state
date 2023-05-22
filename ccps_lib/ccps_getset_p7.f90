
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_p7_dummy
     return
  end subroutine ccps_getset_p7_dummy

subroutine psget_phi_ec_launch(iobj, n1, data, ierr)
    !! Phi of center of launcher, this may be left at zero for
    !! axisymmetric simulations.
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
    call ccps_argcheck('psget_phi_ec_launch', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % Phi_EC_launch
end subroutine psget_phi_ec_launch


subroutine psset_phi_ec_launch(iobj, n1, data, ierr)
    !! Phi of center of launcher, this may be left at zero for
    !! axisymmetric simulations.
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
    call ccps_argcheck('psset_phi_ec_launch', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % Phi_EC_launch = data(1:n1)
end subroutine psset_phi_ec_launch


subroutine psgetrnk_phi_ec_launch(iobj, ndims, ierr)
    !! Phi of center of launcher, this may be left at zero for
    !! axisymmetric simulations.
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
end subroutine psgetrnk_phi_ec_launch


subroutine psgetdim_phi_ec_launch(iobj, ndims, dims, ierr)
    !! Phi of center of launcher, this may be left at zero for
    !! axisymmetric simulations.
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
    call ccps_argcheck('psgetdim_phi_ec_launch', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % Phi_EC_launch ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % Phi_EC_launch)
    endif
end subroutine psgetdim_phi_ec_launch


subroutine psget_power_nbi_trace(iobj, n1, data, ierr)
    !! power injected as trace element (if any), total power on
    !! beam ib is power_nbi(ib) + power_nbi_trace(ib)
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
    call ccps_argcheck('psget_power_nbi_trace', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % power_nbi_trace
end subroutine psget_power_nbi_trace


subroutine psset_power_nbi_trace(iobj, n1, data, ierr)
    !! power injected as trace element (if any), total power on
    !! beam ib is power_nbi(ib) + power_nbi_trace(ib)
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
    call ccps_argcheck('psset_power_nbi_trace', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % power_nbi_trace = data(1:n1)
end subroutine psset_power_nbi_trace


subroutine psgetrnk_power_nbi_trace(iobj, ndims, ierr)
    !! power injected as trace element (if any), total power on
    !! beam ib is power_nbi(ib) + power_nbi_trace(ib)
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
end subroutine psgetrnk_power_nbi_trace


subroutine psgetdim_power_nbi_trace(iobj, ndims, dims, ierr)
    !! power injected as trace element (if any), total power on
    !! beam ib is power_nbi(ib) + power_nbi_trace(ib)
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
    call ccps_argcheck('psgetdim_power_nbi_trace', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % power_nbi_trace ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % power_nbi_trace)
    endif
end subroutine psgetdim_power_nbi_trace


subroutine psget_phit(iobj, n1, data, ierr)
    !! toroidal flux vs. rho, By convention, this profile always
    !! positive; 0 on axis., See kccw_Bphi for specification of
    !! direction of toroidal field.
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
    call ccps_argcheck('psget_phit', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % phit
end subroutine psget_phit


subroutine psset_phit(iobj, n1, data, ierr)
    !! toroidal flux vs. rho, By convention, this profile always
    !! positive; 0 on axis., See kccw_Bphi for specification of
    !! direction of toroidal field.
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
    call ccps_argcheck('psset_phit', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % phit = data(1:n1)
end subroutine psset_phit


subroutine psgetrnk_phit(iobj, ndims, ierr)
    !! toroidal flux vs. rho, By convention, this profile always
    !! positive; 0 on axis., See kccw_Bphi for specification of
    !! direction of toroidal field.
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
end subroutine psgetrnk_phit


subroutine psgetdim_phit(iobj, ndims, dims, ierr)
    !! toroidal flux vs. rho, By convention, this profile always
    !! positive; 0 on axis., See kccw_Bphi for specification of
    !! direction of toroidal field.
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
    call ccps_argcheck('psgetdim_phit', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % phit ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % phit)
    endif
end subroutine psgetdim_phit


subroutine psget_psirz(iobj, n1, n2, data, ierr)
    !! Psi(R,Z), Note on Psi(rho) and Psi(R,Z) both: convention is
    !! to set, Psi=0 at the magnetic axis and increasing towards the
    !! plasma, boundary.  Direction of poloidal field is taken from
    !! element, kccw_Jphi:, , BR = kccw_Jphi*(1/R)*d(Psi/dZ), BZ
    !! =-kccw_Jphi*(1/R)*d(Psi/dR), , see also: Psi_to_machine_axis.
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
    call ccps_argcheck('psget_psirz', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % PsiRZ
end subroutine psget_psirz


subroutine psset_psirz(iobj, n1, n2, data, ierr)
    !! Psi(R,Z), Note on Psi(rho) and Psi(R,Z) both: convention is
    !! to set, Psi=0 at the magnetic axis and increasing towards the
    !! plasma, boundary.  Direction of poloidal field is taken from
    !! element, kccw_Jphi:, , BR = kccw_Jphi*(1/R)*d(Psi/dZ), BZ
    !! =-kccw_Jphi*(1/R)*d(Psi/dR), , see also: Psi_to_machine_axis.
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
    call ccps_argcheck('psset_psirz', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % PsiRZ = data(1:n1, 1: n2)
end subroutine psset_psirz


subroutine psgetrnk_psirz(iobj, ndims, ierr)
    !! Psi(R,Z), Note on Psi(rho) and Psi(R,Z) both: convention is
    !! to set, Psi=0 at the magnetic axis and increasing towards the
    !! plasma, boundary.  Direction of poloidal field is taken from
    !! element, kccw_Jphi:, , BR = kccw_Jphi*(1/R)*d(Psi/dZ), BZ
    !! =-kccw_Jphi*(1/R)*d(Psi/dR), , see also: Psi_to_machine_axis.
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
end subroutine psgetrnk_psirz


subroutine psgetdim_psirz(iobj, ndims, dims, ierr)
    !! Psi(R,Z), Note on Psi(rho) and Psi(R,Z) both: convention is
    !! to set, Psi=0 at the magnetic axis and increasing towards the
    !! plasma, boundary.  Direction of poloidal field is taken from
    !! element, kccw_Jphi:, , BR = kccw_Jphi*(1/R)*d(Psi/dZ), BZ
    !! =-kccw_Jphi*(1/R)*d(Psi/dR), , see also: Psi_to_machine_axis.
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
    call ccps_argcheck('psgetdim_psirz', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % PsiRZ ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % PsiRZ)
    endif
end subroutine psgetdim_psirz


subroutine psget_prad_li(iobj, n1, data, ierr)
    !! line radiation contribution
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
    call ccps_argcheck('psget_prad_li', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % prad_li
end subroutine psget_prad_li


subroutine psset_prad_li(iobj, n1, data, ierr)
    !! line radiation contribution
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
    call ccps_argcheck('psset_prad_li', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % prad_li = data(1:n1)
end subroutine psset_prad_li


subroutine psgetrnk_prad_li(iobj, ndims, ierr)
    !! line radiation contribution
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
end subroutine psgetrnk_prad_li


subroutine psgetdim_prad_li(iobj, ndims, dims, ierr)
    !! line radiation contribution
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
    call ccps_argcheck('psgetdim_prad_li', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % prad_li ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % prad_li)
    endif
end subroutine psgetdim_prad_li


subroutine psget_pilh(iobj, n1, data, ierr)
    !! ion heating by LH
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
    call ccps_argcheck('psget_pilh', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % pilh
end subroutine psget_pilh


subroutine psset_pilh(iobj, n1, data, ierr)
    !! ion heating by LH
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
    call ccps_argcheck('psset_pilh', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % pilh = data(1:n1)
end subroutine psset_pilh


subroutine psgetrnk_pilh(iobj, ndims, ierr)
    !! ion heating by LH
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
end subroutine psgetrnk_pilh


subroutine psgetdim_pilh(iobj, ndims, dims, ierr)
    !! ion heating by LH
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
    call ccps_argcheck('psgetdim_pilh', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % pilh ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % pilh)
    endif
end subroutine psgetdim_pilh

