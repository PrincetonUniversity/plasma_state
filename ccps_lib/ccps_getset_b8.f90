
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_b8_dummy
     return
  end subroutine ccps_getset_b8_dummy

subroutine psget_b_hdivergence(iobj, n1, data, ierr)
    !! horizontal divergence (D) of beam, for nbshape(j)="circle"
    !! sources this is the ONLY divergence., --> definition of D
    !! (b_Hdivergence(...) given in DEGREES):, for alpha the random
    !! angle of deviation from the direction of, aiming, P(alpha) ~
    !! exp(-alpha**2/D**2); P(D) = (1/e)*P(0), I.e. D is the angle of
    !! deviation from the aiming angle, in, either direction, for a
    !! (1/e) reduction in probability of, launch of a beam particle.
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
    call ccps_argcheck('psget_b_hdivergence', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % b_Hdivergence
end subroutine psget_b_hdivergence


subroutine psset_b_hdivergence(iobj, n1, data, ierr)
    !! horizontal divergence (D) of beam, for nbshape(j)="circle"
    !! sources this is the ONLY divergence., --> definition of D
    !! (b_Hdivergence(...) given in DEGREES):, for alpha the random
    !! angle of deviation from the direction of, aiming, P(alpha) ~
    !! exp(-alpha**2/D**2); P(D) = (1/e)*P(0), I.e. D is the angle of
    !! deviation from the aiming angle, in, either direction, for a
    !! (1/e) reduction in probability of, launch of a beam particle.
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
    call ccps_argcheck('psset_b_hdivergence', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % b_Hdivergence = data(1:n1)
end subroutine psset_b_hdivergence


subroutine psgetrnk_b_hdivergence(iobj, ndims, ierr)
    !! horizontal divergence (D) of beam, for nbshape(j)="circle"
    !! sources this is the ONLY divergence., --> definition of D
    !! (b_Hdivergence(...) given in DEGREES):, for alpha the random
    !! angle of deviation from the direction of, aiming, P(alpha) ~
    !! exp(-alpha**2/D**2); P(D) = (1/e)*P(0), I.e. D is the angle of
    !! deviation from the aiming angle, in, either direction, for a
    !! (1/e) reduction in probability of, launch of a beam particle.
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
end subroutine psgetrnk_b_hdivergence


subroutine psgetdim_b_hdivergence(iobj, ndims, dims, ierr)
    !! horizontal divergence (D) of beam, for nbshape(j)="circle"
    !! sources this is the ONLY divergence., --> definition of D
    !! (b_Hdivergence(...) given in DEGREES):, for alpha the random
    !! angle of deviation from the direction of, aiming, P(alpha) ~
    !! exp(-alpha**2/D**2); P(D) = (1/e)*P(0), I.e. D is the angle of
    !! deviation from the aiming angle, in, either direction, for a
    !! (1/e) reduction in probability of, launch of a beam particle.
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
    call ccps_argcheck('psgetdim_b_hdivergence', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % b_Hdivergence ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % b_Hdivergence)
    endif
end subroutine psgetdim_b_hdivergence


subroutine psget_b_min_lcfs(iobj, data, ierr)
    !! |B|_min of last closed flux surface
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
    call ccps_argcheck('psget_b_min_lcfs', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % B_min_lcfs
end subroutine psget_b_min_lcfs


subroutine psset_b_min_lcfs(iobj, data, ierr)
    !! |B|_min of last closed flux surface
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
    call ccps_argcheck('psset_b_min_lcfs', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % B_min_lcfs = data
end subroutine psset_b_min_lcfs


subroutine psget_balloon_status(iobj, n1, data, ierr)
    !! completion status: Ballooning codes, ** see also: balstab
    !! profile, below **
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    INTEGER, intent(out)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_balloon_status', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % Balloon_status
end subroutine psget_balloon_status


subroutine psset_balloon_status(iobj, n1, data, ierr)
    !! completion status: Ballooning codes, ** see also: balstab
    !! profile, below **
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    INTEGER, intent(in)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_balloon_status', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % Balloon_status = data(1:n1)
end subroutine psset_balloon_status


subroutine psgetrnk_balloon_status(iobj, ndims, ierr)
    !! completion status: Ballooning codes, ** see also: balstab
    !! profile, below **
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
end subroutine psgetrnk_balloon_status


subroutine psgetdim_balloon_status(iobj, ndims, dims, ierr)
    !! completion status: Ballooning codes, ** see also: balstab
    !! profile, below **
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
    call ccps_argcheck('psgetdim_balloon_status', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % Balloon_status ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % Balloon_status)
    endif
end subroutine psgetdim_balloon_status


subroutine psget_bphirz(iobj, n1, n2, data, ierr)
    !! Toroidal field
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
    call ccps_argcheck('psget_bphirz', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % BphiRZ
end subroutine psget_bphirz


subroutine psset_bphirz(iobj, n1, n2, data, ierr)
    !! Toroidal field
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
    call ccps_argcheck('psset_bphirz', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % BphiRZ = data(1:n1, 1: n2)
end subroutine psset_bphirz


subroutine psgetrnk_bphirz(iobj, ndims, ierr)
    !! Toroidal field
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
end subroutine psgetrnk_bphirz


subroutine psgetdim_bphirz(iobj, ndims, dims, ierr)
    !! Toroidal field
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
    call ccps_argcheck('psgetdim_bphirz', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % BphiRZ ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % BphiRZ)
    endif
end subroutine psgetdim_bphirz


subroutine psget_bidiff_dtrap(iobj, n1, n2, n3, data, ierr)
    !! beam ion diffusivity: deeply trapped orbits
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1, n2, n3 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(out)  :: data(n1, n2, n3) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_bidiff_dtrap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2, 1: n3) = ss % bidiff_dtrap
end subroutine psget_bidiff_dtrap


subroutine psset_bidiff_dtrap(iobj, n1, n2, n3, data, ierr)
    !! beam ion diffusivity: deeply trapped orbits
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1, n2, n3 ! list of dimensions (if any)
    REAL(KIND=rspec), intent(in)  :: data(n1, n2, n3) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_bidiff_dtrap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % bidiff_dtrap = data(1:n1, 1: n2, 1: n3)
end subroutine psset_bidiff_dtrap


subroutine psgetrnk_bidiff_dtrap(iobj, ndims, ierr)
    !! beam ion diffusivity: deeply trapped orbits
    use plasma_state_mod, only :  plasma_state_container, plasma_state, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(out) :: ndims  ! number of dimensions
    integer, intent(out) :: ierr   ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    ndims = 3
end subroutine psgetrnk_bidiff_dtrap


subroutine psgetdim_bidiff_dtrap(iobj, ndims, dims, ierr)
    !! beam ion diffusivity: deeply trapped orbits
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
    if (ndims /= 3) ierr = 1
    call ccps_argcheck('psgetdim_bidiff_dtrap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % bidiff_dtrap ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % bidiff_dtrap)
    endif
end subroutine psgetdim_bidiff_dtrap

