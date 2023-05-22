
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_z2_dummy
     return
  end subroutine ccps_getset_z2_dummy

subroutine psget_zbap(iobj, n1, data, ierr)
    !! elevation of beam centerline at aperture, At a distance
    !! (Lbscap) from the beam source, the beam centerline, passes
    !! above (+) or below (-) machine midplane as defined by Z=0, in
    !! the equilibrium specification.  If defaulted, the centerline,
    !! is centered on the midplane (Z=0) when it crosses the plane of
    !! the, aperture.  If both Zbap(...) and Zbsc(...) are defaulted,
    !! the, entire beam centerline lies in the midplane., Note that Z
    !! at the beam centerline tangency radius is:, Ztan = Zbsc +
    !! (Lbsctan/Lbscap)*(Zbap-Zbsc)
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
    call ccps_argcheck('psget_zbap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % Zbap
end subroutine psget_zbap


subroutine psset_zbap(iobj, n1, data, ierr)
    !! elevation of beam centerline at aperture, At a distance
    !! (Lbscap) from the beam source, the beam centerline, passes
    !! above (+) or below (-) machine midplane as defined by Z=0, in
    !! the equilibrium specification.  If defaulted, the centerline,
    !! is centered on the midplane (Z=0) when it crosses the plane of
    !! the, aperture.  If both Zbap(...) and Zbsc(...) are defaulted,
    !! the, entire beam centerline lies in the midplane., Note that Z
    !! at the beam centerline tangency radius is:, Ztan = Zbsc +
    !! (Lbsctan/Lbscap)*(Zbap-Zbsc)
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
    call ccps_argcheck('psset_zbap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % Zbap = data(1:n1)
end subroutine psset_zbap


subroutine psgetrnk_zbap(iobj, ndims, ierr)
    !! elevation of beam centerline at aperture, At a distance
    !! (Lbscap) from the beam source, the beam centerline, passes
    !! above (+) or below (-) machine midplane as defined by Z=0, in
    !! the equilibrium specification.  If defaulted, the centerline,
    !! is centered on the midplane (Z=0) when it crosses the plane of
    !! the, aperture.  If both Zbap(...) and Zbsc(...) are defaulted,
    !! the, entire beam centerline lies in the midplane., Note that Z
    !! at the beam centerline tangency radius is:, Ztan = Zbsc +
    !! (Lbsctan/Lbscap)*(Zbap-Zbsc)
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
end subroutine psgetrnk_zbap


subroutine psgetdim_zbap(iobj, ndims, dims, ierr)
    !! elevation of beam centerline at aperture, At a distance
    !! (Lbscap) from the beam source, the beam centerline, passes
    !! above (+) or below (-) machine midplane as defined by Z=0, in
    !! the equilibrium specification.  If defaulted, the centerline,
    !! is centered on the midplane (Z=0) when it crosses the plane of
    !! the, aperture.  If both Zbap(...) and Zbsc(...) are defaulted,
    !! the, entire beam centerline lies in the midplane., Note that Z
    !! at the beam centerline tangency radius is:, Ztan = Zbsc +
    !! (Lbsctan/Lbscap)*(Zbap-Zbsc)
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
    call ccps_argcheck('psgetdim_zbap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % Zbap ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % Zbap)
    endif
end subroutine psgetdim_zbap


subroutine psget_zimp1(iobj, data, ierr)
    !! lower limit on Z of impurity in reduced species list SA;, by
    !! default the lowest available Z impurity is chosen., After the
    !! SA list is defined, this value is reset to, match the lower Z
    !! impurity selected for the SA list.
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(out)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_zimp1', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % Zimp1
end subroutine psget_zimp1


subroutine psset_zimp1(iobj, data, ierr)
    !! lower limit on Z of impurity in reduced species list SA;, by
    !! default the lowest available Z impurity is chosen., After the
    !! SA list is defined, this value is reset to, match the lower Z
    !! impurity selected for the SA list.
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(in)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_zimp1', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % Zimp1 = data
end subroutine psset_zimp1


subroutine psget_z0_momeq(iobj, n1, data, ierr)
    !! Z0 of flux surface, Fourier Spline representation
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
    call ccps_argcheck('psget_z0_momeq', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % Z0_momeq
end subroutine psget_z0_momeq


subroutine psset_z0_momeq(iobj, n1, data, ierr)
    !! Z0 of flux surface, Fourier Spline representation
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
    call ccps_argcheck('psset_z0_momeq', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % Z0_momeq = data(1:n1)
end subroutine psset_z0_momeq


subroutine psgetrnk_z0_momeq(iobj, ndims, ierr)
    !! Z0 of flux surface, Fourier Spline representation
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
end subroutine psgetrnk_z0_momeq


subroutine psgetdim_z0_momeq(iobj, ndims, dims, ierr)
    !! Z0 of flux surface, Fourier Spline representation
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
    call ccps_argcheck('psgetdim_z0_momeq', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % Z0_momeq ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % Z0_momeq)
    endif
end subroutine psgetdim_z0_momeq

