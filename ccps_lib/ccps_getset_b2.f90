
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_b2_dummy
     return
  end subroutine ccps_getset_b2_dummy

subroutine psget_balstab(iobj, n1, data, ierr)
    !! Ballooning/Mercier stability indicator, = 0.0: stable;, =
    !! 1.0: Ballooning unstable;, = 2.0: Mercier unstable, = 3.0: Both
    !! Ballooning and Mercier unstable, (note a floating point profile
    !! is used; interpolation to another, grid would yield non-integer
    !! values that if non-zero would indicate, instability in the
    !! vicinity).
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
    call ccps_argcheck('psget_balstab', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % balstab
end subroutine psget_balstab


subroutine psset_balstab(iobj, n1, data, ierr)
    !! Ballooning/Mercier stability indicator, = 0.0: stable;, =
    !! 1.0: Ballooning unstable;, = 2.0: Mercier unstable, = 3.0: Both
    !! Ballooning and Mercier unstable, (note a floating point profile
    !! is used; interpolation to another, grid would yield non-integer
    !! values that if non-zero would indicate, instability in the
    !! vicinity).
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
    call ccps_argcheck('psset_balstab', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % balstab = data(1:n1)
end subroutine psset_balstab


subroutine psgetrnk_balstab(iobj, ndims, ierr)
    !! Ballooning/Mercier stability indicator, = 0.0: stable;, =
    !! 1.0: Ballooning unstable;, = 2.0: Mercier unstable, = 3.0: Both
    !! Ballooning and Mercier unstable, (note a floating point profile
    !! is used; interpolation to another, grid would yield non-integer
    !! values that if non-zero would indicate, instability in the
    !! vicinity).
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
end subroutine psgetrnk_balstab


subroutine psgetdim_balstab(iobj, ndims, dims, ierr)
    !! Ballooning/Mercier stability indicator, = 0.0: stable;, =
    !! 1.0: Ballooning unstable;, = 2.0: Mercier unstable, = 3.0: Both
    !! Ballooning and Mercier unstable, (note a floating point profile
    !! is used; interpolation to another, grid would yield non-integer
    !! values that if non-zero would indicate, instability in the
    !! vicinity).
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
    call ccps_argcheck('psgetdim_balstab', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % balstab ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % balstab)
    endif
end subroutine psgetdim_balstab


subroutine psget_bidiff_bpass_ctr(iobj, n1, n2, n3, data, ierr)
    !! beam ion diffusivity: ctr barely passing orbits
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
    call ccps_argcheck('psget_bidiff_bpass_ctr', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2, 1: n3) = ss % bidiff_bpass_ctr
end subroutine psget_bidiff_bpass_ctr


subroutine psset_bidiff_bpass_ctr(iobj, n1, n2, n3, data, ierr)
    !! beam ion diffusivity: ctr barely passing orbits
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
    call ccps_argcheck('psset_bidiff_bpass_ctr', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % bidiff_bpass_ctr = data(1:n1, 1: n2, 1: n3)
end subroutine psset_bidiff_bpass_ctr


subroutine psgetrnk_bidiff_bpass_ctr(iobj, ndims, ierr)
    !! beam ion diffusivity: ctr barely passing orbits
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
end subroutine psgetrnk_bidiff_bpass_ctr


subroutine psgetdim_bidiff_bpass_ctr(iobj, ndims, dims, ierr)
    !! beam ion diffusivity: ctr barely passing orbits
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
    call ccps_argcheck('psgetdim_bidiff_bpass_ctr', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % bidiff_bpass_ctr ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % bidiff_bpass_ctr)
    endif
end subroutine psgetdim_bidiff_bpass_ctr

