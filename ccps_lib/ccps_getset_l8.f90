
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 2.056
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_l8_dummy
     return
  end subroutine ccps_getset_l8_dummy

subroutine psget_lbsctan(iobj, n1, data, ierr)
    !! distance, source to tangency point, This is the length of
    !! the centerline from the center of the neutral', "beam's
    !! original ion source to the centerline tangency point,
    !! |sRtcen|", 'from the machine axis.  The length is given in
    !! meters.
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
    call ccps_argcheck('psget_lbsctan', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % Lbsctan
end subroutine psget_lbsctan


subroutine psset_lbsctan(iobj, n1, data, ierr)
    !! distance, source to tangency point, This is the length of
    !! the centerline from the center of the neutral', "beam's
    !! original ion source to the centerline tangency point,
    !! |sRtcen|", 'from the machine axis.  The length is given in
    !! meters.
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
    call ccps_argcheck('psset_lbsctan', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % Lbsctan = data(1:n1)
end subroutine psset_lbsctan


subroutine psgetrnk_lbsctan(iobj, ndims, ierr)
    !! distance, source to tangency point, This is the length of
    !! the centerline from the center of the neutral', "beam's
    !! original ion source to the centerline tangency point,
    !! |sRtcen|", 'from the machine axis.  The length is given in
    !! meters.
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
end subroutine psgetrnk_lbsctan


subroutine psgetdim_lbsctan(iobj, ndims, dims, ierr)
    !! distance, source to tangency point, This is the length of
    !! the centerline from the center of the neutral', "beam's
    !! original ion source to the centerline tangency point,
    !! |sRtcen|", 'from the machine axis.  The length is given in
    !! meters.
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
    call ccps_argcheck('psgetdim_lbsctan', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % Lbsctan ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % Lbsctan)
    endif
end subroutine psgetdim_lbsctan

