
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_e9_dummy
     return
  end subroutine ccps_getset_e9_dummy

subroutine psget_ec_princ_width_vert(iobj, n1, data, ierr)
    !! principal beam width in vertical direction
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
    call ccps_argcheck('psget_ec_princ_width_vert', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % EC_PRINC_WIDTH_VERT
end subroutine psget_ec_princ_width_vert


subroutine psset_ec_princ_width_vert(iobj, n1, data, ierr)
    !! principal beam width in vertical direction
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
    call ccps_argcheck('psset_ec_princ_width_vert', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % EC_PRINC_WIDTH_VERT = data(1:n1)
end subroutine psset_ec_princ_width_vert


subroutine psgetrnk_ec_princ_width_vert(iobj, ndims, ierr)
    !! principal beam width in vertical direction
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
end subroutine psgetrnk_ec_princ_width_vert


subroutine psgetdim_ec_princ_width_vert(iobj, ndims, dims, ierr)
    !! principal beam width in vertical direction
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
    call ccps_argcheck('psgetdim_ec_princ_width_vert', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % EC_PRINC_WIDTH_VERT ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % EC_PRINC_WIDTH_VERT)
    endif
end subroutine psgetdim_ec_princ_width_vert


subroutine psget_e0_av(iobj, n1, data, ierr)
    !! average energy of neutral sources, "(3/2)*T0", [in reference
    !! frame of (vphi0_av)]; some models, impose a minimum value such
    !! as 0.005 KeV.  The, value can effect penetration of neutrals
    !! and, depth of edge-driven charge exchange loss, but, (depending
    !! on plasma parameters) the effect, is often weak.
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
    call ccps_argcheck('psget_e0_av', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % e0_av
end subroutine psget_e0_av


subroutine psset_e0_av(iobj, n1, data, ierr)
    !! average energy of neutral sources, "(3/2)*T0", [in reference
    !! frame of (vphi0_av)]; some models, impose a minimum value such
    !! as 0.005 KeV.  The, value can effect penetration of neutrals
    !! and, depth of edge-driven charge exchange loss, but, (depending
    !! on plasma parameters) the effect, is often weak.
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
    call ccps_argcheck('psset_e0_av', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % e0_av = data(1:n1)
end subroutine psset_e0_av


subroutine psgetrnk_e0_av(iobj, ndims, ierr)
    !! average energy of neutral sources, "(3/2)*T0", [in reference
    !! frame of (vphi0_av)]; some models, impose a minimum value such
    !! as 0.005 KeV.  The, value can effect penetration of neutrals
    !! and, depth of edge-driven charge exchange loss, but, (depending
    !! on plasma parameters) the effect, is often weak.
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
end subroutine psgetrnk_e0_av


subroutine psgetdim_e0_av(iobj, ndims, dims, ierr)
    !! average energy of neutral sources, "(3/2)*T0", [in reference
    !! frame of (vphi0_av)]; some models, impose a minimum value such
    !! as 0.005 KeV.  The, value can effect penetration of neutrals
    !! and, depth of edge-driven charge exchange loss, but, (depending
    !! on plasma parameters) the effect, is often weak.
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
    call ccps_argcheck('psgetdim_e0_av', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % e0_av ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % e0_av)
    endif
end subroutine psgetdim_e0_av

