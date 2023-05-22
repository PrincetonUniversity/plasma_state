
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_w5_dummy
     return
  end subroutine ccps_getset_w5_dummy

subroutine psget_wt_nphi_abs(iobj, n1, n2, data, ierr)
    !! absorbed spectrum n_phi weight,
    !! power_ic(iant)*wt_nphi_abs(ind,iant), gives power in toroidal
    !! mode, nphi(ind,iant), "ind" covers range 1:num_nphi(iant)., see
    !! vacuum spectrum wt_nphi(...)
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
    call ccps_argcheck('psget_wt_nphi_abs', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % wt_nphi_abs
end subroutine psget_wt_nphi_abs


subroutine psset_wt_nphi_abs(iobj, n1, n2, data, ierr)
    !! absorbed spectrum n_phi weight,
    !! power_ic(iant)*wt_nphi_abs(ind,iant), gives power in toroidal
    !! mode, nphi(ind,iant), "ind" covers range 1:num_nphi(iant)., see
    !! vacuum spectrum wt_nphi(...)
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
    call ccps_argcheck('psset_wt_nphi_abs', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % wt_nphi_abs = data(1:n1, 1: n2)
end subroutine psset_wt_nphi_abs


subroutine psgetrnk_wt_nphi_abs(iobj, ndims, ierr)
    !! absorbed spectrum n_phi weight,
    !! power_ic(iant)*wt_nphi_abs(ind,iant), gives power in toroidal
    !! mode, nphi(ind,iant), "ind" covers range 1:num_nphi(iant)., see
    !! vacuum spectrum wt_nphi(...)
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
end subroutine psgetrnk_wt_nphi_abs


subroutine psgetdim_wt_nphi_abs(iobj, ndims, dims, ierr)
    !! absorbed spectrum n_phi weight,
    !! power_ic(iant)*wt_nphi_abs(ind,iant), gives power in toroidal
    !! mode, nphi(ind,iant), "ind" covers range 1:num_nphi(iant)., see
    !! vacuum spectrum wt_nphi(...)
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
    call ccps_argcheck('psgetdim_wt_nphi_abs', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % wt_nphi_abs ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % wt_nphi_abs)
    endif
end subroutine psgetdim_wt_nphi_abs

