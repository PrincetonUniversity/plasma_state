
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 2.056
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_b3_dummy
     return
  end subroutine ccps_getset_b3_dummy

subroutine psget_bidiff_dpass_ctr(iobj, n1, n2, n3, data, ierr)
    !! beam ion diffusivity: ctr passing |vpll/v| -> 1
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
    call ccps_argcheck('psget_bidiff_dpass_ctr', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2, 1: n3) = ss % bidiff_dpass_ctr
end subroutine psget_bidiff_dpass_ctr


subroutine psset_bidiff_dpass_ctr(iobj, n1, n2, n3, data, ierr)
    !! beam ion diffusivity: ctr passing |vpll/v| -> 1
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
    call ccps_argcheck('psset_bidiff_dpass_ctr', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % bidiff_dpass_ctr = data(1:n1, 1: n2, 1: n3)
end subroutine psset_bidiff_dpass_ctr


subroutine psgetrnk_bidiff_dpass_ctr(iobj, ndims, ierr)
    !! beam ion diffusivity: ctr passing |vpll/v| -> 1
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
end subroutine psgetrnk_bidiff_dpass_ctr


subroutine psgetdim_bidiff_dpass_ctr(iobj, ndims, dims, ierr)
    !! beam ion diffusivity: ctr passing |vpll/v| -> 1
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
    call ccps_argcheck('psgetdim_bidiff_dpass_ctr', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % bidiff_dpass_ctr ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % bidiff_dpass_ctr)
    endif
end subroutine psgetdim_bidiff_dpass_ctr

