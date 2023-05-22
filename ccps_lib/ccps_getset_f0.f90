
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_f0_dummy
     return
  end subroutine ccps_getset_f0_dummy

subroutine psget_fracmin(iobj, n1, data, ierr)
    !! minority density fraction if kdens_rfmin="fraction", if so,
    !! it must have non-zero positive value for each minority ion
    !! specie., sum(fracmin) << 1 is expected.
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
    call ccps_argcheck('psget_fracmin', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % fracmin
end subroutine psget_fracmin


subroutine psset_fracmin(iobj, n1, data, ierr)
    !! minority density fraction if kdens_rfmin="fraction", if so,
    !! it must have non-zero positive value for each minority ion
    !! specie., sum(fracmin) << 1 is expected.
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
    call ccps_argcheck('psset_fracmin', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % fracmin = data(1:n1)
end subroutine psset_fracmin


subroutine psgetrnk_fracmin(iobj, ndims, ierr)
    !! minority density fraction if kdens_rfmin="fraction", if so,
    !! it must have non-zero positive value for each minority ion
    !! specie., sum(fracmin) << 1 is expected.
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
end subroutine psgetrnk_fracmin


subroutine psgetdim_fracmin(iobj, ndims, dims, ierr)
    !! minority density fraction if kdens_rfmin="fraction", if so,
    !! it must have non-zero positive value for each minority ion
    !! specie., sum(fracmin) << 1 is expected.
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
    call ccps_argcheck('psgetdim_fracmin', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % fracmin ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % fracmin)
    endif
end subroutine psgetdim_fracmin

