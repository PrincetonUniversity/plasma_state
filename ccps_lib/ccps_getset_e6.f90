
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 2.056
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_e6_dummy
     return
  end subroutine ccps_getset_e6_dummy

subroutine psget_ee_mobility_factor(iobj, n1, data, ierr)
    !! chi_e/chi_i, ratio of electron heat diffusivity to ion heat
    !! diffusivity, a model input, (does not effect relative
    !! non-diffusive heat pinch velocities).
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
    call ccps_argcheck('psget_ee_mobility_factor', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % ee_mobility_factor
end subroutine psget_ee_mobility_factor


subroutine psset_ee_mobility_factor(iobj, n1, data, ierr)
    !! chi_e/chi_i, ratio of electron heat diffusivity to ion heat
    !! diffusivity, a model input, (does not effect relative
    !! non-diffusive heat pinch velocities).
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
    call ccps_argcheck('psset_ee_mobility_factor', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % ee_mobility_factor = data(1:n1)
end subroutine psset_ee_mobility_factor


subroutine psgetrnk_ee_mobility_factor(iobj, ndims, ierr)
    !! chi_e/chi_i, ratio of electron heat diffusivity to ion heat
    !! diffusivity, a model input, (does not effect relative
    !! non-diffusive heat pinch velocities).
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
end subroutine psgetrnk_ee_mobility_factor


subroutine psgetdim_ee_mobility_factor(iobj, ndims, dims, ierr)
    !! chi_e/chi_i, ratio of electron heat diffusivity to ion heat
    !! diffusivity, a model input, (does not effect relative
    !! non-diffusive heat pinch velocities).
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
    call ccps_argcheck('psgetdim_ee_mobility_factor', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % ee_mobility_factor ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % ee_mobility_factor)
    endif
end subroutine psgetdim_ee_mobility_factor

