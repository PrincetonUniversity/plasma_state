
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 2.056
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_c8_dummy
     return
  end subroutine ccps_getset_c8_dummy

subroutine psget_curr_ohmic(iobj, n1, data, ierr)
    !! Ohmic current, i.e. inductive current driven by toroidal
    !! loop voltage, in the toroidal direction, Amps/zone:  sum of:,
    !! curr_ohmic + curr_bootstrap + RF driven currents +, fast ion
    !! driven currents + runaway electron current +, diamagnetic
    !! current => curt (total enclosed toroidal, current) in the EQ
    !! component.
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
    call ccps_argcheck('psget_curr_ohmic', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % curr_ohmic
end subroutine psget_curr_ohmic


subroutine psset_curr_ohmic(iobj, n1, data, ierr)
    !! Ohmic current, i.e. inductive current driven by toroidal
    !! loop voltage, in the toroidal direction, Amps/zone:  sum of:,
    !! curr_ohmic + curr_bootstrap + RF driven currents +, fast ion
    !! driven currents + runaway electron current +, diamagnetic
    !! current => curt (total enclosed toroidal, current) in the EQ
    !! component.
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
    call ccps_argcheck('psset_curr_ohmic', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % curr_ohmic = data(1:n1)
end subroutine psset_curr_ohmic


subroutine psgetrnk_curr_ohmic(iobj, ndims, ierr)
    !! Ohmic current, i.e. inductive current driven by toroidal
    !! loop voltage, in the toroidal direction, Amps/zone:  sum of:,
    !! curr_ohmic + curr_bootstrap + RF driven currents +, fast ion
    !! driven currents + runaway electron current +, diamagnetic
    !! current => curt (total enclosed toroidal, current) in the EQ
    !! component.
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
end subroutine psgetrnk_curr_ohmic


subroutine psgetdim_curr_ohmic(iobj, ndims, dims, ierr)
    !! Ohmic current, i.e. inductive current driven by toroidal
    !! loop voltage, in the toroidal direction, Amps/zone:  sum of:,
    !! curr_ohmic + curr_bootstrap + RF driven currents +, fast ion
    !! driven currents + runaway electron current +, diamagnetic
    !! current => curt (total enclosed toroidal, current) in the EQ
    !! component.
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
    call ccps_argcheck('psgetdim_curr_ohmic', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % curr_ohmic ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % curr_ohmic)
    endif
end subroutine psgetdim_curr_ohmic


subroutine psget_curich(iobj, n1, data, ierr)
    !! total driven current (all IC srcs)
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
    call ccps_argcheck('psget_curich', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % curich
end subroutine psget_curich


subroutine psset_curich(iobj, n1, data, ierr)
    !! total driven current (all IC srcs)
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
    call ccps_argcheck('psset_curich', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % curich = data(1:n1)
end subroutine psset_curich


subroutine psgetrnk_curich(iobj, ndims, ierr)
    !! total driven current (all IC srcs)
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
end subroutine psgetrnk_curich


subroutine psgetdim_curich(iobj, ndims, dims, ierr)
    !! total driven current (all IC srcs)
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
    call ccps_argcheck('psgetdim_curich', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % curich ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % curich)
    endif
end subroutine psgetdim_curich

