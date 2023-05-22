
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 2.056
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_b4_dummy
     return
  end subroutine ccps_getset_b4_dummy

subroutine psget_b_halfwidth(iobj, n1, data, ierr)
    !! beam half-width at source, If nbshape(j)="Rectangle" this is
    !! the horizontal half-width of, the source rectangle for beam j;
    !! if nbshape(j)="circle" it is, the radius of the circular source
    !! for beam j, in meters.
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
    call ccps_argcheck('psget_b_halfwidth', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % b_halfwidth
end subroutine psget_b_halfwidth


subroutine psset_b_halfwidth(iobj, n1, data, ierr)
    !! beam half-width at source, If nbshape(j)="Rectangle" this is
    !! the horizontal half-width of, the source rectangle for beam j;
    !! if nbshape(j)="circle" it is, the radius of the circular source
    !! for beam j, in meters.
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
    call ccps_argcheck('psset_b_halfwidth', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % b_halfwidth = data(1:n1)
end subroutine psset_b_halfwidth


subroutine psgetrnk_b_halfwidth(iobj, ndims, ierr)
    !! beam half-width at source, If nbshape(j)="Rectangle" this is
    !! the horizontal half-width of, the source rectangle for beam j;
    !! if nbshape(j)="circle" it is, the radius of the circular source
    !! for beam j, in meters.
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
end subroutine psgetrnk_b_halfwidth


subroutine psgetdim_b_halfwidth(iobj, ndims, dims, ierr)
    !! beam half-width at source, If nbshape(j)="Rectangle" this is
    !! the horizontal half-width of, the source rectangle for beam j;
    !! if nbshape(j)="circle" it is, the radius of the circular source
    !! for beam j, in meters.
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
    call ccps_argcheck('psgetdim_b_halfwidth', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % b_halfwidth ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % b_halfwidth)
    endif
end subroutine psgetdim_b_halfwidth

