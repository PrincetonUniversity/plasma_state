
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 2.056
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_a0_dummy
     return
  end subroutine ccps_getset_a0_dummy

subroutine psget_ap2_vert_offset(iobj, n1, data, ierr)
    !! vertical offset, 2nd aperture, opening relative to beam
    !! centerline: positive value means the, aperture opening is
    !! shifted upward relative to the beam centerline
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
    call ccps_argcheck('psget_ap2_vert_offset', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % ap2_vert_offset
end subroutine psget_ap2_vert_offset


subroutine psset_ap2_vert_offset(iobj, n1, data, ierr)
    !! vertical offset, 2nd aperture, opening relative to beam
    !! centerline: positive value means the, aperture opening is
    !! shifted upward relative to the beam centerline
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
    call ccps_argcheck('psset_ap2_vert_offset', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % ap2_vert_offset = data(1:n1)
end subroutine psset_ap2_vert_offset


subroutine psgetrnk_ap2_vert_offset(iobj, ndims, ierr)
    !! vertical offset, 2nd aperture, opening relative to beam
    !! centerline: positive value means the, aperture opening is
    !! shifted upward relative to the beam centerline
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
end subroutine psgetrnk_ap2_vert_offset


subroutine psgetdim_ap2_vert_offset(iobj, ndims, dims, ierr)
    !! vertical offset, 2nd aperture, opening relative to beam
    !! centerline: positive value means the, aperture opening is
    !! shifted upward relative to the beam centerline
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
    call ccps_argcheck('psgetdim_ap2_vert_offset', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % ap2_vert_offset ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % ap2_vert_offset)
    endif
end subroutine psgetdim_ap2_vert_offset


subroutine psget_atorminlh(iobj, n1, n2, data, ierr)
    !! toroidal mode number left of lumps
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
    call ccps_argcheck('psget_atorminlh', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % atorminlh
end subroutine psget_atorminlh


subroutine psset_atorminlh(iobj, n1, n2, data, ierr)
    !! toroidal mode number left of lumps
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
    call ccps_argcheck('psset_atorminlh', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % atorminlh = data(1:n1, 1: n2)
end subroutine psset_atorminlh


subroutine psgetrnk_atorminlh(iobj, ndims, ierr)
    !! toroidal mode number left of lumps
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
end subroutine psgetrnk_atorminlh


subroutine psgetdim_atorminlh(iobj, ndims, dims, ierr)
    !! toroidal mode number left of lumps
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
    call ccps_argcheck('psgetdim_atorminlh', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % atorminlh ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % atorminlh)
    endif
end subroutine psgetdim_atorminlh


subroutine psget_all_index(iobj, n1, data, ierr)
    !! species index in original lists, ion species j in "all"
    !! corresponds to ion species:, all_index(j) in "s" if all_type(j)
    !! = ps_electron, ps_therm_ion, or, ps_impurity, all_index(j) in
    !! "snbi" if all_type(j) = ps_beam_ion, all_index(j) in "rfmin" if
    !! all_type(j) = ps_rf_minority, all_index(j) in "sfus" if
    !! all_type(j) = ps_fusion_ion
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    INTEGER, intent(out)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_all_index', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % all_index
end subroutine psget_all_index


subroutine psset_all_index(iobj, n1, data, ierr)
    !! species index in original lists, ion species j in "all"
    !! corresponds to ion species:, all_index(j) in "s" if all_type(j)
    !! = ps_electron, ps_therm_ion, or, ps_impurity, all_index(j) in
    !! "snbi" if all_type(j) = ps_beam_ion, all_index(j) in "rfmin" if
    !! all_type(j) = ps_rf_minority, all_index(j) in "sfus" if
    !! all_type(j) = ps_fusion_ion
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    INTEGER, intent(in)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_all_index', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % all_index = data(1:n1)
end subroutine psset_all_index


subroutine psgetrnk_all_index(iobj, ndims, ierr)
    !! species index in original lists, ion species j in "all"
    !! corresponds to ion species:, all_index(j) in "s" if all_type(j)
    !! = ps_electron, ps_therm_ion, or, ps_impurity, all_index(j) in
    !! "snbi" if all_type(j) = ps_beam_ion, all_index(j) in "rfmin" if
    !! all_type(j) = ps_rf_minority, all_index(j) in "sfus" if
    !! all_type(j) = ps_fusion_ion
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
end subroutine psgetrnk_all_index


subroutine psgetdim_all_index(iobj, ndims, dims, ierr)
    !! species index in original lists, ion species j in "all"
    !! corresponds to ion species:, all_index(j) in "s" if all_type(j)
    !! = ps_electron, ps_therm_ion, or, ps_impurity, all_index(j) in
    !! "snbi" if all_type(j) = ps_beam_ion, all_index(j) in "rfmin" if
    !! all_type(j) = ps_rf_minority, all_index(j) in "sfus" if
    !! all_type(j) = ps_fusion_ion
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
    call ccps_argcheck('psgetdim_all_index', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % all_index ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % all_index)
    endif
end subroutine psgetdim_all_index

