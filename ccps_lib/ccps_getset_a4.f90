
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 2.056
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_a4_dummy
     return
  end subroutine ccps_getset_a4_dummy

subroutine psget_ap_vert_offset(iobj, n1, data, ierr)
    !! vertical offset of aperture, opening relative to beam
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
    call ccps_argcheck('psget_ap_vert_offset', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % ap_vert_offset
end subroutine psget_ap_vert_offset


subroutine psset_ap_vert_offset(iobj, n1, data, ierr)
    !! vertical offset of aperture, opening relative to beam
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
    call ccps_argcheck('psset_ap_vert_offset', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % ap_vert_offset = data(1:n1)
end subroutine psset_ap_vert_offset


subroutine psgetrnk_ap_vert_offset(iobj, ndims, ierr)
    !! vertical offset of aperture, opening relative to beam
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
end subroutine psgetrnk_ap_vert_offset


subroutine psgetdim_ap_vert_offset(iobj, ndims, dims, ierr)
    !! vertical offset of aperture, opening relative to beam
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
    call ccps_argcheck('psgetdim_ap_vert_offset', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % ap_vert_offset ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % ap_vert_offset)
    endif
end subroutine psgetdim_ap_vert_offset


subroutine psget_alla_index(iobj, n1, data, ierr)
    !! species index in original lists, alla_index(j)=-1 indicates
    !! that element (j) of the "alla" list is, derived from a
    !! composite of ions in the "s" list (using Zeff, and
    !! quasineutrality equations)., otherwise for thermal ions
    !! alla_index(j) gives the index of the, species in "S".  For
    !! example, if index 2 corresponds to Deuterium, thermal ions in
    !! "alla", then alla_index(2) gives the index in "S", for
    !! Deuterium thermal ions.  For non-thermal ions, ion species j,
    !! corresponds to species index:, alla_index(j) in "SNBI" if
    !! all_type(j) = ps_beam_ion, alla_index(j) in "RFMIN" if
    !! all_type(j) = ps_rf_minority, alla_index(j) in "SFUS" if
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
    call ccps_argcheck('psget_alla_index', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % alla_index
end subroutine psget_alla_index


subroutine psset_alla_index(iobj, n1, data, ierr)
    !! species index in original lists, alla_index(j)=-1 indicates
    !! that element (j) of the "alla" list is, derived from a
    !! composite of ions in the "s" list (using Zeff, and
    !! quasineutrality equations)., otherwise for thermal ions
    !! alla_index(j) gives the index of the, species in "S".  For
    !! example, if index 2 corresponds to Deuterium, thermal ions in
    !! "alla", then alla_index(2) gives the index in "S", for
    !! Deuterium thermal ions.  For non-thermal ions, ion species j,
    !! corresponds to species index:, alla_index(j) in "SNBI" if
    !! all_type(j) = ps_beam_ion, alla_index(j) in "RFMIN" if
    !! all_type(j) = ps_rf_minority, alla_index(j) in "SFUS" if
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
    call ccps_argcheck('psset_alla_index', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % alla_index = data(1:n1)
end subroutine psset_alla_index


subroutine psgetrnk_alla_index(iobj, ndims, ierr)
    !! species index in original lists, alla_index(j)=-1 indicates
    !! that element (j) of the "alla" list is, derived from a
    !! composite of ions in the "s" list (using Zeff, and
    !! quasineutrality equations)., otherwise for thermal ions
    !! alla_index(j) gives the index of the, species in "S".  For
    !! example, if index 2 corresponds to Deuterium, thermal ions in
    !! "alla", then alla_index(2) gives the index in "S", for
    !! Deuterium thermal ions.  For non-thermal ions, ion species j,
    !! corresponds to species index:, alla_index(j) in "SNBI" if
    !! all_type(j) = ps_beam_ion, alla_index(j) in "RFMIN" if
    !! all_type(j) = ps_rf_minority, alla_index(j) in "SFUS" if
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
end subroutine psgetrnk_alla_index


subroutine psgetdim_alla_index(iobj, ndims, dims, ierr)
    !! species index in original lists, alla_index(j)=-1 indicates
    !! that element (j) of the "alla" list is, derived from a
    !! composite of ions in the "s" list (using Zeff, and
    !! quasineutrality equations)., otherwise for thermal ions
    !! alla_index(j) gives the index of the, species in "S".  For
    !! example, if index 2 corresponds to Deuterium, thermal ions in
    !! "alla", then alla_index(2) gives the index in "S", for
    !! Deuterium thermal ions.  For non-thermal ions, ion species j,
    !! corresponds to species index:, alla_index(j) in "SNBI" if
    !! all_type(j) = ps_beam_ion, alla_index(j) in "RFMIN" if
    !! all_type(j) = ps_rf_minority, alla_index(j) in "SFUS" if
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
    call ccps_argcheck('psgetdim_alla_index', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % alla_index ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % alla_index)
    endif
end subroutine psgetdim_alla_index

