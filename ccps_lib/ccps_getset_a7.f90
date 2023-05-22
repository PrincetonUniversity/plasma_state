
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_a7_dummy
     return
  end subroutine ccps_getset_a7_dummy

subroutine psget_ang_href_coil(iobj, n1, data, ierr)
    !! angle w.r.t. horizontal of coil base, zero means, base is
    !! alligned horizontally, +10 (degrees) means, tilted up to the
    !! right;, -10 (degrees) means, tilted down to the right
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
    call ccps_argcheck('psget_ang_href_coil', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % Ang_Href_coil
end subroutine psget_ang_href_coil


subroutine psset_ang_href_coil(iobj, n1, data, ierr)
    !! angle w.r.t. horizontal of coil base, zero means, base is
    !! alligned horizontally, +10 (degrees) means, tilted up to the
    !! right;, -10 (degrees) means, tilted down to the right
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
    call ccps_argcheck('psset_ang_href_coil', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % Ang_Href_coil = data(1:n1)
end subroutine psset_ang_href_coil


subroutine psgetrnk_ang_href_coil(iobj, ndims, ierr)
    !! angle w.r.t. horizontal of coil base, zero means, base is
    !! alligned horizontally, +10 (degrees) means, tilted up to the
    !! right;, -10 (degrees) means, tilted down to the right
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
end subroutine psgetrnk_ang_href_coil


subroutine psgetdim_ang_href_coil(iobj, ndims, dims, ierr)
    !! angle w.r.t. horizontal of coil base, zero means, base is
    !! alligned horizontally, +10 (degrees) means, tilted up to the
    !! right;, -10 (degrees) means, tilted down to the right
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
    call ccps_argcheck('psgetdim_ang_href_coil', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % Ang_Href_coil ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % Ang_Href_coil)
    endif
end subroutine psgetdim_ang_href_coil


subroutine psget_ap2_halfwidth(iobj, n1, data, ierr)
    !! 2nd aperture half-width, for nbap_shape(j)="rectangle", this
    !! is the horizontal size, specification; for
    !! nbap_shape(j)="circle", ap_halfwidth(j) is, the radius; ignored
    !! if there is no 2nd aperture.
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
    call ccps_argcheck('psget_ap2_halfwidth', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % ap2_halfwidth
end subroutine psget_ap2_halfwidth


subroutine psset_ap2_halfwidth(iobj, n1, data, ierr)
    !! 2nd aperture half-width, for nbap_shape(j)="rectangle", this
    !! is the horizontal size, specification; for
    !! nbap_shape(j)="circle", ap_halfwidth(j) is, the radius; ignored
    !! if there is no 2nd aperture.
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
    call ccps_argcheck('psset_ap2_halfwidth', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % ap2_halfwidth = data(1:n1)
end subroutine psset_ap2_halfwidth


subroutine psgetrnk_ap2_halfwidth(iobj, ndims, ierr)
    !! 2nd aperture half-width, for nbap_shape(j)="rectangle", this
    !! is the horizontal size, specification; for
    !! nbap_shape(j)="circle", ap_halfwidth(j) is, the radius; ignored
    !! if there is no 2nd aperture.
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
end subroutine psgetrnk_ap2_halfwidth


subroutine psgetdim_ap2_halfwidth(iobj, ndims, dims, ierr)
    !! 2nd aperture half-width, for nbap_shape(j)="rectangle", this
    !! is the horizontal size, specification; for
    !! nbap_shape(j)="circle", ap_halfwidth(j) is, the radius; ignored
    !! if there is no 2nd aperture.
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
    call ccps_argcheck('psgetdim_ap2_halfwidth', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % ap2_halfwidth ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % ap2_halfwidth)
    endif
end subroutine psgetdim_ap2_halfwidth


subroutine psget_ant_model(iobj, n1, data, ierr)
    !! antenna model filenames (1 per antenna source)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    CHARACTER*(*), intent(out)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_ant_model', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % ant_model
end subroutine psget_ant_model


subroutine psset_ant_model(iobj, n1, data, ierr)
    !! antenna model filenames (1 per antenna source)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    integer, intent(in) :: n1 ! list of dimensions (if any)
    CHARACTER*(*), intent(in)  :: data(n1) ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_ant_model', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % ant_model = data(1:n1)
end subroutine psset_ant_model


subroutine psgetsz_ant_model(iobj, size, ierr)
    !! antenna model filenames (1 per antenna source)
    use plasma_state_mod, only :  plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(out) :: size    ! size of character string
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psgetsz_ant_model', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if ant_model is an array)
    size = len(ss % ant_model)
end subroutine psgetsz_ant_model


subroutine psgetrnk_ant_model(iobj, ndims, ierr)
    !! antenna model filenames (1 per antenna source)
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
end subroutine psgetrnk_ant_model


subroutine psgetdim_ant_model(iobj, ndims, dims, ierr)
    !! antenna model filenames (1 per antenna source)
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
    call ccps_argcheck('psgetdim_ant_model', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % ant_model ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % ant_model)
    endif
end subroutine psgetdim_ant_model

