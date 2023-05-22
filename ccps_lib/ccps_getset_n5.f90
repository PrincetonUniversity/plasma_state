
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 2.056
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_n5_dummy
     return
  end subroutine ccps_getset_n5_dummy

subroutine psget_nbi_src_name(iobj, n1, data, ierr)
    !! number & name of neutral beams
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
    call ccps_argcheck('psget_nbi_src_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % nbi_src_name
end subroutine psget_nbi_src_name


subroutine psset_nbi_src_name(iobj, n1, data, ierr)
    !! number & name of neutral beams
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
    call ccps_argcheck('psset_nbi_src_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % nbi_src_name = data(1:n1)
end subroutine psset_nbi_src_name


subroutine psgetsz_nbi_src_name(iobj, size, ierr)
    !! number & name of neutral beams
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
    call ccps_argcheck('psgetsz_nbi_src_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if nbi_src_name is an array)
    size = len(ss % nbi_src_name)
end subroutine psgetsz_nbi_src_name


subroutine psgetrnk_nbi_src_name(iobj, ndims, ierr)
    !! number & name of neutral beams
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
end subroutine psgetrnk_nbi_src_name


subroutine psgetdim_nbi_src_name(iobj, ndims, dims, ierr)
    !! number & name of neutral beams
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
    call ccps_argcheck('psgetdim_nbi_src_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % nbi_src_name ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % nbi_src_name)
    endif
end subroutine psgetdim_nbi_src_name


subroutine psget_nbap2_shape(iobj, n1, data, ierr)
    !! keyword for optional 2nd aperture', "For beam j,
    !! nbap2_shape= blank or 'None': no 2nd aperture", 'Other possible
    !! values:  "Rectangle"; "Circle"., The beam centerline is assumed
    !! to pass through the center of the, aperture.
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
    call ccps_argcheck('psget_nbap2_shape', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % nbap2_shape
end subroutine psget_nbap2_shape


subroutine psset_nbap2_shape(iobj, n1, data, ierr)
    !! keyword for optional 2nd aperture', "For beam j,
    !! nbap2_shape= blank or 'None': no 2nd aperture", 'Other possible
    !! values:  "Rectangle"; "Circle"., The beam centerline is assumed
    !! to pass through the center of the, aperture.
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
    call ccps_argcheck('psset_nbap2_shape', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % nbap2_shape = data(1:n1)
end subroutine psset_nbap2_shape


subroutine psgetsz_nbap2_shape(iobj, size, ierr)
    !! keyword for optional 2nd aperture', "For beam j,
    !! nbap2_shape= blank or 'None': no 2nd aperture", 'Other possible
    !! values:  "Rectangle"; "Circle"., The beam centerline is assumed
    !! to pass through the center of the, aperture.
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
    call ccps_argcheck('psgetsz_nbap2_shape', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if nbap2_shape is an array)
    size = len(ss % nbap2_shape)
end subroutine psgetsz_nbap2_shape


subroutine psgetrnk_nbap2_shape(iobj, ndims, ierr)
    !! keyword for optional 2nd aperture', "For beam j,
    !! nbap2_shape= blank or 'None': no 2nd aperture", 'Other possible
    !! values:  "Rectangle"; "Circle"., The beam centerline is assumed
    !! to pass through the center of the, aperture.
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
end subroutine psgetrnk_nbap2_shape


subroutine psgetdim_nbap2_shape(iobj, ndims, dims, ierr)
    !! keyword for optional 2nd aperture', "For beam j,
    !! nbap2_shape= blank or 'None': no 2nd aperture", 'Other possible
    !! values:  "Rectangle"; "Circle"., The beam centerline is assumed
    !! to pass through the center of the, aperture.
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
    call ccps_argcheck('psgetdim_nbap2_shape', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % nbap2_shape ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % nbap2_shape)
    endif
end subroutine psgetdim_nbap2_shape


subroutine psget_n_t_einj_standard(iobj, data, ierr)
    !! no. pts in T beam energy fractions table
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(out)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_n_t_einj_standard', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % n_T_einj_standard
end subroutine psget_n_t_einj_standard


subroutine psset_n_t_einj_standard(iobj, data, ierr)
    !! no. pts in T beam energy fractions table
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(in)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_n_t_einj_standard', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % n_T_einj_standard = data
end subroutine psset_n_t_einj_standard


subroutine psget_nicrf_src(iobj, data, ierr)
    !! item list dimension of icrf_src_name (icrf_source)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(out)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_nicrf_src', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % nicrf_src
end subroutine psget_nicrf_src


subroutine psset_nicrf_src(iobj, data, ierr)
    !! item list dimension of icrf_src_name (icrf_source)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(in)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_nicrf_src', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % nicrf_src = data
end subroutine psset_nicrf_src


subroutine psget_nbion(iobj, n1, data, ierr)
    !! name of species injected by each beam., Possible values are:
    !! "H", "D", "T", "He3", and "He4" at present., DMC Apr 2010:
    !! added: "Ne", "Ar", "Kr", "Xe", Each beam can inject one
    !! species.  Multiple beams (or usually, all beams) can inject the
    !! same species., the SNBI species list will be constructed from
    !! nbion elements;, in many experiments this will consist only of
    !! the single element "D".
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
    call ccps_argcheck('psget_nbion', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % nbion
end subroutine psget_nbion


subroutine psset_nbion(iobj, n1, data, ierr)
    !! name of species injected by each beam., Possible values are:
    !! "H", "D", "T", "He3", and "He4" at present., DMC Apr 2010:
    !! added: "Ne", "Ar", "Kr", "Xe", Each beam can inject one
    !! species.  Multiple beams (or usually, all beams) can inject the
    !! same species., the SNBI species list will be constructed from
    !! nbion elements;, in many experiments this will consist only of
    !! the single element "D".
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
    call ccps_argcheck('psset_nbion', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % nbion = data(1:n1)
end subroutine psset_nbion


subroutine psgetsz_nbion(iobj, size, ierr)
    !! name of species injected by each beam., Possible values are:
    !! "H", "D", "T", "He3", and "He4" at present., DMC Apr 2010:
    !! added: "Ne", "Ar", "Kr", "Xe", Each beam can inject one
    !! species.  Multiple beams (or usually, all beams) can inject the
    !! same species., the SNBI species list will be constructed from
    !! nbion elements;, in many experiments this will consist only of
    !! the single element "D".
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
    call ccps_argcheck('psgetsz_nbion', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if nbion is an array)
    size = len(ss % nbion)
end subroutine psgetsz_nbion


subroutine psgetrnk_nbion(iobj, ndims, ierr)
    !! name of species injected by each beam., Possible values are:
    !! "H", "D", "T", "He3", and "He4" at present., DMC Apr 2010:
    !! added: "Ne", "Ar", "Kr", "Xe", Each beam can inject one
    !! species.  Multiple beams (or usually, all beams) can inject the
    !! same species., the SNBI species list will be constructed from
    !! nbion elements;, in many experiments this will consist only of
    !! the single element "D".
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
end subroutine psgetrnk_nbion


subroutine psgetdim_nbion(iobj, ndims, dims, ierr)
    !! name of species injected by each beam., Possible values are:
    !! "H", "D", "T", "He3", and "He4" at present., DMC Apr 2010:
    !! added: "Ne", "Ar", "Kr", "Xe", Each beam can inject one
    !! species.  Multiple beams (or usually, all beams) can inject the
    !! same species., the SNBI species list will be constructed from
    !! nbion elements;, in many experiments this will consist only of
    !! the single element "D".
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
    call ccps_argcheck('psgetdim_nbion', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % nbion ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % nbion)
    endif
end subroutine psgetdim_nbion


subroutine psget_ngsc0(iobj, data, ierr)
    !! item list dimension of gs_name (gas_source)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(out)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_ngsc0', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % ngsc0
end subroutine psget_ngsc0


subroutine psset_ngsc0(iobj, data, ierr)
    !! item list dimension of gs_name (gas_source)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(in)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_ngsc0', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % ngsc0 = data
end subroutine psset_ngsc0


subroutine psget_nz(iobj, data, ierr)
    !! grid dimension of Z_grid (Z coordinate)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(out)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_nz', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % nz
end subroutine psget_nz


subroutine psset_nz(iobj, data, ierr)
    !! grid dimension of Z_grid (Z coordinate)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(in)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_nz', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % nz = data
end subroutine psset_nz


subroutine psget_nballoon(iobj, data, ierr)
    !! item list dimension of Balloon_code (Balloon_codes)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(out)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_nballoon', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % nballoon
end subroutine psget_nballoon


subroutine psset_nballoon(iobj, data, ierr)
    !! item list dimension of Balloon_code (Balloon_codes)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(in)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_nballoon', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % nballoon = data
end subroutine psset_nballoon


subroutine psget_nrho_anomth(iobj, data, ierr)
    !! grid dimension of rho_anomth (RHO coordinate)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(out)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_nrho_anomth', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % nrho_anomth
end subroutine psget_nrho_anomth


subroutine psset_nrho_anomth(iobj, data, ierr)
    !! grid dimension of rho_anomth (RHO coordinate)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(in)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_nrho_anomth', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % nrho_anomth = data
end subroutine psset_nrho_anomth


subroutine psget_nky_anomth(iobj, data, ierr)
    !! number of points, ky grid ( anomalous data over ky spectrum)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(out)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_nky_anomth', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % nky_anomth
end subroutine psget_nky_anomth


subroutine psset_nky_anomth(iobj, data, ierr)
    !! number of points, ky grid ( anomalous data over ky spectrum)
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(in)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_nky_anomth', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % nky_anomth = data
end subroutine psset_nky_anomth


subroutine psget_nmdifb(iobj, data, ierr)
    !! type of fast ion diffusivity, =4: 2D fast ion diffusivity
    !! from ufiles, =6: fast ion diffusivity from dep code
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(out)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_nmdifb', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % nmdifb
end subroutine psget_nmdifb


subroutine psset_nmdifb(iobj, data, ierr)
    !! type of fast ion diffusivity, =4: 2D fast ion diffusivity
    !! from ufiles, =6: fast ion diffusivity from dep code
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(in)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_nmdifb', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % nmdifb = data
end subroutine psset_nmdifb


subroutine psget_neflux_ky_tglf(iobj, n1, n2, data, ierr)
    !! electron density flux spectrum
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
    call ccps_argcheck('psget_neflux_ky_tglf', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % neflux_ky_tglf
end subroutine psget_neflux_ky_tglf


subroutine psset_neflux_ky_tglf(iobj, n1, n2, data, ierr)
    !! electron density flux spectrum
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
    call ccps_argcheck('psset_neflux_ky_tglf', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % neflux_ky_tglf = data(1:n1, 1: n2)
end subroutine psset_neflux_ky_tglf


subroutine psgetrnk_neflux_ky_tglf(iobj, ndims, ierr)
    !! electron density flux spectrum
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
end subroutine psgetrnk_neflux_ky_tglf


subroutine psgetdim_neflux_ky_tglf(iobj, ndims, dims, ierr)
    !! electron density flux spectrum
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
    call ccps_argcheck('psgetdim_neflux_ky_tglf', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % neflux_ky_tglf ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % neflux_ky_tglf)
    endif
end subroutine psgetdim_neflux_ky_tglf

