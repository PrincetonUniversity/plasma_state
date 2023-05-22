
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_s7_dummy
     return
  end subroutine ccps_getset_s7_dummy

subroutine psget_srtcen(iobj, n1, data, ierr)
    !! signed tangency radius, This is the major radius of tangency
    !! of the neutral beam centerlines--, i.e. the distance of closest
    !! approach of each beam centerline to the, machine axis, in
    !! meters., Sign convention: + for beams which impart angular
    !! momentum in the, counter-clockwise direction around the torus
    !! as viewed from above.
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
    call ccps_argcheck('psget_srtcen', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % sRtcen
end subroutine psget_srtcen


subroutine psset_srtcen(iobj, n1, data, ierr)
    !! signed tangency radius, This is the major radius of tangency
    !! of the neutral beam centerlines--, i.e. the distance of closest
    !! approach of each beam centerline to the, machine axis, in
    !! meters., Sign convention: + for beams which impart angular
    !! momentum in the, counter-clockwise direction around the torus
    !! as viewed from above.
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
    call ccps_argcheck('psset_srtcen', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % sRtcen = data(1:n1)
end subroutine psset_srtcen


subroutine psgetrnk_srtcen(iobj, ndims, ierr)
    !! signed tangency radius, This is the major radius of tangency
    !! of the neutral beam centerlines--, i.e. the distance of closest
    !! approach of each beam centerline to the, machine axis, in
    !! meters., Sign convention: + for beams which impart angular
    !! momentum in the, counter-clockwise direction around the torus
    !! as viewed from above.
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
end subroutine psgetrnk_srtcen


subroutine psgetdim_srtcen(iobj, ndims, dims, ierr)
    !! signed tangency radius, This is the major radius of tangency
    !! of the neutral beam centerlines--, i.e. the distance of closest
    !! approach of each beam centerline to the, machine axis, in
    !! meters., Sign convention: + for beams which impart angular
    !! momentum in the, counter-clockwise direction around the torus
    !! as viewed from above.
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
    call ccps_argcheck('psgetdim_srtcen', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % sRtcen ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % sRtcen)
    endif
end subroutine psgetdim_srtcen


subroutine psget_sa_index(iobj, n1, data, ierr)
    !! species index in original lists, sa_index(j)=-1 indicates
    !! that element (j) of the "SA" list is, derived from a composite
    !! of ions in the "S" list (using Zeff, and quasineutrality
    !! equations)., otherwise sa_index(j) gives the index of the
    !! species in "S", For example, if index 2 corresponds to
    !! Deuterium ions in "SA", then, sa_index(2) gives the index in
    !! "S" for Deuterium ions., For non-impurities usually
    !! sa_index(j)=j but this could be altered, if He3+1 or He4+1
    !! (less than fully stripped He ions) are present in, the model.
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
    call ccps_argcheck('psget_sa_index', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % sa_index
end subroutine psget_sa_index


subroutine psset_sa_index(iobj, n1, data, ierr)
    !! species index in original lists, sa_index(j)=-1 indicates
    !! that element (j) of the "SA" list is, derived from a composite
    !! of ions in the "S" list (using Zeff, and quasineutrality
    !! equations)., otherwise sa_index(j) gives the index of the
    !! species in "S", For example, if index 2 corresponds to
    !! Deuterium ions in "SA", then, sa_index(2) gives the index in
    !! "S" for Deuterium ions., For non-impurities usually
    !! sa_index(j)=j but this could be altered, if He3+1 or He4+1
    !! (less than fully stripped He ions) are present in, the model.
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
    call ccps_argcheck('psset_sa_index', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % sa_index = data(1:n1)
end subroutine psset_sa_index


subroutine psgetrnk_sa_index(iobj, ndims, ierr)
    !! species index in original lists, sa_index(j)=-1 indicates
    !! that element (j) of the "SA" list is, derived from a composite
    !! of ions in the "S" list (using Zeff, and quasineutrality
    !! equations)., otherwise sa_index(j) gives the index of the
    !! species in "S", For example, if index 2 corresponds to
    !! Deuterium ions in "SA", then, sa_index(2) gives the index in
    !! "S" for Deuterium ions., For non-impurities usually
    !! sa_index(j)=j but this could be altered, if He3+1 or He4+1
    !! (less than fully stripped He ions) are present in, the model.
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
end subroutine psgetrnk_sa_index


subroutine psgetdim_sa_index(iobj, ndims, dims, ierr)
    !! species index in original lists, sa_index(j)=-1 indicates
    !! that element (j) of the "SA" list is, derived from a composite
    !! of ions in the "S" list (using Zeff, and quasineutrality
    !! equations)., otherwise sa_index(j) gives the index of the
    !! species in "S", For example, if index 2 corresponds to
    !! Deuterium ions in "SA", then, sa_index(2) gives the index in
    !! "S" for Deuterium ions., For non-impurities usually
    !! sa_index(j)=j but this could be altered, if He3+1 or He4+1
    !! (less than fully stripped He ions) are present in, the model.
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
    call ccps_argcheck('psgetdim_sa_index', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % sa_index ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % sa_index)
    endif
end subroutine psgetdim_sa_index


subroutine psget_snbi_to_alla(iobj, n1, data, ierr)
    !! map snbi index to "alla" list
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
    call ccps_argcheck('psget_snbi_to_alla', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % snbi_to_alla
end subroutine psget_snbi_to_alla


subroutine psset_snbi_to_alla(iobj, n1, data, ierr)
    !! map snbi index to "alla" list
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
    call ccps_argcheck('psset_snbi_to_alla', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % snbi_to_alla = data(1:n1)
end subroutine psset_snbi_to_alla


subroutine psgetrnk_snbi_to_alla(iobj, ndims, ierr)
    !! map snbi index to "alla" list
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
end subroutine psgetrnk_snbi_to_alla


subroutine psgetdim_snbi_to_alla(iobj, ndims, dims, ierr)
    !! map snbi index to "alla" list
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
    call ccps_argcheck('psgetdim_snbi_to_alla', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % snbi_to_alla ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % snbi_to_alla)
    endif
end subroutine psgetdim_snbi_to_alla


subroutine psget_simpi_name(iobj, n1, data, ierr)
    !! impurity ion species in simulation
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
    call ccps_argcheck('psget_simpi_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % SIMPI_name
end subroutine psget_simpi_name


subroutine psset_simpi_name(iobj, n1, data, ierr)
    !! impurity ion species in simulation
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
    call ccps_argcheck('psset_simpi_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % SIMPI_name = data(1:n1)
end subroutine psset_simpi_name


subroutine psgetsz_simpi_name(iobj, size, ierr)
    !! impurity ion species in simulation
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
    call ccps_argcheck('psgetsz_simpi_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if SIMPI_name is an array)
    size = len(ss % SIMPI_name)
end subroutine psgetsz_simpi_name


subroutine psgetrnk_simpi_name(iobj, ndims, ierr)
    !! impurity ion species in simulation
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
end subroutine psgetrnk_simpi_name


subroutine psgetdim_simpi_name(iobj, ndims, dims, ierr)
    !! impurity ion species in simulation
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
    call ccps_argcheck('psgetdim_simpi_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % SIMPI_name ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % SIMPI_name)
    endif
end subroutine psgetdim_simpi_name


subroutine psget_simp0_type(iobj, n1, data, ierr)
    !! SIMP0 specie types
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
    call ccps_argcheck('psget_simp0_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % SIMP0_type
end subroutine psget_simp0_type


subroutine psset_simp0_type(iobj, n1, data, ierr)
    !! SIMP0 specie types
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
    call ccps_argcheck('psset_simp0_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % SIMP0_type = data(1:n1)
end subroutine psset_simp0_type


subroutine psgetrnk_simp0_type(iobj, ndims, ierr)
    !! SIMP0 specie types
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
end subroutine psgetrnk_simp0_type


subroutine psgetdim_simp0_type(iobj, ndims, dims, ierr)
    !! SIMP0 specie types
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
    call ccps_argcheck('psgetdim_simp0_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % SIMP0_type ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % SIMP0_type)
    endif
end subroutine psgetdim_simp0_type


subroutine psget_sn_transcat(iobj, n1, n2, n3, data, ierr)
    !! species transport
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
    call ccps_argcheck('psget_sn_transcat', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2, 1: n3) = ss % sn_transcat
end subroutine psget_sn_transcat


subroutine psset_sn_transcat(iobj, n1, n2, n3, data, ierr)
    !! species transport
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
    call ccps_argcheck('psset_sn_transcat', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % sn_transcat = data(1:n1, 1: n2, 1: n3)
end subroutine psset_sn_transcat


subroutine psgetrnk_sn_transcat(iobj, ndims, ierr)
    !! species transport
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
end subroutine psgetrnk_sn_transcat


subroutine psgetdim_sn_transcat(iobj, ndims, dims, ierr)
    !! species transport
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
    call ccps_argcheck('psgetdim_sn_transcat', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % sn_transcat ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % sn_transcat)
    endif
end subroutine psgetdim_sn_transcat


subroutine psget_sbsce(iobj, n1, n2, data, ierr)
    !! net source, thermal ions & electrons, ...cold electron
    !! deposition; ion thermalization;, corrections for charge
    !! exchange (beam deposition halo, mainly),, halo recapture, for
    !! thermal ions:, sbsce = sbtherm - sb0halo + sb0halo_recap,
    !! (caution: sbtherm is defined over beam species list;, sbsce
    !! over thermal species list;, sb0halo & sb0halo_recap over the
    !! neutral gas species list)., for electrons:, sbsce = sbedep +
    !! sbehalo
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
    call ccps_argcheck('psget_sbsce', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % sbsce
end subroutine psget_sbsce


subroutine psset_sbsce(iobj, n1, n2, data, ierr)
    !! net source, thermal ions & electrons, ...cold electron
    !! deposition; ion thermalization;, corrections for charge
    !! exchange (beam deposition halo, mainly),, halo recapture, for
    !! thermal ions:, sbsce = sbtherm - sb0halo + sb0halo_recap,
    !! (caution: sbtherm is defined over beam species list;, sbsce
    !! over thermal species list;, sb0halo & sb0halo_recap over the
    !! neutral gas species list)., for electrons:, sbsce = sbedep +
    !! sbehalo
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
    call ccps_argcheck('psset_sbsce', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % sbsce = data(1:n1, 1: n2)
end subroutine psset_sbsce


subroutine psgetrnk_sbsce(iobj, ndims, ierr)
    !! net source, thermal ions & electrons, ...cold electron
    !! deposition; ion thermalization;, corrections for charge
    !! exchange (beam deposition halo, mainly),, halo recapture, for
    !! thermal ions:, sbsce = sbtherm - sb0halo + sb0halo_recap,
    !! (caution: sbtherm is defined over beam species list;, sbsce
    !! over thermal species list;, sb0halo & sb0halo_recap over the
    !! neutral gas species list)., for electrons:, sbsce = sbedep +
    !! sbehalo
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
end subroutine psgetrnk_sbsce


subroutine psgetdim_sbsce(iobj, ndims, dims, ierr)
    !! net source, thermal ions & electrons, ...cold electron
    !! deposition; ion thermalization;, corrections for charge
    !! exchange (beam deposition halo, mainly),, halo recapture, for
    !! thermal ions:, sbsce = sbtherm - sb0halo + sb0halo_recap,
    !! (caution: sbtherm is defined over beam species list;, sbsce
    !! over thermal species list;, sb0halo & sb0halo_recap over the
    !! neutral gas species list)., for electrons:, sbsce = sbedep +
    !! sbehalo
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
    call ccps_argcheck('psgetdim_sbsce', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % sbsce ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % sbsce)
    endif
end subroutine psgetdim_sbsce


subroutine psget_sc_anom(iobj, n1, n2, data, ierr)
    !! anomalous sources
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
    call ccps_argcheck('psget_sc_anom', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % sc_anom
end subroutine psget_sc_anom


subroutine psset_sc_anom(iobj, n1, n2, data, ierr)
    !! anomalous sources
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
    call ccps_argcheck('psset_sc_anom', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % sc_anom = data(1:n1, 1: n2)
end subroutine psset_sc_anom


subroutine psgetrnk_sc_anom(iobj, ndims, ierr)
    !! anomalous sources
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
end subroutine psgetrnk_sc_anom


subroutine psgetdim_sc_anom(iobj, ndims, dims, ierr)
    !! anomalous sources
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
    call ccps_argcheck('psgetdim_sc_anom', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % sc_anom ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % sc_anom)
    endif
end subroutine psgetdim_sc_anom

