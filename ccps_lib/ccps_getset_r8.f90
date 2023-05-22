
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 2.056
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_r8_dummy
     return
  end subroutine ccps_getset_r8_dummy

subroutine psget_rfmin_name(iobj, n1, data, ierr)
    !! ICRF minority species
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
    call ccps_argcheck('psget_rfmin_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % RFMIN_name
end subroutine psget_rfmin_name


subroutine psset_rfmin_name(iobj, n1, data, ierr)
    !! ICRF minority species
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
    call ccps_argcheck('psset_rfmin_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % RFMIN_name = data(1:n1)
end subroutine psset_rfmin_name


subroutine psgetsz_rfmin_name(iobj, size, ierr)
    !! ICRF minority species
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
    call ccps_argcheck('psgetsz_rfmin_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if RFMIN_name is an array)
    size = len(ss % RFMIN_name)
end subroutine psgetsz_rfmin_name


subroutine psgetrnk_rfmin_name(iobj, ndims, ierr)
    !! ICRF minority species
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
end subroutine psgetrnk_rfmin_name


subroutine psgetdim_rfmin_name(iobj, ndims, dims, ierr)
    !! ICRF minority species
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
    call ccps_argcheck('psgetdim_rfmin_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % RFMIN_name ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % RFMIN_name)
    endif
end subroutine psgetdim_rfmin_name


subroutine psget_rad_code_info(iobj, data, ierr)
    !! Information: code implementing RAD component
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    CHARACTER*(*), intent(out)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_rad_code_info', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % RAD_Code_Info
end subroutine psget_rad_code_info


subroutine psset_rad_code_info(iobj, data, ierr)
    !! Information: code implementing RAD component
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    CHARACTER*(*), intent(in)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_rad_code_info', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % RAD_Code_Info = data
end subroutine psset_rad_code_info


subroutine psgetsz_rad_code_info(iobj, size, ierr)
    !! Information: code implementing RAD component
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
    call ccps_argcheck('psgetsz_rad_code_info', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if RAD_Code_Info is an array)
    size = len(ss % RAD_Code_Info)
end subroutine psgetsz_rad_code_info


subroutine psget_rho_eq_geo(iobj, n1, data, ierr)
    !! rho grid (EQ flux surface averages), this grid is built from
    !! rho_eq: nrho_eq_geo = 2*nrho_eq - 1,
    !! rho_eq_geo(1:nrho_eq_geo:2) = rho_eq(1:nrho_eq),
    !! rho_eq_geo(2:nrho_eq_geo:2) =,
    !! (rho_eq(1:nrho_eq-1)+rho_eq(2:nrho_eq))/2, i.e. a doubly fine
    !! grid for metric flux surface averages stored, as piecewise
    !! linear radidal profiles.
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
    call ccps_argcheck('psget_rho_eq_geo', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % rho_eq_geo
end subroutine psget_rho_eq_geo


subroutine psset_rho_eq_geo(iobj, n1, data, ierr)
    !! rho grid (EQ flux surface averages), this grid is built from
    !! rho_eq: nrho_eq_geo = 2*nrho_eq - 1,
    !! rho_eq_geo(1:nrho_eq_geo:2) = rho_eq(1:nrho_eq),
    !! rho_eq_geo(2:nrho_eq_geo:2) =,
    !! (rho_eq(1:nrho_eq-1)+rho_eq(2:nrho_eq))/2, i.e. a doubly fine
    !! grid for metric flux surface averages stored, as piecewise
    !! linear radidal profiles.
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
    call ccps_argcheck('psset_rho_eq_geo', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % rho_eq_geo = data(1:n1)
end subroutine psset_rho_eq_geo


subroutine psgetrnk_rho_eq_geo(iobj, ndims, ierr)
    !! rho grid (EQ flux surface averages), this grid is built from
    !! rho_eq: nrho_eq_geo = 2*nrho_eq - 1,
    !! rho_eq_geo(1:nrho_eq_geo:2) = rho_eq(1:nrho_eq),
    !! rho_eq_geo(2:nrho_eq_geo:2) =,
    !! (rho_eq(1:nrho_eq-1)+rho_eq(2:nrho_eq))/2, i.e. a doubly fine
    !! grid for metric flux surface averages stored, as piecewise
    !! linear radidal profiles.
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
end subroutine psgetrnk_rho_eq_geo


subroutine psgetdim_rho_eq_geo(iobj, ndims, dims, ierr)
    !! rho grid (EQ flux surface averages), this grid is built from
    !! rho_eq: nrho_eq_geo = 2*nrho_eq - 1,
    !! rho_eq_geo(1:nrho_eq_geo:2) = rho_eq(1:nrho_eq),
    !! rho_eq_geo(2:nrho_eq_geo:2) =,
    !! (rho_eq(1:nrho_eq-1)+rho_eq(2:nrho_eq))/2, i.e. a doubly fine
    !! grid for metric flux surface averages stored, as piecewise
    !! linear radidal profiles.
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
    call ccps_argcheck('psgetdim_rho_eq_geo', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % rho_eq_geo ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % rho_eq_geo)
    endif
end subroutine psgetdim_rho_eq_geo


subroutine psget_rho_anom(iobj, n1, data, ierr)
    !! rho grid (1d anomalous transport profiles)
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
    call ccps_argcheck('psget_rho_anom', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % rho_anom
end subroutine psget_rho_anom


subroutine psset_rho_anom(iobj, n1, data, ierr)
    !! rho grid (1d anomalous transport profiles)
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
    call ccps_argcheck('psset_rho_anom', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % rho_anom = data(1:n1)
end subroutine psset_rho_anom


subroutine psgetrnk_rho_anom(iobj, ndims, ierr)
    !! rho grid (1d anomalous transport profiles)
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
end subroutine psgetrnk_rho_anom


subroutine psgetdim_rho_anom(iobj, ndims, dims, ierr)
    !! rho grid (1d anomalous transport profiles)
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
    call ccps_argcheck('psgetdim_rho_anom', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % rho_anom ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % rho_anom)
    endif
end subroutine psgetdim_rho_anom


subroutine psget_res_tq(iobj, n1, data, ierr)
    !! rediduals for angular momentum transport eqn
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
    call ccps_argcheck('psget_res_tq', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % res_tq
end subroutine psget_res_tq


subroutine psset_res_tq(iobj, n1, data, ierr)
    !! rediduals for angular momentum transport eqn
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
    call ccps_argcheck('psset_res_tq', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % res_tq = data(1:n1)
end subroutine psset_res_tq


subroutine psgetrnk_res_tq(iobj, ndims, ierr)
    !! rediduals for angular momentum transport eqn
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
end subroutine psgetrnk_res_tq


subroutine psgetdim_res_tq(iobj, ndims, dims, ierr)
    !! rediduals for angular momentum transport eqn
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
    call ccps_argcheck('psgetdim_res_tq', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % res_tq ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % res_tq)
    endif
end subroutine psgetdim_res_tq


subroutine psget_r_geo(iobj, n1, n2, data, ierr)
    !! flux surfaces R(rho,theta)
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
    call ccps_argcheck('psget_r_geo', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % R_geo
end subroutine psget_r_geo


subroutine psset_r_geo(iobj, n1, n2, data, ierr)
    !! flux surfaces R(rho,theta)
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
    call ccps_argcheck('psset_r_geo', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % R_geo = data(1:n1, 1: n2)
end subroutine psset_r_geo


subroutine psgetrnk_r_geo(iobj, ndims, ierr)
    !! flux surfaces R(rho,theta)
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
end subroutine psgetrnk_r_geo


subroutine psgetdim_r_geo(iobj, ndims, dims, ierr)
    !! flux surfaces R(rho,theta)
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
    call ccps_argcheck('psgetdim_r_geo', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % R_geo ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % R_geo)
    endif
end subroutine psgetdim_r_geo

