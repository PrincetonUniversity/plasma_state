
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_s5_dummy
     return
  end subroutine ccps_getset_s5_dummy

subroutine psget_snbi_type(iobj, n1, data, ierr)
    !! SNBI specie types
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
    call ccps_argcheck('psget_snbi_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % SNBI_type
end subroutine psget_snbi_type


subroutine psset_snbi_type(iobj, n1, data, ierr)
    !! SNBI specie types
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
    call ccps_argcheck('psset_snbi_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % SNBI_type = data(1:n1)
end subroutine psset_snbi_type


subroutine psgetrnk_snbi_type(iobj, ndims, ierr)
    !! SNBI specie types
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
end subroutine psgetrnk_snbi_type


subroutine psgetdim_snbi_type(iobj, ndims, dims, ierr)
    !! SNBI specie types
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
    call ccps_argcheck('psgetdim_snbi_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % SNBI_type ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % SNBI_type)
    endif
end subroutine psgetdim_snbi_type


subroutine psget_s_to_sa(iobj, n1, data, ierr)
    !! address in sa species list, if species s(j) is in list SA,
    !! s_to_sa(j) gives its address in SA., if species s(j) is NOT in
    !! list SA, s_to_sa(j)=-1.
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
    call ccps_argcheck('psget_s_to_sa', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % s_to_sa
end subroutine psget_s_to_sa


subroutine psset_s_to_sa(iobj, n1, data, ierr)
    !! address in sa species list, if species s(j) is in list SA,
    !! s_to_sa(j) gives its address in SA., if species s(j) is NOT in
    !! list SA, s_to_sa(j)=-1.
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
    call ccps_argcheck('psset_s_to_sa', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % s_to_sa = data(1:n1)
end subroutine psset_s_to_sa


subroutine psgetrnk_s_to_sa(iobj, ndims, ierr)
    !! address in sa species list, if species s(j) is in list SA,
    !! s_to_sa(j) gives its address in SA., if species s(j) is NOT in
    !! list SA, s_to_sa(j)=-1.
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
end subroutine psgetrnk_s_to_sa


subroutine psgetdim_s_to_sa(iobj, ndims, dims, ierr)
    !! address in sa species list, if species s(j) is in list SA,
    !! s_to_sa(j) gives its address in SA., if species s(j) is NOT in
    !! list SA, s_to_sa(j)=-1.
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
    call ccps_argcheck('psgetdim_s_to_sa', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % s_to_sa ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % s_to_sa)
    endif
end subroutine psgetdim_s_to_sa


subroutine psget_sgas_to_s(iobj, n1, data, ierr)
    !! map sgas index to "s" list
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
    call ccps_argcheck('psget_sgas_to_s', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % sgas_to_s
end subroutine psget_sgas_to_s


subroutine psset_sgas_to_s(iobj, n1, data, ierr)
    !! map sgas index to "s" list
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
    call ccps_argcheck('psset_sgas_to_s', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % sgas_to_s = data(1:n1)
end subroutine psset_sgas_to_s


subroutine psgetrnk_sgas_to_s(iobj, ndims, ierr)
    !! map sgas index to "s" list
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
end subroutine psgetrnk_sgas_to_s


subroutine psgetdim_sgas_to_s(iobj, ndims, dims, ierr)
    !! map sgas index to "s" list
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
    call ccps_argcheck('psgetdim_sgas_to_s', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % sgas_to_s ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % sgas_to_s)
    endif
end subroutine psgetdim_sgas_to_s


subroutine psget_sprof0(iobj, n1, n2, n3, data, ierr)
    !! gas species source/sinks per unit species influx, due to
    !! charge exchange, single species neutral, source (sc0(i)) leads
    !! to multi species ion sce., remark (Jan. 2009):, charge exchange
    !! can affect impurity charge, states -- may need to add profiles
    !! to track, this interaction, in the future...
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
    call ccps_argcheck('psget_sprof0', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2, 1: n3) = ss % sprof0
end subroutine psget_sprof0


subroutine psset_sprof0(iobj, n1, n2, n3, data, ierr)
    !! gas species source/sinks per unit species influx, due to
    !! charge exchange, single species neutral, source (sc0(i)) leads
    !! to multi species ion sce., remark (Jan. 2009):, charge exchange
    !! can affect impurity charge, states -- may need to add profiles
    !! to track, this interaction, in the future...
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
    call ccps_argcheck('psset_sprof0', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % sprof0 = data(1:n1, 1: n2, 1: n3)
end subroutine psset_sprof0


subroutine psgetrnk_sprof0(iobj, ndims, ierr)
    !! gas species source/sinks per unit species influx, due to
    !! charge exchange, single species neutral, source (sc0(i)) leads
    !! to multi species ion sce., remark (Jan. 2009):, charge exchange
    !! can affect impurity charge, states -- may need to add profiles
    !! to track, this interaction, in the future...
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
end subroutine psgetrnk_sprof0


subroutine psgetdim_sprof0(iobj, ndims, dims, ierr)
    !! gas species source/sinks per unit species influx, due to
    !! charge exchange, single species neutral, source (sc0(i)) leads
    !! to multi species ion sce., remark (Jan. 2009):, charge exchange
    !! can affect impurity charge, states -- may need to add profiles
    !! to track, this interaction, in the future...
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
    call ccps_argcheck('psgetdim_sprof0', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % sprof0 ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % sprof0)
    endif
end subroutine psgetdim_sprof0

