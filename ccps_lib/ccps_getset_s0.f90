
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_s0_dummy
     return
  end subroutine ccps_getset_s0_dummy

subroutine psget_s_type(iobj, n1, data, ierr)
    !! S specie types
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
    call ccps_argcheck('psget_s_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % S_type
end subroutine psget_s_type


subroutine psset_s_type(iobj, n1, data, ierr)
    !! S specie types
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
    call ccps_argcheck('psset_s_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % S_type = data(1:n1)
end subroutine psset_s_type


subroutine psgetrnk_s_type(iobj, ndims, ierr)
    !! S specie types
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
end subroutine psgetrnk_s_type


subroutine psgetdim_s_type(iobj, ndims, dims, ierr)
    !! S specie types
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
    call ccps_argcheck('psgetdim_s_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % S_type ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % S_type)
    endif
end subroutine psgetdim_s_type


subroutine psget_sgas_type(iobj, n1, data, ierr)
    !! SGAS specie types
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
    call ccps_argcheck('psget_sgas_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % SGAS_type
end subroutine psget_sgas_type


subroutine psset_sgas_type(iobj, n1, data, ierr)
    !! SGAS specie types
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
    call ccps_argcheck('psset_sgas_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % SGAS_type = data(1:n1)
end subroutine psset_sgas_type


subroutine psgetrnk_sgas_type(iobj, ndims, ierr)
    !! SGAS specie types
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
end subroutine psgetrnk_sgas_type


subroutine psgetdim_sgas_type(iobj, ndims, dims, ierr)
    !! SGAS specie types
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
    call ccps_argcheck('psgetdim_sgas_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % SGAS_type ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % SGAS_type)
    endif
end subroutine psgetdim_sgas_type


subroutine psget_sb0halo(iobj, n1, n2, data, ierr)
    !! beam dep cx sink, thermal neutrals created by fast neutral
    !! deposition and/or recapture, of fast neutrals, psc_halo =~
    !! (3/2)*Ti*(sb0halo_recap-sb0halo)*1.602e-16
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
    call ccps_argcheck('psget_sb0halo', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % sb0halo
end subroutine psget_sb0halo


subroutine psset_sb0halo(iobj, n1, n2, data, ierr)
    !! beam dep cx sink, thermal neutrals created by fast neutral
    !! deposition and/or recapture, of fast neutrals, psc_halo =~
    !! (3/2)*Ti*(sb0halo_recap-sb0halo)*1.602e-16
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
    call ccps_argcheck('psset_sb0halo', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % sb0halo = data(1:n1, 1: n2)
end subroutine psset_sb0halo


subroutine psgetrnk_sb0halo(iobj, ndims, ierr)
    !! beam dep cx sink, thermal neutrals created by fast neutral
    !! deposition and/or recapture, of fast neutrals, psc_halo =~
    !! (3/2)*Ti*(sb0halo_recap-sb0halo)*1.602e-16
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
end subroutine psgetrnk_sb0halo


subroutine psgetdim_sb0halo(iobj, ndims, dims, ierr)
    !! beam dep cx sink, thermal neutrals created by fast neutral
    !! deposition and/or recapture, of fast neutrals, psc_halo =~
    !! (3/2)*Ti*(sb0halo_recap-sb0halo)*1.602e-16
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
    call ccps_argcheck('psgetdim_sb0halo', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % sb0halo ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % sb0halo)
    endif
end subroutine psgetdim_sb0halo


subroutine psget_s0reco(iobj, n1, n2, data, ierr)
    !! recombination neutral sce, psc_reco =~
    !! (3/2)*Ti*(s0reco_recap-s0reco)*1.602e-16, note: electron sink =
    !! sum (Z(i)*s0reco(:,i)), where Z(i) is the number of electrons
    !! carried by a neutral atom, of species (i).
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
    call ccps_argcheck('psget_s0reco', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % s0reco
end subroutine psget_s0reco


subroutine psset_s0reco(iobj, n1, n2, data, ierr)
    !! recombination neutral sce, psc_reco =~
    !! (3/2)*Ti*(s0reco_recap-s0reco)*1.602e-16, note: electron sink =
    !! sum (Z(i)*s0reco(:,i)), where Z(i) is the number of electrons
    !! carried by a neutral atom, of species (i).
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
    call ccps_argcheck('psset_s0reco', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % s0reco = data(1:n1, 1: n2)
end subroutine psset_s0reco


subroutine psgetrnk_s0reco(iobj, ndims, ierr)
    !! recombination neutral sce, psc_reco =~
    !! (3/2)*Ti*(s0reco_recap-s0reco)*1.602e-16, note: electron sink =
    !! sum (Z(i)*s0reco(:,i)), where Z(i) is the number of electrons
    !! carried by a neutral atom, of species (i).
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
end subroutine psgetrnk_s0reco


subroutine psgetdim_s0reco(iobj, ndims, dims, ierr)
    !! recombination neutral sce, psc_reco =~
    !! (3/2)*Ti*(s0reco_recap-s0reco)*1.602e-16, note: electron sink =
    !! sum (Z(i)*s0reco(:,i)), where Z(i) is the number of electrons
    !! carried by a neutral atom, of species (i).
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
    call ccps_argcheck('psgetdim_s0reco', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % s0reco ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % s0reco)
    endif
end subroutine psgetdim_s0reco

