
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_s2_dummy
     return
  end subroutine ccps_getset_s2_dummy

subroutine psget_sfus_name(iobj, n1, data, ierr)
    !! fusion product ion species
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
    call ccps_argcheck('psget_sfus_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % SFUS_name
end subroutine psget_sfus_name


subroutine psset_sfus_name(iobj, n1, data, ierr)
    !! fusion product ion species
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
    call ccps_argcheck('psset_sfus_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % SFUS_name = data(1:n1)
end subroutine psset_sfus_name


subroutine psgetsz_sfus_name(iobj, size, ierr)
    !! fusion product ion species
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
    call ccps_argcheck('psgetsz_sfus_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if SFUS_name is an array)
    size = len(ss % SFUS_name)
end subroutine psgetsz_sfus_name


subroutine psgetrnk_sfus_name(iobj, ndims, ierr)
    !! fusion product ion species
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
end subroutine psgetrnk_sfus_name


subroutine psgetdim_sfus_name(iobj, ndims, dims, ierr)
    !! fusion product ion species
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
    call ccps_argcheck('psgetdim_sfus_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % SFUS_name ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % SFUS_name)
    endif
end subroutine psgetdim_sfus_name


subroutine psget_sa_type(iobj, n1, data, ierr)
    !! SA specie types
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
    call ccps_argcheck('psget_sa_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % SA_type
end subroutine psget_sa_type


subroutine psset_sa_type(iobj, n1, data, ierr)
    !! SA specie types
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
    call ccps_argcheck('psset_sa_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % SA_type = data(1:n1)
end subroutine psset_sa_type


subroutine psgetrnk_sa_type(iobj, ndims, ierr)
    !! SA specie types
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
end subroutine psgetrnk_sa_type


subroutine psgetdim_sa_type(iobj, ndims, dims, ierr)
    !! SA specie types
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
    call ccps_argcheck('psgetdim_sa_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % SA_type ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % SA_type)
    endif
end subroutine psgetdim_sa_type


subroutine psget_simp0_to_s(iobj, n1, data, ierr)
    !! map simp0 index to "s" list
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
    call ccps_argcheck('psget_simp0_to_s', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % simp0_to_s
end subroutine psget_simp0_to_s


subroutine psset_simp0_to_s(iobj, n1, data, ierr)
    !! map simp0 index to "s" list
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
    call ccps_argcheck('psset_simp0_to_s', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % simp0_to_s = data(1:n1)
end subroutine psset_simp0_to_s


subroutine psgetrnk_simp0_to_s(iobj, ndims, ierr)
    !! map simp0 index to "s" list
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
end subroutine psgetrnk_simp0_to_s


subroutine psgetdim_simp0_to_s(iobj, ndims, dims, ierr)
    !! map simp0 index to "s" list
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
    call ccps_argcheck('psgetdim_simp0_to_s', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % simp0_to_s ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % simp0_to_s)
    endif
end subroutine psgetdim_simp0_to_s


subroutine psget_surf(iobj, n1, data, ierr)
    !! area of flux surfaces
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
    call ccps_argcheck('psget_surf', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % surf
end subroutine psget_surf


subroutine psset_surf(iobj, n1, data, ierr)
    !! area of flux surfaces
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
    call ccps_argcheck('psset_surf', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % surf = data(1:n1)
end subroutine psset_surf


subroutine psgetrnk_surf(iobj, ndims, ierr)
    !! area of flux surfaces
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
end subroutine psgetrnk_surf


subroutine psgetdim_surf(iobj, ndims, dims, ierr)
    !! area of flux surfaces
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
    call ccps_argcheck('psgetdim_surf', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % surf ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % surf)
    endif
end subroutine psgetdim_surf


subroutine psget_sb0halo_recap(iobj, n1, n2, data, ierr)
    !! cx neutral recapture, recapture of thermal neutrals -- ion
    !! source, psc_halo =~ (3/2)*Ti*(sb0halo_recap-sb0halo)*1.602e-16,
    !! (cross species CX source/sink contributions may occur), note:
    !! electron recapture, cold source = sum
    !! (Z(i)*sb0halo_recap(:,i)), where Z(i) is the number of
    !! electrons carried by a neutral atom, of species (i).  => this
    !! is to be included in sbsce(:,0).
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
    call ccps_argcheck('psget_sb0halo_recap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % sb0halo_recap
end subroutine psget_sb0halo_recap


subroutine psset_sb0halo_recap(iobj, n1, n2, data, ierr)
    !! cx neutral recapture, recapture of thermal neutrals -- ion
    !! source, psc_halo =~ (3/2)*Ti*(sb0halo_recap-sb0halo)*1.602e-16,
    !! (cross species CX source/sink contributions may occur), note:
    !! electron recapture, cold source = sum
    !! (Z(i)*sb0halo_recap(:,i)), where Z(i) is the number of
    !! electrons carried by a neutral atom, of species (i).  => this
    !! is to be included in sbsce(:,0).
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
    call ccps_argcheck('psset_sb0halo_recap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % sb0halo_recap = data(1:n1, 1: n2)
end subroutine psset_sb0halo_recap


subroutine psgetrnk_sb0halo_recap(iobj, ndims, ierr)
    !! cx neutral recapture, recapture of thermal neutrals -- ion
    !! source, psc_halo =~ (3/2)*Ti*(sb0halo_recap-sb0halo)*1.602e-16,
    !! (cross species CX source/sink contributions may occur), note:
    !! electron recapture, cold source = sum
    !! (Z(i)*sb0halo_recap(:,i)), where Z(i) is the number of
    !! electrons carried by a neutral atom, of species (i).  => this
    !! is to be included in sbsce(:,0).
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
end subroutine psgetrnk_sb0halo_recap


subroutine psgetdim_sb0halo_recap(iobj, ndims, dims, ierr)
    !! cx neutral recapture, recapture of thermal neutrals -- ion
    !! source, psc_halo =~ (3/2)*Ti*(sb0halo_recap-sb0halo)*1.602e-16,
    !! (cross species CX source/sink contributions may occur), note:
    !! electron recapture, cold source = sum
    !! (Z(i)*sb0halo_recap(:,i)), where Z(i) is the number of
    !! electrons carried by a neutral atom, of species (i).  => this
    !! is to be included in sbsce(:,0).
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
    call ccps_argcheck('psgetdim_sb0halo_recap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % sb0halo_recap ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % sb0halo_recap)
    endif
end subroutine psgetdim_sb0halo_recap


subroutine psget_sfsce(iobj, n1, n2, data, ierr)
    !! net thermalization source of fusion ions, sources due to
    !! fusion ions:  fusion ion thermalization;, corrections due to
    !! charge exchange effects (usually small).
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
    call ccps_argcheck('psget_sfsce', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % sfsce
end subroutine psget_sfsce


subroutine psset_sfsce(iobj, n1, n2, data, ierr)
    !! net thermalization source of fusion ions, sources due to
    !! fusion ions:  fusion ion thermalization;, corrections due to
    !! charge exchange effects (usually small).
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
    call ccps_argcheck('psset_sfsce', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % sfsce = data(1:n1, 1: n2)
end subroutine psset_sfsce


subroutine psgetrnk_sfsce(iobj, ndims, ierr)
    !! net thermalization source of fusion ions, sources due to
    !! fusion ions:  fusion ion thermalization;, corrections due to
    !! charge exchange effects (usually small).
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
end subroutine psgetrnk_sfsce


subroutine psgetdim_sfsce(iobj, ndims, dims, ierr)
    !! net thermalization source of fusion ions, sources due to
    !! fusion ions:  fusion ion thermalization;, corrections due to
    !! charge exchange effects (usually small).
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
    call ccps_argcheck('psgetdim_sfsce', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % sfsce ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % sfsce)
    endif
end subroutine psgetdim_sfsce


subroutine psget_s0reco_e(iobj, n1, data, ierr)
    !! net electron source associated with recombination:,
    !! sum[-Z(i)*s0reco(:,i) + Z(i)*s0reco_recap(:,i)]
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
    call ccps_argcheck('psget_s0reco_e', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % s0reco_e
end subroutine psget_s0reco_e


subroutine psset_s0reco_e(iobj, n1, data, ierr)
    !! net electron source associated with recombination:,
    !! sum[-Z(i)*s0reco(:,i) + Z(i)*s0reco_recap(:,i)]
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
    call ccps_argcheck('psset_s0reco_e', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % s0reco_e = data(1:n1)
end subroutine psset_s0reco_e


subroutine psgetrnk_s0reco_e(iobj, ndims, ierr)
    !! net electron source associated with recombination:,
    !! sum[-Z(i)*s0reco(:,i) + Z(i)*s0reco_recap(:,i)]
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
end subroutine psgetrnk_s0reco_e


subroutine psgetdim_s0reco_e(iobj, ndims, dims, ierr)
    !! net electron source associated with recombination:,
    !! sum[-Z(i)*s0reco(:,i) + Z(i)*s0reco_recap(:,i)]
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
    call ccps_argcheck('psgetdim_s0reco_e', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % s0reco_e ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % s0reco_e)
    endif
end subroutine psgetdim_s0reco_e

