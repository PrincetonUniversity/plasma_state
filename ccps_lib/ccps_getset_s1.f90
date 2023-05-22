
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_s1_dummy
     return
  end subroutine ccps_getset_s1_dummy

subroutine psget_simp0_name(iobj, n1, data, ierr)
    !! atomic impurity species in simulation
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
    call ccps_argcheck('psget_simp0_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % SIMP0_name
end subroutine psget_simp0_name


subroutine psset_simp0_name(iobj, n1, data, ierr)
    !! atomic impurity species in simulation
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
    call ccps_argcheck('psset_simp0_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % SIMP0_name = data(1:n1)
end subroutine psset_simp0_name


subroutine psgetsz_simp0_name(iobj, size, ierr)
    !! atomic impurity species in simulation
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
    call ccps_argcheck('psgetsz_simp0_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if SIMP0_name is an array)
    size = len(ss % SIMP0_name)
end subroutine psgetsz_simp0_name


subroutine psgetrnk_simp0_name(iobj, ndims, ierr)
    !! atomic impurity species in simulation
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
end subroutine psgetrnk_simp0_name


subroutine psgetdim_simp0_name(iobj, ndims, dims, ierr)
    !! atomic impurity species in simulation
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
    call ccps_argcheck('psgetdim_simp0_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % SIMP0_name ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % SIMP0_name)
    endif
end subroutine psgetdim_simp0_name


subroutine psget_sbehalo(iobj, n1, data, ierr)
    !! cold electron sce due to, recapture of beam halo charge
    !! exchange thermal neutrals, = sum Z(i)*sb0halo_recap(i), if
    !! electrons do not go into, an impurity charge state spectrum.
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
    call ccps_argcheck('psget_sbehalo', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % sbehalo
end subroutine psget_sbehalo


subroutine psset_sbehalo(iobj, n1, data, ierr)
    !! cold electron sce due to, recapture of beam halo charge
    !! exchange thermal neutrals, = sum Z(i)*sb0halo_recap(i), if
    !! electrons do not go into, an impurity charge state spectrum.
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
    call ccps_argcheck('psset_sbehalo', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % sbehalo = data(1:n1)
end subroutine psset_sbehalo


subroutine psgetrnk_sbehalo(iobj, ndims, ierr)
    !! cold electron sce due to, recapture of beam halo charge
    !! exchange thermal neutrals, = sum Z(i)*sb0halo_recap(i), if
    !! electrons do not go into, an impurity charge state spectrum.
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
end subroutine psgetrnk_sbehalo


subroutine psgetdim_sbehalo(iobj, ndims, dims, ierr)
    !! cold electron sce due to, recapture of beam halo charge
    !! exchange thermal neutrals, = sum Z(i)*sb0halo_recap(i), if
    !! electrons do not go into, an impurity charge state spectrum.
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
    call ccps_argcheck('psgetdim_sbehalo', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % sbehalo ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % sbehalo)
    endif
end subroutine psgetdim_sbehalo


subroutine psget_s0reco_recap(iobj, n1, n2, data, ierr)
    !! recombination neutrals recapture, recapture of recombination
    !! source thermal neutrals -- ion source, psc_reco =~
    !! (3/2)*Ti*(sb0reco_recap-sb0reco)*1.602e-16
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
    call ccps_argcheck('psget_s0reco_recap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % s0reco_recap
end subroutine psget_s0reco_recap


subroutine psset_s0reco_recap(iobj, n1, n2, data, ierr)
    !! recombination neutrals recapture, recapture of recombination
    !! source thermal neutrals -- ion source, psc_reco =~
    !! (3/2)*Ti*(sb0reco_recap-sb0reco)*1.602e-16
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
    call ccps_argcheck('psset_s0reco_recap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % s0reco_recap = data(1:n1, 1: n2)
end subroutine psset_s0reco_recap


subroutine psgetrnk_s0reco_recap(iobj, ndims, ierr)
    !! recombination neutrals recapture, recapture of recombination
    !! source thermal neutrals -- ion source, psc_reco =~
    !! (3/2)*Ti*(sb0reco_recap-sb0reco)*1.602e-16
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
end subroutine psgetrnk_s0reco_recap


subroutine psgetdim_s0reco_recap(iobj, ndims, dims, ierr)
    !! recombination neutrals recapture, recapture of recombination
    !! source thermal neutrals -- ion source, psc_reco =~
    !! (3/2)*Ti*(sb0reco_recap-sb0reco)*1.602e-16
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
    call ccps_argcheck('psgetdim_s0reco_recap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % s0reco_recap ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % s0reco_recap)
    endif
end subroutine psgetdim_s0reco_recap

