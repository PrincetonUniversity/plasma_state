
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 2.056
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_b9_dummy
     return
  end subroutine ccps_getset_b9_dummy

subroutine psget_b_vdivergence(iobj, n1, data, ierr)
    !! vertical divergence (D) of beam, used for
    !! nbshape(j)="Rectangle" sources ONLY., --> definition of D
    !! (b_Vdivergence(...) given in DEGREES):, for alpha the random
    !! angle of deviation from the direction of, aiming, P(alpha) ~
    !! exp(-alpha**2/D**2); P(D) = (1/e)*P(0), I.e. D is the angle of
    !! deviation from the aiming angle, in, either direction, for a
    !! (1/e) reduction in probability of, launch of a beam particle.
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
    call ccps_argcheck('psget_b_vdivergence', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % b_Vdivergence
end subroutine psget_b_vdivergence


subroutine psset_b_vdivergence(iobj, n1, data, ierr)
    !! vertical divergence (D) of beam, used for
    !! nbshape(j)="Rectangle" sources ONLY., --> definition of D
    !! (b_Vdivergence(...) given in DEGREES):, for alpha the random
    !! angle of deviation from the direction of, aiming, P(alpha) ~
    !! exp(-alpha**2/D**2); P(D) = (1/e)*P(0), I.e. D is the angle of
    !! deviation from the aiming angle, in, either direction, for a
    !! (1/e) reduction in probability of, launch of a beam particle.
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
    call ccps_argcheck('psset_b_vdivergence', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % b_Vdivergence = data(1:n1)
end subroutine psset_b_vdivergence


subroutine psgetrnk_b_vdivergence(iobj, ndims, ierr)
    !! vertical divergence (D) of beam, used for
    !! nbshape(j)="Rectangle" sources ONLY., --> definition of D
    !! (b_Vdivergence(...) given in DEGREES):, for alpha the random
    !! angle of deviation from the direction of, aiming, P(alpha) ~
    !! exp(-alpha**2/D**2); P(D) = (1/e)*P(0), I.e. D is the angle of
    !! deviation from the aiming angle, in, either direction, for a
    !! (1/e) reduction in probability of, launch of a beam particle.
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
end subroutine psgetrnk_b_vdivergence


subroutine psgetdim_b_vdivergence(iobj, ndims, dims, ierr)
    !! vertical divergence (D) of beam, used for
    !! nbshape(j)="Rectangle" sources ONLY., --> definition of D
    !! (b_Vdivergence(...) given in DEGREES):, for alpha the random
    !! angle of deviation from the direction of, aiming, P(alpha) ~
    !! exp(-alpha**2/D**2); P(D) = (1/e)*P(0), I.e. D is the angle of
    !! deviation from the aiming angle, in, either direction, for a
    !! (1/e) reduction in probability of, launch of a beam particle.
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
    call ccps_argcheck('psgetdim_b_vdivergence', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % b_Vdivergence ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % b_Vdivergence)
    endif
end subroutine psgetdim_b_vdivergence


subroutine psget_balloon_mapper(iobj, n1, data, ierr)
    !! mapper code used for each ballooning code
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
    call ccps_argcheck('psget_balloon_mapper', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % Balloon_mapper
end subroutine psget_balloon_mapper


subroutine psset_balloon_mapper(iobj, n1, data, ierr)
    !! mapper code used for each ballooning code
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
    call ccps_argcheck('psset_balloon_mapper', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % Balloon_mapper = data(1:n1)
end subroutine psset_balloon_mapper


subroutine psgetsz_balloon_mapper(iobj, size, ierr)
    !! mapper code used for each ballooning code
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
    call ccps_argcheck('psgetsz_balloon_mapper', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if Balloon_mapper is an array)
    size = len(ss % Balloon_mapper)
end subroutine psgetsz_balloon_mapper


subroutine psgetrnk_balloon_mapper(iobj, ndims, ierr)
    !! mapper code used for each ballooning code
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
end subroutine psgetrnk_balloon_mapper


subroutine psgetdim_balloon_mapper(iobj, ndims, dims, ierr)
    !! mapper code used for each ballooning code
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
    call ccps_argcheck('psgetdim_balloon_mapper', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % Balloon_mapper ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % Balloon_mapper)
    endif
end subroutine psgetdim_balloon_mapper


subroutine psget_b_max_lcfs(iobj, data, ierr)
    !! |B|_max of last closed flux surface
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    REAL(KIND=rspec), intent(out)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_b_max_lcfs', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % B_max_lcfs
end subroutine psget_b_max_lcfs


subroutine psset_b_max_lcfs(iobj, data, ierr)
    !! |B|_max of last closed flux surface
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    REAL(KIND=rspec), intent(in)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_b_max_lcfs', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % B_max_lcfs = data
end subroutine psset_b_max_lcfs


subroutine psget_balloon_stable(iobj, n1, data, ierr)
    !! 0: stable everywhere; =1: unstable somewhere
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
    call ccps_argcheck('psget_balloon_stable', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % Balloon_stable
end subroutine psget_balloon_stable


subroutine psset_balloon_stable(iobj, n1, data, ierr)
    !! 0: stable everywhere; =1: unstable somewhere
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
    call ccps_argcheck('psset_balloon_stable', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % Balloon_stable = data(1:n1)
end subroutine psset_balloon_stable


subroutine psgetrnk_balloon_stable(iobj, ndims, ierr)
    !! 0: stable everywhere; =1: unstable somewhere
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
end subroutine psgetrnk_balloon_stable


subroutine psgetdim_balloon_stable(iobj, ndims, dims, ierr)
    !! 0: stable everywhere; =1: unstable somewhere
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
    call ccps_argcheck('psgetdim_balloon_stable', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % Balloon_stable ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % Balloon_stable)
    endif
end subroutine psgetdim_balloon_stable


subroutine psget_brrz(iobj, n1, n2, data, ierr)
    !! R component of poloidal field
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
    call ccps_argcheck('psget_brrz', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % BRRZ
end subroutine psget_brrz


subroutine psset_brrz(iobj, n1, n2, data, ierr)
    !! R component of poloidal field
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
    call ccps_argcheck('psset_brrz', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % BRRZ = data(1:n1, 1: n2)
end subroutine psset_brrz


subroutine psgetrnk_brrz(iobj, ndims, ierr)
    !! R component of poloidal field
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
end subroutine psgetrnk_brrz


subroutine psgetdim_brrz(iobj, ndims, dims, ierr)
    !! R component of poloidal field
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
    call ccps_argcheck('psgetdim_brrz', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % BRRZ ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % BRRZ)
    endif
end subroutine psgetdim_brrz


subroutine psget_bidiff_btrap(iobj, n1, n2, n3, data, ierr)
    !! beam ion diffusivity: barely trapped orbits
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
    call ccps_argcheck('psget_bidiff_btrap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2, 1: n3) = ss % bidiff_btrap
end subroutine psget_bidiff_btrap


subroutine psset_bidiff_btrap(iobj, n1, n2, n3, data, ierr)
    !! beam ion diffusivity: barely trapped orbits
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
    call ccps_argcheck('psset_bidiff_btrap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % bidiff_btrap = data(1:n1, 1: n2, 1: n3)
end subroutine psset_bidiff_btrap


subroutine psgetrnk_bidiff_btrap(iobj, ndims, ierr)
    !! beam ion diffusivity: barely trapped orbits
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
end subroutine psgetrnk_bidiff_btrap


subroutine psgetdim_bidiff_btrap(iobj, ndims, dims, ierr)
    !! beam ion diffusivity: barely trapped orbits
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
    call ccps_argcheck('psgetdim_bidiff_btrap', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % bidiff_btrap ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % bidiff_btrap)
    endif
end subroutine psgetdim_bidiff_btrap

