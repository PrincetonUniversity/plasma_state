
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_c0_dummy
     return
  end subroutine ccps_getset_c0_dummy

subroutine psget_circuit_name(iobj, n1, data, ierr)
    !! Number & names of circuits, connecting coils affecting
    !! poloidal field-- axisymmetric coils,, or eddy currents
    !! represented as cancelling coil pairs
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
    call ccps_argcheck('psget_circuit_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % circuit_name
end subroutine psget_circuit_name


subroutine psset_circuit_name(iobj, n1, data, ierr)
    !! Number & names of circuits, connecting coils affecting
    !! poloidal field-- axisymmetric coils,, or eddy currents
    !! represented as cancelling coil pairs
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
    call ccps_argcheck('psset_circuit_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % circuit_name = data(1:n1)
end subroutine psset_circuit_name


subroutine psgetsz_circuit_name(iobj, size, ierr)
    !! Number & names of circuits, connecting coils affecting
    !! poloidal field-- axisymmetric coils,, or eddy currents
    !! represented as cancelling coil pairs
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
    call ccps_argcheck('psgetsz_circuit_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if circuit_name is an array)
    size = len(ss % circuit_name)
end subroutine psgetsz_circuit_name


subroutine psgetrnk_circuit_name(iobj, ndims, ierr)
    !! Number & names of circuits, connecting coils affecting
    !! poloidal field-- axisymmetric coils,, or eddy currents
    !! represented as cancelling coil pairs
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
end subroutine psgetrnk_circuit_name


subroutine psgetdim_circuit_name(iobj, ndims, dims, ierr)
    !! Number & names of circuits, connecting coils affecting
    !! poloidal field-- axisymmetric coils,, or eddy currents
    !! represented as cancelling coil pairs
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
    call ccps_argcheck('psgetdim_circuit_name', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % circuit_name ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % circuit_name)
    endif
end subroutine psgetdim_circuit_name


subroutine psget_curt(iobj, n1, data, ierr)
    !! total enclosed toroidal current, this includes <j.B>
    !! contributions and <j x B> contributions and, is related to the
    !! poloidal field:, mu0*I(rho) =
    !! rho*phit(nrho_eq)/(q(rho)*2*pi*pi), * grho2r2i(rho) *
    !! (dVol/drho)', "(From Ampere's Law, mu0*I_phi = loop integral
    !! (B_pol*dl)", '= loop integral (dl * grad(psi)/R), =
    !! rho*phit(nrho_eq)/(q(rho)*pi) *, loop integral (dl *
    !! grad(rho)/R), (2*pi) * loop integral (dl * grad(rho)/rho), =
    !! loop integral (dl * 2*pi*R/grad(rho) * grad(rho)**2/R**2), =
    !! grho2r2i(rho) * dVol/drho, derivation depends on the definition
    !! of the radial coord.:, rho = sqrt(phit(rho)/phit(rho=1)),
    !! curt(nrho_eq) gives the total current, >0 by convention;, see
    !! kccw_Jphi, +/-1, for direction of toroidal current.
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
    call ccps_argcheck('psget_curt', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % curt
end subroutine psget_curt


subroutine psset_curt(iobj, n1, data, ierr)
    !! total enclosed toroidal current, this includes <j.B>
    !! contributions and <j x B> contributions and, is related to the
    !! poloidal field:, mu0*I(rho) =
    !! rho*phit(nrho_eq)/(q(rho)*2*pi*pi), * grho2r2i(rho) *
    !! (dVol/drho)', "(From Ampere's Law, mu0*I_phi = loop integral
    !! (B_pol*dl)", '= loop integral (dl * grad(psi)/R), =
    !! rho*phit(nrho_eq)/(q(rho)*pi) *, loop integral (dl *
    !! grad(rho)/R), (2*pi) * loop integral (dl * grad(rho)/rho), =
    !! loop integral (dl * 2*pi*R/grad(rho) * grad(rho)**2/R**2), =
    !! grho2r2i(rho) * dVol/drho, derivation depends on the definition
    !! of the radial coord.:, rho = sqrt(phit(rho)/phit(rho=1)),
    !! curt(nrho_eq) gives the total current, >0 by convention;, see
    !! kccw_Jphi, +/-1, for direction of toroidal current.
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
    call ccps_argcheck('psset_curt', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % curt = data(1:n1)
end subroutine psset_curt


subroutine psgetrnk_curt(iobj, ndims, ierr)
    !! total enclosed toroidal current, this includes <j.B>
    !! contributions and <j x B> contributions and, is related to the
    !! poloidal field:, mu0*I(rho) =
    !! rho*phit(nrho_eq)/(q(rho)*2*pi*pi), * grho2r2i(rho) *
    !! (dVol/drho)', "(From Ampere's Law, mu0*I_phi = loop integral
    !! (B_pol*dl)", '= loop integral (dl * grad(psi)/R), =
    !! rho*phit(nrho_eq)/(q(rho)*pi) *, loop integral (dl *
    !! grad(rho)/R), (2*pi) * loop integral (dl * grad(rho)/rho), =
    !! loop integral (dl * 2*pi*R/grad(rho) * grad(rho)**2/R**2), =
    !! grho2r2i(rho) * dVol/drho, derivation depends on the definition
    !! of the radial coord.:, rho = sqrt(phit(rho)/phit(rho=1)),
    !! curt(nrho_eq) gives the total current, >0 by convention;, see
    !! kccw_Jphi, +/-1, for direction of toroidal current.
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
end subroutine psgetrnk_curt


subroutine psgetdim_curt(iobj, ndims, dims, ierr)
    !! total enclosed toroidal current, this includes <j.B>
    !! contributions and <j x B> contributions and, is related to the
    !! poloidal field:, mu0*I(rho) =
    !! rho*phit(nrho_eq)/(q(rho)*2*pi*pi), * grho2r2i(rho) *
    !! (dVol/drho)', "(From Ampere's Law, mu0*I_phi = loop integral
    !! (B_pol*dl)", '= loop integral (dl * grad(psi)/R), =
    !! rho*phit(nrho_eq)/(q(rho)*pi) *, loop integral (dl *
    !! grad(rho)/R), (2*pi) * loop integral (dl * grad(rho)/rho), =
    !! loop integral (dl * 2*pi*R/grad(rho) * grad(rho)**2/R**2), =
    !! grho2r2i(rho) * dVol/drho, derivation depends on the definition
    !! of the radial coord.:, rho = sqrt(phit(rho)/phit(rho=1)),
    !! curt(nrho_eq) gives the total current, >0 by convention;, see
    !! kccw_Jphi, +/-1, for direction of toroidal current.
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
    call ccps_argcheck('psgetdim_curt', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % curt ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % curt)
    endif
end subroutine psgetdim_curt


subroutine psget_curlh(iobj, n1, data, ierr)
    !! LH current drive
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
    call ccps_argcheck('psget_curlh', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % curlh
end subroutine psget_curlh


subroutine psset_curlh(iobj, n1, data, ierr)
    !! LH current drive
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
    call ccps_argcheck('psset_curlh', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % curlh = data(1:n1)
end subroutine psset_curlh


subroutine psgetrnk_curlh(iobj, ndims, ierr)
    !! LH current drive
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
end subroutine psgetrnk_curlh


subroutine psgetdim_curlh(iobj, ndims, dims, ierr)
    !! LH current drive
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
    call ccps_argcheck('psgetdim_curlh', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % curlh ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % curlh)
    endif
end subroutine psgetdim_curlh


subroutine psget_chii_mmm71(iobj, n1, data, ierr)
    !! Ti-equation, diffusivity term
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
    call ccps_argcheck('psget_chii_mmm71', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % chii_mmm71
end subroutine psget_chii_mmm71


subroutine psset_chii_mmm71(iobj, n1, data, ierr)
    !! Ti-equation, diffusivity term
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
    call ccps_argcheck('psset_chii_mmm71', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % chii_mmm71 = data(1:n1)
end subroutine psset_chii_mmm71


subroutine psgetrnk_chii_mmm71(iobj, ndims, ierr)
    !! Ti-equation, diffusivity term
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
end subroutine psgetrnk_chii_mmm71


subroutine psgetdim_chii_mmm71(iobj, ndims, dims, ierr)
    !! Ti-equation, diffusivity term
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
    call ccps_argcheck('psgetdim_chii_mmm71', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % chii_mmm71 ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % chii_mmm71)
    endif
end subroutine psgetdim_chii_mmm71


subroutine psget_chie_glf(iobj, n1, data, ierr)
    !! Te-equation, diffusivity from GLF23 model
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
    call ccps_argcheck('psget_chie_glf', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % chie_glf
end subroutine psget_chie_glf


subroutine psset_chie_glf(iobj, n1, data, ierr)
    !! Te-equation, diffusivity from GLF23 model
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
    call ccps_argcheck('psset_chie_glf', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % chie_glf = data(1:n1)
end subroutine psset_chie_glf


subroutine psgetrnk_chie_glf(iobj, ndims, ierr)
    !! Te-equation, diffusivity from GLF23 model
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
end subroutine psgetrnk_chie_glf


subroutine psgetdim_chie_glf(iobj, ndims, dims, ierr)
    !! Te-equation, diffusivity from GLF23 model
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
    call ccps_argcheck('psgetdim_chie_glf', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % chie_glf ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % chie_glf)
    endif
end subroutine psgetdim_chie_glf


subroutine psget_chie_nclass(iobj, n1, data, ierr)
    !! Te-equation, neoclassical diffusivity from NCLASS model
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
    call ccps_argcheck('psget_chie_nclass', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % chie_nclass
end subroutine psget_chie_nclass


subroutine psset_chie_nclass(iobj, n1, data, ierr)
    !! Te-equation, neoclassical diffusivity from NCLASS model
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
    call ccps_argcheck('psset_chie_nclass', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % chie_nclass = data(1:n1)
end subroutine psset_chie_nclass


subroutine psgetrnk_chie_nclass(iobj, ndims, ierr)
    !! Te-equation, neoclassical diffusivity from NCLASS model
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
end subroutine psgetrnk_chie_nclass


subroutine psgetdim_chie_nclass(iobj, ndims, dims, ierr)
    !! Te-equation, neoclassical diffusivity from NCLASS model
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
    call ccps_argcheck('psgetdim_chie_nclass', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % chie_nclass ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % chie_nclass)
    endif
end subroutine psgetdim_chie_nclass

