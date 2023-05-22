
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_b6_dummy
     return
  end subroutine ccps_getset_b6_dummy

subroutine psget_beam_type(iobj, n1, data, ierr)
    !! beam type:, "standard", H/D/T beam using standard
    !! full-half-third energy, injection fractions; energy dependence
    !! of standard beam energy, fractions are defined as part of the
    !! machine description, "negative_ion", H/D/T beam 100% injected
    !! at full energy, "Helium" for Helium or He3 beam, energy
    !! fraction is 100% full energy., "Impurity" for injection of one
    !! of: {Ne,Ar,Xe,Kr}: 100% full energy.
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
    call ccps_argcheck('psget_beam_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % beam_type
end subroutine psget_beam_type


subroutine psset_beam_type(iobj, n1, data, ierr)
    !! beam type:, "standard", H/D/T beam using standard
    !! full-half-third energy, injection fractions; energy dependence
    !! of standard beam energy, fractions are defined as part of the
    !! machine description, "negative_ion", H/D/T beam 100% injected
    !! at full energy, "Helium" for Helium or He3 beam, energy
    !! fraction is 100% full energy., "Impurity" for injection of one
    !! of: {Ne,Ar,Xe,Kr}: 100% full energy.
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
    call ccps_argcheck('psset_beam_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % beam_type = data(1:n1)
end subroutine psset_beam_type


subroutine psgetsz_beam_type(iobj, size, ierr)
    !! beam type:, "standard", H/D/T beam using standard
    !! full-half-third energy, injection fractions; energy dependence
    !! of standard beam energy, fractions are defined as part of the
    !! machine description, "negative_ion", H/D/T beam 100% injected
    !! at full energy, "Helium" for Helium or He3 beam, energy
    !! fraction is 100% full energy., "Impurity" for injection of one
    !! of: {Ne,Ar,Xe,Kr}: 100% full energy.
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
    call ccps_argcheck('psgetsz_beam_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if beam_type is an array)
    size = len(ss % beam_type)
end subroutine psgetsz_beam_type


subroutine psgetrnk_beam_type(iobj, ndims, ierr)
    !! beam type:, "standard", H/D/T beam using standard
    !! full-half-third energy, injection fractions; energy dependence
    !! of standard beam energy, fractions are defined as part of the
    !! machine description, "negative_ion", H/D/T beam 100% injected
    !! at full energy, "Helium" for Helium or He3 beam, energy
    !! fraction is 100% full energy., "Impurity" for injection of one
    !! of: {Ne,Ar,Xe,Kr}: 100% full energy.
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
end subroutine psgetrnk_beam_type


subroutine psgetdim_beam_type(iobj, ndims, dims, ierr)
    !! beam type:, "standard", H/D/T beam using standard
    !! full-half-third energy, injection fractions; energy dependence
    !! of standard beam energy, fractions are defined as part of the
    !! machine description, "negative_ion", H/D/T beam 100% injected
    !! at full energy, "Helium" for Helium or He3 beam, energy
    !! fraction is 100% full energy., "Impurity" for injection of one
    !! of: {Ne,Ar,Xe,Kr}: 100% full energy.
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
    call ccps_argcheck('psgetdim_beam_type', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % beam_type ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % beam_type)
    endif
end subroutine psgetdim_beam_type


subroutine psget_b_hfocal_length(iobj, n1, data, ierr)
    !! horizontal focal length, for nbshape(j)="circle" sources
    !! this is the only focal length.
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
    call ccps_argcheck('psget_b_hfocal_length', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % b_Hfocal_length
end subroutine psget_b_hfocal_length


subroutine psset_b_hfocal_length(iobj, n1, data, ierr)
    !! horizontal focal length, for nbshape(j)="circle" sources
    !! this is the only focal length.
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
    call ccps_argcheck('psset_b_hfocal_length', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % b_Hfocal_length = data(1:n1)
end subroutine psset_b_hfocal_length


subroutine psgetrnk_b_hfocal_length(iobj, ndims, ierr)
    !! horizontal focal length, for nbshape(j)="circle" sources
    !! this is the only focal length.
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
end subroutine psgetrnk_b_hfocal_length


subroutine psgetdim_b_hfocal_length(iobj, ndims, dims, ierr)
    !! horizontal focal length, for nbshape(j)="circle" sources
    !! this is the only focal length.
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
    call ccps_argcheck('psgetdim_b_hfocal_length', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % b_Hfocal_length ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % b_Hfocal_length)
    endif
end subroutine psgetdim_b_hfocal_length


subroutine psget_balloon_code(iobj, n1, data, ierr)
    !! name(s) of Ballooning code(s) used
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
    call ccps_argcheck('psget_balloon_code', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % Balloon_code
end subroutine psget_balloon_code


subroutine psset_balloon_code(iobj, n1, data, ierr)
    !! name(s) of Ballooning code(s) used
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
    call ccps_argcheck('psset_balloon_code', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % Balloon_code = data(1:n1)
end subroutine psset_balloon_code


subroutine psgetsz_balloon_code(iobj, size, ierr)
    !! name(s) of Ballooning code(s) used
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
    call ccps_argcheck('psgetsz_balloon_code', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if Balloon_code is an array)
    size = len(ss % Balloon_code)
end subroutine psgetsz_balloon_code


subroutine psgetrnk_balloon_code(iobj, ndims, ierr)
    !! name(s) of Ballooning code(s) used
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
end subroutine psgetrnk_balloon_code


subroutine psgetdim_balloon_code(iobj, ndims, dims, ierr)
    !! name(s) of Ballooning code(s) used
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
    call ccps_argcheck('psgetdim_balloon_code', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % Balloon_code ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % Balloon_code)
    endif
end subroutine psgetdim_balloon_code

