
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 2.056
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_e3_dummy
     return
  end subroutine ccps_getset_e3_dummy

subroutine psget_ec_beam_elongation(iobj, n1, data, ierr)
    !! EC beam ellipticity, The conical EC beam shape can be
    !! elongated, "vertically" (elongation > 1) or "horizontally",
    !! (elongation < 1).  Here "vertical" means in the, direction
    !! normal to the plane contaiing the beam, centerline and the +phi
    !! unit vector; "horizontal", means normal to the plane of the
    !! beam centerline, and the "vertical" direction.  If this
    !! quantity is, defaulted to zero, it is reset to 1.0 exactly,
    !! i.e., for a beam with circular cross section.
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
    call ccps_argcheck('psget_ec_beam_elongation', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % EC_Beam_Elongation
end subroutine psget_ec_beam_elongation


subroutine psset_ec_beam_elongation(iobj, n1, data, ierr)
    !! EC beam ellipticity, The conical EC beam shape can be
    !! elongated, "vertically" (elongation > 1) or "horizontally",
    !! (elongation < 1).  Here "vertical" means in the, direction
    !! normal to the plane contaiing the beam, centerline and the +phi
    !! unit vector; "horizontal", means normal to the plane of the
    !! beam centerline, and the "vertical" direction.  If this
    !! quantity is, defaulted to zero, it is reset to 1.0 exactly,
    !! i.e., for a beam with circular cross section.
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
    call ccps_argcheck('psset_ec_beam_elongation', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % EC_Beam_Elongation = data(1:n1)
end subroutine psset_ec_beam_elongation


subroutine psgetrnk_ec_beam_elongation(iobj, ndims, ierr)
    !! EC beam ellipticity, The conical EC beam shape can be
    !! elongated, "vertically" (elongation > 1) or "horizontally",
    !! (elongation < 1).  Here "vertical" means in the, direction
    !! normal to the plane contaiing the beam, centerline and the +phi
    !! unit vector; "horizontal", means normal to the plane of the
    !! beam centerline, and the "vertical" direction.  If this
    !! quantity is, defaulted to zero, it is reset to 1.0 exactly,
    !! i.e., for a beam with circular cross section.
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
end subroutine psgetrnk_ec_beam_elongation


subroutine psgetdim_ec_beam_elongation(iobj, ndims, dims, ierr)
    !! EC beam ellipticity, The conical EC beam shape can be
    !! elongated, "vertically" (elongation > 1) or "horizontally",
    !! (elongation < 1).  Here "vertical" means in the, direction
    !! normal to the plane contaiing the beam, centerline and the +phi
    !! unit vector; "horizontal", means normal to the plane of the
    !! beam centerline, and the "vertical" direction.  If this
    !! quantity is, defaulted to zero, it is reset to 1.0 exactly,
    !! i.e., for a beam with circular cross section.
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
    call ccps_argcheck('psgetdim_ec_beam_elongation', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % EC_Beam_Elongation ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % EC_Beam_Elongation)
    endif
end subroutine psgetdim_ec_beam_elongation


subroutine psget_ec_phi_aim(iobj, n1, data, ierr)
    !! toroidal aiming angle, 0 degrees-> component of beam in +R
    !! direction, 90 degrees-> component of beam in +phi direction,
    !! 180 degrees-> component of beam in -R direction, 270 degrees or
    !! -90 degrees-> component in -phi direction, An EC beam with a
    !! component in the +phi direction, will push electrons in the
    !! +phi direction, counter, clockwise (CCW) around the machine
    !! axis as viewed, from above; and hence drive current in a
    !! clockwise, (CW) direction.  See shot configuration variable,
    !! kCCW_Jphi for orientation of main plasma current., See also
    !! EC_theta_aim(...).
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
    call ccps_argcheck('psget_ec_phi_aim', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % EC_phi_aim
end subroutine psget_ec_phi_aim


subroutine psset_ec_phi_aim(iobj, n1, data, ierr)
    !! toroidal aiming angle, 0 degrees-> component of beam in +R
    !! direction, 90 degrees-> component of beam in +phi direction,
    !! 180 degrees-> component of beam in -R direction, 270 degrees or
    !! -90 degrees-> component in -phi direction, An EC beam with a
    !! component in the +phi direction, will push electrons in the
    !! +phi direction, counter, clockwise (CCW) around the machine
    !! axis as viewed, from above; and hence drive current in a
    !! clockwise, (CW) direction.  See shot configuration variable,
    !! kCCW_Jphi for orientation of main plasma current., See also
    !! EC_theta_aim(...).
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
    call ccps_argcheck('psset_ec_phi_aim', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % EC_phi_aim = data(1:n1)
end subroutine psset_ec_phi_aim


subroutine psgetrnk_ec_phi_aim(iobj, ndims, ierr)
    !! toroidal aiming angle, 0 degrees-> component of beam in +R
    !! direction, 90 degrees-> component of beam in +phi direction,
    !! 180 degrees-> component of beam in -R direction, 270 degrees or
    !! -90 degrees-> component in -phi direction, An EC beam with a
    !! component in the +phi direction, will push electrons in the
    !! +phi direction, counter, clockwise (CCW) around the machine
    !! axis as viewed, from above; and hence drive current in a
    !! clockwise, (CW) direction.  See shot configuration variable,
    !! kCCW_Jphi for orientation of main plasma current., See also
    !! EC_theta_aim(...).
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
end subroutine psgetrnk_ec_phi_aim


subroutine psgetdim_ec_phi_aim(iobj, ndims, dims, ierr)
    !! toroidal aiming angle, 0 degrees-> component of beam in +R
    !! direction, 90 degrees-> component of beam in +phi direction,
    !! 180 degrees-> component of beam in -R direction, 270 degrees or
    !! -90 degrees-> component in -phi direction, An EC beam with a
    !! component in the +phi direction, will push electrons in the
    !! +phi direction, counter, clockwise (CCW) around the machine
    !! axis as viewed, from above; and hence drive current in a
    !! clockwise, (CW) direction.  See shot configuration variable,
    !! kCCW_Jphi for orientation of main plasma current., See also
    !! EC_theta_aim(...).
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
    call ccps_argcheck('psgetdim_ec_phi_aim', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % EC_phi_aim ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % EC_phi_aim)
    endif
end subroutine psgetdim_ec_phi_aim


subroutine psget_eqmom_num(iobj, n1, data, ierr)
    !! Equilibrium moments', "neqmom = nmom; eqmom_num(1)='1',
    !! eqmom_num(2)='2', etc..."]
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
    call ccps_argcheck('psget_eqmom_num', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % eqmom_num
end subroutine psget_eqmom_num


subroutine psset_eqmom_num(iobj, n1, data, ierr)
    !! Equilibrium moments', "neqmom = nmom; eqmom_num(1)='1',
    !! eqmom_num(2)='2', etc..."]
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
    call ccps_argcheck('psset_eqmom_num', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % eqmom_num = data(1:n1)
end subroutine psset_eqmom_num


subroutine psgetsz_eqmom_num(iobj, size, ierr)
    !! Equilibrium moments', "neqmom = nmom; eqmom_num(1)='1',
    !! eqmom_num(2)='2', etc..."]
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
    call ccps_argcheck('psgetsz_eqmom_num', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if eqmom_num is an array)
    size = len(ss % eqmom_num)
end subroutine psgetsz_eqmom_num


subroutine psgetrnk_eqmom_num(iobj, ndims, ierr)
    !! Equilibrium moments', "neqmom = nmom; eqmom_num(1)='1',
    !! eqmom_num(2)='2', etc..."]
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
end subroutine psgetrnk_eqmom_num


subroutine psgetdim_eqmom_num(iobj, ndims, dims, ierr)
    !! Equilibrium moments', "neqmom = nmom; eqmom_num(1)='1',
    !! eqmom_num(2)='2', etc..."]
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
    call ccps_argcheck('psgetdim_eqmom_num', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % eqmom_num ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % eqmom_num)
    endif
end subroutine psgetdim_eqmom_num


subroutine psget_eperp_mini(iobj, n1, n2, data, ierr)
    !! minority species <Eperp>, lab frame
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
    call ccps_argcheck('psget_eperp_mini', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1, 1: n2) = ss % eperp_mini
end subroutine psget_eperp_mini


subroutine psset_eperp_mini(iobj, n1, n2, data, ierr)
    !! minority species <Eperp>, lab frame
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
    call ccps_argcheck('psset_eperp_mini', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % eperp_mini = data(1:n1, 1: n2)
end subroutine psset_eperp_mini


subroutine psgetrnk_eperp_mini(iobj, ndims, ierr)
    !! minority species <Eperp>, lab frame
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
end subroutine psgetrnk_eperp_mini


subroutine psgetdim_eperp_mini(iobj, ndims, dims, ierr)
    !! minority species <Eperp>, lab frame
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
    call ccps_argcheck('psgetdim_eperp_mini', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % eperp_mini ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % eperp_mini)
    endif
end subroutine psgetdim_eperp_mini


subroutine psget_epll_rw(iobj, n1, data, ierr)
    !! runaway electron <Epll>
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
    call ccps_argcheck('psget_epll_rw', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data(1:n1) = ss % epll_rw
end subroutine psget_epll_rw


subroutine psset_epll_rw(iobj, n1, data, ierr)
    !! runaway electron <Epll>
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
    call ccps_argcheck('psset_epll_rw', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % epll_rw = data(1:n1)
end subroutine psset_epll_rw


subroutine psgetrnk_epll_rw(iobj, ndims, ierr)
    !! runaway electron <Epll>
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
end subroutine psgetrnk_epll_rw


subroutine psgetdim_epll_rw(iobj, ndims, dims, ierr)
    !! runaway electron <Epll>
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
    call ccps_argcheck('psgetdim_epll_rw', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    dims = 0
    if (allocated( ss % epll_rw ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss % epll_rw)
    endif
end subroutine psgetdim_epll_rw

