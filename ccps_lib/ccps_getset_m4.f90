
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 2.056
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_m4_dummy
     return
  end subroutine ccps_getset_m4_dummy

subroutine psget_mhd_eq_status(iobj, data, ierr)
    !! 0: OK; =1: MHD equilibrium refinement failed, if this is
    !! non-zero, the entire LMHD calculation is abandoned!!
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(out)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psget_mhd_eq_status', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % MHD_eq_status
end subroutine psget_mhd_eq_status


subroutine psset_mhd_eq_status(iobj, data, ierr)
    !! 0: OK; =1: MHD equilibrium refinement failed, if this is
    !! non-zero, the entire LMHD calculation is abandoned!!
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
     ! list of dimensions (if any)
    INTEGER, intent(in)  :: data ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('psset_mhd_eq_status', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % MHD_eq_status = data
end subroutine psset_mhd_eq_status

