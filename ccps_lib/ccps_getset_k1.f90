
! written by plasma_cwrite.py
! input: plasma_state_spec.dat   version 3.000
! *** do not edit *** (use code generator instead)


  subroutine ccps_getset_k1_dummy
     return
  end subroutine ccps_getset_k1_dummy

subroutine psget_kdens_rfmin(iobj, data, ierr)
    !! = "fraction" => nmini(i) = fracmin(i)*ne, = any other value
    !! (e.g. "data" or "model") then, the minority density is set by
    !! some other, means (e.g. read in from another data source).
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
    call ccps_argcheck('psget_kdens_rfmin', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    data = ss % kdens_rfmin
end subroutine psget_kdens_rfmin


subroutine psset_kdens_rfmin(iobj, data, ierr)
    !! = "fraction" => nmini(i) = fracmin(i)*ne, = any other value
    !! (e.g. "data" or "model") then, the minority density is set by
    !! some other, means (e.g. read in from another data source).
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
    call ccps_argcheck('psset_kdens_rfmin', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! set
    ss % kdens_rfmin = data
end subroutine psset_kdens_rfmin


subroutine psgetsz_kdens_rfmin(iobj, size, ierr)
    !! = "fraction" => nmini(i) = fracmin(i)*ne, = any other value
    !! (e.g. "data" or "model") then, the minority density is set by
    !! some other, means (e.g. read in from another data source).
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
    call ccps_argcheck('psgetsz_kdens_rfmin', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr % ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if kdens_rfmin is an array)
    size = len(ss % kdens_rfmin)
end subroutine psgetsz_kdens_rfmin

