subroutine ccps_intrp_1d_single(iobj,id,ideriv,xval,result,ierr)

  ! interpolate a 1d state element profile at a single point

  use plasma_state_mod
  implicit NONE

  integer :: iobj(*)        ! (ps_container_size) words are used
  integer, intent(in) :: id ! profile ID
  integer, intent(in) :: ideriv   ! 0 for value, 1 for d/dx, 2 for d2/dx2
  ! which are supported depends on profile interpolation method

  REAL(kind=rspec), intent(in) :: xval  ! value at which to interpolate

  REAL(kind=rspec), intent(out) :: result  ! interpolated value returned.

  integer, intent(out) :: ierr    ! status code returned, 0=normal

  !--------------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !--------------------------------

  call ccps_argcheck('ccps_intrp_1d_single',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_intrp_1d(xval,id,result,ierr, state=ss, ideriv=ideriv)

end subroutine ccps_intrp_1d_single
