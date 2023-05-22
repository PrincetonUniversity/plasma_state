subroutine ccps_intrp_2d_single(iobj,id,ideriv1,ideriv2,x1val,x2val, &
     result,ierr)

  ! interpolate a 2d f(x1,x2) state element profile at a single point

  use plasma_state_mod
  implicit NONE

  integer :: iobj(*)        ! (ps_container_size) words are used
  integer, intent(in) :: id ! profile ID
  integer, intent(in) :: ideriv1   ! 0 for value, 1 for d/dx1, 2 for d2/d[x1]2
  integer, intent(in) :: ideriv2   ! 0 for value, 1 for d/dx2, 2 for d2/d[x2]2
  !   e.g. ideriv1=ideriv2=1 for cross partial: d2f/d(x1)d(x2)
  !   allowed values for ideriv1,ideriv2 depends on the ID'd profiles 
  !   method of interpolation (as defined in the state specification).

  REAL(kind=rspec), intent(in) :: x1val,x2val ! values at which to interpolate

  REAL(kind=rspec), intent(out) :: result  ! interpolation result

  integer, intent(out) :: ierr    ! status code returned, 0=normal

  !--------------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !--------------------------------

  call ccps_argcheck('ccps_intrp_2d_single',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_intrp_2d(x1val,x2val,id,result,ierr, state=ss, &
       ideriv1=ideriv1, ideriv2=ideriv2)

end subroutine ccps_intrp_2d_single
