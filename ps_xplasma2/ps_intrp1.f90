subroutine ps_intrp1(nvec,xvec,id,iobj,ideriv,iccw_th,ansvec,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 13 Oct 2006
  !
  ! XPLASMA2 implementation of plasma_state subroutine
  ! interpolate a 1d profile to a vector of target points.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer, intent(in) :: nvec  ! size of interpolation target vector
  REAL(KIND=rspec), intent(in) :: xvec(nvec)  ! interpolation target vector

  integer, intent(in) :: id    ! profile ID

  integer :: iobj(ps_container_size)     ! xplasma opaque container

  integer, intent(in) :: ideriv   ! derivative control:
  !  0 for profile value; 1 for d[profile]/dx; 2 for d2[profile]/dx2

  integer, intent(in) :: iccw_th  ! poloidal angle orientation control (CCW)

  REAL(KIND=rspec), intent(out) :: ansvec(nvec)  ! interpolation results

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  !-----------------------
  integer :: igrid,icoord,irank
  logical :: jccw
  character*32 :: profname

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_intrp1: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_prof_info(s,id,ierr, name=profname, gridId1=igrid, &
       rank=irank)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp1: xplasma_prof_info call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

  if(irank.ne.1) then
     write(iout,*) ' ?ps_intrp1: profile id=',id,' name="',trim(profname), &
          '" has rank: ',irank
     write(iout,*) '  This profile cannot be evaluated by 1d interpolation.'
     ierr=77
     return
  endif

  call xplasma_grid_info(s,igrid,ierr, coord=icoord)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp1: xplasma_grid_info call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

  if(icoord.eq.xplasma_theta_coord) then
     jccw = (iccw_th.ne.ps_ccw_reverse)
  else
     jccw = .TRUE.
  endif

  call xplasma_eval_prof(s,id,xvec,ansvec,ierr, &
       ideriv1=ideriv,ccwflag1=jccw)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp1: xplasma_eval_prof call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

end subroutine ps_intrp1
