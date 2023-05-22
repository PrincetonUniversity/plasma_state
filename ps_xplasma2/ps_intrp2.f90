subroutine ps_intrp2(nvec,xvec1,xvec2,id,iobj,ideriv1,ideriv2,iccw_th, &
     ansvec,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 13 Oct 2006
  !
  ! XPLASMA2 implementation of plasma_state subroutine
  ! interpolate a 2d profile to a vector of target points.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer, intent(in) :: nvec  ! size of interpolation target vector
  REAL(KIND=rspec), intent(in) :: xvec1(nvec),xvec2(nvec)
         ! interpolation target vector (2d)

  integer, intent(in) :: id    ! profile ID

  integer :: iobj(ps_container_size)     ! xplasma opaque container

  integer, intent(in) :: ideriv1,ideriv2   ! derivative control:
  !  0 for profile value; 1 for d[profile]/dx; 2 for d2[profile]/dx2
  !  1 control for each coordinate

  integer, intent(in) :: iccw_th  ! poloidal angle orientation control (CCW)

  REAL(KIND=rspec), intent(out) :: ansvec(nvec)  ! interpolation results

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  !-----------------------
  integer :: igrid1,igrid2,icoord1,icoord2,irank
  logical :: jccw1,jccw2
  character*32 :: profname

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_intrp2: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_prof_info(s,id,ierr, name=profname, gridId1=igrid1, &
       gridId2=igrid2,rank=irank)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp2: xplasma_prof_info call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

  if(irank.ne.2) then
     write(iout,*) ' ?ps_intrp2: profile id=',id,' name="',trim(profname), &
          '" has rank: ',irank
     write(iout,*) '  This profile cannot be evaluated by 2d interpolation.'
     ierr=77
     return
  endif

  call xplasma_grid_info(s,igrid1,ierr, coord=icoord1)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp2: xplasma_grid_info call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

  call xplasma_grid_info(s,igrid2,ierr, coord=icoord2)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp2: xplasma_grid_info call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

  if(icoord1.eq.xplasma_theta_coord) then
     jccw1 = (iccw_th.ne.ps_ccw_reverse)
  else
     jccw1 = .TRUE.
  endif

  if(icoord2.eq.xplasma_theta_coord) then
     jccw2 = (iccw_th.ne.ps_ccw_reverse)
  else
     jccw2 = .TRUE.
  endif

  call xplasma_eval_prof(s,id,icoord1,xvec1,icoord2,xvec2,ansvec,ierr, &
       ideriv1=ideriv1,ideriv2=ideriv2,ccwflag1=jccw1,ccwflag2=jccw2)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp2: xplasma_eval_prof call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

end subroutine ps_intrp2
