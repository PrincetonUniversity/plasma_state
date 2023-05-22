subroutine ps_intrp2s(nvec,xvec1,xvec2,nids,ids,iobj, &
     ideriv1s,ideriv2s,iccw_th,ansa,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 13 Oct 2006
  !
  ! XPLASMA2 implementation of plasma_state subroutine
  ! interpolate a set of 2d profiles to a vector of target points,
  !   returning a set of vector results.
  !
  ! All profiles must be defined over the same set of grids and 
  ! all must be interpolated by the same method.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer, intent(in) :: nvec  ! size of interpolation target vector
  REAL(KIND=rspec), intent(in) :: xvec1(nvec),xvec2(nvec)
                               ! interpolation target vector

  integer, intent(in) :: nids  ! number of profiles to evaluate
  integer, intent(in) :: ids(nids)  ! the profile IDs

  integer :: iobj(ps_container_size)     ! xplasma opaque container

  integer, intent(in) :: ideriv1s(nids),ideriv2s(nids) ! derivative control:
  !  0 for profile value; 1 for d[profile]/dx; 2 for d2[profile]/dx2
  !  separately provided for each profile & each coordinate

  integer, intent(in) :: iccw_th  ! poloidal angle orientation control (CCW)

  REAL(KIND=rspec), intent(out) :: ansa(nvec,nids)  ! interpolation results

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  !-----------------------
  integer :: ii,irank,igrid1,igrid2,jgrid1,jgrid2,icoord1,icoord2
  integer :: ispline,jspline
  logical :: jccw1,jccw2
  character*32 :: profname1,profname2

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_intrp2s: invalid or unitialized xplasma object.'
     return
  endif

  !  look to 1st profile ID to define grids and interpolation type

  call xplasma_prof_info(s,ids(1),ierr, name=profname1, gridId1=igrid1, &
       gridId2=igrid2, splineType = ispline, rank=irank)
  
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp2s: 1st xplasma_prof_info call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

  if(irank.ne.2) then
     write(iout,*) ' ?ps_intrp2: profile id=',ids(1), &
          ' name="',trim(profname1),'" has rank: ',irank
     write(iout,*) '  This profile cannot be evaluated by 2d interpolation.'
     ierr=77
     return
  endif

  call xplasma_grid_info(s,igrid1,ierr, coord=icoord1)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp2s: xplasma_grid_info call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

  call xplasma_grid_info(s,igrid2,ierr, coord=icoord2)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp2s: xplasma_grid_info call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

  !  check for theta reversal request

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

  !  check that all additional profiles are on the same grid and of same
  !  rank and spline type...

  do ii=2,nids

     call xplasma_prof_info(s,ids(ii),ierr, name=profname2, gridId1=jgrid1, &
          gridId2=jgrid2, splineType = jspline, rank=irank)
  
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_intrp2s: 2nd xplasma_prof_info call failed:'
        call xplasma_error(s,ierr,iout)
        ierr=77
        return
     endif

     if(irank.ne.2) then
        write(iout,*) ' ?ps_intrp2: profile id=',ids(ii), &
             ' name="',trim(profname2),'" has rank: ',irank
        write(iout,*) '  This profile cannot be evaluated by 1d interpolation.'
        ierr=77
        return
     endif

     if((igrid1.ne.jgrid1).or.(igrid2.ne.jgrid2)) then
        write(iout,*) ' ?ps_intrp2s: profiles "', &
             trim(profname1),'" and "',trim(profname2),'" are'
        write(iout,*) '  not defined over the same grids-- use separate'
        write(iout,*) '  interpolation calls.'
        ierr=77
        return
     endif

     if(ispline.ne.jspline) then
        write(iout,*) ' ?ps_intrp2s: profiles "', &
             trim(profname1),'" and "',trim(profname2),'" are'
        write(iout,*) '  employ different interpolation methods-- use separate'
        write(iout,*) '  interpolation calls.'
        ierr=77
        return
     endif

  enddo

  call xplasma_eval_prof(s,ids,icoord1,xvec1,icoord2,xvec2,ansa,ierr, &
       ideriv1s=ideriv1s,ideriv2s=ideriv2s,ccwflag1=jccw1,ccwflag2=jccw2)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp2s: xplasma_eval_prof call failed:'
     call xplasma_error(s,ierr,iout)
     return
  endif

end subroutine ps_intrp2s
