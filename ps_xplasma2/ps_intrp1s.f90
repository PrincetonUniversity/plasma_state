subroutine ps_intrp1s(nvec,xvec,nids,ids,iobj,iderivs,iccw_th,ansa,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 13 Oct 2006
  !
  ! XPLASMA2 implementation of plasma_state subroutine
  ! interpolate a set of 1d profiles to a vector of target points,
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
  REAL(KIND=rspec), intent(in) :: xvec(nvec)  ! interpolation target vector

  integer, intent(in) :: nids  ! number of profiles to evaluate
  integer, intent(in) :: ids(nids)  ! the profile IDs

  integer :: iobj(ps_container_size)     ! xplasma opaque container

  integer, intent(in) :: iderivs(nids) ! derivative control:
  !  0 for profile value; 1 for d[profile]/dx; 2 for d2[profile]/dx2
  !  separately provided for each profile

  integer, intent(in) :: iccw_th  ! poloidal angle orientation control (CCW)

  REAL(KIND=rspec), intent(out) :: ansa(nvec,nids)  ! interpolation results

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  !-----------------------
  integer :: ii,irank,igrid,jgrid,icoord,ispline,jspline
  logical :: jccw
  character*32 :: profname1,profname2

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_intrp1s: invalid or unitialized xplasma object.'
     return
  endif

  !  look to 1st profile ID to define grids and interpolation type

  call xplasma_prof_info(s,ids(1),ierr, name=profname1, gridId1=igrid, &
       splineType = ispline, rank=irank)
  
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp1s: 1st xplasma_prof_info call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

  if(irank.ne.1) then
     write(iout,*) ' ?ps_intrp1: profile id=',ids(1), &
          ' name="',trim(profname1),'" has rank: ',irank
     write(iout,*) '  This profile cannot be evaluated by 1d interpolation.'
     ierr=77
     return
  endif

  call xplasma_grid_info(s,igrid,ierr, coord=icoord)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp1s: xplasma_grid_info call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

  !  check for theta reversal request

  if(icoord.eq.xplasma_theta_coord) then
     jccw = (iccw_th.ne.ps_ccw_reverse)
  else
     jccw = .TRUE.
  endif

  !  check that all additional profiles are on the same grid and of same
  !  rank and spline type...

  do ii=2,nids

     call xplasma_prof_info(s,ids(ii),ierr, name=profname2, gridId1=jgrid, &
          splineType = jspline, rank=irank)
  
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_intrp1s: 2nd xplasma_prof_info call failed:'
        call xplasma_error(s,ierr,iout)
        ierr=77
        return
     endif

     if(irank.ne.1) then
        write(iout,*) ' ?ps_intrp1: profile id=',ids(ii), &
             ' name="',trim(profname2),'" has rank: ',irank
        write(iout,*) '  This profile cannot be evaluated by 1d interpolation.'
        ierr=77
        return
     endif

     if(igrid.ne.jgrid) then
        write(iout,*) ' ?ps_intrp1s: profiles "', &
             trim(profname1),'" and "',trim(profname2),'" are'
        write(iout,*) '  not defined over the same grids-- use separate'
        write(iout,*) '  interpolation calls.'
        ierr=77
        return
     endif

     if(ispline.ne.jspline) then
        write(iout,*) ' ?ps_intrp1s: profiles "', &
             trim(profname1),'" and "',trim(profname2),'" are'
        write(iout,*) '  employ different interpolation methods-- use separate'
        write(iout,*) '  interpolation calls.'
        ierr=77
        return
     endif

  enddo

  call xplasma_eval_prof(s,ids,xvec,ansa,ierr, &
       ideriv1s=iderivs,ccwflag1=jccw)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_intrp1s: xplasma_eval_prof call failed:'
     call xplasma_error(s,ierr,iout)
     return
  endif

end subroutine ps_intrp1s
