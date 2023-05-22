subroutine ps_rezone_rho(ss,nrho,rho,nids,ids,idws,curdens,nonorm,zsmoo, &
     ansa,dvola,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 16 Oct 2006
  !
  ! XPLASMA2 implementation of plasma_state subroutine
  !   rezone a list of profiles such that their (possibly weighted) 
  !   volume- or area- integrals are conserved.
  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod
  implicit NONE

  !-----------------------
  !  passed arguments:

  type (plasma_state) :: ss              ! object containing profile to rezone

  integer, intent(in) :: nrho  ! size of target rho grid -- rho BDYs
  REAL(KIND=rspec), intent(in) :: rho(nrho)  ! target rho grid -- BDYs

  integer, intent(in) :: nids  ! number of profiles to evaluate
  integer, intent(in) :: ids(nids)  ! the profile IDs
  integer, intent(in) :: idws(nids) ! weighting profile IDs (0 for no profile)

  logical :: curdens         ! .TRUE. for AREA instead of VOLUME normalization

  logical :: nonorm          ! for quantities which are densities, undo the
  !  density normalization, if nonorm=.TRUE.; for averaged value profiles
  !  (Temperatures, average energies, rotation velocities) this switch has
  !  no effect.

  logical :: zsmoo           ! .TRUE. for integral smoothing operator

  REAL(KIND=rspec), intent(out) :: ansa(nrho-1,nids)  ! interpolation results

  REAL(KIND=rspec), intent(out) :: dvola(nrho-1)      ! zone volumes (or areas)

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  !-----------------------
  integer :: ii,irank,igrid,igrid2,jgrid,jgrid2,icoord,idv,id_intg
  logical :: jccw
  character*32 :: profname1,profname2
  REAL(KIND=rspec) bdytol
  REAL(KIND=rspec), dimension(:), allocatable :: zvol,zarea
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  xobj = transfer(ss%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_rezone_rho: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_global_info(s,ierr, bdytol = bdytol)

  if((abs(rho(1)).gt.bdytol).or. &
       (abs(rho(nrho)-1.0_rspec).gt.bdytol)) then
     write(iout,*) ' ?ps_rho_rezone: input rho grid does not span [0:1]:'
     write(iout,*) '  rho(1) = ',rho(1)
     write(iout,*) '  rho(',nrho,') = ',rho(nrho)
     ierr = 999
     return
  endif

  !  look to 1st profile ID to define grids and interpolation type

  call xplasma_prof_info(s,ids(1),ierr, name=profname1, gridId1=igrid, &
       gridId2=igrid2, rank=irank)
  
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_rezone_rho: 1st xplasma_prof_info call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

  if(irank.gt.2) then
     write(iout,*) ' ?ps_rezone_rho: profile id=',ids(1), &
          ' name="',trim(profname1),'" has rank: ',irank
     write(iout,*) '  rank too high:'
     write(iout,*) '  This profile not eligible for rho rezone operation.'
     ierr=77
     return
  endif

  call xplasma_grid_info(s,igrid,ierr, coord=icoord)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_rezone_rho: xplasma_grid_info call failed:'
     call xplasma_error(s,ierr,iout)
     ierr=77
     return
  endif

  if(.not.((icoord.eq.xplasma_theta_coord).or. &
       (icoord.eq.xplasma_rho_coord))) then
     write(iout,*) ' ?ps_rezone_rho: profile id=',ids(1), &
          ' name="',trim(profname1),'" defined over'
     write(iout,*) '  grid other than rho or theta.'
     write(iout,*) '  This profile not eligible for rho rezone operation.'
     ierr=77
     return
  endif

  if(irank.eq.1) then

     if(.not.(icoord.eq.xplasma_rho_coord)) then
        write(iout,*) ' ?ps_rezone_rho: rank 1 profile id=',ids(1), &
             ' name="',trim(profname1),'" defined over'
        write(iout,*) '  grid other than rho.'
        write(iout,*) '  This profile not eligible for rho rezone operation.'
        ierr=77
        return
     endif

  else if(irank.eq.2) then

     call xplasma_grid_info(s,igrid2,ierr, coord=icoord)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_rezone_rho: xplasma_grid_info call failed:'
        call xplasma_error(s,ierr,iout)
        ierr=77
        return
     endif

     if(.not.((icoord.eq.xplasma_theta_coord).or. &
          (icoord.eq.xplasma_rho_coord))) then
        write(iout,*) ' ?ps_rezone_rho: profile id=',ids(1), &
             ' name="',trim(profname1),'" defined over'
        write(iout,*) '  grid other than rho or theta.'
        write(iout,*) '  This profile not eligible for rho rezone operation.'
        ierr=77
        return
     endif
  endif

  !  check that all additional profiles are on the same grid and of same
  !  rank and spline type...

  do ii=2,nids

     call xplasma_prof_info(s,ids(ii),ierr, name=profname2, gridId1=jgrid, &
          gridId2=jgrid2)
  
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_rezone_rho: 2nd xplasma_prof_info call failed:'
        call xplasma_error(s,ierr,iout)
        ierr=77
        return
     endif

     if((igrid.ne.jgrid).or.(igrid2.ne.jgrid2)) then
        write(iout,*) ' ?ps_rezone_rho: profiles "', &
             trim(profname1),'" and "',trim(profname2),'" are'
        write(iout,*) '  not defined over the same grids-- use separate'
        write(iout,*) '  interpolation calls.'
        ierr=77
        return
     endif

  enddo

  !  make sure the normalizing profile is defined...

  call xplasma_blkbxId(s,'__RZ_RHO_INTEGRATOR',id_intg)

  idv=0
  if(curdens) then
     allocate(zarea(nrho))
     if(id_intg.gt.0) then
        call xplasma_area(s,rho,zarea,ierr)
        if(ierr.ne.0) then
           write(iout,*) ' ?ps_rezone_rho: xplasma_area call error:'
           call xplasma_error(s,ierr,iout)
        else
           call xplasma_profId(s,'XPLASMA_AREA',idv)
        endif
     else if(ss%id_area.gt.0) then
        call xplasma_eval_prof(s,ss%id_area,rho,zarea,ierr)
        if(ierr.ne.0) then
           write(iout,*) ' ?ps_rezone_rho: area profile interpolation error:'
           call xplasma_error(s,ierr,iout)
        else
           idv=ss%id_area
        endif
     endif
     if(ierr.eq.0) then
        do ii=1,nrho-1
           dvola(ii)=zarea(ii+1)-zarea(ii)
        enddo
     endif
     deallocate(zarea)

  else
     allocate(zvol(nrho))
     if(id_intg.gt.0) then
        call xplasma_volume(s,rho,zvol,ierr)
        if(ierr.ne.0) then
           write(iout,*) ' ?ps_rezone_rho: xplasma_volume call error:'
           call xplasma_error(s,ierr,iout)
        else
           call xplasma_profId(s,'XPLASMA_VOLUME',idv)
        endif
     else if(ss%id_vol.gt.0) then
        call xplasma_eval_prof(s,ss%id_vol,rho,zvol,ierr)
        if(ierr.ne.0) then
           write(iout,*) ' ?ps_rezone_rho: volume profile interpolation error:'
           call xplasma_error(s,ierr,iout)
        else
           idv=ss%id_vol
        endif
     endif
     if(ierr.eq.0) then
        do ii=1,nrho-1
           dvola(ii)=zvol(ii+1)-zvol(ii)
        enddo
     endif
     deallocate(zvol)

  endif
  if(ierr.ne.0) return

  if(irank.eq.1) then

     call xplasma_avg1d(s,rho,idv,ids,ansa,ierr,idw=idws,ismoo=zsmoo)
     if(ierr.eq.0) then
        !  undo volume/area normalization if switch is set
        if(nonorm) then
           do ii=1,nids
              if(idws(ii).eq.0) then
                 ansa(1:nrho-1,ii)=ansa(1:nrho-1,ii)*dvola
              endif
           enddo
        endif
     else
        write(iout,*) ' ?ps_rezone_rho: xplasma_avg1d error:'
        call xplasma_error(s,ierr,iout)
        ansa=0
     endif

  else

     ierr=999
     write(iout,*) ' ?? ps_rezone: rank > 1 not implemented yet.'

  endif

end subroutine ps_rezone_rho

subroutine ps_rezone_chk(ss,id_intg,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 16 Oct 2006
  !
  ! XPLASMA2 implementation of plasma_state subroutine
  !   rezone a list of profiles such that their (possibly weighted) 
  !   volume- or area- integrals are conserved.
  !
  ! Check if rezone operation is available:
  !   If so, silently set id_intg to a positive number
  !   If not, print a brief warning and set id_intg = 0.
  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod
  implicit NONE

  !-----------------------
  !  passed arguments:

  type (plasma_state) :: ss       ! object to check

  integer, intent(out) :: id_intg ! integrator ID (if exists) or 0

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  !-----------------------
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  xobj = transfer(ss%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_rezone_chk: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_blkbxId(s,'__RZ_RHO_INTEGRATOR',id_intg)

  if(id_intg.eq.0) then
     if((ss%id_vol.eq.0).or.(ss%id_area.eq.0)) then
        write(iout,*) &
             ' %ps_rezone_chk: no rezone operator available (no MHD equilibrium).'
     else
        id_intg=10000000  ! rezones can proceed
     endif
  endif

end subroutine ps_rezone_chk
