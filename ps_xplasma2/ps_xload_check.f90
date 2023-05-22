subroutine ps_xload_check(ss,iout,icheck,ierr)

  ! load xplasma from state structure; only load items that changed
  ! or are not found in the xplasma object (small items may be loaded
  ! regardless).  Check for rebuild of equilibrium in xplasma.

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  TYPE (plasma_state) :: ss     ! state instance from which to load
  INTEGER, intent(in) :: iout   ! I/O channel for messages

  INTEGER, intent(in) :: icheck ! =0 to SKIP equilibrium check
                                ! =1 to perform all checks
                                ! =2 to perform core equilibrium check but
                                !    skip free boundary "RZ" check.

  INTEGER, intent(out) :: ierr  ! completion status (0=OK)

  !-----------------------------
  INTEGER :: iwarn,iertmp,id_lim,kccw_Jphi,kccw_Bphi,icount,id_hash
  INTEGER :: id_rho,id_th,id_Rgrid,id_Zgrid,id_R,id_Z,id_prof,id_psi,id
  INTEGER :: id_g,id_Bphi
  INTEGER :: ict,ic2,idiff,im
  LOGICAL :: plasma_state_hash_match, ichanged
  LOGICAL :: enough_eq, enough_eqRZ, enough_RZsurf
  LOGICAL :: ichange_eq, ichange_eqRZ, ichange_RZsurf, ichange_flag
  LOGICAL :: mdescr_change, config_change, simcon_change,inew
  CHARACTER*32 :: rlabel
  REAL*8, dimension(:,:), allocatable :: Rtest,Ztest
  REAL*8, dimension(:,:,:), allocatable :: Rderivs,Zderivs
  REAL*8, parameter :: ZERO = 0.0_rspec

  real*8, parameter ::  zvar = 0.00010_rspec  ! see also: ps_update_mhdeq.f90...

  integer, dimension(:), pointer :: ia_ptr
  real*8, dimension(:), pointer :: r8a_ptr

  integer :: ii,jj
  logical :: izero  ! flag Psi(R,Z)=0 everywhere

  integer :: loc_cclist(ps_ccount)
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------------

  external eqm_bpsi, eqm_brz_adhoc

  !=====================================================================
  !  check that state has associated xplasma object; set pointer.

  if(ps_debug.ge.2) then
     write(iout,*) ' Entering: ps_xload_check; ss%global_label = "'// &
          trim(ss%global_label)//'".'
  endif

  ichange_flag = .FALSE.

  do
     call ps_tagchk('ps_xload_check',ss%itag,iout,ierr)
     if(ierr.ne.0) exit

     xobj = transfer(ss%iobj,xobj)
     s => xobj%s_xpobj

     call xplasma_ajac_maxvar_set(s,zvar,ierr)
     if(ierr.ne.0) exit

     ! probe xplasma contents

     call xplasma_profId(s,'G',id_g)
     call xplasma_profId(s,'Bphi',id_Bphi)

     !---------------------------------------
     !  step 0.  Make sure all state elements are allocated.
     !           Update active_dims count.

     if(ps_debug.ge.2) write(iout,*) ' call plasma_state_empty_alloc()...'
     call plasma_state_empty_alloc(ss,ierr); call ckerr(ierr)
     if(ierr.ne.0) exit

     !---------------------------------------
     !  step 1.  Update state hash codes; this gives summary information
     !           on what has changed since the state was read or last copied
     !           or stored.

     if(ps_debug.ge.2) write(iout,*) ' call plasma_state_hash_code()...'
     call plasma_state_hash_code(ss,ps_cc_all,ierr); call ckerr(ierr)
     if(ierr.ne.0) exit

     if((ss%nrho_eq.eq.0).or.(ss%nth_eq.eq.0)) call ps_reset_mhdeq_hash(ss)

     mdescr_change = &
          .not.plasma_state_hash_match(ss,ps_shash_machine_description,iertmp)

     config_change = &
          .not.plasma_state_hash_match(ss,ps_shash_shot_configuration,iertmp)

     simcon_change = &
          .not.plasma_state_hash_match(ss,ps_shash_simulation_init,iertmp)

     if(mdescr_change) then
        if(ps_debug.ge.2) write(iout,*) '...changed machine description detected.'
     endif

     if(config_change) then
        if(ps_debug.ge.2) write(iout,*) '...changed shot configuration detected.'
     endif

     if(simcon_change) then
        if(ps_debug.ge.2) write(iout,*) '...changed simulation initialization data detected.'
     endif

     !---------------------------------------
     !  step 2.  Fetch or create xplasma hash array

     call xpobj_blkbx(xobj, &
          xpobj_hash_name,ps_hash_total,r8hash_size,inew,id_hash, &
          ia_ptr,r8a_ptr,iout,ierr)

     if(inew) then
        ia_ptr = ps_unInit
        r8a_ptr = 1.0_rspec
     endif

     if(icheck.eq.0) exit

     !---------------------------------------
     !  step 3.  Update equilibrium related quantities as needed.
     !
     !  First determine if there is sufficient data to build an equilibrium

     if(ps_debug.ge.2) write(iout,*) ' check equilibrium data...'

     enough_eqRZ = .FALSE.    ! core and Psi(R,Z) and limiter
     enough_eq = .FALSE.      ! core equilibrium only
     enough_RZsurf = .FALSE.  ! {(R,Z) vs. (rho,theta)} surfaces only

     do
        ! flux coordinate grids (test if allocated)
        rlabel='(rho,theta) grid'
        if(.not.allocated(ss%rho_eq)) exit
        if(.not.allocated(ss%th_eq)) exit

        ! flux surfaces
        rlabel='R_geo profile'
        if(ss%new_hash_codes(ps_phash_R_geo).lt.0) exit
        rlabel='Z_geo profile'
        if(ss%new_hash_codes(ps_phash_Z_geo).lt.0) exit

        rlabel='dR_geo_drho profile'
        if(ss%new_hash_codes(ps_phash_dR_geo_drho).lt.0) exit
        rlabel='dZ_geo_drho profile'
        if(ss%new_hash_codes(ps_phash_dZ_geo_drho).lt.0) exit

        rlabel='dR_geo_dth profile'
        if(ss%new_hash_codes(ps_phash_dR_geo_dth).lt.0) exit
        rlabel='dZ_geo_dth profile'
        if(ss%new_hash_codes(ps_phash_dZ_geo_dth).lt.0) exit

        rlabel='d2R_geo_drhodth profile'
        if(ss%new_hash_codes(ps_phash_d2R_geo_drhodth).lt.0) exit
        rlabel='d2Z_geo_drhodth profile'
        if(ss%new_hash_codes(ps_phash_d2Z_geo_drhodth).lt.0) exit

        enough_RZsurf = .TRUE.  ! flux surface data exists 

        ! field and current orientation?
        rlabel='Bphi ccw orientation'
        if(ss%kccw_Bphi.eq.0) exit
        rlabel='Jphi ccw orientation'
        if(ss%kccw_Jphi.eq.0) exit

        ! profiles (test if anywhere non-zero)
        ! in 1d profiles check skip P_eq: it could be zero.

        rlabel='q_eq profile'
        if(ss%new_hash_codes(ps_phash_q_eq).lt.0) exit
        rlabel='g_eq profile'
        if(ss%new_hash_codes(ps_phash_g_eq).lt.0) exit

        rlabel='phit profile'
        if(ss%new_hash_codes(ps_phash_phit).lt.0) exit
        rlabel='psipol profile'
        if(ss%new_hash_codes(ps_phash_psipol).lt.0) exit

        ! OK
        enough_eq = .TRUE.

        ! (R,Z) grids (test if allocated)
        rlabel='(R,Z) grids'
        if(.not.allocated(ss%R_grid)) exit
        if(.not.allocated(ss%Z_grid)) exit

        ! limiter?
        rlabel = 'limiter'
        if(ss%num_rzlim.eq.0) exit
        ! non-zero so arrays were allocated at step 0.
        if(maxval(abs(ss%rlim)).eq.0.0_rspec) exit
        if(maxval(abs(ss%zlim)).eq.0.0_rspec) exit

        enough_eqRZ = .TRUE.

        ! Psi(R,Z) and (R,Z) grids not checked yet.
        exit
     enddo

     if(enough_eq) then
        ! enough data exists to define core equilibrium at least; now see if 
        ! anything changed s.t. derived quantities actually need to be updated.

        if(ps_debug.ge.2) write(iout,*) &
             'ps_xload_check:  found sufficient data for MHD equilibrium.'

        ict = 0
        ic2 = 0

        ichange_RZsurf = .TRUE.    ! initially assume surface change

        do

           ! check flux coordinate grids
           call chk_fluxgrids
           if((id_rho.eq.0).or.(id_th.eq.0)) then
              ict = ict + 1
              exit
           endif

           call xplasma_profId(s,'R',id_R)
           call xplasma_profId(s,'Z',id_Z)
           if((ps_debug.ge.2).and.((id_R.eq.0).or.(id_Z.eq.0))) write(iout,*) &
                '...equilibrium flux surfaces new for xplasma.'

           if((id_R.eq.0).or.(id_Z.eq.0)) exit

           call chk_surfaces
           if(ierr.ne.0) exit           
           if(ichanged) exit

           ! the data is all there and it is unchanged...

           ichange_RZsurf = .FALSE.
           exit
        enddo
        if(ierr.ne.0) exit

        ichange_eq = .TRUE.       ! initially assume data changed

        if(enough_eqRZ) then
           ichange_eqRZ = .TRUE.  ! initially assume data changed
        else
           ichange_eqRZ = .FALSE. ! data is not there
        endif

        do

           if(enough_eqRZ) then

              ! check limiter
              call cklim
              if(id_lim.eq.0) then
                 if(ps_debug.ge.2) write(iout,*) &
                      '...axisymmetric limiter data new for xplasma.'
                 ict = ict + 1
              endif

              ! check (R,Z) grids

              call ckgrid(ss%R_grid,'__RGRID',id_Rgrid,0)
              if((ps_debug.ge.2).and.(id_Rgrid.eq.0)) write(iout,*) &
                   '...psi(R,Z) "R" grid new for xplasma.'

              call ckgrid(ss%Z_grid,'__ZGRID',id_Zgrid,0)
              if((ps_debug.ge.2).and.(id_Zgrid.eq.0)) write(iout,*) &
                   '...psi(R,Z) "Z" grid new for xplasma.'

              if((id_Rgrid.eq.0).or.(id_Zgrid.eq.0)) ic2 = ic2 + 1

           endif

           ! check field orientation (check against 0 already was done)

           if((abs(ss%kccw_Bphi).ne.1).or.(abs(ss%kccw_Jphi).ne.1)) then
              write(iout,*) &
                   ' ?ps_xload_check: kccw factors must be +/-1, but found:'
              write(iout,*) '  ss%kccw_Jphi = ',ss%kccw_Jphi
              write(iout,*) '  ss%kccw_Bphi = ',ss%kccw_Bphi
              ierr=9999
              exit
           endif

           call xplasma_global_info(s,ierr, &
                Bphi_ccw=kccw_Bphi, Jphi_ccw=kccw_Jphi); call xckerr(ierr)
           if(ierr.ne.0) exit
           if((kccw_Bphi.ne.ss%kccw_Bphi).or. &
                (kccw_Jphi.ne.ss%kccw_Jphi)) then
              if(ps_debug.ge.2) write(iout,*) &
                   '...field orientation data new for xplasma.'
              exit
           endif

           ! exit now if there were limiter or flux coordinate grid changes:
           ! entire set of equilibrium splines will have to be recomputed.
           if(enough_eqRZ.and.(ict.gt.0)) exit 

           ! equilibrium may be OK as is:
           ! checked for change in flux surface geometry (see prior loop)

           if(ichange_RZsurf) then
              if(ps_debug.ge.2) write(iout,*) '...change was detected in flux surface geometry...'
              exit
           endif

           call ckprof(ss%g_eq,'G',id_prof)
           if(id_prof.eq.0) then
              if(ps_debug.ge.2) write(iout,*) '...G profile change.'
              exit
           else if(id_g.eq.0) then
              if(ps_debug.ge.2) write(iout,*) '...xplasma G profile restore needed.'
              exit
           else if(id_Bphi.eq.0) then
              if(ps_debug.ge.2) write(iout,*) '...xplasma Bphi profile restore needed.'
              exit
           endif

           call ckprof(ss%q_eq,'q',id_prof)
           if(id_prof.eq.0) then
              if(ps_debug.ge.2) write(iout,*) '...q profile change.'
              exit
           endif

           call ckprof(ss%P_eq,'P',id_prof)
           if(id_prof.eq.0) then
              if(ps_debug.ge.2) write(iout,*) '...P profile change.'
              exit
           endif

           call ckprof(ss%phit,'PHITOR',id_prof)
           if(id_prof.eq.0) then
              if(ps_debug.ge.2) write(iout,*) &
                   '...Toroidal flux profile change.'
              exit
           endif

           call ckprof(ss%psipol,'PSI',id_prof)
           if(id_prof.eq.0) then
              if(ps_debug.ge.2) write(iout,*) &
                   '...Poloidal flux profile change.'
              exit
           endif

           ! OK: limiter, orientation, (rho,theta) grids, and flux surfaces
           ! and core profiles are unchanged...

           ichange_eq = .FALSE.
           
           ! exit now if (R,Z) grids changed or are missing
           if(enough_eqRZ.and.(ic2.gt.0)) exit
           if(.not.enough_eqRZ) exit

           ! check Psi(R,Z)
           call xplasma_profId(s,'PSI_RZ',id_prof)
           if(id_prof.eq.0) then
              idiff=1
              ierr=0
           else
              call xplasma_diff_prof(s,id_prof,ss%psiRZ,idiff,ierr, &
                   splineType=1)
           endif
           call xckerr(ierr)
           if(ierr.ne.0) exit

           ichanged = (idiff.ne.0)
           if(ichanged) then
              ichange_eq = .TRUE.
              if(ps_debug.ge.2) write(iout,*) '...detected psi(R,Z) change.'
              exit
           endif
                
           ichange_eqRZ = .FALSE.  ! data is present but unchanged

           exit
        enddo
        if(ierr.ne.0) exit

        if(ichange_eq.or.ichange_eqRZ) then
           ichange_flag=.TRUE.
           call xplasma_author_set(s,'EQ',ierr); call xckerr(ierr)
           if(ierr.ne.0) exit
        endif

        if(ichange_eq.or.ichange_eqRZ) then

           ! rebuild xplasma equilibrium representation

           if(ichange_eqRZ) then
              if(id_lim.eq.0) then
                 !  Limiter...
                 if(ps_debug.ge.2) write(iout,*) '...loading limiter data...'
                 call xplasma_mklim_contour(s, &
                      ss%rlim(1:ss%num_rzlim),ss%zlim(1:ss%num_rzlim),iwarn, &
                      ierr)
                 call xckerr(ierr)
                 if(ierr.ne.0) exit
              endif

              !  Save Psi0

              call xplasma_save_Psi0(s,ss%Psi_to_machine_axis,ierr)
              if(ierr.ne.0) exit

           endif

           if(ichange_RZsurf) then
              call build_surfaces
              if(ierr.ne.0) exit
           endif

           if(ichange_eq.or.ichange_RZsurf) then
              ! 1d profiles

              call xplasma_create_1dprof(s,'PSI', &
                   id_rho,ss%psipol,id_prof,ierr, &
                   ispline=2, ibca=1, zbca=ZERO, ibcb=0, &
                   label='Poloidal Flux',units='Wb/rad')
              call xckerr(ierr)
              if(ierr.ne.0) exit

              id_psi=id_prof
              ss%id_psipol=id_prof

              call xplasma_create_1dprof(s,'PHITOR', &
                   id_rho,ss%phit,id_prof,ierr, &
                   ispline=2, ibca=1, zbca=ZERO, ibcb=0, &
                   label='Toroidal Flux',units='Wb')
              call xckerr(ierr)
              if(ierr.ne.0) exit

              ss%id_phit=id_prof

              call xplasma_create_1dprof(s,'G', &
                   id_rho,ss%g_eq,id_prof,ierr, &
                   ispline=2, ibca=1, zbca=ZERO, ibcb=0, &
                   label='R*B_phi (toroidal field functino)',units='T*m')
              call xckerr(ierr)
              if(ierr.ne.0) exit

              ss%id_g_eq=id_prof

              call xplasma_create_1dprof(s,'Q', &
                   id_rho,ss%q_eq,id_prof,ierr, &
                   ispline=2, ibca=0, ibcb=0, &
                   label='q profile',units=' ')
              call xckerr(ierr)
              if(ierr.ne.0) exit

              ss%id_q_eq=id_prof

              call xplasma_create_1dprof(s,'P', &
                   id_rho,ss%P_eq,id_prof,ierr, &
                   ispline=2, ibca=1, zbca=ZERO, ibcb=0, &
                   label='MHD equilibrium pressure',units='Pa')
              call xckerr(ierr)
              if(ierr.ne.0) exit

              call xplasma_set_pressure_id(s,id_prof,ierr)
              call xckerr(ierr)
              if(ierr.ne.0) exit

              ss%id_P_eq=id_prof
 
              !  set the field and current orientations

              call xplasma_field_ccw_set(s,ierr, &
                   bphi_ccw=ss%kccw_Bphi,jphi_ccw=ss%kccw_Jphi)
              call xckerr(ierr)
              if(ierr.ne.0) exit
 
              ! ...OK, geom. and psi and g are set, compute 
              ! B,BR,BZ vs. (rho,th); the field and current orientations 
              ! have been specified.
 
              call xplasma_eqcheck(s,.FALSE.,icount,ierr)
              call xckerr(ierr)
              if(ierr.ne.0) exit

              ! update remaining derived scalars...

              call ps_mhdeq_derived_scalars(ss,iout,ierr)
              call ckerr(ierr)
              if(ierr.ne.0) exit
           endif

           ! and derived profiles...

           if(ichange_RZsurf) then
              call ps_mhdeq_volumes(ss,iout,ierr); call ckerr(ierr)
              if(ierr.ne.0) exit
              call ps_mhdeq_fsmoms(ss,iout,ierr); call ckerr(ierr)
              if(ierr.ne.0) exit
           endif

        endif

        if(ichange_eqRZ) then
           ! if this is .TRUE. then presence of sufficient data has also
           ! been checked.

           ! (R,Z) grids

           if((id_Rgrid.eq.0).or.(id_Zgrid.eq.0)) then
              call xplasma_create_RZgrid(s,ss%R_grid,ss%Z_grid, &
                   id_Rgrid,id_Zgrid,ierr)
              call xckerr(ierr)
              if(ierr.ne.0) exit

              !  update [R,Z] extrema
              call ps_update_rzbox(ss,iout,ierr)
              call ckerr(ierr)
              if(ierr.ne.0) exit
           endif
        endif

        if(icheck.eq.2) ichange_eqRZ=.FALSE.

        if(ichange_eqRZ) then

           ! Psi(R,Z)

           izero=.TRUE.
           do jj=1,ss%nZ
              do ii=1,ss%nR
                 if(ss%psiRZ(ii,jj).ne.0.0_rspec) then
                    izero=.FALSE.
                    exit
                 endif
              enddo
              if(.not.izero) exit
           enddo

           if(.not.izero) then

              ! real Psi(R,Z) was provided; use it...

              call xplasma_create_2dprof(s,'PSI_RZ', &
                   id_Rgrid,id_Zgrid,ss%psirz,id_prof,ierr, &
                   ispline=1,assoc_id=id_psi, &
                   label='Poloidal Flux',units='Wb/rad')
              call xckerr(ierr)
              if(ierr.ne.0) exit

              ss%id_psirz=id_prof

              ! B(R,Z) computed in xplasma

              call xplasma_brz(s,eqm_bpsi,ierr)
              call xckerr(ierr)
              if(ierr.ne.0) then
                 write(iout,*) '  --> error getting B(R,Z) (eqm_bpsi).'
                 exit
              endif

           else

              ! first, generate ad hoc Psi(R,Z) using smoothed extrapolation

              call xplasma_brz(s,eqm_brz_adhoc,ierr)
              call xckerr(ierr)
              if(ierr.ne.0) exit

              ! save generated Psi(R,Z) in state object

              call xplasma_profId(s,'Psi_RZ',ss%id_psirz)
              call xplasma_eval_prof(s,ss%id_psirz,ss%psirz,ierr)
              call xckerr(ierr)
              if(ierr.ne.0) then
                 write(iout,*) '  --> error getting B(R,Z) (eqm_brz_adhoc).'
                 exit
              endif

              ! now, recompute B(R,Z) based on the Psi(R,Z) just generated.
              ! this may seem redundant, but it is necessary to get 
              ! the same B(R,Z) field as would be the case if the current
              ! state were written and read back in.

              call xplasma_brz(s,eqm_bpsi,ierr)
              call xckerr(ierr)
              if(ierr.ne.0) then
                 write(iout,*) '  --> error getting B(R,Z) (eqm_bpsi).'
                 exit
              endif

           endif

           ! and update B(R,Z) component profiles in state object...

           call ps_update_brz(ss,iout,ierr)
           call ckerr(ierr)
           if(ierr.ne.0) exit

        endif

        if(ichange_flag) then
           call xplasma_author_clear(s,'EQ',ierr); call xckerr(ierr)
           call xckerr(ierr)
           if(ierr.ne.0) exit

           ! update hash codes (equilibrium data was updated).
           loc_cclist = ps_noCheck
           loc_cclist(ps_cnum_EQ)=ps_chkProfs ! update EQ prof hashcodes only
           call plasma_state_hash_code(ss,loc_cclist,ierr); call ckerr(ierr)
           if(ierr.ne.0) exit

        else
           if(ps_debug.ge.2) then
              write(iout,*) ' no change to existing MHD equilibrium.'
           endif
        endif

     else if(enough_RZsurf) then

        ! this block reached if geometry data is present but field data
        ! is missing...

        if(ps_debug.ge.2) then
           write(iout,*) &
                ' (R,Z) flux surfaces exist but equilibrium not complete:'// &
                ' missing data: '//trim(rlabel)
        endif

        ichange_eq = .TRUE.

        ! see if there are changes s.t. xplasma requires updating...
        do
           ! check flux coordinate grids
           call chk_fluxgrids
           if((id_rho.eq.0).or.(id_th.eq.0)) exit

           ! check flux surfaces

           call xplasma_profId(s,'R',id_R)
           call xplasma_profId(s,'Z',id_Z)
           if((ps_debug.ge.2).and.((id_R.eq.0).or.(id_Z.eq.0))) write(iout,*) &
                '...equilibrium flux surfaces new for xplasma.'

           if((id_R.eq.0).or.(id_Z.eq.0)) exit

           call chk_surfaces
           if(ierr.ne.0) exit           

           if(ichanged) then
              if(ps_debug.ge.2) write(iout,*) '...detected change in flux surface geometry...'
              exit
           endif

           ichange_eq=.FALSE.
           exit
        enddo
        if(ierr.ne.0) exit

        if(ichange_eq) then
           call xplasma_author_set(s,'EQ',ierr); call xckerr(ierr)
           if(ierr.ne.0) exit

           call build_surfaces
           if(ierr.ne.0) exit

           call xplasma_author_clear(s,'EQ',ierr); call xckerr(ierr)
           if(ierr.ne.0) exit

           call ps_mhdeq_volumes(ss,iout,ierr); call ckerr(ierr)
           if(ierr.ne.0) exit

           call ps_mhdeq_fsmoms(ss,iout,ierr); call ckerr(ierr)
           if(ierr.ne.0) exit

           ! update hash codes again
           loc_cclist = ps_noCheck
           loc_cclist(ps_cnum_EQ)=ps_chkProfs ! update EQ prof hashcodes only
           call plasma_state_hash_code(ss,loc_cclist,ierr); call ckerr(ierr)
           if(ierr.ne.0) exit
        else
           if(ps_debug.ge.2) then
              write(iout,*) ' no change to existing (R,Z).'
           endif
        endif

     else
        if(ps_debug.ge.2) then
           write(iout,*) ' no MHD equilibrium: missing data: '//trim(rlabel)
        endif
     endif

     !---------------------------------------
     !  step 4.  Update non-equilibrium quantities as needed.
     !           (this call moved to plasma_state_store.f90)
     !---------------------------------------
     !  ALL DONE

     exit
  enddo


CONTAINS

  subroutine chk_fluxgrids

    call ckgrid(ss%rho_eq,'__RHO',id_rho,0)
    if((ps_debug.ge.2).and.(id_rho.eq.0)) write(iout,*) &
         '...equilibrium "rho" grid new for xplasma.'

    call ckgrid(ss%th_eq,'__CHI',id_th,1)
    if((ps_debug.ge.2).and.(id_th.eq.0)) write(iout,*) &
         '...equilibrium "th" grid new for xplasma.'

  end subroutine chk_fluxgrids

  subroutine chk_surfaces

    allocate(Rtest(ss%nth_eq,ss%nrho_eq))
    allocate(Ztest(ss%nth_eq,ss%nrho_eq))
    allocate(Rderivs(3,ss%nth_eq,ss%nrho_eq))
    allocate(Zderivs(3,ss%nth_eq,ss%nrho_eq))

    do jj=1,ss%nrho_eq
       do ii=1,ss%nth_eq
          Rtest(ii,jj) = ss%R_geo(jj,ii)
          Ztest(ii,jj) = ss%Z_geo(jj,ii)
          Rderivs(1,ii,jj) = ss%dR_geo_dth(jj,ii)
          Rderivs(2,ii,jj) = ss%dR_geo_drho(jj,ii)
          Rderivs(3,ii,jj) = ss%d2R_geo_drhodth(jj,ii)
          Zderivs(1,ii,jj) = ss%dZ_geo_dth(jj,ii)
          Zderivs(2,ii,jj) = ss%dZ_geo_drho(jj,ii)
          Zderivs(3,ii,jj) = ss%d2Z_geo_drhodth(jj,ii)
       enddo
    enddo

    ! xplasma has flux surfaces... do they need to be updated?

    call xplasma_diff_prof(s,id_R,Rtest,idiff,ierr, &
         coeffs=Rderivs); call xckerr(ierr)
    if((ierr.eq.0).and.(idiff.eq.0)) then
       call xplasma_diff_prof(s,id_Z,Ztest,idiff,ierr, &
            coeffs=Zderivs); call xckerr(ierr)
    endif

    deallocate(Rtest,Ztest,Rderivs,Zderivs)

    ichanged = (idiff.ne.0)

  end subroutine chk_surfaces

  subroutine build_surfaces

    character*3 zint

    if(id_rho.eq.0) then
       !  rho grid
       call xplasma_create_grid(s,'__RHO',xplasma_rho_coord, &
            ss%rho_eq,id_rho,ierr)
       call xckerr(ierr)
       if(ierr.ne.0) return
    endif

    if(id_th.eq.0) then
       !  th grid
       call xplasma_create_grid(s,'__CHI',xplasma_theta_coord, &
            ss%th_eq,id_th,ierr)
       call xckerr(ierr)
       if(ierr.ne.0) return
    endif

    ! the flux surface geometry -- Hermite representation
    ! although the Hermite derivatives are likely from a C2 spline

    if(ss%neqmom.gt.0) ss%nmom = ss%neqmom

    call xplasma_kmom_set(s, ss%nmom, ierr)
    call xckerr(ierr)
    if(ierr.ne.0) return

    if(ss%neqmom.eq.0) then
       ss%neqmom = ss%nmom
       call plasma_state_empty_alloc(ss,ierr); call ckerr(ierr)
       if(ierr.ne.0) return
       ! set up eqmom_num labels...
       do im=1,ss%nmom
          zint=' '
          if(im.le.9) then
             write(zint(1:1),'(I1)') im
          else if(im.le.99) then
             write(zint(1:2),'(I2)') im
          else
             write(zint(1:3),'(I3)') im
          endif
          ss%eqmom_num(im)=zint
       enddo
    endif

    allocate(Rderivs(3,ss%nrho_eq,ss%nth_eq))
    allocate(Zderivs(3,ss%nrho_eq,ss%nth_eq))

    Rderivs(1,:,:) = ss%dR_geo_drho
    Rderivs(2,:,:) = ss%dR_geo_dth
    Rderivs(3,:,:) = ss%d2R_geo_drhodth

    Zderivs(1,:,:) = ss%dZ_geo_drho
    Zderivs(2,:,:) = ss%dZ_geo_dth
    Zderivs(3,:,:) = ss%d2Z_geo_drhodth

    call xplasma_rzmag(s,id_rho,id_th,ss%R_geo,ss%Z_geo, &
         id_R,id_Z,ierr, &
         lhermite=.TRUE., Rderivs=Rderivs, Zderivs=Zderivs)
    call xckerr(ierr)

    if(ierr.eq.0) then
       ! set state IDs; mark profiles as up-to-date
       ss%id_R_geo = id_R
       ss%id_Z_geo = id_Z
    endif

    deallocate(Rderivs,Zderivs)

  end subroutine build_surfaces

  subroutine xckerr(ierr)
     integer :: ierr

     if(ierr.ne.0) then
        write(iout,*) ' ?ps_xload_check: xplasma error detected.'
        call xplasma_error(s,ierr,iout)
        ierr = 9999
     endif
   end subroutine xckerr

  subroutine ckerr(ierr)
     integer :: ierr

     if(ierr.ne.0) then
        write(iout,*) ' ?ps_xload_check: error detected.'
        ierr = 9999
     endif
  end subroutine ckerr

  subroutine cklim

    !-------------
    !  check if limiter exists; and if so if it has changed.

    integer :: itype,isizo,i,iflag
    real*8, dimension(:), allocatable :: rlimo,zlimo
    !-------------

    iflag = 0

    call xplasma_find_item(s,'__LIMITER',id_lim,iertmp,nf_noerr=.TRUE.)
    if(id_lim.eq.0) return

    call xplasma_lim_info(s,iertmp,itype=itype)
    if(itype.ne.100) then
       id_lim=0
       return
    endif

    ! there is a contour limiter

    call xplasma_lim_info(s,iertmp, npts=isizo)
    if(isizo.ne.ss%num_rzlim) then
       iflag = 1   ! size mismatch
    endif

    if(iflag.eq.0) then
       allocate(rlimo(isizo),zlimo(isizo))
       call xplasma_lim_info(s,iertmp, rpts=rlimo, zpts=zlimo)

       do i=1,isizo
          if((rlimo(i).ne.ss%Rlim(i)).or.(zlimo(i).ne.ss%Zlim(i))) then
             iflag = 1   ! value mismatch
             exit
          endif
       enddo

       deallocate(rlimo,zlimo)
    endif

    if(iflag.eq.1) then
       id_lim = 0  !... limiter changed; replace it...
    endif

  end subroutine cklim

  subroutine ckgrid(zdata,zname,id,iper)

    real*8, dimension(:), intent(in) :: zdata
    character*(*), intent(in) :: zname
    integer, intent(out) :: id
    integer, intent(in) :: iper   ! =1 for periodic grid coordinate

    !-----------
    !  check if grid is already defined & identical to new grid
 
    integer :: i,isizo
    real*8 :: zgrido(size(zdata)),ztol

    real*8, parameter :: loctol = 1.0d-10
    real*8, dimension(:,:), allocatable :: xpkg
    real*8 :: zdum
    !-----------

    call xplasma_find_item(s,zname,id,iertmp,nf_noerr=.TRUE.)
    if(id.eq.0) return

    call xplasma_grid_size(s,id,isizo,iertmp)
    if(iertmp.ne.0) then
       id=0
       return
    endif

    if(isizo.ne.size(zdata)) then
       id=0
       return
    endif

    call xplasma_grid(s,id,zgrido,iertmp)
    if(iertmp.ne.0) then
       id=0
       return
    endif

    ztol=loctol*max(abs(zgrido(1)),abs(zgrido(isizo)))

    ! compare grid this way, to match grid storage logic in xplasma
    ! (genxpkg, as called, makes nearly-evenly-spaced grids exactly
    !  evenly spaced)

    zdum=0.0
    allocate(xpkg(isizo,4))
    call genxpkg(isizo,zdata,xpkg,iper,0,0,zdum,3,iertmp)
    if(iertmp.ne.0) then
       id=0
       deallocate(xpkg)
       return
    endif

    do i=1,isizo
       if(abs(zgrido(i)-xpkg(i,1)).gt.ztol) then
          id=0  ! value changed...
          exit
       endif
    enddo

  end subroutine ckgrid

  subroutine ckprof(zdata,zname,id)

    ! check for match of 1d spline profile data

    real*8, dimension(:), intent(in) :: zdata
    character*(*), intent(in) :: zname
    integer, intent(out) :: id

    !------------------
    integer :: idiff
    !------------------

    call xplasma_profId(s,zname,id)
    if(id.eq.0) return

    call xplasma_diff_prof(s,id,zdata,idiff,ierr, splineType=2)
    call xckerr(ierr)

    if(idiff.ne.0) id=0
    
  end subroutine ckprof

end subroutine ps_xload_check
     
