! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_copy(cclist,ss,ss2,idims_only,idata_only, &
                             icopy_lists, iwant_1deq, &
                             ierr)

  ! Copy plasma state ss -> ss2
  !   this routine ONLY copies the state object in memory
  !   a separate step is needed to deal with backing files & interpolation
  !     data.

  ! cclist(...) control specifies the components for which all grids and
  !   profiles are copied.  If cclist(j)=1 all are copied for component j.
  !   If cclist(j)=0, only scalars, species lists, and item lists are copied;
  !   profiles and grids and grid dimensions for component j are not
  !   copied in this case.

  ! if icopy_lists=0, cclist(j)=0 suppresses copy of item lists for
  ! component j, as well as profiles and grids.  Note that species lists
  ! are shared accross components; copying cannot be suppressed.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components to fully copy
                                ! use ps_cc_all to copy all components

  TYPE (plasma_state) :: ss     ! state data to copy *from*
  TYPE (plasma_state) :: ss2    ! state data to copy *to*

  INTEGER, INTENT(IN) :: idims_only  ! =1 to copy dimensions ONLY: no alloc

  INTEGER, INTENT(IN) :: idata_only  ! =1 to copy data ONLY: non-zero dims
                                     ! MUST match
                                     ! =0: dims are copied.

  INTEGER, INTENT(IN) :: icopy_lists ! =1 to require copying of all lists

  INTEGER, INTENT(IN) :: iwant_1deq  ! =1 to request fcns of (rho_eq)
                                     !  even if cclist(ps_cnum_EQ).eq.0

  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------
  INTEGER :: iout,ii,itot,isize,jsize,iertmp
  INTEGER :: ilock_mdescr,ilock_sconfig,ilock_siminit
  LOGICAL :: partial_copy
  !-------------------

  ierr = 0
  iout = SWIM_err_out_file

  IF((idata_only.EQ.1).or.(idims_only.EQ.1)) then
     partial_copy=.TRUE.
  ELSE
     partial_copy=.FALSE.
     DO ii=1,ps_ccount
        IF(ps_cc_all(ii).NE.cclist(ii)) THEN
           partial_copy=.TRUE.
        ENDIF
     ENDDO
  ENDIF

  !---------------------------------------------------
  ! copy array dimensions (grids for indicated components only...)


  IF(ALLOCATED(ss%nrz_antgeo)) THEN
     ss%max_nrz_antgeo = max(ss%max_nrz_antgeo,maxval(ss%nrz_antgeo))
  ENDIF

  IF(ALLOCATED(ss%nspectrumlh_src)) THEN
     ss%max_nspectrumlh_src = max(ss%max_nspectrumlh_src,maxval(ss%nspectrumlh_src))
  ENDIF

  IF(ALLOCATED(ss%num_nphi)) THEN
     ss%max_num_nphi = max(ss%max_num_nphi,maxval(ss%num_nphi))
  ENDIF

  IF(ALLOCATED(ss%num_nphi_vac)) THEN
     ss%max_num_nphi_vac = max(ss%max_num_nphi_vac,maxval(ss%num_nphi_vac))
  ENDIF

  IF(ALLOCATED(ss%n_straps)) THEN
     ss%max_n_straps = max(ss%max_n_straps,maxval(ss%n_straps))
  ENDIF

  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_IC).EQ.1)) &
           call copydim("max_nrz_antgeo", ss%max_nrz_antgeo,ss2%max_nrz_antgeo)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_LH).EQ.1)) &
           call copydim("max_nspectrumlh_src", ss%max_nspectrumlh_src,ss2%max_nspectrumlh_src)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_IC).EQ.1)) &
           call copydim("max_num_nphi", ss%max_num_nphi,ss2%max_num_nphi)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_IC).EQ.1)) &
           call copydim("max_num_nphi_vac", ss%max_num_nphi_vac,ss2%max_num_nphi_vac)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_IC).EQ.1)) &
           call copydim("max_n_straps", ss%max_n_straps,ss2%max_n_straps)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) &
           call copydim("mmodes", ss%mmodes,ss2%mmodes)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) &
           call copydim("nballoon", ss%nballoon,ss2%nballoon)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_NBI).EQ.1)) &
           call copydim("nbeam", ss%nbeam,ss2%nbeam)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_EQ).EQ.1)) &
           call copydim("ncircuits", ss%ncircuits,ss2%ncircuits)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_EQ).EQ.1)) &
           call copydim("ncoils", ss%ncoils,ss2%ncoils)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) &
           call copydim("ndelta_w", ss%ndelta_w,ss2%ndelta_w)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_EC).EQ.1)) &
           call copydim("necrf_src", ss%necrf_src,ss2%necrf_src)
  IF(cclist(ps_cnum_ANOM).EQ.1) &
           call copydim("nefi_anom", ss%nefi_anom,ss2%nefi_anom)
  IF(cclist(ps_cnum_ANOM).EQ.1) &
           call copydim("nefi_anom2", ss%nefi_anom2,ss2%nefi_anom2)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_EQ).EQ.1)) &
           call copydim("neqmom", ss%neqmom,ss2%neqmom)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_GAS).EQ.1)) &
           call copydim("ngsc0", ss%ngsc0,ss2%ngsc0)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_IC).EQ.1)) &
           call copydim("nicrf_src", ss%nicrf_src,ss2%nicrf_src)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_ANOM).EQ.1)) &
           call copydim("nky_anomth", ss%nky_anomth,ss2%nky_anomth)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_ANOM).EQ.1)) &
           call copydim("nky_mmm", ss%nky_mmm,ss2%nky_mmm)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_LH).EQ.1)) &
           call copydim("nlhrf_src", ss%nlhrf_src,ss2%nlhrf_src)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) &
           call copydim("nnewcomb", ss%nnewcomb,ss2%nnewcomb)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_EQ).EQ.1)) &
           call copydim("npsmom", ss%npsmom,ss2%npsmom)
  IF(cclist(ps_cnum_EQ).EQ.1) &
           call copydim("nr", ss%nr,ss2%nr)
  IF(cclist(ps_cnum_PLASMA).EQ.1) &
           call copydim("nrho", ss%nrho,ss2%nrho)
  IF(cclist(ps_cnum_ANOM).EQ.1) &
           call copydim("nrho_anom", ss%nrho_anom,ss2%nrho_anom)
  IF(cclist(ps_cnum_ANOM).EQ.1) &
           call copydim("nrho_anom2", ss%nrho_anom2,ss2%nrho_anom2)
  IF(cclist(ps_cnum_ANOM).EQ.1) &
           call copydim("nrho_anomth", ss%nrho_anomth,ss2%nrho_anomth)
  IF(cclist(ps_cnum_EC).EQ.1) &
           call copydim("nrho_ecrf", ss%nrho_ecrf,ss2%nrho_ecrf)
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(IWANT_1dEQ.eq.1)) &
           call copydim("nrho_eq", ss%nrho_eq,ss2%nrho_eq)
  IF(cclist(ps_cnum_EQ).EQ.1) &
           call copydim("nrho_eq_geo", ss%nrho_eq_geo,ss2%nrho_eq_geo)
  IF(cclist(ps_cnum_FUS).EQ.1) &
           call copydim("nrho_fus", ss%nrho_fus,ss2%nrho_fus)
  IF(cclist(ps_cnum_GAS).EQ.1) &
           call copydim("nrho_gas", ss%nrho_gas,ss2%nrho_gas)
  IF(cclist(ps_cnum_IC).EQ.1) &
           call copydim("nrho_icrf", ss%nrho_icrf,ss2%nrho_icrf)
  IF(cclist(ps_cnum_LH).EQ.1) &
           call copydim("nrho_lhrf", ss%nrho_lhrf,ss2%nrho_lhrf)
  IF(cclist(ps_cnum_LMHD).EQ.1) &
           call copydim("nrho_lmhd", ss%nrho_lmhd,ss2%nrho_lmhd)
  IF(cclist(ps_cnum_RAD).EQ.1) &
           call copydim("nrho_mist", ss%nrho_mist,ss2%nrho_mist)
  IF(cclist(ps_cnum_NBI).EQ.1) &
           call copydim("nrho_nbi", ss%nrho_nbi,ss2%nrho_nbi)
  IF(cclist(ps_cnum_PEL).EQ.1) &
           call copydim("nrho_pel", ss%nrho_pel,ss2%nrho_pel)
  IF(cclist(ps_cnum_RAD).EQ.1) &
           call copydim("nrho_rad", ss%nrho_rad,ss2%nrho_rad)
  IF(cclist(ps_cnum_RUNAWAY).EQ.1) &
           call copydim("nrho_rw", ss%nrho_rw,ss2%nrho_rw)
  IF(cclist(ps_cnum_RIPPLE).EQ.1) &
           call copydim("nr_rip", ss%nr_rip,ss2%nr_rip)
  call copydim("nspec_all", ss%nspec_all,ss2%nspec_all)
  call copydim("nspec_alla", ss%nspec_alla,ss2%nspec_alla)
  call copydim("nspec_beam", ss%nspec_beam,ss2%nspec_beam)
  call copydim("nspec_fusion", ss%nspec_fusion,ss2%nspec_fusion)
  call copydim("nspec_gas", ss%nspec_gas,ss2%nspec_gas)
  call copydim("nspec_imp0", ss%nspec_imp0,ss2%nspec_imp0)
  call copydim("nspec_impi", ss%nspec_impi,ss2%nspec_impi)
  call copydim("nspec_rfmin", ss%nspec_rfmin,ss2%nspec_rfmin)
  call copydim("nspec_th", ss%nspec_th,ss2%nspec_th)
  call copydim("nspec_tha", ss%nspec_tha,ss2%nspec_tha)
  IF(cclist(ps_cnum_EQ).EQ.1) &
           call copydim("nth_eq", ss%nth_eq,ss2%nth_eq)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_PLASMA).EQ.1)) &
           call copydim("ntrcat", ss%ntrcat,ss2%ntrcat)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_EQ).EQ.1)) &
           call copydim("num_rzlim", ss%num_rzlim,ss2%num_rzlim)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_RAD).EQ.1)) &
           call copydim("nxs", ss%nxs,ss2%nxs)
  IF(cclist(ps_cnum_EQ).EQ.1) &
           call copydim("nz", ss%nz,ss2%nz)
  IF(cclist(ps_cnum_RIPPLE).EQ.1) &
           call copydim("nz_rip", ss%nz_rip,ss2%nz_rip)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_NBI).EQ.1)) &
           call copydim("n_D_einj_standard", ss%n_D_einj_standard,ss2%n_D_einj_standard)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_NBI).EQ.1)) &
           call copydim("n_H_einj_standard", ss%n_H_einj_standard,ss2%n_H_einj_standard)
  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_NBI).EQ.1)) &
           call copydim("n_T_einj_standard", ss%n_T_einj_standard,ss2%n_T_einj_standard)

  IF(ierr.NE.0) return
  IF(idims_only.EQ.1) return

  !---------------------------------------------------
  ! allocate ss2 arrays as allowed by non-zero dimensions

  call plasma_state_empty_alloc(ss2,ierr)
  IF(ierr.NE.0) RETURN

  !---------------------------------------------------
  ! copy scalars and arrays that are allocated...
  ! (but not array dimensions)

  call plasma_state_copy_0(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)
  call plasma_state_copy_1(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)
  call plasma_state_copy_2(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)
  call plasma_state_copy_3(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)
  call plasma_state_copy_4(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)
  call plasma_state_copy_5(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)
  call plasma_state_copy_6(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)
  call plasma_state_copy_7(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)
  call plasma_state_copy_8(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)
  call plasma_state_copy_9(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)

  IF(partial_copy) THEN
     IF(ss2%lock_sim_init.EQ.ps_locked) THEN
        WRITE(iout,*) ' %plasma_state_copy: partial copy: Simulation_Init unlocked.'
        ss2%lock_sim_init = ps_unlocked
     ENDIF
  ENDIF

  ! set hash codes of new copy of state

  ilock_mdescr = ss2%lock_machine_descr
  ilock_sconfig = ss2%lock_shot_config
  ilock_siminit = ss2%lock_sim_init

  ss2%lock_machine_descr = ps_unlocked
  ss2%lock_shot_config = ps_unlocked
  ss2%lock_sim_init = ps_unlocked

  CALL plasma_state_hash_code(ss2,ps_cc_all,iertmp)
  IF(idata_only.NE.1) THEN
     ss2%hash_codes = ss2%new_hash_codes
  ENDIF

  if(cclist(ps_cnum_EQ).eq.1) THEN
     ! copy aux hash codes
     ss2%hash_codes(ps_hash_aux1:ps_hash_aux9+1) = ss%hash_codes(ps_hash_aux1:ps_hash_aux9+1)
     ss2%new_hash_codes(ps_hash_aux1:ps_hash_aux9+1) = ss%new_hash_codes(ps_hash_aux1:ps_hash_aux9+1)

  endif

  ss2%lock_machine_descr = ilock_mdescr
  ss2%lock_shot_config = ilock_sconfig
  ss2%lock_sim_init = ilock_siminit

contains

  subroutine copydim(name,fromdim,todim)

    character*(*), intent(in) :: name
    integer, intent(in) :: fromdim
    integer, intent(inout) :: todim

    if (fromdim.eq.0) then
       continue             ! (old value retained in target state)

    else if(todim.eq.0) then
       if(idata_only.eq.0) then
          todim = fromdim      ! (dimension value is copied)
       endif

    else if(todim.ne.fromdim) then
       ierr = 1             ! (both non-zero and values are unequal)
       write(iout,*) ' ?PLASMA_STATE_COPY: target state array dimension has'
       write(iout,*) '  inconsistent value: dimension name is: '//trim(name)
       write(iout,*) '  source state value: ',fromdim,'; target state value: ',todim
    endif
  end subroutine copydim

END SUBROUTINE plasma_state_copy
