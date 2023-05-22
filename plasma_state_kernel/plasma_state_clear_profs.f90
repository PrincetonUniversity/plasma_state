! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_clear_profs(cclist,ss,ierr)

  !   set profile data to ZERO
  !   scalar data and id & name variables unaffected.
  !   grids and species lists and item lists unaffected.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components in which to clear profiles
                                ! use ps_cc_all to clear all components

  TYPE (plasma_state) :: ss     ! state object whose profiles are to be cleared
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  ! errors -- expected array dimensions undefined
  !           arrays already allocated

  !-------------------
  INTEGER :: iout,isize
  !-------------------

  iout = SWIM_err_out_file

  ierr = 0


  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%anom_evar)) ss%anom_evar = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%area)) ss%area = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_LMHD).EQ.1) THEN
     IF(ALLOCATED(ss%balstab)) ss%balstab = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%bidiff_bpass_co)) ss%bidiff_bpass_co = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%bidiff_bpass_ctr)) ss%bidiff_bpass_ctr = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%bidiff_btrap)) ss%bidiff_btrap = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%bidiff_dpass_co)) ss%bidiff_dpass_co = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%bidiff_dpass_ctr)) ss%bidiff_dpass_ctr = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%bidiff_dtrap)) ss%bidiff_dtrap = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%BphiRZ)) ss%BphiRZ = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%BRRZ)) ss%BRRZ = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%BZRZ)) ss%BZRZ = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%B_surfMax)) ss%B_surfMax = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%B_surfMin)) ss%B_surfMin = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%cdicrf)) ss%cdicrf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%cdicrf_nphi)) ss%cdicrf_nphi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chie_anom)) ss%chie_anom = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chie_drbm)) ss%chie_drbm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chie_etg)) ss%chie_etg = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chie_glf)) ss%chie_glf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chie_gtcneo)) ss%chie_gtcneo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chie_mmm71)) ss%chie_mmm71 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chie_mtm)) ss%chie_mtm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chie_nclass)) ss%chie_nclass = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chie_neo)) ss%chie_neo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chie_ntm)) ss%chie_ntm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chie_paleo)) ss%chie_paleo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chie_tglf)) ss%chie_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%chie_trans)) ss%chie_trans = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%chie_transcat)) ss%chie_transcat = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chie_w19)) ss%chie_w19 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chii_anom)) ss%chii_anom = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chii_drbm)) ss%chii_drbm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chii_glf)) ss%chii_glf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chii_gtcneo)) ss%chii_gtcneo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chii_mmm71)) ss%chii_mmm71 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chii_nclass)) ss%chii_nclass = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chii_neo)) ss%chii_neo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chii_ntm)) ss%chii_ntm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chii_tglf)) ss%chii_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%chii_trans)) ss%chii_trans = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%chii_transcat)) ss%chii_transcat = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chii_w19)) ss%chii_w19 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%chimo_trans)) ss%chimo_trans = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%chimo_transcat)) ss%chimo_transcat = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chiphi_anom)) ss%chiphi_anom = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chiphi_drbm)) ss%chiphi_drbm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chiphi_glf)) ss%chiphi_glf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chiphi_gtcneo)) ss%chiphi_gtcneo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chiphi_mmm71)) ss%chiphi_mmm71 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chiphi_neo)) ss%chiphi_neo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chiphi_tglf)) ss%chiphi_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chiphi_w19)) ss%chiphi_w19 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%chitheta_mmm71)) ss%chitheta_mmm71 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%curbeam)) ss%curbeam = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EC).EQ.1) THEN
     IF(ALLOCATED(ss%curech)) ss%curech = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EC).EQ.1) THEN
     IF(ALLOCATED(ss%curech_src)) ss%curech_src = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_FUS).EQ.1) THEN
     IF(ALLOCATED(ss%curfusn)) ss%curfusn = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%curich)) ss%curich = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_LH).EQ.1) THEN
     IF(ALLOCATED(ss%curlh)) ss%curlh = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_LH).EQ.1) THEN
     IF(ALLOCATED(ss%curlh_src)) ss%curlh_src = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%curmino)) ss%curmino = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%curr_bootstrap)) ss%curr_bootstrap = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%curr_ohmic)) ss%curr_ohmic = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%curt)) ss%curt = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(ALLOCATED(ss%cur_rw)) ss%cur_rw = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%d2R_geo_dRHOdTH)) ss%d2R_geo_dRHOdTH = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%d2Z_geo_dRHOdTH)) ss%d2Z_geo_dRHOdTH = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%db_mtm)) ss%db_mtm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(ALLOCATED(ss%dcur_rw_dVloop)) ss%dcur_rw_dVloop = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difb_fusi)) ss%difb_fusi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difb_nbi)) ss%difb_nbi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difb_rfmi)) ss%difb_rfmi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%diff_trans)) ss%diff_trans = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%diff_transcat)) ss%diff_transcat = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difne_nclass)) ss%difne_nclass = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difnh_nclass)) ss%difnh_nclass = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difns_anom)) ss%difns_anom = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difns_drbm)) ss%difns_drbm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difns_glf)) ss%difns_glf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difns_gtcneo)) ss%difns_gtcneo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difns_mmm71)) ss%difns_mmm71 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difns_nclass)) ss%difns_nclass = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difns_neo)) ss%difns_neo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difns_tglf)) ss%difns_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difns_w19)) ss%difns_w19 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difnx_nclass)) ss%difnx_nclass = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%difnz_mmm71)) ss%difnz_mmm71 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%dQedGrdte_tglf)) ss%dQedGrdte_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%dQedGrdti_tglf)) ss%dQedGrdti_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%dQidGrdte_tglf)) ss%dQidGrdte_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%dQidGrdti_tglf)) ss%dQidGrdti_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%dR0_momeq_dRHO)) ss%dR0_momeq_dRHO = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%dR_geo_dRHO)) ss%dR_geo_dRHO = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%dR_geo_dTH)) ss%dR_geo_dTH = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%dxRjcos_momeq_dRHO)) ss%dxRjcos_momeq_dRHO = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%dxRjsin_momeq_dRHO)) ss%dxRjsin_momeq_dRHO = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%dxZjcos_momeq_dRHO)) ss%dxZjcos_momeq_dRHO = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%dxZjsin_momeq_dRHO)) ss%dxZjsin_momeq_dRHO = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%dZ0_momeq_dRHO)) ss%dZ0_momeq_dRHO = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%dZ_geo_dRHO)) ss%dZ_geo_dRHO = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%dZ_geo_dTH)) ss%dZ_geo_dTH = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%ee_mobility_factor)) ss%ee_mobility_factor = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%elong)) ss%elong = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%eperp_beami)) ss%eperp_beami = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_FUS).EQ.1) THEN
     IF(ALLOCATED(ss%eperp_fusi)) ss%eperp_fusi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%eperp_mini)) ss%eperp_mini = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(ALLOCATED(ss%eperp_rw)) ss%eperp_rw = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%epll_beami)) ss%epll_beami = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_FUS).EQ.1) THEN
     IF(ALLOCATED(ss%epll_fusi)) ss%epll_fusi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%epll_mini)) ss%epll_mini = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(ALLOCATED(ss%epll_rw)) ss%epll_rw = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%Epot)) ss%Epot = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%eta_parallel)) ss%eta_parallel = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%fidiff_bpass_co)) ss%fidiff_bpass_co = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%fidiff_bpass_ctr)) ss%fidiff_bpass_ctr = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%fidiff_btrap)) ss%fidiff_btrap = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%fidiff_dpass_co)) ss%fidiff_dpass_co = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%fidiff_dpass_ctr)) ss%fidiff_dpass_ctr = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%fidiff_dtrap)) ss%fidiff_dtrap = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%Fi_depletion)) ss%Fi_depletion = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%freq_drbm)) ss%freq_drbm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%freq_etg)) ss%freq_etg = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%freq_glf)) ss%freq_glf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%freq_max_tglf)) ss%freq_max_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%freq_mmm71)) ss%freq_mmm71 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%freq_mtm)) ss%freq_mtm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%freq_tglf)) ss%freq_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%gamma_nc)) ss%gamma_nc = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%gb1)) ss%gb1 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%gb2)) ss%gb2 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%gb2i)) ss%gb2i = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%gbr2)) ss%gbr2 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%gncfb2h)) ss%gncfb2h = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%gncfh)) ss%gncfh = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%gr1)) ss%gr1 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%gr2)) ss%gr2 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%gr2i)) ss%gr2i = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%gr2rho2)) ss%gr2rho2 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%gr3i)) ss%gr3i = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%grho1)) ss%grho1 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%grho2)) ss%grho2 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%grho2b2i)) ss%grho2b2i = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%grho2r2i)) ss%grho2r2i = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%grho2r3i)) ss%grho2r3i = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%gri)) ss%gri = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%grirhoi)) ss%grirhoi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%growthrate_drbm)) ss%growthrate_drbm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%growthrate_etg)) ss%growthrate_etg = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%growthrate_glf)) ss%growthrate_glf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%growthrate_max_tglf)) ss%growthrate_max_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%growthrate_mmm71)) ss%growthrate_mmm71 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%growthrate_mtm)) ss%growthrate_mtm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%growthrate_tglf)) ss%growthrate_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%growthrate_vpar)) ss%growthrate_vpar = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%g_eq)) ss%g_eq = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%iota)) ss%iota = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%jdotb)) ss%jdotb = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%kyrhos_drbm)) ss%kyrhos_drbm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%kyrhos_etg)) ss%kyrhos_etg = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%kyrhos_mtm)) ss%kyrhos_mtm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%kyspectrum_tglf)) ss%kyspectrum_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%Lpol)) ss%Lpol = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%n0norm)) ss%n0norm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%n0_halo)) ss%n0_halo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%n0_reco)) ss%n0_reco = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%nbeami)) ss%nbeami = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%neflux_ky_tglf)) ss%neflux_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%ne_ky_tglf)) ss%ne_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_FUS).EQ.1) THEN
     IF(ALLOCATED(ss%nfusi)) ss%nfusi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%ngradb2_av)) ss%ngradb2_av = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%ni)) ss%ni = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%niflux_ky_tglf)) ss%niflux_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%ni_ky_tglf)) ss%ni_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%nmini)) ss%nmini = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%nmodel)) ss%nmodel = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(ALLOCATED(ss%nrw)) ss%nrw = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%ns)) ss%ns = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%nzflux_ky_tglf)) ss%nzflux_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%nz_ky_tglf)) ss%nz_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%omeg0cx)) ss%omeg0cx = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%omeg0sc0)) ss%omeg0sc0 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%omeg0_halo)) ss%omeg0_halo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%omeg0_reco)) ss%omeg0_reco = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%omegat)) ss%omegat = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%omega_exb)) ss%omega_exb = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%p0_reco)) ss%p0_reco = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%pb0_halo)) ss%pb0_halo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%pbe)) ss%pbe = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%pbi)) ss%pbi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%pbth)) ss%pbth = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%pcx_halo)) ss%pcx_halo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%pcx_reco)) ss%pcx_reco = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EC).EQ.1) THEN
     IF(ALLOCATED(ss%peech)) ss%peech = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EC).EQ.1) THEN
     IF(ALLOCATED(ss%peech_src)) ss%peech_src = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_LH).EQ.1) THEN
     IF(ALLOCATED(ss%pelh)) ss%pelh = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_LH).EQ.1) THEN
     IF(ALLOCATED(ss%pelh_src)) ss%pelh_src = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%pe_anom)) ss%pe_anom = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%pe_trans)) ss%pe_trans = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%pe_transcat)) ss%pe_transcat = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_FUS).EQ.1) THEN
     IF(ALLOCATED(ss%pfuse)) ss%pfuse = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_FUS).EQ.1) THEN
     IF(ALLOCATED(ss%pfusi)) ss%pfusi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_FUS).EQ.1) THEN
     IF(ALLOCATED(ss%pfusth)) ss%pfusth = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%phiflux_ky_tglf)) ss%phiflux_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%phit)) ss%phit = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%picrf_nphi_srcs)) ss%picrf_nphi_srcs = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%picrf_srcs)) ss%picrf_srcs = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%picrf_totals)) ss%picrf_totals = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%picth)) ss%picth = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_LH).EQ.1) THEN
     IF(ALLOCATED(ss%pilh)) ss%pilh = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_LH).EQ.1) THEN
     IF(ALLOCATED(ss%pilh_src)) ss%pilh_src = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%pi_anom)) ss%pi_anom = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%pi_trans)) ss%pi_trans = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%pi_transcat)) ss%pi_transcat = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%pmine)) ss%pmine = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%pmini)) ss%pmini = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_IC).EQ.1) THEN
     IF(ALLOCATED(ss%pminth)) ss%pminth = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%pohme)) ss%pohme = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%pot_ky_tglf)) ss%pot_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_RAD).EQ.1) THEN
     IF(ALLOCATED(ss%prad)) ss%prad = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_RAD).EQ.1) THEN
     IF(ALLOCATED(ss%prad_br)) ss%prad_br = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_RAD).EQ.1) THEN
     IF(ALLOCATED(ss%prad_cy)) ss%prad_cy = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_RAD).EQ.1) THEN
     IF(ALLOCATED(ss%prad_li)) ss%prad_li = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%psc_halo)) ss%psc_halo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%psc_reco)) ss%psc_reco = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%psipol)) ss%psipol = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%PsiRZ)) ss%PsiRZ = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%psmom_errck)) ss%psmom_errck = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%psmom_nc)) ss%psmom_nc = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%P_eq)) ss%P_eq = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%qbeami)) ss%qbeami = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%qcomp_e)) ss%qcomp_e = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%qcomp_i)) ss%qcomp_i = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%qie)) ss%qie = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%qioniz)) ss%qioniz = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%qqcx)) ss%qqcx = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%qrot_conv)) ss%qrot_conv = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%qrot_diff)) ss%qrot_diff = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%q_eq)) ss%q_eq = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%R0_momeq)) ss%R0_momeq = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%rate_sinb0i)) ss%rate_sinb0i = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%rate_sinb0x)) ss%rate_sinb0x = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%rate_sinb0xs)) ss%rate_sinb0xs = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_FUS).EQ.1) THEN
     IF(ALLOCATED(ss%rate_sinf0i)) ss%rate_sinf0i = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_FUS).EQ.1) THEN
     IF(ALLOCATED(ss%rate_sinf0x)) ss%rate_sinf0x = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_FUS).EQ.1) THEN
     IF(ALLOCATED(ss%rate_sinf0xs)) ss%rate_sinf0xs = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%res_sn)) ss%res_sn = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%res_te)) ss%res_te = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%res_ti)) ss%res_ti = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%res_tq)) ss%res_tq = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%Rmajor_mean)) ss%Rmajor_mean = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%rMinor_mean)) ss%rMinor_mean = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%R_geo)) ss%R_geo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%R_midp_in)) ss%R_midp_in = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%R_midp_out)) ss%R_midp_out = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%R_surfMax)) ss%R_surfMax = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%R_surfMin)) ss%R_surfMin = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%s0reco)) ss%s0reco = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%s0reco_e)) ss%s0reco_e = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%s0reco_recap)) ss%s0reco_recap = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%sb0halo)) ss%sb0halo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%sb0halo_recap)) ss%sb0halo_recap = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%sbedep)) ss%sbedep = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%sbehalo)) ss%sbehalo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%sbsce)) ss%sbsce = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%sbtherm)) ss%sbtherm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%sc_anom)) ss%sc_anom = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_FUS).EQ.1) THEN
     IF(ALLOCATED(ss%sfsce)) ss%sfsce = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_FUS).EQ.1) THEN
     IF(ALLOCATED(ss%sftherm)) ss%sftherm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_RAD).EQ.1) THEN
     IF(ALLOCATED(ss%sii_xs)) ss%sii_xs = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%sn_trans)) ss%sn_trans = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%sn_transcat)) ss%sn_transcat = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%sprof0)) ss%sprof0 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%sprof0e)) ss%sprof0e = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PEL).EQ.1) THEN
     IF(ALLOCATED(ss%spsce)) ss%spsce = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%squareLO)) ss%squareLO = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%squareUO)) ss%squareUO = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%surf)) ss%surf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%T0cx)) ss%T0cx = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%T0sc0)) ss%T0sc0 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%T0_halo)) ss%T0_halo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%T0_reco)) ss%T0_reco = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%teflux_ky_tglf)) ss%teflux_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%te_ky_tglf)) ss%te_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_RIPPLE).EQ.1) THEN
     IF(ALLOCATED(ss%tfrip2_log)) ss%tfrip2_log = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_RIPPLE).EQ.1) THEN
     IF(ALLOCATED(ss%tfrip_log)) ss%tfrip_log = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%Ti)) ss%Ti = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%tiflux_ky_tglf)) ss%tiflux_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%ti_ky_tglf)) ss%ti_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%tmodel)) ss%tmodel = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%tq0_reco)) ss%tq0_reco = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%tqb0_halo)) ss%tqb0_halo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%tqbe)) ss%tqbe = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%tqbi)) ss%tqbi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%tqbjxb)) ss%tqbjxb = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%tqbth)) ss%tqbth = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%tqcx_halo)) ss%tqcx_halo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%tqcx_reco)) ss%tqcx_reco = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%tqioniz)) ss%tqioniz = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%tqqcx)) ss%tqqcx = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_NBI).EQ.1) THEN
     IF(ALLOCATED(ss%tqsc_halo)) ss%tqsc_halo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     IF(ALLOCATED(ss%tqsc_reco)) ss%tqsc_reco = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%tq_anom)) ss%tq_anom = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%tq_trans)) ss%tq_trans = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%tq_transcat)) ss%tq_transcat = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%triang)) ss%triang = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%triangL)) ss%triangL = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%triangU)) ss%triangU = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%triang_miller_L)) ss%triang_miller_L = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%triang_miller_U)) ss%triang_miller_U = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%Ts)) ss%Ts = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%tzflux_ky_tglf)) ss%tzflux_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%tz_ky_tglf)) ss%tz_ky_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%upwind_pfrac_ns)) ss%upwind_pfrac_ns = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%upwind_pfrac_omega)) ss%upwind_pfrac_omega = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%upwind_pfrac_Te)) ss%upwind_pfrac_Te = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%upwind_pfrac_Ti)) ss%upwind_pfrac_Ti = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%vee_trans)) ss%vee_trans = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%vee_transcat)) ss%vee_transcat = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velb_fusi)) ss%velb_fusi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velb_nbi)) ss%velb_nbi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velb_rfmi)) ss%velb_rfmi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velne_nclass)) ss%velne_nclass = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velnh_nclass)) ss%velnh_nclass = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velni_drbm)) ss%velni_drbm = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velns_anom)) ss%velns_anom = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velns_glf)) ss%velns_glf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velns_gtcneo)) ss%velns_gtcneo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velns_mmm71)) ss%velns_mmm71 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velns_nclass)) ss%velns_nclass = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velns_neo)) ss%velns_neo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velns_tglf)) ss%velns_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velnx_nclass)) ss%velnx_nclass = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velpphi_anom)) ss%velpphi_anom = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velpphi_glf)) ss%velpphi_glf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velpphi_gtcneo)) ss%velpphi_gtcneo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velpphi_mmm71)) ss%velpphi_mmm71 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velpphi_neo)) ss%velpphi_neo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velpphi_tglf)) ss%velpphi_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velte_anom)) ss%velte_anom = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velte_glf)) ss%velte_glf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velte_gtcneo)) ss%velte_gtcneo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velte_mmm71)) ss%velte_mmm71 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velte_nclass)) ss%velte_nclass = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velte_neo)) ss%velte_neo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velte_paleo)) ss%velte_paleo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velte_tglf)) ss%velte_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velti_anom)) ss%velti_anom = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velti_glf)) ss%velti_glf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velti_gtcneo)) ss%velti_gtcneo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velti_mmm71)) ss%velti_mmm71 = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velti_nclass)) ss%velti_nclass = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velti_neo)) ss%velti_neo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     IF(ALLOCATED(ss%velti_tglf)) ss%velti_tglf = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%vie_trans)) ss%vie_trans = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%vie_transcat)) ss%vie_transcat = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%vmo_trans)) ss%vmo_trans = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%vmo_transcat)) ss%vmo_transcat = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%vn_trans)) ss%vn_trans = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%vn_transcat)) ss%vn_transcat = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%vol)) ss%vol = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%vpol_inmp)) ss%vpol_inmp = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%vpol_omp)) ss%vpol_omp = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%vtor_inmp)) ss%vtor_inmp = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%vtor_omp)) ss%vtor_omp = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%V_loop)) ss%V_loop = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%v_pars)) ss%v_pars = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%v_pers)) ss%v_pers = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%xRjcos_momeq)) ss%xRjcos_momeq = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%xRjsin_momeq)) ss%xRjsin_momeq = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%xZjcos_momeq)) ss%xZjcos_momeq = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%xZjsin_momeq)) ss%xZjsin_momeq = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%Z0_momeq)) ss%Z0_momeq = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%Zeff)) ss%Zeff = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%Zeff_fi)) ss%Zeff_fi = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     IF(ALLOCATED(ss%Zeff_th)) ss%Zeff_th = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%Z_geo)) ss%Z_geo = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%Z_midp)) ss%Z_midp = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%Z_surfMax)) ss%Z_surfMax = 0.0_rspec
  ENDIF
  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     IF(ALLOCATED(ss%Z_surfMin)) ss%Z_surfMin = 0.0_rspec
  ENDIF


END SUBROUTINE plasma_state_clear_profs
