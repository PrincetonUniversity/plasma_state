! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_free(ss,ierr)

  ! deallocate state object
  !   deallocate anything that was previously allocated.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss    ! state object whose elements are deallocated
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  ! errors -- expected array dimensions undefined
  !           arrays already allocated

  !-------------------
  INTEGER :: iout,isize
  !-------------------

  iout = SWIM_err_out_file

  ierr = 0

  IF(ALLOCATED(ss%id_zonwt)) DEALLOCATE(ss%id_zonwt)


  IF(ALLOCATED(ss%alla_index)) DEALLOCATE(ss%alla_index)

  IF(ALLOCATED(ss%ALLA_name)) DEALLOCATE(ss%ALLA_name)

  IF(ALLOCATED(ss%ALLA_type)) DEALLOCATE(ss%ALLA_type)

  IF(ALLOCATED(ss%all_index)) DEALLOCATE(ss%all_index)

  IF(ALLOCATED(ss%ALL_name)) DEALLOCATE(ss%ALL_name)

  IF(ALLOCATED(ss%ALL_type)) DEALLOCATE(ss%ALL_type)

  IF(ALLOCATED(ss%amaxlh)) DEALLOCATE(ss%amaxlh)

  IF(ALLOCATED(ss%aminlh)) DEALLOCATE(ss%aminlh)

  IF(ALLOCATED(ss%Ang_Href_coil)) DEALLOCATE(ss%Ang_Href_coil)

  IF(ALLOCATED(ss%Ang_Vref_coil)) DEALLOCATE(ss%Ang_Vref_coil)

  IF(ALLOCATED(ss%anom_evar)) DEALLOCATE(ss%anom_evar)

  IF(ALLOCATED(ss%ant_model)) DEALLOCATE(ss%ant_model)

  IF(ALLOCATED(ss%ap2_halfheight)) DEALLOCATE(ss%ap2_halfheight)

  IF(ALLOCATED(ss%ap2_halfwidth)) DEALLOCATE(ss%ap2_halfwidth)

  IF(ALLOCATED(ss%ap2_horiz_offset)) DEALLOCATE(ss%ap2_horiz_offset)

  IF(ALLOCATED(ss%ap2_vert_offset)) DEALLOCATE(ss%ap2_vert_offset)

  IF(ALLOCATED(ss%apolmaxlh)) DEALLOCATE(ss%apolmaxlh)

  IF(ALLOCATED(ss%apolminlh)) DEALLOCATE(ss%apolminlh)

  IF(ALLOCATED(ss%ap_halfheight)) DEALLOCATE(ss%ap_halfheight)

  IF(ALLOCATED(ss%ap_halfwidth)) DEALLOCATE(ss%ap_halfwidth)

  IF(ALLOCATED(ss%ap_horiz_offset)) DEALLOCATE(ss%ap_horiz_offset)

  IF(ALLOCATED(ss%ap_vert_offset)) DEALLOCATE(ss%ap_vert_offset)

  IF(ALLOCATED(ss%area)) DEALLOCATE(ss%area)

  IF(ALLOCATED(ss%atormaxlh)) DEALLOCATE(ss%atormaxlh)

  IF(ALLOCATED(ss%atorminlh)) DEALLOCATE(ss%atorminlh)

  IF(ALLOCATED(ss%Balloon_code)) DEALLOCATE(ss%Balloon_code)

  IF(ALLOCATED(ss%Balloon_mapper)) DEALLOCATE(ss%Balloon_mapper)

  IF(ALLOCATED(ss%Balloon_stable)) DEALLOCATE(ss%Balloon_stable)

  IF(ALLOCATED(ss%Balloon_status)) DEALLOCATE(ss%Balloon_status)

  IF(ALLOCATED(ss%balstab)) DEALLOCATE(ss%balstab)

  IF(ALLOCATED(ss%beam_type)) DEALLOCATE(ss%beam_type)

  IF(ALLOCATED(ss%id_bidiff_bpass_co)) DEALLOCATE(ss%id_bidiff_bpass_co)
  IF(ALLOCATED(ss%bidiff_bpass_co)) DEALLOCATE(ss%bidiff_bpass_co)

  IF(ALLOCATED(ss%id_bidiff_bpass_ctr)) DEALLOCATE(ss%id_bidiff_bpass_ctr)
  IF(ALLOCATED(ss%bidiff_bpass_ctr)) DEALLOCATE(ss%bidiff_bpass_ctr)

  IF(ALLOCATED(ss%id_bidiff_btrap)) DEALLOCATE(ss%id_bidiff_btrap)
  IF(ALLOCATED(ss%bidiff_btrap)) DEALLOCATE(ss%bidiff_btrap)

  IF(ALLOCATED(ss%id_bidiff_dpass_co)) DEALLOCATE(ss%id_bidiff_dpass_co)
  IF(ALLOCATED(ss%bidiff_dpass_co)) DEALLOCATE(ss%bidiff_dpass_co)

  IF(ALLOCATED(ss%id_bidiff_dpass_ctr)) DEALLOCATE(ss%id_bidiff_dpass_ctr)
  IF(ALLOCATED(ss%bidiff_dpass_ctr)) DEALLOCATE(ss%bidiff_dpass_ctr)

  IF(ALLOCATED(ss%id_bidiff_dtrap)) DEALLOCATE(ss%id_bidiff_dtrap)
  IF(ALLOCATED(ss%bidiff_dtrap)) DEALLOCATE(ss%bidiff_dtrap)

  IF(ALLOCATED(ss%BphiRZ)) DEALLOCATE(ss%BphiRZ)

  IF(ALLOCATED(ss%BRRZ)) DEALLOCATE(ss%BRRZ)

  IF(ALLOCATED(ss%BZRZ)) DEALLOCATE(ss%BZRZ)

  IF(ALLOCATED(ss%b_halfHeight)) DEALLOCATE(ss%b_halfHeight)

  IF(ALLOCATED(ss%b_halfwidth)) DEALLOCATE(ss%b_halfwidth)

  IF(ALLOCATED(ss%b_Hdivergence)) DEALLOCATE(ss%b_Hdivergence)

  IF(ALLOCATED(ss%b_Hfocal_length)) DEALLOCATE(ss%b_Hfocal_length)

  IF(ALLOCATED(ss%B_surfMax)) DEALLOCATE(ss%B_surfMax)

  IF(ALLOCATED(ss%B_surfMin)) DEALLOCATE(ss%B_surfMin)

  IF(ALLOCATED(ss%b_Vdivergence)) DEALLOCATE(ss%b_Vdivergence)

  IF(ALLOCATED(ss%b_Vfocal_length)) DEALLOCATE(ss%b_Vfocal_length)

  IF(ALLOCATED(ss%id_cdicrf)) DEALLOCATE(ss%id_cdicrf)
  IF(ALLOCATED(ss%cdicrf)) DEALLOCATE(ss%cdicrf)

  IF(ALLOCATED(ss%id_cdicrf_nphi)) DEALLOCATE(ss%id_cdicrf_nphi)
  IF(ALLOCATED(ss%cdicrf_nphi)) DEALLOCATE(ss%cdicrf_nphi)

  IF(ALLOCATED(ss%chie_anom)) DEALLOCATE(ss%chie_anom)

  IF(ALLOCATED(ss%chie_drbm)) DEALLOCATE(ss%chie_drbm)

  IF(ALLOCATED(ss%chie_etg)) DEALLOCATE(ss%chie_etg)

  IF(ALLOCATED(ss%chie_glf)) DEALLOCATE(ss%chie_glf)

  IF(ALLOCATED(ss%chie_gtcneo)) DEALLOCATE(ss%chie_gtcneo)

  IF(ALLOCATED(ss%chie_mmm71)) DEALLOCATE(ss%chie_mmm71)

  IF(ALLOCATED(ss%chie_mtm)) DEALLOCATE(ss%chie_mtm)

  IF(ALLOCATED(ss%chie_nclass)) DEALLOCATE(ss%chie_nclass)

  IF(ALLOCATED(ss%chie_neo)) DEALLOCATE(ss%chie_neo)

  IF(ALLOCATED(ss%chie_ntm)) DEALLOCATE(ss%chie_ntm)

  IF(ALLOCATED(ss%chie_paleo)) DEALLOCATE(ss%chie_paleo)

  IF(ALLOCATED(ss%chie_tglf)) DEALLOCATE(ss%chie_tglf)

  IF(ALLOCATED(ss%chie_trans)) DEALLOCATE(ss%chie_trans)

  IF(ALLOCATED(ss%id_chie_transcat)) DEALLOCATE(ss%id_chie_transcat)
  IF(ALLOCATED(ss%chie_transcat)) DEALLOCATE(ss%chie_transcat)

  IF(ALLOCATED(ss%chie_w19)) DEALLOCATE(ss%chie_w19)

  IF(ALLOCATED(ss%chii_anom)) DEALLOCATE(ss%chii_anom)

  IF(ALLOCATED(ss%chii_drbm)) DEALLOCATE(ss%chii_drbm)

  IF(ALLOCATED(ss%chii_glf)) DEALLOCATE(ss%chii_glf)

  IF(ALLOCATED(ss%chii_gtcneo)) DEALLOCATE(ss%chii_gtcneo)

  IF(ALLOCATED(ss%chii_mmm71)) DEALLOCATE(ss%chii_mmm71)

  IF(ALLOCATED(ss%chii_nclass)) DEALLOCATE(ss%chii_nclass)

  IF(ALLOCATED(ss%chii_neo)) DEALLOCATE(ss%chii_neo)

  IF(ALLOCATED(ss%chii_ntm)) DEALLOCATE(ss%chii_ntm)

  IF(ALLOCATED(ss%chii_tglf)) DEALLOCATE(ss%chii_tglf)

  IF(ALLOCATED(ss%chii_trans)) DEALLOCATE(ss%chii_trans)

  IF(ALLOCATED(ss%id_chii_transcat)) DEALLOCATE(ss%id_chii_transcat)
  IF(ALLOCATED(ss%chii_transcat)) DEALLOCATE(ss%chii_transcat)

  IF(ALLOCATED(ss%chii_w19)) DEALLOCATE(ss%chii_w19)

  IF(ALLOCATED(ss%chimo_trans)) DEALLOCATE(ss%chimo_trans)

  IF(ALLOCATED(ss%id_chimo_transcat)) DEALLOCATE(ss%id_chimo_transcat)
  IF(ALLOCATED(ss%chimo_transcat)) DEALLOCATE(ss%chimo_transcat)

  IF(ALLOCATED(ss%chiphi_anom)) DEALLOCATE(ss%chiphi_anom)

  IF(ALLOCATED(ss%chiphi_drbm)) DEALLOCATE(ss%chiphi_drbm)

  IF(ALLOCATED(ss%chiphi_glf)) DEALLOCATE(ss%chiphi_glf)

  IF(ALLOCATED(ss%chiphi_gtcneo)) DEALLOCATE(ss%chiphi_gtcneo)

  IF(ALLOCATED(ss%chiphi_mmm71)) DEALLOCATE(ss%chiphi_mmm71)

  IF(ALLOCATED(ss%chiphi_neo)) DEALLOCATE(ss%chiphi_neo)

  IF(ALLOCATED(ss%chiphi_tglf)) DEALLOCATE(ss%chiphi_tglf)

  IF(ALLOCATED(ss%chiphi_w19)) DEALLOCATE(ss%chiphi_w19)

  IF(ALLOCATED(ss%chitheta_mmm71)) DEALLOCATE(ss%chitheta_mmm71)

  IF(ALLOCATED(ss%circuit_name)) DEALLOCATE(ss%circuit_name)

  IF(ALLOCATED(ss%coil_apt)) DEALLOCATE(ss%coil_apt)

  IF(ALLOCATED(ss%coil_in_circuit)) DEALLOCATE(ss%coil_in_circuit)

  IF(ALLOCATED(ss%coil_name)) DEALLOCATE(ss%coil_name)

  IF(ALLOCATED(ss%coil_resispt)) DEALLOCATE(ss%coil_resispt)

  IF(ALLOCATED(ss%curbeam)) DEALLOCATE(ss%curbeam)

  IF(ALLOCATED(ss%curech)) DEALLOCATE(ss%curech)

  IF(ALLOCATED(ss%id_curech_src)) DEALLOCATE(ss%id_curech_src)
  IF(ALLOCATED(ss%curech_src)) DEALLOCATE(ss%curech_src)

  IF(ALLOCATED(ss%curfusn)) DEALLOCATE(ss%curfusn)

  IF(ALLOCATED(ss%curich)) DEALLOCATE(ss%curich)

  IF(ALLOCATED(ss%curlh)) DEALLOCATE(ss%curlh)

  IF(ALLOCATED(ss%id_curlh_src)) DEALLOCATE(ss%id_curlh_src)
  IF(ALLOCATED(ss%curlh_src)) DEALLOCATE(ss%curlh_src)

  IF(ALLOCATED(ss%curmino)) DEALLOCATE(ss%curmino)

  IF(ALLOCATED(ss%curr_bootstrap)) DEALLOCATE(ss%curr_bootstrap)

  IF(ALLOCATED(ss%curr_ohmic)) DEALLOCATE(ss%curr_ohmic)

  IF(ALLOCATED(ss%curt)) DEALLOCATE(ss%curt)

  IF(ALLOCATED(ss%cur_rw)) DEALLOCATE(ss%cur_rw)

  IF(ALLOCATED(ss%d2R_geo_dRHOdTH)) DEALLOCATE(ss%d2R_geo_dRHOdTH)

  IF(ALLOCATED(ss%d2Z_geo_dRHOdTH)) DEALLOCATE(ss%d2Z_geo_dRHOdTH)

  IF(ALLOCATED(ss%db_mtm)) DEALLOCATE(ss%db_mtm)

  IF(ALLOCATED(ss%dcur_rw_dVloop)) DEALLOCATE(ss%dcur_rw_dVloop)

  IF(ALLOCATED(ss%DeltaW_code)) DEALLOCATE(ss%DeltaW_code)

  IF(ALLOCATED(ss%DeltaW_mapper)) DEALLOCATE(ss%DeltaW_mapper)

  IF(ALLOCATED(ss%DeltaW_stable)) DEALLOCATE(ss%DeltaW_stable)

  IF(ALLOCATED(ss%DeltaW_status)) DEALLOCATE(ss%DeltaW_status)

  IF(ALLOCATED(ss%difb_fusi)) DEALLOCATE(ss%difb_fusi)

  IF(ALLOCATED(ss%difb_nbi)) DEALLOCATE(ss%difb_nbi)

  IF(ALLOCATED(ss%difb_rfmi)) DEALLOCATE(ss%difb_rfmi)

  IF(ALLOCATED(ss%id_diff_trans)) DEALLOCATE(ss%id_diff_trans)
  IF(ALLOCATED(ss%diff_trans)) DEALLOCATE(ss%diff_trans)

  IF(ALLOCATED(ss%id_diff_transcat)) DEALLOCATE(ss%id_diff_transcat)
  IF(ALLOCATED(ss%diff_transcat)) DEALLOCATE(ss%diff_transcat)

  IF(ALLOCATED(ss%difne_nclass)) DEALLOCATE(ss%difne_nclass)

  IF(ALLOCATED(ss%difnh_nclass)) DEALLOCATE(ss%difnh_nclass)

  IF(ALLOCATED(ss%id_difns_anom)) DEALLOCATE(ss%id_difns_anom)
  IF(ALLOCATED(ss%difns_anom)) DEALLOCATE(ss%difns_anom)

  IF(ALLOCATED(ss%id_difns_drbm)) DEALLOCATE(ss%id_difns_drbm)
  IF(ALLOCATED(ss%difns_drbm)) DEALLOCATE(ss%difns_drbm)

  IF(ALLOCATED(ss%id_difns_glf)) DEALLOCATE(ss%id_difns_glf)
  IF(ALLOCATED(ss%difns_glf)) DEALLOCATE(ss%difns_glf)

  IF(ALLOCATED(ss%id_difns_gtcneo)) DEALLOCATE(ss%id_difns_gtcneo)
  IF(ALLOCATED(ss%difns_gtcneo)) DEALLOCATE(ss%difns_gtcneo)

  IF(ALLOCATED(ss%id_difns_mmm71)) DEALLOCATE(ss%id_difns_mmm71)
  IF(ALLOCATED(ss%difns_mmm71)) DEALLOCATE(ss%difns_mmm71)

  IF(ALLOCATED(ss%id_difns_nclass)) DEALLOCATE(ss%id_difns_nclass)
  IF(ALLOCATED(ss%difns_nclass)) DEALLOCATE(ss%difns_nclass)

  IF(ALLOCATED(ss%id_difns_neo)) DEALLOCATE(ss%id_difns_neo)
  IF(ALLOCATED(ss%difns_neo)) DEALLOCATE(ss%difns_neo)

  IF(ALLOCATED(ss%id_difns_tglf)) DEALLOCATE(ss%id_difns_tglf)
  IF(ALLOCATED(ss%difns_tglf)) DEALLOCATE(ss%difns_tglf)

  IF(ALLOCATED(ss%id_difns_w19)) DEALLOCATE(ss%id_difns_w19)
  IF(ALLOCATED(ss%difns_w19)) DEALLOCATE(ss%difns_w19)

  IF(ALLOCATED(ss%difnx_nclass)) DEALLOCATE(ss%difnx_nclass)

  IF(ALLOCATED(ss%difnz_mmm71)) DEALLOCATE(ss%difnz_mmm71)

  IF(ALLOCATED(ss%dist_fun)) DEALLOCATE(ss%dist_fun)

  IF(ALLOCATED(ss%dQedGrdte_tglf)) DEALLOCATE(ss%dQedGrdte_tglf)

  IF(ALLOCATED(ss%dQedGrdti_tglf)) DEALLOCATE(ss%dQedGrdti_tglf)

  IF(ALLOCATED(ss%dQidGrdte_tglf)) DEALLOCATE(ss%dQidGrdte_tglf)

  IF(ALLOCATED(ss%dQidGrdti_tglf)) DEALLOCATE(ss%dQidGrdti_tglf)

  IF(ALLOCATED(ss%dR0_momeq_dRHO)) DEALLOCATE(ss%dR0_momeq_dRHO)

  IF(ALLOCATED(ss%dR_geo_dRHO)) DEALLOCATE(ss%dR_geo_dRHO)

  IF(ALLOCATED(ss%dR_geo_dTH)) DEALLOCATE(ss%dR_geo_dTH)

  IF(ALLOCATED(ss%dxRjcos_momeq_dRHO)) DEALLOCATE(ss%dxRjcos_momeq_dRHO)

  IF(ALLOCATED(ss%dxRjsin_momeq_dRHO)) DEALLOCATE(ss%dxRjsin_momeq_dRHO)

  IF(ALLOCATED(ss%dxZjcos_momeq_dRHO)) DEALLOCATE(ss%dxZjcos_momeq_dRHO)

  IF(ALLOCATED(ss%dxZjsin_momeq_dRHO)) DEALLOCATE(ss%dxZjsin_momeq_dRHO)

  IF(ALLOCATED(ss%dx_fshield)) DEALLOCATE(ss%dx_fshield)

  IF(ALLOCATED(ss%dZ0_momeq_dRHO)) DEALLOCATE(ss%dZ0_momeq_dRHO)

  IF(ALLOCATED(ss%dZ_geo_dRHO)) DEALLOCATE(ss%dZ_geo_dRHO)

  IF(ALLOCATED(ss%dZ_geo_dTH)) DEALLOCATE(ss%dZ_geo_dTH)

  IF(ALLOCATED(ss%D_einj_standard)) DEALLOCATE(ss%D_einj_standard)

  IF(ALLOCATED(ss%D_ffull_standard)) DEALLOCATE(ss%D_ffull_standard)

  IF(ALLOCATED(ss%D_fhalf_standard)) DEALLOCATE(ss%D_fhalf_standard)

  IF(ALLOCATED(ss%e0_av)) DEALLOCATE(ss%e0_av)

  IF(ALLOCATED(ss%ecrf_src_name)) DEALLOCATE(ss%ecrf_src_name)

  IF(ALLOCATED(ss%EC_Beam_Elongation)) DEALLOCATE(ss%EC_Beam_Elongation)

  IF(ALLOCATED(ss%EC_Half_Power_Angle)) DEALLOCATE(ss%EC_Half_Power_Angle)

  IF(ALLOCATED(ss%EC_Omode_Fraction)) DEALLOCATE(ss%EC_Omode_Fraction)

  IF(ALLOCATED(ss%EC_phi_aim)) DEALLOCATE(ss%EC_phi_aim)

  IF(ALLOCATED(ss%EC_PRINC_CURV_HOR)) DEALLOCATE(ss%EC_PRINC_CURV_HOR)

  IF(ALLOCATED(ss%EC_PRINC_CURV_VERT)) DEALLOCATE(ss%EC_PRINC_CURV_VERT)

  IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR)) DEALLOCATE(ss%EC_PRINC_WIDTH_HOR)

  IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT)) DEALLOCATE(ss%EC_PRINC_WIDTH_VERT)

  IF(ALLOCATED(ss%EC_theta_aim)) DEALLOCATE(ss%EC_theta_aim)

  IF(ALLOCATED(ss%ee_mobility_factor)) DEALLOCATE(ss%ee_mobility_factor)

  IF(ALLOCATED(ss%Einj_max)) DEALLOCATE(ss%Einj_max)

  IF(ALLOCATED(ss%Einj_min)) DEALLOCATE(ss%Einj_min)

  IF(ALLOCATED(ss%elong)) DEALLOCATE(ss%elong)

  IF(ALLOCATED(ss%id_eperp_beami)) DEALLOCATE(ss%id_eperp_beami)
  IF(ALLOCATED(ss%eperp_beami)) DEALLOCATE(ss%eperp_beami)

  IF(ALLOCATED(ss%id_eperp_fusi)) DEALLOCATE(ss%id_eperp_fusi)
  IF(ALLOCATED(ss%eperp_fusi)) DEALLOCATE(ss%eperp_fusi)

  IF(ALLOCATED(ss%id_eperp_mini)) DEALLOCATE(ss%id_eperp_mini)
  IF(ALLOCATED(ss%eperp_mini)) DEALLOCATE(ss%eperp_mini)

  IF(ALLOCATED(ss%eperp_rw)) DEALLOCATE(ss%eperp_rw)

  IF(ALLOCATED(ss%id_epll_beami)) DEALLOCATE(ss%id_epll_beami)
  IF(ALLOCATED(ss%epll_beami)) DEALLOCATE(ss%epll_beami)

  IF(ALLOCATED(ss%id_epll_fusi)) DEALLOCATE(ss%id_epll_fusi)
  IF(ALLOCATED(ss%epll_fusi)) DEALLOCATE(ss%epll_fusi)

  IF(ALLOCATED(ss%id_epll_mini)) DEALLOCATE(ss%id_epll_mini)
  IF(ALLOCATED(ss%epll_mini)) DEALLOCATE(ss%epll_mini)

  IF(ALLOCATED(ss%epll_rw)) DEALLOCATE(ss%epll_rw)

  IF(ALLOCATED(ss%Epot)) DEALLOCATE(ss%Epot)

  IF(ALLOCATED(ss%eqmom_num)) DEALLOCATE(ss%eqmom_num)

  IF(ALLOCATED(ss%eta_parallel)) DEALLOCATE(ss%eta_parallel)

  IF(ALLOCATED(ss%E_anom)) DEALLOCATE(ss%E_anom)

  IF(ALLOCATED(ss%E_anom2)) DEALLOCATE(ss%E_anom2)

  IF(ALLOCATED(ss%id_fidiff_bpass_co)) DEALLOCATE(ss%id_fidiff_bpass_co)
  IF(ALLOCATED(ss%fidiff_bpass_co)) DEALLOCATE(ss%fidiff_bpass_co)

  IF(ALLOCATED(ss%id_fidiff_bpass_ctr)) DEALLOCATE(ss%id_fidiff_bpass_ctr)
  IF(ALLOCATED(ss%fidiff_bpass_ctr)) DEALLOCATE(ss%fidiff_bpass_ctr)

  IF(ALLOCATED(ss%id_fidiff_btrap)) DEALLOCATE(ss%id_fidiff_btrap)
  IF(ALLOCATED(ss%fidiff_btrap)) DEALLOCATE(ss%fidiff_btrap)

  IF(ALLOCATED(ss%id_fidiff_dpass_co)) DEALLOCATE(ss%id_fidiff_dpass_co)
  IF(ALLOCATED(ss%fidiff_dpass_co)) DEALLOCATE(ss%fidiff_dpass_co)

  IF(ALLOCATED(ss%id_fidiff_dpass_ctr)) DEALLOCATE(ss%id_fidiff_dpass_ctr)
  IF(ALLOCATED(ss%fidiff_dpass_ctr)) DEALLOCATE(ss%fidiff_dpass_ctr)

  IF(ALLOCATED(ss%id_fidiff_dtrap)) DEALLOCATE(ss%id_fidiff_dtrap)
  IF(ALLOCATED(ss%fidiff_dtrap)) DEALLOCATE(ss%fidiff_dtrap)

  IF(ALLOCATED(ss%Fi_depletion)) DEALLOCATE(ss%Fi_depletion)

  IF(ALLOCATED(ss%fracmin)) DEALLOCATE(ss%fracmin)

  IF(ALLOCATED(ss%fracpslh)) DEALLOCATE(ss%fracpslh)

  IF(ALLOCATED(ss%frac_full)) DEALLOCATE(ss%frac_full)

  IF(ALLOCATED(ss%frac_half)) DEALLOCATE(ss%frac_half)

  IF(ALLOCATED(ss%freq_drbm)) DEALLOCATE(ss%freq_drbm)

  IF(ALLOCATED(ss%freq_ec)) DEALLOCATE(ss%freq_ec)

  IF(ALLOCATED(ss%freq_etg)) DEALLOCATE(ss%freq_etg)

  IF(ALLOCATED(ss%freq_glf)) DEALLOCATE(ss%freq_glf)

  IF(ALLOCATED(ss%freq_ic)) DEALLOCATE(ss%freq_ic)

  IF(ALLOCATED(ss%freq_lh)) DEALLOCATE(ss%freq_lh)

  IF(ALLOCATED(ss%freq_max_tglf)) DEALLOCATE(ss%freq_max_tglf)

  IF(ALLOCATED(ss%id_freq_mmm71)) DEALLOCATE(ss%id_freq_mmm71)
  IF(ALLOCATED(ss%freq_mmm71)) DEALLOCATE(ss%freq_mmm71)

  IF(ALLOCATED(ss%freq_mtm)) DEALLOCATE(ss%freq_mtm)

  IF(ALLOCATED(ss%id_freq_tglf)) DEALLOCATE(ss%id_freq_tglf)
  IF(ALLOCATED(ss%freq_tglf)) DEALLOCATE(ss%freq_tglf)

  IF(ALLOCATED(ss%gamma_lmhd)) DEALLOCATE(ss%gamma_lmhd)

  IF(ALLOCATED(ss%gamma_nc)) DEALLOCATE(ss%gamma_nc)

  IF(ALLOCATED(ss%gas_atom)) DEALLOCATE(ss%gas_atom)

  IF(ALLOCATED(ss%gb1)) DEALLOCATE(ss%gb1)

  IF(ALLOCATED(ss%gb2)) DEALLOCATE(ss%gb2)

  IF(ALLOCATED(ss%gb2i)) DEALLOCATE(ss%gb2i)

  IF(ALLOCATED(ss%gbr2)) DEALLOCATE(ss%gbr2)

  IF(ALLOCATED(ss%gncfb2h)) DEALLOCATE(ss%gncfb2h)

  IF(ALLOCATED(ss%gncfh)) DEALLOCATE(ss%gncfh)

  IF(ALLOCATED(ss%gr1)) DEALLOCATE(ss%gr1)

  IF(ALLOCATED(ss%gr2)) DEALLOCATE(ss%gr2)

  IF(ALLOCATED(ss%gr2i)) DEALLOCATE(ss%gr2i)

  IF(ALLOCATED(ss%gr2rho2)) DEALLOCATE(ss%gr2rho2)

  IF(ALLOCATED(ss%gr3i)) DEALLOCATE(ss%gr3i)

  IF(ALLOCATED(ss%grho1)) DEALLOCATE(ss%grho1)

  IF(ALLOCATED(ss%grho2)) DEALLOCATE(ss%grho2)

  IF(ALLOCATED(ss%grho2b2i)) DEALLOCATE(ss%grho2b2i)

  IF(ALLOCATED(ss%grho2r2i)) DEALLOCATE(ss%grho2r2i)

  IF(ALLOCATED(ss%grho2r3i)) DEALLOCATE(ss%grho2r3i)

  IF(ALLOCATED(ss%gri)) DEALLOCATE(ss%gri)

  IF(ALLOCATED(ss%grirhoi)) DEALLOCATE(ss%grirhoi)

  IF(ALLOCATED(ss%growthrate_drbm)) DEALLOCATE(ss%growthrate_drbm)

  IF(ALLOCATED(ss%growthrate_etg)) DEALLOCATE(ss%growthrate_etg)

  IF(ALLOCATED(ss%growthrate_glf)) DEALLOCATE(ss%growthrate_glf)

  IF(ALLOCATED(ss%growthrate_max_tglf)) DEALLOCATE(ss%growthrate_max_tglf)

  IF(ALLOCATED(ss%id_growthrate_mmm71)) DEALLOCATE(ss%id_growthrate_mmm71)
  IF(ALLOCATED(ss%growthrate_mmm71)) DEALLOCATE(ss%growthrate_mmm71)

  IF(ALLOCATED(ss%growthrate_mtm)) DEALLOCATE(ss%growthrate_mtm)

  IF(ALLOCATED(ss%id_growthrate_tglf)) DEALLOCATE(ss%id_growthrate_tglf)
  IF(ALLOCATED(ss%growthrate_tglf)) DEALLOCATE(ss%growthrate_tglf)

  IF(ALLOCATED(ss%id_growthrate_vpar)) DEALLOCATE(ss%id_growthrate_vpar)
  IF(ALLOCATED(ss%growthrate_vpar)) DEALLOCATE(ss%growthrate_vpar)

  IF(ALLOCATED(ss%gs_name)) DEALLOCATE(ss%gs_name)

  IF(ALLOCATED(ss%g_eq)) DEALLOCATE(ss%g_eq)

  IF(ALLOCATED(ss%hegrilh)) DEALLOCATE(ss%hegrilh)

  IF(ALLOCATED(ss%Hsize_coil)) DEALLOCATE(ss%Hsize_coil)

  IF(ALLOCATED(ss%H_einj_standard)) DEALLOCATE(ss%H_einj_standard)

  IF(ALLOCATED(ss%H_ffull_standard)) DEALLOCATE(ss%H_ffull_standard)

  IF(ALLOCATED(ss%H_fhalf_standard)) DEALLOCATE(ss%H_fhalf_standard)

  IF(ALLOCATED(ss%icircuit_coil)) DEALLOCATE(ss%icircuit_coil)

  IF(ALLOCATED(ss%icrf_src_name)) DEALLOCATE(ss%icrf_src_name)

  IF(ALLOCATED(ss%Imag_ant_coef)) DEALLOCATE(ss%Imag_ant_coef)

  IF(ALLOCATED(ss%iota)) DEALLOCATE(ss%iota)

  IF(ALLOCATED(ss%isThermal)) DEALLOCATE(ss%isThermal)

  IF(ALLOCATED(ss%is_recycling)) DEALLOCATE(ss%is_recycling)

  IF(ALLOCATED(ss%jdotb)) DEALLOCATE(ss%jdotb)

  IF(ALLOCATED(ss%kvolt_nbi)) DEALLOCATE(ss%kvolt_nbi)

  IF(ALLOCATED(ss%kyrhos_drbm)) DEALLOCATE(ss%kyrhos_drbm)

  IF(ALLOCATED(ss%kyrhos_etg)) DEALLOCATE(ss%kyrhos_etg)

  IF(ALLOCATED(ss%kyrhos_mtm)) DEALLOCATE(ss%kyrhos_mtm)

  IF(ALLOCATED(ss%id_kyspectrum_tglf)) DEALLOCATE(ss%id_kyspectrum_tglf)
  IF(ALLOCATED(ss%kyspectrum_tglf)) DEALLOCATE(ss%kyspectrum_tglf)

  IF(ALLOCATED(ss%Lbscap)) DEALLOCATE(ss%Lbscap)

  IF(ALLOCATED(ss%Lbscap2)) DEALLOCATE(ss%Lbscap2)

  IF(ALLOCATED(ss%Lbsctan)) DEALLOCATE(ss%Lbsctan)

  IF(ALLOCATED(ss%lhrf_src_name)) DEALLOCATE(ss%lhrf_src_name)

  IF(ALLOCATED(ss%Lpol)) DEALLOCATE(ss%Lpol)

  IF(ALLOCATED(ss%Mercier_stable)) DEALLOCATE(ss%Mercier_stable)

  IF(ALLOCATED(ss%m_ALL)) DEALLOCATE(ss%m_ALL)

  IF(ALLOCATED(ss%m_ALLA)) DEALLOCATE(ss%m_ALLA)

  IF(ALLOCATED(ss%m_RFMIN)) DEALLOCATE(ss%m_RFMIN)

  IF(ALLOCATED(ss%m_S)) DEALLOCATE(ss%m_S)

  IF(ALLOCATED(ss%m_SA)) DEALLOCATE(ss%m_SA)

  IF(ALLOCATED(ss%m_SFUS)) DEALLOCATE(ss%m_SFUS)

  IF(ALLOCATED(ss%m_SGAS)) DEALLOCATE(ss%m_SGAS)

  IF(ALLOCATED(ss%m_SIMP0)) DEALLOCATE(ss%m_SIMP0)

  IF(ALLOCATED(ss%m_SIMPI)) DEALLOCATE(ss%m_SIMPI)

  IF(ALLOCATED(ss%m_SNBI)) DEALLOCATE(ss%m_SNBI)

  IF(ALLOCATED(ss%id_n0norm)) DEALLOCATE(ss%id_n0norm)
  IF(ALLOCATED(ss%n0norm)) DEALLOCATE(ss%n0norm)

  IF(ALLOCATED(ss%id_n0_halo)) DEALLOCATE(ss%id_n0_halo)
  IF(ALLOCATED(ss%n0_halo)) DEALLOCATE(ss%n0_halo)

  IF(ALLOCATED(ss%id_n0_reco)) DEALLOCATE(ss%id_n0_reco)
  IF(ALLOCATED(ss%n0_reco)) DEALLOCATE(ss%n0_reco)

  IF(ALLOCATED(ss%nbap2_shape)) DEALLOCATE(ss%nbap2_shape)

  IF(ALLOCATED(ss%nbap_shape)) DEALLOCATE(ss%nbap_shape)

  IF(ALLOCATED(ss%id_nbeami)) DEALLOCATE(ss%id_nbeami)
  IF(ALLOCATED(ss%nbeami)) DEALLOCATE(ss%nbeami)

  IF(ALLOCATED(ss%nbeami_bdy)) DEALLOCATE(ss%nbeami_bdy)

  IF(ALLOCATED(ss%nbion)) DEALLOCATE(ss%nbion)

  IF(ALLOCATED(ss%nbion_trace)) DEALLOCATE(ss%nbion_trace)

  IF(ALLOCATED(ss%nbi_src_name)) DEALLOCATE(ss%nbi_src_name)

  IF(ALLOCATED(ss%nbshape)) DEALLOCATE(ss%nbshape)

  IF(ALLOCATED(ss%id_neflux_ky_tglf)) DEALLOCATE(ss%id_neflux_ky_tglf)
  IF(ALLOCATED(ss%neflux_ky_tglf)) DEALLOCATE(ss%neflux_ky_tglf)

  IF(ALLOCATED(ss%Newcomb_code)) DEALLOCATE(ss%Newcomb_code)

  IF(ALLOCATED(ss%Newcomb_mapper)) DEALLOCATE(ss%Newcomb_mapper)

  IF(ALLOCATED(ss%Newcomb_stable)) DEALLOCATE(ss%Newcomb_stable)

  IF(ALLOCATED(ss%Newcomb_status)) DEALLOCATE(ss%Newcomb_status)

  IF(ALLOCATED(ss%id_ne_ky_tglf)) DEALLOCATE(ss%id_ne_ky_tglf)
  IF(ALLOCATED(ss%ne_ky_tglf)) DEALLOCATE(ss%ne_ky_tglf)

  IF(ALLOCATED(ss%id_nfusi)) DEALLOCATE(ss%id_nfusi)
  IF(ALLOCATED(ss%nfusi)) DEALLOCATE(ss%nfusi)

  IF(ALLOCATED(ss%nfusi_bdy)) DEALLOCATE(ss%nfusi_bdy)

  IF(ALLOCATED(ss%ngradb2_av)) DEALLOCATE(ss%ngradb2_av)

  IF(ALLOCATED(ss%ni)) DEALLOCATE(ss%ni)

  IF(ALLOCATED(ss%id_niflux_ky_tglf)) DEALLOCATE(ss%id_niflux_ky_tglf)
  IF(ALLOCATED(ss%niflux_ky_tglf)) DEALLOCATE(ss%niflux_ky_tglf)

  IF(ALLOCATED(ss%id_ni_ky_tglf)) DEALLOCATE(ss%id_ni_ky_tglf)
  IF(ALLOCATED(ss%ni_ky_tglf)) DEALLOCATE(ss%ni_ky_tglf)

  IF(ALLOCATED(ss%id_nmini)) DEALLOCATE(ss%id_nmini)
  IF(ALLOCATED(ss%nmini)) DEALLOCATE(ss%nmini)

  IF(ALLOCATED(ss%nmini_bdy)) DEALLOCATE(ss%nmini_bdy)

  IF(ALLOCATED(ss%nmodel)) DEALLOCATE(ss%nmodel)

  IF(ALLOCATED(ss%nnkparlh)) DEALLOCATE(ss%nnkparlh)

  IF(ALLOCATED(ss%nnkpollh)) DEALLOCATE(ss%nnkpollh)

  IF(ALLOCATED(ss%nnktorlh)) DEALLOCATE(ss%nnktorlh)

  IF(ALLOCATED(ss%nphi)) DEALLOCATE(ss%nphi)

  IF(ALLOCATED(ss%nrw)) DEALLOCATE(ss%nrw)

  IF(ALLOCATED(ss%nrz_antgeo)) DEALLOCATE(ss%nrz_antgeo)

  IF(ALLOCATED(ss%id_ns)) DEALLOCATE(ss%id_ns)
  IF(ALLOCATED(ss%ns)) DEALLOCATE(ss%ns)

  IF(ALLOCATED(ss%nspectrumlh_src)) DEALLOCATE(ss%nspectrumlh_src)

  IF(ALLOCATED(ss%ns_bdy)) DEALLOCATE(ss%ns_bdy)

  IF(ALLOCATED(ss%ns_is_input)) DEALLOCATE(ss%ns_is_input)

  IF(ALLOCATED(ss%nthinlh)) DEALLOCATE(ss%nthinlh)

  IF(ALLOCATED(ss%Nturns)) DEALLOCATE(ss%Nturns)

  IF(ALLOCATED(ss%num_nphi)) DEALLOCATE(ss%num_nphi)

  IF(ALLOCATED(ss%num_nphi_vac)) DEALLOCATE(ss%num_nphi_vac)

  IF(ALLOCATED(ss%id_nzflux_ky_tglf)) DEALLOCATE(ss%id_nzflux_ky_tglf)
  IF(ALLOCATED(ss%nzflux_ky_tglf)) DEALLOCATE(ss%nzflux_ky_tglf)

  IF(ALLOCATED(ss%id_nz_ky_tglf)) DEALLOCATE(ss%id_nz_ky_tglf)
  IF(ALLOCATED(ss%nz_ky_tglf)) DEALLOCATE(ss%nz_ky_tglf)

  IF(ALLOCATED(ss%n_straps)) DEALLOCATE(ss%n_straps)

  IF(ALLOCATED(ss%id_omeg0cx)) DEALLOCATE(ss%id_omeg0cx)
  IF(ALLOCATED(ss%omeg0cx)) DEALLOCATE(ss%omeg0cx)

  IF(ALLOCATED(ss%id_omeg0sc0)) DEALLOCATE(ss%id_omeg0sc0)
  IF(ALLOCATED(ss%omeg0sc0)) DEALLOCATE(ss%omeg0sc0)

  IF(ALLOCATED(ss%id_omeg0_halo)) DEALLOCATE(ss%id_omeg0_halo)
  IF(ALLOCATED(ss%omeg0_halo)) DEALLOCATE(ss%omeg0_halo)

  IF(ALLOCATED(ss%id_omeg0_reco)) DEALLOCATE(ss%id_omeg0_reco)
  IF(ALLOCATED(ss%omeg0_reco)) DEALLOCATE(ss%omeg0_reco)

  IF(ALLOCATED(ss%omegat)) DEALLOCATE(ss%omegat)

  IF(ALLOCATED(ss%omega_exb)) DEALLOCATE(ss%omega_exb)

  IF(ALLOCATED(ss%p0_reco)) DEALLOCATE(ss%p0_reco)

  IF(ALLOCATED(ss%pb0_halo)) DEALLOCATE(ss%pb0_halo)

  IF(ALLOCATED(ss%pbe)) DEALLOCATE(ss%pbe)

  IF(ALLOCATED(ss%pbi)) DEALLOCATE(ss%pbi)

  IF(ALLOCATED(ss%pbth)) DEALLOCATE(ss%pbth)

  IF(ALLOCATED(ss%pcx_halo)) DEALLOCATE(ss%pcx_halo)

  IF(ALLOCATED(ss%pcx_reco)) DEALLOCATE(ss%pcx_reco)

  IF(ALLOCATED(ss%peech)) DEALLOCATE(ss%peech)

  IF(ALLOCATED(ss%id_peech_src)) DEALLOCATE(ss%id_peech_src)
  IF(ALLOCATED(ss%peech_src)) DEALLOCATE(ss%peech_src)

  IF(ALLOCATED(ss%pelh)) DEALLOCATE(ss%pelh)

  IF(ALLOCATED(ss%id_pelh_src)) DEALLOCATE(ss%id_pelh_src)
  IF(ALLOCATED(ss%pelh_src)) DEALLOCATE(ss%pelh_src)

  IF(ALLOCATED(ss%pe_anom)) DEALLOCATE(ss%pe_anom)

  IF(ALLOCATED(ss%pe_trans)) DEALLOCATE(ss%pe_trans)

  IF(ALLOCATED(ss%id_pe_transcat)) DEALLOCATE(ss%id_pe_transcat)
  IF(ALLOCATED(ss%pe_transcat)) DEALLOCATE(ss%pe_transcat)

  IF(ALLOCATED(ss%pfuse)) DEALLOCATE(ss%pfuse)

  IF(ALLOCATED(ss%pfusi)) DEALLOCATE(ss%pfusi)

  IF(ALLOCATED(ss%pfusth)) DEALLOCATE(ss%pfusth)

  IF(ALLOCATED(ss%Phibsc)) DEALLOCATE(ss%Phibsc)

  IF(ALLOCATED(ss%id_phiflux_ky_tglf)) DEALLOCATE(ss%id_phiflux_ky_tglf)
  IF(ALLOCATED(ss%phiflux_ky_tglf)) DEALLOCATE(ss%phiflux_ky_tglf)

  IF(ALLOCATED(ss%phit)) DEALLOCATE(ss%phit)

  IF(ALLOCATED(ss%Phi_EC_launch)) DEALLOCATE(ss%Phi_EC_launch)

  IF(ALLOCATED(ss%picrf_abs)) DEALLOCATE(ss%picrf_abs)

  IF(ALLOCATED(ss%picrf_ext)) DEALLOCATE(ss%picrf_ext)

  IF(ALLOCATED(ss%id_picrf_nphi_srcs)) DEALLOCATE(ss%id_picrf_nphi_srcs)
  IF(ALLOCATED(ss%picrf_nphi_srcs)) DEALLOCATE(ss%picrf_nphi_srcs)

  IF(ALLOCATED(ss%id_picrf_srcs)) DEALLOCATE(ss%id_picrf_srcs)
  IF(ALLOCATED(ss%picrf_srcs)) DEALLOCATE(ss%picrf_srcs)

  IF(ALLOCATED(ss%id_picrf_totals)) DEALLOCATE(ss%id_picrf_totals)
  IF(ALLOCATED(ss%picrf_totals)) DEALLOCATE(ss%picrf_totals)

  IF(ALLOCATED(ss%picth)) DEALLOCATE(ss%picth)

  IF(ALLOCATED(ss%pilh)) DEALLOCATE(ss%pilh)

  IF(ALLOCATED(ss%id_pilh_src)) DEALLOCATE(ss%id_pilh_src)
  IF(ALLOCATED(ss%pilh_src)) DEALLOCATE(ss%pilh_src)

  IF(ALLOCATED(ss%pi_anom)) DEALLOCATE(ss%pi_anom)

  IF(ALLOCATED(ss%pi_trans)) DEALLOCATE(ss%pi_trans)

  IF(ALLOCATED(ss%id_pi_transcat)) DEALLOCATE(ss%id_pi_transcat)
  IF(ALLOCATED(ss%pi_transcat)) DEALLOCATE(ss%pi_transcat)

  IF(ALLOCATED(ss%pmine)) DEALLOCATE(ss%pmine)

  IF(ALLOCATED(ss%pmini)) DEALLOCATE(ss%pmini)

  IF(ALLOCATED(ss%pminth)) DEALLOCATE(ss%pminth)

  IF(ALLOCATED(ss%pohme)) DEALLOCATE(ss%pohme)

  IF(ALLOCATED(ss%id_pot_ky_tglf)) DEALLOCATE(ss%id_pot_ky_tglf)
  IF(ALLOCATED(ss%pot_ky_tglf)) DEALLOCATE(ss%pot_ky_tglf)

  IF(ALLOCATED(ss%power_ec)) DEALLOCATE(ss%power_ec)

  IF(ALLOCATED(ss%power_ic)) DEALLOCATE(ss%power_ic)

  IF(ALLOCATED(ss%power_lh)) DEALLOCATE(ss%power_lh)

  IF(ALLOCATED(ss%power_nbi)) DEALLOCATE(ss%power_nbi)

  IF(ALLOCATED(ss%power_nbi_trace)) DEALLOCATE(ss%power_nbi_trace)

  IF(ALLOCATED(ss%prad)) DEALLOCATE(ss%prad)

  IF(ALLOCATED(ss%prad_br)) DEALLOCATE(ss%prad_br)

  IF(ALLOCATED(ss%prad_cy)) DEALLOCATE(ss%prad_cy)

  IF(ALLOCATED(ss%prad_li)) DEALLOCATE(ss%prad_li)

  IF(ALLOCATED(ss%psc_halo)) DEALLOCATE(ss%psc_halo)

  IF(ALLOCATED(ss%psc_reco)) DEALLOCATE(ss%psc_reco)

  IF(ALLOCATED(ss%psipol)) DEALLOCATE(ss%psipol)

  IF(ALLOCATED(ss%PsiRZ)) DEALLOCATE(ss%PsiRZ)

  IF(ALLOCATED(ss%psmom_errck)) DEALLOCATE(ss%psmom_errck)

  IF(ALLOCATED(ss%id_psmom_nc)) DEALLOCATE(ss%id_psmom_nc)
  IF(ALLOCATED(ss%psmom_nc)) DEALLOCATE(ss%psmom_nc)

  IF(ALLOCATED(ss%psmom_num)) DEALLOCATE(ss%psmom_num)

  IF(ALLOCATED(ss%P_eq)) DEALLOCATE(ss%P_eq)

  IF(ALLOCATED(ss%qatom_ALL)) DEALLOCATE(ss%qatom_ALL)

  IF(ALLOCATED(ss%qatom_ALLA)) DEALLOCATE(ss%qatom_ALLA)

  IF(ALLOCATED(ss%qatom_RFMIN)) DEALLOCATE(ss%qatom_RFMIN)

  IF(ALLOCATED(ss%qatom_S)) DEALLOCATE(ss%qatom_S)

  IF(ALLOCATED(ss%qatom_SA)) DEALLOCATE(ss%qatom_SA)

  IF(ALLOCATED(ss%qatom_SFUS)) DEALLOCATE(ss%qatom_SFUS)

  IF(ALLOCATED(ss%qatom_SGAS)) DEALLOCATE(ss%qatom_SGAS)

  IF(ALLOCATED(ss%qatom_SIMP0)) DEALLOCATE(ss%qatom_SIMP0)

  IF(ALLOCATED(ss%qatom_SIMPI)) DEALLOCATE(ss%qatom_SIMPI)

  IF(ALLOCATED(ss%qatom_SNBI)) DEALLOCATE(ss%qatom_SNBI)

  IF(ALLOCATED(ss%id_qbeami)) DEALLOCATE(ss%id_qbeami)
  IF(ALLOCATED(ss%qbeami)) DEALLOCATE(ss%qbeami)

  IF(ALLOCATED(ss%qcomp_e)) DEALLOCATE(ss%qcomp_e)

  IF(ALLOCATED(ss%qcomp_i)) DEALLOCATE(ss%qcomp_i)

  IF(ALLOCATED(ss%qie)) DEALLOCATE(ss%qie)

  IF(ALLOCATED(ss%id_qioniz)) DEALLOCATE(ss%id_qioniz)
  IF(ALLOCATED(ss%qioniz)) DEALLOCATE(ss%qioniz)

  IF(ALLOCATED(ss%ql_operator)) DEALLOCATE(ss%ql_operator)

  IF(ALLOCATED(ss%id_qqcx)) DEALLOCATE(ss%id_qqcx)
  IF(ALLOCATED(ss%qqcx)) DEALLOCATE(ss%qqcx)

  IF(ALLOCATED(ss%qrot_conv)) DEALLOCATE(ss%qrot_conv)

  IF(ALLOCATED(ss%qrot_diff)) DEALLOCATE(ss%qrot_diff)

  IF(ALLOCATED(ss%q_ALL)) DEALLOCATE(ss%q_ALL)

  IF(ALLOCATED(ss%q_ALLA)) DEALLOCATE(ss%q_ALLA)

  IF(ALLOCATED(ss%q_eq)) DEALLOCATE(ss%q_eq)

  IF(ALLOCATED(ss%q_RFMIN)) DEALLOCATE(ss%q_RFMIN)

  IF(ALLOCATED(ss%q_S)) DEALLOCATE(ss%q_S)

  IF(ALLOCATED(ss%q_SA)) DEALLOCATE(ss%q_SA)

  IF(ALLOCATED(ss%q_SFUS)) DEALLOCATE(ss%q_SFUS)

  IF(ALLOCATED(ss%q_SGAS)) DEALLOCATE(ss%q_SGAS)

  IF(ALLOCATED(ss%q_SIMP0)) DEALLOCATE(ss%q_SIMP0)

  IF(ALLOCATED(ss%q_SIMPI)) DEALLOCATE(ss%q_SIMPI)

  IF(ALLOCATED(ss%q_SNBI)) DEALLOCATE(ss%q_SNBI)

  IF(ALLOCATED(ss%R0_momeq)) DEALLOCATE(ss%R0_momeq)

  IF(ALLOCATED(ss%id_rate_sinb0i)) DEALLOCATE(ss%id_rate_sinb0i)
  IF(ALLOCATED(ss%rate_sinb0i)) DEALLOCATE(ss%rate_sinb0i)

  IF(ALLOCATED(ss%id_rate_sinb0x)) DEALLOCATE(ss%id_rate_sinb0x)
  IF(ALLOCATED(ss%rate_sinb0x)) DEALLOCATE(ss%rate_sinb0x)

  IF(ALLOCATED(ss%id_rate_sinb0xs)) DEALLOCATE(ss%id_rate_sinb0xs)
  IF(ALLOCATED(ss%rate_sinb0xs)) DEALLOCATE(ss%rate_sinb0xs)

  IF(ALLOCATED(ss%id_rate_sinf0i)) DEALLOCATE(ss%id_rate_sinf0i)
  IF(ALLOCATED(ss%rate_sinf0i)) DEALLOCATE(ss%rate_sinf0i)

  IF(ALLOCATED(ss%id_rate_sinf0x)) DEALLOCATE(ss%id_rate_sinf0x)
  IF(ALLOCATED(ss%rate_sinf0x)) DEALLOCATE(ss%rate_sinf0x)

  IF(ALLOCATED(ss%id_rate_sinf0xs)) DEALLOCATE(ss%id_rate_sinf0xs)
  IF(ALLOCATED(ss%rate_sinf0xs)) DEALLOCATE(ss%rate_sinf0xs)

  IF(ALLOCATED(ss%Real_ant_coef)) DEALLOCATE(ss%Real_ant_coef)

  IF(ALLOCATED(ss%id_res_sn)) DEALLOCATE(ss%id_res_sn)
  IF(ALLOCATED(ss%res_sn)) DEALLOCATE(ss%res_sn)

  IF(ALLOCATED(ss%res_te)) DEALLOCATE(ss%res_te)

  IF(ALLOCATED(ss%res_ti)) DEALLOCATE(ss%res_ti)

  IF(ALLOCATED(ss%res_tq)) DEALLOCATE(ss%res_tq)

  IF(ALLOCATED(ss%RFMIN_name)) DEALLOCATE(ss%RFMIN_name)

  IF(ALLOCATED(ss%rfmin_to_all)) DEALLOCATE(ss%rfmin_to_all)

  IF(ALLOCATED(ss%rfmin_to_alla)) DEALLOCATE(ss%rfmin_to_alla)

  IF(ALLOCATED(ss%RFMIN_type)) DEALLOCATE(ss%RFMIN_type)

  IF(ALLOCATED(ss%rho)) DEALLOCATE(ss%rho)

  IF(ALLOCATED(ss%rhopsi0lh)) DEALLOCATE(ss%rhopsi0lh)

  IF(ALLOCATED(ss%rho_anom)) DEALLOCATE(ss%rho_anom)

  IF(ALLOCATED(ss%rho_anom2)) DEALLOCATE(ss%rho_anom2)

  IF(ALLOCATED(ss%rho_anomth)) DEALLOCATE(ss%rho_anomth)

  IF(ALLOCATED(ss%rho_bdy_ns)) DEALLOCATE(ss%rho_bdy_ns)

  IF(ALLOCATED(ss%rho_ecrf)) DEALLOCATE(ss%rho_ecrf)

  IF(ALLOCATED(ss%rho_eq)) DEALLOCATE(ss%rho_eq)

  IF(ALLOCATED(ss%rho_eq_geo)) DEALLOCATE(ss%rho_eq_geo)

  IF(ALLOCATED(ss%rho_fus)) DEALLOCATE(ss%rho_fus)

  IF(ALLOCATED(ss%rho_gas)) DEALLOCATE(ss%rho_gas)

  IF(ALLOCATED(ss%rho_icrf)) DEALLOCATE(ss%rho_icrf)

  IF(ALLOCATED(ss%rho_lhrf)) DEALLOCATE(ss%rho_lhrf)

  IF(ALLOCATED(ss%rho_lmhd)) DEALLOCATE(ss%rho_lmhd)

  IF(ALLOCATED(ss%rho_mist)) DEALLOCATE(ss%rho_mist)

  IF(ALLOCATED(ss%rho_nbi)) DEALLOCATE(ss%rho_nbi)

  IF(ALLOCATED(ss%rho_pel)) DEALLOCATE(ss%rho_pel)

  IF(ALLOCATED(ss%rho_rad)) DEALLOCATE(ss%rho_rad)

  IF(ALLOCATED(ss%rho_rw)) DEALLOCATE(ss%rho_rw)

  IF(ALLOCATED(ss%rlim)) DEALLOCATE(ss%rlim)

  IF(ALLOCATED(ss%Rloc_coil)) DEALLOCATE(ss%Rloc_coil)

  IF(ALLOCATED(ss%Rmajor_mean)) DEALLOCATE(ss%Rmajor_mean)

  IF(ALLOCATED(ss%rMinor_mean)) DEALLOCATE(ss%rMinor_mean)

  IF(ALLOCATED(ss%R_ant)) DEALLOCATE(ss%R_ant)

  IF(ALLOCATED(ss%R_antgeo)) DEALLOCATE(ss%R_antgeo)

  IF(ALLOCATED(ss%R_EC_launch)) DEALLOCATE(ss%R_EC_launch)

  IF(ALLOCATED(ss%R_geo)) DEALLOCATE(ss%R_geo)

  IF(ALLOCATED(ss%R_grid)) DEALLOCATE(ss%R_grid)

  IF(ALLOCATED(ss%R_midp_in)) DEALLOCATE(ss%R_midp_in)

  IF(ALLOCATED(ss%R_midp_out)) DEALLOCATE(ss%R_midp_out)

  IF(ALLOCATED(ss%R_ripple)) DEALLOCATE(ss%R_ripple)

  IF(ALLOCATED(ss%R_surfMax)) DEALLOCATE(ss%R_surfMax)

  IF(ALLOCATED(ss%R_surfMin)) DEALLOCATE(ss%R_surfMin)

  IF(ALLOCATED(ss%id_s0reco)) DEALLOCATE(ss%id_s0reco)
  IF(ALLOCATED(ss%s0reco)) DEALLOCATE(ss%s0reco)

  IF(ALLOCATED(ss%s0reco_e)) DEALLOCATE(ss%s0reco_e)

  IF(ALLOCATED(ss%id_s0reco_recap)) DEALLOCATE(ss%id_s0reco_recap)
  IF(ALLOCATED(ss%s0reco_recap)) DEALLOCATE(ss%s0reco_recap)

  IF(ALLOCATED(ss%sa_index)) DEALLOCATE(ss%sa_index)

  IF(ALLOCATED(ss%SA_name)) DEALLOCATE(ss%SA_name)

  IF(ALLOCATED(ss%SA_type)) DEALLOCATE(ss%SA_type)

  IF(ALLOCATED(ss%id_sb0halo)) DEALLOCATE(ss%id_sb0halo)
  IF(ALLOCATED(ss%sb0halo)) DEALLOCATE(ss%sb0halo)

  IF(ALLOCATED(ss%id_sb0halo_recap)) DEALLOCATE(ss%id_sb0halo_recap)
  IF(ALLOCATED(ss%sb0halo_recap)) DEALLOCATE(ss%sb0halo_recap)

  IF(ALLOCATED(ss%sbedep)) DEALLOCATE(ss%sbedep)

  IF(ALLOCATED(ss%sbehalo)) DEALLOCATE(ss%sbehalo)

  IF(ALLOCATED(ss%id_sbsce)) DEALLOCATE(ss%id_sbsce)
  IF(ALLOCATED(ss%sbsce)) DEALLOCATE(ss%sbsce)

  IF(ALLOCATED(ss%id_sbtherm)) DEALLOCATE(ss%id_sbtherm)
  IF(ALLOCATED(ss%sbtherm)) DEALLOCATE(ss%sbtherm)

  IF(ALLOCATED(ss%sc0)) DEALLOCATE(ss%sc0)

  IF(ALLOCATED(ss%sc0_to_sgas)) DEALLOCATE(ss%sc0_to_sgas)

  IF(ALLOCATED(ss%id_sc_anom)) DEALLOCATE(ss%id_sc_anom)
  IF(ALLOCATED(ss%sc_anom)) DEALLOCATE(ss%sc_anom)

  IF(ALLOCATED(ss%id_sfsce)) DEALLOCATE(ss%id_sfsce)
  IF(ALLOCATED(ss%sfsce)) DEALLOCATE(ss%sfsce)

  IF(ALLOCATED(ss%id_sftherm)) DEALLOCATE(ss%id_sftherm)
  IF(ALLOCATED(ss%sftherm)) DEALLOCATE(ss%sftherm)

  IF(ALLOCATED(ss%SFUS_name)) DEALLOCATE(ss%SFUS_name)

  IF(ALLOCATED(ss%sfus_to_all)) DEALLOCATE(ss%sfus_to_all)

  IF(ALLOCATED(ss%sfus_to_alla)) DEALLOCATE(ss%sfus_to_alla)

  IF(ALLOCATED(ss%SFUS_type)) DEALLOCATE(ss%SFUS_type)

  IF(ALLOCATED(ss%SGAS_name)) DEALLOCATE(ss%SGAS_name)

  IF(ALLOCATED(ss%sgas_to_s)) DEALLOCATE(ss%sgas_to_s)

  IF(ALLOCATED(ss%SGAS_type)) DEALLOCATE(ss%SGAS_type)

  IF(ALLOCATED(ss%id_sii_xs)) DEALLOCATE(ss%id_sii_xs)
  IF(ALLOCATED(ss%sii_xs)) DEALLOCATE(ss%sii_xs)

  IF(ALLOCATED(ss%SIMP0_name)) DEALLOCATE(ss%SIMP0_name)

  IF(ALLOCATED(ss%simp0_to_s)) DEALLOCATE(ss%simp0_to_s)

  IF(ALLOCATED(ss%SIMP0_type)) DEALLOCATE(ss%SIMP0_type)

  IF(ALLOCATED(ss%SIMPI_name)) DEALLOCATE(ss%SIMPI_name)

  IF(ALLOCATED(ss%simpi_to_s)) DEALLOCATE(ss%simpi_to_s)

  IF(ALLOCATED(ss%SIMPI_type)) DEALLOCATE(ss%SIMPI_type)

  IF(ALLOCATED(ss%SNBI_name)) DEALLOCATE(ss%SNBI_name)

  IF(ALLOCATED(ss%snbi_to_all)) DEALLOCATE(ss%snbi_to_all)

  IF(ALLOCATED(ss%snbi_to_alla)) DEALLOCATE(ss%snbi_to_alla)

  IF(ALLOCATED(ss%SNBI_type)) DEALLOCATE(ss%SNBI_type)

  IF(ALLOCATED(ss%id_sn_trans)) DEALLOCATE(ss%id_sn_trans)
  IF(ALLOCATED(ss%sn_trans)) DEALLOCATE(ss%sn_trans)

  IF(ALLOCATED(ss%id_sn_transcat)) DEALLOCATE(ss%id_sn_transcat)
  IF(ALLOCATED(ss%sn_transcat)) DEALLOCATE(ss%sn_transcat)

  IF(ALLOCATED(ss%id_sprof0)) DEALLOCATE(ss%id_sprof0)
  IF(ALLOCATED(ss%sprof0)) DEALLOCATE(ss%sprof0)

  IF(ALLOCATED(ss%id_sprof0e)) DEALLOCATE(ss%id_sprof0e)
  IF(ALLOCATED(ss%sprof0e)) DEALLOCATE(ss%sprof0e)

  IF(ALLOCATED(ss%id_spsce)) DEALLOCATE(ss%id_spsce)
  IF(ALLOCATED(ss%spsce)) DEALLOCATE(ss%spsce)

  IF(ALLOCATED(ss%squareLO)) DEALLOCATE(ss%squareLO)

  IF(ALLOCATED(ss%squareUO)) DEALLOCATE(ss%squareUO)

  IF(ALLOCATED(ss%sRtcen)) DEALLOCATE(ss%sRtcen)

  IF(ALLOCATED(ss%surf)) DEALLOCATE(ss%surf)

  IF(ALLOCATED(ss%S_name)) DEALLOCATE(ss%S_name)

  IF(ALLOCATED(ss%s_to_sa)) DEALLOCATE(ss%s_to_sa)

  IF(ALLOCATED(ss%S_type)) DEALLOCATE(ss%S_type)

  IF(ALLOCATED(ss%id_T0cx)) DEALLOCATE(ss%id_T0cx)
  IF(ALLOCATED(ss%T0cx)) DEALLOCATE(ss%T0cx)

  IF(ALLOCATED(ss%id_T0sc0)) DEALLOCATE(ss%id_T0sc0)
  IF(ALLOCATED(ss%T0sc0)) DEALLOCATE(ss%T0sc0)

  IF(ALLOCATED(ss%id_T0_halo)) DEALLOCATE(ss%id_T0_halo)
  IF(ALLOCATED(ss%T0_halo)) DEALLOCATE(ss%T0_halo)

  IF(ALLOCATED(ss%id_T0_reco)) DEALLOCATE(ss%id_T0_reco)
  IF(ALLOCATED(ss%T0_reco)) DEALLOCATE(ss%T0_reco)

  IF(ALLOCATED(ss%id_teflux_ky_tglf)) DEALLOCATE(ss%id_teflux_ky_tglf)
  IF(ALLOCATED(ss%teflux_ky_tglf)) DEALLOCATE(ss%teflux_ky_tglf)

  IF(ALLOCATED(ss%id_te_ky_tglf)) DEALLOCATE(ss%id_te_ky_tglf)
  IF(ALLOCATED(ss%te_ky_tglf)) DEALLOCATE(ss%te_ky_tglf)

  IF(ALLOCATED(ss%tfrip2_log)) DEALLOCATE(ss%tfrip2_log)

  IF(ALLOCATED(ss%tfrip_log)) DEALLOCATE(ss%tfrip_log)

  IF(ALLOCATED(ss%thegrilh)) DEALLOCATE(ss%thegrilh)

  IF(ALLOCATED(ss%th_eq)) DEALLOCATE(ss%th_eq)

  IF(ALLOCATED(ss%Ti)) DEALLOCATE(ss%Ti)

  IF(ALLOCATED(ss%id_tiflux_ky_tglf)) DEALLOCATE(ss%id_tiflux_ky_tglf)
  IF(ALLOCATED(ss%tiflux_ky_tglf)) DEALLOCATE(ss%tiflux_ky_tglf)

  IF(ALLOCATED(ss%id_ti_ky_tglf)) DEALLOCATE(ss%id_ti_ky_tglf)
  IF(ALLOCATED(ss%ti_ky_tglf)) DEALLOCATE(ss%ti_ky_tglf)

  IF(ALLOCATED(ss%tmodel)) DEALLOCATE(ss%tmodel)

  IF(ALLOCATED(ss%Tornum_balcrit)) DEALLOCATE(ss%Tornum_balcrit)

  IF(ALLOCATED(ss%Tor_mode_label)) DEALLOCATE(ss%Tor_mode_label)

  IF(ALLOCATED(ss%Tor_mode_No)) DEALLOCATE(ss%Tor_mode_No)

  IF(ALLOCATED(ss%tq0_reco)) DEALLOCATE(ss%tq0_reco)

  IF(ALLOCATED(ss%tqb0_halo)) DEALLOCATE(ss%tqb0_halo)

  IF(ALLOCATED(ss%tqbe)) DEALLOCATE(ss%tqbe)

  IF(ALLOCATED(ss%tqbi)) DEALLOCATE(ss%tqbi)

  IF(ALLOCATED(ss%tqbjxb)) DEALLOCATE(ss%tqbjxb)

  IF(ALLOCATED(ss%tqbth)) DEALLOCATE(ss%tqbth)

  IF(ALLOCATED(ss%tqcx_halo)) DEALLOCATE(ss%tqcx_halo)

  IF(ALLOCATED(ss%tqcx_reco)) DEALLOCATE(ss%tqcx_reco)

  IF(ALLOCATED(ss%id_tqioniz)) DEALLOCATE(ss%id_tqioniz)
  IF(ALLOCATED(ss%tqioniz)) DEALLOCATE(ss%tqioniz)

  IF(ALLOCATED(ss%id_tqqcx)) DEALLOCATE(ss%id_tqqcx)
  IF(ALLOCATED(ss%tqqcx)) DEALLOCATE(ss%tqqcx)

  IF(ALLOCATED(ss%tqsc_halo)) DEALLOCATE(ss%tqsc_halo)

  IF(ALLOCATED(ss%tqsc_reco)) DEALLOCATE(ss%tqsc_reco)

  IF(ALLOCATED(ss%tq_anom)) DEALLOCATE(ss%tq_anom)

  IF(ALLOCATED(ss%tq_trans)) DEALLOCATE(ss%tq_trans)

  IF(ALLOCATED(ss%id_tq_transcat)) DEALLOCATE(ss%id_tq_transcat)
  IF(ALLOCATED(ss%tq_transcat)) DEALLOCATE(ss%tq_transcat)

  IF(ALLOCATED(ss%trace_flag)) DEALLOCATE(ss%trace_flag)

  IF(ALLOCATED(ss%trcat)) DEALLOCATE(ss%trcat)

  IF(ALLOCATED(ss%triang)) DEALLOCATE(ss%triang)

  IF(ALLOCATED(ss%triangL)) DEALLOCATE(ss%triangL)

  IF(ALLOCATED(ss%triangU)) DEALLOCATE(ss%triangU)

  IF(ALLOCATED(ss%triang_miller_L)) DEALLOCATE(ss%triang_miller_L)

  IF(ALLOCATED(ss%triang_miller_U)) DEALLOCATE(ss%triang_miller_U)

  IF(ALLOCATED(ss%trmodels)) DEALLOCATE(ss%trmodels)

  IF(ALLOCATED(ss%id_Ts)) DEALLOCATE(ss%id_Ts)
  IF(ALLOCATED(ss%Ts)) DEALLOCATE(ss%Ts)

  IF(ALLOCATED(ss%Ts_is_input)) DEALLOCATE(ss%Ts_is_input)

  IF(ALLOCATED(ss%id_tzflux_ky_tglf)) DEALLOCATE(ss%id_tzflux_ky_tglf)
  IF(ALLOCATED(ss%tzflux_ky_tglf)) DEALLOCATE(ss%tzflux_ky_tglf)

  IF(ALLOCATED(ss%id_tz_ky_tglf)) DEALLOCATE(ss%id_tz_ky_tglf)
  IF(ALLOCATED(ss%tz_ky_tglf)) DEALLOCATE(ss%tz_ky_tglf)

  IF(ALLOCATED(ss%T_einj_standard)) DEALLOCATE(ss%T_einj_standard)

  IF(ALLOCATED(ss%T_ffull_standard)) DEALLOCATE(ss%T_ffull_standard)

  IF(ALLOCATED(ss%T_fhalf_standard)) DEALLOCATE(ss%T_fhalf_standard)

  IF(ALLOCATED(ss%id_upwind_pfrac_ns)) DEALLOCATE(ss%id_upwind_pfrac_ns)
  IF(ALLOCATED(ss%upwind_pfrac_ns)) DEALLOCATE(ss%upwind_pfrac_ns)

  IF(ALLOCATED(ss%upwind_pfrac_omega)) DEALLOCATE(ss%upwind_pfrac_omega)

  IF(ALLOCATED(ss%upwind_pfrac_Te)) DEALLOCATE(ss%upwind_pfrac_Te)

  IF(ALLOCATED(ss%upwind_pfrac_Ti)) DEALLOCATE(ss%upwind_pfrac_Ti)

  IF(ALLOCATED(ss%vee_trans)) DEALLOCATE(ss%vee_trans)

  IF(ALLOCATED(ss%id_vee_transcat)) DEALLOCATE(ss%id_vee_transcat)
  IF(ALLOCATED(ss%vee_transcat)) DEALLOCATE(ss%vee_transcat)

  IF(ALLOCATED(ss%velb_fusi)) DEALLOCATE(ss%velb_fusi)

  IF(ALLOCATED(ss%velb_nbi)) DEALLOCATE(ss%velb_nbi)

  IF(ALLOCATED(ss%velb_rfmi)) DEALLOCATE(ss%velb_rfmi)

  IF(ALLOCATED(ss%velne_nclass)) DEALLOCATE(ss%velne_nclass)

  IF(ALLOCATED(ss%velnh_nclass)) DEALLOCATE(ss%velnh_nclass)

  IF(ALLOCATED(ss%velni_drbm)) DEALLOCATE(ss%velni_drbm)

  IF(ALLOCATED(ss%id_velns_anom)) DEALLOCATE(ss%id_velns_anom)
  IF(ALLOCATED(ss%velns_anom)) DEALLOCATE(ss%velns_anom)

  IF(ALLOCATED(ss%id_velns_glf)) DEALLOCATE(ss%id_velns_glf)
  IF(ALLOCATED(ss%velns_glf)) DEALLOCATE(ss%velns_glf)

  IF(ALLOCATED(ss%id_velns_gtcneo)) DEALLOCATE(ss%id_velns_gtcneo)
  IF(ALLOCATED(ss%velns_gtcneo)) DEALLOCATE(ss%velns_gtcneo)

  IF(ALLOCATED(ss%id_velns_mmm71)) DEALLOCATE(ss%id_velns_mmm71)
  IF(ALLOCATED(ss%velns_mmm71)) DEALLOCATE(ss%velns_mmm71)

  IF(ALLOCATED(ss%id_velns_nclass)) DEALLOCATE(ss%id_velns_nclass)
  IF(ALLOCATED(ss%velns_nclass)) DEALLOCATE(ss%velns_nclass)

  IF(ALLOCATED(ss%id_velns_neo)) DEALLOCATE(ss%id_velns_neo)
  IF(ALLOCATED(ss%velns_neo)) DEALLOCATE(ss%velns_neo)

  IF(ALLOCATED(ss%id_velns_tglf)) DEALLOCATE(ss%id_velns_tglf)
  IF(ALLOCATED(ss%velns_tglf)) DEALLOCATE(ss%velns_tglf)

  IF(ALLOCATED(ss%velnx_nclass)) DEALLOCATE(ss%velnx_nclass)

  IF(ALLOCATED(ss%velpphi_anom)) DEALLOCATE(ss%velpphi_anom)

  IF(ALLOCATED(ss%velpphi_glf)) DEALLOCATE(ss%velpphi_glf)

  IF(ALLOCATED(ss%velpphi_gtcneo)) DEALLOCATE(ss%velpphi_gtcneo)

  IF(ALLOCATED(ss%velpphi_mmm71)) DEALLOCATE(ss%velpphi_mmm71)

  IF(ALLOCATED(ss%velpphi_neo)) DEALLOCATE(ss%velpphi_neo)

  IF(ALLOCATED(ss%velpphi_tglf)) DEALLOCATE(ss%velpphi_tglf)

  IF(ALLOCATED(ss%velte_anom)) DEALLOCATE(ss%velte_anom)

  IF(ALLOCATED(ss%velte_glf)) DEALLOCATE(ss%velte_glf)

  IF(ALLOCATED(ss%velte_gtcneo)) DEALLOCATE(ss%velte_gtcneo)

  IF(ALLOCATED(ss%velte_mmm71)) DEALLOCATE(ss%velte_mmm71)

  IF(ALLOCATED(ss%velte_nclass)) DEALLOCATE(ss%velte_nclass)

  IF(ALLOCATED(ss%velte_neo)) DEALLOCATE(ss%velte_neo)

  IF(ALLOCATED(ss%velte_paleo)) DEALLOCATE(ss%velte_paleo)

  IF(ALLOCATED(ss%velte_tglf)) DEALLOCATE(ss%velte_tglf)

  IF(ALLOCATED(ss%velti_anom)) DEALLOCATE(ss%velti_anom)

  IF(ALLOCATED(ss%velti_glf)) DEALLOCATE(ss%velti_glf)

  IF(ALLOCATED(ss%velti_gtcneo)) DEALLOCATE(ss%velti_gtcneo)

  IF(ALLOCATED(ss%velti_mmm71)) DEALLOCATE(ss%velti_mmm71)

  IF(ALLOCATED(ss%velti_nclass)) DEALLOCATE(ss%velti_nclass)

  IF(ALLOCATED(ss%velti_neo)) DEALLOCATE(ss%velti_neo)

  IF(ALLOCATED(ss%velti_tglf)) DEALLOCATE(ss%velti_tglf)

  IF(ALLOCATED(ss%vie_trans)) DEALLOCATE(ss%vie_trans)

  IF(ALLOCATED(ss%id_vie_transcat)) DEALLOCATE(ss%id_vie_transcat)
  IF(ALLOCATED(ss%vie_transcat)) DEALLOCATE(ss%vie_transcat)

  IF(ALLOCATED(ss%vmo_trans)) DEALLOCATE(ss%vmo_trans)

  IF(ALLOCATED(ss%id_vmo_transcat)) DEALLOCATE(ss%id_vmo_transcat)
  IF(ALLOCATED(ss%vmo_transcat)) DEALLOCATE(ss%vmo_transcat)

  IF(ALLOCATED(ss%id_vn_trans)) DEALLOCATE(ss%id_vn_trans)
  IF(ALLOCATED(ss%vn_trans)) DEALLOCATE(ss%vn_trans)

  IF(ALLOCATED(ss%id_vn_transcat)) DEALLOCATE(ss%id_vn_transcat)
  IF(ALLOCATED(ss%vn_transcat)) DEALLOCATE(ss%vn_transcat)

  IF(ALLOCATED(ss%vol)) DEALLOCATE(ss%vol)

  IF(ALLOCATED(ss%vphi0_av)) DEALLOCATE(ss%vphi0_av)

  IF(ALLOCATED(ss%id_vpol_inmp)) DEALLOCATE(ss%id_vpol_inmp)
  IF(ALLOCATED(ss%vpol_inmp)) DEALLOCATE(ss%vpol_inmp)

  IF(ALLOCATED(ss%vpol_is_input)) DEALLOCATE(ss%vpol_is_input)

  IF(ALLOCATED(ss%id_vpol_omp)) DEALLOCATE(ss%id_vpol_omp)
  IF(ALLOCATED(ss%vpol_omp)) DEALLOCATE(ss%vpol_omp)

  IF(ALLOCATED(ss%VRep_coil_count)) DEALLOCATE(ss%VRep_coil_count)

  IF(ALLOCATED(ss%Vrep_coil_spacing)) DEALLOCATE(ss%Vrep_coil_spacing)

  IF(ALLOCATED(ss%Vsize_coil)) DEALLOCATE(ss%Vsize_coil)

  IF(ALLOCATED(ss%id_vtor_inmp)) DEALLOCATE(ss%id_vtor_inmp)
  IF(ALLOCATED(ss%vtor_inmp)) DEALLOCATE(ss%vtor_inmp)

  IF(ALLOCATED(ss%vtor_is_input)) DEALLOCATE(ss%vtor_is_input)

  IF(ALLOCATED(ss%id_vtor_omp)) DEALLOCATE(ss%id_vtor_omp)
  IF(ALLOCATED(ss%vtor_omp)) DEALLOCATE(ss%vtor_omp)

  IF(ALLOCATED(ss%V_loop)) DEALLOCATE(ss%V_loop)

  IF(ALLOCATED(ss%id_v_pars)) DEALLOCATE(ss%id_v_pars)
  IF(ALLOCATED(ss%v_pars)) DEALLOCATE(ss%v_pars)

  IF(ALLOCATED(ss%id_v_pers)) DEALLOCATE(ss%id_v_pers)
  IF(ALLOCATED(ss%v_pers)) DEALLOCATE(ss%v_pers)

  IF(ALLOCATED(ss%wt_nphi)) DEALLOCATE(ss%wt_nphi)

  IF(ALLOCATED(ss%wt_nphi_abs)) DEALLOCATE(ss%wt_nphi_abs)

  IF(ALLOCATED(ss%wt_nphi_ext)) DEALLOCATE(ss%wt_nphi_ext)

  IF(ALLOCATED(ss%id_xRjcos_momeq)) DEALLOCATE(ss%id_xRjcos_momeq)
  IF(ALLOCATED(ss%xRjcos_momeq)) DEALLOCATE(ss%xRjcos_momeq)

  IF(ALLOCATED(ss%id_xRjsin_momeq)) DEALLOCATE(ss%id_xRjsin_momeq)
  IF(ALLOCATED(ss%xRjsin_momeq)) DEALLOCATE(ss%xRjsin_momeq)

  IF(ALLOCATED(ss%xs)) DEALLOCATE(ss%xs)

  IF(ALLOCATED(ss%xs_name)) DEALLOCATE(ss%xs_name)

  IF(ALLOCATED(ss%id_xZjcos_momeq)) DEALLOCATE(ss%id_xZjcos_momeq)
  IF(ALLOCATED(ss%xZjcos_momeq)) DEALLOCATE(ss%xZjcos_momeq)

  IF(ALLOCATED(ss%id_xZjsin_momeq)) DEALLOCATE(ss%id_xZjsin_momeq)
  IF(ALLOCATED(ss%xZjsin_momeq)) DEALLOCATE(ss%xZjsin_momeq)

  IF(ALLOCATED(ss%Z0_momeq)) DEALLOCATE(ss%Z0_momeq)

  IF(ALLOCATED(ss%Zbap)) DEALLOCATE(ss%Zbap)

  IF(ALLOCATED(ss%Zbsc)) DEALLOCATE(ss%Zbsc)

  IF(ALLOCATED(ss%Zeff)) DEALLOCATE(ss%Zeff)

  IF(ALLOCATED(ss%Zeff_fi)) DEALLOCATE(ss%Zeff_fi)

  IF(ALLOCATED(ss%Zeff_th)) DEALLOCATE(ss%Zeff_th)

  IF(ALLOCATED(ss%zlim)) DEALLOCATE(ss%zlim)

  IF(ALLOCATED(ss%Zloc_coil)) DEALLOCATE(ss%Zloc_coil)

  IF(ALLOCATED(ss%Z_ant)) DEALLOCATE(ss%Z_ant)

  IF(ALLOCATED(ss%Z_antgeo)) DEALLOCATE(ss%Z_antgeo)

  IF(ALLOCATED(ss%Z_EC_launch)) DEALLOCATE(ss%Z_EC_launch)

  IF(ALLOCATED(ss%Z_geo)) DEALLOCATE(ss%Z_geo)

  IF(ALLOCATED(ss%Z_grid)) DEALLOCATE(ss%Z_grid)

  IF(ALLOCATED(ss%Z_midp)) DEALLOCATE(ss%Z_midp)

  IF(ALLOCATED(ss%Z_mid_ant)) DEALLOCATE(ss%Z_mid_ant)

  IF(ALLOCATED(ss%Z_ripple)) DEALLOCATE(ss%Z_ripple)

  IF(ALLOCATED(ss%Z_surfMax)) DEALLOCATE(ss%Z_surfMax)

  IF(ALLOCATED(ss%Z_surfMin)) DEALLOCATE(ss%Z_surfMin)


END SUBROUTINE plasma_state_free
