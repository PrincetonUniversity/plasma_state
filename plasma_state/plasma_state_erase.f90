! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_erase(ss,ierr)

  ! deallocate state object and reset all scalars to their initial values
  !   (empty state made identical to its initial instantiation)

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss    ! state object whose elements are reinitialized
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------
  INTEGER :: iout,isize
  !-------------------

  iout = SWIM_err_out_file

  !  clear associated xplasma structures...
  call ps_init_xpl(ss%iobj,iout,ierr)  ! ierr return ignored.

  call plasma_state_free(ss,ierr)

  !  ss%itag, ss%chtag, ss%iobj not touched...

  ss%version_id = ps_version_id  ! this goes with ps code version

  ss%hash_codes = ps_unInit
  ss%new_hash_codes = ps_unInit
  ss%saved_hash_codes = ps_unInit

  ss%active_dims = 0

  ss%ANOM_Code_Info = " "

  ss%id_anom_evar = 0

  ss%id_area = 0

  ss%id_balstab = 0

  ss%id_BphiRZ = 0

  ss%id_BRRZ = 0

  ss%id_BZRZ = 0

  ss%B_axis = 0.0_rspec

  ss%B_axis_vac = 0.0_rspec

  ss%B_max_lcfs = 0.0_rspec

  ss%B_min_lcfs = 0.0_rspec

  ss%id_B_surfMax = 0

  ss%id_B_surfMin = 0

  ss%id_chie_anom = 0

  ss%id_chie_drbm = 0

  ss%id_chie_etg = 0

  ss%id_chie_glf = 0

  ss%id_chie_gtcneo = 0

  ss%id_chie_mmm71 = 0

  ss%id_chie_mtm = 0

  ss%id_chie_nclass = 0

  ss%id_chie_neo = 0

  ss%id_chie_ntm = 0

  ss%id_chie_paleo = 0

  ss%id_chie_tglf = 0

  ss%id_chie_trans = 0

  ss%id_chie_w19 = 0

  ss%id_chii_anom = 0

  ss%id_chii_drbm = 0

  ss%id_chii_glf = 0

  ss%id_chii_gtcneo = 0

  ss%id_chii_mmm71 = 0

  ss%id_chii_nclass = 0

  ss%id_chii_neo = 0

  ss%id_chii_ntm = 0

  ss%id_chii_tglf = 0

  ss%id_chii_trans = 0

  ss%id_chii_w19 = 0

  ss%id_chimo_trans = 0

  ss%id_chiphi_anom = 0

  ss%id_chiphi_drbm = 0

  ss%id_chiphi_glf = 0

  ss%id_chiphi_gtcneo = 0

  ss%id_chiphi_mmm71 = 0

  ss%id_chiphi_neo = 0

  ss%id_chiphi_tglf = 0

  ss%id_chiphi_w19 = 0

  ss%id_chitheta_mmm71 = 0

  ss%id_curbeam = 0

  ss%id_curech = 0

  ss%id_curfusn = 0

  ss%id_curich = 0

  ss%id_curlh = 0

  ss%id_curmino = 0

  ss%id_curr_bootstrap = 0

  ss%id_curr_ohmic = 0

  ss%id_curt = 0

  ss%CUR_Data_Info = " "

  ss%id_cur_rw = 0

  ss%id_db_mtm = 0

  ss%id_dcur_rw_dVloop = 0

  ss%id_difb_fusi = 0

  ss%id_difb_nbi = 0

  ss%id_difb_rfmi = 0

  ss%id_difne_nclass = 0

  ss%id_difnh_nclass = 0

  ss%id_difnx_nclass = 0

  ss%id_difnz_mmm71 = 0

  ss%dn0out = 0.0_rspec

  ss%id_dQedGrdte_tglf = 0

  ss%id_dQedGrdti_tglf = 0

  ss%id_dQidGrdte_tglf = 0

  ss%id_dQidGrdti_tglf = 0

  ss%EC_Code_Info = " "

  ss%EC_Data_Info = " "

  ss%id_ee_mobility_factor = 0

  ss%eldot = 0.0_rspec

  ss%id_elong = 0

  ss%id_eperp_rw = 0

  ss%id_epll_rw = 0

  ss%id_Epot = 0

  ss%eqdsk_file = " "

  ss%EQ_Code_Info = " "

  ss%EQ_Data_Info = " "

  ss%id_eta_parallel = 0

  ss%id_Fi_depletion = 0

  ss%id_freq_drbm = 0

  ss%id_freq_etg = 0

  ss%id_freq_glf = 0

  ss%id_freq_max_tglf = 0

  ss%id_freq_mtm = 0

  ss%FUS_Code_Info = " "

  ss%id_gamma_nc = 0

  ss%GAS_Code_Info = " "

  ss%id_gb1 = 0

  ss%id_gb2 = 0

  ss%id_gb2i = 0

  ss%id_gbr2 = 0

  ss%geometry = " "

  ss%Global_label = " "

  ss%id_gncfb2h = 0

  ss%id_gncfh = 0

  ss%id_gr1 = 0

  ss%id_gr2 = 0

  ss%id_gr2i = 0

  ss%id_gr2rho2 = 0

  ss%id_gr3i = 0

  ss%id_grho1 = 0

  ss%id_grho2 = 0

  ss%id_grho2b2i = 0

  ss%id_grho2r2i = 0

  ss%id_grho2r3i = 0

  ss%id_gri = 0

  ss%id_grirhoi = 0

  ss%id_growthrate_drbm = 0

  ss%id_growthrate_etg = 0

  ss%id_growthrate_glf = 0

  ss%id_growthrate_max_tglf = 0

  ss%id_growthrate_mtm = 0

  ss%id_g_eq = 0

  ss%IC_Code_Info = " "

  ss%IC_Data_Info = " "

  ss%interchange_R = 0.0_rspec

  ss%id_iota = 0

  ss%i_npoloidal = ps_unInit

  ss%id_jdotb = 0

  ss%kccw_Bphi = 0

  ss%kccw_Jphi = 0

  ss%kdens_rfmin = "data"

  ss%id_kyrhos_drbm = 0

  ss%id_kyrhos_etg = 0

  ss%id_kyrhos_mtm = 0

  ss%LH_Code_Info = " "

  ss%LH_Data_Info = " "

  ss%LMHD_eq_code = " "

  ss%LMHD_status = ps_unInit

  ss%lock_machine_descr = 0

  ss%lock_shot_config = 0

  ss%lock_sim_init = 0

  ss%id_Lpol = 0

  ss%max_nrz_antgeo = 0

  ss%max_nspectrumlh_src = 0

  ss%max_num_nphi = 0

  ss%max_num_nphi_vac = 0

  ss%max_n_straps = 0

  ss%MHD_eq_status = ps_unInit

  ss%mmodes = 0

  ss%nballoon = 0

  ss%nbeam = 0

  ss%NBI_Code_Info = " "

  ss%NBI_Data_Info = " "

  ss%ncircuits = 0

  ss%ncoils = 0

  ss%ndelta_w = 0

  ss%nebar = 0.0_rspec

  ss%necrf_src = 0

  ss%nefi_anom = 0

  ss%nefi_anom2 = 0

  ss%neqmom = 0

  ss%id_ngradb2_av = 0

  ss%ngsc0 = 0

  ss%id_ni = 0

  ss%nicrf_src = 0

  ss%nky_anomth = 0

  ss%nky_mmm = 0

  ss%nlhrf_src = 0

  ss%nmdifb = 0

  ss%id_nmodel = 0

  ss%nmodel_bdy = 0.0_rspec

  ss%nmom = 16

  ss%nnewcomb = 0

  ss%npsmom = 0

  ss%nr = 0

  ss%nrho = 0

  ss%nrho_anom = 0

  ss%nrho_anom2 = 0

  ss%nrho_anomth = 0

  ss%nrho_ecrf = 0

  ss%nrho_eq = 0

  ss%nrho_eq_geo = 0

  ss%nrho_fus = 0

  ss%nrho_gas = 0

  ss%nrho_icrf = 0

  ss%nrho_lhrf = 0

  ss%nrho_lmhd = 0

  ss%nrho_mist = 0

  ss%nrho_nbi = 0

  ss%nrho_pel = 0

  ss%nrho_rad = 0

  ss%nrho_rw = 0

  ss%id_nrw = 0

  ss%nr_rip = 0

  ss%nspec_all = 0

  ss%nspec_alla = 0

  ss%nspec_beam = 0

  ss%nspec_fusion = 0

  ss%nspec_gas = 0

  ss%nspec_imp0 = 0

  ss%nspec_impi = 0

  ss%nspec_rfmin = 0

  ss%nspec_th = 0

  ss%nspec_tha = 0

  ss%ns_Data_Info = " "

  ss%ntf_coil2 = ps_unInit

  ss%ntf_coils = ps_unInit

  ss%nth_eq = 0

  ss%ntrcat = 0

  ss%num_rzlim = 0

  ss%nxs = 0

  ss%nz = 0

  ss%nz_rip = 0

  ss%n_D_einj_standard = 0

  ss%n_H_einj_standard = 0

  ss%n_T_einj_standard = 0

  ss%id_omegat = 0

  ss%omegat_bdy = 0.0_rspec

  ss%id_omega_exb = 0

  ss%id_p0_reco = 0

  ss%id_pb0_halo = 0

  ss%id_pbe = 0

  ss%id_pbi = 0

  ss%id_pbth = 0

  ss%id_pcx_halo = 0

  ss%id_pcx_reco = 0

  ss%id_peech = 0

  ss%id_pelh = 0

  ss%PEL_Code_Info = " "

  ss%PEL_Data_Info = " "

  ss%id_pe_anom = 0

  ss%id_pe_trans = 0

  ss%id_pfuse = 0

  ss%id_pfusi = 0

  ss%id_pfusth = 0

  ss%id_phit = 0

  ss%id_picth = 0

  ss%id_pilh = 0

  ss%id_pi_anom = 0

  ss%id_pi_trans = 0

  ss%PLASMA_Code_Info = " "

  ss%id_pmine = 0

  ss%id_pmini = 0

  ss%id_pminth = 0

  ss%id_pohme = 0

  ss%id_prad = 0

  ss%id_prad_br = 0

  ss%id_prad_cy = 0

  ss%id_prad_li = 0

  ss%id_psc_halo = 0

  ss%id_psc_reco = 0

  ss%id_psipol = 0

  ss%id_PsiRZ = 0

  ss%Psi_to_machine_axis = 0.0_rspec

  ss%id_psmom_errck = 0

  ss%id_P_eq = 0

  ss%id_qcomp_e = 0

  ss%id_qcomp_i = 0

  ss%id_qie = 0

  ss%id_qrot_conv = 0

  ss%id_qrot_diff = 0

  ss%id_q_eq = 0

  ss%id_R0_momeq = 0

  ss%RAD_Code_Info = " "

  ss%RAD_Data_Info = " "

  ss%id_res_te = 0

  ss%id_res_ti = 0

  ss%id_res_tq = 0

  ss%rho_bdy_omegat = 0.0_rspec

  ss%rho_bdy_Te = 0.0_rspec

  ss%rho_bdy_Ti = 0.0_rspec

  ss%id_Rmajor_mean = 0

  ss%id_rMinor_mean = 0

  ss%RUNAWAY_Code_Info = " "

  ss%RunID = " "

  ss%R_axis = 0.0_rspec

  ss%id_R_geo = 0

  ss%R_max_box = 0.0_rspec

  ss%R_max_lcfs = 0.0_rspec

  ss%id_R_midp_in = 0

  ss%id_R_midp_out = 0

  ss%R_min_box = 0.0_rspec

  ss%R_min_lcfs = 0.0_rspec

  ss%id_R_surfMax = 0

  ss%id_R_surfMin = 0

  ss%id_s0reco_e = 0

  ss%id_sbedep = 0

  ss%id_sbehalo = 0

  ss%shot_number = ps_unInit

  ss%id_squareLO = 0

  ss%id_squareUO = 0

  ss%id_surf = 0

  ss%t0 = 0.0_rspec

  ss%t1 = 0.0_rspec

  ss%Te_bdy = 0.0_rspec

  ss%tfinal = 0.0_rspec

  ss%id_tfrip2_log = 0

  ss%id_tfrip_log = 0

  ss%TF_Data_Info = " "

  ss%id_Ti = 0

  ss%tinit = 0.0_rspec

  ss%Ti_bdy = 0.0_rspec

  ss%id_tmodel = 0

  ss%tmodel_bdy = 0.0_rspec

  ss%tokamak_id = " "

  ss%id_tq0_reco = 0

  ss%id_tqb0_halo = 0

  ss%id_tqbe = 0

  ss%id_tqbi = 0

  ss%id_tqbjxb = 0

  ss%id_tqbth = 0

  ss%id_tqcx_halo = 0

  ss%id_tqcx_reco = 0

  ss%id_tqsc_halo = 0

  ss%id_tqsc_reco = 0

  ss%id_tq_anom = 0

  ss%id_tq_trans = 0

  ss%id_triang = 0

  ss%id_triangL = 0

  ss%id_triangU = 0

  ss%id_triang_miller_L = 0

  ss%id_triang_miller_U = 0

  ss%Ts_Data_Info = " "

  ss%id_upwind_pfrac_omega = 0

  ss%id_upwind_pfrac_Te = 0

  ss%id_upwind_pfrac_Ti = 0

  ss%id_vee_trans = 0

  ss%id_velb_fusi = 0

  ss%id_velb_nbi = 0

  ss%id_velb_rfmi = 0

  ss%id_velne_nclass = 0

  ss%id_velnh_nclass = 0

  ss%id_velni_drbm = 0

  ss%id_velnx_nclass = 0

  ss%id_velpphi_anom = 0

  ss%id_velpphi_glf = 0

  ss%id_velpphi_gtcneo = 0

  ss%id_velpphi_mmm71 = 0

  ss%id_velpphi_neo = 0

  ss%id_velpphi_tglf = 0

  ss%id_velte_anom = 0

  ss%id_velte_glf = 0

  ss%id_velte_gtcneo = 0

  ss%id_velte_mmm71 = 0

  ss%id_velte_nclass = 0

  ss%id_velte_neo = 0

  ss%id_velte_paleo = 0

  ss%id_velte_tglf = 0

  ss%id_velti_anom = 0

  ss%id_velti_glf = 0

  ss%id_velti_gtcneo = 0

  ss%id_velti_mmm71 = 0

  ss%id_velti_nclass = 0

  ss%id_velti_neo = 0

  ss%id_velti_tglf = 0

  ss%id_vie_trans = 0

  ss%id_vmo_trans = 0

  ss%id_vol = 0

  ss%vpol_Data_Info = " "

  ss%vsur = 0.0_rspec

  ss%VSUR_Data_Info = " "

  ss%vtor_Data_Info = " "

  ss%id_V_loop = 0

  ss%Z0max = 2

  ss%id_Z0_momeq = 0

  ss%id_Zeff = 0

  ss%ZEFF_Data_Info = " "

  ss%id_Zeff_fi = 0

  ss%id_Zeff_th = 0

  ss%Zimp1 = 2

  ss%Zimp2 = 1000

  ss%Z_axis = 0.0_rspec

  ss%id_Z_geo = 0

  ss%Z_max_box = 0.0_rspec

  ss%Z_max_lcfs = 0.0_rspec

  ss%id_Z_midp = 0

  ss%Z_min_box = 0.0_rspec

  ss%Z_min_lcfs = 0.0_rspec

  ss%id_Z_surfMax = 0

  ss%id_Z_surfMin = 0


END SUBROUTINE plasma_state_erase
