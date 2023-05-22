! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


MODULE plasma_state_util_mod

  ! plasma_state header:

  use swim_global_data_mod, only: rspec, swim_err_out_file, swim_error
  implicit NONE


  SAVE
  PRIVATE

  !--------------------------------------------
  !
  !  PLASMA STATE MODULE -- UTILITY VARIABLES
  !    debug switch; counter for each defined component.
  !
  !--------------------------------------------
  !  The only SAVEd global variables are:
  !     ps_debug
  !
  !  All the rest are PARAMETERs.
  !--------------------------------------------

  INTEGER, PUBLIC :: ps_debug = 0  ! set =1 for summary debugging;
  ! set ps_debug = 2 for detailed debugging of plasma_state_store operations.

  INTEGER, PARAMETER, PUBLIC :: ps_ignore = 0
  INTEGER, PARAMETER, PUBLIC :: ps_warn = 1
  INTEGER, PARAMETER, PUBLIC :: ps_error = 2

  PUBLIC :: ps_diff
  PUBLIC :: ps_diff_c0,ps_diff_c1
  PUBLIC :: ps_diff_i0,ps_diff_i1,ps_diff_i2,ps_diff_i3
  PUBLIC :: ps_diff_r0,ps_diff_r1,ps_diff_r2

  INTERFACE ps_diff
     MODULE PROCEDURE ps_diff_c0,ps_diff_c1
     MODULE PROCEDURE ps_diff_i0,ps_diff_i1,ps_diff_i2,ps_diff_i3
     MODULE PROCEDURE ps_diff_r0,ps_diff_r1,ps_diff_r2
  END INTERFACE

  PUBLIC :: ps_conformal
  PUBLIC :: ps_conformal_c1
  PUBLIC :: ps_conformal_i1,ps_conformal_i2,ps_conformal_i3
  PUBLIC :: ps_conformal_r1,ps_conformal_r2,ps_conformal_r3,ps_conformal_r4

  INTERFACE ps_conformal
     MODULE PROCEDURE ps_conformal_c1
     MODULE PROCEDURE ps_conformal_i1,ps_conformal_i2,ps_conformal_i3
     MODULE PROCEDURE ps_conformal_r1,ps_conformal_r2,ps_conformal_r3,ps_conformal_r4
  END INTERFACE


  ! component information

  INTEGER, PARAMETER, PUBLIC :: ps_ccount = 14  ! number of known components

  ! C*32 parameter name for each component:
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_ANOM = 'ANOM'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_ANOM = &
     'Anomalous transport'
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_EC = 'EC'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_EC = &
     'Electron cyclotron heating and current drive'
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_EQ = 'EQ'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_EQ = &
     'MHD equilibrium'
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_FUS = 'FUS'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_FUS = &
     'Fusion product fast ions'
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_GAS = 'GAS'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_GAS = &
     'Neutral Gas sources & transport'
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_IC = 'IC'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_IC = &
     'Ion cyclotron heating'
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_LH = 'LH'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_LH = &
     'Lower Hybrid heating and current drive'
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_LMHD = 'LMHD'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_LMHD = &
     'Linear MHD stability'
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_NBI = 'NBI'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_NBI = &
     'Neutral beam'
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_PEL = 'PEL'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_PEL = &
     'Pellet injection'
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_PLASMA = 'PLASMA'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_PLASMA = &
     'Thermal plasma parameters; fluid profile advance'
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_RAD = 'RAD'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_RAD = &
     'Radiated Power & impurity transport'
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_RIPPLE = 'RIPPLE'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_RIPPLE = &
     'TF field ripple'
  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_RUNAWAY = 'RUNAWAY'
  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_RUNAWAY = &
     'Runaway electrons'

  ! INTEGER parameter number for each component:
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_ANOM = 1
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_EC = 2
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_EQ = 3
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_FUS = 4
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_GAS = 5
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_IC = 6
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_LH = 7
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_LMHD = 8
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_NBI = 9
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_PEL = 10
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_PLASMA = 11
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_RAD = 12
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_RIPPLE = 13
  INTEGER, PARAMETER, PUBLIC :: ps_cnum_RUNAWAY = 14

  ! array of names for each component:
  CHARACTER*32, PARAMETER, DIMENSION(ps_ccount), PUBLIC :: ps_cnames = (/ &
       ps_cname_ANOM, &
       ps_cname_EC, &
       ps_cname_EQ, &
       ps_cname_FUS, &
       ps_cname_GAS, &
       ps_cname_IC, &
       ps_cname_LH, &
       ps_cname_LMHD, &
       ps_cname_NBI, &
       ps_cname_PEL, &
       ps_cname_PLASMA, &
       ps_cname_RAD, &
       ps_cname_RIPPLE, &
       ps_cname_RUNAWAY /)

  ! array of descriptions for each component:
  CHARACTER*80, PARAMETER, DIMENSION(ps_ccount), PUBLIC :: ps_cdescrs = (/ &
       ps_cdescr_ANOM, &
       ps_cdescr_EC, &
       ps_cdescr_EQ, &
       ps_cdescr_FUS, &
       ps_cdescr_GAS, &
       ps_cdescr_IC, &
       ps_cdescr_LH, &
       ps_cdescr_LMHD, &
       ps_cdescr_NBI, &
       ps_cdescr_PEL, &
       ps_cdescr_PLASMA, &
       ps_cdescr_RAD, &
       ps_cdescr_RIPPLE, &
       ps_cdescr_RUNAWAY /)

  INTEGER, PARAMETER, DIMENSION(ps_ccount), PUBLIC :: ps_cczero = &
       (/ 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /)

  INTEGER, PARAMETER, DIMENSION(ps_ccount), PUBLIC :: ps_cc_all = &
       (/ 1,1,1,1,1,1,1,1,1,1,1,1,1,1 /)

  INTEGER, PARAMETER, DIMENSION(ps_ccount), PUBLIC :: ps_ndims = &
       (/ 7,2,10,2,4,7,3,5,6,1,7,3,2,1 /)

  ! coordinate information

  INTEGER, PARAMETER :: ps_coordCount = 6

  INTEGER, PARAMETER :: ps_coordNum_E = 1
  INTEGER, PARAMETER :: ps_coordNum_KY = 2
  INTEGER, PARAMETER :: ps_coordNum_R = 3
  INTEGER, PARAMETER :: ps_coordNum_RHO = 4
  INTEGER, PARAMETER :: ps_coordNum_TH = 5
  INTEGER, PARAMETER :: ps_coordNum_Z = 6

  CHARACTER*32, PARAMETER :: ps_coordName_E = "E"
  CHARACTER*32, PARAMETER :: ps_coordName_KY = "KY"
  CHARACTER*32, PARAMETER :: ps_coordName_R = "R"
  CHARACTER*32, PARAMETER :: ps_coordName_RHO = "RHO"
  CHARACTER*32, PARAMETER :: ps_coordName_TH = "TH"
  CHARACTER*32, PARAMETER :: ps_coordName_Z = "Z"

  INTEGER, PARAMETER, DIMENSION(ps_coordCount) :: ps_allCoords = (/ 1,1,1,1,1,1 /)
  INTEGER, PARAMETER, DIMENSION(ps_coordCount) :: ps_noCoords = (/ 0,0,0,0,0,0 /)

  CHARACTER*32, PARAMETER, DIMENSION(ps_coordCount) :: ps_coordNames = (/ &
       ps_coordName_E, &
       ps_coordName_KY, &
       ps_coordName_R, &
       ps_coordName_RHO, &
       ps_coordName_TH, &
       ps_coordName_Z /)

  ! hash addresses


  INTEGER, PARAMETER, PUBLIC :: ps_shash_machine_description = 1

  INTEGER, PARAMETER, PUBLIC :: ps_shash_Ang_Href_coil = 3
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Ang_Vref_coil = 5
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ant_model = 7
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ap2_halfheight = 9
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ap2_halfwidth = 11
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ap2_horiz_offset = 13
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ap2_vert_offset = 15
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ap_halfheight = 17
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ap_halfwidth = 19
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ap_horiz_offset = 21
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ap_vert_offset = 23
  INTEGER, PARAMETER, PUBLIC :: ps_shash_beam_type = 25
  INTEGER, PARAMETER, PUBLIC :: ps_shash_b_halfHeight = 27
  INTEGER, PARAMETER, PUBLIC :: ps_shash_b_halfwidth = 29
  INTEGER, PARAMETER, PUBLIC :: ps_shash_b_Hdivergence = 31
  INTEGER, PARAMETER, PUBLIC :: ps_shash_b_Hfocal_length = 33
  INTEGER, PARAMETER, PUBLIC :: ps_shash_b_Vdivergence = 35
  INTEGER, PARAMETER, PUBLIC :: ps_shash_b_Vfocal_length = 37
  INTEGER, PARAMETER, PUBLIC :: ps_shash_coil_in_circuit = 39
  INTEGER, PARAMETER, PUBLIC :: ps_shash_coil_resispt = 41
  INTEGER, PARAMETER, PUBLIC :: ps_shash_dx_fshield = 43
  INTEGER, PARAMETER, PUBLIC :: ps_shash_D_einj_standard = 45
  INTEGER, PARAMETER, PUBLIC :: ps_shash_D_ffull_standard = 47
  INTEGER, PARAMETER, PUBLIC :: ps_shash_D_fhalf_standard = 49
  INTEGER, PARAMETER, PUBLIC :: ps_shash_EC_Beam_Elongation = 51
  INTEGER, PARAMETER, PUBLIC :: ps_shash_EC_Half_Power_Angle = 53
  INTEGER, PARAMETER, PUBLIC :: ps_shash_EC_PRINC_CURV_HOR = 55
  INTEGER, PARAMETER, PUBLIC :: ps_shash_EC_PRINC_CURV_VERT = 57
  INTEGER, PARAMETER, PUBLIC :: ps_shash_EC_PRINC_WIDTH_HOR = 59
  INTEGER, PARAMETER, PUBLIC :: ps_shash_EC_PRINC_WIDTH_VERT = 61
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Einj_max = 63
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Einj_min = 65
  INTEGER, PARAMETER, PUBLIC :: ps_shash_geometry = 67
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Hsize_coil = 69
  INTEGER, PARAMETER, PUBLIC :: ps_shash_H_einj_standard = 71
  INTEGER, PARAMETER, PUBLIC :: ps_shash_H_ffull_standard = 73
  INTEGER, PARAMETER, PUBLIC :: ps_shash_H_fhalf_standard = 75
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Lbscap = 77
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Lbscap2 = 79
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Lbsctan = 81
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nbap2_shape = 83
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nbap_shape = 85
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nbshape = 87
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nrz_antgeo = 89
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ntf_coil2 = 91
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ntf_coils = 93
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Nturns = 95
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Phibsc = 97
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Phi_EC_launch = 99
  INTEGER, PARAMETER, PUBLIC :: ps_shash_rlim = 101
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Rloc_coil = 103
  INTEGER, PARAMETER, PUBLIC :: ps_shash_R_antgeo = 105
  INTEGER, PARAMETER, PUBLIC :: ps_shash_R_EC_launch = 107
  INTEGER, PARAMETER, PUBLIC :: ps_shash_R_max_box = 109
  INTEGER, PARAMETER, PUBLIC :: ps_shash_R_min_box = 111
  INTEGER, PARAMETER, PUBLIC :: ps_shash_sRtcen = 113
  INTEGER, PARAMETER, PUBLIC :: ps_shash_tokamak_id = 115
  INTEGER, PARAMETER, PUBLIC :: ps_shash_T_einj_standard = 117
  INTEGER, PARAMETER, PUBLIC :: ps_shash_T_ffull_standard = 119
  INTEGER, PARAMETER, PUBLIC :: ps_shash_T_fhalf_standard = 121
  INTEGER, PARAMETER, PUBLIC :: ps_shash_VRep_coil_count = 123
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Vrep_coil_spacing = 125
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Vsize_coil = 127
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Zbap = 129
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Zbsc = 131
  INTEGER, PARAMETER, PUBLIC :: ps_shash_zlim = 133
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Zloc_coil = 135
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Z_antgeo = 137
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Z_EC_launch = 139
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Z_max_box = 141
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Z_min_box = 143

  INTEGER, PARAMETER, PUBLIC :: ps_shash_shot_configuration = 145

  INTEGER, PARAMETER, PUBLIC :: ps_shash_amaxlh = 147
  INTEGER, PARAMETER, PUBLIC :: ps_shash_aminlh = 149
  INTEGER, PARAMETER, PUBLIC :: ps_shash_apolmaxlh = 151
  INTEGER, PARAMETER, PUBLIC :: ps_shash_apolminlh = 153
  INTEGER, PARAMETER, PUBLIC :: ps_shash_atormaxlh = 155
  INTEGER, PARAMETER, PUBLIC :: ps_shash_atorminlh = 157
  INTEGER, PARAMETER, PUBLIC :: ps_shash_EC_Omode_Fraction = 159
  INTEGER, PARAMETER, PUBLIC :: ps_shash_EC_phi_aim = 161
  INTEGER, PARAMETER, PUBLIC :: ps_shash_EC_theta_aim = 163
  INTEGER, PARAMETER, PUBLIC :: ps_shash_fracpslh = 165
  INTEGER, PARAMETER, PUBLIC :: ps_shash_freq_ec = 167
  INTEGER, PARAMETER, PUBLIC :: ps_shash_freq_ic = 169
  INTEGER, PARAMETER, PUBLIC :: ps_shash_freq_lh = 171
  INTEGER, PARAMETER, PUBLIC :: ps_shash_gas_atom = 173
  INTEGER, PARAMETER, PUBLIC :: ps_shash_hegrilh = 175
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Imag_ant_coef = 177
  INTEGER, PARAMETER, PUBLIC :: ps_shash_is_recycling = 179
  INTEGER, PARAMETER, PUBLIC :: ps_shash_i_npoloidal = 181
  INTEGER, PARAMETER, PUBLIC :: ps_shash_kccw_Bphi = 183
  INTEGER, PARAMETER, PUBLIC :: ps_shash_kccw_Jphi = 185
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nbion = 187
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nbion_trace = 189
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nnkparlh = 191
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nnkpollh = 193
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nnktorlh = 195
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nphi = 197
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nspectrumlh_src = 199
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nthinlh = 201
  INTEGER, PARAMETER, PUBLIC :: ps_shash_num_nphi = 203
  INTEGER, PARAMETER, PUBLIC :: ps_shash_num_nphi_vac = 205
  INTEGER, PARAMETER, PUBLIC :: ps_shash_n_straps = 207
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Real_ant_coef = 209
  INTEGER, PARAMETER, PUBLIC :: ps_shash_rhopsi0lh = 211
  INTEGER, PARAMETER, PUBLIC :: ps_shash_R_ant = 213
  INTEGER, PARAMETER, PUBLIC :: ps_shash_shot_number = 215
  INTEGER, PARAMETER, PUBLIC :: ps_shash_thegrilh = 217
  INTEGER, PARAMETER, PUBLIC :: ps_shash_wt_nphi = 219
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Z_ant = 221
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Z_mid_ant = 223

  INTEGER, PARAMETER, PUBLIC :: ps_shash_simulation_init = 225

  INTEGER, PARAMETER, PUBLIC :: ps_shash_alla_index = 227
  INTEGER, PARAMETER, PUBLIC :: ps_shash_all_index = 229
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ANOM_Code_Info = 231
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Balloon_mapper = 233
  INTEGER, PARAMETER, PUBLIC :: ps_shash_CUR_Data_Info = 235
  INTEGER, PARAMETER, PUBLIC :: ps_shash_DeltaW_mapper = 237
  INTEGER, PARAMETER, PUBLIC :: ps_shash_EC_Code_Info = 239
  INTEGER, PARAMETER, PUBLIC :: ps_shash_EC_Data_Info = 241
  INTEGER, PARAMETER, PUBLIC :: ps_shash_EQ_Code_Info = 243
  INTEGER, PARAMETER, PUBLIC :: ps_shash_EQ_Data_Info = 245
  INTEGER, PARAMETER, PUBLIC :: ps_shash_fracmin = 247
  INTEGER, PARAMETER, PUBLIC :: ps_shash_FUS_Code_Info = 249
  INTEGER, PARAMETER, PUBLIC :: ps_shash_GAS_Code_Info = 251
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Global_label = 253
  INTEGER, PARAMETER, PUBLIC :: ps_shash_icircuit_coil = 255
  INTEGER, PARAMETER, PUBLIC :: ps_shash_IC_Code_Info = 257
  INTEGER, PARAMETER, PUBLIC :: ps_shash_IC_Data_Info = 259
  INTEGER, PARAMETER, PUBLIC :: ps_shash_kdens_rfmin = 261
  INTEGER, PARAMETER, PUBLIC :: ps_shash_LH_Code_Info = 263
  INTEGER, PARAMETER, PUBLIC :: ps_shash_LH_Data_Info = 265
  INTEGER, PARAMETER, PUBLIC :: ps_shash_LMHD_eq_code = 267
  INTEGER, PARAMETER, PUBLIC :: ps_shash_NBI_Code_Info = 269
  INTEGER, PARAMETER, PUBLIC :: ps_shash_NBI_Data_Info = 271
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Newcomb_mapper = 273
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nmom = 275
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ns_Data_Info = 277
  INTEGER, PARAMETER, PUBLIC :: ps_shash_PEL_Code_Info = 279
  INTEGER, PARAMETER, PUBLIC :: ps_shash_PEL_Data_Info = 281
  INTEGER, PARAMETER, PUBLIC :: ps_shash_PLASMA_Code_Info = 283
  INTEGER, PARAMETER, PUBLIC :: ps_shash_RAD_Code_Info = 285
  INTEGER, PARAMETER, PUBLIC :: ps_shash_RAD_Data_Info = 287
  INTEGER, PARAMETER, PUBLIC :: ps_shash_rfmin_to_all = 289
  INTEGER, PARAMETER, PUBLIC :: ps_shash_rfmin_to_alla = 291
  INTEGER, PARAMETER, PUBLIC :: ps_shash_RUNAWAY_Code_Info = 293
  INTEGER, PARAMETER, PUBLIC :: ps_shash_RunID = 295
  INTEGER, PARAMETER, PUBLIC :: ps_shash_sa_index = 297
  INTEGER, PARAMETER, PUBLIC :: ps_shash_sc0_to_sgas = 299
  INTEGER, PARAMETER, PUBLIC :: ps_shash_sfus_to_all = 301
  INTEGER, PARAMETER, PUBLIC :: ps_shash_sfus_to_alla = 303
  INTEGER, PARAMETER, PUBLIC :: ps_shash_sgas_to_s = 305
  INTEGER, PARAMETER, PUBLIC :: ps_shash_simp0_to_s = 307
  INTEGER, PARAMETER, PUBLIC :: ps_shash_simpi_to_s = 309
  INTEGER, PARAMETER, PUBLIC :: ps_shash_snbi_to_all = 311
  INTEGER, PARAMETER, PUBLIC :: ps_shash_snbi_to_alla = 313
  INTEGER, PARAMETER, PUBLIC :: ps_shash_s_to_sa = 315
  INTEGER, PARAMETER, PUBLIC :: ps_shash_tfinal = 317
  INTEGER, PARAMETER, PUBLIC :: ps_shash_TF_Data_Info = 319
  INTEGER, PARAMETER, PUBLIC :: ps_shash_tinit = 321
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Tor_mode_No = 323
  INTEGER, PARAMETER, PUBLIC :: ps_shash_trace_flag = 325
  INTEGER, PARAMETER, PUBLIC :: ps_shash_trmodels = 327
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Ts_Data_Info = 329
  INTEGER, PARAMETER, PUBLIC :: ps_shash_vpol_Data_Info = 331
  INTEGER, PARAMETER, PUBLIC :: ps_shash_VSUR_Data_Info = 333
  INTEGER, PARAMETER, PUBLIC :: ps_shash_vtor_Data_Info = 335
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Z0max = 337
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ZEFF_Data_Info = 339
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Zimp1 = 341
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Zimp2 = 343

  INTEGER, PARAMETER, PUBLIC :: ps_shash_state_data = 345

  INTEGER, PARAMETER, PUBLIC :: ps_shash_Balloon_stable = 347
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Balloon_status = 349
  INTEGER, PARAMETER, PUBLIC :: ps_shash_B_axis = 351
  INTEGER, PARAMETER, PUBLIC :: ps_shash_B_axis_vac = 353
  INTEGER, PARAMETER, PUBLIC :: ps_shash_B_max_lcfs = 355
  INTEGER, PARAMETER, PUBLIC :: ps_shash_B_min_lcfs = 357
  INTEGER, PARAMETER, PUBLIC :: ps_shash_coil_apt = 359
  INTEGER, PARAMETER, PUBLIC :: ps_shash_DeltaW_stable = 361
  INTEGER, PARAMETER, PUBLIC :: ps_shash_DeltaW_status = 363
  INTEGER, PARAMETER, PUBLIC :: ps_shash_dist_fun = 365
  INTEGER, PARAMETER, PUBLIC :: ps_shash_dn0out = 367
  INTEGER, PARAMETER, PUBLIC :: ps_shash_e0_av = 369
  INTEGER, PARAMETER, PUBLIC :: ps_shash_eldot = 371
  INTEGER, PARAMETER, PUBLIC :: ps_shash_eqdsk_file = 373
  INTEGER, PARAMETER, PUBLIC :: ps_shash_frac_full = 375
  INTEGER, PARAMETER, PUBLIC :: ps_shash_frac_half = 377
  INTEGER, PARAMETER, PUBLIC :: ps_shash_gamma_lmhd = 379
  INTEGER, PARAMETER, PUBLIC :: ps_shash_interchange_R = 381
  INTEGER, PARAMETER, PUBLIC :: ps_shash_isThermal = 383
  INTEGER, PARAMETER, PUBLIC :: ps_shash_kvolt_nbi = 385
  INTEGER, PARAMETER, PUBLIC :: ps_shash_LMHD_status = 387
  INTEGER, PARAMETER, PUBLIC :: ps_shash_lock_machine_descr = 389
  INTEGER, PARAMETER, PUBLIC :: ps_shash_lock_shot_config = 391
  INTEGER, PARAMETER, PUBLIC :: ps_shash_lock_sim_init = 393
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Mercier_stable = 395
  INTEGER, PARAMETER, PUBLIC :: ps_shash_MHD_eq_status = 397
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nbeami_bdy = 399
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nebar = 401
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Newcomb_stable = 403
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Newcomb_status = 405
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nfusi_bdy = 407
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nmdifb = 409
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nmini_bdy = 411
  INTEGER, PARAMETER, PUBLIC :: ps_shash_nmodel_bdy = 413
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ns_bdy = 415
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ns_is_input = 417
  INTEGER, PARAMETER, PUBLIC :: ps_shash_omegat_bdy = 419
  INTEGER, PARAMETER, PUBLIC :: ps_shash_picrf_abs = 421
  INTEGER, PARAMETER, PUBLIC :: ps_shash_picrf_ext = 423
  INTEGER, PARAMETER, PUBLIC :: ps_shash_power_ec = 425
  INTEGER, PARAMETER, PUBLIC :: ps_shash_power_ic = 427
  INTEGER, PARAMETER, PUBLIC :: ps_shash_power_lh = 429
  INTEGER, PARAMETER, PUBLIC :: ps_shash_power_nbi = 431
  INTEGER, PARAMETER, PUBLIC :: ps_shash_power_nbi_trace = 433
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Psi_to_machine_axis = 435
  INTEGER, PARAMETER, PUBLIC :: ps_shash_ql_operator = 437
  INTEGER, PARAMETER, PUBLIC :: ps_shash_rho_bdy_ns = 439
  INTEGER, PARAMETER, PUBLIC :: ps_shash_rho_bdy_omegat = 441
  INTEGER, PARAMETER, PUBLIC :: ps_shash_rho_bdy_Te = 443
  INTEGER, PARAMETER, PUBLIC :: ps_shash_rho_bdy_Ti = 445
  INTEGER, PARAMETER, PUBLIC :: ps_shash_R_axis = 447
  INTEGER, PARAMETER, PUBLIC :: ps_shash_R_max_lcfs = 449
  INTEGER, PARAMETER, PUBLIC :: ps_shash_R_min_lcfs = 451
  INTEGER, PARAMETER, PUBLIC :: ps_shash_sc0 = 453
  INTEGER, PARAMETER, PUBLIC :: ps_shash_t0 = 455
  INTEGER, PARAMETER, PUBLIC :: ps_shash_t1 = 457
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Te_bdy = 459
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Ti_bdy = 461
  INTEGER, PARAMETER, PUBLIC :: ps_shash_tmodel_bdy = 463
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Tornum_balcrit = 465
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Ts_is_input = 467
  INTEGER, PARAMETER, PUBLIC :: ps_shash_vphi0_av = 469
  INTEGER, PARAMETER, PUBLIC :: ps_shash_vpol_is_input = 471
  INTEGER, PARAMETER, PUBLIC :: ps_shash_vsur = 473
  INTEGER, PARAMETER, PUBLIC :: ps_shash_vtor_is_input = 475
  INTEGER, PARAMETER, PUBLIC :: ps_shash_wt_nphi_abs = 477
  INTEGER, PARAMETER, PUBLIC :: ps_shash_wt_nphi_ext = 479
  INTEGER, PARAMETER, PUBLIC :: ps_shash_xs = 481
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Z_axis = 483
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Z_max_lcfs = 485
  INTEGER, PARAMETER, PUBLIC :: ps_shash_Z_min_lcfs = 487

  INTEGER, PARAMETER, PUBLIC :: ps_shash_state_profiles = 489


  INTEGER, PARAMETER, PUBLIC :: ps_dhash_max_nrz_antgeo = 491
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_max_nspectrumlh_src = 493
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_max_num_nphi = 495
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_max_num_nphi_vac = 497
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_max_n_straps = 499
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_mmodes = 501
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nballoon = 503
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nbeam = 505
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_ncircuits = 507
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_ncoils = 509
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_ndelta_w = 511
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_necrf_src = 513
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nefi_anom = 515
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nefi_anom2 = 517
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_neqmom = 519
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_ngsc0 = 521
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nicrf_src = 523
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nky_anomth = 525
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nky_mmm = 527
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nlhrf_src = 529
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nnewcomb = 531
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_npsmom = 533
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nr = 535
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho = 537
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_anom = 539
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_anom2 = 541
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_anomth = 543
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_ecrf = 545
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_eq = 547
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_eq_geo = 549
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_fus = 551
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_gas = 553
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_icrf = 555
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_lhrf = 557
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_lmhd = 559
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_mist = 561
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_nbi = 563
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_pel = 565
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_rad = 567
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nrho_rw = 569
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nr_rip = 571
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nspec_all = 573
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nspec_alla = 575
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nspec_beam = 577
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nspec_fusion = 579
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nspec_gas = 581
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nspec_imp0 = 583
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nspec_impi = 585
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nspec_rfmin = 587
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nspec_th = 589
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nspec_tha = 591
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nth_eq = 593
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_ntrcat = 595
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_num_rzlim = 597
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nxs = 599
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nz = 601
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_nz_rip = 603
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_n_d_einj_standard = 605
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_n_h_einj_standard = 607
  INTEGER, PARAMETER, PUBLIC :: ps_dhash_n_t_einj_standard = 609

  INTEGER, PARAMETER, PUBLIC :: ps_phash_anom_evar = 611
  INTEGER, PARAMETER, PUBLIC :: ps_phash_area = 613
  INTEGER, PARAMETER, PUBLIC :: ps_phash_balstab = 615
  INTEGER, PARAMETER, PUBLIC :: ps_phash_bidiff_bpass_co = 617
  INTEGER, PARAMETER, PUBLIC :: ps_phash_bidiff_bpass_ctr = 619
  INTEGER, PARAMETER, PUBLIC :: ps_phash_bidiff_btrap = 621
  INTEGER, PARAMETER, PUBLIC :: ps_phash_bidiff_dpass_co = 623
  INTEGER, PARAMETER, PUBLIC :: ps_phash_bidiff_dpass_ctr = 625
  INTEGER, PARAMETER, PUBLIC :: ps_phash_bidiff_dtrap = 627
  INTEGER, PARAMETER, PUBLIC :: ps_phash_bphirz = 629
  INTEGER, PARAMETER, PUBLIC :: ps_phash_brrz = 631
  INTEGER, PARAMETER, PUBLIC :: ps_phash_bzrz = 633
  INTEGER, PARAMETER, PUBLIC :: ps_phash_b_surfmax = 635
  INTEGER, PARAMETER, PUBLIC :: ps_phash_b_surfmin = 637
  INTEGER, PARAMETER, PUBLIC :: ps_phash_cdicrf = 639
  INTEGER, PARAMETER, PUBLIC :: ps_phash_cdicrf_nphi = 641
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_anom = 643
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_drbm = 645
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_etg = 647
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_glf = 649
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_gtcneo = 651
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_mmm71 = 653
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_mtm = 655
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_nclass = 657
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_neo = 659
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_ntm = 661
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_paleo = 663
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_tglf = 665
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_trans = 667
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_transcat = 669
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chie_w19 = 671
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chii_anom = 673
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chii_drbm = 675
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chii_glf = 677
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chii_gtcneo = 679
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chii_mmm71 = 681
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chii_nclass = 683
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chii_neo = 685
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chii_ntm = 687
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chii_tglf = 689
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chii_trans = 691
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chii_transcat = 693
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chii_w19 = 695
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chimo_trans = 697
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chimo_transcat = 699
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chiphi_anom = 701
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chiphi_drbm = 703
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chiphi_glf = 705
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chiphi_gtcneo = 707
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chiphi_mmm71 = 709
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chiphi_neo = 711
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chiphi_tglf = 713
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chiphi_w19 = 715
  INTEGER, PARAMETER, PUBLIC :: ps_phash_chitheta_mmm71 = 717
  INTEGER, PARAMETER, PUBLIC :: ps_phash_curbeam = 719
  INTEGER, PARAMETER, PUBLIC :: ps_phash_curech = 721
  INTEGER, PARAMETER, PUBLIC :: ps_phash_curech_src = 723
  INTEGER, PARAMETER, PUBLIC :: ps_phash_curfusn = 725
  INTEGER, PARAMETER, PUBLIC :: ps_phash_curich = 727
  INTEGER, PARAMETER, PUBLIC :: ps_phash_curlh = 729
  INTEGER, PARAMETER, PUBLIC :: ps_phash_curlh_src = 731
  INTEGER, PARAMETER, PUBLIC :: ps_phash_curmino = 733
  INTEGER, PARAMETER, PUBLIC :: ps_phash_curr_bootstrap = 735
  INTEGER, PARAMETER, PUBLIC :: ps_phash_curr_ohmic = 737
  INTEGER, PARAMETER, PUBLIC :: ps_phash_curt = 739
  INTEGER, PARAMETER, PUBLIC :: ps_phash_cur_rw = 741
  INTEGER, PARAMETER, PUBLIC :: ps_phash_d2r_geo_drhodth = 743
  INTEGER, PARAMETER, PUBLIC :: ps_phash_d2z_geo_drhodth = 745
  INTEGER, PARAMETER, PUBLIC :: ps_phash_db_mtm = 747
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dcur_rw_dvloop = 749
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difb_fusi = 751
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difb_nbi = 753
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difb_rfmi = 755
  INTEGER, PARAMETER, PUBLIC :: ps_phash_diff_trans = 757
  INTEGER, PARAMETER, PUBLIC :: ps_phash_diff_transcat = 759
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difne_nclass = 761
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difnh_nclass = 763
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difns_anom = 765
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difns_drbm = 767
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difns_glf = 769
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difns_gtcneo = 771
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difns_mmm71 = 773
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difns_nclass = 775
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difns_neo = 777
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difns_tglf = 779
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difns_w19 = 781
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difnx_nclass = 783
  INTEGER, PARAMETER, PUBLIC :: ps_phash_difnz_mmm71 = 785
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dqedgrdte_tglf = 787
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dqedgrdti_tglf = 789
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dqidgrdte_tglf = 791
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dqidgrdti_tglf = 793
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dr0_momeq_drho = 795
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dr_geo_drho = 797
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dr_geo_dth = 799
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dxrjcos_momeq_drho = 801
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dxrjsin_momeq_drho = 803
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dxzjcos_momeq_drho = 805
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dxzjsin_momeq_drho = 807
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dz0_momeq_drho = 809
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dz_geo_drho = 811
  INTEGER, PARAMETER, PUBLIC :: ps_phash_dz_geo_dth = 813
  INTEGER, PARAMETER, PUBLIC :: ps_phash_ee_mobility_factor = 815
  INTEGER, PARAMETER, PUBLIC :: ps_phash_elong = 817
  INTEGER, PARAMETER, PUBLIC :: ps_phash_eperp_beami = 819
  INTEGER, PARAMETER, PUBLIC :: ps_phash_eperp_fusi = 821
  INTEGER, PARAMETER, PUBLIC :: ps_phash_eperp_mini = 823
  INTEGER, PARAMETER, PUBLIC :: ps_phash_eperp_rw = 825
  INTEGER, PARAMETER, PUBLIC :: ps_phash_epll_beami = 827
  INTEGER, PARAMETER, PUBLIC :: ps_phash_epll_fusi = 829
  INTEGER, PARAMETER, PUBLIC :: ps_phash_epll_mini = 831
  INTEGER, PARAMETER, PUBLIC :: ps_phash_epll_rw = 833
  INTEGER, PARAMETER, PUBLIC :: ps_phash_epot = 835
  INTEGER, PARAMETER, PUBLIC :: ps_phash_eta_parallel = 837
  INTEGER, PARAMETER, PUBLIC :: ps_phash_fidiff_bpass_co = 839
  INTEGER, PARAMETER, PUBLIC :: ps_phash_fidiff_bpass_ctr = 841
  INTEGER, PARAMETER, PUBLIC :: ps_phash_fidiff_btrap = 843
  INTEGER, PARAMETER, PUBLIC :: ps_phash_fidiff_dpass_co = 845
  INTEGER, PARAMETER, PUBLIC :: ps_phash_fidiff_dpass_ctr = 847
  INTEGER, PARAMETER, PUBLIC :: ps_phash_fidiff_dtrap = 849
  INTEGER, PARAMETER, PUBLIC :: ps_phash_fi_depletion = 851
  INTEGER, PARAMETER, PUBLIC :: ps_phash_freq_drbm = 853
  INTEGER, PARAMETER, PUBLIC :: ps_phash_freq_etg = 855
  INTEGER, PARAMETER, PUBLIC :: ps_phash_freq_glf = 857
  INTEGER, PARAMETER, PUBLIC :: ps_phash_freq_max_tglf = 859
  INTEGER, PARAMETER, PUBLIC :: ps_phash_freq_mmm71 = 861
  INTEGER, PARAMETER, PUBLIC :: ps_phash_freq_mtm = 863
  INTEGER, PARAMETER, PUBLIC :: ps_phash_freq_tglf = 865
  INTEGER, PARAMETER, PUBLIC :: ps_phash_gamma_nc = 867
  INTEGER, PARAMETER, PUBLIC :: ps_phash_gb1 = 869
  INTEGER, PARAMETER, PUBLIC :: ps_phash_gb2 = 871
  INTEGER, PARAMETER, PUBLIC :: ps_phash_gb2i = 873
  INTEGER, PARAMETER, PUBLIC :: ps_phash_gbr2 = 875
  INTEGER, PARAMETER, PUBLIC :: ps_phash_gncfb2h = 877
  INTEGER, PARAMETER, PUBLIC :: ps_phash_gncfh = 879
  INTEGER, PARAMETER, PUBLIC :: ps_phash_gr1 = 881
  INTEGER, PARAMETER, PUBLIC :: ps_phash_gr2 = 883
  INTEGER, PARAMETER, PUBLIC :: ps_phash_gr2i = 885
  INTEGER, PARAMETER, PUBLIC :: ps_phash_gr2rho2 = 887
  INTEGER, PARAMETER, PUBLIC :: ps_phash_gr3i = 889
  INTEGER, PARAMETER, PUBLIC :: ps_phash_grho1 = 891
  INTEGER, PARAMETER, PUBLIC :: ps_phash_grho2 = 893
  INTEGER, PARAMETER, PUBLIC :: ps_phash_grho2b2i = 895
  INTEGER, PARAMETER, PUBLIC :: ps_phash_grho2r2i = 897
  INTEGER, PARAMETER, PUBLIC :: ps_phash_grho2r3i = 899
  INTEGER, PARAMETER, PUBLIC :: ps_phash_gri = 901
  INTEGER, PARAMETER, PUBLIC :: ps_phash_grirhoi = 903
  INTEGER, PARAMETER, PUBLIC :: ps_phash_growthrate_drbm = 905
  INTEGER, PARAMETER, PUBLIC :: ps_phash_growthrate_etg = 907
  INTEGER, PARAMETER, PUBLIC :: ps_phash_growthrate_glf = 909
  INTEGER, PARAMETER, PUBLIC :: ps_phash_growthrate_max_tglf = 911
  INTEGER, PARAMETER, PUBLIC :: ps_phash_growthrate_mmm71 = 913
  INTEGER, PARAMETER, PUBLIC :: ps_phash_growthrate_mtm = 915
  INTEGER, PARAMETER, PUBLIC :: ps_phash_growthrate_tglf = 917
  INTEGER, PARAMETER, PUBLIC :: ps_phash_growthrate_vpar = 919
  INTEGER, PARAMETER, PUBLIC :: ps_phash_g_eq = 921
  INTEGER, PARAMETER, PUBLIC :: ps_phash_iota = 923
  INTEGER, PARAMETER, PUBLIC :: ps_phash_jdotb = 925
  INTEGER, PARAMETER, PUBLIC :: ps_phash_kyrhos_drbm = 927
  INTEGER, PARAMETER, PUBLIC :: ps_phash_kyrhos_etg = 929
  INTEGER, PARAMETER, PUBLIC :: ps_phash_kyrhos_mtm = 931
  INTEGER, PARAMETER, PUBLIC :: ps_phash_kyspectrum_tglf = 933
  INTEGER, PARAMETER, PUBLIC :: ps_phash_lpol = 935
  INTEGER, PARAMETER, PUBLIC :: ps_phash_n0norm = 937
  INTEGER, PARAMETER, PUBLIC :: ps_phash_n0_halo = 939
  INTEGER, PARAMETER, PUBLIC :: ps_phash_n0_reco = 941
  INTEGER, PARAMETER, PUBLIC :: ps_phash_nbeami = 943
  INTEGER, PARAMETER, PUBLIC :: ps_phash_neflux_ky_tglf = 945
  INTEGER, PARAMETER, PUBLIC :: ps_phash_ne_ky_tglf = 947
  INTEGER, PARAMETER, PUBLIC :: ps_phash_nfusi = 949
  INTEGER, PARAMETER, PUBLIC :: ps_phash_ngradb2_av = 951
  INTEGER, PARAMETER, PUBLIC :: ps_phash_ni = 953
  INTEGER, PARAMETER, PUBLIC :: ps_phash_niflux_ky_tglf = 955
  INTEGER, PARAMETER, PUBLIC :: ps_phash_ni_ky_tglf = 957
  INTEGER, PARAMETER, PUBLIC :: ps_phash_nmini = 959
  INTEGER, PARAMETER, PUBLIC :: ps_phash_nmodel = 961
  INTEGER, PARAMETER, PUBLIC :: ps_phash_nrw = 963
  INTEGER, PARAMETER, PUBLIC :: ps_phash_ns = 965
  INTEGER, PARAMETER, PUBLIC :: ps_phash_nzflux_ky_tglf = 967
  INTEGER, PARAMETER, PUBLIC :: ps_phash_nz_ky_tglf = 969
  INTEGER, PARAMETER, PUBLIC :: ps_phash_omeg0cx = 971
  INTEGER, PARAMETER, PUBLIC :: ps_phash_omeg0sc0 = 973
  INTEGER, PARAMETER, PUBLIC :: ps_phash_omeg0_halo = 975
  INTEGER, PARAMETER, PUBLIC :: ps_phash_omeg0_reco = 977
  INTEGER, PARAMETER, PUBLIC :: ps_phash_omegat = 979
  INTEGER, PARAMETER, PUBLIC :: ps_phash_omega_exb = 981
  INTEGER, PARAMETER, PUBLIC :: ps_phash_p0_reco = 983
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pb0_halo = 985
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pbe = 987
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pbi = 989
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pbth = 991
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pcx_halo = 993
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pcx_reco = 995
  INTEGER, PARAMETER, PUBLIC :: ps_phash_peech = 997
  INTEGER, PARAMETER, PUBLIC :: ps_phash_peech_src = 999
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pelh = 1001
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pelh_src = 1003
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pe_anom = 1005
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pe_trans = 1007
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pe_transcat = 1009
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pfuse = 1011
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pfusi = 1013
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pfusth = 1015
  INTEGER, PARAMETER, PUBLIC :: ps_phash_phiflux_ky_tglf = 1017
  INTEGER, PARAMETER, PUBLIC :: ps_phash_phit = 1019
  INTEGER, PARAMETER, PUBLIC :: ps_phash_picrf_nphi_srcs = 1021
  INTEGER, PARAMETER, PUBLIC :: ps_phash_picrf_srcs = 1023
  INTEGER, PARAMETER, PUBLIC :: ps_phash_picrf_totals = 1025
  INTEGER, PARAMETER, PUBLIC :: ps_phash_picth = 1027
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pilh = 1029
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pilh_src = 1031
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pi_anom = 1033
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pi_trans = 1035
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pi_transcat = 1037
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pmine = 1039
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pmini = 1041
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pminth = 1043
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pohme = 1045
  INTEGER, PARAMETER, PUBLIC :: ps_phash_pot_ky_tglf = 1047
  INTEGER, PARAMETER, PUBLIC :: ps_phash_prad = 1049
  INTEGER, PARAMETER, PUBLIC :: ps_phash_prad_br = 1051
  INTEGER, PARAMETER, PUBLIC :: ps_phash_prad_cy = 1053
  INTEGER, PARAMETER, PUBLIC :: ps_phash_prad_li = 1055
  INTEGER, PARAMETER, PUBLIC :: ps_phash_psc_halo = 1057
  INTEGER, PARAMETER, PUBLIC :: ps_phash_psc_reco = 1059
  INTEGER, PARAMETER, PUBLIC :: ps_phash_psipol = 1061
  INTEGER, PARAMETER, PUBLIC :: ps_phash_psirz = 1063
  INTEGER, PARAMETER, PUBLIC :: ps_phash_psmom_errck = 1065
  INTEGER, PARAMETER, PUBLIC :: ps_phash_psmom_nc = 1067
  INTEGER, PARAMETER, PUBLIC :: ps_phash_p_eq = 1069
  INTEGER, PARAMETER, PUBLIC :: ps_phash_qbeami = 1071
  INTEGER, PARAMETER, PUBLIC :: ps_phash_qcomp_e = 1073
  INTEGER, PARAMETER, PUBLIC :: ps_phash_qcomp_i = 1075
  INTEGER, PARAMETER, PUBLIC :: ps_phash_qie = 1077
  INTEGER, PARAMETER, PUBLIC :: ps_phash_qioniz = 1079
  INTEGER, PARAMETER, PUBLIC :: ps_phash_qqcx = 1081
  INTEGER, PARAMETER, PUBLIC :: ps_phash_qrot_conv = 1083
  INTEGER, PARAMETER, PUBLIC :: ps_phash_qrot_diff = 1085
  INTEGER, PARAMETER, PUBLIC :: ps_phash_q_eq = 1087
  INTEGER, PARAMETER, PUBLIC :: ps_phash_r0_momeq = 1089
  INTEGER, PARAMETER, PUBLIC :: ps_phash_rate_sinb0i = 1091
  INTEGER, PARAMETER, PUBLIC :: ps_phash_rate_sinb0x = 1093
  INTEGER, PARAMETER, PUBLIC :: ps_phash_rate_sinb0xs = 1095
  INTEGER, PARAMETER, PUBLIC :: ps_phash_rate_sinf0i = 1097
  INTEGER, PARAMETER, PUBLIC :: ps_phash_rate_sinf0x = 1099
  INTEGER, PARAMETER, PUBLIC :: ps_phash_rate_sinf0xs = 1101
  INTEGER, PARAMETER, PUBLIC :: ps_phash_res_sn = 1103
  INTEGER, PARAMETER, PUBLIC :: ps_phash_res_te = 1105
  INTEGER, PARAMETER, PUBLIC :: ps_phash_res_ti = 1107
  INTEGER, PARAMETER, PUBLIC :: ps_phash_res_tq = 1109
  INTEGER, PARAMETER, PUBLIC :: ps_phash_rmajor_mean = 1111
  INTEGER, PARAMETER, PUBLIC :: ps_phash_rminor_mean = 1113
  INTEGER, PARAMETER, PUBLIC :: ps_phash_r_geo = 1115
  INTEGER, PARAMETER, PUBLIC :: ps_phash_r_midp_in = 1117
  INTEGER, PARAMETER, PUBLIC :: ps_phash_r_midp_out = 1119
  INTEGER, PARAMETER, PUBLIC :: ps_phash_r_surfmax = 1121
  INTEGER, PARAMETER, PUBLIC :: ps_phash_r_surfmin = 1123
  INTEGER, PARAMETER, PUBLIC :: ps_phash_s0reco = 1125
  INTEGER, PARAMETER, PUBLIC :: ps_phash_s0reco_e = 1127
  INTEGER, PARAMETER, PUBLIC :: ps_phash_s0reco_recap = 1129
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sb0halo = 1131
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sb0halo_recap = 1133
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sbedep = 1135
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sbehalo = 1137
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sbsce = 1139
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sbtherm = 1141
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sc_anom = 1143
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sfsce = 1145
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sftherm = 1147
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sii_xs = 1149
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sn_trans = 1151
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sn_transcat = 1153
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sprof0 = 1155
  INTEGER, PARAMETER, PUBLIC :: ps_phash_sprof0e = 1157
  INTEGER, PARAMETER, PUBLIC :: ps_phash_spsce = 1159
  INTEGER, PARAMETER, PUBLIC :: ps_phash_squarelo = 1161
  INTEGER, PARAMETER, PUBLIC :: ps_phash_squareuo = 1163
  INTEGER, PARAMETER, PUBLIC :: ps_phash_surf = 1165
  INTEGER, PARAMETER, PUBLIC :: ps_phash_t0cx = 1167
  INTEGER, PARAMETER, PUBLIC :: ps_phash_t0sc0 = 1169
  INTEGER, PARAMETER, PUBLIC :: ps_phash_t0_halo = 1171
  INTEGER, PARAMETER, PUBLIC :: ps_phash_t0_reco = 1173
  INTEGER, PARAMETER, PUBLIC :: ps_phash_teflux_ky_tglf = 1175
  INTEGER, PARAMETER, PUBLIC :: ps_phash_te_ky_tglf = 1177
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tfrip2_log = 1179
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tfrip_log = 1181
  INTEGER, PARAMETER, PUBLIC :: ps_phash_ti = 1183
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tiflux_ky_tglf = 1185
  INTEGER, PARAMETER, PUBLIC :: ps_phash_ti_ky_tglf = 1187
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tmodel = 1189
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tq0_reco = 1191
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tqb0_halo = 1193
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tqbe = 1195
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tqbi = 1197
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tqbjxb = 1199
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tqbth = 1201
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tqcx_halo = 1203
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tqcx_reco = 1205
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tqioniz = 1207
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tqqcx = 1209
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tqsc_halo = 1211
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tqsc_reco = 1213
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tq_anom = 1215
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tq_trans = 1217
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tq_transcat = 1219
  INTEGER, PARAMETER, PUBLIC :: ps_phash_triang = 1221
  INTEGER, PARAMETER, PUBLIC :: ps_phash_triangl = 1223
  INTEGER, PARAMETER, PUBLIC :: ps_phash_triangu = 1225
  INTEGER, PARAMETER, PUBLIC :: ps_phash_triang_miller_l = 1227
  INTEGER, PARAMETER, PUBLIC :: ps_phash_triang_miller_u = 1229
  INTEGER, PARAMETER, PUBLIC :: ps_phash_ts = 1231
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tzflux_ky_tglf = 1233
  INTEGER, PARAMETER, PUBLIC :: ps_phash_tz_ky_tglf = 1235
  INTEGER, PARAMETER, PUBLIC :: ps_phash_upwind_pfrac_ns = 1237
  INTEGER, PARAMETER, PUBLIC :: ps_phash_upwind_pfrac_omega = 1239
  INTEGER, PARAMETER, PUBLIC :: ps_phash_upwind_pfrac_te = 1241
  INTEGER, PARAMETER, PUBLIC :: ps_phash_upwind_pfrac_ti = 1243
  INTEGER, PARAMETER, PUBLIC :: ps_phash_vee_trans = 1245
  INTEGER, PARAMETER, PUBLIC :: ps_phash_vee_transcat = 1247
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velb_fusi = 1249
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velb_nbi = 1251
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velb_rfmi = 1253
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velne_nclass = 1255
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velnh_nclass = 1257
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velni_drbm = 1259
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velns_anom = 1261
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velns_glf = 1263
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velns_gtcneo = 1265
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velns_mmm71 = 1267
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velns_nclass = 1269
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velns_neo = 1271
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velns_tglf = 1273
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velnx_nclass = 1275
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velpphi_anom = 1277
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velpphi_glf = 1279
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velpphi_gtcneo = 1281
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velpphi_mmm71 = 1283
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velpphi_neo = 1285
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velpphi_tglf = 1287
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velte_anom = 1289
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velte_glf = 1291
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velte_gtcneo = 1293
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velte_mmm71 = 1295
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velte_nclass = 1297
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velte_neo = 1299
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velte_paleo = 1301
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velte_tglf = 1303
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velti_anom = 1305
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velti_glf = 1307
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velti_gtcneo = 1309
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velti_mmm71 = 1311
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velti_nclass = 1313
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velti_neo = 1315
  INTEGER, PARAMETER, PUBLIC :: ps_phash_velti_tglf = 1317
  INTEGER, PARAMETER, PUBLIC :: ps_phash_vie_trans = 1319
  INTEGER, PARAMETER, PUBLIC :: ps_phash_vie_transcat = 1321
  INTEGER, PARAMETER, PUBLIC :: ps_phash_vmo_trans = 1323
  INTEGER, PARAMETER, PUBLIC :: ps_phash_vmo_transcat = 1325
  INTEGER, PARAMETER, PUBLIC :: ps_phash_vn_trans = 1327
  INTEGER, PARAMETER, PUBLIC :: ps_phash_vn_transcat = 1329
  INTEGER, PARAMETER, PUBLIC :: ps_phash_vol = 1331
  INTEGER, PARAMETER, PUBLIC :: ps_phash_vpol_inmp = 1333
  INTEGER, PARAMETER, PUBLIC :: ps_phash_vpol_omp = 1335
  INTEGER, PARAMETER, PUBLIC :: ps_phash_vtor_inmp = 1337
  INTEGER, PARAMETER, PUBLIC :: ps_phash_vtor_omp = 1339
  INTEGER, PARAMETER, PUBLIC :: ps_phash_v_loop = 1341
  INTEGER, PARAMETER, PUBLIC :: ps_phash_v_pars = 1343
  INTEGER, PARAMETER, PUBLIC :: ps_phash_v_pers = 1345
  INTEGER, PARAMETER, PUBLIC :: ps_phash_xrjcos_momeq = 1347
  INTEGER, PARAMETER, PUBLIC :: ps_phash_xrjsin_momeq = 1349
  INTEGER, PARAMETER, PUBLIC :: ps_phash_xzjcos_momeq = 1351
  INTEGER, PARAMETER, PUBLIC :: ps_phash_xzjsin_momeq = 1353
  INTEGER, PARAMETER, PUBLIC :: ps_phash_z0_momeq = 1355
  INTEGER, PARAMETER, PUBLIC :: ps_phash_zeff = 1357
  INTEGER, PARAMETER, PUBLIC :: ps_phash_zeff_fi = 1359
  INTEGER, PARAMETER, PUBLIC :: ps_phash_zeff_th = 1361
  INTEGER, PARAMETER, PUBLIC :: ps_phash_z_geo = 1363
  INTEGER, PARAMETER, PUBLIC :: ps_phash_z_midp = 1365
  INTEGER, PARAMETER, PUBLIC :: ps_phash_z_surfmax = 1367
  INTEGER, PARAMETER, PUBLIC :: ps_phash_z_surfmin = 1369

  ! auxilliary hash code pair addresses:
  INTEGER, PARAMETER, PUBLIC :: ps_hash_aux1 = 1371
  INTEGER, PARAMETER, PUBLIC :: ps_hash_aux2 = 1373
  INTEGER, PARAMETER, PUBLIC :: ps_hash_aux3 = 1375
  INTEGER, PARAMETER, PUBLIC :: ps_hash_aux4 = 1377
  INTEGER, PARAMETER, PUBLIC :: ps_hash_aux5 = 1379
  INTEGER, PARAMETER, PUBLIC :: ps_hash_aux6 = 1381
  INTEGER, PARAMETER, PUBLIC :: ps_hash_aux7 = 1383
  INTEGER, PARAMETER, PUBLIC :: ps_hash_aux8 = 1385
  INTEGER, PARAMETER, PUBLIC :: ps_hash_aux9 = 1387

  ! total number of hash codes:
  INTEGER, PARAMETER, PUBLIC :: ps_hash_total = 1388


contains

   ! routines to check for differences in values
   ! set iflag if differences are detected AND policy = ps_error

   subroutine ps_diff_c0(name,c0a,c0b,policy,iout,iflag)
     character*(*), intent(in) :: name
     character*(*), intent(in) :: c0a
     character*(*), intent(in) :: c0b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     if(c0a.ne.c0b) then

        msghd = '%'
        if(policy.eq.ps_error) then
           iflag = 1
           msghd = '?'
        endif

        write(iout,*) msghd//'ps_diff: value difference detected in: '// &
             trim(name)
     endif
   end subroutine ps_diff_c0

   subroutine ps_diff_c1(name,c1a,c1b,policy,iout,iflag)
     character*(*), intent(in) :: name
     character*(*), dimension(:), intent(in) :: c1a
     character*(*), dimension(:), intent(in) :: c1b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,idiff

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     idiff=0
     if(len(c1a(1)).ne.len(c1b(1))) idiff=2
     if(size(c1a).ne.size(c1b)) idiff=2
     if(idiff.eq.0) then
        do ii=1,size(c1a)
           if(c1a(ii).ne.c1b(ii)) then
              idiff=1
              exit
           endif
        enddo
     endif

     if(idiff.ne.0) then

        msghd = '%'
        if((policy.eq.ps_error).OR.(idiff.eq.2)) then
           iflag = 1
           msghd = '?'
        endif

        if(idiff.eq.1) then
           write(iout,*) msghd//'ps_diff: value difference detected in: '// &
                trim(name)
        else
           write(iout,*) msghd//'ps_diff: shape difference detected in: '// &
                trim(name)
        endif
     endif
   end subroutine ps_diff_c1

   subroutine ps_diff_i0(name,i0a,i0b,policy,iout,iflag)
     character*(*), intent(in) :: name
     integer, intent(in) :: i0a
     integer, intent(in) :: i0b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     if(i0a.ne.i0b) then

        msghd = '%'
        if(policy.eq.ps_error) then
           iflag = 1
           msghd = '?'
        endif

        write(iout,*) msghd//'ps_diff: value difference detected in: '// &
             trim(name)
     endif
   end subroutine ps_diff_i0

   subroutine ps_diff_i1(name,i1a,i1b,policy,iout,iflag)
     character*(*), intent(in) :: name
     integer, dimension(:), intent(in) :: i1a
     integer, dimension(:), intent(in) :: i1b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,idiff

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     idiff=0
     if(size(i1a).ne.size(i1b)) idiff=2
     if(idiff.eq.0) then
        do ii=1,size(i1a)
           if(i1a(ii).ne.i1b(ii)) then
              idiff=1
              exit
           endif
        enddo
     endif

     if(idiff.ne.0) then

        msghd = '%'
        if((policy.eq.ps_error).OR.(idiff.eq.2)) then
           iflag = 1
           msghd = '?'
        endif

        if(idiff.eq.1) then
           write(iout,*) msghd//'ps_diff: value difference detected in: '// &
                trim(name)
        else
           write(iout,*) msghd//'ps_diff: shape difference detected in: '// &
                trim(name)
        endif
     endif
   end subroutine ps_diff_i1

   subroutine ps_diff_i2(name,i2a,i2b,policy,iout,iflag)
     character*(*), intent(in) :: name
     integer, dimension(:,:), intent(in) :: i2a
     integer, dimension(:,:), intent(in) :: i2b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,idiff

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     idiff=0
     if(size(i2a,1).ne.size(i2b,1)) idiff=2
     if(size(i2a,2).ne.size(i2b,2)) idiff=2
     if(idiff.eq.0) then
        do jj=1,size(i2a,2)
           do ii=1,size(i2a,1)
              if(i2a(ii,jj).ne.i2b(ii,jj)) then
                 idiff=1
                 exit
              endif
           enddo
           if(idiff.ne.0) exit
        enddo
     endif

     if(idiff.ne.0) then

        msghd = '%'
        if((policy.eq.ps_error).OR.(idiff.eq.2)) then
           iflag = 1
           msghd = '?'
        endif

        if(idiff.eq.1) then
           write(iout,*) msghd//'ps_diff: value difference detected in: '// &
                trim(name)
        else
           write(iout,*) msghd//'ps_diff: shape difference detected in: '// &
                trim(name)
        endif
     endif
   end subroutine ps_diff_i2

   subroutine ps_diff_i3(name,i3a,i3b,policy,iout,iflag)
     character*(*), intent(in) :: name
     integer, dimension(:,:,:), intent(in) :: i3a
     integer, dimension(:,:,:), intent(in) :: i3b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,kk,idiff

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     idiff=0
     if(size(i3a,1).ne.size(i3b,1)) idiff=2
     if(size(i3a,2).ne.size(i3b,2)) idiff=2
     if(size(i3a,3).ne.size(i3b,3)) idiff=2
     if(idiff.eq.0) then
        do kk=1,size(i3a,3)
           do jj=1,size(i3a,2)
              do ii=1,size(i3a,1)
                 if(i3a(ii,jj,kk).ne.i3b(ii,jj,kk)) then
                    idiff=1
                    exit
                 endif
              enddo
              if(idiff.ne.0) exit
           enddo
        enddo
     endif

     if(idiff.ne.0) then

        msghd = '%'
        if((policy.eq.ps_error).OR.(idiff.eq.2)) then
           iflag = 1
           msghd = '?'
        endif

        if(idiff.eq.1) then
           write(iout,*) msghd//'ps_diff: value difference detected in: '// &
                trim(name)
        else
           write(iout,*) msghd//'ps_diff: shape difference detected in: '// &
                trim(name)
        endif
     endif
   end subroutine ps_diff_i3

   subroutine ps_diff_r0(name,r0a,r0b,policy,iout,iflag)
     character*(*), intent(in) :: name
     real*8, intent(in) :: r0a
     real*8, intent(in) :: r0b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     if(r0a.ne.r0b) then

        msghd = '%'
        if(policy.eq.ps_error) then
           iflag = 1
           msghd = '?'
        endif

        write(iout,*) msghd//'ps_diff: value difference detected in: '// &
             trim(name)
     endif
   end subroutine ps_diff_r0

   subroutine ps_diff_r1(name,r1a,r1b,policy,iout,iflag)
     character*(*), intent(in) :: name
     real*8, dimension(:), intent(in) :: r1a
     real*8, dimension(:), intent(in) :: r1b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,idiff

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     idiff=0
     if(size(r1a).ne.size(r1b)) idiff=2
     if(idiff.eq.0) then
        do ii=1,size(r1a)
           if(r1a(ii).ne.r1b(ii)) then
              idiff=1
              exit
           endif
        enddo
     endif

     if(idiff.ne.0) then

        msghd = '%'
        if((policy.eq.ps_error).OR.(idiff.eq.2)) then
           iflag = 1
           msghd = '?'
        endif

        if(idiff.eq.1) then
           write(iout,*) msghd//'ps_diff: value difference detected in: '// &
                trim(name)
        else
           write(iout,*) msghd//'ps_diff: shape difference detected in: '// &
                trim(name)
        endif
     endif
   end subroutine ps_diff_r1

   subroutine ps_diff_r2(name,r2a,r2b,policy,iout,iflag)
     character*(*), intent(in) :: name
     real*8, dimension(:,:), intent(in) :: r2a
     real*8, dimension(:,:), intent(in) :: r2b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,idiff

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     idiff=0
     if(size(r2a,1).ne.size(r2b,1)) idiff=2
     if(size(r2a,2).ne.size(r2b,2)) idiff=2
     if(idiff.eq.0) then
        do jj=1,size(r2a,2)
           do ii=1,size(r2a,1)
              if(r2a(ii,jj).ne.r2b(ii,jj)) then
                 idiff=1
                 exit
              endif
           enddo
           if(idiff.ne.0) exit
        enddo
     endif

     if(idiff.ne.0) then

        msghd = '%'
        if((policy.eq.ps_error).OR.(idiff.eq.2)) then
           iflag = 1
           msghd = '?'
        endif

        if(idiff.eq.1) then
           write(iout,*) msghd//'ps_diff: value difference detected in: '// &
                trim(name)
        else
           write(iout,*) msghd//'ps_diff: shape difference detected in: '// &
                trim(name)
        endif
     endif
   end subroutine ps_diff_r2

   ! routines to check for conformal dimensioning of arrays
   ! set iflag and issue message if array dimensions do not match

   subroutine ps_conformal_c1(name,c1a,c1b,iout,iflag)
     character*(*), intent(in) :: name
     character*(*), dimension(:), intent(in) :: c1a
     character*(*), dimension(:), intent(in) :: c1b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(len(c1a(1)).ne.len(c1b(1))) idiff=2
     if(size(c1a).ne.size(c1b)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_c1

   subroutine ps_conformal_i1(name,i1a,i1b,iout,iflag)
     character*(*), intent(in) :: name
     integer, dimension(:), intent(in) :: i1a
     integer, dimension(:), intent(in) :: i1b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(i1a).ne.size(i1b)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_i1

   subroutine ps_conformal_i2(name,i2a,i2b,iout,iflag)
     character*(*), intent(in) :: name
     integer, dimension(:,:), intent(in) :: i2a
     integer, dimension(:,:), intent(in) :: i2b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(i2a,1).ne.size(i2b,1)) idiff=2
     if(size(i2a,2).ne.size(i2b,2)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_i2

   subroutine ps_conformal_i3(name,i3a,i3b,iout,iflag)
     character*(*), intent(in) :: name
     integer, dimension(:,:,:), intent(in) :: i3a
     integer, dimension(:,:,:), intent(in) :: i3b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,kk,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(i3a,1).ne.size(i3b,1)) idiff=2
     if(size(i3a,2).ne.size(i3b,2)) idiff=2
     if(size(i3a,3).ne.size(i3b,3)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_i3

   subroutine ps_conformal_r1(name,r1a,r1b,iout,iflag)
     character*(*), intent(in) :: name
     real*8, dimension(:), intent(in) :: r1a
     real*8, dimension(:), intent(in) :: r1b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(r1a).ne.size(r1b)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_r1

   subroutine ps_conformal_r2(name,r2a,r2b,iout,iflag)
     character*(*), intent(in) :: name
     real*8, dimension(:,:), intent(in) :: r2a
     real*8, dimension(:,:), intent(in) :: r2b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(r2a,1).ne.size(r2b,1)) idiff=2
     if(size(r2a,2).ne.size(r2b,2)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_r2

   subroutine ps_conformal_r3(name,r3a,r3b,iout,iflag)
     character*(*), intent(in) :: name
     real*8, dimension(:,:,:), intent(in) :: r3a
     real*8, dimension(:,:,:), intent(in) :: r3b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,kk,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(r3a,1).ne.size(r3b,1)) idiff=2
     if(size(r3a,2).ne.size(r3b,2)) idiff=2
     if(size(r3a,3).ne.size(r3b,3)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_r3

   subroutine ps_conformal_r4(name,r4a,r4b,iout,iflag)
     character*(*), intent(in) :: name
     real*8, dimension(:,:,:,:), intent(in) :: r4a
     real*8, dimension(:,:,:,:), intent(in) :: r4b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,kk,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(r4a,1).ne.size(r4b,1)) idiff=2
     if(size(r4a,2).ne.size(r4b,2)) idiff=2
     if(size(r4a,3).ne.size(r4b,3)) idiff=2
     if(size(r4a,4).ne.size(r4b,4)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_r4

end module plasma_state_util_mod
