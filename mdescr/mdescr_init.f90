! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)



SUBROUTINE mdescr_init

  ! initialize mdescr_data

  USE mdescr_data
  implicit NONE

  !-------------------------------

  tokamak_id = " "
  geometry = " "
  num_rzlim = 0
  rlim = 0.0_rspec
  zlim = 0.0_rspec
  R_min_box = 0.0_rspec
  R_max_box = 0.0_rspec
  Z_min_box = 0.0_rspec
  Z_max_box = 0.0_rspec
  circuit_name = " "
  coil_name = " "
  coil_in_circuit = " "
  Rloc_coil = 0.0_rspec
  Zloc_coil = 0.0_rspec
  Hsize_coil = 0.0_rspec
  Ang_Href_coil = 0.0_rspec
  Vsize_coil = 0.0_rspec
  Ang_Vref_coil = 0.0_rspec
  Nturns = 0
  VRep_coil_count = 0
  Vrep_coil_spacing = 0.0_rspec
  coil_resispt = 0.0_rspec
  nbi_src_name = " "
  beam_type = " "
  sRtcen = 0.0_rspec
  Lbsctan = 0.0_rspec
  Zbsc = 0.0_rspec
  Phibsc = 0.0_rspec
  Lbscap = 0.0_rspec
  Zbap = 0.0_rspec
  nbshape = " "
  b_halfwidth = 0.0_rspec
  b_halfHeight = 0.0_rspec
  b_Hfocal_length = 0.0_rspec
  b_Vfocal_length = 0.0_rspec
  b_Hdivergence = 0.0_rspec
  b_Vdivergence = 0.0_rspec
  nbap_shape = " "
  ap_halfwidth = 0.0_rspec
  ap_halfheight = 0.0_rspec
  ap_horiz_offset = 0.0_rspec
  ap_vert_offset = 0.0_rspec
  nbap2_shape = " "
  Lbscap2 = 0.0_rspec
  ap2_halfwidth = 0.0_rspec
  ap2_halfheight = 0.0_rspec
  ap2_horiz_offset = 0.0_rspec
  ap2_vert_offset = 0.0_rspec
  Einj_min = 0.0_rspec
  Einj_max = 0.0_rspec
  n_H_einj_standard = 0
  n_D_einj_standard = 0
  n_T_einj_standard = 0
  H_einj_standard = 0.0_rspec
  H_ffull_standard = 0.0_rspec
  H_fhalf_standard = 0.0_rspec
  D_einj_standard = 0.0_rspec
  D_ffull_standard = 0.0_rspec
  D_fhalf_standard = 0.0_rspec
  T_einj_standard = 0.0_rspec
  T_ffull_standard = 0.0_rspec
  T_fhalf_standard = 0.0_rspec
  icrf_src_name = " "
  ant_model = " "
  nrz_antgeo = 0
  max_nrz_antgeo = 0
  R_antgeo = 0.0_rspec
  Z_antgeo = 0.0_rspec
  dx_fshield = 0.0_rspec
  ecrf_src_name = " "
  R_EC_launch = 0.0_rspec
  Z_EC_launch = 0.0_rspec
  Phi_EC_launch = 0.0_rspec
  EC_PRINC_WIDTH_HOR = 0.0_rspec
  EC_PRINC_WIDTH_VERT = 0.0_rspec
  EC_PRINC_CURV_HOR = 0.0_rspec
  EC_PRINC_CURV_VERT = 0.0_rspec
  EC_Half_Power_Angle = 0.0_rspec
  EC_Beam_Elongation = 0.0_rspec
  lhrf_src_name = " "
  ntf_coils = 0
  ntf_coil2 = 0

END SUBROUTINE mdescr_init
