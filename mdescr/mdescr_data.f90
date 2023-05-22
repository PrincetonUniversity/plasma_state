! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)



MODULE mdescr_data

  ! Plasma State machine description data -- standalone module

  implicit NONE
  SAVE
  PUBLIC

  INTEGER, PARAMETER :: rspec = SELECTED_REAL_KIND(p=15,r=307)

  INTEGER, PARAMETER :: ps_max_static = 500
  INTEGER, PARAMETER :: ps_max_static_2d = 128

  !---------------------------------
  INTEGER :: iout = 6
  !---------------------------------

  CHARACTER*32 :: tokamak_id
  CHARACTER*256 :: geometry
  INTEGER :: num_rzlim
  REAL(KIND=rspec), dimension(ps_max_static) :: rlim
  REAL(KIND=rspec), dimension(ps_max_static) :: zlim
  REAL(KIND=rspec) :: R_min_box
  REAL(KIND=rspec) :: R_max_box
  REAL(KIND=rspec) :: Z_min_box
  REAL(KIND=rspec) :: Z_max_box
  CHARACTER*32, dimension(ps_max_static) :: circuit_name
  INTEGER :: NCIRCUITS = 0   ! ** dimension variable **

  CHARACTER*32, dimension(ps_max_static) :: coil_name
  INTEGER :: NCOILS = 0   ! ** dimension variable **

  CHARACTER*32, dimension(ps_max_static) :: coil_in_circuit
  REAL(KIND=rspec), dimension(ps_max_static) :: Rloc_coil
  REAL(KIND=rspec), dimension(ps_max_static) :: Zloc_coil
  REAL(KIND=rspec), dimension(ps_max_static) :: Hsize_coil
  REAL(KIND=rspec), dimension(ps_max_static) :: Ang_Href_coil
  REAL(KIND=rspec), dimension(ps_max_static) :: Vsize_coil
  REAL(KIND=rspec), dimension(ps_max_static) :: Ang_Vref_coil
  INTEGER, dimension(ps_max_static) :: Nturns
  INTEGER, dimension(ps_max_static) :: VRep_coil_count
  REAL(KIND=rspec), dimension(ps_max_static) :: Vrep_coil_spacing
  REAL(KIND=rspec), dimension(ps_max_static) :: coil_resispt
  CHARACTER*32, dimension(ps_max_static) :: nbi_src_name
  INTEGER :: NBEAM = 0   ! ** dimension variable **

  CHARACTER*32, dimension(ps_max_static) :: beam_type
  REAL(KIND=rspec), dimension(ps_max_static) :: sRtcen
  REAL(KIND=rspec), dimension(ps_max_static) :: Lbsctan
  REAL(KIND=rspec), dimension(ps_max_static) :: Zbsc
  REAL(KIND=rspec), dimension(ps_max_static) :: Phibsc
  REAL(KIND=rspec), dimension(ps_max_static) :: Lbscap
  REAL(KIND=rspec), dimension(ps_max_static) :: Zbap
  CHARACTER*32, dimension(ps_max_static) :: nbshape
  REAL(KIND=rspec), dimension(ps_max_static) :: b_halfwidth
  REAL(KIND=rspec), dimension(ps_max_static) :: b_halfHeight
  REAL(KIND=rspec), dimension(ps_max_static) :: b_Hfocal_length
  REAL(KIND=rspec), dimension(ps_max_static) :: b_Vfocal_length
  REAL(KIND=rspec), dimension(ps_max_static) :: b_Hdivergence
  REAL(KIND=rspec), dimension(ps_max_static) :: b_Vdivergence
  CHARACTER*32, dimension(ps_max_static) :: nbap_shape
  REAL(KIND=rspec), dimension(ps_max_static) :: ap_halfwidth
  REAL(KIND=rspec), dimension(ps_max_static) :: ap_halfheight
  REAL(KIND=rspec), dimension(ps_max_static) :: ap_horiz_offset
  REAL(KIND=rspec), dimension(ps_max_static) :: ap_vert_offset
  CHARACTER*32, dimension(ps_max_static) :: nbap2_shape
  REAL(KIND=rspec), dimension(ps_max_static) :: Lbscap2
  REAL(KIND=rspec), dimension(ps_max_static) :: ap2_halfwidth
  REAL(KIND=rspec), dimension(ps_max_static) :: ap2_halfheight
  REAL(KIND=rspec), dimension(ps_max_static) :: ap2_horiz_offset
  REAL(KIND=rspec), dimension(ps_max_static) :: ap2_vert_offset
  REAL(KIND=rspec), dimension(ps_max_static) :: Einj_min
  REAL(KIND=rspec), dimension(ps_max_static) :: Einj_max
  INTEGER :: n_H_einj_standard
  INTEGER :: n_D_einj_standard
  INTEGER :: n_T_einj_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: H_einj_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: H_ffull_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: H_fhalf_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: D_einj_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: D_ffull_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: D_fhalf_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: T_einj_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: T_ffull_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: T_fhalf_standard
  CHARACTER*32, dimension(ps_max_static_2d) :: icrf_src_name
  INTEGER :: NICRF_SRC = 0   ! ** dimension variable **

  CHARACTER*256, dimension(ps_max_static_2d) :: ant_model
  INTEGER, dimension(ps_max_static_2d) :: nrz_antgeo
  INTEGER :: max_nrz_antgeo
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: R_antgeo
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: Z_antgeo
  REAL(KIND=rspec), dimension(ps_max_static_2d) :: dx_fshield
  CHARACTER*32, dimension(ps_max_static) :: ecrf_src_name
  INTEGER :: NECRF_SRC = 0   ! ** dimension variable **

  REAL(KIND=rspec), dimension(ps_max_static) :: R_EC_launch
  REAL(KIND=rspec), dimension(ps_max_static) :: Z_EC_launch
  REAL(KIND=rspec), dimension(ps_max_static) :: Phi_EC_launch
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_PRINC_WIDTH_HOR
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_PRINC_WIDTH_VERT
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_PRINC_CURV_HOR
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_PRINC_CURV_VERT
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_Half_Power_Angle
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_Beam_Elongation
  CHARACTER*32, dimension(ps_max_static) :: lhrf_src_name
  INTEGER :: NLHRF_SRC = 0   ! ** dimension variable **

  INTEGER :: ntf_coils
  INTEGER :: ntf_coil2

  NAMELIST /mdescr/ &
       tokamak_id, &
       geometry, &
       num_rzlim, &
       rlim, &
       zlim, &
       R_min_box, &
       R_max_box, &
       Z_min_box, &
       Z_max_box, &
       circuit_name, &
       coil_name, &
       coil_in_circuit, &
       Rloc_coil, &
       Zloc_coil, &
       Hsize_coil, &
       Ang_Href_coil, &
       Vsize_coil, &
       Ang_Vref_coil, &
       Nturns, &
       VRep_coil_count, &
       Vrep_coil_spacing, &
       coil_resispt, &
       nbi_src_name, &
       beam_type, &
       sRtcen, &
       Lbsctan, &
       Zbsc, &
       Phibsc, &
       Lbscap, &
       Zbap, &
       nbshape, &
       b_halfwidth, &
       b_halfHeight, &
       b_Hfocal_length, &
       b_Vfocal_length, &
       b_Hdivergence, &
       b_Vdivergence, &
       nbap_shape, &
       ap_halfwidth, &
       ap_halfheight, &
       ap_horiz_offset, &
       ap_vert_offset, &
       nbap2_shape, &
       Lbscap2, &
       ap2_halfwidth, &
       ap2_halfheight, &
       ap2_horiz_offset, &
       ap2_vert_offset, &
       Einj_min, &
       Einj_max, &
       n_H_einj_standard, &
       n_D_einj_standard, &
       n_T_einj_standard, &
       H_einj_standard, &
       H_ffull_standard, &
       H_fhalf_standard, &
       D_einj_standard, &
       D_ffull_standard, &
       D_fhalf_standard, &
       T_einj_standard, &
       T_ffull_standard, &
       T_fhalf_standard, &
       icrf_src_name, &
       ant_model, &
       nrz_antgeo, &
       max_nrz_antgeo, &
       R_antgeo, &
       Z_antgeo, &
       dx_fshield, &
       ecrf_src_name, &
       R_EC_launch, &
       Z_EC_launch, &
       Phi_EC_launch, &
       EC_PRINC_WIDTH_HOR, &
       EC_PRINC_WIDTH_VERT, &
       EC_PRINC_CURV_HOR, &
       EC_PRINC_CURV_VERT, &
       EC_Half_Power_Angle, &
       EC_Beam_Elongation, &
       lhrf_src_name, &
       ntf_coils, &
       ntf_coil2

END MODULE mdescr_data
