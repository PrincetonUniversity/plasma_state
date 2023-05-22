! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_merge_2(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)


  ! Merge floating point data
  !   ss3 <-- w1*ss1 + (1-w1)*ss2

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components to merge
                                ! use ps_cc_all to merge all components

  REAL(KIND=rspec) :: w1        ! weight on ss1
  TYPE (plasma_state) :: ss1    ! state data to copy *from* with weight (w1)
  TYPE (plasma_state) :: ss2    ! state data to copy *from* with weight (1-w1)
  TYPE (plasma_state) :: ss3    ! state to which data is copied.
  INTEGER, INTENT(IN) :: icheck ! control for checking match of non-floating-
  !  point data: ps_ignore, ps_warn, or ps_error on mismatch.

  INTEGER, INTENT(IN) :: iout
  INTEGER, INTENT(INOUT) :: ierr  ! completion code (not cleared,set on error)

  !-------------------
  INTEGER :: isize
  !-------------------

  !-----------------------------------------------
  ! copy scalars and arrays that are allocated...

  IF((ALLOCATED(ss3%D_einj_standard)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%D_einj_standard).AND.ALLOCATED(ss2%D_einj_standard)) THEN
        CALL ps_diff("D_einj_standard",ss1%D_einj_standard,ss2%D_einj_standard,icheck,iout,ierr)
        CALL ps_conformal("D_einj_standard",ss1%D_einj_standard,ss3%D_einj_standard,iout,ierr)
        if(ierr.eq.0) ss3%D_einj_standard = ss1%D_einj_standard
     ELSE IF(ALLOCATED(ss1%D_einj_standard)) THEN
        CALL ps_conformal("D_einj_standard",ss1%D_einj_standard,ss3%D_einj_standard,iout,ierr)
        ss3%D_einj_standard = ss1%D_einj_standard
     ELSE IF(ALLOCATED(ss2%D_einj_standard)) THEN
        CALL ps_conformal("D_einj_standard",ss2%D_einj_standard,ss3%D_einj_standard,iout,ierr)
        ss3%D_einj_standard = ss2%D_einj_standard
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%D_ffull_standard)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%D_ffull_standard).AND.ALLOCATED(ss2%D_ffull_standard)) THEN
        CALL ps_diff("D_ffull_standard",ss1%D_ffull_standard,ss2%D_ffull_standard,icheck,iout,ierr)
        CALL ps_conformal("D_ffull_standard",ss1%D_ffull_standard,ss3%D_ffull_standard,iout,ierr)
        if(ierr.eq.0) ss3%D_ffull_standard = ss1%D_ffull_standard
     ELSE IF(ALLOCATED(ss1%D_ffull_standard)) THEN
        CALL ps_conformal("D_ffull_standard",ss1%D_ffull_standard,ss3%D_ffull_standard,iout,ierr)
        ss3%D_ffull_standard = ss1%D_ffull_standard
     ELSE IF(ALLOCATED(ss2%D_ffull_standard)) THEN
        CALL ps_conformal("D_ffull_standard",ss2%D_ffull_standard,ss3%D_ffull_standard,iout,ierr)
        ss3%D_ffull_standard = ss2%D_ffull_standard
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%D_fhalf_standard)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%D_fhalf_standard).AND.ALLOCATED(ss2%D_fhalf_standard)) THEN
        CALL ps_diff("D_fhalf_standard",ss1%D_fhalf_standard,ss2%D_fhalf_standard,icheck,iout,ierr)
        CALL ps_conformal("D_fhalf_standard",ss1%D_fhalf_standard,ss3%D_fhalf_standard,iout,ierr)
        if(ierr.eq.0) ss3%D_fhalf_standard = ss1%D_fhalf_standard
     ELSE IF(ALLOCATED(ss1%D_fhalf_standard)) THEN
        CALL ps_conformal("D_fhalf_standard",ss1%D_fhalf_standard,ss3%D_fhalf_standard,iout,ierr)
        ss3%D_fhalf_standard = ss1%D_fhalf_standard
     ELSE IF(ALLOCATED(ss2%D_fhalf_standard)) THEN
        CALL ps_conformal("D_fhalf_standard",ss2%D_fhalf_standard,ss3%D_fhalf_standard,iout,ierr)
        ss3%D_fhalf_standard = ss2%D_fhalf_standard
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%e0_av)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%e0_av).AND.ALLOCATED(ss2%e0_av)) THEN
        CALL ps_conformal("e0_av",ss2%e0_av,ss3%e0_av,iout,ierr)
        CALL ps_conformal("e0_av",ss1%e0_av,ss3%e0_av,iout,ierr)
        if(ierr.eq.0) ss3%e0_av = w1*ss1%e0_av + (1.0_rspec-w1)*ss2%e0_av
     ELSE IF(ALLOCATED(ss1%e0_av)) THEN
        CALL ps_conformal("e0_av",ss1%e0_av,ss3%e0_av,iout,ierr)
        ss3%e0_av = ss1%e0_av
     ELSE IF(ALLOCATED(ss2%e0_av)) THEN
        CALL ps_conformal("e0_av",ss2%e0_av,ss3%e0_av,iout,ierr)
        ss3%e0_av = ss2%e0_av
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ecrf_src_name)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%ecrf_src_name).AND.ALLOCATED(ss2%ecrf_src_name)) THEN
        CALL ps_diff("ecrf_src_name",ss1%ecrf_src_name,ss2%ecrf_src_name,icheck,iout,ierr)
        CALL ps_conformal("ecrf_src_name",ss1%ecrf_src_name,ss3%ecrf_src_name,iout,ierr)
        if(ierr.eq.0) ss3%ecrf_src_name = ss1%ecrf_src_name
     ELSE IF(ALLOCATED(ss1%ecrf_src_name)) THEN
        CALL ps_conformal("ecrf_src_name",ss1%ecrf_src_name,ss3%ecrf_src_name,iout,ierr)
        ss3%ecrf_src_name = ss1%ecrf_src_name
     ELSE IF(ALLOCATED(ss2%ecrf_src_name)) THEN
        CALL ps_conformal("ecrf_src_name",ss2%ecrf_src_name,ss3%ecrf_src_name,iout,ierr)
        ss3%ecrf_src_name = ss2%ecrf_src_name
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%EC_Beam_Elongation)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%EC_Beam_Elongation).AND.ALLOCATED(ss2%EC_Beam_Elongation)) THEN
        CALL ps_diff("EC_Beam_Elongation",ss1%EC_Beam_Elongation,ss2%EC_Beam_Elongation,icheck,iout,ierr)
        CALL ps_conformal("EC_Beam_Elongation",ss1%EC_Beam_Elongation,ss3%EC_Beam_Elongation,iout,ierr)
        if(ierr.eq.0) ss3%EC_Beam_Elongation = ss1%EC_Beam_Elongation
     ELSE IF(ALLOCATED(ss1%EC_Beam_Elongation)) THEN
        CALL ps_conformal("EC_Beam_Elongation",ss1%EC_Beam_Elongation,ss3%EC_Beam_Elongation,iout,ierr)
        ss3%EC_Beam_Elongation = ss1%EC_Beam_Elongation
     ELSE IF(ALLOCATED(ss2%EC_Beam_Elongation)) THEN
        CALL ps_conformal("EC_Beam_Elongation",ss2%EC_Beam_Elongation,ss3%EC_Beam_Elongation,iout,ierr)
        ss3%EC_Beam_Elongation = ss2%EC_Beam_Elongation
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EC).EQ.1) THEN

     CALL ps_diff("EC_Code_Info",ss1%EC_Code_Info,ss2%EC_Code_Info,icheck,iout,ierr)
     ss3%EC_Code_Info = ss1%EC_Code_Info
  ENDIF

  IF(cclist(ps_cnum_EC).EQ.1) THEN

     CALL ps_diff("EC_Data_Info",ss1%EC_Data_Info,ss2%EC_Data_Info,icheck,iout,ierr)
     ss3%EC_Data_Info = ss1%EC_Data_Info
  ENDIF

  IF((ALLOCATED(ss3%EC_Half_Power_Angle)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%EC_Half_Power_Angle).AND.ALLOCATED(ss2%EC_Half_Power_Angle)) THEN
        CALL ps_diff("EC_Half_Power_Angle",ss1%EC_Half_Power_Angle,ss2%EC_Half_Power_Angle,icheck,iout,ierr)
        CALL ps_conformal("EC_Half_Power_Angle",ss1%EC_Half_Power_Angle,ss3%EC_Half_Power_Angle,iout,ierr)
        if(ierr.eq.0) ss3%EC_Half_Power_Angle = ss1%EC_Half_Power_Angle
     ELSE IF(ALLOCATED(ss1%EC_Half_Power_Angle)) THEN
        CALL ps_conformal("EC_Half_Power_Angle",ss1%EC_Half_Power_Angle,ss3%EC_Half_Power_Angle,iout,ierr)
        ss3%EC_Half_Power_Angle = ss1%EC_Half_Power_Angle
     ELSE IF(ALLOCATED(ss2%EC_Half_Power_Angle)) THEN
        CALL ps_conformal("EC_Half_Power_Angle",ss2%EC_Half_Power_Angle,ss3%EC_Half_Power_Angle,iout,ierr)
        ss3%EC_Half_Power_Angle = ss2%EC_Half_Power_Angle
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%EC_Omode_Fraction)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%EC_Omode_Fraction).AND.ALLOCATED(ss2%EC_Omode_Fraction)) THEN
        CALL ps_diff("EC_Omode_Fraction",ss1%EC_Omode_Fraction,ss2%EC_Omode_Fraction,icheck,iout,ierr)
        CALL ps_conformal("EC_Omode_Fraction",ss1%EC_Omode_Fraction,ss3%EC_Omode_Fraction,iout,ierr)
        if(ierr.eq.0) ss3%EC_Omode_Fraction = ss1%EC_Omode_Fraction
     ELSE IF(ALLOCATED(ss1%EC_Omode_Fraction)) THEN
        CALL ps_conformal("EC_Omode_Fraction",ss1%EC_Omode_Fraction,ss3%EC_Omode_Fraction,iout,ierr)
        ss3%EC_Omode_Fraction = ss1%EC_Omode_Fraction
     ELSE IF(ALLOCATED(ss2%EC_Omode_Fraction)) THEN
        CALL ps_conformal("EC_Omode_Fraction",ss2%EC_Omode_Fraction,ss3%EC_Omode_Fraction,iout,ierr)
        ss3%EC_Omode_Fraction = ss2%EC_Omode_Fraction
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%EC_phi_aim)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%EC_phi_aim).AND.ALLOCATED(ss2%EC_phi_aim)) THEN
        CALL ps_diff("EC_phi_aim",ss1%EC_phi_aim,ss2%EC_phi_aim,icheck,iout,ierr)
        CALL ps_conformal("EC_phi_aim",ss1%EC_phi_aim,ss3%EC_phi_aim,iout,ierr)
        if(ierr.eq.0) ss3%EC_phi_aim = ss1%EC_phi_aim
     ELSE IF(ALLOCATED(ss1%EC_phi_aim)) THEN
        CALL ps_conformal("EC_phi_aim",ss1%EC_phi_aim,ss3%EC_phi_aim,iout,ierr)
        ss3%EC_phi_aim = ss1%EC_phi_aim
     ELSE IF(ALLOCATED(ss2%EC_phi_aim)) THEN
        CALL ps_conformal("EC_phi_aim",ss2%EC_phi_aim,ss3%EC_phi_aim,iout,ierr)
        ss3%EC_phi_aim = ss2%EC_phi_aim
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%EC_PRINC_CURV_HOR)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%EC_PRINC_CURV_HOR).AND.ALLOCATED(ss2%EC_PRINC_CURV_HOR)) THEN
        CALL ps_diff("EC_PRINC_CURV_HOR",ss1%EC_PRINC_CURV_HOR,ss2%EC_PRINC_CURV_HOR,icheck,iout,ierr)
        CALL ps_conformal("EC_PRINC_CURV_HOR",ss1%EC_PRINC_CURV_HOR,ss3%EC_PRINC_CURV_HOR,iout,ierr)
        if(ierr.eq.0) ss3%EC_PRINC_CURV_HOR = ss1%EC_PRINC_CURV_HOR
     ELSE IF(ALLOCATED(ss1%EC_PRINC_CURV_HOR)) THEN
        CALL ps_conformal("EC_PRINC_CURV_HOR",ss1%EC_PRINC_CURV_HOR,ss3%EC_PRINC_CURV_HOR,iout,ierr)
        ss3%EC_PRINC_CURV_HOR = ss1%EC_PRINC_CURV_HOR
     ELSE IF(ALLOCATED(ss2%EC_PRINC_CURV_HOR)) THEN
        CALL ps_conformal("EC_PRINC_CURV_HOR",ss2%EC_PRINC_CURV_HOR,ss3%EC_PRINC_CURV_HOR,iout,ierr)
        ss3%EC_PRINC_CURV_HOR = ss2%EC_PRINC_CURV_HOR
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%EC_PRINC_CURV_VERT)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%EC_PRINC_CURV_VERT).AND.ALLOCATED(ss2%EC_PRINC_CURV_VERT)) THEN
        CALL ps_diff("EC_PRINC_CURV_VERT",ss1%EC_PRINC_CURV_VERT,ss2%EC_PRINC_CURV_VERT,icheck,iout,ierr)
        CALL ps_conformal("EC_PRINC_CURV_VERT",ss1%EC_PRINC_CURV_VERT,ss3%EC_PRINC_CURV_VERT,iout,ierr)
        if(ierr.eq.0) ss3%EC_PRINC_CURV_VERT = ss1%EC_PRINC_CURV_VERT
     ELSE IF(ALLOCATED(ss1%EC_PRINC_CURV_VERT)) THEN
        CALL ps_conformal("EC_PRINC_CURV_VERT",ss1%EC_PRINC_CURV_VERT,ss3%EC_PRINC_CURV_VERT,iout,ierr)
        ss3%EC_PRINC_CURV_VERT = ss1%EC_PRINC_CURV_VERT
     ELSE IF(ALLOCATED(ss2%EC_PRINC_CURV_VERT)) THEN
        CALL ps_conformal("EC_PRINC_CURV_VERT",ss2%EC_PRINC_CURV_VERT,ss3%EC_PRINC_CURV_VERT,iout,ierr)
        ss3%EC_PRINC_CURV_VERT = ss2%EC_PRINC_CURV_VERT
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%EC_PRINC_WIDTH_HOR)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%EC_PRINC_WIDTH_HOR).AND.ALLOCATED(ss2%EC_PRINC_WIDTH_HOR)) THEN
        CALL ps_diff("EC_PRINC_WIDTH_HOR",ss1%EC_PRINC_WIDTH_HOR,ss2%EC_PRINC_WIDTH_HOR,icheck,iout,ierr)
        CALL ps_conformal("EC_PRINC_WIDTH_HOR",ss1%EC_PRINC_WIDTH_HOR,ss3%EC_PRINC_WIDTH_HOR,iout,ierr)
        if(ierr.eq.0) ss3%EC_PRINC_WIDTH_HOR = ss1%EC_PRINC_WIDTH_HOR
     ELSE IF(ALLOCATED(ss1%EC_PRINC_WIDTH_HOR)) THEN
        CALL ps_conformal("EC_PRINC_WIDTH_HOR",ss1%EC_PRINC_WIDTH_HOR,ss3%EC_PRINC_WIDTH_HOR,iout,ierr)
        ss3%EC_PRINC_WIDTH_HOR = ss1%EC_PRINC_WIDTH_HOR
     ELSE IF(ALLOCATED(ss2%EC_PRINC_WIDTH_HOR)) THEN
        CALL ps_conformal("EC_PRINC_WIDTH_HOR",ss2%EC_PRINC_WIDTH_HOR,ss3%EC_PRINC_WIDTH_HOR,iout,ierr)
        ss3%EC_PRINC_WIDTH_HOR = ss2%EC_PRINC_WIDTH_HOR
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%EC_PRINC_WIDTH_VERT)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%EC_PRINC_WIDTH_VERT).AND.ALLOCATED(ss2%EC_PRINC_WIDTH_VERT)) THEN
        CALL ps_diff("EC_PRINC_WIDTH_VERT",ss1%EC_PRINC_WIDTH_VERT,ss2%EC_PRINC_WIDTH_VERT,icheck,iout,ierr)
        CALL ps_conformal("EC_PRINC_WIDTH_VERT",ss1%EC_PRINC_WIDTH_VERT,ss3%EC_PRINC_WIDTH_VERT,iout,ierr)
        if(ierr.eq.0) ss3%EC_PRINC_WIDTH_VERT = ss1%EC_PRINC_WIDTH_VERT
     ELSE IF(ALLOCATED(ss1%EC_PRINC_WIDTH_VERT)) THEN
        CALL ps_conformal("EC_PRINC_WIDTH_VERT",ss1%EC_PRINC_WIDTH_VERT,ss3%EC_PRINC_WIDTH_VERT,iout,ierr)
        ss3%EC_PRINC_WIDTH_VERT = ss1%EC_PRINC_WIDTH_VERT
     ELSE IF(ALLOCATED(ss2%EC_PRINC_WIDTH_VERT)) THEN
        CALL ps_conformal("EC_PRINC_WIDTH_VERT",ss2%EC_PRINC_WIDTH_VERT,ss3%EC_PRINC_WIDTH_VERT,iout,ierr)
        ss3%EC_PRINC_WIDTH_VERT = ss2%EC_PRINC_WIDTH_VERT
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%EC_theta_aim)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%EC_theta_aim).AND.ALLOCATED(ss2%EC_theta_aim)) THEN
        CALL ps_diff("EC_theta_aim",ss1%EC_theta_aim,ss2%EC_theta_aim,icheck,iout,ierr)
        CALL ps_conformal("EC_theta_aim",ss1%EC_theta_aim,ss3%EC_theta_aim,iout,ierr)
        if(ierr.eq.0) ss3%EC_theta_aim = ss1%EC_theta_aim
     ELSE IF(ALLOCATED(ss1%EC_theta_aim)) THEN
        CALL ps_conformal("EC_theta_aim",ss1%EC_theta_aim,ss3%EC_theta_aim,iout,ierr)
        ss3%EC_theta_aim = ss1%EC_theta_aim
     ELSE IF(ALLOCATED(ss2%EC_theta_aim)) THEN
        CALL ps_conformal("EC_theta_aim",ss2%EC_theta_aim,ss3%EC_theta_aim,iout,ierr)
        ss3%EC_theta_aim = ss2%EC_theta_aim
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ee_mobility_factor)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%ee_mobility_factor).AND.ALLOCATED(ss2%ee_mobility_factor)) THEN
        CALL ps_conformal("ee_mobility_factor",ss2%ee_mobility_factor,ss3%ee_mobility_factor,iout,ierr)
        CALL ps_conformal("ee_mobility_factor",ss1%ee_mobility_factor,ss3%ee_mobility_factor,iout,ierr)
        if(ierr.eq.0) ss3%ee_mobility_factor = w1*ss1%ee_mobility_factor + (1.0_rspec-w1)*ss2%ee_mobility_factor
     ELSE IF(ALLOCATED(ss1%ee_mobility_factor)) THEN
        CALL ps_conformal("ee_mobility_factor",ss1%ee_mobility_factor,ss3%ee_mobility_factor,iout,ierr)
        ss3%ee_mobility_factor = ss1%ee_mobility_factor
     ELSE IF(ALLOCATED(ss2%ee_mobility_factor)) THEN
        CALL ps_conformal("ee_mobility_factor",ss2%ee_mobility_factor,ss3%ee_mobility_factor,iout,ierr)
        ss3%ee_mobility_factor = ss2%ee_mobility_factor
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Einj_max)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%Einj_max).AND.ALLOCATED(ss2%Einj_max)) THEN
        CALL ps_diff("Einj_max",ss1%Einj_max,ss2%Einj_max,icheck,iout,ierr)
        CALL ps_conformal("Einj_max",ss1%Einj_max,ss3%Einj_max,iout,ierr)
        if(ierr.eq.0) ss3%Einj_max = ss1%Einj_max
     ELSE IF(ALLOCATED(ss1%Einj_max)) THEN
        CALL ps_conformal("Einj_max",ss1%Einj_max,ss3%Einj_max,iout,ierr)
        ss3%Einj_max = ss1%Einj_max
     ELSE IF(ALLOCATED(ss2%Einj_max)) THEN
        CALL ps_conformal("Einj_max",ss2%Einj_max,ss3%Einj_max,iout,ierr)
        ss3%Einj_max = ss2%Einj_max
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Einj_min)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%Einj_min).AND.ALLOCATED(ss2%Einj_min)) THEN
        CALL ps_diff("Einj_min",ss1%Einj_min,ss2%Einj_min,icheck,iout,ierr)
        CALL ps_conformal("Einj_min",ss1%Einj_min,ss3%Einj_min,iout,ierr)
        if(ierr.eq.0) ss3%Einj_min = ss1%Einj_min
     ELSE IF(ALLOCATED(ss1%Einj_min)) THEN
        CALL ps_conformal("Einj_min",ss1%Einj_min,ss3%Einj_min,iout,ierr)
        ss3%Einj_min = ss1%Einj_min
     ELSE IF(ALLOCATED(ss2%Einj_min)) THEN
        CALL ps_conformal("Einj_min",ss2%Einj_min,ss3%Einj_min,iout,ierr)
        ss3%Einj_min = ss2%Einj_min
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     ss3%eldot = w1*ss1%eldot + (1.0_rspec-w1)*ss2%eldot
  ENDIF

  IF((ALLOCATED(ss3%elong)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%elong).AND.ALLOCATED(ss2%elong)) THEN
        CALL ps_conformal("elong",ss2%elong,ss3%elong,iout,ierr)
        CALL ps_conformal("elong",ss1%elong,ss3%elong,iout,ierr)
        if(ierr.eq.0) ss3%elong = w1*ss1%elong + (1.0_rspec-w1)*ss2%elong
     ELSE IF(ALLOCATED(ss1%elong)) THEN
        CALL ps_conformal("elong",ss1%elong,ss3%elong,iout,ierr)
        ss3%elong = ss1%elong
     ELSE IF(ALLOCATED(ss2%elong)) THEN
        CALL ps_conformal("elong",ss2%elong,ss3%elong,iout,ierr)
        ss3%elong = ss2%elong
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%eperp_beami)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%eperp_beami).AND.ALLOCATED(ss2%eperp_beami)) THEN
        CALL ps_conformal("eperp_beami",ss2%eperp_beami,ss3%eperp_beami,iout,ierr)
        CALL ps_conformal("eperp_beami",ss1%eperp_beami,ss3%eperp_beami,iout,ierr)
        if(ierr.eq.0) ss3%eperp_beami = w1*ss1%eperp_beami + (1.0_rspec-w1)*ss2%eperp_beami
     ELSE IF(ALLOCATED(ss1%eperp_beami)) THEN
        CALL ps_conformal("eperp_beami",ss1%eperp_beami,ss3%eperp_beami,iout,ierr)
        ss3%eperp_beami = ss1%eperp_beami
     ELSE IF(ALLOCATED(ss2%eperp_beami)) THEN
        CALL ps_conformal("eperp_beami",ss2%eperp_beami,ss3%eperp_beami,iout,ierr)
        ss3%eperp_beami = ss2%eperp_beami
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%eperp_fusi)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%eperp_fusi).AND.ALLOCATED(ss2%eperp_fusi)) THEN
        CALL ps_conformal("eperp_fusi",ss2%eperp_fusi,ss3%eperp_fusi,iout,ierr)
        CALL ps_conformal("eperp_fusi",ss1%eperp_fusi,ss3%eperp_fusi,iout,ierr)
        if(ierr.eq.0) ss3%eperp_fusi = w1*ss1%eperp_fusi + (1.0_rspec-w1)*ss2%eperp_fusi
     ELSE IF(ALLOCATED(ss1%eperp_fusi)) THEN
        CALL ps_conformal("eperp_fusi",ss1%eperp_fusi,ss3%eperp_fusi,iout,ierr)
        ss3%eperp_fusi = ss1%eperp_fusi
     ELSE IF(ALLOCATED(ss2%eperp_fusi)) THEN
        CALL ps_conformal("eperp_fusi",ss2%eperp_fusi,ss3%eperp_fusi,iout,ierr)
        ss3%eperp_fusi = ss2%eperp_fusi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%eperp_mini)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%eperp_mini).AND.ALLOCATED(ss2%eperp_mini)) THEN
        CALL ps_conformal("eperp_mini",ss2%eperp_mini,ss3%eperp_mini,iout,ierr)
        CALL ps_conformal("eperp_mini",ss1%eperp_mini,ss3%eperp_mini,iout,ierr)
        if(ierr.eq.0) ss3%eperp_mini = w1*ss1%eperp_mini + (1.0_rspec-w1)*ss2%eperp_mini
     ELSE IF(ALLOCATED(ss1%eperp_mini)) THEN
        CALL ps_conformal("eperp_mini",ss1%eperp_mini,ss3%eperp_mini,iout,ierr)
        ss3%eperp_mini = ss1%eperp_mini
     ELSE IF(ALLOCATED(ss2%eperp_mini)) THEN
        CALL ps_conformal("eperp_mini",ss2%eperp_mini,ss3%eperp_mini,iout,ierr)
        ss3%eperp_mini = ss2%eperp_mini
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%eperp_rw)).AND.(cclist(ps_cnum_RUNAWAY).EQ.1)) THEN
     IF(ALLOCATED(ss1%eperp_rw).AND.ALLOCATED(ss2%eperp_rw)) THEN
        CALL ps_conformal("eperp_rw",ss2%eperp_rw,ss3%eperp_rw,iout,ierr)
        CALL ps_conformal("eperp_rw",ss1%eperp_rw,ss3%eperp_rw,iout,ierr)
        if(ierr.eq.0) ss3%eperp_rw = w1*ss1%eperp_rw + (1.0_rspec-w1)*ss2%eperp_rw
     ELSE IF(ALLOCATED(ss1%eperp_rw)) THEN
        CALL ps_conformal("eperp_rw",ss1%eperp_rw,ss3%eperp_rw,iout,ierr)
        ss3%eperp_rw = ss1%eperp_rw
     ELSE IF(ALLOCATED(ss2%eperp_rw)) THEN
        CALL ps_conformal("eperp_rw",ss2%eperp_rw,ss3%eperp_rw,iout,ierr)
        ss3%eperp_rw = ss2%eperp_rw
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%epll_beami)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%epll_beami).AND.ALLOCATED(ss2%epll_beami)) THEN
        CALL ps_conformal("epll_beami",ss2%epll_beami,ss3%epll_beami,iout,ierr)
        CALL ps_conformal("epll_beami",ss1%epll_beami,ss3%epll_beami,iout,ierr)
        if(ierr.eq.0) ss3%epll_beami = w1*ss1%epll_beami + (1.0_rspec-w1)*ss2%epll_beami
     ELSE IF(ALLOCATED(ss1%epll_beami)) THEN
        CALL ps_conformal("epll_beami",ss1%epll_beami,ss3%epll_beami,iout,ierr)
        ss3%epll_beami = ss1%epll_beami
     ELSE IF(ALLOCATED(ss2%epll_beami)) THEN
        CALL ps_conformal("epll_beami",ss2%epll_beami,ss3%epll_beami,iout,ierr)
        ss3%epll_beami = ss2%epll_beami
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%epll_fusi)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%epll_fusi).AND.ALLOCATED(ss2%epll_fusi)) THEN
        CALL ps_conformal("epll_fusi",ss2%epll_fusi,ss3%epll_fusi,iout,ierr)
        CALL ps_conformal("epll_fusi",ss1%epll_fusi,ss3%epll_fusi,iout,ierr)
        if(ierr.eq.0) ss3%epll_fusi = w1*ss1%epll_fusi + (1.0_rspec-w1)*ss2%epll_fusi
     ELSE IF(ALLOCATED(ss1%epll_fusi)) THEN
        CALL ps_conformal("epll_fusi",ss1%epll_fusi,ss3%epll_fusi,iout,ierr)
        ss3%epll_fusi = ss1%epll_fusi
     ELSE IF(ALLOCATED(ss2%epll_fusi)) THEN
        CALL ps_conformal("epll_fusi",ss2%epll_fusi,ss3%epll_fusi,iout,ierr)
        ss3%epll_fusi = ss2%epll_fusi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%epll_mini)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%epll_mini).AND.ALLOCATED(ss2%epll_mini)) THEN
        CALL ps_conformal("epll_mini",ss2%epll_mini,ss3%epll_mini,iout,ierr)
        CALL ps_conformal("epll_mini",ss1%epll_mini,ss3%epll_mini,iout,ierr)
        if(ierr.eq.0) ss3%epll_mini = w1*ss1%epll_mini + (1.0_rspec-w1)*ss2%epll_mini
     ELSE IF(ALLOCATED(ss1%epll_mini)) THEN
        CALL ps_conformal("epll_mini",ss1%epll_mini,ss3%epll_mini,iout,ierr)
        ss3%epll_mini = ss1%epll_mini
     ELSE IF(ALLOCATED(ss2%epll_mini)) THEN
        CALL ps_conformal("epll_mini",ss2%epll_mini,ss3%epll_mini,iout,ierr)
        ss3%epll_mini = ss2%epll_mini
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%epll_rw)).AND.(cclist(ps_cnum_RUNAWAY).EQ.1)) THEN
     IF(ALLOCATED(ss1%epll_rw).AND.ALLOCATED(ss2%epll_rw)) THEN
        CALL ps_conformal("epll_rw",ss2%epll_rw,ss3%epll_rw,iout,ierr)
        CALL ps_conformal("epll_rw",ss1%epll_rw,ss3%epll_rw,iout,ierr)
        if(ierr.eq.0) ss3%epll_rw = w1*ss1%epll_rw + (1.0_rspec-w1)*ss2%epll_rw
     ELSE IF(ALLOCATED(ss1%epll_rw)) THEN
        CALL ps_conformal("epll_rw",ss1%epll_rw,ss3%epll_rw,iout,ierr)
        ss3%epll_rw = ss1%epll_rw
     ELSE IF(ALLOCATED(ss2%epll_rw)) THEN
        CALL ps_conformal("epll_rw",ss2%epll_rw,ss3%epll_rw,iout,ierr)
        ss3%epll_rw = ss2%epll_rw
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Epot)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%Epot).AND.ALLOCATED(ss2%Epot)) THEN
        CALL ps_conformal("Epot",ss2%Epot,ss3%Epot,iout,ierr)
        CALL ps_conformal("Epot",ss1%Epot,ss3%Epot,iout,ierr)
        if(ierr.eq.0) ss3%Epot = w1*ss1%Epot + (1.0_rspec-w1)*ss2%Epot
     ELSE IF(ALLOCATED(ss1%Epot)) THEN
        CALL ps_conformal("Epot",ss1%Epot,ss3%Epot,iout,ierr)
        ss3%Epot = ss1%Epot
     ELSE IF(ALLOCATED(ss2%Epot)) THEN
        CALL ps_conformal("Epot",ss2%Epot,ss3%Epot,iout,ierr)
        ss3%Epot = ss2%Epot
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN

     CALL ps_diff("eqdsk_file",ss1%eqdsk_file,ss2%eqdsk_file,icheck,iout,ierr)
     ss3%eqdsk_file = ss1%eqdsk_file
  ENDIF

  IF((ALLOCATED(ss3%eqmom_num)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%eqmom_num).AND.ALLOCATED(ss2%eqmom_num)) THEN
        CALL ps_diff("eqmom_num",ss1%eqmom_num,ss2%eqmom_num,icheck,iout,ierr)
        CALL ps_conformal("eqmom_num",ss1%eqmom_num,ss3%eqmom_num,iout,ierr)
        if(ierr.eq.0) ss3%eqmom_num = ss1%eqmom_num
     ELSE IF(ALLOCATED(ss1%eqmom_num)) THEN
        CALL ps_conformal("eqmom_num",ss1%eqmom_num,ss3%eqmom_num,iout,ierr)
        ss3%eqmom_num = ss1%eqmom_num
     ELSE IF(ALLOCATED(ss2%eqmom_num)) THEN
        CALL ps_conformal("eqmom_num",ss2%eqmom_num,ss3%eqmom_num,iout,ierr)
        ss3%eqmom_num = ss2%eqmom_num
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN

     CALL ps_diff("EQ_Code_Info",ss1%EQ_Code_Info,ss2%EQ_Code_Info,icheck,iout,ierr)
     ss3%EQ_Code_Info = ss1%EQ_Code_Info
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN

     CALL ps_diff("EQ_Data_Info",ss1%EQ_Data_Info,ss2%EQ_Data_Info,icheck,iout,ierr)
     ss3%EQ_Data_Info = ss1%EQ_Data_Info
  ENDIF

  IF((ALLOCATED(ss3%eta_parallel)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%eta_parallel).AND.ALLOCATED(ss2%eta_parallel)) THEN
        CALL ps_conformal("eta_parallel",ss2%eta_parallel,ss3%eta_parallel,iout,ierr)
        CALL ps_conformal("eta_parallel",ss1%eta_parallel,ss3%eta_parallel,iout,ierr)
        if(ierr.eq.0) ss3%eta_parallel = w1*ss1%eta_parallel + (1.0_rspec-w1)*ss2%eta_parallel
     ELSE IF(ALLOCATED(ss1%eta_parallel)) THEN
        CALL ps_conformal("eta_parallel",ss1%eta_parallel,ss3%eta_parallel,iout,ierr)
        ss3%eta_parallel = ss1%eta_parallel
     ELSE IF(ALLOCATED(ss2%eta_parallel)) THEN
        CALL ps_conformal("eta_parallel",ss2%eta_parallel,ss3%eta_parallel,iout,ierr)
        ss3%eta_parallel = ss2%eta_parallel
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%E_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%E_anom).AND.ALLOCATED(ss2%E_anom)) THEN
        CALL ps_diff("E_anom",ss1%E_anom,ss2%E_anom,ps_error,iout,ierr)
        CALL ps_conformal("E_anom",ss1%E_anom,ss3%E_anom,iout,ierr)
        if(ierr.eq.0) ss3%E_anom = ss1%E_anom
     ELSE IF(ALLOCATED(ss1%E_anom)) THEN
        CALL ps_conformal("E_anom",ss1%E_anom,ss3%E_anom,iout,ierr)
        ss3%E_anom = ss1%E_anom
     ELSE IF(ALLOCATED(ss2%E_anom)) THEN
        CALL ps_conformal("E_anom",ss2%E_anom,ss3%E_anom,iout,ierr)
        ss3%E_anom = ss2%E_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%E_anom2)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%E_anom2).AND.ALLOCATED(ss2%E_anom2)) THEN
        CALL ps_diff("E_anom2",ss1%E_anom2,ss2%E_anom2,ps_error,iout,ierr)
        CALL ps_conformal("E_anom2",ss1%E_anom2,ss3%E_anom2,iout,ierr)
        if(ierr.eq.0) ss3%E_anom2 = ss1%E_anom2
     ELSE IF(ALLOCATED(ss1%E_anom2)) THEN
        CALL ps_conformal("E_anom2",ss1%E_anom2,ss3%E_anom2,iout,ierr)
        ss3%E_anom2 = ss1%E_anom2
     ELSE IF(ALLOCATED(ss2%E_anom2)) THEN
        CALL ps_conformal("E_anom2",ss2%E_anom2,ss3%E_anom2,iout,ierr)
        ss3%E_anom2 = ss2%E_anom2
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%fidiff_bpass_co)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%fidiff_bpass_co).AND.ALLOCATED(ss2%fidiff_bpass_co)) THEN
        CALL ps_conformal("fidiff_bpass_co",ss2%fidiff_bpass_co,ss3%fidiff_bpass_co,iout,ierr)
        CALL ps_conformal("fidiff_bpass_co",ss1%fidiff_bpass_co,ss3%fidiff_bpass_co,iout,ierr)
        if(ierr.eq.0) ss3%fidiff_bpass_co = w1*ss1%fidiff_bpass_co + (1.0_rspec-w1)*ss2%fidiff_bpass_co
     ELSE IF(ALLOCATED(ss1%fidiff_bpass_co)) THEN
        CALL ps_conformal("fidiff_bpass_co",ss1%fidiff_bpass_co,ss3%fidiff_bpass_co,iout,ierr)
        ss3%fidiff_bpass_co = ss1%fidiff_bpass_co
     ELSE IF(ALLOCATED(ss2%fidiff_bpass_co)) THEN
        CALL ps_conformal("fidiff_bpass_co",ss2%fidiff_bpass_co,ss3%fidiff_bpass_co,iout,ierr)
        ss3%fidiff_bpass_co = ss2%fidiff_bpass_co
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%fidiff_bpass_ctr)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%fidiff_bpass_ctr).AND.ALLOCATED(ss2%fidiff_bpass_ctr)) THEN
        CALL ps_conformal("fidiff_bpass_ctr",ss2%fidiff_bpass_ctr,ss3%fidiff_bpass_ctr,iout,ierr)
        CALL ps_conformal("fidiff_bpass_ctr",ss1%fidiff_bpass_ctr,ss3%fidiff_bpass_ctr,iout,ierr)
        if(ierr.eq.0) ss3%fidiff_bpass_ctr = w1*ss1%fidiff_bpass_ctr + (1.0_rspec-w1)*ss2%fidiff_bpass_ctr
     ELSE IF(ALLOCATED(ss1%fidiff_bpass_ctr)) THEN
        CALL ps_conformal("fidiff_bpass_ctr",ss1%fidiff_bpass_ctr,ss3%fidiff_bpass_ctr,iout,ierr)
        ss3%fidiff_bpass_ctr = ss1%fidiff_bpass_ctr
     ELSE IF(ALLOCATED(ss2%fidiff_bpass_ctr)) THEN
        CALL ps_conformal("fidiff_bpass_ctr",ss2%fidiff_bpass_ctr,ss3%fidiff_bpass_ctr,iout,ierr)
        ss3%fidiff_bpass_ctr = ss2%fidiff_bpass_ctr
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%fidiff_btrap)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%fidiff_btrap).AND.ALLOCATED(ss2%fidiff_btrap)) THEN
        CALL ps_conformal("fidiff_btrap",ss2%fidiff_btrap,ss3%fidiff_btrap,iout,ierr)
        CALL ps_conformal("fidiff_btrap",ss1%fidiff_btrap,ss3%fidiff_btrap,iout,ierr)
        if(ierr.eq.0) ss3%fidiff_btrap = w1*ss1%fidiff_btrap + (1.0_rspec-w1)*ss2%fidiff_btrap
     ELSE IF(ALLOCATED(ss1%fidiff_btrap)) THEN
        CALL ps_conformal("fidiff_btrap",ss1%fidiff_btrap,ss3%fidiff_btrap,iout,ierr)
        ss3%fidiff_btrap = ss1%fidiff_btrap
     ELSE IF(ALLOCATED(ss2%fidiff_btrap)) THEN
        CALL ps_conformal("fidiff_btrap",ss2%fidiff_btrap,ss3%fidiff_btrap,iout,ierr)
        ss3%fidiff_btrap = ss2%fidiff_btrap
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%fidiff_dpass_co)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%fidiff_dpass_co).AND.ALLOCATED(ss2%fidiff_dpass_co)) THEN
        CALL ps_conformal("fidiff_dpass_co",ss2%fidiff_dpass_co,ss3%fidiff_dpass_co,iout,ierr)
        CALL ps_conformal("fidiff_dpass_co",ss1%fidiff_dpass_co,ss3%fidiff_dpass_co,iout,ierr)
        if(ierr.eq.0) ss3%fidiff_dpass_co = w1*ss1%fidiff_dpass_co + (1.0_rspec-w1)*ss2%fidiff_dpass_co
     ELSE IF(ALLOCATED(ss1%fidiff_dpass_co)) THEN
        CALL ps_conformal("fidiff_dpass_co",ss1%fidiff_dpass_co,ss3%fidiff_dpass_co,iout,ierr)
        ss3%fidiff_dpass_co = ss1%fidiff_dpass_co
     ELSE IF(ALLOCATED(ss2%fidiff_dpass_co)) THEN
        CALL ps_conformal("fidiff_dpass_co",ss2%fidiff_dpass_co,ss3%fidiff_dpass_co,iout,ierr)
        ss3%fidiff_dpass_co = ss2%fidiff_dpass_co
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%fidiff_dpass_ctr)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%fidiff_dpass_ctr).AND.ALLOCATED(ss2%fidiff_dpass_ctr)) THEN
        CALL ps_conformal("fidiff_dpass_ctr",ss2%fidiff_dpass_ctr,ss3%fidiff_dpass_ctr,iout,ierr)
        CALL ps_conformal("fidiff_dpass_ctr",ss1%fidiff_dpass_ctr,ss3%fidiff_dpass_ctr,iout,ierr)
        if(ierr.eq.0) ss3%fidiff_dpass_ctr = w1*ss1%fidiff_dpass_ctr + (1.0_rspec-w1)*ss2%fidiff_dpass_ctr
     ELSE IF(ALLOCATED(ss1%fidiff_dpass_ctr)) THEN
        CALL ps_conformal("fidiff_dpass_ctr",ss1%fidiff_dpass_ctr,ss3%fidiff_dpass_ctr,iout,ierr)
        ss3%fidiff_dpass_ctr = ss1%fidiff_dpass_ctr
     ELSE IF(ALLOCATED(ss2%fidiff_dpass_ctr)) THEN
        CALL ps_conformal("fidiff_dpass_ctr",ss2%fidiff_dpass_ctr,ss3%fidiff_dpass_ctr,iout,ierr)
        ss3%fidiff_dpass_ctr = ss2%fidiff_dpass_ctr
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%fidiff_dtrap)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%fidiff_dtrap).AND.ALLOCATED(ss2%fidiff_dtrap)) THEN
        CALL ps_conformal("fidiff_dtrap",ss2%fidiff_dtrap,ss3%fidiff_dtrap,iout,ierr)
        CALL ps_conformal("fidiff_dtrap",ss1%fidiff_dtrap,ss3%fidiff_dtrap,iout,ierr)
        if(ierr.eq.0) ss3%fidiff_dtrap = w1*ss1%fidiff_dtrap + (1.0_rspec-w1)*ss2%fidiff_dtrap
     ELSE IF(ALLOCATED(ss1%fidiff_dtrap)) THEN
        CALL ps_conformal("fidiff_dtrap",ss1%fidiff_dtrap,ss3%fidiff_dtrap,iout,ierr)
        ss3%fidiff_dtrap = ss1%fidiff_dtrap
     ELSE IF(ALLOCATED(ss2%fidiff_dtrap)) THEN
        CALL ps_conformal("fidiff_dtrap",ss2%fidiff_dtrap,ss3%fidiff_dtrap,iout,ierr)
        ss3%fidiff_dtrap = ss2%fidiff_dtrap
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Fi_depletion)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%Fi_depletion).AND.ALLOCATED(ss2%Fi_depletion)) THEN
        CALL ps_conformal("Fi_depletion",ss2%Fi_depletion,ss3%Fi_depletion,iout,ierr)
        CALL ps_conformal("Fi_depletion",ss1%Fi_depletion,ss3%Fi_depletion,iout,ierr)
        if(ierr.eq.0) ss3%Fi_depletion = w1*ss1%Fi_depletion + (1.0_rspec-w1)*ss2%Fi_depletion
     ELSE IF(ALLOCATED(ss1%Fi_depletion)) THEN
        CALL ps_conformal("Fi_depletion",ss1%Fi_depletion,ss3%Fi_depletion,iout,ierr)
        ss3%Fi_depletion = ss1%Fi_depletion
     ELSE IF(ALLOCATED(ss2%Fi_depletion)) THEN
        CALL ps_conformal("Fi_depletion",ss2%Fi_depletion,ss3%Fi_depletion,iout,ierr)
        ss3%Fi_depletion = ss2%Fi_depletion
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%fracmin)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%fracmin).AND.ALLOCATED(ss2%fracmin)) THEN
        CALL ps_diff("fracmin",ss1%fracmin,ss2%fracmin,icheck,iout,ierr)
        CALL ps_conformal("fracmin",ss1%fracmin,ss3%fracmin,iout,ierr)
        if(ierr.eq.0) ss3%fracmin = ss1%fracmin
     ELSE IF(ALLOCATED(ss1%fracmin)) THEN
        CALL ps_conformal("fracmin",ss1%fracmin,ss3%fracmin,iout,ierr)
        ss3%fracmin = ss1%fracmin
     ELSE IF(ALLOCATED(ss2%fracmin)) THEN
        CALL ps_conformal("fracmin",ss2%fracmin,ss3%fracmin,iout,ierr)
        ss3%fracmin = ss2%fracmin
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%fracpslh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%fracpslh).AND.ALLOCATED(ss2%fracpslh)) THEN
        CALL ps_diff("fracpslh",ss1%fracpslh,ss2%fracpslh,icheck,iout,ierr)
        CALL ps_conformal("fracpslh",ss1%fracpslh,ss3%fracpslh,iout,ierr)
        if(ierr.eq.0) ss3%fracpslh = ss1%fracpslh
     ELSE IF(ALLOCATED(ss1%fracpslh)) THEN
        CALL ps_conformal("fracpslh",ss1%fracpslh,ss3%fracpslh,iout,ierr)
        ss3%fracpslh = ss1%fracpslh
     ELSE IF(ALLOCATED(ss2%fracpslh)) THEN
        CALL ps_conformal("fracpslh",ss2%fracpslh,ss3%fracpslh,iout,ierr)
        ss3%fracpslh = ss2%fracpslh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%frac_full)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%frac_full).AND.ALLOCATED(ss2%frac_full)) THEN
        CALL ps_conformal("frac_full",ss2%frac_full,ss3%frac_full,iout,ierr)
        CALL ps_conformal("frac_full",ss1%frac_full,ss3%frac_full,iout,ierr)
        if(ierr.eq.0) ss3%frac_full = w1*ss1%frac_full + (1.0_rspec-w1)*ss2%frac_full
     ELSE IF(ALLOCATED(ss1%frac_full)) THEN
        CALL ps_conformal("frac_full",ss1%frac_full,ss3%frac_full,iout,ierr)
        ss3%frac_full = ss1%frac_full
     ELSE IF(ALLOCATED(ss2%frac_full)) THEN
        CALL ps_conformal("frac_full",ss2%frac_full,ss3%frac_full,iout,ierr)
        ss3%frac_full = ss2%frac_full
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%frac_half)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%frac_half).AND.ALLOCATED(ss2%frac_half)) THEN
        CALL ps_conformal("frac_half",ss2%frac_half,ss3%frac_half,iout,ierr)
        CALL ps_conformal("frac_half",ss1%frac_half,ss3%frac_half,iout,ierr)
        if(ierr.eq.0) ss3%frac_half = w1*ss1%frac_half + (1.0_rspec-w1)*ss2%frac_half
     ELSE IF(ALLOCATED(ss1%frac_half)) THEN
        CALL ps_conformal("frac_half",ss1%frac_half,ss3%frac_half,iout,ierr)
        ss3%frac_half = ss1%frac_half
     ELSE IF(ALLOCATED(ss2%frac_half)) THEN
        CALL ps_conformal("frac_half",ss2%frac_half,ss3%frac_half,iout,ierr)
        ss3%frac_half = ss2%frac_half
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%freq_drbm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%freq_drbm).AND.ALLOCATED(ss2%freq_drbm)) THEN
        CALL ps_conformal("freq_drbm",ss2%freq_drbm,ss3%freq_drbm,iout,ierr)
        CALL ps_conformal("freq_drbm",ss1%freq_drbm,ss3%freq_drbm,iout,ierr)
        if(ierr.eq.0) ss3%freq_drbm = w1*ss1%freq_drbm + (1.0_rspec-w1)*ss2%freq_drbm
     ELSE IF(ALLOCATED(ss1%freq_drbm)) THEN
        CALL ps_conformal("freq_drbm",ss1%freq_drbm,ss3%freq_drbm,iout,ierr)
        ss3%freq_drbm = ss1%freq_drbm
     ELSE IF(ALLOCATED(ss2%freq_drbm)) THEN
        CALL ps_conformal("freq_drbm",ss2%freq_drbm,ss3%freq_drbm,iout,ierr)
        ss3%freq_drbm = ss2%freq_drbm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%freq_ec)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%freq_ec).AND.ALLOCATED(ss2%freq_ec)) THEN
        CALL ps_diff("freq_ec",ss1%freq_ec,ss2%freq_ec,icheck,iout,ierr)
        CALL ps_conformal("freq_ec",ss1%freq_ec,ss3%freq_ec,iout,ierr)
        if(ierr.eq.0) ss3%freq_ec = ss1%freq_ec
     ELSE IF(ALLOCATED(ss1%freq_ec)) THEN
        CALL ps_conformal("freq_ec",ss1%freq_ec,ss3%freq_ec,iout,ierr)
        ss3%freq_ec = ss1%freq_ec
     ELSE IF(ALLOCATED(ss2%freq_ec)) THEN
        CALL ps_conformal("freq_ec",ss2%freq_ec,ss3%freq_ec,iout,ierr)
        ss3%freq_ec = ss2%freq_ec
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%freq_etg)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%freq_etg).AND.ALLOCATED(ss2%freq_etg)) THEN
        CALL ps_conformal("freq_etg",ss2%freq_etg,ss3%freq_etg,iout,ierr)
        CALL ps_conformal("freq_etg",ss1%freq_etg,ss3%freq_etg,iout,ierr)
        if(ierr.eq.0) ss3%freq_etg = w1*ss1%freq_etg + (1.0_rspec-w1)*ss2%freq_etg
     ELSE IF(ALLOCATED(ss1%freq_etg)) THEN
        CALL ps_conformal("freq_etg",ss1%freq_etg,ss3%freq_etg,iout,ierr)
        ss3%freq_etg = ss1%freq_etg
     ELSE IF(ALLOCATED(ss2%freq_etg)) THEN
        CALL ps_conformal("freq_etg",ss2%freq_etg,ss3%freq_etg,iout,ierr)
        ss3%freq_etg = ss2%freq_etg
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%freq_glf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%freq_glf).AND.ALLOCATED(ss2%freq_glf)) THEN
        CALL ps_conformal("freq_glf",ss2%freq_glf,ss3%freq_glf,iout,ierr)
        CALL ps_conformal("freq_glf",ss1%freq_glf,ss3%freq_glf,iout,ierr)
        if(ierr.eq.0) ss3%freq_glf = w1*ss1%freq_glf + (1.0_rspec-w1)*ss2%freq_glf
     ELSE IF(ALLOCATED(ss1%freq_glf)) THEN
        CALL ps_conformal("freq_glf",ss1%freq_glf,ss3%freq_glf,iout,ierr)
        ss3%freq_glf = ss1%freq_glf
     ELSE IF(ALLOCATED(ss2%freq_glf)) THEN
        CALL ps_conformal("freq_glf",ss2%freq_glf,ss3%freq_glf,iout,ierr)
        ss3%freq_glf = ss2%freq_glf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%freq_ic)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%freq_ic).AND.ALLOCATED(ss2%freq_ic)) THEN
        CALL ps_diff("freq_ic",ss1%freq_ic,ss2%freq_ic,icheck,iout,ierr)
        CALL ps_conformal("freq_ic",ss1%freq_ic,ss3%freq_ic,iout,ierr)
        if(ierr.eq.0) ss3%freq_ic = ss1%freq_ic
     ELSE IF(ALLOCATED(ss1%freq_ic)) THEN
        CALL ps_conformal("freq_ic",ss1%freq_ic,ss3%freq_ic,iout,ierr)
        ss3%freq_ic = ss1%freq_ic
     ELSE IF(ALLOCATED(ss2%freq_ic)) THEN
        CALL ps_conformal("freq_ic",ss2%freq_ic,ss3%freq_ic,iout,ierr)
        ss3%freq_ic = ss2%freq_ic
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%freq_lh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%freq_lh).AND.ALLOCATED(ss2%freq_lh)) THEN
        CALL ps_diff("freq_lh",ss1%freq_lh,ss2%freq_lh,icheck,iout,ierr)
        CALL ps_conformal("freq_lh",ss1%freq_lh,ss3%freq_lh,iout,ierr)
        if(ierr.eq.0) ss3%freq_lh = ss1%freq_lh
     ELSE IF(ALLOCATED(ss1%freq_lh)) THEN
        CALL ps_conformal("freq_lh",ss1%freq_lh,ss3%freq_lh,iout,ierr)
        ss3%freq_lh = ss1%freq_lh
     ELSE IF(ALLOCATED(ss2%freq_lh)) THEN
        CALL ps_conformal("freq_lh",ss2%freq_lh,ss3%freq_lh,iout,ierr)
        ss3%freq_lh = ss2%freq_lh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%freq_max_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%freq_max_tglf).AND.ALLOCATED(ss2%freq_max_tglf)) THEN
        CALL ps_conformal("freq_max_tglf",ss2%freq_max_tglf,ss3%freq_max_tglf,iout,ierr)
        CALL ps_conformal("freq_max_tglf",ss1%freq_max_tglf,ss3%freq_max_tglf,iout,ierr)
        if(ierr.eq.0) ss3%freq_max_tglf = w1*ss1%freq_max_tglf + (1.0_rspec-w1)*ss2%freq_max_tglf
     ELSE IF(ALLOCATED(ss1%freq_max_tglf)) THEN
        CALL ps_conformal("freq_max_tglf",ss1%freq_max_tglf,ss3%freq_max_tglf,iout,ierr)
        ss3%freq_max_tglf = ss1%freq_max_tglf
     ELSE IF(ALLOCATED(ss2%freq_max_tglf)) THEN
        CALL ps_conformal("freq_max_tglf",ss2%freq_max_tglf,ss3%freq_max_tglf,iout,ierr)
        ss3%freq_max_tglf = ss2%freq_max_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%freq_mmm71)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%freq_mmm71).AND.ALLOCATED(ss2%freq_mmm71)) THEN
        CALL ps_conformal("freq_mmm71",ss2%freq_mmm71,ss3%freq_mmm71,iout,ierr)
        CALL ps_conformal("freq_mmm71",ss1%freq_mmm71,ss3%freq_mmm71,iout,ierr)
        if(ierr.eq.0) ss3%freq_mmm71 = w1*ss1%freq_mmm71 + (1.0_rspec-w1)*ss2%freq_mmm71
     ELSE IF(ALLOCATED(ss1%freq_mmm71)) THEN
        CALL ps_conformal("freq_mmm71",ss1%freq_mmm71,ss3%freq_mmm71,iout,ierr)
        ss3%freq_mmm71 = ss1%freq_mmm71
     ELSE IF(ALLOCATED(ss2%freq_mmm71)) THEN
        CALL ps_conformal("freq_mmm71",ss2%freq_mmm71,ss3%freq_mmm71,iout,ierr)
        ss3%freq_mmm71 = ss2%freq_mmm71
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%freq_mtm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%freq_mtm).AND.ALLOCATED(ss2%freq_mtm)) THEN
        CALL ps_conformal("freq_mtm",ss2%freq_mtm,ss3%freq_mtm,iout,ierr)
        CALL ps_conformal("freq_mtm",ss1%freq_mtm,ss3%freq_mtm,iout,ierr)
        if(ierr.eq.0) ss3%freq_mtm = w1*ss1%freq_mtm + (1.0_rspec-w1)*ss2%freq_mtm
     ELSE IF(ALLOCATED(ss1%freq_mtm)) THEN
        CALL ps_conformal("freq_mtm",ss1%freq_mtm,ss3%freq_mtm,iout,ierr)
        ss3%freq_mtm = ss1%freq_mtm
     ELSE IF(ALLOCATED(ss2%freq_mtm)) THEN
        CALL ps_conformal("freq_mtm",ss2%freq_mtm,ss3%freq_mtm,iout,ierr)
        ss3%freq_mtm = ss2%freq_mtm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%freq_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%freq_tglf).AND.ALLOCATED(ss2%freq_tglf)) THEN
        CALL ps_conformal("freq_tglf",ss2%freq_tglf,ss3%freq_tglf,iout,ierr)
        CALL ps_conformal("freq_tglf",ss1%freq_tglf,ss3%freq_tglf,iout,ierr)
        if(ierr.eq.0) ss3%freq_tglf = w1*ss1%freq_tglf + (1.0_rspec-w1)*ss2%freq_tglf
     ELSE IF(ALLOCATED(ss1%freq_tglf)) THEN
        CALL ps_conformal("freq_tglf",ss1%freq_tglf,ss3%freq_tglf,iout,ierr)
        ss3%freq_tglf = ss1%freq_tglf
     ELSE IF(ALLOCATED(ss2%freq_tglf)) THEN
        CALL ps_conformal("freq_tglf",ss2%freq_tglf,ss3%freq_tglf,iout,ierr)
        ss3%freq_tglf = ss2%freq_tglf
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_FUS).EQ.1) THEN

     CALL ps_diff("FUS_Code_Info",ss1%FUS_Code_Info,ss2%FUS_Code_Info,icheck,iout,ierr)
     ss3%FUS_Code_Info = ss1%FUS_Code_Info
  ENDIF

  IF((ALLOCATED(ss3%gamma_lmhd)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%gamma_lmhd).AND.ALLOCATED(ss2%gamma_lmhd)) THEN
        CALL ps_conformal("gamma_lmhd",ss2%gamma_lmhd,ss3%gamma_lmhd,iout,ierr)
        CALL ps_conformal("gamma_lmhd",ss1%gamma_lmhd,ss3%gamma_lmhd,iout,ierr)
        if(ierr.eq.0) ss3%gamma_lmhd = w1*ss1%gamma_lmhd + (1.0_rspec-w1)*ss2%gamma_lmhd
     ELSE IF(ALLOCATED(ss1%gamma_lmhd)) THEN
        CALL ps_conformal("gamma_lmhd",ss1%gamma_lmhd,ss3%gamma_lmhd,iout,ierr)
        ss3%gamma_lmhd = ss1%gamma_lmhd
     ELSE IF(ALLOCATED(ss2%gamma_lmhd)) THEN
        CALL ps_conformal("gamma_lmhd",ss2%gamma_lmhd,ss3%gamma_lmhd,iout,ierr)
        ss3%gamma_lmhd = ss2%gamma_lmhd
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%gamma_nc)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%gamma_nc).AND.ALLOCATED(ss2%gamma_nc)) THEN
        CALL ps_conformal("gamma_nc",ss2%gamma_nc,ss3%gamma_nc,iout,ierr)
        CALL ps_conformal("gamma_nc",ss1%gamma_nc,ss3%gamma_nc,iout,ierr)
        if(ierr.eq.0) ss3%gamma_nc = w1*ss1%gamma_nc + (1.0_rspec-w1)*ss2%gamma_nc
     ELSE IF(ALLOCATED(ss1%gamma_nc)) THEN
        CALL ps_conformal("gamma_nc",ss1%gamma_nc,ss3%gamma_nc,iout,ierr)
        ss3%gamma_nc = ss1%gamma_nc
     ELSE IF(ALLOCATED(ss2%gamma_nc)) THEN
        CALL ps_conformal("gamma_nc",ss2%gamma_nc,ss3%gamma_nc,iout,ierr)
        ss3%gamma_nc = ss2%gamma_nc
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%gas_atom)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%gas_atom).AND.ALLOCATED(ss2%gas_atom)) THEN
        CALL ps_diff("gas_atom",ss1%gas_atom,ss2%gas_atom,icheck,iout,ierr)
        CALL ps_conformal("gas_atom",ss1%gas_atom,ss3%gas_atom,iout,ierr)
        if(ierr.eq.0) ss3%gas_atom = ss1%gas_atom
     ELSE IF(ALLOCATED(ss1%gas_atom)) THEN
        CALL ps_conformal("gas_atom",ss1%gas_atom,ss3%gas_atom,iout,ierr)
        ss3%gas_atom = ss1%gas_atom
     ELSE IF(ALLOCATED(ss2%gas_atom)) THEN
        CALL ps_conformal("gas_atom",ss2%gas_atom,ss3%gas_atom,iout,ierr)
        ss3%gas_atom = ss2%gas_atom
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_GAS).EQ.1) THEN

     CALL ps_diff("GAS_Code_Info",ss1%GAS_Code_Info,ss2%GAS_Code_Info,icheck,iout,ierr)
     ss3%GAS_Code_Info = ss1%GAS_Code_Info
  ENDIF

  IF((ALLOCATED(ss3%gb1)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%gb1).AND.ALLOCATED(ss2%gb1)) THEN
        CALL ps_conformal("gb1",ss2%gb1,ss3%gb1,iout,ierr)
        CALL ps_conformal("gb1",ss1%gb1,ss3%gb1,iout,ierr)
        if(ierr.eq.0) ss3%gb1 = w1*ss1%gb1 + (1.0_rspec-w1)*ss2%gb1
     ELSE IF(ALLOCATED(ss1%gb1)) THEN
        CALL ps_conformal("gb1",ss1%gb1,ss3%gb1,iout,ierr)
        ss3%gb1 = ss1%gb1
     ELSE IF(ALLOCATED(ss2%gb1)) THEN
        CALL ps_conformal("gb1",ss2%gb1,ss3%gb1,iout,ierr)
        ss3%gb1 = ss2%gb1
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%gb2)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%gb2).AND.ALLOCATED(ss2%gb2)) THEN
        CALL ps_conformal("gb2",ss2%gb2,ss3%gb2,iout,ierr)
        CALL ps_conformal("gb2",ss1%gb2,ss3%gb2,iout,ierr)
        if(ierr.eq.0) ss3%gb2 = w1*ss1%gb2 + (1.0_rspec-w1)*ss2%gb2
     ELSE IF(ALLOCATED(ss1%gb2)) THEN
        CALL ps_conformal("gb2",ss1%gb2,ss3%gb2,iout,ierr)
        ss3%gb2 = ss1%gb2
     ELSE IF(ALLOCATED(ss2%gb2)) THEN
        CALL ps_conformal("gb2",ss2%gb2,ss3%gb2,iout,ierr)
        ss3%gb2 = ss2%gb2
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%gb2i)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%gb2i).AND.ALLOCATED(ss2%gb2i)) THEN
        CALL ps_conformal("gb2i",ss2%gb2i,ss3%gb2i,iout,ierr)
        CALL ps_conformal("gb2i",ss1%gb2i,ss3%gb2i,iout,ierr)
        if(ierr.eq.0) ss3%gb2i = w1*ss1%gb2i + (1.0_rspec-w1)*ss2%gb2i
     ELSE IF(ALLOCATED(ss1%gb2i)) THEN
        CALL ps_conformal("gb2i",ss1%gb2i,ss3%gb2i,iout,ierr)
        ss3%gb2i = ss1%gb2i
     ELSE IF(ALLOCATED(ss2%gb2i)) THEN
        CALL ps_conformal("gb2i",ss2%gb2i,ss3%gb2i,iout,ierr)
        ss3%gb2i = ss2%gb2i
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%gbr2)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%gbr2).AND.ALLOCATED(ss2%gbr2)) THEN
        CALL ps_conformal("gbr2",ss2%gbr2,ss3%gbr2,iout,ierr)
        CALL ps_conformal("gbr2",ss1%gbr2,ss3%gbr2,iout,ierr)
        if(ierr.eq.0) ss3%gbr2 = w1*ss1%gbr2 + (1.0_rspec-w1)*ss2%gbr2
     ELSE IF(ALLOCATED(ss1%gbr2)) THEN
        CALL ps_conformal("gbr2",ss1%gbr2,ss3%gbr2,iout,ierr)
        ss3%gbr2 = ss1%gbr2
     ELSE IF(ALLOCATED(ss2%gbr2)) THEN
        CALL ps_conformal("gbr2",ss2%gbr2,ss3%gbr2,iout,ierr)
        ss3%gbr2 = ss2%gbr2
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN

     CALL ps_diff("geometry",ss1%geometry,ss2%geometry,icheck,iout,ierr)
     ss3%geometry = ss1%geometry
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("Global_label",ss1%Global_label,ss2%Global_label,icheck,iout,ierr)
     ss3%Global_label = ss1%Global_label
  ENDIF

  IF((ALLOCATED(ss3%gncfb2h)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%gncfb2h).AND.ALLOCATED(ss2%gncfb2h)) THEN
        CALL ps_conformal("gncfb2h",ss2%gncfb2h,ss3%gncfb2h,iout,ierr)
        CALL ps_conformal("gncfb2h",ss1%gncfb2h,ss3%gncfb2h,iout,ierr)
        if(ierr.eq.0) ss3%gncfb2h = w1*ss1%gncfb2h + (1.0_rspec-w1)*ss2%gncfb2h
     ELSE IF(ALLOCATED(ss1%gncfb2h)) THEN
        CALL ps_conformal("gncfb2h",ss1%gncfb2h,ss3%gncfb2h,iout,ierr)
        ss3%gncfb2h = ss1%gncfb2h
     ELSE IF(ALLOCATED(ss2%gncfb2h)) THEN
        CALL ps_conformal("gncfb2h",ss2%gncfb2h,ss3%gncfb2h,iout,ierr)
        ss3%gncfb2h = ss2%gncfb2h
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%gncfh)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%gncfh).AND.ALLOCATED(ss2%gncfh)) THEN
        CALL ps_conformal("gncfh",ss2%gncfh,ss3%gncfh,iout,ierr)
        CALL ps_conformal("gncfh",ss1%gncfh,ss3%gncfh,iout,ierr)
        if(ierr.eq.0) ss3%gncfh = w1*ss1%gncfh + (1.0_rspec-w1)*ss2%gncfh
     ELSE IF(ALLOCATED(ss1%gncfh)) THEN
        CALL ps_conformal("gncfh",ss1%gncfh,ss3%gncfh,iout,ierr)
        ss3%gncfh = ss1%gncfh
     ELSE IF(ALLOCATED(ss2%gncfh)) THEN
        CALL ps_conformal("gncfh",ss2%gncfh,ss3%gncfh,iout,ierr)
        ss3%gncfh = ss2%gncfh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%gr1)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%gr1).AND.ALLOCATED(ss2%gr1)) THEN
        CALL ps_conformal("gr1",ss2%gr1,ss3%gr1,iout,ierr)
        CALL ps_conformal("gr1",ss1%gr1,ss3%gr1,iout,ierr)
        if(ierr.eq.0) ss3%gr1 = w1*ss1%gr1 + (1.0_rspec-w1)*ss2%gr1
     ELSE IF(ALLOCATED(ss1%gr1)) THEN
        CALL ps_conformal("gr1",ss1%gr1,ss3%gr1,iout,ierr)
        ss3%gr1 = ss1%gr1
     ELSE IF(ALLOCATED(ss2%gr1)) THEN
        CALL ps_conformal("gr1",ss2%gr1,ss3%gr1,iout,ierr)
        ss3%gr1 = ss2%gr1
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%gr2)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%gr2).AND.ALLOCATED(ss2%gr2)) THEN
        CALL ps_conformal("gr2",ss2%gr2,ss3%gr2,iout,ierr)
        CALL ps_conformal("gr2",ss1%gr2,ss3%gr2,iout,ierr)
        if(ierr.eq.0) ss3%gr2 = w1*ss1%gr2 + (1.0_rspec-w1)*ss2%gr2
     ELSE IF(ALLOCATED(ss1%gr2)) THEN
        CALL ps_conformal("gr2",ss1%gr2,ss3%gr2,iout,ierr)
        ss3%gr2 = ss1%gr2
     ELSE IF(ALLOCATED(ss2%gr2)) THEN
        CALL ps_conformal("gr2",ss2%gr2,ss3%gr2,iout,ierr)
        ss3%gr2 = ss2%gr2
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%gr2i)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%gr2i).AND.ALLOCATED(ss2%gr2i)) THEN
        CALL ps_conformal("gr2i",ss2%gr2i,ss3%gr2i,iout,ierr)
        CALL ps_conformal("gr2i",ss1%gr2i,ss3%gr2i,iout,ierr)
        if(ierr.eq.0) ss3%gr2i = w1*ss1%gr2i + (1.0_rspec-w1)*ss2%gr2i
     ELSE IF(ALLOCATED(ss1%gr2i)) THEN
        CALL ps_conformal("gr2i",ss1%gr2i,ss3%gr2i,iout,ierr)
        ss3%gr2i = ss1%gr2i
     ELSE IF(ALLOCATED(ss2%gr2i)) THEN
        CALL ps_conformal("gr2i",ss2%gr2i,ss3%gr2i,iout,ierr)
        ss3%gr2i = ss2%gr2i
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%gr2rho2)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%gr2rho2).AND.ALLOCATED(ss2%gr2rho2)) THEN
        CALL ps_conformal("gr2rho2",ss2%gr2rho2,ss3%gr2rho2,iout,ierr)
        CALL ps_conformal("gr2rho2",ss1%gr2rho2,ss3%gr2rho2,iout,ierr)
        if(ierr.eq.0) ss3%gr2rho2 = w1*ss1%gr2rho2 + (1.0_rspec-w1)*ss2%gr2rho2
     ELSE IF(ALLOCATED(ss1%gr2rho2)) THEN
        CALL ps_conformal("gr2rho2",ss1%gr2rho2,ss3%gr2rho2,iout,ierr)
        ss3%gr2rho2 = ss1%gr2rho2
     ELSE IF(ALLOCATED(ss2%gr2rho2)) THEN
        CALL ps_conformal("gr2rho2",ss2%gr2rho2,ss3%gr2rho2,iout,ierr)
        ss3%gr2rho2 = ss2%gr2rho2
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%gr3i)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%gr3i).AND.ALLOCATED(ss2%gr3i)) THEN
        CALL ps_conformal("gr3i",ss2%gr3i,ss3%gr3i,iout,ierr)
        CALL ps_conformal("gr3i",ss1%gr3i,ss3%gr3i,iout,ierr)
        if(ierr.eq.0) ss3%gr3i = w1*ss1%gr3i + (1.0_rspec-w1)*ss2%gr3i
     ELSE IF(ALLOCATED(ss1%gr3i)) THEN
        CALL ps_conformal("gr3i",ss1%gr3i,ss3%gr3i,iout,ierr)
        ss3%gr3i = ss1%gr3i
     ELSE IF(ALLOCATED(ss2%gr3i)) THEN
        CALL ps_conformal("gr3i",ss2%gr3i,ss3%gr3i,iout,ierr)
        ss3%gr3i = ss2%gr3i
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%grho1)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%grho1).AND.ALLOCATED(ss2%grho1)) THEN
        CALL ps_conformal("grho1",ss2%grho1,ss3%grho1,iout,ierr)
        CALL ps_conformal("grho1",ss1%grho1,ss3%grho1,iout,ierr)
        if(ierr.eq.0) ss3%grho1 = w1*ss1%grho1 + (1.0_rspec-w1)*ss2%grho1
     ELSE IF(ALLOCATED(ss1%grho1)) THEN
        CALL ps_conformal("grho1",ss1%grho1,ss3%grho1,iout,ierr)
        ss3%grho1 = ss1%grho1
     ELSE IF(ALLOCATED(ss2%grho1)) THEN
        CALL ps_conformal("grho1",ss2%grho1,ss3%grho1,iout,ierr)
        ss3%grho1 = ss2%grho1
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_merge_2
