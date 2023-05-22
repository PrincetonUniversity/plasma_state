! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_copy_2(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)


  ! Copy scalars, and, arrays that are allocated
  !   ss2 <-- ss

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components to merge
                                ! use ps_cc_all to merge all components

  TYPE (plasma_state) :: ss     ! state data to copy *from*
  TYPE (plasma_state) :: ss2    ! state data to copy *to*

  INTEGER, INTENT(IN) :: icopy_lists ! =1 to require copying of all lists

  INTEGER, INTENT(IN) :: iwant_1deq  ! =1 to request fcns of (rho_eq)
                                     !  even if cclist(ps_cnum_EQ).eq.0

  LOGICAL, INTENT(IN) :: partial_copy ! .TRUE. if full state copy not in effect

  INTEGER, INTENT(INOUT) :: ierr  ! completion flag (not cleared, set by error)

  !-------------------
  INTEGER :: iout
  !-------------------

  iout = SWIM_err_out_file


  !-----------------------------------------------
  ! copy scalars and arrays that are allocated...
  IF( ((ALLOCATED(ss2%D_einj_standard)).AND.(ALLOCATED(ss%D_einj_standard))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%D_einj_standard = ss%D_einj_standard
  ENDIF
  IF( ((ALLOCATED(ss2%D_ffull_standard)).AND.(ALLOCATED(ss%D_ffull_standard))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%D_ffull_standard = ss%D_ffull_standard
  ENDIF
  IF( ((ALLOCATED(ss2%D_fhalf_standard)).AND.(ALLOCATED(ss%D_fhalf_standard))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%D_fhalf_standard = ss%D_fhalf_standard
  ENDIF
  IF( ((ALLOCATED(ss2%e0_av)).AND.(ALLOCATED(ss%e0_av))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_GAS).EQ.1)) ) THEN
     ss2%e0_av = ss%e0_av
  ENDIF
  IF( ((ALLOCATED(ss2%ecrf_src_name)).AND.(ALLOCATED(ss%ecrf_src_name))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%ecrf_src_name = ss%ecrf_src_name
  ENDIF
  IF( ((ALLOCATED(ss2%EC_Beam_Elongation)).AND.(ALLOCATED(ss%EC_Beam_Elongation))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%EC_Beam_Elongation = ss%EC_Beam_Elongation
  ENDIF
  IF((cclist(ps_cnum_EC).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%EC_Code_Info = ss%EC_Code_Info
  ENDIF
  IF((cclist(ps_cnum_EC).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%EC_Data_Info = ss%EC_Data_Info
  ENDIF
  IF( ((ALLOCATED(ss2%EC_Half_Power_Angle)).AND.(ALLOCATED(ss%EC_Half_Power_Angle))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%EC_Half_Power_Angle = ss%EC_Half_Power_Angle
  ENDIF
  IF( ((ALLOCATED(ss2%EC_Omode_Fraction)).AND.(ALLOCATED(ss%EC_Omode_Fraction))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%EC_Omode_Fraction = ss%EC_Omode_Fraction
  ENDIF
  IF( ((ALLOCATED(ss2%EC_phi_aim)).AND.(ALLOCATED(ss%EC_phi_aim))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%EC_phi_aim = ss%EC_phi_aim
  ENDIF
  IF( ((ALLOCATED(ss2%EC_PRINC_CURV_HOR)).AND.(ALLOCATED(ss%EC_PRINC_CURV_HOR))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%EC_PRINC_CURV_HOR = ss%EC_PRINC_CURV_HOR
  ENDIF
  IF( ((ALLOCATED(ss2%EC_PRINC_CURV_VERT)).AND.(ALLOCATED(ss%EC_PRINC_CURV_VERT))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%EC_PRINC_CURV_VERT = ss%EC_PRINC_CURV_VERT
  ENDIF
  IF( ((ALLOCATED(ss2%EC_PRINC_WIDTH_HOR)).AND.(ALLOCATED(ss%EC_PRINC_WIDTH_HOR))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%EC_PRINC_WIDTH_HOR = ss%EC_PRINC_WIDTH_HOR
  ENDIF
  IF( ((ALLOCATED(ss2%EC_PRINC_WIDTH_VERT)).AND.(ALLOCATED(ss%EC_PRINC_WIDTH_VERT))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%EC_PRINC_WIDTH_VERT = ss%EC_PRINC_WIDTH_VERT
  ENDIF
  IF( ((ALLOCATED(ss2%EC_theta_aim)).AND.(ALLOCATED(ss%EC_theta_aim))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%EC_theta_aim = ss%EC_theta_aim
  ENDIF
  IF(partial_copy) THEN
     ss2%id_ee_mobility_factor = 0
  ENDIF
  IF((ALLOCATED(ss%ee_mobility_factor).AND.ALLOCATED(ss2%ee_mobility_factor)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%ee_mobility_factor = ss%ee_mobility_factor
     IF(.not.partial_copy) THEN
        ss2%id_ee_mobility_factor = ss%id_ee_mobility_factor
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%Einj_max)).AND.(ALLOCATED(ss%Einj_max))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%Einj_max = ss%Einj_max
  ENDIF
  IF( ((ALLOCATED(ss2%Einj_min)).AND.(ALLOCATED(ss%Einj_min))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%Einj_min = ss%Einj_min
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%eldot = ss%eldot
  ENDIF
  IF(partial_copy) THEN
     ss2%id_elong = 0
  ENDIF
  IF((ALLOCATED(ss%elong).AND.ALLOCATED(ss2%elong)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%elong = ss%elong
     IF(.not.partial_copy) THEN
        ss2%id_elong = ss%id_elong
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_eperp_beami)) THEN
     IF(partial_copy) THEN
        ss2%id_eperp_beami = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%eperp_beami).AND.ALLOCATED(ss2%eperp_beami)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%eperp_beami = ss%eperp_beami
     IF(ALLOCATED(ss2%id_eperp_beami)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_eperp_beami = ss%id_eperp_beami
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_eperp_fusi)) THEN
     IF(partial_copy) THEN
        ss2%id_eperp_fusi = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%eperp_fusi).AND.ALLOCATED(ss2%eperp_fusi)).AND. &
        ((cclist(ps_cnum_FUS).EQ.1))) THEN
     ss2%eperp_fusi = ss%eperp_fusi
     IF(ALLOCATED(ss2%id_eperp_fusi)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_eperp_fusi = ss%id_eperp_fusi
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_eperp_mini)) THEN
     IF(partial_copy) THEN
        ss2%id_eperp_mini = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%eperp_mini).AND.ALLOCATED(ss2%eperp_mini)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%eperp_mini = ss%eperp_mini
     IF(ALLOCATED(ss2%id_eperp_mini)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_eperp_mini = ss%id_eperp_mini
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_eperp_rw = 0
  ENDIF
  IF((ALLOCATED(ss%eperp_rw).AND.ALLOCATED(ss2%eperp_rw)).AND. &
        ((cclist(ps_cnum_RUNAWAY).EQ.1))) THEN
     ss2%eperp_rw = ss%eperp_rw
     IF(.not.partial_copy) THEN
        ss2%id_eperp_rw = ss%id_eperp_rw
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_epll_beami)) THEN
     IF(partial_copy) THEN
        ss2%id_epll_beami = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%epll_beami).AND.ALLOCATED(ss2%epll_beami)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%epll_beami = ss%epll_beami
     IF(ALLOCATED(ss2%id_epll_beami)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_epll_beami = ss%id_epll_beami
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_epll_fusi)) THEN
     IF(partial_copy) THEN
        ss2%id_epll_fusi = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%epll_fusi).AND.ALLOCATED(ss2%epll_fusi)).AND. &
        ((cclist(ps_cnum_FUS).EQ.1))) THEN
     ss2%epll_fusi = ss%epll_fusi
     IF(ALLOCATED(ss2%id_epll_fusi)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_epll_fusi = ss%id_epll_fusi
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_epll_mini)) THEN
     IF(partial_copy) THEN
        ss2%id_epll_mini = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%epll_mini).AND.ALLOCATED(ss2%epll_mini)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%epll_mini = ss%epll_mini
     IF(ALLOCATED(ss2%id_epll_mini)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_epll_mini = ss%id_epll_mini
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_epll_rw = 0
  ENDIF
  IF((ALLOCATED(ss%epll_rw).AND.ALLOCATED(ss2%epll_rw)).AND. &
        ((cclist(ps_cnum_RUNAWAY).EQ.1))) THEN
     ss2%epll_rw = ss%epll_rw
     IF(.not.partial_copy) THEN
        ss2%id_epll_rw = ss%id_epll_rw
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_Epot = 0
  ENDIF
  IF((ALLOCATED(ss%Epot).AND.ALLOCATED(ss2%Epot)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%Epot = ss%Epot
     IF(.not.partial_copy) THEN
        ss2%id_Epot = ss%id_Epot
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%eqdsk_file = ss%eqdsk_file
  ENDIF
  IF( ((ALLOCATED(ss2%eqmom_num)).AND.(ALLOCATED(ss%eqmom_num))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%eqmom_num = ss%eqmom_num
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%EQ_Code_Info = ss%EQ_Code_Info
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%EQ_Data_Info = ss%EQ_Data_Info
  ENDIF
  IF(partial_copy) THEN
     ss2%id_eta_parallel = 0
  ENDIF
  IF((ALLOCATED(ss%eta_parallel).AND.ALLOCATED(ss2%eta_parallel)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%eta_parallel = ss%eta_parallel
     IF(.not.partial_copy) THEN
        ss2%id_eta_parallel = ss%id_eta_parallel
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%E_anom).AND.ALLOCATED(ss2%E_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%E_anom = ss%E_anom
  ENDIF
  IF((ALLOCATED(ss%E_anom2).AND.ALLOCATED(ss2%E_anom2)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%E_anom2 = ss%E_anom2
  ENDIF
  IF(ALLOCATED(ss2%id_fidiff_bpass_co)) THEN
     IF(partial_copy) THEN
        ss2%id_fidiff_bpass_co = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%fidiff_bpass_co).AND.ALLOCATED(ss2%fidiff_bpass_co)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%fidiff_bpass_co = ss%fidiff_bpass_co
     IF(ALLOCATED(ss2%id_fidiff_bpass_co)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_fidiff_bpass_co = ss%id_fidiff_bpass_co
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_fidiff_bpass_ctr)) THEN
     IF(partial_copy) THEN
        ss2%id_fidiff_bpass_ctr = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%fidiff_bpass_ctr).AND.ALLOCATED(ss2%fidiff_bpass_ctr)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%fidiff_bpass_ctr = ss%fidiff_bpass_ctr
     IF(ALLOCATED(ss2%id_fidiff_bpass_ctr)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_fidiff_bpass_ctr = ss%id_fidiff_bpass_ctr
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_fidiff_btrap)) THEN
     IF(partial_copy) THEN
        ss2%id_fidiff_btrap = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%fidiff_btrap).AND.ALLOCATED(ss2%fidiff_btrap)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%fidiff_btrap = ss%fidiff_btrap
     IF(ALLOCATED(ss2%id_fidiff_btrap)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_fidiff_btrap = ss%id_fidiff_btrap
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_fidiff_dpass_co)) THEN
     IF(partial_copy) THEN
        ss2%id_fidiff_dpass_co = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%fidiff_dpass_co).AND.ALLOCATED(ss2%fidiff_dpass_co)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%fidiff_dpass_co = ss%fidiff_dpass_co
     IF(ALLOCATED(ss2%id_fidiff_dpass_co)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_fidiff_dpass_co = ss%id_fidiff_dpass_co
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_fidiff_dpass_ctr)) THEN
     IF(partial_copy) THEN
        ss2%id_fidiff_dpass_ctr = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%fidiff_dpass_ctr).AND.ALLOCATED(ss2%fidiff_dpass_ctr)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%fidiff_dpass_ctr = ss%fidiff_dpass_ctr
     IF(ALLOCATED(ss2%id_fidiff_dpass_ctr)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_fidiff_dpass_ctr = ss%id_fidiff_dpass_ctr
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_fidiff_dtrap)) THEN
     IF(partial_copy) THEN
        ss2%id_fidiff_dtrap = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%fidiff_dtrap).AND.ALLOCATED(ss2%fidiff_dtrap)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%fidiff_dtrap = ss%fidiff_dtrap
     IF(ALLOCATED(ss2%id_fidiff_dtrap)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_fidiff_dtrap = ss%id_fidiff_dtrap
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_Fi_depletion = 0
  ENDIF
  IF((ALLOCATED(ss%Fi_depletion).AND.ALLOCATED(ss2%Fi_depletion)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%Fi_depletion = ss%Fi_depletion
     IF(.not.partial_copy) THEN
        ss2%id_Fi_depletion = ss%id_Fi_depletion
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%fracmin)).AND.(ALLOCATED(ss%fracmin))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%fracmin = ss%fracmin
  ENDIF
  IF( ((ALLOCATED(ss2%fracpslh)).AND.(ALLOCATED(ss%fracpslh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%fracpslh = ss%fracpslh
  ENDIF
  IF( ((ALLOCATED(ss2%frac_full)).AND.(ALLOCATED(ss%frac_full))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%frac_full = ss%frac_full
  ENDIF
  IF( ((ALLOCATED(ss2%frac_half)).AND.(ALLOCATED(ss%frac_half))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%frac_half = ss%frac_half
  ENDIF
  IF(partial_copy) THEN
     ss2%id_freq_drbm = 0
  ENDIF
  IF((ALLOCATED(ss%freq_drbm).AND.ALLOCATED(ss2%freq_drbm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%freq_drbm = ss%freq_drbm
     IF(.not.partial_copy) THEN
        ss2%id_freq_drbm = ss%id_freq_drbm
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%freq_ec)).AND.(ALLOCATED(ss%freq_ec))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%freq_ec = ss%freq_ec
  ENDIF
  IF(partial_copy) THEN
     ss2%id_freq_etg = 0
  ENDIF
  IF((ALLOCATED(ss%freq_etg).AND.ALLOCATED(ss2%freq_etg)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%freq_etg = ss%freq_etg
     IF(.not.partial_copy) THEN
        ss2%id_freq_etg = ss%id_freq_etg
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_freq_glf = 0
  ENDIF
  IF((ALLOCATED(ss%freq_glf).AND.ALLOCATED(ss2%freq_glf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%freq_glf = ss%freq_glf
     IF(.not.partial_copy) THEN
        ss2%id_freq_glf = ss%id_freq_glf
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%freq_ic)).AND.(ALLOCATED(ss%freq_ic))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%freq_ic = ss%freq_ic
  ENDIF
  IF( ((ALLOCATED(ss2%freq_lh)).AND.(ALLOCATED(ss%freq_lh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%freq_lh = ss%freq_lh
  ENDIF
  IF(partial_copy) THEN
     ss2%id_freq_max_tglf = 0
  ENDIF
  IF((ALLOCATED(ss%freq_max_tglf).AND.ALLOCATED(ss2%freq_max_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%freq_max_tglf = ss%freq_max_tglf
     IF(.not.partial_copy) THEN
        ss2%id_freq_max_tglf = ss%id_freq_max_tglf
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_freq_mmm71)) THEN
     IF(partial_copy) THEN
        ss2%id_freq_mmm71 = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%freq_mmm71).AND.ALLOCATED(ss2%freq_mmm71)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%freq_mmm71 = ss%freq_mmm71
     IF(ALLOCATED(ss2%id_freq_mmm71)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_freq_mmm71 = ss%id_freq_mmm71
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_freq_mtm = 0
  ENDIF
  IF((ALLOCATED(ss%freq_mtm).AND.ALLOCATED(ss2%freq_mtm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%freq_mtm = ss%freq_mtm
     IF(.not.partial_copy) THEN
        ss2%id_freq_mtm = ss%id_freq_mtm
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_freq_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_freq_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%freq_tglf).AND.ALLOCATED(ss2%freq_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%freq_tglf = ss%freq_tglf
     IF(ALLOCATED(ss2%id_freq_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_freq_tglf = ss%id_freq_tglf
        ENDIF
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_FUS).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%FUS_Code_Info = ss%FUS_Code_Info
  ENDIF
  IF( ((ALLOCATED(ss2%gamma_lmhd)).AND.(ALLOCATED(ss%gamma_lmhd))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%gamma_lmhd = ss%gamma_lmhd
  ENDIF
  IF(partial_copy) THEN
     ss2%id_gamma_nc = 0
  ENDIF
  IF((ALLOCATED(ss%gamma_nc).AND.ALLOCATED(ss2%gamma_nc)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%gamma_nc = ss%gamma_nc
     IF(.not.partial_copy) THEN
        ss2%id_gamma_nc = ss%id_gamma_nc
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%gas_atom)).AND.(ALLOCATED(ss%gas_atom))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_GAS).EQ.1)) ) THEN
     ss2%gas_atom = ss%gas_atom
  ENDIF
  IF((cclist(ps_cnum_GAS).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%GAS_Code_Info = ss%GAS_Code_Info
  ENDIF
  IF(partial_copy) THEN
     ss2%id_gb1 = 0
  ENDIF
  IF((ALLOCATED(ss%gb1).AND.ALLOCATED(ss2%gb1)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%gb1 = ss%gb1
     IF(.not.partial_copy) THEN
        ss2%id_gb1 = ss%id_gb1
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_gb2 = 0
  ENDIF
  IF((ALLOCATED(ss%gb2).AND.ALLOCATED(ss2%gb2)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%gb2 = ss%gb2
     IF(.not.partial_copy) THEN
        ss2%id_gb2 = ss%id_gb2
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_gb2i = 0
  ENDIF
  IF((ALLOCATED(ss%gb2i).AND.ALLOCATED(ss2%gb2i)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%gb2i = ss%gb2i
     IF(.not.partial_copy) THEN
        ss2%id_gb2i = ss%id_gb2i
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_gbr2 = 0
  ENDIF
  IF((ALLOCATED(ss%gbr2).AND.ALLOCATED(ss2%gbr2)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%gbr2 = ss%gbr2
     IF(.not.partial_copy) THEN
        ss2%id_gbr2 = ss%id_gbr2
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%geometry = ss%geometry
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%Global_label = ss%Global_label
  ENDIF
  IF(partial_copy) THEN
     ss2%id_gncfb2h = 0
  ENDIF
  IF((ALLOCATED(ss%gncfb2h).AND.ALLOCATED(ss2%gncfb2h)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%gncfb2h = ss%gncfb2h
     IF(.not.partial_copy) THEN
        ss2%id_gncfb2h = ss%id_gncfb2h
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_gncfh = 0
  ENDIF
  IF((ALLOCATED(ss%gncfh).AND.ALLOCATED(ss2%gncfh)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%gncfh = ss%gncfh
     IF(.not.partial_copy) THEN
        ss2%id_gncfh = ss%id_gncfh
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_gr1 = 0
  ENDIF
  IF((ALLOCATED(ss%gr1).AND.ALLOCATED(ss2%gr1)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%gr1 = ss%gr1
     IF(.not.partial_copy) THEN
        ss2%id_gr1 = ss%id_gr1
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_gr2 = 0
  ENDIF
  IF((ALLOCATED(ss%gr2).AND.ALLOCATED(ss2%gr2)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%gr2 = ss%gr2
     IF(.not.partial_copy) THEN
        ss2%id_gr2 = ss%id_gr2
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_gr2i = 0
  ENDIF
  IF((ALLOCATED(ss%gr2i).AND.ALLOCATED(ss2%gr2i)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%gr2i = ss%gr2i
     IF(.not.partial_copy) THEN
        ss2%id_gr2i = ss%id_gr2i
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_gr2rho2 = 0
  ENDIF
  IF((ALLOCATED(ss%gr2rho2).AND.ALLOCATED(ss2%gr2rho2)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%gr2rho2 = ss%gr2rho2
     IF(.not.partial_copy) THEN
        ss2%id_gr2rho2 = ss%id_gr2rho2
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_gr3i = 0
  ENDIF
  IF((ALLOCATED(ss%gr3i).AND.ALLOCATED(ss2%gr3i)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%gr3i = ss%gr3i
     IF(.not.partial_copy) THEN
        ss2%id_gr3i = ss%id_gr3i
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_grho1 = 0
  ENDIF
  IF((ALLOCATED(ss%grho1).AND.ALLOCATED(ss2%grho1)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%grho1 = ss%grho1
     IF(.not.partial_copy) THEN
        ss2%id_grho1 = ss%id_grho1
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_copy_2
