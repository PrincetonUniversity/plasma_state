! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_copy_6(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)


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
  IF(partial_copy) THEN
     ss2%id_prad_li = 0
  ENDIF
  IF((ALLOCATED(ss%prad_li).AND.ALLOCATED(ss2%prad_li)).AND. &
        ((cclist(ps_cnum_RAD).EQ.1))) THEN
     ss2%prad_li = ss%prad_li
     IF(.not.partial_copy) THEN
        ss2%id_prad_li = ss%id_prad_li
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_psc_halo = 0
  ENDIF
  IF((ALLOCATED(ss%psc_halo).AND.ALLOCATED(ss2%psc_halo)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%psc_halo = ss%psc_halo
     IF(.not.partial_copy) THEN
        ss2%id_psc_halo = ss%id_psc_halo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_psc_reco = 0
  ENDIF
  IF((ALLOCATED(ss%psc_reco).AND.ALLOCATED(ss2%psc_reco)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%psc_reco = ss%psc_reco
     IF(.not.partial_copy) THEN
        ss2%id_psc_reco = ss%id_psc_reco
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_psipol = 0
  ENDIF
  IF((ALLOCATED(ss%psipol).AND.ALLOCATED(ss2%psipol)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%psipol = ss%psipol
     IF(.not.partial_copy) THEN
        ss2%id_psipol = ss%id_psipol
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_PsiRZ = 0
  ENDIF
  IF((ALLOCATED(ss%PsiRZ).AND.ALLOCATED(ss2%PsiRZ)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%PsiRZ = ss%PsiRZ
     IF(.not.partial_copy) THEN
        ss2%id_PsiRZ = ss%id_PsiRZ
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%Psi_to_machine_axis = ss%Psi_to_machine_axis
  ENDIF
  IF(partial_copy) THEN
     ss2%id_psmom_errck = 0
  ENDIF
  IF((ALLOCATED(ss%psmom_errck).AND.ALLOCATED(ss2%psmom_errck)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%psmom_errck = ss%psmom_errck
     IF(.not.partial_copy) THEN
        ss2%id_psmom_errck = ss%id_psmom_errck
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_psmom_nc)) THEN
     IF(partial_copy) THEN
        ss2%id_psmom_nc = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%psmom_nc).AND.ALLOCATED(ss2%psmom_nc)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%psmom_nc = ss%psmom_nc
     IF(ALLOCATED(ss2%id_psmom_nc)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_psmom_nc = ss%id_psmom_nc
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%psmom_num)).AND.(ALLOCATED(ss%psmom_num))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%psmom_num = ss%psmom_num
  ENDIF
  IF(partial_copy) THEN
     ss2%id_P_eq = 0
  ENDIF
  IF((ALLOCATED(ss%P_eq).AND.ALLOCATED(ss2%P_eq)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%P_eq = ss%P_eq
     IF(.not.partial_copy) THEN
        ss2%id_P_eq = ss%id_P_eq
     ENDIF
  ENDIF
  IF((ALLOCATED(ss2%qatom_ALL)).AND.(ALLOCATED(ss%qatom_ALL))) THEN
     ss2%qatom_ALL = ss%qatom_ALL
  ENDIF
  IF((ALLOCATED(ss2%qatom_ALLA)).AND.(ALLOCATED(ss%qatom_ALLA))) THEN
     ss2%qatom_ALLA = ss%qatom_ALLA
  ENDIF
  IF((ALLOCATED(ss2%qatom_RFMIN)).AND.(ALLOCATED(ss%qatom_RFMIN))) THEN
     ss2%qatom_RFMIN = ss%qatom_RFMIN
  ENDIF
  IF((ALLOCATED(ss2%qatom_S)).AND.(ALLOCATED(ss%qatom_S))) THEN
     ss2%qatom_S = ss%qatom_S
  ENDIF
  IF((ALLOCATED(ss2%qatom_SA)).AND.(ALLOCATED(ss%qatom_SA))) THEN
     ss2%qatom_SA = ss%qatom_SA
  ENDIF
  IF((ALLOCATED(ss2%qatom_SFUS)).AND.(ALLOCATED(ss%qatom_SFUS))) THEN
     ss2%qatom_SFUS = ss%qatom_SFUS
  ENDIF
  IF((ALLOCATED(ss2%qatom_SGAS)).AND.(ALLOCATED(ss%qatom_SGAS))) THEN
     ss2%qatom_SGAS = ss%qatom_SGAS
  ENDIF
  IF((ALLOCATED(ss2%qatom_SIMP0)).AND.(ALLOCATED(ss%qatom_SIMP0))) THEN
     ss2%qatom_SIMP0 = ss%qatom_SIMP0
  ENDIF
  IF((ALLOCATED(ss2%qatom_SIMPI)).AND.(ALLOCATED(ss%qatom_SIMPI))) THEN
     ss2%qatom_SIMPI = ss%qatom_SIMPI
  ENDIF
  IF((ALLOCATED(ss2%qatom_SNBI)).AND.(ALLOCATED(ss%qatom_SNBI))) THEN
     ss2%qatom_SNBI = ss%qatom_SNBI
  ENDIF
  IF(ALLOCATED(ss2%id_qbeami)) THEN
     IF(partial_copy) THEN
        ss2%id_qbeami = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%qbeami).AND.ALLOCATED(ss2%qbeami)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%qbeami = ss%qbeami
     IF(ALLOCATED(ss2%id_qbeami)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_qbeami = ss%id_qbeami
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_qcomp_e = 0
  ENDIF
  IF((ALLOCATED(ss%qcomp_e).AND.ALLOCATED(ss2%qcomp_e)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%qcomp_e = ss%qcomp_e
     IF(.not.partial_copy) THEN
        ss2%id_qcomp_e = ss%id_qcomp_e
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_qcomp_i = 0
  ENDIF
  IF((ALLOCATED(ss%qcomp_i).AND.ALLOCATED(ss2%qcomp_i)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%qcomp_i = ss%qcomp_i
     IF(.not.partial_copy) THEN
        ss2%id_qcomp_i = ss%id_qcomp_i
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_qie = 0
  ENDIF
  IF((ALLOCATED(ss%qie).AND.ALLOCATED(ss2%qie)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%qie = ss%qie
     IF(.not.partial_copy) THEN
        ss2%id_qie = ss%id_qie
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_qioniz)) THEN
     IF(partial_copy) THEN
        ss2%id_qioniz = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%qioniz).AND.ALLOCATED(ss2%qioniz)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%qioniz = ss%qioniz
     IF(ALLOCATED(ss2%id_qioniz)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_qioniz = ss%id_qioniz
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%ql_operator)).AND.(ALLOCATED(ss%ql_operator))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%ql_operator = ss%ql_operator
  ENDIF
  IF(ALLOCATED(ss2%id_qqcx)) THEN
     IF(partial_copy) THEN
        ss2%id_qqcx = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%qqcx).AND.ALLOCATED(ss2%qqcx)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%qqcx = ss%qqcx
     IF(ALLOCATED(ss2%id_qqcx)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_qqcx = ss%id_qqcx
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_qrot_conv = 0
  ENDIF
  IF((ALLOCATED(ss%qrot_conv).AND.ALLOCATED(ss2%qrot_conv)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%qrot_conv = ss%qrot_conv
     IF(.not.partial_copy) THEN
        ss2%id_qrot_conv = ss%id_qrot_conv
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_qrot_diff = 0
  ENDIF
  IF((ALLOCATED(ss%qrot_diff).AND.ALLOCATED(ss2%qrot_diff)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%qrot_diff = ss%qrot_diff
     IF(.not.partial_copy) THEN
        ss2%id_qrot_diff = ss%id_qrot_diff
     ENDIF
  ENDIF
  IF((ALLOCATED(ss2%q_ALL)).AND.(ALLOCATED(ss%q_ALL))) THEN
     ss2%q_ALL = ss%q_ALL
  ENDIF
  IF((ALLOCATED(ss2%q_ALLA)).AND.(ALLOCATED(ss%q_ALLA))) THEN
     ss2%q_ALLA = ss%q_ALLA
  ENDIF
  IF(partial_copy) THEN
     ss2%id_q_eq = 0
  ENDIF
  IF((ALLOCATED(ss%q_eq).AND.ALLOCATED(ss2%q_eq)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%q_eq = ss%q_eq
     IF(.not.partial_copy) THEN
        ss2%id_q_eq = ss%id_q_eq
     ENDIF
  ENDIF
  IF((ALLOCATED(ss2%q_RFMIN)).AND.(ALLOCATED(ss%q_RFMIN))) THEN
     ss2%q_RFMIN = ss%q_RFMIN
  ENDIF
  IF((ALLOCATED(ss2%q_S)).AND.(ALLOCATED(ss%q_S))) THEN
     ss2%q_S = ss%q_S
  ENDIF
  IF((ALLOCATED(ss2%q_SA)).AND.(ALLOCATED(ss%q_SA))) THEN
     ss2%q_SA = ss%q_SA
  ENDIF
  IF((ALLOCATED(ss2%q_SFUS)).AND.(ALLOCATED(ss%q_SFUS))) THEN
     ss2%q_SFUS = ss%q_SFUS
  ENDIF
  IF((ALLOCATED(ss2%q_SGAS)).AND.(ALLOCATED(ss%q_SGAS))) THEN
     ss2%q_SGAS = ss%q_SGAS
  ENDIF
  IF((ALLOCATED(ss2%q_SIMP0)).AND.(ALLOCATED(ss%q_SIMP0))) THEN
     ss2%q_SIMP0 = ss%q_SIMP0
  ENDIF
  IF((ALLOCATED(ss2%q_SIMPI)).AND.(ALLOCATED(ss%q_SIMPI))) THEN
     ss2%q_SIMPI = ss%q_SIMPI
  ENDIF
  IF((ALLOCATED(ss2%q_SNBI)).AND.(ALLOCATED(ss%q_SNBI))) THEN
     ss2%q_SNBI = ss%q_SNBI
  ENDIF
  IF(partial_copy) THEN
     ss2%id_R0_momeq = 0
  ENDIF
  IF((ALLOCATED(ss%R0_momeq).AND.ALLOCATED(ss2%R0_momeq)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%R0_momeq = ss%R0_momeq
     IF(.not.partial_copy) THEN
        ss2%id_R0_momeq = ss%id_R0_momeq
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_RAD).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%RAD_Code_Info = ss%RAD_Code_Info
  ENDIF
  IF((cclist(ps_cnum_RAD).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%RAD_Data_Info = ss%RAD_Data_Info
  ENDIF
  IF(ALLOCATED(ss2%id_rate_sinb0i)) THEN
     IF(partial_copy) THEN
        ss2%id_rate_sinb0i = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%rate_sinb0i).AND.ALLOCATED(ss2%rate_sinb0i)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%rate_sinb0i = ss%rate_sinb0i
     IF(ALLOCATED(ss2%id_rate_sinb0i)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_rate_sinb0i = ss%id_rate_sinb0i
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_rate_sinb0x)) THEN
     IF(partial_copy) THEN
        ss2%id_rate_sinb0x = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%rate_sinb0x).AND.ALLOCATED(ss2%rate_sinb0x)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%rate_sinb0x = ss%rate_sinb0x
     IF(ALLOCATED(ss2%id_rate_sinb0x)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_rate_sinb0x = ss%id_rate_sinb0x
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_rate_sinb0xs)) THEN
     IF(partial_copy) THEN
        ss2%id_rate_sinb0xs = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%rate_sinb0xs).AND.ALLOCATED(ss2%rate_sinb0xs)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%rate_sinb0xs = ss%rate_sinb0xs
     IF(ALLOCATED(ss2%id_rate_sinb0xs)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_rate_sinb0xs = ss%id_rate_sinb0xs
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_rate_sinf0i)) THEN
     IF(partial_copy) THEN
        ss2%id_rate_sinf0i = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%rate_sinf0i).AND.ALLOCATED(ss2%rate_sinf0i)).AND. &
        ((cclist(ps_cnum_FUS).EQ.1))) THEN
     ss2%rate_sinf0i = ss%rate_sinf0i
     IF(ALLOCATED(ss2%id_rate_sinf0i)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_rate_sinf0i = ss%id_rate_sinf0i
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_rate_sinf0x)) THEN
     IF(partial_copy) THEN
        ss2%id_rate_sinf0x = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%rate_sinf0x).AND.ALLOCATED(ss2%rate_sinf0x)).AND. &
        ((cclist(ps_cnum_FUS).EQ.1))) THEN
     ss2%rate_sinf0x = ss%rate_sinf0x
     IF(ALLOCATED(ss2%id_rate_sinf0x)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_rate_sinf0x = ss%id_rate_sinf0x
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_rate_sinf0xs)) THEN
     IF(partial_copy) THEN
        ss2%id_rate_sinf0xs = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%rate_sinf0xs).AND.ALLOCATED(ss2%rate_sinf0xs)).AND. &
        ((cclist(ps_cnum_FUS).EQ.1))) THEN
     ss2%rate_sinf0xs = ss%rate_sinf0xs
     IF(ALLOCATED(ss2%id_rate_sinf0xs)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_rate_sinf0xs = ss%id_rate_sinf0xs
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%Real_ant_coef)).AND.(ALLOCATED(ss%Real_ant_coef))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%Real_ant_coef = ss%Real_ant_coef
  ENDIF
  IF(ALLOCATED(ss2%id_res_sn)) THEN
     IF(partial_copy) THEN
        ss2%id_res_sn = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%res_sn).AND.ALLOCATED(ss2%res_sn)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%res_sn = ss%res_sn
     IF(ALLOCATED(ss2%id_res_sn)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_res_sn = ss%id_res_sn
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_res_te = 0
  ENDIF
  IF((ALLOCATED(ss%res_te).AND.ALLOCATED(ss2%res_te)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%res_te = ss%res_te
     IF(.not.partial_copy) THEN
        ss2%id_res_te = ss%id_res_te
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_res_ti = 0
  ENDIF
  IF((ALLOCATED(ss%res_ti).AND.ALLOCATED(ss2%res_ti)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%res_ti = ss%res_ti
     IF(.not.partial_copy) THEN
        ss2%id_res_ti = ss%id_res_ti
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_res_tq = 0
  ENDIF
  IF((ALLOCATED(ss%res_tq).AND.ALLOCATED(ss2%res_tq)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%res_tq = ss%res_tq
     IF(.not.partial_copy) THEN
        ss2%id_res_tq = ss%id_res_tq
     ENDIF
  ENDIF
  IF((ALLOCATED(ss2%RFMIN_name)).AND.(ALLOCATED(ss%RFMIN_name))) THEN
     ss2%RFMIN_name = ss%RFMIN_name
  ENDIF
  IF((ALLOCATED(ss2%rfmin_to_all)).AND.(ALLOCATED(ss%rfmin_to_all))) THEN
     ss2%rfmin_to_all = ss%rfmin_to_all
  ENDIF
  IF((ALLOCATED(ss2%rfmin_to_alla)).AND.(ALLOCATED(ss%rfmin_to_alla))) THEN
     ss2%rfmin_to_alla = ss%rfmin_to_alla
  ENDIF
  IF((ALLOCATED(ss2%RFMIN_type)).AND.(ALLOCATED(ss%RFMIN_type))) THEN
     ss2%RFMIN_type = ss%RFMIN_type
  ENDIF
  IF((ALLOCATED(ss%rho).AND.ALLOCATED(ss2%rho)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%rho = ss%rho
  ENDIF
  IF( ((ALLOCATED(ss2%rhopsi0lh)).AND.(ALLOCATED(ss%rhopsi0lh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%rhopsi0lh = ss%rhopsi0lh
  ENDIF
  IF((ALLOCATED(ss%rho_anom).AND.ALLOCATED(ss2%rho_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%rho_anom = ss%rho_anom
  ENDIF
  IF((ALLOCATED(ss%rho_anom2).AND.ALLOCATED(ss2%rho_anom2)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%rho_anom2 = ss%rho_anom2
  ENDIF
  IF((ALLOCATED(ss%rho_anomth).AND.ALLOCATED(ss2%rho_anomth)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%rho_anomth = ss%rho_anomth
  ENDIF
  IF( ((ALLOCATED(ss2%rho_bdy_ns)).AND.(ALLOCATED(ss%rho_bdy_ns))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_PLASMA).EQ.1)) ) THEN
     ss2%rho_bdy_ns = ss%rho_bdy_ns
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%rho_bdy_omegat = ss%rho_bdy_omegat
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%rho_bdy_Te = ss%rho_bdy_Te
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%rho_bdy_Ti = ss%rho_bdy_Ti
  ENDIF
  IF((ALLOCATED(ss%rho_ecrf).AND.ALLOCATED(ss2%rho_ecrf)).AND. &
        ((cclist(ps_cnum_EC).EQ.1))) THEN
     ss2%rho_ecrf = ss%rho_ecrf
  ENDIF
  IF((ALLOCATED(ss%rho_eq).AND.ALLOCATED(ss2%rho_eq)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%rho_eq = ss%rho_eq
  ENDIF
  IF((ALLOCATED(ss%rho_eq_geo).AND.ALLOCATED(ss2%rho_eq_geo)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%rho_eq_geo = ss%rho_eq_geo
  ENDIF
  IF((ALLOCATED(ss%rho_fus).AND.ALLOCATED(ss2%rho_fus)).AND. &
        ((cclist(ps_cnum_FUS).EQ.1))) THEN
     ss2%rho_fus = ss%rho_fus
  ENDIF
  IF((ALLOCATED(ss%rho_gas).AND.ALLOCATED(ss2%rho_gas)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%rho_gas = ss%rho_gas
  ENDIF
  IF((ALLOCATED(ss%rho_icrf).AND.ALLOCATED(ss2%rho_icrf)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%rho_icrf = ss%rho_icrf
  ENDIF
  IF((ALLOCATED(ss%rho_lhrf).AND.ALLOCATED(ss2%rho_lhrf)).AND. &
        ((cclist(ps_cnum_LH).EQ.1))) THEN
     ss2%rho_lhrf = ss%rho_lhrf
  ENDIF
  IF((ALLOCATED(ss%rho_lmhd).AND.ALLOCATED(ss2%rho_lmhd)).AND. &
        ((cclist(ps_cnum_LMHD).EQ.1))) THEN
     ss2%rho_lmhd = ss%rho_lmhd
  ENDIF
  IF((ALLOCATED(ss%rho_mist).AND.ALLOCATED(ss2%rho_mist)).AND. &
        ((cclist(ps_cnum_RAD).EQ.1))) THEN
     ss2%rho_mist = ss%rho_mist
  ENDIF
  IF((ALLOCATED(ss%rho_nbi).AND.ALLOCATED(ss2%rho_nbi)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%rho_nbi = ss%rho_nbi
  ENDIF

END SUBROUTINE plasma_state_copy_6
