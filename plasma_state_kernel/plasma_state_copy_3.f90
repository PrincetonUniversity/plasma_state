! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_copy_3(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)


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
     ss2%id_grho2 = 0
  ENDIF
  IF((ALLOCATED(ss%grho2).AND.ALLOCATED(ss2%grho2)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%grho2 = ss%grho2
     IF(.not.partial_copy) THEN
        ss2%id_grho2 = ss%id_grho2
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_grho2b2i = 0
  ENDIF
  IF((ALLOCATED(ss%grho2b2i).AND.ALLOCATED(ss2%grho2b2i)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%grho2b2i = ss%grho2b2i
     IF(.not.partial_copy) THEN
        ss2%id_grho2b2i = ss%id_grho2b2i
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_grho2r2i = 0
  ENDIF
  IF((ALLOCATED(ss%grho2r2i).AND.ALLOCATED(ss2%grho2r2i)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%grho2r2i = ss%grho2r2i
     IF(.not.partial_copy) THEN
        ss2%id_grho2r2i = ss%id_grho2r2i
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_grho2r3i = 0
  ENDIF
  IF((ALLOCATED(ss%grho2r3i).AND.ALLOCATED(ss2%grho2r3i)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%grho2r3i = ss%grho2r3i
     IF(.not.partial_copy) THEN
        ss2%id_grho2r3i = ss%id_grho2r3i
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_gri = 0
  ENDIF
  IF((ALLOCATED(ss%gri).AND.ALLOCATED(ss2%gri)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%gri = ss%gri
     IF(.not.partial_copy) THEN
        ss2%id_gri = ss%id_gri
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_grirhoi = 0
  ENDIF
  IF((ALLOCATED(ss%grirhoi).AND.ALLOCATED(ss2%grirhoi)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%grirhoi = ss%grirhoi
     IF(.not.partial_copy) THEN
        ss2%id_grirhoi = ss%id_grirhoi
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_growthrate_drbm = 0
  ENDIF
  IF((ALLOCATED(ss%growthrate_drbm).AND.ALLOCATED(ss2%growthrate_drbm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%growthrate_drbm = ss%growthrate_drbm
     IF(.not.partial_copy) THEN
        ss2%id_growthrate_drbm = ss%id_growthrate_drbm
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_growthrate_etg = 0
  ENDIF
  IF((ALLOCATED(ss%growthrate_etg).AND.ALLOCATED(ss2%growthrate_etg)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%growthrate_etg = ss%growthrate_etg
     IF(.not.partial_copy) THEN
        ss2%id_growthrate_etg = ss%id_growthrate_etg
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_growthrate_glf = 0
  ENDIF
  IF((ALLOCATED(ss%growthrate_glf).AND.ALLOCATED(ss2%growthrate_glf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%growthrate_glf = ss%growthrate_glf
     IF(.not.partial_copy) THEN
        ss2%id_growthrate_glf = ss%id_growthrate_glf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_growthrate_max_tglf = 0
  ENDIF
  IF((ALLOCATED(ss%growthrate_max_tglf).AND.ALLOCATED(ss2%growthrate_max_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%growthrate_max_tglf = ss%growthrate_max_tglf
     IF(.not.partial_copy) THEN
        ss2%id_growthrate_max_tglf = ss%id_growthrate_max_tglf
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_growthrate_mmm71)) THEN
     IF(partial_copy) THEN
        ss2%id_growthrate_mmm71 = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%growthrate_mmm71).AND.ALLOCATED(ss2%growthrate_mmm71)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%growthrate_mmm71 = ss%growthrate_mmm71
     IF(ALLOCATED(ss2%id_growthrate_mmm71)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_growthrate_mmm71 = ss%id_growthrate_mmm71
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_growthrate_mtm = 0
  ENDIF
  IF((ALLOCATED(ss%growthrate_mtm).AND.ALLOCATED(ss2%growthrate_mtm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%growthrate_mtm = ss%growthrate_mtm
     IF(.not.partial_copy) THEN
        ss2%id_growthrate_mtm = ss%id_growthrate_mtm
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_growthrate_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_growthrate_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%growthrate_tglf).AND.ALLOCATED(ss2%growthrate_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%growthrate_tglf = ss%growthrate_tglf
     IF(ALLOCATED(ss2%id_growthrate_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_growthrate_tglf = ss%id_growthrate_tglf
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_growthrate_vpar)) THEN
     IF(partial_copy) THEN
        ss2%id_growthrate_vpar = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%growthrate_vpar).AND.ALLOCATED(ss2%growthrate_vpar)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%growthrate_vpar = ss%growthrate_vpar
     IF(ALLOCATED(ss2%id_growthrate_vpar)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_growthrate_vpar = ss%id_growthrate_vpar
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%gs_name)).AND.(ALLOCATED(ss%gs_name))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_GAS).EQ.1)) ) THEN
     ss2%gs_name = ss%gs_name
  ENDIF
  IF(partial_copy) THEN
     ss2%id_g_eq = 0
  ENDIF
  IF((ALLOCATED(ss%g_eq).AND.ALLOCATED(ss2%g_eq)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%g_eq = ss%g_eq
     IF(.not.partial_copy) THEN
        ss2%id_g_eq = ss%id_g_eq
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%hegrilh)).AND.(ALLOCATED(ss%hegrilh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%hegrilh = ss%hegrilh
  ENDIF
  IF( ((ALLOCATED(ss2%Hsize_coil)).AND.(ALLOCATED(ss%Hsize_coil))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%Hsize_coil = ss%Hsize_coil
  ENDIF
  IF( ((ALLOCATED(ss2%H_einj_standard)).AND.(ALLOCATED(ss%H_einj_standard))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%H_einj_standard = ss%H_einj_standard
  ENDIF
  IF( ((ALLOCATED(ss2%H_ffull_standard)).AND.(ALLOCATED(ss%H_ffull_standard))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%H_ffull_standard = ss%H_ffull_standard
  ENDIF
  IF( ((ALLOCATED(ss2%H_fhalf_standard)).AND.(ALLOCATED(ss%H_fhalf_standard))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%H_fhalf_standard = ss%H_fhalf_standard
  ENDIF
  IF( ((ALLOCATED(ss2%icircuit_coil)).AND.(ALLOCATED(ss%icircuit_coil))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%icircuit_coil = ss%icircuit_coil
  ENDIF
  IF( ((ALLOCATED(ss2%icrf_src_name)).AND.(ALLOCATED(ss%icrf_src_name))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%icrf_src_name = ss%icrf_src_name
  ENDIF
  IF((cclist(ps_cnum_IC).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%IC_Code_Info = ss%IC_Code_Info
  ENDIF
  IF((cclist(ps_cnum_IC).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%IC_Data_Info = ss%IC_Data_Info
  ENDIF
  IF( ((ALLOCATED(ss2%Imag_ant_coef)).AND.(ALLOCATED(ss%Imag_ant_coef))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%Imag_ant_coef = ss%Imag_ant_coef
  ENDIF
  IF((cclist(ps_cnum_ANOM).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%interchange_R = ss%interchange_R
  ENDIF
  IF(partial_copy) THEN
     ss2%id_iota = 0
  ENDIF
  IF((ALLOCATED(ss%iota).AND.ALLOCATED(ss2%iota)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%iota = ss%iota
     IF(.not.partial_copy) THEN
        ss2%id_iota = ss%id_iota
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%isThermal)).AND.(ALLOCATED(ss%isThermal))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%isThermal = ss%isThermal
  ENDIF
  IF( ((ALLOCATED(ss2%is_recycling)).AND.(ALLOCATED(ss%is_recycling))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_GAS).EQ.1)) ) THEN
     ss2%is_recycling = ss%is_recycling
  ENDIF
  IF((cclist(ps_cnum_LH).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%i_npoloidal = ss%i_npoloidal
  ENDIF
  IF(partial_copy) THEN
     ss2%id_jdotb = 0
  ENDIF
  IF((ALLOCATED(ss%jdotb).AND.ALLOCATED(ss2%jdotb)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%jdotb = ss%jdotb
     IF(.not.partial_copy) THEN
        ss2%id_jdotb = ss%id_jdotb
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%kccw_Bphi = ss%kccw_Bphi
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%kccw_Jphi = ss%kccw_Jphi
  ENDIF
  IF((cclist(ps_cnum_IC).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%kdens_rfmin = ss%kdens_rfmin
  ENDIF
  IF( ((ALLOCATED(ss2%kvolt_nbi)).AND.(ALLOCATED(ss%kvolt_nbi))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%kvolt_nbi = ss%kvolt_nbi
  ENDIF
  IF(partial_copy) THEN
     ss2%id_kyrhos_drbm = 0
  ENDIF
  IF((ALLOCATED(ss%kyrhos_drbm).AND.ALLOCATED(ss2%kyrhos_drbm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%kyrhos_drbm = ss%kyrhos_drbm
     IF(.not.partial_copy) THEN
        ss2%id_kyrhos_drbm = ss%id_kyrhos_drbm
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_kyrhos_etg = 0
  ENDIF
  IF((ALLOCATED(ss%kyrhos_etg).AND.ALLOCATED(ss2%kyrhos_etg)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%kyrhos_etg = ss%kyrhos_etg
     IF(.not.partial_copy) THEN
        ss2%id_kyrhos_etg = ss%id_kyrhos_etg
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_kyrhos_mtm = 0
  ENDIF
  IF((ALLOCATED(ss%kyrhos_mtm).AND.ALLOCATED(ss2%kyrhos_mtm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%kyrhos_mtm = ss%kyrhos_mtm
     IF(.not.partial_copy) THEN
        ss2%id_kyrhos_mtm = ss%id_kyrhos_mtm
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_kyspectrum_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_kyspectrum_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%kyspectrum_tglf).AND.ALLOCATED(ss2%kyspectrum_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%kyspectrum_tglf = ss%kyspectrum_tglf
     IF(ALLOCATED(ss2%id_kyspectrum_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_kyspectrum_tglf = ss%id_kyspectrum_tglf
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%Lbscap)).AND.(ALLOCATED(ss%Lbscap))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%Lbscap = ss%Lbscap
  ENDIF
  IF( ((ALLOCATED(ss2%Lbscap2)).AND.(ALLOCATED(ss%Lbscap2))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%Lbscap2 = ss%Lbscap2
  ENDIF
  IF( ((ALLOCATED(ss2%Lbsctan)).AND.(ALLOCATED(ss%Lbsctan))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%Lbsctan = ss%Lbsctan
  ENDIF
  IF( ((ALLOCATED(ss2%lhrf_src_name)).AND.(ALLOCATED(ss%lhrf_src_name))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%lhrf_src_name = ss%lhrf_src_name
  ENDIF
  IF((cclist(ps_cnum_LH).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%LH_Code_Info = ss%LH_Code_Info
  ENDIF
  IF((cclist(ps_cnum_LH).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%LH_Data_Info = ss%LH_Data_Info
  ENDIF
  IF((cclist(ps_cnum_LMHD).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%LMHD_eq_code = ss%LMHD_eq_code
  ENDIF
  IF((cclist(ps_cnum_LMHD).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%LMHD_status = ss%LMHD_status
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%lock_machine_descr = ss%lock_machine_descr
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%lock_shot_config = ss%lock_shot_config
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%lock_sim_init = ss%lock_sim_init
  ENDIF
  IF(partial_copy) THEN
     ss2%id_Lpol = 0
  ENDIF
  IF((ALLOCATED(ss%Lpol).AND.ALLOCATED(ss2%Lpol)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%Lpol = ss%Lpol
     IF(.not.partial_copy) THEN
        ss2%id_Lpol = ss%id_Lpol
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%Mercier_stable)).AND.(ALLOCATED(ss%Mercier_stable))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%Mercier_stable = ss%Mercier_stable
  ENDIF
  IF((cclist(ps_cnum_LMHD).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%MHD_eq_status = ss%MHD_eq_status
  ENDIF
  IF((ALLOCATED(ss2%m_ALL)).AND.(ALLOCATED(ss%m_ALL))) THEN
     ss2%m_ALL = ss%m_ALL
  ENDIF
  IF((ALLOCATED(ss2%m_ALLA)).AND.(ALLOCATED(ss%m_ALLA))) THEN
     ss2%m_ALLA = ss%m_ALLA
  ENDIF
  IF((ALLOCATED(ss2%m_RFMIN)).AND.(ALLOCATED(ss%m_RFMIN))) THEN
     ss2%m_RFMIN = ss%m_RFMIN
  ENDIF
  IF((ALLOCATED(ss2%m_S)).AND.(ALLOCATED(ss%m_S))) THEN
     ss2%m_S = ss%m_S
  ENDIF
  IF((ALLOCATED(ss2%m_SA)).AND.(ALLOCATED(ss%m_SA))) THEN
     ss2%m_SA = ss%m_SA
  ENDIF
  IF((ALLOCATED(ss2%m_SFUS)).AND.(ALLOCATED(ss%m_SFUS))) THEN
     ss2%m_SFUS = ss%m_SFUS
  ENDIF
  IF((ALLOCATED(ss2%m_SGAS)).AND.(ALLOCATED(ss%m_SGAS))) THEN
     ss2%m_SGAS = ss%m_SGAS
  ENDIF
  IF((ALLOCATED(ss2%m_SIMP0)).AND.(ALLOCATED(ss%m_SIMP0))) THEN
     ss2%m_SIMP0 = ss%m_SIMP0
  ENDIF
  IF((ALLOCATED(ss2%m_SIMPI)).AND.(ALLOCATED(ss%m_SIMPI))) THEN
     ss2%m_SIMPI = ss%m_SIMPI
  ENDIF
  IF((ALLOCATED(ss2%m_SNBI)).AND.(ALLOCATED(ss%m_SNBI))) THEN
     ss2%m_SNBI = ss%m_SNBI
  ENDIF
  IF(ALLOCATED(ss2%id_n0norm)) THEN
     IF(partial_copy) THEN
        ss2%id_n0norm = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%n0norm).AND.ALLOCATED(ss2%n0norm)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%n0norm = ss%n0norm
     IF(ALLOCATED(ss2%id_n0norm)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_n0norm = ss%id_n0norm
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_n0_halo)) THEN
     IF(partial_copy) THEN
        ss2%id_n0_halo = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%n0_halo).AND.ALLOCATED(ss2%n0_halo)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%n0_halo = ss%n0_halo
     IF(ALLOCATED(ss2%id_n0_halo)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_n0_halo = ss%id_n0_halo
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_n0_reco)) THEN
     IF(partial_copy) THEN
        ss2%id_n0_reco = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%n0_reco).AND.ALLOCATED(ss2%n0_reco)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%n0_reco = ss%n0_reco
     IF(ALLOCATED(ss2%id_n0_reco)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_n0_reco = ss%id_n0_reco
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%nbap2_shape)).AND.(ALLOCATED(ss%nbap2_shape))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%nbap2_shape = ss%nbap2_shape
  ENDIF
  IF( ((ALLOCATED(ss2%nbap_shape)).AND.(ALLOCATED(ss%nbap_shape))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%nbap_shape = ss%nbap_shape
  ENDIF

END SUBROUTINE plasma_state_copy_3
