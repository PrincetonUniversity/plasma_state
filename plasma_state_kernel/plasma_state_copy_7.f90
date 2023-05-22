! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_copy_7(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)


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
  IF((ALLOCATED(ss%rho_pel).AND.ALLOCATED(ss2%rho_pel)).AND. &
        ((cclist(ps_cnum_PEL).EQ.1))) THEN
     ss2%rho_pel = ss%rho_pel
  ENDIF
  IF((ALLOCATED(ss%rho_rad).AND.ALLOCATED(ss2%rho_rad)).AND. &
        ((cclist(ps_cnum_RAD).EQ.1))) THEN
     ss2%rho_rad = ss%rho_rad
  ENDIF
  IF((ALLOCATED(ss%rho_rw).AND.ALLOCATED(ss2%rho_rw)).AND. &
        ((cclist(ps_cnum_RUNAWAY).EQ.1))) THEN
     ss2%rho_rw = ss%rho_rw
  ENDIF
  IF( ((ALLOCATED(ss2%rlim)).AND.(ALLOCATED(ss%rlim))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%rlim = ss%rlim
  ENDIF
  IF( ((ALLOCATED(ss2%Rloc_coil)).AND.(ALLOCATED(ss%Rloc_coil))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%Rloc_coil = ss%Rloc_coil
  ENDIF
  IF(partial_copy) THEN
     ss2%id_Rmajor_mean = 0
  ENDIF
  IF((ALLOCATED(ss%Rmajor_mean).AND.ALLOCATED(ss2%Rmajor_mean)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%Rmajor_mean = ss%Rmajor_mean
     IF(.not.partial_copy) THEN
        ss2%id_Rmajor_mean = ss%id_Rmajor_mean
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_rMinor_mean = 0
  ENDIF
  IF((ALLOCATED(ss%rMinor_mean).AND.ALLOCATED(ss2%rMinor_mean)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%rMinor_mean = ss%rMinor_mean
     IF(.not.partial_copy) THEN
        ss2%id_rMinor_mean = ss%id_rMinor_mean
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_RUNAWAY).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%RUNAWAY_Code_Info = ss%RUNAWAY_Code_Info
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%RunID = ss%RunID
  ENDIF
  IF( ((ALLOCATED(ss2%R_ant)).AND.(ALLOCATED(ss%R_ant))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%R_ant = ss%R_ant
  ENDIF
  IF( ((ALLOCATED(ss2%R_antgeo)).AND.(ALLOCATED(ss%R_antgeo))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%R_antgeo = ss%R_antgeo
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%R_axis = ss%R_axis
  ENDIF
  IF( ((ALLOCATED(ss2%R_EC_launch)).AND.(ALLOCATED(ss%R_EC_launch))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%R_EC_launch = ss%R_EC_launch
  ENDIF
  IF(partial_copy) THEN
     ss2%id_R_geo = 0
  ENDIF
  IF((ALLOCATED(ss%R_geo).AND.ALLOCATED(ss2%R_geo)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%R_geo = ss%R_geo
     IF(.not.partial_copy) THEN
        ss2%id_R_geo = ss%id_R_geo
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%R_grid).AND.ALLOCATED(ss2%R_grid)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%R_grid = ss%R_grid
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%R_max_box = ss%R_max_box
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%R_max_lcfs = ss%R_max_lcfs
  ENDIF
  IF(partial_copy) THEN
     ss2%id_R_midp_in = 0
  ENDIF
  IF((ALLOCATED(ss%R_midp_in).AND.ALLOCATED(ss2%R_midp_in)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%R_midp_in = ss%R_midp_in
     IF(.not.partial_copy) THEN
        ss2%id_R_midp_in = ss%id_R_midp_in
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_R_midp_out = 0
  ENDIF
  IF((ALLOCATED(ss%R_midp_out).AND.ALLOCATED(ss2%R_midp_out)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%R_midp_out = ss%R_midp_out
     IF(.not.partial_copy) THEN
        ss2%id_R_midp_out = ss%id_R_midp_out
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%R_min_box = ss%R_min_box
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%R_min_lcfs = ss%R_min_lcfs
  ENDIF
  IF((ALLOCATED(ss%R_ripple).AND.ALLOCATED(ss2%R_ripple)).AND. &
        ((cclist(ps_cnum_RIPPLE).EQ.1))) THEN
     ss2%R_ripple = ss%R_ripple
  ENDIF
  IF(partial_copy) THEN
     ss2%id_R_surfMax = 0
  ENDIF
  IF((ALLOCATED(ss%R_surfMax).AND.ALLOCATED(ss2%R_surfMax)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%R_surfMax = ss%R_surfMax
     IF(.not.partial_copy) THEN
        ss2%id_R_surfMax = ss%id_R_surfMax
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_R_surfMin = 0
  ENDIF
  IF((ALLOCATED(ss%R_surfMin).AND.ALLOCATED(ss2%R_surfMin)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%R_surfMin = ss%R_surfMin
     IF(.not.partial_copy) THEN
        ss2%id_R_surfMin = ss%id_R_surfMin
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_s0reco)) THEN
     IF(partial_copy) THEN
        ss2%id_s0reco = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%s0reco).AND.ALLOCATED(ss2%s0reco)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%s0reco = ss%s0reco
     IF(ALLOCATED(ss2%id_s0reco)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_s0reco = ss%id_s0reco
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_s0reco_e = 0
  ENDIF
  IF((ALLOCATED(ss%s0reco_e).AND.ALLOCATED(ss2%s0reco_e)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%s0reco_e = ss%s0reco_e
     IF(.not.partial_copy) THEN
        ss2%id_s0reco_e = ss%id_s0reco_e
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_s0reco_recap)) THEN
     IF(partial_copy) THEN
        ss2%id_s0reco_recap = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%s0reco_recap).AND.ALLOCATED(ss2%s0reco_recap)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%s0reco_recap = ss%s0reco_recap
     IF(ALLOCATED(ss2%id_s0reco_recap)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_s0reco_recap = ss%id_s0reco_recap
        ENDIF
     ENDIF
  ENDIF
  IF((ALLOCATED(ss2%sa_index)).AND.(ALLOCATED(ss%sa_index))) THEN
     ss2%sa_index = ss%sa_index
  ENDIF
  IF((ALLOCATED(ss2%SA_name)).AND.(ALLOCATED(ss%SA_name))) THEN
     ss2%SA_name = ss%SA_name
  ENDIF
  IF((ALLOCATED(ss2%SA_type)).AND.(ALLOCATED(ss%SA_type))) THEN
     ss2%SA_type = ss%SA_type
  ENDIF
  IF(ALLOCATED(ss2%id_sb0halo)) THEN
     IF(partial_copy) THEN
        ss2%id_sb0halo = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%sb0halo).AND.ALLOCATED(ss2%sb0halo)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%sb0halo = ss%sb0halo
     IF(ALLOCATED(ss2%id_sb0halo)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_sb0halo = ss%id_sb0halo
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_sb0halo_recap)) THEN
     IF(partial_copy) THEN
        ss2%id_sb0halo_recap = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%sb0halo_recap).AND.ALLOCATED(ss2%sb0halo_recap)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%sb0halo_recap = ss%sb0halo_recap
     IF(ALLOCATED(ss2%id_sb0halo_recap)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_sb0halo_recap = ss%id_sb0halo_recap
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_sbedep = 0
  ENDIF
  IF((ALLOCATED(ss%sbedep).AND.ALLOCATED(ss2%sbedep)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%sbedep = ss%sbedep
     IF(.not.partial_copy) THEN
        ss2%id_sbedep = ss%id_sbedep
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_sbehalo = 0
  ENDIF
  IF((ALLOCATED(ss%sbehalo).AND.ALLOCATED(ss2%sbehalo)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%sbehalo = ss%sbehalo
     IF(.not.partial_copy) THEN
        ss2%id_sbehalo = ss%id_sbehalo
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_sbsce)) THEN
     IF(partial_copy) THEN
        ss2%id_sbsce = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%sbsce).AND.ALLOCATED(ss2%sbsce)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%sbsce = ss%sbsce
     IF(ALLOCATED(ss2%id_sbsce)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_sbsce = ss%id_sbsce
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_sbtherm)) THEN
     IF(partial_copy) THEN
        ss2%id_sbtherm = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%sbtherm).AND.ALLOCATED(ss2%sbtherm)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%sbtherm = ss%sbtherm
     IF(ALLOCATED(ss2%id_sbtherm)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_sbtherm = ss%id_sbtherm
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%sc0)).AND.(ALLOCATED(ss%sc0))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_GAS).EQ.1)) ) THEN
     ss2%sc0 = ss%sc0
  ENDIF
  IF( ((ALLOCATED(ss2%sc0_to_sgas)).AND.(ALLOCATED(ss%sc0_to_sgas))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_GAS).EQ.1)) ) THEN
     ss2%sc0_to_sgas = ss%sc0_to_sgas
  ENDIF
  IF(ALLOCATED(ss2%id_sc_anom)) THEN
     IF(partial_copy) THEN
        ss2%id_sc_anom = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%sc_anom).AND.ALLOCATED(ss2%sc_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%sc_anom = ss%sc_anom
     IF(ALLOCATED(ss2%id_sc_anom)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_sc_anom = ss%id_sc_anom
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_sfsce)) THEN
     IF(partial_copy) THEN
        ss2%id_sfsce = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%sfsce).AND.ALLOCATED(ss2%sfsce)).AND. &
        ((cclist(ps_cnum_FUS).EQ.1))) THEN
     ss2%sfsce = ss%sfsce
     IF(ALLOCATED(ss2%id_sfsce)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_sfsce = ss%id_sfsce
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_sftherm)) THEN
     IF(partial_copy) THEN
        ss2%id_sftherm = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%sftherm).AND.ALLOCATED(ss2%sftherm)).AND. &
        ((cclist(ps_cnum_FUS).EQ.1))) THEN
     ss2%sftherm = ss%sftherm
     IF(ALLOCATED(ss2%id_sftherm)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_sftherm = ss%id_sftherm
        ENDIF
     ENDIF
  ENDIF
  IF((ALLOCATED(ss2%SFUS_name)).AND.(ALLOCATED(ss%SFUS_name))) THEN
     ss2%SFUS_name = ss%SFUS_name
  ENDIF
  IF((ALLOCATED(ss2%sfus_to_all)).AND.(ALLOCATED(ss%sfus_to_all))) THEN
     ss2%sfus_to_all = ss%sfus_to_all
  ENDIF
  IF((ALLOCATED(ss2%sfus_to_alla)).AND.(ALLOCATED(ss%sfus_to_alla))) THEN
     ss2%sfus_to_alla = ss%sfus_to_alla
  ENDIF
  IF((ALLOCATED(ss2%SFUS_type)).AND.(ALLOCATED(ss%SFUS_type))) THEN
     ss2%SFUS_type = ss%SFUS_type
  ENDIF
  IF((ALLOCATED(ss2%SGAS_name)).AND.(ALLOCATED(ss%SGAS_name))) THEN
     ss2%SGAS_name = ss%SGAS_name
  ENDIF
  IF((ALLOCATED(ss2%sgas_to_s)).AND.(ALLOCATED(ss%sgas_to_s))) THEN
     ss2%sgas_to_s = ss%sgas_to_s
  ENDIF
  IF((ALLOCATED(ss2%SGAS_type)).AND.(ALLOCATED(ss%SGAS_type))) THEN
     ss2%SGAS_type = ss%SGAS_type
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%shot_number = ss%shot_number
  ENDIF
  IF(ALLOCATED(ss2%id_sii_xs)) THEN
     IF(partial_copy) THEN
        ss2%id_sii_xs = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%sii_xs).AND.ALLOCATED(ss2%sii_xs)).AND. &
        ((cclist(ps_cnum_RAD).EQ.1))) THEN
     ss2%sii_xs = ss%sii_xs
     IF(ALLOCATED(ss2%id_sii_xs)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_sii_xs = ss%id_sii_xs
        ENDIF
     ENDIF
  ENDIF
  IF((ALLOCATED(ss2%SIMP0_name)).AND.(ALLOCATED(ss%SIMP0_name))) THEN
     ss2%SIMP0_name = ss%SIMP0_name
  ENDIF
  IF((ALLOCATED(ss2%simp0_to_s)).AND.(ALLOCATED(ss%simp0_to_s))) THEN
     ss2%simp0_to_s = ss%simp0_to_s
  ENDIF
  IF((ALLOCATED(ss2%SIMP0_type)).AND.(ALLOCATED(ss%SIMP0_type))) THEN
     ss2%SIMP0_type = ss%SIMP0_type
  ENDIF
  IF((ALLOCATED(ss2%SIMPI_name)).AND.(ALLOCATED(ss%SIMPI_name))) THEN
     ss2%SIMPI_name = ss%SIMPI_name
  ENDIF
  IF((ALLOCATED(ss2%simpi_to_s)).AND.(ALLOCATED(ss%simpi_to_s))) THEN
     ss2%simpi_to_s = ss%simpi_to_s
  ENDIF
  IF((ALLOCATED(ss2%SIMPI_type)).AND.(ALLOCATED(ss%SIMPI_type))) THEN
     ss2%SIMPI_type = ss%SIMPI_type
  ENDIF
  IF((ALLOCATED(ss2%SNBI_name)).AND.(ALLOCATED(ss%SNBI_name))) THEN
     ss2%SNBI_name = ss%SNBI_name
  ENDIF
  IF((ALLOCATED(ss2%snbi_to_all)).AND.(ALLOCATED(ss%snbi_to_all))) THEN
     ss2%snbi_to_all = ss%snbi_to_all
  ENDIF
  IF((ALLOCATED(ss2%snbi_to_alla)).AND.(ALLOCATED(ss%snbi_to_alla))) THEN
     ss2%snbi_to_alla = ss%snbi_to_alla
  ENDIF
  IF((ALLOCATED(ss2%SNBI_type)).AND.(ALLOCATED(ss%SNBI_type))) THEN
     ss2%SNBI_type = ss%SNBI_type
  ENDIF
  IF(ALLOCATED(ss2%id_sn_trans)) THEN
     IF(partial_copy) THEN
        ss2%id_sn_trans = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%sn_trans).AND.ALLOCATED(ss2%sn_trans)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%sn_trans = ss%sn_trans
     IF(ALLOCATED(ss2%id_sn_trans)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_sn_trans = ss%id_sn_trans
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_sn_transcat)) THEN
     IF(partial_copy) THEN
        ss2%id_sn_transcat = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%sn_transcat).AND.ALLOCATED(ss2%sn_transcat)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%sn_transcat = ss%sn_transcat
     IF(ALLOCATED(ss2%id_sn_transcat)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_sn_transcat = ss%id_sn_transcat
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_sprof0)) THEN
     IF(partial_copy) THEN
        ss2%id_sprof0 = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%sprof0).AND.ALLOCATED(ss2%sprof0)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%sprof0 = ss%sprof0
     IF(ALLOCATED(ss2%id_sprof0)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_sprof0 = ss%id_sprof0
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_sprof0e)) THEN
     IF(partial_copy) THEN
        ss2%id_sprof0e = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%sprof0e).AND.ALLOCATED(ss2%sprof0e)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%sprof0e = ss%sprof0e
     IF(ALLOCATED(ss2%id_sprof0e)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_sprof0e = ss%id_sprof0e
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_spsce)) THEN
     IF(partial_copy) THEN
        ss2%id_spsce = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%spsce).AND.ALLOCATED(ss2%spsce)).AND. &
        ((cclist(ps_cnum_PEL).EQ.1))) THEN
     ss2%spsce = ss%spsce
     IF(ALLOCATED(ss2%id_spsce)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_spsce = ss%id_spsce
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_squareLO = 0
  ENDIF
  IF((ALLOCATED(ss%squareLO).AND.ALLOCATED(ss2%squareLO)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%squareLO = ss%squareLO
     IF(.not.partial_copy) THEN
        ss2%id_squareLO = ss%id_squareLO
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_squareUO = 0
  ENDIF
  IF((ALLOCATED(ss%squareUO).AND.ALLOCATED(ss2%squareUO)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%squareUO = ss%squareUO
     IF(.not.partial_copy) THEN
        ss2%id_squareUO = ss%id_squareUO
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%sRtcen)).AND.(ALLOCATED(ss%sRtcen))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%sRtcen = ss%sRtcen
  ENDIF
  IF(partial_copy) THEN
     ss2%id_surf = 0
  ENDIF
  IF((ALLOCATED(ss%surf).AND.ALLOCATED(ss2%surf)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%surf = ss%surf
     IF(.not.partial_copy) THEN
        ss2%id_surf = ss%id_surf
     ENDIF
  ENDIF
  IF((ALLOCATED(ss2%S_name)).AND.(ALLOCATED(ss%S_name))) THEN
     ss2%S_name = ss%S_name
  ENDIF
  IF((ALLOCATED(ss2%s_to_sa)).AND.(ALLOCATED(ss%s_to_sa))) THEN
     ss2%s_to_sa = ss%s_to_sa
  ENDIF
  IF((ALLOCATED(ss2%S_type)).AND.(ALLOCATED(ss%S_type))) THEN
     ss2%S_type = ss%S_type
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%t0 = ss%t0
  ENDIF
  IF(ALLOCATED(ss2%id_T0cx)) THEN
     IF(partial_copy) THEN
        ss2%id_T0cx = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%T0cx).AND.ALLOCATED(ss2%T0cx)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%T0cx = ss%T0cx
     IF(ALLOCATED(ss2%id_T0cx)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_T0cx = ss%id_T0cx
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_T0sc0)) THEN
     IF(partial_copy) THEN
        ss2%id_T0sc0 = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%T0sc0).AND.ALLOCATED(ss2%T0sc0)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%T0sc0 = ss%T0sc0
     IF(ALLOCATED(ss2%id_T0sc0)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_T0sc0 = ss%id_T0sc0
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_T0_halo)) THEN
     IF(partial_copy) THEN
        ss2%id_T0_halo = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%T0_halo).AND.ALLOCATED(ss2%T0_halo)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%T0_halo = ss%T0_halo
     IF(ALLOCATED(ss2%id_T0_halo)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_T0_halo = ss%id_T0_halo
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_T0_reco)) THEN
     IF(partial_copy) THEN
        ss2%id_T0_reco = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%T0_reco).AND.ALLOCATED(ss2%T0_reco)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%T0_reco = ss%T0_reco
     IF(ALLOCATED(ss2%id_T0_reco)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_T0_reco = ss%id_T0_reco
        ENDIF
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_copy_7
