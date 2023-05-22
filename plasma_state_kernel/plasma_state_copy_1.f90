! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_copy_1(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)


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
     ss2%id_chii_ntm = 0
  ENDIF
  IF((ALLOCATED(ss%chii_ntm).AND.ALLOCATED(ss2%chii_ntm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chii_ntm = ss%chii_ntm
     IF(.not.partial_copy) THEN
        ss2%id_chii_ntm = ss%id_chii_ntm
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chii_tglf = 0
  ENDIF
  IF((ALLOCATED(ss%chii_tglf).AND.ALLOCATED(ss2%chii_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chii_tglf = ss%chii_tglf
     IF(.not.partial_copy) THEN
        ss2%id_chii_tglf = ss%id_chii_tglf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chii_trans = 0
  ENDIF
  IF((ALLOCATED(ss%chii_trans).AND.ALLOCATED(ss2%chii_trans)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%chii_trans = ss%chii_trans
     IF(.not.partial_copy) THEN
        ss2%id_chii_trans = ss%id_chii_trans
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_chii_transcat)) THEN
     IF(partial_copy) THEN
        ss2%id_chii_transcat = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%chii_transcat).AND.ALLOCATED(ss2%chii_transcat)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%chii_transcat = ss%chii_transcat
     IF(ALLOCATED(ss2%id_chii_transcat)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_chii_transcat = ss%id_chii_transcat
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chii_w19 = 0
  ENDIF
  IF((ALLOCATED(ss%chii_w19).AND.ALLOCATED(ss2%chii_w19)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chii_w19 = ss%chii_w19
     IF(.not.partial_copy) THEN
        ss2%id_chii_w19 = ss%id_chii_w19
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chimo_trans = 0
  ENDIF
  IF((ALLOCATED(ss%chimo_trans).AND.ALLOCATED(ss2%chimo_trans)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%chimo_trans = ss%chimo_trans
     IF(.not.partial_copy) THEN
        ss2%id_chimo_trans = ss%id_chimo_trans
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_chimo_transcat)) THEN
     IF(partial_copy) THEN
        ss2%id_chimo_transcat = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%chimo_transcat).AND.ALLOCATED(ss2%chimo_transcat)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%chimo_transcat = ss%chimo_transcat
     IF(ALLOCATED(ss2%id_chimo_transcat)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_chimo_transcat = ss%id_chimo_transcat
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chiphi_anom = 0
  ENDIF
  IF((ALLOCATED(ss%chiphi_anom).AND.ALLOCATED(ss2%chiphi_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chiphi_anom = ss%chiphi_anom
     IF(.not.partial_copy) THEN
        ss2%id_chiphi_anom = ss%id_chiphi_anom
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chiphi_drbm = 0
  ENDIF
  IF((ALLOCATED(ss%chiphi_drbm).AND.ALLOCATED(ss2%chiphi_drbm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chiphi_drbm = ss%chiphi_drbm
     IF(.not.partial_copy) THEN
        ss2%id_chiphi_drbm = ss%id_chiphi_drbm
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chiphi_glf = 0
  ENDIF
  IF((ALLOCATED(ss%chiphi_glf).AND.ALLOCATED(ss2%chiphi_glf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chiphi_glf = ss%chiphi_glf
     IF(.not.partial_copy) THEN
        ss2%id_chiphi_glf = ss%id_chiphi_glf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chiphi_gtcneo = 0
  ENDIF
  IF((ALLOCATED(ss%chiphi_gtcneo).AND.ALLOCATED(ss2%chiphi_gtcneo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chiphi_gtcneo = ss%chiphi_gtcneo
     IF(.not.partial_copy) THEN
        ss2%id_chiphi_gtcneo = ss%id_chiphi_gtcneo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chiphi_mmm71 = 0
  ENDIF
  IF((ALLOCATED(ss%chiphi_mmm71).AND.ALLOCATED(ss2%chiphi_mmm71)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chiphi_mmm71 = ss%chiphi_mmm71
     IF(.not.partial_copy) THEN
        ss2%id_chiphi_mmm71 = ss%id_chiphi_mmm71
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chiphi_neo = 0
  ENDIF
  IF((ALLOCATED(ss%chiphi_neo).AND.ALLOCATED(ss2%chiphi_neo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chiphi_neo = ss%chiphi_neo
     IF(.not.partial_copy) THEN
        ss2%id_chiphi_neo = ss%id_chiphi_neo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chiphi_tglf = 0
  ENDIF
  IF((ALLOCATED(ss%chiphi_tglf).AND.ALLOCATED(ss2%chiphi_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chiphi_tglf = ss%chiphi_tglf
     IF(.not.partial_copy) THEN
        ss2%id_chiphi_tglf = ss%id_chiphi_tglf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chiphi_w19 = 0
  ENDIF
  IF((ALLOCATED(ss%chiphi_w19).AND.ALLOCATED(ss2%chiphi_w19)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chiphi_w19 = ss%chiphi_w19
     IF(.not.partial_copy) THEN
        ss2%id_chiphi_w19 = ss%id_chiphi_w19
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chitheta_mmm71 = 0
  ENDIF
  IF((ALLOCATED(ss%chitheta_mmm71).AND.ALLOCATED(ss2%chitheta_mmm71)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chitheta_mmm71 = ss%chitheta_mmm71
     IF(.not.partial_copy) THEN
        ss2%id_chitheta_mmm71 = ss%id_chitheta_mmm71
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%circuit_name)).AND.(ALLOCATED(ss%circuit_name))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%circuit_name = ss%circuit_name
  ENDIF
  IF( ((ALLOCATED(ss2%coil_apt)).AND.(ALLOCATED(ss%coil_apt))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%coil_apt = ss%coil_apt
  ENDIF
  IF( ((ALLOCATED(ss2%coil_in_circuit)).AND.(ALLOCATED(ss%coil_in_circuit))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%coil_in_circuit = ss%coil_in_circuit
  ENDIF
  IF( ((ALLOCATED(ss2%coil_name)).AND.(ALLOCATED(ss%coil_name))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%coil_name = ss%coil_name
  ENDIF
  IF( ((ALLOCATED(ss2%coil_resispt)).AND.(ALLOCATED(ss%coil_resispt))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%coil_resispt = ss%coil_resispt
  ENDIF
  IF(partial_copy) THEN
     ss2%id_curbeam = 0
  ENDIF
  IF((ALLOCATED(ss%curbeam).AND.ALLOCATED(ss2%curbeam)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%curbeam = ss%curbeam
     IF(.not.partial_copy) THEN
        ss2%id_curbeam = ss%id_curbeam
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_curech = 0
  ENDIF
  IF((ALLOCATED(ss%curech).AND.ALLOCATED(ss2%curech)).AND. &
        ((cclist(ps_cnum_EC).EQ.1))) THEN
     ss2%curech = ss%curech
     IF(.not.partial_copy) THEN
        ss2%id_curech = ss%id_curech
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_curech_src)) THEN
     IF(partial_copy) THEN
        ss2%id_curech_src = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%curech_src).AND.ALLOCATED(ss2%curech_src)).AND. &
        ((cclist(ps_cnum_EC).EQ.1))) THEN
     ss2%curech_src = ss%curech_src
     IF(ALLOCATED(ss2%id_curech_src)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_curech_src = ss%id_curech_src
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_curfusn = 0
  ENDIF
  IF((ALLOCATED(ss%curfusn).AND.ALLOCATED(ss2%curfusn)).AND. &
        ((cclist(ps_cnum_FUS).EQ.1))) THEN
     ss2%curfusn = ss%curfusn
     IF(.not.partial_copy) THEN
        ss2%id_curfusn = ss%id_curfusn
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_curich = 0
  ENDIF
  IF((ALLOCATED(ss%curich).AND.ALLOCATED(ss2%curich)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%curich = ss%curich
     IF(.not.partial_copy) THEN
        ss2%id_curich = ss%id_curich
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_curlh = 0
  ENDIF
  IF((ALLOCATED(ss%curlh).AND.ALLOCATED(ss2%curlh)).AND. &
        ((cclist(ps_cnum_LH).EQ.1))) THEN
     ss2%curlh = ss%curlh
     IF(.not.partial_copy) THEN
        ss2%id_curlh = ss%id_curlh
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_curlh_src)) THEN
     IF(partial_copy) THEN
        ss2%id_curlh_src = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%curlh_src).AND.ALLOCATED(ss2%curlh_src)).AND. &
        ((cclist(ps_cnum_LH).EQ.1))) THEN
     ss2%curlh_src = ss%curlh_src
     IF(ALLOCATED(ss2%id_curlh_src)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_curlh_src = ss%id_curlh_src
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_curmino = 0
  ENDIF
  IF((ALLOCATED(ss%curmino).AND.ALLOCATED(ss2%curmino)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%curmino = ss%curmino
     IF(.not.partial_copy) THEN
        ss2%id_curmino = ss%id_curmino
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_curr_bootstrap = 0
  ENDIF
  IF((ALLOCATED(ss%curr_bootstrap).AND.ALLOCATED(ss2%curr_bootstrap)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%curr_bootstrap = ss%curr_bootstrap
     IF(.not.partial_copy) THEN
        ss2%id_curr_bootstrap = ss%id_curr_bootstrap
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_curr_ohmic = 0
  ENDIF
  IF((ALLOCATED(ss%curr_ohmic).AND.ALLOCATED(ss2%curr_ohmic)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%curr_ohmic = ss%curr_ohmic
     IF(.not.partial_copy) THEN
        ss2%id_curr_ohmic = ss%id_curr_ohmic
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_curt = 0
  ENDIF
  IF((ALLOCATED(ss%curt).AND.ALLOCATED(ss2%curt)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%curt = ss%curt
     IF(.not.partial_copy) THEN
        ss2%id_curt = ss%id_curt
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%CUR_Data_Info = ss%CUR_Data_Info
  ENDIF
  IF(partial_copy) THEN
     ss2%id_cur_rw = 0
  ENDIF
  IF((ALLOCATED(ss%cur_rw).AND.ALLOCATED(ss2%cur_rw)).AND. &
        ((cclist(ps_cnum_RUNAWAY).EQ.1))) THEN
     ss2%cur_rw = ss%cur_rw
     IF(.not.partial_copy) THEN
        ss2%id_cur_rw = ss%id_cur_rw
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%d2R_geo_dRHOdTH).AND.ALLOCATED(ss2%d2R_geo_dRHOdTH)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%d2R_geo_dRHOdTH = ss%d2R_geo_dRHOdTH
  ENDIF
  IF((ALLOCATED(ss%d2Z_geo_dRHOdTH).AND.ALLOCATED(ss2%d2Z_geo_dRHOdTH)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%d2Z_geo_dRHOdTH = ss%d2Z_geo_dRHOdTH
  ENDIF
  IF(partial_copy) THEN
     ss2%id_db_mtm = 0
  ENDIF
  IF((ALLOCATED(ss%db_mtm).AND.ALLOCATED(ss2%db_mtm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%db_mtm = ss%db_mtm
     IF(.not.partial_copy) THEN
        ss2%id_db_mtm = ss%id_db_mtm
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_dcur_rw_dVloop = 0
  ENDIF
  IF((ALLOCATED(ss%dcur_rw_dVloop).AND.ALLOCATED(ss2%dcur_rw_dVloop)).AND. &
        ((cclist(ps_cnum_RUNAWAY).EQ.1))) THEN
     ss2%dcur_rw_dVloop = ss%dcur_rw_dVloop
     IF(.not.partial_copy) THEN
        ss2%id_dcur_rw_dVloop = ss%id_dcur_rw_dVloop
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%DeltaW_code)).AND.(ALLOCATED(ss%DeltaW_code))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%DeltaW_code = ss%DeltaW_code
  ENDIF
  IF( ((ALLOCATED(ss2%DeltaW_mapper)).AND.(ALLOCATED(ss%DeltaW_mapper))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%DeltaW_mapper = ss%DeltaW_mapper
  ENDIF
  IF( ((ALLOCATED(ss2%DeltaW_stable)).AND.(ALLOCATED(ss%DeltaW_stable))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%DeltaW_stable = ss%DeltaW_stable
  ENDIF
  IF( ((ALLOCATED(ss2%DeltaW_status)).AND.(ALLOCATED(ss%DeltaW_status))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%DeltaW_status = ss%DeltaW_status
  ENDIF
  IF(partial_copy) THEN
     ss2%id_difb_fusi = 0
  ENDIF
  IF((ALLOCATED(ss%difb_fusi).AND.ALLOCATED(ss2%difb_fusi)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difb_fusi = ss%difb_fusi
     IF(.not.partial_copy) THEN
        ss2%id_difb_fusi = ss%id_difb_fusi
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_difb_nbi = 0
  ENDIF
  IF((ALLOCATED(ss%difb_nbi).AND.ALLOCATED(ss2%difb_nbi)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difb_nbi = ss%difb_nbi
     IF(.not.partial_copy) THEN
        ss2%id_difb_nbi = ss%id_difb_nbi
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_difb_rfmi = 0
  ENDIF
  IF((ALLOCATED(ss%difb_rfmi).AND.ALLOCATED(ss2%difb_rfmi)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difb_rfmi = ss%difb_rfmi
     IF(.not.partial_copy) THEN
        ss2%id_difb_rfmi = ss%id_difb_rfmi
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_diff_trans)) THEN
     IF(partial_copy) THEN
        ss2%id_diff_trans = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%diff_trans).AND.ALLOCATED(ss2%diff_trans)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%diff_trans = ss%diff_trans
     IF(ALLOCATED(ss2%id_diff_trans)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_diff_trans = ss%id_diff_trans
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_diff_transcat)) THEN
     IF(partial_copy) THEN
        ss2%id_diff_transcat = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%diff_transcat).AND.ALLOCATED(ss2%diff_transcat)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%diff_transcat = ss%diff_transcat
     IF(ALLOCATED(ss2%id_diff_transcat)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_diff_transcat = ss%id_diff_transcat
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_difne_nclass = 0
  ENDIF
  IF((ALLOCATED(ss%difne_nclass).AND.ALLOCATED(ss2%difne_nclass)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difne_nclass = ss%difne_nclass
     IF(.not.partial_copy) THEN
        ss2%id_difne_nclass = ss%id_difne_nclass
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_difnh_nclass = 0
  ENDIF
  IF((ALLOCATED(ss%difnh_nclass).AND.ALLOCATED(ss2%difnh_nclass)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difnh_nclass = ss%difnh_nclass
     IF(.not.partial_copy) THEN
        ss2%id_difnh_nclass = ss%id_difnh_nclass
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_difns_anom)) THEN
     IF(partial_copy) THEN
        ss2%id_difns_anom = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%difns_anom).AND.ALLOCATED(ss2%difns_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difns_anom = ss%difns_anom
     IF(ALLOCATED(ss2%id_difns_anom)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_difns_anom = ss%id_difns_anom
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_difns_drbm)) THEN
     IF(partial_copy) THEN
        ss2%id_difns_drbm = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%difns_drbm).AND.ALLOCATED(ss2%difns_drbm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difns_drbm = ss%difns_drbm
     IF(ALLOCATED(ss2%id_difns_drbm)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_difns_drbm = ss%id_difns_drbm
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_difns_glf)) THEN
     IF(partial_copy) THEN
        ss2%id_difns_glf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%difns_glf).AND.ALLOCATED(ss2%difns_glf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difns_glf = ss%difns_glf
     IF(ALLOCATED(ss2%id_difns_glf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_difns_glf = ss%id_difns_glf
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_difns_gtcneo)) THEN
     IF(partial_copy) THEN
        ss2%id_difns_gtcneo = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%difns_gtcneo).AND.ALLOCATED(ss2%difns_gtcneo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difns_gtcneo = ss%difns_gtcneo
     IF(ALLOCATED(ss2%id_difns_gtcneo)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_difns_gtcneo = ss%id_difns_gtcneo
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_difns_mmm71)) THEN
     IF(partial_copy) THEN
        ss2%id_difns_mmm71 = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%difns_mmm71).AND.ALLOCATED(ss2%difns_mmm71)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difns_mmm71 = ss%difns_mmm71
     IF(ALLOCATED(ss2%id_difns_mmm71)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_difns_mmm71 = ss%id_difns_mmm71
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_difns_nclass)) THEN
     IF(partial_copy) THEN
        ss2%id_difns_nclass = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%difns_nclass).AND.ALLOCATED(ss2%difns_nclass)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difns_nclass = ss%difns_nclass
     IF(ALLOCATED(ss2%id_difns_nclass)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_difns_nclass = ss%id_difns_nclass
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_difns_neo)) THEN
     IF(partial_copy) THEN
        ss2%id_difns_neo = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%difns_neo).AND.ALLOCATED(ss2%difns_neo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difns_neo = ss%difns_neo
     IF(ALLOCATED(ss2%id_difns_neo)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_difns_neo = ss%id_difns_neo
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_difns_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_difns_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%difns_tglf).AND.ALLOCATED(ss2%difns_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difns_tglf = ss%difns_tglf
     IF(ALLOCATED(ss2%id_difns_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_difns_tglf = ss%id_difns_tglf
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_difns_w19)) THEN
     IF(partial_copy) THEN
        ss2%id_difns_w19 = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%difns_w19).AND.ALLOCATED(ss2%difns_w19)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difns_w19 = ss%difns_w19
     IF(ALLOCATED(ss2%id_difns_w19)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_difns_w19 = ss%id_difns_w19
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_difnx_nclass = 0
  ENDIF
  IF((ALLOCATED(ss%difnx_nclass).AND.ALLOCATED(ss2%difnx_nclass)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difnx_nclass = ss%difnx_nclass
     IF(.not.partial_copy) THEN
        ss2%id_difnx_nclass = ss%id_difnx_nclass
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_difnz_mmm71 = 0
  ENDIF
  IF((ALLOCATED(ss%difnz_mmm71).AND.ALLOCATED(ss2%difnz_mmm71)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%difnz_mmm71 = ss%difnz_mmm71
     IF(.not.partial_copy) THEN
        ss2%id_difnz_mmm71 = ss%id_difnz_mmm71
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%dist_fun)).AND.(ALLOCATED(ss%dist_fun))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%dist_fun = ss%dist_fun
  ENDIF
  IF((cclist(ps_cnum_GAS).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%dn0out = ss%dn0out
  ENDIF
  IF(partial_copy) THEN
     ss2%id_dQedGrdte_tglf = 0
  ENDIF
  IF((ALLOCATED(ss%dQedGrdte_tglf).AND.ALLOCATED(ss2%dQedGrdte_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%dQedGrdte_tglf = ss%dQedGrdte_tglf
     IF(.not.partial_copy) THEN
        ss2%id_dQedGrdte_tglf = ss%id_dQedGrdte_tglf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_dQedGrdti_tglf = 0
  ENDIF
  IF((ALLOCATED(ss%dQedGrdti_tglf).AND.ALLOCATED(ss2%dQedGrdti_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%dQedGrdti_tglf = ss%dQedGrdti_tglf
     IF(.not.partial_copy) THEN
        ss2%id_dQedGrdti_tglf = ss%id_dQedGrdti_tglf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_dQidGrdte_tglf = 0
  ENDIF
  IF((ALLOCATED(ss%dQidGrdte_tglf).AND.ALLOCATED(ss2%dQidGrdte_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%dQidGrdte_tglf = ss%dQidGrdte_tglf
     IF(.not.partial_copy) THEN
        ss2%id_dQidGrdte_tglf = ss%id_dQidGrdte_tglf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_dQidGrdti_tglf = 0
  ENDIF
  IF((ALLOCATED(ss%dQidGrdti_tglf).AND.ALLOCATED(ss2%dQidGrdti_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%dQidGrdti_tglf = ss%dQidGrdti_tglf
     IF(.not.partial_copy) THEN
        ss2%id_dQidGrdti_tglf = ss%id_dQidGrdti_tglf
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%dR0_momeq_dRHO).AND.ALLOCATED(ss2%dR0_momeq_dRHO)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%dR0_momeq_dRHO = ss%dR0_momeq_dRHO
  ENDIF
  IF((ALLOCATED(ss%dR_geo_dRHO).AND.ALLOCATED(ss2%dR_geo_dRHO)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%dR_geo_dRHO = ss%dR_geo_dRHO
  ENDIF
  IF((ALLOCATED(ss%dR_geo_dTH).AND.ALLOCATED(ss2%dR_geo_dTH)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%dR_geo_dTH = ss%dR_geo_dTH
  ENDIF
  IF((ALLOCATED(ss%dxRjcos_momeq_dRHO).AND.ALLOCATED(ss2%dxRjcos_momeq_dRHO)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%dxRjcos_momeq_dRHO = ss%dxRjcos_momeq_dRHO
  ENDIF
  IF((ALLOCATED(ss%dxRjsin_momeq_dRHO).AND.ALLOCATED(ss2%dxRjsin_momeq_dRHO)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%dxRjsin_momeq_dRHO = ss%dxRjsin_momeq_dRHO
  ENDIF
  IF((ALLOCATED(ss%dxZjcos_momeq_dRHO).AND.ALLOCATED(ss2%dxZjcos_momeq_dRHO)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%dxZjcos_momeq_dRHO = ss%dxZjcos_momeq_dRHO
  ENDIF
  IF((ALLOCATED(ss%dxZjsin_momeq_dRHO).AND.ALLOCATED(ss2%dxZjsin_momeq_dRHO)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%dxZjsin_momeq_dRHO = ss%dxZjsin_momeq_dRHO
  ENDIF
  IF( ((ALLOCATED(ss2%dx_fshield)).AND.(ALLOCATED(ss%dx_fshield))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%dx_fshield = ss%dx_fshield
  ENDIF
  IF((ALLOCATED(ss%dZ0_momeq_dRHO).AND.ALLOCATED(ss2%dZ0_momeq_dRHO)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%dZ0_momeq_dRHO = ss%dZ0_momeq_dRHO
  ENDIF
  IF((ALLOCATED(ss%dZ_geo_dRHO).AND.ALLOCATED(ss2%dZ_geo_dRHO)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%dZ_geo_dRHO = ss%dZ_geo_dRHO
  ENDIF
  IF((ALLOCATED(ss%dZ_geo_dTH).AND.ALLOCATED(ss2%dZ_geo_dTH)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%dZ_geo_dTH = ss%dZ_geo_dTH
  ENDIF

END SUBROUTINE plasma_state_copy_1
