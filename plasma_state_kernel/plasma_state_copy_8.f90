! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_copy_8(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)


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
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%t1 = ss%t1
  ENDIF
  IF(ALLOCATED(ss2%id_teflux_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_teflux_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%teflux_ky_tglf).AND.ALLOCATED(ss2%teflux_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%teflux_ky_tglf = ss%teflux_ky_tglf
     IF(ALLOCATED(ss2%id_teflux_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_teflux_ky_tglf = ss%id_teflux_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%Te_bdy = ss%Te_bdy
  ENDIF
  IF(ALLOCATED(ss2%id_te_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_te_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%te_ky_tglf).AND.ALLOCATED(ss2%te_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%te_ky_tglf = ss%te_ky_tglf
     IF(ALLOCATED(ss2%id_te_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_te_ky_tglf = ss%id_te_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%tfinal = ss%tfinal
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tfrip2_log = 0
  ENDIF
  IF((ALLOCATED(ss%tfrip2_log).AND.ALLOCATED(ss2%tfrip2_log)).AND. &
        ((cclist(ps_cnum_RIPPLE).EQ.1))) THEN
     ss2%tfrip2_log = ss%tfrip2_log
     IF(.not.partial_copy) THEN
        ss2%id_tfrip2_log = ss%id_tfrip2_log
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tfrip_log = 0
  ENDIF
  IF((ALLOCATED(ss%tfrip_log).AND.ALLOCATED(ss2%tfrip_log)).AND. &
        ((cclist(ps_cnum_RIPPLE).EQ.1))) THEN
     ss2%tfrip_log = ss%tfrip_log
     IF(.not.partial_copy) THEN
        ss2%id_tfrip_log = ss%id_tfrip_log
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%TF_Data_Info = ss%TF_Data_Info
  ENDIF
  IF( ((ALLOCATED(ss2%thegrilh)).AND.(ALLOCATED(ss%thegrilh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%thegrilh = ss%thegrilh
  ENDIF
  IF((ALLOCATED(ss%th_eq).AND.ALLOCATED(ss2%th_eq)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%th_eq = ss%th_eq
  ENDIF
  IF(partial_copy) THEN
     ss2%id_Ti = 0
  ENDIF
  IF((ALLOCATED(ss%Ti).AND.ALLOCATED(ss2%Ti)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%Ti = ss%Ti
     IF(.not.partial_copy) THEN
        ss2%id_Ti = ss%id_Ti
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_tiflux_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_tiflux_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%tiflux_ky_tglf).AND.ALLOCATED(ss2%tiflux_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%tiflux_ky_tglf = ss%tiflux_ky_tglf
     IF(ALLOCATED(ss2%id_tiflux_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_tiflux_ky_tglf = ss%id_tiflux_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%tinit = ss%tinit
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%Ti_bdy = ss%Ti_bdy
  ENDIF
  IF(ALLOCATED(ss2%id_ti_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_ti_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%ti_ky_tglf).AND.ALLOCATED(ss2%ti_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%ti_ky_tglf = ss%ti_ky_tglf
     IF(ALLOCATED(ss2%id_ti_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_ti_ky_tglf = ss%id_ti_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tmodel = 0
  ENDIF
  IF((ALLOCATED(ss%tmodel).AND.ALLOCATED(ss2%tmodel)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%tmodel = ss%tmodel
     IF(.not.partial_copy) THEN
        ss2%id_tmodel = ss%id_tmodel
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_ANOM).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%tmodel_bdy = ss%tmodel_bdy
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%tokamak_id = ss%tokamak_id
  ENDIF
  IF( ((ALLOCATED(ss2%Tornum_balcrit)).AND.(ALLOCATED(ss%Tornum_balcrit))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%Tornum_balcrit = ss%Tornum_balcrit
  ENDIF
  IF( ((ALLOCATED(ss2%Tor_mode_label)).AND.(ALLOCATED(ss%Tor_mode_label))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%Tor_mode_label = ss%Tor_mode_label
  ENDIF
  IF( ((ALLOCATED(ss2%Tor_mode_No)).AND.(ALLOCATED(ss%Tor_mode_No))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%Tor_mode_No = ss%Tor_mode_No
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tq0_reco = 0
  ENDIF
  IF((ALLOCATED(ss%tq0_reco).AND.ALLOCATED(ss2%tq0_reco)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%tq0_reco = ss%tq0_reco
     IF(.not.partial_copy) THEN
        ss2%id_tq0_reco = ss%id_tq0_reco
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tqb0_halo = 0
  ENDIF
  IF((ALLOCATED(ss%tqb0_halo).AND.ALLOCATED(ss2%tqb0_halo)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%tqb0_halo = ss%tqb0_halo
     IF(.not.partial_copy) THEN
        ss2%id_tqb0_halo = ss%id_tqb0_halo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tqbe = 0
  ENDIF
  IF((ALLOCATED(ss%tqbe).AND.ALLOCATED(ss2%tqbe)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%tqbe = ss%tqbe
     IF(.not.partial_copy) THEN
        ss2%id_tqbe = ss%id_tqbe
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tqbi = 0
  ENDIF
  IF((ALLOCATED(ss%tqbi).AND.ALLOCATED(ss2%tqbi)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%tqbi = ss%tqbi
     IF(.not.partial_copy) THEN
        ss2%id_tqbi = ss%id_tqbi
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tqbjxb = 0
  ENDIF
  IF((ALLOCATED(ss%tqbjxb).AND.ALLOCATED(ss2%tqbjxb)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%tqbjxb = ss%tqbjxb
     IF(.not.partial_copy) THEN
        ss2%id_tqbjxb = ss%id_tqbjxb
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tqbth = 0
  ENDIF
  IF((ALLOCATED(ss%tqbth).AND.ALLOCATED(ss2%tqbth)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%tqbth = ss%tqbth
     IF(.not.partial_copy) THEN
        ss2%id_tqbth = ss%id_tqbth
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tqcx_halo = 0
  ENDIF
  IF((ALLOCATED(ss%tqcx_halo).AND.ALLOCATED(ss2%tqcx_halo)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%tqcx_halo = ss%tqcx_halo
     IF(.not.partial_copy) THEN
        ss2%id_tqcx_halo = ss%id_tqcx_halo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tqcx_reco = 0
  ENDIF
  IF((ALLOCATED(ss%tqcx_reco).AND.ALLOCATED(ss2%tqcx_reco)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%tqcx_reco = ss%tqcx_reco
     IF(.not.partial_copy) THEN
        ss2%id_tqcx_reco = ss%id_tqcx_reco
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_tqioniz)) THEN
     IF(partial_copy) THEN
        ss2%id_tqioniz = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%tqioniz).AND.ALLOCATED(ss2%tqioniz)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%tqioniz = ss%tqioniz
     IF(ALLOCATED(ss2%id_tqioniz)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_tqioniz = ss%id_tqioniz
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_tqqcx)) THEN
     IF(partial_copy) THEN
        ss2%id_tqqcx = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%tqqcx).AND.ALLOCATED(ss2%tqqcx)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%tqqcx = ss%tqqcx
     IF(ALLOCATED(ss2%id_tqqcx)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_tqqcx = ss%id_tqqcx
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tqsc_halo = 0
  ENDIF
  IF((ALLOCATED(ss%tqsc_halo).AND.ALLOCATED(ss2%tqsc_halo)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%tqsc_halo = ss%tqsc_halo
     IF(.not.partial_copy) THEN
        ss2%id_tqsc_halo = ss%id_tqsc_halo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tqsc_reco = 0
  ENDIF
  IF((ALLOCATED(ss%tqsc_reco).AND.ALLOCATED(ss2%tqsc_reco)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%tqsc_reco = ss%tqsc_reco
     IF(.not.partial_copy) THEN
        ss2%id_tqsc_reco = ss%id_tqsc_reco
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tq_anom = 0
  ENDIF
  IF((ALLOCATED(ss%tq_anom).AND.ALLOCATED(ss2%tq_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%tq_anom = ss%tq_anom
     IF(.not.partial_copy) THEN
        ss2%id_tq_anom = ss%id_tq_anom
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_tq_trans = 0
  ENDIF
  IF((ALLOCATED(ss%tq_trans).AND.ALLOCATED(ss2%tq_trans)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%tq_trans = ss%tq_trans
     IF(.not.partial_copy) THEN
        ss2%id_tq_trans = ss%id_tq_trans
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_tq_transcat)) THEN
     IF(partial_copy) THEN
        ss2%id_tq_transcat = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%tq_transcat).AND.ALLOCATED(ss2%tq_transcat)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%tq_transcat = ss%tq_transcat
     IF(ALLOCATED(ss2%id_tq_transcat)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_tq_transcat = ss%id_tq_transcat
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%trace_flag)).AND.(ALLOCATED(ss%trace_flag))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%trace_flag = ss%trace_flag
  ENDIF
  IF( ((ALLOCATED(ss2%trcat)).AND.(ALLOCATED(ss%trcat))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_PLASMA).EQ.1)) ) THEN
     ss2%trcat = ss%trcat
  ENDIF
  IF(partial_copy) THEN
     ss2%id_triang = 0
  ENDIF
  IF((ALLOCATED(ss%triang).AND.ALLOCATED(ss2%triang)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%triang = ss%triang
     IF(.not.partial_copy) THEN
        ss2%id_triang = ss%id_triang
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_triangL = 0
  ENDIF
  IF((ALLOCATED(ss%triangL).AND.ALLOCATED(ss2%triangL)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%triangL = ss%triangL
     IF(.not.partial_copy) THEN
        ss2%id_triangL = ss%id_triangL
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_triangU = 0
  ENDIF
  IF((ALLOCATED(ss%triangU).AND.ALLOCATED(ss2%triangU)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%triangU = ss%triangU
     IF(.not.partial_copy) THEN
        ss2%id_triangU = ss%id_triangU
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_triang_miller_L = 0
  ENDIF
  IF((ALLOCATED(ss%triang_miller_L).AND.ALLOCATED(ss2%triang_miller_L)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%triang_miller_L = ss%triang_miller_L
     IF(.not.partial_copy) THEN
        ss2%id_triang_miller_L = ss%id_triang_miller_L
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_triang_miller_U = 0
  ENDIF
  IF((ALLOCATED(ss%triang_miller_U).AND.ALLOCATED(ss2%triang_miller_U)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%triang_miller_U = ss%triang_miller_U
     IF(.not.partial_copy) THEN
        ss2%id_triang_miller_U = ss%id_triang_miller_U
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%trmodels)).AND.(ALLOCATED(ss%trmodels))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_PLASMA).EQ.1)) ) THEN
     ss2%trmodels = ss%trmodels
  ENDIF
  IF(ALLOCATED(ss2%id_Ts)) THEN
     IF(partial_copy) THEN
        ss2%id_Ts = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%Ts).AND.ALLOCATED(ss2%Ts)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%Ts = ss%Ts
     IF(ALLOCATED(ss2%id_Ts)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_Ts = ss%id_Ts
        ENDIF
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%Ts_Data_Info = ss%Ts_Data_Info
  ENDIF
  IF( ((ALLOCATED(ss2%Ts_is_input)).AND.(ALLOCATED(ss%Ts_is_input))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_PLASMA).EQ.1)) ) THEN
     ss2%Ts_is_input = ss%Ts_is_input
  ENDIF
  IF(ALLOCATED(ss2%id_tzflux_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_tzflux_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%tzflux_ky_tglf).AND.ALLOCATED(ss2%tzflux_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%tzflux_ky_tglf = ss%tzflux_ky_tglf
     IF(ALLOCATED(ss2%id_tzflux_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_tzflux_ky_tglf = ss%id_tzflux_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_tz_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_tz_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%tz_ky_tglf).AND.ALLOCATED(ss2%tz_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%tz_ky_tglf = ss%tz_ky_tglf
     IF(ALLOCATED(ss2%id_tz_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_tz_ky_tglf = ss%id_tz_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%T_einj_standard)).AND.(ALLOCATED(ss%T_einj_standard))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%T_einj_standard = ss%T_einj_standard
  ENDIF
  IF( ((ALLOCATED(ss2%T_ffull_standard)).AND.(ALLOCATED(ss%T_ffull_standard))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%T_ffull_standard = ss%T_ffull_standard
  ENDIF
  IF( ((ALLOCATED(ss2%T_fhalf_standard)).AND.(ALLOCATED(ss%T_fhalf_standard))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%T_fhalf_standard = ss%T_fhalf_standard
  ENDIF
  IF(ALLOCATED(ss2%id_upwind_pfrac_ns)) THEN
     IF(partial_copy) THEN
        ss2%id_upwind_pfrac_ns = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%upwind_pfrac_ns).AND.ALLOCATED(ss2%upwind_pfrac_ns)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%upwind_pfrac_ns = ss%upwind_pfrac_ns
     IF(ALLOCATED(ss2%id_upwind_pfrac_ns)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_upwind_pfrac_ns = ss%id_upwind_pfrac_ns
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_upwind_pfrac_omega = 0
  ENDIF
  IF((ALLOCATED(ss%upwind_pfrac_omega).AND.ALLOCATED(ss2%upwind_pfrac_omega)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%upwind_pfrac_omega = ss%upwind_pfrac_omega
     IF(.not.partial_copy) THEN
        ss2%id_upwind_pfrac_omega = ss%id_upwind_pfrac_omega
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_upwind_pfrac_Te = 0
  ENDIF
  IF((ALLOCATED(ss%upwind_pfrac_Te).AND.ALLOCATED(ss2%upwind_pfrac_Te)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%upwind_pfrac_Te = ss%upwind_pfrac_Te
     IF(.not.partial_copy) THEN
        ss2%id_upwind_pfrac_Te = ss%id_upwind_pfrac_Te
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_upwind_pfrac_Ti = 0
  ENDIF
  IF((ALLOCATED(ss%upwind_pfrac_Ti).AND.ALLOCATED(ss2%upwind_pfrac_Ti)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%upwind_pfrac_Ti = ss%upwind_pfrac_Ti
     IF(.not.partial_copy) THEN
        ss2%id_upwind_pfrac_Ti = ss%id_upwind_pfrac_Ti
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_vee_trans = 0
  ENDIF
  IF((ALLOCATED(ss%vee_trans).AND.ALLOCATED(ss2%vee_trans)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%vee_trans = ss%vee_trans
     IF(.not.partial_copy) THEN
        ss2%id_vee_trans = ss%id_vee_trans
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_vee_transcat)) THEN
     IF(partial_copy) THEN
        ss2%id_vee_transcat = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%vee_transcat).AND.ALLOCATED(ss2%vee_transcat)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%vee_transcat = ss%vee_transcat
     IF(ALLOCATED(ss2%id_vee_transcat)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_vee_transcat = ss%id_vee_transcat
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velb_fusi = 0
  ENDIF
  IF((ALLOCATED(ss%velb_fusi).AND.ALLOCATED(ss2%velb_fusi)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velb_fusi = ss%velb_fusi
     IF(.not.partial_copy) THEN
        ss2%id_velb_fusi = ss%id_velb_fusi
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velb_nbi = 0
  ENDIF
  IF((ALLOCATED(ss%velb_nbi).AND.ALLOCATED(ss2%velb_nbi)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velb_nbi = ss%velb_nbi
     IF(.not.partial_copy) THEN
        ss2%id_velb_nbi = ss%id_velb_nbi
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velb_rfmi = 0
  ENDIF
  IF((ALLOCATED(ss%velb_rfmi).AND.ALLOCATED(ss2%velb_rfmi)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velb_rfmi = ss%velb_rfmi
     IF(.not.partial_copy) THEN
        ss2%id_velb_rfmi = ss%id_velb_rfmi
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velne_nclass = 0
  ENDIF
  IF((ALLOCATED(ss%velne_nclass).AND.ALLOCATED(ss2%velne_nclass)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velne_nclass = ss%velne_nclass
     IF(.not.partial_copy) THEN
        ss2%id_velne_nclass = ss%id_velne_nclass
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velnh_nclass = 0
  ENDIF
  IF((ALLOCATED(ss%velnh_nclass).AND.ALLOCATED(ss2%velnh_nclass)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velnh_nclass = ss%velnh_nclass
     IF(.not.partial_copy) THEN
        ss2%id_velnh_nclass = ss%id_velnh_nclass
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velni_drbm = 0
  ENDIF
  IF((ALLOCATED(ss%velni_drbm).AND.ALLOCATED(ss2%velni_drbm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velni_drbm = ss%velni_drbm
     IF(.not.partial_copy) THEN
        ss2%id_velni_drbm = ss%id_velni_drbm
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_velns_anom)) THEN
     IF(partial_copy) THEN
        ss2%id_velns_anom = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%velns_anom).AND.ALLOCATED(ss2%velns_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velns_anom = ss%velns_anom
     IF(ALLOCATED(ss2%id_velns_anom)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_velns_anom = ss%id_velns_anom
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_velns_glf)) THEN
     IF(partial_copy) THEN
        ss2%id_velns_glf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%velns_glf).AND.ALLOCATED(ss2%velns_glf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velns_glf = ss%velns_glf
     IF(ALLOCATED(ss2%id_velns_glf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_velns_glf = ss%id_velns_glf
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_velns_gtcneo)) THEN
     IF(partial_copy) THEN
        ss2%id_velns_gtcneo = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%velns_gtcneo).AND.ALLOCATED(ss2%velns_gtcneo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velns_gtcneo = ss%velns_gtcneo
     IF(ALLOCATED(ss2%id_velns_gtcneo)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_velns_gtcneo = ss%id_velns_gtcneo
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_velns_mmm71)) THEN
     IF(partial_copy) THEN
        ss2%id_velns_mmm71 = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%velns_mmm71).AND.ALLOCATED(ss2%velns_mmm71)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velns_mmm71 = ss%velns_mmm71
     IF(ALLOCATED(ss2%id_velns_mmm71)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_velns_mmm71 = ss%id_velns_mmm71
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_velns_nclass)) THEN
     IF(partial_copy) THEN
        ss2%id_velns_nclass = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%velns_nclass).AND.ALLOCATED(ss2%velns_nclass)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velns_nclass = ss%velns_nclass
     IF(ALLOCATED(ss2%id_velns_nclass)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_velns_nclass = ss%id_velns_nclass
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_velns_neo)) THEN
     IF(partial_copy) THEN
        ss2%id_velns_neo = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%velns_neo).AND.ALLOCATED(ss2%velns_neo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velns_neo = ss%velns_neo
     IF(ALLOCATED(ss2%id_velns_neo)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_velns_neo = ss%id_velns_neo
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_velns_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_velns_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%velns_tglf).AND.ALLOCATED(ss2%velns_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velns_tglf = ss%velns_tglf
     IF(ALLOCATED(ss2%id_velns_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_velns_tglf = ss%id_velns_tglf
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velnx_nclass = 0
  ENDIF
  IF((ALLOCATED(ss%velnx_nclass).AND.ALLOCATED(ss2%velnx_nclass)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velnx_nclass = ss%velnx_nclass
     IF(.not.partial_copy) THEN
        ss2%id_velnx_nclass = ss%id_velnx_nclass
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velpphi_anom = 0
  ENDIF
  IF((ALLOCATED(ss%velpphi_anom).AND.ALLOCATED(ss2%velpphi_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velpphi_anom = ss%velpphi_anom
     IF(.not.partial_copy) THEN
        ss2%id_velpphi_anom = ss%id_velpphi_anom
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velpphi_glf = 0
  ENDIF
  IF((ALLOCATED(ss%velpphi_glf).AND.ALLOCATED(ss2%velpphi_glf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velpphi_glf = ss%velpphi_glf
     IF(.not.partial_copy) THEN
        ss2%id_velpphi_glf = ss%id_velpphi_glf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velpphi_gtcneo = 0
  ENDIF
  IF((ALLOCATED(ss%velpphi_gtcneo).AND.ALLOCATED(ss2%velpphi_gtcneo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velpphi_gtcneo = ss%velpphi_gtcneo
     IF(.not.partial_copy) THEN
        ss2%id_velpphi_gtcneo = ss%id_velpphi_gtcneo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velpphi_mmm71 = 0
  ENDIF
  IF((ALLOCATED(ss%velpphi_mmm71).AND.ALLOCATED(ss2%velpphi_mmm71)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velpphi_mmm71 = ss%velpphi_mmm71
     IF(.not.partial_copy) THEN
        ss2%id_velpphi_mmm71 = ss%id_velpphi_mmm71
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velpphi_neo = 0
  ENDIF
  IF((ALLOCATED(ss%velpphi_neo).AND.ALLOCATED(ss2%velpphi_neo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velpphi_neo = ss%velpphi_neo
     IF(.not.partial_copy) THEN
        ss2%id_velpphi_neo = ss%id_velpphi_neo
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_copy_8
