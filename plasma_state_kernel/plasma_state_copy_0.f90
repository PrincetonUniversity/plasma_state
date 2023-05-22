! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_copy_0(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)


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
  IF((ALLOCATED(ss2%alla_index)).AND.(ALLOCATED(ss%alla_index))) THEN
     ss2%alla_index = ss%alla_index
  ENDIF
  IF((ALLOCATED(ss2%ALLA_name)).AND.(ALLOCATED(ss%ALLA_name))) THEN
     ss2%ALLA_name = ss%ALLA_name
  ENDIF
  IF((ALLOCATED(ss2%ALLA_type)).AND.(ALLOCATED(ss%ALLA_type))) THEN
     ss2%ALLA_type = ss%ALLA_type
  ENDIF
  IF((ALLOCATED(ss2%all_index)).AND.(ALLOCATED(ss%all_index))) THEN
     ss2%all_index = ss%all_index
  ENDIF
  IF((ALLOCATED(ss2%ALL_name)).AND.(ALLOCATED(ss%ALL_name))) THEN
     ss2%ALL_name = ss%ALL_name
  ENDIF
  IF((ALLOCATED(ss2%ALL_type)).AND.(ALLOCATED(ss%ALL_type))) THEN
     ss2%ALL_type = ss%ALL_type
  ENDIF
  IF( ((ALLOCATED(ss2%amaxlh)).AND.(ALLOCATED(ss%amaxlh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%amaxlh = ss%amaxlh
  ENDIF
  IF( ((ALLOCATED(ss2%aminlh)).AND.(ALLOCATED(ss%aminlh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%aminlh = ss%aminlh
  ENDIF
  IF( ((ALLOCATED(ss2%Ang_Href_coil)).AND.(ALLOCATED(ss%Ang_Href_coil))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%Ang_Href_coil = ss%Ang_Href_coil
  ENDIF
  IF( ((ALLOCATED(ss2%Ang_Vref_coil)).AND.(ALLOCATED(ss%Ang_Vref_coil))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%Ang_Vref_coil = ss%Ang_Vref_coil
  ENDIF
  IF((cclist(ps_cnum_ANOM).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%ANOM_Code_Info = ss%ANOM_Code_Info
  ENDIF
  IF(partial_copy) THEN
     ss2%id_anom_evar = 0
  ENDIF
  IF((ALLOCATED(ss%anom_evar).AND.ALLOCATED(ss2%anom_evar)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%anom_evar = ss%anom_evar
     IF(.not.partial_copy) THEN
        ss2%id_anom_evar = ss%id_anom_evar
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%ant_model)).AND.(ALLOCATED(ss%ant_model))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%ant_model = ss%ant_model
  ENDIF
  IF( ((ALLOCATED(ss2%ap2_halfheight)).AND.(ALLOCATED(ss%ap2_halfheight))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%ap2_halfheight = ss%ap2_halfheight
  ENDIF
  IF( ((ALLOCATED(ss2%ap2_halfwidth)).AND.(ALLOCATED(ss%ap2_halfwidth))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%ap2_halfwidth = ss%ap2_halfwidth
  ENDIF
  IF( ((ALLOCATED(ss2%ap2_horiz_offset)).AND.(ALLOCATED(ss%ap2_horiz_offset))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%ap2_horiz_offset = ss%ap2_horiz_offset
  ENDIF
  IF( ((ALLOCATED(ss2%ap2_vert_offset)).AND.(ALLOCATED(ss%ap2_vert_offset))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%ap2_vert_offset = ss%ap2_vert_offset
  ENDIF
  IF( ((ALLOCATED(ss2%apolmaxlh)).AND.(ALLOCATED(ss%apolmaxlh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%apolmaxlh = ss%apolmaxlh
  ENDIF
  IF( ((ALLOCATED(ss2%apolminlh)).AND.(ALLOCATED(ss%apolminlh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%apolminlh = ss%apolminlh
  ENDIF
  IF( ((ALLOCATED(ss2%ap_halfheight)).AND.(ALLOCATED(ss%ap_halfheight))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%ap_halfheight = ss%ap_halfheight
  ENDIF
  IF( ((ALLOCATED(ss2%ap_halfwidth)).AND.(ALLOCATED(ss%ap_halfwidth))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%ap_halfwidth = ss%ap_halfwidth
  ENDIF
  IF( ((ALLOCATED(ss2%ap_horiz_offset)).AND.(ALLOCATED(ss%ap_horiz_offset))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%ap_horiz_offset = ss%ap_horiz_offset
  ENDIF
  IF( ((ALLOCATED(ss2%ap_vert_offset)).AND.(ALLOCATED(ss%ap_vert_offset))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%ap_vert_offset = ss%ap_vert_offset
  ENDIF
  IF(partial_copy) THEN
     ss2%id_area = 0
  ENDIF
  IF((ALLOCATED(ss%area).AND.ALLOCATED(ss2%area)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%area = ss%area
     IF(.not.partial_copy) THEN
        ss2%id_area = ss%id_area
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%atormaxlh)).AND.(ALLOCATED(ss%atormaxlh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%atormaxlh = ss%atormaxlh
  ENDIF
  IF( ((ALLOCATED(ss2%atorminlh)).AND.(ALLOCATED(ss%atorminlh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%atorminlh = ss%atorminlh
  ENDIF
  IF( ((ALLOCATED(ss2%Balloon_code)).AND.(ALLOCATED(ss%Balloon_code))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%Balloon_code = ss%Balloon_code
  ENDIF
  IF( ((ALLOCATED(ss2%Balloon_mapper)).AND.(ALLOCATED(ss%Balloon_mapper))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%Balloon_mapper = ss%Balloon_mapper
  ENDIF
  IF( ((ALLOCATED(ss2%Balloon_stable)).AND.(ALLOCATED(ss%Balloon_stable))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%Balloon_stable = ss%Balloon_stable
  ENDIF
  IF( ((ALLOCATED(ss2%Balloon_status)).AND.(ALLOCATED(ss%Balloon_status))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%Balloon_status = ss%Balloon_status
  ENDIF
  IF(partial_copy) THEN
     ss2%id_balstab = 0
  ENDIF
  IF((ALLOCATED(ss%balstab).AND.ALLOCATED(ss2%balstab)).AND. &
        ((cclist(ps_cnum_LMHD).EQ.1))) THEN
     ss2%balstab = ss%balstab
     IF(.not.partial_copy) THEN
        ss2%id_balstab = ss%id_balstab
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%beam_type)).AND.(ALLOCATED(ss%beam_type))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%beam_type = ss%beam_type
  ENDIF
  IF(ALLOCATED(ss2%id_bidiff_bpass_co)) THEN
     IF(partial_copy) THEN
        ss2%id_bidiff_bpass_co = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%bidiff_bpass_co).AND.ALLOCATED(ss2%bidiff_bpass_co)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%bidiff_bpass_co = ss%bidiff_bpass_co
     IF(ALLOCATED(ss2%id_bidiff_bpass_co)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_bidiff_bpass_co = ss%id_bidiff_bpass_co
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_bidiff_bpass_ctr)) THEN
     IF(partial_copy) THEN
        ss2%id_bidiff_bpass_ctr = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%bidiff_bpass_ctr).AND.ALLOCATED(ss2%bidiff_bpass_ctr)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%bidiff_bpass_ctr = ss%bidiff_bpass_ctr
     IF(ALLOCATED(ss2%id_bidiff_bpass_ctr)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_bidiff_bpass_ctr = ss%id_bidiff_bpass_ctr
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_bidiff_btrap)) THEN
     IF(partial_copy) THEN
        ss2%id_bidiff_btrap = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%bidiff_btrap).AND.ALLOCATED(ss2%bidiff_btrap)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%bidiff_btrap = ss%bidiff_btrap
     IF(ALLOCATED(ss2%id_bidiff_btrap)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_bidiff_btrap = ss%id_bidiff_btrap
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_bidiff_dpass_co)) THEN
     IF(partial_copy) THEN
        ss2%id_bidiff_dpass_co = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%bidiff_dpass_co).AND.ALLOCATED(ss2%bidiff_dpass_co)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%bidiff_dpass_co = ss%bidiff_dpass_co
     IF(ALLOCATED(ss2%id_bidiff_dpass_co)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_bidiff_dpass_co = ss%id_bidiff_dpass_co
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_bidiff_dpass_ctr)) THEN
     IF(partial_copy) THEN
        ss2%id_bidiff_dpass_ctr = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%bidiff_dpass_ctr).AND.ALLOCATED(ss2%bidiff_dpass_ctr)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%bidiff_dpass_ctr = ss%bidiff_dpass_ctr
     IF(ALLOCATED(ss2%id_bidiff_dpass_ctr)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_bidiff_dpass_ctr = ss%id_bidiff_dpass_ctr
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_bidiff_dtrap)) THEN
     IF(partial_copy) THEN
        ss2%id_bidiff_dtrap = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%bidiff_dtrap).AND.ALLOCATED(ss2%bidiff_dtrap)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%bidiff_dtrap = ss%bidiff_dtrap
     IF(ALLOCATED(ss2%id_bidiff_dtrap)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_bidiff_dtrap = ss%id_bidiff_dtrap
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_BphiRZ = 0
  ENDIF
  IF((ALLOCATED(ss%BphiRZ).AND.ALLOCATED(ss2%BphiRZ)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%BphiRZ = ss%BphiRZ
     IF(.not.partial_copy) THEN
        ss2%id_BphiRZ = ss%id_BphiRZ
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_BRRZ = 0
  ENDIF
  IF((ALLOCATED(ss%BRRZ).AND.ALLOCATED(ss2%BRRZ)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%BRRZ = ss%BRRZ
     IF(.not.partial_copy) THEN
        ss2%id_BRRZ = ss%id_BRRZ
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_BZRZ = 0
  ENDIF
  IF((ALLOCATED(ss%BZRZ).AND.ALLOCATED(ss2%BZRZ)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%BZRZ = ss%BZRZ
     IF(.not.partial_copy) THEN
        ss2%id_BZRZ = ss%id_BZRZ
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%B_axis = ss%B_axis
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%B_axis_vac = ss%B_axis_vac
  ENDIF
  IF( ((ALLOCATED(ss2%b_halfHeight)).AND.(ALLOCATED(ss%b_halfHeight))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%b_halfHeight = ss%b_halfHeight
  ENDIF
  IF( ((ALLOCATED(ss2%b_halfwidth)).AND.(ALLOCATED(ss%b_halfwidth))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%b_halfwidth = ss%b_halfwidth
  ENDIF
  IF( ((ALLOCATED(ss2%b_Hdivergence)).AND.(ALLOCATED(ss%b_Hdivergence))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%b_Hdivergence = ss%b_Hdivergence
  ENDIF
  IF( ((ALLOCATED(ss2%b_Hfocal_length)).AND.(ALLOCATED(ss%b_Hfocal_length))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%b_Hfocal_length = ss%b_Hfocal_length
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%B_max_lcfs = ss%B_max_lcfs
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%B_min_lcfs = ss%B_min_lcfs
  ENDIF
  IF(partial_copy) THEN
     ss2%id_B_surfMax = 0
  ENDIF
  IF((ALLOCATED(ss%B_surfMax).AND.ALLOCATED(ss2%B_surfMax)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%B_surfMax = ss%B_surfMax
     IF(.not.partial_copy) THEN
        ss2%id_B_surfMax = ss%id_B_surfMax
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_B_surfMin = 0
  ENDIF
  IF((ALLOCATED(ss%B_surfMin).AND.ALLOCATED(ss2%B_surfMin)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%B_surfMin = ss%B_surfMin
     IF(.not.partial_copy) THEN
        ss2%id_B_surfMin = ss%id_B_surfMin
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%b_Vdivergence)).AND.(ALLOCATED(ss%b_Vdivergence))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%b_Vdivergence = ss%b_Vdivergence
  ENDIF
  IF( ((ALLOCATED(ss2%b_Vfocal_length)).AND.(ALLOCATED(ss%b_Vfocal_length))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%b_Vfocal_length = ss%b_Vfocal_length
  ENDIF
  IF(ALLOCATED(ss2%id_cdicrf)) THEN
     IF(partial_copy) THEN
        ss2%id_cdicrf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%cdicrf).AND.ALLOCATED(ss2%cdicrf)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%cdicrf = ss%cdicrf
     IF(ALLOCATED(ss2%id_cdicrf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_cdicrf = ss%id_cdicrf
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_cdicrf_nphi)) THEN
     IF(partial_copy) THEN
        ss2%id_cdicrf_nphi = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%cdicrf_nphi).AND.ALLOCATED(ss2%cdicrf_nphi)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%cdicrf_nphi = ss%cdicrf_nphi
     IF(ALLOCATED(ss2%id_cdicrf_nphi)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_cdicrf_nphi = ss%id_cdicrf_nphi
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_anom = 0
  ENDIF
  IF((ALLOCATED(ss%chie_anom).AND.ALLOCATED(ss2%chie_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chie_anom = ss%chie_anom
     IF(.not.partial_copy) THEN
        ss2%id_chie_anom = ss%id_chie_anom
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_drbm = 0
  ENDIF
  IF((ALLOCATED(ss%chie_drbm).AND.ALLOCATED(ss2%chie_drbm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chie_drbm = ss%chie_drbm
     IF(.not.partial_copy) THEN
        ss2%id_chie_drbm = ss%id_chie_drbm
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_etg = 0
  ENDIF
  IF((ALLOCATED(ss%chie_etg).AND.ALLOCATED(ss2%chie_etg)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chie_etg = ss%chie_etg
     IF(.not.partial_copy) THEN
        ss2%id_chie_etg = ss%id_chie_etg
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_glf = 0
  ENDIF
  IF((ALLOCATED(ss%chie_glf).AND.ALLOCATED(ss2%chie_glf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chie_glf = ss%chie_glf
     IF(.not.partial_copy) THEN
        ss2%id_chie_glf = ss%id_chie_glf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_gtcneo = 0
  ENDIF
  IF((ALLOCATED(ss%chie_gtcneo).AND.ALLOCATED(ss2%chie_gtcneo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chie_gtcneo = ss%chie_gtcneo
     IF(.not.partial_copy) THEN
        ss2%id_chie_gtcneo = ss%id_chie_gtcneo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_mmm71 = 0
  ENDIF
  IF((ALLOCATED(ss%chie_mmm71).AND.ALLOCATED(ss2%chie_mmm71)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chie_mmm71 = ss%chie_mmm71
     IF(.not.partial_copy) THEN
        ss2%id_chie_mmm71 = ss%id_chie_mmm71
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_mtm = 0
  ENDIF
  IF((ALLOCATED(ss%chie_mtm).AND.ALLOCATED(ss2%chie_mtm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chie_mtm = ss%chie_mtm
     IF(.not.partial_copy) THEN
        ss2%id_chie_mtm = ss%id_chie_mtm
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_nclass = 0
  ENDIF
  IF((ALLOCATED(ss%chie_nclass).AND.ALLOCATED(ss2%chie_nclass)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chie_nclass = ss%chie_nclass
     IF(.not.partial_copy) THEN
        ss2%id_chie_nclass = ss%id_chie_nclass
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_neo = 0
  ENDIF
  IF((ALLOCATED(ss%chie_neo).AND.ALLOCATED(ss2%chie_neo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chie_neo = ss%chie_neo
     IF(.not.partial_copy) THEN
        ss2%id_chie_neo = ss%id_chie_neo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_ntm = 0
  ENDIF
  IF((ALLOCATED(ss%chie_ntm).AND.ALLOCATED(ss2%chie_ntm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chie_ntm = ss%chie_ntm
     IF(.not.partial_copy) THEN
        ss2%id_chie_ntm = ss%id_chie_ntm
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_paleo = 0
  ENDIF
  IF((ALLOCATED(ss%chie_paleo).AND.ALLOCATED(ss2%chie_paleo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chie_paleo = ss%chie_paleo
     IF(.not.partial_copy) THEN
        ss2%id_chie_paleo = ss%id_chie_paleo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_tglf = 0
  ENDIF
  IF((ALLOCATED(ss%chie_tglf).AND.ALLOCATED(ss2%chie_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chie_tglf = ss%chie_tglf
     IF(.not.partial_copy) THEN
        ss2%id_chie_tglf = ss%id_chie_tglf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_trans = 0
  ENDIF
  IF((ALLOCATED(ss%chie_trans).AND.ALLOCATED(ss2%chie_trans)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%chie_trans = ss%chie_trans
     IF(.not.partial_copy) THEN
        ss2%id_chie_trans = ss%id_chie_trans
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_chie_transcat)) THEN
     IF(partial_copy) THEN
        ss2%id_chie_transcat = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%chie_transcat).AND.ALLOCATED(ss2%chie_transcat)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%chie_transcat = ss%chie_transcat
     IF(ALLOCATED(ss2%id_chie_transcat)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_chie_transcat = ss%id_chie_transcat
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chie_w19 = 0
  ENDIF
  IF((ALLOCATED(ss%chie_w19).AND.ALLOCATED(ss2%chie_w19)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chie_w19 = ss%chie_w19
     IF(.not.partial_copy) THEN
        ss2%id_chie_w19 = ss%id_chie_w19
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chii_anom = 0
  ENDIF
  IF((ALLOCATED(ss%chii_anom).AND.ALLOCATED(ss2%chii_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chii_anom = ss%chii_anom
     IF(.not.partial_copy) THEN
        ss2%id_chii_anom = ss%id_chii_anom
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chii_drbm = 0
  ENDIF
  IF((ALLOCATED(ss%chii_drbm).AND.ALLOCATED(ss2%chii_drbm)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chii_drbm = ss%chii_drbm
     IF(.not.partial_copy) THEN
        ss2%id_chii_drbm = ss%id_chii_drbm
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chii_glf = 0
  ENDIF
  IF((ALLOCATED(ss%chii_glf).AND.ALLOCATED(ss2%chii_glf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chii_glf = ss%chii_glf
     IF(.not.partial_copy) THEN
        ss2%id_chii_glf = ss%id_chii_glf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chii_gtcneo = 0
  ENDIF
  IF((ALLOCATED(ss%chii_gtcneo).AND.ALLOCATED(ss2%chii_gtcneo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chii_gtcneo = ss%chii_gtcneo
     IF(.not.partial_copy) THEN
        ss2%id_chii_gtcneo = ss%id_chii_gtcneo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chii_mmm71 = 0
  ENDIF
  IF((ALLOCATED(ss%chii_mmm71).AND.ALLOCATED(ss2%chii_mmm71)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chii_mmm71 = ss%chii_mmm71
     IF(.not.partial_copy) THEN
        ss2%id_chii_mmm71 = ss%id_chii_mmm71
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chii_nclass = 0
  ENDIF
  IF((ALLOCATED(ss%chii_nclass).AND.ALLOCATED(ss2%chii_nclass)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chii_nclass = ss%chii_nclass
     IF(.not.partial_copy) THEN
        ss2%id_chii_nclass = ss%id_chii_nclass
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_chii_neo = 0
  ENDIF
  IF((ALLOCATED(ss%chii_neo).AND.ALLOCATED(ss2%chii_neo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%chii_neo = ss%chii_neo
     IF(.not.partial_copy) THEN
        ss2%id_chii_neo = ss%id_chii_neo
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_copy_0
