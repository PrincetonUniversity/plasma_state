! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_copy_9(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)


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
     ss2%id_velpphi_tglf = 0
  ENDIF
  IF((ALLOCATED(ss%velpphi_tglf).AND.ALLOCATED(ss2%velpphi_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velpphi_tglf = ss%velpphi_tglf
     IF(.not.partial_copy) THEN
        ss2%id_velpphi_tglf = ss%id_velpphi_tglf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velte_anom = 0
  ENDIF
  IF((ALLOCATED(ss%velte_anom).AND.ALLOCATED(ss2%velte_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velte_anom = ss%velte_anom
     IF(.not.partial_copy) THEN
        ss2%id_velte_anom = ss%id_velte_anom
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velte_glf = 0
  ENDIF
  IF((ALLOCATED(ss%velte_glf).AND.ALLOCATED(ss2%velte_glf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velte_glf = ss%velte_glf
     IF(.not.partial_copy) THEN
        ss2%id_velte_glf = ss%id_velte_glf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velte_gtcneo = 0
  ENDIF
  IF((ALLOCATED(ss%velte_gtcneo).AND.ALLOCATED(ss2%velte_gtcneo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velte_gtcneo = ss%velte_gtcneo
     IF(.not.partial_copy) THEN
        ss2%id_velte_gtcneo = ss%id_velte_gtcneo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velte_mmm71 = 0
  ENDIF
  IF((ALLOCATED(ss%velte_mmm71).AND.ALLOCATED(ss2%velte_mmm71)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velte_mmm71 = ss%velte_mmm71
     IF(.not.partial_copy) THEN
        ss2%id_velte_mmm71 = ss%id_velte_mmm71
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velte_nclass = 0
  ENDIF
  IF((ALLOCATED(ss%velte_nclass).AND.ALLOCATED(ss2%velte_nclass)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velte_nclass = ss%velte_nclass
     IF(.not.partial_copy) THEN
        ss2%id_velte_nclass = ss%id_velte_nclass
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velte_neo = 0
  ENDIF
  IF((ALLOCATED(ss%velte_neo).AND.ALLOCATED(ss2%velte_neo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velte_neo = ss%velte_neo
     IF(.not.partial_copy) THEN
        ss2%id_velte_neo = ss%id_velte_neo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velte_paleo = 0
  ENDIF
  IF((ALLOCATED(ss%velte_paleo).AND.ALLOCATED(ss2%velte_paleo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velte_paleo = ss%velte_paleo
     IF(.not.partial_copy) THEN
        ss2%id_velte_paleo = ss%id_velte_paleo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velte_tglf = 0
  ENDIF
  IF((ALLOCATED(ss%velte_tglf).AND.ALLOCATED(ss2%velte_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velte_tglf = ss%velte_tglf
     IF(.not.partial_copy) THEN
        ss2%id_velte_tglf = ss%id_velte_tglf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velti_anom = 0
  ENDIF
  IF((ALLOCATED(ss%velti_anom).AND.ALLOCATED(ss2%velti_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velti_anom = ss%velti_anom
     IF(.not.partial_copy) THEN
        ss2%id_velti_anom = ss%id_velti_anom
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velti_glf = 0
  ENDIF
  IF((ALLOCATED(ss%velti_glf).AND.ALLOCATED(ss2%velti_glf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velti_glf = ss%velti_glf
     IF(.not.partial_copy) THEN
        ss2%id_velti_glf = ss%id_velti_glf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velti_gtcneo = 0
  ENDIF
  IF((ALLOCATED(ss%velti_gtcneo).AND.ALLOCATED(ss2%velti_gtcneo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velti_gtcneo = ss%velti_gtcneo
     IF(.not.partial_copy) THEN
        ss2%id_velti_gtcneo = ss%id_velti_gtcneo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velti_mmm71 = 0
  ENDIF
  IF((ALLOCATED(ss%velti_mmm71).AND.ALLOCATED(ss2%velti_mmm71)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velti_mmm71 = ss%velti_mmm71
     IF(.not.partial_copy) THEN
        ss2%id_velti_mmm71 = ss%id_velti_mmm71
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velti_nclass = 0
  ENDIF
  IF((ALLOCATED(ss%velti_nclass).AND.ALLOCATED(ss2%velti_nclass)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velti_nclass = ss%velti_nclass
     IF(.not.partial_copy) THEN
        ss2%id_velti_nclass = ss%id_velti_nclass
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velti_neo = 0
  ENDIF
  IF((ALLOCATED(ss%velti_neo).AND.ALLOCATED(ss2%velti_neo)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velti_neo = ss%velti_neo
     IF(.not.partial_copy) THEN
        ss2%id_velti_neo = ss%id_velti_neo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_velti_tglf = 0
  ENDIF
  IF((ALLOCATED(ss%velti_tglf).AND.ALLOCATED(ss2%velti_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%velti_tglf = ss%velti_tglf
     IF(.not.partial_copy) THEN
        ss2%id_velti_tglf = ss%id_velti_tglf
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_vie_trans = 0
  ENDIF
  IF((ALLOCATED(ss%vie_trans).AND.ALLOCATED(ss2%vie_trans)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%vie_trans = ss%vie_trans
     IF(.not.partial_copy) THEN
        ss2%id_vie_trans = ss%id_vie_trans
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_vie_transcat)) THEN
     IF(partial_copy) THEN
        ss2%id_vie_transcat = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%vie_transcat).AND.ALLOCATED(ss2%vie_transcat)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%vie_transcat = ss%vie_transcat
     IF(ALLOCATED(ss2%id_vie_transcat)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_vie_transcat = ss%id_vie_transcat
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_vmo_trans = 0
  ENDIF
  IF((ALLOCATED(ss%vmo_trans).AND.ALLOCATED(ss2%vmo_trans)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%vmo_trans = ss%vmo_trans
     IF(.not.partial_copy) THEN
        ss2%id_vmo_trans = ss%id_vmo_trans
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_vmo_transcat)) THEN
     IF(partial_copy) THEN
        ss2%id_vmo_transcat = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%vmo_transcat).AND.ALLOCATED(ss2%vmo_transcat)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%vmo_transcat = ss%vmo_transcat
     IF(ALLOCATED(ss2%id_vmo_transcat)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_vmo_transcat = ss%id_vmo_transcat
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_vn_trans)) THEN
     IF(partial_copy) THEN
        ss2%id_vn_trans = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%vn_trans).AND.ALLOCATED(ss2%vn_trans)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%vn_trans = ss%vn_trans
     IF(ALLOCATED(ss2%id_vn_trans)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_vn_trans = ss%id_vn_trans
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_vn_transcat)) THEN
     IF(partial_copy) THEN
        ss2%id_vn_transcat = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%vn_transcat).AND.ALLOCATED(ss2%vn_transcat)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%vn_transcat = ss%vn_transcat
     IF(ALLOCATED(ss2%id_vn_transcat)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_vn_transcat = ss%id_vn_transcat
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_vol = 0
  ENDIF
  IF((ALLOCATED(ss%vol).AND.ALLOCATED(ss2%vol)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%vol = ss%vol
     IF(.not.partial_copy) THEN
        ss2%id_vol = ss%id_vol
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%vphi0_av)).AND.(ALLOCATED(ss%vphi0_av))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_GAS).EQ.1)) ) THEN
     ss2%vphi0_av = ss%vphi0_av
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%vpol_Data_Info = ss%vpol_Data_Info
  ENDIF
  IF(ALLOCATED(ss2%id_vpol_inmp)) THEN
     IF(partial_copy) THEN
        ss2%id_vpol_inmp = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%vpol_inmp).AND.ALLOCATED(ss2%vpol_inmp)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%vpol_inmp = ss%vpol_inmp
     IF(ALLOCATED(ss2%id_vpol_inmp)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_vpol_inmp = ss%id_vpol_inmp
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%vpol_is_input)).AND.(ALLOCATED(ss%vpol_is_input))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_PLASMA).EQ.1)) ) THEN
     ss2%vpol_is_input = ss%vpol_is_input
  ENDIF
  IF(ALLOCATED(ss2%id_vpol_omp)) THEN
     IF(partial_copy) THEN
        ss2%id_vpol_omp = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%vpol_omp).AND.ALLOCATED(ss2%vpol_omp)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%vpol_omp = ss%vpol_omp
     IF(ALLOCATED(ss2%id_vpol_omp)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_vpol_omp = ss%id_vpol_omp
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%VRep_coil_count)).AND.(ALLOCATED(ss%VRep_coil_count))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%VRep_coil_count = ss%VRep_coil_count
  ENDIF
  IF( ((ALLOCATED(ss2%Vrep_coil_spacing)).AND.(ALLOCATED(ss%Vrep_coil_spacing))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%Vrep_coil_spacing = ss%Vrep_coil_spacing
  ENDIF
  IF( ((ALLOCATED(ss2%Vsize_coil)).AND.(ALLOCATED(ss%Vsize_coil))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%Vsize_coil = ss%Vsize_coil
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%vsur = ss%vsur
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%VSUR_Data_Info = ss%VSUR_Data_Info
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%vtor_Data_Info = ss%vtor_Data_Info
  ENDIF
  IF(ALLOCATED(ss2%id_vtor_inmp)) THEN
     IF(partial_copy) THEN
        ss2%id_vtor_inmp = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%vtor_inmp).AND.ALLOCATED(ss2%vtor_inmp)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%vtor_inmp = ss%vtor_inmp
     IF(ALLOCATED(ss2%id_vtor_inmp)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_vtor_inmp = ss%id_vtor_inmp
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%vtor_is_input)).AND.(ALLOCATED(ss%vtor_is_input))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_PLASMA).EQ.1)) ) THEN
     ss2%vtor_is_input = ss%vtor_is_input
  ENDIF
  IF(ALLOCATED(ss2%id_vtor_omp)) THEN
     IF(partial_copy) THEN
        ss2%id_vtor_omp = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%vtor_omp).AND.ALLOCATED(ss2%vtor_omp)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%vtor_omp = ss%vtor_omp
     IF(ALLOCATED(ss2%id_vtor_omp)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_vtor_omp = ss%id_vtor_omp
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_V_loop = 0
  ENDIF
  IF((ALLOCATED(ss%V_loop).AND.ALLOCATED(ss2%V_loop)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%V_loop = ss%V_loop
     IF(.not.partial_copy) THEN
        ss2%id_V_loop = ss%id_V_loop
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_v_pars)) THEN
     IF(partial_copy) THEN
        ss2%id_v_pars = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%v_pars).AND.ALLOCATED(ss2%v_pars)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%v_pars = ss%v_pars
     IF(ALLOCATED(ss2%id_v_pars)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_v_pars = ss%id_v_pars
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_v_pers)) THEN
     IF(partial_copy) THEN
        ss2%id_v_pers = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%v_pers).AND.ALLOCATED(ss2%v_pers)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%v_pers = ss%v_pers
     IF(ALLOCATED(ss2%id_v_pers)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_v_pers = ss%id_v_pers
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%wt_nphi)).AND.(ALLOCATED(ss%wt_nphi))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%wt_nphi = ss%wt_nphi
  ENDIF
  IF( ((ALLOCATED(ss2%wt_nphi_abs)).AND.(ALLOCATED(ss%wt_nphi_abs))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%wt_nphi_abs = ss%wt_nphi_abs
  ENDIF
  IF( ((ALLOCATED(ss2%wt_nphi_ext)).AND.(ALLOCATED(ss%wt_nphi_ext))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%wt_nphi_ext = ss%wt_nphi_ext
  ENDIF
  IF(ALLOCATED(ss2%id_xRjcos_momeq)) THEN
     IF(partial_copy) THEN
        ss2%id_xRjcos_momeq = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%xRjcos_momeq).AND.ALLOCATED(ss2%xRjcos_momeq)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%xRjcos_momeq = ss%xRjcos_momeq
     IF(ALLOCATED(ss2%id_xRjcos_momeq)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_xRjcos_momeq = ss%id_xRjcos_momeq
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_xRjsin_momeq)) THEN
     IF(partial_copy) THEN
        ss2%id_xRjsin_momeq = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%xRjsin_momeq).AND.ALLOCATED(ss2%xRjsin_momeq)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%xRjsin_momeq = ss%xRjsin_momeq
     IF(ALLOCATED(ss2%id_xRjsin_momeq)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_xRjsin_momeq = ss%id_xRjsin_momeq
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%xs)).AND.(ALLOCATED(ss%xs))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_RAD).EQ.1)) ) THEN
     ss2%xs = ss%xs
  ENDIF
  IF( ((ALLOCATED(ss2%xs_name)).AND.(ALLOCATED(ss%xs_name))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_RAD).EQ.1)) ) THEN
     ss2%xs_name = ss%xs_name
  ENDIF
  IF(ALLOCATED(ss2%id_xZjcos_momeq)) THEN
     IF(partial_copy) THEN
        ss2%id_xZjcos_momeq = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%xZjcos_momeq).AND.ALLOCATED(ss2%xZjcos_momeq)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%xZjcos_momeq = ss%xZjcos_momeq
     IF(ALLOCATED(ss2%id_xZjcos_momeq)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_xZjcos_momeq = ss%id_xZjcos_momeq
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_xZjsin_momeq)) THEN
     IF(partial_copy) THEN
        ss2%id_xZjsin_momeq = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%xZjsin_momeq).AND.ALLOCATED(ss2%xZjsin_momeq)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%xZjsin_momeq = ss%xZjsin_momeq
     IF(ALLOCATED(ss2%id_xZjsin_momeq)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_xZjsin_momeq = ss%id_xZjsin_momeq
        ENDIF
     ENDIF
  ENDIF
  ss2%Z0max = ss%Z0max
  IF(partial_copy) THEN
     ss2%id_Z0_momeq = 0
  ENDIF
  IF((ALLOCATED(ss%Z0_momeq).AND.ALLOCATED(ss2%Z0_momeq)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%Z0_momeq = ss%Z0_momeq
     IF(.not.partial_copy) THEN
        ss2%id_Z0_momeq = ss%id_Z0_momeq
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%Zbap)).AND.(ALLOCATED(ss%Zbap))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%Zbap = ss%Zbap
  ENDIF
  IF( ((ALLOCATED(ss2%Zbsc)).AND.(ALLOCATED(ss%Zbsc))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%Zbsc = ss%Zbsc
  ENDIF
  IF(partial_copy) THEN
     ss2%id_Zeff = 0
  ENDIF
  IF((ALLOCATED(ss%Zeff).AND.ALLOCATED(ss2%Zeff)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%Zeff = ss%Zeff
     IF(.not.partial_copy) THEN
        ss2%id_Zeff = ss%id_Zeff
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%ZEFF_Data_Info = ss%ZEFF_Data_Info
  ENDIF
  IF(partial_copy) THEN
     ss2%id_Zeff_fi = 0
  ENDIF
  IF((ALLOCATED(ss%Zeff_fi).AND.ALLOCATED(ss2%Zeff_fi)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%Zeff_fi = ss%Zeff_fi
     IF(.not.partial_copy) THEN
        ss2%id_Zeff_fi = ss%id_Zeff_fi
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_Zeff_th = 0
  ENDIF
  IF((ALLOCATED(ss%Zeff_th).AND.ALLOCATED(ss2%Zeff_th)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%Zeff_th = ss%Zeff_th
     IF(.not.partial_copy) THEN
        ss2%id_Zeff_th = ss%id_Zeff_th
     ENDIF
  ENDIF
  ss2%Zimp1 = ss%Zimp1
  ss2%Zimp2 = ss%Zimp2
  IF( ((ALLOCATED(ss2%zlim)).AND.(ALLOCATED(ss%zlim))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%zlim = ss%zlim
  ENDIF
  IF( ((ALLOCATED(ss2%Zloc_coil)).AND.(ALLOCATED(ss%Zloc_coil))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%Zloc_coil = ss%Zloc_coil
  ENDIF
  IF( ((ALLOCATED(ss2%Z_ant)).AND.(ALLOCATED(ss%Z_ant))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%Z_ant = ss%Z_ant
  ENDIF
  IF( ((ALLOCATED(ss2%Z_antgeo)).AND.(ALLOCATED(ss%Z_antgeo))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%Z_antgeo = ss%Z_antgeo
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%Z_axis = ss%Z_axis
  ENDIF
  IF( ((ALLOCATED(ss2%Z_EC_launch)).AND.(ALLOCATED(ss%Z_EC_launch))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%Z_EC_launch = ss%Z_EC_launch
  ENDIF
  IF(partial_copy) THEN
     ss2%id_Z_geo = 0
  ENDIF
  IF((ALLOCATED(ss%Z_geo).AND.ALLOCATED(ss2%Z_geo)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%Z_geo = ss%Z_geo
     IF(.not.partial_copy) THEN
        ss2%id_Z_geo = ss%id_Z_geo
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%Z_grid).AND.ALLOCATED(ss2%Z_grid)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%Z_grid = ss%Z_grid
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%Z_max_box = ss%Z_max_box
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%Z_max_lcfs = ss%Z_max_lcfs
  ENDIF
  IF(partial_copy) THEN
     ss2%id_Z_midp = 0
  ENDIF
  IF((ALLOCATED(ss%Z_midp).AND.ALLOCATED(ss2%Z_midp)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%Z_midp = ss%Z_midp
     IF(.not.partial_copy) THEN
        ss2%id_Z_midp = ss%id_Z_midp
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%Z_mid_ant)).AND.(ALLOCATED(ss%Z_mid_ant))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%Z_mid_ant = ss%Z_mid_ant
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%Z_min_box = ss%Z_min_box
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%Z_min_lcfs = ss%Z_min_lcfs
  ENDIF
  IF((ALLOCATED(ss%Z_ripple).AND.ALLOCATED(ss2%Z_ripple)).AND. &
        ((cclist(ps_cnum_RIPPLE).EQ.1))) THEN
     ss2%Z_ripple = ss%Z_ripple
  ENDIF
  IF(partial_copy) THEN
     ss2%id_Z_surfMax = 0
  ENDIF
  IF((ALLOCATED(ss%Z_surfMax).AND.ALLOCATED(ss2%Z_surfMax)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%Z_surfMax = ss%Z_surfMax
     IF(.not.partial_copy) THEN
        ss2%id_Z_surfMax = ss%id_Z_surfMax
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_Z_surfMin = 0
  ENDIF
  IF((ALLOCATED(ss%Z_surfMin).AND.ALLOCATED(ss2%Z_surfMin)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%Z_surfMin = ss%Z_surfMin
     IF(.not.partial_copy) THEN
        ss2%id_Z_surfMin = ss%id_Z_surfMin
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_copy_9
