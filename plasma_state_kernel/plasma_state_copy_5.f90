! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_copy_5(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)


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
  IF( ((ALLOCATED(ss2%ns_bdy)).AND.(ALLOCATED(ss%ns_bdy))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_PLASMA).EQ.1)) ) THEN
     ss2%ns_bdy = ss%ns_bdy
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%ns_Data_Info = ss%ns_Data_Info
  ENDIF
  IF( ((ALLOCATED(ss2%ns_is_input)).AND.(ALLOCATED(ss%ns_is_input))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_PLASMA).EQ.1)) ) THEN
     ss2%ns_is_input = ss%ns_is_input
  ENDIF
  IF((cclist(ps_cnum_RIPPLE).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%ntf_coil2 = ss%ntf_coil2
  ENDIF
  IF((cclist(ps_cnum_RIPPLE).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%ntf_coils = ss%ntf_coils
  ENDIF
  IF( ((ALLOCATED(ss2%nthinlh)).AND.(ALLOCATED(ss%nthinlh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%nthinlh = ss%nthinlh
  ENDIF
  IF( ((ALLOCATED(ss2%Nturns)).AND.(ALLOCATED(ss%Nturns))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EQ).EQ.1)) ) THEN
     ss2%Nturns = ss%Nturns
  ENDIF
  IF( ((ALLOCATED(ss2%num_nphi)).AND.(ALLOCATED(ss%num_nphi))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%num_nphi = ss%num_nphi
  ENDIF
  IF( ((ALLOCATED(ss2%num_nphi_vac)).AND.(ALLOCATED(ss%num_nphi_vac))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%num_nphi_vac = ss%num_nphi_vac
  ENDIF
  IF(ALLOCATED(ss2%id_nzflux_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_nzflux_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%nzflux_ky_tglf).AND.ALLOCATED(ss2%nzflux_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%nzflux_ky_tglf = ss%nzflux_ky_tglf
     IF(ALLOCATED(ss2%id_nzflux_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_nzflux_ky_tglf = ss%id_nzflux_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_nz_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_nz_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%nz_ky_tglf).AND.ALLOCATED(ss2%nz_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%nz_ky_tglf = ss%nz_ky_tglf
     IF(ALLOCATED(ss2%id_nz_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_nz_ky_tglf = ss%id_nz_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%n_straps)).AND.(ALLOCATED(ss%n_straps))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%n_straps = ss%n_straps
  ENDIF
  IF(ALLOCATED(ss2%id_omeg0cx)) THEN
     IF(partial_copy) THEN
        ss2%id_omeg0cx = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%omeg0cx).AND.ALLOCATED(ss2%omeg0cx)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%omeg0cx = ss%omeg0cx
     IF(ALLOCATED(ss2%id_omeg0cx)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_omeg0cx = ss%id_omeg0cx
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_omeg0sc0)) THEN
     IF(partial_copy) THEN
        ss2%id_omeg0sc0 = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%omeg0sc0).AND.ALLOCATED(ss2%omeg0sc0)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%omeg0sc0 = ss%omeg0sc0
     IF(ALLOCATED(ss2%id_omeg0sc0)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_omeg0sc0 = ss%id_omeg0sc0
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_omeg0_halo)) THEN
     IF(partial_copy) THEN
        ss2%id_omeg0_halo = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%omeg0_halo).AND.ALLOCATED(ss2%omeg0_halo)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%omeg0_halo = ss%omeg0_halo
     IF(ALLOCATED(ss2%id_omeg0_halo)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_omeg0_halo = ss%id_omeg0_halo
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_omeg0_reco)) THEN
     IF(partial_copy) THEN
        ss2%id_omeg0_reco = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%omeg0_reco).AND.ALLOCATED(ss2%omeg0_reco)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%omeg0_reco = ss%omeg0_reco
     IF(ALLOCATED(ss2%id_omeg0_reco)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_omeg0_reco = ss%id_omeg0_reco
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_omegat = 0
  ENDIF
  IF((ALLOCATED(ss%omegat).AND.ALLOCATED(ss2%omegat)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%omegat = ss%omegat
     IF(.not.partial_copy) THEN
        ss2%id_omegat = ss%id_omegat
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%omegat_bdy = ss%omegat_bdy
  ENDIF
  IF(partial_copy) THEN
     ss2%id_omega_exb = 0
  ENDIF
  IF((ALLOCATED(ss%omega_exb).AND.ALLOCATED(ss2%omega_exb)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%omega_exb = ss%omega_exb
     IF(.not.partial_copy) THEN
        ss2%id_omega_exb = ss%id_omega_exb
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_p0_reco = 0
  ENDIF
  IF((ALLOCATED(ss%p0_reco).AND.ALLOCATED(ss2%p0_reco)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%p0_reco = ss%p0_reco
     IF(.not.partial_copy) THEN
        ss2%id_p0_reco = ss%id_p0_reco
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pb0_halo = 0
  ENDIF
  IF((ALLOCATED(ss%pb0_halo).AND.ALLOCATED(ss2%pb0_halo)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%pb0_halo = ss%pb0_halo
     IF(.not.partial_copy) THEN
        ss2%id_pb0_halo = ss%id_pb0_halo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pbe = 0
  ENDIF
  IF((ALLOCATED(ss%pbe).AND.ALLOCATED(ss2%pbe)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%pbe = ss%pbe
     IF(.not.partial_copy) THEN
        ss2%id_pbe = ss%id_pbe
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pbi = 0
  ENDIF
  IF((ALLOCATED(ss%pbi).AND.ALLOCATED(ss2%pbi)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%pbi = ss%pbi
     IF(.not.partial_copy) THEN
        ss2%id_pbi = ss%id_pbi
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pbth = 0
  ENDIF
  IF((ALLOCATED(ss%pbth).AND.ALLOCATED(ss2%pbth)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%pbth = ss%pbth
     IF(.not.partial_copy) THEN
        ss2%id_pbth = ss%id_pbth
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pcx_halo = 0
  ENDIF
  IF((ALLOCATED(ss%pcx_halo).AND.ALLOCATED(ss2%pcx_halo)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%pcx_halo = ss%pcx_halo
     IF(.not.partial_copy) THEN
        ss2%id_pcx_halo = ss%id_pcx_halo
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pcx_reco = 0
  ENDIF
  IF((ALLOCATED(ss%pcx_reco).AND.ALLOCATED(ss2%pcx_reco)).AND. &
        ((cclist(ps_cnum_GAS).EQ.1))) THEN
     ss2%pcx_reco = ss%pcx_reco
     IF(.not.partial_copy) THEN
        ss2%id_pcx_reco = ss%id_pcx_reco
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_peech = 0
  ENDIF
  IF((ALLOCATED(ss%peech).AND.ALLOCATED(ss2%peech)).AND. &
        ((cclist(ps_cnum_EC).EQ.1))) THEN
     ss2%peech = ss%peech
     IF(.not.partial_copy) THEN
        ss2%id_peech = ss%id_peech
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_peech_src)) THEN
     IF(partial_copy) THEN
        ss2%id_peech_src = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%peech_src).AND.ALLOCATED(ss2%peech_src)).AND. &
        ((cclist(ps_cnum_EC).EQ.1))) THEN
     ss2%peech_src = ss%peech_src
     IF(ALLOCATED(ss2%id_peech_src)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_peech_src = ss%id_peech_src
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pelh = 0
  ENDIF
  IF((ALLOCATED(ss%pelh).AND.ALLOCATED(ss2%pelh)).AND. &
        ((cclist(ps_cnum_LH).EQ.1))) THEN
     ss2%pelh = ss%pelh
     IF(.not.partial_copy) THEN
        ss2%id_pelh = ss%id_pelh
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_pelh_src)) THEN
     IF(partial_copy) THEN
        ss2%id_pelh_src = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%pelh_src).AND.ALLOCATED(ss2%pelh_src)).AND. &
        ((cclist(ps_cnum_LH).EQ.1))) THEN
     ss2%pelh_src = ss%pelh_src
     IF(ALLOCATED(ss2%id_pelh_src)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_pelh_src = ss%id_pelh_src
        ENDIF
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_PEL).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%PEL_Code_Info = ss%PEL_Code_Info
  ENDIF
  IF((cclist(ps_cnum_PEL).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%PEL_Data_Info = ss%PEL_Data_Info
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pe_anom = 0
  ENDIF
  IF((ALLOCATED(ss%pe_anom).AND.ALLOCATED(ss2%pe_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%pe_anom = ss%pe_anom
     IF(.not.partial_copy) THEN
        ss2%id_pe_anom = ss%id_pe_anom
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pe_trans = 0
  ENDIF
  IF((ALLOCATED(ss%pe_trans).AND.ALLOCATED(ss2%pe_trans)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%pe_trans = ss%pe_trans
     IF(.not.partial_copy) THEN
        ss2%id_pe_trans = ss%id_pe_trans
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_pe_transcat)) THEN
     IF(partial_copy) THEN
        ss2%id_pe_transcat = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%pe_transcat).AND.ALLOCATED(ss2%pe_transcat)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%pe_transcat = ss%pe_transcat
     IF(ALLOCATED(ss2%id_pe_transcat)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_pe_transcat = ss%id_pe_transcat
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pfuse = 0
  ENDIF
  IF((ALLOCATED(ss%pfuse).AND.ALLOCATED(ss2%pfuse)).AND. &
        ((cclist(ps_cnum_FUS).EQ.1))) THEN
     ss2%pfuse = ss%pfuse
     IF(.not.partial_copy) THEN
        ss2%id_pfuse = ss%id_pfuse
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pfusi = 0
  ENDIF
  IF((ALLOCATED(ss%pfusi).AND.ALLOCATED(ss2%pfusi)).AND. &
        ((cclist(ps_cnum_FUS).EQ.1))) THEN
     ss2%pfusi = ss%pfusi
     IF(.not.partial_copy) THEN
        ss2%id_pfusi = ss%id_pfusi
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pfusth = 0
  ENDIF
  IF((ALLOCATED(ss%pfusth).AND.ALLOCATED(ss2%pfusth)).AND. &
        ((cclist(ps_cnum_FUS).EQ.1))) THEN
     ss2%pfusth = ss%pfusth
     IF(.not.partial_copy) THEN
        ss2%id_pfusth = ss%id_pfusth
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%Phibsc)).AND.(ALLOCATED(ss%Phibsc))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%Phibsc = ss%Phibsc
  ENDIF
  IF(ALLOCATED(ss2%id_phiflux_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_phiflux_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%phiflux_ky_tglf).AND.ALLOCATED(ss2%phiflux_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%phiflux_ky_tglf = ss%phiflux_ky_tglf
     IF(ALLOCATED(ss2%id_phiflux_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_phiflux_ky_tglf = ss%id_phiflux_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_phit = 0
  ENDIF
  IF((ALLOCATED(ss%phit).AND.ALLOCATED(ss2%phit)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1).OR.(iwant_1deq.eq.1))) THEN
     ss2%phit = ss%phit
     IF(.not.partial_copy) THEN
        ss2%id_phit = ss%id_phit
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%Phi_EC_launch)).AND.(ALLOCATED(ss%Phi_EC_launch))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%Phi_EC_launch = ss%Phi_EC_launch
  ENDIF
  IF( ((ALLOCATED(ss2%picrf_abs)).AND.(ALLOCATED(ss%picrf_abs))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%picrf_abs = ss%picrf_abs
  ENDIF
  IF( ((ALLOCATED(ss2%picrf_ext)).AND.(ALLOCATED(ss%picrf_ext))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%picrf_ext = ss%picrf_ext
  ENDIF
  IF(ALLOCATED(ss2%id_picrf_nphi_srcs)) THEN
     IF(partial_copy) THEN
        ss2%id_picrf_nphi_srcs = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%picrf_nphi_srcs).AND.ALLOCATED(ss2%picrf_nphi_srcs)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%picrf_nphi_srcs = ss%picrf_nphi_srcs
     IF(ALLOCATED(ss2%id_picrf_nphi_srcs)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_picrf_nphi_srcs = ss%id_picrf_nphi_srcs
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_picrf_srcs)) THEN
     IF(partial_copy) THEN
        ss2%id_picrf_srcs = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%picrf_srcs).AND.ALLOCATED(ss2%picrf_srcs)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%picrf_srcs = ss%picrf_srcs
     IF(ALLOCATED(ss2%id_picrf_srcs)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_picrf_srcs = ss%id_picrf_srcs
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_picrf_totals)) THEN
     IF(partial_copy) THEN
        ss2%id_picrf_totals = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%picrf_totals).AND.ALLOCATED(ss2%picrf_totals)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%picrf_totals = ss%picrf_totals
     IF(ALLOCATED(ss2%id_picrf_totals)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_picrf_totals = ss%id_picrf_totals
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_picth = 0
  ENDIF
  IF((ALLOCATED(ss%picth).AND.ALLOCATED(ss2%picth)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%picth = ss%picth
     IF(.not.partial_copy) THEN
        ss2%id_picth = ss%id_picth
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pilh = 0
  ENDIF
  IF((ALLOCATED(ss%pilh).AND.ALLOCATED(ss2%pilh)).AND. &
        ((cclist(ps_cnum_LH).EQ.1))) THEN
     ss2%pilh = ss%pilh
     IF(.not.partial_copy) THEN
        ss2%id_pilh = ss%id_pilh
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_pilh_src)) THEN
     IF(partial_copy) THEN
        ss2%id_pilh_src = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%pilh_src).AND.ALLOCATED(ss2%pilh_src)).AND. &
        ((cclist(ps_cnum_LH).EQ.1))) THEN
     ss2%pilh_src = ss%pilh_src
     IF(ALLOCATED(ss2%id_pilh_src)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_pilh_src = ss%id_pilh_src
        ENDIF
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pi_anom = 0
  ENDIF
  IF((ALLOCATED(ss%pi_anom).AND.ALLOCATED(ss2%pi_anom)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%pi_anom = ss%pi_anom
     IF(.not.partial_copy) THEN
        ss2%id_pi_anom = ss%id_pi_anom
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pi_trans = 0
  ENDIF
  IF((ALLOCATED(ss%pi_trans).AND.ALLOCATED(ss2%pi_trans)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%pi_trans = ss%pi_trans
     IF(.not.partial_copy) THEN
        ss2%id_pi_trans = ss%id_pi_trans
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_pi_transcat)) THEN
     IF(partial_copy) THEN
        ss2%id_pi_transcat = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%pi_transcat).AND.ALLOCATED(ss2%pi_transcat)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%pi_transcat = ss%pi_transcat
     IF(ALLOCATED(ss2%id_pi_transcat)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_pi_transcat = ss%id_pi_transcat
        ENDIF
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%PLASMA_Code_Info = ss%PLASMA_Code_Info
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pmine = 0
  ENDIF
  IF((ALLOCATED(ss%pmine).AND.ALLOCATED(ss2%pmine)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%pmine = ss%pmine
     IF(.not.partial_copy) THEN
        ss2%id_pmine = ss%id_pmine
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pmini = 0
  ENDIF
  IF((ALLOCATED(ss%pmini).AND.ALLOCATED(ss2%pmini)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%pmini = ss%pmini
     IF(.not.partial_copy) THEN
        ss2%id_pmini = ss%id_pmini
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pminth = 0
  ENDIF
  IF((ALLOCATED(ss%pminth).AND.ALLOCATED(ss2%pminth)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%pminth = ss%pminth
     IF(.not.partial_copy) THEN
        ss2%id_pminth = ss%id_pminth
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_pohme = 0
  ENDIF
  IF((ALLOCATED(ss%pohme).AND.ALLOCATED(ss2%pohme)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%pohme = ss%pohme
     IF(.not.partial_copy) THEN
        ss2%id_pohme = ss%id_pohme
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_pot_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_pot_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%pot_ky_tglf).AND.ALLOCATED(ss2%pot_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%pot_ky_tglf = ss%pot_ky_tglf
     IF(ALLOCATED(ss2%id_pot_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_pot_ky_tglf = ss%id_pot_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%power_ec)).AND.(ALLOCATED(ss%power_ec))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_EC).EQ.1)) ) THEN
     ss2%power_ec = ss%power_ec
  ENDIF
  IF( ((ALLOCATED(ss2%power_ic)).AND.(ALLOCATED(ss%power_ic))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%power_ic = ss%power_ic
  ENDIF
  IF( ((ALLOCATED(ss2%power_lh)).AND.(ALLOCATED(ss%power_lh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%power_lh = ss%power_lh
  ENDIF
  IF( ((ALLOCATED(ss2%power_nbi)).AND.(ALLOCATED(ss%power_nbi))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%power_nbi = ss%power_nbi
  ENDIF
  IF( ((ALLOCATED(ss2%power_nbi_trace)).AND.(ALLOCATED(ss%power_nbi_trace))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%power_nbi_trace = ss%power_nbi_trace
  ENDIF
  IF(partial_copy) THEN
     ss2%id_prad = 0
  ENDIF
  IF((ALLOCATED(ss%prad).AND.ALLOCATED(ss2%prad)).AND. &
        ((cclist(ps_cnum_RAD).EQ.1))) THEN
     ss2%prad = ss%prad
     IF(.not.partial_copy) THEN
        ss2%id_prad = ss%id_prad
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_prad_br = 0
  ENDIF
  IF((ALLOCATED(ss%prad_br).AND.ALLOCATED(ss2%prad_br)).AND. &
        ((cclist(ps_cnum_RAD).EQ.1))) THEN
     ss2%prad_br = ss%prad_br
     IF(.not.partial_copy) THEN
        ss2%id_prad_br = ss%id_prad_br
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_prad_cy = 0
  ENDIF
  IF((ALLOCATED(ss%prad_cy).AND.ALLOCATED(ss2%prad_cy)).AND. &
        ((cclist(ps_cnum_RAD).EQ.1))) THEN
     ss2%prad_cy = ss%prad_cy
     IF(.not.partial_copy) THEN
        ss2%id_prad_cy = ss%id_prad_cy
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_copy_5
