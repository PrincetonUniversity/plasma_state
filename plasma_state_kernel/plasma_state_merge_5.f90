! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_merge_5(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)


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

  IF((ALLOCATED(ss3%ns_bdy)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%ns_bdy).AND.ALLOCATED(ss2%ns_bdy)) THEN
        CALL ps_conformal("ns_bdy",ss2%ns_bdy,ss3%ns_bdy,iout,ierr)
        CALL ps_conformal("ns_bdy",ss1%ns_bdy,ss3%ns_bdy,iout,ierr)
        if(ierr.eq.0) ss3%ns_bdy = w1*ss1%ns_bdy + (1.0_rspec-w1)*ss2%ns_bdy
     ELSE IF(ALLOCATED(ss1%ns_bdy)) THEN
        CALL ps_conformal("ns_bdy",ss1%ns_bdy,ss3%ns_bdy,iout,ierr)
        ss3%ns_bdy = ss1%ns_bdy
     ELSE IF(ALLOCATED(ss2%ns_bdy)) THEN
        CALL ps_conformal("ns_bdy",ss2%ns_bdy,ss3%ns_bdy,iout,ierr)
        ss3%ns_bdy = ss2%ns_bdy
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("ns_Data_Info",ss1%ns_Data_Info,ss2%ns_Data_Info,icheck,iout,ierr)
     ss3%ns_Data_Info = ss1%ns_Data_Info
  ENDIF

  IF((ALLOCATED(ss3%ns_is_input)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%ns_is_input).AND.ALLOCATED(ss2%ns_is_input)) THEN
        CALL ps_diff("ns_is_input",ss1%ns_is_input,ss2%ns_is_input,icheck,iout,ierr)
        CALL ps_conformal("ns_is_input",ss1%ns_is_input,ss3%ns_is_input,iout,ierr)
        if(ierr.eq.0) ss3%ns_is_input = ss1%ns_is_input
     ELSE IF(ALLOCATED(ss1%ns_is_input)) THEN
        CALL ps_conformal("ns_is_input",ss1%ns_is_input,ss3%ns_is_input,iout,ierr)
        ss3%ns_is_input = ss1%ns_is_input
     ELSE IF(ALLOCATED(ss2%ns_is_input)) THEN
        CALL ps_conformal("ns_is_input",ss2%ns_is_input,ss3%ns_is_input,iout,ierr)
        ss3%ns_is_input = ss2%ns_is_input
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_RIPPLE).EQ.1) THEN

     CALL ps_diff("ntf_coil2",ss1%ntf_coil2,ss2%ntf_coil2,icheck,iout,ierr)
     ss3%ntf_coil2 = ss1%ntf_coil2
  ENDIF

  IF(cclist(ps_cnum_RIPPLE).EQ.1) THEN

     CALL ps_diff("ntf_coils",ss1%ntf_coils,ss2%ntf_coils,icheck,iout,ierr)
     ss3%ntf_coils = ss1%ntf_coils
  ENDIF

  IF((ALLOCATED(ss3%nthinlh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%nthinlh).AND.ALLOCATED(ss2%nthinlh)) THEN
        CALL ps_diff("nthinlh",ss1%nthinlh,ss2%nthinlh,icheck,iout,ierr)
        CALL ps_conformal("nthinlh",ss1%nthinlh,ss3%nthinlh,iout,ierr)
        if(ierr.eq.0) ss3%nthinlh = ss1%nthinlh
     ELSE IF(ALLOCATED(ss1%nthinlh)) THEN
        CALL ps_conformal("nthinlh",ss1%nthinlh,ss3%nthinlh,iout,ierr)
        ss3%nthinlh = ss1%nthinlh
     ELSE IF(ALLOCATED(ss2%nthinlh)) THEN
        CALL ps_conformal("nthinlh",ss2%nthinlh,ss3%nthinlh,iout,ierr)
        ss3%nthinlh = ss2%nthinlh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Nturns)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Nturns).AND.ALLOCATED(ss2%Nturns)) THEN
        CALL ps_diff("Nturns",ss1%Nturns,ss2%Nturns,icheck,iout,ierr)
        CALL ps_conformal("Nturns",ss1%Nturns,ss3%Nturns,iout,ierr)
        if(ierr.eq.0) ss3%Nturns = ss1%Nturns
     ELSE IF(ALLOCATED(ss1%Nturns)) THEN
        CALL ps_conformal("Nturns",ss1%Nturns,ss3%Nturns,iout,ierr)
        ss3%Nturns = ss1%Nturns
     ELSE IF(ALLOCATED(ss2%Nturns)) THEN
        CALL ps_conformal("Nturns",ss2%Nturns,ss3%Nturns,iout,ierr)
        ss3%Nturns = ss2%Nturns
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%num_nphi)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%num_nphi).AND.ALLOCATED(ss2%num_nphi)) THEN
        CALL ps_diff("num_nphi",ss1%num_nphi,ss2%num_nphi,icheck,iout,ierr)
        CALL ps_conformal("num_nphi",ss1%num_nphi,ss3%num_nphi,iout,ierr)
        if(ierr.eq.0) ss3%num_nphi = ss1%num_nphi
     ELSE IF(ALLOCATED(ss1%num_nphi)) THEN
        CALL ps_conformal("num_nphi",ss1%num_nphi,ss3%num_nphi,iout,ierr)
        ss3%num_nphi = ss1%num_nphi
     ELSE IF(ALLOCATED(ss2%num_nphi)) THEN
        CALL ps_conformal("num_nphi",ss2%num_nphi,ss3%num_nphi,iout,ierr)
        ss3%num_nphi = ss2%num_nphi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%num_nphi_vac)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%num_nphi_vac).AND.ALLOCATED(ss2%num_nphi_vac)) THEN
        CALL ps_diff("num_nphi_vac",ss1%num_nphi_vac,ss2%num_nphi_vac,icheck,iout,ierr)
        CALL ps_conformal("num_nphi_vac",ss1%num_nphi_vac,ss3%num_nphi_vac,iout,ierr)
        if(ierr.eq.0) ss3%num_nphi_vac = ss1%num_nphi_vac
     ELSE IF(ALLOCATED(ss1%num_nphi_vac)) THEN
        CALL ps_conformal("num_nphi_vac",ss1%num_nphi_vac,ss3%num_nphi_vac,iout,ierr)
        ss3%num_nphi_vac = ss1%num_nphi_vac
     ELSE IF(ALLOCATED(ss2%num_nphi_vac)) THEN
        CALL ps_conformal("num_nphi_vac",ss2%num_nphi_vac,ss3%num_nphi_vac,iout,ierr)
        ss3%num_nphi_vac = ss2%num_nphi_vac
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nzflux_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%nzflux_ky_tglf).AND.ALLOCATED(ss2%nzflux_ky_tglf)) THEN
        CALL ps_conformal("nzflux_ky_tglf",ss2%nzflux_ky_tglf,ss3%nzflux_ky_tglf,iout,ierr)
        CALL ps_conformal("nzflux_ky_tglf",ss1%nzflux_ky_tglf,ss3%nzflux_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%nzflux_ky_tglf = w1*ss1%nzflux_ky_tglf + (1.0_rspec-w1)*ss2%nzflux_ky_tglf
     ELSE IF(ALLOCATED(ss1%nzflux_ky_tglf)) THEN
        CALL ps_conformal("nzflux_ky_tglf",ss1%nzflux_ky_tglf,ss3%nzflux_ky_tglf,iout,ierr)
        ss3%nzflux_ky_tglf = ss1%nzflux_ky_tglf
     ELSE IF(ALLOCATED(ss2%nzflux_ky_tglf)) THEN
        CALL ps_conformal("nzflux_ky_tglf",ss2%nzflux_ky_tglf,ss3%nzflux_ky_tglf,iout,ierr)
        ss3%nzflux_ky_tglf = ss2%nzflux_ky_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nz_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%nz_ky_tglf).AND.ALLOCATED(ss2%nz_ky_tglf)) THEN
        CALL ps_conformal("nz_ky_tglf",ss2%nz_ky_tglf,ss3%nz_ky_tglf,iout,ierr)
        CALL ps_conformal("nz_ky_tglf",ss1%nz_ky_tglf,ss3%nz_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%nz_ky_tglf = w1*ss1%nz_ky_tglf + (1.0_rspec-w1)*ss2%nz_ky_tglf
     ELSE IF(ALLOCATED(ss1%nz_ky_tglf)) THEN
        CALL ps_conformal("nz_ky_tglf",ss1%nz_ky_tglf,ss3%nz_ky_tglf,iout,ierr)
        ss3%nz_ky_tglf = ss1%nz_ky_tglf
     ELSE IF(ALLOCATED(ss2%nz_ky_tglf)) THEN
        CALL ps_conformal("nz_ky_tglf",ss2%nz_ky_tglf,ss3%nz_ky_tglf,iout,ierr)
        ss3%nz_ky_tglf = ss2%nz_ky_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%n_straps)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%n_straps).AND.ALLOCATED(ss2%n_straps)) THEN
        CALL ps_diff("n_straps",ss1%n_straps,ss2%n_straps,icheck,iout,ierr)
        CALL ps_conformal("n_straps",ss1%n_straps,ss3%n_straps,iout,ierr)
        if(ierr.eq.0) ss3%n_straps = ss1%n_straps
     ELSE IF(ALLOCATED(ss1%n_straps)) THEN
        CALL ps_conformal("n_straps",ss1%n_straps,ss3%n_straps,iout,ierr)
        ss3%n_straps = ss1%n_straps
     ELSE IF(ALLOCATED(ss2%n_straps)) THEN
        CALL ps_conformal("n_straps",ss2%n_straps,ss3%n_straps,iout,ierr)
        ss3%n_straps = ss2%n_straps
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%omeg0cx)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%omeg0cx).AND.ALLOCATED(ss2%omeg0cx)) THEN
        CALL ps_conformal("omeg0cx",ss2%omeg0cx,ss3%omeg0cx,iout,ierr)
        CALL ps_conformal("omeg0cx",ss1%omeg0cx,ss3%omeg0cx,iout,ierr)
        if(ierr.eq.0) ss3%omeg0cx = w1*ss1%omeg0cx + (1.0_rspec-w1)*ss2%omeg0cx
     ELSE IF(ALLOCATED(ss1%omeg0cx)) THEN
        CALL ps_conformal("omeg0cx",ss1%omeg0cx,ss3%omeg0cx,iout,ierr)
        ss3%omeg0cx = ss1%omeg0cx
     ELSE IF(ALLOCATED(ss2%omeg0cx)) THEN
        CALL ps_conformal("omeg0cx",ss2%omeg0cx,ss3%omeg0cx,iout,ierr)
        ss3%omeg0cx = ss2%omeg0cx
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%omeg0sc0)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%omeg0sc0).AND.ALLOCATED(ss2%omeg0sc0)) THEN
        CALL ps_conformal("omeg0sc0",ss2%omeg0sc0,ss3%omeg0sc0,iout,ierr)
        CALL ps_conformal("omeg0sc0",ss1%omeg0sc0,ss3%omeg0sc0,iout,ierr)
        if(ierr.eq.0) ss3%omeg0sc0 = w1*ss1%omeg0sc0 + (1.0_rspec-w1)*ss2%omeg0sc0
     ELSE IF(ALLOCATED(ss1%omeg0sc0)) THEN
        CALL ps_conformal("omeg0sc0",ss1%omeg0sc0,ss3%omeg0sc0,iout,ierr)
        ss3%omeg0sc0 = ss1%omeg0sc0
     ELSE IF(ALLOCATED(ss2%omeg0sc0)) THEN
        CALL ps_conformal("omeg0sc0",ss2%omeg0sc0,ss3%omeg0sc0,iout,ierr)
        ss3%omeg0sc0 = ss2%omeg0sc0
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%omeg0_halo)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%omeg0_halo).AND.ALLOCATED(ss2%omeg0_halo)) THEN
        CALL ps_conformal("omeg0_halo",ss2%omeg0_halo,ss3%omeg0_halo,iout,ierr)
        CALL ps_conformal("omeg0_halo",ss1%omeg0_halo,ss3%omeg0_halo,iout,ierr)
        if(ierr.eq.0) ss3%omeg0_halo = w1*ss1%omeg0_halo + (1.0_rspec-w1)*ss2%omeg0_halo
     ELSE IF(ALLOCATED(ss1%omeg0_halo)) THEN
        CALL ps_conformal("omeg0_halo",ss1%omeg0_halo,ss3%omeg0_halo,iout,ierr)
        ss3%omeg0_halo = ss1%omeg0_halo
     ELSE IF(ALLOCATED(ss2%omeg0_halo)) THEN
        CALL ps_conformal("omeg0_halo",ss2%omeg0_halo,ss3%omeg0_halo,iout,ierr)
        ss3%omeg0_halo = ss2%omeg0_halo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%omeg0_reco)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%omeg0_reco).AND.ALLOCATED(ss2%omeg0_reco)) THEN
        CALL ps_conformal("omeg0_reco",ss2%omeg0_reco,ss3%omeg0_reco,iout,ierr)
        CALL ps_conformal("omeg0_reco",ss1%omeg0_reco,ss3%omeg0_reco,iout,ierr)
        if(ierr.eq.0) ss3%omeg0_reco = w1*ss1%omeg0_reco + (1.0_rspec-w1)*ss2%omeg0_reco
     ELSE IF(ALLOCATED(ss1%omeg0_reco)) THEN
        CALL ps_conformal("omeg0_reco",ss1%omeg0_reco,ss3%omeg0_reco,iout,ierr)
        ss3%omeg0_reco = ss1%omeg0_reco
     ELSE IF(ALLOCATED(ss2%omeg0_reco)) THEN
        CALL ps_conformal("omeg0_reco",ss2%omeg0_reco,ss3%omeg0_reco,iout,ierr)
        ss3%omeg0_reco = ss2%omeg0_reco
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%omegat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%omegat).AND.ALLOCATED(ss2%omegat)) THEN
        CALL ps_conformal("omegat",ss2%omegat,ss3%omegat,iout,ierr)
        CALL ps_conformal("omegat",ss1%omegat,ss3%omegat,iout,ierr)
        if(ierr.eq.0) ss3%omegat = w1*ss1%omegat + (1.0_rspec-w1)*ss2%omegat
     ELSE IF(ALLOCATED(ss1%omegat)) THEN
        CALL ps_conformal("omegat",ss1%omegat,ss3%omegat,iout,ierr)
        ss3%omegat = ss1%omegat
     ELSE IF(ALLOCATED(ss2%omegat)) THEN
        CALL ps_conformal("omegat",ss2%omegat,ss3%omegat,iout,ierr)
        ss3%omegat = ss2%omegat
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     ss3%omegat_bdy = w1*ss1%omegat_bdy + (1.0_rspec-w1)*ss2%omegat_bdy
  ENDIF

  IF((ALLOCATED(ss3%omega_exb)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%omega_exb).AND.ALLOCATED(ss2%omega_exb)) THEN
        CALL ps_conformal("omega_exb",ss2%omega_exb,ss3%omega_exb,iout,ierr)
        CALL ps_conformal("omega_exb",ss1%omega_exb,ss3%omega_exb,iout,ierr)
        if(ierr.eq.0) ss3%omega_exb = w1*ss1%omega_exb + (1.0_rspec-w1)*ss2%omega_exb
     ELSE IF(ALLOCATED(ss1%omega_exb)) THEN
        CALL ps_conformal("omega_exb",ss1%omega_exb,ss3%omega_exb,iout,ierr)
        ss3%omega_exb = ss1%omega_exb
     ELSE IF(ALLOCATED(ss2%omega_exb)) THEN
        CALL ps_conformal("omega_exb",ss2%omega_exb,ss3%omega_exb,iout,ierr)
        ss3%omega_exb = ss2%omega_exb
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%p0_reco)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%p0_reco).AND.ALLOCATED(ss2%p0_reco)) THEN
        CALL ps_conformal("p0_reco",ss2%p0_reco,ss3%p0_reco,iout,ierr)
        CALL ps_conformal("p0_reco",ss1%p0_reco,ss3%p0_reco,iout,ierr)
        if(ierr.eq.0) ss3%p0_reco = w1*ss1%p0_reco + (1.0_rspec-w1)*ss2%p0_reco
     ELSE IF(ALLOCATED(ss1%p0_reco)) THEN
        CALL ps_conformal("p0_reco",ss1%p0_reco,ss3%p0_reco,iout,ierr)
        ss3%p0_reco = ss1%p0_reco
     ELSE IF(ALLOCATED(ss2%p0_reco)) THEN
        CALL ps_conformal("p0_reco",ss2%p0_reco,ss3%p0_reco,iout,ierr)
        ss3%p0_reco = ss2%p0_reco
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pb0_halo)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%pb0_halo).AND.ALLOCATED(ss2%pb0_halo)) THEN
        CALL ps_conformal("pb0_halo",ss2%pb0_halo,ss3%pb0_halo,iout,ierr)
        CALL ps_conformal("pb0_halo",ss1%pb0_halo,ss3%pb0_halo,iout,ierr)
        if(ierr.eq.0) ss3%pb0_halo = w1*ss1%pb0_halo + (1.0_rspec-w1)*ss2%pb0_halo
     ELSE IF(ALLOCATED(ss1%pb0_halo)) THEN
        CALL ps_conformal("pb0_halo",ss1%pb0_halo,ss3%pb0_halo,iout,ierr)
        ss3%pb0_halo = ss1%pb0_halo
     ELSE IF(ALLOCATED(ss2%pb0_halo)) THEN
        CALL ps_conformal("pb0_halo",ss2%pb0_halo,ss3%pb0_halo,iout,ierr)
        ss3%pb0_halo = ss2%pb0_halo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pbe)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%pbe).AND.ALLOCATED(ss2%pbe)) THEN
        CALL ps_conformal("pbe",ss2%pbe,ss3%pbe,iout,ierr)
        CALL ps_conformal("pbe",ss1%pbe,ss3%pbe,iout,ierr)
        if(ierr.eq.0) ss3%pbe = w1*ss1%pbe + (1.0_rspec-w1)*ss2%pbe
     ELSE IF(ALLOCATED(ss1%pbe)) THEN
        CALL ps_conformal("pbe",ss1%pbe,ss3%pbe,iout,ierr)
        ss3%pbe = ss1%pbe
     ELSE IF(ALLOCATED(ss2%pbe)) THEN
        CALL ps_conformal("pbe",ss2%pbe,ss3%pbe,iout,ierr)
        ss3%pbe = ss2%pbe
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pbi)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%pbi).AND.ALLOCATED(ss2%pbi)) THEN
        CALL ps_conformal("pbi",ss2%pbi,ss3%pbi,iout,ierr)
        CALL ps_conformal("pbi",ss1%pbi,ss3%pbi,iout,ierr)
        if(ierr.eq.0) ss3%pbi = w1*ss1%pbi + (1.0_rspec-w1)*ss2%pbi
     ELSE IF(ALLOCATED(ss1%pbi)) THEN
        CALL ps_conformal("pbi",ss1%pbi,ss3%pbi,iout,ierr)
        ss3%pbi = ss1%pbi
     ELSE IF(ALLOCATED(ss2%pbi)) THEN
        CALL ps_conformal("pbi",ss2%pbi,ss3%pbi,iout,ierr)
        ss3%pbi = ss2%pbi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pbth)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%pbth).AND.ALLOCATED(ss2%pbth)) THEN
        CALL ps_conformal("pbth",ss2%pbth,ss3%pbth,iout,ierr)
        CALL ps_conformal("pbth",ss1%pbth,ss3%pbth,iout,ierr)
        if(ierr.eq.0) ss3%pbth = w1*ss1%pbth + (1.0_rspec-w1)*ss2%pbth
     ELSE IF(ALLOCATED(ss1%pbth)) THEN
        CALL ps_conformal("pbth",ss1%pbth,ss3%pbth,iout,ierr)
        ss3%pbth = ss1%pbth
     ELSE IF(ALLOCATED(ss2%pbth)) THEN
        CALL ps_conformal("pbth",ss2%pbth,ss3%pbth,iout,ierr)
        ss3%pbth = ss2%pbth
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pcx_halo)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%pcx_halo).AND.ALLOCATED(ss2%pcx_halo)) THEN
        CALL ps_conformal("pcx_halo",ss2%pcx_halo,ss3%pcx_halo,iout,ierr)
        CALL ps_conformal("pcx_halo",ss1%pcx_halo,ss3%pcx_halo,iout,ierr)
        if(ierr.eq.0) ss3%pcx_halo = w1*ss1%pcx_halo + (1.0_rspec-w1)*ss2%pcx_halo
     ELSE IF(ALLOCATED(ss1%pcx_halo)) THEN
        CALL ps_conformal("pcx_halo",ss1%pcx_halo,ss3%pcx_halo,iout,ierr)
        ss3%pcx_halo = ss1%pcx_halo
     ELSE IF(ALLOCATED(ss2%pcx_halo)) THEN
        CALL ps_conformal("pcx_halo",ss2%pcx_halo,ss3%pcx_halo,iout,ierr)
        ss3%pcx_halo = ss2%pcx_halo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pcx_reco)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%pcx_reco).AND.ALLOCATED(ss2%pcx_reco)) THEN
        CALL ps_conformal("pcx_reco",ss2%pcx_reco,ss3%pcx_reco,iout,ierr)
        CALL ps_conformal("pcx_reco",ss1%pcx_reco,ss3%pcx_reco,iout,ierr)
        if(ierr.eq.0) ss3%pcx_reco = w1*ss1%pcx_reco + (1.0_rspec-w1)*ss2%pcx_reco
     ELSE IF(ALLOCATED(ss1%pcx_reco)) THEN
        CALL ps_conformal("pcx_reco",ss1%pcx_reco,ss3%pcx_reco,iout,ierr)
        ss3%pcx_reco = ss1%pcx_reco
     ELSE IF(ALLOCATED(ss2%pcx_reco)) THEN
        CALL ps_conformal("pcx_reco",ss2%pcx_reco,ss3%pcx_reco,iout,ierr)
        ss3%pcx_reco = ss2%pcx_reco
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%peech)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%peech).AND.ALLOCATED(ss2%peech)) THEN
        CALL ps_conformal("peech",ss2%peech,ss3%peech,iout,ierr)
        CALL ps_conformal("peech",ss1%peech,ss3%peech,iout,ierr)
        if(ierr.eq.0) ss3%peech = w1*ss1%peech + (1.0_rspec-w1)*ss2%peech
     ELSE IF(ALLOCATED(ss1%peech)) THEN
        CALL ps_conformal("peech",ss1%peech,ss3%peech,iout,ierr)
        ss3%peech = ss1%peech
     ELSE IF(ALLOCATED(ss2%peech)) THEN
        CALL ps_conformal("peech",ss2%peech,ss3%peech,iout,ierr)
        ss3%peech = ss2%peech
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%peech_src)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%peech_src).AND.ALLOCATED(ss2%peech_src)) THEN
        CALL ps_conformal("peech_src",ss2%peech_src,ss3%peech_src,iout,ierr)
        CALL ps_conformal("peech_src",ss1%peech_src,ss3%peech_src,iout,ierr)
        if(ierr.eq.0) ss3%peech_src = w1*ss1%peech_src + (1.0_rspec-w1)*ss2%peech_src
     ELSE IF(ALLOCATED(ss1%peech_src)) THEN
        CALL ps_conformal("peech_src",ss1%peech_src,ss3%peech_src,iout,ierr)
        ss3%peech_src = ss1%peech_src
     ELSE IF(ALLOCATED(ss2%peech_src)) THEN
        CALL ps_conformal("peech_src",ss2%peech_src,ss3%peech_src,iout,ierr)
        ss3%peech_src = ss2%peech_src
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pelh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%pelh).AND.ALLOCATED(ss2%pelh)) THEN
        CALL ps_conformal("pelh",ss2%pelh,ss3%pelh,iout,ierr)
        CALL ps_conformal("pelh",ss1%pelh,ss3%pelh,iout,ierr)
        if(ierr.eq.0) ss3%pelh = w1*ss1%pelh + (1.0_rspec-w1)*ss2%pelh
     ELSE IF(ALLOCATED(ss1%pelh)) THEN
        CALL ps_conformal("pelh",ss1%pelh,ss3%pelh,iout,ierr)
        ss3%pelh = ss1%pelh
     ELSE IF(ALLOCATED(ss2%pelh)) THEN
        CALL ps_conformal("pelh",ss2%pelh,ss3%pelh,iout,ierr)
        ss3%pelh = ss2%pelh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pelh_src)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%pelh_src).AND.ALLOCATED(ss2%pelh_src)) THEN
        CALL ps_conformal("pelh_src",ss2%pelh_src,ss3%pelh_src,iout,ierr)
        CALL ps_conformal("pelh_src",ss1%pelh_src,ss3%pelh_src,iout,ierr)
        if(ierr.eq.0) ss3%pelh_src = w1*ss1%pelh_src + (1.0_rspec-w1)*ss2%pelh_src
     ELSE IF(ALLOCATED(ss1%pelh_src)) THEN
        CALL ps_conformal("pelh_src",ss1%pelh_src,ss3%pelh_src,iout,ierr)
        ss3%pelh_src = ss1%pelh_src
     ELSE IF(ALLOCATED(ss2%pelh_src)) THEN
        CALL ps_conformal("pelh_src",ss2%pelh_src,ss3%pelh_src,iout,ierr)
        ss3%pelh_src = ss2%pelh_src
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PEL).EQ.1) THEN

     CALL ps_diff("PEL_Code_Info",ss1%PEL_Code_Info,ss2%PEL_Code_Info,icheck,iout,ierr)
     ss3%PEL_Code_Info = ss1%PEL_Code_Info
  ENDIF

  IF(cclist(ps_cnum_PEL).EQ.1) THEN

     CALL ps_diff("PEL_Data_Info",ss1%PEL_Data_Info,ss2%PEL_Data_Info,icheck,iout,ierr)
     ss3%PEL_Data_Info = ss1%PEL_Data_Info
  ENDIF

  IF((ALLOCATED(ss3%pe_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%pe_anom).AND.ALLOCATED(ss2%pe_anom)) THEN
        CALL ps_conformal("pe_anom",ss2%pe_anom,ss3%pe_anom,iout,ierr)
        CALL ps_conformal("pe_anom",ss1%pe_anom,ss3%pe_anom,iout,ierr)
        if(ierr.eq.0) ss3%pe_anom = w1*ss1%pe_anom + (1.0_rspec-w1)*ss2%pe_anom
     ELSE IF(ALLOCATED(ss1%pe_anom)) THEN
        CALL ps_conformal("pe_anom",ss1%pe_anom,ss3%pe_anom,iout,ierr)
        ss3%pe_anom = ss1%pe_anom
     ELSE IF(ALLOCATED(ss2%pe_anom)) THEN
        CALL ps_conformal("pe_anom",ss2%pe_anom,ss3%pe_anom,iout,ierr)
        ss3%pe_anom = ss2%pe_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pe_trans)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%pe_trans).AND.ALLOCATED(ss2%pe_trans)) THEN
        CALL ps_conformal("pe_trans",ss2%pe_trans,ss3%pe_trans,iout,ierr)
        CALL ps_conformal("pe_trans",ss1%pe_trans,ss3%pe_trans,iout,ierr)
        if(ierr.eq.0) ss3%pe_trans = w1*ss1%pe_trans + (1.0_rspec-w1)*ss2%pe_trans
     ELSE IF(ALLOCATED(ss1%pe_trans)) THEN
        CALL ps_conformal("pe_trans",ss1%pe_trans,ss3%pe_trans,iout,ierr)
        ss3%pe_trans = ss1%pe_trans
     ELSE IF(ALLOCATED(ss2%pe_trans)) THEN
        CALL ps_conformal("pe_trans",ss2%pe_trans,ss3%pe_trans,iout,ierr)
        ss3%pe_trans = ss2%pe_trans
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pe_transcat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%pe_transcat).AND.ALLOCATED(ss2%pe_transcat)) THEN
        CALL ps_conformal("pe_transcat",ss2%pe_transcat,ss3%pe_transcat,iout,ierr)
        CALL ps_conformal("pe_transcat",ss1%pe_transcat,ss3%pe_transcat,iout,ierr)
        if(ierr.eq.0) ss3%pe_transcat = w1*ss1%pe_transcat + (1.0_rspec-w1)*ss2%pe_transcat
     ELSE IF(ALLOCATED(ss1%pe_transcat)) THEN
        CALL ps_conformal("pe_transcat",ss1%pe_transcat,ss3%pe_transcat,iout,ierr)
        ss3%pe_transcat = ss1%pe_transcat
     ELSE IF(ALLOCATED(ss2%pe_transcat)) THEN
        CALL ps_conformal("pe_transcat",ss2%pe_transcat,ss3%pe_transcat,iout,ierr)
        ss3%pe_transcat = ss2%pe_transcat
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pfuse)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%pfuse).AND.ALLOCATED(ss2%pfuse)) THEN
        CALL ps_conformal("pfuse",ss2%pfuse,ss3%pfuse,iout,ierr)
        CALL ps_conformal("pfuse",ss1%pfuse,ss3%pfuse,iout,ierr)
        if(ierr.eq.0) ss3%pfuse = w1*ss1%pfuse + (1.0_rspec-w1)*ss2%pfuse
     ELSE IF(ALLOCATED(ss1%pfuse)) THEN
        CALL ps_conformal("pfuse",ss1%pfuse,ss3%pfuse,iout,ierr)
        ss3%pfuse = ss1%pfuse
     ELSE IF(ALLOCATED(ss2%pfuse)) THEN
        CALL ps_conformal("pfuse",ss2%pfuse,ss3%pfuse,iout,ierr)
        ss3%pfuse = ss2%pfuse
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pfusi)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%pfusi).AND.ALLOCATED(ss2%pfusi)) THEN
        CALL ps_conformal("pfusi",ss2%pfusi,ss3%pfusi,iout,ierr)
        CALL ps_conformal("pfusi",ss1%pfusi,ss3%pfusi,iout,ierr)
        if(ierr.eq.0) ss3%pfusi = w1*ss1%pfusi + (1.0_rspec-w1)*ss2%pfusi
     ELSE IF(ALLOCATED(ss1%pfusi)) THEN
        CALL ps_conformal("pfusi",ss1%pfusi,ss3%pfusi,iout,ierr)
        ss3%pfusi = ss1%pfusi
     ELSE IF(ALLOCATED(ss2%pfusi)) THEN
        CALL ps_conformal("pfusi",ss2%pfusi,ss3%pfusi,iout,ierr)
        ss3%pfusi = ss2%pfusi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pfusth)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%pfusth).AND.ALLOCATED(ss2%pfusth)) THEN
        CALL ps_conformal("pfusth",ss2%pfusth,ss3%pfusth,iout,ierr)
        CALL ps_conformal("pfusth",ss1%pfusth,ss3%pfusth,iout,ierr)
        if(ierr.eq.0) ss3%pfusth = w1*ss1%pfusth + (1.0_rspec-w1)*ss2%pfusth
     ELSE IF(ALLOCATED(ss1%pfusth)) THEN
        CALL ps_conformal("pfusth",ss1%pfusth,ss3%pfusth,iout,ierr)
        ss3%pfusth = ss1%pfusth
     ELSE IF(ALLOCATED(ss2%pfusth)) THEN
        CALL ps_conformal("pfusth",ss2%pfusth,ss3%pfusth,iout,ierr)
        ss3%pfusth = ss2%pfusth
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Phibsc)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%Phibsc).AND.ALLOCATED(ss2%Phibsc)) THEN
        CALL ps_diff("Phibsc",ss1%Phibsc,ss2%Phibsc,icheck,iout,ierr)
        CALL ps_conformal("Phibsc",ss1%Phibsc,ss3%Phibsc,iout,ierr)
        if(ierr.eq.0) ss3%Phibsc = ss1%Phibsc
     ELSE IF(ALLOCATED(ss1%Phibsc)) THEN
        CALL ps_conformal("Phibsc",ss1%Phibsc,ss3%Phibsc,iout,ierr)
        ss3%Phibsc = ss1%Phibsc
     ELSE IF(ALLOCATED(ss2%Phibsc)) THEN
        CALL ps_conformal("Phibsc",ss2%Phibsc,ss3%Phibsc,iout,ierr)
        ss3%Phibsc = ss2%Phibsc
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%phiflux_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%phiflux_ky_tglf).AND.ALLOCATED(ss2%phiflux_ky_tglf)) THEN
        CALL ps_conformal("phiflux_ky_tglf",ss2%phiflux_ky_tglf,ss3%phiflux_ky_tglf,iout,ierr)
        CALL ps_conformal("phiflux_ky_tglf",ss1%phiflux_ky_tglf,ss3%phiflux_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%phiflux_ky_tglf = w1*ss1%phiflux_ky_tglf + (1.0_rspec-w1)*ss2%phiflux_ky_tglf
     ELSE IF(ALLOCATED(ss1%phiflux_ky_tglf)) THEN
        CALL ps_conformal("phiflux_ky_tglf",ss1%phiflux_ky_tglf,ss3%phiflux_ky_tglf,iout,ierr)
        ss3%phiflux_ky_tglf = ss1%phiflux_ky_tglf
     ELSE IF(ALLOCATED(ss2%phiflux_ky_tglf)) THEN
        CALL ps_conformal("phiflux_ky_tglf",ss2%phiflux_ky_tglf,ss3%phiflux_ky_tglf,iout,ierr)
        ss3%phiflux_ky_tglf = ss2%phiflux_ky_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%phit)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%phit).AND.ALLOCATED(ss2%phit)) THEN
        CALL ps_conformal("phit",ss2%phit,ss3%phit,iout,ierr)
        CALL ps_conformal("phit",ss1%phit,ss3%phit,iout,ierr)
        if(ierr.eq.0) ss3%phit = w1*ss1%phit + (1.0_rspec-w1)*ss2%phit
     ELSE IF(ALLOCATED(ss1%phit)) THEN
        CALL ps_conformal("phit",ss1%phit,ss3%phit,iout,ierr)
        ss3%phit = ss1%phit
     ELSE IF(ALLOCATED(ss2%phit)) THEN
        CALL ps_conformal("phit",ss2%phit,ss3%phit,iout,ierr)
        ss3%phit = ss2%phit
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Phi_EC_launch)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%Phi_EC_launch).AND.ALLOCATED(ss2%Phi_EC_launch)) THEN
        CALL ps_diff("Phi_EC_launch",ss1%Phi_EC_launch,ss2%Phi_EC_launch,icheck,iout,ierr)
        CALL ps_conformal("Phi_EC_launch",ss1%Phi_EC_launch,ss3%Phi_EC_launch,iout,ierr)
        if(ierr.eq.0) ss3%Phi_EC_launch = ss1%Phi_EC_launch
     ELSE IF(ALLOCATED(ss1%Phi_EC_launch)) THEN
        CALL ps_conformal("Phi_EC_launch",ss1%Phi_EC_launch,ss3%Phi_EC_launch,iout,ierr)
        ss3%Phi_EC_launch = ss1%Phi_EC_launch
     ELSE IF(ALLOCATED(ss2%Phi_EC_launch)) THEN
        CALL ps_conformal("Phi_EC_launch",ss2%Phi_EC_launch,ss3%Phi_EC_launch,iout,ierr)
        ss3%Phi_EC_launch = ss2%Phi_EC_launch
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%picrf_abs)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%picrf_abs).AND.ALLOCATED(ss2%picrf_abs)) THEN
        CALL ps_conformal("picrf_abs",ss2%picrf_abs,ss3%picrf_abs,iout,ierr)
        CALL ps_conformal("picrf_abs",ss1%picrf_abs,ss3%picrf_abs,iout,ierr)
        if(ierr.eq.0) ss3%picrf_abs = w1*ss1%picrf_abs + (1.0_rspec-w1)*ss2%picrf_abs
     ELSE IF(ALLOCATED(ss1%picrf_abs)) THEN
        CALL ps_conformal("picrf_abs",ss1%picrf_abs,ss3%picrf_abs,iout,ierr)
        ss3%picrf_abs = ss1%picrf_abs
     ELSE IF(ALLOCATED(ss2%picrf_abs)) THEN
        CALL ps_conformal("picrf_abs",ss2%picrf_abs,ss3%picrf_abs,iout,ierr)
        ss3%picrf_abs = ss2%picrf_abs
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%picrf_ext)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%picrf_ext).AND.ALLOCATED(ss2%picrf_ext)) THEN
        CALL ps_conformal("picrf_ext",ss2%picrf_ext,ss3%picrf_ext,iout,ierr)
        CALL ps_conformal("picrf_ext",ss1%picrf_ext,ss3%picrf_ext,iout,ierr)
        if(ierr.eq.0) ss3%picrf_ext = w1*ss1%picrf_ext + (1.0_rspec-w1)*ss2%picrf_ext
     ELSE IF(ALLOCATED(ss1%picrf_ext)) THEN
        CALL ps_conformal("picrf_ext",ss1%picrf_ext,ss3%picrf_ext,iout,ierr)
        ss3%picrf_ext = ss1%picrf_ext
     ELSE IF(ALLOCATED(ss2%picrf_ext)) THEN
        CALL ps_conformal("picrf_ext",ss2%picrf_ext,ss3%picrf_ext,iout,ierr)
        ss3%picrf_ext = ss2%picrf_ext
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%picrf_nphi_srcs)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%picrf_nphi_srcs).AND.ALLOCATED(ss2%picrf_nphi_srcs)) THEN
        CALL ps_conformal("picrf_nphi_srcs",ss2%picrf_nphi_srcs,ss3%picrf_nphi_srcs,iout,ierr)
        CALL ps_conformal("picrf_nphi_srcs",ss1%picrf_nphi_srcs,ss3%picrf_nphi_srcs,iout,ierr)
        if(ierr.eq.0) ss3%picrf_nphi_srcs = w1*ss1%picrf_nphi_srcs + (1.0_rspec-w1)*ss2%picrf_nphi_srcs
     ELSE IF(ALLOCATED(ss1%picrf_nphi_srcs)) THEN
        CALL ps_conformal("picrf_nphi_srcs",ss1%picrf_nphi_srcs,ss3%picrf_nphi_srcs,iout,ierr)
        ss3%picrf_nphi_srcs = ss1%picrf_nphi_srcs
     ELSE IF(ALLOCATED(ss2%picrf_nphi_srcs)) THEN
        CALL ps_conformal("picrf_nphi_srcs",ss2%picrf_nphi_srcs,ss3%picrf_nphi_srcs,iout,ierr)
        ss3%picrf_nphi_srcs = ss2%picrf_nphi_srcs
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%picrf_srcs)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%picrf_srcs).AND.ALLOCATED(ss2%picrf_srcs)) THEN
        CALL ps_conformal("picrf_srcs",ss2%picrf_srcs,ss3%picrf_srcs,iout,ierr)
        CALL ps_conformal("picrf_srcs",ss1%picrf_srcs,ss3%picrf_srcs,iout,ierr)
        if(ierr.eq.0) ss3%picrf_srcs = w1*ss1%picrf_srcs + (1.0_rspec-w1)*ss2%picrf_srcs
     ELSE IF(ALLOCATED(ss1%picrf_srcs)) THEN
        CALL ps_conformal("picrf_srcs",ss1%picrf_srcs,ss3%picrf_srcs,iout,ierr)
        ss3%picrf_srcs = ss1%picrf_srcs
     ELSE IF(ALLOCATED(ss2%picrf_srcs)) THEN
        CALL ps_conformal("picrf_srcs",ss2%picrf_srcs,ss3%picrf_srcs,iout,ierr)
        ss3%picrf_srcs = ss2%picrf_srcs
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%picrf_totals)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%picrf_totals).AND.ALLOCATED(ss2%picrf_totals)) THEN
        CALL ps_conformal("picrf_totals",ss2%picrf_totals,ss3%picrf_totals,iout,ierr)
        CALL ps_conformal("picrf_totals",ss1%picrf_totals,ss3%picrf_totals,iout,ierr)
        if(ierr.eq.0) ss3%picrf_totals = w1*ss1%picrf_totals + (1.0_rspec-w1)*ss2%picrf_totals
     ELSE IF(ALLOCATED(ss1%picrf_totals)) THEN
        CALL ps_conformal("picrf_totals",ss1%picrf_totals,ss3%picrf_totals,iout,ierr)
        ss3%picrf_totals = ss1%picrf_totals
     ELSE IF(ALLOCATED(ss2%picrf_totals)) THEN
        CALL ps_conformal("picrf_totals",ss2%picrf_totals,ss3%picrf_totals,iout,ierr)
        ss3%picrf_totals = ss2%picrf_totals
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%picth)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%picth).AND.ALLOCATED(ss2%picth)) THEN
        CALL ps_conformal("picth",ss2%picth,ss3%picth,iout,ierr)
        CALL ps_conformal("picth",ss1%picth,ss3%picth,iout,ierr)
        if(ierr.eq.0) ss3%picth = w1*ss1%picth + (1.0_rspec-w1)*ss2%picth
     ELSE IF(ALLOCATED(ss1%picth)) THEN
        CALL ps_conformal("picth",ss1%picth,ss3%picth,iout,ierr)
        ss3%picth = ss1%picth
     ELSE IF(ALLOCATED(ss2%picth)) THEN
        CALL ps_conformal("picth",ss2%picth,ss3%picth,iout,ierr)
        ss3%picth = ss2%picth
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pilh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%pilh).AND.ALLOCATED(ss2%pilh)) THEN
        CALL ps_conformal("pilh",ss2%pilh,ss3%pilh,iout,ierr)
        CALL ps_conformal("pilh",ss1%pilh,ss3%pilh,iout,ierr)
        if(ierr.eq.0) ss3%pilh = w1*ss1%pilh + (1.0_rspec-w1)*ss2%pilh
     ELSE IF(ALLOCATED(ss1%pilh)) THEN
        CALL ps_conformal("pilh",ss1%pilh,ss3%pilh,iout,ierr)
        ss3%pilh = ss1%pilh
     ELSE IF(ALLOCATED(ss2%pilh)) THEN
        CALL ps_conformal("pilh",ss2%pilh,ss3%pilh,iout,ierr)
        ss3%pilh = ss2%pilh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pilh_src)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%pilh_src).AND.ALLOCATED(ss2%pilh_src)) THEN
        CALL ps_conformal("pilh_src",ss2%pilh_src,ss3%pilh_src,iout,ierr)
        CALL ps_conformal("pilh_src",ss1%pilh_src,ss3%pilh_src,iout,ierr)
        if(ierr.eq.0) ss3%pilh_src = w1*ss1%pilh_src + (1.0_rspec-w1)*ss2%pilh_src
     ELSE IF(ALLOCATED(ss1%pilh_src)) THEN
        CALL ps_conformal("pilh_src",ss1%pilh_src,ss3%pilh_src,iout,ierr)
        ss3%pilh_src = ss1%pilh_src
     ELSE IF(ALLOCATED(ss2%pilh_src)) THEN
        CALL ps_conformal("pilh_src",ss2%pilh_src,ss3%pilh_src,iout,ierr)
        ss3%pilh_src = ss2%pilh_src
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pi_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%pi_anom).AND.ALLOCATED(ss2%pi_anom)) THEN
        CALL ps_conformal("pi_anom",ss2%pi_anom,ss3%pi_anom,iout,ierr)
        CALL ps_conformal("pi_anom",ss1%pi_anom,ss3%pi_anom,iout,ierr)
        if(ierr.eq.0) ss3%pi_anom = w1*ss1%pi_anom + (1.0_rspec-w1)*ss2%pi_anom
     ELSE IF(ALLOCATED(ss1%pi_anom)) THEN
        CALL ps_conformal("pi_anom",ss1%pi_anom,ss3%pi_anom,iout,ierr)
        ss3%pi_anom = ss1%pi_anom
     ELSE IF(ALLOCATED(ss2%pi_anom)) THEN
        CALL ps_conformal("pi_anom",ss2%pi_anom,ss3%pi_anom,iout,ierr)
        ss3%pi_anom = ss2%pi_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pi_trans)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%pi_trans).AND.ALLOCATED(ss2%pi_trans)) THEN
        CALL ps_conformal("pi_trans",ss2%pi_trans,ss3%pi_trans,iout,ierr)
        CALL ps_conformal("pi_trans",ss1%pi_trans,ss3%pi_trans,iout,ierr)
        if(ierr.eq.0) ss3%pi_trans = w1*ss1%pi_trans + (1.0_rspec-w1)*ss2%pi_trans
     ELSE IF(ALLOCATED(ss1%pi_trans)) THEN
        CALL ps_conformal("pi_trans",ss1%pi_trans,ss3%pi_trans,iout,ierr)
        ss3%pi_trans = ss1%pi_trans
     ELSE IF(ALLOCATED(ss2%pi_trans)) THEN
        CALL ps_conformal("pi_trans",ss2%pi_trans,ss3%pi_trans,iout,ierr)
        ss3%pi_trans = ss2%pi_trans
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pi_transcat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%pi_transcat).AND.ALLOCATED(ss2%pi_transcat)) THEN
        CALL ps_conformal("pi_transcat",ss2%pi_transcat,ss3%pi_transcat,iout,ierr)
        CALL ps_conformal("pi_transcat",ss1%pi_transcat,ss3%pi_transcat,iout,ierr)
        if(ierr.eq.0) ss3%pi_transcat = w1*ss1%pi_transcat + (1.0_rspec-w1)*ss2%pi_transcat
     ELSE IF(ALLOCATED(ss1%pi_transcat)) THEN
        CALL ps_conformal("pi_transcat",ss1%pi_transcat,ss3%pi_transcat,iout,ierr)
        ss3%pi_transcat = ss1%pi_transcat
     ELSE IF(ALLOCATED(ss2%pi_transcat)) THEN
        CALL ps_conformal("pi_transcat",ss2%pi_transcat,ss3%pi_transcat,iout,ierr)
        ss3%pi_transcat = ss2%pi_transcat
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("PLASMA_Code_Info",ss1%PLASMA_Code_Info,ss2%PLASMA_Code_Info,icheck,iout,ierr)
     ss3%PLASMA_Code_Info = ss1%PLASMA_Code_Info
  ENDIF

  IF((ALLOCATED(ss3%pmine)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%pmine).AND.ALLOCATED(ss2%pmine)) THEN
        CALL ps_conformal("pmine",ss2%pmine,ss3%pmine,iout,ierr)
        CALL ps_conformal("pmine",ss1%pmine,ss3%pmine,iout,ierr)
        if(ierr.eq.0) ss3%pmine = w1*ss1%pmine + (1.0_rspec-w1)*ss2%pmine
     ELSE IF(ALLOCATED(ss1%pmine)) THEN
        CALL ps_conformal("pmine",ss1%pmine,ss3%pmine,iout,ierr)
        ss3%pmine = ss1%pmine
     ELSE IF(ALLOCATED(ss2%pmine)) THEN
        CALL ps_conformal("pmine",ss2%pmine,ss3%pmine,iout,ierr)
        ss3%pmine = ss2%pmine
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pmini)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%pmini).AND.ALLOCATED(ss2%pmini)) THEN
        CALL ps_conformal("pmini",ss2%pmini,ss3%pmini,iout,ierr)
        CALL ps_conformal("pmini",ss1%pmini,ss3%pmini,iout,ierr)
        if(ierr.eq.0) ss3%pmini = w1*ss1%pmini + (1.0_rspec-w1)*ss2%pmini
     ELSE IF(ALLOCATED(ss1%pmini)) THEN
        CALL ps_conformal("pmini",ss1%pmini,ss3%pmini,iout,ierr)
        ss3%pmini = ss1%pmini
     ELSE IF(ALLOCATED(ss2%pmini)) THEN
        CALL ps_conformal("pmini",ss2%pmini,ss3%pmini,iout,ierr)
        ss3%pmini = ss2%pmini
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pminth)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%pminth).AND.ALLOCATED(ss2%pminth)) THEN
        CALL ps_conformal("pminth",ss2%pminth,ss3%pminth,iout,ierr)
        CALL ps_conformal("pminth",ss1%pminth,ss3%pminth,iout,ierr)
        if(ierr.eq.0) ss3%pminth = w1*ss1%pminth + (1.0_rspec-w1)*ss2%pminth
     ELSE IF(ALLOCATED(ss1%pminth)) THEN
        CALL ps_conformal("pminth",ss1%pminth,ss3%pminth,iout,ierr)
        ss3%pminth = ss1%pminth
     ELSE IF(ALLOCATED(ss2%pminth)) THEN
        CALL ps_conformal("pminth",ss2%pminth,ss3%pminth,iout,ierr)
        ss3%pminth = ss2%pminth
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pohme)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%pohme).AND.ALLOCATED(ss2%pohme)) THEN
        CALL ps_conformal("pohme",ss2%pohme,ss3%pohme,iout,ierr)
        CALL ps_conformal("pohme",ss1%pohme,ss3%pohme,iout,ierr)
        if(ierr.eq.0) ss3%pohme = w1*ss1%pohme + (1.0_rspec-w1)*ss2%pohme
     ELSE IF(ALLOCATED(ss1%pohme)) THEN
        CALL ps_conformal("pohme",ss1%pohme,ss3%pohme,iout,ierr)
        ss3%pohme = ss1%pohme
     ELSE IF(ALLOCATED(ss2%pohme)) THEN
        CALL ps_conformal("pohme",ss2%pohme,ss3%pohme,iout,ierr)
        ss3%pohme = ss2%pohme
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%pot_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%pot_ky_tglf).AND.ALLOCATED(ss2%pot_ky_tglf)) THEN
        CALL ps_conformal("pot_ky_tglf",ss2%pot_ky_tglf,ss3%pot_ky_tglf,iout,ierr)
        CALL ps_conformal("pot_ky_tglf",ss1%pot_ky_tglf,ss3%pot_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%pot_ky_tglf = w1*ss1%pot_ky_tglf + (1.0_rspec-w1)*ss2%pot_ky_tglf
     ELSE IF(ALLOCATED(ss1%pot_ky_tglf)) THEN
        CALL ps_conformal("pot_ky_tglf",ss1%pot_ky_tglf,ss3%pot_ky_tglf,iout,ierr)
        ss3%pot_ky_tglf = ss1%pot_ky_tglf
     ELSE IF(ALLOCATED(ss2%pot_ky_tglf)) THEN
        CALL ps_conformal("pot_ky_tglf",ss2%pot_ky_tglf,ss3%pot_ky_tglf,iout,ierr)
        ss3%pot_ky_tglf = ss2%pot_ky_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%power_ec)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%power_ec).AND.ALLOCATED(ss2%power_ec)) THEN
        CALL ps_conformal("power_ec",ss2%power_ec,ss3%power_ec,iout,ierr)
        CALL ps_conformal("power_ec",ss1%power_ec,ss3%power_ec,iout,ierr)
        if(ierr.eq.0) ss3%power_ec = w1*ss1%power_ec + (1.0_rspec-w1)*ss2%power_ec
     ELSE IF(ALLOCATED(ss1%power_ec)) THEN
        CALL ps_conformal("power_ec",ss1%power_ec,ss3%power_ec,iout,ierr)
        ss3%power_ec = ss1%power_ec
     ELSE IF(ALLOCATED(ss2%power_ec)) THEN
        CALL ps_conformal("power_ec",ss2%power_ec,ss3%power_ec,iout,ierr)
        ss3%power_ec = ss2%power_ec
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%power_ic)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%power_ic).AND.ALLOCATED(ss2%power_ic)) THEN
        CALL ps_conformal("power_ic",ss2%power_ic,ss3%power_ic,iout,ierr)
        CALL ps_conformal("power_ic",ss1%power_ic,ss3%power_ic,iout,ierr)
        if(ierr.eq.0) ss3%power_ic = w1*ss1%power_ic + (1.0_rspec-w1)*ss2%power_ic
     ELSE IF(ALLOCATED(ss1%power_ic)) THEN
        CALL ps_conformal("power_ic",ss1%power_ic,ss3%power_ic,iout,ierr)
        ss3%power_ic = ss1%power_ic
     ELSE IF(ALLOCATED(ss2%power_ic)) THEN
        CALL ps_conformal("power_ic",ss2%power_ic,ss3%power_ic,iout,ierr)
        ss3%power_ic = ss2%power_ic
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%power_lh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%power_lh).AND.ALLOCATED(ss2%power_lh)) THEN
        CALL ps_conformal("power_lh",ss2%power_lh,ss3%power_lh,iout,ierr)
        CALL ps_conformal("power_lh",ss1%power_lh,ss3%power_lh,iout,ierr)
        if(ierr.eq.0) ss3%power_lh = w1*ss1%power_lh + (1.0_rspec-w1)*ss2%power_lh
     ELSE IF(ALLOCATED(ss1%power_lh)) THEN
        CALL ps_conformal("power_lh",ss1%power_lh,ss3%power_lh,iout,ierr)
        ss3%power_lh = ss1%power_lh
     ELSE IF(ALLOCATED(ss2%power_lh)) THEN
        CALL ps_conformal("power_lh",ss2%power_lh,ss3%power_lh,iout,ierr)
        ss3%power_lh = ss2%power_lh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%power_nbi)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%power_nbi).AND.ALLOCATED(ss2%power_nbi)) THEN
        CALL ps_conformal("power_nbi",ss2%power_nbi,ss3%power_nbi,iout,ierr)
        CALL ps_conformal("power_nbi",ss1%power_nbi,ss3%power_nbi,iout,ierr)
        if(ierr.eq.0) ss3%power_nbi = w1*ss1%power_nbi + (1.0_rspec-w1)*ss2%power_nbi
     ELSE IF(ALLOCATED(ss1%power_nbi)) THEN
        CALL ps_conformal("power_nbi",ss1%power_nbi,ss3%power_nbi,iout,ierr)
        ss3%power_nbi = ss1%power_nbi
     ELSE IF(ALLOCATED(ss2%power_nbi)) THEN
        CALL ps_conformal("power_nbi",ss2%power_nbi,ss3%power_nbi,iout,ierr)
        ss3%power_nbi = ss2%power_nbi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%power_nbi_trace)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%power_nbi_trace).AND.ALLOCATED(ss2%power_nbi_trace)) THEN
        CALL ps_conformal("power_nbi_trace",ss2%power_nbi_trace,ss3%power_nbi_trace,iout,ierr)
        CALL ps_conformal("power_nbi_trace",ss1%power_nbi_trace,ss3%power_nbi_trace,iout,ierr)
        if(ierr.eq.0) ss3%power_nbi_trace = w1*ss1%power_nbi_trace + (1.0_rspec-w1)*ss2%power_nbi_trace
     ELSE IF(ALLOCATED(ss1%power_nbi_trace)) THEN
        CALL ps_conformal("power_nbi_trace",ss1%power_nbi_trace,ss3%power_nbi_trace,iout,ierr)
        ss3%power_nbi_trace = ss1%power_nbi_trace
     ELSE IF(ALLOCATED(ss2%power_nbi_trace)) THEN
        CALL ps_conformal("power_nbi_trace",ss2%power_nbi_trace,ss3%power_nbi_trace,iout,ierr)
        ss3%power_nbi_trace = ss2%power_nbi_trace
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%prad)).AND.(cclist(ps_cnum_RAD).EQ.1)) THEN
     IF(ALLOCATED(ss1%prad).AND.ALLOCATED(ss2%prad)) THEN
        CALL ps_conformal("prad",ss2%prad,ss3%prad,iout,ierr)
        CALL ps_conformal("prad",ss1%prad,ss3%prad,iout,ierr)
        if(ierr.eq.0) ss3%prad = w1*ss1%prad + (1.0_rspec-w1)*ss2%prad
     ELSE IF(ALLOCATED(ss1%prad)) THEN
        CALL ps_conformal("prad",ss1%prad,ss3%prad,iout,ierr)
        ss3%prad = ss1%prad
     ELSE IF(ALLOCATED(ss2%prad)) THEN
        CALL ps_conformal("prad",ss2%prad,ss3%prad,iout,ierr)
        ss3%prad = ss2%prad
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%prad_br)).AND.(cclist(ps_cnum_RAD).EQ.1)) THEN
     IF(ALLOCATED(ss1%prad_br).AND.ALLOCATED(ss2%prad_br)) THEN
        CALL ps_conformal("prad_br",ss2%prad_br,ss3%prad_br,iout,ierr)
        CALL ps_conformal("prad_br",ss1%prad_br,ss3%prad_br,iout,ierr)
        if(ierr.eq.0) ss3%prad_br = w1*ss1%prad_br + (1.0_rspec-w1)*ss2%prad_br
     ELSE IF(ALLOCATED(ss1%prad_br)) THEN
        CALL ps_conformal("prad_br",ss1%prad_br,ss3%prad_br,iout,ierr)
        ss3%prad_br = ss1%prad_br
     ELSE IF(ALLOCATED(ss2%prad_br)) THEN
        CALL ps_conformal("prad_br",ss2%prad_br,ss3%prad_br,iout,ierr)
        ss3%prad_br = ss2%prad_br
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%prad_cy)).AND.(cclist(ps_cnum_RAD).EQ.1)) THEN
     IF(ALLOCATED(ss1%prad_cy).AND.ALLOCATED(ss2%prad_cy)) THEN
        CALL ps_conformal("prad_cy",ss2%prad_cy,ss3%prad_cy,iout,ierr)
        CALL ps_conformal("prad_cy",ss1%prad_cy,ss3%prad_cy,iout,ierr)
        if(ierr.eq.0) ss3%prad_cy = w1*ss1%prad_cy + (1.0_rspec-w1)*ss2%prad_cy
     ELSE IF(ALLOCATED(ss1%prad_cy)) THEN
        CALL ps_conformal("prad_cy",ss1%prad_cy,ss3%prad_cy,iout,ierr)
        ss3%prad_cy = ss1%prad_cy
     ELSE IF(ALLOCATED(ss2%prad_cy)) THEN
        CALL ps_conformal("prad_cy",ss2%prad_cy,ss3%prad_cy,iout,ierr)
        ss3%prad_cy = ss2%prad_cy
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_merge_5
