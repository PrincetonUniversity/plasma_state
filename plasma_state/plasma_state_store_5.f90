! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_store_def_5(ss,icdf,iout,iertot)


  !  define subset of variables and arrays for NetCDF file

  USE plasma_state_definition_mod
  USE netcdf
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to read
  INTEGER, INTENT(IN) :: icdf   ! NetCDF file ID
  INTEGER, INTENT(IN) :: iout   ! message LUN
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !--------------

  INTEGER :: varid,dimid,status,ival,idims(20),indim
  CHARACTER*80 :: nc_errmsg

  !--------------
  !  define variables...


  IF(ALLOCATED(ss%ns_bdy)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("ns_bdy","R",indim,idims, &
            "specie densities at/beyond boundary", &
            "m^-3","PLASMA","STATE_DATA", &
            "R|units=m^-3 ns_bdy(0:nspec_th)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("ns_Data_Info","C*80",indim,idims, &
         "information on source of Density data", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "C*80   ns_Data_Info")

  IF(ALLOCATED(ss%ns_is_input)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("ns_is_input","I",indim,idims, &
            "specie density measurement flag", &
            "__NONE","PLASMA","STATE_DATA", &
            "I ns_is_input(0:nspec_th)")
  ENDIF

  indim = 0
  CALL define_var("ntf_coil2","I",indim,idims, &
         "number of TF coils involved in inducing 2nd component", &
         "__NONE","RIPPLE","MACHINE_DESCRIPTION", &
         "I ntf_coil2")

  indim = 0
  CALL define_var("ntf_coils","I",indim,idims, &
         "number of TF coils", &
         "__NONE","RIPPLE","MACHINE_DESCRIPTION", &
         "I ntf_coils")

  IF(ALLOCATED(ss%nthinlh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("nthinlh","I",indim,idims, &
            "need description", &
            "__NONE","LH","SHOT_CONFIGURATION", &
            "I nthinlh(nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%Nturns)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncoils",idims(indim))
     CALL define_var("Nturns","I",indim,idims, &
            "SIGNED number of conductor turns wrapped in each coil", &
            "__NONE","EQ","MACHINE_DESCRIPTION", &
            "I  Nturns(ncoils)")
  ENDIF

  IF(ALLOCATED(ss%num_nphi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("num_nphi","I",indim,idims, &
            "number of non-zero n_phi values", &
            "__NONE","IC","SHOT_CONFIGURATION", &
            "I|ENUM:nphi  num_nphi(nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%num_nphi_vac)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("num_nphi_vac","I",indim,idims, &
            "number of non-zero n_phi values", &
            "__NONE","IC","SHOT_CONFIGURATION", &
            "I|ENUM num_nphi_vac(nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%nzflux_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("nzflux_ky_tglf","R",indim,idims, &
            "impurity density flux spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin nzflux_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  IF(ALLOCATED(ss%nz_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("nz_ky_tglf","R",indim,idims, &
            "impurity density fluctuation spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin nz_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  IF(ALLOCATED(ss%n_straps)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("n_straps","I",indim,idims, &
            "number of straps in the antenna", &
            "__NONE","IC","SHOT_CONFIGURATION", &
            "I|ENUM n_straps(nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%omeg0cx)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("omeg0cx","R",indim,idims, &
            "effective neutral angular velocity for CX torque", &
            "rad/sec","GAS","STATE_PROFILES", &
            "R|step*tqqcx|units=rad/sec  omeg0cx(~nrho_gas,ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%omeg0sc0)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("omeg0sc0","R",indim,idims, &
            "toroidal angular velocity", &
            "(rad/sec)","GAS","STATE_PROFILES", &
            "R|step*n0norm|units=(rad/sec)  omeg0sc0(~nrho_gas,nspec_gas,ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%omeg0_halo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("omeg0_halo","R",indim,idims, &
            "halo neutral toroidal angular velocity", &
            "rad/sec","NBI","STATE_PROFILES", &
            "R|units=rad/sec|step*n0_halo  omeg0_halo(~nrho_nbi,nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%omeg0_reco)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("omeg0_reco","R",indim,idims, &
            "reco neutral toroidal angular velocity", &
            "rad/sec","GAS","STATE_PROFILES", &
            "R|units=rad/sec|step*n0_reco  omeg0_reco(~nrho_gas,nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%omegat)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("omegat","R",indim,idims, &
            "ion toroidal angular velocity", &
            "rad/sec","PLASMA","STATE_PROFILES", &
            "R|units=rad/sec|step*ni  omegat(~nrho)")
  ENDIF

  indim = 0
  CALL define_var("omegat_bdy","R",indim,idims, &
         "toroidal angular velocity at/beyond boundary", &
         "rad/sec","PLASMA","STATE_DATA", &
         "R|units=rad/sec  omegat_bdy")

  IF(ALLOCATED(ss%omega_exb)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("omega_exb","R",indim,idims, &
            "ExB flow shear rate", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin omega_exb(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%p0_reco)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     CALL define_var("p0_reco","R",indim,idims, &
            "power in recombination neutral source", &
            "W","GAS","STATE_PROFILES", &
            "R|step*dV   p0_reco(~nrho_gas)")
  ENDIF

  IF(ALLOCATED(ss%pb0_halo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("pb0_halo","R",indim,idims, &
            "power in halo neutral source", &
            "W","NBI","STATE_PROFILES", &
            "R|step*dV   pb0_halo(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%pbe)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("pbe","R",indim,idims, &
            "electron heating by all beam ions", &
            "W","NBI","STATE_PROFILES", &
            "R|step*dV   pbe(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%pbi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("pbi","R",indim,idims, &
            "thermal ion heating by all beam ions", &
            "W","NBI","STATE_PROFILES", &
            "R|step*dV   pbi(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%pbth)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("pbth","R",indim,idims, &
            "thermalization of beam ions", &
            "W","NBI","STATE_PROFILES", &
            "R|step*dV   pbth(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%pcx_halo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("pcx_halo","R",indim,idims, &
            "beam halo driven CX power", &
            "W","NBI","STATE_PROFILES", &
            "R|step*dV   pcx_halo(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%pcx_reco)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     CALL define_var("pcx_reco","R",indim,idims, &
            "reco driven CX power", &
            "W","GAS","STATE_PROFILES", &
            "R|step*dV   pcx_reco(~nrho_gas)")
  ENDIF

  IF(ALLOCATED(ss%peech)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_ecrf",idims(indim))
     CALL define_var("peech","R",indim,idims, &
            "electron heating by ECH", &
            "W","EC","STATE_PROFILES", &
            "R|step*dV|units=W   peech(~nrho_ecrf)")
  ENDIF

  IF(ALLOCATED(ss%peech_src)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_ecrf",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("peech_src","R",indim,idims, &
            "ECH heating (by antenna)", &
            "W","EC","STATE_PROFILES", &
            "R|step*dV|units=W   peech_src(~nrho_ecrf,necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%pelh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_lhrf",idims(indim))
     CALL define_var("pelh","R",indim,idims, &
            "electron heating by LH", &
            "W","LH","STATE_PROFILES", &
            "R|step*dV   pelh(~nrho_lhrf)")
  ENDIF

  IF(ALLOCATED(ss%pelh_src)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_lhrf",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("pelh_src","R",indim,idims, &
            "LH electron heating by antenna", &
            "W","LH","STATE_PROFILES", &
            "R|step*dV|units=W   pelh_src(~nrho_lhrf,nlhrf_src)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("PEL_Code_Info","C*80",indim,idims, &
         "Information: code implementing PELLET component", &
         "__NONE","PEL","SIMULATION_INIT", &
         "C*80   PEL_Code_Info")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("PEL_Data_Info","C*80",indim,idims, &
         "Information: on source of pellet injection", &
         "__NONE","PEL","SIMULATION_INIT", &
         "C*80   PEL_Data_Info")

  IF(ALLOCATED(ss%pe_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_anomth",idims(indim))
     CALL define_var("pe_anom","R",indim,idims, &
            "anomalous electron heating", &
            "W","ANOM","STATE_PROFILES", &
            "R|units=W|step*dV  pe_anom(~nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%pe_trans)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("pe_trans","R",indim,idims, &
            "electron transport (loss) power", &
            "W","PLASMA","STATE_PROFILES", &
            "R|step*dV   pe_trans(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%pe_transcat)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("pe_transcat","R",indim,idims, &
            "electron transport", &
            "W","PLASMA","STATE_PROFILES", &
            "R|step*dV|units=W  pe_transcat(~nrho,ntrcat)")
  ENDIF

  IF(ALLOCATED(ss%pfuse)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_fus",idims(indim))
     CALL define_var("pfuse","R",indim,idims, &
            "electron heating by all fusion ions", &
            "W","FUS","STATE_PROFILES", &
            "R|step*dV   pfuse(~nrho_fus)")
  ENDIF

  IF(ALLOCATED(ss%pfusi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_fus",idims(indim))
     CALL define_var("pfusi","R",indim,idims, &
            "thermal ion heating by all fusion ions", &
            "W","FUS","STATE_PROFILES", &
            "R|step*dV   pfusi(~nrho_fus)")
  ENDIF

  IF(ALLOCATED(ss%pfusth)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_fus",idims(indim))
     CALL define_var("pfusth","R",indim,idims, &
            "thermalization of fusion ions", &
            "W","FUS","STATE_PROFILES", &
            "R|step*dV   pfusth(~nrho_fus)")
  ENDIF

  IF(ALLOCATED(ss%Phibsc)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("Phibsc","R",indim,idims, &
            "toroidal location of beam source", &
            "degrees","NBI","MACHINE_DESCRIPTION", &
            "R|units=degrees       Phibsc(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%phiflux_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("phiflux_ky_tglf","R",indim,idims, &
            "momentum fluctuation spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin phiflux_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  IF(ALLOCATED(ss%phit)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("phit","R",indim,idims, &
            "toroidal flux vs. rho", &
            "Wb","EQ","STATE_PROFILES", &
            "R|units=Wb|Spline_00       phit(nrho_eq)")
  ENDIF

  IF(ALLOCATED(ss%Phi_EC_launch)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("Phi_EC_launch","R",indim,idims, &
            "Phi of center of launcher", &
            "degrees","EC","MACHINE_DESCRIPTION", &
            "R|units=degrees  Phi_EC_launch(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%picrf_abs)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("picrf_abs","R",indim,idims, &
            "RF power absorbed inside plasma", &
            "W","IC","STATE_DATA", &
            "R|units=W picrf_abs(nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%picrf_ext)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("picrf_ext","R",indim,idims, &
            "RF power deposited outside plasma", &
            "W","IC","STATE_DATA", &
            "R|units=W picrf_ext(nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%picrf_nphi_srcs)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_num_nphi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_alla",idims(indim))
     CALL define_var("picrf_nphi_srcs","R",indim,idims, &
            "direct ICRF power deposition", &
            "W","IC","STATE_PROFILES", &
            "R|units=W|alias=picrf_nphi_|step*dV   picrf_nphi_srcs(~nrho_icrf,num_nphi(),nicrf_src,0:nspec_alla)")
  ENDIF

  IF(ALLOCATED(ss%picrf_srcs)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_alla",idims(indim))
     CALL define_var("picrf_srcs","R",indim,idims, &
            "direct ICRF power deposition", &
            "W","IC","STATE_PROFILES", &
            "R|units=W|alias=picrfs_|step*dV   picrf_srcs(~nrho_icrf,nicrf_src,0:nspec_alla)")
  ENDIF

  IF(ALLOCATED(ss%picrf_totals)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_alla",idims(indim))
     CALL define_var("picrf_totals","R",indim,idims, &
            "direct ICRF power deposition", &
            "W","IC","STATE_PROFILES", &
            "R|units=W|alias=picrf_|step*dV    picrf_totals(~nrho_icrf,0:nspec_alla)")
  ENDIF

  IF(ALLOCATED(ss%picth)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     CALL define_var("picth","R",indim,idims, &
            "direct thermal ion heating by ICRF", &
            "W","IC","STATE_PROFILES", &
            "R|step*dV|units=W   picth(~nrho_icrf)")
  ENDIF

  IF(ALLOCATED(ss%pilh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_lhrf",idims(indim))
     CALL define_var("pilh","R",indim,idims, &
            "ion heating by LH", &
            "W","LH","STATE_PROFILES", &
            "R|step*dV   pilh(~nrho_lhrf)")
  ENDIF

  IF(ALLOCATED(ss%pilh_src)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_lhrf",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("pilh_src","R",indim,idims, &
            "LH ion heating by antenna", &
            "W","LH","STATE_PROFILES", &
            "R|step*dV|units=W   pilh_src(~nrho_lhrf,nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%pi_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_anomth",idims(indim))
     CALL define_var("pi_anom","R",indim,idims, &
            "anomalous ion heating", &
            "W","ANOM","STATE_PROFILES", &
            "R|units=W|step*dV  pi_anom(~nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%pi_trans)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("pi_trans","R",indim,idims, &
            "ion transport (loss) power", &
            "W","PLASMA","STATE_PROFILES", &
            "R|step*dV   pi_trans(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%pi_transcat)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("pi_transcat","R",indim,idims, &
            "ion transport", &
            "W","PLASMA","STATE_PROFILES", &
            "R|step*dV|units=W  pi_transcat(~nrho,ntrcat)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("PLASMA_Code_Info","C*80",indim,idims, &
         "Information: code managing PLASMA component", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "C*80   PLASMA_Code_Info")

  IF(ALLOCATED(ss%pmine)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     CALL define_var("pmine","R",indim,idims, &
            "electron heating by minority ions", &
            "W","IC","STATE_PROFILES", &
            "R|step*dV   pmine(~nrho_icrf)")
  ENDIF

  IF(ALLOCATED(ss%pmini)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     CALL define_var("pmini","R",indim,idims, &
            "thermal ion heating by minority ions", &
            "W","IC","STATE_PROFILES", &
            "R|step*dV   pmini(~nrho_icrf)")
  ENDIF

  IF(ALLOCATED(ss%pminth)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     CALL define_var("pminth","R",indim,idims, &
            "(de)thermalization of minority ions", &
            "W","IC","STATE_PROFILES", &
            "R|step*dV   pminth(~nrho_icrf)")
  ENDIF

  IF(ALLOCATED(ss%pohme)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("pohme","R",indim,idims, &
            "Ohmic heating of electrons", &
            "W","PLASMA","STATE_PROFILES", &
            "R|units=W|step*dV   pohme(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%pot_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("pot_ky_tglf","R",indim,idims, &
            "potential fluctuation spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin pot_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  IF(ALLOCATED(ss%power_ec)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("power_ec","R",indim,idims, &
            "power on each ECRF source", &
            "W","EC","STATE_DATA", &
            "R|units=W  power_ec(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%power_ic)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("power_ic","R",indim,idims, &
            "power on each ICRF source", &
            "W","IC","STATE_DATA", &
            "R|units=W  power_ic(nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%power_lh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("power_lh","R",indim,idims, &
            "power on each LHRF source", &
            "W","LH","STATE_DATA", &
            "R|units=W  power_lh(nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%power_nbi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("power_nbi","R",indim,idims, &
            "power on each beam source", &
            "W","NBI","STATE_DATA", &
            "R|units=W  power_nbi(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%power_nbi_trace)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("power_nbi_trace","R",indim,idims, &
            "power injected as trace element (if any)", &
            "W","NBI","STATE_DATA", &
            "R|units=W  power_nbi_trace(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%prad)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_rad",idims(indim))
     CALL define_var("prad","R",indim,idims, &
            "total radiated power", &
            "W","RAD","STATE_PROFILES", &
            "R|step*dV  prad(~nrho_rad)")
  ENDIF

  IF(ALLOCATED(ss%prad_br)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_rad",idims(indim))
     CALL define_var("prad_br","R",indim,idims, &
            "bremsstrahlung contribution", &
            "W","RAD","STATE_PROFILES", &
            "R|step*dV  prad_br(~nrho_rad)")
  ENDIF

  IF(ALLOCATED(ss%prad_cy)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_rad",idims(indim))
     CALL define_var("prad_cy","R",indim,idims, &
            "cyclotron radiation contribution", &
            "W","RAD","STATE_PROFILES", &
            "R|step*dV  prad_cy(~nrho_rad)")
  ENDIF

  CONTAINS


     SUBROUTINE define_var(name,dtype,indim,idims, &
          long_name,units,component,section,spec)

        CHARACTER*(*), INTENT(in) :: name
        CHARACTER*(*), INTENT(in) :: dtype
        INTEGER, INTENT(in) :: indim
        INTEGER, dimension(:), INTENT(in) :: idims
        CHARACTER*(*), INTENT(in) :: long_name
        CHARACTER*(*), INTENT(in) :: units
        CHARACTER*(*), INTENT(in) :: component
        CHARACTER*(*), INTENT(in) :: section
        CHARACTER*(*), INTENT(in) :: spec

        integer :: itype

        IF(dtype.EQ.'R') THEN
           itype = NF90_DOUBLE

        ELSE IF (dtype.EQ.'I') THEN
           itype = NF90_INT

        ELSE
           itype = NF90_CHAR

        ENDIF

        IF(indim.EQ.0) THEN
           status=nf90_def_var(icdf,name,itype, varid=varid)

        ELSE
           status=nf90_def_var(icdf,name,itype, dimids=idims(1:indim),varid=varid)

        ENDIF

        IF(status.NE.NF90_NOERR) THEN
           WRITE(iout,*) " ?plasma_state_store: error defining variable: "//trim(name)
           CALL ncerr(status)
           iertot = iertot + 1
           RETURN
        ENDIF

        ! Variable OK, now add attributes:

        CALL add_attrib("long_name",long_name)
        IF(dtype.EQ.'R') CALL add_attrib("units",units)
        CALL add_attrib("component",component)
        CALL add_attrib("section",section)
        CALL add_attrib("specification",spec)

     END SUBROUTINE define_var

     SUBROUTINE add_attrib(name,value)
        CHARACTER*(*), intent(in) :: name
        CHARACTER*(*), intent(in) :: value

        status=nf90_put_att(icdf,varid,name,value)
        IF(status.NE.NF90_NOERR) THEN
           WRITE(iout,*) " ?plasma_state_store: error defining attribute: "//trim(name)//' = '//trim(value)
           CALL ncerr(status)
           iertot = iertot + 1
        ENDIF
     END SUBROUTINE add_attrib

     SUBROUTINE ncerr(status)
        INTEGER, INTENT(in) :: status

        nc_errmsg = NF90_strerror(status)
        write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
     END SUBROUTINE ncerr

END SUBROUTINE plasma_state_store_def_5


SUBROUTINE plasma_state_store_5(ss,icdf,iout,iertot)


  !  write subset of variables and arrays from NetCDF file

  USE plasma_state_definition_mod
  USE netcdf
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to read
  INTEGER, INTENT(IN) :: icdf   ! NetCDF file ID
  INTEGER, INTENT(IN) :: iout   ! message LUN
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !--------------

  INTEGER :: varid,dimid,status,ival,idims(20),indim
  CHARACTER*80 :: nc_errmsg

  !--------------
  !  write variables...


  IF(ALLOCATED(ss%ns_bdy)) THEN
     status = nf90_inq_varid(icdf,"ns_bdy",varid)
     status = nf90_put_var(icdf,varid,ss%ns_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ns_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ns_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%ns_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: ns_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%ns_is_input)) THEN
     status = nf90_inq_varid(icdf,"ns_is_input",varid)
     status = nf90_put_var(icdf,varid,ss%ns_is_input)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ns_is_input"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ntf_coil2",varid)
  status = nf90_put_var(icdf,varid,ss%ntf_coil2)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: ntf_coil2"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"ntf_coils",varid)
  status = nf90_put_var(icdf,varid,ss%ntf_coils)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: ntf_coils"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%nthinlh)) THEN
     status = nf90_inq_varid(icdf,"nthinlh",varid)
     status = nf90_put_var(icdf,varid,ss%nthinlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nthinlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Nturns)) THEN
     status = nf90_inq_varid(icdf,"Nturns",varid)
     status = nf90_put_var(icdf,varid,ss%Nturns)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Nturns"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%num_nphi)) THEN
     status = nf90_inq_varid(icdf,"num_nphi",varid)
     status = nf90_put_var(icdf,varid,ss%num_nphi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: num_nphi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%num_nphi_vac)) THEN
     status = nf90_inq_varid(icdf,"num_nphi_vac",varid)
     status = nf90_put_var(icdf,varid,ss%num_nphi_vac)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: num_nphi_vac"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nzflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"nzflux_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%nzflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nzflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nz_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"nz_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%nz_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nz_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%n_straps)) THEN
     status = nf90_inq_varid(icdf,"n_straps",varid)
     status = nf90_put_var(icdf,varid,ss%n_straps)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: n_straps"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%omeg0cx)) THEN
     status = nf90_inq_varid(icdf,"omeg0cx",varid)
     status = nf90_put_var(icdf,varid,ss%omeg0cx)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: omeg0cx"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%omeg0sc0)) THEN
     status = nf90_inq_varid(icdf,"omeg0sc0",varid)
     status = nf90_put_var(icdf,varid,ss%omeg0sc0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: omeg0sc0"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%omeg0_halo)) THEN
     status = nf90_inq_varid(icdf,"omeg0_halo",varid)
     status = nf90_put_var(icdf,varid,ss%omeg0_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: omeg0_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%omeg0_reco)) THEN
     status = nf90_inq_varid(icdf,"omeg0_reco",varid)
     status = nf90_put_var(icdf,varid,ss%omeg0_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: omeg0_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%omegat)) THEN
     status = nf90_inq_varid(icdf,"omegat",varid)
     status = nf90_put_var(icdf,varid,ss%omegat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: omegat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omegat_bdy",varid)
  status = nf90_put_var(icdf,varid,ss%omegat_bdy)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: omegat_bdy"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%omega_exb)) THEN
     status = nf90_inq_varid(icdf,"omega_exb",varid)
     status = nf90_put_var(icdf,varid,ss%omega_exb)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: omega_exb"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%p0_reco)) THEN
     status = nf90_inq_varid(icdf,"p0_reco",varid)
     status = nf90_put_var(icdf,varid,ss%p0_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: p0_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pb0_halo)) THEN
     status = nf90_inq_varid(icdf,"pb0_halo",varid)
     status = nf90_put_var(icdf,varid,ss%pb0_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pb0_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pbe)) THEN
     status = nf90_inq_varid(icdf,"pbe",varid)
     status = nf90_put_var(icdf,varid,ss%pbe)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pbe"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pbi)) THEN
     status = nf90_inq_varid(icdf,"pbi",varid)
     status = nf90_put_var(icdf,varid,ss%pbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pbi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pbth)) THEN
     status = nf90_inq_varid(icdf,"pbth",varid)
     status = nf90_put_var(icdf,varid,ss%pbth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pbth"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pcx_halo)) THEN
     status = nf90_inq_varid(icdf,"pcx_halo",varid)
     status = nf90_put_var(icdf,varid,ss%pcx_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pcx_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pcx_reco)) THEN
     status = nf90_inq_varid(icdf,"pcx_reco",varid)
     status = nf90_put_var(icdf,varid,ss%pcx_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pcx_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%peech)) THEN
     status = nf90_inq_varid(icdf,"peech",varid)
     status = nf90_put_var(icdf,varid,ss%peech)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: peech"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%peech_src)) THEN
     status = nf90_inq_varid(icdf,"peech_src",varid)
     status = nf90_put_var(icdf,varid,ss%peech_src)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: peech_src"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pelh)) THEN
     status = nf90_inq_varid(icdf,"pelh",varid)
     status = nf90_put_var(icdf,varid,ss%pelh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pelh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pelh_src)) THEN
     status = nf90_inq_varid(icdf,"pelh_src",varid)
     status = nf90_put_var(icdf,varid,ss%pelh_src)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pelh_src"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"PEL_Code_Info",varid)
  status = nf90_put_var(icdf,varid,ss%PEL_Code_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: PEL_Code_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"PEL_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%PEL_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: PEL_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%pe_anom)) THEN
     status = nf90_inq_varid(icdf,"pe_anom",varid)
     status = nf90_put_var(icdf,varid,ss%pe_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pe_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pe_trans)) THEN
     status = nf90_inq_varid(icdf,"pe_trans",varid)
     status = nf90_put_var(icdf,varid,ss%pe_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pe_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pe_transcat)) THEN
     status = nf90_inq_varid(icdf,"pe_transcat",varid)
     status = nf90_put_var(icdf,varid,ss%pe_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pe_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pfuse)) THEN
     status = nf90_inq_varid(icdf,"pfuse",varid)
     status = nf90_put_var(icdf,varid,ss%pfuse)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pfuse"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pfusi)) THEN
     status = nf90_inq_varid(icdf,"pfusi",varid)
     status = nf90_put_var(icdf,varid,ss%pfusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pfusi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pfusth)) THEN
     status = nf90_inq_varid(icdf,"pfusth",varid)
     status = nf90_put_var(icdf,varid,ss%pfusth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pfusth"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Phibsc)) THEN
     status = nf90_inq_varid(icdf,"Phibsc",varid)
     status = nf90_put_var(icdf,varid,ss%Phibsc)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Phibsc"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%phiflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"phiflux_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%phiflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: phiflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%phit)) THEN
     status = nf90_inq_varid(icdf,"phit",varid)
     status = nf90_put_var(icdf,varid,ss%phit)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: phit"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Phi_EC_launch)) THEN
     status = nf90_inq_varid(icdf,"Phi_EC_launch",varid)
     status = nf90_put_var(icdf,varid,ss%Phi_EC_launch)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Phi_EC_launch"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%picrf_abs)) THEN
     status = nf90_inq_varid(icdf,"picrf_abs",varid)
     status = nf90_put_var(icdf,varid,ss%picrf_abs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: picrf_abs"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%picrf_ext)) THEN
     status = nf90_inq_varid(icdf,"picrf_ext",varid)
     status = nf90_put_var(icdf,varid,ss%picrf_ext)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: picrf_ext"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%picrf_nphi_srcs)) THEN
     status = nf90_inq_varid(icdf,"picrf_nphi_srcs",varid)
     status = nf90_put_var(icdf,varid,ss%picrf_nphi_srcs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: picrf_nphi_srcs"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%picrf_srcs)) THEN
     status = nf90_inq_varid(icdf,"picrf_srcs",varid)
     status = nf90_put_var(icdf,varid,ss%picrf_srcs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: picrf_srcs"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%picrf_totals)) THEN
     status = nf90_inq_varid(icdf,"picrf_totals",varid)
     status = nf90_put_var(icdf,varid,ss%picrf_totals)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: picrf_totals"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%picth)) THEN
     status = nf90_inq_varid(icdf,"picth",varid)
     status = nf90_put_var(icdf,varid,ss%picth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: picth"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pilh)) THEN
     status = nf90_inq_varid(icdf,"pilh",varid)
     status = nf90_put_var(icdf,varid,ss%pilh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pilh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pilh_src)) THEN
     status = nf90_inq_varid(icdf,"pilh_src",varid)
     status = nf90_put_var(icdf,varid,ss%pilh_src)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pilh_src"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pi_anom)) THEN
     status = nf90_inq_varid(icdf,"pi_anom",varid)
     status = nf90_put_var(icdf,varid,ss%pi_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pi_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pi_trans)) THEN
     status = nf90_inq_varid(icdf,"pi_trans",varid)
     status = nf90_put_var(icdf,varid,ss%pi_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pi_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pi_transcat)) THEN
     status = nf90_inq_varid(icdf,"pi_transcat",varid)
     status = nf90_put_var(icdf,varid,ss%pi_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pi_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"PLASMA_Code_Info",varid)
  status = nf90_put_var(icdf,varid,ss%PLASMA_Code_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: PLASMA_Code_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%pmine)) THEN
     status = nf90_inq_varid(icdf,"pmine",varid)
     status = nf90_put_var(icdf,varid,ss%pmine)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pmine"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pmini)) THEN
     status = nf90_inq_varid(icdf,"pmini",varid)
     status = nf90_put_var(icdf,varid,ss%pmini)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pmini"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pminth)) THEN
     status = nf90_inq_varid(icdf,"pminth",varid)
     status = nf90_put_var(icdf,varid,ss%pminth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pminth"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pohme)) THEN
     status = nf90_inq_varid(icdf,"pohme",varid)
     status = nf90_put_var(icdf,varid,ss%pohme)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pohme"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pot_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"pot_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%pot_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: pot_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%power_ec)) THEN
     status = nf90_inq_varid(icdf,"power_ec",varid)
     status = nf90_put_var(icdf,varid,ss%power_ec)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: power_ec"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%power_ic)) THEN
     status = nf90_inq_varid(icdf,"power_ic",varid)
     status = nf90_put_var(icdf,varid,ss%power_ic)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: power_ic"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%power_lh)) THEN
     status = nf90_inq_varid(icdf,"power_lh",varid)
     status = nf90_put_var(icdf,varid,ss%power_lh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: power_lh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%power_nbi)) THEN
     status = nf90_inq_varid(icdf,"power_nbi",varid)
     status = nf90_put_var(icdf,varid,ss%power_nbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: power_nbi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%power_nbi_trace)) THEN
     status = nf90_inq_varid(icdf,"power_nbi_trace",varid)
     status = nf90_put_var(icdf,varid,ss%power_nbi_trace)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: power_nbi_trace"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%prad)) THEN
     status = nf90_inq_varid(icdf,"prad",varid)
     status = nf90_put_var(icdf,varid,ss%prad)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: prad"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%prad_br)) THEN
     status = nf90_inq_varid(icdf,"prad_br",varid)
     status = nf90_put_var(icdf,varid,ss%prad_br)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: prad_br"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%prad_cy)) THEN
     status = nf90_inq_varid(icdf,"prad_cy",varid)
     status = nf90_put_var(icdf,varid,ss%prad_cy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: prad_cy"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  CONTAINS


     SUBROUTINE ncerr(status)
        INTEGER, INTENT(in) :: status

        nc_errmsg = NF90_strerror(status)
        write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
     END SUBROUTINE ncerr

END SUBROUTINE plasma_state_store_5
