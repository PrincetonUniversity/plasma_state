! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_store_def_8(ss,icdf,iout,iertot)


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


  indim = 0
  CALL define_var("t1","R",indim,idims, &
         "time at end of IPS macro timestep", &
         "sec","PLASMA","STATE_DATA", &
         "R  t1")

  IF(ALLOCATED(ss%teflux_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("teflux_ky_tglf","R",indim,idims, &
            "electron density flux spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin teflux_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  indim = 0
  CALL define_var("Te_bdy","R",indim,idims, &
         "Electron temperature at/beyond boundary", &
         "KeV","PLASMA","STATE_DATA", &
         "R Te_bdy")

  IF(ALLOCATED(ss%te_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("te_ky_tglf","R",indim,idims, &
            "electron density fluctuation spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin te_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  indim = 0
  CALL define_var("tfinal","R",indim,idims, &
         "stop time of simulation (may be adjustable)", &
         "sec","PLASMA","SIMULATION_INIT", &
         "R  tfinal")

  IF(ALLOCATED(ss%tfrip2_log)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nr_rip",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nz_rip",idims(indim))
     CALL define_var("tfrip2_log","R",indim,idims, &
            "2nd component of TF ripple", &
            "-","RIPPLE","STATE_PROFILES", &
            "R|pclin  tfrip2_log(nR_rip,nZ_rip)")
  ENDIF

  IF(ALLOCATED(ss%tfrip_log)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nr_rip",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nz_rip",idims(indim))
     CALL define_var("tfrip_log","R",indim,idims, &
            "TF ripple: log((Bmax-Bmin)/(Bmax+Bmin))", &
            "-","RIPPLE","STATE_PROFILES", &
            "R|pclin  tfrip_log(nR_rip,nZ_rip)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("TF_Data_Info","C*80",indim,idims, &
         "information on source of toroidal field data", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "C*80   TF_Data_Info")

  IF(ALLOCATED(ss%thegrilh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("thegrilh","R",indim,idims, &
            "need description", &
            "-","LH","SHOT_CONFIGURATION", &
            "R|units=- thegrilh(nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%th_eq)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nth_eq",idims(indim))
     CALL define_var("th_eq","R",indim,idims, &
            "theta grid (EQ)", &
            "rad","EQ","SIMULATION_INIT", &
            "G|CCW   th_eq(nth_eq)")
  ENDIF

  IF(ALLOCATED(ss%Ti)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("Ti","R",indim,idims, &
            "ion temperature", &
            "keV","PLASMA","STATE_PROFILES", &
            "R|units=keV|step*ni Ti(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%tiflux_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("tiflux_ky_tglf","R",indim,idims, &
            "ion density flux spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin tiflux_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  indim = 0
  CALL define_var("tinit","R",indim,idims, &
         "start time of simulation", &
         "sec","PLASMA","SIMULATION_INIT", &
         "R  tinit")

  indim = 0
  CALL define_var("Ti_bdy","R",indim,idims, &
         "Ion temperature at/beyond boundary", &
         "KeV","PLASMA","STATE_DATA", &
         "R Ti_bdy")

  IF(ALLOCATED(ss%ti_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("ti_ky_tglf","R",indim,idims, &
            "ion density fluctuation spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin ti_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  IF(ALLOCATED(ss%tmodel)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_anomth",idims(indim))
     CALL define_var("tmodel","R",indim,idims, &
            "target temperature profile", &
            "keV","ANOM","STATE_PROFILES", &
            "R|units=keV|step*nmodel   tmodel(~nrho_anomth)")
  ENDIF

  indim = 0
  CALL define_var("tmodel_bdy","R",indim,idims, &
         "model temperature, boundary value", &
         "keV","ANOM","STATE_DATA", &
         "R|units=keV    tmodel_bdy")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
  CALL define_var("tokamak_id","N",indim,idims, &
         "Tokamak (Machine Description) ID", &
         "__NONE","PLASMA","MACHINE_DESCRIPTION", &
         "N  tokamak_id")

  IF(ALLOCATED(ss%Tornum_balcrit)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nballoon",idims(indim))
     CALL define_var("Tornum_balcrit","I",indim,idims, &
            "critical toroidal mode number for ballooning", &
            "__NONE","LMHD","STATE_DATA", &
            "I   Tornum_balcrit(nballoon)")
  ENDIF

  IF(ALLOCATED(ss%Tor_mode_label)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_mmodes",idims(indim))
     CALL define_var("Tor_mode_label","N",indim,idims, &
            "Toroidal modes to monitor", &
            "__NONE","LMHD","SIMULATION_INIT", &
            "L|toroidal_modes Tor_mode_label(mmodes)")
  ENDIF

  IF(ALLOCATED(ss%Tor_mode_No)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_mmodes",idims(indim))
     CALL define_var("Tor_mode_No","I",indim,idims, &
            "(from input) Toroidal modes monitored", &
            "__NONE","LMHD","SIMULATION_INIT", &
            "I   Tor_mode_No(mmodes)")
  ENDIF

  IF(ALLOCATED(ss%tq0_reco)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     CALL define_var("tq0_reco","R",indim,idims, &
            "torque of recombination neutral source", &
            "Nt*m","GAS","STATE_PROFILES", &
            "R|step*dV   tq0_reco(~nrho_gas)")
  ENDIF

  IF(ALLOCATED(ss%tqb0_halo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("tqb0_halo","R",indim,idims, &
            "torque of halo neutral source", &
            "Nt*m","NBI","STATE_PROFILES", &
            "R|step*dV   tqb0_halo(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%tqbe)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("tqbe","R",indim,idims, &
            "collisional beam torque to electrons", &
            "Nt*m","NBI","STATE_PROFILES", &
            "R|step*dV   tqbe(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%tqbi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("tqbi","R",indim,idims, &
            "collisional beam torque to thermal ions", &
            "Nt*m","NBI","STATE_PROFILES", &
            "R|step*dV   tqbi(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%tqbjxb)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("tqbjxb","R",indim,idims, &
            "JxB beam torque", &
            "Nt*m","NBI","STATE_PROFILES", &
            "R|step*dV   tqbjxb(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%tqbth)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("tqbth","R",indim,idims, &
            "momentum in thermalization of beam ions", &
            "Nt*m","NBI","STATE_PROFILES", &
            "R|step*dV   tqbth(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%tqcx_halo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("tqcx_halo","R",indim,idims, &
            "beam halo driven CX momentum torque", &
            "Nt*m","NBI","STATE_PROFILES", &
            "R|step*dV   tqcx_halo(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%tqcx_reco)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     CALL define_var("tqcx_reco","R",indim,idims, &
            "reco driven CX momentum torque", &
            "Nt*m","GAS","STATE_PROFILES", &
            "R|step*dV   tqcx_reco(~nrho_gas)")
  ENDIF

  IF(ALLOCATED(ss%tqioniz)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("tqioniz","R",indim,idims, &
            "ionization torque per unit influx (sc0)", &
            "Nt*m/(#/sec)","GAS","STATE_PROFILES", &
            "R|step*dV|units=Nt*m/(#/sec)     tqioniz(~nrho_gas,ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%tqqcx)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("tqqcx","R",indim,idims, &
            "cx torque per unit influx per (omeg0-omegi)", &
            "Nt*m/(#/sec)/(rad/sec)","GAS","STATE_PROFILES", &
            "R|step*dV|units=Nt*m/(#/sec)/(rad/sec)  tqqcx(~nrho_gas,ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%tqsc_halo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("tqsc_halo","R",indim,idims, &
            "beam halo torque: recapture - tqb0_halo", &
            "Nt*m","NBI","STATE_PROFILES", &
            "R|step*dV   tqsc_halo(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%tqsc_reco)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     CALL define_var("tqsc_reco","R",indim,idims, &
            "reco torque: recapture - tq0_reco", &
            "Nt*m","GAS","STATE_PROFILES", &
            "R|step*dV   tqsc_reco(~nrho_gas)")
  ENDIF

  IF(ALLOCATED(ss%tq_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_anomth",idims(indim))
     CALL define_var("tq_anom","R",indim,idims, &
            "anomalous torque", &
            "Nt*m","ANOM","STATE_PROFILES", &
            "R|units=Nt*m|step*dV  tq_anom(~nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%tq_trans)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("tq_trans","R",indim,idims, &
            "angular momentum transport (loss)", &
            "Nt*m","PLASMA","STATE_PROFILES", &
            "R|units=Nt*m|step*dV  tq_trans(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%tq_transcat)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("tq_transcat","R",indim,idims, &
            "toroidal angular momentum transport", &
            "Nt*m","PLASMA","STATE_PROFILES", &
            "R|step*dV|units=Nt*m  tq_transcat(~nrho,ntrcat)")
  ENDIF

  IF(ALLOCATED(ss%trace_flag)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("trace_flag","I",indim,idims, &
            "trace element flag", &
            "__NONE","NBI","SIMULATION_INIT", &
            "I  trace_flag(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%trcat)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("trcat","N",indim,idims, &
            "Application defined list of", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "L|TR_categories  trcat(ntrcat)")
  ENDIF

  IF(ALLOCATED(ss%triang)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("triang","R",indim,idims, &
            "triangularity (symmetrized)", &
            "-","EQ","STATE_PROFILES", &
            "R|pclin  triang(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%triangL)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("triangL","R",indim,idims, &
            "lower triangularity", &
            "-","EQ","STATE_PROFILES", &
            "R|pclin  triangL(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%triangU)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("triangU","R",indim,idims, &
            "upper triangularity", &
            "-","EQ","STATE_PROFILES", &
            "R|pclin  triangU(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%triang_miller_L)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("triang_miller_L","R",indim,idims, &
            "Miller lower triangularity", &
            "-","EQ","STATE_PROFILES", &
            "R|pclin  triang_miller_L(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%triang_miller_U)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("triang_miller_U","R",indim,idims, &
            "Miller upper triangularity", &
            "-","EQ","STATE_PROFILES", &
            "R|pclin  triang_miller_U(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%trmodels)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_256",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("trmodels","C*256",indim,idims, &
            "short description or ID of transport models", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "C*256 trmodels(ntrcat)")
  ENDIF

  IF(ALLOCATED(ss%Ts)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("Ts","R",indim,idims, &
            "thermal specie temperature", &
            "keV","PLASMA","STATE_PROFILES", &
            "R|units=keV|alias=T|step*ns  Ts(~nrho,0:nspec_th)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("Ts_Data_Info","C*80",indim,idims, &
         "information on source of Temperature data", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "C*80   Ts_Data_Info")

  IF(ALLOCATED(ss%Ts_is_input)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("Ts_is_input","I",indim,idims, &
            "specie temperature measurement flag", &
            "__NONE","PLASMA","STATE_DATA", &
            "I Ts_is_input(0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%tzflux_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("tzflux_ky_tglf","R",indim,idims, &
            "impurity density flux spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin tzflux_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  IF(ALLOCATED(ss%tz_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("tz_ky_tglf","R",indim,idims, &
            "impurity density fluctuation spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin tz_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  IF(ALLOCATED(ss%T_einj_standard)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_n_t_einj_standard",idims(indim))
     CALL define_var("T_einj_standard","R",indim,idims, &
            "T beam table energies", &
            "keV","NBI","MACHINE_DESCRIPTION", &
            "R|units=keV T_einj_standard(n_T_einj_standard)")
  ENDIF

  IF(ALLOCATED(ss%T_ffull_standard)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_n_t_einj_standard",idims(indim))
     CALL define_var("T_ffull_standard","R",indim,idims, &
            "T beam full energy fractions", &
            "-","NBI","MACHINE_DESCRIPTION", &
            "R|units=- T_ffull_standard(n_T_einj_standard)")
  ENDIF

  IF(ALLOCATED(ss%T_fhalf_standard)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_n_t_einj_standard",idims(indim))
     CALL define_var("T_fhalf_standard","R",indim,idims, &
            "T beam half energy fractions", &
            "-","NBI","MACHINE_DESCRIPTION", &
            "R|units=- T_fhalf_standard(n_T_einj_standard)")
  ENDIF

  IF(ALLOCATED(ss%upwind_pfrac_ns)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("upwind_pfrac_ns","R",indim,idims, &
            "specie density forward upwind fractions", &
            "-","PLASMA","STATE_PROFILES", &
            "R|units=-|pclin upwind_pfrac_ns(nrho,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%upwind_pfrac_omega)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("upwind_pfrac_omega","R",indim,idims, &
            "omega (ang. velocity) forward upwind fraction", &
            "-","PLASMA","STATE_PROFILES", &
            "R|pclin upwind_pfrac_omega(nrho)")
  ENDIF

  IF(ALLOCATED(ss%upwind_pfrac_Te)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("upwind_pfrac_Te","R",indim,idims, &
            "Te (electron energy advection) forward upwind fraction", &
            "-","PLASMA","STATE_PROFILES", &
            "R|pclin upwind_pfrac_Te(nrho)")
  ENDIF

  IF(ALLOCATED(ss%upwind_pfrac_Ti)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("upwind_pfrac_Ti","R",indim,idims, &
            "Ti (ion energy advection) forward upwind fraction", &
            "-","PLASMA","STATE_PROFILES", &
            "R|pclin upwind_pfrac_Ti(nrho)")
  ENDIF

  IF(ALLOCATED(ss%vee_trans)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("vee_trans","R",indim,idims, &
            "electron energy radial velocity", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|pclin   vee_trans(nrho)")
  ENDIF

  IF(ALLOCATED(ss%vee_transcat)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("vee_transcat","R",indim,idims, &
            "Electron energy radial velocity", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|units=m/sec|pclin vee_transcat(nrho,ntrcat)")
  ENDIF

  IF(ALLOCATED(ss%velb_fusi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom",idims(indim))
     CALL define_var("velb_fusi","R",indim,idims, &
            "fusion ion anom. radial velocity", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|pclin  velb_fusi(nrho_anom)")
  ENDIF

  IF(ALLOCATED(ss%velb_nbi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom",idims(indim))
     CALL define_var("velb_nbi","R",indim,idims, &
            "beam ion anom. radial velocity", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|pclin  velb_nbi(nrho_anom)")
  ENDIF

  IF(ALLOCATED(ss%velb_rfmi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom",idims(indim))
     CALL define_var("velb_rfmi","R",indim,idims, &
            "RF minority ion anom. radial velocity", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|pclin  velb_rfmi(nrho_anom)")
  ENDIF

  IF(ALLOCATED(ss%velne_nclass)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velne_nclass","R",indim,idims, &
            "electron-density-equations, advective velocity term from NCLASS model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin velne_nclass(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velnh_nclass)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velnh_nclass","R",indim,idims, &
            "main-hydrogenic-density-equations, advective velocity term from NCLASS model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin velnh_nclass(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velni_drbm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velni_drbm","R",indim,idims, &
            "ion convective velocity", &
            "m/s","ANOM","STATE_PROFILES", &
            "R|units=m/s|pclin velni_drbm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velns_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("velns_anom","R",indim,idims, &
            "density-equations, advective velocity term", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velns_anom(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%velns_glf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("velns_glf","R",indim,idims, &
            "density-equations, advective velocity term from GLF23 model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velns_glf(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%velns_gtcneo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("velns_gtcneo","R",indim,idims, &
            "density-equations, advective velocity term from GTC_NEO model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velns_gtcneo(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%velns_mmm71)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("velns_mmm71","R",indim,idims, &
            "density-equations, advective velocity term from MMM71 model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velns_mmm71(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%velns_nclass)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("velns_nclass","R",indim,idims, &
            "density-equations, advective velocity term from NCLASS model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin velns_nclass(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%velns_neo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("velns_neo","R",indim,idims, &
            "density-equations, advective velocity term from NEO model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velns_neo(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%velns_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("velns_tglf","R",indim,idims, &
            "density-equations, advective velocity term from TGLF model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin velns_tglf(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%velnx_nclass)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velnx_nclass","R",indim,idims, &
            "main-impurity-density-equations, advective velocity term from NCLASS model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin velnx_nclass(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velpphi_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velpphi_anom","R",indim,idims, &
            "momentum-equation, advective velocity term", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velpphi_anom(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velpphi_glf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velpphi_glf","R",indim,idims, &
            "momentum-equation, advective velocity term from GLF23 model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velpphi_glf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velpphi_gtcneo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velpphi_gtcneo","R",indim,idims, &
            "momentum-equation, advective velocity term from GTC_NEO model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velpphi_gtcneo(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velpphi_mmm71)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velpphi_mmm71","R",indim,idims, &
            "momentum-equation, advective velocity term from MMM71 model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velpphi_mmm71(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velpphi_neo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velpphi_neo","R",indim,idims, &
            "momentum-equation, advective velocity term from NEO model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velpphi_neo(nrho_anomth)")
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

END SUBROUTINE plasma_state_store_def_8


SUBROUTINE plasma_state_store_8(ss,icdf,iout,iertot)


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


  status = nf90_inq_varid(icdf,"t1",varid)
  status = nf90_put_var(icdf,varid,ss%t1)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: t1"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%teflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"teflux_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%teflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: teflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Te_bdy",varid)
  status = nf90_put_var(icdf,varid,ss%Te_bdy)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: Te_bdy"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%te_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"te_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%te_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: te_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tfinal",varid)
  status = nf90_put_var(icdf,varid,ss%tfinal)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: tfinal"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%tfrip2_log)) THEN
     status = nf90_inq_varid(icdf,"tfrip2_log",varid)
     status = nf90_put_var(icdf,varid,ss%tfrip2_log)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tfrip2_log"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tfrip_log)) THEN
     status = nf90_inq_varid(icdf,"tfrip_log",varid)
     status = nf90_put_var(icdf,varid,ss%tfrip_log)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tfrip_log"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"TF_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%TF_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: TF_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%thegrilh)) THEN
     status = nf90_inq_varid(icdf,"thegrilh",varid)
     status = nf90_put_var(icdf,varid,ss%thegrilh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: thegrilh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%th_eq)) THEN
     status = nf90_inq_varid(icdf,"th_eq",varid)
     status = nf90_put_var(icdf,varid,ss%th_eq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: th_eq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Ti)) THEN
     status = nf90_inq_varid(icdf,"Ti",varid)
     status = nf90_put_var(icdf,varid,ss%Ti)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Ti"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tiflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"tiflux_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%tiflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tiflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tinit",varid)
  status = nf90_put_var(icdf,varid,ss%tinit)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: tinit"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"Ti_bdy",varid)
  status = nf90_put_var(icdf,varid,ss%Ti_bdy)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: Ti_bdy"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%ti_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"ti_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%ti_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ti_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tmodel)) THEN
     status = nf90_inq_varid(icdf,"tmodel",varid)
     status = nf90_put_var(icdf,varid,ss%tmodel)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tmodel"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tmodel_bdy",varid)
  status = nf90_put_var(icdf,varid,ss%tmodel_bdy)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: tmodel_bdy"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"tokamak_id",varid)
  status = nf90_put_var(icdf,varid,ss%tokamak_id)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: tokamak_id"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%Tornum_balcrit)) THEN
     status = nf90_inq_varid(icdf,"Tornum_balcrit",varid)
     status = nf90_put_var(icdf,varid,ss%Tornum_balcrit)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Tornum_balcrit"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Tor_mode_label)) THEN
     status = nf90_inq_varid(icdf,"Tor_mode_label",varid)
     status = nf90_put_var(icdf,varid,ss%Tor_mode_label)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Tor_mode_label"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Tor_mode_No)) THEN
     status = nf90_inq_varid(icdf,"Tor_mode_No",varid)
     status = nf90_put_var(icdf,varid,ss%Tor_mode_No)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Tor_mode_No"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tq0_reco)) THEN
     status = nf90_inq_varid(icdf,"tq0_reco",varid)
     status = nf90_put_var(icdf,varid,ss%tq0_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tq0_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqb0_halo)) THEN
     status = nf90_inq_varid(icdf,"tqb0_halo",varid)
     status = nf90_put_var(icdf,varid,ss%tqb0_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tqb0_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqbe)) THEN
     status = nf90_inq_varid(icdf,"tqbe",varid)
     status = nf90_put_var(icdf,varid,ss%tqbe)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tqbe"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqbi)) THEN
     status = nf90_inq_varid(icdf,"tqbi",varid)
     status = nf90_put_var(icdf,varid,ss%tqbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tqbi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqbjxb)) THEN
     status = nf90_inq_varid(icdf,"tqbjxb",varid)
     status = nf90_put_var(icdf,varid,ss%tqbjxb)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tqbjxb"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqbth)) THEN
     status = nf90_inq_varid(icdf,"tqbth",varid)
     status = nf90_put_var(icdf,varid,ss%tqbth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tqbth"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqcx_halo)) THEN
     status = nf90_inq_varid(icdf,"tqcx_halo",varid)
     status = nf90_put_var(icdf,varid,ss%tqcx_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tqcx_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqcx_reco)) THEN
     status = nf90_inq_varid(icdf,"tqcx_reco",varid)
     status = nf90_put_var(icdf,varid,ss%tqcx_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tqcx_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqioniz)) THEN
     status = nf90_inq_varid(icdf,"tqioniz",varid)
     status = nf90_put_var(icdf,varid,ss%tqioniz)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tqioniz"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqqcx)) THEN
     status = nf90_inq_varid(icdf,"tqqcx",varid)
     status = nf90_put_var(icdf,varid,ss%tqqcx)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tqqcx"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqsc_halo)) THEN
     status = nf90_inq_varid(icdf,"tqsc_halo",varid)
     status = nf90_put_var(icdf,varid,ss%tqsc_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tqsc_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqsc_reco)) THEN
     status = nf90_inq_varid(icdf,"tqsc_reco",varid)
     status = nf90_put_var(icdf,varid,ss%tqsc_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tqsc_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tq_anom)) THEN
     status = nf90_inq_varid(icdf,"tq_anom",varid)
     status = nf90_put_var(icdf,varid,ss%tq_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tq_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tq_trans)) THEN
     status = nf90_inq_varid(icdf,"tq_trans",varid)
     status = nf90_put_var(icdf,varid,ss%tq_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tq_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tq_transcat)) THEN
     status = nf90_inq_varid(icdf,"tq_transcat",varid)
     status = nf90_put_var(icdf,varid,ss%tq_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tq_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%trace_flag)) THEN
     status = nf90_inq_varid(icdf,"trace_flag",varid)
     status = nf90_put_var(icdf,varid,ss%trace_flag)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: trace_flag"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%trcat)) THEN
     status = nf90_inq_varid(icdf,"trcat",varid)
     status = nf90_put_var(icdf,varid,ss%trcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: trcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%triang)) THEN
     status = nf90_inq_varid(icdf,"triang",varid)
     status = nf90_put_var(icdf,varid,ss%triang)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: triang"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%triangL)) THEN
     status = nf90_inq_varid(icdf,"triangL",varid)
     status = nf90_put_var(icdf,varid,ss%triangL)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: triangL"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%triangU)) THEN
     status = nf90_inq_varid(icdf,"triangU",varid)
     status = nf90_put_var(icdf,varid,ss%triangU)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: triangU"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%triang_miller_L)) THEN
     status = nf90_inq_varid(icdf,"triang_miller_L",varid)
     status = nf90_put_var(icdf,varid,ss%triang_miller_L)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: triang_miller_L"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%triang_miller_U)) THEN
     status = nf90_inq_varid(icdf,"triang_miller_U",varid)
     status = nf90_put_var(icdf,varid,ss%triang_miller_U)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: triang_miller_U"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%trmodels)) THEN
     status = nf90_inq_varid(icdf,"trmodels",varid)
     status = nf90_put_var(icdf,varid,ss%trmodels)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: trmodels"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Ts)) THEN
     status = nf90_inq_varid(icdf,"Ts",varid)
     status = nf90_put_var(icdf,varid,ss%Ts)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Ts"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Ts_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%Ts_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: Ts_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%Ts_is_input)) THEN
     status = nf90_inq_varid(icdf,"Ts_is_input",varid)
     status = nf90_put_var(icdf,varid,ss%Ts_is_input)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Ts_is_input"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tzflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"tzflux_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%tzflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tzflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tz_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"tz_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%tz_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: tz_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%T_einj_standard)) THEN
     status = nf90_inq_varid(icdf,"T_einj_standard",varid)
     status = nf90_put_var(icdf,varid,ss%T_einj_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: T_einj_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%T_ffull_standard)) THEN
     status = nf90_inq_varid(icdf,"T_ffull_standard",varid)
     status = nf90_put_var(icdf,varid,ss%T_ffull_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: T_ffull_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%T_fhalf_standard)) THEN
     status = nf90_inq_varid(icdf,"T_fhalf_standard",varid)
     status = nf90_put_var(icdf,varid,ss%T_fhalf_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: T_fhalf_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%upwind_pfrac_ns)) THEN
     status = nf90_inq_varid(icdf,"upwind_pfrac_ns",varid)
     status = nf90_put_var(icdf,varid,ss%upwind_pfrac_ns)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: upwind_pfrac_ns"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%upwind_pfrac_omega)) THEN
     status = nf90_inq_varid(icdf,"upwind_pfrac_omega",varid)
     status = nf90_put_var(icdf,varid,ss%upwind_pfrac_omega)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: upwind_pfrac_omega"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%upwind_pfrac_Te)) THEN
     status = nf90_inq_varid(icdf,"upwind_pfrac_Te",varid)
     status = nf90_put_var(icdf,varid,ss%upwind_pfrac_Te)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: upwind_pfrac_Te"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%upwind_pfrac_Ti)) THEN
     status = nf90_inq_varid(icdf,"upwind_pfrac_Ti",varid)
     status = nf90_put_var(icdf,varid,ss%upwind_pfrac_Ti)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: upwind_pfrac_Ti"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vee_trans)) THEN
     status = nf90_inq_varid(icdf,"vee_trans",varid)
     status = nf90_put_var(icdf,varid,ss%vee_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vee_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vee_transcat)) THEN
     status = nf90_inq_varid(icdf,"vee_transcat",varid)
     status = nf90_put_var(icdf,varid,ss%vee_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vee_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velb_fusi)) THEN
     status = nf90_inq_varid(icdf,"velb_fusi",varid)
     status = nf90_put_var(icdf,varid,ss%velb_fusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velb_fusi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velb_nbi)) THEN
     status = nf90_inq_varid(icdf,"velb_nbi",varid)
     status = nf90_put_var(icdf,varid,ss%velb_nbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velb_nbi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velb_rfmi)) THEN
     status = nf90_inq_varid(icdf,"velb_rfmi",varid)
     status = nf90_put_var(icdf,varid,ss%velb_rfmi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velb_rfmi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velne_nclass)) THEN
     status = nf90_inq_varid(icdf,"velne_nclass",varid)
     status = nf90_put_var(icdf,varid,ss%velne_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velne_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velnh_nclass)) THEN
     status = nf90_inq_varid(icdf,"velnh_nclass",varid)
     status = nf90_put_var(icdf,varid,ss%velnh_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velnh_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velni_drbm)) THEN
     status = nf90_inq_varid(icdf,"velni_drbm",varid)
     status = nf90_put_var(icdf,varid,ss%velni_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velni_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_anom)) THEN
     status = nf90_inq_varid(icdf,"velns_anom",varid)
     status = nf90_put_var(icdf,varid,ss%velns_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velns_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_glf)) THEN
     status = nf90_inq_varid(icdf,"velns_glf",varid)
     status = nf90_put_var(icdf,varid,ss%velns_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velns_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"velns_gtcneo",varid)
     status = nf90_put_var(icdf,varid,ss%velns_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velns_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_mmm71)) THEN
     status = nf90_inq_varid(icdf,"velns_mmm71",varid)
     status = nf90_put_var(icdf,varid,ss%velns_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velns_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_nclass)) THEN
     status = nf90_inq_varid(icdf,"velns_nclass",varid)
     status = nf90_put_var(icdf,varid,ss%velns_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velns_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_neo)) THEN
     status = nf90_inq_varid(icdf,"velns_neo",varid)
     status = nf90_put_var(icdf,varid,ss%velns_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velns_neo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_tglf)) THEN
     status = nf90_inq_varid(icdf,"velns_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%velns_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velns_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velnx_nclass)) THEN
     status = nf90_inq_varid(icdf,"velnx_nclass",varid)
     status = nf90_put_var(icdf,varid,ss%velnx_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velnx_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velpphi_anom)) THEN
     status = nf90_inq_varid(icdf,"velpphi_anom",varid)
     status = nf90_put_var(icdf,varid,ss%velpphi_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velpphi_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velpphi_glf)) THEN
     status = nf90_inq_varid(icdf,"velpphi_glf",varid)
     status = nf90_put_var(icdf,varid,ss%velpphi_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velpphi_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velpphi_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"velpphi_gtcneo",varid)
     status = nf90_put_var(icdf,varid,ss%velpphi_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velpphi_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velpphi_mmm71)) THEN
     status = nf90_inq_varid(icdf,"velpphi_mmm71",varid)
     status = nf90_put_var(icdf,varid,ss%velpphi_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velpphi_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velpphi_neo)) THEN
     status = nf90_inq_varid(icdf,"velpphi_neo",varid)
     status = nf90_put_var(icdf,varid,ss%velpphi_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velpphi_neo"
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

END SUBROUTINE plasma_state_store_8
