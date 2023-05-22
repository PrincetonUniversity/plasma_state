! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_store_def_1(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%chii_ntm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chii_ntm","R",indim,idims, &
            "Ti-equation, neoclassical diffusivity from NTM model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chii_ntm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chii_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chii_tglf","R",indim,idims, &
            "Ti-equation, diffusivity from TGLF model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chii_tglf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chii_trans)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("chii_trans","R",indim,idims, &
            "ion thermal diffusivity", &
            "m^2/sec","PLASMA","STATE_PROFILES", &
            "R|pclin chii_trans(nrho)")
  ENDIF

  IF(ALLOCATED(ss%chii_transcat)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("chii_transcat","R",indim,idims, &
            "Ion energy diffusivity", &
            "m^2/sec","PLASMA","STATE_PROFILES", &
            "R|units=m^2/sec|pclin chii_transcat(nrho,ntrcat)")
  ENDIF

  IF(ALLOCATED(ss%chii_w19)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chii_w19","R",indim,idims, &
            "Ti-equation, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  chii_w19(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chimo_trans)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("chimo_trans","R",indim,idims, &
            "angular momentum diffusivity", &
            "m^2/sec","PLASMA","STATE_PROFILES", &
            "R|pclin chimo_trans(nrho)")
  ENDIF

  IF(ALLOCATED(ss%chimo_transcat)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("chimo_transcat","R",indim,idims, &
            "Angular momentum diffusivity", &
            "m^2/sec","PLASMA","STATE_PROFILES", &
            "R|units=m^2/sec|pclin chimo_transcat(nrho,ntrcat)")
  ENDIF

  IF(ALLOCATED(ss%chiphi_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chiphi_anom","R",indim,idims, &
            "momentum-equation, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  chiphi_anom(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chiphi_drbm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chiphi_drbm","R",indim,idims, &
            "momentum-equation, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  chiphi_drbm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chiphi_glf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chiphi_glf","R",indim,idims, &
            "momentum-equation, diffusivity from GLF23 model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chiphi_glf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chiphi_gtcneo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chiphi_gtcneo","R",indim,idims, &
            "momentum-equation, neoclassical diffusivity from GTC_NEO model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chiphi_gtcneo(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chiphi_mmm71)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chiphi_mmm71","R",indim,idims, &
            "momentum-equation, diffusivity term, toroidal", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  chiphi_mmm71(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chiphi_neo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chiphi_neo","R",indim,idims, &
            "momentum-equation, neoclassical diffusivity from NEO model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chiphi_neo(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chiphi_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chiphi_tglf","R",indim,idims, &
            "momentum-equation, diffusivity from TGLF model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chiphi_tglf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chiphi_w19)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chiphi_w19","R",indim,idims, &
            "momentum-equation, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  chiphi_w19(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chitheta_mmm71)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chitheta_mmm71","R",indim,idims, &
            "momentum-equation, diffusivity term, poloidal", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  chitheta_mmm71(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%circuit_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncircuits",idims(indim))
     CALL define_var("circuit_name","N",indim,idims, &
            "Number & names of circuits", &
            "__NONE","EQ","MACHINE_DESCRIPTION", &
            "L|pf_circuits  circuit_name(ncircuits)")
  ENDIF

  IF(ALLOCATED(ss%coil_apt)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncircuits",idims(indim))
     CALL define_var("coil_apt","R",indim,idims, &
            "SIGNED circuit current", &
            "amps/turn","EQ","STATE_DATA", &
            "R|units=amps/turn coil_apt(ncircuits)")
  ENDIF

  IF(ALLOCATED(ss%coil_in_circuit)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncoils",idims(indim))
     CALL define_var("coil_in_circuit","N",indim,idims, &
            "circuit to which each coil belongs", &
            "__NONE","EQ","MACHINE_DESCRIPTION", &
            "N  coil_in_circuit(ncoils)")
  ENDIF

  IF(ALLOCATED(ss%coil_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncoils",idims(indim))
     CALL define_var("coil_name","N",indim,idims, &
            "Number & name of axisymmetric coils", &
            "__NONE","EQ","MACHINE_DESCRIPTION", &
            "L|pf_coils     coil_name(ncoils)")
  ENDIF

  IF(ALLOCATED(ss%coil_resispt)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncoils",idims(indim))
     CALL define_var("coil_resispt","R",indim,idims, &
            "coil resistance per turn", &
            "ohms/turn","EQ","MACHINE_DESCRIPTION", &
            "R|units=ohms/turn  coil_resispt(ncoils)")
  ENDIF

  IF(ALLOCATED(ss%curbeam)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("curbeam","R",indim,idims, &
            "beam ion driven current (shielded)", &
            "A","NBI","STATE_PROFILES", &
            "R|step*dA|units=A   curbeam(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%curech)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_ecrf",idims(indim))
     CALL define_var("curech","R",indim,idims, &
            "ECH current drive", &
            "A","EC","STATE_PROFILES", &
            "R|step*dA|units=A   curech(~nrho_ecrf)")
  ENDIF

  IF(ALLOCATED(ss%curech_src)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_ecrf",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("curech_src","R",indim,idims, &
            "ECH current drive (by antenna)", &
            "A","EC","STATE_PROFILES", &
            "R|step*dA|units=A   curech_src(~nrho_ecrf,necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%curfusn)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_fus",idims(indim))
     CALL define_var("curfusn","R",indim,idims, &
            "fusion ion driven current (shielded)", &
            "A","FUS","STATE_PROFILES", &
            "R|step*dA|units=A   curfusn(~nrho_fus)")
  ENDIF

  IF(ALLOCATED(ss%curich)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     CALL define_var("curich","R",indim,idims, &
            "total driven current (all IC srcs)", &
            "A","IC","STATE_PROFILES", &
            "R|units=A|step*dA   curich(~nrho_icrf)")
  ENDIF

  IF(ALLOCATED(ss%curlh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_lhrf",idims(indim))
     CALL define_var("curlh","R",indim,idims, &
            "LH current drive", &
            "A","LH","STATE_PROFILES", &
            "R|step*dA|units=A   curlh(~nrho_lhrf)")
  ENDIF

  IF(ALLOCATED(ss%curlh_src)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_lhrf",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("curlh_src","R",indim,idims, &
            "LH current drive (by antenna)", &
            "A","LH","STATE_PROFILES", &
            "R|step*dA|units=A   curlh_src(~nrho_lhrf,nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%curmino)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     CALL define_var("curmino","R",indim,idims, &
            "minority ion driven cur (shielded).", &
            "A","IC","STATE_PROFILES", &
            "R|step*dA|units=A   curmino(~nrho_icrf)")
  ENDIF

  IF(ALLOCATED(ss%curr_bootstrap)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("curr_bootstrap","R",indim,idims, &
            "Neoclassical bootstrap current", &
            "A","PLASMA","STATE_PROFILES", &
            "R|units=A|step*dA   curr_bootstrap(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%curr_ohmic)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("curr_ohmic","R",indim,idims, &
            "Ohmic current", &
            "A","PLASMA","STATE_PROFILES", &
            "R|units=A|step*dA   curr_ohmic(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%curt)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("curt","R",indim,idims, &
            "total enclosed toroidal current", &
            "A","EQ","STATE_PROFILES", &
            "R|units=A|Spline_00   curt(nrho_eq)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("CUR_Data_Info","C*80",indim,idims, &
         "information on source of plasma current data", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "C*80   CUR_Data_Info")

  IF(ALLOCATED(ss%cur_rw)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_rw",idims(indim))
     CALL define_var("cur_rw","R",indim,idims, &
            "current carried by fast electrons", &
            "A","RUNAWAY","STATE_PROFILES", &
            "R|step*dA|units=A   cur_rw(~nrho_rw)")
  ENDIF

  IF(ALLOCATED(ss%d2R_geo_dRHOdTH)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nth_eq",idims(indim))
     CALL define_var("d2R_geo_dRHOdTH","R",indim,idims, &
            "flux surfaces R(rho,theta)", &
            "m/-/rad","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit R_geo(nrho_eq,nth_eq)")
  ENDIF

  IF(ALLOCATED(ss%d2Z_geo_dRHOdTH)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nth_eq",idims(indim))
     CALL define_var("d2Z_geo_dRHOdTH","R",indim,idims, &
            "flux surfaces Z(rho,theta)", &
            "m/-/rad","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit Z_geo(nrho_eq,nth_eq)")
  ENDIF

  IF(ALLOCATED(ss%db_mtm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("db_mtm","R",indim,idims, &
            "dB/B from MTM model", &
            "-","ANOM","STATE_PROFILES", &
            "R|units=-|pclin db_mtm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%dcur_rw_dVloop)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_rw",idims(indim))
     CALL define_var("dcur_rw_dVloop","R",indim,idims, &
            "sensitivity to loop volts", &
            "A/Volt","RUNAWAY","STATE_PROFILES", &
            "R|step*dA|units=A/Volt dcur_rw_dVloop(~nrho_rw)")
  ENDIF

  IF(ALLOCATED(ss%DeltaW_code)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ndelta_w",idims(indim))
     CALL define_var("DeltaW_code","N",indim,idims, &
            "name(s) of delta(W) code(s) used", &
            "__NONE","LMHD","SIMULATION_INIT", &
            "L|Delta_W_codes  DeltaW_code(ndelta_w)")
  ENDIF

  IF(ALLOCATED(ss%DeltaW_mapper)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ndelta_w",idims(indim))
     CALL define_var("DeltaW_mapper","N",indim,idims, &
            "mapper code used for each delta(W) code", &
            "__NONE","LMHD","SIMULATION_INIT", &
            "N  DeltaW_mapper(ndelta_W)")
  ENDIF

  IF(ALLOCATED(ss%DeltaW_stable)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_mmodes",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ndelta_w",idims(indim))
     CALL define_var("DeltaW_stable","I",indim,idims, &
            "0: stable; 1: unstable;", &
            "__NONE","LMHD","STATE_DATA", &
            "I   DeltaW_stable(mmodes,ndelta_W)")
  ENDIF

  IF(ALLOCATED(ss%DeltaW_status)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ndelta_w",idims(indim))
     CALL define_var("DeltaW_status","I",indim,idims, &
            "completion status: Delta(W) codes", &
            "__NONE","LMHD","STATE_DATA", &
            "I   DeltaW_status(ndelta_W)")
  ENDIF

  IF(ALLOCATED(ss%difb_fusi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom",idims(indim))
     CALL define_var("difb_fusi","R",indim,idims, &
            "fusion ion anomalous diffusivity", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|pclin  difb_fusi(nrho_anom)")
  ENDIF

  IF(ALLOCATED(ss%difb_nbi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom",idims(indim))
     CALL define_var("difb_nbi","R",indim,idims, &
            "beam ion anomalous diffusivity", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|pclin  difb_nbi(nrho_anom)")
  ENDIF

  IF(ALLOCATED(ss%difb_rfmi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom",idims(indim))
     CALL define_var("difb_rfmi","R",indim,idims, &
            "RF minority ion anomalous diffusivity", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|pclin  difb_rfmi(nrho_anom)")
  ENDIF

  IF(ALLOCATED(ss%diff_trans)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("diff_trans","R",indim,idims, &
            "specie ptcl diffusivity", &
            "m^2/sec","PLASMA","STATE_PROFILES", &
            "R|units=m^2/sec|pclin diff_trans(nrho,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%diff_transcat)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("diff_transcat","R",indim,idims, &
            "specie diffusivity", &
            "m^2/sec","PLASMA","STATE_PROFILES", &
            "R|units=m^2/sec|pclin diff_transcat(nrho,0:nspec_th,ntrcat)")
  ENDIF

  IF(ALLOCATED(ss%difne_nclass)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("difne_nclass","R",indim,idims, &
            "electron-density-equations, neoclassical diffusivity from NCLASS model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin difne_nclass(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%difnh_nclass)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("difnh_nclass","R",indim,idims, &
            "main-hydrogenic-density-equations, neoclassical diffusivity from NCLASS model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin difnh_nclass(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%difns_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("difns_anom","R",indim,idims, &
            "density-equations, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  difns_anom(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%difns_drbm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("difns_drbm","R",indim,idims, &
            "density-equations, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  difns_drbm(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%difns_glf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("difns_glf","R",indim,idims, &
            "density-equations, diffusivity from GLF23 model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin difns_glf(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%difns_gtcneo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("difns_gtcneo","R",indim,idims, &
            "density-equations, neoclassical diffusivity from GTC_NEO model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin difns_gtcneo(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%difns_mmm71)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("difns_mmm71","R",indim,idims, &
            "density-equations, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  difns_mmm71(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%difns_nclass)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("difns_nclass","R",indim,idims, &
            "density-equations, neoclassical diffusivity from NCLASS model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin difns_nclass(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%difns_neo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("difns_neo","R",indim,idims, &
            "density-equations, neoclassical diffusivity from NEO model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin difns_neo(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%difns_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("difns_tglf","R",indim,idims, &
            "density-equations, diffusivity from TGLF model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin difns_tglf(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%difns_w19)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("difns_w19","R",indim,idims, &
            "density-equations, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  difns_w19(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%difnx_nclass)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("difnx_nclass","R",indim,idims, &
            "main-impurity-density-equations, neoclassical diffusivity from NCLASS model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin difnx_nclass(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%difnz_mmm71)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("difnz_mmm71","R",indim,idims, &
            "impurity ion diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  difnz_mmm71(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%dist_fun)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_256",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_alla",idims(indim))
     CALL define_var("dist_fun","F",indim,idims, &
            "distribution function filenames", &
            "__NONE","IC","STATE_DATA", &
            "F  dist_fun(0:nspec_alla)")
  ENDIF

  indim = 0
  CALL define_var("dn0out","R",indim,idims, &
         "Hydrogenic neutral density in scrape off region", &
         "m^-3","GAS","STATE_DATA", &
         "R|units=m^-3  dn0out")

  IF(ALLOCATED(ss%dQedGrdte_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("dQedGrdte_tglf","R",indim,idims, &
            "Te flux gradient term from TGLF model", &
            "1/(m*s)","ANOM","STATE_PROFILES", &
            "R|units=1/(m*s)|pclin dQedGrdte_tglf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%dQedGrdti_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("dQedGrdti_tglf","R",indim,idims, &
            "Te flux gradient term from TGLF model", &
            "1/(m*s)","ANOM","STATE_PROFILES", &
            "R|units=1/(m*s)|pclin dQedGrdti_tglf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%dQidGrdte_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("dQidGrdte_tglf","R",indim,idims, &
            "Ti flux gradient term from TGLF model", &
            "1/(m*s)","ANOM","STATE_PROFILES", &
            "R|units=1/(m*s)|pclin dQidGrdte_tglf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%dQidGrdti_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("dQidGrdti_tglf","R",indim,idims, &
            "Ti flux gradient term from TGLF model", &
            "1/(m*s)","ANOM","STATE_PROFILES", &
            "R|units=1/(m*s)|pclin dQidGrdti_tglf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%dR0_momeq_dRHO)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("dR0_momeq_dRHO","R",indim,idims, &
            "R0 of flux surface", &
            "m/-","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit  R0_momeq(nrho_eq)")
  ENDIF

  IF(ALLOCATED(ss%dR_geo_dRHO)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nth_eq",idims(indim))
     CALL define_var("dR_geo_dRHO","R",indim,idims, &
            "flux surfaces R(rho,theta)", &
            "m/-","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit R_geo(nrho_eq,nth_eq)")
  ENDIF

  IF(ALLOCATED(ss%dR_geo_dTH)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nth_eq",idims(indim))
     CALL define_var("dR_geo_dTH","R",indim,idims, &
            "flux surfaces R(rho,theta)", &
            "m/rad","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit R_geo(nrho_eq,nth_eq)")
  ENDIF

  IF(ALLOCATED(ss%dxRjcos_momeq_dRHO)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_neqmom",idims(indim))
     CALL define_var("dxRjcos_momeq_dRHO","R",indim,idims, &
            "scaled R cos moments", &
            "m/-","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit  xRjcos_momeq(nrho_eq,neqmom)")
  ENDIF

  IF(ALLOCATED(ss%dxRjsin_momeq_dRHO)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_neqmom",idims(indim))
     CALL define_var("dxRjsin_momeq_dRHO","R",indim,idims, &
            "scaled R sin moments", &
            "m/-","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit  xRjsin_momeq(nrho_eq,neqmom)")
  ENDIF

  IF(ALLOCATED(ss%dxZjcos_momeq_dRHO)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_neqmom",idims(indim))
     CALL define_var("dxZjcos_momeq_dRHO","R",indim,idims, &
            "scaled Z cos moments", &
            "m/-","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit  xZjcos_momeq(nrho_eq,neqmom)")
  ENDIF

  IF(ALLOCATED(ss%dxZjsin_momeq_dRHO)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_neqmom",idims(indim))
     CALL define_var("dxZjsin_momeq_dRHO","R",indim,idims, &
            "scaled Z sin moments", &
            "m/-","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit  xZjsin_momeq(nrho_eq,neqmom)")
  ENDIF

  IF(ALLOCATED(ss%dx_fshield)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("dx_fshield","R",indim,idims, &
            "distance, antenna to Faraday shield", &
            "m","IC","MACHINE_DESCRIPTION", &
            "R|units=m dx_fshield(nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%dZ0_momeq_dRHO)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("dZ0_momeq_dRHO","R",indim,idims, &
            "Z0 of flux surface", &
            "m/-","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit  Z0_momeq(nrho_eq)")
  ENDIF

  IF(ALLOCATED(ss%dZ_geo_dRHO)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nth_eq",idims(indim))
     CALL define_var("dZ_geo_dRHO","R",indim,idims, &
            "flux surfaces Z(rho,theta)", &
            "m/-","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit Z_geo(nrho_eq,nth_eq)")
  ENDIF

  IF(ALLOCATED(ss%dZ_geo_dTH)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nth_eq",idims(indim))
     CALL define_var("dZ_geo_dTH","R",indim,idims, &
            "flux surfaces Z(rho,theta)", &
            "m/rad","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit Z_geo(nrho_eq,nth_eq)")
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

END SUBROUTINE plasma_state_store_def_1


SUBROUTINE plasma_state_store_1(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%chii_ntm)) THEN
     status = nf90_inq_varid(icdf,"chii_ntm",varid)
     status = nf90_put_var(icdf,varid,ss%chii_ntm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chii_ntm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_tglf)) THEN
     status = nf90_inq_varid(icdf,"chii_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%chii_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chii_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_trans)) THEN
     status = nf90_inq_varid(icdf,"chii_trans",varid)
     status = nf90_put_var(icdf,varid,ss%chii_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chii_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_transcat)) THEN
     status = nf90_inq_varid(icdf,"chii_transcat",varid)
     status = nf90_put_var(icdf,varid,ss%chii_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chii_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_w19)) THEN
     status = nf90_inq_varid(icdf,"chii_w19",varid)
     status = nf90_put_var(icdf,varid,ss%chii_w19)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chii_w19"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chimo_trans)) THEN
     status = nf90_inq_varid(icdf,"chimo_trans",varid)
     status = nf90_put_var(icdf,varid,ss%chimo_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chimo_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chimo_transcat)) THEN
     status = nf90_inq_varid(icdf,"chimo_transcat",varid)
     status = nf90_put_var(icdf,varid,ss%chimo_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chimo_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_anom)) THEN
     status = nf90_inq_varid(icdf,"chiphi_anom",varid)
     status = nf90_put_var(icdf,varid,ss%chiphi_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chiphi_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_drbm)) THEN
     status = nf90_inq_varid(icdf,"chiphi_drbm",varid)
     status = nf90_put_var(icdf,varid,ss%chiphi_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chiphi_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_glf)) THEN
     status = nf90_inq_varid(icdf,"chiphi_glf",varid)
     status = nf90_put_var(icdf,varid,ss%chiphi_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chiphi_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"chiphi_gtcneo",varid)
     status = nf90_put_var(icdf,varid,ss%chiphi_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chiphi_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_mmm71)) THEN
     status = nf90_inq_varid(icdf,"chiphi_mmm71",varid)
     status = nf90_put_var(icdf,varid,ss%chiphi_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chiphi_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_neo)) THEN
     status = nf90_inq_varid(icdf,"chiphi_neo",varid)
     status = nf90_put_var(icdf,varid,ss%chiphi_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chiphi_neo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_tglf)) THEN
     status = nf90_inq_varid(icdf,"chiphi_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%chiphi_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chiphi_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_w19)) THEN
     status = nf90_inq_varid(icdf,"chiphi_w19",varid)
     status = nf90_put_var(icdf,varid,ss%chiphi_w19)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chiphi_w19"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chitheta_mmm71)) THEN
     status = nf90_inq_varid(icdf,"chitheta_mmm71",varid)
     status = nf90_put_var(icdf,varid,ss%chitheta_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chitheta_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%circuit_name)) THEN
     status = nf90_inq_varid(icdf,"circuit_name",varid)
     status = nf90_put_var(icdf,varid,ss%circuit_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: circuit_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%coil_apt)) THEN
     status = nf90_inq_varid(icdf,"coil_apt",varid)
     status = nf90_put_var(icdf,varid,ss%coil_apt)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: coil_apt"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%coil_in_circuit)) THEN
     status = nf90_inq_varid(icdf,"coil_in_circuit",varid)
     status = nf90_put_var(icdf,varid,ss%coil_in_circuit)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: coil_in_circuit"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%coil_name)) THEN
     status = nf90_inq_varid(icdf,"coil_name",varid)
     status = nf90_put_var(icdf,varid,ss%coil_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: coil_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%coil_resispt)) THEN
     status = nf90_inq_varid(icdf,"coil_resispt",varid)
     status = nf90_put_var(icdf,varid,ss%coil_resispt)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: coil_resispt"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curbeam)) THEN
     status = nf90_inq_varid(icdf,"curbeam",varid)
     status = nf90_put_var(icdf,varid,ss%curbeam)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: curbeam"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curech)) THEN
     status = nf90_inq_varid(icdf,"curech",varid)
     status = nf90_put_var(icdf,varid,ss%curech)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: curech"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curech_src)) THEN
     status = nf90_inq_varid(icdf,"curech_src",varid)
     status = nf90_put_var(icdf,varid,ss%curech_src)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: curech_src"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curfusn)) THEN
     status = nf90_inq_varid(icdf,"curfusn",varid)
     status = nf90_put_var(icdf,varid,ss%curfusn)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: curfusn"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curich)) THEN
     status = nf90_inq_varid(icdf,"curich",varid)
     status = nf90_put_var(icdf,varid,ss%curich)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: curich"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curlh)) THEN
     status = nf90_inq_varid(icdf,"curlh",varid)
     status = nf90_put_var(icdf,varid,ss%curlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: curlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curlh_src)) THEN
     status = nf90_inq_varid(icdf,"curlh_src",varid)
     status = nf90_put_var(icdf,varid,ss%curlh_src)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: curlh_src"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curmino)) THEN
     status = nf90_inq_varid(icdf,"curmino",varid)
     status = nf90_put_var(icdf,varid,ss%curmino)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: curmino"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curr_bootstrap)) THEN
     status = nf90_inq_varid(icdf,"curr_bootstrap",varid)
     status = nf90_put_var(icdf,varid,ss%curr_bootstrap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: curr_bootstrap"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curr_ohmic)) THEN
     status = nf90_inq_varid(icdf,"curr_ohmic",varid)
     status = nf90_put_var(icdf,varid,ss%curr_ohmic)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: curr_ohmic"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curt)) THEN
     status = nf90_inq_varid(icdf,"curt",varid)
     status = nf90_put_var(icdf,varid,ss%curt)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: curt"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"CUR_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%CUR_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: CUR_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%cur_rw)) THEN
     status = nf90_inq_varid(icdf,"cur_rw",varid)
     status = nf90_put_var(icdf,varid,ss%cur_rw)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: cur_rw"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%d2R_geo_dRHOdTH)) THEN
     status = nf90_inq_varid(icdf,"d2R_geo_dRHOdTH",varid)
     status = nf90_put_var(icdf,varid,ss%d2R_geo_dRHOdTH)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: d2R_geo_dRHOdTH"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%d2Z_geo_dRHOdTH)) THEN
     status = nf90_inq_varid(icdf,"d2Z_geo_dRHOdTH",varid)
     status = nf90_put_var(icdf,varid,ss%d2Z_geo_dRHOdTH)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: d2Z_geo_dRHOdTH"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%db_mtm)) THEN
     status = nf90_inq_varid(icdf,"db_mtm",varid)
     status = nf90_put_var(icdf,varid,ss%db_mtm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: db_mtm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dcur_rw_dVloop)) THEN
     status = nf90_inq_varid(icdf,"dcur_rw_dVloop",varid)
     status = nf90_put_var(icdf,varid,ss%dcur_rw_dVloop)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dcur_rw_dVloop"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%DeltaW_code)) THEN
     status = nf90_inq_varid(icdf,"DeltaW_code",varid)
     status = nf90_put_var(icdf,varid,ss%DeltaW_code)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: DeltaW_code"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%DeltaW_mapper)) THEN
     status = nf90_inq_varid(icdf,"DeltaW_mapper",varid)
     status = nf90_put_var(icdf,varid,ss%DeltaW_mapper)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: DeltaW_mapper"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%DeltaW_stable)) THEN
     status = nf90_inq_varid(icdf,"DeltaW_stable",varid)
     status = nf90_put_var(icdf,varid,ss%DeltaW_stable)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: DeltaW_stable"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%DeltaW_status)) THEN
     status = nf90_inq_varid(icdf,"DeltaW_status",varid)
     status = nf90_put_var(icdf,varid,ss%DeltaW_status)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: DeltaW_status"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difb_fusi)) THEN
     status = nf90_inq_varid(icdf,"difb_fusi",varid)
     status = nf90_put_var(icdf,varid,ss%difb_fusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difb_fusi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difb_nbi)) THEN
     status = nf90_inq_varid(icdf,"difb_nbi",varid)
     status = nf90_put_var(icdf,varid,ss%difb_nbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difb_nbi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difb_rfmi)) THEN
     status = nf90_inq_varid(icdf,"difb_rfmi",varid)
     status = nf90_put_var(icdf,varid,ss%difb_rfmi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difb_rfmi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%diff_trans)) THEN
     status = nf90_inq_varid(icdf,"diff_trans",varid)
     status = nf90_put_var(icdf,varid,ss%diff_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: diff_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%diff_transcat)) THEN
     status = nf90_inq_varid(icdf,"diff_transcat",varid)
     status = nf90_put_var(icdf,varid,ss%diff_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: diff_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difne_nclass)) THEN
     status = nf90_inq_varid(icdf,"difne_nclass",varid)
     status = nf90_put_var(icdf,varid,ss%difne_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difne_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difnh_nclass)) THEN
     status = nf90_inq_varid(icdf,"difnh_nclass",varid)
     status = nf90_put_var(icdf,varid,ss%difnh_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difnh_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_anom)) THEN
     status = nf90_inq_varid(icdf,"difns_anom",varid)
     status = nf90_put_var(icdf,varid,ss%difns_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difns_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_drbm)) THEN
     status = nf90_inq_varid(icdf,"difns_drbm",varid)
     status = nf90_put_var(icdf,varid,ss%difns_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difns_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_glf)) THEN
     status = nf90_inq_varid(icdf,"difns_glf",varid)
     status = nf90_put_var(icdf,varid,ss%difns_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difns_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"difns_gtcneo",varid)
     status = nf90_put_var(icdf,varid,ss%difns_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difns_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_mmm71)) THEN
     status = nf90_inq_varid(icdf,"difns_mmm71",varid)
     status = nf90_put_var(icdf,varid,ss%difns_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difns_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_nclass)) THEN
     status = nf90_inq_varid(icdf,"difns_nclass",varid)
     status = nf90_put_var(icdf,varid,ss%difns_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difns_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_neo)) THEN
     status = nf90_inq_varid(icdf,"difns_neo",varid)
     status = nf90_put_var(icdf,varid,ss%difns_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difns_neo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_tglf)) THEN
     status = nf90_inq_varid(icdf,"difns_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%difns_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difns_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_w19)) THEN
     status = nf90_inq_varid(icdf,"difns_w19",varid)
     status = nf90_put_var(icdf,varid,ss%difns_w19)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difns_w19"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difnx_nclass)) THEN
     status = nf90_inq_varid(icdf,"difnx_nclass",varid)
     status = nf90_put_var(icdf,varid,ss%difnx_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difnx_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difnz_mmm71)) THEN
     status = nf90_inq_varid(icdf,"difnz_mmm71",varid)
     status = nf90_put_var(icdf,varid,ss%difnz_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: difnz_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dist_fun)) THEN
     status = nf90_inq_varid(icdf,"dist_fun",varid)
     status = nf90_put_var(icdf,varid,ss%dist_fun)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dist_fun"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"dn0out",varid)
  status = nf90_put_var(icdf,varid,ss%dn0out)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: dn0out"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%dQedGrdte_tglf)) THEN
     status = nf90_inq_varid(icdf,"dQedGrdte_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%dQedGrdte_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dQedGrdte_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dQedGrdti_tglf)) THEN
     status = nf90_inq_varid(icdf,"dQedGrdti_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%dQedGrdti_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dQedGrdti_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dQidGrdte_tglf)) THEN
     status = nf90_inq_varid(icdf,"dQidGrdte_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%dQidGrdte_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dQidGrdte_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dQidGrdti_tglf)) THEN
     status = nf90_inq_varid(icdf,"dQidGrdti_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%dQidGrdti_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dQidGrdti_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dR0_momeq_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dR0_momeq_dRHO",varid)
     status = nf90_put_var(icdf,varid,ss%dR0_momeq_dRHO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dR0_momeq_dRHO"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dR_geo_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dR_geo_dRHO",varid)
     status = nf90_put_var(icdf,varid,ss%dR_geo_dRHO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dR_geo_dRHO"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dR_geo_dTH)) THEN
     status = nf90_inq_varid(icdf,"dR_geo_dTH",varid)
     status = nf90_put_var(icdf,varid,ss%dR_geo_dTH)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dR_geo_dTH"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dxRjcos_momeq_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dxRjcos_momeq_dRHO",varid)
     status = nf90_put_var(icdf,varid,ss%dxRjcos_momeq_dRHO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dxRjcos_momeq_dRHO"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dxRjsin_momeq_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dxRjsin_momeq_dRHO",varid)
     status = nf90_put_var(icdf,varid,ss%dxRjsin_momeq_dRHO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dxRjsin_momeq_dRHO"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dxZjcos_momeq_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dxZjcos_momeq_dRHO",varid)
     status = nf90_put_var(icdf,varid,ss%dxZjcos_momeq_dRHO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dxZjcos_momeq_dRHO"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dxZjsin_momeq_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dxZjsin_momeq_dRHO",varid)
     status = nf90_put_var(icdf,varid,ss%dxZjsin_momeq_dRHO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dxZjsin_momeq_dRHO"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dx_fshield)) THEN
     status = nf90_inq_varid(icdf,"dx_fshield",varid)
     status = nf90_put_var(icdf,varid,ss%dx_fshield)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dx_fshield"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dZ0_momeq_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dZ0_momeq_dRHO",varid)
     status = nf90_put_var(icdf,varid,ss%dZ0_momeq_dRHO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dZ0_momeq_dRHO"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dZ_geo_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dZ_geo_dRHO",varid)
     status = nf90_put_var(icdf,varid,ss%dZ_geo_dRHO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dZ_geo_dRHO"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dZ_geo_dTH)) THEN
     status = nf90_inq_varid(icdf,"dZ_geo_dTH",varid)
     status = nf90_put_var(icdf,varid,ss%dZ_geo_dTH)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: dZ_geo_dTH"
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

END SUBROUTINE plasma_state_store_1
