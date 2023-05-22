! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_store_def_3(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%grho2)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("grho2","R",indim,idims, &
            "<|grad(rho)|^2>", &
            "m^-2","EQ","STATE_PROFILES", &
            "R|units=m^-2|pclin grho2(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%grho2b2i)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("grho2b2i","R",indim,idims, &
            "<|grad(rho)|^2/B^2>", &
            "T^-2*m^-2","EQ","STATE_PROFILES", &
            "R|units=T^-2*m^-2|pclin grho2b2i(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%grho2r2i)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("grho2r2i","R",indim,idims, &
            "<|grad(rho)|^2/R^2>", &
            "m^-4","EQ","STATE_PROFILES", &
            "R|units=m^-4|pclin grho2r2i(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%grho2r3i)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("grho2r3i","R",indim,idims, &
            "<|grad(rho)|^2/R^3>", &
            "m^-5","EQ","STATE_PROFILES", &
            "R|units=m^-5|pclin grho2r3i(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%gri)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("gri","R",indim,idims, &
            "<1/R>", &
            "m^-2","EQ","STATE_PROFILES", &
            "R|units=m^-2|pclin gri(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%grirhoi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("grirhoi","R",indim,idims, &
            "<1/(R*|grad(rho)|)>", &
            "-","EQ","STATE_PROFILES", &
            "R|units=-|pclin grirhoi(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%growthrate_drbm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("growthrate_drbm","R",indim,idims, &
            "growthrate from DRBM model", &
            "1/sec","ANOM","STATE_PROFILES", &
            "R|units=1/sec|pclin growthrate_drbm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%growthrate_etg)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("growthrate_etg","R",indim,idims, &
            "growthrate from ETG model", &
            "1/sec","ANOM","STATE_PROFILES", &
            "R|units=1/sec|pclin growthrate_etg(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%growthrate_glf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("growthrate_glf","R",indim,idims, &
            "growthrate from GLF23 model", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin growthrate_glf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%growthrate_max_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("growthrate_max_tglf","R",indim,idims, &
            "maximum growthrate from TGLF model", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin growthrate_max_tglf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%growthrate_mmm71)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_mmm",idims(indim))
     CALL define_var("growthrate_mmm71","R",indim,idims, &
            "growthrate from mmm71 model", &
            "1/sec","ANOM","STATE_PROFILES", &
            "R|units=1/sec|pclin growthrate_mmm71(nrho_anomth,nky_mmm)")
  ENDIF

  IF(ALLOCATED(ss%growthrate_mtm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("growthrate_mtm","R",indim,idims, &
            "growthrate from MTM model", &
            "1/sec","ANOM","STATE_PROFILES", &
            "R|units=1/sec|pclin growthrate_mtm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%growthrate_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("growthrate_tglf","R",indim,idims, &
            "growthrate from TGLF model", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin growthrate_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  IF(ALLOCATED(ss%growthrate_vpar)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("growthrate_vpar","R",indim,idims, &
            "growthrate from parallel velocity shear", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin growthrate_vpar(nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%gs_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("gs_name","N",indim,idims, &
            "number & name of edge neutral gas sources", &
            "__NONE","GAS","SHOT_CONFIGURATION", &
            "L|gas_source    gs_name(ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%g_eq)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("g_eq","R",indim,idims, &
            "equilibrium R*|B_phi|", &
            "T*m","EQ","STATE_PROFILES", &
            "R|units=T*m|Spline_00      g_eq(nrho_eq)")
  ENDIF

  IF(ALLOCATED(ss%hegrilh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("hegrilh","R",indim,idims, &
            "need description", &
            "-","LH","SHOT_CONFIGURATION", &
            "R|units=- hegrilh(nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%Hsize_coil)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncoils",idims(indim))
     CALL define_var("Hsize_coil","R",indim,idims, &
            "(Rotatable) Horizontal coil base size", &
            "m","EQ","MACHINE_DESCRIPTION", &
            "R|units=m  Hsize_coil(ncoils)")
  ENDIF

  IF(ALLOCATED(ss%H_einj_standard)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_n_h_einj_standard",idims(indim))
     CALL define_var("H_einj_standard","R",indim,idims, &
            "H beam table energies", &
            "keV","NBI","MACHINE_DESCRIPTION", &
            "R|units=keV H_einj_standard(n_H_einj_standard)")
  ENDIF

  IF(ALLOCATED(ss%H_ffull_standard)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_n_h_einj_standard",idims(indim))
     CALL define_var("H_ffull_standard","R",indim,idims, &
            "H beam full energy fractions", &
            "-","NBI","MACHINE_DESCRIPTION", &
            "R|units=- H_ffull_standard(n_H_einj_standard)")
  ENDIF

  IF(ALLOCATED(ss%H_fhalf_standard)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_n_h_einj_standard",idims(indim))
     CALL define_var("H_fhalf_standard","R",indim,idims, &
            "H beam half energy fractions", &
            "-","NBI","MACHINE_DESCRIPTION", &
            "R|units=- H_fhalf_standard(n_H_einj_standard)")
  ENDIF

  IF(ALLOCATED(ss%icircuit_coil)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncoils",idims(indim))
     CALL define_var("icircuit_coil","I",indim,idims, &
            "index of circuit to which each coil belongs", &
            "__NONE","EQ","SIMULATION_INIT", &
            "I  icircuit_coil(ncoils)")
  ENDIF

  IF(ALLOCATED(ss%icrf_src_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("icrf_src_name","N",indim,idims, &
            "number & name of ICRF sources", &
            "__NONE","IC","MACHINE_DESCRIPTION", &
            "L|icrf_source   icrf_src_name(nicrf_src)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("IC_Code_Info","C*80",indim,idims, &
         "Information: code implementing IC component", &
         "__NONE","IC","SIMULATION_INIT", &
         "C*80   IC_Code_Info")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("IC_Data_Info","C*80",indim,idims, &
         "information on source of ICRF power data", &
         "__NONE","IC","SIMULATION_INIT", &
         "C*80   IC_Data_Info")

  IF(ALLOCATED(ss%Imag_ant_coef)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_num_nphi_vac",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("Imag_ant_coef","R",indim,idims, &
            "imag part of Fourier Coef", &
            "Amps/m","IC","SHOT_CONFIGURATION", &
            "R|units=Amps/m Imag_ant_coef(num_nphi_vac(),nicrf_src)")
  ENDIF

  indim = 0
  CALL define_var("interchange_R","R",indim,idims, &
         "norm rho of the location of the interchange mode from TGLF model", &
         "-","ANOM","STATE_DATA", &
         "R|units=-      interchange_R")

  IF(ALLOCATED(ss%iota)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("iota","R",indim,idims, &
            "iota profile (1/q)", &
            "-","PLASMA","STATE_PROFILES", &
            "R|units=-|Spline_00  iota(nrho)")
  ENDIF

  IF(ALLOCATED(ss%isThermal)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",idims(indim))
     CALL define_var("isThermal","I",indim,idims, &
            "=1 if minority specie is thermalized", &
            "__NONE","IC","STATE_DATA", &
            "I  isThermal(nspec_rfmin)")
  ENDIF

  IF(ALLOCATED(ss%is_recycling)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("is_recycling","I",indim,idims, &
            "0/1; =1 to for recycling sources", &
            "__NONE","GAS","SHOT_CONFIGURATION", &
            "I  is_recycling(ngsc0)")
  ENDIF

  indim = 0
  CALL define_var("i_npoloidal","I",indim,idims, &
         "need description", &
         "__NONE","LH","SHOT_CONFIGURATION", &
         "I i_npoloidal")

  IF(ALLOCATED(ss%jdotb)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("jdotb","R",indim,idims, &
            "<J.B>", &
            "A*T/m^2","EQ","STATE_PROFILES", &
            "R|units=A*T/m^2|Hermite_00     jdotb(nrho_eq)")
  ENDIF

  indim = 0
  CALL define_var("kccw_Bphi","I",indim,idims, &
         "B_phi orientation: +1 means CCW viewed from above", &
         "__NONE","EQ","SHOT_CONFIGURATION", &
         "I  kccw_Bphi = 0")

  indim = 0
  CALL define_var("kccw_Jphi","I",indim,idims, &
         "J_phi orientation: +1 means CCW viewed from above", &
         "__NONE","EQ","SHOT_CONFIGURATION", &
         "I  kccw_Jphi = 0")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
  CALL define_var("kdens_rfmin","N",indim,idims, &
         '= "fraction" => nmini(i) = fracmin(i)*ne', &
         "__NONE","IC","SIMULATION_INIT", &
         'N  kdens_rfmin = "data"')

  IF(ALLOCATED(ss%kvolt_nbi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("kvolt_nbi","R",indim,idims, &
            "energy of each beam source **keV**", &
            "keV","NBI","STATE_DATA", &
            "R|units=keV kvolt_nbi(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%kyrhos_drbm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("kyrhos_drbm","R",indim,idims, &
            "ky*rhos at max growth rate for DRBM model", &
            "-","ANOM","STATE_PROFILES", &
            "R|units=-|pclin kyrhos_drbm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%kyrhos_etg)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("kyrhos_etg","R",indim,idims, &
            "ky*rhos at max growth rate for ETG model", &
            "-","ANOM","STATE_PROFILES", &
            "R|units=-|pclin kyrhos_etg(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%kyrhos_mtm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("kyrhos_mtm","R",indim,idims, &
            "ky*rhos at max growth rate for MTM model", &
            "-","ANOM","STATE_PROFILES", &
            "R|units=-|pclin kyrhos_mtm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%kyspectrum_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("kyspectrum_tglf","R",indim,idims, &
            "ky-spectrum from TGLF model", &
            "-","ANOM","STATE_PROFILES", &
            "R|units=-|pclin kyspectrum_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  IF(ALLOCATED(ss%Lbscap)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("Lbscap","R",indim,idims, &
            "distance, source to aperture", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m       Lbscap(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%Lbscap2)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("Lbscap2","R",indim,idims, &
            "distance, source to 2nd aperture", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m       Lbscap2(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%Lbsctan)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("Lbsctan","R",indim,idims, &
            "distance, source to tangency point", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m       Lbsctan(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%lhrf_src_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("lhrf_src_name","N",indim,idims, &
            "number & name of LHRF sources", &
            "__NONE","LH","MACHINE_DESCRIPTION", &
            "L|lhrf_source   lhrf_src_name(nlhrf_src)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("LH_Code_Info","C*80",indim,idims, &
         "Information: code implementing LH component", &
         "__NONE","LH","SIMULATION_INIT", &
         "C*80   LH_Code_Info")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("LH_Data_Info","C*80",indim,idims, &
         "Information on source of LHRF power data", &
         "__NONE","LH","SIMULATION_INIT", &
         "C*80   LH_Data_Info")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
  CALL define_var("LMHD_eq_code","N",indim,idims, &
         "name of MHD equilibrium refinement code used", &
         "__NONE","LMHD","SIMULATION_INIT", &
         "N  LMHD_eq_code")

  indim = 0
  CALL define_var("LMHD_status","I",indim,idims, &
         "0: OK; =1: at least one code error occurred.", &
         "__NONE","LMHD","STATE_DATA", &
         "I   LMHD_status")

  indim = 0
  CALL define_var("lock_machine_descr","I",indim,idims, &
         "Machine_Description section lock", &
         "__NONE","PLASMA","STATE_DATA", &
         "I  lock_machine_descr = 0")

  indim = 0
  CALL define_var("lock_shot_config","I",indim,idims, &
         "Shot_Configuration section lock", &
         "__NONE","PLASMA","STATE_DATA", &
         "I  lock_shot_config = 0")

  indim = 0
  CALL define_var("lock_sim_init","I",indim,idims, &
         "Simulation_Init section lock", &
         "__NONE","PLASMA","STATE_DATA", &
         "I  lock_sim_init = 0")

  IF(ALLOCATED(ss%Lpol)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("Lpol","R",indim,idims, &
            "poloidal path length", &
            "m","EQ","STATE_PROFILES", &
            "R|units=m|Spline Lpol(nrho_eq)")
  ENDIF

  IF(ALLOCATED(ss%Mercier_stable)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nballoon",idims(indim))
     CALL define_var("Mercier_stable","I",indim,idims, &
            "0: stable everywhere; =1: unstable somewhere", &
            "__NONE","LMHD","STATE_DATA", &
            "I   Mercier_stable(nballoon)")
  ENDIF

  indim = 0
  CALL define_var("MHD_eq_status","I",indim,idims, &
         "0: OK; =1: MHD equilibrium refinement failed", &
         "__NONE","LMHD","STATE_DATA", &
         "I   MHD_eq_status")

  IF(ALLOCATED(ss%m_ALL)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_all",idims(indim))
     CALL define_var("m_ALL","R",indim,idims, &
            "ALL specie mass", &
            "kg","PLASMA","SIMULATION_INIT", &
            "S|specie  ALL(0:nspec_all)")
  ENDIF

  IF(ALLOCATED(ss%m_ALLA)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_alla",idims(indim))
     CALL define_var("m_ALLA","R",indim,idims, &
            "ALLA specie mass", &
            "kg","PLASMA","SIMULATION_INIT", &
            "S|specie  ALLA(0:nspec_alla)")
  ENDIF

  IF(ALLOCATED(ss%m_RFMIN)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",idims(indim))
     CALL define_var("m_RFMIN","R",indim,idims, &
            "RFMIN specie mass", &
            "kg","IC","SHOT_CONFIGURATION", &
            "S|RF_minority  RFMIN(nspec_rfmin)")
  ENDIF

  IF(ALLOCATED(ss%m_S)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("m_S","R",indim,idims, &
            "S specie mass", &
            "kg","PLASMA","SHOT_CONFIGURATION", &
            "S|thermal_specie  S(0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%m_SA)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_tha",idims(indim))
     CALL define_var("m_SA","R",indim,idims, &
            "SA specie mass", &
            "kg","PLASMA","SIMULATION_INIT", &
            "S|thermal_specie  SA(0:nspec_tha)")
  ENDIF

  IF(ALLOCATED(ss%m_SFUS)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("m_SFUS","R",indim,idims, &
            "SFUS specie mass", &
            "kg","FUS","SHOT_CONFIGURATION", &
            "S|fusion_ion   SFUS(nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%m_SGAS)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("m_SGAS","R",indim,idims, &
            "SGAS specie mass", &
            "kg","GAS","SIMULATION_INIT", &
            "S|neutral_gas  SGAS(nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%m_SIMP0)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_imp0",idims(indim))
     CALL define_var("m_SIMP0","R",indim,idims, &
            "SIMP0 specie mass", &
            "kg","GAS","SIMULATION_INIT", &
            "S|impurity_atoms SIMP0(nspec_imp0)")
  ENDIF

  IF(ALLOCATED(ss%m_SIMPI)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_impi",idims(indim))
     CALL define_var("m_SIMPI","R",indim,idims, &
            "SIMPI specie mass", &
            "kg","PLASMA","SIMULATION_INIT", &
            "S|impurity_atoms SIMPI(nspec_impi)")
  ENDIF

  IF(ALLOCATED(ss%m_SNBI)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("m_SNBI","R",indim,idims, &
            "SNBI specie mass", &
            "kg","NBI","SIMULATION_INIT", &
            "S|beam_ion     SNBI(nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%n0norm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("n0norm","R",indim,idims, &
            "neutral density per unit gas influx (sc0)", &
            "(m^-3)/(#/sec)","GAS","STATE_PROFILES", &
            "R|step|units=(m^-3)/(#/sec)    n0norm(~nrho_gas,nspec_gas,ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%n0_halo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("n0_halo","R",indim,idims, &
            "halo thermal neutral density", &
            "m^-3","NBI","STATE_PROFILES", &
            "R|units=m^-3|step         n0_halo(~nrho_nbi,nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%n0_reco)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("n0_reco","R",indim,idims, &
            "reco thermal neutral density", &
            "m^-3","GAS","STATE_PROFILES", &
            "R|units=m^-3|step         n0_reco(~nrho_gas,nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%nbap2_shape)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("nbap2_shape","N",indim,idims, &
            "keyword for optional 2nd aperture", &
            "__NONE","NBI","MACHINE_DESCRIPTION", &
            "N     nbap2_shape(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%nbap_shape)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("nbap_shape","N",indim,idims, &
            "shape keyword for beam aperture", &
            "__NONE","NBI","MACHINE_DESCRIPTION", &
            "N     nbap_shape(nbeam)")
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

END SUBROUTINE plasma_state_store_def_3


SUBROUTINE plasma_state_store_3(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%grho2)) THEN
     status = nf90_inq_varid(icdf,"grho2",varid)
     status = nf90_put_var(icdf,varid,ss%grho2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: grho2"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%grho2b2i)) THEN
     status = nf90_inq_varid(icdf,"grho2b2i",varid)
     status = nf90_put_var(icdf,varid,ss%grho2b2i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: grho2b2i"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%grho2r2i)) THEN
     status = nf90_inq_varid(icdf,"grho2r2i",varid)
     status = nf90_put_var(icdf,varid,ss%grho2r2i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: grho2r2i"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%grho2r3i)) THEN
     status = nf90_inq_varid(icdf,"grho2r3i",varid)
     status = nf90_put_var(icdf,varid,ss%grho2r3i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: grho2r3i"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gri)) THEN
     status = nf90_inq_varid(icdf,"gri",varid)
     status = nf90_put_var(icdf,varid,ss%gri)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gri"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%grirhoi)) THEN
     status = nf90_inq_varid(icdf,"grirhoi",varid)
     status = nf90_put_var(icdf,varid,ss%grirhoi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: grirhoi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_drbm)) THEN
     status = nf90_inq_varid(icdf,"growthrate_drbm",varid)
     status = nf90_put_var(icdf,varid,ss%growthrate_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: growthrate_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_etg)) THEN
     status = nf90_inq_varid(icdf,"growthrate_etg",varid)
     status = nf90_put_var(icdf,varid,ss%growthrate_etg)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: growthrate_etg"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_glf)) THEN
     status = nf90_inq_varid(icdf,"growthrate_glf",varid)
     status = nf90_put_var(icdf,varid,ss%growthrate_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: growthrate_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_max_tglf)) THEN
     status = nf90_inq_varid(icdf,"growthrate_max_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%growthrate_max_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: growthrate_max_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_mmm71)) THEN
     status = nf90_inq_varid(icdf,"growthrate_mmm71",varid)
     status = nf90_put_var(icdf,varid,ss%growthrate_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: growthrate_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_mtm)) THEN
     status = nf90_inq_varid(icdf,"growthrate_mtm",varid)
     status = nf90_put_var(icdf,varid,ss%growthrate_mtm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: growthrate_mtm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_tglf)) THEN
     status = nf90_inq_varid(icdf,"growthrate_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%growthrate_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: growthrate_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_vpar)) THEN
     status = nf90_inq_varid(icdf,"growthrate_vpar",varid)
     status = nf90_put_var(icdf,varid,ss%growthrate_vpar)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: growthrate_vpar"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gs_name)) THEN
     status = nf90_inq_varid(icdf,"gs_name",varid)
     status = nf90_put_var(icdf,varid,ss%gs_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gs_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%g_eq)) THEN
     status = nf90_inq_varid(icdf,"g_eq",varid)
     status = nf90_put_var(icdf,varid,ss%g_eq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: g_eq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%hegrilh)) THEN
     status = nf90_inq_varid(icdf,"hegrilh",varid)
     status = nf90_put_var(icdf,varid,ss%hegrilh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: hegrilh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Hsize_coil)) THEN
     status = nf90_inq_varid(icdf,"Hsize_coil",varid)
     status = nf90_put_var(icdf,varid,ss%Hsize_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Hsize_coil"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%H_einj_standard)) THEN
     status = nf90_inq_varid(icdf,"H_einj_standard",varid)
     status = nf90_put_var(icdf,varid,ss%H_einj_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: H_einj_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%H_ffull_standard)) THEN
     status = nf90_inq_varid(icdf,"H_ffull_standard",varid)
     status = nf90_put_var(icdf,varid,ss%H_ffull_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: H_ffull_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%H_fhalf_standard)) THEN
     status = nf90_inq_varid(icdf,"H_fhalf_standard",varid)
     status = nf90_put_var(icdf,varid,ss%H_fhalf_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: H_fhalf_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%icircuit_coil)) THEN
     status = nf90_inq_varid(icdf,"icircuit_coil",varid)
     status = nf90_put_var(icdf,varid,ss%icircuit_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: icircuit_coil"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%icrf_src_name)) THEN
     status = nf90_inq_varid(icdf,"icrf_src_name",varid)
     status = nf90_put_var(icdf,varid,ss%icrf_src_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: icrf_src_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"IC_Code_Info",varid)
  status = nf90_put_var(icdf,varid,ss%IC_Code_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: IC_Code_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"IC_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%IC_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: IC_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%Imag_ant_coef)) THEN
     status = nf90_inq_varid(icdf,"Imag_ant_coef",varid)
     status = nf90_put_var(icdf,varid,ss%Imag_ant_coef)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Imag_ant_coef"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"interchange_R",varid)
  status = nf90_put_var(icdf,varid,ss%interchange_R)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: interchange_R"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%iota)) THEN
     status = nf90_inq_varid(icdf,"iota",varid)
     status = nf90_put_var(icdf,varid,ss%iota)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: iota"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%isThermal)) THEN
     status = nf90_inq_varid(icdf,"isThermal",varid)
     status = nf90_put_var(icdf,varid,ss%isThermal)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: isThermal"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%is_recycling)) THEN
     status = nf90_inq_varid(icdf,"is_recycling",varid)
     status = nf90_put_var(icdf,varid,ss%is_recycling)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: is_recycling"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"i_npoloidal",varid)
  status = nf90_put_var(icdf,varid,ss%i_npoloidal)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: i_npoloidal"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%jdotb)) THEN
     status = nf90_inq_varid(icdf,"jdotb",varid)
     status = nf90_put_var(icdf,varid,ss%jdotb)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: jdotb"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"kccw_Bphi",varid)
  status = nf90_put_var(icdf,varid,ss%kccw_Bphi)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: kccw_Bphi"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"kccw_Jphi",varid)
  status = nf90_put_var(icdf,varid,ss%kccw_Jphi)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: kccw_Jphi"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"kdens_rfmin",varid)
  status = nf90_put_var(icdf,varid,ss%kdens_rfmin)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: kdens_rfmin"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%kvolt_nbi)) THEN
     status = nf90_inq_varid(icdf,"kvolt_nbi",varid)
     status = nf90_put_var(icdf,varid,ss%kvolt_nbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: kvolt_nbi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%kyrhos_drbm)) THEN
     status = nf90_inq_varid(icdf,"kyrhos_drbm",varid)
     status = nf90_put_var(icdf,varid,ss%kyrhos_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: kyrhos_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%kyrhos_etg)) THEN
     status = nf90_inq_varid(icdf,"kyrhos_etg",varid)
     status = nf90_put_var(icdf,varid,ss%kyrhos_etg)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: kyrhos_etg"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%kyrhos_mtm)) THEN
     status = nf90_inq_varid(icdf,"kyrhos_mtm",varid)
     status = nf90_put_var(icdf,varid,ss%kyrhos_mtm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: kyrhos_mtm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%kyspectrum_tglf)) THEN
     status = nf90_inq_varid(icdf,"kyspectrum_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%kyspectrum_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: kyspectrum_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Lbscap)) THEN
     status = nf90_inq_varid(icdf,"Lbscap",varid)
     status = nf90_put_var(icdf,varid,ss%Lbscap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Lbscap"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Lbscap2)) THEN
     status = nf90_inq_varid(icdf,"Lbscap2",varid)
     status = nf90_put_var(icdf,varid,ss%Lbscap2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Lbscap2"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Lbsctan)) THEN
     status = nf90_inq_varid(icdf,"Lbsctan",varid)
     status = nf90_put_var(icdf,varid,ss%Lbsctan)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Lbsctan"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%lhrf_src_name)) THEN
     status = nf90_inq_varid(icdf,"lhrf_src_name",varid)
     status = nf90_put_var(icdf,varid,ss%lhrf_src_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: lhrf_src_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"LH_Code_Info",varid)
  status = nf90_put_var(icdf,varid,ss%LH_Code_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: LH_Code_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"LH_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%LH_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: LH_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"LMHD_eq_code",varid)
  status = nf90_put_var(icdf,varid,ss%LMHD_eq_code)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: LMHD_eq_code"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"LMHD_status",varid)
  status = nf90_put_var(icdf,varid,ss%LMHD_status)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: LMHD_status"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"lock_machine_descr",varid)
  status = nf90_put_var(icdf,varid,ss%lock_machine_descr)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: lock_machine_descr"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"lock_shot_config",varid)
  status = nf90_put_var(icdf,varid,ss%lock_shot_config)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: lock_shot_config"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"lock_sim_init",varid)
  status = nf90_put_var(icdf,varid,ss%lock_sim_init)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: lock_sim_init"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%Lpol)) THEN
     status = nf90_inq_varid(icdf,"Lpol",varid)
     status = nf90_put_var(icdf,varid,ss%Lpol)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Lpol"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Mercier_stable)) THEN
     status = nf90_inq_varid(icdf,"Mercier_stable",varid)
     status = nf90_put_var(icdf,varid,ss%Mercier_stable)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Mercier_stable"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"MHD_eq_status",varid)
  status = nf90_put_var(icdf,varid,ss%MHD_eq_status)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: MHD_eq_status"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%m_ALL)) THEN
     status = nf90_inq_varid(icdf,"m_ALL",varid)
     status = nf90_put_var(icdf,varid,ss%m_ALL)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: m_ALL"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_ALLA)) THEN
     status = nf90_inq_varid(icdf,"m_ALLA",varid)
     status = nf90_put_var(icdf,varid,ss%m_ALLA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: m_ALLA"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_RFMIN)) THEN
     status = nf90_inq_varid(icdf,"m_RFMIN",varid)
     status = nf90_put_var(icdf,varid,ss%m_RFMIN)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: m_RFMIN"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_S)) THEN
     status = nf90_inq_varid(icdf,"m_S",varid)
     status = nf90_put_var(icdf,varid,ss%m_S)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: m_S"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_SA)) THEN
     status = nf90_inq_varid(icdf,"m_SA",varid)
     status = nf90_put_var(icdf,varid,ss%m_SA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: m_SA"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_SFUS)) THEN
     status = nf90_inq_varid(icdf,"m_SFUS",varid)
     status = nf90_put_var(icdf,varid,ss%m_SFUS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: m_SFUS"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_SGAS)) THEN
     status = nf90_inq_varid(icdf,"m_SGAS",varid)
     status = nf90_put_var(icdf,varid,ss%m_SGAS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: m_SGAS"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_SIMP0)) THEN
     status = nf90_inq_varid(icdf,"m_SIMP0",varid)
     status = nf90_put_var(icdf,varid,ss%m_SIMP0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: m_SIMP0"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_SIMPI)) THEN
     status = nf90_inq_varid(icdf,"m_SIMPI",varid)
     status = nf90_put_var(icdf,varid,ss%m_SIMPI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: m_SIMPI"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_SNBI)) THEN
     status = nf90_inq_varid(icdf,"m_SNBI",varid)
     status = nf90_put_var(icdf,varid,ss%m_SNBI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: m_SNBI"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%n0norm)) THEN
     status = nf90_inq_varid(icdf,"n0norm",varid)
     status = nf90_put_var(icdf,varid,ss%n0norm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: n0norm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%n0_halo)) THEN
     status = nf90_inq_varid(icdf,"n0_halo",varid)
     status = nf90_put_var(icdf,varid,ss%n0_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: n0_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%n0_reco)) THEN
     status = nf90_inq_varid(icdf,"n0_reco",varid)
     status = nf90_put_var(icdf,varid,ss%n0_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: n0_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nbap2_shape)) THEN
     status = nf90_inq_varid(icdf,"nbap2_shape",varid)
     status = nf90_put_var(icdf,varid,ss%nbap2_shape)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nbap2_shape"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nbap_shape)) THEN
     status = nf90_inq_varid(icdf,"nbap_shape",varid)
     status = nf90_put_var(icdf,varid,ss%nbap_shape)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nbap_shape"
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

END SUBROUTINE plasma_state_store_3
