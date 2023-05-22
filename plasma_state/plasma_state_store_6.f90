! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_store_def_6(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%prad_li)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_rad",idims(indim))
     CALL define_var("prad_li","R",indim,idims, &
            "line radiation contribution", &
            "W","RAD","STATE_PROFILES", &
            "R|step*dV  prad_li(~nrho_rad)")
  ENDIF

  IF(ALLOCATED(ss%psc_halo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("psc_halo","R",indim,idims, &
            "beam halo power: recapture - pb0_halo", &
            "W","NBI","STATE_PROFILES", &
            "R|step*dV   psc_halo(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%psc_reco)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     CALL define_var("psc_reco","R",indim,idims, &
            "reco power: recapture - p0_reco", &
            "W","GAS","STATE_PROFILES", &
            "R|step*dV   psc_reco(~nrho_gas)")
  ENDIF

  IF(ALLOCATED(ss%psipol)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("psipol","R",indim,idims, &
            "poloidal flux vs. rho", &
            "Wb/rad","EQ","STATE_PROFILES", &
            "R|units=Wb/rad|Spline_00   psipol(nrho_eq)")
  ENDIF

  IF(ALLOCATED(ss%PsiRZ)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nr",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nz",idims(indim))
     CALL define_var("PsiRZ","R",indim,idims, &
            "Psi(R,Z)", &
            "Wb/rad","EQ","STATE_PROFILES", &
            "R|units=Wb/rad|Spline  PsiRZ(nR,nZ)")
  ENDIF

  indim = 0
  CALL define_var("Psi_to_machine_axis","R",indim,idims, &
         "delta(Psi) from magnetic axis to machine axis", &
         "Wb/rad","EQ","STATE_DATA", &
         "R|units=Wb/rad   Psi_to_machine_axis")

  IF(ALLOCATED(ss%psmom_errck)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("psmom_errck","R",indim,idims, &
            "PS moments sum convergence", &
            "-","EQ","STATE_PROFILES", &
            "R|units=-|pclin       psmom_errck(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%psmom_nc)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_npsmom",idims(indim))
     CALL define_var("psmom_nc","R",indim,idims, &
            "PS moments", &
            "m^-2","EQ","STATE_PROFILES", &
            "R|units=m^-2|pclin    psmom_nc(nrho_eq_geo,npsmom)")
  ENDIF

  IF(ALLOCATED(ss%psmom_num)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_npsmom",idims(indim))
     CALL define_var("psmom_num","N",indim,idims, &
            "Pfirsch-Schlutter moments", &
            "__NONE","EQ","SIMULATION_INIT", &
            "L|PS_moments  psmom_num(npsmom)")
  ENDIF

  IF(ALLOCATED(ss%P_eq)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("P_eq","R",indim,idims, &
            "equilibrium scalar pressure", &
            "Pa","EQ","STATE_PROFILES", &
            "R|units=Pa|Spline_00       P_eq(nrho_eq)")
  ENDIF

  IF(ALLOCATED(ss%qatom_ALL)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_all",idims(indim))
     CALL define_var("qatom_ALL","R",indim,idims, &
            "ALL atomic number", &
            "C","PLASMA","SIMULATION_INIT", &
            "S|specie  ALL(0:nspec_all)")
  ENDIF

  IF(ALLOCATED(ss%qatom_ALLA)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_alla",idims(indim))
     CALL define_var("qatom_ALLA","R",indim,idims, &
            "ALLA atomic number", &
            "C","PLASMA","SIMULATION_INIT", &
            "S|specie  ALLA(0:nspec_alla)")
  ENDIF

  IF(ALLOCATED(ss%qatom_RFMIN)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",idims(indim))
     CALL define_var("qatom_RFMIN","R",indim,idims, &
            "RFMIN atomic number", &
            "C","IC","SHOT_CONFIGURATION", &
            "S|RF_minority  RFMIN(nspec_rfmin)")
  ENDIF

  IF(ALLOCATED(ss%qatom_S)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("qatom_S","R",indim,idims, &
            "S atomic number", &
            "C","PLASMA","SHOT_CONFIGURATION", &
            "S|thermal_specie  S(0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%qatom_SA)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_tha",idims(indim))
     CALL define_var("qatom_SA","R",indim,idims, &
            "SA atomic number", &
            "C","PLASMA","SIMULATION_INIT", &
            "S|thermal_specie  SA(0:nspec_tha)")
  ENDIF

  IF(ALLOCATED(ss%qatom_SFUS)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("qatom_SFUS","R",indim,idims, &
            "SFUS atomic number", &
            "C","FUS","SHOT_CONFIGURATION", &
            "S|fusion_ion   SFUS(nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%qatom_SGAS)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("qatom_SGAS","R",indim,idims, &
            "SGAS atomic number", &
            "C","GAS","SIMULATION_INIT", &
            "S|neutral_gas  SGAS(nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%qatom_SIMP0)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_imp0",idims(indim))
     CALL define_var("qatom_SIMP0","R",indim,idims, &
            "SIMP0 atomic number", &
            "C","GAS","SIMULATION_INIT", &
            "S|impurity_atoms SIMP0(nspec_imp0)")
  ENDIF

  IF(ALLOCATED(ss%qatom_SIMPI)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_impi",idims(indim))
     CALL define_var("qatom_SIMPI","R",indim,idims, &
            "SIMPI atomic number", &
            "C","PLASMA","SIMULATION_INIT", &
            "S|impurity_atoms SIMPI(nspec_impi)")
  ENDIF

  IF(ALLOCATED(ss%qatom_SNBI)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("qatom_SNBI","R",indim,idims, &
            "SNBI atomic number", &
            "C","NBI","SIMULATION_INIT", &
            "S|beam_ion     SNBI(nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%qbeami)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("qbeami","R",indim,idims, &
            "average beam ion charge", &
            "C","NBI","STATE_PROFILES", &
            "R|units=C|step*nbeami qbeami(~nrho_nbi,nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%qcomp_e)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("qcomp_e","R",indim,idims, &
            "Compressional electron heating", &
            "W","PLASMA","STATE_PROFILES", &
            "R|step*dV   qcomp_e(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%qcomp_i)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("qcomp_i","R",indim,idims, &
            "Compressional ion heating", &
            "W","PLASMA","STATE_PROFILES", &
            "R|step*dV   qcomp_i(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%qie)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("qie","R",indim,idims, &
            "Ion->Electron heat coupling", &
            "W","PLASMA","STATE_PROFILES", &
            "R|units=W|step*dV   qie(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%qioniz)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("qioniz","R",indim,idims, &
            "ionization power per unit influx (sc0)", &
            "W/(#/sec)","GAS","STATE_PROFILES", &
            "R|step*dV|units=W/(#/sec)     qioniz(~nrho_gas,ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%ql_operator)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_256",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_alla",idims(indim))
     CALL define_var("ql_operator","F",indim,idims, &
            "quasilinear operator filenames", &
            "__NONE","IC","STATE_DATA", &
            "F  ql_operator(0:nspec_alla)")
  ENDIF

  IF(ALLOCATED(ss%qqcx)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("qqcx","R",indim,idims, &
            "cx power per unit influx per (T0-Ti)", &
            "W/(#/sec)/keV","GAS","STATE_PROFILES", &
            "R|step*dV|units=W/(#/sec)/keV  qqcx(~nrho_gas,ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%qrot_conv)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("qrot_conv","R",indim,idims, &
            "rotational energy dissipation (convective)", &
            "W","PLASMA","STATE_PROFILES", &
            "R|step*dV   qrot_conv(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%qrot_diff)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("qrot_diff","R",indim,idims, &
            "rotational energy dissipation (diffusive)", &
            "W","PLASMA","STATE_PROFILES", &
            "R|step*dV   qrot_diff(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%q_ALL)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_all",idims(indim))
     CALL define_var("q_ALL","R",indim,idims, &
            "ALL specie charge", &
            "C","PLASMA","SIMULATION_INIT", &
            "S|specie  ALL(0:nspec_all)")
  ENDIF

  IF(ALLOCATED(ss%q_ALLA)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_alla",idims(indim))
     CALL define_var("q_ALLA","R",indim,idims, &
            "ALLA specie charge", &
            "C","PLASMA","SIMULATION_INIT", &
            "S|specie  ALLA(0:nspec_alla)")
  ENDIF

  IF(ALLOCATED(ss%q_eq)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("q_eq","R",indim,idims, &
            "equilibrium q profile", &
            "-","EQ","STATE_PROFILES", &
            "R|units=-|Spline           q_eq(nrho_eq)")
  ENDIF

  IF(ALLOCATED(ss%q_RFMIN)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",idims(indim))
     CALL define_var("q_RFMIN","R",indim,idims, &
            "RFMIN specie charge", &
            "C","IC","SHOT_CONFIGURATION", &
            "S|RF_minority  RFMIN(nspec_rfmin)")
  ENDIF

  IF(ALLOCATED(ss%q_S)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("q_S","R",indim,idims, &
            "S specie charge", &
            "C","PLASMA","SHOT_CONFIGURATION", &
            "S|thermal_specie  S(0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%q_SA)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_tha",idims(indim))
     CALL define_var("q_SA","R",indim,idims, &
            "SA specie charge", &
            "C","PLASMA","SIMULATION_INIT", &
            "S|thermal_specie  SA(0:nspec_tha)")
  ENDIF

  IF(ALLOCATED(ss%q_SFUS)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("q_SFUS","R",indim,idims, &
            "SFUS specie charge", &
            "C","FUS","SHOT_CONFIGURATION", &
            "S|fusion_ion   SFUS(nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%q_SGAS)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("q_SGAS","R",indim,idims, &
            "SGAS specie charge", &
            "C","GAS","SIMULATION_INIT", &
            "S|neutral_gas  SGAS(nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%q_SIMP0)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_imp0",idims(indim))
     CALL define_var("q_SIMP0","R",indim,idims, &
            "SIMP0 specie charge", &
            "C","GAS","SIMULATION_INIT", &
            "S|impurity_atoms SIMP0(nspec_imp0)")
  ENDIF

  IF(ALLOCATED(ss%q_SIMPI)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_impi",idims(indim))
     CALL define_var("q_SIMPI","R",indim,idims, &
            "SIMPI specie charge", &
            "C","PLASMA","SIMULATION_INIT", &
            "S|impurity_atoms SIMPI(nspec_impi)")
  ENDIF

  IF(ALLOCATED(ss%q_SNBI)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("q_SNBI","R",indim,idims, &
            "SNBI specie charge", &
            "C","NBI","SIMULATION_INIT", &
            "S|beam_ion     SNBI(nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%R0_momeq)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("R0_momeq","R",indim,idims, &
            "R0 of flux surface", &
            "m","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit  R0_momeq(nrho_eq)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("RAD_Code_Info","C*80",indim,idims, &
         "Information: code implementing RAD component", &
         "__NONE","RAD","SIMULATION_INIT", &
         "C*80   RAD_Code_Info")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("RAD_Data_Info","C*80",indim,idims, &
         "Information on source of radiated power data", &
         "__NONE","RAD","SIMULATION_INIT", &
         "C*80   RAD_Data_Info")

  IF(ALLOCATED(ss%rate_sinb0i)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("rate_sinb0i","R",indim,idims, &
            "sink rate due to ionization by beam ions", &
            "1/sec","NBI","STATE_PROFILES", &
            "R|step|units=1/sec       rate_sinb0i(~nrho_nbi,nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%rate_sinb0x)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("rate_sinb0x","R",indim,idims, &
            "sink rate due to CX with beam ions (total)", &
            "1/sec","NBI","STATE_PROFILES", &
            "R|step|units=1/sec       rate_sinb0x(~nrho_nbi,nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%rate_sinb0xs)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("rate_sinb0xs","R",indim,idims, &
            "sink rate due to CX with beam ions", &
            "1/sec","NBI","STATE_PROFILES", &
            "R|step|units=1/sec       rate_sinb0xs(~nrho_nbi,nspec_gas,nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%rate_sinf0i)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_fus",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("rate_sinf0i","R",indim,idims, &
            "sink rate due to ionization by fusion ions", &
            "1/sec","FUS","STATE_PROFILES", &
            "R|step|units=1/sec       rate_sinf0i(~nrho_fus,nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%rate_sinf0x)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_fus",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("rate_sinf0x","R",indim,idims, &
            "sink rate due to CX with fusion ions (total)", &
            "1/sec","FUS","STATE_PROFILES", &
            "R|step|units=1/sec       rate_sinf0x(~nrho_fus,nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%rate_sinf0xs)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_fus",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("rate_sinf0xs","R",indim,idims, &
            "sink rate due to CX with fusion ions", &
            "1/sec","FUS","STATE_PROFILES", &
            "R|step|units=1/sec       rate_sinf0xs(~nrho_fus,nspec_gas,nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%Real_ant_coef)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_num_nphi_vac",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("Real_ant_coef","R",indim,idims, &
            "real part of Fourier Coef", &
            "Amps/m","IC","SHOT_CONFIGURATION", &
            "R|units=Amps/m Real_ant_coef(num_nphi_vac(),nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%res_sn)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("res_sn","R",indim,idims, &
            "residuals for particle transport eqn", &
            "Norm","PLASMA","STATE_PROFILES", &
            "R|units=Norm|step*dV res_sn(~nrho,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%res_te)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("res_te","R",indim,idims, &
            "residuals for electron transport eqn", &
            "Norm","PLASMA","STATE_PROFILES", &
            "R|step*dV res_te(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%res_ti)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("res_ti","R",indim,idims, &
            "residuals for ion transport eqn", &
            "Norm","PLASMA","STATE_PROFILES", &
            "R|step*dV res_ti(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%res_tq)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("res_tq","R",indim,idims, &
            "rediduals for angular momentum transport eqn", &
            "Norm","PLASMA","STATE_PROFILES", &
            "R|units=Norm|step*dV res_tq(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%RFMIN_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",idims(indim))
     CALL define_var("RFMIN_name","N",indim,idims, &
            "ICRF minority species", &
            "__NONE","IC","SHOT_CONFIGURATION", &
            "S|RF_minority  RFMIN(nspec_rfmin)")
  ENDIF

  IF(ALLOCATED(ss%rfmin_to_all)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",idims(indim))
     CALL define_var("rfmin_to_all","I",indim,idims, &
            'map rfmin index to "all" list', &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "I  rfmin_to_all(nspec_rfmin)")
  ENDIF

  IF(ALLOCATED(ss%rfmin_to_alla)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",idims(indim))
     CALL define_var("rfmin_to_alla","I",indim,idims, &
            'map rfmin index to "alla" list', &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "I  rfmin_to_alla(nspec_rfmin)")
  ENDIF

  IF(ALLOCATED(ss%RFMIN_type)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",idims(indim))
     CALL define_var("RFMIN_type","I",indim,idims, &
            "RFMIN specie types", &
            "__NONE","IC","SHOT_CONFIGURATION", &
            "S|RF_minority  RFMIN(nspec_rfmin)")
  ENDIF

  IF(ALLOCATED(ss%rho)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("rho","R",indim,idims, &
            "rho grid (PLASMA)", &
            "-","PLASMA","SIMULATION_INIT", &
            "G   rho(nrho)")
  ENDIF

  IF(ALLOCATED(ss%rhopsi0lh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("rhopsi0lh","R",indim,idims, &
            "need description", &
            "-","LH","SHOT_CONFIGURATION", &
            "R|units=- rhopsi0lh(nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%rho_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom",idims(indim))
     CALL define_var("rho_anom","R",indim,idims, &
            "rho grid (1d anomalous transport profiles)", &
            "-","ANOM","SIMULATION_INIT", &
            "G  rho_anom(nrho_anom)")
  ENDIF

  IF(ALLOCATED(ss%rho_anom2)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom2",idims(indim))
     CALL define_var("rho_anom2","R",indim,idims, &
            "rho grid (2d anomalous transport profiles)", &
            "-","ANOM","SIMULATION_INIT", &
            "G  rho_anom2(nrho_anom2)")
  ENDIF

  IF(ALLOCATED(ss%rho_anomth)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("rho_anomth","R",indim,idims, &
            "rho grid (1d anomalous data for 1d", &
            "-","ANOM","SIMULATION_INIT", &
            "G  rho_anomth(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%rho_bdy_ns)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("rho_bdy_ns","R",indim,idims, &
            "locations beyond which densities are known", &
            "-","PLASMA","STATE_DATA", &
            "R|units=- rho_bdy_ns(0:nspec_th)")
  ENDIF

  indim = 0
  CALL define_var("rho_bdy_omegat","R",indim,idims, &
         "location beyond which angular velocity is known", &
         "-","PLASMA","STATE_DATA", &
         "R rho_bdy_omegat")

  indim = 0
  CALL define_var("rho_bdy_Te","R",indim,idims, &
         "location beyond which Te is known", &
         "-","PLASMA","STATE_DATA", &
         "R rho_bdy_Te")

  indim = 0
  CALL define_var("rho_bdy_Ti","R",indim,idims, &
         "location beyond which Ti is known", &
         "-","PLASMA","STATE_DATA", &
         "R rho_bdy_Ti")

  IF(ALLOCATED(ss%rho_ecrf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_ecrf",idims(indim))
     CALL define_var("rho_ecrf","R",indim,idims, &
            "rho grid -- ECRF", &
            "-","EC","SIMULATION_INIT", &
            "G  rho_ecrf(nrho_ecrf)")
  ENDIF

  IF(ALLOCATED(ss%rho_eq)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("rho_eq","R",indim,idims, &
            "rho grid (EQ)", &
            "-","EQ","SIMULATION_INIT", &
            "G   rho_eq(nrho_eq)")
  ENDIF

  IF(ALLOCATED(ss%rho_eq_geo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("rho_eq_geo","R",indim,idims, &
            "rho grid (EQ flux surface averages)", &
            "-","EQ","SIMULATION_INIT", &
            "G   rho_eq_geo(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%rho_fus)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_fus",idims(indim))
     CALL define_var("rho_fus","R",indim,idims, &
            "rho grid -- Fusion products model", &
            "-","FUS","SIMULATION_INIT", &
            "G  rho_fus(nrho_fus)")
  ENDIF

  IF(ALLOCATED(ss%rho_gas)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_gas",idims(indim))
     CALL define_var("rho_gas","R",indim,idims, &
            "rho grid (GAS -- neutral gas model)", &
            "-","GAS","SIMULATION_INIT", &
            "G  rho_gas(nrho_gas)")
  ENDIF

  IF(ALLOCATED(ss%rho_icrf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_icrf",idims(indim))
     CALL define_var("rho_icrf","R",indim,idims, &
            "rho grid (RF)", &
            "-","IC","SIMULATION_INIT", &
            "G  rho_icrf(nrho_icrf)")
  ENDIF

  IF(ALLOCATED(ss%rho_lhrf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_lhrf",idims(indim))
     CALL define_var("rho_lhrf","R",indim,idims, &
            "rho grid -- LHRF", &
            "-","LH","SIMULATION_INIT", &
            "G  rho_lhrf(nrho_lhrf)")
  ENDIF

  IF(ALLOCATED(ss%rho_lmhd)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_lmhd",idims(indim))
     CALL define_var("rho_lmhd","R",indim,idims, &
            "rho grid (LMHD component)", &
            "-","LMHD","SIMULATION_INIT", &
            "G  rho_lmhd(nrho_lmhd)")
  ENDIF

  IF(ALLOCATED(ss%rho_mist)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_mist",idims(indim))
     CALL define_var("rho_mist","R",indim,idims, &
            "rho grid (PLASMA) impurity-driven sources", &
            "-","RAD","SIMULATION_INIT", &
            "G  rho_mist(nrho_mist)")
  ENDIF

  IF(ALLOCATED(ss%rho_nbi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_nbi",idims(indim))
     CALL define_var("rho_nbi","R",indim,idims, &
            "rho grid -- NBI", &
            "-","NBI","SIMULATION_INIT", &
            "G  rho_nbi(nrho_nbi)")
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

END SUBROUTINE plasma_state_store_def_6


SUBROUTINE plasma_state_store_6(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%prad_li)) THEN
     status = nf90_inq_varid(icdf,"prad_li",varid)
     status = nf90_put_var(icdf,varid,ss%prad_li)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: prad_li"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%psc_halo)) THEN
     status = nf90_inq_varid(icdf,"psc_halo",varid)
     status = nf90_put_var(icdf,varid,ss%psc_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: psc_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%psc_reco)) THEN
     status = nf90_inq_varid(icdf,"psc_reco",varid)
     status = nf90_put_var(icdf,varid,ss%psc_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: psc_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%psipol)) THEN
     status = nf90_inq_varid(icdf,"psipol",varid)
     status = nf90_put_var(icdf,varid,ss%psipol)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: psipol"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%PsiRZ)) THEN
     status = nf90_inq_varid(icdf,"PsiRZ",varid)
     status = nf90_put_var(icdf,varid,ss%PsiRZ)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: PsiRZ"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Psi_to_machine_axis",varid)
  status = nf90_put_var(icdf,varid,ss%Psi_to_machine_axis)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: Psi_to_machine_axis"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%psmom_errck)) THEN
     status = nf90_inq_varid(icdf,"psmom_errck",varid)
     status = nf90_put_var(icdf,varid,ss%psmom_errck)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: psmom_errck"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%psmom_nc)) THEN
     status = nf90_inq_varid(icdf,"psmom_nc",varid)
     status = nf90_put_var(icdf,varid,ss%psmom_nc)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: psmom_nc"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%psmom_num)) THEN
     status = nf90_inq_varid(icdf,"psmom_num",varid)
     status = nf90_put_var(icdf,varid,ss%psmom_num)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: psmom_num"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%P_eq)) THEN
     status = nf90_inq_varid(icdf,"P_eq",varid)
     status = nf90_put_var(icdf,varid,ss%P_eq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: P_eq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_ALL)) THEN
     status = nf90_inq_varid(icdf,"qatom_ALL",varid)
     status = nf90_put_var(icdf,varid,ss%qatom_ALL)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qatom_ALL"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_ALLA)) THEN
     status = nf90_inq_varid(icdf,"qatom_ALLA",varid)
     status = nf90_put_var(icdf,varid,ss%qatom_ALLA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qatom_ALLA"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_RFMIN)) THEN
     status = nf90_inq_varid(icdf,"qatom_RFMIN",varid)
     status = nf90_put_var(icdf,varid,ss%qatom_RFMIN)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qatom_RFMIN"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_S)) THEN
     status = nf90_inq_varid(icdf,"qatom_S",varid)
     status = nf90_put_var(icdf,varid,ss%qatom_S)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qatom_S"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_SA)) THEN
     status = nf90_inq_varid(icdf,"qatom_SA",varid)
     status = nf90_put_var(icdf,varid,ss%qatom_SA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qatom_SA"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_SFUS)) THEN
     status = nf90_inq_varid(icdf,"qatom_SFUS",varid)
     status = nf90_put_var(icdf,varid,ss%qatom_SFUS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qatom_SFUS"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_SGAS)) THEN
     status = nf90_inq_varid(icdf,"qatom_SGAS",varid)
     status = nf90_put_var(icdf,varid,ss%qatom_SGAS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qatom_SGAS"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_SIMP0)) THEN
     status = nf90_inq_varid(icdf,"qatom_SIMP0",varid)
     status = nf90_put_var(icdf,varid,ss%qatom_SIMP0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qatom_SIMP0"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_SIMPI)) THEN
     status = nf90_inq_varid(icdf,"qatom_SIMPI",varid)
     status = nf90_put_var(icdf,varid,ss%qatom_SIMPI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qatom_SIMPI"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_SNBI)) THEN
     status = nf90_inq_varid(icdf,"qatom_SNBI",varid)
     status = nf90_put_var(icdf,varid,ss%qatom_SNBI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qatom_SNBI"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qbeami)) THEN
     status = nf90_inq_varid(icdf,"qbeami",varid)
     status = nf90_put_var(icdf,varid,ss%qbeami)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qbeami"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qcomp_e)) THEN
     status = nf90_inq_varid(icdf,"qcomp_e",varid)
     status = nf90_put_var(icdf,varid,ss%qcomp_e)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qcomp_e"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qcomp_i)) THEN
     status = nf90_inq_varid(icdf,"qcomp_i",varid)
     status = nf90_put_var(icdf,varid,ss%qcomp_i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qcomp_i"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qie)) THEN
     status = nf90_inq_varid(icdf,"qie",varid)
     status = nf90_put_var(icdf,varid,ss%qie)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qie"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qioniz)) THEN
     status = nf90_inq_varid(icdf,"qioniz",varid)
     status = nf90_put_var(icdf,varid,ss%qioniz)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qioniz"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ql_operator)) THEN
     status = nf90_inq_varid(icdf,"ql_operator",varid)
     status = nf90_put_var(icdf,varid,ss%ql_operator)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ql_operator"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qqcx)) THEN
     status = nf90_inq_varid(icdf,"qqcx",varid)
     status = nf90_put_var(icdf,varid,ss%qqcx)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qqcx"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qrot_conv)) THEN
     status = nf90_inq_varid(icdf,"qrot_conv",varid)
     status = nf90_put_var(icdf,varid,ss%qrot_conv)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qrot_conv"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qrot_diff)) THEN
     status = nf90_inq_varid(icdf,"qrot_diff",varid)
     status = nf90_put_var(icdf,varid,ss%qrot_diff)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: qrot_diff"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_ALL)) THEN
     status = nf90_inq_varid(icdf,"q_ALL",varid)
     status = nf90_put_var(icdf,varid,ss%q_ALL)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: q_ALL"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_ALLA)) THEN
     status = nf90_inq_varid(icdf,"q_ALLA",varid)
     status = nf90_put_var(icdf,varid,ss%q_ALLA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: q_ALLA"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_eq)) THEN
     status = nf90_inq_varid(icdf,"q_eq",varid)
     status = nf90_put_var(icdf,varid,ss%q_eq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: q_eq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_RFMIN)) THEN
     status = nf90_inq_varid(icdf,"q_RFMIN",varid)
     status = nf90_put_var(icdf,varid,ss%q_RFMIN)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: q_RFMIN"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_S)) THEN
     status = nf90_inq_varid(icdf,"q_S",varid)
     status = nf90_put_var(icdf,varid,ss%q_S)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: q_S"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_SA)) THEN
     status = nf90_inq_varid(icdf,"q_SA",varid)
     status = nf90_put_var(icdf,varid,ss%q_SA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: q_SA"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_SFUS)) THEN
     status = nf90_inq_varid(icdf,"q_SFUS",varid)
     status = nf90_put_var(icdf,varid,ss%q_SFUS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: q_SFUS"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_SGAS)) THEN
     status = nf90_inq_varid(icdf,"q_SGAS",varid)
     status = nf90_put_var(icdf,varid,ss%q_SGAS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: q_SGAS"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_SIMP0)) THEN
     status = nf90_inq_varid(icdf,"q_SIMP0",varid)
     status = nf90_put_var(icdf,varid,ss%q_SIMP0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: q_SIMP0"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_SIMPI)) THEN
     status = nf90_inq_varid(icdf,"q_SIMPI",varid)
     status = nf90_put_var(icdf,varid,ss%q_SIMPI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: q_SIMPI"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_SNBI)) THEN
     status = nf90_inq_varid(icdf,"q_SNBI",varid)
     status = nf90_put_var(icdf,varid,ss%q_SNBI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: q_SNBI"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R0_momeq)) THEN
     status = nf90_inq_varid(icdf,"R0_momeq",varid)
     status = nf90_put_var(icdf,varid,ss%R0_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: R0_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RAD_Code_Info",varid)
  status = nf90_put_var(icdf,varid,ss%RAD_Code_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: RAD_Code_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"RAD_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%RAD_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: RAD_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%rate_sinb0i)) THEN
     status = nf90_inq_varid(icdf,"rate_sinb0i",varid)
     status = nf90_put_var(icdf,varid,ss%rate_sinb0i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rate_sinb0i"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rate_sinb0x)) THEN
     status = nf90_inq_varid(icdf,"rate_sinb0x",varid)
     status = nf90_put_var(icdf,varid,ss%rate_sinb0x)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rate_sinb0x"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rate_sinb0xs)) THEN
     status = nf90_inq_varid(icdf,"rate_sinb0xs",varid)
     status = nf90_put_var(icdf,varid,ss%rate_sinb0xs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rate_sinb0xs"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rate_sinf0i)) THEN
     status = nf90_inq_varid(icdf,"rate_sinf0i",varid)
     status = nf90_put_var(icdf,varid,ss%rate_sinf0i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rate_sinf0i"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rate_sinf0x)) THEN
     status = nf90_inq_varid(icdf,"rate_sinf0x",varid)
     status = nf90_put_var(icdf,varid,ss%rate_sinf0x)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rate_sinf0x"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rate_sinf0xs)) THEN
     status = nf90_inq_varid(icdf,"rate_sinf0xs",varid)
     status = nf90_put_var(icdf,varid,ss%rate_sinf0xs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rate_sinf0xs"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Real_ant_coef)) THEN
     status = nf90_inq_varid(icdf,"Real_ant_coef",varid)
     status = nf90_put_var(icdf,varid,ss%Real_ant_coef)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Real_ant_coef"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%res_sn)) THEN
     status = nf90_inq_varid(icdf,"res_sn",varid)
     status = nf90_put_var(icdf,varid,ss%res_sn)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: res_sn"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%res_te)) THEN
     status = nf90_inq_varid(icdf,"res_te",varid)
     status = nf90_put_var(icdf,varid,ss%res_te)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: res_te"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%res_ti)) THEN
     status = nf90_inq_varid(icdf,"res_ti",varid)
     status = nf90_put_var(icdf,varid,ss%res_ti)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: res_ti"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%res_tq)) THEN
     status = nf90_inq_varid(icdf,"res_tq",varid)
     status = nf90_put_var(icdf,varid,ss%res_tq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: res_tq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%RFMIN_name)) THEN
     status = nf90_inq_varid(icdf,"RFMIN_name",varid)
     status = nf90_put_var(icdf,varid,ss%RFMIN_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: RFMIN_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rfmin_to_all)) THEN
     status = nf90_inq_varid(icdf,"rfmin_to_all",varid)
     status = nf90_put_var(icdf,varid,ss%rfmin_to_all)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rfmin_to_all"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rfmin_to_alla)) THEN
     status = nf90_inq_varid(icdf,"rfmin_to_alla",varid)
     status = nf90_put_var(icdf,varid,ss%rfmin_to_alla)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rfmin_to_alla"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%RFMIN_type)) THEN
     status = nf90_inq_varid(icdf,"RFMIN_type",varid)
     status = nf90_put_var(icdf,varid,ss%RFMIN_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: RFMIN_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho)) THEN
     status = nf90_inq_varid(icdf,"rho",varid)
     status = nf90_put_var(icdf,varid,ss%rho)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rhopsi0lh)) THEN
     status = nf90_inq_varid(icdf,"rhopsi0lh",varid)
     status = nf90_put_var(icdf,varid,ss%rhopsi0lh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rhopsi0lh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_anom)) THEN
     status = nf90_inq_varid(icdf,"rho_anom",varid)
     status = nf90_put_var(icdf,varid,ss%rho_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_anom2)) THEN
     status = nf90_inq_varid(icdf,"rho_anom2",varid)
     status = nf90_put_var(icdf,varid,ss%rho_anom2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_anom2"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_anomth)) THEN
     status = nf90_inq_varid(icdf,"rho_anomth",varid)
     status = nf90_put_var(icdf,varid,ss%rho_anomth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_anomth"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_bdy_ns)) THEN
     status = nf90_inq_varid(icdf,"rho_bdy_ns",varid)
     status = nf90_put_var(icdf,varid,ss%rho_bdy_ns)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_bdy_ns"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_omegat",varid)
  status = nf90_put_var(icdf,varid,ss%rho_bdy_omegat)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: rho_bdy_omegat"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_Te",varid)
  status = nf90_put_var(icdf,varid,ss%rho_bdy_Te)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: rho_bdy_Te"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_Ti",varid)
  status = nf90_put_var(icdf,varid,ss%rho_bdy_Ti)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: rho_bdy_Ti"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%rho_ecrf)) THEN
     status = nf90_inq_varid(icdf,"rho_ecrf",varid)
     status = nf90_put_var(icdf,varid,ss%rho_ecrf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_ecrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_eq)) THEN
     status = nf90_inq_varid(icdf,"rho_eq",varid)
     status = nf90_put_var(icdf,varid,ss%rho_eq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_eq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_eq_geo)) THEN
     status = nf90_inq_varid(icdf,"rho_eq_geo",varid)
     status = nf90_put_var(icdf,varid,ss%rho_eq_geo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_eq_geo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_fus)) THEN
     status = nf90_inq_varid(icdf,"rho_fus",varid)
     status = nf90_put_var(icdf,varid,ss%rho_fus)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_fus"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_gas)) THEN
     status = nf90_inq_varid(icdf,"rho_gas",varid)
     status = nf90_put_var(icdf,varid,ss%rho_gas)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_gas"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_icrf)) THEN
     status = nf90_inq_varid(icdf,"rho_icrf",varid)
     status = nf90_put_var(icdf,varid,ss%rho_icrf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_icrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_lhrf)) THEN
     status = nf90_inq_varid(icdf,"rho_lhrf",varid)
     status = nf90_put_var(icdf,varid,ss%rho_lhrf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_lhrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_lmhd)) THEN
     status = nf90_inq_varid(icdf,"rho_lmhd",varid)
     status = nf90_put_var(icdf,varid,ss%rho_lmhd)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_lmhd"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_mist)) THEN
     status = nf90_inq_varid(icdf,"rho_mist",varid)
     status = nf90_put_var(icdf,varid,ss%rho_mist)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_mist"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_nbi)) THEN
     status = nf90_inq_varid(icdf,"rho_nbi",varid)
     status = nf90_put_var(icdf,varid,ss%rho_nbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_nbi"
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

END SUBROUTINE plasma_state_store_6
