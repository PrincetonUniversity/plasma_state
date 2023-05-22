! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_store_def_7(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%rho_pel)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_pel",idims(indim))
     CALL define_var("rho_pel","R",indim,idims, &
            "rho grid -- PEL", &
            "-","PEL","SIMULATION_INIT", &
            "G  rho_pel(nrho_pel)")
  ENDIF

  IF(ALLOCATED(ss%rho_rad)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_rad",idims(indim))
     CALL define_var("rho_rad","R",indim,idims, &
            "rho grid (RAD)", &
            "-","RAD","SIMULATION_INIT", &
            "G  rho_rad(nrho_rad)")
  ENDIF

  IF(ALLOCATED(ss%rho_rw)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_rw",idims(indim))
     CALL define_var("rho_rw","R",indim,idims, &
            "rho grid -- RUNAWAY electrons", &
            "-","RUNAWAY","SIMULATION_INIT", &
            "G  rho_rw(nrho_rw)")
  ENDIF

  IF(ALLOCATED(ss%rlim)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_num_rzlim",idims(indim))
     CALL define_var("rlim","R",indim,idims, &
            "R points in closed (R,Z) contour sequence", &
            "m","EQ","MACHINE_DESCRIPTION", &
            "R|units=m  rlim(num_rzlim)")
  ENDIF

  IF(ALLOCATED(ss%Rloc_coil)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncoils",idims(indim))
     CALL define_var("Rloc_coil","R",indim,idims, &
            "R location of coil, lower left corner", &
            "m","EQ","MACHINE_DESCRIPTION", &
            "R|units=m  Rloc_coil(ncoils)")
  ENDIF

  IF(ALLOCATED(ss%Rmajor_mean)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("Rmajor_mean","R",indim,idims, &
            "Rmajor_mean = (R_Surfmax+R_surfmin)/2", &
            "m","EQ","STATE_PROFILES", &
            "R|pclin  Rmajor_mean(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%rMinor_mean)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("rMinor_mean","R",indim,idims, &
            "rMinor_mean = (R_Surfmax-R_surfmin)/2", &
            "m","EQ","STATE_PROFILES", &
            "R|pclin  rMinor_mean(nrho_eq_geo)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("RUNAWAY_Code_Info","C*80",indim,idims, &
         "Information: code implementing RUNAWAY component", &
         "__NONE","RUNAWAY","SIMULATION_INIT", &
         "C*80   RUNAWAY_Code_Info")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
  CALL define_var("RunID","N",indim,idims, &
         "Run ID", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "N      RunID")

  IF(ALLOCATED(ss%R_ant)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("R_ant","R",indim,idims, &
            "major radius of antenna", &
            "m","IC","SHOT_CONFIGURATION", &
            "R|units=m R_ant(nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%R_antgeo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_nrz_antgeo",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("R_antgeo","R",indim,idims, &
            "antenna geo: R pts", &
            "m","IC","MACHINE_DESCRIPTION", &
            "R|units=m R_antgeo(nrz_antgeo(),nicrf_src)")
  ENDIF

  indim = 0
  CALL define_var("R_axis","R",indim,idims, &
         "R of magnetic axis", &
         "m","EQ","STATE_DATA", &
         "R  R_axis")

  IF(ALLOCATED(ss%R_EC_launch)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("R_EC_launch","R",indim,idims, &
            "R of center of launcher", &
            "m","EC","MACHINE_DESCRIPTION", &
            "R|units=m   R_EC_launch(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%R_geo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nth_eq",idims(indim))
     CALL define_var("R_geo","R",indim,idims, &
            "flux surfaces R(rho,theta)", &
            "m","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit R_geo(nrho_eq,nth_eq)")
  ENDIF

  IF(ALLOCATED(ss%R_grid)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nr",idims(indim))
     CALL define_var("R_grid","R",indim,idims, &
            "R grid", &
            "m","EQ","SIMULATION_INIT", &
            "G   R_grid(nR)")
  ENDIF

  indim = 0
  CALL define_var("R_max_box","R",indim,idims, &
         "R_max of bounding box", &
         "m","EQ","MACHINE_DESCRIPTION", &
         "R  R_max_box")

  indim = 0
  CALL define_var("R_max_lcfs","R",indim,idims, &
         "R_max of last closed flux surface", &
         "m","EQ","STATE_DATA", &
         "R  R_max_lcfs")

  IF(ALLOCATED(ss%R_midp_in)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("R_midp_in","R",indim,idims, &
            "R (high field side) midplane intercept", &
            "m","EQ","STATE_PROFILES", &
            "R|pclin  R_midp_in(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%R_midp_out)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("R_midp_out","R",indim,idims, &
            "R (low field side) midplane intercept", &
            "m","EQ","STATE_PROFILES", &
            "R|pclin  R_midp_out(nrho_eq_geo)")
  ENDIF

  indim = 0
  CALL define_var("R_min_box","R",indim,idims, &
         "R_min of bounding box", &
         "m","EQ","MACHINE_DESCRIPTION", &
         "R  R_min_box")

  indim = 0
  CALL define_var("R_min_lcfs","R",indim,idims, &
         "R_min of last closed flux surface", &
         "m","EQ","STATE_DATA", &
         "R  R_min_lcfs")

  IF(ALLOCATED(ss%R_ripple)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nr_rip",idims(indim))
     CALL define_var("R_ripple","R",indim,idims, &
            "R grid for TF ripple field(s)", &
            "m","RIPPLE","SIMULATION_INIT", &
            "G  R_ripple(nR_rip)")
  ENDIF

  IF(ALLOCATED(ss%R_surfMax)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("R_surfMax","R",indim,idims, &
            "max R on flux surface", &
            "m","EQ","STATE_PROFILES", &
            "R|pclin  R_surfMax(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%R_surfMin)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("R_surfMin","R",indim,idims, &
            "min R on flux surface", &
            "m","EQ","STATE_PROFILES", &
            "R|pclin  R_surfMin(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%s0reco)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("s0reco","R",indim,idims, &
            "recombination neutral sce", &
            "#/sec","GAS","STATE_PROFILES", &
            "R|step*dV|units=#/sec s0reco(~nrho_gas,nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%s0reco_e)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     CALL define_var("s0reco_e","R",indim,idims, &
            "net electron source associated with recombination:", &
            "#/sec","GAS","STATE_PROFILES", &
            "R|step*dV|units=#/sec s0reco_e(~nrho_gas)")
  ENDIF

  IF(ALLOCATED(ss%s0reco_recap)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("s0reco_recap","R",indim,idims, &
            "recombination neutrals recapture", &
            "#/sec","GAS","STATE_PROFILES", &
            "R|step*dV|units=#/sec s0reco_recap(~nrho_gas,nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%sa_index)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_all",idims(indim))
     CALL define_var("sa_index","I",indim,idims, &
            "species index in original lists", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "I sa_index(0:nspec_all)")
  ENDIF

  IF(ALLOCATED(ss%SA_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_tha",idims(indim))
     CALL define_var("SA_name","N",indim,idims, &
            "thermal species abridged", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "S|thermal_specie  SA(0:nspec_tha)")
  ENDIF

  IF(ALLOCATED(ss%SA_type)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_tha",idims(indim))
     CALL define_var("SA_type","I",indim,idims, &
            "SA specie types", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "S|thermal_specie  SA(0:nspec_tha)")
  ENDIF

  IF(ALLOCATED(ss%sb0halo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("sb0halo","R",indim,idims, &
            "beam dep cx sink", &
            "#/sec","NBI","STATE_PROFILES", &
            "R|step*dV|units=#/sec sb0halo(~nrho_nbi,nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%sb0halo_recap)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("sb0halo_recap","R",indim,idims, &
            "cx neutral recapture", &
            "#/sec","NBI","STATE_PROFILES", &
            "R|step*dV|units=#/sec sb0halo_recap(~nrho_nbi,nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%sbedep)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("sbedep","R",indim,idims, &
            "beam deposition cold electron sce", &
            "#/sec","NBI","STATE_PROFILES", &
            "R|step*dV|units=#/sec sbedep(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%sbehalo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     CALL define_var("sbehalo","R",indim,idims, &
            "cold electron sce due to", &
            "#/sec","NBI","STATE_PROFILES", &
            "R|step*dV|units=#/sec sbehalo(~nrho_nbi)")
  ENDIF

  IF(ALLOCATED(ss%sbsce)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_tha",idims(indim))
     CALL define_var("sbsce","R",indim,idims, &
            "net source, thermal ions & electrons", &
            "#/sec","NBI","STATE_PROFILES", &
            "R|step*dV|units=#/sec   sbsce(~nrho_nbi,0:nspec_tha)")
  ENDIF

  IF(ALLOCATED(ss%sbtherm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("sbtherm","R",indim,idims, &
            "beam ion thermalization", &
            "#/sec","NBI","STATE_PROFILES", &
            "R|step*dV|units=#/sec sbtherm(~nrho_nbi,nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%sc0)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("sc0","R",indim,idims, &
            "edge neutral sources (atoms/sec)", &
            "#/sec","GAS","STATE_DATA", &
            "R|units=#/sec  sc0(ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%sc0_to_sgas)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("sc0_to_sgas","I",indim,idims, &
            "index map from neutral source to neutral species", &
            "__NONE","GAS","SIMULATION_INIT", &
            "I  sc0_to_sgas(ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%sc_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("sc_anom","R",indim,idims, &
            "anomalous sources", &
            "#/sec","ANOM","STATE_PROFILES", &
            "R|units=#/sec|step*dV sc_anom(~nrho_anomth,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%sfsce)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_fus",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_tha",idims(indim))
     CALL define_var("sfsce","R",indim,idims, &
            "net thermalization source of fusion ions", &
            "#/sec","FUS","STATE_PROFILES", &
            "R|step*dV|units=#/sec   sfsce(~nrho_fus,0:nspec_tha)")
  ENDIF

  IF(ALLOCATED(ss%sftherm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_fus",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("sftherm","R",indim,idims, &
            "fusion ion thermalization", &
            "#/sec","FUS","STATE_PROFILES", &
            "R|step*dV|units=#/sec   sftherm(~nrho_fus,nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%SFUS_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("SFUS_name","N",indim,idims, &
            "fusion product ion species", &
            "__NONE","FUS","SHOT_CONFIGURATION", &
            "S|fusion_ion   SFUS(nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%sfus_to_all)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("sfus_to_all","I",indim,idims, &
            'map sfus index to "all" list', &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "I  sfus_to_all(nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%sfus_to_alla)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("sfus_to_alla","I",indim,idims, &
            'map sfus index to "alla" list', &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "I  sfus_to_alla(nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%SFUS_type)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("SFUS_type","I",indim,idims, &
            "SFUS specie types", &
            "__NONE","FUS","SHOT_CONFIGURATION", &
            "S|fusion_ion   SFUS(nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%SGAS_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("SGAS_name","N",indim,idims, &
            "atomic neutral species in plasma core", &
            "__NONE","GAS","SIMULATION_INIT", &
            "S|neutral_gas  SGAS(nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%sgas_to_s)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("sgas_to_s","I",indim,idims, &
            'map sgas index to "s" list', &
            "__NONE","GAS","SIMULATION_INIT", &
            "I  sgas_to_s(nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%SGAS_type)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("SGAS_type","I",indim,idims, &
            "SGAS specie types", &
            "__NONE","GAS","SIMULATION_INIT", &
            "S|neutral_gas  SGAS(nspec_gas)")
  ENDIF

  indim = 0
  CALL define_var("shot_number","I",indim,idims, &
         "integer shot number", &
         "__NONE","PLASMA","SHOT_CONFIGURATION", &
         "I shot_number")

  IF(ALLOCATED(ss%sii_xs)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_mist",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nxs",idims(indim))
     CALL define_var("sii_xs","R",indim,idims, &
            "species source/sinks driven by xs(1:nxs)", &
            "(#/sec)/(#/sec)","RAD","STATE_PROFILES", &
            "R|step*dV|units=(#/sec)/(#/sec) sii_xs(~nrho_mist,0:nspec_th,nxs)")
  ENDIF

  IF(ALLOCATED(ss%SIMP0_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_imp0",idims(indim))
     CALL define_var("SIMP0_name","N",indim,idims, &
            "atomic impurity species in simulation", &
            "__NONE","GAS","SIMULATION_INIT", &
            "S|impurity_atoms SIMP0(nspec_imp0)")
  ENDIF

  IF(ALLOCATED(ss%simp0_to_s)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_imp0",idims(indim))
     CALL define_var("simp0_to_s","I",indim,idims, &
            'map simp0 index to "s" list', &
            "__NONE","GAS","SIMULATION_INIT", &
            "I  simp0_to_s(nspec_imp0)")
  ENDIF

  IF(ALLOCATED(ss%SIMP0_type)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_imp0",idims(indim))
     CALL define_var("SIMP0_type","I",indim,idims, &
            "SIMP0 specie types", &
            "__NONE","GAS","SIMULATION_INIT", &
            "S|impurity_atoms SIMP0(nspec_imp0)")
  ENDIF

  IF(ALLOCATED(ss%SIMPI_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_impi",idims(indim))
     CALL define_var("SIMPI_name","N",indim,idims, &
            "impurity ion species in simulation", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "S|impurity_atoms SIMPI(nspec_impi)")
  ENDIF

  IF(ALLOCATED(ss%simpi_to_s)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_impi",idims(indim))
     CALL define_var("simpi_to_s","I",indim,idims, &
            'map simpi index to "s" list', &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "I  simpi_to_s(nspec_impi)")
  ENDIF

  IF(ALLOCATED(ss%SIMPI_type)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_impi",idims(indim))
     CALL define_var("SIMPI_type","I",indim,idims, &
            "SIMPI specie types", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "S|impurity_atoms SIMPI(nspec_impi)")
  ENDIF

  IF(ALLOCATED(ss%SNBI_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("SNBI_name","N",indim,idims, &
            "beam ion species", &
            "__NONE","NBI","SIMULATION_INIT", &
            "S|beam_ion     SNBI(nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%snbi_to_all)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("snbi_to_all","I",indim,idims, &
            'map snbi index to "all" list', &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "I  snbi_to_all(nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%snbi_to_alla)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("snbi_to_alla","I",indim,idims, &
            'map snbi index to "alla" list', &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "I  snbi_to_alla(nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%SNBI_type)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("SNBI_type","I",indim,idims, &
            "SNBI specie types", &
            "__NONE","NBI","SIMULATION_INIT", &
            "S|beam_ion     SNBI(nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%sn_trans)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("sn_trans","R",indim,idims, &
            "particle transport (loss)", &
            "#/sec","PLASMA","STATE_PROFILES", &
            "R|units=#/sec|step*dV sn_trans(~nrho,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%sn_transcat)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("sn_transcat","R",indim,idims, &
            "species transport", &
            "#/sec","PLASMA","STATE_PROFILES", &
            "R|step*dV|units=#/sec  sn_transcat(~nrho,0:nspec_th,ntrcat)")
  ENDIF

  IF(ALLOCATED(ss%sprof0)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("sprof0","R",indim,idims, &
            "gas species source/sinks per unit species influx", &
            "(#/sec)/(#/sec)","GAS","STATE_PROFILES", &
            "R|step*dV|units=(#/sec)/(#/sec)  sprof0(~nrho_gas,nspec_gas,ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%sprof0e)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("sprof0e","R",indim,idims, &
            "electron source per unit neutral species influx", &
            "(#/sec)/(#/sec)","GAS","STATE_PROFILES", &
            "R|step*dV|units=(#/sec)/(#/sec)  sprof0e(~nrho_gas,ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%spsce)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_pel",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_tha",idims(indim))
     CALL define_var("spsce","R",indim,idims, &
            "net source, thermal ions & electrons", &
            "#/sec","PEL","STATE_PROFILES", &
            "R|step*dV|units=#/sec   spsce(~nrho_pel,0:nspec_tha)")
  ENDIF

  IF(ALLOCATED(ss%squareLO)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("squareLO","R",indim,idims, &
            "lower outer squareness", &
            "-","EQ","STATE_PROFILES", &
            "R|pclin  squareLO(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%squareUO)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("squareUO","R",indim,idims, &
            "upper outer squareness", &
            "-","EQ","STATE_PROFILES", &
            "R|pclin  squareUO(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%sRtcen)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("sRtcen","R",indim,idims, &
            "signed tangency radius", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m       sRtcen(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%surf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("surf","R",indim,idims, &
            "area of flux surfaces", &
            "m^2","EQ","STATE_PROFILES", &
            "R|units=m^2|Spline surf(nrho_eq)")
  ENDIF

  IF(ALLOCATED(ss%S_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("S_name","N",indim,idims, &
            "thermal species list", &
            "__NONE","PLASMA","SHOT_CONFIGURATION", &
            "S|thermal_specie  S(0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%s_to_sa)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("s_to_sa","I",indim,idims, &
            "address in sa species list", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "I s_to_sa(0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%S_type)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("S_type","I",indim,idims, &
            "S specie types", &
            "__NONE","PLASMA","SHOT_CONFIGURATION", &
            "S|thermal_specie  S(0:nspec_th)")
  ENDIF

  indim = 0
  CALL define_var("t0","R",indim,idims, &
         "time at beginning of IPS macro timestep", &
         "sec","PLASMA","STATE_DATA", &
         "R  t0")

  IF(ALLOCATED(ss%T0cx)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("T0cx","R",indim,idims, &
            "effective neutral temperature for CX power", &
            "keV","GAS","STATE_PROFILES", &
            "R|step*qqcx|units=keV  T0cx(~nrho_gas,ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%T0sc0)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("T0sc0","R",indim,idims, &
            "neutral temperature (2/3)<E0>", &
            "keV","GAS","STATE_PROFILES", &
            "R|step*n0norm|units=keV        T0sc0(~nrho_gas,nspec_gas,ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%T0_halo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("T0_halo","R",indim,idims, &
            "halo neutral temperature (2/3)<E0>", &
            "keV","NBI","STATE_PROFILES", &
            "R|units=keV|step*n0_halo  T0_halo(~nrho_nbi,nspec_gas)")
  ENDIF

  IF(ALLOCATED(ss%T0_reco)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_gas",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_gas",idims(indim))
     CALL define_var("T0_reco","R",indim,idims, &
            "reco neutral temperature (2/3)<E0>", &
            "keV","GAS","STATE_PROFILES", &
            "R|units=keV|step*n0_reco  T0_reco(~nrho_gas,nspec_gas)")
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

END SUBROUTINE plasma_state_store_def_7


SUBROUTINE plasma_state_store_7(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%rho_pel)) THEN
     status = nf90_inq_varid(icdf,"rho_pel",varid)
     status = nf90_put_var(icdf,varid,ss%rho_pel)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_pel"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_rad)) THEN
     status = nf90_inq_varid(icdf,"rho_rad",varid)
     status = nf90_put_var(icdf,varid,ss%rho_rad)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_rad"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_rw)) THEN
     status = nf90_inq_varid(icdf,"rho_rw",varid)
     status = nf90_put_var(icdf,varid,ss%rho_rw)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rho_rw"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rlim)) THEN
     status = nf90_inq_varid(icdf,"rlim",varid)
     status = nf90_put_var(icdf,varid,ss%rlim)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rlim"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Rloc_coil)) THEN
     status = nf90_inq_varid(icdf,"Rloc_coil",varid)
     status = nf90_put_var(icdf,varid,ss%Rloc_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Rloc_coil"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Rmajor_mean)) THEN
     status = nf90_inq_varid(icdf,"Rmajor_mean",varid)
     status = nf90_put_var(icdf,varid,ss%Rmajor_mean)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Rmajor_mean"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rMinor_mean)) THEN
     status = nf90_inq_varid(icdf,"rMinor_mean",varid)
     status = nf90_put_var(icdf,varid,ss%rMinor_mean)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: rMinor_mean"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RUNAWAY_Code_Info",varid)
  status = nf90_put_var(icdf,varid,ss%RUNAWAY_Code_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: RUNAWAY_Code_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"RunID",varid)
  status = nf90_put_var(icdf,varid,ss%RunID)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: RunID"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%R_ant)) THEN
     status = nf90_inq_varid(icdf,"R_ant",varid)
     status = nf90_put_var(icdf,varid,ss%R_ant)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: R_ant"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_antgeo)) THEN
     status = nf90_inq_varid(icdf,"R_antgeo",varid)
     status = nf90_put_var(icdf,varid,ss%R_antgeo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: R_antgeo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_axis",varid)
  status = nf90_put_var(icdf,varid,ss%R_axis)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: R_axis"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%R_EC_launch)) THEN
     status = nf90_inq_varid(icdf,"R_EC_launch",varid)
     status = nf90_put_var(icdf,varid,ss%R_EC_launch)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: R_EC_launch"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_geo)) THEN
     status = nf90_inq_varid(icdf,"R_geo",varid)
     status = nf90_put_var(icdf,varid,ss%R_geo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: R_geo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_grid)) THEN
     status = nf90_inq_varid(icdf,"R_grid",varid)
     status = nf90_put_var(icdf,varid,ss%R_grid)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: R_grid"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_max_box",varid)
  status = nf90_put_var(icdf,varid,ss%R_max_box)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: R_max_box"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"R_max_lcfs",varid)
  status = nf90_put_var(icdf,varid,ss%R_max_lcfs)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: R_max_lcfs"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%R_midp_in)) THEN
     status = nf90_inq_varid(icdf,"R_midp_in",varid)
     status = nf90_put_var(icdf,varid,ss%R_midp_in)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: R_midp_in"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_midp_out)) THEN
     status = nf90_inq_varid(icdf,"R_midp_out",varid)
     status = nf90_put_var(icdf,varid,ss%R_midp_out)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: R_midp_out"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_min_box",varid)
  status = nf90_put_var(icdf,varid,ss%R_min_box)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: R_min_box"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"R_min_lcfs",varid)
  status = nf90_put_var(icdf,varid,ss%R_min_lcfs)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: R_min_lcfs"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%R_ripple)) THEN
     status = nf90_inq_varid(icdf,"R_ripple",varid)
     status = nf90_put_var(icdf,varid,ss%R_ripple)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: R_ripple"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_surfMax)) THEN
     status = nf90_inq_varid(icdf,"R_surfMax",varid)
     status = nf90_put_var(icdf,varid,ss%R_surfMax)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: R_surfMax"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_surfMin)) THEN
     status = nf90_inq_varid(icdf,"R_surfMin",varid)
     status = nf90_put_var(icdf,varid,ss%R_surfMin)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: R_surfMin"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%s0reco)) THEN
     status = nf90_inq_varid(icdf,"s0reco",varid)
     status = nf90_put_var(icdf,varid,ss%s0reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: s0reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%s0reco_e)) THEN
     status = nf90_inq_varid(icdf,"s0reco_e",varid)
     status = nf90_put_var(icdf,varid,ss%s0reco_e)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: s0reco_e"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%s0reco_recap)) THEN
     status = nf90_inq_varid(icdf,"s0reco_recap",varid)
     status = nf90_put_var(icdf,varid,ss%s0reco_recap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: s0reco_recap"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sa_index)) THEN
     status = nf90_inq_varid(icdf,"sa_index",varid)
     status = nf90_put_var(icdf,varid,ss%sa_index)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sa_index"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SA_name)) THEN
     status = nf90_inq_varid(icdf,"SA_name",varid)
     status = nf90_put_var(icdf,varid,ss%SA_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: SA_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SA_type)) THEN
     status = nf90_inq_varid(icdf,"SA_type",varid)
     status = nf90_put_var(icdf,varid,ss%SA_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: SA_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sb0halo)) THEN
     status = nf90_inq_varid(icdf,"sb0halo",varid)
     status = nf90_put_var(icdf,varid,ss%sb0halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sb0halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sb0halo_recap)) THEN
     status = nf90_inq_varid(icdf,"sb0halo_recap",varid)
     status = nf90_put_var(icdf,varid,ss%sb0halo_recap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sb0halo_recap"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sbedep)) THEN
     status = nf90_inq_varid(icdf,"sbedep",varid)
     status = nf90_put_var(icdf,varid,ss%sbedep)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sbedep"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sbehalo)) THEN
     status = nf90_inq_varid(icdf,"sbehalo",varid)
     status = nf90_put_var(icdf,varid,ss%sbehalo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sbehalo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sbsce)) THEN
     status = nf90_inq_varid(icdf,"sbsce",varid)
     status = nf90_put_var(icdf,varid,ss%sbsce)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sbsce"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sbtherm)) THEN
     status = nf90_inq_varid(icdf,"sbtherm",varid)
     status = nf90_put_var(icdf,varid,ss%sbtherm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sbtherm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sc0)) THEN
     status = nf90_inq_varid(icdf,"sc0",varid)
     status = nf90_put_var(icdf,varid,ss%sc0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sc0"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sc0_to_sgas)) THEN
     status = nf90_inq_varid(icdf,"sc0_to_sgas",varid)
     status = nf90_put_var(icdf,varid,ss%sc0_to_sgas)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sc0_to_sgas"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sc_anom)) THEN
     status = nf90_inq_varid(icdf,"sc_anom",varid)
     status = nf90_put_var(icdf,varid,ss%sc_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sc_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sfsce)) THEN
     status = nf90_inq_varid(icdf,"sfsce",varid)
     status = nf90_put_var(icdf,varid,ss%sfsce)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sfsce"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sftherm)) THEN
     status = nf90_inq_varid(icdf,"sftherm",varid)
     status = nf90_put_var(icdf,varid,ss%sftherm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sftherm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SFUS_name)) THEN
     status = nf90_inq_varid(icdf,"SFUS_name",varid)
     status = nf90_put_var(icdf,varid,ss%SFUS_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: SFUS_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sfus_to_all)) THEN
     status = nf90_inq_varid(icdf,"sfus_to_all",varid)
     status = nf90_put_var(icdf,varid,ss%sfus_to_all)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sfus_to_all"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sfus_to_alla)) THEN
     status = nf90_inq_varid(icdf,"sfus_to_alla",varid)
     status = nf90_put_var(icdf,varid,ss%sfus_to_alla)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sfus_to_alla"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SFUS_type)) THEN
     status = nf90_inq_varid(icdf,"SFUS_type",varid)
     status = nf90_put_var(icdf,varid,ss%SFUS_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: SFUS_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SGAS_name)) THEN
     status = nf90_inq_varid(icdf,"SGAS_name",varid)
     status = nf90_put_var(icdf,varid,ss%SGAS_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: SGAS_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sgas_to_s)) THEN
     status = nf90_inq_varid(icdf,"sgas_to_s",varid)
     status = nf90_put_var(icdf,varid,ss%sgas_to_s)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sgas_to_s"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SGAS_type)) THEN
     status = nf90_inq_varid(icdf,"SGAS_type",varid)
     status = nf90_put_var(icdf,varid,ss%SGAS_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: SGAS_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"shot_number",varid)
  status = nf90_put_var(icdf,varid,ss%shot_number)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: shot_number"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%sii_xs)) THEN
     status = nf90_inq_varid(icdf,"sii_xs",varid)
     status = nf90_put_var(icdf,varid,ss%sii_xs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sii_xs"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SIMP0_name)) THEN
     status = nf90_inq_varid(icdf,"SIMP0_name",varid)
     status = nf90_put_var(icdf,varid,ss%SIMP0_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: SIMP0_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%simp0_to_s)) THEN
     status = nf90_inq_varid(icdf,"simp0_to_s",varid)
     status = nf90_put_var(icdf,varid,ss%simp0_to_s)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: simp0_to_s"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SIMP0_type)) THEN
     status = nf90_inq_varid(icdf,"SIMP0_type",varid)
     status = nf90_put_var(icdf,varid,ss%SIMP0_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: SIMP0_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SIMPI_name)) THEN
     status = nf90_inq_varid(icdf,"SIMPI_name",varid)
     status = nf90_put_var(icdf,varid,ss%SIMPI_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: SIMPI_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%simpi_to_s)) THEN
     status = nf90_inq_varid(icdf,"simpi_to_s",varid)
     status = nf90_put_var(icdf,varid,ss%simpi_to_s)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: simpi_to_s"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SIMPI_type)) THEN
     status = nf90_inq_varid(icdf,"SIMPI_type",varid)
     status = nf90_put_var(icdf,varid,ss%SIMPI_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: SIMPI_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SNBI_name)) THEN
     status = nf90_inq_varid(icdf,"SNBI_name",varid)
     status = nf90_put_var(icdf,varid,ss%SNBI_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: SNBI_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%snbi_to_all)) THEN
     status = nf90_inq_varid(icdf,"snbi_to_all",varid)
     status = nf90_put_var(icdf,varid,ss%snbi_to_all)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: snbi_to_all"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%snbi_to_alla)) THEN
     status = nf90_inq_varid(icdf,"snbi_to_alla",varid)
     status = nf90_put_var(icdf,varid,ss%snbi_to_alla)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: snbi_to_alla"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SNBI_type)) THEN
     status = nf90_inq_varid(icdf,"SNBI_type",varid)
     status = nf90_put_var(icdf,varid,ss%SNBI_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: SNBI_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sn_trans)) THEN
     status = nf90_inq_varid(icdf,"sn_trans",varid)
     status = nf90_put_var(icdf,varid,ss%sn_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sn_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sn_transcat)) THEN
     status = nf90_inq_varid(icdf,"sn_transcat",varid)
     status = nf90_put_var(icdf,varid,ss%sn_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sn_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sprof0)) THEN
     status = nf90_inq_varid(icdf,"sprof0",varid)
     status = nf90_put_var(icdf,varid,ss%sprof0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sprof0"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sprof0e)) THEN
     status = nf90_inq_varid(icdf,"sprof0e",varid)
     status = nf90_put_var(icdf,varid,ss%sprof0e)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sprof0e"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%spsce)) THEN
     status = nf90_inq_varid(icdf,"spsce",varid)
     status = nf90_put_var(icdf,varid,ss%spsce)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: spsce"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%squareLO)) THEN
     status = nf90_inq_varid(icdf,"squareLO",varid)
     status = nf90_put_var(icdf,varid,ss%squareLO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: squareLO"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%squareUO)) THEN
     status = nf90_inq_varid(icdf,"squareUO",varid)
     status = nf90_put_var(icdf,varid,ss%squareUO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: squareUO"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sRtcen)) THEN
     status = nf90_inq_varid(icdf,"sRtcen",varid)
     status = nf90_put_var(icdf,varid,ss%sRtcen)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: sRtcen"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%surf)) THEN
     status = nf90_inq_varid(icdf,"surf",varid)
     status = nf90_put_var(icdf,varid,ss%surf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: surf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%S_name)) THEN
     status = nf90_inq_varid(icdf,"S_name",varid)
     status = nf90_put_var(icdf,varid,ss%S_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: S_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%s_to_sa)) THEN
     status = nf90_inq_varid(icdf,"s_to_sa",varid)
     status = nf90_put_var(icdf,varid,ss%s_to_sa)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: s_to_sa"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%S_type)) THEN
     status = nf90_inq_varid(icdf,"S_type",varid)
     status = nf90_put_var(icdf,varid,ss%S_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: S_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"t0",varid)
  status = nf90_put_var(icdf,varid,ss%t0)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: t0"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%T0cx)) THEN
     status = nf90_inq_varid(icdf,"T0cx",varid)
     status = nf90_put_var(icdf,varid,ss%T0cx)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: T0cx"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%T0sc0)) THEN
     status = nf90_inq_varid(icdf,"T0sc0",varid)
     status = nf90_put_var(icdf,varid,ss%T0sc0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: T0sc0"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%T0_halo)) THEN
     status = nf90_inq_varid(icdf,"T0_halo",varid)
     status = nf90_put_var(icdf,varid,ss%T0_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: T0_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%T0_reco)) THEN
     status = nf90_inq_varid(icdf,"T0_reco",varid)
     status = nf90_put_var(icdf,varid,ss%T0_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: T0_reco"
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

END SUBROUTINE plasma_state_store_7
