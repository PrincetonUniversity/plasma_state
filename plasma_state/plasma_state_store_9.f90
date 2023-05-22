! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_store_def_9(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%velpphi_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velpphi_tglf","R",indim,idims, &
            "momentum-equation, advective velocity term from TGLF model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin velpphi_tglf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velte_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velte_anom","R",indim,idims, &
            "Te-equation, advective velocity term", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velte_anom(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velte_glf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velte_glf","R",indim,idims, &
            "Te-equation, advective velocity term from GLF23 model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velte_glf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velte_gtcneo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velte_gtcneo","R",indim,idims, &
            "Te-equation, advective velocity term from GTC_NEO model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velte_gtcneo(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velte_mmm71)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velte_mmm71","R",indim,idims, &
            "Te-equation, advective velocity term from MMM71 model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velte_mmm71(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velte_nclass)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velte_nclass","R",indim,idims, &
            "Te-equation, advective velocity term from NCLASS model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin velte_nclass(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velte_neo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velte_neo","R",indim,idims, &
            "Te-equation, advective velocity term from NEO model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velte_neo(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velte_paleo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velte_paleo","R",indim,idims, &
            "Te-equation, convection term from PALEO model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin   velte_paleo(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velte_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velte_tglf","R",indim,idims, &
            "Te-equation, advective velocity term from TGLF model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin velte_tglf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velti_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velti_anom","R",indim,idims, &
            "Ti-equation, advective velocity term", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velti_anom(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velti_glf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velti_glf","R",indim,idims, &
            "Ti-equation, advective velocity term from GLF23 model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velti_glf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velti_gtcneo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velti_gtcneo","R",indim,idims, &
            "Ti-equation, advective velocity term from GTC_NEO model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velti_gtcneo(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velti_mmm71)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velti_mmm71","R",indim,idims, &
            "Ti-equation, advective velocity term from MMM71 model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velti_mmm71(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velti_nclass)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velti_nclass","R",indim,idims, &
            "Ti-equation, advective velocity term from NCLASS model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin velti_nclass(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velti_neo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velti_neo","R",indim,idims, &
            "Ti-equation, advective velocity term from NEO model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin  velti_neo(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%velti_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("velti_tglf","R",indim,idims, &
            "Ti-equation, advective velocity term from TGLF model", &
            "m/sec","ANOM","STATE_PROFILES", &
            "R|units=m/sec|pclin velti_tglf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%vie_trans)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("vie_trans","R",indim,idims, &
            "ion energy radial velocity", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|pclin   vie_trans(nrho)")
  ENDIF

  IF(ALLOCATED(ss%vie_transcat)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("vie_transcat","R",indim,idims, &
            "Ion energy radial velocity", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|units=m/sec|pclin vie_transcat(nrho,ntrcat)")
  ENDIF

  IF(ALLOCATED(ss%vmo_trans)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("vmo_trans","R",indim,idims, &
            "angular momentum radial velocity", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|pclin   vmo_trans(nrho)")
  ENDIF

  IF(ALLOCATED(ss%vmo_transcat)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("vmo_transcat","R",indim,idims, &
            "Angular momentum radial velocity", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|units=m/sec|pclin vmo_transcat(nrho,ntrcat)")
  ENDIF

  IF(ALLOCATED(ss%vn_trans)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("vn_trans","R",indim,idims, &
            "specie radial velocity", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|units=m/sec|pclin   vn_trans(nrho,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%vn_transcat)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("vn_transcat","R",indim,idims, &
            "specie radial velocity", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|units=m/sec|pclin vn_transcat(nrho,0:nspec_th,ntrcat)")
  ENDIF

  IF(ALLOCATED(ss%vol)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("vol","R",indim,idims, &
            "enclosed volume", &
            "m^3","EQ","STATE_PROFILES", &
            "R|units=m^3|Spline_00  vol(nrho_eq)")
  ENDIF

  IF(ALLOCATED(ss%vphi0_av)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("vphi0_av","R",indim,idims, &
            "average toroidal velocity of neutral source", &
            "m/sec","GAS","STATE_DATA", &
            "R|units=m/sec vphi0_av(ngsc0)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("vpol_Data_Info","C*80",indim,idims, &
         "information on source of poloidal velocity data", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "C*80   vpol_Data_Info")

  IF(ALLOCATED(ss%vpol_inmp)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("vpol_inmp","R",indim,idims, &
            "poloidal velocity, inner half midplane", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|units=m/sec|step*ns vpol_inmp(~nrho,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%vpol_is_input)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("vpol_is_input","I",indim,idims, &
            "specie outer midplane poloidal velocit flag", &
            "__NONE","PLASMA","STATE_DATA", &
            "I vpol_is_input(0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%vpol_omp)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("vpol_omp","R",indim,idims, &
            "poloidal velocity, outer half midplane", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|units=m/sec|step*ns vpol_omp(~nrho,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%VRep_coil_count)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncoils",idims(indim))
     CALL define_var("VRep_coil_count","I",indim,idims, &
            "replication count, vertical coil stack", &
            "__NONE","EQ","MACHINE_DESCRIPTION", &
            "I  VRep_coil_count(ncoils)")
  ENDIF

  IF(ALLOCATED(ss%Vrep_coil_spacing)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncoils",idims(indim))
     CALL define_var("Vrep_coil_spacing","R",indim,idims, &
            "replicated coil separation", &
            "m","EQ","MACHINE_DESCRIPTION", &
            "R|units=m Vrep_coil_spacing(ncoils)")
  ENDIF

  IF(ALLOCATED(ss%Vsize_coil)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncoils",idims(indim))
     CALL define_var("Vsize_coil","R",indim,idims, &
            "(Rotatable) Vertical coil size", &
            "m","EQ","MACHINE_DESCRIPTION", &
            "R|units=m  Vsize_coil(ncoils)")
  ENDIF

  indim = 0
  CALL define_var("vsur","R",indim,idims, &
         "toroidal voltage at surface", &
         "Volts","PLASMA","STATE_DATA", &
         "R  vsur")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("VSUR_Data_Info","C*80",indim,idims, &
         "information on source of surface voltage data", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "C*80   VSUR_Data_Info")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("vtor_Data_Info","C*80",indim,idims, &
         "information on source of toroidal velocity data", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "C*80   vtor_Data_Info")

  IF(ALLOCATED(ss%vtor_inmp)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("vtor_inmp","R",indim,idims, &
            "toroidal velocity, inner half midplane", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|units=m/sec|step*ns vtor_inmp(~nrho,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%vtor_is_input)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("vtor_is_input","I",indim,idims, &
            "specie outer midplane toroidal velocit flag", &
            "__NONE","PLASMA","STATE_DATA", &
            "I vtor_is_input(0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%vtor_omp)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("vtor_omp","R",indim,idims, &
            "toroidal velocity, outer half midplane", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|units=m/sec|step*ns vtor_omp(~nrho,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%V_loop)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("V_loop","R",indim,idims, &
            "loop voltage", &
            "Volts","PLASMA","STATE_PROFILES", &
            "R|units=Volts|pclin	V_loop(nrho)")
  ENDIF

  IF(ALLOCATED(ss%v_pars)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("v_pars","R",indim,idims, &
            "thermal specie <vpll>", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|units=m/sec|alias=vpll_|step*ns v_pars(~nrho,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%v_pers)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("v_pers","R",indim,idims, &
            "thermal specie <vper>", &
            "m/sec","PLASMA","STATE_PROFILES", &
            "R|units=m/sec|alias=vper_|step*ns v_pers(~nrho,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%wt_nphi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_num_nphi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("wt_nphi","R",indim,idims, &
            "vacuum spectrum n_phi weight", &
            "-","IC","SHOT_CONFIGURATION", &
            "R|units=- wt_nphi(num_nphi(),nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%wt_nphi_abs)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_num_nphi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("wt_nphi_abs","R",indim,idims, &
            "absorbed spectrum n_phi weight", &
            "-","IC","STATE_DATA", &
            "R|units=- wt_nphi_abs(num_nphi(),nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%wt_nphi_ext)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_num_nphi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("wt_nphi_ext","R",indim,idims, &
            "fraction of power deposited outside", &
            "-","IC","STATE_DATA", &
            "R|units=- wt_nphi_ext(num_nphi(),nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%xRjcos_momeq)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_neqmom",idims(indim))
     CALL define_var("xRjcos_momeq","R",indim,idims, &
            "scaled R cos moments", &
            "m","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit  xRjcos_momeq(nrho_eq,neqmom)")
  ENDIF

  IF(ALLOCATED(ss%xRjsin_momeq)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_neqmom",idims(indim))
     CALL define_var("xRjsin_momeq","R",indim,idims, &
            "scaled R sin moments", &
            "m","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit  xRjsin_momeq(nrho_eq,neqmom)")
  ENDIF

  IF(ALLOCATED(ss%xs)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nxs",idims(indim))
     CALL define_var("xs","R",indim,idims, &
            "impurity source magnitude (atoms/sec)", &
            "#/sec","RAD","STATE_DATA", &
            "R|units=#/sec    xs(nxs)")
  ENDIF

  IF(ALLOCATED(ss%xs_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nxs",idims(indim))
     CALL define_var("xs_name","N",indim,idims, &
            "number & name of impurity sources", &
            "__NONE","RAD","SHOT_CONFIGURATION", &
            "L|impurity_source  xs_name(nxs)")
  ENDIF

  IF(ALLOCATED(ss%xZjcos_momeq)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_neqmom",idims(indim))
     CALL define_var("xZjcos_momeq","R",indim,idims, &
            "scaled Z cos moments", &
            "m","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit  xZjcos_momeq(nrho_eq,neqmom)")
  ENDIF

  IF(ALLOCATED(ss%xZjsin_momeq)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_neqmom",idims(indim))
     CALL define_var("xZjsin_momeq","R",indim,idims, &
            "scaled Z sin moments", &
            "m","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit  xZjsin_momeq(nrho_eq,neqmom)")
  ENDIF

  indim = 0
  CALL define_var("Z0max","I",indim,idims, &
         "maximum Z of non-impurity species", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "IS  Z0max = 2")

  IF(ALLOCATED(ss%Z0_momeq)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("Z0_momeq","R",indim,idims, &
            "Z0 of flux surface", &
            "m","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit  Z0_momeq(nrho_eq)")
  ENDIF

  IF(ALLOCATED(ss%Zbap)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("Zbap","R",indim,idims, &
            "elevation of beam centerline at aperture", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m       Zbap(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%Zbsc)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("Zbsc","R",indim,idims, &
            "elevation of beam source", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m       Zbsc(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%Zeff)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("Zeff","R",indim,idims, &
            "Zeff (sum[nj*Zj**2]/ne)", &
            "-","PLASMA","STATE_PROFILES", &
            "R|step*ns(:,ps_elec_index)  Zeff(~nrho)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("ZEFF_Data_Info","C*80",indim,idims, &
         "information on source of Zeff data", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "C*80   ZEFF_Data_Info")

  IF(ALLOCATED(ss%Zeff_fi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("Zeff_fi","R",indim,idims, &
            "Fast ion contribution to Zeff", &
            "-","PLASMA","STATE_PROFILES", &
            "R|step*ns(:,ps_elec_index)  Zeff_fi(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%Zeff_th)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("Zeff_th","R",indim,idims, &
            "Thermal ion contribution to Zeff", &
            "-","PLASMA","STATE_PROFILES", &
            "R|step*ns(:,ps_elec_index)  Zeff_th(~nrho)")
  ENDIF

  indim = 0
  CALL define_var("Zimp1","I",indim,idims, &
         "lower limit on Z of impurity in reduced species list SA;", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "IS  Zimp1 = 2")

  indim = 0
  CALL define_var("Zimp2","I",indim,idims, &
         "upper limit on Z of impurity in SA species list;", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "IS  Zimp2 = 1000")

  IF(ALLOCATED(ss%zlim)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_num_rzlim",idims(indim))
     CALL define_var("zlim","R",indim,idims, &
            "Z points in closed (R,Z) contour sequence", &
            "m","EQ","MACHINE_DESCRIPTION", &
            "R|units=m  zlim(num_rzlim)")
  ENDIF

  IF(ALLOCATED(ss%Zloc_coil)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncoils",idims(indim))
     CALL define_var("Zloc_coil","R",indim,idims, &
            "Z location of coil, lower left corner", &
            "m","EQ","MACHINE_DESCRIPTION", &
            "R|units=m  Zloc_coil(ncoils)")
  ENDIF

  IF(ALLOCATED(ss%Z_ant)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("Z_ant","R",indim,idims, &
            "height of antenna", &
            "m","IC","SHOT_CONFIGURATION", &
            "R|units=m Z_ant(nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%Z_antgeo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_nrz_antgeo",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("Z_antgeo","R",indim,idims, &
            "antenna geo: Z pts", &
            "m","IC","MACHINE_DESCRIPTION", &
            "R|units=m Z_antgeo(nrz_antgeo(),nicrf_src)")
  ENDIF

  indim = 0
  CALL define_var("Z_axis","R",indim,idims, &
         "Z of magnetic axis", &
         "m","EQ","STATE_DATA", &
         "R  Z_axis")

  IF(ALLOCATED(ss%Z_EC_launch)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("Z_EC_launch","R",indim,idims, &
            "Z of center of launcher", &
            "m","EC","MACHINE_DESCRIPTION", &
            "R|units=m   Z_EC_launch(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%Z_geo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nth_eq",idims(indim))
     CALL define_var("Z_geo","R",indim,idims, &
            "flux surfaces Z(rho,theta)", &
            "m","EQ","STATE_PROFILES", &
            "R|units=m|Hermite_explicit Z_geo(nrho_eq,nth_eq)")
  ENDIF

  IF(ALLOCATED(ss%Z_grid)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nz",idims(indim))
     CALL define_var("Z_grid","R",indim,idims, &
            "Z grid", &
            "m","EQ","SIMULATION_INIT", &
            "G   Z_grid(nZ)")
  ENDIF

  indim = 0
  CALL define_var("Z_max_box","R",indim,idims, &
         "Z_max of bounding box", &
         "m","EQ","MACHINE_DESCRIPTION", &
         "R  Z_max_box")

  indim = 0
  CALL define_var("Z_max_lcfs","R",indim,idims, &
         "Z_max of last closed flux surface", &
         "m","EQ","STATE_DATA", &
         "R  Z_max_lcfs")

  IF(ALLOCATED(ss%Z_midp)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("Z_midp","R",indim,idims, &
            "flux surface midplane elevation", &
            "m","EQ","STATE_PROFILES", &
            "R|pclin  Z_midp(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%Z_mid_ant)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("Z_mid_ant","R",indim,idims, &
            "center of antenna relative to TF coil midplane", &
            "m","IC","SHOT_CONFIGURATION", &
            "R|units=m Z_mid_ant(nicrf_src)")
  ENDIF

  indim = 0
  CALL define_var("Z_min_box","R",indim,idims, &
         "Z_min of bounding box", &
         "m","EQ","MACHINE_DESCRIPTION", &
         "R  Z_min_box")

  indim = 0
  CALL define_var("Z_min_lcfs","R",indim,idims, &
         "Z_min of last closed flux surface", &
         "m","EQ","STATE_DATA", &
         "R  Z_min_lcfs")

  IF(ALLOCATED(ss%Z_ripple)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nz_rip",idims(indim))
     CALL define_var("Z_ripple","R",indim,idims, &
            "Z grid for TF ripple field(s)", &
            "m","RIPPLE","SIMULATION_INIT", &
            "G  Z_ripple(nZ_rip)")
  ENDIF

  IF(ALLOCATED(ss%Z_surfMax)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("Z_surfMax","R",indim,idims, &
            "max Z on flux surface", &
            "m","EQ","STATE_PROFILES", &
            "R|pclin  Z_surfMax(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%Z_surfMin)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("Z_surfMin","R",indim,idims, &
            "min Z on flux surface", &
            "m","EQ","STATE_PROFILES", &
            "R|pclin  Z_surfMin(nrho_eq_geo)")
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

END SUBROUTINE plasma_state_store_def_9


SUBROUTINE plasma_state_store_9(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%velpphi_tglf)) THEN
     status = nf90_inq_varid(icdf,"velpphi_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%velpphi_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velpphi_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_anom)) THEN
     status = nf90_inq_varid(icdf,"velte_anom",varid)
     status = nf90_put_var(icdf,varid,ss%velte_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velte_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_glf)) THEN
     status = nf90_inq_varid(icdf,"velte_glf",varid)
     status = nf90_put_var(icdf,varid,ss%velte_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velte_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"velte_gtcneo",varid)
     status = nf90_put_var(icdf,varid,ss%velte_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velte_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_mmm71)) THEN
     status = nf90_inq_varid(icdf,"velte_mmm71",varid)
     status = nf90_put_var(icdf,varid,ss%velte_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velte_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_nclass)) THEN
     status = nf90_inq_varid(icdf,"velte_nclass",varid)
     status = nf90_put_var(icdf,varid,ss%velte_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velte_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_neo)) THEN
     status = nf90_inq_varid(icdf,"velte_neo",varid)
     status = nf90_put_var(icdf,varid,ss%velte_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velte_neo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_paleo)) THEN
     status = nf90_inq_varid(icdf,"velte_paleo",varid)
     status = nf90_put_var(icdf,varid,ss%velte_paleo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velte_paleo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_tglf)) THEN
     status = nf90_inq_varid(icdf,"velte_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%velte_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velte_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_anom)) THEN
     status = nf90_inq_varid(icdf,"velti_anom",varid)
     status = nf90_put_var(icdf,varid,ss%velti_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velti_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_glf)) THEN
     status = nf90_inq_varid(icdf,"velti_glf",varid)
     status = nf90_put_var(icdf,varid,ss%velti_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velti_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"velti_gtcneo",varid)
     status = nf90_put_var(icdf,varid,ss%velti_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velti_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_mmm71)) THEN
     status = nf90_inq_varid(icdf,"velti_mmm71",varid)
     status = nf90_put_var(icdf,varid,ss%velti_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velti_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_nclass)) THEN
     status = nf90_inq_varid(icdf,"velti_nclass",varid)
     status = nf90_put_var(icdf,varid,ss%velti_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velti_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_neo)) THEN
     status = nf90_inq_varid(icdf,"velti_neo",varid)
     status = nf90_put_var(icdf,varid,ss%velti_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velti_neo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_tglf)) THEN
     status = nf90_inq_varid(icdf,"velti_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%velti_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: velti_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vie_trans)) THEN
     status = nf90_inq_varid(icdf,"vie_trans",varid)
     status = nf90_put_var(icdf,varid,ss%vie_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vie_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vie_transcat)) THEN
     status = nf90_inq_varid(icdf,"vie_transcat",varid)
     status = nf90_put_var(icdf,varid,ss%vie_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vie_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vmo_trans)) THEN
     status = nf90_inq_varid(icdf,"vmo_trans",varid)
     status = nf90_put_var(icdf,varid,ss%vmo_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vmo_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vmo_transcat)) THEN
     status = nf90_inq_varid(icdf,"vmo_transcat",varid)
     status = nf90_put_var(icdf,varid,ss%vmo_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vmo_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vn_trans)) THEN
     status = nf90_inq_varid(icdf,"vn_trans",varid)
     status = nf90_put_var(icdf,varid,ss%vn_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vn_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vn_transcat)) THEN
     status = nf90_inq_varid(icdf,"vn_transcat",varid)
     status = nf90_put_var(icdf,varid,ss%vn_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vn_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vol)) THEN
     status = nf90_inq_varid(icdf,"vol",varid)
     status = nf90_put_var(icdf,varid,ss%vol)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vol"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vphi0_av)) THEN
     status = nf90_inq_varid(icdf,"vphi0_av",varid)
     status = nf90_put_var(icdf,varid,ss%vphi0_av)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vphi0_av"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vpol_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%vpol_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: vpol_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%vpol_inmp)) THEN
     status = nf90_inq_varid(icdf,"vpol_inmp",varid)
     status = nf90_put_var(icdf,varid,ss%vpol_inmp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vpol_inmp"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vpol_is_input)) THEN
     status = nf90_inq_varid(icdf,"vpol_is_input",varid)
     status = nf90_put_var(icdf,varid,ss%vpol_is_input)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vpol_is_input"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vpol_omp)) THEN
     status = nf90_inq_varid(icdf,"vpol_omp",varid)
     status = nf90_put_var(icdf,varid,ss%vpol_omp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vpol_omp"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%VRep_coil_count)) THEN
     status = nf90_inq_varid(icdf,"VRep_coil_count",varid)
     status = nf90_put_var(icdf,varid,ss%VRep_coil_count)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: VRep_coil_count"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Vrep_coil_spacing)) THEN
     status = nf90_inq_varid(icdf,"Vrep_coil_spacing",varid)
     status = nf90_put_var(icdf,varid,ss%Vrep_coil_spacing)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Vrep_coil_spacing"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Vsize_coil)) THEN
     status = nf90_inq_varid(icdf,"Vsize_coil",varid)
     status = nf90_put_var(icdf,varid,ss%Vsize_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Vsize_coil"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vsur",varid)
  status = nf90_put_var(icdf,varid,ss%vsur)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: vsur"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"VSUR_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%VSUR_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: VSUR_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"vtor_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%vtor_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: vtor_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%vtor_inmp)) THEN
     status = nf90_inq_varid(icdf,"vtor_inmp",varid)
     status = nf90_put_var(icdf,varid,ss%vtor_inmp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vtor_inmp"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vtor_is_input)) THEN
     status = nf90_inq_varid(icdf,"vtor_is_input",varid)
     status = nf90_put_var(icdf,varid,ss%vtor_is_input)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vtor_is_input"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vtor_omp)) THEN
     status = nf90_inq_varid(icdf,"vtor_omp",varid)
     status = nf90_put_var(icdf,varid,ss%vtor_omp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: vtor_omp"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%V_loop)) THEN
     status = nf90_inq_varid(icdf,"V_loop",varid)
     status = nf90_put_var(icdf,varid,ss%V_loop)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: V_loop"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%v_pars)) THEN
     status = nf90_inq_varid(icdf,"v_pars",varid)
     status = nf90_put_var(icdf,varid,ss%v_pars)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: v_pars"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%v_pers)) THEN
     status = nf90_inq_varid(icdf,"v_pers",varid)
     status = nf90_put_var(icdf,varid,ss%v_pers)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: v_pers"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%wt_nphi)) THEN
     status = nf90_inq_varid(icdf,"wt_nphi",varid)
     status = nf90_put_var(icdf,varid,ss%wt_nphi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: wt_nphi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%wt_nphi_abs)) THEN
     status = nf90_inq_varid(icdf,"wt_nphi_abs",varid)
     status = nf90_put_var(icdf,varid,ss%wt_nphi_abs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: wt_nphi_abs"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%wt_nphi_ext)) THEN
     status = nf90_inq_varid(icdf,"wt_nphi_ext",varid)
     status = nf90_put_var(icdf,varid,ss%wt_nphi_ext)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: wt_nphi_ext"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%xRjcos_momeq)) THEN
     status = nf90_inq_varid(icdf,"xRjcos_momeq",varid)
     status = nf90_put_var(icdf,varid,ss%xRjcos_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: xRjcos_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%xRjsin_momeq)) THEN
     status = nf90_inq_varid(icdf,"xRjsin_momeq",varid)
     status = nf90_put_var(icdf,varid,ss%xRjsin_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: xRjsin_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%xs)) THEN
     status = nf90_inq_varid(icdf,"xs",varid)
     status = nf90_put_var(icdf,varid,ss%xs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: xs"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%xs_name)) THEN
     status = nf90_inq_varid(icdf,"xs_name",varid)
     status = nf90_put_var(icdf,varid,ss%xs_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: xs_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%xZjcos_momeq)) THEN
     status = nf90_inq_varid(icdf,"xZjcos_momeq",varid)
     status = nf90_put_var(icdf,varid,ss%xZjcos_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: xZjcos_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%xZjsin_momeq)) THEN
     status = nf90_inq_varid(icdf,"xZjsin_momeq",varid)
     status = nf90_put_var(icdf,varid,ss%xZjsin_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: xZjsin_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z0max",varid)
  status = nf90_put_var(icdf,varid,ss%Z0max)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: Z0max"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%Z0_momeq)) THEN
     status = nf90_inq_varid(icdf,"Z0_momeq",varid)
     status = nf90_put_var(icdf,varid,ss%Z0_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Z0_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Zbap)) THEN
     status = nf90_inq_varid(icdf,"Zbap",varid)
     status = nf90_put_var(icdf,varid,ss%Zbap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Zbap"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Zbsc)) THEN
     status = nf90_inq_varid(icdf,"Zbsc",varid)
     status = nf90_put_var(icdf,varid,ss%Zbsc)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Zbsc"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Zeff)) THEN
     status = nf90_inq_varid(icdf,"Zeff",varid)
     status = nf90_put_var(icdf,varid,ss%Zeff)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Zeff"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ZEFF_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%ZEFF_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: ZEFF_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%Zeff_fi)) THEN
     status = nf90_inq_varid(icdf,"Zeff_fi",varid)
     status = nf90_put_var(icdf,varid,ss%Zeff_fi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Zeff_fi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Zeff_th)) THEN
     status = nf90_inq_varid(icdf,"Zeff_th",varid)
     status = nf90_put_var(icdf,varid,ss%Zeff_th)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Zeff_th"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zimp1",varid)
  status = nf90_put_var(icdf,varid,ss%Zimp1)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: Zimp1"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"Zimp2",varid)
  status = nf90_put_var(icdf,varid,ss%Zimp2)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: Zimp2"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%zlim)) THEN
     status = nf90_inq_varid(icdf,"zlim",varid)
     status = nf90_put_var(icdf,varid,ss%zlim)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: zlim"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Zloc_coil)) THEN
     status = nf90_inq_varid(icdf,"Zloc_coil",varid)
     status = nf90_put_var(icdf,varid,ss%Zloc_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Zloc_coil"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_ant)) THEN
     status = nf90_inq_varid(icdf,"Z_ant",varid)
     status = nf90_put_var(icdf,varid,ss%Z_ant)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Z_ant"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_antgeo)) THEN
     status = nf90_inq_varid(icdf,"Z_antgeo",varid)
     status = nf90_put_var(icdf,varid,ss%Z_antgeo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Z_antgeo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_axis",varid)
  status = nf90_put_var(icdf,varid,ss%Z_axis)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: Z_axis"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%Z_EC_launch)) THEN
     status = nf90_inq_varid(icdf,"Z_EC_launch",varid)
     status = nf90_put_var(icdf,varid,ss%Z_EC_launch)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Z_EC_launch"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_geo)) THEN
     status = nf90_inq_varid(icdf,"Z_geo",varid)
     status = nf90_put_var(icdf,varid,ss%Z_geo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Z_geo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_grid)) THEN
     status = nf90_inq_varid(icdf,"Z_grid",varid)
     status = nf90_put_var(icdf,varid,ss%Z_grid)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Z_grid"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_max_box",varid)
  status = nf90_put_var(icdf,varid,ss%Z_max_box)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: Z_max_box"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"Z_max_lcfs",varid)
  status = nf90_put_var(icdf,varid,ss%Z_max_lcfs)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: Z_max_lcfs"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%Z_midp)) THEN
     status = nf90_inq_varid(icdf,"Z_midp",varid)
     status = nf90_put_var(icdf,varid,ss%Z_midp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Z_midp"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_mid_ant)) THEN
     status = nf90_inq_varid(icdf,"Z_mid_ant",varid)
     status = nf90_put_var(icdf,varid,ss%Z_mid_ant)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Z_mid_ant"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_min_box",varid)
  status = nf90_put_var(icdf,varid,ss%Z_min_box)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: Z_min_box"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"Z_min_lcfs",varid)
  status = nf90_put_var(icdf,varid,ss%Z_min_lcfs)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: Z_min_lcfs"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%Z_ripple)) THEN
     status = nf90_inq_varid(icdf,"Z_ripple",varid)
     status = nf90_put_var(icdf,varid,ss%Z_ripple)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Z_ripple"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_surfMax)) THEN
     status = nf90_inq_varid(icdf,"Z_surfMax",varid)
     status = nf90_put_var(icdf,varid,ss%Z_surfMax)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Z_surfMax"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_surfMin)) THEN
     status = nf90_inq_varid(icdf,"Z_surfMin",varid)
     status = nf90_put_var(icdf,varid,ss%Z_surfMin)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Z_surfMin"
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

END SUBROUTINE plasma_state_store_9
