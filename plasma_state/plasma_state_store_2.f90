! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_store_def_2(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%D_einj_standard)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_n_d_einj_standard",idims(indim))
     CALL define_var("D_einj_standard","R",indim,idims, &
            "D beam table energies", &
            "keV","NBI","MACHINE_DESCRIPTION", &
            "R|units=keV D_einj_standard(n_D_einj_standard)")
  ENDIF

  IF(ALLOCATED(ss%D_ffull_standard)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_n_d_einj_standard",idims(indim))
     CALL define_var("D_ffull_standard","R",indim,idims, &
            "D beam full energy fractions", &
            "-","NBI","MACHINE_DESCRIPTION", &
            "R|units=- D_ffull_standard(n_D_einj_standard)")
  ENDIF

  IF(ALLOCATED(ss%D_fhalf_standard)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_n_d_einj_standard",idims(indim))
     CALL define_var("D_fhalf_standard","R",indim,idims, &
            "D beam half energy fractions", &
            "-","NBI","MACHINE_DESCRIPTION", &
            "R|units=- D_fhalf_standard(n_D_einj_standard)")
  ENDIF

  IF(ALLOCATED(ss%e0_av)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("e0_av","R",indim,idims, &
            'average energy of neutral sources, "(3/2)*T0"', &
            "KeV","GAS","STATE_DATA", &
            "R|units=KeV   e0_av(ngsc0)")
  ENDIF

  IF(ALLOCATED(ss%ecrf_src_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("ecrf_src_name","N",indim,idims, &
            "number & name of ECRF sources", &
            "__NONE","EC","MACHINE_DESCRIPTION", &
            "L|ecrf_source   ecrf_src_name(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%EC_Beam_Elongation)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("EC_Beam_Elongation","R",indim,idims, &
            "EC beam ellipticity", &
            "-","EC","MACHINE_DESCRIPTION", &
            "R|units=-   EC_Beam_Elongation(necrf_src)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("EC_Code_Info","C*80",indim,idims, &
         "Information: code implementing EC component", &
         "__NONE","EC","SIMULATION_INIT", &
         "C*80   EC_Code_Info")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("EC_Data_Info","C*80",indim,idims, &
         "information on source of ECRF power data", &
         "__NONE","EC","SIMULATION_INIT", &
         "C*80   EC_Data_Info")

  IF(ALLOCATED(ss%EC_Half_Power_Angle)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("EC_Half_Power_Angle","R",indim,idims, &
            "Divergence of EC beam", &
            "degrees","EC","MACHINE_DESCRIPTION", &
            "R|units=degrees  EC_Half_Power_Angle(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%EC_Omode_Fraction)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("EC_Omode_Fraction","R",indim,idims, &
            "O-mode fraction of EC source", &
            "-","EC","SHOT_CONFIGURATION", &
            "R|units=-   EC_Omode_Fraction(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%EC_phi_aim)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("EC_phi_aim","R",indim,idims, &
            "toroidal aiming angle", &
            "degrees","EC","SHOT_CONFIGURATION", &
            "R|units=degrees  EC_phi_aim(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_CURV_HOR)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("EC_PRINC_CURV_HOR","R",indim,idims, &
            "principal curvature in horizontal direction", &
            "m","EC","MACHINE_DESCRIPTION", &
            "R|units=m  EC_PRINC_CURV_HOR(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_CURV_VERT)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("EC_PRINC_CURV_VERT","R",indim,idims, &
            "principal curvature in vertical direction", &
            "m","EC","MACHINE_DESCRIPTION", &
            "R|units=m  EC_PRINC_CURV_VERT(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("EC_PRINC_WIDTH_HOR","R",indim,idims, &
            "principal beam width in horizontal direction", &
            "m","EC","MACHINE_DESCRIPTION", &
            "R|units=m  EC_PRINC_WIDTH_HOR(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("EC_PRINC_WIDTH_VERT","R",indim,idims, &
            "principal beam width in vertical direction", &
            "m","EC","MACHINE_DESCRIPTION", &
            "R|units=m  EC_PRINC_WIDTH_VERT(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%EC_theta_aim)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("EC_theta_aim","R",indim,idims, &
            "poloidal aiming angle", &
            "degrees","EC","SHOT_CONFIGURATION", &
            "R|units=degrees  EC_theta_aim(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%ee_mobility_factor)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("ee_mobility_factor","R",indim,idims, &
            "chi_e/chi_i", &
            "-","ANOM","STATE_PROFILES", &
            "R|units=-|Hermite  ee_mobility_factor(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%Einj_max)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("Einj_max","R",indim,idims, &
            "maximum injection energy", &
            "keV","NBI","MACHINE_DESCRIPTION", &
            "R|units=keV       Einj_max(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%Einj_min)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("Einj_min","R",indim,idims, &
            "minimum injection energy", &
            "keV","NBI","MACHINE_DESCRIPTION", &
            "R|units=keV       Einj_min(nbeam)")
  ENDIF

  indim = 0
  CALL define_var("eldot","R",indim,idims, &
         "[d(Philim)/dt]/[2*Philim]", &
         "1/sec","PLASMA","STATE_DATA", &
         "R|units=1/sec  eldot")

  IF(ALLOCATED(ss%elong)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("elong","R",indim,idims, &
            "elongation (b/a)", &
            "-","EQ","STATE_PROFILES", &
            "R|pclin  elong(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%eperp_beami)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("eperp_beami","R",indim,idims, &
            "beam species <Eperp>, lab frame", &
            "keV","NBI","STATE_PROFILES", &
            "R|units=keV|alias=eperp_|step*nbeami eperp_beami(~nrho_nbi,nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%eperp_fusi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_fus",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("eperp_fusi","R",indim,idims, &
            "fusion ion <Eperp>, lab frame", &
            "keV","FUS","STATE_PROFILES", &
            "R|units=keV|alias=eperp_|step*nfusi eperp_fusi(~nrho_fus,nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%eperp_mini)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",idims(indim))
     CALL define_var("eperp_mini","R",indim,idims, &
            "minority species <Eperp>, lab frame", &
            "keV","IC","STATE_PROFILES", &
            "R|units=keV|alias=eperp_|step*nmini eperp_mini(~nrho_icrf,nspec_rfmin)")
  ENDIF

  IF(ALLOCATED(ss%eperp_rw)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_rw",idims(indim))
     CALL define_var("eperp_rw","R",indim,idims, &
            "runaway electron <Eperp>", &
            "keV","RUNAWAY","STATE_PROFILES", &
            "R|units=keV|step*nrw  eperp_rw(~nrho_rw)")
  ENDIF

  IF(ALLOCATED(ss%epll_beami)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("epll_beami","R",indim,idims, &
            "beam species <Epll>, lab frame", &
            "keV","NBI","STATE_PROFILES", &
            "R|units=keV|alias=epll_|step*nbeami  epll_beami(~nrho_nbi,nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%epll_fusi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_fus",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("epll_fusi","R",indim,idims, &
            "fusion ion <Epll>, lab frame", &
            "keV","FUS","STATE_PROFILES", &
            "R|units=keV|alias=epll_|step*nfusi  epll_fusi(~nrho_fus,nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%epll_mini)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",idims(indim))
     CALL define_var("epll_mini","R",indim,idims, &
            "minority species <Epll>, lab frame", &
            "keV","IC","STATE_PROFILES", &
            "R|units=keV|alias=epll_|step*nmini  epll_mini(~nrho_icrf,nspec_rfmin)")
  ENDIF

  IF(ALLOCATED(ss%epll_rw)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_rw",idims(indim))
     CALL define_var("epll_rw","R",indim,idims, &
            "runaway electron <Epll>", &
            "keV","RUNAWAY","STATE_PROFILES", &
            "R|units=keV|step*nrw  epll_rw(~nrho_rw)")
  ENDIF

  IF(ALLOCATED(ss%Epot)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("Epot","R",indim,idims, &
            "radial electrostatic potential", &
            "keV","PLASMA","STATE_PROFILES", &
            "R|units=keV|pclin  Epot(nrho)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_256",idims(1))
  CALL define_var("eqdsk_file","F",indim,idims, &
         "EFIT G-eqdsk file: psi(R,Z), g(psi), etc.", &
         "__NONE","EQ","STATE_DATA", &
         "F  eqdsk_file")

  IF(ALLOCATED(ss%eqmom_num)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_neqmom",idims(indim))
     CALL define_var("eqmom_num","N",indim,idims, &
            "Equilibrium moments", &
            "__NONE","EQ","SIMULATION_INIT", &
            "L|EQ_moments  eqmom_num(neqmom)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("EQ_Code_Info","C*80",indim,idims, &
         "Information: code implementing EQ component", &
         "__NONE","EQ","SIMULATION_INIT", &
         "C*80   EQ_Code_Info")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("EQ_Data_Info","C*80",indim,idims, &
         "Information: source of EQ input data", &
         "__NONE","EQ","SIMULATION_INIT", &
         "C*80   EQ_Data_Info")

  IF(ALLOCATED(ss%eta_parallel)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("eta_parallel","R",indim,idims, &
            "parallel electrical resistivity", &
            "ohm*m","PLASMA","STATE_PROFILES", &
            "R|units=ohm*m|Hermite eta_parallel(nrho)")
  ENDIF

  IF(ALLOCATED(ss%E_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom",idims(indim))
     CALL define_var("E_anom","R",indim,idims, &
            "(optional) energy grid (1d profiles)", &
            "keV","ANOM","SIMULATION_INIT", &
            "G  E_anom(nefi_anom)")
  ENDIF

  IF(ALLOCATED(ss%E_anom2)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom2",idims(indim))
     CALL define_var("E_anom2","R",indim,idims, &
            "energy grid (2d anomalous transport profiles)", &
            "keV","ANOM","SIMULATION_INIT", &
            "G  E_anom2(nefi_anom2)")
  ENDIF

  IF(ALLOCATED(ss%fidiff_bpass_co)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("fidiff_bpass_co","R",indim,idims, &
            "fusion ion diffusivity: co barely passing orbits", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  fidiff_bpass_co(nefi_anom2,nrho_anom2,nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%fidiff_bpass_ctr)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("fidiff_bpass_ctr","R",indim,idims, &
            "fusion ion diffusivity: ctr barely passing orbits", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  fidiff_bpass_ctr(nefi_anom2,nrho_anom2,nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%fidiff_btrap)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("fidiff_btrap","R",indim,idims, &
            "fusion ion diffusivity: barely trapped orbits", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  fidiff_btrap(nefi_anom2,nrho_anom2,nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%fidiff_dpass_co)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("fidiff_dpass_co","R",indim,idims, &
            "fusion ion diffusivity: co passing |vpll/v| -> 1", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  fidiff_dpass_co(nefi_anom2,nrho_anom2,nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%fidiff_dpass_ctr)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("fidiff_dpass_ctr","R",indim,idims, &
            "fusion ion diffusivity: ctr passing |vpll/v| -> 1", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  fidiff_dpass_ctr(nefi_anom2,nrho_anom2,nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%fidiff_dtrap)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("fidiff_dtrap","R",indim,idims, &
            "fusion ion diffusivity: deeply trapped orbits", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  fidiff_dtrap(nefi_anom2,nrho_anom2,nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%Fi_depletion)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("Fi_depletion","R",indim,idims, &
            "fast ion depletion: sum[nj*Zj/ne]", &
            "-","PLASMA","STATE_PROFILES", &
            "R|units=-|step*ns(:,ps_elec_index) Fi_depletion(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%fracmin)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",idims(indim))
     CALL define_var("fracmin","R",indim,idims, &
            'minority density fraction if kdens_rfmin="fraction"', &
            "-","IC","SIMULATION_INIT", &
            "R|units=- fracmin(nspec_rfmin)")
  ENDIF

  IF(ALLOCATED(ss%fracpslh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_nspectrumlh_src",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("fracpslh","R",indim,idims, &
            "fraction of power on each peak (the sum over all peaks must be 1)", &
            "-","LH","SHOT_CONFIGURATION", &
            "R|units=- fracpslh(nspectrumlh_src(),nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%frac_full)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("frac_full","R",indim,idims, &
            "fraction of beam current at full voltage", &
            "-","NBI","STATE_DATA", &
            "R|units=-  frac_full(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%frac_half)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("frac_half","R",indim,idims, &
            "fraction of beam current at half voltage", &
            "-","NBI","STATE_DATA", &
            "R|units=-  frac_half(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%freq_drbm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("freq_drbm","R",indim,idims, &
            "frequency from DRBM model", &
            "1/sec","ANOM","STATE_PROFILES", &
            "R|units=1/sec|pclin freq_drbm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%freq_ec)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_necrf_src",idims(indim))
     CALL define_var("freq_ec","R",indim,idims, &
            "frequency of EC source", &
            "Hz","EC","SHOT_CONFIGURATION", &
            "R|units=Hz freq_ec(necrf_src)")
  ENDIF

  IF(ALLOCATED(ss%freq_etg)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("freq_etg","R",indim,idims, &
            "frequency from ETG model", &
            "1/sec","ANOM","STATE_PROFILES", &
            "R|units=1/sec|pclin freq_etg(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%freq_glf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("freq_glf","R",indim,idims, &
            "frequency from GLF23 model", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin freq_glf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%freq_ic)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("freq_ic","R",indim,idims, &
            "frequency on each ICRF source", &
            "Hz","IC","SHOT_CONFIGURATION", &
            "R|units=Hz freq_ic(nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%freq_lh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("freq_lh","R",indim,idims, &
            "frequency on each LHRF source", &
            "Hz","LH","SHOT_CONFIGURATION", &
            "R|units=Hz freq_lh(nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%freq_max_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("freq_max_tglf","R",indim,idims, &
            "frequency corresponding maximum growthrate from TGLF model", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin freq_max_tglf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%freq_mmm71)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_mmm",idims(indim))
     CALL define_var("freq_mmm71","R",indim,idims, &
            "frequency from mmm71 model", &
            "1/sec","ANOM","STATE_PROFILES", &
            "R|units=1/sec|pclin freq_mmm71(nrho_anomth,nky_mmm)")
  ENDIF

  IF(ALLOCATED(ss%freq_mtm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("freq_mtm","R",indim,idims, &
            "frequency from MTM model", &
            "1/sec","ANOM","STATE_PROFILES", &
            "R|units=1/sec|pclin freq_mtm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%freq_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("freq_tglf","R",indim,idims, &
            "frequency from TGLF model", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin freq_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("FUS_Code_Info","C*80",indim,idims, &
         "Information: code implementing FUS component", &
         "__NONE","FUS","SIMULATION_INIT", &
         "C*80   FUS_Code_Info")

  IF(ALLOCATED(ss%gamma_lmhd)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_mmodes",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ndelta_w",idims(indim))
     CALL define_var("gamma_lmhd","R",indim,idims, &
            "ideal growth rate", &
            "-","LMHD","STATE_DATA", &
            "R|units=-  gamma_lmhd(mmodes,ndelta_W)")
  ENDIF

  IF(ALLOCATED(ss%gamma_nc)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("gamma_nc","R",indim,idims, &
            "NC gamma", &
            "m^-1","EQ","STATE_PROFILES", &
            "R|units=m^-1|pclin   gamma_nc(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%gas_atom)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ngsc0",idims(indim))
     CALL define_var("gas_atom","N",indim,idims, &
            "name of species introduced by each gas source", &
            "__NONE","GAS","SHOT_CONFIGURATION", &
            "N  gas_atom(ngsc0)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("GAS_Code_Info","C*80",indim,idims, &
         "Information: code implementing GAS component", &
         "__NONE","GAS","SIMULATION_INIT", &
         "C*80   GAS_Code_Info")

  IF(ALLOCATED(ss%gb1)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("gb1","R",indim,idims, &
            "<|B|>", &
            "T","EQ","STATE_PROFILES", &
            "R|units=T|pclin      gb1(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%gb2)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("gb2","R",indim,idims, &
            "<B^2>", &
            "T^2","EQ","STATE_PROFILES", &
            "R|units=T^2|pclin    gb2(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%gb2i)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("gb2i","R",indim,idims, &
            "<1/B^2>", &
            "T^-2","EQ","STATE_PROFILES", &
            "R|units=T^-2|pclin   gb2i(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%gbr2)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("gbr2","R",indim,idims, &
            "<|B|*R^2>", &
            "T*m^2","EQ","STATE_PROFILES", &
            "R|units=T*m^2|pclin  gbr2(nrho_eq_geo)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_256",idims(1))
  CALL define_var("geometry","F",indim,idims, &
         "path to file(s) vac. vessel & coil descr.", &
         "__NONE","EQ","MACHINE_DESCRIPTION", &
         "F  geometry")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("Global_label","C*80",indim,idims, &
         "Run Label", &
         "__NONE","PLASMA","SIMULATION_INIT", &
         "C*80   Global_label")

  IF(ALLOCATED(ss%gncfb2h)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("gncfb2h","R",indim,idims, &
            "<(B^-2)*((1-H)^0.5-(1/3)*(1-H)^1.5)>; H=B/Bmax", &
            "T^-2","EQ","STATE_PROFILES", &
            "R|units=T^-2|pclin  gncfb2h(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%gncfh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("gncfh","R",indim,idims, &
            "<H^-2*(1-SQRT(1-H)*(1+H/2))>; H=B/Bmax", &
            "-","EQ","STATE_PROFILES", &
            "R|units=-|pclin  gncfh(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%gr1)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("gr1","R",indim,idims, &
            "<R>", &
            "m","EQ","STATE_PROFILES", &
            "R|units=m|pclin   gr1(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%gr2)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("gr2","R",indim,idims, &
            "<R^2>", &
            "m^2","EQ","STATE_PROFILES", &
            "R|units=m^2|pclin gr2(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%gr2i)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("gr2i","R",indim,idims, &
            "<R^-2>", &
            "m^-2","EQ","STATE_PROFILES", &
            "R|units=m^-2|pclin gr2i(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%gr2rho2)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("gr2rho2","R",indim,idims, &
            "<R^2*|grad(rho)|^2>", &
            "-","EQ","STATE_PROFILES", &
            "R|units=-|pclin gr2rho2(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%gr3i)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("gr3i","R",indim,idims, &
            "<R^-3>", &
            "m^-3","EQ","STATE_PROFILES", &
            "R|units=m^-3|pclin gr3i(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%grho1)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("grho1","R",indim,idims, &
            "<|grad(rho)|>", &
            "m^-1","EQ","STATE_PROFILES", &
            "R|units=m^-1|pclin grho1(nrho_eq_geo)")
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

END SUBROUTINE plasma_state_store_def_2


SUBROUTINE plasma_state_store_2(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%D_einj_standard)) THEN
     status = nf90_inq_varid(icdf,"D_einj_standard",varid)
     status = nf90_put_var(icdf,varid,ss%D_einj_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: D_einj_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%D_ffull_standard)) THEN
     status = nf90_inq_varid(icdf,"D_ffull_standard",varid)
     status = nf90_put_var(icdf,varid,ss%D_ffull_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: D_ffull_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%D_fhalf_standard)) THEN
     status = nf90_inq_varid(icdf,"D_fhalf_standard",varid)
     status = nf90_put_var(icdf,varid,ss%D_fhalf_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: D_fhalf_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%e0_av)) THEN
     status = nf90_inq_varid(icdf,"e0_av",varid)
     status = nf90_put_var(icdf,varid,ss%e0_av)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: e0_av"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ecrf_src_name)) THEN
     status = nf90_inq_varid(icdf,"ecrf_src_name",varid)
     status = nf90_put_var(icdf,varid,ss%ecrf_src_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ecrf_src_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_Beam_Elongation)) THEN
     status = nf90_inq_varid(icdf,"EC_Beam_Elongation",varid)
     status = nf90_put_var(icdf,varid,ss%EC_Beam_Elongation)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: EC_Beam_Elongation"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Code_Info",varid)
  status = nf90_put_var(icdf,varid,ss%EC_Code_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: EC_Code_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%EC_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: EC_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%EC_Half_Power_Angle)) THEN
     status = nf90_inq_varid(icdf,"EC_Half_Power_Angle",varid)
     status = nf90_put_var(icdf,varid,ss%EC_Half_Power_Angle)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: EC_Half_Power_Angle"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_Omode_Fraction)) THEN
     status = nf90_inq_varid(icdf,"EC_Omode_Fraction",varid)
     status = nf90_put_var(icdf,varid,ss%EC_Omode_Fraction)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: EC_Omode_Fraction"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_phi_aim)) THEN
     status = nf90_inq_varid(icdf,"EC_phi_aim",varid)
     status = nf90_put_var(icdf,varid,ss%EC_phi_aim)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: EC_phi_aim"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_CURV_HOR)) THEN
     status = nf90_inq_varid(icdf,"EC_PRINC_CURV_HOR",varid)
     status = nf90_put_var(icdf,varid,ss%EC_PRINC_CURV_HOR)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: EC_PRINC_CURV_HOR"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_CURV_VERT)) THEN
     status = nf90_inq_varid(icdf,"EC_PRINC_CURV_VERT",varid)
     status = nf90_put_var(icdf,varid,ss%EC_PRINC_CURV_VERT)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: EC_PRINC_CURV_VERT"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR)) THEN
     status = nf90_inq_varid(icdf,"EC_PRINC_WIDTH_HOR",varid)
     status = nf90_put_var(icdf,varid,ss%EC_PRINC_WIDTH_HOR)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: EC_PRINC_WIDTH_HOR"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT)) THEN
     status = nf90_inq_varid(icdf,"EC_PRINC_WIDTH_VERT",varid)
     status = nf90_put_var(icdf,varid,ss%EC_PRINC_WIDTH_VERT)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: EC_PRINC_WIDTH_VERT"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_theta_aim)) THEN
     status = nf90_inq_varid(icdf,"EC_theta_aim",varid)
     status = nf90_put_var(icdf,varid,ss%EC_theta_aim)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: EC_theta_aim"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ee_mobility_factor)) THEN
     status = nf90_inq_varid(icdf,"ee_mobility_factor",varid)
     status = nf90_put_var(icdf,varid,ss%ee_mobility_factor)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ee_mobility_factor"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Einj_max)) THEN
     status = nf90_inq_varid(icdf,"Einj_max",varid)
     status = nf90_put_var(icdf,varid,ss%Einj_max)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Einj_max"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Einj_min)) THEN
     status = nf90_inq_varid(icdf,"Einj_min",varid)
     status = nf90_put_var(icdf,varid,ss%Einj_min)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Einj_min"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eldot",varid)
  status = nf90_put_var(icdf,varid,ss%eldot)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: eldot"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%elong)) THEN
     status = nf90_inq_varid(icdf,"elong",varid)
     status = nf90_put_var(icdf,varid,ss%elong)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: elong"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%eperp_beami)) THEN
     status = nf90_inq_varid(icdf,"eperp_beami",varid)
     status = nf90_put_var(icdf,varid,ss%eperp_beami)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: eperp_beami"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%eperp_fusi)) THEN
     status = nf90_inq_varid(icdf,"eperp_fusi",varid)
     status = nf90_put_var(icdf,varid,ss%eperp_fusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: eperp_fusi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%eperp_mini)) THEN
     status = nf90_inq_varid(icdf,"eperp_mini",varid)
     status = nf90_put_var(icdf,varid,ss%eperp_mini)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: eperp_mini"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%eperp_rw)) THEN
     status = nf90_inq_varid(icdf,"eperp_rw",varid)
     status = nf90_put_var(icdf,varid,ss%eperp_rw)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: eperp_rw"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%epll_beami)) THEN
     status = nf90_inq_varid(icdf,"epll_beami",varid)
     status = nf90_put_var(icdf,varid,ss%epll_beami)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: epll_beami"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%epll_fusi)) THEN
     status = nf90_inq_varid(icdf,"epll_fusi",varid)
     status = nf90_put_var(icdf,varid,ss%epll_fusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: epll_fusi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%epll_mini)) THEN
     status = nf90_inq_varid(icdf,"epll_mini",varid)
     status = nf90_put_var(icdf,varid,ss%epll_mini)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: epll_mini"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%epll_rw)) THEN
     status = nf90_inq_varid(icdf,"epll_rw",varid)
     status = nf90_put_var(icdf,varid,ss%epll_rw)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: epll_rw"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Epot)) THEN
     status = nf90_inq_varid(icdf,"Epot",varid)
     status = nf90_put_var(icdf,varid,ss%Epot)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Epot"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eqdsk_file",varid)
  status = nf90_put_var(icdf,varid,ss%eqdsk_file)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: eqdsk_file"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%eqmom_num)) THEN
     status = nf90_inq_varid(icdf,"eqmom_num",varid)
     status = nf90_put_var(icdf,varid,ss%eqmom_num)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: eqmom_num"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EQ_Code_Info",varid)
  status = nf90_put_var(icdf,varid,ss%EQ_Code_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: EQ_Code_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"EQ_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%EQ_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: EQ_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%eta_parallel)) THEN
     status = nf90_inq_varid(icdf,"eta_parallel",varid)
     status = nf90_put_var(icdf,varid,ss%eta_parallel)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: eta_parallel"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%E_anom)) THEN
     status = nf90_inq_varid(icdf,"E_anom",varid)
     status = nf90_put_var(icdf,varid,ss%E_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: E_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%E_anom2)) THEN
     status = nf90_inq_varid(icdf,"E_anom2",varid)
     status = nf90_put_var(icdf,varid,ss%E_anom2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: E_anom2"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fidiff_bpass_co)) THEN
     status = nf90_inq_varid(icdf,"fidiff_bpass_co",varid)
     status = nf90_put_var(icdf,varid,ss%fidiff_bpass_co)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: fidiff_bpass_co"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fidiff_bpass_ctr)) THEN
     status = nf90_inq_varid(icdf,"fidiff_bpass_ctr",varid)
     status = nf90_put_var(icdf,varid,ss%fidiff_bpass_ctr)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: fidiff_bpass_ctr"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fidiff_btrap)) THEN
     status = nf90_inq_varid(icdf,"fidiff_btrap",varid)
     status = nf90_put_var(icdf,varid,ss%fidiff_btrap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: fidiff_btrap"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fidiff_dpass_co)) THEN
     status = nf90_inq_varid(icdf,"fidiff_dpass_co",varid)
     status = nf90_put_var(icdf,varid,ss%fidiff_dpass_co)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: fidiff_dpass_co"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fidiff_dpass_ctr)) THEN
     status = nf90_inq_varid(icdf,"fidiff_dpass_ctr",varid)
     status = nf90_put_var(icdf,varid,ss%fidiff_dpass_ctr)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: fidiff_dpass_ctr"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fidiff_dtrap)) THEN
     status = nf90_inq_varid(icdf,"fidiff_dtrap",varid)
     status = nf90_put_var(icdf,varid,ss%fidiff_dtrap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: fidiff_dtrap"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Fi_depletion)) THEN
     status = nf90_inq_varid(icdf,"Fi_depletion",varid)
     status = nf90_put_var(icdf,varid,ss%Fi_depletion)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Fi_depletion"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fracmin)) THEN
     status = nf90_inq_varid(icdf,"fracmin",varid)
     status = nf90_put_var(icdf,varid,ss%fracmin)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: fracmin"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fracpslh)) THEN
     status = nf90_inq_varid(icdf,"fracpslh",varid)
     status = nf90_put_var(icdf,varid,ss%fracpslh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: fracpslh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%frac_full)) THEN
     status = nf90_inq_varid(icdf,"frac_full",varid)
     status = nf90_put_var(icdf,varid,ss%frac_full)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: frac_full"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%frac_half)) THEN
     status = nf90_inq_varid(icdf,"frac_half",varid)
     status = nf90_put_var(icdf,varid,ss%frac_half)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: frac_half"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_drbm)) THEN
     status = nf90_inq_varid(icdf,"freq_drbm",varid)
     status = nf90_put_var(icdf,varid,ss%freq_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: freq_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_ec)) THEN
     status = nf90_inq_varid(icdf,"freq_ec",varid)
     status = nf90_put_var(icdf,varid,ss%freq_ec)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: freq_ec"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_etg)) THEN
     status = nf90_inq_varid(icdf,"freq_etg",varid)
     status = nf90_put_var(icdf,varid,ss%freq_etg)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: freq_etg"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_glf)) THEN
     status = nf90_inq_varid(icdf,"freq_glf",varid)
     status = nf90_put_var(icdf,varid,ss%freq_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: freq_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_ic)) THEN
     status = nf90_inq_varid(icdf,"freq_ic",varid)
     status = nf90_put_var(icdf,varid,ss%freq_ic)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: freq_ic"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_lh)) THEN
     status = nf90_inq_varid(icdf,"freq_lh",varid)
     status = nf90_put_var(icdf,varid,ss%freq_lh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: freq_lh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_max_tglf)) THEN
     status = nf90_inq_varid(icdf,"freq_max_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%freq_max_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: freq_max_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_mmm71)) THEN
     status = nf90_inq_varid(icdf,"freq_mmm71",varid)
     status = nf90_put_var(icdf,varid,ss%freq_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: freq_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_mtm)) THEN
     status = nf90_inq_varid(icdf,"freq_mtm",varid)
     status = nf90_put_var(icdf,varid,ss%freq_mtm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: freq_mtm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_tglf)) THEN
     status = nf90_inq_varid(icdf,"freq_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%freq_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: freq_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"FUS_Code_Info",varid)
  status = nf90_put_var(icdf,varid,ss%FUS_Code_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: FUS_Code_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%gamma_lmhd)) THEN
     status = nf90_inq_varid(icdf,"gamma_lmhd",varid)
     status = nf90_put_var(icdf,varid,ss%gamma_lmhd)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gamma_lmhd"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gamma_nc)) THEN
     status = nf90_inq_varid(icdf,"gamma_nc",varid)
     status = nf90_put_var(icdf,varid,ss%gamma_nc)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gamma_nc"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gas_atom)) THEN
     status = nf90_inq_varid(icdf,"gas_atom",varid)
     status = nf90_put_var(icdf,varid,ss%gas_atom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gas_atom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"GAS_Code_Info",varid)
  status = nf90_put_var(icdf,varid,ss%GAS_Code_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: GAS_Code_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%gb1)) THEN
     status = nf90_inq_varid(icdf,"gb1",varid)
     status = nf90_put_var(icdf,varid,ss%gb1)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gb1"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gb2)) THEN
     status = nf90_inq_varid(icdf,"gb2",varid)
     status = nf90_put_var(icdf,varid,ss%gb2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gb2"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gb2i)) THEN
     status = nf90_inq_varid(icdf,"gb2i",varid)
     status = nf90_put_var(icdf,varid,ss%gb2i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gb2i"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gbr2)) THEN
     status = nf90_inq_varid(icdf,"gbr2",varid)
     status = nf90_put_var(icdf,varid,ss%gbr2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gbr2"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"geometry",varid)
  status = nf90_put_var(icdf,varid,ss%geometry)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: geometry"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"Global_label",varid)
  status = nf90_put_var(icdf,varid,ss%Global_label)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: Global_label"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%gncfb2h)) THEN
     status = nf90_inq_varid(icdf,"gncfb2h",varid)
     status = nf90_put_var(icdf,varid,ss%gncfb2h)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gncfb2h"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gncfh)) THEN
     status = nf90_inq_varid(icdf,"gncfh",varid)
     status = nf90_put_var(icdf,varid,ss%gncfh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gncfh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gr1)) THEN
     status = nf90_inq_varid(icdf,"gr1",varid)
     status = nf90_put_var(icdf,varid,ss%gr1)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gr1"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gr2)) THEN
     status = nf90_inq_varid(icdf,"gr2",varid)
     status = nf90_put_var(icdf,varid,ss%gr2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gr2"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gr2i)) THEN
     status = nf90_inq_varid(icdf,"gr2i",varid)
     status = nf90_put_var(icdf,varid,ss%gr2i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gr2i"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gr2rho2)) THEN
     status = nf90_inq_varid(icdf,"gr2rho2",varid)
     status = nf90_put_var(icdf,varid,ss%gr2rho2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gr2rho2"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gr3i)) THEN
     status = nf90_inq_varid(icdf,"gr3i",varid)
     status = nf90_put_var(icdf,varid,ss%gr3i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: gr3i"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%grho1)) THEN
     status = nf90_inq_varid(icdf,"grho1",varid)
     status = nf90_put_var(icdf,varid,ss%grho1)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: grho1"
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

END SUBROUTINE plasma_state_store_2
