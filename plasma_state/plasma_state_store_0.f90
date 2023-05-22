! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_store_def_0(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%alla_index)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_alla",idims(indim))
     CALL define_var("alla_index","I",indim,idims, &
            "species index in original lists", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "I alla_index(0:nspec_alla)")
  ENDIF

  IF(ALLOCATED(ss%ALLA_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_alla",idims(indim))
     CALL define_var("ALLA_name","N",indim,idims, &
            "all species abridged", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "S|specie  ALLA(0:nspec_alla)")
  ENDIF

  IF(ALLOCATED(ss%ALLA_type)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_alla",idims(indim))
     CALL define_var("ALLA_type","I",indim,idims, &
            "ALLA specie types", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "S|specie  ALLA(0:nspec_alla)")
  ENDIF

  IF(ALLOCATED(ss%all_index)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_all",idims(indim))
     CALL define_var("all_index","I",indim,idims, &
            "species index in original lists", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "I all_index(0:nspec_all)")
  ENDIF

  IF(ALLOCATED(ss%ALL_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_all",idims(indim))
     CALL define_var("ALL_name","N",indim,idims, &
            "all species", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "S|specie  ALL(0:nspec_all)")
  ENDIF

  IF(ALLOCATED(ss%ALL_type)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_all",idims(indim))
     CALL define_var("ALL_type","I",indim,idims, &
            "ALL specie types", &
            "__NONE","PLASMA","SIMULATION_INIT", &
            "S|specie  ALL(0:nspec_all)")
  ENDIF

  IF(ALLOCATED(ss%amaxlh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_nspectrumlh_src",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("amaxlh","R",indim,idims, &
            "width of the spectrum peak", &
            "-","LH","SHOT_CONFIGURATION", &
            "R|units=- amaxlh(nspectrumlh_src(),nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%aminlh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_nspectrumlh_src",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("aminlh","R",indim,idims, &
            "parallel mode number, center of the spectrum peak", &
            "-","LH","SHOT_CONFIGURATION", &
            "R|units=- aminlh(nspectrumlh_src(),nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%Ang_Href_coil)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncoils",idims(indim))
     CALL define_var("Ang_Href_coil","R",indim,idims, &
            "angle w.r.t. horizontal of coil base", &
            "degrees","EQ","MACHINE_DESCRIPTION", &
            "R|units=degrees Ang_Href_coil(ncoils)")
  ENDIF

  IF(ALLOCATED(ss%Ang_Vref_coil)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ncoils",idims(indim))
     CALL define_var("Ang_Vref_coil","R",indim,idims, &
            "angle w.r.t. vertical of coil sides", &
            "degrees","EQ","MACHINE_DESCRIPTION", &
            "R|units=degrees Ang_Vref_coil(ncoils)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("ANOM_Code_Info","C*80",indim,idims, &
         "Information: code implementing ANOM component", &
         "__NONE","ANOM","SIMULATION_INIT", &
         "C*80   ANOM_Code_Info")

  IF(ALLOCATED(ss%anom_evar)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom",idims(indim))
     CALL define_var("anom_evar","R",indim,idims, &
            "anomalous transport energy variation", &
            "-","ANOM","STATE_PROFILES", &
            "R|pclin|units=- anom_evar(nefi_anom)")
  ENDIF

  IF(ALLOCATED(ss%ant_model)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_256",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("ant_model","F",indim,idims, &
            "antenna model filenames (1 per antenna source)", &
            "__NONE","IC","MACHINE_DESCRIPTION", &
            "F ant_model(nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%ap2_halfheight)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("ap2_halfheight","R",indim,idims, &
            "aperture half-height", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m         ap2_halfheight(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%ap2_halfwidth)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("ap2_halfwidth","R",indim,idims, &
            "2nd aperture half-width", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m         ap2_halfwidth(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%ap2_horiz_offset)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("ap2_horiz_offset","R",indim,idims, &
            "horizontal offset, 2nd aperture", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m         ap2_horiz_offset(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%ap2_vert_offset)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("ap2_vert_offset","R",indim,idims, &
            "vertical offset, 2nd aperture", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m         ap2_vert_offset(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%apolmaxlh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_nspectrumlh_src",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("apolmaxlh","R",indim,idims, &
            "poloidal mode number right of lumps", &
            "-","LH","SHOT_CONFIGURATION", &
            "R|units=- apolmaxlh(nspectrumlh_src(),nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%apolminlh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_nspectrumlh_src",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("apolminlh","R",indim,idims, &
            "poloidal mode number left of lumps", &
            "-","LH","SHOT_CONFIGURATION", &
            "R|units=- apolminlh(nspectrumlh_src(),nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%ap_halfheight)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("ap_halfheight","R",indim,idims, &
            "aperture half-height", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m         ap_halfheight(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%ap_halfwidth)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("ap_halfwidth","R",indim,idims, &
            "aperture half-width", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m         ap_halfwidth(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%ap_horiz_offset)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("ap_horiz_offset","R",indim,idims, &
            "horizontal offset of aperture", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m         ap_horiz_offset(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%ap_vert_offset)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("ap_vert_offset","R",indim,idims, &
            "vertical offset of aperture", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m         ap_vert_offset(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%area)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq",idims(indim))
     CALL define_var("area","R",indim,idims, &
            "enclosed area", &
            "m^2","EQ","STATE_PROFILES", &
            "R|units=m^2|Spline_00 area(nrho_eq)")
  ENDIF

  IF(ALLOCATED(ss%atormaxlh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_nspectrumlh_src",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("atormaxlh","R",indim,idims, &
            "toroidal mode number right of lumps", &
            "-","LH","SHOT_CONFIGURATION", &
            "R|units=- atormaxlh(nspectrumlh_src(),nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%atorminlh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_nspectrumlh_src",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("atorminlh","R",indim,idims, &
            "toroidal mode number left of lumps", &
            "-","LH","SHOT_CONFIGURATION", &
            "R|units=- atorminlh(nspectrumlh_src(),nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%Balloon_code)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nballoon",idims(indim))
     CALL define_var("Balloon_code","N",indim,idims, &
            "name(s) of Ballooning code(s) used", &
            "__NONE","LMHD","SIMULATION_INIT", &
            "L|Balloon_codes  Balloon_code(nballoon)")
  ENDIF

  IF(ALLOCATED(ss%Balloon_mapper)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nballoon",idims(indim))
     CALL define_var("Balloon_mapper","N",indim,idims, &
            "mapper code used for each ballooning code", &
            "__NONE","LMHD","SIMULATION_INIT", &
            "N  Balloon_mapper(nballoon)")
  ENDIF

  IF(ALLOCATED(ss%Balloon_stable)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nballoon",idims(indim))
     CALL define_var("Balloon_stable","I",indim,idims, &
            "0: stable everywhere; =1: unstable somewhere", &
            "__NONE","LMHD","STATE_DATA", &
            "I   Balloon_stable(nballoon)")
  ENDIF

  IF(ALLOCATED(ss%Balloon_status)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nballoon",idims(indim))
     CALL define_var("Balloon_status","I",indim,idims, &
            "completion status: Ballooning codes", &
            "__NONE","LMHD","STATE_DATA", &
            "I   Balloon_status(nballoon)")
  ENDIF

  IF(ALLOCATED(ss%balstab)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_lmhd",idims(indim))
     CALL define_var("balstab","R",indim,idims, &
            "Ballooning/Mercier stability indicator", &
            "-","LMHD","STATE_PROFILES", &
            "R|units=-|pclin balstab(nrho_lmhd)")
  ENDIF

  IF(ALLOCATED(ss%beam_type)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("beam_type","N",indim,idims, &
            "beam type:", &
            "__NONE","NBI","MACHINE_DESCRIPTION", &
            "N     beam_type(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%bidiff_bpass_co)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("bidiff_bpass_co","R",indim,idims, &
            "beam ion diffusivity: co barely passing orbits", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  bidiff_bpass_co(nefi_anom2,nrho_anom2,nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%bidiff_bpass_ctr)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("bidiff_bpass_ctr","R",indim,idims, &
            "beam ion diffusivity: ctr barely passing orbits", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  bidiff_bpass_ctr(nefi_anom2,nrho_anom2,nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%bidiff_btrap)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("bidiff_btrap","R",indim,idims, &
            "beam ion diffusivity: barely trapped orbits", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  bidiff_btrap(nefi_anom2,nrho_anom2,nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%bidiff_dpass_co)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("bidiff_dpass_co","R",indim,idims, &
            "beam ion diffusivity: co passing |vpll/v| -> 1", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  bidiff_dpass_co(nefi_anom2,nrho_anom2,nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%bidiff_dpass_ctr)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("bidiff_dpass_ctr","R",indim,idims, &
            "beam ion diffusivity: ctr passing |vpll/v| -> 1", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  bidiff_dpass_ctr(nefi_anom2,nrho_anom2,nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%bidiff_dtrap)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nefi_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anom2",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("bidiff_dtrap","R",indim,idims, &
            "beam ion diffusivity: deeply trapped orbits", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  bidiff_dtrap(nefi_anom2,nrho_anom2,nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%BphiRZ)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nr",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nz",idims(indim))
     CALL define_var("BphiRZ","R",indim,idims, &
            "Toroidal field", &
            "T","EQ","STATE_PROFILES", &
            "R|units=T|Hermite      BphiRZ(nR,nZ)")
  ENDIF

  IF(ALLOCATED(ss%BRRZ)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nr",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nz",idims(indim))
     CALL define_var("BRRZ","R",indim,idims, &
            "R component of poloidal field", &
            "T","EQ","STATE_PROFILES", &
            "R|units=T|Hermite      BRRZ(nR,nZ)")
  ENDIF

  IF(ALLOCATED(ss%BZRZ)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nr",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nz",idims(indim))
     CALL define_var("BZRZ","R",indim,idims, &
            "Z component of poloidal field", &
            "T","EQ","STATE_PROFILES", &
            "R|units=T|Hermite      BZRZ(nR,nZ)")
  ENDIF

  indim = 0
  CALL define_var("B_axis","R",indim,idims, &
         "|B| at magnetic axis", &
         "T","EQ","STATE_DATA", &
         "R  B_axis")

  indim = 0
  CALL define_var("B_axis_vac","R",indim,idims, &
         "vacuum B field at axis", &
         "T","EQ","STATE_DATA", &
         "R  B_axis_vac")

  IF(ALLOCATED(ss%b_halfHeight)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("b_halfHeight","R",indim,idims, &
            "beam half-height at source", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m       b_halfHeight(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%b_halfwidth)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("b_halfwidth","R",indim,idims, &
            "beam half-width at source", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m       b_halfwidth(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%b_Hdivergence)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("b_Hdivergence","R",indim,idims, &
            "horizontal divergence (D) of beam", &
            "degrees","NBI","MACHINE_DESCRIPTION", &
            "R|units=degrees   b_Hdivergence(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%b_Hfocal_length)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("b_Hfocal_length","R",indim,idims, &
            "horizontal focal length", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m       b_Hfocal_length(nbeam)")
  ENDIF

  indim = 0
  CALL define_var("B_max_lcfs","R",indim,idims, &
         "|B|_max of last closed flux surface", &
         "T","EQ","STATE_DATA", &
         "R  B_max_lcfs")

  indim = 0
  CALL define_var("B_min_lcfs","R",indim,idims, &
         "|B|_min of last closed flux surface", &
         "T","EQ","STATE_DATA", &
         "R  B_min_lcfs")

  IF(ALLOCATED(ss%B_surfMax)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("B_surfMax","R",indim,idims, &
            "max mod(B) on flux surface", &
            "T","EQ","STATE_PROFILES", &
            "R|pclin  B_surfMax(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%B_surfMin)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("B_surfMin","R",indim,idims, &
            "min mod(B) on flux surface", &
            "T","EQ","STATE_PROFILES", &
            "R|pclin  B_surfMin(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%b_Vdivergence)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("b_Vdivergence","R",indim,idims, &
            "vertical divergence (D) of beam", &
            "degrees","NBI","MACHINE_DESCRIPTION", &
            "R|units=degrees   b_Vdivergence(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%b_Vfocal_length)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("b_Vfocal_length","R",indim,idims, &
            "vertical focal length", &
            "m","NBI","MACHINE_DESCRIPTION", &
            "R|units=m       b_Vfocal_length(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%cdicrf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("cdicrf","R",indim,idims, &
            "ICRF current drive", &
            "A","IC","STATE_PROFILES", &
            "R|units=A|step*dA   cdicrf(~nrho_icrf,nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%cdicrf_nphi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_num_nphi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("cdicrf_nphi","R",indim,idims, &
            "ICRF current drive", &
            "A","IC","STATE_PROFILES", &
            "R|units=A|step*dA   cdicrf_nphi(~nrho_icrf,num_nphi(),nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%chie_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chie_anom","R",indim,idims, &
            "Te-equation, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  chie_anom(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chie_drbm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chie_drbm","R",indim,idims, &
            "Te-equation, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  chie_drbm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chie_etg)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chie_etg","R",indim,idims, &
            "Te-equation, diffusivity term ETG model", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin chie_etg(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chie_glf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chie_glf","R",indim,idims, &
            "Te-equation, diffusivity from GLF23 model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chie_glf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chie_gtcneo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chie_gtcneo","R",indim,idims, &
            "Te-equation, neoclassical diffusivity from GTC_NEO model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chie_gtcneo(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chie_mmm71)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chie_mmm71","R",indim,idims, &
            "Te-equation, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  chie_mmm71(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chie_mtm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chie_mtm","R",indim,idims, &
            "Te-equation, thermal diffusivity term MTM model", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin chie_mtm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chie_nclass)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chie_nclass","R",indim,idims, &
            "Te-equation, neoclassical diffusivity from NCLASS model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chie_nclass(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chie_neo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chie_neo","R",indim,idims, &
            "Te-equation, neoclassical diffusivity from NEO model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chie_neo(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chie_ntm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chie_ntm","R",indim,idims, &
            "Te-equation, neoclassical diffusivity from NTM model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chie_ntm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chie_paleo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chie_paleo","R",indim,idims, &
            "Te-equation, diffusivity term PALEO model", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin chie_paleo(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chie_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chie_tglf","R",indim,idims, &
            "Te-equation, diffusivity from TGLF model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chie_tglf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chie_trans)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     CALL define_var("chie_trans","R",indim,idims, &
            "electron thermal diffusivity", &
            "m^2/sec","PLASMA","STATE_PROFILES", &
            "R|pclin chie_trans(nrho)")
  ENDIF

  IF(ALLOCATED(ss%chie_transcat)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_ntrcat",idims(indim))
     CALL define_var("chie_transcat","R",indim,idims, &
            "Electron energy diffusivity", &
            "m^2/sec","PLASMA","STATE_PROFILES", &
            "R|units=m^2/sec|pclin chie_transcat(nrho,ntrcat)")
  ENDIF

  IF(ALLOCATED(ss%chie_w19)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chie_w19","R",indim,idims, &
            "Te-equation, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  chie_w19(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chii_anom)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chii_anom","R",indim,idims, &
            "Ti-equation, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  chii_anom(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chii_drbm)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chii_drbm","R",indim,idims, &
            "Ti-equation, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  chii_drbm(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chii_glf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chii_glf","R",indim,idims, &
            "Ti-equation, diffusivity from GLF23 model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chii_glf(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chii_gtcneo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chii_gtcneo","R",indim,idims, &
            "Ti-equation, neoclassical diffusivity from GTC_NEO model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chii_gtcneo(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chii_mmm71)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chii_mmm71","R",indim,idims, &
            "Ti-equation, diffusivity term", &
            "m^2/sec","ANOM","STATE_PROFILES", &
            "R|units=m^2/sec|pclin  chii_mmm71(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chii_nclass)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chii_nclass","R",indim,idims, &
            "Ti-equation, neoclassical diffusivity from NCALSS model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chii_nclass(nrho_anomth)")
  ENDIF

  IF(ALLOCATED(ss%chii_neo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     CALL define_var("chii_neo","R",indim,idims, &
            "Ti-equation, neoclassical diffusivity from NEO model", &
            "m^2/s","ANOM","STATE_PROFILES", &
            "R|units=m^2/s|pclin chii_neo(nrho_anomth)")
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

END SUBROUTINE plasma_state_store_def_0


SUBROUTINE plasma_state_store_0(ss,icdf,iout,iertot)


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


  IF(ALLOCATED(ss%alla_index)) THEN
     status = nf90_inq_varid(icdf,"alla_index",varid)
     status = nf90_put_var(icdf,varid,ss%alla_index)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: alla_index"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ALLA_name)) THEN
     status = nf90_inq_varid(icdf,"ALLA_name",varid)
     status = nf90_put_var(icdf,varid,ss%ALLA_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ALLA_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ALLA_type)) THEN
     status = nf90_inq_varid(icdf,"ALLA_type",varid)
     status = nf90_put_var(icdf,varid,ss%ALLA_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ALLA_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%all_index)) THEN
     status = nf90_inq_varid(icdf,"all_index",varid)
     status = nf90_put_var(icdf,varid,ss%all_index)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: all_index"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ALL_name)) THEN
     status = nf90_inq_varid(icdf,"ALL_name",varid)
     status = nf90_put_var(icdf,varid,ss%ALL_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ALL_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ALL_type)) THEN
     status = nf90_inq_varid(icdf,"ALL_type",varid)
     status = nf90_put_var(icdf,varid,ss%ALL_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ALL_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%amaxlh)) THEN
     status = nf90_inq_varid(icdf,"amaxlh",varid)
     status = nf90_put_var(icdf,varid,ss%amaxlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: amaxlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%aminlh)) THEN
     status = nf90_inq_varid(icdf,"aminlh",varid)
     status = nf90_put_var(icdf,varid,ss%aminlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: aminlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Ang_Href_coil)) THEN
     status = nf90_inq_varid(icdf,"Ang_Href_coil",varid)
     status = nf90_put_var(icdf,varid,ss%Ang_Href_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Ang_Href_coil"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Ang_Vref_coil)) THEN
     status = nf90_inq_varid(icdf,"Ang_Vref_coil",varid)
     status = nf90_put_var(icdf,varid,ss%Ang_Vref_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Ang_Vref_coil"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ANOM_Code_Info",varid)
  status = nf90_put_var(icdf,varid,ss%ANOM_Code_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: ANOM_Code_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%anom_evar)) THEN
     status = nf90_inq_varid(icdf,"anom_evar",varid)
     status = nf90_put_var(icdf,varid,ss%anom_evar)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: anom_evar"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ant_model)) THEN
     status = nf90_inq_varid(icdf,"ant_model",varid)
     status = nf90_put_var(icdf,varid,ss%ant_model)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ant_model"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap2_halfheight)) THEN
     status = nf90_inq_varid(icdf,"ap2_halfheight",varid)
     status = nf90_put_var(icdf,varid,ss%ap2_halfheight)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ap2_halfheight"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap2_halfwidth)) THEN
     status = nf90_inq_varid(icdf,"ap2_halfwidth",varid)
     status = nf90_put_var(icdf,varid,ss%ap2_halfwidth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ap2_halfwidth"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap2_horiz_offset)) THEN
     status = nf90_inq_varid(icdf,"ap2_horiz_offset",varid)
     status = nf90_put_var(icdf,varid,ss%ap2_horiz_offset)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ap2_horiz_offset"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap2_vert_offset)) THEN
     status = nf90_inq_varid(icdf,"ap2_vert_offset",varid)
     status = nf90_put_var(icdf,varid,ss%ap2_vert_offset)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ap2_vert_offset"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%apolmaxlh)) THEN
     status = nf90_inq_varid(icdf,"apolmaxlh",varid)
     status = nf90_put_var(icdf,varid,ss%apolmaxlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: apolmaxlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%apolminlh)) THEN
     status = nf90_inq_varid(icdf,"apolminlh",varid)
     status = nf90_put_var(icdf,varid,ss%apolminlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: apolminlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap_halfheight)) THEN
     status = nf90_inq_varid(icdf,"ap_halfheight",varid)
     status = nf90_put_var(icdf,varid,ss%ap_halfheight)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ap_halfheight"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap_halfwidth)) THEN
     status = nf90_inq_varid(icdf,"ap_halfwidth",varid)
     status = nf90_put_var(icdf,varid,ss%ap_halfwidth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ap_halfwidth"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap_horiz_offset)) THEN
     status = nf90_inq_varid(icdf,"ap_horiz_offset",varid)
     status = nf90_put_var(icdf,varid,ss%ap_horiz_offset)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ap_horiz_offset"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap_vert_offset)) THEN
     status = nf90_inq_varid(icdf,"ap_vert_offset",varid)
     status = nf90_put_var(icdf,varid,ss%ap_vert_offset)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ap_vert_offset"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%area)) THEN
     status = nf90_inq_varid(icdf,"area",varid)
     status = nf90_put_var(icdf,varid,ss%area)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: area"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%atormaxlh)) THEN
     status = nf90_inq_varid(icdf,"atormaxlh",varid)
     status = nf90_put_var(icdf,varid,ss%atormaxlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: atormaxlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%atorminlh)) THEN
     status = nf90_inq_varid(icdf,"atorminlh",varid)
     status = nf90_put_var(icdf,varid,ss%atorminlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: atorminlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Balloon_code)) THEN
     status = nf90_inq_varid(icdf,"Balloon_code",varid)
     status = nf90_put_var(icdf,varid,ss%Balloon_code)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Balloon_code"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Balloon_mapper)) THEN
     status = nf90_inq_varid(icdf,"Balloon_mapper",varid)
     status = nf90_put_var(icdf,varid,ss%Balloon_mapper)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Balloon_mapper"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Balloon_stable)) THEN
     status = nf90_inq_varid(icdf,"Balloon_stable",varid)
     status = nf90_put_var(icdf,varid,ss%Balloon_stable)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Balloon_stable"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Balloon_status)) THEN
     status = nf90_inq_varid(icdf,"Balloon_status",varid)
     status = nf90_put_var(icdf,varid,ss%Balloon_status)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Balloon_status"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%balstab)) THEN
     status = nf90_inq_varid(icdf,"balstab",varid)
     status = nf90_put_var(icdf,varid,ss%balstab)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: balstab"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%beam_type)) THEN
     status = nf90_inq_varid(icdf,"beam_type",varid)
     status = nf90_put_var(icdf,varid,ss%beam_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: beam_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%bidiff_bpass_co)) THEN
     status = nf90_inq_varid(icdf,"bidiff_bpass_co",varid)
     status = nf90_put_var(icdf,varid,ss%bidiff_bpass_co)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: bidiff_bpass_co"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%bidiff_bpass_ctr)) THEN
     status = nf90_inq_varid(icdf,"bidiff_bpass_ctr",varid)
     status = nf90_put_var(icdf,varid,ss%bidiff_bpass_ctr)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: bidiff_bpass_ctr"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%bidiff_btrap)) THEN
     status = nf90_inq_varid(icdf,"bidiff_btrap",varid)
     status = nf90_put_var(icdf,varid,ss%bidiff_btrap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: bidiff_btrap"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%bidiff_dpass_co)) THEN
     status = nf90_inq_varid(icdf,"bidiff_dpass_co",varid)
     status = nf90_put_var(icdf,varid,ss%bidiff_dpass_co)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: bidiff_dpass_co"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%bidiff_dpass_ctr)) THEN
     status = nf90_inq_varid(icdf,"bidiff_dpass_ctr",varid)
     status = nf90_put_var(icdf,varid,ss%bidiff_dpass_ctr)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: bidiff_dpass_ctr"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%bidiff_dtrap)) THEN
     status = nf90_inq_varid(icdf,"bidiff_dtrap",varid)
     status = nf90_put_var(icdf,varid,ss%bidiff_dtrap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: bidiff_dtrap"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%BphiRZ)) THEN
     status = nf90_inq_varid(icdf,"BphiRZ",varid)
     status = nf90_put_var(icdf,varid,ss%BphiRZ)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: BphiRZ"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%BRRZ)) THEN
     status = nf90_inq_varid(icdf,"BRRZ",varid)
     status = nf90_put_var(icdf,varid,ss%BRRZ)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: BRRZ"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%BZRZ)) THEN
     status = nf90_inq_varid(icdf,"BZRZ",varid)
     status = nf90_put_var(icdf,varid,ss%BZRZ)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: BZRZ"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_axis",varid)
  status = nf90_put_var(icdf,varid,ss%B_axis)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: B_axis"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"B_axis_vac",varid)
  status = nf90_put_var(icdf,varid,ss%B_axis_vac)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: B_axis_vac"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%b_halfHeight)) THEN
     status = nf90_inq_varid(icdf,"b_halfHeight",varid)
     status = nf90_put_var(icdf,varid,ss%b_halfHeight)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: b_halfHeight"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%b_halfwidth)) THEN
     status = nf90_inq_varid(icdf,"b_halfwidth",varid)
     status = nf90_put_var(icdf,varid,ss%b_halfwidth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: b_halfwidth"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%b_Hdivergence)) THEN
     status = nf90_inq_varid(icdf,"b_Hdivergence",varid)
     status = nf90_put_var(icdf,varid,ss%b_Hdivergence)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: b_Hdivergence"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%b_Hfocal_length)) THEN
     status = nf90_inq_varid(icdf,"b_Hfocal_length",varid)
     status = nf90_put_var(icdf,varid,ss%b_Hfocal_length)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: b_Hfocal_length"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_max_lcfs",varid)
  status = nf90_put_var(icdf,varid,ss%B_max_lcfs)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: B_max_lcfs"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"B_min_lcfs",varid)
  status = nf90_put_var(icdf,varid,ss%B_min_lcfs)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: B_min_lcfs"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%B_surfMax)) THEN
     status = nf90_inq_varid(icdf,"B_surfMax",varid)
     status = nf90_put_var(icdf,varid,ss%B_surfMax)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: B_surfMax"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%B_surfMin)) THEN
     status = nf90_inq_varid(icdf,"B_surfMin",varid)
     status = nf90_put_var(icdf,varid,ss%B_surfMin)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: B_surfMin"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%b_Vdivergence)) THEN
     status = nf90_inq_varid(icdf,"b_Vdivergence",varid)
     status = nf90_put_var(icdf,varid,ss%b_Vdivergence)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: b_Vdivergence"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%b_Vfocal_length)) THEN
     status = nf90_inq_varid(icdf,"b_Vfocal_length",varid)
     status = nf90_put_var(icdf,varid,ss%b_Vfocal_length)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: b_Vfocal_length"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%cdicrf)) THEN
     status = nf90_inq_varid(icdf,"cdicrf",varid)
     status = nf90_put_var(icdf,varid,ss%cdicrf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: cdicrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%cdicrf_nphi)) THEN
     status = nf90_inq_varid(icdf,"cdicrf_nphi",varid)
     status = nf90_put_var(icdf,varid,ss%cdicrf_nphi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: cdicrf_nphi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_anom)) THEN
     status = nf90_inq_varid(icdf,"chie_anom",varid)
     status = nf90_put_var(icdf,varid,ss%chie_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_drbm)) THEN
     status = nf90_inq_varid(icdf,"chie_drbm",varid)
     status = nf90_put_var(icdf,varid,ss%chie_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_etg)) THEN
     status = nf90_inq_varid(icdf,"chie_etg",varid)
     status = nf90_put_var(icdf,varid,ss%chie_etg)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_etg"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_glf)) THEN
     status = nf90_inq_varid(icdf,"chie_glf",varid)
     status = nf90_put_var(icdf,varid,ss%chie_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"chie_gtcneo",varid)
     status = nf90_put_var(icdf,varid,ss%chie_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_mmm71)) THEN
     status = nf90_inq_varid(icdf,"chie_mmm71",varid)
     status = nf90_put_var(icdf,varid,ss%chie_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_mtm)) THEN
     status = nf90_inq_varid(icdf,"chie_mtm",varid)
     status = nf90_put_var(icdf,varid,ss%chie_mtm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_mtm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_nclass)) THEN
     status = nf90_inq_varid(icdf,"chie_nclass",varid)
     status = nf90_put_var(icdf,varid,ss%chie_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_neo)) THEN
     status = nf90_inq_varid(icdf,"chie_neo",varid)
     status = nf90_put_var(icdf,varid,ss%chie_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_neo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_ntm)) THEN
     status = nf90_inq_varid(icdf,"chie_ntm",varid)
     status = nf90_put_var(icdf,varid,ss%chie_ntm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_ntm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_paleo)) THEN
     status = nf90_inq_varid(icdf,"chie_paleo",varid)
     status = nf90_put_var(icdf,varid,ss%chie_paleo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_paleo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_tglf)) THEN
     status = nf90_inq_varid(icdf,"chie_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%chie_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_trans)) THEN
     status = nf90_inq_varid(icdf,"chie_trans",varid)
     status = nf90_put_var(icdf,varid,ss%chie_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_transcat)) THEN
     status = nf90_inq_varid(icdf,"chie_transcat",varid)
     status = nf90_put_var(icdf,varid,ss%chie_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_w19)) THEN
     status = nf90_inq_varid(icdf,"chie_w19",varid)
     status = nf90_put_var(icdf,varid,ss%chie_w19)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chie_w19"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_anom)) THEN
     status = nf90_inq_varid(icdf,"chii_anom",varid)
     status = nf90_put_var(icdf,varid,ss%chii_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chii_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_drbm)) THEN
     status = nf90_inq_varid(icdf,"chii_drbm",varid)
     status = nf90_put_var(icdf,varid,ss%chii_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chii_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_glf)) THEN
     status = nf90_inq_varid(icdf,"chii_glf",varid)
     status = nf90_put_var(icdf,varid,ss%chii_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chii_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"chii_gtcneo",varid)
     status = nf90_put_var(icdf,varid,ss%chii_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chii_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_mmm71)) THEN
     status = nf90_inq_varid(icdf,"chii_mmm71",varid)
     status = nf90_put_var(icdf,varid,ss%chii_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chii_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_nclass)) THEN
     status = nf90_inq_varid(icdf,"chii_nclass",varid)
     status = nf90_put_var(icdf,varid,ss%chii_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chii_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_neo)) THEN
     status = nf90_inq_varid(icdf,"chii_neo",varid)
     status = nf90_put_var(icdf,varid,ss%chii_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: chii_neo"
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

END SUBROUTINE plasma_state_store_0
