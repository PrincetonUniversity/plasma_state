! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_write_def_7(ss,icdf,cclist,nids,iout,iwarn,iertot)


  ! Define updated data

  USE plasma_state_definition_mod
  USE netcdf
  USE ps_nidlist_defn
  IMPLICIT NONE

  TYPE (plasma_state) :: ss  ! state containing array elements to be allocated
  INTEGER, INTENT(IN) :: icdf  ! NetCDF file ID
  INTEGER, INTENT(IN) :: cclist(ps_ccount)  ! component selection
  TYPE (nidlist) :: nids       ! defined dimensions list
  INTEGER, INTENT(IN) :: iout  ! LUN for messages
  INTEGER, INTENT(INOUT) :: iwarn   ! warn count
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !-------------------
  LOGICAL :: plasma_state_hash_match,ichange

  INTEGER :: varid,pvarid,vvarid,dimid,status,iertmp,iaddr
  INTEGER :: idims(20),indim,ival,ii,ierr
  CHARACTER*80 :: nc_errmsg

  !-------------------

  ierr = 0   ! local

  iaddr = ps_dhash_nrho_pel
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_pel).AND.ichange) THEN
     IF(cclist(ps_cnum_PEL).NE.1) THEN
        CALL ccwarn("rho_pel","PEL")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_pel.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_pel,"dim_nrho_pel",ss%nrho_pel)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_pel
        CALL define_var("rho_pel","R",indim,idims, &
               "rho grid -- PEL", &
               "-","PEL","SIMULATION_INIT", &
               "G  rho_pel(nrho_pel)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_rad
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_rad).AND.ichange) THEN
     IF(cclist(ps_cnum_RAD).NE.1) THEN
        CALL ccwarn("rho_rad","RAD")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_rad.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_rad,"dim_nrho_rad",ss%nrho_rad)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_rad
        CALL define_var("rho_rad","R",indim,idims, &
               "rho grid (RAD)", &
               "-","RAD","SIMULATION_INIT", &
               "G  rho_rad(nrho_rad)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_rw
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_rw).AND.ichange) THEN
     IF(cclist(ps_cnum_RUNAWAY).NE.1) THEN
        CALL ccwarn("rho_rw","RUNAWAY")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_rw.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_rw,"dim_nrho_rw",ss%nrho_rw)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_rw
        CALL define_var("rho_rw","R",indim,idims, &
               "rho grid -- RUNAWAY electrons", &
               "-","RUNAWAY","SIMULATION_INIT", &
               "G  rho_rw(nrho_rw)")
     ENDIF
  ENDIF

  iaddr = ps_shash_rlim
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rlim).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("rlim","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_num_rzlim.EQ.0) THEN
           CALL define_dim(nids%nid_dim_num_rzlim,"dim_num_rzlim",ss%num_rzlim)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_num_rzlim
        CALL define_var("rlim","R",indim,idims, &
               "R points in closed (R,Z) contour sequence", &
               "m","EQ","MACHINE_DESCRIPTION", &
               "R|units=m  rlim(num_rzlim)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Rloc_coil
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Rloc_coil).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Rloc_coil","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_ncoils.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ncoils,"dim_ncoils",ss%ncoils)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ncoils
        CALL define_var("Rloc_coil","R",indim,idims, &
               "R location of coil, lower left corner", &
               "m","EQ","MACHINE_DESCRIPTION", &
               "R|units=m  Rloc_coil(ncoils)")
     ENDIF
  ENDIF

  iaddr = ps_phash_Rmajor_mean
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Rmajor_mean).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Rmajor_mean","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_eq_geo.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_eq_geo,"dim_nrho_eq_geo",ss%nrho_eq_geo)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_eq_geo
        CALL define_var("Rmajor_mean","R",indim,idims, &
               "Rmajor_mean = (R_Surfmax+R_surfmin)/2", &
               "m","EQ","STATE_PROFILES", &
               "R|pclin  Rmajor_mean(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_rMinor_mean
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rMinor_mean).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("rMinor_mean","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_eq_geo.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_eq_geo,"dim_nrho_eq_geo",ss%nrho_eq_geo)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_eq_geo
        CALL define_var("rMinor_mean","R",indim,idims, &
               "rMinor_mean = (R_Surfmax-R_surfmin)/2", &
               "m","EQ","STATE_PROFILES", &
               "R|pclin  rMinor_mean(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_shash_RUNAWAY_Code_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_RUNAWAY).NE.1) THEN
        CALL ccwarn("RUNAWAY_Code_Info","RUNAWAY")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("RUNAWAY_Code_Info","C*80",indim,idims, &
               "Information: code implementing RUNAWAY component", &
               "__NONE","RUNAWAY","SIMULATION_INIT", &
               "C*80   RUNAWAY_Code_Info")
     ENDIF
  ENDIF

  iaddr = ps_shash_RunID
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("RunID","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        CALL define_var("RunID","N",indim,idims, &
               "Run ID", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "N      RunID")
     ENDIF
  ENDIF

  iaddr = ps_shash_R_ant
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%R_ant).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("R_ant","IC")
     ELSE
        indim = 0
        IF(nids%nid_dim_nicrf_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nicrf_src,"dim_nicrf_src",ss%nicrf_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nicrf_src
        CALL define_var("R_ant","R",indim,idims, &
               "major radius of antenna", &
               "m","IC","SHOT_CONFIGURATION", &
               "R|units=m R_ant(nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_R_antgeo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%R_antgeo).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("R_antgeo","IC")
     ELSE
        indim = 0
        IF(nids%nid_dim_max_nrz_antgeo.EQ.0) THEN
           CALL define_dim(nids%nid_dim_max_nrz_antgeo,"dim_max_nrz_antgeo",ss%max_nrz_antgeo)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_max_nrz_antgeo
        IF(nids%nid_dim_nicrf_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nicrf_src,"dim_nicrf_src",ss%nicrf_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nicrf_src
        CALL define_var("R_antgeo","R",indim,idims, &
               "antenna geo: R pts", &
               "m","IC","MACHINE_DESCRIPTION", &
               "R|units=m R_antgeo(nrz_antgeo(),nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_R_axis
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("R_axis","EQ")
     ELSE
        indim = 0
        CALL define_var("R_axis","R",indim,idims, &
               "R of magnetic axis", &
               "m","EQ","STATE_DATA", &
               "R  R_axis")
     ENDIF
  ENDIF

  iaddr = ps_shash_R_EC_launch
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%R_EC_launch).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("R_EC_launch","EC")
     ELSE
        indim = 0
        IF(nids%nid_dim_necrf_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_necrf_src,"dim_necrf_src",ss%necrf_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_necrf_src
        CALL define_var("R_EC_launch","R",indim,idims, &
               "R of center of launcher", &
               "m","EC","MACHINE_DESCRIPTION", &
               "R|units=m   R_EC_launch(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_R_geo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%R_geo).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("R_geo","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_eq.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_eq,"dim_nrho_eq",ss%nrho_eq)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_eq
        IF(nids%nid_dim_nth_eq.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nth_eq,"dim_nth_eq",ss%nth_eq)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nth_eq
        CALL define_var("R_geo","R",indim,idims, &
               "flux surfaces R(rho,theta)", &
               "m","EQ","STATE_PROFILES", &
               "R|units=m|Hermite_explicit R_geo(nrho_eq,nth_eq)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nr
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%R_grid).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("R_grid","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_nr.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nr,"dim_nr",ss%nr)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nr
        CALL define_var("R_grid","R",indim,idims, &
               "R grid", &
               "m","EQ","SIMULATION_INIT", &
               "G   R_grid(nR)")
     ENDIF
  ENDIF

  iaddr = ps_shash_R_max_box
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("R_max_box","EQ")
     ELSE
        indim = 0
        CALL define_var("R_max_box","R",indim,idims, &
               "R_max of bounding box", &
               "m","EQ","MACHINE_DESCRIPTION", &
               "R  R_max_box")
     ENDIF
  ENDIF

  iaddr = ps_shash_R_max_lcfs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("R_max_lcfs","EQ")
     ELSE
        indim = 0
        CALL define_var("R_max_lcfs","R",indim,idims, &
               "R_max of last closed flux surface", &
               "m","EQ","STATE_DATA", &
               "R  R_max_lcfs")
     ENDIF
  ENDIF

  iaddr = ps_phash_R_midp_in
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%R_midp_in).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("R_midp_in","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_eq_geo.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_eq_geo,"dim_nrho_eq_geo",ss%nrho_eq_geo)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_eq_geo
        CALL define_var("R_midp_in","R",indim,idims, &
               "R (high field side) midplane intercept", &
               "m","EQ","STATE_PROFILES", &
               "R|pclin  R_midp_in(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_R_midp_out
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%R_midp_out).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("R_midp_out","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_eq_geo.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_eq_geo,"dim_nrho_eq_geo",ss%nrho_eq_geo)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_eq_geo
        CALL define_var("R_midp_out","R",indim,idims, &
               "R (low field side) midplane intercept", &
               "m","EQ","STATE_PROFILES", &
               "R|pclin  R_midp_out(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_shash_R_min_box
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("R_min_box","EQ")
     ELSE
        indim = 0
        CALL define_var("R_min_box","R",indim,idims, &
               "R_min of bounding box", &
               "m","EQ","MACHINE_DESCRIPTION", &
               "R  R_min_box")
     ENDIF
  ENDIF

  iaddr = ps_shash_R_min_lcfs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("R_min_lcfs","EQ")
     ELSE
        indim = 0
        CALL define_var("R_min_lcfs","R",indim,idims, &
               "R_min of last closed flux surface", &
               "m","EQ","STATE_DATA", &
               "R  R_min_lcfs")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nr_rip
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%R_ripple).AND.ichange) THEN
     IF(cclist(ps_cnum_RIPPLE).NE.1) THEN
        CALL ccwarn("R_ripple","RIPPLE")
     ELSE
        indim = 0
        IF(nids%nid_dim_nr_rip.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nr_rip,"dim_nr_rip",ss%nr_rip)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nr_rip
        CALL define_var("R_ripple","R",indim,idims, &
               "R grid for TF ripple field(s)", &
               "m","RIPPLE","SIMULATION_INIT", &
               "G  R_ripple(nR_rip)")
     ENDIF
  ENDIF

  iaddr = ps_phash_R_surfMax
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%R_surfMax).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("R_surfMax","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_eq_geo.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_eq_geo,"dim_nrho_eq_geo",ss%nrho_eq_geo)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_eq_geo
        CALL define_var("R_surfMax","R",indim,idims, &
               "max R on flux surface", &
               "m","EQ","STATE_PROFILES", &
               "R|pclin  R_surfMax(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_R_surfMin
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%R_surfMin).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("R_surfMin","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_eq_geo.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_eq_geo,"dim_nrho_eq_geo",ss%nrho_eq_geo)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_eq_geo
        CALL define_var("R_surfMin","R",indim,idims, &
               "min R on flux surface", &
               "m","EQ","STATE_PROFILES", &
               "R|pclin  R_surfMin(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_s0reco
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%s0reco).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("s0reco","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_gas,"dm1_nrho_gas",ss%nrho_gas-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_gas
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        CALL define_var("s0reco","R",indim,idims, &
               "recombination neutral sce", &
               "#/sec","GAS","STATE_PROFILES", &
               "R|step*dV|units=#/sec s0reco(~nrho_gas,nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_s0reco_e
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%s0reco_e).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("s0reco_e","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_gas,"dm1_nrho_gas",ss%nrho_gas-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_gas
        CALL define_var("s0reco_e","R",indim,idims, &
               "net electron source associated with recombination:", &
               "#/sec","GAS","STATE_PROFILES", &
               "R|step*dV|units=#/sec s0reco_e(~nrho_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_s0reco_recap
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%s0reco_recap).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("s0reco_recap","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_gas,"dm1_nrho_gas",ss%nrho_gas-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_gas
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        CALL define_var("s0reco_recap","R",indim,idims, &
               "recombination neutrals recapture", &
               "#/sec","GAS","STATE_PROFILES", &
               "R|step*dV|units=#/sec s0reco_recap(~nrho_gas,nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_shash_sa_index
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sa_index).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("sa_index","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dp1_nspec_all.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_all,"dp1_nspec_all",ss%nspec_all+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_all
        CALL define_var("sa_index","I",indim,idims, &
               "species index in original lists", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "I sa_index(0:nspec_all)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_tha
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%SA_name).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("SA_name","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dp1_nspec_tha.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_tha,"dp1_nspec_tha",ss%nspec_tha+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_tha
        CALL define_var("SA_name","N",indim,idims, &
               "thermal species abridged", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "S|thermal_specie  SA(0:nspec_tha)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_tha
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%SA_type).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("SA_type","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dp1_nspec_tha.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_tha,"dp1_nspec_tha",ss%nspec_tha+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_tha
        CALL define_var("SA_type","I",indim,idims, &
               "SA specie types", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "S|thermal_specie  SA(0:nspec_tha)")
     ENDIF
  ENDIF

  iaddr = ps_phash_sb0halo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sb0halo).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("sb0halo","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_nbi.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_nbi,"dm1_nrho_nbi",ss%nrho_nbi-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_nbi
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        CALL define_var("sb0halo","R",indim,idims, &
               "beam dep cx sink", &
               "#/sec","NBI","STATE_PROFILES", &
               "R|step*dV|units=#/sec sb0halo(~nrho_nbi,nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_sb0halo_recap
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sb0halo_recap).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("sb0halo_recap","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_nbi.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_nbi,"dm1_nrho_nbi",ss%nrho_nbi-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_nbi
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        CALL define_var("sb0halo_recap","R",indim,idims, &
               "cx neutral recapture", &
               "#/sec","NBI","STATE_PROFILES", &
               "R|step*dV|units=#/sec sb0halo_recap(~nrho_nbi,nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_sbedep
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sbedep).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("sbedep","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_nbi.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_nbi,"dm1_nrho_nbi",ss%nrho_nbi-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_nbi
        CALL define_var("sbedep","R",indim,idims, &
               "beam deposition cold electron sce", &
               "#/sec","NBI","STATE_PROFILES", &
               "R|step*dV|units=#/sec sbedep(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_sbehalo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sbehalo).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("sbehalo","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_nbi.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_nbi,"dm1_nrho_nbi",ss%nrho_nbi-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_nbi
        CALL define_var("sbehalo","R",indim,idims, &
               "cold electron sce due to", &
               "#/sec","NBI","STATE_PROFILES", &
               "R|step*dV|units=#/sec sbehalo(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_sbsce
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sbsce).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("sbsce","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_nbi.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_nbi,"dm1_nrho_nbi",ss%nrho_nbi-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_nbi
        IF(nids%nid_dp1_nspec_tha.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_tha,"dp1_nspec_tha",ss%nspec_tha+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_tha
        CALL define_var("sbsce","R",indim,idims, &
               "net source, thermal ions & electrons", &
               "#/sec","NBI","STATE_PROFILES", &
               "R|step*dV|units=#/sec   sbsce(~nrho_nbi,0:nspec_tha)")
     ENDIF
  ENDIF

  iaddr = ps_phash_sbtherm
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sbtherm).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("sbtherm","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_nbi.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_nbi,"dm1_nrho_nbi",ss%nrho_nbi-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_nbi
        IF(nids%nid_dim_nspec_beam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_beam,"dim_nspec_beam",ss%nspec_beam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_beam
        CALL define_var("sbtherm","R",indim,idims, &
               "beam ion thermalization", &
               "#/sec","NBI","STATE_PROFILES", &
               "R|step*dV|units=#/sec sbtherm(~nrho_nbi,nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_sc0
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sc0).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("sc0","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dim_ngsc0.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ngsc0,"dim_ngsc0",ss%ngsc0)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ngsc0
        CALL define_var("sc0","R",indim,idims, &
               "edge neutral sources (atoms/sec)", &
               "#/sec","GAS","STATE_DATA", &
               "R|units=#/sec  sc0(ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_shash_sc0_to_sgas
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sc0_to_sgas).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("sc0_to_sgas","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dim_ngsc0.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ngsc0,"dim_ngsc0",ss%ngsc0)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ngsc0
        CALL define_var("sc0_to_sgas","I",indim,idims, &
               "index map from neutral source to neutral species", &
               "__NONE","GAS","SIMULATION_INIT", &
               "I  sc0_to_sgas(ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_phash_sc_anom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sc_anom).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("sc_anom","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_anomth.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_anomth,"dm1_nrho_anomth",ss%nrho_anomth-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_anomth
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        CALL define_var("sc_anom","R",indim,idims, &
               "anomalous sources", &
               "#/sec","ANOM","STATE_PROFILES", &
               "R|units=#/sec|step*dV sc_anom(~nrho_anomth,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_sfsce
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sfsce).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("sfsce","FUS")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_fus.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_fus,"dm1_nrho_fus",ss%nrho_fus-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_fus
        IF(nids%nid_dp1_nspec_tha.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_tha,"dp1_nspec_tha",ss%nspec_tha+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_tha
        CALL define_var("sfsce","R",indim,idims, &
               "net thermalization source of fusion ions", &
               "#/sec","FUS","STATE_PROFILES", &
               "R|step*dV|units=#/sec   sfsce(~nrho_fus,0:nspec_tha)")
     ENDIF
  ENDIF

  iaddr = ps_phash_sftherm
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sftherm).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("sftherm","FUS")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_fus.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_fus,"dm1_nrho_fus",ss%nrho_fus-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_fus
        IF(nids%nid_dim_nspec_fusion.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_fusion,"dim_nspec_fusion",ss%nspec_fusion)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_fusion
        CALL define_var("sftherm","R",indim,idims, &
               "fusion ion thermalization", &
               "#/sec","FUS","STATE_PROFILES", &
               "R|step*dV|units=#/sec   sftherm(~nrho_fus,nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_fusion
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%SFUS_name).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("SFUS_name","FUS")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_nspec_fusion.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_fusion,"dim_nspec_fusion",ss%nspec_fusion)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_fusion
        CALL define_var("SFUS_name","N",indim,idims, &
               "fusion product ion species", &
               "__NONE","FUS","SHOT_CONFIGURATION", &
               "S|fusion_ion   SFUS(nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_shash_sfus_to_all
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sfus_to_all).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("sfus_to_all","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dim_nspec_fusion.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_fusion,"dim_nspec_fusion",ss%nspec_fusion)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_fusion
        CALL define_var("sfus_to_all","I",indim,idims, &
               'map sfus index to "all" list', &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "I  sfus_to_all(nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_shash_sfus_to_alla
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sfus_to_alla).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("sfus_to_alla","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dim_nspec_fusion.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_fusion,"dim_nspec_fusion",ss%nspec_fusion)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_fusion
        CALL define_var("sfus_to_alla","I",indim,idims, &
               'map sfus index to "alla" list', &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "I  sfus_to_alla(nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_fusion
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%SFUS_type).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("SFUS_type","FUS")
     ELSE
        indim = 0
        IF(nids%nid_dim_nspec_fusion.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_fusion,"dim_nspec_fusion",ss%nspec_fusion)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_fusion
        CALL define_var("SFUS_type","I",indim,idims, &
               "SFUS specie types", &
               "__NONE","FUS","SHOT_CONFIGURATION", &
               "S|fusion_ion   SFUS(nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_gas
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%SGAS_name).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("SGAS_name","GAS")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        CALL define_var("SGAS_name","N",indim,idims, &
               "atomic neutral species in plasma core", &
               "__NONE","GAS","SIMULATION_INIT", &
               "S|neutral_gas  SGAS(nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_shash_sgas_to_s
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sgas_to_s).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("sgas_to_s","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        CALL define_var("sgas_to_s","I",indim,idims, &
               'map sgas index to "s" list', &
               "__NONE","GAS","SIMULATION_INIT", &
               "I  sgas_to_s(nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_gas
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%SGAS_type).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("SGAS_type","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        CALL define_var("SGAS_type","I",indim,idims, &
               "SGAS specie types", &
               "__NONE","GAS","SIMULATION_INIT", &
               "S|neutral_gas  SGAS(nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_shash_shot_number
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("shot_number","PLASMA")
     ELSE
        indim = 0
        CALL define_var("shot_number","I",indim,idims, &
               "integer shot number", &
               "__NONE","PLASMA","SHOT_CONFIGURATION", &
               "I shot_number")
     ENDIF
  ENDIF

  iaddr = ps_phash_sii_xs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sii_xs).AND.ichange) THEN
     IF(cclist(ps_cnum_RAD).NE.1) THEN
        CALL ccwarn("sii_xs","RAD")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_mist.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_mist,"dm1_nrho_mist",ss%nrho_mist-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_mist
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        IF(nids%nid_dim_nxs.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nxs,"dim_nxs",ss%nxs)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nxs
        CALL define_var("sii_xs","R",indim,idims, &
               "species source/sinks driven by xs(1:nxs)", &
               "(#/sec)/(#/sec)","RAD","STATE_PROFILES", &
               "R|step*dV|units=(#/sec)/(#/sec) sii_xs(~nrho_mist,0:nspec_th,nxs)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_imp0
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%SIMP0_name).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("SIMP0_name","GAS")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_nspec_imp0.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_imp0,"dim_nspec_imp0",ss%nspec_imp0)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_imp0
        CALL define_var("SIMP0_name","N",indim,idims, &
               "atomic impurity species in simulation", &
               "__NONE","GAS","SIMULATION_INIT", &
               "S|impurity_atoms SIMP0(nspec_imp0)")
     ENDIF
  ENDIF

  iaddr = ps_shash_simp0_to_s
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%simp0_to_s).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("simp0_to_s","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dim_nspec_imp0.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_imp0,"dim_nspec_imp0",ss%nspec_imp0)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_imp0
        CALL define_var("simp0_to_s","I",indim,idims, &
               'map simp0 index to "s" list', &
               "__NONE","GAS","SIMULATION_INIT", &
               "I  simp0_to_s(nspec_imp0)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_imp0
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%SIMP0_type).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("SIMP0_type","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dim_nspec_imp0.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_imp0,"dim_nspec_imp0",ss%nspec_imp0)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_imp0
        CALL define_var("SIMP0_type","I",indim,idims, &
               "SIMP0 specie types", &
               "__NONE","GAS","SIMULATION_INIT", &
               "S|impurity_atoms SIMP0(nspec_imp0)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_impi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%SIMPI_name).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("SIMPI_name","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_nspec_impi.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_impi,"dim_nspec_impi",ss%nspec_impi)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_impi
        CALL define_var("SIMPI_name","N",indim,idims, &
               "impurity ion species in simulation", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "S|impurity_atoms SIMPI(nspec_impi)")
     ENDIF
  ENDIF

  iaddr = ps_shash_simpi_to_s
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%simpi_to_s).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("simpi_to_s","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dim_nspec_impi.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_impi,"dim_nspec_impi",ss%nspec_impi)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_impi
        CALL define_var("simpi_to_s","I",indim,idims, &
               'map simpi index to "s" list', &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "I  simpi_to_s(nspec_impi)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_impi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%SIMPI_type).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("SIMPI_type","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dim_nspec_impi.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_impi,"dim_nspec_impi",ss%nspec_impi)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_impi
        CALL define_var("SIMPI_type","I",indim,idims, &
               "SIMPI specie types", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "S|impurity_atoms SIMPI(nspec_impi)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_beam
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%SNBI_name).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("SNBI_name","NBI")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_nspec_beam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_beam,"dim_nspec_beam",ss%nspec_beam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_beam
        CALL define_var("SNBI_name","N",indim,idims, &
               "beam ion species", &
               "__NONE","NBI","SIMULATION_INIT", &
               "S|beam_ion     SNBI(nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_snbi_to_all
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%snbi_to_all).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("snbi_to_all","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dim_nspec_beam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_beam,"dim_nspec_beam",ss%nspec_beam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_beam
        CALL define_var("snbi_to_all","I",indim,idims, &
               'map snbi index to "all" list', &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "I  snbi_to_all(nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_snbi_to_alla
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%snbi_to_alla).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("snbi_to_alla","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dim_nspec_beam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_beam,"dim_nspec_beam",ss%nspec_beam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_beam
        CALL define_var("snbi_to_alla","I",indim,idims, &
               'map snbi index to "alla" list', &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "I  snbi_to_alla(nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_beam
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%SNBI_type).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("SNBI_type","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dim_nspec_beam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_beam,"dim_nspec_beam",ss%nspec_beam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_beam
        CALL define_var("SNBI_type","I",indim,idims, &
               "SNBI specie types", &
               "__NONE","NBI","SIMULATION_INIT", &
               "S|beam_ion     SNBI(nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_sn_trans
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sn_trans).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("sn_trans","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho,"dm1_nrho",ss%nrho-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        CALL define_var("sn_trans","R",indim,idims, &
               "particle transport (loss)", &
               "#/sec","PLASMA","STATE_PROFILES", &
               "R|units=#/sec|step*dV sn_trans(~nrho,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_sn_transcat
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sn_transcat).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("sn_transcat","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho,"dm1_nrho",ss%nrho-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        IF(nids%nid_dim_ntrcat.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ntrcat,"dim_ntrcat",ss%ntrcat)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ntrcat
        CALL define_var("sn_transcat","R",indim,idims, &
               "species transport", &
               "#/sec","PLASMA","STATE_PROFILES", &
               "R|step*dV|units=#/sec  sn_transcat(~nrho,0:nspec_th,ntrcat)")
     ENDIF
  ENDIF

  iaddr = ps_phash_sprof0
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sprof0).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("sprof0","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_gas,"dm1_nrho_gas",ss%nrho_gas-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_gas
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        IF(nids%nid_dim_ngsc0.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ngsc0,"dim_ngsc0",ss%ngsc0)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ngsc0
        CALL define_var("sprof0","R",indim,idims, &
               "gas species source/sinks per unit species influx", &
               "(#/sec)/(#/sec)","GAS","STATE_PROFILES", &
               "R|step*dV|units=(#/sec)/(#/sec)  sprof0(~nrho_gas,nspec_gas,ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_phash_sprof0e
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sprof0e).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("sprof0e","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_gas,"dm1_nrho_gas",ss%nrho_gas-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_gas
        IF(nids%nid_dim_ngsc0.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ngsc0,"dim_ngsc0",ss%ngsc0)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ngsc0
        CALL define_var("sprof0e","R",indim,idims, &
               "electron source per unit neutral species influx", &
               "(#/sec)/(#/sec)","GAS","STATE_PROFILES", &
               "R|step*dV|units=(#/sec)/(#/sec)  sprof0e(~nrho_gas,ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_phash_spsce
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%spsce).AND.ichange) THEN
     IF(cclist(ps_cnum_PEL).NE.1) THEN
        CALL ccwarn("spsce","PEL")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_pel.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_pel,"dm1_nrho_pel",ss%nrho_pel-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_pel
        IF(nids%nid_dp1_nspec_tha.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_tha,"dp1_nspec_tha",ss%nspec_tha+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_tha
        CALL define_var("spsce","R",indim,idims, &
               "net source, thermal ions & electrons", &
               "#/sec","PEL","STATE_PROFILES", &
               "R|step*dV|units=#/sec   spsce(~nrho_pel,0:nspec_tha)")
     ENDIF
  ENDIF

  iaddr = ps_phash_squareLO
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%squareLO).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("squareLO","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_eq_geo.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_eq_geo,"dim_nrho_eq_geo",ss%nrho_eq_geo)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_eq_geo
        CALL define_var("squareLO","R",indim,idims, &
               "lower outer squareness", &
               "-","EQ","STATE_PROFILES", &
               "R|pclin  squareLO(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_squareUO
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%squareUO).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("squareUO","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_eq_geo.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_eq_geo,"dim_nrho_eq_geo",ss%nrho_eq_geo)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_eq_geo
        CALL define_var("squareUO","R",indim,idims, &
               "upper outer squareness", &
               "-","EQ","STATE_PROFILES", &
               "R|pclin  squareUO(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_shash_sRtcen
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%sRtcen).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("sRtcen","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dim_nbeam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nbeam,"dim_nbeam",ss%nbeam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nbeam
        CALL define_var("sRtcen","R",indim,idims, &
               "signed tangency radius", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m       sRtcen(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_surf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%surf).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("surf","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_eq.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_eq,"dim_nrho_eq",ss%nrho_eq)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_eq
        CALL define_var("surf","R",indim,idims, &
               "area of flux surfaces", &
               "m^2","EQ","STATE_PROFILES", &
               "R|units=m^2|Spline surf(nrho_eq)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_th
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%S_name).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("S_name","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        CALL define_var("S_name","N",indim,idims, &
               "thermal species list", &
               "__NONE","PLASMA","SHOT_CONFIGURATION", &
               "S|thermal_specie  S(0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_shash_s_to_sa
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%s_to_sa).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("s_to_sa","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        CALL define_var("s_to_sa","I",indim,idims, &
               "address in sa species list", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "I s_to_sa(0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_th
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%S_type).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("S_type","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        CALL define_var("S_type","I",indim,idims, &
               "S specie types", &
               "__NONE","PLASMA","SHOT_CONFIGURATION", &
               "S|thermal_specie  S(0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_shash_t0
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("t0","PLASMA")
     ELSE
        indim = 0
        CALL define_var("t0","R",indim,idims, &
               "time at beginning of IPS macro timestep", &
               "sec","PLASMA","STATE_DATA", &
               "R  t0")
     ENDIF
  ENDIF

  iaddr = ps_phash_T0cx
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%T0cx).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("T0cx","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_gas,"dm1_nrho_gas",ss%nrho_gas-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_gas
        IF(nids%nid_dim_ngsc0.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ngsc0,"dim_ngsc0",ss%ngsc0)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ngsc0
        CALL define_var("T0cx","R",indim,idims, &
               "effective neutral temperature for CX power", &
               "keV","GAS","STATE_PROFILES", &
               "R|step*qqcx|units=keV  T0cx(~nrho_gas,ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_phash_T0sc0
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%T0sc0).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("T0sc0","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_gas,"dm1_nrho_gas",ss%nrho_gas-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_gas
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        IF(nids%nid_dim_ngsc0.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ngsc0,"dim_ngsc0",ss%ngsc0)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ngsc0
        CALL define_var("T0sc0","R",indim,idims, &
               "neutral temperature (2/3)<E0>", &
               "keV","GAS","STATE_PROFILES", &
               "R|step*n0norm|units=keV        T0sc0(~nrho_gas,nspec_gas,ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_phash_T0_halo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%T0_halo).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("T0_halo","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_nbi.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_nbi,"dm1_nrho_nbi",ss%nrho_nbi-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_nbi
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        CALL define_var("T0_halo","R",indim,idims, &
               "halo neutral temperature (2/3)<E0>", &
               "keV","NBI","STATE_PROFILES", &
               "R|units=keV|step*n0_halo  T0_halo(~nrho_nbi,nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_T0_reco
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%T0_reco).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("T0_reco","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_gas,"dm1_nrho_gas",ss%nrho_gas-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_gas
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        CALL define_var("T0_reco","R",indim,idims, &
               "reco neutral temperature (2/3)<E0>", &
               "keV","GAS","STATE_PROFILES", &
               "R|units=keV|step*n0_reco  T0_reco(~nrho_gas,nspec_gas)")
     ENDIF
  ENDIF
  CONTAINS


     SUBROUTINE define_dim(dimid,dname,idval)
        INTEGER, intent(out) :: dimid
        CHARACTER*(*), INTENT(in) :: dname
        INTEGER, intent(in) :: idval

        status = nf90_def_dim(icdf,dname,idval,dimid)
        IF(status.NE.NF90_NOERR) THEN
           WRITE(iout,*) ' ?ps_update_file_write: error defining dimension "'// &
                 trim(dname)//'".'
           IERR=9999
        ENDIF
     END SUBROUTINE define_dim



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



     SUBROUTINE ccwarn(varnam,compnam)

        CHARACTER*(*), intent(in) :: varnam   ! name of variable or array
        CHARACTER*(*), intent(in) :: compnam  ! name of component

        if(iwarn.eq.0) then
           write(iout,*) ' '
           write(iout,*) ' %ps_update_file_write: modified state elements'
           write(iout,*) '  exist which were not written due to the "cclist"'
           write(iout,*) '  input variable which screens out elements from'
           write(iout,*) '  selected components: cclist = ',cclist,';'
           write(iout,*) '  element values of 1 indicate components that are'
           write(iout,*) '  enabled for write.'
           write(iout,*) ' '
        endif

        iwarn = iwarn + 1

        write(iout,*) ' %ps_update_file_write: modified: ss%'// &
             trim(varnam)//' ('//trim(compnam)//' component) NOT WRITTEN.'

     END SUBROUTINE ccwarn

END SUBROUTINE ps_update_file_write_def_7

SUBROUTINE ps_update_file_write_7(ss,icdf,cclist,iucount,iout,iwarn,iertot)


  ! Write updated data

  USE plasma_state_definition_mod
  USE netcdf
  IMPLICIT NONE

  TYPE (plasma_state) :: ss  ! state containing array elements to be allocated
  INTEGER, INTENT(IN) :: icdf  ! NetCDF file ID
  INTEGER, INTENT(IN) :: cclist(ps_ccount)  ! component selection
  INTEGER, INTENT(INOUT) :: iucount ! update count
  INTEGER, INTENT(IN) :: iout  ! LUN for messages
  INTEGER, INTENT(INOUT) :: iwarn   ! warn count
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !-------------------

  INTEGER :: varid,status
  CHARACTER*80 :: nc_errmsg

  !-------------------


  status = nf90_inq_varid(icdf,"rho_pel",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_pel)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_pel"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_rad",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_rad)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_rad"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_rw",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_rw)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_rw"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rlim",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rlim)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rlim"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Rloc_coil",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Rloc_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Rloc_coil"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Rmajor_mean",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Rmajor_mean)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Rmajor_mean"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rMinor_mean",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rMinor_mean)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rMinor_mean"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RUNAWAY_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%RUNAWAY_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: RUNAWAY_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RunID",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%RunID)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: RunID"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_ant",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_ant)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_ant"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_antgeo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_antgeo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_antgeo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_axis",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_axis)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_axis"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_EC_launch",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_EC_launch)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_EC_launch"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_geo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_geo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_geo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_grid",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_grid)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_grid"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_max_box",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_max_box)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_max_box"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_max_lcfs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_max_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_max_lcfs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_midp_in",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_midp_in)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_midp_in"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_midp_out",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_midp_out)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_midp_out"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_min_box",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_min_box)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_min_box"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_min_lcfs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_min_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_min_lcfs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_ripple",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_ripple)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_ripple"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_surfMax",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_surfMax)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_surfMax"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_surfMin",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R_surfMin)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R_surfMin"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"s0reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%s0reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: s0reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"s0reco_e",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%s0reco_e)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: s0reco_e"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"s0reco_recap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%s0reco_recap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: s0reco_recap"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sa_index",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sa_index)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sa_index"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SA_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%SA_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: SA_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SA_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%SA_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: SA_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sb0halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sb0halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sb0halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sb0halo_recap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sb0halo_recap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sb0halo_recap"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sbedep",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sbedep)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sbedep"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sbehalo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sbehalo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sbehalo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sbsce",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sbsce)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sbsce"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sbtherm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sbtherm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sbtherm"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sc0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sc0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sc0"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sc0_to_sgas",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sc0_to_sgas)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sc0_to_sgas"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sc_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sc_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sc_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sfsce",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sfsce)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sfsce"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sftherm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sftherm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sftherm"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SFUS_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%SFUS_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: SFUS_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sfus_to_all",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sfus_to_all)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sfus_to_all"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sfus_to_alla",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sfus_to_alla)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sfus_to_alla"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SFUS_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%SFUS_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: SFUS_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SGAS_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%SGAS_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: SGAS_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sgas_to_s",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sgas_to_s)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sgas_to_s"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SGAS_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%SGAS_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: SGAS_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"shot_number",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%shot_number)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: shot_number"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sii_xs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sii_xs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sii_xs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SIMP0_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%SIMP0_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: SIMP0_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"simp0_to_s",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%simp0_to_s)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: simp0_to_s"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SIMP0_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%SIMP0_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: SIMP0_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SIMPI_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%SIMPI_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: SIMPI_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"simpi_to_s",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%simpi_to_s)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: simpi_to_s"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SIMPI_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%SIMPI_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: SIMPI_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SNBI_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%SNBI_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: SNBI_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"snbi_to_all",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%snbi_to_all)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: snbi_to_all"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"snbi_to_alla",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%snbi_to_alla)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: snbi_to_alla"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SNBI_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%SNBI_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: SNBI_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sn_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sn_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sn_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sn_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sn_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sn_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sprof0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sprof0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sprof0"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sprof0e",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sprof0e)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sprof0e"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"spsce",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%spsce)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: spsce"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"squareLO",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%squareLO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: squareLO"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"squareUO",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%squareUO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: squareUO"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sRtcen",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%sRtcen)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: sRtcen"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"surf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%surf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: surf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"S_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%S_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: S_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"s_to_sa",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%s_to_sa)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: s_to_sa"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"S_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%S_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: S_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"t0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%t0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: t0"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"T0cx",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%T0cx)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: T0cx"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"T0sc0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%T0sc0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: T0sc0"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"T0_halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%T0_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: T0_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"T0_reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%T0_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: T0_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF
  CONTAINS


     SUBROUTINE ncerr(status)
        INTEGER, INTENT(in) :: status

        nc_errmsg = NF90_strerror(status)
        write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
     END SUBROUTINE ncerr

END SUBROUTINE ps_update_file_write_7
