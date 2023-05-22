! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_write_def_2(ss,icdf,cclist,nids,iout,iwarn,iertot)


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

  iaddr = ps_shash_D_einj_standard
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%D_einj_standard).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("D_einj_standard","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dim_n_d_einj_standard.EQ.0) THEN
           CALL define_dim(nids%nid_dim_n_d_einj_standard,"dim_n_d_einj_standard",ss%n_d_einj_standard)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_n_d_einj_standard
        CALL define_var("D_einj_standard","R",indim,idims, &
               "D beam table energies", &
               "keV","NBI","MACHINE_DESCRIPTION", &
               "R|units=keV D_einj_standard(n_D_einj_standard)")
     ENDIF
  ENDIF

  iaddr = ps_shash_D_ffull_standard
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%D_ffull_standard).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("D_ffull_standard","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dim_n_d_einj_standard.EQ.0) THEN
           CALL define_dim(nids%nid_dim_n_d_einj_standard,"dim_n_d_einj_standard",ss%n_d_einj_standard)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_n_d_einj_standard
        CALL define_var("D_ffull_standard","R",indim,idims, &
               "D beam full energy fractions", &
               "-","NBI","MACHINE_DESCRIPTION", &
               "R|units=- D_ffull_standard(n_D_einj_standard)")
     ENDIF
  ENDIF

  iaddr = ps_shash_D_fhalf_standard
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%D_fhalf_standard).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("D_fhalf_standard","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dim_n_d_einj_standard.EQ.0) THEN
           CALL define_dim(nids%nid_dim_n_d_einj_standard,"dim_n_d_einj_standard",ss%n_d_einj_standard)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_n_d_einj_standard
        CALL define_var("D_fhalf_standard","R",indim,idims, &
               "D beam half energy fractions", &
               "-","NBI","MACHINE_DESCRIPTION", &
               "R|units=- D_fhalf_standard(n_D_einj_standard)")
     ENDIF
  ENDIF

  iaddr = ps_shash_e0_av
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%e0_av).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("e0_av","GAS")
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
        CALL define_var("e0_av","R",indim,idims, &
               'average energy of neutral sources, "(3/2)*T0"', &
               "KeV","GAS","STATE_DATA", &
               "R|units=KeV   e0_av(ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_necrf_src
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ecrf_src_name).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("ecrf_src_name","EC")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_necrf_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_necrf_src,"dim_necrf_src",ss%necrf_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_necrf_src
        CALL define_var("ecrf_src_name","N",indim,idims, &
               "number & name of ECRF sources", &
               "__NONE","EC","MACHINE_DESCRIPTION", &
               "L|ecrf_source   ecrf_src_name(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_EC_Beam_Elongation
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%EC_Beam_Elongation).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("EC_Beam_Elongation","EC")
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
        CALL define_var("EC_Beam_Elongation","R",indim,idims, &
               "EC beam ellipticity", &
               "-","EC","MACHINE_DESCRIPTION", &
               "R|units=-   EC_Beam_Elongation(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_EC_Code_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("EC_Code_Info","EC")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("EC_Code_Info","C*80",indim,idims, &
               "Information: code implementing EC component", &
               "__NONE","EC","SIMULATION_INIT", &
               "C*80   EC_Code_Info")
     ENDIF
  ENDIF

  iaddr = ps_shash_EC_Data_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("EC_Data_Info","EC")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("EC_Data_Info","C*80",indim,idims, &
               "information on source of ECRF power data", &
               "__NONE","EC","SIMULATION_INIT", &
               "C*80   EC_Data_Info")
     ENDIF
  ENDIF

  iaddr = ps_shash_EC_Half_Power_Angle
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%EC_Half_Power_Angle).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("EC_Half_Power_Angle","EC")
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
        CALL define_var("EC_Half_Power_Angle","R",indim,idims, &
               "Divergence of EC beam", &
               "degrees","EC","MACHINE_DESCRIPTION", &
               "R|units=degrees  EC_Half_Power_Angle(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_EC_Omode_Fraction
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%EC_Omode_Fraction).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("EC_Omode_Fraction","EC")
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
        CALL define_var("EC_Omode_Fraction","R",indim,idims, &
               "O-mode fraction of EC source", &
               "-","EC","SHOT_CONFIGURATION", &
               "R|units=-   EC_Omode_Fraction(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_EC_phi_aim
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%EC_phi_aim).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("EC_phi_aim","EC")
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
        CALL define_var("EC_phi_aim","R",indim,idims, &
               "toroidal aiming angle", &
               "degrees","EC","SHOT_CONFIGURATION", &
               "R|units=degrees  EC_phi_aim(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_EC_PRINC_CURV_HOR
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%EC_PRINC_CURV_HOR).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("EC_PRINC_CURV_HOR","EC")
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
        CALL define_var("EC_PRINC_CURV_HOR","R",indim,idims, &
               "principal curvature in horizontal direction", &
               "m","EC","MACHINE_DESCRIPTION", &
               "R|units=m  EC_PRINC_CURV_HOR(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_EC_PRINC_CURV_VERT
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%EC_PRINC_CURV_VERT).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("EC_PRINC_CURV_VERT","EC")
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
        CALL define_var("EC_PRINC_CURV_VERT","R",indim,idims, &
               "principal curvature in vertical direction", &
               "m","EC","MACHINE_DESCRIPTION", &
               "R|units=m  EC_PRINC_CURV_VERT(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_EC_PRINC_WIDTH_HOR
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("EC_PRINC_WIDTH_HOR","EC")
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
        CALL define_var("EC_PRINC_WIDTH_HOR","R",indim,idims, &
               "principal beam width in horizontal direction", &
               "m","EC","MACHINE_DESCRIPTION", &
               "R|units=m  EC_PRINC_WIDTH_HOR(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_EC_PRINC_WIDTH_VERT
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("EC_PRINC_WIDTH_VERT","EC")
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
        CALL define_var("EC_PRINC_WIDTH_VERT","R",indim,idims, &
               "principal beam width in vertical direction", &
               "m","EC","MACHINE_DESCRIPTION", &
               "R|units=m  EC_PRINC_WIDTH_VERT(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_EC_theta_aim
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%EC_theta_aim).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("EC_theta_aim","EC")
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
        CALL define_var("EC_theta_aim","R",indim,idims, &
               "poloidal aiming angle", &
               "degrees","EC","SHOT_CONFIGURATION", &
               "R|units=degrees  EC_theta_aim(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_ee_mobility_factor
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ee_mobility_factor).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("ee_mobility_factor","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_anomth.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anomth,"dim_nrho_anomth",ss%nrho_anomth)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anomth
        CALL define_var("ee_mobility_factor","R",indim,idims, &
               "chi_e/chi_i", &
               "-","ANOM","STATE_PROFILES", &
               "R|units=-|Hermite  ee_mobility_factor(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Einj_max
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Einj_max).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("Einj_max","NBI")
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
        CALL define_var("Einj_max","R",indim,idims, &
               "maximum injection energy", &
               "keV","NBI","MACHINE_DESCRIPTION", &
               "R|units=keV       Einj_max(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Einj_min
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Einj_min).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("Einj_min","NBI")
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
        CALL define_var("Einj_min","R",indim,idims, &
               "minimum injection energy", &
               "keV","NBI","MACHINE_DESCRIPTION", &
               "R|units=keV       Einj_min(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_eldot
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("eldot","PLASMA")
     ELSE
        indim = 0
        CALL define_var("eldot","R",indim,idims, &
               "[d(Philim)/dt]/[2*Philim]", &
               "1/sec","PLASMA","STATE_DATA", &
               "R|units=1/sec  eldot")
     ENDIF
  ENDIF

  iaddr = ps_phash_elong
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%elong).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("elong","EQ")
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
        CALL define_var("elong","R",indim,idims, &
               "elongation (b/a)", &
               "-","EQ","STATE_PROFILES", &
               "R|pclin  elong(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_eperp_beami
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%eperp_beami).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("eperp_beami","NBI")
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
        CALL define_var("eperp_beami","R",indim,idims, &
               "beam species <Eperp>, lab frame", &
               "keV","NBI","STATE_PROFILES", &
               "R|units=keV|alias=eperp_|step*nbeami eperp_beami(~nrho_nbi,nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_eperp_fusi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%eperp_fusi).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("eperp_fusi","FUS")
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
        CALL define_var("eperp_fusi","R",indim,idims, &
               "fusion ion <Eperp>, lab frame", &
               "keV","FUS","STATE_PROFILES", &
               "R|units=keV|alias=eperp_|step*nfusi eperp_fusi(~nrho_fus,nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_phash_eperp_mini
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%eperp_mini).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("eperp_mini","IC")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_icrf.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_icrf,"dm1_nrho_icrf",ss%nrho_icrf-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_icrf
        IF(nids%nid_dim_nspec_rfmin.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_rfmin,"dim_nspec_rfmin",ss%nspec_rfmin)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_rfmin
        CALL define_var("eperp_mini","R",indim,idims, &
               "minority species <Eperp>, lab frame", &
               "keV","IC","STATE_PROFILES", &
               "R|units=keV|alias=eperp_|step*nmini eperp_mini(~nrho_icrf,nspec_rfmin)")
     ENDIF
  ENDIF

  iaddr = ps_phash_eperp_rw
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%eperp_rw).AND.ichange) THEN
     IF(cclist(ps_cnum_RUNAWAY).NE.1) THEN
        CALL ccwarn("eperp_rw","RUNAWAY")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_rw.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_rw,"dm1_nrho_rw",ss%nrho_rw-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_rw
        CALL define_var("eperp_rw","R",indim,idims, &
               "runaway electron <Eperp>", &
               "keV","RUNAWAY","STATE_PROFILES", &
               "R|units=keV|step*nrw  eperp_rw(~nrho_rw)")
     ENDIF
  ENDIF

  iaddr = ps_phash_epll_beami
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%epll_beami).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("epll_beami","NBI")
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
        CALL define_var("epll_beami","R",indim,idims, &
               "beam species <Epll>, lab frame", &
               "keV","NBI","STATE_PROFILES", &
               "R|units=keV|alias=epll_|step*nbeami  epll_beami(~nrho_nbi,nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_epll_fusi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%epll_fusi).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("epll_fusi","FUS")
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
        CALL define_var("epll_fusi","R",indim,idims, &
               "fusion ion <Epll>, lab frame", &
               "keV","FUS","STATE_PROFILES", &
               "R|units=keV|alias=epll_|step*nfusi  epll_fusi(~nrho_fus,nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_phash_epll_mini
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%epll_mini).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("epll_mini","IC")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_icrf.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_icrf,"dm1_nrho_icrf",ss%nrho_icrf-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_icrf
        IF(nids%nid_dim_nspec_rfmin.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_rfmin,"dim_nspec_rfmin",ss%nspec_rfmin)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_rfmin
        CALL define_var("epll_mini","R",indim,idims, &
               "minority species <Epll>, lab frame", &
               "keV","IC","STATE_PROFILES", &
               "R|units=keV|alias=epll_|step*nmini  epll_mini(~nrho_icrf,nspec_rfmin)")
     ENDIF
  ENDIF

  iaddr = ps_phash_epll_rw
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%epll_rw).AND.ichange) THEN
     IF(cclist(ps_cnum_RUNAWAY).NE.1) THEN
        CALL ccwarn("epll_rw","RUNAWAY")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_rw.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_rw,"dm1_nrho_rw",ss%nrho_rw-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_rw
        CALL define_var("epll_rw","R",indim,idims, &
               "runaway electron <Epll>", &
               "keV","RUNAWAY","STATE_PROFILES", &
               "R|units=keV|step*nrw  epll_rw(~nrho_rw)")
     ENDIF
  ENDIF

  iaddr = ps_phash_Epot
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Epot).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Epot","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho,"dim_nrho",ss%nrho)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho
        CALL define_var("Epot","R",indim,idims, &
               "radial electrostatic potential", &
               "keV","PLASMA","STATE_PROFILES", &
               "R|units=keV|pclin  Epot(nrho)")
     ENDIF
  ENDIF

  iaddr = ps_shash_eqdsk_file
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("eqdsk_file","EQ")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_256
        CALL define_var("eqdsk_file","F",indim,idims, &
               "EFIT G-eqdsk file: psi(R,Z), g(psi), etc.", &
               "__NONE","EQ","STATE_DATA", &
               "F  eqdsk_file")
     ENDIF
  ENDIF

  iaddr = ps_dhash_neqmom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%eqmom_num).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("eqmom_num","EQ")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_neqmom.EQ.0) THEN
           CALL define_dim(nids%nid_dim_neqmom,"dim_neqmom",ss%neqmom)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_neqmom
        CALL define_var("eqmom_num","N",indim,idims, &
               "Equilibrium moments", &
               "__NONE","EQ","SIMULATION_INIT", &
               "L|EQ_moments  eqmom_num(neqmom)")
     ENDIF
  ENDIF

  iaddr = ps_shash_EQ_Code_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("EQ_Code_Info","EQ")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("EQ_Code_Info","C*80",indim,idims, &
               "Information: code implementing EQ component", &
               "__NONE","EQ","SIMULATION_INIT", &
               "C*80   EQ_Code_Info")
     ENDIF
  ENDIF

  iaddr = ps_shash_EQ_Data_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("EQ_Data_Info","EQ")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("EQ_Data_Info","C*80",indim,idims, &
               "Information: source of EQ input data", &
               "__NONE","EQ","SIMULATION_INIT", &
               "C*80   EQ_Data_Info")
     ENDIF
  ENDIF

  iaddr = ps_phash_eta_parallel
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%eta_parallel).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("eta_parallel","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho,"dim_nrho",ss%nrho)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho
        CALL define_var("eta_parallel","R",indim,idims, &
               "parallel electrical resistivity", &
               "ohm*m","PLASMA","STATE_PROFILES", &
               "R|units=ohm*m|Hermite eta_parallel(nrho)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nefi_anom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%E_anom).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("E_anom","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nefi_anom.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nefi_anom,"dim_nefi_anom",ss%nefi_anom)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nefi_anom
        CALL define_var("E_anom","R",indim,idims, &
               "(optional) energy grid (1d profiles)", &
               "keV","ANOM","SIMULATION_INIT", &
               "G  E_anom(nefi_anom)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nefi_anom2
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%E_anom2).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("E_anom2","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nefi_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nefi_anom2,"dim_nefi_anom2",ss%nefi_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nefi_anom2
        CALL define_var("E_anom2","R",indim,idims, &
               "energy grid (2d anomalous transport profiles)", &
               "keV","ANOM","SIMULATION_INIT", &
               "G  E_anom2(nefi_anom2)")
     ENDIF
  ENDIF

  iaddr = ps_phash_fidiff_bpass_co
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%fidiff_bpass_co).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("fidiff_bpass_co","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nefi_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nefi_anom2,"dim_nefi_anom2",ss%nefi_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nefi_anom2
        IF(nids%nid_dim_nrho_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anom2,"dim_nrho_anom2",ss%nrho_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anom2
        IF(nids%nid_dim_nspec_fusion.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_fusion,"dim_nspec_fusion",ss%nspec_fusion)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_fusion
        CALL define_var("fidiff_bpass_co","R",indim,idims, &
               "fusion ion diffusivity: co barely passing orbits", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  fidiff_bpass_co(nefi_anom2,nrho_anom2,nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_phash_fidiff_bpass_ctr
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%fidiff_bpass_ctr).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("fidiff_bpass_ctr","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nefi_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nefi_anom2,"dim_nefi_anom2",ss%nefi_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nefi_anom2
        IF(nids%nid_dim_nrho_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anom2,"dim_nrho_anom2",ss%nrho_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anom2
        IF(nids%nid_dim_nspec_fusion.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_fusion,"dim_nspec_fusion",ss%nspec_fusion)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_fusion
        CALL define_var("fidiff_bpass_ctr","R",indim,idims, &
               "fusion ion diffusivity: ctr barely passing orbits", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  fidiff_bpass_ctr(nefi_anom2,nrho_anom2,nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_phash_fidiff_btrap
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%fidiff_btrap).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("fidiff_btrap","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nefi_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nefi_anom2,"dim_nefi_anom2",ss%nefi_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nefi_anom2
        IF(nids%nid_dim_nrho_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anom2,"dim_nrho_anom2",ss%nrho_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anom2
        IF(nids%nid_dim_nspec_fusion.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_fusion,"dim_nspec_fusion",ss%nspec_fusion)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_fusion
        CALL define_var("fidiff_btrap","R",indim,idims, &
               "fusion ion diffusivity: barely trapped orbits", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  fidiff_btrap(nefi_anom2,nrho_anom2,nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_phash_fidiff_dpass_co
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%fidiff_dpass_co).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("fidiff_dpass_co","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nefi_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nefi_anom2,"dim_nefi_anom2",ss%nefi_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nefi_anom2
        IF(nids%nid_dim_nrho_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anom2,"dim_nrho_anom2",ss%nrho_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anom2
        IF(nids%nid_dim_nspec_fusion.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_fusion,"dim_nspec_fusion",ss%nspec_fusion)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_fusion
        CALL define_var("fidiff_dpass_co","R",indim,idims, &
               "fusion ion diffusivity: co passing |vpll/v| -> 1", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  fidiff_dpass_co(nefi_anom2,nrho_anom2,nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_phash_fidiff_dpass_ctr
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%fidiff_dpass_ctr).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("fidiff_dpass_ctr","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nefi_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nefi_anom2,"dim_nefi_anom2",ss%nefi_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nefi_anom2
        IF(nids%nid_dim_nrho_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anom2,"dim_nrho_anom2",ss%nrho_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anom2
        IF(nids%nid_dim_nspec_fusion.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_fusion,"dim_nspec_fusion",ss%nspec_fusion)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_fusion
        CALL define_var("fidiff_dpass_ctr","R",indim,idims, &
               "fusion ion diffusivity: ctr passing |vpll/v| -> 1", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  fidiff_dpass_ctr(nefi_anom2,nrho_anom2,nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_phash_fidiff_dtrap
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%fidiff_dtrap).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("fidiff_dtrap","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nefi_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nefi_anom2,"dim_nefi_anom2",ss%nefi_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nefi_anom2
        IF(nids%nid_dim_nrho_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anom2,"dim_nrho_anom2",ss%nrho_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anom2
        IF(nids%nid_dim_nspec_fusion.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_fusion,"dim_nspec_fusion",ss%nspec_fusion)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_fusion
        CALL define_var("fidiff_dtrap","R",indim,idims, &
               "fusion ion diffusivity: deeply trapped orbits", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  fidiff_dtrap(nefi_anom2,nrho_anom2,nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_phash_Fi_depletion
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Fi_depletion).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Fi_depletion","PLASMA")
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
        CALL define_var("Fi_depletion","R",indim,idims, &
               "fast ion depletion: sum[nj*Zj/ne]", &
               "-","PLASMA","STATE_PROFILES", &
               "R|units=-|step*ns(:,ps_elec_index) Fi_depletion(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_shash_fracmin
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%fracmin).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("fracmin","IC")
     ELSE
        indim = 0
        IF(nids%nid_dim_nspec_rfmin.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_rfmin,"dim_nspec_rfmin",ss%nspec_rfmin)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_rfmin
        CALL define_var("fracmin","R",indim,idims, &
               'minority density fraction if kdens_rfmin="fraction"', &
               "-","IC","SIMULATION_INIT", &
               "R|units=- fracmin(nspec_rfmin)")
     ENDIF
  ENDIF

  iaddr = ps_shash_fracpslh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%fracpslh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("fracpslh","LH")
     ELSE
        indim = 0
        IF(nids%nid_dim_max_nspectrumlh_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_max_nspectrumlh_src,"dim_max_nspectrumlh_src",ss%max_nspectrumlh_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_max_nspectrumlh_src
        IF(nids%nid_dim_nlhrf_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nlhrf_src,"dim_nlhrf_src",ss%nlhrf_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nlhrf_src
        CALL define_var("fracpslh","R",indim,idims, &
               "fraction of power on each peak (the sum over all peaks must be 1)", &
               "-","LH","SHOT_CONFIGURATION", &
               "R|units=- fracpslh(nspectrumlh_src(),nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_frac_full
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%frac_full).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("frac_full","NBI")
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
        CALL define_var("frac_full","R",indim,idims, &
               "fraction of beam current at full voltage", &
               "-","NBI","STATE_DATA", &
               "R|units=-  frac_full(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_frac_half
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%frac_half).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("frac_half","NBI")
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
        CALL define_var("frac_half","R",indim,idims, &
               "fraction of beam current at half voltage", &
               "-","NBI","STATE_DATA", &
               "R|units=-  frac_half(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_freq_drbm
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%freq_drbm).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("freq_drbm","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_anomth.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anomth,"dim_nrho_anomth",ss%nrho_anomth)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anomth
        CALL define_var("freq_drbm","R",indim,idims, &
               "frequency from DRBM model", &
               "1/sec","ANOM","STATE_PROFILES", &
               "R|units=1/sec|pclin freq_drbm(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_freq_ec
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%freq_ec).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("freq_ec","EC")
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
        CALL define_var("freq_ec","R",indim,idims, &
               "frequency of EC source", &
               "Hz","EC","SHOT_CONFIGURATION", &
               "R|units=Hz freq_ec(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_freq_etg
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%freq_etg).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("freq_etg","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_anomth.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anomth,"dim_nrho_anomth",ss%nrho_anomth)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anomth
        CALL define_var("freq_etg","R",indim,idims, &
               "frequency from ETG model", &
               "1/sec","ANOM","STATE_PROFILES", &
               "R|units=1/sec|pclin freq_etg(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_freq_glf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%freq_glf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("freq_glf","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_anomth.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anomth,"dim_nrho_anomth",ss%nrho_anomth)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anomth
        CALL define_var("freq_glf","R",indim,idims, &
               "frequency from GLF23 model", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin freq_glf(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_freq_ic
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%freq_ic).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("freq_ic","IC")
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
        CALL define_var("freq_ic","R",indim,idims, &
               "frequency on each ICRF source", &
               "Hz","IC","SHOT_CONFIGURATION", &
               "R|units=Hz freq_ic(nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_freq_lh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%freq_lh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("freq_lh","LH")
     ELSE
        indim = 0
        IF(nids%nid_dim_nlhrf_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nlhrf_src,"dim_nlhrf_src",ss%nlhrf_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nlhrf_src
        CALL define_var("freq_lh","R",indim,idims, &
               "frequency on each LHRF source", &
               "Hz","LH","SHOT_CONFIGURATION", &
               "R|units=Hz freq_lh(nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_freq_max_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%freq_max_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("freq_max_tglf","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_anomth.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anomth,"dim_nrho_anomth",ss%nrho_anomth)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anomth
        CALL define_var("freq_max_tglf","R",indim,idims, &
               "frequency corresponding maximum growthrate from TGLF model", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin freq_max_tglf(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_freq_mmm71
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%freq_mmm71).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("freq_mmm71","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_anomth.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anomth,"dim_nrho_anomth",ss%nrho_anomth)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anomth
        IF(nids%nid_dim_nky_mmm.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nky_mmm,"dim_nky_mmm",ss%nky_mmm)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nky_mmm
        CALL define_var("freq_mmm71","R",indim,idims, &
               "frequency from mmm71 model", &
               "1/sec","ANOM","STATE_PROFILES", &
               "R|units=1/sec|pclin freq_mmm71(nrho_anomth,nky_mmm)")
     ENDIF
  ENDIF

  iaddr = ps_phash_freq_mtm
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%freq_mtm).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("freq_mtm","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_anomth.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anomth,"dim_nrho_anomth",ss%nrho_anomth)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anomth
        CALL define_var("freq_mtm","R",indim,idims, &
               "frequency from MTM model", &
               "1/sec","ANOM","STATE_PROFILES", &
               "R|units=1/sec|pclin freq_mtm(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_freq_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%freq_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("freq_tglf","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_anomth.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anomth,"dim_nrho_anomth",ss%nrho_anomth)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anomth
        IF(nids%nid_dim_nky_anomth.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nky_anomth,"dim_nky_anomth",ss%nky_anomth)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nky_anomth
        CALL define_var("freq_tglf","R",indim,idims, &
               "frequency from TGLF model", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin freq_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_FUS_Code_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("FUS_Code_Info","FUS")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("FUS_Code_Info","C*80",indim,idims, &
               "Information: code implementing FUS component", &
               "__NONE","FUS","SIMULATION_INIT", &
               "C*80   FUS_Code_Info")
     ENDIF
  ENDIF

  iaddr = ps_shash_gamma_lmhd
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gamma_lmhd).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("gamma_lmhd","LMHD")
     ELSE
        indim = 0
        IF(nids%nid_dim_mmodes.EQ.0) THEN
           CALL define_dim(nids%nid_dim_mmodes,"dim_mmodes",ss%mmodes)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_mmodes
        IF(nids%nid_dim_ndelta_w.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ndelta_w,"dim_ndelta_w",ss%ndelta_w)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ndelta_w
        CALL define_var("gamma_lmhd","R",indim,idims, &
               "ideal growth rate", &
               "-","LMHD","STATE_DATA", &
               "R|units=-  gamma_lmhd(mmodes,ndelta_W)")
     ENDIF
  ENDIF

  iaddr = ps_phash_gamma_nc
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gamma_nc).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("gamma_nc","EQ")
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
        CALL define_var("gamma_nc","R",indim,idims, &
               "NC gamma", &
               "m^-1","EQ","STATE_PROFILES", &
               "R|units=m^-1|pclin   gamma_nc(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_shash_gas_atom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gas_atom).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("gas_atom","GAS")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_ngsc0.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ngsc0,"dim_ngsc0",ss%ngsc0)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ngsc0
        CALL define_var("gas_atom","N",indim,idims, &
               "name of species introduced by each gas source", &
               "__NONE","GAS","SHOT_CONFIGURATION", &
               "N  gas_atom(ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_shash_GAS_Code_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("GAS_Code_Info","GAS")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("GAS_Code_Info","C*80",indim,idims, &
               "Information: code implementing GAS component", &
               "__NONE","GAS","SIMULATION_INIT", &
               "C*80   GAS_Code_Info")
     ENDIF
  ENDIF

  iaddr = ps_phash_gb1
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gb1).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("gb1","EQ")
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
        CALL define_var("gb1","R",indim,idims, &
               "<|B|>", &
               "T","EQ","STATE_PROFILES", &
               "R|units=T|pclin      gb1(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_gb2
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gb2).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("gb2","EQ")
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
        CALL define_var("gb2","R",indim,idims, &
               "<B^2>", &
               "T^2","EQ","STATE_PROFILES", &
               "R|units=T^2|pclin    gb2(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_gb2i
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gb2i).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("gb2i","EQ")
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
        CALL define_var("gb2i","R",indim,idims, &
               "<1/B^2>", &
               "T^-2","EQ","STATE_PROFILES", &
               "R|units=T^-2|pclin   gb2i(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_gbr2
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gbr2).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("gbr2","EQ")
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
        CALL define_var("gbr2","R",indim,idims, &
               "<|B|*R^2>", &
               "T*m^2","EQ","STATE_PROFILES", &
               "R|units=T*m^2|pclin  gbr2(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_shash_geometry
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("geometry","EQ")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_256
        CALL define_var("geometry","F",indim,idims, &
               "path to file(s) vac. vessel & coil descr.", &
               "__NONE","EQ","MACHINE_DESCRIPTION", &
               "F  geometry")
     ENDIF
  ENDIF

  iaddr = ps_shash_Global_label
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Global_label","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("Global_label","C*80",indim,idims, &
               "Run Label", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "C*80   Global_label")
     ENDIF
  ENDIF

  iaddr = ps_phash_gncfb2h
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gncfb2h).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("gncfb2h","EQ")
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
        CALL define_var("gncfb2h","R",indim,idims, &
               "<(B^-2)*((1-H)^0.5-(1/3)*(1-H)^1.5)>; H=B/Bmax", &
               "T^-2","EQ","STATE_PROFILES", &
               "R|units=T^-2|pclin  gncfb2h(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_gncfh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gncfh).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("gncfh","EQ")
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
        CALL define_var("gncfh","R",indim,idims, &
               "<H^-2*(1-SQRT(1-H)*(1+H/2))>; H=B/Bmax", &
               "-","EQ","STATE_PROFILES", &
               "R|units=-|pclin  gncfh(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_gr1
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gr1).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("gr1","EQ")
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
        CALL define_var("gr1","R",indim,idims, &
               "<R>", &
               "m","EQ","STATE_PROFILES", &
               "R|units=m|pclin   gr1(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_gr2
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gr2).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("gr2","EQ")
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
        CALL define_var("gr2","R",indim,idims, &
               "<R^2>", &
               "m^2","EQ","STATE_PROFILES", &
               "R|units=m^2|pclin gr2(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_gr2i
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gr2i).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("gr2i","EQ")
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
        CALL define_var("gr2i","R",indim,idims, &
               "<R^-2>", &
               "m^-2","EQ","STATE_PROFILES", &
               "R|units=m^-2|pclin gr2i(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_gr2rho2
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gr2rho2).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("gr2rho2","EQ")
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
        CALL define_var("gr2rho2","R",indim,idims, &
               "<R^2*|grad(rho)|^2>", &
               "-","EQ","STATE_PROFILES", &
               "R|units=-|pclin gr2rho2(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_gr3i
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%gr3i).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("gr3i","EQ")
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
        CALL define_var("gr3i","R",indim,idims, &
               "<R^-3>", &
               "m^-3","EQ","STATE_PROFILES", &
               "R|units=m^-3|pclin gr3i(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_grho1
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%grho1).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("grho1","EQ")
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
        CALL define_var("grho1","R",indim,idims, &
               "<|grad(rho)|>", &
               "m^-1","EQ","STATE_PROFILES", &
               "R|units=m^-1|pclin grho1(nrho_eq_geo)")
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

END SUBROUTINE ps_update_file_write_def_2

SUBROUTINE ps_update_file_write_2(ss,icdf,cclist,iucount,iout,iwarn,iertot)


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


  status = nf90_inq_varid(icdf,"D_einj_standard",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%D_einj_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: D_einj_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"D_ffull_standard",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%D_ffull_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: D_ffull_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"D_fhalf_standard",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%D_fhalf_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: D_fhalf_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"e0_av",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%e0_av)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: e0_av"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ecrf_src_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ecrf_src_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ecrf_src_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Beam_Elongation",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%EC_Beam_Elongation)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: EC_Beam_Elongation"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%EC_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: EC_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%EC_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: EC_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Half_Power_Angle",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%EC_Half_Power_Angle)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: EC_Half_Power_Angle"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Omode_Fraction",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%EC_Omode_Fraction)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: EC_Omode_Fraction"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_phi_aim",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%EC_phi_aim)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: EC_phi_aim"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_PRINC_CURV_HOR",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%EC_PRINC_CURV_HOR)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: EC_PRINC_CURV_HOR"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_PRINC_CURV_VERT",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%EC_PRINC_CURV_VERT)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: EC_PRINC_CURV_VERT"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_PRINC_WIDTH_HOR",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%EC_PRINC_WIDTH_HOR)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: EC_PRINC_WIDTH_HOR"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_PRINC_WIDTH_VERT",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%EC_PRINC_WIDTH_VERT)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: EC_PRINC_WIDTH_VERT"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_theta_aim",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%EC_theta_aim)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: EC_theta_aim"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ee_mobility_factor",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ee_mobility_factor)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ee_mobility_factor"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Einj_max",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Einj_max)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Einj_max"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Einj_min",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Einj_min)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Einj_min"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eldot",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%eldot)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: eldot"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"elong",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%elong)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: elong"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eperp_beami",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%eperp_beami)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: eperp_beami"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eperp_fusi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%eperp_fusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: eperp_fusi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eperp_mini",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%eperp_mini)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: eperp_mini"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eperp_rw",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%eperp_rw)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: eperp_rw"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"epll_beami",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%epll_beami)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: epll_beami"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"epll_fusi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%epll_fusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: epll_fusi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"epll_mini",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%epll_mini)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: epll_mini"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"epll_rw",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%epll_rw)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: epll_rw"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Epot",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Epot)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Epot"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eqdsk_file",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%eqdsk_file)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: eqdsk_file"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eqmom_num",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%eqmom_num)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: eqmom_num"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EQ_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%EQ_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: EQ_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EQ_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%EQ_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: EQ_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eta_parallel",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%eta_parallel)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: eta_parallel"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"E_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%E_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: E_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"E_anom2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%E_anom2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: E_anom2"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fidiff_bpass_co",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%fidiff_bpass_co)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: fidiff_bpass_co"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fidiff_bpass_ctr",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%fidiff_bpass_ctr)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: fidiff_bpass_ctr"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fidiff_btrap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%fidiff_btrap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: fidiff_btrap"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fidiff_dpass_co",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%fidiff_dpass_co)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: fidiff_dpass_co"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fidiff_dpass_ctr",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%fidiff_dpass_ctr)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: fidiff_dpass_ctr"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fidiff_dtrap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%fidiff_dtrap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: fidiff_dtrap"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Fi_depletion",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Fi_depletion)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Fi_depletion"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fracmin",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%fracmin)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: fracmin"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fracpslh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%fracpslh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: fracpslh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"frac_full",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%frac_full)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: frac_full"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"frac_half",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%frac_half)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: frac_half"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_drbm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%freq_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: freq_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_ec",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%freq_ec)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: freq_ec"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_etg",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%freq_etg)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: freq_etg"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_glf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%freq_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: freq_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_ic",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%freq_ic)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: freq_ic"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_lh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%freq_lh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: freq_lh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_max_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%freq_max_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: freq_max_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_mmm71",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%freq_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: freq_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_mtm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%freq_mtm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: freq_mtm"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%freq_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: freq_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"FUS_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%FUS_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: FUS_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gamma_lmhd",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gamma_lmhd)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gamma_lmhd"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gamma_nc",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gamma_nc)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gamma_nc"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gas_atom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gas_atom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gas_atom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"GAS_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%GAS_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: GAS_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gb1",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gb1)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gb1"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gb2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gb2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gb2"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gb2i",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gb2i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gb2i"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gbr2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gbr2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gbr2"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"geometry",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%geometry)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: geometry"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Global_label",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Global_label)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Global_label"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gncfb2h",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gncfb2h)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gncfb2h"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gncfh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gncfh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gncfh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gr1",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gr1)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gr1"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gr2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gr2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gr2"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gr2i",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gr2i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gr2i"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gr2rho2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gr2rho2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gr2rho2"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gr3i",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%gr3i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: gr3i"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"grho1",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%grho1)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: grho1"
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

END SUBROUTINE ps_update_file_write_2
