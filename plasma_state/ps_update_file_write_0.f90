! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_write_def_0(ss,icdf,cclist,nids,iout,iwarn,iertot)


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

  iaddr = ps_shash_alla_index
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%alla_index).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("alla_index","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dp1_nspec_alla.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_alla,"dp1_nspec_alla",ss%nspec_alla+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_alla
        CALL define_var("alla_index","I",indim,idims, &
               "species index in original lists", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "I alla_index(0:nspec_alla)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_alla
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ALLA_name).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("ALLA_name","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dp1_nspec_alla.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_alla,"dp1_nspec_alla",ss%nspec_alla+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_alla
        CALL define_var("ALLA_name","N",indim,idims, &
               "all species abridged", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "S|specie  ALLA(0:nspec_alla)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_alla
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ALLA_type).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("ALLA_type","PLASMA")
     ELSE
        indim = 0
        IF(nids%nid_dp1_nspec_alla.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_alla,"dp1_nspec_alla",ss%nspec_alla+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_alla
        CALL define_var("ALLA_type","I",indim,idims, &
               "ALLA specie types", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "S|specie  ALLA(0:nspec_alla)")
     ENDIF
  ENDIF

  iaddr = ps_shash_all_index
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%all_index).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("all_index","PLASMA")
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
        CALL define_var("all_index","I",indim,idims, &
               "species index in original lists", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "I all_index(0:nspec_all)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_all
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ALL_name).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("ALL_name","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dp1_nspec_all.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_all,"dp1_nspec_all",ss%nspec_all+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_all
        CALL define_var("ALL_name","N",indim,idims, &
               "all species", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "S|specie  ALL(0:nspec_all)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_all
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ALL_type).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("ALL_type","PLASMA")
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
        CALL define_var("ALL_type","I",indim,idims, &
               "ALL specie types", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "S|specie  ALL(0:nspec_all)")
     ENDIF
  ENDIF

  iaddr = ps_shash_amaxlh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%amaxlh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("amaxlh","LH")
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
        CALL define_var("amaxlh","R",indim,idims, &
               "width of the spectrum peak", &
               "-","LH","SHOT_CONFIGURATION", &
               "R|units=- amaxlh(nspectrumlh_src(),nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_aminlh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%aminlh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("aminlh","LH")
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
        CALL define_var("aminlh","R",indim,idims, &
               "parallel mode number, center of the spectrum peak", &
               "-","LH","SHOT_CONFIGURATION", &
               "R|units=- aminlh(nspectrumlh_src(),nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Ang_Href_coil
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Ang_Href_coil).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Ang_Href_coil","EQ")
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
        CALL define_var("Ang_Href_coil","R",indim,idims, &
               "angle w.r.t. horizontal of coil base", &
               "degrees","EQ","MACHINE_DESCRIPTION", &
               "R|units=degrees Ang_Href_coil(ncoils)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Ang_Vref_coil
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Ang_Vref_coil).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Ang_Vref_coil","EQ")
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
        CALL define_var("Ang_Vref_coil","R",indim,idims, &
               "angle w.r.t. vertical of coil sides", &
               "degrees","EQ","MACHINE_DESCRIPTION", &
               "R|units=degrees Ang_Vref_coil(ncoils)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ANOM_Code_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("ANOM_Code_Info","ANOM")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("ANOM_Code_Info","C*80",indim,idims, &
               "Information: code implementing ANOM component", &
               "__NONE","ANOM","SIMULATION_INIT", &
               "C*80   ANOM_Code_Info")
     ENDIF
  ENDIF

  iaddr = ps_phash_anom_evar
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%anom_evar).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("anom_evar","ANOM")
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
        CALL define_var("anom_evar","R",indim,idims, &
               "anomalous transport energy variation", &
               "-","ANOM","STATE_PROFILES", &
               "R|pclin|units=- anom_evar(nefi_anom)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ant_model
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ant_model).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("ant_model","IC")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_256
        IF(nids%nid_dim_nicrf_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nicrf_src,"dim_nicrf_src",ss%nicrf_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nicrf_src
        CALL define_var("ant_model","F",indim,idims, &
               "antenna model filenames (1 per antenna source)", &
               "__NONE","IC","MACHINE_DESCRIPTION", &
               "F ant_model(nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ap2_halfheight
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ap2_halfheight).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("ap2_halfheight","NBI")
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
        CALL define_var("ap2_halfheight","R",indim,idims, &
               "aperture half-height", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m         ap2_halfheight(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ap2_halfwidth
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ap2_halfwidth).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("ap2_halfwidth","NBI")
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
        CALL define_var("ap2_halfwidth","R",indim,idims, &
               "2nd aperture half-width", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m         ap2_halfwidth(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ap2_horiz_offset
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ap2_horiz_offset).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("ap2_horiz_offset","NBI")
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
        CALL define_var("ap2_horiz_offset","R",indim,idims, &
               "horizontal offset, 2nd aperture", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m         ap2_horiz_offset(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ap2_vert_offset
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ap2_vert_offset).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("ap2_vert_offset","NBI")
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
        CALL define_var("ap2_vert_offset","R",indim,idims, &
               "vertical offset, 2nd aperture", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m         ap2_vert_offset(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_apolmaxlh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%apolmaxlh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("apolmaxlh","LH")
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
        CALL define_var("apolmaxlh","R",indim,idims, &
               "poloidal mode number right of lumps", &
               "-","LH","SHOT_CONFIGURATION", &
               "R|units=- apolmaxlh(nspectrumlh_src(),nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_apolminlh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%apolminlh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("apolminlh","LH")
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
        CALL define_var("apolminlh","R",indim,idims, &
               "poloidal mode number left of lumps", &
               "-","LH","SHOT_CONFIGURATION", &
               "R|units=- apolminlh(nspectrumlh_src(),nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ap_halfheight
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ap_halfheight).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("ap_halfheight","NBI")
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
        CALL define_var("ap_halfheight","R",indim,idims, &
               "aperture half-height", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m         ap_halfheight(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ap_halfwidth
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ap_halfwidth).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("ap_halfwidth","NBI")
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
        CALL define_var("ap_halfwidth","R",indim,idims, &
               "aperture half-width", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m         ap_halfwidth(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ap_horiz_offset
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ap_horiz_offset).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("ap_horiz_offset","NBI")
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
        CALL define_var("ap_horiz_offset","R",indim,idims, &
               "horizontal offset of aperture", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m         ap_horiz_offset(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ap_vert_offset
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ap_vert_offset).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("ap_vert_offset","NBI")
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
        CALL define_var("ap_vert_offset","R",indim,idims, &
               "vertical offset of aperture", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m         ap_vert_offset(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_area
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%area).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("area","EQ")
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
        CALL define_var("area","R",indim,idims, &
               "enclosed area", &
               "m^2","EQ","STATE_PROFILES", &
               "R|units=m^2|Spline_00 area(nrho_eq)")
     ENDIF
  ENDIF

  iaddr = ps_shash_atormaxlh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%atormaxlh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("atormaxlh","LH")
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
        CALL define_var("atormaxlh","R",indim,idims, &
               "toroidal mode number right of lumps", &
               "-","LH","SHOT_CONFIGURATION", &
               "R|units=- atormaxlh(nspectrumlh_src(),nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_atorminlh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%atorminlh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("atorminlh","LH")
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
        CALL define_var("atorminlh","R",indim,idims, &
               "toroidal mode number left of lumps", &
               "-","LH","SHOT_CONFIGURATION", &
               "R|units=- atorminlh(nspectrumlh_src(),nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nballoon
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Balloon_code).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("Balloon_code","LMHD")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_nballoon.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nballoon,"dim_nballoon",ss%nballoon)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nballoon
        CALL define_var("Balloon_code","N",indim,idims, &
               "name(s) of Ballooning code(s) used", &
               "__NONE","LMHD","SIMULATION_INIT", &
               "L|Balloon_codes  Balloon_code(nballoon)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Balloon_mapper
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Balloon_mapper).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("Balloon_mapper","LMHD")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_nballoon.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nballoon,"dim_nballoon",ss%nballoon)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nballoon
        CALL define_var("Balloon_mapper","N",indim,idims, &
               "mapper code used for each ballooning code", &
               "__NONE","LMHD","SIMULATION_INIT", &
               "N  Balloon_mapper(nballoon)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Balloon_stable
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Balloon_stable).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("Balloon_stable","LMHD")
     ELSE
        indim = 0
        IF(nids%nid_dim_nballoon.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nballoon,"dim_nballoon",ss%nballoon)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nballoon
        CALL define_var("Balloon_stable","I",indim,idims, &
               "0: stable everywhere; =1: unstable somewhere", &
               "__NONE","LMHD","STATE_DATA", &
               "I   Balloon_stable(nballoon)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Balloon_status
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Balloon_status).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("Balloon_status","LMHD")
     ELSE
        indim = 0
        IF(nids%nid_dim_nballoon.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nballoon,"dim_nballoon",ss%nballoon)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nballoon
        CALL define_var("Balloon_status","I",indim,idims, &
               "completion status: Ballooning codes", &
               "__NONE","LMHD","STATE_DATA", &
               "I   Balloon_status(nballoon)")
     ENDIF
  ENDIF

  iaddr = ps_phash_balstab
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%balstab).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("balstab","LMHD")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_lmhd.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_lmhd,"dim_nrho_lmhd",ss%nrho_lmhd)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_lmhd
        CALL define_var("balstab","R",indim,idims, &
               "Ballooning/Mercier stability indicator", &
               "-","LMHD","STATE_PROFILES", &
               "R|units=-|pclin balstab(nrho_lmhd)")
     ENDIF
  ENDIF

  iaddr = ps_shash_beam_type
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%beam_type).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("beam_type","NBI")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_nbeam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nbeam,"dim_nbeam",ss%nbeam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nbeam
        CALL define_var("beam_type","N",indim,idims, &
               "beam type:", &
               "__NONE","NBI","MACHINE_DESCRIPTION", &
               "N     beam_type(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_bidiff_bpass_co
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%bidiff_bpass_co).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("bidiff_bpass_co","ANOM")
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
        IF(nids%nid_dim_nspec_beam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_beam,"dim_nspec_beam",ss%nspec_beam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_beam
        CALL define_var("bidiff_bpass_co","R",indim,idims, &
               "beam ion diffusivity: co barely passing orbits", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  bidiff_bpass_co(nefi_anom2,nrho_anom2,nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_bidiff_bpass_ctr
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%bidiff_bpass_ctr).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("bidiff_bpass_ctr","ANOM")
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
        IF(nids%nid_dim_nspec_beam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_beam,"dim_nspec_beam",ss%nspec_beam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_beam
        CALL define_var("bidiff_bpass_ctr","R",indim,idims, &
               "beam ion diffusivity: ctr barely passing orbits", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  bidiff_bpass_ctr(nefi_anom2,nrho_anom2,nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_bidiff_btrap
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%bidiff_btrap).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("bidiff_btrap","ANOM")
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
        IF(nids%nid_dim_nspec_beam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_beam,"dim_nspec_beam",ss%nspec_beam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_beam
        CALL define_var("bidiff_btrap","R",indim,idims, &
               "beam ion diffusivity: barely trapped orbits", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  bidiff_btrap(nefi_anom2,nrho_anom2,nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_bidiff_dpass_co
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%bidiff_dpass_co).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("bidiff_dpass_co","ANOM")
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
        IF(nids%nid_dim_nspec_beam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_beam,"dim_nspec_beam",ss%nspec_beam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_beam
        CALL define_var("bidiff_dpass_co","R",indim,idims, &
               "beam ion diffusivity: co passing |vpll/v| -> 1", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  bidiff_dpass_co(nefi_anom2,nrho_anom2,nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_bidiff_dpass_ctr
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%bidiff_dpass_ctr).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("bidiff_dpass_ctr","ANOM")
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
        IF(nids%nid_dim_nspec_beam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_beam,"dim_nspec_beam",ss%nspec_beam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_beam
        CALL define_var("bidiff_dpass_ctr","R",indim,idims, &
               "beam ion diffusivity: ctr passing |vpll/v| -> 1", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  bidiff_dpass_ctr(nefi_anom2,nrho_anom2,nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_bidiff_dtrap
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%bidiff_dtrap).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("bidiff_dtrap","ANOM")
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
        IF(nids%nid_dim_nspec_beam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_beam,"dim_nspec_beam",ss%nspec_beam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_beam
        CALL define_var("bidiff_dtrap","R",indim,idims, &
               "beam ion diffusivity: deeply trapped orbits", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  bidiff_dtrap(nefi_anom2,nrho_anom2,nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_BphiRZ
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%BphiRZ).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("BphiRZ","EQ")
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
        IF(nids%nid_dim_nz.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nz,"dim_nz",ss%nz)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nz
        CALL define_var("BphiRZ","R",indim,idims, &
               "Toroidal field", &
               "T","EQ","STATE_PROFILES", &
               "R|units=T|Hermite      BphiRZ(nR,nZ)")
     ENDIF
  ENDIF

  iaddr = ps_phash_BRRZ
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%BRRZ).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("BRRZ","EQ")
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
        IF(nids%nid_dim_nz.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nz,"dim_nz",ss%nz)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nz
        CALL define_var("BRRZ","R",indim,idims, &
               "R component of poloidal field", &
               "T","EQ","STATE_PROFILES", &
               "R|units=T|Hermite      BRRZ(nR,nZ)")
     ENDIF
  ENDIF

  iaddr = ps_phash_BZRZ
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%BZRZ).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("BZRZ","EQ")
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
        IF(nids%nid_dim_nz.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nz,"dim_nz",ss%nz)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nz
        CALL define_var("BZRZ","R",indim,idims, &
               "Z component of poloidal field", &
               "T","EQ","STATE_PROFILES", &
               "R|units=T|Hermite      BZRZ(nR,nZ)")
     ENDIF
  ENDIF

  iaddr = ps_shash_B_axis
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("B_axis","EQ")
     ELSE
        indim = 0
        CALL define_var("B_axis","R",indim,idims, &
               "|B| at magnetic axis", &
               "T","EQ","STATE_DATA", &
               "R  B_axis")
     ENDIF
  ENDIF

  iaddr = ps_shash_B_axis_vac
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("B_axis_vac","EQ")
     ELSE
        indim = 0
        CALL define_var("B_axis_vac","R",indim,idims, &
               "vacuum B field at axis", &
               "T","EQ","STATE_DATA", &
               "R  B_axis_vac")
     ENDIF
  ENDIF

  iaddr = ps_shash_b_halfHeight
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%b_halfHeight).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("b_halfHeight","NBI")
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
        CALL define_var("b_halfHeight","R",indim,idims, &
               "beam half-height at source", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m       b_halfHeight(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_b_halfwidth
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%b_halfwidth).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("b_halfwidth","NBI")
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
        CALL define_var("b_halfwidth","R",indim,idims, &
               "beam half-width at source", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m       b_halfwidth(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_b_Hdivergence
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%b_Hdivergence).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("b_Hdivergence","NBI")
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
        CALL define_var("b_Hdivergence","R",indim,idims, &
               "horizontal divergence (D) of beam", &
               "degrees","NBI","MACHINE_DESCRIPTION", &
               "R|units=degrees   b_Hdivergence(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_b_Hfocal_length
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%b_Hfocal_length).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("b_Hfocal_length","NBI")
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
        CALL define_var("b_Hfocal_length","R",indim,idims, &
               "horizontal focal length", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m       b_Hfocal_length(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_B_max_lcfs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("B_max_lcfs","EQ")
     ELSE
        indim = 0
        CALL define_var("B_max_lcfs","R",indim,idims, &
               "|B|_max of last closed flux surface", &
               "T","EQ","STATE_DATA", &
               "R  B_max_lcfs")
     ENDIF
  ENDIF

  iaddr = ps_shash_B_min_lcfs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("B_min_lcfs","EQ")
     ELSE
        indim = 0
        CALL define_var("B_min_lcfs","R",indim,idims, &
               "|B|_min of last closed flux surface", &
               "T","EQ","STATE_DATA", &
               "R  B_min_lcfs")
     ENDIF
  ENDIF

  iaddr = ps_phash_B_surfMax
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%B_surfMax).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("B_surfMax","EQ")
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
        CALL define_var("B_surfMax","R",indim,idims, &
               "max mod(B) on flux surface", &
               "T","EQ","STATE_PROFILES", &
               "R|pclin  B_surfMax(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_B_surfMin
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%B_surfMin).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("B_surfMin","EQ")
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
        CALL define_var("B_surfMin","R",indim,idims, &
               "min mod(B) on flux surface", &
               "T","EQ","STATE_PROFILES", &
               "R|pclin  B_surfMin(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_shash_b_Vdivergence
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%b_Vdivergence).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("b_Vdivergence","NBI")
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
        CALL define_var("b_Vdivergence","R",indim,idims, &
               "vertical divergence (D) of beam", &
               "degrees","NBI","MACHINE_DESCRIPTION", &
               "R|units=degrees   b_Vdivergence(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_b_Vfocal_length
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%b_Vfocal_length).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("b_Vfocal_length","NBI")
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
        CALL define_var("b_Vfocal_length","R",indim,idims, &
               "vertical focal length", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m       b_Vfocal_length(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_cdicrf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%cdicrf).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("cdicrf","IC")
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
        IF(nids%nid_dim_nicrf_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nicrf_src,"dim_nicrf_src",ss%nicrf_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nicrf_src
        CALL define_var("cdicrf","R",indim,idims, &
               "ICRF current drive", &
               "A","IC","STATE_PROFILES", &
               "R|units=A|step*dA   cdicrf(~nrho_icrf,nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_cdicrf_nphi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%cdicrf_nphi).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("cdicrf_nphi","IC")
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
        IF(nids%nid_dim_max_num_nphi.EQ.0) THEN
           CALL define_dim(nids%nid_dim_max_num_nphi,"dim_max_num_nphi",ss%max_num_nphi)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_max_num_nphi
        IF(nids%nid_dim_nicrf_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nicrf_src,"dim_nicrf_src",ss%nicrf_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nicrf_src
        CALL define_var("cdicrf_nphi","R",indim,idims, &
               "ICRF current drive", &
               "A","IC","STATE_PROFILES", &
               "R|units=A|step*dA   cdicrf_nphi(~nrho_icrf,num_nphi(),nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_anom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_anom).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chie_anom","ANOM")
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
        CALL define_var("chie_anom","R",indim,idims, &
               "Te-equation, diffusivity term", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  chie_anom(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_drbm
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_drbm).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chie_drbm","ANOM")
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
        CALL define_var("chie_drbm","R",indim,idims, &
               "Te-equation, diffusivity term", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  chie_drbm(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_etg
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_etg).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chie_etg","ANOM")
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
        CALL define_var("chie_etg","R",indim,idims, &
               "Te-equation, diffusivity term ETG model", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin chie_etg(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_glf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_glf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chie_glf","ANOM")
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
        CALL define_var("chie_glf","R",indim,idims, &
               "Te-equation, diffusivity from GLF23 model", &
               "m^2/s","ANOM","STATE_PROFILES", &
               "R|units=m^2/s|pclin chie_glf(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_gtcneo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_gtcneo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chie_gtcneo","ANOM")
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
        CALL define_var("chie_gtcneo","R",indim,idims, &
               "Te-equation, neoclassical diffusivity from GTC_NEO model", &
               "m^2/s","ANOM","STATE_PROFILES", &
               "R|units=m^2/s|pclin chie_gtcneo(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_mmm71
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_mmm71).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chie_mmm71","ANOM")
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
        CALL define_var("chie_mmm71","R",indim,idims, &
               "Te-equation, diffusivity term", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  chie_mmm71(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_mtm
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_mtm).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chie_mtm","ANOM")
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
        CALL define_var("chie_mtm","R",indim,idims, &
               "Te-equation, thermal diffusivity term MTM model", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin chie_mtm(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_nclass
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_nclass).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chie_nclass","ANOM")
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
        CALL define_var("chie_nclass","R",indim,idims, &
               "Te-equation, neoclassical diffusivity from NCLASS model", &
               "m^2/s","ANOM","STATE_PROFILES", &
               "R|units=m^2/s|pclin chie_nclass(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_neo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_neo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chie_neo","ANOM")
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
        CALL define_var("chie_neo","R",indim,idims, &
               "Te-equation, neoclassical diffusivity from NEO model", &
               "m^2/s","ANOM","STATE_PROFILES", &
               "R|units=m^2/s|pclin chie_neo(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_ntm
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_ntm).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chie_ntm","ANOM")
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
        CALL define_var("chie_ntm","R",indim,idims, &
               "Te-equation, neoclassical diffusivity from NTM model", &
               "m^2/s","ANOM","STATE_PROFILES", &
               "R|units=m^2/s|pclin chie_ntm(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_paleo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_paleo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chie_paleo","ANOM")
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
        CALL define_var("chie_paleo","R",indim,idims, &
               "Te-equation, diffusivity term PALEO model", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin chie_paleo(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chie_tglf","ANOM")
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
        CALL define_var("chie_tglf","R",indim,idims, &
               "Te-equation, diffusivity from TGLF model", &
               "m^2/s","ANOM","STATE_PROFILES", &
               "R|units=m^2/s|pclin chie_tglf(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_trans
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_trans).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("chie_trans","PLASMA")
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
        CALL define_var("chie_trans","R",indim,idims, &
               "electron thermal diffusivity", &
               "m^2/sec","PLASMA","STATE_PROFILES", &
               "R|pclin chie_trans(nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_transcat
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_transcat).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("chie_transcat","PLASMA")
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
        IF(nids%nid_dim_ntrcat.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ntrcat,"dim_ntrcat",ss%ntrcat)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ntrcat
        CALL define_var("chie_transcat","R",indim,idims, &
               "Electron energy diffusivity", &
               "m^2/sec","PLASMA","STATE_PROFILES", &
               "R|units=m^2/sec|pclin chie_transcat(nrho,ntrcat)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chie_w19
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chie_w19).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chie_w19","ANOM")
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
        CALL define_var("chie_w19","R",indim,idims, &
               "Te-equation, diffusivity term", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  chie_w19(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chii_anom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chii_anom).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chii_anom","ANOM")
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
        CALL define_var("chii_anom","R",indim,idims, &
               "Ti-equation, diffusivity term", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  chii_anom(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chii_drbm
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chii_drbm).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chii_drbm","ANOM")
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
        CALL define_var("chii_drbm","R",indim,idims, &
               "Ti-equation, diffusivity term", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  chii_drbm(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chii_glf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chii_glf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chii_glf","ANOM")
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
        CALL define_var("chii_glf","R",indim,idims, &
               "Ti-equation, diffusivity from GLF23 model", &
               "m^2/s","ANOM","STATE_PROFILES", &
               "R|units=m^2/s|pclin chii_glf(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chii_gtcneo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chii_gtcneo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chii_gtcneo","ANOM")
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
        CALL define_var("chii_gtcneo","R",indim,idims, &
               "Ti-equation, neoclassical diffusivity from GTC_NEO model", &
               "m^2/s","ANOM","STATE_PROFILES", &
               "R|units=m^2/s|pclin chii_gtcneo(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chii_mmm71
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chii_mmm71).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chii_mmm71","ANOM")
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
        CALL define_var("chii_mmm71","R",indim,idims, &
               "Ti-equation, diffusivity term", &
               "m^2/sec","ANOM","STATE_PROFILES", &
               "R|units=m^2/sec|pclin  chii_mmm71(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chii_nclass
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chii_nclass).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chii_nclass","ANOM")
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
        CALL define_var("chii_nclass","R",indim,idims, &
               "Ti-equation, neoclassical diffusivity from NCALSS model", &
               "m^2/s","ANOM","STATE_PROFILES", &
               "R|units=m^2/s|pclin chii_nclass(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_chii_neo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%chii_neo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("chii_neo","ANOM")
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
        CALL define_var("chii_neo","R",indim,idims, &
               "Ti-equation, neoclassical diffusivity from NEO model", &
               "m^2/s","ANOM","STATE_PROFILES", &
               "R|units=m^2/s|pclin chii_neo(nrho_anomth)")
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

END SUBROUTINE ps_update_file_write_def_0

SUBROUTINE ps_update_file_write_0(ss,icdf,cclist,iucount,iout,iwarn,iertot)


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


  status = nf90_inq_varid(icdf,"alla_index",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%alla_index)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: alla_index"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ALLA_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ALLA_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ALLA_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ALLA_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ALLA_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ALLA_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"all_index",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%all_index)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: all_index"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ALL_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ALL_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ALL_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ALL_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ALL_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ALL_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"amaxlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%amaxlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: amaxlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"aminlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%aminlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: aminlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Ang_Href_coil",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Ang_Href_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Ang_Href_coil"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Ang_Vref_coil",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Ang_Vref_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Ang_Vref_coil"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ANOM_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ANOM_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ANOM_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"anom_evar",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%anom_evar)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: anom_evar"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ant_model",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ant_model)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ant_model"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap2_halfheight",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ap2_halfheight)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ap2_halfheight"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap2_halfwidth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ap2_halfwidth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ap2_halfwidth"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap2_horiz_offset",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ap2_horiz_offset)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ap2_horiz_offset"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap2_vert_offset",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ap2_vert_offset)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ap2_vert_offset"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"apolmaxlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%apolmaxlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: apolmaxlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"apolminlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%apolminlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: apolminlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap_halfheight",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ap_halfheight)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ap_halfheight"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap_halfwidth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ap_halfwidth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ap_halfwidth"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap_horiz_offset",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ap_horiz_offset)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ap_horiz_offset"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap_vert_offset",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ap_vert_offset)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ap_vert_offset"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"area",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%area)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: area"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"atormaxlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%atormaxlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: atormaxlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"atorminlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%atorminlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: atorminlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Balloon_code",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Balloon_code)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Balloon_code"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Balloon_mapper",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Balloon_mapper)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Balloon_mapper"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Balloon_stable",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Balloon_stable)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Balloon_stable"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Balloon_status",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Balloon_status)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Balloon_status"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"balstab",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%balstab)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: balstab"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"beam_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%beam_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: beam_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"bidiff_bpass_co",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%bidiff_bpass_co)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: bidiff_bpass_co"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"bidiff_bpass_ctr",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%bidiff_bpass_ctr)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: bidiff_bpass_ctr"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"bidiff_btrap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%bidiff_btrap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: bidiff_btrap"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"bidiff_dpass_co",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%bidiff_dpass_co)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: bidiff_dpass_co"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"bidiff_dpass_ctr",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%bidiff_dpass_ctr)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: bidiff_dpass_ctr"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"bidiff_dtrap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%bidiff_dtrap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: bidiff_dtrap"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"BphiRZ",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%BphiRZ)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: BphiRZ"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"BRRZ",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%BRRZ)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: BRRZ"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"BZRZ",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%BZRZ)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: BZRZ"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_axis",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%B_axis)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: B_axis"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_axis_vac",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%B_axis_vac)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: B_axis_vac"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"b_halfHeight",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%b_halfHeight)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: b_halfHeight"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"b_halfwidth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%b_halfwidth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: b_halfwidth"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"b_Hdivergence",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%b_Hdivergence)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: b_Hdivergence"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"b_Hfocal_length",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%b_Hfocal_length)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: b_Hfocal_length"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_max_lcfs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%B_max_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: B_max_lcfs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_min_lcfs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%B_min_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: B_min_lcfs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_surfMax",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%B_surfMax)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: B_surfMax"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_surfMin",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%B_surfMin)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: B_surfMin"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"b_Vdivergence",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%b_Vdivergence)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: b_Vdivergence"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"b_Vfocal_length",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%b_Vfocal_length)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: b_Vfocal_length"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"cdicrf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%cdicrf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: cdicrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"cdicrf_nphi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%cdicrf_nphi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: cdicrf_nphi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_drbm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_etg",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_etg)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_etg"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_glf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_gtcneo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_mmm71",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_mtm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_mtm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_mtm"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_nclass",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_neo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_neo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_ntm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_ntm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_ntm"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_paleo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_paleo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_paleo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_w19",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chie_w19)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chie_w19"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chii_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chii_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_drbm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chii_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chii_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_glf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chii_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chii_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_gtcneo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chii_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chii_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_mmm71",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chii_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chii_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_nclass",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chii_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chii_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_neo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%chii_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: chii_neo"
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

END SUBROUTINE ps_update_file_write_0
