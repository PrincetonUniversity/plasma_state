! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_write_def_4(ss,icdf,cclist,nids,iout,iwarn,iertot)


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

  iaddr = ps_phash_nbeami
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nbeami).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("nbeami","NBI")
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
        CALL define_var("nbeami","R",indim,idims, &
               "beam species density", &
               "m^-3","NBI","STATE_PROFILES", &
               "R|units=m^-3|alias=n|step  nbeami(~nrho_nbi,nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nbeami_bdy
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nbeami_bdy).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("nbeami_bdy","NBI")
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
        CALL define_var("nbeami_bdy","R",indim,idims, &
               "beam ion densities at/beyond boundary", &
               "m^-3","NBI","STATE_DATA", &
               "R|units=m^-3  nbeami_bdy(nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nbion
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nbion).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("nbion","NBI")
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
        CALL define_var("nbion","N",indim,idims, &
               "name of species injected by each beam.", &
               "__NONE","NBI","SHOT_CONFIGURATION", &
               "N  nbion(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nbion_trace
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nbion_trace).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("nbion_trace","NBI")
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
        CALL define_var("nbion_trace","N",indim,idims, &
               "(if non-blank) beam trace element", &
               "__NONE","NBI","SHOT_CONFIGURATION", &
               "N  nbion_trace(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_NBI_Code_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("NBI_Code_Info","NBI")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("NBI_Code_Info","C*80",indim,idims, &
               "Information: code implementing NBI component", &
               "__NONE","NBI","SIMULATION_INIT", &
               "C*80   NBI_Code_Info")
     ENDIF
  ENDIF

  iaddr = ps_shash_NBI_Data_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("NBI_Data_Info","NBI")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("NBI_Data_Info","C*80",indim,idims, &
               "information on source of beam power & voltage data", &
               "__NONE","NBI","SIMULATION_INIT", &
               "C*80   NBI_Data_Info")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nbeam
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nbi_src_name).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("nbi_src_name","NBI")
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
        CALL define_var("nbi_src_name","N",indim,idims, &
               "number & name of neutral beams", &
               "__NONE","NBI","MACHINE_DESCRIPTION", &
               "L|neutral_beams nbi_src_name(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nbshape
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nbshape).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("nbshape","NBI")
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
        CALL define_var("nbshape","N",indim,idims, &
               "shape keyword for beam source grid", &
               "__NONE","NBI","MACHINE_DESCRIPTION", &
               "N     nbshape(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nebar
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("nebar","PLASMA")
     ELSE
        indim = 0
        CALL define_var("nebar","R",indim,idims, &
               "line-averaged electron density", &
               "#/m^2","PLASMA","STATE_DATA", &
               "R|units=#/m^2 nebar")
     ENDIF
  ENDIF

  iaddr = ps_phash_neflux_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%neflux_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("neflux_ky_tglf","ANOM")
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
        CALL define_var("neflux_ky_tglf","R",indim,idims, &
               "electron density flux spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin neflux_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nnewcomb
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Newcomb_code).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("Newcomb_code","LMHD")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_nnewcomb.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nnewcomb,"dim_nnewcomb",ss%nnewcomb)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nnewcomb
        CALL define_var("Newcomb_code","N",indim,idims, &
               "name(s) of Newcomb code(s) used", &
               "__NONE","LMHD","SIMULATION_INIT", &
               "L|Newcomb_codes  Newcomb_code(nnewcomb)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Newcomb_mapper
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Newcomb_mapper).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("Newcomb_mapper","LMHD")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_nnewcomb.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nnewcomb,"dim_nnewcomb",ss%nnewcomb)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nnewcomb
        CALL define_var("Newcomb_mapper","N",indim,idims, &
               "mapper code used for each Newcomb code", &
               "__NONE","LMHD","SIMULATION_INIT", &
               "N  Newcomb_mapper(nnewcomb)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Newcomb_stable
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Newcomb_stable).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("Newcomb_stable","LMHD")
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
        IF(nids%nid_dim_nnewcomb.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nnewcomb,"dim_nnewcomb",ss%nnewcomb)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nnewcomb
        CALL define_var("Newcomb_stable","I",indim,idims, &
               "0: stable; 1: unstable;", &
               "__NONE","LMHD","STATE_DATA", &
               "I   Newcomb_stable(mmodes,nnewcomb)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Newcomb_status
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Newcomb_status).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("Newcomb_status","LMHD")
     ELSE
        indim = 0
        IF(nids%nid_dim_nnewcomb.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nnewcomb,"dim_nnewcomb",ss%nnewcomb)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nnewcomb
        CALL define_var("Newcomb_status","I",indim,idims, &
               "completion status: Newcomb codes", &
               "__NONE","LMHD","STATE_DATA", &
               "I   Newcomb_status(nnewcomb)")
     ENDIF
  ENDIF

  iaddr = ps_phash_ne_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ne_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("ne_ky_tglf","ANOM")
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
        CALL define_var("ne_ky_tglf","R",indim,idims, &
               "electron density fluctuation spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin ne_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_nfusi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nfusi).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("nfusi","FUS")
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
        CALL define_var("nfusi","R",indim,idims, &
               "fusion ion density", &
               "m^-3","FUS","STATE_PROFILES", &
               "R|units=m^-3|alias=n|step  nfusi(~nrho_fus,nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nfusi_bdy
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nfusi_bdy).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("nfusi_bdy","IC")
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
        CALL define_var("nfusi_bdy","R",indim,idims, &
               "fusion ion densities at/beyond boundary", &
               "m^-3","IC","STATE_DATA", &
               "R|units=m^-3  nfusi_bdy(nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_phash_ngradb2_av
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ngradb2_av).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("ngradb2_av","EQ")
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
        CALL define_var("ngradb2_av","R",indim,idims, &
               "<(B.grad(B)/|B|)^2>", &
               "T^2*m^-2","EQ","STATE_PROFILES", &
               "R|units=T^2*m^-2|pclin   ngradb2_av(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_ni
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ni).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("ni","PLASMA")
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
        CALL define_var("ni","R",indim,idims, &
               "total thermal ion density", &
               "m^-3","PLASMA","STATE_PROFILES", &
               "R|units=m^-3|step  ni(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_niflux_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%niflux_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("niflux_ky_tglf","ANOM")
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
        CALL define_var("niflux_ky_tglf","R",indim,idims, &
               "ion density flux spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin niflux_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_ni_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ni_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("ni_ky_tglf","ANOM")
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
        CALL define_var("ni_ky_tglf","R",indim,idims, &
               "ion density fluctuation spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin ni_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nmdifb
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("nmdifb","ANOM")
     ELSE
        indim = 0
        CALL define_var("nmdifb","I",indim,idims, &
               "type of fast ion diffusivity", &
               "__NONE","ANOM","STATE_DATA", &
               "I   nmdifb = 0")
     ENDIF
  ENDIF

  iaddr = ps_phash_nmini
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nmini).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("nmini","IC")
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
        CALL define_var("nmini","R",indim,idims, &
               "minority species density", &
               "m^-3","IC","STATE_PROFILES", &
               "R|units=m^-3|alias=n|step  nmini(~nrho_icrf,nspec_rfmin)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nmini_bdy
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nmini_bdy).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("nmini_bdy","FUS")
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
        CALL define_var("nmini_bdy","R",indim,idims, &
               "RF minority ion densities at/beyond boundary", &
               "m^-3","FUS","STATE_DATA", &
               "R|units=m^-3  nmini_bdy(nspec_rfmin)")
     ENDIF
  ENDIF

  iaddr = ps_phash_nmodel
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nmodel).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("nmodel","ANOM")
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
        CALL define_var("nmodel","R",indim,idims, &
               "target density profile", &
               "m^-3","ANOM","STATE_PROFILES", &
               "R|units=m^-3|step  nmodel(~nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nmodel_bdy
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("nmodel_bdy","ANOM")
     ELSE
        indim = 0
        CALL define_var("nmodel_bdy","R",indim,idims, &
               "model density, boundary value", &
               "m^-3","ANOM","STATE_DATA", &
               "R|units=m^-3   nmodel_bdy")
     ENDIF
  ENDIF

  iaddr = ps_shash_nmom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("nmom","EQ")
     ELSE
        indim = 0
        CALL define_var("nmom","I",indim,idims, &
               "number of Fourier (R,Z) moments", &
               "__NONE","EQ","SIMULATION_INIT", &
               "I  nmom = 16")
     ENDIF
  ENDIF

  iaddr = ps_shash_nnkparlh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nnkparlh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("nnkparlh","LH")
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
        CALL define_var("nnkparlh","I",indim,idims, &
               "need description", &
               "__NONE","LH","SHOT_CONFIGURATION", &
               "I nnkparlh(nspectrumlh_src(),nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nnkpollh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nnkpollh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("nnkpollh","LH")
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
        CALL define_var("nnkpollh","I",indim,idims, &
               "need description", &
               "__NONE","LH","SHOT_CONFIGURATION", &
               "I nnkpollh(nspectrumlh_src(),nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nnktorlh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nnktorlh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("nnktorlh","LH")
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
        CALL define_var("nnktorlh","I",indim,idims, &
               "need description", &
               "__NONE","LH","SHOT_CONFIGURATION", &
               "I nnktorlh(nspectrumlh_src(),nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nphi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nphi).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("nphi","IC")
     ELSE
        indim = 0
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
        CALL define_var("nphi","I",indim,idims, &
               "n_phi wave spectrum from antenna", &
               "__NONE","IC","SHOT_CONFIGURATION", &
               "I nphi(num_nphi(),nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_nrw
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nrw).AND.ichange) THEN
     IF(cclist(ps_cnum_RUNAWAY).NE.1) THEN
        CALL ccwarn("nrw","RUNAWAY")
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
        CALL define_var("nrw","R",indim,idims, &
               "runaway electron density", &
               "m^-3","RUNAWAY","STATE_PROFILES", &
               "R|units=m^-3|step   nrw(~nrho_rw)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nrz_antgeo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nrz_antgeo).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("nrz_antgeo","IC")
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
        CALL define_var("nrz_antgeo","I",indim,idims, &
               "number of (R,Z) points, antenna geometries", &
               "__NONE","IC","MACHINE_DESCRIPTION", &
               "I|ENUM nrz_antgeo(nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_ns
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ns).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("ns","PLASMA")
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
        CALL define_var("ns","R",indim,idims, &
               "thermal specie density", &
               "m^-3","PLASMA","STATE_PROFILES", &
               "R|units=m^-3|alias=n|step  ns(~nrho,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_shash_nspectrumlh_src
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nspectrumlh_src).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("nspectrumlh_src","LH")
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
        CALL define_var("nspectrumlh_src","I",indim,idims, &
               "number of spectrum peaks", &
               "__NONE","LH","SHOT_CONFIGURATION", &
               "I|ENUM nspectrumlh_src(nlhrf_src)")
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

END SUBROUTINE ps_update_file_write_def_4

SUBROUTINE ps_update_file_write_4(ss,icdf,cclist,iucount,iout,iwarn,iertot)


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


  status = nf90_inq_varid(icdf,"nbeami",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nbeami)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nbeami"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nbeami_bdy",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nbeami_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nbeami_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nbion",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nbion)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nbion"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nbion_trace",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nbion_trace)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nbion_trace"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"NBI_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%NBI_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: NBI_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"NBI_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%NBI_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: NBI_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nbi_src_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nbi_src_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nbi_src_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nbshape",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nbshape)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nbshape"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nebar",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nebar)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nebar"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"neflux_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%neflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: neflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Newcomb_code",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Newcomb_code)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Newcomb_code"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Newcomb_mapper",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Newcomb_mapper)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Newcomb_mapper"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Newcomb_stable",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Newcomb_stable)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Newcomb_stable"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Newcomb_status",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Newcomb_status)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Newcomb_status"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ne_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ne_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ne_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nfusi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nfusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nfusi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nfusi_bdy",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nfusi_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nfusi_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ngradb2_av",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ngradb2_av)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ngradb2_av"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ni",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ni)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ni"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"niflux_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%niflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: niflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ni_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ni_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ni_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nmdifb",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nmdifb)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nmdifb"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nmini",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nmini)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nmini"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nmini_bdy",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nmini_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nmini_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nmodel",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nmodel)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nmodel"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nmodel_bdy",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nmodel_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nmodel_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nmom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nmom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nmom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nnkparlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nnkparlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nnkparlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nnkpollh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nnkpollh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nnkpollh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nnktorlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nnktorlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nnktorlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nphi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nphi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nphi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nrw",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nrw)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nrw"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nrz_antgeo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nrz_antgeo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nrz_antgeo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ns",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ns)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ns"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nspectrumlh_src",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nspectrumlh_src)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nspectrumlh_src"
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

END SUBROUTINE ps_update_file_write_4
