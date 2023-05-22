! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_write_def_9(ss,icdf,cclist,nids,iout,iwarn,iertot)


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

  iaddr = ps_phash_velpphi_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velpphi_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velpphi_tglf","ANOM")
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
        CALL define_var("velpphi_tglf","R",indim,idims, &
               "momentum-equation, advective velocity term from TGLF model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin velpphi_tglf(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velte_anom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velte_anom).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velte_anom","ANOM")
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
        CALL define_var("velte_anom","R",indim,idims, &
               "Te-equation, advective velocity term", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velte_anom(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velte_glf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velte_glf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velte_glf","ANOM")
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
        CALL define_var("velte_glf","R",indim,idims, &
               "Te-equation, advective velocity term from GLF23 model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velte_glf(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velte_gtcneo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velte_gtcneo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velte_gtcneo","ANOM")
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
        CALL define_var("velte_gtcneo","R",indim,idims, &
               "Te-equation, advective velocity term from GTC_NEO model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velte_gtcneo(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velte_mmm71
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velte_mmm71).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velte_mmm71","ANOM")
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
        CALL define_var("velte_mmm71","R",indim,idims, &
               "Te-equation, advective velocity term from MMM71 model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velte_mmm71(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velte_nclass
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velte_nclass).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velte_nclass","ANOM")
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
        CALL define_var("velte_nclass","R",indim,idims, &
               "Te-equation, advective velocity term from NCLASS model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin velte_nclass(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velte_neo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velte_neo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velte_neo","ANOM")
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
        CALL define_var("velte_neo","R",indim,idims, &
               "Te-equation, advective velocity term from NEO model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velte_neo(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velte_paleo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velte_paleo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velte_paleo","ANOM")
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
        CALL define_var("velte_paleo","R",indim,idims, &
               "Te-equation, convection term from PALEO model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin   velte_paleo(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velte_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velte_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velte_tglf","ANOM")
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
        CALL define_var("velte_tglf","R",indim,idims, &
               "Te-equation, advective velocity term from TGLF model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin velte_tglf(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velti_anom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velti_anom).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velti_anom","ANOM")
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
        CALL define_var("velti_anom","R",indim,idims, &
               "Ti-equation, advective velocity term", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velti_anom(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velti_glf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velti_glf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velti_glf","ANOM")
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
        CALL define_var("velti_glf","R",indim,idims, &
               "Ti-equation, advective velocity term from GLF23 model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velti_glf(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velti_gtcneo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velti_gtcneo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velti_gtcneo","ANOM")
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
        CALL define_var("velti_gtcneo","R",indim,idims, &
               "Ti-equation, advective velocity term from GTC_NEO model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velti_gtcneo(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velti_mmm71
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velti_mmm71).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velti_mmm71","ANOM")
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
        CALL define_var("velti_mmm71","R",indim,idims, &
               "Ti-equation, advective velocity term from MMM71 model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velti_mmm71(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velti_nclass
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velti_nclass).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velti_nclass","ANOM")
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
        CALL define_var("velti_nclass","R",indim,idims, &
               "Ti-equation, advective velocity term from NCLASS model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin velti_nclass(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velti_neo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velti_neo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velti_neo","ANOM")
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
        CALL define_var("velti_neo","R",indim,idims, &
               "Ti-equation, advective velocity term from NEO model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velti_neo(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velti_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velti_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velti_tglf","ANOM")
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
        CALL define_var("velti_tglf","R",indim,idims, &
               "Ti-equation, advective velocity term from TGLF model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin velti_tglf(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_vie_trans
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vie_trans).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vie_trans","PLASMA")
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
        CALL define_var("vie_trans","R",indim,idims, &
               "ion energy radial velocity", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|pclin   vie_trans(nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_vie_transcat
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vie_transcat).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vie_transcat","PLASMA")
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
        CALL define_var("vie_transcat","R",indim,idims, &
               "Ion energy radial velocity", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|units=m/sec|pclin vie_transcat(nrho,ntrcat)")
     ENDIF
  ENDIF

  iaddr = ps_phash_vmo_trans
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vmo_trans).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vmo_trans","PLASMA")
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
        CALL define_var("vmo_trans","R",indim,idims, &
               "angular momentum radial velocity", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|pclin   vmo_trans(nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_vmo_transcat
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vmo_transcat).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vmo_transcat","PLASMA")
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
        CALL define_var("vmo_transcat","R",indim,idims, &
               "Angular momentum radial velocity", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|units=m/sec|pclin vmo_transcat(nrho,ntrcat)")
     ENDIF
  ENDIF

  iaddr = ps_phash_vn_trans
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vn_trans).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vn_trans","PLASMA")
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
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        CALL define_var("vn_trans","R",indim,idims, &
               "specie radial velocity", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|units=m/sec|pclin   vn_trans(nrho,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_vn_transcat
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vn_transcat).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vn_transcat","PLASMA")
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
        CALL define_var("vn_transcat","R",indim,idims, &
               "specie radial velocity", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|units=m/sec|pclin vn_transcat(nrho,0:nspec_th,ntrcat)")
     ENDIF
  ENDIF

  iaddr = ps_phash_vol
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vol).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("vol","EQ")
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
        CALL define_var("vol","R",indim,idims, &
               "enclosed volume", &
               "m^3","EQ","STATE_PROFILES", &
               "R|units=m^3|Spline_00  vol(nrho_eq)")
     ENDIF
  ENDIF

  iaddr = ps_shash_vphi0_av
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vphi0_av).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("vphi0_av","GAS")
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
        CALL define_var("vphi0_av","R",indim,idims, &
               "average toroidal velocity of neutral source", &
               "m/sec","GAS","STATE_DATA", &
               "R|units=m/sec vphi0_av(ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_shash_vpol_Data_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vpol_Data_Info","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("vpol_Data_Info","C*80",indim,idims, &
               "information on source of poloidal velocity data", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "C*80   vpol_Data_Info")
     ENDIF
  ENDIF

  iaddr = ps_phash_vpol_inmp
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vpol_inmp).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vpol_inmp","PLASMA")
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
        CALL define_var("vpol_inmp","R",indim,idims, &
               "poloidal velocity, inner half midplane", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|units=m/sec|step*ns vpol_inmp(~nrho,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_shash_vpol_is_input
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vpol_is_input).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vpol_is_input","PLASMA")
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
        CALL define_var("vpol_is_input","I",indim,idims, &
               "specie outer midplane poloidal velocit flag", &
               "__NONE","PLASMA","STATE_DATA", &
               "I vpol_is_input(0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_vpol_omp
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vpol_omp).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vpol_omp","PLASMA")
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
        CALL define_var("vpol_omp","R",indim,idims, &
               "poloidal velocity, outer half midplane", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|units=m/sec|step*ns vpol_omp(~nrho,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_shash_VRep_coil_count
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%VRep_coil_count).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("VRep_coil_count","EQ")
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
        CALL define_var("VRep_coil_count","I",indim,idims, &
               "replication count, vertical coil stack", &
               "__NONE","EQ","MACHINE_DESCRIPTION", &
               "I  VRep_coil_count(ncoils)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Vrep_coil_spacing
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Vrep_coil_spacing).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Vrep_coil_spacing","EQ")
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
        CALL define_var("Vrep_coil_spacing","R",indim,idims, &
               "replicated coil separation", &
               "m","EQ","MACHINE_DESCRIPTION", &
               "R|units=m Vrep_coil_spacing(ncoils)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Vsize_coil
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Vsize_coil).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Vsize_coil","EQ")
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
        CALL define_var("Vsize_coil","R",indim,idims, &
               "(Rotatable) Vertical coil size", &
               "m","EQ","MACHINE_DESCRIPTION", &
               "R|units=m  Vsize_coil(ncoils)")
     ENDIF
  ENDIF

  iaddr = ps_shash_vsur
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vsur","PLASMA")
     ELSE
        indim = 0
        CALL define_var("vsur","R",indim,idims, &
               "toroidal voltage at surface", &
               "Volts","PLASMA","STATE_DATA", &
               "R  vsur")
     ENDIF
  ENDIF

  iaddr = ps_shash_VSUR_Data_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("VSUR_Data_Info","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("VSUR_Data_Info","C*80",indim,idims, &
               "information on source of surface voltage data", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "C*80   VSUR_Data_Info")
     ENDIF
  ENDIF

  iaddr = ps_shash_vtor_Data_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vtor_Data_Info","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("vtor_Data_Info","C*80",indim,idims, &
               "information on source of toroidal velocity data", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "C*80   vtor_Data_Info")
     ENDIF
  ENDIF

  iaddr = ps_phash_vtor_inmp
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vtor_inmp).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vtor_inmp","PLASMA")
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
        CALL define_var("vtor_inmp","R",indim,idims, &
               "toroidal velocity, inner half midplane", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|units=m/sec|step*ns vtor_inmp(~nrho,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_shash_vtor_is_input
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vtor_is_input).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vtor_is_input","PLASMA")
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
        CALL define_var("vtor_is_input","I",indim,idims, &
               "specie outer midplane toroidal velocit flag", &
               "__NONE","PLASMA","STATE_DATA", &
               "I vtor_is_input(0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_vtor_omp
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vtor_omp).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vtor_omp","PLASMA")
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
        CALL define_var("vtor_omp","R",indim,idims, &
               "toroidal velocity, outer half midplane", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|units=m/sec|step*ns vtor_omp(~nrho,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_V_loop
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%V_loop).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("V_loop","PLASMA")
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
        CALL define_var("V_loop","R",indim,idims, &
               "loop voltage", &
               "Volts","PLASMA","STATE_PROFILES", &
               "R|units=Volts|pclin	V_loop(nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_v_pars
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%v_pars).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("v_pars","PLASMA")
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
        CALL define_var("v_pars","R",indim,idims, &
               "thermal specie <vpll>", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|units=m/sec|alias=vpll_|step*ns v_pars(~nrho,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_v_pers
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%v_pers).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("v_pers","PLASMA")
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
        CALL define_var("v_pers","R",indim,idims, &
               "thermal specie <vper>", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|units=m/sec|alias=vper_|step*ns v_pers(~nrho,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_shash_wt_nphi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%wt_nphi).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("wt_nphi","IC")
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
        CALL define_var("wt_nphi","R",indim,idims, &
               "vacuum spectrum n_phi weight", &
               "-","IC","SHOT_CONFIGURATION", &
               "R|units=- wt_nphi(num_nphi(),nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_wt_nphi_abs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%wt_nphi_abs).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("wt_nphi_abs","IC")
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
        CALL define_var("wt_nphi_abs","R",indim,idims, &
               "absorbed spectrum n_phi weight", &
               "-","IC","STATE_DATA", &
               "R|units=- wt_nphi_abs(num_nphi(),nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_wt_nphi_ext
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%wt_nphi_ext).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("wt_nphi_ext","IC")
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
        CALL define_var("wt_nphi_ext","R",indim,idims, &
               "fraction of power deposited outside", &
               "-","IC","STATE_DATA", &
               "R|units=- wt_nphi_ext(num_nphi(),nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_xRjcos_momeq
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%xRjcos_momeq).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("xRjcos_momeq","EQ")
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
        IF(nids%nid_dim_neqmom.EQ.0) THEN
           CALL define_dim(nids%nid_dim_neqmom,"dim_neqmom",ss%neqmom)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_neqmom
        CALL define_var("xRjcos_momeq","R",indim,idims, &
               "scaled R cos moments", &
               "m","EQ","STATE_PROFILES", &
               "R|units=m|Hermite_explicit  xRjcos_momeq(nrho_eq,neqmom)")
     ENDIF
  ENDIF

  iaddr = ps_phash_xRjsin_momeq
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%xRjsin_momeq).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("xRjsin_momeq","EQ")
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
        IF(nids%nid_dim_neqmom.EQ.0) THEN
           CALL define_dim(nids%nid_dim_neqmom,"dim_neqmom",ss%neqmom)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_neqmom
        CALL define_var("xRjsin_momeq","R",indim,idims, &
               "scaled R sin moments", &
               "m","EQ","STATE_PROFILES", &
               "R|units=m|Hermite_explicit  xRjsin_momeq(nrho_eq,neqmom)")
     ENDIF
  ENDIF

  iaddr = ps_shash_xs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%xs).AND.ichange) THEN
     IF(cclist(ps_cnum_RAD).NE.1) THEN
        CALL ccwarn("xs","RAD")
     ELSE
        indim = 0
        IF(nids%nid_dim_nxs.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nxs,"dim_nxs",ss%nxs)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nxs
        CALL define_var("xs","R",indim,idims, &
               "impurity source magnitude (atoms/sec)", &
               "#/sec","RAD","STATE_DATA", &
               "R|units=#/sec    xs(nxs)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nxs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%xs_name).AND.ichange) THEN
     IF(cclist(ps_cnum_RAD).NE.1) THEN
        CALL ccwarn("xs_name","RAD")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_nxs.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nxs,"dim_nxs",ss%nxs)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nxs
        CALL define_var("xs_name","N",indim,idims, &
               "number & name of impurity sources", &
               "__NONE","RAD","SHOT_CONFIGURATION", &
               "L|impurity_source  xs_name(nxs)")
     ENDIF
  ENDIF

  iaddr = ps_phash_xZjcos_momeq
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%xZjcos_momeq).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("xZjcos_momeq","EQ")
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
        IF(nids%nid_dim_neqmom.EQ.0) THEN
           CALL define_dim(nids%nid_dim_neqmom,"dim_neqmom",ss%neqmom)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_neqmom
        CALL define_var("xZjcos_momeq","R",indim,idims, &
               "scaled Z cos moments", &
               "m","EQ","STATE_PROFILES", &
               "R|units=m|Hermite_explicit  xZjcos_momeq(nrho_eq,neqmom)")
     ENDIF
  ENDIF

  iaddr = ps_phash_xZjsin_momeq
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%xZjsin_momeq).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("xZjsin_momeq","EQ")
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
        IF(nids%nid_dim_neqmom.EQ.0) THEN
           CALL define_dim(nids%nid_dim_neqmom,"dim_neqmom",ss%neqmom)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_neqmom
        CALL define_var("xZjsin_momeq","R",indim,idims, &
               "scaled Z sin moments", &
               "m","EQ","STATE_PROFILES", &
               "R|units=m|Hermite_explicit  xZjsin_momeq(nrho_eq,neqmom)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Z0max
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Z0max","PLASMA")
     ELSE
        indim = 0
        CALL define_var("Z0max","I",indim,idims, &
               "maximum Z of non-impurity species", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "IS  Z0max = 2")
     ENDIF
  ENDIF

  iaddr = ps_phash_Z0_momeq
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Z0_momeq).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Z0_momeq","EQ")
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
        CALL define_var("Z0_momeq","R",indim,idims, &
               "Z0 of flux surface", &
               "m","EQ","STATE_PROFILES", &
               "R|units=m|Hermite_explicit  Z0_momeq(nrho_eq)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Zbap
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Zbap).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("Zbap","NBI")
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
        CALL define_var("Zbap","R",indim,idims, &
               "elevation of beam centerline at aperture", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m       Zbap(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Zbsc
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Zbsc).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("Zbsc","NBI")
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
        CALL define_var("Zbsc","R",indim,idims, &
               "elevation of beam source", &
               "m","NBI","MACHINE_DESCRIPTION", &
               "R|units=m       Zbsc(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_Zeff
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Zeff).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Zeff","PLASMA")
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
        CALL define_var("Zeff","R",indim,idims, &
               "Zeff (sum[nj*Zj**2]/ne)", &
               "-","PLASMA","STATE_PROFILES", &
               "R|step*ns(:,ps_elec_index)  Zeff(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ZEFF_Data_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("ZEFF_Data_Info","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("ZEFF_Data_Info","C*80",indim,idims, &
               "information on source of Zeff data", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "C*80   ZEFF_Data_Info")
     ENDIF
  ENDIF

  iaddr = ps_phash_Zeff_fi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Zeff_fi).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Zeff_fi","PLASMA")
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
        CALL define_var("Zeff_fi","R",indim,idims, &
               "Fast ion contribution to Zeff", &
               "-","PLASMA","STATE_PROFILES", &
               "R|step*ns(:,ps_elec_index)  Zeff_fi(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_Zeff_th
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Zeff_th).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Zeff_th","PLASMA")
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
        CALL define_var("Zeff_th","R",indim,idims, &
               "Thermal ion contribution to Zeff", &
               "-","PLASMA","STATE_PROFILES", &
               "R|step*ns(:,ps_elec_index)  Zeff_th(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Zimp1
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Zimp1","PLASMA")
     ELSE
        indim = 0
        CALL define_var("Zimp1","I",indim,idims, &
               "lower limit on Z of impurity in reduced species list SA;", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "IS  Zimp1 = 2")
     ENDIF
  ENDIF

  iaddr = ps_shash_Zimp2
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Zimp2","PLASMA")
     ELSE
        indim = 0
        CALL define_var("Zimp2","I",indim,idims, &
               "upper limit on Z of impurity in SA species list;", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "IS  Zimp2 = 1000")
     ENDIF
  ENDIF

  iaddr = ps_shash_zlim
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%zlim).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("zlim","EQ")
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
        CALL define_var("zlim","R",indim,idims, &
               "Z points in closed (R,Z) contour sequence", &
               "m","EQ","MACHINE_DESCRIPTION", &
               "R|units=m  zlim(num_rzlim)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Zloc_coil
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Zloc_coil).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Zloc_coil","EQ")
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
        CALL define_var("Zloc_coil","R",indim,idims, &
               "Z location of coil, lower left corner", &
               "m","EQ","MACHINE_DESCRIPTION", &
               "R|units=m  Zloc_coil(ncoils)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Z_ant
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Z_ant).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("Z_ant","IC")
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
        CALL define_var("Z_ant","R",indim,idims, &
               "height of antenna", &
               "m","IC","SHOT_CONFIGURATION", &
               "R|units=m Z_ant(nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Z_antgeo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Z_antgeo).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("Z_antgeo","IC")
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
        CALL define_var("Z_antgeo","R",indim,idims, &
               "antenna geo: Z pts", &
               "m","IC","MACHINE_DESCRIPTION", &
               "R|units=m Z_antgeo(nrz_antgeo(),nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Z_axis
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Z_axis","EQ")
     ELSE
        indim = 0
        CALL define_var("Z_axis","R",indim,idims, &
               "Z of magnetic axis", &
               "m","EQ","STATE_DATA", &
               "R  Z_axis")
     ENDIF
  ENDIF

  iaddr = ps_shash_Z_EC_launch
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Z_EC_launch).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("Z_EC_launch","EC")
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
        CALL define_var("Z_EC_launch","R",indim,idims, &
               "Z of center of launcher", &
               "m","EC","MACHINE_DESCRIPTION", &
               "R|units=m   Z_EC_launch(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_Z_geo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Z_geo).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Z_geo","EQ")
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
        CALL define_var("Z_geo","R",indim,idims, &
               "flux surfaces Z(rho,theta)", &
               "m","EQ","STATE_PROFILES", &
               "R|units=m|Hermite_explicit Z_geo(nrho_eq,nth_eq)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nz
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Z_grid).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Z_grid","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_nz.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nz,"dim_nz",ss%nz)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nz
        CALL define_var("Z_grid","R",indim,idims, &
               "Z grid", &
               "m","EQ","SIMULATION_INIT", &
               "G   Z_grid(nZ)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Z_max_box
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Z_max_box","EQ")
     ELSE
        indim = 0
        CALL define_var("Z_max_box","R",indim,idims, &
               "Z_max of bounding box", &
               "m","EQ","MACHINE_DESCRIPTION", &
               "R  Z_max_box")
     ENDIF
  ENDIF

  iaddr = ps_shash_Z_max_lcfs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Z_max_lcfs","EQ")
     ELSE
        indim = 0
        CALL define_var("Z_max_lcfs","R",indim,idims, &
               "Z_max of last closed flux surface", &
               "m","EQ","STATE_DATA", &
               "R  Z_max_lcfs")
     ENDIF
  ENDIF

  iaddr = ps_phash_Z_midp
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Z_midp).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Z_midp","EQ")
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
        CALL define_var("Z_midp","R",indim,idims, &
               "flux surface midplane elevation", &
               "m","EQ","STATE_PROFILES", &
               "R|pclin  Z_midp(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Z_mid_ant
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Z_mid_ant).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("Z_mid_ant","IC")
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
        CALL define_var("Z_mid_ant","R",indim,idims, &
               "center of antenna relative to TF coil midplane", &
               "m","IC","SHOT_CONFIGURATION", &
               "R|units=m Z_mid_ant(nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Z_min_box
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Z_min_box","EQ")
     ELSE
        indim = 0
        CALL define_var("Z_min_box","R",indim,idims, &
               "Z_min of bounding box", &
               "m","EQ","MACHINE_DESCRIPTION", &
               "R  Z_min_box")
     ENDIF
  ENDIF

  iaddr = ps_shash_Z_min_lcfs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Z_min_lcfs","EQ")
     ELSE
        indim = 0
        CALL define_var("Z_min_lcfs","R",indim,idims, &
               "Z_min of last closed flux surface", &
               "m","EQ","STATE_DATA", &
               "R  Z_min_lcfs")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nz_rip
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Z_ripple).AND.ichange) THEN
     IF(cclist(ps_cnum_RIPPLE).NE.1) THEN
        CALL ccwarn("Z_ripple","RIPPLE")
     ELSE
        indim = 0
        IF(nids%nid_dim_nz_rip.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nz_rip,"dim_nz_rip",ss%nz_rip)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nz_rip
        CALL define_var("Z_ripple","R",indim,idims, &
               "Z grid for TF ripple field(s)", &
               "m","RIPPLE","SIMULATION_INIT", &
               "G  Z_ripple(nZ_rip)")
     ENDIF
  ENDIF

  iaddr = ps_phash_Z_surfMax
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Z_surfMax).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Z_surfMax","EQ")
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
        CALL define_var("Z_surfMax","R",indim,idims, &
               "max Z on flux surface", &
               "m","EQ","STATE_PROFILES", &
               "R|pclin  Z_surfMax(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_Z_surfMin
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Z_surfMin).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Z_surfMin","EQ")
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
        CALL define_var("Z_surfMin","R",indim,idims, &
               "min Z on flux surface", &
               "m","EQ","STATE_PROFILES", &
               "R|pclin  Z_surfMin(nrho_eq_geo)")
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

END SUBROUTINE ps_update_file_write_def_9

SUBROUTINE ps_update_file_write_9(ss,icdf,cclist,iucount,iout,iwarn,iertot)


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


  status = nf90_inq_varid(icdf,"velpphi_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velpphi_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velpphi_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velte_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velte_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_glf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velte_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velte_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_gtcneo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velte_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velte_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_mmm71",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velte_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velte_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_nclass",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velte_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velte_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_neo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velte_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velte_neo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_paleo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velte_paleo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velte_paleo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velte_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velte_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velti_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velti_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_glf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velti_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velti_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_gtcneo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velti_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velti_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_mmm71",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velti_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velti_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_nclass",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velti_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velti_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_neo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velti_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velti_neo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velti_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velti_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vie_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vie_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vie_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vie_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vie_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vie_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vmo_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vmo_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vmo_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vmo_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vmo_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vmo_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vn_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vn_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vn_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vn_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vn_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vn_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vol",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vol)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vol"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vphi0_av",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vphi0_av)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vphi0_av"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vpol_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vpol_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vpol_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vpol_inmp",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vpol_inmp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vpol_inmp"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vpol_is_input",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vpol_is_input)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vpol_is_input"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vpol_omp",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vpol_omp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vpol_omp"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"VRep_coil_count",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%VRep_coil_count)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: VRep_coil_count"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Vrep_coil_spacing",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Vrep_coil_spacing)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Vrep_coil_spacing"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Vsize_coil",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Vsize_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Vsize_coil"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vsur",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vsur)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vsur"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"VSUR_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%VSUR_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: VSUR_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vtor_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vtor_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vtor_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vtor_inmp",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vtor_inmp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vtor_inmp"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vtor_is_input",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vtor_is_input)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vtor_is_input"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vtor_omp",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vtor_omp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vtor_omp"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"V_loop",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%V_loop)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: V_loop"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"v_pars",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%v_pars)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: v_pars"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"v_pers",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%v_pers)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: v_pers"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"wt_nphi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%wt_nphi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: wt_nphi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"wt_nphi_abs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%wt_nphi_abs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: wt_nphi_abs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"wt_nphi_ext",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%wt_nphi_ext)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: wt_nphi_ext"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"xRjcos_momeq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%xRjcos_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: xRjcos_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"xRjsin_momeq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%xRjsin_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: xRjsin_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"xs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%xs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: xs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"xs_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%xs_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: xs_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"xZjcos_momeq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%xZjcos_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: xZjcos_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"xZjsin_momeq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%xZjsin_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: xZjsin_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z0max",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z0max)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z0max"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z0_momeq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z0_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z0_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zbap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Zbap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Zbap"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zbsc",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Zbsc)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Zbsc"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zeff",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Zeff)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Zeff"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ZEFF_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ZEFF_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ZEFF_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zeff_fi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Zeff_fi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Zeff_fi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zeff_th",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Zeff_th)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Zeff_th"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zimp1",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Zimp1)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Zimp1"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zimp2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Zimp2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Zimp2"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"zlim",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%zlim)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: zlim"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zloc_coil",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Zloc_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Zloc_coil"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_ant",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_ant)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_ant"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_antgeo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_antgeo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_antgeo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_axis",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_axis)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_axis"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_EC_launch",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_EC_launch)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_EC_launch"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_geo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_geo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_geo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_grid",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_grid)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_grid"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_max_box",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_max_box)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_max_box"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_max_lcfs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_max_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_max_lcfs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_midp",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_midp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_midp"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_mid_ant",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_mid_ant)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_mid_ant"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_min_box",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_min_box)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_min_box"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_min_lcfs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_min_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_min_lcfs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_ripple",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_ripple)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_ripple"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_surfMax",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_surfMax)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_surfMax"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_surfMin",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Z_surfMin)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Z_surfMin"
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

END SUBROUTINE ps_update_file_write_9
