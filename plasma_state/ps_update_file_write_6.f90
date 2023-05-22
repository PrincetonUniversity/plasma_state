! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_write_def_6(ss,icdf,cclist,nids,iout,iwarn,iertot)


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

  iaddr = ps_phash_prad_li
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%prad_li).AND.ichange) THEN
     IF(cclist(ps_cnum_RAD).NE.1) THEN
        CALL ccwarn("prad_li","RAD")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_rad.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_rad,"dm1_nrho_rad",ss%nrho_rad-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_rad
        CALL define_var("prad_li","R",indim,idims, &
               "line radiation contribution", &
               "W","RAD","STATE_PROFILES", &
               "R|step*dV  prad_li(~nrho_rad)")
     ENDIF
  ENDIF

  iaddr = ps_phash_psc_halo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%psc_halo).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("psc_halo","NBI")
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
        CALL define_var("psc_halo","R",indim,idims, &
               "beam halo power: recapture - pb0_halo", &
               "W","NBI","STATE_PROFILES", &
               "R|step*dV   psc_halo(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_psc_reco
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%psc_reco).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("psc_reco","GAS")
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
        CALL define_var("psc_reco","R",indim,idims, &
               "reco power: recapture - p0_reco", &
               "W","GAS","STATE_PROFILES", &
               "R|step*dV   psc_reco(~nrho_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_psipol
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%psipol).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("psipol","EQ")
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
        CALL define_var("psipol","R",indim,idims, &
               "poloidal flux vs. rho", &
               "Wb/rad","EQ","STATE_PROFILES", &
               "R|units=Wb/rad|Spline_00   psipol(nrho_eq)")
     ENDIF
  ENDIF

  iaddr = ps_phash_PsiRZ
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%PsiRZ).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("PsiRZ","EQ")
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
        CALL define_var("PsiRZ","R",indim,idims, &
               "Psi(R,Z)", &
               "Wb/rad","EQ","STATE_PROFILES", &
               "R|units=Wb/rad|Spline  PsiRZ(nR,nZ)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Psi_to_machine_axis
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Psi_to_machine_axis","EQ")
     ELSE
        indim = 0
        CALL define_var("Psi_to_machine_axis","R",indim,idims, &
               "delta(Psi) from magnetic axis to machine axis", &
               "Wb/rad","EQ","STATE_DATA", &
               "R|units=Wb/rad   Psi_to_machine_axis")
     ENDIF
  ENDIF

  iaddr = ps_phash_psmom_errck
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%psmom_errck).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("psmom_errck","EQ")
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
        CALL define_var("psmom_errck","R",indim,idims, &
               "PS moments sum convergence", &
               "-","EQ","STATE_PROFILES", &
               "R|units=-|pclin       psmom_errck(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_psmom_nc
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%psmom_nc).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("psmom_nc","EQ")
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
        IF(nids%nid_dim_npsmom.EQ.0) THEN
           CALL define_dim(nids%nid_dim_npsmom,"dim_npsmom",ss%npsmom)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_npsmom
        CALL define_var("psmom_nc","R",indim,idims, &
               "PS moments", &
               "m^-2","EQ","STATE_PROFILES", &
               "R|units=m^-2|pclin    psmom_nc(nrho_eq_geo,npsmom)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_npsmom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%psmom_num).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("psmom_num","EQ")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_npsmom.EQ.0) THEN
           CALL define_dim(nids%nid_dim_npsmom,"dim_npsmom",ss%npsmom)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_npsmom
        CALL define_var("psmom_num","N",indim,idims, &
               "Pfirsch-Schlutter moments", &
               "__NONE","EQ","SIMULATION_INIT", &
               "L|PS_moments  psmom_num(npsmom)")
     ENDIF
  ENDIF

  iaddr = ps_phash_P_eq
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%P_eq).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("P_eq","EQ")
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
        CALL define_var("P_eq","R",indim,idims, &
               "equilibrium scalar pressure", &
               "Pa","EQ","STATE_PROFILES", &
               "R|units=Pa|Spline_00       P_eq(nrho_eq)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_all
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qatom_ALL).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("qatom_ALL","PLASMA")
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
        CALL define_var("qatom_ALL","R",indim,idims, &
               "ALL atomic number", &
               "C","PLASMA","SIMULATION_INIT", &
               "S|specie  ALL(0:nspec_all)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_alla
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qatom_ALLA).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("qatom_ALLA","PLASMA")
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
        CALL define_var("qatom_ALLA","R",indim,idims, &
               "ALLA atomic number", &
               "C","PLASMA","SIMULATION_INIT", &
               "S|specie  ALLA(0:nspec_alla)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_rfmin
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qatom_RFMIN).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("qatom_RFMIN","IC")
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
        CALL define_var("qatom_RFMIN","R",indim,idims, &
               "RFMIN atomic number", &
               "C","IC","SHOT_CONFIGURATION", &
               "S|RF_minority  RFMIN(nspec_rfmin)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_th
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qatom_S).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("qatom_S","PLASMA")
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
        CALL define_var("qatom_S","R",indim,idims, &
               "S atomic number", &
               "C","PLASMA","SHOT_CONFIGURATION", &
               "S|thermal_specie  S(0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_tha
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qatom_SA).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("qatom_SA","PLASMA")
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
        CALL define_var("qatom_SA","R",indim,idims, &
               "SA atomic number", &
               "C","PLASMA","SIMULATION_INIT", &
               "S|thermal_specie  SA(0:nspec_tha)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_fusion
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qatom_SFUS).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("qatom_SFUS","FUS")
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
        CALL define_var("qatom_SFUS","R",indim,idims, &
               "SFUS atomic number", &
               "C","FUS","SHOT_CONFIGURATION", &
               "S|fusion_ion   SFUS(nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_gas
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qatom_SGAS).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("qatom_SGAS","GAS")
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
        CALL define_var("qatom_SGAS","R",indim,idims, &
               "SGAS atomic number", &
               "C","GAS","SIMULATION_INIT", &
               "S|neutral_gas  SGAS(nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_imp0
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qatom_SIMP0).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("qatom_SIMP0","GAS")
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
        CALL define_var("qatom_SIMP0","R",indim,idims, &
               "SIMP0 atomic number", &
               "C","GAS","SIMULATION_INIT", &
               "S|impurity_atoms SIMP0(nspec_imp0)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_impi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qatom_SIMPI).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("qatom_SIMPI","PLASMA")
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
        CALL define_var("qatom_SIMPI","R",indim,idims, &
               "SIMPI atomic number", &
               "C","PLASMA","SIMULATION_INIT", &
               "S|impurity_atoms SIMPI(nspec_impi)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_beam
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qatom_SNBI).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("qatom_SNBI","NBI")
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
        CALL define_var("qatom_SNBI","R",indim,idims, &
               "SNBI atomic number", &
               "C","NBI","SIMULATION_INIT", &
               "S|beam_ion     SNBI(nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_qbeami
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qbeami).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("qbeami","NBI")
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
        CALL define_var("qbeami","R",indim,idims, &
               "average beam ion charge", &
               "C","NBI","STATE_PROFILES", &
               "R|units=C|step*nbeami qbeami(~nrho_nbi,nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_qcomp_e
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qcomp_e).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("qcomp_e","PLASMA")
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
        CALL define_var("qcomp_e","R",indim,idims, &
               "Compressional electron heating", &
               "W","PLASMA","STATE_PROFILES", &
               "R|step*dV   qcomp_e(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_qcomp_i
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qcomp_i).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("qcomp_i","PLASMA")
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
        CALL define_var("qcomp_i","R",indim,idims, &
               "Compressional ion heating", &
               "W","PLASMA","STATE_PROFILES", &
               "R|step*dV   qcomp_i(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_qie
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qie).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("qie","PLASMA")
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
        CALL define_var("qie","R",indim,idims, &
               "Ion->Electron heat coupling", &
               "W","PLASMA","STATE_PROFILES", &
               "R|units=W|step*dV   qie(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_qioniz
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qioniz).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("qioniz","GAS")
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
        CALL define_var("qioniz","R",indim,idims, &
               "ionization power per unit influx (sc0)", &
               "W/(#/sec)","GAS","STATE_PROFILES", &
               "R|step*dV|units=W/(#/sec)     qioniz(~nrho_gas,ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ql_operator
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ql_operator).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("ql_operator","IC")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_256
        IF(nids%nid_dp1_nspec_alla.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_alla,"dp1_nspec_alla",ss%nspec_alla+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_alla
        CALL define_var("ql_operator","F",indim,idims, &
               "quasilinear operator filenames", &
               "__NONE","IC","STATE_DATA", &
               "F  ql_operator(0:nspec_alla)")
     ENDIF
  ENDIF

  iaddr = ps_phash_qqcx
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qqcx).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("qqcx","GAS")
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
        CALL define_var("qqcx","R",indim,idims, &
               "cx power per unit influx per (T0-Ti)", &
               "W/(#/sec)/keV","GAS","STATE_PROFILES", &
               "R|step*dV|units=W/(#/sec)/keV  qqcx(~nrho_gas,ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_phash_qrot_conv
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qrot_conv).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("qrot_conv","PLASMA")
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
        CALL define_var("qrot_conv","R",indim,idims, &
               "rotational energy dissipation (convective)", &
               "W","PLASMA","STATE_PROFILES", &
               "R|step*dV   qrot_conv(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_qrot_diff
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%qrot_diff).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("qrot_diff","PLASMA")
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
        CALL define_var("qrot_diff","R",indim,idims, &
               "rotational energy dissipation (diffusive)", &
               "W","PLASMA","STATE_PROFILES", &
               "R|step*dV   qrot_diff(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_all
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%q_ALL).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("q_ALL","PLASMA")
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
        CALL define_var("q_ALL","R",indim,idims, &
               "ALL specie charge", &
               "C","PLASMA","SIMULATION_INIT", &
               "S|specie  ALL(0:nspec_all)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_alla
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%q_ALLA).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("q_ALLA","PLASMA")
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
        CALL define_var("q_ALLA","R",indim,idims, &
               "ALLA specie charge", &
               "C","PLASMA","SIMULATION_INIT", &
               "S|specie  ALLA(0:nspec_alla)")
     ENDIF
  ENDIF

  iaddr = ps_phash_q_eq
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%q_eq).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("q_eq","EQ")
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
        CALL define_var("q_eq","R",indim,idims, &
               "equilibrium q profile", &
               "-","EQ","STATE_PROFILES", &
               "R|units=-|Spline           q_eq(nrho_eq)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_rfmin
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%q_RFMIN).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("q_RFMIN","IC")
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
        CALL define_var("q_RFMIN","R",indim,idims, &
               "RFMIN specie charge", &
               "C","IC","SHOT_CONFIGURATION", &
               "S|RF_minority  RFMIN(nspec_rfmin)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_th
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%q_S).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("q_S","PLASMA")
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
        CALL define_var("q_S","R",indim,idims, &
               "S specie charge", &
               "C","PLASMA","SHOT_CONFIGURATION", &
               "S|thermal_specie  S(0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_tha
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%q_SA).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("q_SA","PLASMA")
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
        CALL define_var("q_SA","R",indim,idims, &
               "SA specie charge", &
               "C","PLASMA","SIMULATION_INIT", &
               "S|thermal_specie  SA(0:nspec_tha)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_fusion
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%q_SFUS).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("q_SFUS","FUS")
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
        CALL define_var("q_SFUS","R",indim,idims, &
               "SFUS specie charge", &
               "C","FUS","SHOT_CONFIGURATION", &
               "S|fusion_ion   SFUS(nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_gas
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%q_SGAS).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("q_SGAS","GAS")
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
        CALL define_var("q_SGAS","R",indim,idims, &
               "SGAS specie charge", &
               "C","GAS","SIMULATION_INIT", &
               "S|neutral_gas  SGAS(nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_imp0
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%q_SIMP0).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("q_SIMP0","GAS")
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
        CALL define_var("q_SIMP0","R",indim,idims, &
               "SIMP0 specie charge", &
               "C","GAS","SIMULATION_INIT", &
               "S|impurity_atoms SIMP0(nspec_imp0)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_impi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%q_SIMPI).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("q_SIMPI","PLASMA")
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
        CALL define_var("q_SIMPI","R",indim,idims, &
               "SIMPI specie charge", &
               "C","PLASMA","SIMULATION_INIT", &
               "S|impurity_atoms SIMPI(nspec_impi)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_beam
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%q_SNBI).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("q_SNBI","NBI")
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
        CALL define_var("q_SNBI","R",indim,idims, &
               "SNBI specie charge", &
               "C","NBI","SIMULATION_INIT", &
               "S|beam_ion     SNBI(nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_R0_momeq
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%R0_momeq).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("R0_momeq","EQ")
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
        CALL define_var("R0_momeq","R",indim,idims, &
               "R0 of flux surface", &
               "m","EQ","STATE_PROFILES", &
               "R|units=m|Hermite_explicit  R0_momeq(nrho_eq)")
     ENDIF
  ENDIF

  iaddr = ps_shash_RAD_Code_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_RAD).NE.1) THEN
        CALL ccwarn("RAD_Code_Info","RAD")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("RAD_Code_Info","C*80",indim,idims, &
               "Information: code implementing RAD component", &
               "__NONE","RAD","SIMULATION_INIT", &
               "C*80   RAD_Code_Info")
     ENDIF
  ENDIF

  iaddr = ps_shash_RAD_Data_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_RAD).NE.1) THEN
        CALL ccwarn("RAD_Data_Info","RAD")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("RAD_Data_Info","C*80",indim,idims, &
               "Information on source of radiated power data", &
               "__NONE","RAD","SIMULATION_INIT", &
               "C*80   RAD_Data_Info")
     ENDIF
  ENDIF

  iaddr = ps_phash_rate_sinb0i
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rate_sinb0i).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("rate_sinb0i","NBI")
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
        CALL define_var("rate_sinb0i","R",indim,idims, &
               "sink rate due to ionization by beam ions", &
               "1/sec","NBI","STATE_PROFILES", &
               "R|step|units=1/sec       rate_sinb0i(~nrho_nbi,nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_rate_sinb0x
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rate_sinb0x).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("rate_sinb0x","NBI")
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
        CALL define_var("rate_sinb0x","R",indim,idims, &
               "sink rate due to CX with beam ions (total)", &
               "1/sec","NBI","STATE_PROFILES", &
               "R|step|units=1/sec       rate_sinb0x(~nrho_nbi,nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_rate_sinb0xs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rate_sinb0xs).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("rate_sinb0xs","NBI")
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
        IF(nids%nid_dim_nspec_beam.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_beam,"dim_nspec_beam",ss%nspec_beam)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_beam
        CALL define_var("rate_sinb0xs","R",indim,idims, &
               "sink rate due to CX with beam ions", &
               "1/sec","NBI","STATE_PROFILES", &
               "R|step|units=1/sec       rate_sinb0xs(~nrho_nbi,nspec_gas,nspec_beam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_rate_sinf0i
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rate_sinf0i).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("rate_sinf0i","FUS")
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
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        CALL define_var("rate_sinf0i","R",indim,idims, &
               "sink rate due to ionization by fusion ions", &
               "1/sec","FUS","STATE_PROFILES", &
               "R|step|units=1/sec       rate_sinf0i(~nrho_fus,nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_rate_sinf0x
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rate_sinf0x).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("rate_sinf0x","FUS")
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
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        CALL define_var("rate_sinf0x","R",indim,idims, &
               "sink rate due to CX with fusion ions (total)", &
               "1/sec","FUS","STATE_PROFILES", &
               "R|step|units=1/sec       rate_sinf0x(~nrho_fus,nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_rate_sinf0xs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rate_sinf0xs).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("rate_sinf0xs","FUS")
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
        IF(nids%nid_dim_nspec_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_gas,"dim_nspec_gas",ss%nspec_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_gas
        IF(nids%nid_dim_nspec_fusion.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_fusion,"dim_nspec_fusion",ss%nspec_fusion)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_fusion
        CALL define_var("rate_sinf0xs","R",indim,idims, &
               "sink rate due to CX with fusion ions", &
               "1/sec","FUS","STATE_PROFILES", &
               "R|step|units=1/sec       rate_sinf0xs(~nrho_fus,nspec_gas,nspec_fusion)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Real_ant_coef
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Real_ant_coef).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("Real_ant_coef","IC")
     ELSE
        indim = 0
        IF(nids%nid_dim_max_num_nphi_vac.EQ.0) THEN
           CALL define_dim(nids%nid_dim_max_num_nphi_vac,"dim_max_num_nphi_vac",ss%max_num_nphi_vac)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_max_num_nphi_vac
        IF(nids%nid_dim_nicrf_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nicrf_src,"dim_nicrf_src",ss%nicrf_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nicrf_src
        CALL define_var("Real_ant_coef","R",indim,idims, &
               "real part of Fourier Coef", &
               "Amps/m","IC","SHOT_CONFIGURATION", &
               "R|units=Amps/m Real_ant_coef(num_nphi_vac(),nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_res_sn
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%res_sn).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("res_sn","PLASMA")
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
        CALL define_var("res_sn","R",indim,idims, &
               "residuals for particle transport eqn", &
               "Norm","PLASMA","STATE_PROFILES", &
               "R|units=Norm|step*dV res_sn(~nrho,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_res_te
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%res_te).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("res_te","PLASMA")
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
        CALL define_var("res_te","R",indim,idims, &
               "residuals for electron transport eqn", &
               "Norm","PLASMA","STATE_PROFILES", &
               "R|step*dV res_te(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_res_ti
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%res_ti).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("res_ti","PLASMA")
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
        CALL define_var("res_ti","R",indim,idims, &
               "residuals for ion transport eqn", &
               "Norm","PLASMA","STATE_PROFILES", &
               "R|step*dV res_ti(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_res_tq
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%res_tq).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("res_tq","PLASMA")
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
        CALL define_var("res_tq","R",indim,idims, &
               "rediduals for angular momentum transport eqn", &
               "Norm","PLASMA","STATE_PROFILES", &
               "R|units=Norm|step*dV res_tq(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_rfmin
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%RFMIN_name).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("RFMIN_name","IC")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_nspec_rfmin.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nspec_rfmin,"dim_nspec_rfmin",ss%nspec_rfmin)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nspec_rfmin
        CALL define_var("RFMIN_name","N",indim,idims, &
               "ICRF minority species", &
               "__NONE","IC","SHOT_CONFIGURATION", &
               "S|RF_minority  RFMIN(nspec_rfmin)")
     ENDIF
  ENDIF

  iaddr = ps_shash_rfmin_to_all
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rfmin_to_all).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("rfmin_to_all","PLASMA")
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
        CALL define_var("rfmin_to_all","I",indim,idims, &
               'map rfmin index to "all" list', &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "I  rfmin_to_all(nspec_rfmin)")
     ENDIF
  ENDIF

  iaddr = ps_shash_rfmin_to_alla
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rfmin_to_alla).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("rfmin_to_alla","PLASMA")
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
        CALL define_var("rfmin_to_alla","I",indim,idims, &
               'map rfmin index to "alla" list', &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "I  rfmin_to_alla(nspec_rfmin)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nspec_rfmin
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%RFMIN_type).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("RFMIN_type","IC")
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
        CALL define_var("RFMIN_type","I",indim,idims, &
               "RFMIN specie types", &
               "__NONE","IC","SHOT_CONFIGURATION", &
               "S|RF_minority  RFMIN(nspec_rfmin)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("rho","PLASMA")
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
        CALL define_var("rho","R",indim,idims, &
               "rho grid (PLASMA)", &
               "-","PLASMA","SIMULATION_INIT", &
               "G   rho(nrho)")
     ENDIF
  ENDIF

  iaddr = ps_shash_rhopsi0lh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rhopsi0lh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("rhopsi0lh","LH")
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
        CALL define_var("rhopsi0lh","R",indim,idims, &
               "need description", &
               "-","LH","SHOT_CONFIGURATION", &
               "R|units=- rhopsi0lh(nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_anom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_anom).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("rho_anom","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_anom.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anom,"dim_nrho_anom",ss%nrho_anom)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anom
        CALL define_var("rho_anom","R",indim,idims, &
               "rho grid (1d anomalous transport profiles)", &
               "-","ANOM","SIMULATION_INIT", &
               "G  rho_anom(nrho_anom)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_anom2
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_anom2).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("rho_anom2","ANOM")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_anom2.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_anom2,"dim_nrho_anom2",ss%nrho_anom2)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_anom2
        CALL define_var("rho_anom2","R",indim,idims, &
               "rho grid (2d anomalous transport profiles)", &
               "-","ANOM","SIMULATION_INIT", &
               "G  rho_anom2(nrho_anom2)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_anomth
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_anomth).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("rho_anomth","ANOM")
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
        CALL define_var("rho_anomth","R",indim,idims, &
               "rho grid (1d anomalous data for 1d", &
               "-","ANOM","SIMULATION_INIT", &
               "G  rho_anomth(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_rho_bdy_ns
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_bdy_ns).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("rho_bdy_ns","PLASMA")
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
        CALL define_var("rho_bdy_ns","R",indim,idims, &
               "locations beyond which densities are known", &
               "-","PLASMA","STATE_DATA", &
               "R|units=- rho_bdy_ns(0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_shash_rho_bdy_omegat
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("rho_bdy_omegat","PLASMA")
     ELSE
        indim = 0
        CALL define_var("rho_bdy_omegat","R",indim,idims, &
               "location beyond which angular velocity is known", &
               "-","PLASMA","STATE_DATA", &
               "R rho_bdy_omegat")
     ENDIF
  ENDIF

  iaddr = ps_shash_rho_bdy_Te
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("rho_bdy_Te","PLASMA")
     ELSE
        indim = 0
        CALL define_var("rho_bdy_Te","R",indim,idims, &
               "location beyond which Te is known", &
               "-","PLASMA","STATE_DATA", &
               "R rho_bdy_Te")
     ENDIF
  ENDIF

  iaddr = ps_shash_rho_bdy_Ti
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("rho_bdy_Ti","PLASMA")
     ELSE
        indim = 0
        CALL define_var("rho_bdy_Ti","R",indim,idims, &
               "location beyond which Ti is known", &
               "-","PLASMA","STATE_DATA", &
               "R rho_bdy_Ti")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_ecrf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_ecrf).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("rho_ecrf","EC")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_ecrf.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_ecrf,"dim_nrho_ecrf",ss%nrho_ecrf)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_ecrf
        CALL define_var("rho_ecrf","R",indim,idims, &
               "rho grid -- ECRF", &
               "-","EC","SIMULATION_INIT", &
               "G  rho_ecrf(nrho_ecrf)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_eq
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_eq).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("rho_eq","EQ")
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
        CALL define_var("rho_eq","R",indim,idims, &
               "rho grid (EQ)", &
               "-","EQ","SIMULATION_INIT", &
               "G   rho_eq(nrho_eq)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_eq_geo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_eq_geo).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("rho_eq_geo","EQ")
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
        CALL define_var("rho_eq_geo","R",indim,idims, &
               "rho grid (EQ flux surface averages)", &
               "-","EQ","SIMULATION_INIT", &
               "G   rho_eq_geo(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_fus
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_fus).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("rho_fus","FUS")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_fus.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_fus,"dim_nrho_fus",ss%nrho_fus)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_fus
        CALL define_var("rho_fus","R",indim,idims, &
               "rho grid -- Fusion products model", &
               "-","FUS","SIMULATION_INIT", &
               "G  rho_fus(nrho_fus)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_gas
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_gas).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("rho_gas","GAS")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_gas.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_gas,"dim_nrho_gas",ss%nrho_gas)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_gas
        CALL define_var("rho_gas","R",indim,idims, &
               "rho grid (GAS -- neutral gas model)", &
               "-","GAS","SIMULATION_INIT", &
               "G  rho_gas(nrho_gas)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_icrf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_icrf).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("rho_icrf","IC")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_icrf.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_icrf,"dim_nrho_icrf",ss%nrho_icrf)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_icrf
        CALL define_var("rho_icrf","R",indim,idims, &
               "rho grid (RF)", &
               "-","IC","SIMULATION_INIT", &
               "G  rho_icrf(nrho_icrf)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_lhrf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_lhrf).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("rho_lhrf","LH")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_lhrf.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_lhrf,"dim_nrho_lhrf",ss%nrho_lhrf)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_lhrf
        CALL define_var("rho_lhrf","R",indim,idims, &
               "rho grid -- LHRF", &
               "-","LH","SIMULATION_INIT", &
               "G  rho_lhrf(nrho_lhrf)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_lmhd
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_lmhd).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("rho_lmhd","LMHD")
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
        CALL define_var("rho_lmhd","R",indim,idims, &
               "rho grid (LMHD component)", &
               "-","LMHD","SIMULATION_INIT", &
               "G  rho_lmhd(nrho_lmhd)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_mist
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_mist).AND.ichange) THEN
     IF(cclist(ps_cnum_RAD).NE.1) THEN
        CALL ccwarn("rho_mist","RAD")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_mist.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_mist,"dim_nrho_mist",ss%nrho_mist)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_mist
        CALL define_var("rho_mist","R",indim,idims, &
               "rho grid (PLASMA) impurity-driven sources", &
               "-","RAD","SIMULATION_INIT", &
               "G  rho_mist(nrho_mist)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nrho_nbi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%rho_nbi).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("rho_nbi","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dim_nrho_nbi.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nrho_nbi,"dim_nrho_nbi",ss%nrho_nbi)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nrho_nbi
        CALL define_var("rho_nbi","R",indim,idims, &
               "rho grid -- NBI", &
               "-","NBI","SIMULATION_INIT", &
               "G  rho_nbi(nrho_nbi)")
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

END SUBROUTINE ps_update_file_write_def_6

SUBROUTINE ps_update_file_write_6(ss,icdf,cclist,iucount,iout,iwarn,iertot)


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


  status = nf90_inq_varid(icdf,"prad_li",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%prad_li)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: prad_li"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"psc_halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%psc_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: psc_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"psc_reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%psc_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: psc_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"psipol",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%psipol)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: psipol"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"PsiRZ",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%PsiRZ)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: PsiRZ"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Psi_to_machine_axis",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Psi_to_machine_axis)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Psi_to_machine_axis"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"psmom_errck",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%psmom_errck)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: psmom_errck"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"psmom_nc",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%psmom_nc)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: psmom_nc"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"psmom_num",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%psmom_num)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: psmom_num"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"P_eq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%P_eq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: P_eq"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_ALL",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qatom_ALL)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qatom_ALL"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_ALLA",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qatom_ALLA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qatom_ALLA"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_RFMIN",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qatom_RFMIN)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qatom_RFMIN"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_S",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qatom_S)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qatom_S"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_SA",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qatom_SA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qatom_SA"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_SFUS",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qatom_SFUS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qatom_SFUS"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_SGAS",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qatom_SGAS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qatom_SGAS"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_SIMP0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qatom_SIMP0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qatom_SIMP0"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_SIMPI",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qatom_SIMPI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qatom_SIMPI"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_SNBI",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qatom_SNBI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qatom_SNBI"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qbeami",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qbeami)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qbeami"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qcomp_e",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qcomp_e)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qcomp_e"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qcomp_i",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qcomp_i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qcomp_i"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qie",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qie)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qie"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qioniz",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qioniz)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qioniz"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ql_operator",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ql_operator)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ql_operator"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qqcx",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qqcx)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qqcx"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qrot_conv",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qrot_conv)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qrot_conv"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qrot_diff",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%qrot_diff)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: qrot_diff"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_ALL",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%q_ALL)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: q_ALL"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_ALLA",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%q_ALLA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: q_ALLA"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_eq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%q_eq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: q_eq"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_RFMIN",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%q_RFMIN)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: q_RFMIN"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_S",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%q_S)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: q_S"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_SA",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%q_SA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: q_SA"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_SFUS",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%q_SFUS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: q_SFUS"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_SGAS",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%q_SGAS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: q_SGAS"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_SIMP0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%q_SIMP0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: q_SIMP0"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_SIMPI",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%q_SIMPI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: q_SIMPI"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_SNBI",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%q_SNBI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: q_SNBI"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R0_momeq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%R0_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: R0_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RAD_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%RAD_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: RAD_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RAD_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%RAD_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: RAD_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rate_sinb0i",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rate_sinb0i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rate_sinb0i"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rate_sinb0x",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rate_sinb0x)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rate_sinb0x"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rate_sinb0xs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rate_sinb0xs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rate_sinb0xs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rate_sinf0i",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rate_sinf0i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rate_sinf0i"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rate_sinf0x",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rate_sinf0x)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rate_sinf0x"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rate_sinf0xs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rate_sinf0xs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rate_sinf0xs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Real_ant_coef",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Real_ant_coef)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Real_ant_coef"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"res_sn",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%res_sn)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: res_sn"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"res_te",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%res_te)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: res_te"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"res_ti",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%res_ti)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: res_ti"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"res_tq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%res_tq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: res_tq"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RFMIN_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%RFMIN_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: RFMIN_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rfmin_to_all",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rfmin_to_all)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rfmin_to_all"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rfmin_to_alla",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rfmin_to_alla)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rfmin_to_alla"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RFMIN_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%RFMIN_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: RFMIN_type"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rhopsi0lh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rhopsi0lh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rhopsi0lh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_anom2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_anom2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_anom2"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_anomth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_anomth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_anomth"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_ns",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_bdy_ns)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_bdy_ns"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_omegat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_bdy_omegat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_bdy_omegat"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_Te",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_bdy_Te)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_bdy_Te"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_Ti",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_bdy_Ti)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_bdy_Ti"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_ecrf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_ecrf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_ecrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_eq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_eq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_eq"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_eq_geo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_eq_geo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_eq_geo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_fus",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_fus)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_fus"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_gas",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_gas)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_gas"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_icrf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_icrf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_icrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_lhrf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_lhrf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_lhrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_lmhd",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_lmhd)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_lmhd"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_mist",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_mist)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_mist"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_nbi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%rho_nbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: rho_nbi"
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

END SUBROUTINE ps_update_file_write_6
