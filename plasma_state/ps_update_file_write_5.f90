! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_write_def_5(ss,icdf,cclist,nids,iout,iwarn,iertot)


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

  iaddr = ps_shash_ns_bdy
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ns_bdy).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("ns_bdy","PLASMA")
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
        CALL define_var("ns_bdy","R",indim,idims, &
               "specie densities at/beyond boundary", &
               "m^-3","PLASMA","STATE_DATA", &
               "R|units=m^-3 ns_bdy(0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ns_Data_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("ns_Data_Info","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("ns_Data_Info","C*80",indim,idims, &
               "information on source of Density data", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "C*80   ns_Data_Info")
     ENDIF
  ENDIF

  iaddr = ps_shash_ns_is_input
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ns_is_input).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("ns_is_input","PLASMA")
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
        CALL define_var("ns_is_input","I",indim,idims, &
               "specie density measurement flag", &
               "__NONE","PLASMA","STATE_DATA", &
               "I ns_is_input(0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_shash_ntf_coil2
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_RIPPLE).NE.1) THEN
        CALL ccwarn("ntf_coil2","RIPPLE")
     ELSE
        indim = 0
        CALL define_var("ntf_coil2","I",indim,idims, &
               "number of TF coils involved in inducing 2nd component", &
               "__NONE","RIPPLE","MACHINE_DESCRIPTION", &
               "I ntf_coil2")
     ENDIF
  ENDIF

  iaddr = ps_shash_ntf_coils
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_RIPPLE).NE.1) THEN
        CALL ccwarn("ntf_coils","RIPPLE")
     ELSE
        indim = 0
        CALL define_var("ntf_coils","I",indim,idims, &
               "number of TF coils", &
               "__NONE","RIPPLE","MACHINE_DESCRIPTION", &
               "I ntf_coils")
     ENDIF
  ENDIF

  iaddr = ps_shash_nthinlh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nthinlh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("nthinlh","LH")
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
        CALL define_var("nthinlh","I",indim,idims, &
               "need description", &
               "__NONE","LH","SHOT_CONFIGURATION", &
               "I nthinlh(nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Nturns
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Nturns).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("Nturns","EQ")
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
        CALL define_var("Nturns","I",indim,idims, &
               "SIGNED number of conductor turns wrapped in each coil", &
               "__NONE","EQ","MACHINE_DESCRIPTION", &
               "I  Nturns(ncoils)")
     ENDIF
  ENDIF

  iaddr = ps_shash_num_nphi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%num_nphi).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("num_nphi","IC")
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
        CALL define_var("num_nphi","I",indim,idims, &
               "number of non-zero n_phi values", &
               "__NONE","IC","SHOT_CONFIGURATION", &
               "I|ENUM:nphi  num_nphi(nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_num_nphi_vac
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%num_nphi_vac).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("num_nphi_vac","IC")
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
        CALL define_var("num_nphi_vac","I",indim,idims, &
               "number of non-zero n_phi values", &
               "__NONE","IC","SHOT_CONFIGURATION", &
               "I|ENUM num_nphi_vac(nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_nzflux_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nzflux_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("nzflux_ky_tglf","ANOM")
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
        CALL define_var("nzflux_ky_tglf","R",indim,idims, &
               "impurity density flux spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin nzflux_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_nz_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%nz_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("nz_ky_tglf","ANOM")
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
        CALL define_var("nz_ky_tglf","R",indim,idims, &
               "impurity density fluctuation spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin nz_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_n_straps
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%n_straps).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("n_straps","IC")
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
        CALL define_var("n_straps","I",indim,idims, &
               "number of straps in the antenna", &
               "__NONE","IC","SHOT_CONFIGURATION", &
               "I|ENUM n_straps(nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_omeg0cx
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%omeg0cx).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("omeg0cx","GAS")
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
        CALL define_var("omeg0cx","R",indim,idims, &
               "effective neutral angular velocity for CX torque", &
               "rad/sec","GAS","STATE_PROFILES", &
               "R|step*tqqcx|units=rad/sec  omeg0cx(~nrho_gas,ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_phash_omeg0sc0
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%omeg0sc0).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("omeg0sc0","GAS")
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
        CALL define_var("omeg0sc0","R",indim,idims, &
               "toroidal angular velocity", &
               "(rad/sec)","GAS","STATE_PROFILES", &
               "R|step*n0norm|units=(rad/sec)  omeg0sc0(~nrho_gas,nspec_gas,ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_phash_omeg0_halo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%omeg0_halo).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("omeg0_halo","NBI")
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
        CALL define_var("omeg0_halo","R",indim,idims, &
               "halo neutral toroidal angular velocity", &
               "rad/sec","NBI","STATE_PROFILES", &
               "R|units=rad/sec|step*n0_halo  omeg0_halo(~nrho_nbi,nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_omeg0_reco
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%omeg0_reco).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("omeg0_reco","GAS")
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
        CALL define_var("omeg0_reco","R",indim,idims, &
               "reco neutral toroidal angular velocity", &
               "rad/sec","GAS","STATE_PROFILES", &
               "R|units=rad/sec|step*n0_reco  omeg0_reco(~nrho_gas,nspec_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_omegat
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%omegat).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("omegat","PLASMA")
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
        CALL define_var("omegat","R",indim,idims, &
               "ion toroidal angular velocity", &
               "rad/sec","PLASMA","STATE_PROFILES", &
               "R|units=rad/sec|step*ni  omegat(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_shash_omegat_bdy
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("omegat_bdy","PLASMA")
     ELSE
        indim = 0
        CALL define_var("omegat_bdy","R",indim,idims, &
               "toroidal angular velocity at/beyond boundary", &
               "rad/sec","PLASMA","STATE_DATA", &
               "R|units=rad/sec  omegat_bdy")
     ENDIF
  ENDIF

  iaddr = ps_phash_omega_exb
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%omega_exb).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("omega_exb","ANOM")
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
        CALL define_var("omega_exb","R",indim,idims, &
               "ExB flow shear rate", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin omega_exb(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_p0_reco
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%p0_reco).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("p0_reco","GAS")
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
        CALL define_var("p0_reco","R",indim,idims, &
               "power in recombination neutral source", &
               "W","GAS","STATE_PROFILES", &
               "R|step*dV   p0_reco(~nrho_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pb0_halo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pb0_halo).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("pb0_halo","NBI")
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
        CALL define_var("pb0_halo","R",indim,idims, &
               "power in halo neutral source", &
               "W","NBI","STATE_PROFILES", &
               "R|step*dV   pb0_halo(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pbe
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pbe).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("pbe","NBI")
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
        CALL define_var("pbe","R",indim,idims, &
               "electron heating by all beam ions", &
               "W","NBI","STATE_PROFILES", &
               "R|step*dV   pbe(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pbi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pbi).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("pbi","NBI")
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
        CALL define_var("pbi","R",indim,idims, &
               "thermal ion heating by all beam ions", &
               "W","NBI","STATE_PROFILES", &
               "R|step*dV   pbi(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pbth
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pbth).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("pbth","NBI")
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
        CALL define_var("pbth","R",indim,idims, &
               "thermalization of beam ions", &
               "W","NBI","STATE_PROFILES", &
               "R|step*dV   pbth(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pcx_halo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pcx_halo).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("pcx_halo","NBI")
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
        CALL define_var("pcx_halo","R",indim,idims, &
               "beam halo driven CX power", &
               "W","NBI","STATE_PROFILES", &
               "R|step*dV   pcx_halo(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pcx_reco
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pcx_reco).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("pcx_reco","GAS")
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
        CALL define_var("pcx_reco","R",indim,idims, &
               "reco driven CX power", &
               "W","GAS","STATE_PROFILES", &
               "R|step*dV   pcx_reco(~nrho_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_peech
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%peech).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("peech","EC")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_ecrf.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_ecrf,"dm1_nrho_ecrf",ss%nrho_ecrf-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_ecrf
        CALL define_var("peech","R",indim,idims, &
               "electron heating by ECH", &
               "W","EC","STATE_PROFILES", &
               "R|step*dV|units=W   peech(~nrho_ecrf)")
     ENDIF
  ENDIF

  iaddr = ps_phash_peech_src
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%peech_src).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("peech_src","EC")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_ecrf.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_ecrf,"dm1_nrho_ecrf",ss%nrho_ecrf-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_ecrf
        IF(nids%nid_dim_necrf_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_necrf_src,"dim_necrf_src",ss%necrf_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_necrf_src
        CALL define_var("peech_src","R",indim,idims, &
               "ECH heating (by antenna)", &
               "W","EC","STATE_PROFILES", &
               "R|step*dV|units=W   peech_src(~nrho_ecrf,necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pelh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pelh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("pelh","LH")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_lhrf.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_lhrf,"dm1_nrho_lhrf",ss%nrho_lhrf-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_lhrf
        CALL define_var("pelh","R",indim,idims, &
               "electron heating by LH", &
               "W","LH","STATE_PROFILES", &
               "R|step*dV   pelh(~nrho_lhrf)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pelh_src
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pelh_src).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("pelh_src","LH")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_lhrf.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_lhrf,"dm1_nrho_lhrf",ss%nrho_lhrf-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_lhrf
        IF(nids%nid_dim_nlhrf_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nlhrf_src,"dim_nlhrf_src",ss%nlhrf_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nlhrf_src
        CALL define_var("pelh_src","R",indim,idims, &
               "LH electron heating by antenna", &
               "W","LH","STATE_PROFILES", &
               "R|step*dV|units=W   pelh_src(~nrho_lhrf,nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_PEL_Code_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PEL).NE.1) THEN
        CALL ccwarn("PEL_Code_Info","PEL")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("PEL_Code_Info","C*80",indim,idims, &
               "Information: code implementing PELLET component", &
               "__NONE","PEL","SIMULATION_INIT", &
               "C*80   PEL_Code_Info")
     ENDIF
  ENDIF

  iaddr = ps_shash_PEL_Data_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PEL).NE.1) THEN
        CALL ccwarn("PEL_Data_Info","PEL")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("PEL_Data_Info","C*80",indim,idims, &
               "Information: on source of pellet injection", &
               "__NONE","PEL","SIMULATION_INIT", &
               "C*80   PEL_Data_Info")
     ENDIF
  ENDIF

  iaddr = ps_phash_pe_anom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pe_anom).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("pe_anom","ANOM")
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
        CALL define_var("pe_anom","R",indim,idims, &
               "anomalous electron heating", &
               "W","ANOM","STATE_PROFILES", &
               "R|units=W|step*dV  pe_anom(~nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pe_trans
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pe_trans).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("pe_trans","PLASMA")
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
        CALL define_var("pe_trans","R",indim,idims, &
               "electron transport (loss) power", &
               "W","PLASMA","STATE_PROFILES", &
               "R|step*dV   pe_trans(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pe_transcat
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pe_transcat).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("pe_transcat","PLASMA")
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
        IF(nids%nid_dim_ntrcat.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ntrcat,"dim_ntrcat",ss%ntrcat)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ntrcat
        CALL define_var("pe_transcat","R",indim,idims, &
               "electron transport", &
               "W","PLASMA","STATE_PROFILES", &
               "R|step*dV|units=W  pe_transcat(~nrho,ntrcat)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pfuse
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pfuse).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("pfuse","FUS")
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
        CALL define_var("pfuse","R",indim,idims, &
               "electron heating by all fusion ions", &
               "W","FUS","STATE_PROFILES", &
               "R|step*dV   pfuse(~nrho_fus)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pfusi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pfusi).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("pfusi","FUS")
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
        CALL define_var("pfusi","R",indim,idims, &
               "thermal ion heating by all fusion ions", &
               "W","FUS","STATE_PROFILES", &
               "R|step*dV   pfusi(~nrho_fus)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pfusth
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pfusth).AND.ichange) THEN
     IF(cclist(ps_cnum_FUS).NE.1) THEN
        CALL ccwarn("pfusth","FUS")
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
        CALL define_var("pfusth","R",indim,idims, &
               "thermalization of fusion ions", &
               "W","FUS","STATE_PROFILES", &
               "R|step*dV   pfusth(~nrho_fus)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Phibsc
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Phibsc).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("Phibsc","NBI")
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
        CALL define_var("Phibsc","R",indim,idims, &
               "toroidal location of beam source", &
               "degrees","NBI","MACHINE_DESCRIPTION", &
               "R|units=degrees       Phibsc(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_phiflux_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%phiflux_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("phiflux_ky_tglf","ANOM")
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
        CALL define_var("phiflux_ky_tglf","R",indim,idims, &
               "momentum fluctuation spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin phiflux_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_phit
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%phit).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("phit","EQ")
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
        CALL define_var("phit","R",indim,idims, &
               "toroidal flux vs. rho", &
               "Wb","EQ","STATE_PROFILES", &
               "R|units=Wb|Spline_00       phit(nrho_eq)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Phi_EC_launch
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Phi_EC_launch).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("Phi_EC_launch","EC")
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
        CALL define_var("Phi_EC_launch","R",indim,idims, &
               "Phi of center of launcher", &
               "degrees","EC","MACHINE_DESCRIPTION", &
               "R|units=degrees  Phi_EC_launch(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_picrf_abs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%picrf_abs).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("picrf_abs","IC")
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
        CALL define_var("picrf_abs","R",indim,idims, &
               "RF power absorbed inside plasma", &
               "W","IC","STATE_DATA", &
               "R|units=W picrf_abs(nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_picrf_ext
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%picrf_ext).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("picrf_ext","IC")
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
        CALL define_var("picrf_ext","R",indim,idims, &
               "RF power deposited outside plasma", &
               "W","IC","STATE_DATA", &
               "R|units=W picrf_ext(nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_picrf_nphi_srcs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%picrf_nphi_srcs).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("picrf_nphi_srcs","IC")
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
        IF(nids%nid_dp1_nspec_alla.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_alla,"dp1_nspec_alla",ss%nspec_alla+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_alla
        CALL define_var("picrf_nphi_srcs","R",indim,idims, &
               "direct ICRF power deposition", &
               "W","IC","STATE_PROFILES", &
               "R|units=W|alias=picrf_nphi_|step*dV   picrf_nphi_srcs(~nrho_icrf,num_nphi(),nicrf_src,0:nspec_alla)")
     ENDIF
  ENDIF

  iaddr = ps_phash_picrf_srcs
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%picrf_srcs).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("picrf_srcs","IC")
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
        IF(nids%nid_dp1_nspec_alla.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_alla,"dp1_nspec_alla",ss%nspec_alla+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_alla
        CALL define_var("picrf_srcs","R",indim,idims, &
               "direct ICRF power deposition", &
               "W","IC","STATE_PROFILES", &
               "R|units=W|alias=picrfs_|step*dV   picrf_srcs(~nrho_icrf,nicrf_src,0:nspec_alla)")
     ENDIF
  ENDIF

  iaddr = ps_phash_picrf_totals
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%picrf_totals).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("picrf_totals","IC")
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
        IF(nids%nid_dp1_nspec_alla.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_alla,"dp1_nspec_alla",ss%nspec_alla+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_alla
        CALL define_var("picrf_totals","R",indim,idims, &
               "direct ICRF power deposition", &
               "W","IC","STATE_PROFILES", &
               "R|units=W|alias=picrf_|step*dV    picrf_totals(~nrho_icrf,0:nspec_alla)")
     ENDIF
  ENDIF

  iaddr = ps_phash_picth
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%picth).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("picth","IC")
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
        CALL define_var("picth","R",indim,idims, &
               "direct thermal ion heating by ICRF", &
               "W","IC","STATE_PROFILES", &
               "R|step*dV|units=W   picth(~nrho_icrf)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pilh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pilh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("pilh","LH")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_lhrf.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_lhrf,"dm1_nrho_lhrf",ss%nrho_lhrf-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_lhrf
        CALL define_var("pilh","R",indim,idims, &
               "ion heating by LH", &
               "W","LH","STATE_PROFILES", &
               "R|step*dV   pilh(~nrho_lhrf)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pilh_src
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pilh_src).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("pilh_src","LH")
     ELSE
        indim = 0
        IF(nids%nid_dm1_nrho_lhrf.EQ.0) THEN
           CALL define_dim(nids%nid_dm1_nrho_lhrf,"dm1_nrho_lhrf",ss%nrho_lhrf-1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dm1_nrho_lhrf
        IF(nids%nid_dim_nlhrf_src.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nlhrf_src,"dim_nlhrf_src",ss%nlhrf_src)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nlhrf_src
        CALL define_var("pilh_src","R",indim,idims, &
               "LH ion heating by antenna", &
               "W","LH","STATE_PROFILES", &
               "R|step*dV|units=W   pilh_src(~nrho_lhrf,nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pi_anom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pi_anom).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("pi_anom","ANOM")
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
        CALL define_var("pi_anom","R",indim,idims, &
               "anomalous ion heating", &
               "W","ANOM","STATE_PROFILES", &
               "R|units=W|step*dV  pi_anom(~nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pi_trans
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pi_trans).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("pi_trans","PLASMA")
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
        CALL define_var("pi_trans","R",indim,idims, &
               "ion transport (loss) power", &
               "W","PLASMA","STATE_PROFILES", &
               "R|step*dV   pi_trans(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pi_transcat
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pi_transcat).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("pi_transcat","PLASMA")
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
        IF(nids%nid_dim_ntrcat.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ntrcat,"dim_ntrcat",ss%ntrcat)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ntrcat
        CALL define_var("pi_transcat","R",indim,idims, &
               "ion transport", &
               "W","PLASMA","STATE_PROFILES", &
               "R|step*dV|units=W  pi_transcat(~nrho,ntrcat)")
     ENDIF
  ENDIF

  iaddr = ps_shash_PLASMA_Code_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("PLASMA_Code_Info","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("PLASMA_Code_Info","C*80",indim,idims, &
               "Information: code managing PLASMA component", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "C*80   PLASMA_Code_Info")
     ENDIF
  ENDIF

  iaddr = ps_phash_pmine
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pmine).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("pmine","IC")
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
        CALL define_var("pmine","R",indim,idims, &
               "electron heating by minority ions", &
               "W","IC","STATE_PROFILES", &
               "R|step*dV   pmine(~nrho_icrf)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pmini
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pmini).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("pmini","IC")
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
        CALL define_var("pmini","R",indim,idims, &
               "thermal ion heating by minority ions", &
               "W","IC","STATE_PROFILES", &
               "R|step*dV   pmini(~nrho_icrf)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pminth
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pminth).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("pminth","IC")
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
        CALL define_var("pminth","R",indim,idims, &
               "(de)thermalization of minority ions", &
               "W","IC","STATE_PROFILES", &
               "R|step*dV   pminth(~nrho_icrf)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pohme
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pohme).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("pohme","PLASMA")
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
        CALL define_var("pohme","R",indim,idims, &
               "Ohmic heating of electrons", &
               "W","PLASMA","STATE_PROFILES", &
               "R|units=W|step*dV   pohme(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_pot_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%pot_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("pot_ky_tglf","ANOM")
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
        CALL define_var("pot_ky_tglf","R",indim,idims, &
               "potential fluctuation spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin pot_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_power_ec
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%power_ec).AND.ichange) THEN
     IF(cclist(ps_cnum_EC).NE.1) THEN
        CALL ccwarn("power_ec","EC")
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
        CALL define_var("power_ec","R",indim,idims, &
               "power on each ECRF source", &
               "W","EC","STATE_DATA", &
               "R|units=W  power_ec(necrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_power_ic
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%power_ic).AND.ichange) THEN
     IF(cclist(ps_cnum_IC).NE.1) THEN
        CALL ccwarn("power_ic","IC")
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
        CALL define_var("power_ic","R",indim,idims, &
               "power on each ICRF source", &
               "W","IC","STATE_DATA", &
               "R|units=W  power_ic(nicrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_power_lh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%power_lh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("power_lh","LH")
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
        CALL define_var("power_lh","R",indim,idims, &
               "power on each LHRF source", &
               "W","LH","STATE_DATA", &
               "R|units=W  power_lh(nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_shash_power_nbi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%power_nbi).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("power_nbi","NBI")
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
        CALL define_var("power_nbi","R",indim,idims, &
               "power on each beam source", &
               "W","NBI","STATE_DATA", &
               "R|units=W  power_nbi(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_shash_power_nbi_trace
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%power_nbi_trace).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("power_nbi_trace","NBI")
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
        CALL define_var("power_nbi_trace","R",indim,idims, &
               "power injected as trace element (if any)", &
               "W","NBI","STATE_DATA", &
               "R|units=W  power_nbi_trace(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_phash_prad
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%prad).AND.ichange) THEN
     IF(cclist(ps_cnum_RAD).NE.1) THEN
        CALL ccwarn("prad","RAD")
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
        CALL define_var("prad","R",indim,idims, &
               "total radiated power", &
               "W","RAD","STATE_PROFILES", &
               "R|step*dV  prad(~nrho_rad)")
     ENDIF
  ENDIF

  iaddr = ps_phash_prad_br
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%prad_br).AND.ichange) THEN
     IF(cclist(ps_cnum_RAD).NE.1) THEN
        CALL ccwarn("prad_br","RAD")
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
        CALL define_var("prad_br","R",indim,idims, &
               "bremsstrahlung contribution", &
               "W","RAD","STATE_PROFILES", &
               "R|step*dV  prad_br(~nrho_rad)")
     ENDIF
  ENDIF

  iaddr = ps_phash_prad_cy
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%prad_cy).AND.ichange) THEN
     IF(cclist(ps_cnum_RAD).NE.1) THEN
        CALL ccwarn("prad_cy","RAD")
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
        CALL define_var("prad_cy","R",indim,idims, &
               "cyclotron radiation contribution", &
               "W","RAD","STATE_PROFILES", &
               "R|step*dV  prad_cy(~nrho_rad)")
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

END SUBROUTINE ps_update_file_write_def_5

SUBROUTINE ps_update_file_write_5(ss,icdf,cclist,iucount,iout,iwarn,iertot)


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


  status = nf90_inq_varid(icdf,"ns_bdy",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ns_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ns_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ns_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ns_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ns_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ns_is_input",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ns_is_input)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ns_is_input"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ntf_coil2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ntf_coil2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ntf_coil2"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ntf_coils",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ntf_coils)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ntf_coils"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nthinlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nthinlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nthinlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Nturns",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Nturns)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Nturns"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"num_nphi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%num_nphi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: num_nphi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"num_nphi_vac",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%num_nphi_vac)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: num_nphi_vac"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nzflux_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nzflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nzflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nz_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%nz_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: nz_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"n_straps",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%n_straps)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: n_straps"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omeg0cx",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%omeg0cx)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: omeg0cx"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omeg0sc0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%omeg0sc0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: omeg0sc0"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omeg0_halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%omeg0_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: omeg0_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omeg0_reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%omeg0_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: omeg0_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omegat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%omegat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: omegat"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omegat_bdy",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%omegat_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: omegat_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omega_exb",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%omega_exb)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: omega_exb"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"p0_reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%p0_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: p0_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pb0_halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pb0_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pb0_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pbe",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pbe)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pbe"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pbi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pbi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pbth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pbth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pbth"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pcx_halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pcx_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pcx_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pcx_reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pcx_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pcx_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"peech",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%peech)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: peech"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"peech_src",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%peech_src)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: peech_src"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pelh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pelh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pelh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pelh_src",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pelh_src)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pelh_src"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"PEL_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%PEL_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: PEL_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"PEL_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%PEL_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: PEL_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pe_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pe_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pe_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pe_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pe_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pe_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pe_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pe_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pe_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pfuse",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pfuse)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pfuse"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pfusi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pfusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pfusi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pfusth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pfusth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pfusth"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Phibsc",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Phibsc)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Phibsc"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"phiflux_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%phiflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: phiflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"phit",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%phit)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: phit"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Phi_EC_launch",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Phi_EC_launch)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Phi_EC_launch"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"picrf_abs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%picrf_abs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: picrf_abs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"picrf_ext",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%picrf_ext)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: picrf_ext"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"picrf_nphi_srcs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%picrf_nphi_srcs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: picrf_nphi_srcs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"picrf_srcs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%picrf_srcs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: picrf_srcs"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"picrf_totals",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%picrf_totals)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: picrf_totals"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"picth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%picth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: picth"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pilh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pilh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pilh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pilh_src",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pilh_src)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pilh_src"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pi_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pi_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pi_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pi_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pi_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pi_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pi_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pi_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pi_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"PLASMA_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%PLASMA_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: PLASMA_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pmine",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pmine)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pmine"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pmini",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pmini)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pmini"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pminth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pminth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pminth"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pohme",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pohme)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pohme"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pot_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%pot_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: pot_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"power_ec",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%power_ec)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: power_ec"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"power_ic",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%power_ic)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: power_ic"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"power_lh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%power_lh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: power_lh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"power_nbi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%power_nbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: power_nbi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"power_nbi_trace",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%power_nbi_trace)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: power_nbi_trace"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"prad",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%prad)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: prad"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"prad_br",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%prad_br)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: prad_br"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"prad_cy",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%prad_cy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: prad_cy"
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

END SUBROUTINE ps_update_file_write_5
