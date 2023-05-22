! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_write_def_8(ss,icdf,cclist,nids,iout,iwarn,iertot)


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

  iaddr = ps_shash_t1
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("t1","PLASMA")
     ELSE
        indim = 0
        CALL define_var("t1","R",indim,idims, &
               "time at end of IPS macro timestep", &
               "sec","PLASMA","STATE_DATA", &
               "R  t1")
     ENDIF
  ENDIF

  iaddr = ps_phash_teflux_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%teflux_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("teflux_ky_tglf","ANOM")
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
        CALL define_var("teflux_ky_tglf","R",indim,idims, &
               "electron density flux spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin teflux_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Te_bdy
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Te_bdy","PLASMA")
     ELSE
        indim = 0
        CALL define_var("Te_bdy","R",indim,idims, &
               "Electron temperature at/beyond boundary", &
               "KeV","PLASMA","STATE_DATA", &
               "R Te_bdy")
     ENDIF
  ENDIF

  iaddr = ps_phash_te_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%te_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("te_ky_tglf","ANOM")
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
        CALL define_var("te_ky_tglf","R",indim,idims, &
               "electron density fluctuation spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin te_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_tfinal
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("tfinal","PLASMA")
     ELSE
        indim = 0
        CALL define_var("tfinal","R",indim,idims, &
               "stop time of simulation (may be adjustable)", &
               "sec","PLASMA","SIMULATION_INIT", &
               "R  tfinal")
     ENDIF
  ENDIF

  iaddr = ps_phash_tfrip2_log
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tfrip2_log).AND.ichange) THEN
     IF(cclist(ps_cnum_RIPPLE).NE.1) THEN
        CALL ccwarn("tfrip2_log","RIPPLE")
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
        IF(nids%nid_dim_nz_rip.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nz_rip,"dim_nz_rip",ss%nz_rip)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nz_rip
        CALL define_var("tfrip2_log","R",indim,idims, &
               "2nd component of TF ripple", &
               "-","RIPPLE","STATE_PROFILES", &
               "R|pclin  tfrip2_log(nR_rip,nZ_rip)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tfrip_log
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tfrip_log).AND.ichange) THEN
     IF(cclist(ps_cnum_RIPPLE).NE.1) THEN
        CALL ccwarn("tfrip_log","RIPPLE")
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
        IF(nids%nid_dim_nz_rip.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nz_rip,"dim_nz_rip",ss%nz_rip)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nz_rip
        CALL define_var("tfrip_log","R",indim,idims, &
               "TF ripple: log((Bmax-Bmin)/(Bmax+Bmin))", &
               "-","RIPPLE","STATE_PROFILES", &
               "R|pclin  tfrip_log(nR_rip,nZ_rip)")
     ENDIF
  ENDIF

  iaddr = ps_shash_TF_Data_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("TF_Data_Info","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("TF_Data_Info","C*80",indim,idims, &
               "information on source of toroidal field data", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "C*80   TF_Data_Info")
     ENDIF
  ENDIF

  iaddr = ps_shash_thegrilh
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%thegrilh).AND.ichange) THEN
     IF(cclist(ps_cnum_LH).NE.1) THEN
        CALL ccwarn("thegrilh","LH")
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
        CALL define_var("thegrilh","R",indim,idims, &
               "need description", &
               "-","LH","SHOT_CONFIGURATION", &
               "R|units=- thegrilh(nlhrf_src)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_nth_eq
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%th_eq).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("th_eq","EQ")
     ELSE
        indim = 0
        IF(nids%nid_dim_nth_eq.EQ.0) THEN
           CALL define_dim(nids%nid_dim_nth_eq,"dim_nth_eq",ss%nth_eq)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_nth_eq
        CALL define_var("th_eq","R",indim,idims, &
               "theta grid (EQ)", &
               "rad","EQ","SIMULATION_INIT", &
               "G|CCW   th_eq(nth_eq)")
     ENDIF
  ENDIF

  iaddr = ps_phash_Ti
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Ti).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Ti","PLASMA")
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
        CALL define_var("Ti","R",indim,idims, &
               "ion temperature", &
               "keV","PLASMA","STATE_PROFILES", &
               "R|units=keV|step*ni Ti(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tiflux_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tiflux_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("tiflux_ky_tglf","ANOM")
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
        CALL define_var("tiflux_ky_tglf","R",indim,idims, &
               "ion density flux spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin tiflux_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_tinit
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("tinit","PLASMA")
     ELSE
        indim = 0
        CALL define_var("tinit","R",indim,idims, &
               "start time of simulation", &
               "sec","PLASMA","SIMULATION_INIT", &
               "R  tinit")
     ENDIF
  ENDIF

  iaddr = ps_shash_Ti_bdy
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Ti_bdy","PLASMA")
     ELSE
        indim = 0
        CALL define_var("Ti_bdy","R",indim,idims, &
               "Ion temperature at/beyond boundary", &
               "KeV","PLASMA","STATE_DATA", &
               "R Ti_bdy")
     ENDIF
  ENDIF

  iaddr = ps_phash_ti_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%ti_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("ti_ky_tglf","ANOM")
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
        CALL define_var("ti_ky_tglf","R",indim,idims, &
               "ion density fluctuation spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin ti_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tmodel
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tmodel).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("tmodel","ANOM")
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
        CALL define_var("tmodel","R",indim,idims, &
               "target temperature profile", &
               "keV","ANOM","STATE_PROFILES", &
               "R|units=keV|step*nmodel   tmodel(~nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_tmodel_bdy
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("tmodel_bdy","ANOM")
     ELSE
        indim = 0
        CALL define_var("tmodel_bdy","R",indim,idims, &
               "model temperature, boundary value", &
               "keV","ANOM","STATE_DATA", &
               "R|units=keV    tmodel_bdy")
     ENDIF
  ENDIF

  iaddr = ps_shash_tokamak_id
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("tokamak_id","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        CALL define_var("tokamak_id","N",indim,idims, &
               "Tokamak (Machine Description) ID", &
               "__NONE","PLASMA","MACHINE_DESCRIPTION", &
               "N  tokamak_id")
     ENDIF
  ENDIF

  iaddr = ps_shash_Tornum_balcrit
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Tornum_balcrit).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("Tornum_balcrit","LMHD")
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
        CALL define_var("Tornum_balcrit","I",indim,idims, &
               "critical toroidal mode number for ballooning", &
               "__NONE","LMHD","STATE_DATA", &
               "I   Tornum_balcrit(nballoon)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_mmodes
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Tor_mode_label).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("Tor_mode_label","LMHD")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_mmodes.EQ.0) THEN
           CALL define_dim(nids%nid_dim_mmodes,"dim_mmodes",ss%mmodes)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_mmodes
        CALL define_var("Tor_mode_label","N",indim,idims, &
               "Toroidal modes to monitor", &
               "__NONE","LMHD","SIMULATION_INIT", &
               "L|toroidal_modes Tor_mode_label(mmodes)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Tor_mode_No
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Tor_mode_No).AND.ichange) THEN
     IF(cclist(ps_cnum_LMHD).NE.1) THEN
        CALL ccwarn("Tor_mode_No","LMHD")
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
        CALL define_var("Tor_mode_No","I",indim,idims, &
               "(from input) Toroidal modes monitored", &
               "__NONE","LMHD","SIMULATION_INIT", &
               "I   Tor_mode_No(mmodes)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tq0_reco
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tq0_reco).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("tq0_reco","GAS")
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
        CALL define_var("tq0_reco","R",indim,idims, &
               "torque of recombination neutral source", &
               "Nt*m","GAS","STATE_PROFILES", &
               "R|step*dV   tq0_reco(~nrho_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tqb0_halo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tqb0_halo).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("tqb0_halo","NBI")
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
        CALL define_var("tqb0_halo","R",indim,idims, &
               "torque of halo neutral source", &
               "Nt*m","NBI","STATE_PROFILES", &
               "R|step*dV   tqb0_halo(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tqbe
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tqbe).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("tqbe","NBI")
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
        CALL define_var("tqbe","R",indim,idims, &
               "collisional beam torque to electrons", &
               "Nt*m","NBI","STATE_PROFILES", &
               "R|step*dV   tqbe(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tqbi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tqbi).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("tqbi","NBI")
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
        CALL define_var("tqbi","R",indim,idims, &
               "collisional beam torque to thermal ions", &
               "Nt*m","NBI","STATE_PROFILES", &
               "R|step*dV   tqbi(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tqbjxb
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tqbjxb).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("tqbjxb","NBI")
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
        CALL define_var("tqbjxb","R",indim,idims, &
               "JxB beam torque", &
               "Nt*m","NBI","STATE_PROFILES", &
               "R|step*dV   tqbjxb(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tqbth
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tqbth).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("tqbth","NBI")
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
        CALL define_var("tqbth","R",indim,idims, &
               "momentum in thermalization of beam ions", &
               "Nt*m","NBI","STATE_PROFILES", &
               "R|step*dV   tqbth(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tqcx_halo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tqcx_halo).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("tqcx_halo","NBI")
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
        CALL define_var("tqcx_halo","R",indim,idims, &
               "beam halo driven CX momentum torque", &
               "Nt*m","NBI","STATE_PROFILES", &
               "R|step*dV   tqcx_halo(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tqcx_reco
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tqcx_reco).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("tqcx_reco","GAS")
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
        CALL define_var("tqcx_reco","R",indim,idims, &
               "reco driven CX momentum torque", &
               "Nt*m","GAS","STATE_PROFILES", &
               "R|step*dV   tqcx_reco(~nrho_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tqioniz
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tqioniz).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("tqioniz","GAS")
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
        CALL define_var("tqioniz","R",indim,idims, &
               "ionization torque per unit influx (sc0)", &
               "Nt*m/(#/sec)","GAS","STATE_PROFILES", &
               "R|step*dV|units=Nt*m/(#/sec)     tqioniz(~nrho_gas,ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tqqcx
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tqqcx).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("tqqcx","GAS")
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
        CALL define_var("tqqcx","R",indim,idims, &
               "cx torque per unit influx per (omeg0-omegi)", &
               "Nt*m/(#/sec)/(rad/sec)","GAS","STATE_PROFILES", &
               "R|step*dV|units=Nt*m/(#/sec)/(rad/sec)  tqqcx(~nrho_gas,ngsc0)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tqsc_halo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tqsc_halo).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("tqsc_halo","NBI")
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
        CALL define_var("tqsc_halo","R",indim,idims, &
               "beam halo torque: recapture - tqb0_halo", &
               "Nt*m","NBI","STATE_PROFILES", &
               "R|step*dV   tqsc_halo(~nrho_nbi)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tqsc_reco
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tqsc_reco).AND.ichange) THEN
     IF(cclist(ps_cnum_GAS).NE.1) THEN
        CALL ccwarn("tqsc_reco","GAS")
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
        CALL define_var("tqsc_reco","R",indim,idims, &
               "reco torque: recapture - tq0_reco", &
               "Nt*m","GAS","STATE_PROFILES", &
               "R|step*dV   tqsc_reco(~nrho_gas)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tq_anom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tq_anom).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("tq_anom","ANOM")
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
        CALL define_var("tq_anom","R",indim,idims, &
               "anomalous torque", &
               "Nt*m","ANOM","STATE_PROFILES", &
               "R|units=Nt*m|step*dV  tq_anom(~nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tq_trans
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tq_trans).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("tq_trans","PLASMA")
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
        CALL define_var("tq_trans","R",indim,idims, &
               "angular momentum transport (loss)", &
               "Nt*m","PLASMA","STATE_PROFILES", &
               "R|units=Nt*m|step*dV  tq_trans(~nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tq_transcat
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tq_transcat).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("tq_transcat","PLASMA")
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
        CALL define_var("tq_transcat","R",indim,idims, &
               "toroidal angular momentum transport", &
               "Nt*m","PLASMA","STATE_PROFILES", &
               "R|step*dV|units=Nt*m  tq_transcat(~nrho,ntrcat)")
     ENDIF
  ENDIF

  iaddr = ps_shash_trace_flag
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%trace_flag).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("trace_flag","NBI")
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
        CALL define_var("trace_flag","I",indim,idims, &
               "trace element flag", &
               "__NONE","NBI","SIMULATION_INIT", &
               "I  trace_flag(nbeam)")
     ENDIF
  ENDIF

  iaddr = ps_dhash_ntrcat
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%trcat).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("trcat","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_32
        IF(nids%nid_dim_ntrcat.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ntrcat,"dim_ntrcat",ss%ntrcat)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ntrcat
        CALL define_var("trcat","N",indim,idims, &
               "Application defined list of", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "L|TR_categories  trcat(ntrcat)")
     ENDIF
  ENDIF

  iaddr = ps_phash_triang
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%triang).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("triang","EQ")
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
        CALL define_var("triang","R",indim,idims, &
               "triangularity (symmetrized)", &
               "-","EQ","STATE_PROFILES", &
               "R|pclin  triang(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_triangL
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%triangL).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("triangL","EQ")
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
        CALL define_var("triangL","R",indim,idims, &
               "lower triangularity", &
               "-","EQ","STATE_PROFILES", &
               "R|pclin  triangL(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_triangU
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%triangU).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("triangU","EQ")
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
        CALL define_var("triangU","R",indim,idims, &
               "upper triangularity", &
               "-","EQ","STATE_PROFILES", &
               "R|pclin  triangU(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_triang_miller_L
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%triang_miller_L).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("triang_miller_L","EQ")
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
        CALL define_var("triang_miller_L","R",indim,idims, &
               "Miller lower triangularity", &
               "-","EQ","STATE_PROFILES", &
               "R|pclin  triang_miller_L(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_phash_triang_miller_U
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%triang_miller_U).AND.ichange) THEN
     IF(cclist(ps_cnum_EQ).NE.1) THEN
        CALL ccwarn("triang_miller_U","EQ")
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
        CALL define_var("triang_miller_U","R",indim,idims, &
               "Miller upper triangularity", &
               "-","EQ","STATE_PROFILES", &
               "R|pclin  triang_miller_U(nrho_eq_geo)")
     ENDIF
  ENDIF

  iaddr = ps_shash_trmodels
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%trmodels).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("trmodels","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_256
        IF(nids%nid_dim_ntrcat.EQ.0) THEN
           CALL define_dim(nids%nid_dim_ntrcat,"dim_ntrcat",ss%ntrcat)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_ntrcat
        CALL define_var("trmodels","C*256",indim,idims, &
               "short description or ID of transport models", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "C*256 trmodels(ntrcat)")
     ENDIF
  ENDIF

  iaddr = ps_phash_Ts
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Ts).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Ts","PLASMA")
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
        CALL define_var("Ts","R",indim,idims, &
               "thermal specie temperature", &
               "keV","PLASMA","STATE_PROFILES", &
               "R|units=keV|alias=T|step*ns  Ts(~nrho,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_shash_Ts_Data_Info
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Ts_Data_Info","PLASMA")
     ELSE
        indim = 1
        idims(1)=nids%nid_clen_80
        CALL define_var("Ts_Data_Info","C*80",indim,idims, &
               "information on source of Temperature data", &
               "__NONE","PLASMA","SIMULATION_INIT", &
               "C*80   Ts_Data_Info")
     ENDIF
  ENDIF

  iaddr = ps_shash_Ts_is_input
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%Ts_is_input).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("Ts_is_input","PLASMA")
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
        CALL define_var("Ts_is_input","I",indim,idims, &
               "specie temperature measurement flag", &
               "__NONE","PLASMA","STATE_DATA", &
               "I Ts_is_input(0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tzflux_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tzflux_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("tzflux_ky_tglf","ANOM")
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
        CALL define_var("tzflux_ky_tglf","R",indim,idims, &
               "impurity density flux spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin tzflux_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_tz_ky_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%tz_ky_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("tz_ky_tglf","ANOM")
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
        CALL define_var("tz_ky_tglf","R",indim,idims, &
               "impurity density fluctuation spectrum", &
               "GB","ANOM","STATE_PROFILES", &
               "R|units=GB|pclin tz_ky_tglf(nrho_anomth,nky_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_shash_T_einj_standard
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%T_einj_standard).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("T_einj_standard","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dim_n_t_einj_standard.EQ.0) THEN
           CALL define_dim(nids%nid_dim_n_t_einj_standard,"dim_n_t_einj_standard",ss%n_t_einj_standard)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_n_t_einj_standard
        CALL define_var("T_einj_standard","R",indim,idims, &
               "T beam table energies", &
               "keV","NBI","MACHINE_DESCRIPTION", &
               "R|units=keV T_einj_standard(n_T_einj_standard)")
     ENDIF
  ENDIF

  iaddr = ps_shash_T_ffull_standard
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%T_ffull_standard).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("T_ffull_standard","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dim_n_t_einj_standard.EQ.0) THEN
           CALL define_dim(nids%nid_dim_n_t_einj_standard,"dim_n_t_einj_standard",ss%n_t_einj_standard)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_n_t_einj_standard
        CALL define_var("T_ffull_standard","R",indim,idims, &
               "T beam full energy fractions", &
               "-","NBI","MACHINE_DESCRIPTION", &
               "R|units=- T_ffull_standard(n_T_einj_standard)")
     ENDIF
  ENDIF

  iaddr = ps_shash_T_fhalf_standard
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%T_fhalf_standard).AND.ichange) THEN
     IF(cclist(ps_cnum_NBI).NE.1) THEN
        CALL ccwarn("T_fhalf_standard","NBI")
     ELSE
        indim = 0
        IF(nids%nid_dim_n_t_einj_standard.EQ.0) THEN
           CALL define_dim(nids%nid_dim_n_t_einj_standard,"dim_n_t_einj_standard",ss%n_t_einj_standard)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dim_n_t_einj_standard
        CALL define_var("T_fhalf_standard","R",indim,idims, &
               "T beam half energy fractions", &
               "-","NBI","MACHINE_DESCRIPTION", &
               "R|units=- T_fhalf_standard(n_T_einj_standard)")
     ENDIF
  ENDIF

  iaddr = ps_phash_upwind_pfrac_ns
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%upwind_pfrac_ns).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("upwind_pfrac_ns","PLASMA")
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
        CALL define_var("upwind_pfrac_ns","R",indim,idims, &
               "specie density forward upwind fractions", &
               "-","PLASMA","STATE_PROFILES", &
               "R|units=-|pclin upwind_pfrac_ns(nrho,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_upwind_pfrac_omega
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%upwind_pfrac_omega).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("upwind_pfrac_omega","PLASMA")
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
        CALL define_var("upwind_pfrac_omega","R",indim,idims, &
               "omega (ang. velocity) forward upwind fraction", &
               "-","PLASMA","STATE_PROFILES", &
               "R|pclin upwind_pfrac_omega(nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_upwind_pfrac_Te
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%upwind_pfrac_Te).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("upwind_pfrac_Te","PLASMA")
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
        CALL define_var("upwind_pfrac_Te","R",indim,idims, &
               "Te (electron energy advection) forward upwind fraction", &
               "-","PLASMA","STATE_PROFILES", &
               "R|pclin upwind_pfrac_Te(nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_upwind_pfrac_Ti
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%upwind_pfrac_Ti).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("upwind_pfrac_Ti","PLASMA")
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
        CALL define_var("upwind_pfrac_Ti","R",indim,idims, &
               "Ti (ion energy advection) forward upwind fraction", &
               "-","PLASMA","STATE_PROFILES", &
               "R|pclin upwind_pfrac_Ti(nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_vee_trans
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vee_trans).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vee_trans","PLASMA")
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
        CALL define_var("vee_trans","R",indim,idims, &
               "electron energy radial velocity", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|pclin   vee_trans(nrho)")
     ENDIF
  ENDIF

  iaddr = ps_phash_vee_transcat
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%vee_transcat).AND.ichange) THEN
     IF(cclist(ps_cnum_PLASMA).NE.1) THEN
        CALL ccwarn("vee_transcat","PLASMA")
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
        CALL define_var("vee_transcat","R",indim,idims, &
               "Electron energy radial velocity", &
               "m/sec","PLASMA","STATE_PROFILES", &
               "R|units=m/sec|pclin vee_transcat(nrho,ntrcat)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velb_fusi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velb_fusi).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velb_fusi","ANOM")
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
        CALL define_var("velb_fusi","R",indim,idims, &
               "fusion ion anom. radial velocity", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|pclin  velb_fusi(nrho_anom)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velb_nbi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velb_nbi).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velb_nbi","ANOM")
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
        CALL define_var("velb_nbi","R",indim,idims, &
               "beam ion anom. radial velocity", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|pclin  velb_nbi(nrho_anom)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velb_rfmi
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velb_rfmi).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velb_rfmi","ANOM")
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
        CALL define_var("velb_rfmi","R",indim,idims, &
               "RF minority ion anom. radial velocity", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|pclin  velb_rfmi(nrho_anom)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velne_nclass
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velne_nclass).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velne_nclass","ANOM")
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
        CALL define_var("velne_nclass","R",indim,idims, &
               "electron-density-equations, advective velocity term from NCLASS model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin velne_nclass(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velnh_nclass
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velnh_nclass).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velnh_nclass","ANOM")
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
        CALL define_var("velnh_nclass","R",indim,idims, &
               "main-hydrogenic-density-equations, advective velocity term from NCLASS model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin velnh_nclass(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velni_drbm
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velni_drbm).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velni_drbm","ANOM")
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
        CALL define_var("velni_drbm","R",indim,idims, &
               "ion convective velocity", &
               "m/s","ANOM","STATE_PROFILES", &
               "R|units=m/s|pclin velni_drbm(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velns_anom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velns_anom).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velns_anom","ANOM")
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
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        CALL define_var("velns_anom","R",indim,idims, &
               "density-equations, advective velocity term", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velns_anom(nrho_anomth,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velns_glf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velns_glf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velns_glf","ANOM")
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
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        CALL define_var("velns_glf","R",indim,idims, &
               "density-equations, advective velocity term from GLF23 model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velns_glf(nrho_anomth,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velns_gtcneo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velns_gtcneo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velns_gtcneo","ANOM")
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
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        CALL define_var("velns_gtcneo","R",indim,idims, &
               "density-equations, advective velocity term from GTC_NEO model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velns_gtcneo(nrho_anomth,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velns_mmm71
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velns_mmm71).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velns_mmm71","ANOM")
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
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        CALL define_var("velns_mmm71","R",indim,idims, &
               "density-equations, advective velocity term from MMM71 model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velns_mmm71(nrho_anomth,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velns_nclass
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velns_nclass).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velns_nclass","ANOM")
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
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        CALL define_var("velns_nclass","R",indim,idims, &
               "density-equations, advective velocity term from NCLASS model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin velns_nclass(nrho_anomth,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velns_neo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velns_neo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velns_neo","ANOM")
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
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        CALL define_var("velns_neo","R",indim,idims, &
               "density-equations, advective velocity term from NEO model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velns_neo(nrho_anomth,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velns_tglf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velns_tglf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velns_tglf","ANOM")
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
        IF(nids%nid_dp1_nspec_th.EQ.0) THEN
           CALL define_dim(nids%nid_dp1_nspec_th,"dp1_nspec_th",ss%nspec_th+1)
           IF(ierr.NE.0) THEN
               iertot=iertot+1
               RETURN
           ENDIF
        ENDIF
        indim = indim + 1
        idims(indim) = nids%nid_dp1_nspec_th
        CALL define_var("velns_tglf","R",indim,idims, &
               "density-equations, advective velocity term from TGLF model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin velns_tglf(nrho_anomth,0:nspec_th)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velnx_nclass
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velnx_nclass).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velnx_nclass","ANOM")
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
        CALL define_var("velnx_nclass","R",indim,idims, &
               "main-impurity-density-equations, advective velocity term from NCLASS model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin velnx_nclass(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velpphi_anom
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velpphi_anom).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velpphi_anom","ANOM")
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
        CALL define_var("velpphi_anom","R",indim,idims, &
               "momentum-equation, advective velocity term", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velpphi_anom(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velpphi_glf
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velpphi_glf).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velpphi_glf","ANOM")
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
        CALL define_var("velpphi_glf","R",indim,idims, &
               "momentum-equation, advective velocity term from GLF23 model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velpphi_glf(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velpphi_gtcneo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velpphi_gtcneo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velpphi_gtcneo","ANOM")
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
        CALL define_var("velpphi_gtcneo","R",indim,idims, &
               "momentum-equation, advective velocity term from GTC_NEO model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velpphi_gtcneo(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velpphi_mmm71
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velpphi_mmm71).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velpphi_mmm71","ANOM")
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
        CALL define_var("velpphi_mmm71","R",indim,idims, &
               "momentum-equation, advective velocity term from MMM71 model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velpphi_mmm71(nrho_anomth)")
     ENDIF
  ENDIF

  iaddr = ps_phash_velpphi_neo
  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)
  IF(ALLOCATED(ss%velpphi_neo).AND.ichange) THEN
     IF(cclist(ps_cnum_ANOM).NE.1) THEN
        CALL ccwarn("velpphi_neo","ANOM")
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
        CALL define_var("velpphi_neo","R",indim,idims, &
               "momentum-equation, advective velocity term from NEO model", &
               "m/sec","ANOM","STATE_PROFILES", &
               "R|units=m/sec|pclin  velpphi_neo(nrho_anomth)")
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

END SUBROUTINE ps_update_file_write_def_8

SUBROUTINE ps_update_file_write_8(ss,icdf,cclist,iucount,iout,iwarn,iertot)


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


  status = nf90_inq_varid(icdf,"t1",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%t1)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: t1"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"teflux_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%teflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: teflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Te_bdy",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Te_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Te_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"te_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%te_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: te_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tfinal",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tfinal)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tfinal"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tfrip2_log",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tfrip2_log)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tfrip2_log"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tfrip_log",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tfrip_log)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tfrip_log"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"TF_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%TF_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: TF_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"thegrilh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%thegrilh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: thegrilh"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"th_eq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%th_eq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: th_eq"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Ti",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Ti)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Ti"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tiflux_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tiflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tiflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tinit",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tinit)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tinit"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Ti_bdy",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Ti_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Ti_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ti_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%ti_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ti_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tmodel",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tmodel)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tmodel"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tmodel_bdy",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tmodel_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tmodel_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tokamak_id",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tokamak_id)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tokamak_id"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Tornum_balcrit",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Tornum_balcrit)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Tornum_balcrit"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Tor_mode_label",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Tor_mode_label)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Tor_mode_label"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Tor_mode_No",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Tor_mode_No)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Tor_mode_No"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tq0_reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tq0_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tq0_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tqb0_halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tqb0_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tqb0_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tqbe",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tqbe)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tqbe"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tqbi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tqbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tqbi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tqbjxb",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tqbjxb)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tqbjxb"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tqbth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tqbth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tqbth"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tqcx_halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tqcx_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tqcx_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tqcx_reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tqcx_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tqcx_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tqioniz",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tqioniz)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tqioniz"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tqqcx",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tqqcx)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tqqcx"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tqsc_halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tqsc_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tqsc_halo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tqsc_reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tqsc_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tqsc_reco"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tq_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tq_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tq_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tq_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tq_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tq_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tq_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tq_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tq_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"trace_flag",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%trace_flag)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: trace_flag"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"trcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%trcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: trcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"triang",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%triang)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: triang"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"triangL",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%triangL)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: triangL"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"triangU",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%triangU)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: triangU"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"triang_miller_L",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%triang_miller_L)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: triang_miller_L"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"triang_miller_U",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%triang_miller_U)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: triang_miller_U"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"trmodels",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%trmodels)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: trmodels"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Ts",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Ts)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Ts"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Ts_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Ts_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Ts_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Ts_is_input",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%Ts_is_input)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: Ts_is_input"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tzflux_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tzflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tzflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tz_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%tz_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: tz_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"T_einj_standard",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%T_einj_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: T_einj_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"T_ffull_standard",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%T_ffull_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: T_ffull_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"T_fhalf_standard",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%T_fhalf_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: T_fhalf_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"upwind_pfrac_ns",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%upwind_pfrac_ns)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: upwind_pfrac_ns"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"upwind_pfrac_omega",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%upwind_pfrac_omega)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: upwind_pfrac_omega"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"upwind_pfrac_Te",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%upwind_pfrac_Te)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: upwind_pfrac_Te"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"upwind_pfrac_Ti",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%upwind_pfrac_Ti)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: upwind_pfrac_Ti"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vee_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vee_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vee_trans"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vee_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%vee_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: vee_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velb_fusi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velb_fusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velb_fusi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velb_nbi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velb_nbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velb_nbi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velb_rfmi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velb_rfmi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velb_rfmi"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velne_nclass",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velne_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velne_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velnh_nclass",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velnh_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velnh_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velni_drbm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velni_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velni_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velns_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velns_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velns_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velns_glf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velns_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velns_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velns_gtcneo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velns_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velns_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velns_mmm71",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velns_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velns_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velns_nclass",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velns_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velns_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velns_neo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velns_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velns_neo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velns_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velns_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velns_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velnx_nclass",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velnx_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velnx_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velpphi_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velpphi_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velpphi_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velpphi_glf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velpphi_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velpphi_glf"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velpphi_gtcneo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velpphi_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velpphi_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velpphi_mmm71",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velpphi_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velpphi_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velpphi_neo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_put_var(icdf,varid,ss%velpphi_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: velpphi_neo"
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

END SUBROUTINE ps_update_file_write_8
