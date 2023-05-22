! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_get_1(ss,icdf,version_change,iout,iertot)


  !  read subset of variables and arrays from NetCDF file

  USE plasma_state_definition_mod
  USE netcdf
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to read
  INTEGER, INTENT(IN) :: icdf   ! NetCDF file ID
  LOGICAL, INTENT(IN) :: version_change  ! flag file/software version mismatch
  INTEGER, INTENT(IN) :: iout   ! message LUN
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !--------------

  INTEGER :: varid,dimid,status,ilen,jlen,idims(20),indim
  INTEGER :: ii,iflag_alloc2,iflag_loc2,isize
  CHARACTER*80 :: nc_errmsg

  !--------------


  !-----------------------------------------------
  ! NetCDF reads...

  IF(ALLOCATED(ss%chii_ntm)) THEN
     status = nf90_inq_varid(icdf,"chii_ntm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chii_ntm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chii_ntm)
        IF(status.NE.NF90_NOERR) THEN
           ss%chii_ntm = 0.0_rspec
           CALL ncerr_name(2,status,"chii_ntm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_tglf)) THEN
     status = nf90_inq_varid(icdf,"chii_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chii_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chii_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%chii_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"chii_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_trans)) THEN
     status = nf90_inq_varid(icdf,"chii_trans",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chii_trans")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chii_trans)
        IF(status.NE.NF90_NOERR) THEN
           ss%chii_trans = 0.0_rspec
           CALL ncerr_name(2,status,"chii_trans")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_transcat)) THEN
     status = nf90_inq_varid(icdf,"chii_transcat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chii_transcat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chii_transcat)
        IF(status.NE.NF90_NOERR) THEN
           ss%chii_transcat = 0.0_rspec
           CALL ncerr_name(2,status,"chii_transcat")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_w19)) THEN
     status = nf90_inq_varid(icdf,"chii_w19",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chii_w19")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chii_w19)
        IF(status.NE.NF90_NOERR) THEN
           ss%chii_w19 = 0.0_rspec
           CALL ncerr_name(2,status,"chii_w19")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chimo_trans)) THEN
     status = nf90_inq_varid(icdf,"chimo_trans",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chimo_trans")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chimo_trans)
        IF(status.NE.NF90_NOERR) THEN
           ss%chimo_trans = 0.0_rspec
           CALL ncerr_name(2,status,"chimo_trans")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chimo_transcat)) THEN
     status = nf90_inq_varid(icdf,"chimo_transcat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chimo_transcat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chimo_transcat)
        IF(status.NE.NF90_NOERR) THEN
           ss%chimo_transcat = 0.0_rspec
           CALL ncerr_name(2,status,"chimo_transcat")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_anom)) THEN
     status = nf90_inq_varid(icdf,"chiphi_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chiphi_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chiphi_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%chiphi_anom = 0.0_rspec
           CALL ncerr_name(2,status,"chiphi_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_drbm)) THEN
     status = nf90_inq_varid(icdf,"chiphi_drbm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chiphi_drbm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chiphi_drbm)
        IF(status.NE.NF90_NOERR) THEN
           ss%chiphi_drbm = 0.0_rspec
           CALL ncerr_name(2,status,"chiphi_drbm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_glf)) THEN
     status = nf90_inq_varid(icdf,"chiphi_glf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chiphi_glf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chiphi_glf)
        IF(status.NE.NF90_NOERR) THEN
           ss%chiphi_glf = 0.0_rspec
           CALL ncerr_name(2,status,"chiphi_glf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"chiphi_gtcneo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chiphi_gtcneo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chiphi_gtcneo)
        IF(status.NE.NF90_NOERR) THEN
           ss%chiphi_gtcneo = 0.0_rspec
           CALL ncerr_name(2,status,"chiphi_gtcneo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_mmm71)) THEN
     status = nf90_inq_varid(icdf,"chiphi_mmm71",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chiphi_mmm71")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chiphi_mmm71)
        IF(status.NE.NF90_NOERR) THEN
           ss%chiphi_mmm71 = 0.0_rspec
           CALL ncerr_name(2,status,"chiphi_mmm71")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_neo)) THEN
     status = nf90_inq_varid(icdf,"chiphi_neo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chiphi_neo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chiphi_neo)
        IF(status.NE.NF90_NOERR) THEN
           ss%chiphi_neo = 0.0_rspec
           CALL ncerr_name(2,status,"chiphi_neo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_tglf)) THEN
     status = nf90_inq_varid(icdf,"chiphi_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chiphi_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chiphi_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%chiphi_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"chiphi_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chiphi_w19)) THEN
     status = nf90_inq_varid(icdf,"chiphi_w19",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chiphi_w19")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chiphi_w19)
        IF(status.NE.NF90_NOERR) THEN
           ss%chiphi_w19 = 0.0_rspec
           CALL ncerr_name(2,status,"chiphi_w19")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chitheta_mmm71)) THEN
     status = nf90_inq_varid(icdf,"chitheta_mmm71",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chitheta_mmm71")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chitheta_mmm71)
        IF(status.NE.NF90_NOERR) THEN
           ss%chitheta_mmm71 = 0.0_rspec
           CALL ncerr_name(2,status,"chitheta_mmm71")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%circuit_name)) THEN
     status = nf90_inq_varid(icdf,"circuit_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"circuit_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%circuit_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%circuit_name = " "
           CALL ncerr_name(2,status,"circuit_name")
        ELSE
           CALL denull_vec(ss%circuit_name,ss%ncircuits)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%coil_apt)) THEN
     status = nf90_inq_varid(icdf,"coil_apt",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"coil_apt")
     ELSE
        status = nf90_get_var(icdf,varid,ss%coil_apt)
        IF(status.NE.NF90_NOERR) THEN
           ss%coil_apt = 0.0_rspec
           CALL ncerr_name(2,status,"coil_apt")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%coil_in_circuit)) THEN
     status = nf90_inq_varid(icdf,"coil_in_circuit",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"coil_in_circuit")
     ELSE
        status = nf90_get_var(icdf,varid,ss%coil_in_circuit)
        IF(status.NE.NF90_NOERR) THEN
           ss%coil_in_circuit = " "
           CALL ncerr_name(2,status,"coil_in_circuit")
        ELSE
           CALL denull_vec(ss%coil_in_circuit,ss%ncoils)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%coil_name)) THEN
     status = nf90_inq_varid(icdf,"coil_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"coil_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%coil_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%coil_name = " "
           CALL ncerr_name(2,status,"coil_name")
        ELSE
           CALL denull_vec(ss%coil_name,ss%ncoils)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%coil_resispt)) THEN
     status = nf90_inq_varid(icdf,"coil_resispt",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"coil_resispt")
     ELSE
        status = nf90_get_var(icdf,varid,ss%coil_resispt)
        IF(status.NE.NF90_NOERR) THEN
           ss%coil_resispt = 0.0_rspec
           CALL ncerr_name(2,status,"coil_resispt")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curbeam)) THEN
     status = nf90_inq_varid(icdf,"curbeam",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"curbeam")
     ELSE
        status = nf90_get_var(icdf,varid,ss%curbeam)
        IF(status.NE.NF90_NOERR) THEN
           ss%curbeam = 0.0_rspec
           CALL ncerr_name(2,status,"curbeam")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curech)) THEN
     status = nf90_inq_varid(icdf,"curech",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"curech")
     ELSE
        status = nf90_get_var(icdf,varid,ss%curech)
        IF(status.NE.NF90_NOERR) THEN
           ss%curech = 0.0_rspec
           CALL ncerr_name(2,status,"curech")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curech_src)) THEN
     status = nf90_inq_varid(icdf,"curech_src",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"curech_src")
     ELSE
        status = nf90_get_var(icdf,varid,ss%curech_src)
        IF(status.NE.NF90_NOERR) THEN
           ss%curech_src = 0.0_rspec
           CALL ncerr_name(2,status,"curech_src")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curfusn)) THEN
     status = nf90_inq_varid(icdf,"curfusn",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"curfusn")
     ELSE
        status = nf90_get_var(icdf,varid,ss%curfusn)
        IF(status.NE.NF90_NOERR) THEN
           ss%curfusn = 0.0_rspec
           CALL ncerr_name(2,status,"curfusn")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curich)) THEN
     status = nf90_inq_varid(icdf,"curich",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"curich")
     ELSE
        status = nf90_get_var(icdf,varid,ss%curich)
        IF(status.NE.NF90_NOERR) THEN
           ss%curich = 0.0_rspec
           CALL ncerr_name(2,status,"curich")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curlh)) THEN
     status = nf90_inq_varid(icdf,"curlh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"curlh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%curlh)
        IF(status.NE.NF90_NOERR) THEN
           ss%curlh = 0.0_rspec
           CALL ncerr_name(2,status,"curlh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curlh_src)) THEN
     status = nf90_inq_varid(icdf,"curlh_src",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"curlh_src")
     ELSE
        status = nf90_get_var(icdf,varid,ss%curlh_src)
        IF(status.NE.NF90_NOERR) THEN
           ss%curlh_src = 0.0_rspec
           CALL ncerr_name(2,status,"curlh_src")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curmino)) THEN
     status = nf90_inq_varid(icdf,"curmino",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"curmino")
     ELSE
        status = nf90_get_var(icdf,varid,ss%curmino)
        IF(status.NE.NF90_NOERR) THEN
           ss%curmino = 0.0_rspec
           CALL ncerr_name(2,status,"curmino")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curr_bootstrap)) THEN
     status = nf90_inq_varid(icdf,"curr_bootstrap",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"curr_bootstrap")
     ELSE
        status = nf90_get_var(icdf,varid,ss%curr_bootstrap)
        IF(status.NE.NF90_NOERR) THEN
           ss%curr_bootstrap = 0.0_rspec
           CALL ncerr_name(2,status,"curr_bootstrap")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curr_ohmic)) THEN
     status = nf90_inq_varid(icdf,"curr_ohmic",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"curr_ohmic")
     ELSE
        status = nf90_get_var(icdf,varid,ss%curr_ohmic)
        IF(status.NE.NF90_NOERR) THEN
           ss%curr_ohmic = 0.0_rspec
           CALL ncerr_name(2,status,"curr_ohmic")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%curt)) THEN
     status = nf90_inq_varid(icdf,"curt",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"curt")
     ELSE
        status = nf90_get_var(icdf,varid,ss%curt)
        IF(status.NE.NF90_NOERR) THEN
           ss%curt = 0.0_rspec
           CALL ncerr_name(2,status,"curt")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"CUR_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"CUR_Data_Info")
  ELSE
     ilen = len(ss%CUR_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%CUR_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%CUR_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%CUR_Data_Info = " "
        CALL ncerr_name(2,status,"CUR_Data_Info")
     ELSE
        CALL denull(ss%CUR_Data_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%cur_rw)) THEN
     status = nf90_inq_varid(icdf,"cur_rw",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"cur_rw")
     ELSE
        status = nf90_get_var(icdf,varid,ss%cur_rw)
        IF(status.NE.NF90_NOERR) THEN
           ss%cur_rw = 0.0_rspec
           CALL ncerr_name(2,status,"cur_rw")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%d2R_geo_dRHOdTH)) THEN
     status = nf90_inq_varid(icdf,"d2R_geo_dRHOdTH",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"d2R_geo_dRHOdTH")
     ELSE
        status = nf90_get_var(icdf,varid,ss%d2R_geo_dRHOdTH)
        IF(status.NE.NF90_NOERR) THEN
           ss%d2R_geo_dRHOdTH = 0.0_rspec
           CALL ncerr_name(2,status,"d2R_geo_dRHOdTH")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%d2Z_geo_dRHOdTH)) THEN
     status = nf90_inq_varid(icdf,"d2Z_geo_dRHOdTH",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"d2Z_geo_dRHOdTH")
     ELSE
        status = nf90_get_var(icdf,varid,ss%d2Z_geo_dRHOdTH)
        IF(status.NE.NF90_NOERR) THEN
           ss%d2Z_geo_dRHOdTH = 0.0_rspec
           CALL ncerr_name(2,status,"d2Z_geo_dRHOdTH")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%db_mtm)) THEN
     status = nf90_inq_varid(icdf,"db_mtm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"db_mtm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%db_mtm)
        IF(status.NE.NF90_NOERR) THEN
           ss%db_mtm = 0.0_rspec
           CALL ncerr_name(2,status,"db_mtm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dcur_rw_dVloop)) THEN
     status = nf90_inq_varid(icdf,"dcur_rw_dVloop",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dcur_rw_dVloop")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dcur_rw_dVloop)
        IF(status.NE.NF90_NOERR) THEN
           ss%dcur_rw_dVloop = 0.0_rspec
           CALL ncerr_name(2,status,"dcur_rw_dVloop")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%DeltaW_code)) THEN
     status = nf90_inq_varid(icdf,"DeltaW_code",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"DeltaW_code")
     ELSE
        status = nf90_get_var(icdf,varid,ss%DeltaW_code)
        IF(status.NE.NF90_NOERR) THEN
           ss%DeltaW_code = " "
           CALL ncerr_name(2,status,"DeltaW_code")
        ELSE
           CALL denull_vec(ss%DeltaW_code,ss%ndelta_w)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%DeltaW_mapper)) THEN
     status = nf90_inq_varid(icdf,"DeltaW_mapper",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"DeltaW_mapper")
     ELSE
        status = nf90_get_var(icdf,varid,ss%DeltaW_mapper)
        IF(status.NE.NF90_NOERR) THEN
           ss%DeltaW_mapper = " "
           CALL ncerr_name(2,status,"DeltaW_mapper")
        ELSE
           CALL denull_vec(ss%DeltaW_mapper,ss%ndelta_w)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%DeltaW_stable)) THEN
     status = nf90_inq_varid(icdf,"DeltaW_stable",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"DeltaW_stable")
     ELSE
        status = nf90_get_var(icdf,varid,ss%DeltaW_stable)
        IF(status.NE.NF90_NOERR) THEN
           ss%DeltaW_stable = 0
           CALL ncerr_name(2,status,"DeltaW_stable")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%DeltaW_status)) THEN
     status = nf90_inq_varid(icdf,"DeltaW_status",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"DeltaW_status")
     ELSE
        status = nf90_get_var(icdf,varid,ss%DeltaW_status)
        IF(status.NE.NF90_NOERR) THEN
           ss%DeltaW_status = 0
           CALL ncerr_name(2,status,"DeltaW_status")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difb_fusi)) THEN
     status = nf90_inq_varid(icdf,"difb_fusi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difb_fusi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difb_fusi)
        IF(status.NE.NF90_NOERR) THEN
           ss%difb_fusi = 0.0_rspec
           CALL ncerr_name(2,status,"difb_fusi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difb_nbi)) THEN
     status = nf90_inq_varid(icdf,"difb_nbi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difb_nbi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difb_nbi)
        IF(status.NE.NF90_NOERR) THEN
           ss%difb_nbi = 0.0_rspec
           CALL ncerr_name(2,status,"difb_nbi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difb_rfmi)) THEN
     status = nf90_inq_varid(icdf,"difb_rfmi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difb_rfmi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difb_rfmi)
        IF(status.NE.NF90_NOERR) THEN
           ss%difb_rfmi = 0.0_rspec
           CALL ncerr_name(2,status,"difb_rfmi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%diff_trans)) THEN
     status = nf90_inq_varid(icdf,"diff_trans",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"diff_trans")
     ELSE
        status = nf90_get_var(icdf,varid,ss%diff_trans)
        IF(status.NE.NF90_NOERR) THEN
           ss%diff_trans = 0.0_rspec
           CALL ncerr_name(2,status,"diff_trans")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%diff_transcat)) THEN
     status = nf90_inq_varid(icdf,"diff_transcat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"diff_transcat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%diff_transcat)
        IF(status.NE.NF90_NOERR) THEN
           ss%diff_transcat = 0.0_rspec
           CALL ncerr_name(2,status,"diff_transcat")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difne_nclass)) THEN
     status = nf90_inq_varid(icdf,"difne_nclass",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difne_nclass")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difne_nclass)
        IF(status.NE.NF90_NOERR) THEN
           ss%difne_nclass = 0.0_rspec
           CALL ncerr_name(2,status,"difne_nclass")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difnh_nclass)) THEN
     status = nf90_inq_varid(icdf,"difnh_nclass",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difnh_nclass")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difnh_nclass)
        IF(status.NE.NF90_NOERR) THEN
           ss%difnh_nclass = 0.0_rspec
           CALL ncerr_name(2,status,"difnh_nclass")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_anom)) THEN
     status = nf90_inq_varid(icdf,"difns_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difns_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difns_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%difns_anom = 0.0_rspec
           CALL ncerr_name(2,status,"difns_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_drbm)) THEN
     status = nf90_inq_varid(icdf,"difns_drbm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difns_drbm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difns_drbm)
        IF(status.NE.NF90_NOERR) THEN
           ss%difns_drbm = 0.0_rspec
           CALL ncerr_name(2,status,"difns_drbm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_glf)) THEN
     status = nf90_inq_varid(icdf,"difns_glf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difns_glf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difns_glf)
        IF(status.NE.NF90_NOERR) THEN
           ss%difns_glf = 0.0_rspec
           CALL ncerr_name(2,status,"difns_glf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"difns_gtcneo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difns_gtcneo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difns_gtcneo)
        IF(status.NE.NF90_NOERR) THEN
           ss%difns_gtcneo = 0.0_rspec
           CALL ncerr_name(2,status,"difns_gtcneo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_mmm71)) THEN
     status = nf90_inq_varid(icdf,"difns_mmm71",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difns_mmm71")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difns_mmm71)
        IF(status.NE.NF90_NOERR) THEN
           ss%difns_mmm71 = 0.0_rspec
           CALL ncerr_name(2,status,"difns_mmm71")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_nclass)) THEN
     status = nf90_inq_varid(icdf,"difns_nclass",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difns_nclass")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difns_nclass)
        IF(status.NE.NF90_NOERR) THEN
           ss%difns_nclass = 0.0_rspec
           CALL ncerr_name(2,status,"difns_nclass")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_neo)) THEN
     status = nf90_inq_varid(icdf,"difns_neo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difns_neo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difns_neo)
        IF(status.NE.NF90_NOERR) THEN
           ss%difns_neo = 0.0_rspec
           CALL ncerr_name(2,status,"difns_neo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_tglf)) THEN
     status = nf90_inq_varid(icdf,"difns_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difns_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difns_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%difns_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"difns_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difns_w19)) THEN
     status = nf90_inq_varid(icdf,"difns_w19",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difns_w19")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difns_w19)
        IF(status.NE.NF90_NOERR) THEN
           ss%difns_w19 = 0.0_rspec
           CALL ncerr_name(2,status,"difns_w19")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difnx_nclass)) THEN
     status = nf90_inq_varid(icdf,"difnx_nclass",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difnx_nclass")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difnx_nclass)
        IF(status.NE.NF90_NOERR) THEN
           ss%difnx_nclass = 0.0_rspec
           CALL ncerr_name(2,status,"difnx_nclass")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%difnz_mmm71)) THEN
     status = nf90_inq_varid(icdf,"difnz_mmm71",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"difnz_mmm71")
     ELSE
        status = nf90_get_var(icdf,varid,ss%difnz_mmm71)
        IF(status.NE.NF90_NOERR) THEN
           ss%difnz_mmm71 = 0.0_rspec
           CALL ncerr_name(2,status,"difnz_mmm71")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dist_fun)) THEN
     status = nf90_inq_varid(icdf,"dist_fun",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dist_fun")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dist_fun)
        IF(status.NE.NF90_NOERR) THEN
           ss%dist_fun = " "
           CALL ncerr_name(2,status,"dist_fun")
        ELSE
           CALL denull_vec(ss%dist_fun,ss%nspec_alla+1)
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"dn0out",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"dn0out")
  ELSE
     status = nf90_get_var(icdf,varid,ss%dn0out)
     IF(status.NE.NF90_NOERR) THEN
        ss%dn0out = 0.0_rspec
        CALL ncerr_name(2,status,"dn0out")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dQedGrdte_tglf)) THEN
     status = nf90_inq_varid(icdf,"dQedGrdte_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dQedGrdte_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dQedGrdte_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%dQedGrdte_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"dQedGrdte_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dQedGrdti_tglf)) THEN
     status = nf90_inq_varid(icdf,"dQedGrdti_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dQedGrdti_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dQedGrdti_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%dQedGrdti_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"dQedGrdti_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dQidGrdte_tglf)) THEN
     status = nf90_inq_varid(icdf,"dQidGrdte_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dQidGrdte_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dQidGrdte_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%dQidGrdte_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"dQidGrdte_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dQidGrdti_tglf)) THEN
     status = nf90_inq_varid(icdf,"dQidGrdti_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dQidGrdti_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dQidGrdti_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%dQidGrdti_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"dQidGrdti_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dR0_momeq_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dR0_momeq_dRHO",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dR0_momeq_dRHO")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dR0_momeq_dRHO)
        IF(status.NE.NF90_NOERR) THEN
           ss%dR0_momeq_dRHO = 0.0_rspec
           CALL ncerr_name(2,status,"dR0_momeq_dRHO")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dR_geo_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dR_geo_dRHO",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dR_geo_dRHO")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dR_geo_dRHO)
        IF(status.NE.NF90_NOERR) THEN
           ss%dR_geo_dRHO = 0.0_rspec
           CALL ncerr_name(2,status,"dR_geo_dRHO")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dR_geo_dTH)) THEN
     status = nf90_inq_varid(icdf,"dR_geo_dTH",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dR_geo_dTH")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dR_geo_dTH)
        IF(status.NE.NF90_NOERR) THEN
           ss%dR_geo_dTH = 0.0_rspec
           CALL ncerr_name(2,status,"dR_geo_dTH")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dxRjcos_momeq_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dxRjcos_momeq_dRHO",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dxRjcos_momeq_dRHO")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dxRjcos_momeq_dRHO)
        IF(status.NE.NF90_NOERR) THEN
           ss%dxRjcos_momeq_dRHO = 0.0_rspec
           CALL ncerr_name(2,status,"dxRjcos_momeq_dRHO")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dxRjsin_momeq_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dxRjsin_momeq_dRHO",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dxRjsin_momeq_dRHO")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dxRjsin_momeq_dRHO)
        IF(status.NE.NF90_NOERR) THEN
           ss%dxRjsin_momeq_dRHO = 0.0_rspec
           CALL ncerr_name(2,status,"dxRjsin_momeq_dRHO")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dxZjcos_momeq_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dxZjcos_momeq_dRHO",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dxZjcos_momeq_dRHO")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dxZjcos_momeq_dRHO)
        IF(status.NE.NF90_NOERR) THEN
           ss%dxZjcos_momeq_dRHO = 0.0_rspec
           CALL ncerr_name(2,status,"dxZjcos_momeq_dRHO")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dxZjsin_momeq_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dxZjsin_momeq_dRHO",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dxZjsin_momeq_dRHO")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dxZjsin_momeq_dRHO)
        IF(status.NE.NF90_NOERR) THEN
           ss%dxZjsin_momeq_dRHO = 0.0_rspec
           CALL ncerr_name(2,status,"dxZjsin_momeq_dRHO")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dx_fshield)) THEN
     status = nf90_inq_varid(icdf,"dx_fshield",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dx_fshield")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dx_fshield)
        IF(status.NE.NF90_NOERR) THEN
           ss%dx_fshield = 0.0_rspec
           CALL ncerr_name(2,status,"dx_fshield")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dZ0_momeq_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dZ0_momeq_dRHO",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dZ0_momeq_dRHO")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dZ0_momeq_dRHO)
        IF(status.NE.NF90_NOERR) THEN
           ss%dZ0_momeq_dRHO = 0.0_rspec
           CALL ncerr_name(2,status,"dZ0_momeq_dRHO")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dZ_geo_dRHO)) THEN
     status = nf90_inq_varid(icdf,"dZ_geo_dRHO",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dZ_geo_dRHO")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dZ_geo_dRHO)
        IF(status.NE.NF90_NOERR) THEN
           ss%dZ_geo_dRHO = 0.0_rspec
           CALL ncerr_name(2,status,"dZ_geo_dRHO")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%dZ_geo_dTH)) THEN
     status = nf90_inq_varid(icdf,"dZ_geo_dTH",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"dZ_geo_dTH")
     ELSE
        status = nf90_get_var(icdf,varid,ss%dZ_geo_dTH)
        IF(status.NE.NF90_NOERR) THEN
           ss%dZ_geo_dTH = 0.0_rspec
           CALL ncerr_name(2,status,"dZ_geo_dTH")
        ENDIF
     ENDIF
  ENDIF

  CONTAINS
     SUBROUTINE denull_vec(vstr,isize)
        INTEGER, intent(in) :: isize
        CHARACTER*(*) :: vstr(isize)
        INTEGER :: i

        do i=1,isize
           call denull(vstr(i))
        enddo
     END SUBROUTINE denull_vec

     SUBROUTINE denull(str)
        ! convert null characters to blank
        CHARACTER*(*) :: str
        INTEGER :: i

        do i=1,len(str)
           if(ichar(str(i:i)).eq.0) str(i:i)=' '
        enddo
     END SUBROUTINE denull

     SUBROUTINE ncerr_name(itype,status,name)
        INTEGER, INTENT(in) :: itype
        INTEGER, INTENT(in) :: status
        CHARACTER*(*), INTENT(in) :: name

        if(itype.eq.1) then
           write(iout,*) ' %plasma_state_get: warning: variable name = "'// &
                  trim(name)//'":'
        else
           write(iout,*) ' ?plasma_state_get: error: variable name = "'// &
                  trim(name)//'":'
           if(version_change) then
              write(iout,*) '   => version change RECOVERY: variable reset to initial value.'
           else
              iertot = iertot + 1
           endif
        endif
        call ncerr(status)
     END SUBROUTINE ncerr_name

     SUBROUTINE ncerr(status)
        INTEGER, INTENT(in) :: status

        nc_errmsg = NF90_strerror(status)
        write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
     END SUBROUTINE ncerr

END SUBROUTINE plasma_state_get_1
