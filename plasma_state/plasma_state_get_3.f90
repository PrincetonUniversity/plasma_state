! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_get_3(ss,icdf,version_change,iout,iertot)


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

  IF(ALLOCATED(ss%grho2)) THEN
     status = nf90_inq_varid(icdf,"grho2",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"grho2")
     ELSE
        status = nf90_get_var(icdf,varid,ss%grho2)
        IF(status.NE.NF90_NOERR) THEN
           ss%grho2 = 0.0_rspec
           CALL ncerr_name(2,status,"grho2")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%grho2b2i)) THEN
     status = nf90_inq_varid(icdf,"grho2b2i",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"grho2b2i")
     ELSE
        status = nf90_get_var(icdf,varid,ss%grho2b2i)
        IF(status.NE.NF90_NOERR) THEN
           ss%grho2b2i = 0.0_rspec
           CALL ncerr_name(2,status,"grho2b2i")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%grho2r2i)) THEN
     status = nf90_inq_varid(icdf,"grho2r2i",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"grho2r2i")
     ELSE
        status = nf90_get_var(icdf,varid,ss%grho2r2i)
        IF(status.NE.NF90_NOERR) THEN
           ss%grho2r2i = 0.0_rspec
           CALL ncerr_name(2,status,"grho2r2i")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%grho2r3i)) THEN
     status = nf90_inq_varid(icdf,"grho2r3i",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"grho2r3i")
     ELSE
        status = nf90_get_var(icdf,varid,ss%grho2r3i)
        IF(status.NE.NF90_NOERR) THEN
           ss%grho2r3i = 0.0_rspec
           CALL ncerr_name(2,status,"grho2r3i")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gri)) THEN
     status = nf90_inq_varid(icdf,"gri",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gri")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gri)
        IF(status.NE.NF90_NOERR) THEN
           ss%gri = 0.0_rspec
           CALL ncerr_name(2,status,"gri")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%grirhoi)) THEN
     status = nf90_inq_varid(icdf,"grirhoi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"grirhoi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%grirhoi)
        IF(status.NE.NF90_NOERR) THEN
           ss%grirhoi = 0.0_rspec
           CALL ncerr_name(2,status,"grirhoi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_drbm)) THEN
     status = nf90_inq_varid(icdf,"growthrate_drbm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"growthrate_drbm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%growthrate_drbm)
        IF(status.NE.NF90_NOERR) THEN
           ss%growthrate_drbm = 0.0_rspec
           CALL ncerr_name(2,status,"growthrate_drbm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_etg)) THEN
     status = nf90_inq_varid(icdf,"growthrate_etg",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"growthrate_etg")
     ELSE
        status = nf90_get_var(icdf,varid,ss%growthrate_etg)
        IF(status.NE.NF90_NOERR) THEN
           ss%growthrate_etg = 0.0_rspec
           CALL ncerr_name(2,status,"growthrate_etg")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_glf)) THEN
     status = nf90_inq_varid(icdf,"growthrate_glf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"growthrate_glf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%growthrate_glf)
        IF(status.NE.NF90_NOERR) THEN
           ss%growthrate_glf = 0.0_rspec
           CALL ncerr_name(2,status,"growthrate_glf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_max_tglf)) THEN
     status = nf90_inq_varid(icdf,"growthrate_max_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"growthrate_max_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%growthrate_max_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%growthrate_max_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"growthrate_max_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_mmm71)) THEN
     status = nf90_inq_varid(icdf,"growthrate_mmm71",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"growthrate_mmm71")
     ELSE
        status = nf90_get_var(icdf,varid,ss%growthrate_mmm71)
        IF(status.NE.NF90_NOERR) THEN
           ss%growthrate_mmm71 = 0.0_rspec
           CALL ncerr_name(2,status,"growthrate_mmm71")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_mtm)) THEN
     status = nf90_inq_varid(icdf,"growthrate_mtm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"growthrate_mtm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%growthrate_mtm)
        IF(status.NE.NF90_NOERR) THEN
           ss%growthrate_mtm = 0.0_rspec
           CALL ncerr_name(2,status,"growthrate_mtm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_tglf)) THEN
     status = nf90_inq_varid(icdf,"growthrate_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"growthrate_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%growthrate_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%growthrate_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"growthrate_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%growthrate_vpar)) THEN
     status = nf90_inq_varid(icdf,"growthrate_vpar",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"growthrate_vpar")
     ELSE
        status = nf90_get_var(icdf,varid,ss%growthrate_vpar)
        IF(status.NE.NF90_NOERR) THEN
           ss%growthrate_vpar = 0.0_rspec
           CALL ncerr_name(2,status,"growthrate_vpar")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gs_name)) THEN
     status = nf90_inq_varid(icdf,"gs_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gs_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gs_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%gs_name = " "
           CALL ncerr_name(2,status,"gs_name")
        ELSE
           CALL denull_vec(ss%gs_name,ss%ngsc0)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%g_eq)) THEN
     status = nf90_inq_varid(icdf,"g_eq",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"g_eq")
     ELSE
        status = nf90_get_var(icdf,varid,ss%g_eq)
        IF(status.NE.NF90_NOERR) THEN
           ss%g_eq = 0.0_rspec
           CALL ncerr_name(2,status,"g_eq")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%hegrilh)) THEN
     status = nf90_inq_varid(icdf,"hegrilh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"hegrilh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%hegrilh)
        IF(status.NE.NF90_NOERR) THEN
           ss%hegrilh = 0.0_rspec
           CALL ncerr_name(2,status,"hegrilh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Hsize_coil)) THEN
     status = nf90_inq_varid(icdf,"Hsize_coil",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Hsize_coil")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Hsize_coil)
        IF(status.NE.NF90_NOERR) THEN
           ss%Hsize_coil = 0.0_rspec
           CALL ncerr_name(2,status,"Hsize_coil")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%H_einj_standard)) THEN
     status = nf90_inq_varid(icdf,"H_einj_standard",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"H_einj_standard")
     ELSE
        status = nf90_get_var(icdf,varid,ss%H_einj_standard)
        IF(status.NE.NF90_NOERR) THEN
           ss%H_einj_standard = 0.0_rspec
           CALL ncerr_name(2,status,"H_einj_standard")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%H_ffull_standard)) THEN
     status = nf90_inq_varid(icdf,"H_ffull_standard",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"H_ffull_standard")
     ELSE
        status = nf90_get_var(icdf,varid,ss%H_ffull_standard)
        IF(status.NE.NF90_NOERR) THEN
           ss%H_ffull_standard = 0.0_rspec
           CALL ncerr_name(2,status,"H_ffull_standard")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%H_fhalf_standard)) THEN
     status = nf90_inq_varid(icdf,"H_fhalf_standard",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"H_fhalf_standard")
     ELSE
        status = nf90_get_var(icdf,varid,ss%H_fhalf_standard)
        IF(status.NE.NF90_NOERR) THEN
           ss%H_fhalf_standard = 0.0_rspec
           CALL ncerr_name(2,status,"H_fhalf_standard")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%icircuit_coil)) THEN
     status = nf90_inq_varid(icdf,"icircuit_coil",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"icircuit_coil")
     ELSE
        status = nf90_get_var(icdf,varid,ss%icircuit_coil)
        IF(status.NE.NF90_NOERR) THEN
           ss%icircuit_coil = 0
           CALL ncerr_name(2,status,"icircuit_coil")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%icrf_src_name)) THEN
     status = nf90_inq_varid(icdf,"icrf_src_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"icrf_src_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%icrf_src_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%icrf_src_name = " "
           CALL ncerr_name(2,status,"icrf_src_name")
        ELSE
           CALL denull_vec(ss%icrf_src_name,ss%nicrf_src)
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"IC_Code_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"IC_Code_Info")
  ELSE
     ilen = len(ss%IC_Code_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%IC_Code_Info = " "
     status = nf90_get_var(icdf,varid,ss%IC_Code_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%IC_Code_Info = " "
        CALL ncerr_name(2,status,"IC_Code_Info")
     ELSE
        CALL denull(ss%IC_Code_Info)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"IC_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"IC_Data_Info")
  ELSE
     ilen = len(ss%IC_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%IC_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%IC_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%IC_Data_Info = " "
        CALL ncerr_name(2,status,"IC_Data_Info")
     ELSE
        CALL denull(ss%IC_Data_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Imag_ant_coef)) THEN
     status = nf90_inq_varid(icdf,"Imag_ant_coef",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Imag_ant_coef")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Imag_ant_coef)
        IF(status.NE.NF90_NOERR) THEN
           ss%Imag_ant_coef = 0.0_rspec
           CALL ncerr_name(2,status,"Imag_ant_coef")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"interchange_R",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"interchange_R")
  ELSE
     status = nf90_get_var(icdf,varid,ss%interchange_R)
     IF(status.NE.NF90_NOERR) THEN
        ss%interchange_R = 0.0_rspec
        CALL ncerr_name(2,status,"interchange_R")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%iota)) THEN
     status = nf90_inq_varid(icdf,"iota",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"iota")
     ELSE
        status = nf90_get_var(icdf,varid,ss%iota)
        IF(status.NE.NF90_NOERR) THEN
           ss%iota = 0.0_rspec
           CALL ncerr_name(2,status,"iota")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%isThermal)) THEN
     status = nf90_inq_varid(icdf,"isThermal",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"isThermal")
     ELSE
        status = nf90_get_var(icdf,varid,ss%isThermal)
        IF(status.NE.NF90_NOERR) THEN
           ss%isThermal = 0
           CALL ncerr_name(2,status,"isThermal")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%is_recycling)) THEN
     status = nf90_inq_varid(icdf,"is_recycling",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"is_recycling")
     ELSE
        status = nf90_get_var(icdf,varid,ss%is_recycling)
        IF(status.NE.NF90_NOERR) THEN
           ss%is_recycling = 0
           CALL ncerr_name(2,status,"is_recycling")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"i_npoloidal",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"i_npoloidal")
  ELSE
     status = nf90_get_var(icdf,varid,ss%i_npoloidal)
     IF(status.NE.NF90_NOERR) THEN
        ss%i_npoloidal = ps_unInit
        CALL ncerr_name(2,status,"i_npoloidal")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%jdotb)) THEN
     status = nf90_inq_varid(icdf,"jdotb",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"jdotb")
     ELSE
        status = nf90_get_var(icdf,varid,ss%jdotb)
        IF(status.NE.NF90_NOERR) THEN
           ss%jdotb = 0.0_rspec
           CALL ncerr_name(2,status,"jdotb")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"kccw_Bphi",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"kccw_Bphi")
  ELSE
     status = nf90_get_var(icdf,varid,ss%kccw_Bphi)
     IF(status.NE.NF90_NOERR) THEN
        ss%kccw_Bphi = 0
        CALL ncerr_name(2,status,"kccw_Bphi")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"kccw_Jphi",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"kccw_Jphi")
  ELSE
     status = nf90_get_var(icdf,varid,ss%kccw_Jphi)
     IF(status.NE.NF90_NOERR) THEN
        ss%kccw_Jphi = 0
        CALL ncerr_name(2,status,"kccw_Jphi")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"kdens_rfmin",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"kdens_rfmin")
  ELSE
     ilen = len(ss%kdens_rfmin)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%kdens_rfmin = "data"
     status = nf90_get_var(icdf,varid,ss%kdens_rfmin(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%kdens_rfmin = "data"
        CALL ncerr_name(2,status,"kdens_rfmin")
     ELSE
        CALL denull(ss%kdens_rfmin)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%kvolt_nbi)) THEN
     status = nf90_inq_varid(icdf,"kvolt_nbi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"kvolt_nbi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%kvolt_nbi)
        IF(status.NE.NF90_NOERR) THEN
           ss%kvolt_nbi = 0.0_rspec
           CALL ncerr_name(2,status,"kvolt_nbi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%kyrhos_drbm)) THEN
     status = nf90_inq_varid(icdf,"kyrhos_drbm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"kyrhos_drbm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%kyrhos_drbm)
        IF(status.NE.NF90_NOERR) THEN
           ss%kyrhos_drbm = 0.0_rspec
           CALL ncerr_name(2,status,"kyrhos_drbm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%kyrhos_etg)) THEN
     status = nf90_inq_varid(icdf,"kyrhos_etg",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"kyrhos_etg")
     ELSE
        status = nf90_get_var(icdf,varid,ss%kyrhos_etg)
        IF(status.NE.NF90_NOERR) THEN
           ss%kyrhos_etg = 0.0_rspec
           CALL ncerr_name(2,status,"kyrhos_etg")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%kyrhos_mtm)) THEN
     status = nf90_inq_varid(icdf,"kyrhos_mtm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"kyrhos_mtm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%kyrhos_mtm)
        IF(status.NE.NF90_NOERR) THEN
           ss%kyrhos_mtm = 0.0_rspec
           CALL ncerr_name(2,status,"kyrhos_mtm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%kyspectrum_tglf)) THEN
     status = nf90_inq_varid(icdf,"kyspectrum_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"kyspectrum_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%kyspectrum_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%kyspectrum_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"kyspectrum_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Lbscap)) THEN
     status = nf90_inq_varid(icdf,"Lbscap",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Lbscap")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Lbscap)
        IF(status.NE.NF90_NOERR) THEN
           ss%Lbscap = 0.0_rspec
           CALL ncerr_name(2,status,"Lbscap")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Lbscap2)) THEN
     status = nf90_inq_varid(icdf,"Lbscap2",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Lbscap2")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Lbscap2)
        IF(status.NE.NF90_NOERR) THEN
           ss%Lbscap2 = 0.0_rspec
           CALL ncerr_name(2,status,"Lbscap2")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Lbsctan)) THEN
     status = nf90_inq_varid(icdf,"Lbsctan",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Lbsctan")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Lbsctan)
        IF(status.NE.NF90_NOERR) THEN
           ss%Lbsctan = 0.0_rspec
           CALL ncerr_name(2,status,"Lbsctan")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%lhrf_src_name)) THEN
     status = nf90_inq_varid(icdf,"lhrf_src_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"lhrf_src_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%lhrf_src_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%lhrf_src_name = " "
           CALL ncerr_name(2,status,"lhrf_src_name")
        ELSE
           CALL denull_vec(ss%lhrf_src_name,ss%nlhrf_src)
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"LH_Code_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"LH_Code_Info")
  ELSE
     ilen = len(ss%LH_Code_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%LH_Code_Info = " "
     status = nf90_get_var(icdf,varid,ss%LH_Code_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%LH_Code_Info = " "
        CALL ncerr_name(2,status,"LH_Code_Info")
     ELSE
        CALL denull(ss%LH_Code_Info)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"LH_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"LH_Data_Info")
  ELSE
     ilen = len(ss%LH_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%LH_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%LH_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%LH_Data_Info = " "
        CALL ncerr_name(2,status,"LH_Data_Info")
     ELSE
        CALL denull(ss%LH_Data_Info)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"LMHD_eq_code",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"LMHD_eq_code")
  ELSE
     ilen = len(ss%LMHD_eq_code)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%LMHD_eq_code = " "
     status = nf90_get_var(icdf,varid,ss%LMHD_eq_code(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%LMHD_eq_code = " "
        CALL ncerr_name(2,status,"LMHD_eq_code")
     ELSE
        CALL denull(ss%LMHD_eq_code)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"LMHD_status",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"LMHD_status")
  ELSE
     status = nf90_get_var(icdf,varid,ss%LMHD_status)
     IF(status.NE.NF90_NOERR) THEN
        ss%LMHD_status = ps_unInit
        CALL ncerr_name(2,status,"LMHD_status")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"lock_machine_descr",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"lock_machine_descr")
  ELSE
     status = nf90_get_var(icdf,varid,ss%lock_machine_descr)
     IF(status.NE.NF90_NOERR) THEN
        ss%lock_machine_descr = 0
        CALL ncerr_name(2,status,"lock_machine_descr")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"lock_shot_config",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"lock_shot_config")
  ELSE
     status = nf90_get_var(icdf,varid,ss%lock_shot_config)
     IF(status.NE.NF90_NOERR) THEN
        ss%lock_shot_config = 0
        CALL ncerr_name(2,status,"lock_shot_config")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"lock_sim_init",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"lock_sim_init")
  ELSE
     status = nf90_get_var(icdf,varid,ss%lock_sim_init)
     IF(status.NE.NF90_NOERR) THEN
        ss%lock_sim_init = 0
        CALL ncerr_name(2,status,"lock_sim_init")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Lpol)) THEN
     status = nf90_inq_varid(icdf,"Lpol",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Lpol")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Lpol)
        IF(status.NE.NF90_NOERR) THEN
           ss%Lpol = 0.0_rspec
           CALL ncerr_name(2,status,"Lpol")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Mercier_stable)) THEN
     status = nf90_inq_varid(icdf,"Mercier_stable",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Mercier_stable")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Mercier_stable)
        IF(status.NE.NF90_NOERR) THEN
           ss%Mercier_stable = 0
           CALL ncerr_name(2,status,"Mercier_stable")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"MHD_eq_status",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"MHD_eq_status")
  ELSE
     status = nf90_get_var(icdf,varid,ss%MHD_eq_status)
     IF(status.NE.NF90_NOERR) THEN
        ss%MHD_eq_status = ps_unInit
        CALL ncerr_name(2,status,"MHD_eq_status")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_ALL)) THEN
     status = nf90_inq_varid(icdf,"m_ALL",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"m_ALL")
     ELSE
        status = nf90_get_var(icdf,varid,ss%m_ALL)
        IF(status.NE.NF90_NOERR) THEN
           ss%m_ALL = 0.0_rspec
           CALL ncerr_name(2,status,"m_ALL")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_ALLA)) THEN
     status = nf90_inq_varid(icdf,"m_ALLA",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"m_ALLA")
     ELSE
        status = nf90_get_var(icdf,varid,ss%m_ALLA)
        IF(status.NE.NF90_NOERR) THEN
           ss%m_ALLA = 0.0_rspec
           CALL ncerr_name(2,status,"m_ALLA")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_RFMIN)) THEN
     status = nf90_inq_varid(icdf,"m_RFMIN",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"m_RFMIN")
     ELSE
        status = nf90_get_var(icdf,varid,ss%m_RFMIN)
        IF(status.NE.NF90_NOERR) THEN
           ss%m_RFMIN = 0.0_rspec
           CALL ncerr_name(2,status,"m_RFMIN")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_S)) THEN
     status = nf90_inq_varid(icdf,"m_S",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"m_S")
     ELSE
        status = nf90_get_var(icdf,varid,ss%m_S)
        IF(status.NE.NF90_NOERR) THEN
           ss%m_S = 0.0_rspec
           CALL ncerr_name(2,status,"m_S")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_SA)) THEN
     status = nf90_inq_varid(icdf,"m_SA",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"m_SA")
     ELSE
        status = nf90_get_var(icdf,varid,ss%m_SA)
        IF(status.NE.NF90_NOERR) THEN
           ss%m_SA = 0.0_rspec
           CALL ncerr_name(2,status,"m_SA")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_SFUS)) THEN
     status = nf90_inq_varid(icdf,"m_SFUS",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"m_SFUS")
     ELSE
        status = nf90_get_var(icdf,varid,ss%m_SFUS)
        IF(status.NE.NF90_NOERR) THEN
           ss%m_SFUS = 0.0_rspec
           CALL ncerr_name(2,status,"m_SFUS")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_SGAS)) THEN
     status = nf90_inq_varid(icdf,"m_SGAS",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"m_SGAS")
     ELSE
        status = nf90_get_var(icdf,varid,ss%m_SGAS)
        IF(status.NE.NF90_NOERR) THEN
           ss%m_SGAS = 0.0_rspec
           CALL ncerr_name(2,status,"m_SGAS")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_SIMP0)) THEN
     status = nf90_inq_varid(icdf,"m_SIMP0",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"m_SIMP0")
     ELSE
        status = nf90_get_var(icdf,varid,ss%m_SIMP0)
        IF(status.NE.NF90_NOERR) THEN
           ss%m_SIMP0 = 0.0_rspec
           CALL ncerr_name(2,status,"m_SIMP0")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_SIMPI)) THEN
     status = nf90_inq_varid(icdf,"m_SIMPI",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"m_SIMPI")
     ELSE
        status = nf90_get_var(icdf,varid,ss%m_SIMPI)
        IF(status.NE.NF90_NOERR) THEN
           ss%m_SIMPI = 0.0_rspec
           CALL ncerr_name(2,status,"m_SIMPI")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%m_SNBI)) THEN
     status = nf90_inq_varid(icdf,"m_SNBI",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"m_SNBI")
     ELSE
        status = nf90_get_var(icdf,varid,ss%m_SNBI)
        IF(status.NE.NF90_NOERR) THEN
           ss%m_SNBI = 0.0_rspec
           CALL ncerr_name(2,status,"m_SNBI")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%n0norm)) THEN
     status = nf90_inq_varid(icdf,"n0norm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"n0norm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%n0norm)
        IF(status.NE.NF90_NOERR) THEN
           ss%n0norm = 0.0_rspec
           CALL ncerr_name(2,status,"n0norm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%n0_halo)) THEN
     status = nf90_inq_varid(icdf,"n0_halo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"n0_halo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%n0_halo)
        IF(status.NE.NF90_NOERR) THEN
           ss%n0_halo = 0.0_rspec
           CALL ncerr_name(2,status,"n0_halo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%n0_reco)) THEN
     status = nf90_inq_varid(icdf,"n0_reco",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"n0_reco")
     ELSE
        status = nf90_get_var(icdf,varid,ss%n0_reco)
        IF(status.NE.NF90_NOERR) THEN
           ss%n0_reco = 0.0_rspec
           CALL ncerr_name(2,status,"n0_reco")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nbap2_shape)) THEN
     status = nf90_inq_varid(icdf,"nbap2_shape",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nbap2_shape")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nbap2_shape)
        IF(status.NE.NF90_NOERR) THEN
           ss%nbap2_shape = " "
           CALL ncerr_name(2,status,"nbap2_shape")
        ELSE
           CALL denull_vec(ss%nbap2_shape,ss%nbeam)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nbap_shape)) THEN
     status = nf90_inq_varid(icdf,"nbap_shape",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nbap_shape")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nbap_shape)
        IF(status.NE.NF90_NOERR) THEN
           ss%nbap_shape = " "
           CALL ncerr_name(2,status,"nbap_shape")
        ELSE
           CALL denull_vec(ss%nbap_shape,ss%nbeam)
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

END SUBROUTINE plasma_state_get_3
