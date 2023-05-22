! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_get_2(ss,icdf,version_change,iout,iertot)


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

  IF(ALLOCATED(ss%D_einj_standard)) THEN
     status = nf90_inq_varid(icdf,"D_einj_standard",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"D_einj_standard")
     ELSE
        status = nf90_get_var(icdf,varid,ss%D_einj_standard)
        IF(status.NE.NF90_NOERR) THEN
           ss%D_einj_standard = 0.0_rspec
           CALL ncerr_name(2,status,"D_einj_standard")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%D_ffull_standard)) THEN
     status = nf90_inq_varid(icdf,"D_ffull_standard",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"D_ffull_standard")
     ELSE
        status = nf90_get_var(icdf,varid,ss%D_ffull_standard)
        IF(status.NE.NF90_NOERR) THEN
           ss%D_ffull_standard = 0.0_rspec
           CALL ncerr_name(2,status,"D_ffull_standard")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%D_fhalf_standard)) THEN
     status = nf90_inq_varid(icdf,"D_fhalf_standard",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"D_fhalf_standard")
     ELSE
        status = nf90_get_var(icdf,varid,ss%D_fhalf_standard)
        IF(status.NE.NF90_NOERR) THEN
           ss%D_fhalf_standard = 0.0_rspec
           CALL ncerr_name(2,status,"D_fhalf_standard")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%e0_av)) THEN
     status = nf90_inq_varid(icdf,"e0_av",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"e0_av")
     ELSE
        status = nf90_get_var(icdf,varid,ss%e0_av)
        IF(status.NE.NF90_NOERR) THEN
           ss%e0_av = 0.0_rspec
           CALL ncerr_name(2,status,"e0_av")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ecrf_src_name)) THEN
     status = nf90_inq_varid(icdf,"ecrf_src_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ecrf_src_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ecrf_src_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%ecrf_src_name = " "
           CALL ncerr_name(2,status,"ecrf_src_name")
        ELSE
           CALL denull_vec(ss%ecrf_src_name,ss%necrf_src)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_Beam_Elongation)) THEN
     status = nf90_inq_varid(icdf,"EC_Beam_Elongation",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"EC_Beam_Elongation")
     ELSE
        status = nf90_get_var(icdf,varid,ss%EC_Beam_Elongation)
        IF(status.NE.NF90_NOERR) THEN
           ss%EC_Beam_Elongation = 0.0_rspec
           CALL ncerr_name(2,status,"EC_Beam_Elongation")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Code_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"EC_Code_Info")
  ELSE
     ilen = len(ss%EC_Code_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%EC_Code_Info = " "
     status = nf90_get_var(icdf,varid,ss%EC_Code_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%EC_Code_Info = " "
        CALL ncerr_name(2,status,"EC_Code_Info")
     ELSE
        CALL denull(ss%EC_Code_Info)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"EC_Data_Info")
  ELSE
     ilen = len(ss%EC_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%EC_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%EC_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%EC_Data_Info = " "
        CALL ncerr_name(2,status,"EC_Data_Info")
     ELSE
        CALL denull(ss%EC_Data_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_Half_Power_Angle)) THEN
     status = nf90_inq_varid(icdf,"EC_Half_Power_Angle",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"EC_Half_Power_Angle")
     ELSE
        status = nf90_get_var(icdf,varid,ss%EC_Half_Power_Angle)
        IF(status.NE.NF90_NOERR) THEN
           ss%EC_Half_Power_Angle = 0.0_rspec
           CALL ncerr_name(2,status,"EC_Half_Power_Angle")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_Omode_Fraction)) THEN
     status = nf90_inq_varid(icdf,"EC_Omode_Fraction",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"EC_Omode_Fraction")
     ELSE
        status = nf90_get_var(icdf,varid,ss%EC_Omode_Fraction)
        IF(status.NE.NF90_NOERR) THEN
           ss%EC_Omode_Fraction = 0.0_rspec
           CALL ncerr_name(2,status,"EC_Omode_Fraction")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_phi_aim)) THEN
     status = nf90_inq_varid(icdf,"EC_phi_aim",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"EC_phi_aim")
     ELSE
        status = nf90_get_var(icdf,varid,ss%EC_phi_aim)
        IF(status.NE.NF90_NOERR) THEN
           ss%EC_phi_aim = 0.0_rspec
           CALL ncerr_name(2,status,"EC_phi_aim")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_CURV_HOR)) THEN
     status = nf90_inq_varid(icdf,"EC_PRINC_CURV_HOR",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"EC_PRINC_CURV_HOR")
     ELSE
        status = nf90_get_var(icdf,varid,ss%EC_PRINC_CURV_HOR)
        IF(status.NE.NF90_NOERR) THEN
           ss%EC_PRINC_CURV_HOR = 0.0_rspec
           CALL ncerr_name(2,status,"EC_PRINC_CURV_HOR")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_CURV_VERT)) THEN
     status = nf90_inq_varid(icdf,"EC_PRINC_CURV_VERT",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"EC_PRINC_CURV_VERT")
     ELSE
        status = nf90_get_var(icdf,varid,ss%EC_PRINC_CURV_VERT)
        IF(status.NE.NF90_NOERR) THEN
           ss%EC_PRINC_CURV_VERT = 0.0_rspec
           CALL ncerr_name(2,status,"EC_PRINC_CURV_VERT")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR)) THEN
     status = nf90_inq_varid(icdf,"EC_PRINC_WIDTH_HOR",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"EC_PRINC_WIDTH_HOR")
     ELSE
        status = nf90_get_var(icdf,varid,ss%EC_PRINC_WIDTH_HOR)
        IF(status.NE.NF90_NOERR) THEN
           ss%EC_PRINC_WIDTH_HOR = 0.0_rspec
           CALL ncerr_name(2,status,"EC_PRINC_WIDTH_HOR")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT)) THEN
     status = nf90_inq_varid(icdf,"EC_PRINC_WIDTH_VERT",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"EC_PRINC_WIDTH_VERT")
     ELSE
        status = nf90_get_var(icdf,varid,ss%EC_PRINC_WIDTH_VERT)
        IF(status.NE.NF90_NOERR) THEN
           ss%EC_PRINC_WIDTH_VERT = 0.0_rspec
           CALL ncerr_name(2,status,"EC_PRINC_WIDTH_VERT")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%EC_theta_aim)) THEN
     status = nf90_inq_varid(icdf,"EC_theta_aim",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"EC_theta_aim")
     ELSE
        status = nf90_get_var(icdf,varid,ss%EC_theta_aim)
        IF(status.NE.NF90_NOERR) THEN
           ss%EC_theta_aim = 0.0_rspec
           CALL ncerr_name(2,status,"EC_theta_aim")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ee_mobility_factor)) THEN
     status = nf90_inq_varid(icdf,"ee_mobility_factor",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ee_mobility_factor")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ee_mobility_factor)
        IF(status.NE.NF90_NOERR) THEN
           ss%ee_mobility_factor = 0.0_rspec
           CALL ncerr_name(2,status,"ee_mobility_factor")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Einj_max)) THEN
     status = nf90_inq_varid(icdf,"Einj_max",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Einj_max")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Einj_max)
        IF(status.NE.NF90_NOERR) THEN
           ss%Einj_max = 0.0_rspec
           CALL ncerr_name(2,status,"Einj_max")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Einj_min)) THEN
     status = nf90_inq_varid(icdf,"Einj_min",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Einj_min")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Einj_min)
        IF(status.NE.NF90_NOERR) THEN
           ss%Einj_min = 0.0_rspec
           CALL ncerr_name(2,status,"Einj_min")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eldot",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"eldot")
  ELSE
     status = nf90_get_var(icdf,varid,ss%eldot)
     IF(status.NE.NF90_NOERR) THEN
        ss%eldot = 0.0_rspec
        CALL ncerr_name(2,status,"eldot")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%elong)) THEN
     status = nf90_inq_varid(icdf,"elong",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"elong")
     ELSE
        status = nf90_get_var(icdf,varid,ss%elong)
        IF(status.NE.NF90_NOERR) THEN
           ss%elong = 0.0_rspec
           CALL ncerr_name(2,status,"elong")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%eperp_beami)) THEN
     status = nf90_inq_varid(icdf,"eperp_beami",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"eperp_beami")
     ELSE
        status = nf90_get_var(icdf,varid,ss%eperp_beami)
        IF(status.NE.NF90_NOERR) THEN
           ss%eperp_beami = 0.0_rspec
           CALL ncerr_name(2,status,"eperp_beami")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%eperp_fusi)) THEN
     status = nf90_inq_varid(icdf,"eperp_fusi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"eperp_fusi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%eperp_fusi)
        IF(status.NE.NF90_NOERR) THEN
           ss%eperp_fusi = 0.0_rspec
           CALL ncerr_name(2,status,"eperp_fusi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%eperp_mini)) THEN
     status = nf90_inq_varid(icdf,"eperp_mini",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"eperp_mini")
     ELSE
        status = nf90_get_var(icdf,varid,ss%eperp_mini)
        IF(status.NE.NF90_NOERR) THEN
           ss%eperp_mini = 0.0_rspec
           CALL ncerr_name(2,status,"eperp_mini")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%eperp_rw)) THEN
     status = nf90_inq_varid(icdf,"eperp_rw",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"eperp_rw")
     ELSE
        status = nf90_get_var(icdf,varid,ss%eperp_rw)
        IF(status.NE.NF90_NOERR) THEN
           ss%eperp_rw = 0.0_rspec
           CALL ncerr_name(2,status,"eperp_rw")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%epll_beami)) THEN
     status = nf90_inq_varid(icdf,"epll_beami",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"epll_beami")
     ELSE
        status = nf90_get_var(icdf,varid,ss%epll_beami)
        IF(status.NE.NF90_NOERR) THEN
           ss%epll_beami = 0.0_rspec
           CALL ncerr_name(2,status,"epll_beami")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%epll_fusi)) THEN
     status = nf90_inq_varid(icdf,"epll_fusi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"epll_fusi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%epll_fusi)
        IF(status.NE.NF90_NOERR) THEN
           ss%epll_fusi = 0.0_rspec
           CALL ncerr_name(2,status,"epll_fusi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%epll_mini)) THEN
     status = nf90_inq_varid(icdf,"epll_mini",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"epll_mini")
     ELSE
        status = nf90_get_var(icdf,varid,ss%epll_mini)
        IF(status.NE.NF90_NOERR) THEN
           ss%epll_mini = 0.0_rspec
           CALL ncerr_name(2,status,"epll_mini")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%epll_rw)) THEN
     status = nf90_inq_varid(icdf,"epll_rw",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"epll_rw")
     ELSE
        status = nf90_get_var(icdf,varid,ss%epll_rw)
        IF(status.NE.NF90_NOERR) THEN
           ss%epll_rw = 0.0_rspec
           CALL ncerr_name(2,status,"epll_rw")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Epot)) THEN
     status = nf90_inq_varid(icdf,"Epot",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Epot")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Epot)
        IF(status.NE.NF90_NOERR) THEN
           ss%Epot = 0.0_rspec
           CALL ncerr_name(2,status,"Epot")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eqdsk_file",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"eqdsk_file")
  ELSE
     status = nf90_get_var(icdf,varid,ss%eqdsk_file)
     IF(status.NE.NF90_NOERR) THEN
        ss%eqdsk_file = " "
        CALL ncerr_name(2,status,"eqdsk_file")
     ELSE
        CALL denull(ss%eqdsk_file)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%eqmom_num)) THEN
     status = nf90_inq_varid(icdf,"eqmom_num",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"eqmom_num")
     ELSE
        status = nf90_get_var(icdf,varid,ss%eqmom_num)
        IF(status.NE.NF90_NOERR) THEN
           ss%eqmom_num = " "
           CALL ncerr_name(2,status,"eqmom_num")
        ELSE
           CALL denull_vec(ss%eqmom_num,ss%neqmom)
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EQ_Code_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"EQ_Code_Info")
  ELSE
     ilen = len(ss%EQ_Code_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%EQ_Code_Info = " "
     status = nf90_get_var(icdf,varid,ss%EQ_Code_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%EQ_Code_Info = " "
        CALL ncerr_name(2,status,"EQ_Code_Info")
     ELSE
        CALL denull(ss%EQ_Code_Info)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EQ_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"EQ_Data_Info")
  ELSE
     ilen = len(ss%EQ_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%EQ_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%EQ_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%EQ_Data_Info = " "
        CALL ncerr_name(2,status,"EQ_Data_Info")
     ELSE
        CALL denull(ss%EQ_Data_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%eta_parallel)) THEN
     status = nf90_inq_varid(icdf,"eta_parallel",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"eta_parallel")
     ELSE
        status = nf90_get_var(icdf,varid,ss%eta_parallel)
        IF(status.NE.NF90_NOERR) THEN
           ss%eta_parallel = 0.0_rspec
           CALL ncerr_name(2,status,"eta_parallel")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%E_anom)) THEN
     status = nf90_inq_varid(icdf,"E_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"E_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%E_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%E_anom = 0.0_rspec
           CALL ncerr_name(2,status,"E_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%E_anom2)) THEN
     status = nf90_inq_varid(icdf,"E_anom2",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"E_anom2")
     ELSE
        status = nf90_get_var(icdf,varid,ss%E_anom2)
        IF(status.NE.NF90_NOERR) THEN
           ss%E_anom2 = 0.0_rspec
           CALL ncerr_name(2,status,"E_anom2")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fidiff_bpass_co)) THEN
     status = nf90_inq_varid(icdf,"fidiff_bpass_co",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"fidiff_bpass_co")
     ELSE
        status = nf90_get_var(icdf,varid,ss%fidiff_bpass_co)
        IF(status.NE.NF90_NOERR) THEN
           ss%fidiff_bpass_co = 0.0_rspec
           CALL ncerr_name(2,status,"fidiff_bpass_co")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fidiff_bpass_ctr)) THEN
     status = nf90_inq_varid(icdf,"fidiff_bpass_ctr",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"fidiff_bpass_ctr")
     ELSE
        status = nf90_get_var(icdf,varid,ss%fidiff_bpass_ctr)
        IF(status.NE.NF90_NOERR) THEN
           ss%fidiff_bpass_ctr = 0.0_rspec
           CALL ncerr_name(2,status,"fidiff_bpass_ctr")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fidiff_btrap)) THEN
     status = nf90_inq_varid(icdf,"fidiff_btrap",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"fidiff_btrap")
     ELSE
        status = nf90_get_var(icdf,varid,ss%fidiff_btrap)
        IF(status.NE.NF90_NOERR) THEN
           ss%fidiff_btrap = 0.0_rspec
           CALL ncerr_name(2,status,"fidiff_btrap")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fidiff_dpass_co)) THEN
     status = nf90_inq_varid(icdf,"fidiff_dpass_co",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"fidiff_dpass_co")
     ELSE
        status = nf90_get_var(icdf,varid,ss%fidiff_dpass_co)
        IF(status.NE.NF90_NOERR) THEN
           ss%fidiff_dpass_co = 0.0_rspec
           CALL ncerr_name(2,status,"fidiff_dpass_co")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fidiff_dpass_ctr)) THEN
     status = nf90_inq_varid(icdf,"fidiff_dpass_ctr",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"fidiff_dpass_ctr")
     ELSE
        status = nf90_get_var(icdf,varid,ss%fidiff_dpass_ctr)
        IF(status.NE.NF90_NOERR) THEN
           ss%fidiff_dpass_ctr = 0.0_rspec
           CALL ncerr_name(2,status,"fidiff_dpass_ctr")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fidiff_dtrap)) THEN
     status = nf90_inq_varid(icdf,"fidiff_dtrap",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"fidiff_dtrap")
     ELSE
        status = nf90_get_var(icdf,varid,ss%fidiff_dtrap)
        IF(status.NE.NF90_NOERR) THEN
           ss%fidiff_dtrap = 0.0_rspec
           CALL ncerr_name(2,status,"fidiff_dtrap")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Fi_depletion)) THEN
     status = nf90_inq_varid(icdf,"Fi_depletion",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Fi_depletion")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Fi_depletion)
        IF(status.NE.NF90_NOERR) THEN
           ss%Fi_depletion = 0.0_rspec
           CALL ncerr_name(2,status,"Fi_depletion")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fracmin)) THEN
     status = nf90_inq_varid(icdf,"fracmin",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"fracmin")
     ELSE
        status = nf90_get_var(icdf,varid,ss%fracmin)
        IF(status.NE.NF90_NOERR) THEN
           ss%fracmin = 0.0_rspec
           CALL ncerr_name(2,status,"fracmin")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%fracpslh)) THEN
     status = nf90_inq_varid(icdf,"fracpslh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"fracpslh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%fracpslh)
        IF(status.NE.NF90_NOERR) THEN
           ss%fracpslh = 0.0_rspec
           CALL ncerr_name(2,status,"fracpslh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%frac_full)) THEN
     status = nf90_inq_varid(icdf,"frac_full",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"frac_full")
     ELSE
        status = nf90_get_var(icdf,varid,ss%frac_full)
        IF(status.NE.NF90_NOERR) THEN
           ss%frac_full = 0.0_rspec
           CALL ncerr_name(2,status,"frac_full")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%frac_half)) THEN
     status = nf90_inq_varid(icdf,"frac_half",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"frac_half")
     ELSE
        status = nf90_get_var(icdf,varid,ss%frac_half)
        IF(status.NE.NF90_NOERR) THEN
           ss%frac_half = 0.0_rspec
           CALL ncerr_name(2,status,"frac_half")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_drbm)) THEN
     status = nf90_inq_varid(icdf,"freq_drbm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"freq_drbm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%freq_drbm)
        IF(status.NE.NF90_NOERR) THEN
           ss%freq_drbm = 0.0_rspec
           CALL ncerr_name(2,status,"freq_drbm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_ec)) THEN
     status = nf90_inq_varid(icdf,"freq_ec",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"freq_ec")
     ELSE
        status = nf90_get_var(icdf,varid,ss%freq_ec)
        IF(status.NE.NF90_NOERR) THEN
           ss%freq_ec = 0.0_rspec
           CALL ncerr_name(2,status,"freq_ec")
        ELSE
           CALL ps_varchk(ss,"freq_ec",icdf,varid,iout)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_etg)) THEN
     status = nf90_inq_varid(icdf,"freq_etg",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"freq_etg")
     ELSE
        status = nf90_get_var(icdf,varid,ss%freq_etg)
        IF(status.NE.NF90_NOERR) THEN
           ss%freq_etg = 0.0_rspec
           CALL ncerr_name(2,status,"freq_etg")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_glf)) THEN
     status = nf90_inq_varid(icdf,"freq_glf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"freq_glf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%freq_glf)
        IF(status.NE.NF90_NOERR) THEN
           ss%freq_glf = 0.0_rspec
           CALL ncerr_name(2,status,"freq_glf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_ic)) THEN
     status = nf90_inq_varid(icdf,"freq_ic",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"freq_ic")
     ELSE
        status = nf90_get_var(icdf,varid,ss%freq_ic)
        IF(status.NE.NF90_NOERR) THEN
           ss%freq_ic = 0.0_rspec
           CALL ncerr_name(2,status,"freq_ic")
        ELSE
           CALL ps_varchk(ss,"freq_ic",icdf,varid,iout)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_lh)) THEN
     status = nf90_inq_varid(icdf,"freq_lh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"freq_lh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%freq_lh)
        IF(status.NE.NF90_NOERR) THEN
           ss%freq_lh = 0.0_rspec
           CALL ncerr_name(2,status,"freq_lh")
        ELSE
           CALL ps_varchk(ss,"freq_lh",icdf,varid,iout)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_max_tglf)) THEN
     status = nf90_inq_varid(icdf,"freq_max_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"freq_max_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%freq_max_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%freq_max_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"freq_max_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_mmm71)) THEN
     status = nf90_inq_varid(icdf,"freq_mmm71",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"freq_mmm71")
     ELSE
        status = nf90_get_var(icdf,varid,ss%freq_mmm71)
        IF(status.NE.NF90_NOERR) THEN
           ss%freq_mmm71 = 0.0_rspec
           CALL ncerr_name(2,status,"freq_mmm71")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_mtm)) THEN
     status = nf90_inq_varid(icdf,"freq_mtm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"freq_mtm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%freq_mtm)
        IF(status.NE.NF90_NOERR) THEN
           ss%freq_mtm = 0.0_rspec
           CALL ncerr_name(2,status,"freq_mtm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%freq_tglf)) THEN
     status = nf90_inq_varid(icdf,"freq_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"freq_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%freq_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%freq_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"freq_tglf")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"FUS_Code_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"FUS_Code_Info")
  ELSE
     ilen = len(ss%FUS_Code_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%FUS_Code_Info = " "
     status = nf90_get_var(icdf,varid,ss%FUS_Code_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%FUS_Code_Info = " "
        CALL ncerr_name(2,status,"FUS_Code_Info")
     ELSE
        CALL denull(ss%FUS_Code_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gamma_lmhd)) THEN
     status = nf90_inq_varid(icdf,"gamma_lmhd",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gamma_lmhd")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gamma_lmhd)
        IF(status.NE.NF90_NOERR) THEN
           ss%gamma_lmhd = 0.0_rspec
           CALL ncerr_name(2,status,"gamma_lmhd")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gamma_nc)) THEN
     status = nf90_inq_varid(icdf,"gamma_nc",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gamma_nc")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gamma_nc)
        IF(status.NE.NF90_NOERR) THEN
           ss%gamma_nc = 0.0_rspec
           CALL ncerr_name(2,status,"gamma_nc")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gas_atom)) THEN
     status = nf90_inq_varid(icdf,"gas_atom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gas_atom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gas_atom)
        IF(status.NE.NF90_NOERR) THEN
           ss%gas_atom = " "
           CALL ncerr_name(2,status,"gas_atom")
        ELSE
           CALL denull_vec(ss%gas_atom,ss%ngsc0)
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"GAS_Code_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"GAS_Code_Info")
  ELSE
     ilen = len(ss%GAS_Code_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%GAS_Code_Info = " "
     status = nf90_get_var(icdf,varid,ss%GAS_Code_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%GAS_Code_Info = " "
        CALL ncerr_name(2,status,"GAS_Code_Info")
     ELSE
        CALL denull(ss%GAS_Code_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gb1)) THEN
     status = nf90_inq_varid(icdf,"gb1",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gb1")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gb1)
        IF(status.NE.NF90_NOERR) THEN
           ss%gb1 = 0.0_rspec
           CALL ncerr_name(2,status,"gb1")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gb2)) THEN
     status = nf90_inq_varid(icdf,"gb2",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gb2")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gb2)
        IF(status.NE.NF90_NOERR) THEN
           ss%gb2 = 0.0_rspec
           CALL ncerr_name(2,status,"gb2")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gb2i)) THEN
     status = nf90_inq_varid(icdf,"gb2i",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gb2i")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gb2i)
        IF(status.NE.NF90_NOERR) THEN
           ss%gb2i = 0.0_rspec
           CALL ncerr_name(2,status,"gb2i")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gbr2)) THEN
     status = nf90_inq_varid(icdf,"gbr2",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gbr2")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gbr2)
        IF(status.NE.NF90_NOERR) THEN
           ss%gbr2 = 0.0_rspec
           CALL ncerr_name(2,status,"gbr2")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"geometry",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"geometry")
  ELSE
     status = nf90_get_var(icdf,varid,ss%geometry)
     IF(status.NE.NF90_NOERR) THEN
        ss%geometry = " "
        CALL ncerr_name(2,status,"geometry")
     ELSE
        CALL denull(ss%geometry)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Global_label",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"Global_label")
  ELSE
     ilen = len(ss%Global_label)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%Global_label = " "
     status = nf90_get_var(icdf,varid,ss%Global_label(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%Global_label = " "
        CALL ncerr_name(2,status,"Global_label")
     ELSE
        CALL denull(ss%Global_label)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gncfb2h)) THEN
     status = nf90_inq_varid(icdf,"gncfb2h",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gncfb2h")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gncfb2h)
        IF(status.NE.NF90_NOERR) THEN
           ss%gncfb2h = 0.0_rspec
           CALL ncerr_name(2,status,"gncfb2h")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gncfh)) THEN
     status = nf90_inq_varid(icdf,"gncfh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gncfh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gncfh)
        IF(status.NE.NF90_NOERR) THEN
           ss%gncfh = 0.0_rspec
           CALL ncerr_name(2,status,"gncfh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gr1)) THEN
     status = nf90_inq_varid(icdf,"gr1",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gr1")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gr1)
        IF(status.NE.NF90_NOERR) THEN
           ss%gr1 = 0.0_rspec
           CALL ncerr_name(2,status,"gr1")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gr2)) THEN
     status = nf90_inq_varid(icdf,"gr2",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gr2")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gr2)
        IF(status.NE.NF90_NOERR) THEN
           ss%gr2 = 0.0_rspec
           CALL ncerr_name(2,status,"gr2")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gr2i)) THEN
     status = nf90_inq_varid(icdf,"gr2i",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gr2i")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gr2i)
        IF(status.NE.NF90_NOERR) THEN
           ss%gr2i = 0.0_rspec
           CALL ncerr_name(2,status,"gr2i")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gr2rho2)) THEN
     status = nf90_inq_varid(icdf,"gr2rho2",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gr2rho2")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gr2rho2)
        IF(status.NE.NF90_NOERR) THEN
           ss%gr2rho2 = 0.0_rspec
           CALL ncerr_name(2,status,"gr2rho2")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%gr3i)) THEN
     status = nf90_inq_varid(icdf,"gr3i",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"gr3i")
     ELSE
        status = nf90_get_var(icdf,varid,ss%gr3i)
        IF(status.NE.NF90_NOERR) THEN
           ss%gr3i = 0.0_rspec
           CALL ncerr_name(2,status,"gr3i")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%grho1)) THEN
     status = nf90_inq_varid(icdf,"grho1",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"grho1")
     ELSE
        status = nf90_get_var(icdf,varid,ss%grho1)
        IF(status.NE.NF90_NOERR) THEN
           ss%grho1 = 0.0_rspec
           CALL ncerr_name(2,status,"grho1")
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

END SUBROUTINE plasma_state_get_2
