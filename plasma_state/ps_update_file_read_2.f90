! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_read_2(ss,icdf,iout,iucount,iertot)


  ! Read updated data

  USE plasma_state_definition_mod
  USE plasma_hash_code
  USE netcdf
  IMPLICIT NONE

  TYPE (plasma_state) :: ss  ! state containing array elements to be allocated
  INTEGER, INTENT(IN) :: icdf  ! NetCDF file ID
  INTEGER, INTENT(IN) :: iout  ! LUN for messages
  INTEGER, INTENT(INOUT) :: iucount ! update count
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !-------------------

  INTEGER :: varid,status
  INTEGER :: ival,jval,ii
  INTEGER :: prehash1,prehash2
  INTEGER :: posthash1,posthash2
  CHARACTER*80 :: nc_errmsg
  LOGICAL :: izero

  !-------------------


  status = nf90_inq_varid(icdf,"D_einj_standard",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%D_einj_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: D_einj_standard"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"D_ffull_standard",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%D_ffull_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: D_ffull_standard"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"D_fhalf_standard",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%D_fhalf_standard)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: D_fhalf_standard"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"e0_av",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%e0_av)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: e0_av"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ecrf_src_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%ecrf_src_name)
     status = nf90_get_var(icdf,varid,ss%ecrf_src_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ecrf_src_name"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("ecrf_src_name",ss%ecrf_src_name)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Beam_Elongation",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%EC_Beam_Elongation)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: EC_Beam_Elongation"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%EC_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: EC_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%EC_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: EC_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Half_Power_Angle",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%EC_Half_Power_Angle)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: EC_Half_Power_Angle"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_Omode_Fraction",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%EC_Omode_Fraction)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: EC_Omode_Fraction"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_phi_aim",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%EC_phi_aim)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: EC_phi_aim"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_PRINC_CURV_HOR",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%EC_PRINC_CURV_HOR)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: EC_PRINC_CURV_HOR"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_PRINC_CURV_VERT",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%EC_PRINC_CURV_VERT)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: EC_PRINC_CURV_VERT"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_PRINC_WIDTH_HOR",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%EC_PRINC_WIDTH_HOR)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: EC_PRINC_WIDTH_HOR"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_PRINC_WIDTH_VERT",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%EC_PRINC_WIDTH_VERT)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: EC_PRINC_WIDTH_VERT"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EC_theta_aim",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%EC_theta_aim)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: EC_theta_aim"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ee_mobility_factor",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ee_mobility_factor)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ee_mobility_factor"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Einj_max",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Einj_max)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Einj_max"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Einj_min",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Einj_min)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Einj_min"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eldot",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%eldot)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: eldot"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"elong",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%elong)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: elong"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eperp_beami",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%eperp_beami)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: eperp_beami"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eperp_fusi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%eperp_fusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: eperp_fusi"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eperp_mini",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%eperp_mini)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: eperp_mini"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eperp_rw",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%eperp_rw)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: eperp_rw"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"epll_beami",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%epll_beami)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: epll_beami"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"epll_fusi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%epll_fusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: epll_fusi"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"epll_mini",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%epll_mini)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: epll_mini"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"epll_rw",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%epll_rw)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: epll_rw"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Epot",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Epot)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Epot"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eqdsk_file",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%eqdsk_file)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: eqdsk_file"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eqmom_num",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%eqmom_num)
     status = nf90_get_var(icdf,varid,ss%eqmom_num)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: eqmom_num"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("eqmom_num",ss%eqmom_num)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EQ_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%EQ_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: EQ_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"EQ_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%EQ_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: EQ_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"eta_parallel",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%eta_parallel)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: eta_parallel"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"E_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%E_anom)
     status = nf90_get_var(icdf,varid,ss%E_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: E_anom"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("E_anom",ss%E_anom)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"E_anom2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%E_anom2)
     status = nf90_get_var(icdf,varid,ss%E_anom2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: E_anom2"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("E_anom2",ss%E_anom2)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fidiff_bpass_co",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%fidiff_bpass_co)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: fidiff_bpass_co"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fidiff_bpass_ctr",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%fidiff_bpass_ctr)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: fidiff_bpass_ctr"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fidiff_btrap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%fidiff_btrap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: fidiff_btrap"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fidiff_dpass_co",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%fidiff_dpass_co)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: fidiff_dpass_co"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fidiff_dpass_ctr",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%fidiff_dpass_ctr)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: fidiff_dpass_ctr"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fidiff_dtrap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%fidiff_dtrap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: fidiff_dtrap"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Fi_depletion",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Fi_depletion)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Fi_depletion"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fracmin",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%fracmin)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: fracmin"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"fracpslh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%fracpslh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: fracpslh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"frac_full",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%frac_full)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: frac_full"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"frac_half",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%frac_half)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: frac_half"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_drbm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%freq_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: freq_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_ec",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%freq_ec)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: freq_ec"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_etg",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%freq_etg)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: freq_etg"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_glf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%freq_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: freq_glf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_ic",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%freq_ic)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: freq_ic"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_lh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%freq_lh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: freq_lh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_max_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%freq_max_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: freq_max_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_mmm71",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%freq_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: freq_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_mtm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%freq_mtm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: freq_mtm"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"freq_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%freq_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: freq_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"FUS_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%FUS_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: FUS_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gamma_lmhd",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gamma_lmhd)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gamma_lmhd"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gamma_nc",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gamma_nc)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gamma_nc"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gas_atom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gas_atom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gas_atom"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"GAS_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%GAS_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: GAS_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gb1",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gb1)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gb1"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gb2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gb2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gb2"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gb2i",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gb2i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gb2i"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gbr2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gbr2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gbr2"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"geometry",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%geometry)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: geometry"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Global_label",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Global_label)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Global_label"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gncfb2h",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gncfb2h)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gncfb2h"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gncfh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gncfh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gncfh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gr1",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gr1)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gr1"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gr2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gr2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gr2"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gr2i",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gr2i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gr2i"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gr2rho2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gr2rho2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gr2rho2"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"gr3i",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%gr3i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: gr3i"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"grho1",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%grho1)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: grho1"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

CONTAINS

  subroutine ichk_zero(iarray)
     integer, dimension(:), intent(in) :: iarray

     integer :: ii

     izero=.TRUE.
     do ii=1,size(iarray)
        if(iarray(ii).ne.0) then
           izero=.FALSE.
           exit
        endif
     enddo

     if(.not.izero) then
        prehash1=17
        prehash2=23
        call hashCode(iarray,prehash1,prehash2)
     endif

  end subroutine ichk_zero

  subroutine rchk_zero(rarray)
     real(kind=rspec), dimension(:), intent(in) :: rarray

     integer :: ii

     izero=.TRUE.
     do ii=1,size(rarray)
        if(rarray(ii).ne.0.0_rspec) then
           izero=.FALSE.
           exit
        endif
     enddo

     if(.not.izero) then
        prehash1=17
        prehash2=23
        call hashCode(rarray,prehash1,prehash2)
     endif

  end subroutine rchk_zero

  subroutine cchk_zero(c32array)
     character*32, dimension(:), intent(in) :: c32array

     integer :: ii

     izero=.TRUE.
     do ii=1,size(c32array)
        if(c32array(ii).ne." ") then
           izero=.FALSE.
           exit
        endif
     enddo

     if(.not.izero) then
        prehash1=17
        prehash2=23
        call hashCode(c32array,prehash1,prehash2)
     endif

  end subroutine cchk_zero

  subroutine ichk_hash(itemName,iarray)
     character*(*), intent(in) :: itemName
     integer, dimension(:), intent(in) :: iarray

     posthash1=17
     posthash2=23
     call hashCode(iarray,posthash1,posthash2)

     if((prehash1.ne.posthash1).OR.(prehash2.ne.posthash2)) then
        write(iout,*) ' ?ps_update_file_read: value change detected in dimension quantity: '//trim(itemName)
        iertot = iertot + 1
     endif

  end subroutine ichk_hash

  subroutine rchk_hash(itemName,rarray)
     character*(*), intent(in) :: itemName
     real(kind=rspec), dimension(:), intent(in) :: rarray

     posthash1=17
     posthash2=23
     call hashCode(rarray,posthash1,posthash2)

     if((prehash1.ne.posthash1).OR.(prehash2.ne.posthash2)) then
        write(iout,*) ' ?ps_update_file_read: value change detected in dimension quantity: '//trim(itemName)
        iertot = iertot + 1
     endif

  end subroutine rchk_hash

  subroutine cchk_hash(itemName,c32array)
     character*(*), intent(in) :: itemName
     character*32, dimension(:), intent(in) :: c32array

     posthash1=17
     posthash2=23
     call hashCode(c32array,posthash1,posthash2)

     if((prehash1.ne.posthash1).OR.(prehash2.ne.posthash2)) then
        write(iout,*) ' ?ps_update_file_read: value change detected in dimension quantity: '//trim(itemName)
        iertot = iertot + 1
     endif

  end subroutine cchk_hash

  SUBROUTINE ichk_dim(ival,jval,dname)
     INTEGER, intent(in) :: ival  ! file dimension value
     INTEGER, intent(in) :: jval  ! state dimension value
     CHARACTER*(*), intent(in) :: dname

     IF(ival.ne.jval) THEN
        WRITE(iout,*) ' ?ps_update_file_read: update file dimension not consistent with state:'
        WRITE(iout,*) '  file value:  ',ival
        WRITE(iout,*) '  state value: ',jval
        WRITE(iout,*) '  dimension name: '//trim(dname)
        iertot = iertot + 1
     ENDIF
  END SUBROUTINE ichk_dim

  SUBROUTINE ncerr(status)
     INTEGER, INTENT(in) :: status

     nc_errmsg = NF90_strerror(status)
     write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
  END SUBROUTINE ncerr

  SUBROUTINE denull(str)
     ! convert null characters to blank
     CHARACTER*(*) :: str
     INTEGER :: i

     do i=1,len(str)
        if(ichar(str(i:i)).eq.0) str(i:i)=' '
     enddo
  END SUBROUTINE denull

END SUBROUTINE ps_update_file_read_2
