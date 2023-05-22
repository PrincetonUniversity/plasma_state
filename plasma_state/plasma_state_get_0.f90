! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_get_0(ss,icdf,version_change,iout,iertot)


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

  IF(ALLOCATED(ss%alla_index)) THEN
     status = nf90_inq_varid(icdf,"alla_index",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"alla_index")
     ELSE
        status = nf90_get_var(icdf,varid,ss%alla_index)
        IF(status.NE.NF90_NOERR) THEN
           ss%alla_index = 0
           CALL ncerr_name(2,status,"alla_index")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ALLA_name)) THEN
     status = nf90_inq_varid(icdf,"ALLA_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ALLA_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ALLA_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%ALLA_name = " "
           CALL ncerr_name(2,status,"ALLA_name")
        ELSE
           CALL denull_vec(ss%ALLA_name,ss%nspec_alla+1)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ALLA_type)) THEN
     status = nf90_inq_varid(icdf,"ALLA_type",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ALLA_type")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ALLA_type)
        IF(status.NE.NF90_NOERR) THEN
           ss%ALLA_type = 0
           CALL ncerr_name(2,status,"ALLA_type")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%all_index)) THEN
     status = nf90_inq_varid(icdf,"all_index",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"all_index")
     ELSE
        status = nf90_get_var(icdf,varid,ss%all_index)
        IF(status.NE.NF90_NOERR) THEN
           ss%all_index = 0
           CALL ncerr_name(2,status,"all_index")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ALL_name)) THEN
     status = nf90_inq_varid(icdf,"ALL_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ALL_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ALL_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%ALL_name = " "
           CALL ncerr_name(2,status,"ALL_name")
        ELSE
           CALL denull_vec(ss%ALL_name,ss%nspec_all+1)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ALL_type)) THEN
     status = nf90_inq_varid(icdf,"ALL_type",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ALL_type")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ALL_type)
        IF(status.NE.NF90_NOERR) THEN
           ss%ALL_type = 0
           CALL ncerr_name(2,status,"ALL_type")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%amaxlh)) THEN
     status = nf90_inq_varid(icdf,"amaxlh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"amaxlh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%amaxlh)
        IF(status.NE.NF90_NOERR) THEN
           ss%amaxlh = 0.0_rspec
           CALL ncerr_name(2,status,"amaxlh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%aminlh)) THEN
     status = nf90_inq_varid(icdf,"aminlh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"aminlh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%aminlh)
        IF(status.NE.NF90_NOERR) THEN
           ss%aminlh = 0.0_rspec
           CALL ncerr_name(2,status,"aminlh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Ang_Href_coil)) THEN
     status = nf90_inq_varid(icdf,"Ang_Href_coil",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Ang_Href_coil")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Ang_Href_coil)
        IF(status.NE.NF90_NOERR) THEN
           ss%Ang_Href_coil = 0.0_rspec
           CALL ncerr_name(2,status,"Ang_Href_coil")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Ang_Vref_coil)) THEN
     status = nf90_inq_varid(icdf,"Ang_Vref_coil",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Ang_Vref_coil")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Ang_Vref_coil)
        IF(status.NE.NF90_NOERR) THEN
           ss%Ang_Vref_coil = 0.0_rspec
           CALL ncerr_name(2,status,"Ang_Vref_coil")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ANOM_Code_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"ANOM_Code_Info")
  ELSE
     ilen = len(ss%ANOM_Code_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%ANOM_Code_Info = " "
     status = nf90_get_var(icdf,varid,ss%ANOM_Code_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%ANOM_Code_Info = " "
        CALL ncerr_name(2,status,"ANOM_Code_Info")
     ELSE
        CALL denull(ss%ANOM_Code_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%anom_evar)) THEN
     status = nf90_inq_varid(icdf,"anom_evar",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"anom_evar")
     ELSE
        status = nf90_get_var(icdf,varid,ss%anom_evar)
        IF(status.NE.NF90_NOERR) THEN
           ss%anom_evar = 0.0_rspec
           CALL ncerr_name(2,status,"anom_evar")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ant_model)) THEN
     status = nf90_inq_varid(icdf,"ant_model",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ant_model")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ant_model)
        IF(status.NE.NF90_NOERR) THEN
           ss%ant_model = " "
           CALL ncerr_name(2,status,"ant_model")
        ELSE
           CALL denull_vec(ss%ant_model,ss%nicrf_src)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap2_halfheight)) THEN
     status = nf90_inq_varid(icdf,"ap2_halfheight",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ap2_halfheight")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ap2_halfheight)
        IF(status.NE.NF90_NOERR) THEN
           ss%ap2_halfheight = 0.0_rspec
           CALL ncerr_name(2,status,"ap2_halfheight")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap2_halfwidth)) THEN
     status = nf90_inq_varid(icdf,"ap2_halfwidth",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ap2_halfwidth")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ap2_halfwidth)
        IF(status.NE.NF90_NOERR) THEN
           ss%ap2_halfwidth = 0.0_rspec
           CALL ncerr_name(2,status,"ap2_halfwidth")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap2_horiz_offset)) THEN
     status = nf90_inq_varid(icdf,"ap2_horiz_offset",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ap2_horiz_offset")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ap2_horiz_offset)
        IF(status.NE.NF90_NOERR) THEN
           ss%ap2_horiz_offset = 0.0_rspec
           CALL ncerr_name(2,status,"ap2_horiz_offset")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap2_vert_offset)) THEN
     status = nf90_inq_varid(icdf,"ap2_vert_offset",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ap2_vert_offset")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ap2_vert_offset)
        IF(status.NE.NF90_NOERR) THEN
           ss%ap2_vert_offset = 0.0_rspec
           CALL ncerr_name(2,status,"ap2_vert_offset")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%apolmaxlh)) THEN
     status = nf90_inq_varid(icdf,"apolmaxlh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"apolmaxlh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%apolmaxlh)
        IF(status.NE.NF90_NOERR) THEN
           ss%apolmaxlh = 0.0_rspec
           CALL ncerr_name(2,status,"apolmaxlh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%apolminlh)) THEN
     status = nf90_inq_varid(icdf,"apolminlh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"apolminlh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%apolminlh)
        IF(status.NE.NF90_NOERR) THEN
           ss%apolminlh = 0.0_rspec
           CALL ncerr_name(2,status,"apolminlh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap_halfheight)) THEN
     status = nf90_inq_varid(icdf,"ap_halfheight",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ap_halfheight")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ap_halfheight)
        IF(status.NE.NF90_NOERR) THEN
           ss%ap_halfheight = 0.0_rspec
           CALL ncerr_name(2,status,"ap_halfheight")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap_halfwidth)) THEN
     status = nf90_inq_varid(icdf,"ap_halfwidth",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ap_halfwidth")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ap_halfwidth)
        IF(status.NE.NF90_NOERR) THEN
           ss%ap_halfwidth = 0.0_rspec
           CALL ncerr_name(2,status,"ap_halfwidth")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap_horiz_offset)) THEN
     status = nf90_inq_varid(icdf,"ap_horiz_offset",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ap_horiz_offset")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ap_horiz_offset)
        IF(status.NE.NF90_NOERR) THEN
           ss%ap_horiz_offset = 0.0_rspec
           CALL ncerr_name(2,status,"ap_horiz_offset")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ap_vert_offset)) THEN
     status = nf90_inq_varid(icdf,"ap_vert_offset",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ap_vert_offset")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ap_vert_offset)
        IF(status.NE.NF90_NOERR) THEN
           ss%ap_vert_offset = 0.0_rspec
           CALL ncerr_name(2,status,"ap_vert_offset")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%area)) THEN
     status = nf90_inq_varid(icdf,"area",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"area")
     ELSE
        status = nf90_get_var(icdf,varid,ss%area)
        IF(status.NE.NF90_NOERR) THEN
           ss%area = 0.0_rspec
           CALL ncerr_name(2,status,"area")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%atormaxlh)) THEN
     status = nf90_inq_varid(icdf,"atormaxlh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"atormaxlh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%atormaxlh)
        IF(status.NE.NF90_NOERR) THEN
           ss%atormaxlh = 0.0_rspec
           CALL ncerr_name(2,status,"atormaxlh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%atorminlh)) THEN
     status = nf90_inq_varid(icdf,"atorminlh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"atorminlh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%atorminlh)
        IF(status.NE.NF90_NOERR) THEN
           ss%atorminlh = 0.0_rspec
           CALL ncerr_name(2,status,"atorminlh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Balloon_code)) THEN
     status = nf90_inq_varid(icdf,"Balloon_code",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Balloon_code")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Balloon_code)
        IF(status.NE.NF90_NOERR) THEN
           ss%Balloon_code = " "
           CALL ncerr_name(2,status,"Balloon_code")
        ELSE
           CALL denull_vec(ss%Balloon_code,ss%nballoon)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Balloon_mapper)) THEN
     status = nf90_inq_varid(icdf,"Balloon_mapper",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Balloon_mapper")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Balloon_mapper)
        IF(status.NE.NF90_NOERR) THEN
           ss%Balloon_mapper = " "
           CALL ncerr_name(2,status,"Balloon_mapper")
        ELSE
           CALL denull_vec(ss%Balloon_mapper,ss%nballoon)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Balloon_stable)) THEN
     status = nf90_inq_varid(icdf,"Balloon_stable",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Balloon_stable")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Balloon_stable)
        IF(status.NE.NF90_NOERR) THEN
           ss%Balloon_stable = 0
           CALL ncerr_name(2,status,"Balloon_stable")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Balloon_status)) THEN
     status = nf90_inq_varid(icdf,"Balloon_status",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Balloon_status")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Balloon_status)
        IF(status.NE.NF90_NOERR) THEN
           ss%Balloon_status = 0
           CALL ncerr_name(2,status,"Balloon_status")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%balstab)) THEN
     status = nf90_inq_varid(icdf,"balstab",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"balstab")
     ELSE
        status = nf90_get_var(icdf,varid,ss%balstab)
        IF(status.NE.NF90_NOERR) THEN
           ss%balstab = 0.0_rspec
           CALL ncerr_name(2,status,"balstab")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%beam_type)) THEN
     status = nf90_inq_varid(icdf,"beam_type",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"beam_type")
     ELSE
        status = nf90_get_var(icdf,varid,ss%beam_type)
        IF(status.NE.NF90_NOERR) THEN
           ss%beam_type = " "
           CALL ncerr_name(2,status,"beam_type")
        ELSE
           CALL denull_vec(ss%beam_type,ss%nbeam)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%bidiff_bpass_co)) THEN
     status = nf90_inq_varid(icdf,"bidiff_bpass_co",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"bidiff_bpass_co")
     ELSE
        status = nf90_get_var(icdf,varid,ss%bidiff_bpass_co)
        IF(status.NE.NF90_NOERR) THEN
           ss%bidiff_bpass_co = 0.0_rspec
           CALL ncerr_name(2,status,"bidiff_bpass_co")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%bidiff_bpass_ctr)) THEN
     status = nf90_inq_varid(icdf,"bidiff_bpass_ctr",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"bidiff_bpass_ctr")
     ELSE
        status = nf90_get_var(icdf,varid,ss%bidiff_bpass_ctr)
        IF(status.NE.NF90_NOERR) THEN
           ss%bidiff_bpass_ctr = 0.0_rspec
           CALL ncerr_name(2,status,"bidiff_bpass_ctr")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%bidiff_btrap)) THEN
     status = nf90_inq_varid(icdf,"bidiff_btrap",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"bidiff_btrap")
     ELSE
        status = nf90_get_var(icdf,varid,ss%bidiff_btrap)
        IF(status.NE.NF90_NOERR) THEN
           ss%bidiff_btrap = 0.0_rspec
           CALL ncerr_name(2,status,"bidiff_btrap")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%bidiff_dpass_co)) THEN
     status = nf90_inq_varid(icdf,"bidiff_dpass_co",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"bidiff_dpass_co")
     ELSE
        status = nf90_get_var(icdf,varid,ss%bidiff_dpass_co)
        IF(status.NE.NF90_NOERR) THEN
           ss%bidiff_dpass_co = 0.0_rspec
           CALL ncerr_name(2,status,"bidiff_dpass_co")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%bidiff_dpass_ctr)) THEN
     status = nf90_inq_varid(icdf,"bidiff_dpass_ctr",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"bidiff_dpass_ctr")
     ELSE
        status = nf90_get_var(icdf,varid,ss%bidiff_dpass_ctr)
        IF(status.NE.NF90_NOERR) THEN
           ss%bidiff_dpass_ctr = 0.0_rspec
           CALL ncerr_name(2,status,"bidiff_dpass_ctr")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%bidiff_dtrap)) THEN
     status = nf90_inq_varid(icdf,"bidiff_dtrap",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"bidiff_dtrap")
     ELSE
        status = nf90_get_var(icdf,varid,ss%bidiff_dtrap)
        IF(status.NE.NF90_NOERR) THEN
           ss%bidiff_dtrap = 0.0_rspec
           CALL ncerr_name(2,status,"bidiff_dtrap")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%BphiRZ)) THEN
     status = nf90_inq_varid(icdf,"BphiRZ",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"BphiRZ")
     ELSE
        status = nf90_get_var(icdf,varid,ss%BphiRZ)
        IF(status.NE.NF90_NOERR) THEN
           ss%BphiRZ = 0.0_rspec
           CALL ncerr_name(2,status,"BphiRZ")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%BRRZ)) THEN
     status = nf90_inq_varid(icdf,"BRRZ",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"BRRZ")
     ELSE
        status = nf90_get_var(icdf,varid,ss%BRRZ)
        IF(status.NE.NF90_NOERR) THEN
           ss%BRRZ = 0.0_rspec
           CALL ncerr_name(2,status,"BRRZ")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%BZRZ)) THEN
     status = nf90_inq_varid(icdf,"BZRZ",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"BZRZ")
     ELSE
        status = nf90_get_var(icdf,varid,ss%BZRZ)
        IF(status.NE.NF90_NOERR) THEN
           ss%BZRZ = 0.0_rspec
           CALL ncerr_name(2,status,"BZRZ")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_axis",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"B_axis")
  ELSE
     status = nf90_get_var(icdf,varid,ss%B_axis)
     IF(status.NE.NF90_NOERR) THEN
        ss%B_axis = 0.0_rspec
        CALL ncerr_name(2,status,"B_axis")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_axis_vac",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"B_axis_vac")
  ELSE
     status = nf90_get_var(icdf,varid,ss%B_axis_vac)
     IF(status.NE.NF90_NOERR) THEN
        ss%B_axis_vac = 0.0_rspec
        CALL ncerr_name(2,status,"B_axis_vac")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%b_halfHeight)) THEN
     status = nf90_inq_varid(icdf,"b_halfHeight",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"b_halfHeight")
     ELSE
        status = nf90_get_var(icdf,varid,ss%b_halfHeight)
        IF(status.NE.NF90_NOERR) THEN
           ss%b_halfHeight = 0.0_rspec
           CALL ncerr_name(2,status,"b_halfHeight")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%b_halfwidth)) THEN
     status = nf90_inq_varid(icdf,"b_halfwidth",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"b_halfwidth")
     ELSE
        status = nf90_get_var(icdf,varid,ss%b_halfwidth)
        IF(status.NE.NF90_NOERR) THEN
           ss%b_halfwidth = 0.0_rspec
           CALL ncerr_name(2,status,"b_halfwidth")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%b_Hdivergence)) THEN
     status = nf90_inq_varid(icdf,"b_Hdivergence",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"b_Hdivergence")
     ELSE
        status = nf90_get_var(icdf,varid,ss%b_Hdivergence)
        IF(status.NE.NF90_NOERR) THEN
           ss%b_Hdivergence = 0.0_rspec
           CALL ncerr_name(2,status,"b_Hdivergence")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%b_Hfocal_length)) THEN
     status = nf90_inq_varid(icdf,"b_Hfocal_length",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"b_Hfocal_length")
     ELSE
        status = nf90_get_var(icdf,varid,ss%b_Hfocal_length)
        IF(status.NE.NF90_NOERR) THEN
           ss%b_Hfocal_length = 0.0_rspec
           CALL ncerr_name(2,status,"b_Hfocal_length")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_max_lcfs",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"B_max_lcfs")
  ELSE
     status = nf90_get_var(icdf,varid,ss%B_max_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        ss%B_max_lcfs = 0.0_rspec
        CALL ncerr_name(2,status,"B_max_lcfs")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_min_lcfs",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"B_min_lcfs")
  ELSE
     status = nf90_get_var(icdf,varid,ss%B_min_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        ss%B_min_lcfs = 0.0_rspec
        CALL ncerr_name(2,status,"B_min_lcfs")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%B_surfMax)) THEN
     status = nf90_inq_varid(icdf,"B_surfMax",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"B_surfMax")
     ELSE
        status = nf90_get_var(icdf,varid,ss%B_surfMax)
        IF(status.NE.NF90_NOERR) THEN
           ss%B_surfMax = 0.0_rspec
           CALL ncerr_name(2,status,"B_surfMax")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%B_surfMin)) THEN
     status = nf90_inq_varid(icdf,"B_surfMin",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"B_surfMin")
     ELSE
        status = nf90_get_var(icdf,varid,ss%B_surfMin)
        IF(status.NE.NF90_NOERR) THEN
           ss%B_surfMin = 0.0_rspec
           CALL ncerr_name(2,status,"B_surfMin")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%b_Vdivergence)) THEN
     status = nf90_inq_varid(icdf,"b_Vdivergence",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"b_Vdivergence")
     ELSE
        status = nf90_get_var(icdf,varid,ss%b_Vdivergence)
        IF(status.NE.NF90_NOERR) THEN
           ss%b_Vdivergence = 0.0_rspec
           CALL ncerr_name(2,status,"b_Vdivergence")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%b_Vfocal_length)) THEN
     status = nf90_inq_varid(icdf,"b_Vfocal_length",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"b_Vfocal_length")
     ELSE
        status = nf90_get_var(icdf,varid,ss%b_Vfocal_length)
        IF(status.NE.NF90_NOERR) THEN
           ss%b_Vfocal_length = 0.0_rspec
           CALL ncerr_name(2,status,"b_Vfocal_length")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%cdicrf)) THEN
     status = nf90_inq_varid(icdf,"cdicrf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"cdicrf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%cdicrf)
        IF(status.NE.NF90_NOERR) THEN
           ss%cdicrf = 0.0_rspec
           CALL ncerr_name(2,status,"cdicrf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%cdicrf_nphi)) THEN
     status = nf90_inq_varid(icdf,"cdicrf_nphi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"cdicrf_nphi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%cdicrf_nphi)
        IF(status.NE.NF90_NOERR) THEN
           ss%cdicrf_nphi = 0.0_rspec
           CALL ncerr_name(2,status,"cdicrf_nphi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_anom)) THEN
     status = nf90_inq_varid(icdf,"chie_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_anom = 0.0_rspec
           CALL ncerr_name(2,status,"chie_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_drbm)) THEN
     status = nf90_inq_varid(icdf,"chie_drbm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_drbm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_drbm)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_drbm = 0.0_rspec
           CALL ncerr_name(2,status,"chie_drbm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_etg)) THEN
     status = nf90_inq_varid(icdf,"chie_etg",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_etg")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_etg)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_etg = 0.0_rspec
           CALL ncerr_name(2,status,"chie_etg")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_glf)) THEN
     status = nf90_inq_varid(icdf,"chie_glf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_glf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_glf)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_glf = 0.0_rspec
           CALL ncerr_name(2,status,"chie_glf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"chie_gtcneo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_gtcneo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_gtcneo)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_gtcneo = 0.0_rspec
           CALL ncerr_name(2,status,"chie_gtcneo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_mmm71)) THEN
     status = nf90_inq_varid(icdf,"chie_mmm71",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_mmm71")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_mmm71)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_mmm71 = 0.0_rspec
           CALL ncerr_name(2,status,"chie_mmm71")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_mtm)) THEN
     status = nf90_inq_varid(icdf,"chie_mtm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_mtm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_mtm)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_mtm = 0.0_rspec
           CALL ncerr_name(2,status,"chie_mtm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_nclass)) THEN
     status = nf90_inq_varid(icdf,"chie_nclass",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_nclass")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_nclass)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_nclass = 0.0_rspec
           CALL ncerr_name(2,status,"chie_nclass")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_neo)) THEN
     status = nf90_inq_varid(icdf,"chie_neo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_neo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_neo)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_neo = 0.0_rspec
           CALL ncerr_name(2,status,"chie_neo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_ntm)) THEN
     status = nf90_inq_varid(icdf,"chie_ntm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_ntm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_ntm)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_ntm = 0.0_rspec
           CALL ncerr_name(2,status,"chie_ntm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_paleo)) THEN
     status = nf90_inq_varid(icdf,"chie_paleo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_paleo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_paleo)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_paleo = 0.0_rspec
           CALL ncerr_name(2,status,"chie_paleo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_tglf)) THEN
     status = nf90_inq_varid(icdf,"chie_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"chie_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_trans)) THEN
     status = nf90_inq_varid(icdf,"chie_trans",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_trans")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_trans)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_trans = 0.0_rspec
           CALL ncerr_name(2,status,"chie_trans")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_transcat)) THEN
     status = nf90_inq_varid(icdf,"chie_transcat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_transcat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_transcat)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_transcat = 0.0_rspec
           CALL ncerr_name(2,status,"chie_transcat")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chie_w19)) THEN
     status = nf90_inq_varid(icdf,"chie_w19",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chie_w19")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chie_w19)
        IF(status.NE.NF90_NOERR) THEN
           ss%chie_w19 = 0.0_rspec
           CALL ncerr_name(2,status,"chie_w19")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_anom)) THEN
     status = nf90_inq_varid(icdf,"chii_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chii_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chii_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%chii_anom = 0.0_rspec
           CALL ncerr_name(2,status,"chii_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_drbm)) THEN
     status = nf90_inq_varid(icdf,"chii_drbm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chii_drbm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chii_drbm)
        IF(status.NE.NF90_NOERR) THEN
           ss%chii_drbm = 0.0_rspec
           CALL ncerr_name(2,status,"chii_drbm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_glf)) THEN
     status = nf90_inq_varid(icdf,"chii_glf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chii_glf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chii_glf)
        IF(status.NE.NF90_NOERR) THEN
           ss%chii_glf = 0.0_rspec
           CALL ncerr_name(2,status,"chii_glf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"chii_gtcneo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chii_gtcneo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chii_gtcneo)
        IF(status.NE.NF90_NOERR) THEN
           ss%chii_gtcneo = 0.0_rspec
           CALL ncerr_name(2,status,"chii_gtcneo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_mmm71)) THEN
     status = nf90_inq_varid(icdf,"chii_mmm71",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chii_mmm71")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chii_mmm71)
        IF(status.NE.NF90_NOERR) THEN
           ss%chii_mmm71 = 0.0_rspec
           CALL ncerr_name(2,status,"chii_mmm71")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_nclass)) THEN
     status = nf90_inq_varid(icdf,"chii_nclass",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chii_nclass")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chii_nclass)
        IF(status.NE.NF90_NOERR) THEN
           ss%chii_nclass = 0.0_rspec
           CALL ncerr_name(2,status,"chii_nclass")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%chii_neo)) THEN
     status = nf90_inq_varid(icdf,"chii_neo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"chii_neo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%chii_neo)
        IF(status.NE.NF90_NOERR) THEN
           ss%chii_neo = 0.0_rspec
           CALL ncerr_name(2,status,"chii_neo")
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

END SUBROUTINE plasma_state_get_0
