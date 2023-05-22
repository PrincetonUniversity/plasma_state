! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_read_0(ss,icdf,iout,iucount,iertot)


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


  status = nf90_inq_varid(icdf,"alla_index",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%alla_index)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: alla_index"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ALLA_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%ALLA_name)
     status = nf90_get_var(icdf,varid,ss%ALLA_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ALLA_name"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("ALLA_name",ss%ALLA_name)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ALLA_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL ichk_zero(ss%ALLA_type)
     status = nf90_get_var(icdf,varid,ss%ALLA_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ALLA_type"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL ichk_hash("ALLA_type",ss%ALLA_type)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"all_index",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%all_index)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: all_index"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ALL_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%ALL_name)
     status = nf90_get_var(icdf,varid,ss%ALL_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ALL_name"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("ALL_name",ss%ALL_name)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ALL_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL ichk_zero(ss%ALL_type)
     status = nf90_get_var(icdf,varid,ss%ALL_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ALL_type"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL ichk_hash("ALL_type",ss%ALL_type)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"amaxlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%amaxlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: amaxlh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"aminlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%aminlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: aminlh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Ang_Href_coil",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Ang_Href_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Ang_Href_coil"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Ang_Vref_coil",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Ang_Vref_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Ang_Vref_coil"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ANOM_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ANOM_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ANOM_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"anom_evar",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%anom_evar)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: anom_evar"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ant_model",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ant_model)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ant_model"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap2_halfheight",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ap2_halfheight)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ap2_halfheight"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap2_halfwidth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ap2_halfwidth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ap2_halfwidth"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap2_horiz_offset",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ap2_horiz_offset)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ap2_horiz_offset"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap2_vert_offset",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ap2_vert_offset)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ap2_vert_offset"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"apolmaxlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%apolmaxlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: apolmaxlh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"apolminlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%apolminlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: apolminlh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap_halfheight",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ap_halfheight)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ap_halfheight"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap_halfwidth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ap_halfwidth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ap_halfwidth"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap_horiz_offset",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ap_horiz_offset)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ap_horiz_offset"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ap_vert_offset",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ap_vert_offset)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ap_vert_offset"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"area",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%area)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: area"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"atormaxlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%atormaxlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: atormaxlh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"atorminlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%atorminlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: atorminlh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Balloon_code",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%Balloon_code)
     status = nf90_get_var(icdf,varid,ss%Balloon_code)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Balloon_code"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("Balloon_code",ss%Balloon_code)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Balloon_mapper",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Balloon_mapper)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Balloon_mapper"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Balloon_stable",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Balloon_stable)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Balloon_stable"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Balloon_status",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Balloon_status)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Balloon_status"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"balstab",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%balstab)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: balstab"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"beam_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%beam_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: beam_type"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"bidiff_bpass_co",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%bidiff_bpass_co)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: bidiff_bpass_co"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"bidiff_bpass_ctr",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%bidiff_bpass_ctr)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: bidiff_bpass_ctr"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"bidiff_btrap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%bidiff_btrap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: bidiff_btrap"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"bidiff_dpass_co",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%bidiff_dpass_co)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: bidiff_dpass_co"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"bidiff_dpass_ctr",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%bidiff_dpass_ctr)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: bidiff_dpass_ctr"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"bidiff_dtrap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%bidiff_dtrap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: bidiff_dtrap"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"BphiRZ",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%BphiRZ)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: BphiRZ"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"BRRZ",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%BRRZ)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: BRRZ"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"BZRZ",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%BZRZ)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: BZRZ"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_axis",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%B_axis)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: B_axis"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_axis_vac",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%B_axis_vac)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: B_axis_vac"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"b_halfHeight",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%b_halfHeight)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: b_halfHeight"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"b_halfwidth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%b_halfwidth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: b_halfwidth"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"b_Hdivergence",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%b_Hdivergence)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: b_Hdivergence"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"b_Hfocal_length",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%b_Hfocal_length)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: b_Hfocal_length"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_max_lcfs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%B_max_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: B_max_lcfs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_min_lcfs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%B_min_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: B_min_lcfs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_surfMax",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%B_surfMax)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: B_surfMax"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"B_surfMin",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%B_surfMin)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: B_surfMin"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"b_Vdivergence",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%b_Vdivergence)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: b_Vdivergence"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"b_Vfocal_length",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%b_Vfocal_length)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: b_Vfocal_length"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"cdicrf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%cdicrf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: cdicrf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"cdicrf_nphi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%cdicrf_nphi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: cdicrf_nphi"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_anom"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_drbm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_etg",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_etg)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_etg"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_glf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_glf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_gtcneo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_mmm71",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_mtm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_mtm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_mtm"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_nclass",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_neo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_neo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_ntm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_ntm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_ntm"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_paleo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_paleo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_paleo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_trans"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chie_w19",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chie_w19)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chie_w19"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chii_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chii_anom"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_drbm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chii_drbm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chii_drbm"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_glf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chii_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chii_glf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_gtcneo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chii_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chii_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_mmm71",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chii_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chii_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_nclass",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chii_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chii_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"chii_neo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%chii_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: chii_neo"
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

END SUBROUTINE ps_update_file_read_0
