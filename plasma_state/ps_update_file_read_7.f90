! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_read_7(ss,icdf,iout,iucount,iertot)


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


  status = nf90_inq_varid(icdf,"rho_pel",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_pel)
     status = nf90_get_var(icdf,varid,ss%rho_pel)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_pel"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_pel",ss%rho_pel)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_rad",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_rad)
     status = nf90_get_var(icdf,varid,ss%rho_rad)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_rad"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_rad",ss%rho_rad)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_rw",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_rw)
     status = nf90_get_var(icdf,varid,ss%rho_rw)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_rw"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_rw",ss%rho_rw)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rlim",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rlim)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rlim"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Rloc_coil",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Rloc_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Rloc_coil"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Rmajor_mean",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Rmajor_mean)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Rmajor_mean"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rMinor_mean",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rMinor_mean)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rMinor_mean"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RUNAWAY_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%RUNAWAY_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: RUNAWAY_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RunID",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%RunID)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: RunID"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_ant",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R_ant)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_ant"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_antgeo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R_antgeo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_antgeo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_axis",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R_axis)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_axis"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_EC_launch",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R_EC_launch)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_EC_launch"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_geo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R_geo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_geo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_grid",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%R_grid)
     status = nf90_get_var(icdf,varid,ss%R_grid)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_grid"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("R_grid",ss%R_grid)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_max_box",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R_max_box)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_max_box"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_max_lcfs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R_max_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_max_lcfs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_midp_in",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R_midp_in)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_midp_in"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_midp_out",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R_midp_out)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_midp_out"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_min_box",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R_min_box)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_min_box"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_min_lcfs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R_min_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_min_lcfs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_ripple",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%R_ripple)
     status = nf90_get_var(icdf,varid,ss%R_ripple)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_ripple"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("R_ripple",ss%R_ripple)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_surfMax",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R_surfMax)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_surfMax"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_surfMin",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R_surfMin)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R_surfMin"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"s0reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%s0reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: s0reco"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"s0reco_e",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%s0reco_e)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: s0reco_e"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"s0reco_recap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%s0reco_recap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: s0reco_recap"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sa_index",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sa_index)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sa_index"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SA_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%SA_name)
     status = nf90_get_var(icdf,varid,ss%SA_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: SA_name"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("SA_name",ss%SA_name)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SA_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL ichk_zero(ss%SA_type)
     status = nf90_get_var(icdf,varid,ss%SA_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: SA_type"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL ichk_hash("SA_type",ss%SA_type)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sb0halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sb0halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sb0halo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sb0halo_recap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sb0halo_recap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sb0halo_recap"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sbedep",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sbedep)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sbedep"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sbehalo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sbehalo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sbehalo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sbsce",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sbsce)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sbsce"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sbtherm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sbtherm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sbtherm"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sc0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sc0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sc0"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sc0_to_sgas",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sc0_to_sgas)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sc0_to_sgas"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sc_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sc_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sc_anom"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sfsce",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sfsce)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sfsce"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sftherm",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sftherm)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sftherm"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SFUS_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%SFUS_name)
     status = nf90_get_var(icdf,varid,ss%SFUS_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: SFUS_name"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("SFUS_name",ss%SFUS_name)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sfus_to_all",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sfus_to_all)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sfus_to_all"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sfus_to_alla",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sfus_to_alla)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sfus_to_alla"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SFUS_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL ichk_zero(ss%SFUS_type)
     status = nf90_get_var(icdf,varid,ss%SFUS_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: SFUS_type"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL ichk_hash("SFUS_type",ss%SFUS_type)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SGAS_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%SGAS_name)
     status = nf90_get_var(icdf,varid,ss%SGAS_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: SGAS_name"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("SGAS_name",ss%SGAS_name)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sgas_to_s",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sgas_to_s)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sgas_to_s"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SGAS_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL ichk_zero(ss%SGAS_type)
     status = nf90_get_var(icdf,varid,ss%SGAS_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: SGAS_type"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL ichk_hash("SGAS_type",ss%SGAS_type)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"shot_number",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%shot_number)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: shot_number"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sii_xs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sii_xs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sii_xs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SIMP0_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%SIMP0_name)
     status = nf90_get_var(icdf,varid,ss%SIMP0_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: SIMP0_name"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("SIMP0_name",ss%SIMP0_name)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"simp0_to_s",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%simp0_to_s)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: simp0_to_s"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SIMP0_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL ichk_zero(ss%SIMP0_type)
     status = nf90_get_var(icdf,varid,ss%SIMP0_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: SIMP0_type"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL ichk_hash("SIMP0_type",ss%SIMP0_type)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SIMPI_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%SIMPI_name)
     status = nf90_get_var(icdf,varid,ss%SIMPI_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: SIMPI_name"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("SIMPI_name",ss%SIMPI_name)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"simpi_to_s",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%simpi_to_s)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: simpi_to_s"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SIMPI_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL ichk_zero(ss%SIMPI_type)
     status = nf90_get_var(icdf,varid,ss%SIMPI_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: SIMPI_type"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL ichk_hash("SIMPI_type",ss%SIMPI_type)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SNBI_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%SNBI_name)
     status = nf90_get_var(icdf,varid,ss%SNBI_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: SNBI_name"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("SNBI_name",ss%SNBI_name)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"snbi_to_all",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%snbi_to_all)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: snbi_to_all"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"snbi_to_alla",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%snbi_to_alla)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: snbi_to_alla"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"SNBI_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL ichk_zero(ss%SNBI_type)
     status = nf90_get_var(icdf,varid,ss%SNBI_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: SNBI_type"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL ichk_hash("SNBI_type",ss%SNBI_type)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sn_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sn_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sn_trans"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sn_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sn_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sn_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sprof0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sprof0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sprof0"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sprof0e",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sprof0e)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sprof0e"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"spsce",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%spsce)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: spsce"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"squareLO",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%squareLO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: squareLO"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"squareUO",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%squareUO)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: squareUO"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"sRtcen",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%sRtcen)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: sRtcen"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"surf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%surf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: surf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"S_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%S_name)
     status = nf90_get_var(icdf,varid,ss%S_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: S_name"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("S_name",ss%S_name)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"s_to_sa",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%s_to_sa)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: s_to_sa"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"S_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL ichk_zero(ss%S_type)
     status = nf90_get_var(icdf,varid,ss%S_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: S_type"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL ichk_hash("S_type",ss%S_type)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"t0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%t0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: t0"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"T0cx",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%T0cx)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: T0cx"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"T0sc0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%T0sc0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: T0sc0"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"T0_halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%T0_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: T0_halo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"T0_reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%T0_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: T0_reco"
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

END SUBROUTINE ps_update_file_read_7
