! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_read_5(ss,icdf,iout,iucount,iertot)


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


  status = nf90_inq_varid(icdf,"ns_bdy",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ns_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ns_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ns_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ns_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ns_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ns_is_input",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ns_is_input)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ns_is_input"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ntf_coil2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ntf_coil2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ntf_coil2"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ntf_coils",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ntf_coils)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ntf_coils"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nthinlh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%nthinlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: nthinlh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Nturns",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Nturns)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Nturns"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"num_nphi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%num_nphi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: num_nphi"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"num_nphi_vac",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%num_nphi_vac)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: num_nphi_vac"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nzflux_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%nzflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: nzflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nz_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%nz_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: nz_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"n_straps",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%n_straps)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: n_straps"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omeg0cx",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%omeg0cx)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: omeg0cx"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omeg0sc0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%omeg0sc0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: omeg0sc0"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omeg0_halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%omeg0_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: omeg0_halo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omeg0_reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%omeg0_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: omeg0_reco"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omegat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%omegat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: omegat"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omegat_bdy",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%omegat_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: omegat_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omega_exb",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%omega_exb)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: omega_exb"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"p0_reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%p0_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: p0_reco"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pb0_halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pb0_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pb0_halo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pbe",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pbe)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pbe"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pbi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pbi"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pbth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pbth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pbth"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pcx_halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pcx_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pcx_halo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pcx_reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pcx_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pcx_reco"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"peech",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%peech)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: peech"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"peech_src",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%peech_src)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: peech_src"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pelh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pelh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pelh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pelh_src",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pelh_src)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pelh_src"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"PEL_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%PEL_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: PEL_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"PEL_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%PEL_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: PEL_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pe_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pe_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pe_anom"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pe_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pe_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pe_trans"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pe_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pe_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pe_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pfuse",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pfuse)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pfuse"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pfusi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pfusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pfusi"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pfusth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pfusth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pfusth"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Phibsc",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Phibsc)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Phibsc"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"phiflux_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%phiflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: phiflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"phit",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%phit)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: phit"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Phi_EC_launch",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Phi_EC_launch)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Phi_EC_launch"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"picrf_abs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%picrf_abs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: picrf_abs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"picrf_ext",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%picrf_ext)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: picrf_ext"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"picrf_nphi_srcs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%picrf_nphi_srcs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: picrf_nphi_srcs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"picrf_srcs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%picrf_srcs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: picrf_srcs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"picrf_totals",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%picrf_totals)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: picrf_totals"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"picth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%picth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: picth"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pilh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pilh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pilh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pilh_src",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pilh_src)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pilh_src"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pi_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pi_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pi_anom"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pi_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pi_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pi_trans"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pi_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pi_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pi_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"PLASMA_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%PLASMA_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: PLASMA_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pmine",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pmine)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pmine"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pmini",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pmini)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pmini"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pminth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pminth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pminth"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pohme",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pohme)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pohme"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"pot_ky_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%pot_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: pot_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"power_ec",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%power_ec)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: power_ec"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"power_ic",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%power_ic)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: power_ic"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"power_lh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%power_lh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: power_lh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"power_nbi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%power_nbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: power_nbi"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"power_nbi_trace",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%power_nbi_trace)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: power_nbi_trace"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"prad",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%prad)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: prad"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"prad_br",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%prad_br)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: prad_br"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"prad_cy",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%prad_cy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: prad_cy"
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

END SUBROUTINE ps_update_file_read_5
