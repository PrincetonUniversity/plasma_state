! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_get_5(ss,icdf,version_change,iout,iertot)


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

  IF(ALLOCATED(ss%ns_bdy)) THEN
     status = nf90_inq_varid(icdf,"ns_bdy",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ns_bdy")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ns_bdy)
        IF(status.NE.NF90_NOERR) THEN
           ss%ns_bdy = 0.0_rspec
           CALL ncerr_name(2,status,"ns_bdy")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ns_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"ns_Data_Info")
  ELSE
     ilen = len(ss%ns_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%ns_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%ns_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%ns_Data_Info = " "
        CALL ncerr_name(2,status,"ns_Data_Info")
     ELSE
        CALL denull(ss%ns_Data_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ns_is_input)) THEN
     status = nf90_inq_varid(icdf,"ns_is_input",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ns_is_input")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ns_is_input)
        IF(status.NE.NF90_NOERR) THEN
           ss%ns_is_input = 0
           CALL ncerr_name(2,status,"ns_is_input")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ntf_coil2",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"ntf_coil2")
  ELSE
     status = nf90_get_var(icdf,varid,ss%ntf_coil2)
     IF(status.NE.NF90_NOERR) THEN
        ss%ntf_coil2 = ps_unInit
        CALL ncerr_name(2,status,"ntf_coil2")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ntf_coils",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"ntf_coils")
  ELSE
     status = nf90_get_var(icdf,varid,ss%ntf_coils)
     IF(status.NE.NF90_NOERR) THEN
        ss%ntf_coils = ps_unInit
        CALL ncerr_name(2,status,"ntf_coils")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nthinlh)) THEN
     status = nf90_inq_varid(icdf,"nthinlh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nthinlh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nthinlh)
        IF(status.NE.NF90_NOERR) THEN
           ss%nthinlh = 0
           CALL ncerr_name(2,status,"nthinlh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Nturns)) THEN
     status = nf90_inq_varid(icdf,"Nturns",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Nturns")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Nturns)
        IF(status.NE.NF90_NOERR) THEN
           ss%Nturns = 0
           CALL ncerr_name(2,status,"Nturns")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%num_nphi)) THEN
     status = nf90_inq_varid(icdf,"num_nphi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"num_nphi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%num_nphi)
        IF(status.NE.NF90_NOERR) THEN
           ss%num_nphi = 0
           CALL ncerr_name(2,status,"num_nphi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%num_nphi_vac)) THEN
     status = nf90_inq_varid(icdf,"num_nphi_vac",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"num_nphi_vac")
     ELSE
        status = nf90_get_var(icdf,varid,ss%num_nphi_vac)
        IF(status.NE.NF90_NOERR) THEN
           ss%num_nphi_vac = 0
           CALL ncerr_name(2,status,"num_nphi_vac")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nzflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"nzflux_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nzflux_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nzflux_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%nzflux_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"nzflux_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nz_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"nz_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nz_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nz_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%nz_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"nz_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%n_straps)) THEN
     status = nf90_inq_varid(icdf,"n_straps",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"n_straps")
     ELSE
        status = nf90_get_var(icdf,varid,ss%n_straps)
        IF(status.NE.NF90_NOERR) THEN
           ss%n_straps = 0
           CALL ncerr_name(2,status,"n_straps")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%omeg0cx)) THEN
     status = nf90_inq_varid(icdf,"omeg0cx",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"omeg0cx")
     ELSE
        status = nf90_get_var(icdf,varid,ss%omeg0cx)
        IF(status.NE.NF90_NOERR) THEN
           ss%omeg0cx = 0.0_rspec
           CALL ncerr_name(2,status,"omeg0cx")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%omeg0sc0)) THEN
     status = nf90_inq_varid(icdf,"omeg0sc0",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"omeg0sc0")
     ELSE
        status = nf90_get_var(icdf,varid,ss%omeg0sc0)
        IF(status.NE.NF90_NOERR) THEN
           ss%omeg0sc0 = 0.0_rspec
           CALL ncerr_name(2,status,"omeg0sc0")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%omeg0_halo)) THEN
     status = nf90_inq_varid(icdf,"omeg0_halo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"omeg0_halo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%omeg0_halo)
        IF(status.NE.NF90_NOERR) THEN
           ss%omeg0_halo = 0.0_rspec
           CALL ncerr_name(2,status,"omeg0_halo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%omeg0_reco)) THEN
     status = nf90_inq_varid(icdf,"omeg0_reco",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"omeg0_reco")
     ELSE
        status = nf90_get_var(icdf,varid,ss%omeg0_reco)
        IF(status.NE.NF90_NOERR) THEN
           ss%omeg0_reco = 0.0_rspec
           CALL ncerr_name(2,status,"omeg0_reco")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%omegat)) THEN
     status = nf90_inq_varid(icdf,"omegat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"omegat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%omegat)
        IF(status.NE.NF90_NOERR) THEN
           ss%omegat = 0.0_rspec
           CALL ncerr_name(2,status,"omegat")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"omegat_bdy",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"omegat_bdy")
  ELSE
     status = nf90_get_var(icdf,varid,ss%omegat_bdy)
     IF(status.NE.NF90_NOERR) THEN
        ss%omegat_bdy = 0.0_rspec
        CALL ncerr_name(2,status,"omegat_bdy")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%omega_exb)) THEN
     status = nf90_inq_varid(icdf,"omega_exb",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"omega_exb")
     ELSE
        status = nf90_get_var(icdf,varid,ss%omega_exb)
        IF(status.NE.NF90_NOERR) THEN
           ss%omega_exb = 0.0_rspec
           CALL ncerr_name(2,status,"omega_exb")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%p0_reco)) THEN
     status = nf90_inq_varid(icdf,"p0_reco",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"p0_reco")
     ELSE
        status = nf90_get_var(icdf,varid,ss%p0_reco)
        IF(status.NE.NF90_NOERR) THEN
           ss%p0_reco = 0.0_rspec
           CALL ncerr_name(2,status,"p0_reco")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pb0_halo)) THEN
     status = nf90_inq_varid(icdf,"pb0_halo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pb0_halo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pb0_halo)
        IF(status.NE.NF90_NOERR) THEN
           ss%pb0_halo = 0.0_rspec
           CALL ncerr_name(2,status,"pb0_halo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pbe)) THEN
     status = nf90_inq_varid(icdf,"pbe",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pbe")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pbe)
        IF(status.NE.NF90_NOERR) THEN
           ss%pbe = 0.0_rspec
           CALL ncerr_name(2,status,"pbe")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pbi)) THEN
     status = nf90_inq_varid(icdf,"pbi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pbi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pbi)
        IF(status.NE.NF90_NOERR) THEN
           ss%pbi = 0.0_rspec
           CALL ncerr_name(2,status,"pbi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pbth)) THEN
     status = nf90_inq_varid(icdf,"pbth",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pbth")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pbth)
        IF(status.NE.NF90_NOERR) THEN
           ss%pbth = 0.0_rspec
           CALL ncerr_name(2,status,"pbth")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pcx_halo)) THEN
     status = nf90_inq_varid(icdf,"pcx_halo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pcx_halo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pcx_halo)
        IF(status.NE.NF90_NOERR) THEN
           ss%pcx_halo = 0.0_rspec
           CALL ncerr_name(2,status,"pcx_halo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pcx_reco)) THEN
     status = nf90_inq_varid(icdf,"pcx_reco",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pcx_reco")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pcx_reco)
        IF(status.NE.NF90_NOERR) THEN
           ss%pcx_reco = 0.0_rspec
           CALL ncerr_name(2,status,"pcx_reco")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%peech)) THEN
     status = nf90_inq_varid(icdf,"peech",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"peech")
     ELSE
        status = nf90_get_var(icdf,varid,ss%peech)
        IF(status.NE.NF90_NOERR) THEN
           ss%peech = 0.0_rspec
           CALL ncerr_name(2,status,"peech")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%peech_src)) THEN
     status = nf90_inq_varid(icdf,"peech_src",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"peech_src")
     ELSE
        status = nf90_get_var(icdf,varid,ss%peech_src)
        IF(status.NE.NF90_NOERR) THEN
           ss%peech_src = 0.0_rspec
           CALL ncerr_name(2,status,"peech_src")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pelh)) THEN
     status = nf90_inq_varid(icdf,"pelh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pelh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pelh)
        IF(status.NE.NF90_NOERR) THEN
           ss%pelh = 0.0_rspec
           CALL ncerr_name(2,status,"pelh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pelh_src)) THEN
     status = nf90_inq_varid(icdf,"pelh_src",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pelh_src")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pelh_src)
        IF(status.NE.NF90_NOERR) THEN
           ss%pelh_src = 0.0_rspec
           CALL ncerr_name(2,status,"pelh_src")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"PEL_Code_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"PEL_Code_Info")
  ELSE
     ilen = len(ss%PEL_Code_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%PEL_Code_Info = " "
     status = nf90_get_var(icdf,varid,ss%PEL_Code_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%PEL_Code_Info = " "
        CALL ncerr_name(2,status,"PEL_Code_Info")
     ELSE
        CALL denull(ss%PEL_Code_Info)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"PEL_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"PEL_Data_Info")
  ELSE
     ilen = len(ss%PEL_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%PEL_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%PEL_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%PEL_Data_Info = " "
        CALL ncerr_name(2,status,"PEL_Data_Info")
     ELSE
        CALL denull(ss%PEL_Data_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pe_anom)) THEN
     status = nf90_inq_varid(icdf,"pe_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pe_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pe_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%pe_anom = 0.0_rspec
           CALL ncerr_name(2,status,"pe_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pe_trans)) THEN
     status = nf90_inq_varid(icdf,"pe_trans",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pe_trans")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pe_trans)
        IF(status.NE.NF90_NOERR) THEN
           ss%pe_trans = 0.0_rspec
           CALL ncerr_name(2,status,"pe_trans")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pe_transcat)) THEN
     status = nf90_inq_varid(icdf,"pe_transcat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pe_transcat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pe_transcat)
        IF(status.NE.NF90_NOERR) THEN
           ss%pe_transcat = 0.0_rspec
           CALL ncerr_name(2,status,"pe_transcat")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pfuse)) THEN
     status = nf90_inq_varid(icdf,"pfuse",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pfuse")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pfuse)
        IF(status.NE.NF90_NOERR) THEN
           ss%pfuse = 0.0_rspec
           CALL ncerr_name(2,status,"pfuse")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pfusi)) THEN
     status = nf90_inq_varid(icdf,"pfusi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pfusi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pfusi)
        IF(status.NE.NF90_NOERR) THEN
           ss%pfusi = 0.0_rspec
           CALL ncerr_name(2,status,"pfusi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pfusth)) THEN
     status = nf90_inq_varid(icdf,"pfusth",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pfusth")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pfusth)
        IF(status.NE.NF90_NOERR) THEN
           ss%pfusth = 0.0_rspec
           CALL ncerr_name(2,status,"pfusth")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Phibsc)) THEN
     status = nf90_inq_varid(icdf,"Phibsc",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Phibsc")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Phibsc)
        IF(status.NE.NF90_NOERR) THEN
           ss%Phibsc = 0.0_rspec
           CALL ncerr_name(2,status,"Phibsc")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%phiflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"phiflux_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"phiflux_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%phiflux_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%phiflux_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"phiflux_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%phit)) THEN
     status = nf90_inq_varid(icdf,"phit",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"phit")
     ELSE
        status = nf90_get_var(icdf,varid,ss%phit)
        IF(status.NE.NF90_NOERR) THEN
           ss%phit = 0.0_rspec
           CALL ncerr_name(2,status,"phit")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Phi_EC_launch)) THEN
     status = nf90_inq_varid(icdf,"Phi_EC_launch",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Phi_EC_launch")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Phi_EC_launch)
        IF(status.NE.NF90_NOERR) THEN
           ss%Phi_EC_launch = 0.0_rspec
           CALL ncerr_name(2,status,"Phi_EC_launch")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%picrf_abs)) THEN
     status = nf90_inq_varid(icdf,"picrf_abs",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"picrf_abs")
     ELSE
        status = nf90_get_var(icdf,varid,ss%picrf_abs)
        IF(status.NE.NF90_NOERR) THEN
           ss%picrf_abs = 0.0_rspec
           CALL ncerr_name(2,status,"picrf_abs")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%picrf_ext)) THEN
     status = nf90_inq_varid(icdf,"picrf_ext",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"picrf_ext")
     ELSE
        status = nf90_get_var(icdf,varid,ss%picrf_ext)
        IF(status.NE.NF90_NOERR) THEN
           ss%picrf_ext = 0.0_rspec
           CALL ncerr_name(2,status,"picrf_ext")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%picrf_nphi_srcs)) THEN
     status = nf90_inq_varid(icdf,"picrf_nphi_srcs",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"picrf_nphi_srcs")
     ELSE
        status = nf90_get_var(icdf,varid,ss%picrf_nphi_srcs)
        IF(status.NE.NF90_NOERR) THEN
           ss%picrf_nphi_srcs = 0.0_rspec
           CALL ncerr_name(2,status,"picrf_nphi_srcs")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%picrf_srcs)) THEN
     status = nf90_inq_varid(icdf,"picrf_srcs",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"picrf_srcs")
     ELSE
        status = nf90_get_var(icdf,varid,ss%picrf_srcs)
        IF(status.NE.NF90_NOERR) THEN
           ss%picrf_srcs = 0.0_rspec
           CALL ncerr_name(2,status,"picrf_srcs")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%picrf_totals)) THEN
     status = nf90_inq_varid(icdf,"picrf_totals",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"picrf_totals")
     ELSE
        status = nf90_get_var(icdf,varid,ss%picrf_totals)
        IF(status.NE.NF90_NOERR) THEN
           ss%picrf_totals = 0.0_rspec
           CALL ncerr_name(2,status,"picrf_totals")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%picth)) THEN
     status = nf90_inq_varid(icdf,"picth",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"picth")
     ELSE
        status = nf90_get_var(icdf,varid,ss%picth)
        IF(status.NE.NF90_NOERR) THEN
           ss%picth = 0.0_rspec
           CALL ncerr_name(2,status,"picth")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pilh)) THEN
     status = nf90_inq_varid(icdf,"pilh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pilh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pilh)
        IF(status.NE.NF90_NOERR) THEN
           ss%pilh = 0.0_rspec
           CALL ncerr_name(2,status,"pilh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pilh_src)) THEN
     status = nf90_inq_varid(icdf,"pilh_src",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pilh_src")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pilh_src)
        IF(status.NE.NF90_NOERR) THEN
           ss%pilh_src = 0.0_rspec
           CALL ncerr_name(2,status,"pilh_src")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pi_anom)) THEN
     status = nf90_inq_varid(icdf,"pi_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pi_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pi_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%pi_anom = 0.0_rspec
           CALL ncerr_name(2,status,"pi_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pi_trans)) THEN
     status = nf90_inq_varid(icdf,"pi_trans",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pi_trans")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pi_trans)
        IF(status.NE.NF90_NOERR) THEN
           ss%pi_trans = 0.0_rspec
           CALL ncerr_name(2,status,"pi_trans")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pi_transcat)) THEN
     status = nf90_inq_varid(icdf,"pi_transcat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pi_transcat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pi_transcat)
        IF(status.NE.NF90_NOERR) THEN
           ss%pi_transcat = 0.0_rspec
           CALL ncerr_name(2,status,"pi_transcat")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"PLASMA_Code_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"PLASMA_Code_Info")
  ELSE
     ilen = len(ss%PLASMA_Code_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%PLASMA_Code_Info = " "
     status = nf90_get_var(icdf,varid,ss%PLASMA_Code_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%PLASMA_Code_Info = " "
        CALL ncerr_name(2,status,"PLASMA_Code_Info")
     ELSE
        CALL denull(ss%PLASMA_Code_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pmine)) THEN
     status = nf90_inq_varid(icdf,"pmine",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pmine")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pmine)
        IF(status.NE.NF90_NOERR) THEN
           ss%pmine = 0.0_rspec
           CALL ncerr_name(2,status,"pmine")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pmini)) THEN
     status = nf90_inq_varid(icdf,"pmini",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pmini")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pmini)
        IF(status.NE.NF90_NOERR) THEN
           ss%pmini = 0.0_rspec
           CALL ncerr_name(2,status,"pmini")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pminth)) THEN
     status = nf90_inq_varid(icdf,"pminth",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pminth")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pminth)
        IF(status.NE.NF90_NOERR) THEN
           ss%pminth = 0.0_rspec
           CALL ncerr_name(2,status,"pminth")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pohme)) THEN
     status = nf90_inq_varid(icdf,"pohme",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pohme")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pohme)
        IF(status.NE.NF90_NOERR) THEN
           ss%pohme = 0.0_rspec
           CALL ncerr_name(2,status,"pohme")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%pot_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"pot_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"pot_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%pot_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%pot_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"pot_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%power_ec)) THEN
     status = nf90_inq_varid(icdf,"power_ec",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"power_ec")
     ELSE
        status = nf90_get_var(icdf,varid,ss%power_ec)
        IF(status.NE.NF90_NOERR) THEN
           ss%power_ec = 0.0_rspec
           CALL ncerr_name(2,status,"power_ec")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%power_ic)) THEN
     status = nf90_inq_varid(icdf,"power_ic",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"power_ic")
     ELSE
        status = nf90_get_var(icdf,varid,ss%power_ic)
        IF(status.NE.NF90_NOERR) THEN
           ss%power_ic = 0.0_rspec
           CALL ncerr_name(2,status,"power_ic")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%power_lh)) THEN
     status = nf90_inq_varid(icdf,"power_lh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"power_lh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%power_lh)
        IF(status.NE.NF90_NOERR) THEN
           ss%power_lh = 0.0_rspec
           CALL ncerr_name(2,status,"power_lh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%power_nbi)) THEN
     status = nf90_inq_varid(icdf,"power_nbi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"power_nbi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%power_nbi)
        IF(status.NE.NF90_NOERR) THEN
           ss%power_nbi = 0.0_rspec
           CALL ncerr_name(2,status,"power_nbi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%power_nbi_trace)) THEN
     status = nf90_inq_varid(icdf,"power_nbi_trace",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"power_nbi_trace")
     ELSE
        status = nf90_get_var(icdf,varid,ss%power_nbi_trace)
        IF(status.NE.NF90_NOERR) THEN
           ss%power_nbi_trace = 0.0_rspec
           CALL ncerr_name(2,status,"power_nbi_trace")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%prad)) THEN
     status = nf90_inq_varid(icdf,"prad",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"prad")
     ELSE
        status = nf90_get_var(icdf,varid,ss%prad)
        IF(status.NE.NF90_NOERR) THEN
           ss%prad = 0.0_rspec
           CALL ncerr_name(2,status,"prad")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%prad_br)) THEN
     status = nf90_inq_varid(icdf,"prad_br",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"prad_br")
     ELSE
        status = nf90_get_var(icdf,varid,ss%prad_br)
        IF(status.NE.NF90_NOERR) THEN
           ss%prad_br = 0.0_rspec
           CALL ncerr_name(2,status,"prad_br")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%prad_cy)) THEN
     status = nf90_inq_varid(icdf,"prad_cy",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"prad_cy")
     ELSE
        status = nf90_get_var(icdf,varid,ss%prad_cy)
        IF(status.NE.NF90_NOERR) THEN
           ss%prad_cy = 0.0_rspec
           CALL ncerr_name(2,status,"prad_cy")
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

END SUBROUTINE plasma_state_get_5
