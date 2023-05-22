! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_get_7(ss,icdf,version_change,iout,iertot)


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

  IF(ALLOCATED(ss%rho_pel)) THEN
     status = nf90_inq_varid(icdf,"rho_pel",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_pel")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_pel)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_pel = 0.0_rspec
           CALL ncerr_name(2,status,"rho_pel")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_rad)) THEN
     status = nf90_inq_varid(icdf,"rho_rad",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_rad")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_rad)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_rad = 0.0_rspec
           CALL ncerr_name(2,status,"rho_rad")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_rw)) THEN
     status = nf90_inq_varid(icdf,"rho_rw",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_rw")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_rw)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_rw = 0.0_rspec
           CALL ncerr_name(2,status,"rho_rw")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rlim)) THEN
     status = nf90_inq_varid(icdf,"rlim",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rlim")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rlim)
        IF(status.NE.NF90_NOERR) THEN
           ss%rlim = 0.0_rspec
           CALL ncerr_name(2,status,"rlim")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Rloc_coil)) THEN
     status = nf90_inq_varid(icdf,"Rloc_coil",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Rloc_coil")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Rloc_coil)
        IF(status.NE.NF90_NOERR) THEN
           ss%Rloc_coil = 0.0_rspec
           CALL ncerr_name(2,status,"Rloc_coil")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Rmajor_mean)) THEN
     status = nf90_inq_varid(icdf,"Rmajor_mean",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Rmajor_mean")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Rmajor_mean)
        IF(status.NE.NF90_NOERR) THEN
           ss%Rmajor_mean = 0.0_rspec
           CALL ncerr_name(2,status,"Rmajor_mean")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rMinor_mean)) THEN
     status = nf90_inq_varid(icdf,"rMinor_mean",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rMinor_mean")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rMinor_mean)
        IF(status.NE.NF90_NOERR) THEN
           ss%rMinor_mean = 0.0_rspec
           CALL ncerr_name(2,status,"rMinor_mean")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RUNAWAY_Code_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"RUNAWAY_Code_Info")
  ELSE
     ilen = len(ss%RUNAWAY_Code_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%RUNAWAY_Code_Info = " "
     status = nf90_get_var(icdf,varid,ss%RUNAWAY_Code_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%RUNAWAY_Code_Info = " "
        CALL ncerr_name(2,status,"RUNAWAY_Code_Info")
     ELSE
        CALL denull(ss%RUNAWAY_Code_Info)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RunID",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"RunID")
  ELSE
     ilen = len(ss%RunID)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%RunID = " "
     status = nf90_get_var(icdf,varid,ss%RunID(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%RunID = " "
        CALL ncerr_name(2,status,"RunID")
     ELSE
        CALL denull(ss%RunID)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_ant)) THEN
     status = nf90_inq_varid(icdf,"R_ant",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"R_ant")
     ELSE
        status = nf90_get_var(icdf,varid,ss%R_ant)
        IF(status.NE.NF90_NOERR) THEN
           ss%R_ant = 0.0_rspec
           CALL ncerr_name(2,status,"R_ant")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_antgeo)) THEN
     status = nf90_inq_varid(icdf,"R_antgeo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"R_antgeo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%R_antgeo)
        IF(status.NE.NF90_NOERR) THEN
           ss%R_antgeo = 0.0_rspec
           CALL ncerr_name(2,status,"R_antgeo")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_axis",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"R_axis")
  ELSE
     status = nf90_get_var(icdf,varid,ss%R_axis)
     IF(status.NE.NF90_NOERR) THEN
        ss%R_axis = 0.0_rspec
        CALL ncerr_name(2,status,"R_axis")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_EC_launch)) THEN
     status = nf90_inq_varid(icdf,"R_EC_launch",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"R_EC_launch")
     ELSE
        status = nf90_get_var(icdf,varid,ss%R_EC_launch)
        IF(status.NE.NF90_NOERR) THEN
           ss%R_EC_launch = 0.0_rspec
           CALL ncerr_name(2,status,"R_EC_launch")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_geo)) THEN
     status = nf90_inq_varid(icdf,"R_geo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"R_geo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%R_geo)
        IF(status.NE.NF90_NOERR) THEN
           ss%R_geo = 0.0_rspec
           CALL ncerr_name(2,status,"R_geo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_grid)) THEN
     status = nf90_inq_varid(icdf,"R_grid",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"R_grid")
     ELSE
        status = nf90_get_var(icdf,varid,ss%R_grid)
        IF(status.NE.NF90_NOERR) THEN
           ss%R_grid = 0.0_rspec
           CALL ncerr_name(2,status,"R_grid")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_max_box",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"R_max_box")
  ELSE
     status = nf90_get_var(icdf,varid,ss%R_max_box)
     IF(status.NE.NF90_NOERR) THEN
        ss%R_max_box = 0.0_rspec
        CALL ncerr_name(2,status,"R_max_box")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_max_lcfs",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"R_max_lcfs")
  ELSE
     status = nf90_get_var(icdf,varid,ss%R_max_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        ss%R_max_lcfs = 0.0_rspec
        CALL ncerr_name(2,status,"R_max_lcfs")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_midp_in)) THEN
     status = nf90_inq_varid(icdf,"R_midp_in",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"R_midp_in")
     ELSE
        status = nf90_get_var(icdf,varid,ss%R_midp_in)
        IF(status.NE.NF90_NOERR) THEN
           ss%R_midp_in = 0.0_rspec
           CALL ncerr_name(2,status,"R_midp_in")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_midp_out)) THEN
     status = nf90_inq_varid(icdf,"R_midp_out",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"R_midp_out")
     ELSE
        status = nf90_get_var(icdf,varid,ss%R_midp_out)
        IF(status.NE.NF90_NOERR) THEN
           ss%R_midp_out = 0.0_rspec
           CALL ncerr_name(2,status,"R_midp_out")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_min_box",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"R_min_box")
  ELSE
     status = nf90_get_var(icdf,varid,ss%R_min_box)
     IF(status.NE.NF90_NOERR) THEN
        ss%R_min_box = 0.0_rspec
        CALL ncerr_name(2,status,"R_min_box")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R_min_lcfs",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"R_min_lcfs")
  ELSE
     status = nf90_get_var(icdf,varid,ss%R_min_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        ss%R_min_lcfs = 0.0_rspec
        CALL ncerr_name(2,status,"R_min_lcfs")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_ripple)) THEN
     status = nf90_inq_varid(icdf,"R_ripple",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"R_ripple")
     ELSE
        status = nf90_get_var(icdf,varid,ss%R_ripple)
        IF(status.NE.NF90_NOERR) THEN
           ss%R_ripple = 0.0_rspec
           CALL ncerr_name(2,status,"R_ripple")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_surfMax)) THEN
     status = nf90_inq_varid(icdf,"R_surfMax",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"R_surfMax")
     ELSE
        status = nf90_get_var(icdf,varid,ss%R_surfMax)
        IF(status.NE.NF90_NOERR) THEN
           ss%R_surfMax = 0.0_rspec
           CALL ncerr_name(2,status,"R_surfMax")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R_surfMin)) THEN
     status = nf90_inq_varid(icdf,"R_surfMin",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"R_surfMin")
     ELSE
        status = nf90_get_var(icdf,varid,ss%R_surfMin)
        IF(status.NE.NF90_NOERR) THEN
           ss%R_surfMin = 0.0_rspec
           CALL ncerr_name(2,status,"R_surfMin")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%s0reco)) THEN
     status = nf90_inq_varid(icdf,"s0reco",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"s0reco")
     ELSE
        status = nf90_get_var(icdf,varid,ss%s0reco)
        IF(status.NE.NF90_NOERR) THEN
           ss%s0reco = 0.0_rspec
           CALL ncerr_name(2,status,"s0reco")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%s0reco_e)) THEN
     status = nf90_inq_varid(icdf,"s0reco_e",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"s0reco_e")
     ELSE
        status = nf90_get_var(icdf,varid,ss%s0reco_e)
        IF(status.NE.NF90_NOERR) THEN
           ss%s0reco_e = 0.0_rspec
           CALL ncerr_name(2,status,"s0reco_e")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%s0reco_recap)) THEN
     status = nf90_inq_varid(icdf,"s0reco_recap",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"s0reco_recap")
     ELSE
        status = nf90_get_var(icdf,varid,ss%s0reco_recap)
        IF(status.NE.NF90_NOERR) THEN
           ss%s0reco_recap = 0.0_rspec
           CALL ncerr_name(2,status,"s0reco_recap")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sa_index)) THEN
     status = nf90_inq_varid(icdf,"sa_index",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sa_index")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sa_index)
        IF(status.NE.NF90_NOERR) THEN
           ss%sa_index = 0
           CALL ncerr_name(2,status,"sa_index")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SA_name)) THEN
     status = nf90_inq_varid(icdf,"SA_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"SA_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%SA_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%SA_name = " "
           CALL ncerr_name(2,status,"SA_name")
        ELSE
           CALL denull_vec(ss%SA_name,ss%nspec_tha+1)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SA_type)) THEN
     status = nf90_inq_varid(icdf,"SA_type",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"SA_type")
     ELSE
        status = nf90_get_var(icdf,varid,ss%SA_type)
        IF(status.NE.NF90_NOERR) THEN
           ss%SA_type = 0
           CALL ncerr_name(2,status,"SA_type")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sb0halo)) THEN
     status = nf90_inq_varid(icdf,"sb0halo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sb0halo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sb0halo)
        IF(status.NE.NF90_NOERR) THEN
           ss%sb0halo = 0.0_rspec
           CALL ncerr_name(2,status,"sb0halo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sb0halo_recap)) THEN
     status = nf90_inq_varid(icdf,"sb0halo_recap",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sb0halo_recap")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sb0halo_recap)
        IF(status.NE.NF90_NOERR) THEN
           ss%sb0halo_recap = 0.0_rspec
           CALL ncerr_name(2,status,"sb0halo_recap")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sbedep)) THEN
     status = nf90_inq_varid(icdf,"sbedep",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sbedep")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sbedep)
        IF(status.NE.NF90_NOERR) THEN
           ss%sbedep = 0.0_rspec
           CALL ncerr_name(2,status,"sbedep")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sbehalo)) THEN
     status = nf90_inq_varid(icdf,"sbehalo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sbehalo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sbehalo)
        IF(status.NE.NF90_NOERR) THEN
           ss%sbehalo = 0.0_rspec
           CALL ncerr_name(2,status,"sbehalo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sbsce)) THEN
     status = nf90_inq_varid(icdf,"sbsce",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sbsce")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sbsce)
        IF(status.NE.NF90_NOERR) THEN
           ss%sbsce = 0.0_rspec
           CALL ncerr_name(2,status,"sbsce")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sbtherm)) THEN
     status = nf90_inq_varid(icdf,"sbtherm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sbtherm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sbtherm)
        IF(status.NE.NF90_NOERR) THEN
           ss%sbtherm = 0.0_rspec
           CALL ncerr_name(2,status,"sbtherm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sc0)) THEN
     status = nf90_inq_varid(icdf,"sc0",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sc0")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sc0)
        IF(status.NE.NF90_NOERR) THEN
           ss%sc0 = 0.0_rspec
           CALL ncerr_name(2,status,"sc0")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sc0_to_sgas)) THEN
     status = nf90_inq_varid(icdf,"sc0_to_sgas",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sc0_to_sgas")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sc0_to_sgas)
        IF(status.NE.NF90_NOERR) THEN
           ss%sc0_to_sgas = 0
           CALL ncerr_name(2,status,"sc0_to_sgas")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sc_anom)) THEN
     status = nf90_inq_varid(icdf,"sc_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sc_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sc_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%sc_anom = 0.0_rspec
           CALL ncerr_name(2,status,"sc_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sfsce)) THEN
     status = nf90_inq_varid(icdf,"sfsce",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sfsce")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sfsce)
        IF(status.NE.NF90_NOERR) THEN
           ss%sfsce = 0.0_rspec
           CALL ncerr_name(2,status,"sfsce")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sftherm)) THEN
     status = nf90_inq_varid(icdf,"sftherm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sftherm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sftherm)
        IF(status.NE.NF90_NOERR) THEN
           ss%sftherm = 0.0_rspec
           CALL ncerr_name(2,status,"sftherm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SFUS_name)) THEN
     status = nf90_inq_varid(icdf,"SFUS_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"SFUS_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%SFUS_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%SFUS_name = " "
           CALL ncerr_name(2,status,"SFUS_name")
        ELSE
           CALL denull_vec(ss%SFUS_name,ss%nspec_fusion)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sfus_to_all)) THEN
     status = nf90_inq_varid(icdf,"sfus_to_all",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sfus_to_all")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sfus_to_all)
        IF(status.NE.NF90_NOERR) THEN
           ss%sfus_to_all = 0
           CALL ncerr_name(2,status,"sfus_to_all")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sfus_to_alla)) THEN
     status = nf90_inq_varid(icdf,"sfus_to_alla",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sfus_to_alla")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sfus_to_alla)
        IF(status.NE.NF90_NOERR) THEN
           ss%sfus_to_alla = 0
           CALL ncerr_name(2,status,"sfus_to_alla")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SFUS_type)) THEN
     status = nf90_inq_varid(icdf,"SFUS_type",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"SFUS_type")
     ELSE
        status = nf90_get_var(icdf,varid,ss%SFUS_type)
        IF(status.NE.NF90_NOERR) THEN
           ss%SFUS_type = 0
           CALL ncerr_name(2,status,"SFUS_type")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SGAS_name)) THEN
     status = nf90_inq_varid(icdf,"SGAS_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"SGAS_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%SGAS_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%SGAS_name = " "
           CALL ncerr_name(2,status,"SGAS_name")
        ELSE
           CALL denull_vec(ss%SGAS_name,ss%nspec_gas)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sgas_to_s)) THEN
     status = nf90_inq_varid(icdf,"sgas_to_s",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sgas_to_s")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sgas_to_s)
        IF(status.NE.NF90_NOERR) THEN
           ss%sgas_to_s = 0
           CALL ncerr_name(2,status,"sgas_to_s")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SGAS_type)) THEN
     status = nf90_inq_varid(icdf,"SGAS_type",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"SGAS_type")
     ELSE
        status = nf90_get_var(icdf,varid,ss%SGAS_type)
        IF(status.NE.NF90_NOERR) THEN
           ss%SGAS_type = 0
           CALL ncerr_name(2,status,"SGAS_type")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"shot_number",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"shot_number")
  ELSE
     status = nf90_get_var(icdf,varid,ss%shot_number)
     IF(status.NE.NF90_NOERR) THEN
        ss%shot_number = ps_unInit
        CALL ncerr_name(2,status,"shot_number")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sii_xs)) THEN
     status = nf90_inq_varid(icdf,"sii_xs",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sii_xs")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sii_xs)
        IF(status.NE.NF90_NOERR) THEN
           ss%sii_xs = 0.0_rspec
           CALL ncerr_name(2,status,"sii_xs")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SIMP0_name)) THEN
     status = nf90_inq_varid(icdf,"SIMP0_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"SIMP0_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%SIMP0_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%SIMP0_name = " "
           CALL ncerr_name(2,status,"SIMP0_name")
        ELSE
           CALL denull_vec(ss%SIMP0_name,ss%nspec_imp0)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%simp0_to_s)) THEN
     status = nf90_inq_varid(icdf,"simp0_to_s",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"simp0_to_s")
     ELSE
        status = nf90_get_var(icdf,varid,ss%simp0_to_s)
        IF(status.NE.NF90_NOERR) THEN
           ss%simp0_to_s = 0
           CALL ncerr_name(2,status,"simp0_to_s")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SIMP0_type)) THEN
     status = nf90_inq_varid(icdf,"SIMP0_type",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"SIMP0_type")
     ELSE
        status = nf90_get_var(icdf,varid,ss%SIMP0_type)
        IF(status.NE.NF90_NOERR) THEN
           ss%SIMP0_type = 0
           CALL ncerr_name(2,status,"SIMP0_type")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SIMPI_name)) THEN
     status = nf90_inq_varid(icdf,"SIMPI_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"SIMPI_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%SIMPI_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%SIMPI_name = " "
           CALL ncerr_name(2,status,"SIMPI_name")
        ELSE
           CALL denull_vec(ss%SIMPI_name,ss%nspec_impi)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%simpi_to_s)) THEN
     status = nf90_inq_varid(icdf,"simpi_to_s",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"simpi_to_s")
     ELSE
        status = nf90_get_var(icdf,varid,ss%simpi_to_s)
        IF(status.NE.NF90_NOERR) THEN
           ss%simpi_to_s = 0
           CALL ncerr_name(2,status,"simpi_to_s")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SIMPI_type)) THEN
     status = nf90_inq_varid(icdf,"SIMPI_type",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"SIMPI_type")
     ELSE
        status = nf90_get_var(icdf,varid,ss%SIMPI_type)
        IF(status.NE.NF90_NOERR) THEN
           ss%SIMPI_type = 0
           CALL ncerr_name(2,status,"SIMPI_type")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SNBI_name)) THEN
     status = nf90_inq_varid(icdf,"SNBI_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"SNBI_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%SNBI_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%SNBI_name = " "
           CALL ncerr_name(2,status,"SNBI_name")
        ELSE
           CALL denull_vec(ss%SNBI_name,ss%nspec_beam)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%snbi_to_all)) THEN
     status = nf90_inq_varid(icdf,"snbi_to_all",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"snbi_to_all")
     ELSE
        status = nf90_get_var(icdf,varid,ss%snbi_to_all)
        IF(status.NE.NF90_NOERR) THEN
           ss%snbi_to_all = 0
           CALL ncerr_name(2,status,"snbi_to_all")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%snbi_to_alla)) THEN
     status = nf90_inq_varid(icdf,"snbi_to_alla",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"snbi_to_alla")
     ELSE
        status = nf90_get_var(icdf,varid,ss%snbi_to_alla)
        IF(status.NE.NF90_NOERR) THEN
           ss%snbi_to_alla = 0
           CALL ncerr_name(2,status,"snbi_to_alla")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%SNBI_type)) THEN
     status = nf90_inq_varid(icdf,"SNBI_type",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"SNBI_type")
     ELSE
        status = nf90_get_var(icdf,varid,ss%SNBI_type)
        IF(status.NE.NF90_NOERR) THEN
           ss%SNBI_type = 0
           CALL ncerr_name(2,status,"SNBI_type")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sn_trans)) THEN
     status = nf90_inq_varid(icdf,"sn_trans",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sn_trans")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sn_trans)
        IF(status.NE.NF90_NOERR) THEN
           ss%sn_trans = 0.0_rspec
           CALL ncerr_name(2,status,"sn_trans")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sn_transcat)) THEN
     status = nf90_inq_varid(icdf,"sn_transcat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sn_transcat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sn_transcat)
        IF(status.NE.NF90_NOERR) THEN
           ss%sn_transcat = 0.0_rspec
           CALL ncerr_name(2,status,"sn_transcat")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sprof0)) THEN
     status = nf90_inq_varid(icdf,"sprof0",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sprof0")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sprof0)
        IF(status.NE.NF90_NOERR) THEN
           ss%sprof0 = 0.0_rspec
           CALL ncerr_name(2,status,"sprof0")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sprof0e)) THEN
     status = nf90_inq_varid(icdf,"sprof0e",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sprof0e")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sprof0e)
        IF(status.NE.NF90_NOERR) THEN
           ss%sprof0e = 0.0_rspec
           CALL ncerr_name(2,status,"sprof0e")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%spsce)) THEN
     status = nf90_inq_varid(icdf,"spsce",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"spsce")
     ELSE
        status = nf90_get_var(icdf,varid,ss%spsce)
        IF(status.NE.NF90_NOERR) THEN
           ss%spsce = 0.0_rspec
           CALL ncerr_name(2,status,"spsce")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%squareLO)) THEN
     status = nf90_inq_varid(icdf,"squareLO",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"squareLO")
     ELSE
        status = nf90_get_var(icdf,varid,ss%squareLO)
        IF(status.NE.NF90_NOERR) THEN
           ss%squareLO = 0.0_rspec
           CALL ncerr_name(2,status,"squareLO")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%squareUO)) THEN
     status = nf90_inq_varid(icdf,"squareUO",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"squareUO")
     ELSE
        status = nf90_get_var(icdf,varid,ss%squareUO)
        IF(status.NE.NF90_NOERR) THEN
           ss%squareUO = 0.0_rspec
           CALL ncerr_name(2,status,"squareUO")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%sRtcen)) THEN
     status = nf90_inq_varid(icdf,"sRtcen",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"sRtcen")
     ELSE
        status = nf90_get_var(icdf,varid,ss%sRtcen)
        IF(status.NE.NF90_NOERR) THEN
           ss%sRtcen = 0.0_rspec
           CALL ncerr_name(2,status,"sRtcen")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%surf)) THEN
     status = nf90_inq_varid(icdf,"surf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"surf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%surf)
        IF(status.NE.NF90_NOERR) THEN
           ss%surf = 0.0_rspec
           CALL ncerr_name(2,status,"surf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%S_name)) THEN
     status = nf90_inq_varid(icdf,"S_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"S_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%S_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%S_name = " "
           CALL ncerr_name(2,status,"S_name")
        ELSE
           CALL denull_vec(ss%S_name,ss%nspec_th+1)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%s_to_sa)) THEN
     status = nf90_inq_varid(icdf,"s_to_sa",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"s_to_sa")
     ELSE
        status = nf90_get_var(icdf,varid,ss%s_to_sa)
        IF(status.NE.NF90_NOERR) THEN
           ss%s_to_sa = 0
           CALL ncerr_name(2,status,"s_to_sa")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%S_type)) THEN
     status = nf90_inq_varid(icdf,"S_type",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"S_type")
     ELSE
        status = nf90_get_var(icdf,varid,ss%S_type)
        IF(status.NE.NF90_NOERR) THEN
           ss%S_type = 0
           CALL ncerr_name(2,status,"S_type")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"t0",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"t0")
  ELSE
     status = nf90_get_var(icdf,varid,ss%t0)
     IF(status.NE.NF90_NOERR) THEN
        ss%t0 = 0.0_rspec
        CALL ncerr_name(2,status,"t0")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%T0cx)) THEN
     status = nf90_inq_varid(icdf,"T0cx",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"T0cx")
     ELSE
        status = nf90_get_var(icdf,varid,ss%T0cx)
        IF(status.NE.NF90_NOERR) THEN
           ss%T0cx = 0.0_rspec
           CALL ncerr_name(2,status,"T0cx")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%T0sc0)) THEN
     status = nf90_inq_varid(icdf,"T0sc0",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"T0sc0")
     ELSE
        status = nf90_get_var(icdf,varid,ss%T0sc0)
        IF(status.NE.NF90_NOERR) THEN
           ss%T0sc0 = 0.0_rspec
           CALL ncerr_name(2,status,"T0sc0")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%T0_halo)) THEN
     status = nf90_inq_varid(icdf,"T0_halo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"T0_halo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%T0_halo)
        IF(status.NE.NF90_NOERR) THEN
           ss%T0_halo = 0.0_rspec
           CALL ncerr_name(2,status,"T0_halo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%T0_reco)) THEN
     status = nf90_inq_varid(icdf,"T0_reco",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"T0_reco")
     ELSE
        status = nf90_get_var(icdf,varid,ss%T0_reco)
        IF(status.NE.NF90_NOERR) THEN
           ss%T0_reco = 0.0_rspec
           CALL ncerr_name(2,status,"T0_reco")
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

END SUBROUTINE plasma_state_get_7
