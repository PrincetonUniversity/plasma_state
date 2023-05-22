
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_singl(ss,xhash,iout,ierr)

  ! called from plasma_state_xload: check grids; store in xplasma structure

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:

  INTEGER, PARAMETER :: max_local = 4

  INTEGER :: icomponent,imod,nitems
  CHARACTER*32 :: listname,enum_name,units
  CHARACTER*80 :: label
  INTEGER :: ahash(ps_max_static)

  CHARACTER*32 :: item_names(max_local)
  INTEGER :: item_ivals(max_local)
  REAL(KIND=rspec) :: item_r8vals(max_local)
  CHARACTER*256 :: item_c256(max_local)
  !-----------------------

  ahash(1) = ps_shash_ANOM_Code_Info
  IF(hcheck(1,1)) THEN
     listname = "ANOM_CODE_INFO"
     icomponent = ps_cnum_ANOM
     units = " "
     label = "Information: code implementing ANOM component"
     nitems = 1
     item_names(1) = "ANOM_Code_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%ANOM_Code_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_CUR_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "CUR_DATA_INFO"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "information on source of plasma current data"
     nitems = 1
     item_names(1) = "CUR_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%CUR_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_dn0out
  IF(hcheck(1,1)) THEN
     listname = "DN0OUT"
     icomponent = ps_cnum_GAS
     units = "m^-3"
     label = "Hydrogenic neutral density in scrape off region"
     nitems = 1
     item_names(1) = "dn0out"
     item_r8vals(1) = ss%dn0out
     item_ivals(1) = 0
     item_c256(1) = " "
     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_D_einj_standard
  IF(hcheck(1,1)) THEN
     listname = "D_EINJ_STANDARD"
     enum_name = "D_EINJ_STANDARD__RVEC"
     icomponent = ps_cnum_NBI
     units = "keV"
     label = "D beam table energies"
     nitems = 1
     item_names(1) = "D_einj_standard"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%n_d_einj_standard
     item_c256(1) = "__enumeration__"
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     IF(ALLOCATED(ss%D_einj_standard)) THEN
        CALL ps_enum_1d_store(ss%iobj,enum_name,icomponent, &
             ss%n_d_einj_standard,ss%D_einj_standard,imod,iout,ierr)
        IF(ierr.NE.0) RETURN
     ENDIF
  ENDIF

  ahash(1) = ps_shash_D_ffull_standard
  IF(hcheck(1,1)) THEN
     listname = "D_FFULL_STANDARD"
     enum_name = "D_FFULL_STANDARD__RVEC"
     icomponent = ps_cnum_NBI
     units = "-"
     label = "D beam full energy fractions"
     nitems = 1
     item_names(1) = "D_ffull_standard"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%n_d_einj_standard
     item_c256(1) = "__enumeration__"
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     IF(ALLOCATED(ss%D_ffull_standard)) THEN
        CALL ps_enum_1d_store(ss%iobj,enum_name,icomponent, &
             ss%n_d_einj_standard,ss%D_ffull_standard,imod,iout,ierr)
        IF(ierr.NE.0) RETURN
     ENDIF
  ENDIF

  ahash(1) = ps_shash_D_fhalf_standard
  IF(hcheck(1,1)) THEN
     listname = "D_FHALF_STANDARD"
     enum_name = "D_FHALF_STANDARD__RVEC"
     icomponent = ps_cnum_NBI
     units = "-"
     label = "D beam half energy fractions"
     nitems = 1
     item_names(1) = "D_fhalf_standard"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%n_d_einj_standard
     item_c256(1) = "__enumeration__"
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     IF(ALLOCATED(ss%D_fhalf_standard)) THEN
        CALL ps_enum_1d_store(ss%iobj,enum_name,icomponent, &
             ss%n_d_einj_standard,ss%D_fhalf_standard,imod,iout,ierr)
        IF(ierr.NE.0) RETURN
     ENDIF
  ENDIF

  ahash(1) = ps_shash_EC_Code_Info
  IF(hcheck(1,1)) THEN
     listname = "EC_CODE_INFO"
     icomponent = ps_cnum_EC
     units = " "
     label = "Information: code implementing EC component"
     nitems = 1
     item_names(1) = "EC_Code_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%EC_Code_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_EC_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "EC_DATA_INFO"
     icomponent = ps_cnum_EC
     units = " "
     label = "information on source of ECRF power data"
     nitems = 1
     item_names(1) = "EC_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%EC_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_eldot
  IF(hcheck(1,1)) THEN
     listname = "ELDOT"
     icomponent = ps_cnum_PLASMA
     units = "1/sec"
     label = "[d(Philim)/dt]/[2*Philim]"
     nitems = 1
     item_names(1) = "eldot"
     item_r8vals(1) = ss%eldot
     item_ivals(1) = 0
     item_c256(1) = " "
     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_EQ_Code_Info
  IF(hcheck(1,1)) THEN
     listname = "EQ_CODE_INFO"
     icomponent = ps_cnum_EQ
     units = " "
     label = "Information: code implementing EQ component"
     nitems = 1
     item_names(1) = "EQ_Code_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%EQ_Code_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_EQ_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "EQ_DATA_INFO"
     icomponent = ps_cnum_EQ
     units = " "
     label = "Information: source of EQ input data"
     nitems = 1
     item_names(1) = "EQ_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%EQ_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_FUS_Code_Info
  IF(hcheck(1,1)) THEN
     listname = "FUS_CODE_INFO"
     icomponent = ps_cnum_FUS
     units = " "
     label = "Information: code implementing FUS component"
     nitems = 1
     item_names(1) = "FUS_Code_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%FUS_Code_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_GAS_Code_Info
  IF(hcheck(1,1)) THEN
     listname = "GAS_CODE_INFO"
     icomponent = ps_cnum_GAS
     units = " "
     label = "Information: code implementing GAS component"
     nitems = 1
     item_names(1) = "GAS_Code_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%GAS_Code_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_geometry
  IF(hcheck(1,1)) THEN
     listname = "GEOMETRY"
     icomponent = ps_cnum_EQ
     units = " "
     label = "path to file(s) vac. vessel & coil descr."
     nitems = 1
     item_names(1) = "geometry"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%geometry
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Global_label
  IF(hcheck(1,1)) THEN
     listname = "GLOBAL_LABEL"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "Run Label"
     nitems = 1
     item_names(1) = "Global_label"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%Global_label
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_H_einj_standard
  IF(hcheck(1,1)) THEN
     listname = "H_EINJ_STANDARD"
     enum_name = "H_EINJ_STANDARD__RVEC"
     icomponent = ps_cnum_NBI
     units = "keV"
     label = "H beam table energies"
     nitems = 1
     item_names(1) = "H_einj_standard"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%n_h_einj_standard
     item_c256(1) = "__enumeration__"
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     IF(ALLOCATED(ss%H_einj_standard)) THEN
        CALL ps_enum_1d_store(ss%iobj,enum_name,icomponent, &
             ss%n_h_einj_standard,ss%H_einj_standard,imod,iout,ierr)
        IF(ierr.NE.0) RETURN
     ENDIF
  ENDIF

  ahash(1) = ps_shash_H_ffull_standard
  IF(hcheck(1,1)) THEN
     listname = "H_FFULL_STANDARD"
     enum_name = "H_FFULL_STANDARD__RVEC"
     icomponent = ps_cnum_NBI
     units = "-"
     label = "H beam full energy fractions"
     nitems = 1
     item_names(1) = "H_ffull_standard"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%n_h_einj_standard
     item_c256(1) = "__enumeration__"
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     IF(ALLOCATED(ss%H_ffull_standard)) THEN
        CALL ps_enum_1d_store(ss%iobj,enum_name,icomponent, &
             ss%n_h_einj_standard,ss%H_ffull_standard,imod,iout,ierr)
        IF(ierr.NE.0) RETURN
     ENDIF
  ENDIF

  ahash(1) = ps_shash_H_fhalf_standard
  IF(hcheck(1,1)) THEN
     listname = "H_FHALF_STANDARD"
     enum_name = "H_FHALF_STANDARD__RVEC"
     icomponent = ps_cnum_NBI
     units = "-"
     label = "H beam half energy fractions"
     nitems = 1
     item_names(1) = "H_fhalf_standard"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%n_h_einj_standard
     item_c256(1) = "__enumeration__"
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     IF(ALLOCATED(ss%H_fhalf_standard)) THEN
        CALL ps_enum_1d_store(ss%iobj,enum_name,icomponent, &
             ss%n_h_einj_standard,ss%H_fhalf_standard,imod,iout,ierr)
        IF(ierr.NE.0) RETURN
     ENDIF
  ENDIF

  ahash(1) = ps_shash_IC_Code_Info
  IF(hcheck(1,1)) THEN
     listname = "IC_CODE_INFO"
     icomponent = ps_cnum_IC
     units = " "
     label = "Information: code implementing IC component"
     nitems = 1
     item_names(1) = "IC_Code_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%IC_Code_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_IC_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "IC_DATA_INFO"
     icomponent = ps_cnum_IC
     units = " "
     label = "information on source of ICRF power data"
     nitems = 1
     item_names(1) = "IC_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%IC_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_interchange_R
  IF(hcheck(1,1)) THEN
     listname = "INTERCHANGE_R"
     icomponent = ps_cnum_ANOM
     units = "-"
     label = "norm rho of the location of the interchange mode from TGLF model"
     nitems = 1
     item_names(1) = "interchange_R"
     item_r8vals(1) = ss%interchange_R
     item_ivals(1) = 0
     item_c256(1) = " "
     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_i_npoloidal
  IF(hcheck(1,1)) THEN
     listname = "I_NPOLOIDAL"
     icomponent = ps_cnum_LH
     units = " "
     label = "need description"
     nitems = 1
     item_names(1) = "i_npoloidal"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%i_npoloidal
     item_c256(1) = " "
     CALL ps_list_store("SHOT_CONFIGURATION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_LH_Code_Info
  IF(hcheck(1,1)) THEN
     listname = "LH_CODE_INFO"
     icomponent = ps_cnum_LH
     units = " "
     label = "Information: code implementing LH component"
     nitems = 1
     item_names(1) = "LH_Code_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%LH_Code_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_LH_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "LH_DATA_INFO"
     icomponent = ps_cnum_LH
     units = " "
     label = "Information on source of LHRF power data"
     nitems = 1
     item_names(1) = "LH_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%LH_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_LMHD_eq_code
  IF(hcheck(1,1)) THEN
     listname = "LMHD_EQ_CODE"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "name of MHD equilibrium refinement code used"
     nitems = 1
     item_names(1) = "LMHD_eq_code"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%LMHD_eq_code
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_NBI_Code_Info
  IF(hcheck(1,1)) THEN
     listname = "NBI_CODE_INFO"
     icomponent = ps_cnum_NBI
     units = " "
     label = "Information: code implementing NBI component"
     nitems = 1
     item_names(1) = "NBI_Code_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%NBI_Code_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_NBI_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "NBI_DATA_INFO"
     icomponent = ps_cnum_NBI
     units = " "
     label = "information on source of beam power & voltage data"
     nitems = 1
     item_names(1) = "NBI_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%NBI_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nebar
  IF(hcheck(1,1)) THEN
     listname = "NEBAR"
     icomponent = ps_cnum_PLASMA
     units = "#/m^2"
     label = "line-averaged electron density"
     nitems = 1
     item_names(1) = "nebar"
     item_r8vals(1) = ss%nebar
     item_ivals(1) = 0
     item_c256(1) = " "
     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_nky_anomth
  IF(hcheck(1,1)) THEN
     listname = "NKY_ANOMTH"
     icomponent = ps_cnum_ANOM
     units = " "
     label = "number of points, ky grid ( anomalous data over ky spectrum)"
     nitems = 1
     item_names(1) = "nky_anomth"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%nky_anomth
     item_c256(1) = " "
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_nky_mmm
  IF(hcheck(1,1)) THEN
     listname = "NKY_MMM"
     icomponent = ps_cnum_ANOM
     units = " "
     label = "number of points, ky grid for MMM model"
     nitems = 1
     item_names(1) = "nky_mmm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%nky_mmm
     item_c256(1) = " "
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nmdifb
  IF(hcheck(1,1)) THEN
     listname = "NMDIFB"
     icomponent = ps_cnum_ANOM
     units = " "
     label = "type of fast ion diffusivity"
     nitems = 1
     item_names(1) = "nmdifb"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%nmdifb
     item_c256(1) = " "
     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nmodel_bdy
  IF(hcheck(1,1)) THEN
     listname = "NMODEL_BDY"
     icomponent = ps_cnum_ANOM
     units = "m^-3"
     label = "model density, boundary value"
     nitems = 1
     item_names(1) = "nmodel_bdy"
     item_r8vals(1) = ss%nmodel_bdy
     item_ivals(1) = 0
     item_c256(1) = " "
     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nmom
  IF(hcheck(1,1)) THEN
     listname = "NMOM"
     icomponent = ps_cnum_EQ
     units = " "
     label = "number of Fourier (R,Z) moments"
     nitems = 1
     item_names(1) = "nmom"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%nmom
     item_c256(1) = " "
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ns_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "NS_DATA_INFO"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "information on source of Density data"
     nitems = 1
     item_names(1) = "ns_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%ns_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ntf_coil2
  IF(hcheck(1,1)) THEN
     listname = "NTF_COIL2"
     icomponent = ps_cnum_RIPPLE
     units = " "
     label = "number of TF coils involved in inducing 2nd component"
     nitems = 1
     item_names(1) = "ntf_coil2"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%ntf_coil2
     item_c256(1) = " "
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ntf_coils
  IF(hcheck(1,1)) THEN
     listname = "NTF_COILS"
     icomponent = ps_cnum_RIPPLE
     units = " "
     label = "number of TF coils"
     nitems = 1
     item_names(1) = "ntf_coils"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%ntf_coils
     item_c256(1) = " "
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_num_rzlim
  IF(hcheck(1,1)) THEN
     listname = "NUM_RZLIM"
     icomponent = ps_cnum_EQ
     units = " "
     label = "number of points, axisymmetric (R,Z) limiter"
     nitems = 1
     item_names(1) = "num_rzlim"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%num_rzlim
     item_c256(1) = " "
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_n_D_einj_standard
  IF(hcheck(1,1)) THEN
     listname = "N_D_EINJ_STANDARD"
     icomponent = ps_cnum_NBI
     units = " "
     label = "no. pts in D beam energy fractions table"
     nitems = 1
     item_names(1) = "n_D_einj_standard"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%n_D_einj_standard
     item_c256(1) = " "
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_n_H_einj_standard
  IF(hcheck(1,1)) THEN
     listname = "N_H_EINJ_STANDARD"
     icomponent = ps_cnum_NBI
     units = " "
     label = "no. pts in H beam energy fractions table"
     nitems = 1
     item_names(1) = "n_H_einj_standard"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%n_H_einj_standard
     item_c256(1) = " "
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_n_T_einj_standard
  IF(hcheck(1,1)) THEN
     listname = "N_T_EINJ_STANDARD"
     icomponent = ps_cnum_NBI
     units = " "
     label = "no. pts in T beam energy fractions table"
     nitems = 1
     item_names(1) = "n_T_einj_standard"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%n_T_einj_standard
     item_c256(1) = " "
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_omegat_bdy
  IF(hcheck(1,1)) THEN
     listname = "OMEGAT_BDY"
     icomponent = ps_cnum_PLASMA
     units = "rad/sec"
     label = "toroidal angular velocity at/beyond boundary"
     nitems = 1
     item_names(1) = "omegat_bdy"
     item_r8vals(1) = ss%omegat_bdy
     item_ivals(1) = 0
     item_c256(1) = " "
     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_PEL_Code_Info
  IF(hcheck(1,1)) THEN
     listname = "PEL_CODE_INFO"
     icomponent = ps_cnum_PEL
     units = " "
     label = "Information: code implementing PELLET component"
     nitems = 1
     item_names(1) = "PEL_Code_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%PEL_Code_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_PEL_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "PEL_DATA_INFO"
     icomponent = ps_cnum_PEL
     units = " "
     label = "Information: on source of pellet injection"
     nitems = 1
     item_names(1) = "PEL_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%PEL_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_PLASMA_Code_Info
  IF(hcheck(1,1)) THEN
     listname = "PLASMA_CODE_INFO"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "Information: code managing PLASMA component"
     nitems = 1
     item_names(1) = "PLASMA_Code_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%PLASMA_Code_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Psi_to_machine_axis
  IF(hcheck(1,1)) THEN
     listname = "PSI_TO_MACHINE_AXIS"
     icomponent = ps_cnum_EQ
     units = "Wb/rad"
     label = "delta(Psi) from magnetic axis to machine axis"
     nitems = 1
     item_names(1) = "Psi_to_machine_axis"
     item_r8vals(1) = ss%Psi_to_machine_axis
     item_ivals(1) = 0
     item_c256(1) = " "
     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_RAD_Code_Info
  IF(hcheck(1,1)) THEN
     listname = "RAD_CODE_INFO"
     icomponent = ps_cnum_RAD
     units = " "
     label = "Information: code implementing RAD component"
     nitems = 1
     item_names(1) = "RAD_Code_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%RAD_Code_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_RAD_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "RAD_DATA_INFO"
     icomponent = ps_cnum_RAD
     units = " "
     label = "Information on source of radiated power data"
     nitems = 1
     item_names(1) = "RAD_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%RAD_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_rlim
  IF(hcheck(1,1)) THEN
     listname = "RLIM"
     enum_name = "RLIM__RVEC"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "R points in closed (R,Z) contour sequence"
     nitems = 1
     item_names(1) = "rlim"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%num_rzlim
     item_c256(1) = "__enumeration__"
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     IF(ALLOCATED(ss%rlim)) THEN
        CALL ps_enum_1d_store(ss%iobj,enum_name,icomponent, &
             ss%num_rzlim,ss%rlim,imod,iout,ierr)
        IF(ierr.NE.0) RETURN
     ENDIF
  ENDIF

  ahash(1) = ps_shash_RUNAWAY_Code_Info
  IF(hcheck(1,1)) THEN
     listname = "RUNAWAY_CODE_INFO"
     icomponent = ps_cnum_RUNAWAY
     units = " "
     label = "Information: code implementing RUNAWAY component"
     nitems = 1
     item_names(1) = "RUNAWAY_Code_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%RUNAWAY_Code_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_RunID
  IF(hcheck(1,1)) THEN
     listname = "RUNID"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "Run ID"
     nitems = 1
     item_names(1) = "RunID"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%RunID
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_shot_number
  IF(hcheck(1,1)) THEN
     listname = "SHOT_NUMBER"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "integer shot number"
     nitems = 1
     item_names(1) = "shot_number"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%shot_number
     item_c256(1) = " "
     CALL ps_list_store("SHOT_CONFIGURATION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_TF_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "TF_DATA_INFO"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "information on source of toroidal field data"
     nitems = 1
     item_names(1) = "TF_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%TF_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_tmodel_bdy
  IF(hcheck(1,1)) THEN
     listname = "TMODEL_BDY"
     icomponent = ps_cnum_ANOM
     units = "keV"
     label = "model temperature, boundary value"
     nitems = 1
     item_names(1) = "tmodel_bdy"
     item_r8vals(1) = ss%tmodel_bdy
     item_ivals(1) = 0
     item_c256(1) = " "
     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_tokamak_id
  IF(hcheck(1,1)) THEN
     listname = "TOKAMAK_ID"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "Tokamak (Machine Description) ID"
     nitems = 1
     item_names(1) = "tokamak_id"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%tokamak_id
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Ts_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "TS_DATA_INFO"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "information on source of Temperature data"
     nitems = 1
     item_names(1) = "Ts_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%Ts_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_T_einj_standard
  IF(hcheck(1,1)) THEN
     listname = "T_EINJ_STANDARD"
     enum_name = "T_EINJ_STANDARD__RVEC"
     icomponent = ps_cnum_NBI
     units = "keV"
     label = "T beam table energies"
     nitems = 1
     item_names(1) = "T_einj_standard"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%n_t_einj_standard
     item_c256(1) = "__enumeration__"
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     IF(ALLOCATED(ss%T_einj_standard)) THEN
        CALL ps_enum_1d_store(ss%iobj,enum_name,icomponent, &
             ss%n_t_einj_standard,ss%T_einj_standard,imod,iout,ierr)
        IF(ierr.NE.0) RETURN
     ENDIF
  ENDIF

  ahash(1) = ps_shash_T_ffull_standard
  IF(hcheck(1,1)) THEN
     listname = "T_FFULL_STANDARD"
     enum_name = "T_FFULL_STANDARD__RVEC"
     icomponent = ps_cnum_NBI
     units = "-"
     label = "T beam full energy fractions"
     nitems = 1
     item_names(1) = "T_ffull_standard"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%n_t_einj_standard
     item_c256(1) = "__enumeration__"
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     IF(ALLOCATED(ss%T_ffull_standard)) THEN
        CALL ps_enum_1d_store(ss%iobj,enum_name,icomponent, &
             ss%n_t_einj_standard,ss%T_ffull_standard,imod,iout,ierr)
        IF(ierr.NE.0) RETURN
     ENDIF
  ENDIF

  ahash(1) = ps_shash_T_fhalf_standard
  IF(hcheck(1,1)) THEN
     listname = "T_FHALF_STANDARD"
     enum_name = "T_FHALF_STANDARD__RVEC"
     icomponent = ps_cnum_NBI
     units = "-"
     label = "T beam half energy fractions"
     nitems = 1
     item_names(1) = "T_fhalf_standard"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%n_t_einj_standard
     item_c256(1) = "__enumeration__"
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     IF(ALLOCATED(ss%T_fhalf_standard)) THEN
        CALL ps_enum_1d_store(ss%iobj,enum_name,icomponent, &
             ss%n_t_einj_standard,ss%T_fhalf_standard,imod,iout,ierr)
        IF(ierr.NE.0) RETURN
     ENDIF
  ENDIF

  ahash(1) = ps_shash_vpol_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "VPOL_DATA_INFO"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "information on source of poloidal velocity data"
     nitems = 1
     item_names(1) = "vpol_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%vpol_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_VSUR_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "VSUR_DATA_INFO"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "information on source of surface voltage data"
     nitems = 1
     item_names(1) = "VSUR_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%VSUR_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_vtor_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "VTOR_DATA_INFO"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "information on source of toroidal velocity data"
     nitems = 1
     item_names(1) = "vtor_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%vtor_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Z0max
  IF(hcheck(1,1)) THEN
     listname = "Z0MAX"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "maximum Z of non-impurity species"
     nitems = 1
     item_names(1) = "Z0max"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%Z0max
     item_c256(1) = " "
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ZEFF_Data_Info
  IF(hcheck(1,1)) THEN
     listname = "ZEFF_DATA_INFO"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "information on source of Zeff data"
     nitems = 1
     item_names(1) = "ZEFF_Data_Info"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%ZEFF_Data_Info
     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_zlim
  IF(hcheck(1,1)) THEN
     listname = "ZLIM"
     enum_name = "ZLIM__RVEC"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "Z points in closed (R,Z) contour sequence"
     nitems = 1
     item_names(1) = "zlim"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%num_rzlim
     item_c256(1) = "__enumeration__"
     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     IF(ALLOCATED(ss%zlim)) THEN
        CALL ps_enum_1d_store(ss%iobj,enum_name,icomponent, &
             ss%num_rzlim,ss%zlim,imod,iout,ierr)
        IF(ierr.NE.0) RETURN
     ENDIF
  ENDIF

CONTAINS

  LOGICAL FUNCTION hcheck(inum,idchk)
     INTEGER, intent(in) :: inum
     INTEGER, intent(in) :: idchk

     !  check all hash codes: set function value TRUE if any show changes.
     !  also set TRUE if idchk = 0

     INTEGER :: ia1,ia2,ii

     if(idchk.eq.0) then
        hcheck=.TRUE.
        return
     endif

     hcheck = .FALSE.
     DO ii=1,inum
        hcheck = .TRUE.
        ia1 = ahash(ii)
        ia2 = ia1 + 1
        IF(ss%hash_codes(ia1).NE.ss%new_hash_codes(ia1)) EXIT
        IF(ss%hash_codes(ia2).NE.ss%new_hash_codes(ia2)) EXIT
        IF(xhash(ia1).NE.ss%new_hash_codes(ia1)) EXIT
        IF(xhash(ia2).NE.ss%new_hash_codes(ia2)) EXIT
        hcheck = .FALSE.
     ENDDO
  END FUNCTION hcheck

  SUBROUTINE lefpack(str)
     CHARACTER*(*), intent(inout) :: str
     !  left pack non-blank characters in str

     INTEGER :: ii,jj
     ii=0
     DO jj=1,len(str)
        IF(str(jj:jj).NE.' ') THEN
           ii = ii + 1
           str(ii:ii) = str(jj:jj)
        ENDIF
     ENDDO
     str(ii+1:) = ' '
  END SUBROUTINE lefpack

END SUBROUTINE plasma_state_xload_singl
