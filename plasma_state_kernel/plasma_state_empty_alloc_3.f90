! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_empty_alloc_3(ss,iout,ierr)


  ! Allocate array data for which all dimensions are provided

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss  ! state containing array elements to be allocated
  INTEGER, INTENT(IN) :: iout
  INTEGER, INTENT(INOUT) :: ierr  ! completion code (not cleared,set on error)

  !-------------------
  INTEGER :: isize,jsize
  !-------------------


  !-----------------------------------------------
  ! allocate arrays...

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%grho2).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%grho2)
  ENDIF
  IF(ALLOCATED(ss%grho2)) then
     jsize = SIZE(ss%grho2)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: grho2 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%grho2(ss%nrho_eq_geo))
     ss%grho2 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%grho2b2i).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%grho2b2i)
  ENDIF
  IF(ALLOCATED(ss%grho2b2i)) then
     jsize = SIZE(ss%grho2b2i)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: grho2b2i already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%grho2b2i(ss%nrho_eq_geo))
     ss%grho2b2i = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%grho2r2i).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%grho2r2i)
  ENDIF
  IF(ALLOCATED(ss%grho2r2i)) then
     jsize = SIZE(ss%grho2r2i)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: grho2r2i already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%grho2r2i(ss%nrho_eq_geo))
     ss%grho2r2i = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%grho2r3i).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%grho2r3i)
  ENDIF
  IF(ALLOCATED(ss%grho2r3i)) then
     jsize = SIZE(ss%grho2r3i)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: grho2r3i already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%grho2r3i(ss%nrho_eq_geo))
     ss%grho2r3i = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%gri).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gri)
  ENDIF
  IF(ALLOCATED(ss%gri)) then
     jsize = SIZE(ss%gri)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gri already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gri(ss%nrho_eq_geo))
     ss%gri = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%grirhoi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%grirhoi)
  ENDIF
  IF(ALLOCATED(ss%grirhoi)) then
     jsize = SIZE(ss%grirhoi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: grirhoi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%grirhoi(ss%nrho_eq_geo))
     ss%grirhoi = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%growthrate_drbm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%growthrate_drbm)
  ENDIF
  IF(ALLOCATED(ss%growthrate_drbm)) then
     jsize = SIZE(ss%growthrate_drbm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: growthrate_drbm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%growthrate_drbm(ss%nrho_anomth))
     ss%growthrate_drbm = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%growthrate_etg).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%growthrate_etg)
  ENDIF
  IF(ALLOCATED(ss%growthrate_etg)) then
     jsize = SIZE(ss%growthrate_etg)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: growthrate_etg already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%growthrate_etg(ss%nrho_anomth))
     ss%growthrate_etg = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%growthrate_glf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%growthrate_glf)
  ENDIF
  IF(ALLOCATED(ss%growthrate_glf)) then
     jsize = SIZE(ss%growthrate_glf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: growthrate_glf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%growthrate_glf(ss%nrho_anomth))
     ss%growthrate_glf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%growthrate_max_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%growthrate_max_tglf)
  ENDIF
  IF(ALLOCATED(ss%growthrate_max_tglf)) then
     jsize = SIZE(ss%growthrate_max_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: growthrate_max_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%growthrate_max_tglf(ss%nrho_anomth))
     ss%growthrate_max_tglf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_mmm)
  IF(ALLOCATED(ss%growthrate_mmm71).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%growthrate_mmm71)
  ENDIF
  IF(ALLOCATED(ss%growthrate_mmm71)) then
     jsize = SIZE(ss%growthrate_mmm71)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: growthrate_mmm71 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%growthrate_mmm71(ss%nrho_anomth,ss%nky_mmm))
     ss%growthrate_mmm71 = 0.0_rspec
     ALLOCATE(ss%id_growthrate_mmm71(ss%nky_mmm))
     ss%id_growthrate_mmm71 = 0
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%growthrate_mtm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%growthrate_mtm)
  ENDIF
  IF(ALLOCATED(ss%growthrate_mtm)) then
     jsize = SIZE(ss%growthrate_mtm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: growthrate_mtm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%growthrate_mtm(ss%nrho_anomth))
     ss%growthrate_mtm = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%growthrate_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%growthrate_tglf)
  ENDIF
  IF(ALLOCATED(ss%growthrate_tglf)) then
     jsize = SIZE(ss%growthrate_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: growthrate_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%growthrate_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%growthrate_tglf = 0.0_rspec
     ALLOCATE(ss%id_growthrate_tglf(ss%nky_anomth))
     ss%id_growthrate_tglf = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%growthrate_vpar).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%growthrate_vpar)
  ENDIF
  IF(ALLOCATED(ss%growthrate_vpar)) then
     jsize = SIZE(ss%growthrate_vpar)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: growthrate_vpar already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%growthrate_vpar(ss%nrho_anomth,0:ss%nspec_th))
     ss%growthrate_vpar = 0.0_rspec
     ALLOCATE(ss%id_growthrate_vpar(0:ss%nspec_th))
     ss%id_growthrate_vpar = 0
  ENDIF

  isize = (ss%ngsc0)
  IF(ALLOCATED(ss%gs_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gs_name)
  ENDIF
  IF(ALLOCATED(ss%gs_name)) then
     jsize = SIZE(ss%gs_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gs_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gs_name(ss%ngsc0))
     ss%gs_name = " "
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%g_eq).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%g_eq)
  ENDIF
  IF(ALLOCATED(ss%g_eq)) then
     jsize = SIZE(ss%g_eq)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: g_eq already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%g_eq(ss%nrho_eq))
     ss%g_eq = 0.0_rspec
  ENDIF

  isize = (ss%nlhrf_src)
  IF(ALLOCATED(ss%hegrilh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%hegrilh)
  ENDIF
  IF(ALLOCATED(ss%hegrilh)) then
     jsize = SIZE(ss%hegrilh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: hegrilh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%hegrilh(ss%nlhrf_src))
     ss%hegrilh = 0.0_rspec
  ENDIF

  isize = (ss%ncoils)
  IF(ALLOCATED(ss%Hsize_coil).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Hsize_coil)
  ENDIF
  IF(ALLOCATED(ss%Hsize_coil)) then
     jsize = SIZE(ss%Hsize_coil)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Hsize_coil already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Hsize_coil(ss%ncoils))
     ss%Hsize_coil = 0.0_rspec
  ENDIF

  isize = (ss%n_h_einj_standard)
  IF(ALLOCATED(ss%H_einj_standard).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%H_einj_standard)
  ENDIF
  IF(ALLOCATED(ss%H_einj_standard)) then
     jsize = SIZE(ss%H_einj_standard)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: H_einj_standard already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%H_einj_standard(ss%n_h_einj_standard))
     ss%H_einj_standard = 0.0_rspec
  ENDIF

  isize = (ss%n_h_einj_standard)
  IF(ALLOCATED(ss%H_ffull_standard).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%H_ffull_standard)
  ENDIF
  IF(ALLOCATED(ss%H_ffull_standard)) then
     jsize = SIZE(ss%H_ffull_standard)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: H_ffull_standard already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%H_ffull_standard(ss%n_h_einj_standard))
     ss%H_ffull_standard = 0.0_rspec
  ENDIF

  isize = (ss%n_h_einj_standard)
  IF(ALLOCATED(ss%H_fhalf_standard).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%H_fhalf_standard)
  ENDIF
  IF(ALLOCATED(ss%H_fhalf_standard)) then
     jsize = SIZE(ss%H_fhalf_standard)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: H_fhalf_standard already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%H_fhalf_standard(ss%n_h_einj_standard))
     ss%H_fhalf_standard = 0.0_rspec
  ENDIF

  isize = (ss%ncoils)
  IF(ALLOCATED(ss%icircuit_coil).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%icircuit_coil)
  ENDIF
  IF(ALLOCATED(ss%icircuit_coil)) then
     jsize = SIZE(ss%icircuit_coil)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: icircuit_coil already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%icircuit_coil(ss%ncoils))
     ss%icircuit_coil = 0
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%icrf_src_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%icrf_src_name)
  ENDIF
  IF(ALLOCATED(ss%icrf_src_name)) then
     jsize = SIZE(ss%icrf_src_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: icrf_src_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%icrf_src_name(ss%nicrf_src))
     ss%icrf_src_name = " "
  ENDIF

  isize = (ss%max_num_nphi_vac)*(ss%nicrf_src)
  IF(ALLOCATED(ss%Imag_ant_coef).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Imag_ant_coef)
  ENDIF
  IF(ALLOCATED(ss%Imag_ant_coef)) then
     jsize = SIZE(ss%Imag_ant_coef)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Imag_ant_coef already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Imag_ant_coef(ss%max_num_nphi_vac,ss%nicrf_src))
     ss%Imag_ant_coef = 0.0_rspec
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%iota).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%iota)
  ENDIF
  IF(ALLOCATED(ss%iota)) then
     jsize = SIZE(ss%iota)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: iota already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%iota(ss%nrho))
     ss%iota = 0.0_rspec
  ENDIF

  isize = (ss%nspec_rfmin)
  IF(ALLOCATED(ss%isThermal).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%isThermal)
  ENDIF
  IF(ALLOCATED(ss%isThermal)) then
     jsize = SIZE(ss%isThermal)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: isThermal already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%isThermal(ss%nspec_rfmin))
     ss%isThermal = 0
  ENDIF

  isize = (ss%ngsc0)
  IF(ALLOCATED(ss%is_recycling).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%is_recycling)
  ENDIF
  IF(ALLOCATED(ss%is_recycling)) then
     jsize = SIZE(ss%is_recycling)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: is_recycling already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%is_recycling(ss%ngsc0))
     ss%is_recycling = 0
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%jdotb).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%jdotb)
  ENDIF
  IF(ALLOCATED(ss%jdotb)) then
     jsize = SIZE(ss%jdotb)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: jdotb already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%jdotb(ss%nrho_eq))
     ss%jdotb = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%kvolt_nbi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%kvolt_nbi)
  ENDIF
  IF(ALLOCATED(ss%kvolt_nbi)) then
     jsize = SIZE(ss%kvolt_nbi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: kvolt_nbi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%kvolt_nbi(ss%nbeam))
     ss%kvolt_nbi = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%kyrhos_drbm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%kyrhos_drbm)
  ENDIF
  IF(ALLOCATED(ss%kyrhos_drbm)) then
     jsize = SIZE(ss%kyrhos_drbm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: kyrhos_drbm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%kyrhos_drbm(ss%nrho_anomth))
     ss%kyrhos_drbm = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%kyrhos_etg).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%kyrhos_etg)
  ENDIF
  IF(ALLOCATED(ss%kyrhos_etg)) then
     jsize = SIZE(ss%kyrhos_etg)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: kyrhos_etg already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%kyrhos_etg(ss%nrho_anomth))
     ss%kyrhos_etg = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%kyrhos_mtm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%kyrhos_mtm)
  ENDIF
  IF(ALLOCATED(ss%kyrhos_mtm)) then
     jsize = SIZE(ss%kyrhos_mtm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: kyrhos_mtm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%kyrhos_mtm(ss%nrho_anomth))
     ss%kyrhos_mtm = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%kyspectrum_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%kyspectrum_tglf)
  ENDIF
  IF(ALLOCATED(ss%kyspectrum_tglf)) then
     jsize = SIZE(ss%kyspectrum_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: kyspectrum_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%kyspectrum_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%kyspectrum_tglf = 0.0_rspec
     ALLOCATE(ss%id_kyspectrum_tglf(ss%nky_anomth))
     ss%id_kyspectrum_tglf = 0
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%Lbscap).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Lbscap)
  ENDIF
  IF(ALLOCATED(ss%Lbscap)) then
     jsize = SIZE(ss%Lbscap)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Lbscap already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Lbscap(ss%nbeam))
     ss%Lbscap = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%Lbscap2).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Lbscap2)
  ENDIF
  IF(ALLOCATED(ss%Lbscap2)) then
     jsize = SIZE(ss%Lbscap2)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Lbscap2 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Lbscap2(ss%nbeam))
     ss%Lbscap2 = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%Lbsctan).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Lbsctan)
  ENDIF
  IF(ALLOCATED(ss%Lbsctan)) then
     jsize = SIZE(ss%Lbsctan)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Lbsctan already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Lbsctan(ss%nbeam))
     ss%Lbsctan = 0.0_rspec
  ENDIF

  isize = (ss%nlhrf_src)
  IF(ALLOCATED(ss%lhrf_src_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%lhrf_src_name)
  ENDIF
  IF(ALLOCATED(ss%lhrf_src_name)) then
     jsize = SIZE(ss%lhrf_src_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: lhrf_src_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%lhrf_src_name(ss%nlhrf_src))
     ss%lhrf_src_name = " "
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%Lpol).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Lpol)
  ENDIF
  IF(ALLOCATED(ss%Lpol)) then
     jsize = SIZE(ss%Lpol)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Lpol already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Lpol(ss%nrho_eq))
     ss%Lpol = 0.0_rspec
  ENDIF

  isize = (ss%nballoon)
  IF(ALLOCATED(ss%Mercier_stable).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Mercier_stable)
  ENDIF
  IF(ALLOCATED(ss%Mercier_stable)) then
     jsize = SIZE(ss%Mercier_stable)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Mercier_stable already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Mercier_stable(ss%nballoon))
     ss%Mercier_stable = 0
  ENDIF

  isize = (ss%nspec_all+1)
  IF(ss%nspec_all.LE.0) isize = 0
  IF(ALLOCATED(ss%m_ALL).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%m_ALL)
  ENDIF
  IF(ALLOCATED(ss%m_ALL)) then
     jsize = SIZE(ss%m_ALL)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: m_ALL already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%m_ALL(0:ss%nspec_all))
     ss%m_ALL = 0.0_rspec
  ENDIF

  isize = (ss%nspec_alla+1)
  IF(ss%nspec_alla.LE.0) isize = 0
  IF(ALLOCATED(ss%m_ALLA).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%m_ALLA)
  ENDIF
  IF(ALLOCATED(ss%m_ALLA)) then
     jsize = SIZE(ss%m_ALLA)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: m_ALLA already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%m_ALLA(0:ss%nspec_alla))
     ss%m_ALLA = 0.0_rspec
  ENDIF

  isize = (ss%nspec_rfmin)
  IF(ALLOCATED(ss%m_RFMIN).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%m_RFMIN)
  ENDIF
  IF(ALLOCATED(ss%m_RFMIN)) then
     jsize = SIZE(ss%m_RFMIN)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: m_RFMIN already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%m_RFMIN(ss%nspec_rfmin))
     ss%m_RFMIN = 0.0_rspec
  ENDIF

  isize = (ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%m_S).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%m_S)
  ENDIF
  IF(ALLOCATED(ss%m_S)) then
     jsize = SIZE(ss%m_S)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: m_S already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%m_S(0:ss%nspec_th))
     ss%m_S = 0.0_rspec
  ENDIF

  isize = (ss%nspec_tha+1)
  IF(ss%nspec_tha.LE.0) isize = 0
  IF(ALLOCATED(ss%m_SA).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%m_SA)
  ENDIF
  IF(ALLOCATED(ss%m_SA)) then
     jsize = SIZE(ss%m_SA)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: m_SA already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%m_SA(0:ss%nspec_tha))
     ss%m_SA = 0.0_rspec
  ENDIF

  isize = (ss%nspec_fusion)
  IF(ALLOCATED(ss%m_SFUS).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%m_SFUS)
  ENDIF
  IF(ALLOCATED(ss%m_SFUS)) then
     jsize = SIZE(ss%m_SFUS)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: m_SFUS already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%m_SFUS(ss%nspec_fusion))
     ss%m_SFUS = 0.0_rspec
  ENDIF

  isize = (ss%nspec_gas)
  IF(ALLOCATED(ss%m_SGAS).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%m_SGAS)
  ENDIF
  IF(ALLOCATED(ss%m_SGAS)) then
     jsize = SIZE(ss%m_SGAS)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: m_SGAS already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%m_SGAS(ss%nspec_gas))
     ss%m_SGAS = 0.0_rspec
  ENDIF

  isize = (ss%nspec_imp0)
  IF(ALLOCATED(ss%m_SIMP0).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%m_SIMP0)
  ENDIF
  IF(ALLOCATED(ss%m_SIMP0)) then
     jsize = SIZE(ss%m_SIMP0)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: m_SIMP0 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%m_SIMP0(ss%nspec_imp0))
     ss%m_SIMP0 = 0.0_rspec
  ENDIF

  isize = (ss%nspec_impi)
  IF(ALLOCATED(ss%m_SIMPI).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%m_SIMPI)
  ENDIF
  IF(ALLOCATED(ss%m_SIMPI)) then
     jsize = SIZE(ss%m_SIMPI)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: m_SIMPI already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%m_SIMPI(ss%nspec_impi))
     ss%m_SIMPI = 0.0_rspec
  ENDIF

  isize = (ss%nspec_beam)
  IF(ALLOCATED(ss%m_SNBI).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%m_SNBI)
  ENDIF
  IF(ALLOCATED(ss%m_SNBI)) then
     jsize = SIZE(ss%m_SNBI)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: m_SNBI already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%m_SNBI(ss%nspec_beam))
     ss%m_SNBI = 0.0_rspec
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%nspec_gas)*(ss%ngsc0)
  IF(ALLOCATED(ss%n0norm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%n0norm)
  ENDIF
  IF(ALLOCATED(ss%n0norm)) then
     jsize = SIZE(ss%n0norm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: n0norm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%n0norm(ss%nrho_gas-1,ss%nspec_gas,ss%ngsc0))
     ss%n0norm = 0.0_rspec
     ALLOCATE(ss%id_n0norm(ss%nspec_gas,ss%ngsc0))
     ss%id_n0norm = 0
  ENDIF

  isize = (ss%nrho_nbi-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%n0_halo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%n0_halo)
  ENDIF
  IF(ALLOCATED(ss%n0_halo)) then
     jsize = SIZE(ss%n0_halo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: n0_halo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%n0_halo(ss%nrho_nbi-1,ss%nspec_gas))
     ss%n0_halo = 0.0_rspec
     ALLOCATE(ss%id_n0_halo(ss%nspec_gas))
     ss%id_n0_halo = 0
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%n0_reco).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%n0_reco)
  ENDIF
  IF(ALLOCATED(ss%n0_reco)) then
     jsize = SIZE(ss%n0_reco)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: n0_reco already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%n0_reco(ss%nrho_gas-1,ss%nspec_gas))
     ss%n0_reco = 0.0_rspec
     ALLOCATE(ss%id_n0_reco(ss%nspec_gas))
     ss%id_n0_reco = 0
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%nbap2_shape).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nbap2_shape)
  ENDIF
  IF(ALLOCATED(ss%nbap2_shape)) then
     jsize = SIZE(ss%nbap2_shape)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nbap2_shape already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nbap2_shape(ss%nbeam))
     ss%nbap2_shape = " "
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%nbap_shape).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nbap_shape)
  ENDIF
  IF(ALLOCATED(ss%nbap_shape)) then
     jsize = SIZE(ss%nbap_shape)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nbap_shape already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nbap_shape(ss%nbeam))
     ss%nbap_shape = " "
  ENDIF

END SUBROUTINE plasma_state_empty_alloc_3
