! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_empty_alloc_1(ss,iout,ierr)


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

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chii_ntm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chii_ntm)
  ENDIF
  IF(ALLOCATED(ss%chii_ntm)) then
     jsize = SIZE(ss%chii_ntm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chii_ntm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chii_ntm(ss%nrho_anomth))
     ss%chii_ntm = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chii_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chii_tglf)
  ENDIF
  IF(ALLOCATED(ss%chii_tglf)) then
     jsize = SIZE(ss%chii_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chii_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chii_tglf(ss%nrho_anomth))
     ss%chii_tglf = 0.0_rspec
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%chii_trans).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chii_trans)
  ENDIF
  IF(ALLOCATED(ss%chii_trans)) then
     jsize = SIZE(ss%chii_trans)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chii_trans already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chii_trans(ss%nrho))
     ss%chii_trans = 0.0_rspec
  ENDIF

  isize = (ss%nrho)*(ss%ntrcat)
  IF(ALLOCATED(ss%chii_transcat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chii_transcat)
  ENDIF
  IF(ALLOCATED(ss%chii_transcat)) then
     jsize = SIZE(ss%chii_transcat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chii_transcat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chii_transcat(ss%nrho,ss%ntrcat))
     ss%chii_transcat = 0.0_rspec
     ALLOCATE(ss%id_chii_transcat(ss%ntrcat))
     ss%id_chii_transcat = 0
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chii_w19).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chii_w19)
  ENDIF
  IF(ALLOCATED(ss%chii_w19)) then
     jsize = SIZE(ss%chii_w19)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chii_w19 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chii_w19(ss%nrho_anomth))
     ss%chii_w19 = 0.0_rspec
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%chimo_trans).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chimo_trans)
  ENDIF
  IF(ALLOCATED(ss%chimo_trans)) then
     jsize = SIZE(ss%chimo_trans)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chimo_trans already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chimo_trans(ss%nrho))
     ss%chimo_trans = 0.0_rspec
  ENDIF

  isize = (ss%nrho)*(ss%ntrcat)
  IF(ALLOCATED(ss%chimo_transcat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chimo_transcat)
  ENDIF
  IF(ALLOCATED(ss%chimo_transcat)) then
     jsize = SIZE(ss%chimo_transcat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chimo_transcat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chimo_transcat(ss%nrho,ss%ntrcat))
     ss%chimo_transcat = 0.0_rspec
     ALLOCATE(ss%id_chimo_transcat(ss%ntrcat))
     ss%id_chimo_transcat = 0
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chiphi_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chiphi_anom)
  ENDIF
  IF(ALLOCATED(ss%chiphi_anom)) then
     jsize = SIZE(ss%chiphi_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chiphi_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chiphi_anom(ss%nrho_anomth))
     ss%chiphi_anom = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chiphi_drbm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chiphi_drbm)
  ENDIF
  IF(ALLOCATED(ss%chiphi_drbm)) then
     jsize = SIZE(ss%chiphi_drbm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chiphi_drbm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chiphi_drbm(ss%nrho_anomth))
     ss%chiphi_drbm = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chiphi_glf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chiphi_glf)
  ENDIF
  IF(ALLOCATED(ss%chiphi_glf)) then
     jsize = SIZE(ss%chiphi_glf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chiphi_glf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chiphi_glf(ss%nrho_anomth))
     ss%chiphi_glf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chiphi_gtcneo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chiphi_gtcneo)
  ENDIF
  IF(ALLOCATED(ss%chiphi_gtcneo)) then
     jsize = SIZE(ss%chiphi_gtcneo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chiphi_gtcneo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chiphi_gtcneo(ss%nrho_anomth))
     ss%chiphi_gtcneo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chiphi_mmm71).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chiphi_mmm71)
  ENDIF
  IF(ALLOCATED(ss%chiphi_mmm71)) then
     jsize = SIZE(ss%chiphi_mmm71)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chiphi_mmm71 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chiphi_mmm71(ss%nrho_anomth))
     ss%chiphi_mmm71 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chiphi_neo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chiphi_neo)
  ENDIF
  IF(ALLOCATED(ss%chiphi_neo)) then
     jsize = SIZE(ss%chiphi_neo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chiphi_neo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chiphi_neo(ss%nrho_anomth))
     ss%chiphi_neo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chiphi_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chiphi_tglf)
  ENDIF
  IF(ALLOCATED(ss%chiphi_tglf)) then
     jsize = SIZE(ss%chiphi_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chiphi_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chiphi_tglf(ss%nrho_anomth))
     ss%chiphi_tglf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chiphi_w19).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chiphi_w19)
  ENDIF
  IF(ALLOCATED(ss%chiphi_w19)) then
     jsize = SIZE(ss%chiphi_w19)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chiphi_w19 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chiphi_w19(ss%nrho_anomth))
     ss%chiphi_w19 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chitheta_mmm71).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chitheta_mmm71)
  ENDIF
  IF(ALLOCATED(ss%chitheta_mmm71)) then
     jsize = SIZE(ss%chitheta_mmm71)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chitheta_mmm71 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chitheta_mmm71(ss%nrho_anomth))
     ss%chitheta_mmm71 = 0.0_rspec
  ENDIF

  isize = (ss%ncircuits)
  IF(ALLOCATED(ss%circuit_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%circuit_name)
  ENDIF
  IF(ALLOCATED(ss%circuit_name)) then
     jsize = SIZE(ss%circuit_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: circuit_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%circuit_name(ss%ncircuits))
     ss%circuit_name = " "
  ENDIF

  isize = (ss%ncircuits)
  IF(ALLOCATED(ss%coil_apt).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%coil_apt)
  ENDIF
  IF(ALLOCATED(ss%coil_apt)) then
     jsize = SIZE(ss%coil_apt)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: coil_apt already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%coil_apt(ss%ncircuits))
     ss%coil_apt = 0.0_rspec
  ENDIF

  isize = (ss%ncoils)
  IF(ALLOCATED(ss%coil_in_circuit).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%coil_in_circuit)
  ENDIF
  IF(ALLOCATED(ss%coil_in_circuit)) then
     jsize = SIZE(ss%coil_in_circuit)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: coil_in_circuit already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%coil_in_circuit(ss%ncoils))
     ss%coil_in_circuit = " "
  ENDIF

  isize = (ss%ncoils)
  IF(ALLOCATED(ss%coil_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%coil_name)
  ENDIF
  IF(ALLOCATED(ss%coil_name)) then
     jsize = SIZE(ss%coil_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: coil_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%coil_name(ss%ncoils))
     ss%coil_name = " "
  ENDIF

  isize = (ss%ncoils)
  IF(ALLOCATED(ss%coil_resispt).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%coil_resispt)
  ENDIF
  IF(ALLOCATED(ss%coil_resispt)) then
     jsize = SIZE(ss%coil_resispt)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: coil_resispt already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%coil_resispt(ss%ncoils))
     ss%coil_resispt = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%curbeam).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%curbeam)
  ENDIF
  IF(ALLOCATED(ss%curbeam)) then
     jsize = SIZE(ss%curbeam)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: curbeam already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%curbeam(ss%nrho_nbi-1))
     ss%curbeam = 0.0_rspec
  ENDIF

  isize = (ss%nrho_ecrf-1)
  IF(ALLOCATED(ss%curech).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%curech)
  ENDIF
  IF(ALLOCATED(ss%curech)) then
     jsize = SIZE(ss%curech)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: curech already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%curech(ss%nrho_ecrf-1))
     ss%curech = 0.0_rspec
  ENDIF

  isize = (ss%nrho_ecrf-1)*(ss%necrf_src)
  IF(ALLOCATED(ss%curech_src).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%curech_src)
  ENDIF
  IF(ALLOCATED(ss%curech_src)) then
     jsize = SIZE(ss%curech_src)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: curech_src already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%curech_src(ss%nrho_ecrf-1,ss%necrf_src))
     ss%curech_src = 0.0_rspec
     ALLOCATE(ss%id_curech_src(ss%necrf_src))
     ss%id_curech_src = 0
  ENDIF

  isize = (ss%nrho_fus-1)
  IF(ALLOCATED(ss%curfusn).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%curfusn)
  ENDIF
  IF(ALLOCATED(ss%curfusn)) then
     jsize = SIZE(ss%curfusn)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: curfusn already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%curfusn(ss%nrho_fus-1))
     ss%curfusn = 0.0_rspec
  ENDIF

  isize = (ss%nrho_icrf-1)
  IF(ALLOCATED(ss%curich).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%curich)
  ENDIF
  IF(ALLOCATED(ss%curich)) then
     jsize = SIZE(ss%curich)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: curich already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%curich(ss%nrho_icrf-1))
     ss%curich = 0.0_rspec
  ENDIF

  isize = (ss%nrho_lhrf-1)
  IF(ALLOCATED(ss%curlh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%curlh)
  ENDIF
  IF(ALLOCATED(ss%curlh)) then
     jsize = SIZE(ss%curlh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: curlh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%curlh(ss%nrho_lhrf-1))
     ss%curlh = 0.0_rspec
  ENDIF

  isize = (ss%nrho_lhrf-1)*(ss%nlhrf_src)
  IF(ALLOCATED(ss%curlh_src).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%curlh_src)
  ENDIF
  IF(ALLOCATED(ss%curlh_src)) then
     jsize = SIZE(ss%curlh_src)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: curlh_src already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%curlh_src(ss%nrho_lhrf-1,ss%nlhrf_src))
     ss%curlh_src = 0.0_rspec
     ALLOCATE(ss%id_curlh_src(ss%nlhrf_src))
     ss%id_curlh_src = 0
  ENDIF

  isize = (ss%nrho_icrf-1)
  IF(ALLOCATED(ss%curmino).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%curmino)
  ENDIF
  IF(ALLOCATED(ss%curmino)) then
     jsize = SIZE(ss%curmino)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: curmino already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%curmino(ss%nrho_icrf-1))
     ss%curmino = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%curr_bootstrap).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%curr_bootstrap)
  ENDIF
  IF(ALLOCATED(ss%curr_bootstrap)) then
     jsize = SIZE(ss%curr_bootstrap)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: curr_bootstrap already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%curr_bootstrap(ss%nrho-1))
     ss%curr_bootstrap = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%curr_ohmic).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%curr_ohmic)
  ENDIF
  IF(ALLOCATED(ss%curr_ohmic)) then
     jsize = SIZE(ss%curr_ohmic)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: curr_ohmic already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%curr_ohmic(ss%nrho-1))
     ss%curr_ohmic = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%curt).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%curt)
  ENDIF
  IF(ALLOCATED(ss%curt)) then
     jsize = SIZE(ss%curt)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: curt already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%curt(ss%nrho_eq))
     ss%curt = 0.0_rspec
  ENDIF

  isize = (ss%nrho_rw-1)
  IF(ALLOCATED(ss%cur_rw).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%cur_rw)
  ENDIF
  IF(ALLOCATED(ss%cur_rw)) then
     jsize = SIZE(ss%cur_rw)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: cur_rw already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%cur_rw(ss%nrho_rw-1))
     ss%cur_rw = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)*(ss%nth_eq)
  IF(ALLOCATED(ss%d2R_geo_dRHOdTH).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%d2R_geo_dRHOdTH)
  ENDIF
  IF(ALLOCATED(ss%d2R_geo_dRHOdTH)) then
     jsize = SIZE(ss%d2R_geo_dRHOdTH)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: d2R_geo_dRHOdTH already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%d2R_geo_dRHOdTH(ss%nrho_eq,ss%nth_eq))
     ss%d2R_geo_dRHOdTH = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)*(ss%nth_eq)
  IF(ALLOCATED(ss%d2Z_geo_dRHOdTH).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%d2Z_geo_dRHOdTH)
  ENDIF
  IF(ALLOCATED(ss%d2Z_geo_dRHOdTH)) then
     jsize = SIZE(ss%d2Z_geo_dRHOdTH)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: d2Z_geo_dRHOdTH already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%d2Z_geo_dRHOdTH(ss%nrho_eq,ss%nth_eq))
     ss%d2Z_geo_dRHOdTH = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%db_mtm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%db_mtm)
  ENDIF
  IF(ALLOCATED(ss%db_mtm)) then
     jsize = SIZE(ss%db_mtm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: db_mtm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%db_mtm(ss%nrho_anomth))
     ss%db_mtm = 0.0_rspec
  ENDIF

  isize = (ss%nrho_rw-1)
  IF(ALLOCATED(ss%dcur_rw_dVloop).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dcur_rw_dVloop)
  ENDIF
  IF(ALLOCATED(ss%dcur_rw_dVloop)) then
     jsize = SIZE(ss%dcur_rw_dVloop)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dcur_rw_dVloop already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dcur_rw_dVloop(ss%nrho_rw-1))
     ss%dcur_rw_dVloop = 0.0_rspec
  ENDIF

  isize = (ss%ndelta_w)
  IF(ALLOCATED(ss%DeltaW_code).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%DeltaW_code)
  ENDIF
  IF(ALLOCATED(ss%DeltaW_code)) then
     jsize = SIZE(ss%DeltaW_code)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: DeltaW_code already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%DeltaW_code(ss%ndelta_w))
     ss%DeltaW_code = " "
  ENDIF

  isize = (ss%ndelta_w)
  IF(ALLOCATED(ss%DeltaW_mapper).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%DeltaW_mapper)
  ENDIF
  IF(ALLOCATED(ss%DeltaW_mapper)) then
     jsize = SIZE(ss%DeltaW_mapper)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: DeltaW_mapper already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%DeltaW_mapper(ss%ndelta_w))
     ss%DeltaW_mapper = " "
  ENDIF

  isize = (ss%mmodes)*(ss%ndelta_w)
  IF(ALLOCATED(ss%DeltaW_stable).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%DeltaW_stable)
  ENDIF
  IF(ALLOCATED(ss%DeltaW_stable)) then
     jsize = SIZE(ss%DeltaW_stable)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: DeltaW_stable already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%DeltaW_stable(ss%mmodes,ss%ndelta_w))
     ss%DeltaW_stable = 0
  ENDIF

  isize = (ss%ndelta_w)
  IF(ALLOCATED(ss%DeltaW_status).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%DeltaW_status)
  ENDIF
  IF(ALLOCATED(ss%DeltaW_status)) then
     jsize = SIZE(ss%DeltaW_status)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: DeltaW_status already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%DeltaW_status(ss%ndelta_w))
     ss%DeltaW_status = 0
  ENDIF

  isize = (ss%nrho_anom)
  IF(ALLOCATED(ss%difb_fusi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difb_fusi)
  ENDIF
  IF(ALLOCATED(ss%difb_fusi)) then
     jsize = SIZE(ss%difb_fusi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difb_fusi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difb_fusi(ss%nrho_anom))
     ss%difb_fusi = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anom)
  IF(ALLOCATED(ss%difb_nbi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difb_nbi)
  ENDIF
  IF(ALLOCATED(ss%difb_nbi)) then
     jsize = SIZE(ss%difb_nbi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difb_nbi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difb_nbi(ss%nrho_anom))
     ss%difb_nbi = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anom)
  IF(ALLOCATED(ss%difb_rfmi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difb_rfmi)
  ENDIF
  IF(ALLOCATED(ss%difb_rfmi)) then
     jsize = SIZE(ss%difb_rfmi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difb_rfmi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difb_rfmi(ss%nrho_anom))
     ss%difb_rfmi = 0.0_rspec
  ENDIF

  isize = (ss%nrho)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%diff_trans).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%diff_trans)
  ENDIF
  IF(ALLOCATED(ss%diff_trans)) then
     jsize = SIZE(ss%diff_trans)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: diff_trans already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%diff_trans(ss%nrho,0:ss%nspec_th))
     ss%diff_trans = 0.0_rspec
     ALLOCATE(ss%id_diff_trans(0:ss%nspec_th))
     ss%id_diff_trans = 0
  ENDIF

  isize = (ss%nrho)*(ss%nspec_th+1)*(ss%ntrcat)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%diff_transcat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%diff_transcat)
  ENDIF
  IF(ALLOCATED(ss%diff_transcat)) then
     jsize = SIZE(ss%diff_transcat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: diff_transcat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%diff_transcat(ss%nrho,0:ss%nspec_th,ss%ntrcat))
     ss%diff_transcat = 0.0_rspec
     ALLOCATE(ss%id_diff_transcat(0:ss%nspec_th,ss%ntrcat))
     ss%id_diff_transcat = 0
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%difne_nclass).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difne_nclass)
  ENDIF
  IF(ALLOCATED(ss%difne_nclass)) then
     jsize = SIZE(ss%difne_nclass)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difne_nclass already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difne_nclass(ss%nrho_anomth))
     ss%difne_nclass = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%difnh_nclass).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difnh_nclass)
  ENDIF
  IF(ALLOCATED(ss%difnh_nclass)) then
     jsize = SIZE(ss%difnh_nclass)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difnh_nclass already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difnh_nclass(ss%nrho_anomth))
     ss%difnh_nclass = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%difns_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difns_anom)
  ENDIF
  IF(ALLOCATED(ss%difns_anom)) then
     jsize = SIZE(ss%difns_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difns_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difns_anom(ss%nrho_anomth,0:ss%nspec_th))
     ss%difns_anom = 0.0_rspec
     ALLOCATE(ss%id_difns_anom(0:ss%nspec_th))
     ss%id_difns_anom = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%difns_drbm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difns_drbm)
  ENDIF
  IF(ALLOCATED(ss%difns_drbm)) then
     jsize = SIZE(ss%difns_drbm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difns_drbm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difns_drbm(ss%nrho_anomth,0:ss%nspec_th))
     ss%difns_drbm = 0.0_rspec
     ALLOCATE(ss%id_difns_drbm(0:ss%nspec_th))
     ss%id_difns_drbm = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%difns_glf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difns_glf)
  ENDIF
  IF(ALLOCATED(ss%difns_glf)) then
     jsize = SIZE(ss%difns_glf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difns_glf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difns_glf(ss%nrho_anomth,0:ss%nspec_th))
     ss%difns_glf = 0.0_rspec
     ALLOCATE(ss%id_difns_glf(0:ss%nspec_th))
     ss%id_difns_glf = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%difns_gtcneo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difns_gtcneo)
  ENDIF
  IF(ALLOCATED(ss%difns_gtcneo)) then
     jsize = SIZE(ss%difns_gtcneo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difns_gtcneo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difns_gtcneo(ss%nrho_anomth,0:ss%nspec_th))
     ss%difns_gtcneo = 0.0_rspec
     ALLOCATE(ss%id_difns_gtcneo(0:ss%nspec_th))
     ss%id_difns_gtcneo = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%difns_mmm71).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difns_mmm71)
  ENDIF
  IF(ALLOCATED(ss%difns_mmm71)) then
     jsize = SIZE(ss%difns_mmm71)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difns_mmm71 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difns_mmm71(ss%nrho_anomth,0:ss%nspec_th))
     ss%difns_mmm71 = 0.0_rspec
     ALLOCATE(ss%id_difns_mmm71(0:ss%nspec_th))
     ss%id_difns_mmm71 = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%difns_nclass).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difns_nclass)
  ENDIF
  IF(ALLOCATED(ss%difns_nclass)) then
     jsize = SIZE(ss%difns_nclass)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difns_nclass already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difns_nclass(ss%nrho_anomth,0:ss%nspec_th))
     ss%difns_nclass = 0.0_rspec
     ALLOCATE(ss%id_difns_nclass(0:ss%nspec_th))
     ss%id_difns_nclass = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%difns_neo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difns_neo)
  ENDIF
  IF(ALLOCATED(ss%difns_neo)) then
     jsize = SIZE(ss%difns_neo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difns_neo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difns_neo(ss%nrho_anomth,0:ss%nspec_th))
     ss%difns_neo = 0.0_rspec
     ALLOCATE(ss%id_difns_neo(0:ss%nspec_th))
     ss%id_difns_neo = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%difns_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difns_tglf)
  ENDIF
  IF(ALLOCATED(ss%difns_tglf)) then
     jsize = SIZE(ss%difns_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difns_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difns_tglf(ss%nrho_anomth,0:ss%nspec_th))
     ss%difns_tglf = 0.0_rspec
     ALLOCATE(ss%id_difns_tglf(0:ss%nspec_th))
     ss%id_difns_tglf = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%difns_w19).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difns_w19)
  ENDIF
  IF(ALLOCATED(ss%difns_w19)) then
     jsize = SIZE(ss%difns_w19)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difns_w19 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difns_w19(ss%nrho_anomth,0:ss%nspec_th))
     ss%difns_w19 = 0.0_rspec
     ALLOCATE(ss%id_difns_w19(0:ss%nspec_th))
     ss%id_difns_w19 = 0
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%difnx_nclass).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difnx_nclass)
  ENDIF
  IF(ALLOCATED(ss%difnx_nclass)) then
     jsize = SIZE(ss%difnx_nclass)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difnx_nclass already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difnx_nclass(ss%nrho_anomth))
     ss%difnx_nclass = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%difnz_mmm71).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%difnz_mmm71)
  ENDIF
  IF(ALLOCATED(ss%difnz_mmm71)) then
     jsize = SIZE(ss%difnz_mmm71)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: difnz_mmm71 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%difnz_mmm71(ss%nrho_anomth))
     ss%difnz_mmm71 = 0.0_rspec
  ENDIF

  isize = (ss%nspec_alla+1)
  IF(ss%nspec_alla.LE.0) isize = 0
  IF(ALLOCATED(ss%dist_fun).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dist_fun)
  ENDIF
  IF(ALLOCATED(ss%dist_fun)) then
     jsize = SIZE(ss%dist_fun)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dist_fun already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dist_fun(0:ss%nspec_alla))
     ss%dist_fun = " "
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%dQedGrdte_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dQedGrdte_tglf)
  ENDIF
  IF(ALLOCATED(ss%dQedGrdte_tglf)) then
     jsize = SIZE(ss%dQedGrdte_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dQedGrdte_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dQedGrdte_tglf(ss%nrho_anomth))
     ss%dQedGrdte_tglf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%dQedGrdti_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dQedGrdti_tglf)
  ENDIF
  IF(ALLOCATED(ss%dQedGrdti_tglf)) then
     jsize = SIZE(ss%dQedGrdti_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dQedGrdti_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dQedGrdti_tglf(ss%nrho_anomth))
     ss%dQedGrdti_tglf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%dQidGrdte_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dQidGrdte_tglf)
  ENDIF
  IF(ALLOCATED(ss%dQidGrdte_tglf)) then
     jsize = SIZE(ss%dQidGrdte_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dQidGrdte_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dQidGrdte_tglf(ss%nrho_anomth))
     ss%dQidGrdte_tglf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%dQidGrdti_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dQidGrdti_tglf)
  ENDIF
  IF(ALLOCATED(ss%dQidGrdti_tglf)) then
     jsize = SIZE(ss%dQidGrdti_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dQidGrdti_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dQidGrdti_tglf(ss%nrho_anomth))
     ss%dQidGrdti_tglf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%dR0_momeq_dRHO).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dR0_momeq_dRHO)
  ENDIF
  IF(ALLOCATED(ss%dR0_momeq_dRHO)) then
     jsize = SIZE(ss%dR0_momeq_dRHO)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dR0_momeq_dRHO already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dR0_momeq_dRHO(ss%nrho_eq))
     ss%dR0_momeq_dRHO = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)*(ss%nth_eq)
  IF(ALLOCATED(ss%dR_geo_dRHO).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dR_geo_dRHO)
  ENDIF
  IF(ALLOCATED(ss%dR_geo_dRHO)) then
     jsize = SIZE(ss%dR_geo_dRHO)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dR_geo_dRHO already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dR_geo_dRHO(ss%nrho_eq,ss%nth_eq))
     ss%dR_geo_dRHO = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)*(ss%nth_eq)
  IF(ALLOCATED(ss%dR_geo_dTH).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dR_geo_dTH)
  ENDIF
  IF(ALLOCATED(ss%dR_geo_dTH)) then
     jsize = SIZE(ss%dR_geo_dTH)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dR_geo_dTH already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dR_geo_dTH(ss%nrho_eq,ss%nth_eq))
     ss%dR_geo_dTH = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)*(ss%neqmom)
  IF(ALLOCATED(ss%dxRjcos_momeq_dRHO).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dxRjcos_momeq_dRHO)
  ENDIF
  IF(ALLOCATED(ss%dxRjcos_momeq_dRHO)) then
     jsize = SIZE(ss%dxRjcos_momeq_dRHO)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dxRjcos_momeq_dRHO already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dxRjcos_momeq_dRHO(ss%nrho_eq,ss%neqmom))
     ss%dxRjcos_momeq_dRHO = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)*(ss%neqmom)
  IF(ALLOCATED(ss%dxRjsin_momeq_dRHO).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dxRjsin_momeq_dRHO)
  ENDIF
  IF(ALLOCATED(ss%dxRjsin_momeq_dRHO)) then
     jsize = SIZE(ss%dxRjsin_momeq_dRHO)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dxRjsin_momeq_dRHO already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dxRjsin_momeq_dRHO(ss%nrho_eq,ss%neqmom))
     ss%dxRjsin_momeq_dRHO = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)*(ss%neqmom)
  IF(ALLOCATED(ss%dxZjcos_momeq_dRHO).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dxZjcos_momeq_dRHO)
  ENDIF
  IF(ALLOCATED(ss%dxZjcos_momeq_dRHO)) then
     jsize = SIZE(ss%dxZjcos_momeq_dRHO)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dxZjcos_momeq_dRHO already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dxZjcos_momeq_dRHO(ss%nrho_eq,ss%neqmom))
     ss%dxZjcos_momeq_dRHO = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)*(ss%neqmom)
  IF(ALLOCATED(ss%dxZjsin_momeq_dRHO).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dxZjsin_momeq_dRHO)
  ENDIF
  IF(ALLOCATED(ss%dxZjsin_momeq_dRHO)) then
     jsize = SIZE(ss%dxZjsin_momeq_dRHO)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dxZjsin_momeq_dRHO already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dxZjsin_momeq_dRHO(ss%nrho_eq,ss%neqmom))
     ss%dxZjsin_momeq_dRHO = 0.0_rspec
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%dx_fshield).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dx_fshield)
  ENDIF
  IF(ALLOCATED(ss%dx_fshield)) then
     jsize = SIZE(ss%dx_fshield)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dx_fshield already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dx_fshield(ss%nicrf_src))
     ss%dx_fshield = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%dZ0_momeq_dRHO).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dZ0_momeq_dRHO)
  ENDIF
  IF(ALLOCATED(ss%dZ0_momeq_dRHO)) then
     jsize = SIZE(ss%dZ0_momeq_dRHO)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dZ0_momeq_dRHO already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dZ0_momeq_dRHO(ss%nrho_eq))
     ss%dZ0_momeq_dRHO = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)*(ss%nth_eq)
  IF(ALLOCATED(ss%dZ_geo_dRHO).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dZ_geo_dRHO)
  ENDIF
  IF(ALLOCATED(ss%dZ_geo_dRHO)) then
     jsize = SIZE(ss%dZ_geo_dRHO)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dZ_geo_dRHO already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dZ_geo_dRHO(ss%nrho_eq,ss%nth_eq))
     ss%dZ_geo_dRHO = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)*(ss%nth_eq)
  IF(ALLOCATED(ss%dZ_geo_dTH).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%dZ_geo_dTH)
  ENDIF
  IF(ALLOCATED(ss%dZ_geo_dTH)) then
     jsize = SIZE(ss%dZ_geo_dTH)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: dZ_geo_dTH already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%dZ_geo_dTH(ss%nrho_eq,ss%nth_eq))
     ss%dZ_geo_dTH = 0.0_rspec
  ENDIF

END SUBROUTINE plasma_state_empty_alloc_1
