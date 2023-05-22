! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_empty_alloc_9(ss,iout,ierr)


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
  IF(ALLOCATED(ss%velpphi_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velpphi_tglf)
  ENDIF
  IF(ALLOCATED(ss%velpphi_tglf)) then
     jsize = SIZE(ss%velpphi_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velpphi_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velpphi_tglf(ss%nrho_anomth))
     ss%velpphi_tglf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velte_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velte_anom)
  ENDIF
  IF(ALLOCATED(ss%velte_anom)) then
     jsize = SIZE(ss%velte_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velte_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velte_anom(ss%nrho_anomth))
     ss%velte_anom = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velte_glf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velte_glf)
  ENDIF
  IF(ALLOCATED(ss%velte_glf)) then
     jsize = SIZE(ss%velte_glf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velte_glf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velte_glf(ss%nrho_anomth))
     ss%velte_glf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velte_gtcneo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velte_gtcneo)
  ENDIF
  IF(ALLOCATED(ss%velte_gtcneo)) then
     jsize = SIZE(ss%velte_gtcneo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velte_gtcneo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velte_gtcneo(ss%nrho_anomth))
     ss%velte_gtcneo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velte_mmm71).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velte_mmm71)
  ENDIF
  IF(ALLOCATED(ss%velte_mmm71)) then
     jsize = SIZE(ss%velte_mmm71)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velte_mmm71 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velte_mmm71(ss%nrho_anomth))
     ss%velte_mmm71 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velte_nclass).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velte_nclass)
  ENDIF
  IF(ALLOCATED(ss%velte_nclass)) then
     jsize = SIZE(ss%velte_nclass)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velte_nclass already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velte_nclass(ss%nrho_anomth))
     ss%velte_nclass = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velte_neo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velte_neo)
  ENDIF
  IF(ALLOCATED(ss%velte_neo)) then
     jsize = SIZE(ss%velte_neo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velte_neo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velte_neo(ss%nrho_anomth))
     ss%velte_neo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velte_paleo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velte_paleo)
  ENDIF
  IF(ALLOCATED(ss%velte_paleo)) then
     jsize = SIZE(ss%velte_paleo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velte_paleo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velte_paleo(ss%nrho_anomth))
     ss%velte_paleo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velte_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velte_tglf)
  ENDIF
  IF(ALLOCATED(ss%velte_tglf)) then
     jsize = SIZE(ss%velte_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velte_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velte_tglf(ss%nrho_anomth))
     ss%velte_tglf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velti_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velti_anom)
  ENDIF
  IF(ALLOCATED(ss%velti_anom)) then
     jsize = SIZE(ss%velti_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velti_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velti_anom(ss%nrho_anomth))
     ss%velti_anom = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velti_glf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velti_glf)
  ENDIF
  IF(ALLOCATED(ss%velti_glf)) then
     jsize = SIZE(ss%velti_glf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velti_glf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velti_glf(ss%nrho_anomth))
     ss%velti_glf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velti_gtcneo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velti_gtcneo)
  ENDIF
  IF(ALLOCATED(ss%velti_gtcneo)) then
     jsize = SIZE(ss%velti_gtcneo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velti_gtcneo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velti_gtcneo(ss%nrho_anomth))
     ss%velti_gtcneo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velti_mmm71).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velti_mmm71)
  ENDIF
  IF(ALLOCATED(ss%velti_mmm71)) then
     jsize = SIZE(ss%velti_mmm71)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velti_mmm71 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velti_mmm71(ss%nrho_anomth))
     ss%velti_mmm71 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velti_nclass).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velti_nclass)
  ENDIF
  IF(ALLOCATED(ss%velti_nclass)) then
     jsize = SIZE(ss%velti_nclass)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velti_nclass already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velti_nclass(ss%nrho_anomth))
     ss%velti_nclass = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velti_neo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velti_neo)
  ENDIF
  IF(ALLOCATED(ss%velti_neo)) then
     jsize = SIZE(ss%velti_neo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velti_neo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velti_neo(ss%nrho_anomth))
     ss%velti_neo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velti_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velti_tglf)
  ENDIF
  IF(ALLOCATED(ss%velti_tglf)) then
     jsize = SIZE(ss%velti_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velti_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velti_tglf(ss%nrho_anomth))
     ss%velti_tglf = 0.0_rspec
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%vie_trans).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vie_trans)
  ENDIF
  IF(ALLOCATED(ss%vie_trans)) then
     jsize = SIZE(ss%vie_trans)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vie_trans already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vie_trans(ss%nrho))
     ss%vie_trans = 0.0_rspec
  ENDIF

  isize = (ss%nrho)*(ss%ntrcat)
  IF(ALLOCATED(ss%vie_transcat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vie_transcat)
  ENDIF
  IF(ALLOCATED(ss%vie_transcat)) then
     jsize = SIZE(ss%vie_transcat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vie_transcat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vie_transcat(ss%nrho,ss%ntrcat))
     ss%vie_transcat = 0.0_rspec
     ALLOCATE(ss%id_vie_transcat(ss%ntrcat))
     ss%id_vie_transcat = 0
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%vmo_trans).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vmo_trans)
  ENDIF
  IF(ALLOCATED(ss%vmo_trans)) then
     jsize = SIZE(ss%vmo_trans)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vmo_trans already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vmo_trans(ss%nrho))
     ss%vmo_trans = 0.0_rspec
  ENDIF

  isize = (ss%nrho)*(ss%ntrcat)
  IF(ALLOCATED(ss%vmo_transcat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vmo_transcat)
  ENDIF
  IF(ALLOCATED(ss%vmo_transcat)) then
     jsize = SIZE(ss%vmo_transcat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vmo_transcat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vmo_transcat(ss%nrho,ss%ntrcat))
     ss%vmo_transcat = 0.0_rspec
     ALLOCATE(ss%id_vmo_transcat(ss%ntrcat))
     ss%id_vmo_transcat = 0
  ENDIF

  isize = (ss%nrho)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%vn_trans).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vn_trans)
  ENDIF
  IF(ALLOCATED(ss%vn_trans)) then
     jsize = SIZE(ss%vn_trans)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vn_trans already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vn_trans(ss%nrho,0:ss%nspec_th))
     ss%vn_trans = 0.0_rspec
     ALLOCATE(ss%id_vn_trans(0:ss%nspec_th))
     ss%id_vn_trans = 0
  ENDIF

  isize = (ss%nrho)*(ss%nspec_th+1)*(ss%ntrcat)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%vn_transcat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vn_transcat)
  ENDIF
  IF(ALLOCATED(ss%vn_transcat)) then
     jsize = SIZE(ss%vn_transcat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vn_transcat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vn_transcat(ss%nrho,0:ss%nspec_th,ss%ntrcat))
     ss%vn_transcat = 0.0_rspec
     ALLOCATE(ss%id_vn_transcat(0:ss%nspec_th,ss%ntrcat))
     ss%id_vn_transcat = 0
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%vol).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vol)
  ENDIF
  IF(ALLOCATED(ss%vol)) then
     jsize = SIZE(ss%vol)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vol already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vol(ss%nrho_eq))
     ss%vol = 0.0_rspec
  ENDIF

  isize = (ss%ngsc0)
  IF(ALLOCATED(ss%vphi0_av).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vphi0_av)
  ENDIF
  IF(ALLOCATED(ss%vphi0_av)) then
     jsize = SIZE(ss%vphi0_av)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vphi0_av already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vphi0_av(ss%ngsc0))
     ss%vphi0_av = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%vpol_inmp).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vpol_inmp)
  ENDIF
  IF(ALLOCATED(ss%vpol_inmp)) then
     jsize = SIZE(ss%vpol_inmp)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vpol_inmp already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vpol_inmp(ss%nrho-1,0:ss%nspec_th))
     ss%vpol_inmp = 0.0_rspec
     ALLOCATE(ss%id_vpol_inmp(0:ss%nspec_th))
     ss%id_vpol_inmp = 0
  ENDIF

  isize = (ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%vpol_is_input).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vpol_is_input)
  ENDIF
  IF(ALLOCATED(ss%vpol_is_input)) then
     jsize = SIZE(ss%vpol_is_input)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vpol_is_input already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vpol_is_input(0:ss%nspec_th))
     ss%vpol_is_input = 0
  ENDIF

  isize = (ss%nrho-1)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%vpol_omp).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vpol_omp)
  ENDIF
  IF(ALLOCATED(ss%vpol_omp)) then
     jsize = SIZE(ss%vpol_omp)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vpol_omp already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vpol_omp(ss%nrho-1,0:ss%nspec_th))
     ss%vpol_omp = 0.0_rspec
     ALLOCATE(ss%id_vpol_omp(0:ss%nspec_th))
     ss%id_vpol_omp = 0
  ENDIF

  isize = (ss%ncoils)
  IF(ALLOCATED(ss%VRep_coil_count).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%VRep_coil_count)
  ENDIF
  IF(ALLOCATED(ss%VRep_coil_count)) then
     jsize = SIZE(ss%VRep_coil_count)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: VRep_coil_count already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%VRep_coil_count(ss%ncoils))
     ss%VRep_coil_count = 0
  ENDIF

  isize = (ss%ncoils)
  IF(ALLOCATED(ss%Vrep_coil_spacing).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Vrep_coil_spacing)
  ENDIF
  IF(ALLOCATED(ss%Vrep_coil_spacing)) then
     jsize = SIZE(ss%Vrep_coil_spacing)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Vrep_coil_spacing already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Vrep_coil_spacing(ss%ncoils))
     ss%Vrep_coil_spacing = 0.0_rspec
  ENDIF

  isize = (ss%ncoils)
  IF(ALLOCATED(ss%Vsize_coil).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Vsize_coil)
  ENDIF
  IF(ALLOCATED(ss%Vsize_coil)) then
     jsize = SIZE(ss%Vsize_coil)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Vsize_coil already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Vsize_coil(ss%ncoils))
     ss%Vsize_coil = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%vtor_inmp).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vtor_inmp)
  ENDIF
  IF(ALLOCATED(ss%vtor_inmp)) then
     jsize = SIZE(ss%vtor_inmp)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vtor_inmp already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vtor_inmp(ss%nrho-1,0:ss%nspec_th))
     ss%vtor_inmp = 0.0_rspec
     ALLOCATE(ss%id_vtor_inmp(0:ss%nspec_th))
     ss%id_vtor_inmp = 0
  ENDIF

  isize = (ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%vtor_is_input).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vtor_is_input)
  ENDIF
  IF(ALLOCATED(ss%vtor_is_input)) then
     jsize = SIZE(ss%vtor_is_input)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vtor_is_input already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vtor_is_input(0:ss%nspec_th))
     ss%vtor_is_input = 0
  ENDIF

  isize = (ss%nrho-1)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%vtor_omp).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vtor_omp)
  ENDIF
  IF(ALLOCATED(ss%vtor_omp)) then
     jsize = SIZE(ss%vtor_omp)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vtor_omp already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vtor_omp(ss%nrho-1,0:ss%nspec_th))
     ss%vtor_omp = 0.0_rspec
     ALLOCATE(ss%id_vtor_omp(0:ss%nspec_th))
     ss%id_vtor_omp = 0
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%V_loop).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%V_loop)
  ENDIF
  IF(ALLOCATED(ss%V_loop)) then
     jsize = SIZE(ss%V_loop)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: V_loop already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%V_loop(ss%nrho))
     ss%V_loop = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%v_pars).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%v_pars)
  ENDIF
  IF(ALLOCATED(ss%v_pars)) then
     jsize = SIZE(ss%v_pars)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: v_pars already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%v_pars(ss%nrho-1,0:ss%nspec_th))
     ss%v_pars = 0.0_rspec
     ALLOCATE(ss%id_v_pars(0:ss%nspec_th))
     ss%id_v_pars = 0
  ENDIF

  isize = (ss%nrho-1)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%v_pers).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%v_pers)
  ENDIF
  IF(ALLOCATED(ss%v_pers)) then
     jsize = SIZE(ss%v_pers)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: v_pers already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%v_pers(ss%nrho-1,0:ss%nspec_th))
     ss%v_pers = 0.0_rspec
     ALLOCATE(ss%id_v_pers(0:ss%nspec_th))
     ss%id_v_pers = 0
  ENDIF

  isize = (ss%max_num_nphi)*(ss%nicrf_src)
  IF(ALLOCATED(ss%wt_nphi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%wt_nphi)
  ENDIF
  IF(ALLOCATED(ss%wt_nphi)) then
     jsize = SIZE(ss%wt_nphi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: wt_nphi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%wt_nphi(ss%max_num_nphi,ss%nicrf_src))
     ss%wt_nphi = 0.0_rspec
  ENDIF

  isize = (ss%max_num_nphi)*(ss%nicrf_src)
  IF(ALLOCATED(ss%wt_nphi_abs).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%wt_nphi_abs)
  ENDIF
  IF(ALLOCATED(ss%wt_nphi_abs)) then
     jsize = SIZE(ss%wt_nphi_abs)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: wt_nphi_abs already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%wt_nphi_abs(ss%max_num_nphi,ss%nicrf_src))
     ss%wt_nphi_abs = 0.0_rspec
  ENDIF

  isize = (ss%max_num_nphi)*(ss%nicrf_src)
  IF(ALLOCATED(ss%wt_nphi_ext).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%wt_nphi_ext)
  ENDIF
  IF(ALLOCATED(ss%wt_nphi_ext)) then
     jsize = SIZE(ss%wt_nphi_ext)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: wt_nphi_ext already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%wt_nphi_ext(ss%max_num_nphi,ss%nicrf_src))
     ss%wt_nphi_ext = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)*(ss%neqmom)
  IF(ALLOCATED(ss%xRjcos_momeq).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%xRjcos_momeq)
  ENDIF
  IF(ALLOCATED(ss%xRjcos_momeq)) then
     jsize = SIZE(ss%xRjcos_momeq)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: xRjcos_momeq already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%xRjcos_momeq(ss%nrho_eq,ss%neqmom))
     ss%xRjcos_momeq = 0.0_rspec
     ALLOCATE(ss%id_xRjcos_momeq(ss%neqmom))
     ss%id_xRjcos_momeq = 0
  ENDIF

  isize = (ss%nrho_eq)*(ss%neqmom)
  IF(ALLOCATED(ss%xRjsin_momeq).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%xRjsin_momeq)
  ENDIF
  IF(ALLOCATED(ss%xRjsin_momeq)) then
     jsize = SIZE(ss%xRjsin_momeq)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: xRjsin_momeq already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%xRjsin_momeq(ss%nrho_eq,ss%neqmom))
     ss%xRjsin_momeq = 0.0_rspec
     ALLOCATE(ss%id_xRjsin_momeq(ss%neqmom))
     ss%id_xRjsin_momeq = 0
  ENDIF

  isize = (ss%nxs)
  IF(ALLOCATED(ss%xs).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%xs)
  ENDIF
  IF(ALLOCATED(ss%xs)) then
     jsize = SIZE(ss%xs)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: xs already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%xs(ss%nxs))
     ss%xs = 0.0_rspec
  ENDIF

  isize = (ss%nxs)
  IF(ALLOCATED(ss%xs_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%xs_name)
  ENDIF
  IF(ALLOCATED(ss%xs_name)) then
     jsize = SIZE(ss%xs_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: xs_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%xs_name(ss%nxs))
     ss%xs_name = " "
  ENDIF

  isize = (ss%nrho_eq)*(ss%neqmom)
  IF(ALLOCATED(ss%xZjcos_momeq).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%xZjcos_momeq)
  ENDIF
  IF(ALLOCATED(ss%xZjcos_momeq)) then
     jsize = SIZE(ss%xZjcos_momeq)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: xZjcos_momeq already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%xZjcos_momeq(ss%nrho_eq,ss%neqmom))
     ss%xZjcos_momeq = 0.0_rspec
     ALLOCATE(ss%id_xZjcos_momeq(ss%neqmom))
     ss%id_xZjcos_momeq = 0
  ENDIF

  isize = (ss%nrho_eq)*(ss%neqmom)
  IF(ALLOCATED(ss%xZjsin_momeq).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%xZjsin_momeq)
  ENDIF
  IF(ALLOCATED(ss%xZjsin_momeq)) then
     jsize = SIZE(ss%xZjsin_momeq)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: xZjsin_momeq already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%xZjsin_momeq(ss%nrho_eq,ss%neqmom))
     ss%xZjsin_momeq = 0.0_rspec
     ALLOCATE(ss%id_xZjsin_momeq(ss%neqmom))
     ss%id_xZjsin_momeq = 0
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%Z0_momeq).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Z0_momeq)
  ENDIF
  IF(ALLOCATED(ss%Z0_momeq)) then
     jsize = SIZE(ss%Z0_momeq)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Z0_momeq already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Z0_momeq(ss%nrho_eq))
     ss%Z0_momeq = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%Zbap).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Zbap)
  ENDIF
  IF(ALLOCATED(ss%Zbap)) then
     jsize = SIZE(ss%Zbap)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Zbap already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Zbap(ss%nbeam))
     ss%Zbap = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%Zbsc).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Zbsc)
  ENDIF
  IF(ALLOCATED(ss%Zbsc)) then
     jsize = SIZE(ss%Zbsc)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Zbsc already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Zbsc(ss%nbeam))
     ss%Zbsc = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%Zeff).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Zeff)
  ENDIF
  IF(ALLOCATED(ss%Zeff)) then
     jsize = SIZE(ss%Zeff)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Zeff already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Zeff(ss%nrho-1))
     ss%Zeff = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%Zeff_fi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Zeff_fi)
  ENDIF
  IF(ALLOCATED(ss%Zeff_fi)) then
     jsize = SIZE(ss%Zeff_fi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Zeff_fi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Zeff_fi(ss%nrho-1))
     ss%Zeff_fi = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%Zeff_th).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Zeff_th)
  ENDIF
  IF(ALLOCATED(ss%Zeff_th)) then
     jsize = SIZE(ss%Zeff_th)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Zeff_th already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Zeff_th(ss%nrho-1))
     ss%Zeff_th = 0.0_rspec
  ENDIF

  isize = (ss%num_rzlim)
  IF(ALLOCATED(ss%zlim).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%zlim)
  ENDIF
  IF(ALLOCATED(ss%zlim)) then
     jsize = SIZE(ss%zlim)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: zlim already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%zlim(ss%num_rzlim))
     ss%zlim = 0.0_rspec
  ENDIF

  isize = (ss%ncoils)
  IF(ALLOCATED(ss%Zloc_coil).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Zloc_coil)
  ENDIF
  IF(ALLOCATED(ss%Zloc_coil)) then
     jsize = SIZE(ss%Zloc_coil)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Zloc_coil already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Zloc_coil(ss%ncoils))
     ss%Zloc_coil = 0.0_rspec
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%Z_ant).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Z_ant)
  ENDIF
  IF(ALLOCATED(ss%Z_ant)) then
     jsize = SIZE(ss%Z_ant)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Z_ant already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Z_ant(ss%nicrf_src))
     ss%Z_ant = 0.0_rspec
  ENDIF

  isize = (ss%max_nrz_antgeo)*(ss%nicrf_src)
  IF(ALLOCATED(ss%Z_antgeo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Z_antgeo)
  ENDIF
  IF(ALLOCATED(ss%Z_antgeo)) then
     jsize = SIZE(ss%Z_antgeo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Z_antgeo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Z_antgeo(ss%max_nrz_antgeo,ss%nicrf_src))
     ss%Z_antgeo = 0.0_rspec
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%Z_EC_launch).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Z_EC_launch)
  ENDIF
  IF(ALLOCATED(ss%Z_EC_launch)) then
     jsize = SIZE(ss%Z_EC_launch)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Z_EC_launch already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Z_EC_launch(ss%necrf_src))
     ss%Z_EC_launch = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)*(ss%nth_eq)
  IF(ALLOCATED(ss%Z_geo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Z_geo)
  ENDIF
  IF(ALLOCATED(ss%Z_geo)) then
     jsize = SIZE(ss%Z_geo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Z_geo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Z_geo(ss%nrho_eq,ss%nth_eq))
     ss%Z_geo = 0.0_rspec
  ENDIF

  isize = (ss%nz)
  IF(ALLOCATED(ss%Z_grid).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Z_grid)
  ENDIF
  IF(ALLOCATED(ss%Z_grid)) then
     jsize = SIZE(ss%Z_grid)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Z_grid already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Z_grid(ss%nz))
     ss%Z_grid = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%Z_midp).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Z_midp)
  ENDIF
  IF(ALLOCATED(ss%Z_midp)) then
     jsize = SIZE(ss%Z_midp)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Z_midp already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Z_midp(ss%nrho_eq_geo))
     ss%Z_midp = 0.0_rspec
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%Z_mid_ant).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Z_mid_ant)
  ENDIF
  IF(ALLOCATED(ss%Z_mid_ant)) then
     jsize = SIZE(ss%Z_mid_ant)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Z_mid_ant already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Z_mid_ant(ss%nicrf_src))
     ss%Z_mid_ant = 0.0_rspec
  ENDIF

  isize = (ss%nz_rip)
  IF(ALLOCATED(ss%Z_ripple).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Z_ripple)
  ENDIF
  IF(ALLOCATED(ss%Z_ripple)) then
     jsize = SIZE(ss%Z_ripple)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Z_ripple already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Z_ripple(ss%nz_rip))
     ss%Z_ripple = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%Z_surfMax).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Z_surfMax)
  ENDIF
  IF(ALLOCATED(ss%Z_surfMax)) then
     jsize = SIZE(ss%Z_surfMax)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Z_surfMax already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Z_surfMax(ss%nrho_eq_geo))
     ss%Z_surfMax = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%Z_surfMin).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Z_surfMin)
  ENDIF
  IF(ALLOCATED(ss%Z_surfMin)) then
     jsize = SIZE(ss%Z_surfMin)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Z_surfMin already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Z_surfMin(ss%nrho_eq_geo))
     ss%Z_surfMin = 0.0_rspec
  ENDIF

END SUBROUTINE plasma_state_empty_alloc_9
