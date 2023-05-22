! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_empty_alloc_2(ss,iout,ierr)


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

  isize = (ss%n_d_einj_standard)
  IF(ALLOCATED(ss%D_einj_standard).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%D_einj_standard)
  ENDIF
  IF(ALLOCATED(ss%D_einj_standard)) then
     jsize = SIZE(ss%D_einj_standard)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: D_einj_standard already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%D_einj_standard(ss%n_d_einj_standard))
     ss%D_einj_standard = 0.0_rspec
  ENDIF

  isize = (ss%n_d_einj_standard)
  IF(ALLOCATED(ss%D_ffull_standard).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%D_ffull_standard)
  ENDIF
  IF(ALLOCATED(ss%D_ffull_standard)) then
     jsize = SIZE(ss%D_ffull_standard)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: D_ffull_standard already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%D_ffull_standard(ss%n_d_einj_standard))
     ss%D_ffull_standard = 0.0_rspec
  ENDIF

  isize = (ss%n_d_einj_standard)
  IF(ALLOCATED(ss%D_fhalf_standard).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%D_fhalf_standard)
  ENDIF
  IF(ALLOCATED(ss%D_fhalf_standard)) then
     jsize = SIZE(ss%D_fhalf_standard)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: D_fhalf_standard already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%D_fhalf_standard(ss%n_d_einj_standard))
     ss%D_fhalf_standard = 0.0_rspec
  ENDIF

  isize = (ss%ngsc0)
  IF(ALLOCATED(ss%e0_av).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%e0_av)
  ENDIF
  IF(ALLOCATED(ss%e0_av)) then
     jsize = SIZE(ss%e0_av)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: e0_av already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%e0_av(ss%ngsc0))
     ss%e0_av = 0.0_rspec
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%ecrf_src_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ecrf_src_name)
  ENDIF
  IF(ALLOCATED(ss%ecrf_src_name)) then
     jsize = SIZE(ss%ecrf_src_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ecrf_src_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ecrf_src_name(ss%necrf_src))
     ss%ecrf_src_name = " "
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%EC_Beam_Elongation).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%EC_Beam_Elongation)
  ENDIF
  IF(ALLOCATED(ss%EC_Beam_Elongation)) then
     jsize = SIZE(ss%EC_Beam_Elongation)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: EC_Beam_Elongation already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%EC_Beam_Elongation(ss%necrf_src))
     ss%EC_Beam_Elongation = 0.0_rspec
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%EC_Half_Power_Angle).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%EC_Half_Power_Angle)
  ENDIF
  IF(ALLOCATED(ss%EC_Half_Power_Angle)) then
     jsize = SIZE(ss%EC_Half_Power_Angle)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: EC_Half_Power_Angle already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%EC_Half_Power_Angle(ss%necrf_src))
     ss%EC_Half_Power_Angle = 0.0_rspec
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%EC_Omode_Fraction).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%EC_Omode_Fraction)
  ENDIF
  IF(ALLOCATED(ss%EC_Omode_Fraction)) then
     jsize = SIZE(ss%EC_Omode_Fraction)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: EC_Omode_Fraction already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%EC_Omode_Fraction(ss%necrf_src))
     ss%EC_Omode_Fraction = 0.0_rspec
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%EC_phi_aim).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%EC_phi_aim)
  ENDIF
  IF(ALLOCATED(ss%EC_phi_aim)) then
     jsize = SIZE(ss%EC_phi_aim)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: EC_phi_aim already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%EC_phi_aim(ss%necrf_src))
     ss%EC_phi_aim = 0.0_rspec
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%EC_PRINC_CURV_HOR).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%EC_PRINC_CURV_HOR)
  ENDIF
  IF(ALLOCATED(ss%EC_PRINC_CURV_HOR)) then
     jsize = SIZE(ss%EC_PRINC_CURV_HOR)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: EC_PRINC_CURV_HOR already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%EC_PRINC_CURV_HOR(ss%necrf_src))
     ss%EC_PRINC_CURV_HOR = 0.0_rspec
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%EC_PRINC_CURV_VERT).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%EC_PRINC_CURV_VERT)
  ENDIF
  IF(ALLOCATED(ss%EC_PRINC_CURV_VERT)) then
     jsize = SIZE(ss%EC_PRINC_CURV_VERT)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: EC_PRINC_CURV_VERT already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%EC_PRINC_CURV_VERT(ss%necrf_src))
     ss%EC_PRINC_CURV_VERT = 0.0_rspec
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%EC_PRINC_WIDTH_HOR)
  ENDIF
  IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR)) then
     jsize = SIZE(ss%EC_PRINC_WIDTH_HOR)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: EC_PRINC_WIDTH_HOR already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%EC_PRINC_WIDTH_HOR(ss%necrf_src))
     ss%EC_PRINC_WIDTH_HOR = 0.0_rspec
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%EC_PRINC_WIDTH_VERT)
  ENDIF
  IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT)) then
     jsize = SIZE(ss%EC_PRINC_WIDTH_VERT)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: EC_PRINC_WIDTH_VERT already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%EC_PRINC_WIDTH_VERT(ss%necrf_src))
     ss%EC_PRINC_WIDTH_VERT = 0.0_rspec
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%EC_theta_aim).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%EC_theta_aim)
  ENDIF
  IF(ALLOCATED(ss%EC_theta_aim)) then
     jsize = SIZE(ss%EC_theta_aim)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: EC_theta_aim already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%EC_theta_aim(ss%necrf_src))
     ss%EC_theta_aim = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%ee_mobility_factor).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ee_mobility_factor)
  ENDIF
  IF(ALLOCATED(ss%ee_mobility_factor)) then
     jsize = SIZE(ss%ee_mobility_factor)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ee_mobility_factor already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ee_mobility_factor(ss%nrho_anomth))
     ss%ee_mobility_factor = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%Einj_max).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Einj_max)
  ENDIF
  IF(ALLOCATED(ss%Einj_max)) then
     jsize = SIZE(ss%Einj_max)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Einj_max already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Einj_max(ss%nbeam))
     ss%Einj_max = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%Einj_min).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Einj_min)
  ENDIF
  IF(ALLOCATED(ss%Einj_min)) then
     jsize = SIZE(ss%Einj_min)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Einj_min already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Einj_min(ss%nbeam))
     ss%Einj_min = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%elong).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%elong)
  ENDIF
  IF(ALLOCATED(ss%elong)) then
     jsize = SIZE(ss%elong)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: elong already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%elong(ss%nrho_eq_geo))
     ss%elong = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)*(ss%nspec_beam)
  IF(ALLOCATED(ss%eperp_beami).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%eperp_beami)
  ENDIF
  IF(ALLOCATED(ss%eperp_beami)) then
     jsize = SIZE(ss%eperp_beami)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: eperp_beami already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%eperp_beami(ss%nrho_nbi-1,ss%nspec_beam))
     ss%eperp_beami = 0.0_rspec
     ALLOCATE(ss%id_eperp_beami(ss%nspec_beam))
     ss%id_eperp_beami = 0
  ENDIF

  isize = (ss%nrho_fus-1)*(ss%nspec_fusion)
  IF(ALLOCATED(ss%eperp_fusi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%eperp_fusi)
  ENDIF
  IF(ALLOCATED(ss%eperp_fusi)) then
     jsize = SIZE(ss%eperp_fusi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: eperp_fusi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%eperp_fusi(ss%nrho_fus-1,ss%nspec_fusion))
     ss%eperp_fusi = 0.0_rspec
     ALLOCATE(ss%id_eperp_fusi(ss%nspec_fusion))
     ss%id_eperp_fusi = 0
  ENDIF

  isize = (ss%nrho_icrf-1)*(ss%nspec_rfmin)
  IF(ALLOCATED(ss%eperp_mini).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%eperp_mini)
  ENDIF
  IF(ALLOCATED(ss%eperp_mini)) then
     jsize = SIZE(ss%eperp_mini)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: eperp_mini already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%eperp_mini(ss%nrho_icrf-1,ss%nspec_rfmin))
     ss%eperp_mini = 0.0_rspec
     ALLOCATE(ss%id_eperp_mini(ss%nspec_rfmin))
     ss%id_eperp_mini = 0
  ENDIF

  isize = (ss%nrho_rw-1)
  IF(ALLOCATED(ss%eperp_rw).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%eperp_rw)
  ENDIF
  IF(ALLOCATED(ss%eperp_rw)) then
     jsize = SIZE(ss%eperp_rw)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: eperp_rw already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%eperp_rw(ss%nrho_rw-1))
     ss%eperp_rw = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)*(ss%nspec_beam)
  IF(ALLOCATED(ss%epll_beami).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%epll_beami)
  ENDIF
  IF(ALLOCATED(ss%epll_beami)) then
     jsize = SIZE(ss%epll_beami)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: epll_beami already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%epll_beami(ss%nrho_nbi-1,ss%nspec_beam))
     ss%epll_beami = 0.0_rspec
     ALLOCATE(ss%id_epll_beami(ss%nspec_beam))
     ss%id_epll_beami = 0
  ENDIF

  isize = (ss%nrho_fus-1)*(ss%nspec_fusion)
  IF(ALLOCATED(ss%epll_fusi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%epll_fusi)
  ENDIF
  IF(ALLOCATED(ss%epll_fusi)) then
     jsize = SIZE(ss%epll_fusi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: epll_fusi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%epll_fusi(ss%nrho_fus-1,ss%nspec_fusion))
     ss%epll_fusi = 0.0_rspec
     ALLOCATE(ss%id_epll_fusi(ss%nspec_fusion))
     ss%id_epll_fusi = 0
  ENDIF

  isize = (ss%nrho_icrf-1)*(ss%nspec_rfmin)
  IF(ALLOCATED(ss%epll_mini).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%epll_mini)
  ENDIF
  IF(ALLOCATED(ss%epll_mini)) then
     jsize = SIZE(ss%epll_mini)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: epll_mini already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%epll_mini(ss%nrho_icrf-1,ss%nspec_rfmin))
     ss%epll_mini = 0.0_rspec
     ALLOCATE(ss%id_epll_mini(ss%nspec_rfmin))
     ss%id_epll_mini = 0
  ENDIF

  isize = (ss%nrho_rw-1)
  IF(ALLOCATED(ss%epll_rw).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%epll_rw)
  ENDIF
  IF(ALLOCATED(ss%epll_rw)) then
     jsize = SIZE(ss%epll_rw)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: epll_rw already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%epll_rw(ss%nrho_rw-1))
     ss%epll_rw = 0.0_rspec
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%Epot).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Epot)
  ENDIF
  IF(ALLOCATED(ss%Epot)) then
     jsize = SIZE(ss%Epot)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Epot already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Epot(ss%nrho))
     ss%Epot = 0.0_rspec
  ENDIF

  isize = (ss%neqmom)
  IF(ALLOCATED(ss%eqmom_num).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%eqmom_num)
  ENDIF
  IF(ALLOCATED(ss%eqmom_num)) then
     jsize = SIZE(ss%eqmom_num)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: eqmom_num already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%eqmom_num(ss%neqmom))
     ss%eqmom_num = " "
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%eta_parallel).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%eta_parallel)
  ENDIF
  IF(ALLOCATED(ss%eta_parallel)) then
     jsize = SIZE(ss%eta_parallel)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: eta_parallel already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%eta_parallel(ss%nrho))
     ss%eta_parallel = 0.0_rspec
  ENDIF

  isize = (ss%nefi_anom)
  IF(ALLOCATED(ss%E_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%E_anom)
  ENDIF
  IF(ALLOCATED(ss%E_anom)) then
     jsize = SIZE(ss%E_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: E_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%E_anom(ss%nefi_anom))
     ss%E_anom = 0.0_rspec
  ENDIF

  isize = (ss%nefi_anom2)
  IF(ALLOCATED(ss%E_anom2).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%E_anom2)
  ENDIF
  IF(ALLOCATED(ss%E_anom2)) then
     jsize = SIZE(ss%E_anom2)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: E_anom2 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%E_anom2(ss%nefi_anom2))
     ss%E_anom2 = 0.0_rspec
  ENDIF

  isize = (ss%nefi_anom2)*(ss%nrho_anom2)*(ss%nspec_fusion)
  IF(ALLOCATED(ss%fidiff_bpass_co).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%fidiff_bpass_co)
  ENDIF
  IF(ALLOCATED(ss%fidiff_bpass_co)) then
     jsize = SIZE(ss%fidiff_bpass_co)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: fidiff_bpass_co already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%fidiff_bpass_co(ss%nefi_anom2,ss%nrho_anom2,ss%nspec_fusion))
     ss%fidiff_bpass_co = 0.0_rspec
     ALLOCATE(ss%id_fidiff_bpass_co(ss%nspec_fusion))
     ss%id_fidiff_bpass_co = 0
  ENDIF

  isize = (ss%nefi_anom2)*(ss%nrho_anom2)*(ss%nspec_fusion)
  IF(ALLOCATED(ss%fidiff_bpass_ctr).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%fidiff_bpass_ctr)
  ENDIF
  IF(ALLOCATED(ss%fidiff_bpass_ctr)) then
     jsize = SIZE(ss%fidiff_bpass_ctr)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: fidiff_bpass_ctr already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%fidiff_bpass_ctr(ss%nefi_anom2,ss%nrho_anom2,ss%nspec_fusion))
     ss%fidiff_bpass_ctr = 0.0_rspec
     ALLOCATE(ss%id_fidiff_bpass_ctr(ss%nspec_fusion))
     ss%id_fidiff_bpass_ctr = 0
  ENDIF

  isize = (ss%nefi_anom2)*(ss%nrho_anom2)*(ss%nspec_fusion)
  IF(ALLOCATED(ss%fidiff_btrap).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%fidiff_btrap)
  ENDIF
  IF(ALLOCATED(ss%fidiff_btrap)) then
     jsize = SIZE(ss%fidiff_btrap)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: fidiff_btrap already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%fidiff_btrap(ss%nefi_anom2,ss%nrho_anom2,ss%nspec_fusion))
     ss%fidiff_btrap = 0.0_rspec
     ALLOCATE(ss%id_fidiff_btrap(ss%nspec_fusion))
     ss%id_fidiff_btrap = 0
  ENDIF

  isize = (ss%nefi_anom2)*(ss%nrho_anom2)*(ss%nspec_fusion)
  IF(ALLOCATED(ss%fidiff_dpass_co).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%fidiff_dpass_co)
  ENDIF
  IF(ALLOCATED(ss%fidiff_dpass_co)) then
     jsize = SIZE(ss%fidiff_dpass_co)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: fidiff_dpass_co already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%fidiff_dpass_co(ss%nefi_anom2,ss%nrho_anom2,ss%nspec_fusion))
     ss%fidiff_dpass_co = 0.0_rspec
     ALLOCATE(ss%id_fidiff_dpass_co(ss%nspec_fusion))
     ss%id_fidiff_dpass_co = 0
  ENDIF

  isize = (ss%nefi_anom2)*(ss%nrho_anom2)*(ss%nspec_fusion)
  IF(ALLOCATED(ss%fidiff_dpass_ctr).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%fidiff_dpass_ctr)
  ENDIF
  IF(ALLOCATED(ss%fidiff_dpass_ctr)) then
     jsize = SIZE(ss%fidiff_dpass_ctr)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: fidiff_dpass_ctr already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%fidiff_dpass_ctr(ss%nefi_anom2,ss%nrho_anom2,ss%nspec_fusion))
     ss%fidiff_dpass_ctr = 0.0_rspec
     ALLOCATE(ss%id_fidiff_dpass_ctr(ss%nspec_fusion))
     ss%id_fidiff_dpass_ctr = 0
  ENDIF

  isize = (ss%nefi_anom2)*(ss%nrho_anom2)*(ss%nspec_fusion)
  IF(ALLOCATED(ss%fidiff_dtrap).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%fidiff_dtrap)
  ENDIF
  IF(ALLOCATED(ss%fidiff_dtrap)) then
     jsize = SIZE(ss%fidiff_dtrap)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: fidiff_dtrap already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%fidiff_dtrap(ss%nefi_anom2,ss%nrho_anom2,ss%nspec_fusion))
     ss%fidiff_dtrap = 0.0_rspec
     ALLOCATE(ss%id_fidiff_dtrap(ss%nspec_fusion))
     ss%id_fidiff_dtrap = 0
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%Fi_depletion).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Fi_depletion)
  ENDIF
  IF(ALLOCATED(ss%Fi_depletion)) then
     jsize = SIZE(ss%Fi_depletion)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Fi_depletion already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Fi_depletion(ss%nrho-1))
     ss%Fi_depletion = 0.0_rspec
  ENDIF

  isize = (ss%nspec_rfmin)
  IF(ALLOCATED(ss%fracmin).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%fracmin)
  ENDIF
  IF(ALLOCATED(ss%fracmin)) then
     jsize = SIZE(ss%fracmin)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: fracmin already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%fracmin(ss%nspec_rfmin))
     ss%fracmin = 0.0_rspec
  ENDIF

  isize = (ss%max_nspectrumlh_src)*(ss%nlhrf_src)
  IF(ALLOCATED(ss%fracpslh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%fracpslh)
  ENDIF
  IF(ALLOCATED(ss%fracpslh)) then
     jsize = SIZE(ss%fracpslh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: fracpslh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%fracpslh(ss%max_nspectrumlh_src,ss%nlhrf_src))
     ss%fracpslh = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%frac_full).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%frac_full)
  ENDIF
  IF(ALLOCATED(ss%frac_full)) then
     jsize = SIZE(ss%frac_full)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: frac_full already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%frac_full(ss%nbeam))
     ss%frac_full = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%frac_half).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%frac_half)
  ENDIF
  IF(ALLOCATED(ss%frac_half)) then
     jsize = SIZE(ss%frac_half)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: frac_half already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%frac_half(ss%nbeam))
     ss%frac_half = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%freq_drbm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%freq_drbm)
  ENDIF
  IF(ALLOCATED(ss%freq_drbm)) then
     jsize = SIZE(ss%freq_drbm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: freq_drbm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%freq_drbm(ss%nrho_anomth))
     ss%freq_drbm = 0.0_rspec
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%freq_ec).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%freq_ec)
  ENDIF
  IF(ALLOCATED(ss%freq_ec)) then
     jsize = SIZE(ss%freq_ec)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: freq_ec already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%freq_ec(ss%necrf_src))
     ss%freq_ec = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%freq_etg).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%freq_etg)
  ENDIF
  IF(ALLOCATED(ss%freq_etg)) then
     jsize = SIZE(ss%freq_etg)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: freq_etg already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%freq_etg(ss%nrho_anomth))
     ss%freq_etg = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%freq_glf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%freq_glf)
  ENDIF
  IF(ALLOCATED(ss%freq_glf)) then
     jsize = SIZE(ss%freq_glf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: freq_glf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%freq_glf(ss%nrho_anomth))
     ss%freq_glf = 0.0_rspec
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%freq_ic).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%freq_ic)
  ENDIF
  IF(ALLOCATED(ss%freq_ic)) then
     jsize = SIZE(ss%freq_ic)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: freq_ic already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%freq_ic(ss%nicrf_src))
     ss%freq_ic = 0.0_rspec
  ENDIF

  isize = (ss%nlhrf_src)
  IF(ALLOCATED(ss%freq_lh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%freq_lh)
  ENDIF
  IF(ALLOCATED(ss%freq_lh)) then
     jsize = SIZE(ss%freq_lh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: freq_lh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%freq_lh(ss%nlhrf_src))
     ss%freq_lh = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%freq_max_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%freq_max_tglf)
  ENDIF
  IF(ALLOCATED(ss%freq_max_tglf)) then
     jsize = SIZE(ss%freq_max_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: freq_max_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%freq_max_tglf(ss%nrho_anomth))
     ss%freq_max_tglf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_mmm)
  IF(ALLOCATED(ss%freq_mmm71).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%freq_mmm71)
  ENDIF
  IF(ALLOCATED(ss%freq_mmm71)) then
     jsize = SIZE(ss%freq_mmm71)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: freq_mmm71 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%freq_mmm71(ss%nrho_anomth,ss%nky_mmm))
     ss%freq_mmm71 = 0.0_rspec
     ALLOCATE(ss%id_freq_mmm71(ss%nky_mmm))
     ss%id_freq_mmm71 = 0
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%freq_mtm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%freq_mtm)
  ENDIF
  IF(ALLOCATED(ss%freq_mtm)) then
     jsize = SIZE(ss%freq_mtm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: freq_mtm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%freq_mtm(ss%nrho_anomth))
     ss%freq_mtm = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%freq_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%freq_tglf)
  ENDIF
  IF(ALLOCATED(ss%freq_tglf)) then
     jsize = SIZE(ss%freq_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: freq_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%freq_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%freq_tglf = 0.0_rspec
     ALLOCATE(ss%id_freq_tglf(ss%nky_anomth))
     ss%id_freq_tglf = 0
  ENDIF

  isize = (ss%mmodes)*(ss%ndelta_w)
  IF(ALLOCATED(ss%gamma_lmhd).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gamma_lmhd)
  ENDIF
  IF(ALLOCATED(ss%gamma_lmhd)) then
     jsize = SIZE(ss%gamma_lmhd)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gamma_lmhd already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gamma_lmhd(ss%mmodes,ss%ndelta_w))
     ss%gamma_lmhd = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%gamma_nc).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gamma_nc)
  ENDIF
  IF(ALLOCATED(ss%gamma_nc)) then
     jsize = SIZE(ss%gamma_nc)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gamma_nc already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gamma_nc(ss%nrho_eq_geo))
     ss%gamma_nc = 0.0_rspec
  ENDIF

  isize = (ss%ngsc0)
  IF(ALLOCATED(ss%gas_atom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gas_atom)
  ENDIF
  IF(ALLOCATED(ss%gas_atom)) then
     jsize = SIZE(ss%gas_atom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gas_atom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gas_atom(ss%ngsc0))
     ss%gas_atom = " "
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%gb1).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gb1)
  ENDIF
  IF(ALLOCATED(ss%gb1)) then
     jsize = SIZE(ss%gb1)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gb1 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gb1(ss%nrho_eq_geo))
     ss%gb1 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%gb2).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gb2)
  ENDIF
  IF(ALLOCATED(ss%gb2)) then
     jsize = SIZE(ss%gb2)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gb2 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gb2(ss%nrho_eq_geo))
     ss%gb2 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%gb2i).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gb2i)
  ENDIF
  IF(ALLOCATED(ss%gb2i)) then
     jsize = SIZE(ss%gb2i)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gb2i already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gb2i(ss%nrho_eq_geo))
     ss%gb2i = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%gbr2).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gbr2)
  ENDIF
  IF(ALLOCATED(ss%gbr2)) then
     jsize = SIZE(ss%gbr2)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gbr2 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gbr2(ss%nrho_eq_geo))
     ss%gbr2 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%gncfb2h).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gncfb2h)
  ENDIF
  IF(ALLOCATED(ss%gncfb2h)) then
     jsize = SIZE(ss%gncfb2h)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gncfb2h already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gncfb2h(ss%nrho_eq_geo))
     ss%gncfb2h = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%gncfh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gncfh)
  ENDIF
  IF(ALLOCATED(ss%gncfh)) then
     jsize = SIZE(ss%gncfh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gncfh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gncfh(ss%nrho_eq_geo))
     ss%gncfh = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%gr1).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gr1)
  ENDIF
  IF(ALLOCATED(ss%gr1)) then
     jsize = SIZE(ss%gr1)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gr1 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gr1(ss%nrho_eq_geo))
     ss%gr1 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%gr2).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gr2)
  ENDIF
  IF(ALLOCATED(ss%gr2)) then
     jsize = SIZE(ss%gr2)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gr2 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gr2(ss%nrho_eq_geo))
     ss%gr2 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%gr2i).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gr2i)
  ENDIF
  IF(ALLOCATED(ss%gr2i)) then
     jsize = SIZE(ss%gr2i)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gr2i already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gr2i(ss%nrho_eq_geo))
     ss%gr2i = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%gr2rho2).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gr2rho2)
  ENDIF
  IF(ALLOCATED(ss%gr2rho2)) then
     jsize = SIZE(ss%gr2rho2)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gr2rho2 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gr2rho2(ss%nrho_eq_geo))
     ss%gr2rho2 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%gr3i).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%gr3i)
  ENDIF
  IF(ALLOCATED(ss%gr3i)) then
     jsize = SIZE(ss%gr3i)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: gr3i already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%gr3i(ss%nrho_eq_geo))
     ss%gr3i = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%grho1).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%grho1)
  ENDIF
  IF(ALLOCATED(ss%grho1)) then
     jsize = SIZE(ss%grho1)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: grho1 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%grho1(ss%nrho_eq_geo))
     ss%grho1 = 0.0_rspec
  ENDIF

END SUBROUTINE plasma_state_empty_alloc_2
