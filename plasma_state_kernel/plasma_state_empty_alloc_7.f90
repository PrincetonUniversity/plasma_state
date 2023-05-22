! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_empty_alloc_7(ss,iout,ierr)


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

  isize = (ss%nrho_pel)
  IF(ALLOCATED(ss%rho_pel).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_pel)
  ENDIF
  IF(ALLOCATED(ss%rho_pel)) then
     jsize = SIZE(ss%rho_pel)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_pel already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_pel(ss%nrho_pel))
     ss%rho_pel = 0.0_rspec
  ENDIF

  isize = (ss%nrho_rad)
  IF(ALLOCATED(ss%rho_rad).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_rad)
  ENDIF
  IF(ALLOCATED(ss%rho_rad)) then
     jsize = SIZE(ss%rho_rad)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_rad already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_rad(ss%nrho_rad))
     ss%rho_rad = 0.0_rspec
  ENDIF

  isize = (ss%nrho_rw)
  IF(ALLOCATED(ss%rho_rw).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_rw)
  ENDIF
  IF(ALLOCATED(ss%rho_rw)) then
     jsize = SIZE(ss%rho_rw)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_rw already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_rw(ss%nrho_rw))
     ss%rho_rw = 0.0_rspec
  ENDIF

  isize = (ss%num_rzlim)
  IF(ALLOCATED(ss%rlim).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rlim)
  ENDIF
  IF(ALLOCATED(ss%rlim)) then
     jsize = SIZE(ss%rlim)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rlim already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rlim(ss%num_rzlim))
     ss%rlim = 0.0_rspec
  ENDIF

  isize = (ss%ncoils)
  IF(ALLOCATED(ss%Rloc_coil).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Rloc_coil)
  ENDIF
  IF(ALLOCATED(ss%Rloc_coil)) then
     jsize = SIZE(ss%Rloc_coil)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Rloc_coil already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Rloc_coil(ss%ncoils))
     ss%Rloc_coil = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%Rmajor_mean).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Rmajor_mean)
  ENDIF
  IF(ALLOCATED(ss%Rmajor_mean)) then
     jsize = SIZE(ss%Rmajor_mean)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Rmajor_mean already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Rmajor_mean(ss%nrho_eq_geo))
     ss%Rmajor_mean = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%rMinor_mean).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rMinor_mean)
  ENDIF
  IF(ALLOCATED(ss%rMinor_mean)) then
     jsize = SIZE(ss%rMinor_mean)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rMinor_mean already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rMinor_mean(ss%nrho_eq_geo))
     ss%rMinor_mean = 0.0_rspec
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%R_ant).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%R_ant)
  ENDIF
  IF(ALLOCATED(ss%R_ant)) then
     jsize = SIZE(ss%R_ant)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: R_ant already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%R_ant(ss%nicrf_src))
     ss%R_ant = 0.0_rspec
  ENDIF

  isize = (ss%max_nrz_antgeo)*(ss%nicrf_src)
  IF(ALLOCATED(ss%R_antgeo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%R_antgeo)
  ENDIF
  IF(ALLOCATED(ss%R_antgeo)) then
     jsize = SIZE(ss%R_antgeo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: R_antgeo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%R_antgeo(ss%max_nrz_antgeo,ss%nicrf_src))
     ss%R_antgeo = 0.0_rspec
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%R_EC_launch).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%R_EC_launch)
  ENDIF
  IF(ALLOCATED(ss%R_EC_launch)) then
     jsize = SIZE(ss%R_EC_launch)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: R_EC_launch already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%R_EC_launch(ss%necrf_src))
     ss%R_EC_launch = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)*(ss%nth_eq)
  IF(ALLOCATED(ss%R_geo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%R_geo)
  ENDIF
  IF(ALLOCATED(ss%R_geo)) then
     jsize = SIZE(ss%R_geo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: R_geo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%R_geo(ss%nrho_eq,ss%nth_eq))
     ss%R_geo = 0.0_rspec
  ENDIF

  isize = (ss%nr)
  IF(ALLOCATED(ss%R_grid).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%R_grid)
  ENDIF
  IF(ALLOCATED(ss%R_grid)) then
     jsize = SIZE(ss%R_grid)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: R_grid already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%R_grid(ss%nr))
     ss%R_grid = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%R_midp_in).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%R_midp_in)
  ENDIF
  IF(ALLOCATED(ss%R_midp_in)) then
     jsize = SIZE(ss%R_midp_in)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: R_midp_in already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%R_midp_in(ss%nrho_eq_geo))
     ss%R_midp_in = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%R_midp_out).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%R_midp_out)
  ENDIF
  IF(ALLOCATED(ss%R_midp_out)) then
     jsize = SIZE(ss%R_midp_out)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: R_midp_out already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%R_midp_out(ss%nrho_eq_geo))
     ss%R_midp_out = 0.0_rspec
  ENDIF

  isize = (ss%nr_rip)
  IF(ALLOCATED(ss%R_ripple).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%R_ripple)
  ENDIF
  IF(ALLOCATED(ss%R_ripple)) then
     jsize = SIZE(ss%R_ripple)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: R_ripple already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%R_ripple(ss%nr_rip))
     ss%R_ripple = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%R_surfMax).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%R_surfMax)
  ENDIF
  IF(ALLOCATED(ss%R_surfMax)) then
     jsize = SIZE(ss%R_surfMax)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: R_surfMax already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%R_surfMax(ss%nrho_eq_geo))
     ss%R_surfMax = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%R_surfMin).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%R_surfMin)
  ENDIF
  IF(ALLOCATED(ss%R_surfMin)) then
     jsize = SIZE(ss%R_surfMin)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: R_surfMin already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%R_surfMin(ss%nrho_eq_geo))
     ss%R_surfMin = 0.0_rspec
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%s0reco).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%s0reco)
  ENDIF
  IF(ALLOCATED(ss%s0reco)) then
     jsize = SIZE(ss%s0reco)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: s0reco already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%s0reco(ss%nrho_gas-1,ss%nspec_gas))
     ss%s0reco = 0.0_rspec
     ALLOCATE(ss%id_s0reco(ss%nspec_gas))
     ss%id_s0reco = 0
  ENDIF

  isize = (ss%nrho_gas-1)
  IF(ALLOCATED(ss%s0reco_e).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%s0reco_e)
  ENDIF
  IF(ALLOCATED(ss%s0reco_e)) then
     jsize = SIZE(ss%s0reco_e)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: s0reco_e already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%s0reco_e(ss%nrho_gas-1))
     ss%s0reco_e = 0.0_rspec
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%s0reco_recap).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%s0reco_recap)
  ENDIF
  IF(ALLOCATED(ss%s0reco_recap)) then
     jsize = SIZE(ss%s0reco_recap)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: s0reco_recap already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%s0reco_recap(ss%nrho_gas-1,ss%nspec_gas))
     ss%s0reco_recap = 0.0_rspec
     ALLOCATE(ss%id_s0reco_recap(ss%nspec_gas))
     ss%id_s0reco_recap = 0
  ENDIF

  isize = (ss%nspec_all+1)
  IF(ss%nspec_all.LE.0) isize = 0
  IF(ALLOCATED(ss%sa_index).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sa_index)
  ENDIF
  IF(ALLOCATED(ss%sa_index)) then
     jsize = SIZE(ss%sa_index)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sa_index already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sa_index(0:ss%nspec_all))
     ss%sa_index = 0
  ENDIF

  isize = (ss%nspec_tha+1)
  IF(ss%nspec_tha.LE.0) isize = 0
  IF(ALLOCATED(ss%SA_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%SA_name)
  ENDIF
  IF(ALLOCATED(ss%SA_name)) then
     jsize = SIZE(ss%SA_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: SA_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%SA_name(0:ss%nspec_tha))
     ss%SA_name = " "
  ENDIF

  isize = (ss%nspec_tha+1)
  IF(ss%nspec_tha.LE.0) isize = 0
  IF(ALLOCATED(ss%SA_type).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%SA_type)
  ENDIF
  IF(ALLOCATED(ss%SA_type)) then
     jsize = SIZE(ss%SA_type)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: SA_type already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%SA_type(0:ss%nspec_tha))
     ss%SA_type = 0
  ENDIF

  isize = (ss%nrho_nbi-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%sb0halo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sb0halo)
  ENDIF
  IF(ALLOCATED(ss%sb0halo)) then
     jsize = SIZE(ss%sb0halo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sb0halo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sb0halo(ss%nrho_nbi-1,ss%nspec_gas))
     ss%sb0halo = 0.0_rspec
     ALLOCATE(ss%id_sb0halo(ss%nspec_gas))
     ss%id_sb0halo = 0
  ENDIF

  isize = (ss%nrho_nbi-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%sb0halo_recap).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sb0halo_recap)
  ENDIF
  IF(ALLOCATED(ss%sb0halo_recap)) then
     jsize = SIZE(ss%sb0halo_recap)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sb0halo_recap already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sb0halo_recap(ss%nrho_nbi-1,ss%nspec_gas))
     ss%sb0halo_recap = 0.0_rspec
     ALLOCATE(ss%id_sb0halo_recap(ss%nspec_gas))
     ss%id_sb0halo_recap = 0
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%sbedep).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sbedep)
  ENDIF
  IF(ALLOCATED(ss%sbedep)) then
     jsize = SIZE(ss%sbedep)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sbedep already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sbedep(ss%nrho_nbi-1))
     ss%sbedep = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%sbehalo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sbehalo)
  ENDIF
  IF(ALLOCATED(ss%sbehalo)) then
     jsize = SIZE(ss%sbehalo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sbehalo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sbehalo(ss%nrho_nbi-1))
     ss%sbehalo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)*(ss%nspec_tha+1)
  IF(ss%nspec_tha.LE.0) isize = 0
  IF(ALLOCATED(ss%sbsce).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sbsce)
  ENDIF
  IF(ALLOCATED(ss%sbsce)) then
     jsize = SIZE(ss%sbsce)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sbsce already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sbsce(ss%nrho_nbi-1,0:ss%nspec_tha))
     ss%sbsce = 0.0_rspec
     ALLOCATE(ss%id_sbsce(0:ss%nspec_tha))
     ss%id_sbsce = 0
  ENDIF

  isize = (ss%nrho_nbi-1)*(ss%nspec_beam)
  IF(ALLOCATED(ss%sbtherm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sbtherm)
  ENDIF
  IF(ALLOCATED(ss%sbtherm)) then
     jsize = SIZE(ss%sbtherm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sbtherm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sbtherm(ss%nrho_nbi-1,ss%nspec_beam))
     ss%sbtherm = 0.0_rspec
     ALLOCATE(ss%id_sbtherm(ss%nspec_beam))
     ss%id_sbtherm = 0
  ENDIF

  isize = (ss%ngsc0)
  IF(ALLOCATED(ss%sc0).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sc0)
  ENDIF
  IF(ALLOCATED(ss%sc0)) then
     jsize = SIZE(ss%sc0)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sc0 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sc0(ss%ngsc0))
     ss%sc0 = 0.0_rspec
  ENDIF

  isize = (ss%ngsc0)
  IF(ALLOCATED(ss%sc0_to_sgas).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sc0_to_sgas)
  ENDIF
  IF(ALLOCATED(ss%sc0_to_sgas)) then
     jsize = SIZE(ss%sc0_to_sgas)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sc0_to_sgas already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sc0_to_sgas(ss%ngsc0))
     ss%sc0_to_sgas = 0
  ENDIF

  isize = (ss%nrho_anomth-1)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%sc_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sc_anom)
  ENDIF
  IF(ALLOCATED(ss%sc_anom)) then
     jsize = SIZE(ss%sc_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sc_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sc_anom(ss%nrho_anomth-1,0:ss%nspec_th))
     ss%sc_anom = 0.0_rspec
     ALLOCATE(ss%id_sc_anom(0:ss%nspec_th))
     ss%id_sc_anom = 0
  ENDIF

  isize = (ss%nrho_fus-1)*(ss%nspec_tha+1)
  IF(ss%nspec_tha.LE.0) isize = 0
  IF(ALLOCATED(ss%sfsce).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sfsce)
  ENDIF
  IF(ALLOCATED(ss%sfsce)) then
     jsize = SIZE(ss%sfsce)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sfsce already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sfsce(ss%nrho_fus-1,0:ss%nspec_tha))
     ss%sfsce = 0.0_rspec
     ALLOCATE(ss%id_sfsce(0:ss%nspec_tha))
     ss%id_sfsce = 0
  ENDIF

  isize = (ss%nrho_fus-1)*(ss%nspec_fusion)
  IF(ALLOCATED(ss%sftherm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sftherm)
  ENDIF
  IF(ALLOCATED(ss%sftherm)) then
     jsize = SIZE(ss%sftherm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sftherm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sftherm(ss%nrho_fus-1,ss%nspec_fusion))
     ss%sftherm = 0.0_rspec
     ALLOCATE(ss%id_sftherm(ss%nspec_fusion))
     ss%id_sftherm = 0
  ENDIF

  isize = (ss%nspec_fusion)
  IF(ALLOCATED(ss%SFUS_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%SFUS_name)
  ENDIF
  IF(ALLOCATED(ss%SFUS_name)) then
     jsize = SIZE(ss%SFUS_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: SFUS_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%SFUS_name(ss%nspec_fusion))
     ss%SFUS_name = " "
  ENDIF

  isize = (ss%nspec_fusion)
  IF(ALLOCATED(ss%sfus_to_all).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sfus_to_all)
  ENDIF
  IF(ALLOCATED(ss%sfus_to_all)) then
     jsize = SIZE(ss%sfus_to_all)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sfus_to_all already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sfus_to_all(ss%nspec_fusion))
     ss%sfus_to_all = 0
  ENDIF

  isize = (ss%nspec_fusion)
  IF(ALLOCATED(ss%sfus_to_alla).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sfus_to_alla)
  ENDIF
  IF(ALLOCATED(ss%sfus_to_alla)) then
     jsize = SIZE(ss%sfus_to_alla)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sfus_to_alla already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sfus_to_alla(ss%nspec_fusion))
     ss%sfus_to_alla = 0
  ENDIF

  isize = (ss%nspec_fusion)
  IF(ALLOCATED(ss%SFUS_type).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%SFUS_type)
  ENDIF
  IF(ALLOCATED(ss%SFUS_type)) then
     jsize = SIZE(ss%SFUS_type)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: SFUS_type already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%SFUS_type(ss%nspec_fusion))
     ss%SFUS_type = 0
  ENDIF

  isize = (ss%nspec_gas)
  IF(ALLOCATED(ss%SGAS_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%SGAS_name)
  ENDIF
  IF(ALLOCATED(ss%SGAS_name)) then
     jsize = SIZE(ss%SGAS_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: SGAS_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%SGAS_name(ss%nspec_gas))
     ss%SGAS_name = " "
  ENDIF

  isize = (ss%nspec_gas)
  IF(ALLOCATED(ss%sgas_to_s).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sgas_to_s)
  ENDIF
  IF(ALLOCATED(ss%sgas_to_s)) then
     jsize = SIZE(ss%sgas_to_s)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sgas_to_s already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sgas_to_s(ss%nspec_gas))
     ss%sgas_to_s = 0
  ENDIF

  isize = (ss%nspec_gas)
  IF(ALLOCATED(ss%SGAS_type).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%SGAS_type)
  ENDIF
  IF(ALLOCATED(ss%SGAS_type)) then
     jsize = SIZE(ss%SGAS_type)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: SGAS_type already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%SGAS_type(ss%nspec_gas))
     ss%SGAS_type = 0
  ENDIF

  isize = (ss%nrho_mist-1)*(ss%nspec_th+1)*(ss%nxs)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%sii_xs).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sii_xs)
  ENDIF
  IF(ALLOCATED(ss%sii_xs)) then
     jsize = SIZE(ss%sii_xs)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sii_xs already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sii_xs(ss%nrho_mist-1,0:ss%nspec_th,ss%nxs))
     ss%sii_xs = 0.0_rspec
     ALLOCATE(ss%id_sii_xs(0:ss%nspec_th,ss%nxs))
     ss%id_sii_xs = 0
  ENDIF

  isize = (ss%nspec_imp0)
  IF(ALLOCATED(ss%SIMP0_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%SIMP0_name)
  ENDIF
  IF(ALLOCATED(ss%SIMP0_name)) then
     jsize = SIZE(ss%SIMP0_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: SIMP0_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%SIMP0_name(ss%nspec_imp0))
     ss%SIMP0_name = " "
  ENDIF

  isize = (ss%nspec_imp0)
  IF(ALLOCATED(ss%simp0_to_s).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%simp0_to_s)
  ENDIF
  IF(ALLOCATED(ss%simp0_to_s)) then
     jsize = SIZE(ss%simp0_to_s)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: simp0_to_s already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%simp0_to_s(ss%nspec_imp0))
     ss%simp0_to_s = 0
  ENDIF

  isize = (ss%nspec_imp0)
  IF(ALLOCATED(ss%SIMP0_type).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%SIMP0_type)
  ENDIF
  IF(ALLOCATED(ss%SIMP0_type)) then
     jsize = SIZE(ss%SIMP0_type)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: SIMP0_type already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%SIMP0_type(ss%nspec_imp0))
     ss%SIMP0_type = 0
  ENDIF

  isize = (ss%nspec_impi)
  IF(ALLOCATED(ss%SIMPI_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%SIMPI_name)
  ENDIF
  IF(ALLOCATED(ss%SIMPI_name)) then
     jsize = SIZE(ss%SIMPI_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: SIMPI_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%SIMPI_name(ss%nspec_impi))
     ss%SIMPI_name = " "
  ENDIF

  isize = (ss%nspec_impi)
  IF(ALLOCATED(ss%simpi_to_s).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%simpi_to_s)
  ENDIF
  IF(ALLOCATED(ss%simpi_to_s)) then
     jsize = SIZE(ss%simpi_to_s)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: simpi_to_s already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%simpi_to_s(ss%nspec_impi))
     ss%simpi_to_s = 0
  ENDIF

  isize = (ss%nspec_impi)
  IF(ALLOCATED(ss%SIMPI_type).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%SIMPI_type)
  ENDIF
  IF(ALLOCATED(ss%SIMPI_type)) then
     jsize = SIZE(ss%SIMPI_type)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: SIMPI_type already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%SIMPI_type(ss%nspec_impi))
     ss%SIMPI_type = 0
  ENDIF

  isize = (ss%nspec_beam)
  IF(ALLOCATED(ss%SNBI_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%SNBI_name)
  ENDIF
  IF(ALLOCATED(ss%SNBI_name)) then
     jsize = SIZE(ss%SNBI_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: SNBI_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%SNBI_name(ss%nspec_beam))
     ss%SNBI_name = " "
  ENDIF

  isize = (ss%nspec_beam)
  IF(ALLOCATED(ss%snbi_to_all).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%snbi_to_all)
  ENDIF
  IF(ALLOCATED(ss%snbi_to_all)) then
     jsize = SIZE(ss%snbi_to_all)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: snbi_to_all already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%snbi_to_all(ss%nspec_beam))
     ss%snbi_to_all = 0
  ENDIF

  isize = (ss%nspec_beam)
  IF(ALLOCATED(ss%snbi_to_alla).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%snbi_to_alla)
  ENDIF
  IF(ALLOCATED(ss%snbi_to_alla)) then
     jsize = SIZE(ss%snbi_to_alla)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: snbi_to_alla already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%snbi_to_alla(ss%nspec_beam))
     ss%snbi_to_alla = 0
  ENDIF

  isize = (ss%nspec_beam)
  IF(ALLOCATED(ss%SNBI_type).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%SNBI_type)
  ENDIF
  IF(ALLOCATED(ss%SNBI_type)) then
     jsize = SIZE(ss%SNBI_type)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: SNBI_type already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%SNBI_type(ss%nspec_beam))
     ss%SNBI_type = 0
  ENDIF

  isize = (ss%nrho-1)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%sn_trans).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sn_trans)
  ENDIF
  IF(ALLOCATED(ss%sn_trans)) then
     jsize = SIZE(ss%sn_trans)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sn_trans already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sn_trans(ss%nrho-1,0:ss%nspec_th))
     ss%sn_trans = 0.0_rspec
     ALLOCATE(ss%id_sn_trans(0:ss%nspec_th))
     ss%id_sn_trans = 0
  ENDIF

  isize = (ss%nrho-1)*(ss%nspec_th+1)*(ss%ntrcat)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%sn_transcat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sn_transcat)
  ENDIF
  IF(ALLOCATED(ss%sn_transcat)) then
     jsize = SIZE(ss%sn_transcat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sn_transcat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sn_transcat(ss%nrho-1,0:ss%nspec_th,ss%ntrcat))
     ss%sn_transcat = 0.0_rspec
     ALLOCATE(ss%id_sn_transcat(0:ss%nspec_th,ss%ntrcat))
     ss%id_sn_transcat = 0
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%nspec_gas)*(ss%ngsc0)
  IF(ALLOCATED(ss%sprof0).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sprof0)
  ENDIF
  IF(ALLOCATED(ss%sprof0)) then
     jsize = SIZE(ss%sprof0)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sprof0 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sprof0(ss%nrho_gas-1,ss%nspec_gas,ss%ngsc0))
     ss%sprof0 = 0.0_rspec
     ALLOCATE(ss%id_sprof0(ss%nspec_gas,ss%ngsc0))
     ss%id_sprof0 = 0
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%ngsc0)
  IF(ALLOCATED(ss%sprof0e).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sprof0e)
  ENDIF
  IF(ALLOCATED(ss%sprof0e)) then
     jsize = SIZE(ss%sprof0e)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sprof0e already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sprof0e(ss%nrho_gas-1,ss%ngsc0))
     ss%sprof0e = 0.0_rspec
     ALLOCATE(ss%id_sprof0e(ss%ngsc0))
     ss%id_sprof0e = 0
  ENDIF

  isize = (ss%nrho_pel-1)*(ss%nspec_tha+1)
  IF(ss%nspec_tha.LE.0) isize = 0
  IF(ALLOCATED(ss%spsce).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%spsce)
  ENDIF
  IF(ALLOCATED(ss%spsce)) then
     jsize = SIZE(ss%spsce)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: spsce already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%spsce(ss%nrho_pel-1,0:ss%nspec_tha))
     ss%spsce = 0.0_rspec
     ALLOCATE(ss%id_spsce(0:ss%nspec_tha))
     ss%id_spsce = 0
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%squareLO).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%squareLO)
  ENDIF
  IF(ALLOCATED(ss%squareLO)) then
     jsize = SIZE(ss%squareLO)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: squareLO already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%squareLO(ss%nrho_eq_geo))
     ss%squareLO = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%squareUO).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%squareUO)
  ENDIF
  IF(ALLOCATED(ss%squareUO)) then
     jsize = SIZE(ss%squareUO)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: squareUO already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%squareUO(ss%nrho_eq_geo))
     ss%squareUO = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%sRtcen).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%sRtcen)
  ENDIF
  IF(ALLOCATED(ss%sRtcen)) then
     jsize = SIZE(ss%sRtcen)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: sRtcen already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%sRtcen(ss%nbeam))
     ss%sRtcen = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%surf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%surf)
  ENDIF
  IF(ALLOCATED(ss%surf)) then
     jsize = SIZE(ss%surf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: surf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%surf(ss%nrho_eq))
     ss%surf = 0.0_rspec
  ENDIF

  isize = (ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%S_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%S_name)
  ENDIF
  IF(ALLOCATED(ss%S_name)) then
     jsize = SIZE(ss%S_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: S_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%S_name(0:ss%nspec_th))
     ss%S_name = " "
  ENDIF

  isize = (ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%s_to_sa).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%s_to_sa)
  ENDIF
  IF(ALLOCATED(ss%s_to_sa)) then
     jsize = SIZE(ss%s_to_sa)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: s_to_sa already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%s_to_sa(0:ss%nspec_th))
     ss%s_to_sa = 0
  ENDIF

  isize = (ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%S_type).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%S_type)
  ENDIF
  IF(ALLOCATED(ss%S_type)) then
     jsize = SIZE(ss%S_type)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: S_type already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%S_type(0:ss%nspec_th))
     ss%S_type = 0
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%ngsc0)
  IF(ALLOCATED(ss%T0cx).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%T0cx)
  ENDIF
  IF(ALLOCATED(ss%T0cx)) then
     jsize = SIZE(ss%T0cx)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: T0cx already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%T0cx(ss%nrho_gas-1,ss%ngsc0))
     ss%T0cx = 0.0_rspec
     ALLOCATE(ss%id_T0cx(ss%ngsc0))
     ss%id_T0cx = 0
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%nspec_gas)*(ss%ngsc0)
  IF(ALLOCATED(ss%T0sc0).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%T0sc0)
  ENDIF
  IF(ALLOCATED(ss%T0sc0)) then
     jsize = SIZE(ss%T0sc0)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: T0sc0 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%T0sc0(ss%nrho_gas-1,ss%nspec_gas,ss%ngsc0))
     ss%T0sc0 = 0.0_rspec
     ALLOCATE(ss%id_T0sc0(ss%nspec_gas,ss%ngsc0))
     ss%id_T0sc0 = 0
  ENDIF

  isize = (ss%nrho_nbi-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%T0_halo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%T0_halo)
  ENDIF
  IF(ALLOCATED(ss%T0_halo)) then
     jsize = SIZE(ss%T0_halo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: T0_halo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%T0_halo(ss%nrho_nbi-1,ss%nspec_gas))
     ss%T0_halo = 0.0_rspec
     ALLOCATE(ss%id_T0_halo(ss%nspec_gas))
     ss%id_T0_halo = 0
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%T0_reco).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%T0_reco)
  ENDIF
  IF(ALLOCATED(ss%T0_reco)) then
     jsize = SIZE(ss%T0_reco)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: T0_reco already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%T0_reco(ss%nrho_gas-1,ss%nspec_gas))
     ss%T0_reco = 0.0_rspec
     ALLOCATE(ss%id_T0_reco(ss%nspec_gas))
     ss%id_T0_reco = 0
  ENDIF

END SUBROUTINE plasma_state_empty_alloc_7
