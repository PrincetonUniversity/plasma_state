! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_empty_alloc_5(ss,iout,ierr)


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

  isize = (ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%ns_bdy).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ns_bdy)
  ENDIF
  IF(ALLOCATED(ss%ns_bdy)) then
     jsize = SIZE(ss%ns_bdy)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ns_bdy already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ns_bdy(0:ss%nspec_th))
     ss%ns_bdy = 0.0_rspec
  ENDIF

  isize = (ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%ns_is_input).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ns_is_input)
  ENDIF
  IF(ALLOCATED(ss%ns_is_input)) then
     jsize = SIZE(ss%ns_is_input)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ns_is_input already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ns_is_input(0:ss%nspec_th))
     ss%ns_is_input = 0
  ENDIF

  isize = (ss%nlhrf_src)
  IF(ALLOCATED(ss%nthinlh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nthinlh)
  ENDIF
  IF(ALLOCATED(ss%nthinlh)) then
     jsize = SIZE(ss%nthinlh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nthinlh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nthinlh(ss%nlhrf_src))
     ss%nthinlh = 0
  ENDIF

  isize = (ss%ncoils)
  IF(ALLOCATED(ss%Nturns).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Nturns)
  ENDIF
  IF(ALLOCATED(ss%Nturns)) then
     jsize = SIZE(ss%Nturns)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Nturns already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Nturns(ss%ncoils))
     ss%Nturns = 0
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%num_nphi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%num_nphi)
  ENDIF
  IF(ALLOCATED(ss%num_nphi)) then
     jsize = SIZE(ss%num_nphi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: num_nphi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%num_nphi(ss%nicrf_src))
     ss%num_nphi = 0
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%num_nphi_vac).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%num_nphi_vac)
  ENDIF
  IF(ALLOCATED(ss%num_nphi_vac)) then
     jsize = SIZE(ss%num_nphi_vac)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: num_nphi_vac already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%num_nphi_vac(ss%nicrf_src))
     ss%num_nphi_vac = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%nzflux_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nzflux_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%nzflux_ky_tglf)) then
     jsize = SIZE(ss%nzflux_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nzflux_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nzflux_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%nzflux_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_nzflux_ky_tglf(ss%nky_anomth))
     ss%id_nzflux_ky_tglf = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%nz_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nz_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%nz_ky_tglf)) then
     jsize = SIZE(ss%nz_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nz_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nz_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%nz_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_nz_ky_tglf(ss%nky_anomth))
     ss%id_nz_ky_tglf = 0
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%n_straps).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%n_straps)
  ENDIF
  IF(ALLOCATED(ss%n_straps)) then
     jsize = SIZE(ss%n_straps)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: n_straps already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%n_straps(ss%nicrf_src))
     ss%n_straps = 0
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%ngsc0)
  IF(ALLOCATED(ss%omeg0cx).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%omeg0cx)
  ENDIF
  IF(ALLOCATED(ss%omeg0cx)) then
     jsize = SIZE(ss%omeg0cx)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: omeg0cx already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%omeg0cx(ss%nrho_gas-1,ss%ngsc0))
     ss%omeg0cx = 0.0_rspec
     ALLOCATE(ss%id_omeg0cx(ss%ngsc0))
     ss%id_omeg0cx = 0
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%nspec_gas)*(ss%ngsc0)
  IF(ALLOCATED(ss%omeg0sc0).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%omeg0sc0)
  ENDIF
  IF(ALLOCATED(ss%omeg0sc0)) then
     jsize = SIZE(ss%omeg0sc0)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: omeg0sc0 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%omeg0sc0(ss%nrho_gas-1,ss%nspec_gas,ss%ngsc0))
     ss%omeg0sc0 = 0.0_rspec
     ALLOCATE(ss%id_omeg0sc0(ss%nspec_gas,ss%ngsc0))
     ss%id_omeg0sc0 = 0
  ENDIF

  isize = (ss%nrho_nbi-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%omeg0_halo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%omeg0_halo)
  ENDIF
  IF(ALLOCATED(ss%omeg0_halo)) then
     jsize = SIZE(ss%omeg0_halo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: omeg0_halo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%omeg0_halo(ss%nrho_nbi-1,ss%nspec_gas))
     ss%omeg0_halo = 0.0_rspec
     ALLOCATE(ss%id_omeg0_halo(ss%nspec_gas))
     ss%id_omeg0_halo = 0
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%omeg0_reco).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%omeg0_reco)
  ENDIF
  IF(ALLOCATED(ss%omeg0_reco)) then
     jsize = SIZE(ss%omeg0_reco)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: omeg0_reco already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%omeg0_reco(ss%nrho_gas-1,ss%nspec_gas))
     ss%omeg0_reco = 0.0_rspec
     ALLOCATE(ss%id_omeg0_reco(ss%nspec_gas))
     ss%id_omeg0_reco = 0
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%omegat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%omegat)
  ENDIF
  IF(ALLOCATED(ss%omegat)) then
     jsize = SIZE(ss%omegat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: omegat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%omegat(ss%nrho-1))
     ss%omegat = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%omega_exb).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%omega_exb)
  ENDIF
  IF(ALLOCATED(ss%omega_exb)) then
     jsize = SIZE(ss%omega_exb)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: omega_exb already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%omega_exb(ss%nrho_anomth))
     ss%omega_exb = 0.0_rspec
  ENDIF

  isize = (ss%nrho_gas-1)
  IF(ALLOCATED(ss%p0_reco).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%p0_reco)
  ENDIF
  IF(ALLOCATED(ss%p0_reco)) then
     jsize = SIZE(ss%p0_reco)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: p0_reco already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%p0_reco(ss%nrho_gas-1))
     ss%p0_reco = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%pb0_halo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pb0_halo)
  ENDIF
  IF(ALLOCATED(ss%pb0_halo)) then
     jsize = SIZE(ss%pb0_halo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pb0_halo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pb0_halo(ss%nrho_nbi-1))
     ss%pb0_halo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%pbe).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pbe)
  ENDIF
  IF(ALLOCATED(ss%pbe)) then
     jsize = SIZE(ss%pbe)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pbe already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pbe(ss%nrho_nbi-1))
     ss%pbe = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%pbi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pbi)
  ENDIF
  IF(ALLOCATED(ss%pbi)) then
     jsize = SIZE(ss%pbi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pbi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pbi(ss%nrho_nbi-1))
     ss%pbi = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%pbth).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pbth)
  ENDIF
  IF(ALLOCATED(ss%pbth)) then
     jsize = SIZE(ss%pbth)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pbth already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pbth(ss%nrho_nbi-1))
     ss%pbth = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%pcx_halo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pcx_halo)
  ENDIF
  IF(ALLOCATED(ss%pcx_halo)) then
     jsize = SIZE(ss%pcx_halo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pcx_halo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pcx_halo(ss%nrho_nbi-1))
     ss%pcx_halo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_gas-1)
  IF(ALLOCATED(ss%pcx_reco).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pcx_reco)
  ENDIF
  IF(ALLOCATED(ss%pcx_reco)) then
     jsize = SIZE(ss%pcx_reco)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pcx_reco already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pcx_reco(ss%nrho_gas-1))
     ss%pcx_reco = 0.0_rspec
  ENDIF

  isize = (ss%nrho_ecrf-1)
  IF(ALLOCATED(ss%peech).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%peech)
  ENDIF
  IF(ALLOCATED(ss%peech)) then
     jsize = SIZE(ss%peech)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: peech already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%peech(ss%nrho_ecrf-1))
     ss%peech = 0.0_rspec
  ENDIF

  isize = (ss%nrho_ecrf-1)*(ss%necrf_src)
  IF(ALLOCATED(ss%peech_src).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%peech_src)
  ENDIF
  IF(ALLOCATED(ss%peech_src)) then
     jsize = SIZE(ss%peech_src)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: peech_src already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%peech_src(ss%nrho_ecrf-1,ss%necrf_src))
     ss%peech_src = 0.0_rspec
     ALLOCATE(ss%id_peech_src(ss%necrf_src))
     ss%id_peech_src = 0
  ENDIF

  isize = (ss%nrho_lhrf-1)
  IF(ALLOCATED(ss%pelh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pelh)
  ENDIF
  IF(ALLOCATED(ss%pelh)) then
     jsize = SIZE(ss%pelh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pelh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pelh(ss%nrho_lhrf-1))
     ss%pelh = 0.0_rspec
  ENDIF

  isize = (ss%nrho_lhrf-1)*(ss%nlhrf_src)
  IF(ALLOCATED(ss%pelh_src).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pelh_src)
  ENDIF
  IF(ALLOCATED(ss%pelh_src)) then
     jsize = SIZE(ss%pelh_src)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pelh_src already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pelh_src(ss%nrho_lhrf-1,ss%nlhrf_src))
     ss%pelh_src = 0.0_rspec
     ALLOCATE(ss%id_pelh_src(ss%nlhrf_src))
     ss%id_pelh_src = 0
  ENDIF

  isize = (ss%nrho_anomth-1)
  IF(ALLOCATED(ss%pe_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pe_anom)
  ENDIF
  IF(ALLOCATED(ss%pe_anom)) then
     jsize = SIZE(ss%pe_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pe_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pe_anom(ss%nrho_anomth-1))
     ss%pe_anom = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%pe_trans).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pe_trans)
  ENDIF
  IF(ALLOCATED(ss%pe_trans)) then
     jsize = SIZE(ss%pe_trans)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pe_trans already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pe_trans(ss%nrho-1))
     ss%pe_trans = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)*(ss%ntrcat)
  IF(ALLOCATED(ss%pe_transcat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pe_transcat)
  ENDIF
  IF(ALLOCATED(ss%pe_transcat)) then
     jsize = SIZE(ss%pe_transcat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pe_transcat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pe_transcat(ss%nrho-1,ss%ntrcat))
     ss%pe_transcat = 0.0_rspec
     ALLOCATE(ss%id_pe_transcat(ss%ntrcat))
     ss%id_pe_transcat = 0
  ENDIF

  isize = (ss%nrho_fus-1)
  IF(ALLOCATED(ss%pfuse).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pfuse)
  ENDIF
  IF(ALLOCATED(ss%pfuse)) then
     jsize = SIZE(ss%pfuse)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pfuse already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pfuse(ss%nrho_fus-1))
     ss%pfuse = 0.0_rspec
  ENDIF

  isize = (ss%nrho_fus-1)
  IF(ALLOCATED(ss%pfusi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pfusi)
  ENDIF
  IF(ALLOCATED(ss%pfusi)) then
     jsize = SIZE(ss%pfusi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pfusi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pfusi(ss%nrho_fus-1))
     ss%pfusi = 0.0_rspec
  ENDIF

  isize = (ss%nrho_fus-1)
  IF(ALLOCATED(ss%pfusth).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pfusth)
  ENDIF
  IF(ALLOCATED(ss%pfusth)) then
     jsize = SIZE(ss%pfusth)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pfusth already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pfusth(ss%nrho_fus-1))
     ss%pfusth = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%Phibsc).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Phibsc)
  ENDIF
  IF(ALLOCATED(ss%Phibsc)) then
     jsize = SIZE(ss%Phibsc)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Phibsc already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Phibsc(ss%nbeam))
     ss%Phibsc = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%phiflux_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%phiflux_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%phiflux_ky_tglf)) then
     jsize = SIZE(ss%phiflux_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: phiflux_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%phiflux_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%phiflux_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_phiflux_ky_tglf(ss%nky_anomth))
     ss%id_phiflux_ky_tglf = 0
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%phit).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%phit)
  ENDIF
  IF(ALLOCATED(ss%phit)) then
     jsize = SIZE(ss%phit)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: phit already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%phit(ss%nrho_eq))
     ss%phit = 0.0_rspec
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%Phi_EC_launch).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Phi_EC_launch)
  ENDIF
  IF(ALLOCATED(ss%Phi_EC_launch)) then
     jsize = SIZE(ss%Phi_EC_launch)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Phi_EC_launch already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Phi_EC_launch(ss%necrf_src))
     ss%Phi_EC_launch = 0.0_rspec
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%picrf_abs).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%picrf_abs)
  ENDIF
  IF(ALLOCATED(ss%picrf_abs)) then
     jsize = SIZE(ss%picrf_abs)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: picrf_abs already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%picrf_abs(ss%nicrf_src))
     ss%picrf_abs = 0.0_rspec
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%picrf_ext).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%picrf_ext)
  ENDIF
  IF(ALLOCATED(ss%picrf_ext)) then
     jsize = SIZE(ss%picrf_ext)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: picrf_ext already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%picrf_ext(ss%nicrf_src))
     ss%picrf_ext = 0.0_rspec
  ENDIF

  isize = (ss%nrho_icrf-1)*(ss%max_num_nphi)*(ss%nicrf_src)*(ss%nspec_alla+1)
  IF(ss%nspec_alla.LE.0) isize = 0
  IF(ALLOCATED(ss%picrf_nphi_srcs).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%picrf_nphi_srcs)
  ENDIF
  IF(ALLOCATED(ss%picrf_nphi_srcs)) then
     jsize = SIZE(ss%picrf_nphi_srcs)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: picrf_nphi_srcs already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%picrf_nphi_srcs(ss%nrho_icrf-1,ss%max_num_nphi,ss%nicrf_src,0:ss%nspec_alla))
     ss%picrf_nphi_srcs = 0.0_rspec
     ALLOCATE(ss%id_picrf_nphi_srcs(ss%max_num_nphi,ss%nicrf_src,0:ss%nspec_alla))
     ss%id_picrf_nphi_srcs = 0
  ENDIF

  isize = (ss%nrho_icrf-1)*(ss%nicrf_src)*(ss%nspec_alla+1)
  IF(ss%nspec_alla.LE.0) isize = 0
  IF(ALLOCATED(ss%picrf_srcs).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%picrf_srcs)
  ENDIF
  IF(ALLOCATED(ss%picrf_srcs)) then
     jsize = SIZE(ss%picrf_srcs)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: picrf_srcs already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%picrf_srcs(ss%nrho_icrf-1,ss%nicrf_src,0:ss%nspec_alla))
     ss%picrf_srcs = 0.0_rspec
     ALLOCATE(ss%id_picrf_srcs(ss%nicrf_src,0:ss%nspec_alla))
     ss%id_picrf_srcs = 0
  ENDIF

  isize = (ss%nrho_icrf-1)*(ss%nspec_alla+1)
  IF(ss%nspec_alla.LE.0) isize = 0
  IF(ALLOCATED(ss%picrf_totals).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%picrf_totals)
  ENDIF
  IF(ALLOCATED(ss%picrf_totals)) then
     jsize = SIZE(ss%picrf_totals)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: picrf_totals already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%picrf_totals(ss%nrho_icrf-1,0:ss%nspec_alla))
     ss%picrf_totals = 0.0_rspec
     ALLOCATE(ss%id_picrf_totals(0:ss%nspec_alla))
     ss%id_picrf_totals = 0
  ENDIF

  isize = (ss%nrho_icrf-1)
  IF(ALLOCATED(ss%picth).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%picth)
  ENDIF
  IF(ALLOCATED(ss%picth)) then
     jsize = SIZE(ss%picth)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: picth already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%picth(ss%nrho_icrf-1))
     ss%picth = 0.0_rspec
  ENDIF

  isize = (ss%nrho_lhrf-1)
  IF(ALLOCATED(ss%pilh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pilh)
  ENDIF
  IF(ALLOCATED(ss%pilh)) then
     jsize = SIZE(ss%pilh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pilh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pilh(ss%nrho_lhrf-1))
     ss%pilh = 0.0_rspec
  ENDIF

  isize = (ss%nrho_lhrf-1)*(ss%nlhrf_src)
  IF(ALLOCATED(ss%pilh_src).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pilh_src)
  ENDIF
  IF(ALLOCATED(ss%pilh_src)) then
     jsize = SIZE(ss%pilh_src)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pilh_src already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pilh_src(ss%nrho_lhrf-1,ss%nlhrf_src))
     ss%pilh_src = 0.0_rspec
     ALLOCATE(ss%id_pilh_src(ss%nlhrf_src))
     ss%id_pilh_src = 0
  ENDIF

  isize = (ss%nrho_anomth-1)
  IF(ALLOCATED(ss%pi_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pi_anom)
  ENDIF
  IF(ALLOCATED(ss%pi_anom)) then
     jsize = SIZE(ss%pi_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pi_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pi_anom(ss%nrho_anomth-1))
     ss%pi_anom = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%pi_trans).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pi_trans)
  ENDIF
  IF(ALLOCATED(ss%pi_trans)) then
     jsize = SIZE(ss%pi_trans)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pi_trans already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pi_trans(ss%nrho-1))
     ss%pi_trans = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)*(ss%ntrcat)
  IF(ALLOCATED(ss%pi_transcat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pi_transcat)
  ENDIF
  IF(ALLOCATED(ss%pi_transcat)) then
     jsize = SIZE(ss%pi_transcat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pi_transcat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pi_transcat(ss%nrho-1,ss%ntrcat))
     ss%pi_transcat = 0.0_rspec
     ALLOCATE(ss%id_pi_transcat(ss%ntrcat))
     ss%id_pi_transcat = 0
  ENDIF

  isize = (ss%nrho_icrf-1)
  IF(ALLOCATED(ss%pmine).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pmine)
  ENDIF
  IF(ALLOCATED(ss%pmine)) then
     jsize = SIZE(ss%pmine)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pmine already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pmine(ss%nrho_icrf-1))
     ss%pmine = 0.0_rspec
  ENDIF

  isize = (ss%nrho_icrf-1)
  IF(ALLOCATED(ss%pmini).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pmini)
  ENDIF
  IF(ALLOCATED(ss%pmini)) then
     jsize = SIZE(ss%pmini)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pmini already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pmini(ss%nrho_icrf-1))
     ss%pmini = 0.0_rspec
  ENDIF

  isize = (ss%nrho_icrf-1)
  IF(ALLOCATED(ss%pminth).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pminth)
  ENDIF
  IF(ALLOCATED(ss%pminth)) then
     jsize = SIZE(ss%pminth)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pminth already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pminth(ss%nrho_icrf-1))
     ss%pminth = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%pohme).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pohme)
  ENDIF
  IF(ALLOCATED(ss%pohme)) then
     jsize = SIZE(ss%pohme)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pohme already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pohme(ss%nrho-1))
     ss%pohme = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%pot_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%pot_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%pot_ky_tglf)) then
     jsize = SIZE(ss%pot_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: pot_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%pot_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%pot_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_pot_ky_tglf(ss%nky_anomth))
     ss%id_pot_ky_tglf = 0
  ENDIF

  isize = (ss%necrf_src)
  IF(ALLOCATED(ss%power_ec).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%power_ec)
  ENDIF
  IF(ALLOCATED(ss%power_ec)) then
     jsize = SIZE(ss%power_ec)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: power_ec already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%power_ec(ss%necrf_src))
     ss%power_ec = 0.0_rspec
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%power_ic).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%power_ic)
  ENDIF
  IF(ALLOCATED(ss%power_ic)) then
     jsize = SIZE(ss%power_ic)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: power_ic already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%power_ic(ss%nicrf_src))
     ss%power_ic = 0.0_rspec
  ENDIF

  isize = (ss%nlhrf_src)
  IF(ALLOCATED(ss%power_lh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%power_lh)
  ENDIF
  IF(ALLOCATED(ss%power_lh)) then
     jsize = SIZE(ss%power_lh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: power_lh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%power_lh(ss%nlhrf_src))
     ss%power_lh = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%power_nbi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%power_nbi)
  ENDIF
  IF(ALLOCATED(ss%power_nbi)) then
     jsize = SIZE(ss%power_nbi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: power_nbi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%power_nbi(ss%nbeam))
     ss%power_nbi = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%power_nbi_trace).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%power_nbi_trace)
  ENDIF
  IF(ALLOCATED(ss%power_nbi_trace)) then
     jsize = SIZE(ss%power_nbi_trace)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: power_nbi_trace already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%power_nbi_trace(ss%nbeam))
     ss%power_nbi_trace = 0.0_rspec
  ENDIF

  isize = (ss%nrho_rad-1)
  IF(ALLOCATED(ss%prad).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%prad)
  ENDIF
  IF(ALLOCATED(ss%prad)) then
     jsize = SIZE(ss%prad)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: prad already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%prad(ss%nrho_rad-1))
     ss%prad = 0.0_rspec
  ENDIF

  isize = (ss%nrho_rad-1)
  IF(ALLOCATED(ss%prad_br).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%prad_br)
  ENDIF
  IF(ALLOCATED(ss%prad_br)) then
     jsize = SIZE(ss%prad_br)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: prad_br already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%prad_br(ss%nrho_rad-1))
     ss%prad_br = 0.0_rspec
  ENDIF

  isize = (ss%nrho_rad-1)
  IF(ALLOCATED(ss%prad_cy).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%prad_cy)
  ENDIF
  IF(ALLOCATED(ss%prad_cy)) then
     jsize = SIZE(ss%prad_cy)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: prad_cy already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%prad_cy(ss%nrho_rad-1))
     ss%prad_cy = 0.0_rspec
  ENDIF

END SUBROUTINE plasma_state_empty_alloc_5
