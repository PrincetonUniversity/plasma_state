! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_empty_alloc_8(ss,iout,ierr)


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

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%teflux_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%teflux_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%teflux_ky_tglf)) then
     jsize = SIZE(ss%teflux_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: teflux_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%teflux_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%teflux_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_teflux_ky_tglf(ss%nky_anomth))
     ss%id_teflux_ky_tglf = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%te_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%te_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%te_ky_tglf)) then
     jsize = SIZE(ss%te_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: te_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%te_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%te_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_te_ky_tglf(ss%nky_anomth))
     ss%id_te_ky_tglf = 0
  ENDIF

  isize = (ss%nr_rip)*(ss%nz_rip)
  IF(ALLOCATED(ss%tfrip2_log).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tfrip2_log)
  ENDIF
  IF(ALLOCATED(ss%tfrip2_log)) then
     jsize = SIZE(ss%tfrip2_log)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tfrip2_log already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tfrip2_log(ss%nr_rip,ss%nz_rip))
     ss%tfrip2_log = 0.0_rspec
  ENDIF

  isize = (ss%nr_rip)*(ss%nz_rip)
  IF(ALLOCATED(ss%tfrip_log).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tfrip_log)
  ENDIF
  IF(ALLOCATED(ss%tfrip_log)) then
     jsize = SIZE(ss%tfrip_log)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tfrip_log already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tfrip_log(ss%nr_rip,ss%nz_rip))
     ss%tfrip_log = 0.0_rspec
  ENDIF

  isize = (ss%nlhrf_src)
  IF(ALLOCATED(ss%thegrilh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%thegrilh)
  ENDIF
  IF(ALLOCATED(ss%thegrilh)) then
     jsize = SIZE(ss%thegrilh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: thegrilh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%thegrilh(ss%nlhrf_src))
     ss%thegrilh = 0.0_rspec
  ENDIF

  isize = (ss%nth_eq)
  IF(ALLOCATED(ss%th_eq).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%th_eq)
  ENDIF
  IF(ALLOCATED(ss%th_eq)) then
     jsize = SIZE(ss%th_eq)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: th_eq already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%th_eq(ss%nth_eq))
     ss%th_eq = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%Ti).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Ti)
  ENDIF
  IF(ALLOCATED(ss%Ti)) then
     jsize = SIZE(ss%Ti)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Ti already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Ti(ss%nrho-1))
     ss%Ti = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%tiflux_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tiflux_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%tiflux_ky_tglf)) then
     jsize = SIZE(ss%tiflux_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tiflux_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tiflux_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%tiflux_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_tiflux_ky_tglf(ss%nky_anomth))
     ss%id_tiflux_ky_tglf = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%ti_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ti_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%ti_ky_tglf)) then
     jsize = SIZE(ss%ti_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ti_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ti_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%ti_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_ti_ky_tglf(ss%nky_anomth))
     ss%id_ti_ky_tglf = 0
  ENDIF

  isize = (ss%nrho_anomth-1)
  IF(ALLOCATED(ss%tmodel).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tmodel)
  ENDIF
  IF(ALLOCATED(ss%tmodel)) then
     jsize = SIZE(ss%tmodel)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tmodel already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tmodel(ss%nrho_anomth-1))
     ss%tmodel = 0.0_rspec
  ENDIF

  isize = (ss%nballoon)
  IF(ALLOCATED(ss%Tornum_balcrit).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Tornum_balcrit)
  ENDIF
  IF(ALLOCATED(ss%Tornum_balcrit)) then
     jsize = SIZE(ss%Tornum_balcrit)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Tornum_balcrit already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Tornum_balcrit(ss%nballoon))
     ss%Tornum_balcrit = 0
  ENDIF

  isize = (ss%mmodes)
  IF(ALLOCATED(ss%Tor_mode_label).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Tor_mode_label)
  ENDIF
  IF(ALLOCATED(ss%Tor_mode_label)) then
     jsize = SIZE(ss%Tor_mode_label)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Tor_mode_label already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Tor_mode_label(ss%mmodes))
     ss%Tor_mode_label = " "
  ENDIF

  isize = (ss%mmodes)
  IF(ALLOCATED(ss%Tor_mode_No).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Tor_mode_No)
  ENDIF
  IF(ALLOCATED(ss%Tor_mode_No)) then
     jsize = SIZE(ss%Tor_mode_No)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Tor_mode_No already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Tor_mode_No(ss%mmodes))
     ss%Tor_mode_No = 0
  ENDIF

  isize = (ss%nrho_gas-1)
  IF(ALLOCATED(ss%tq0_reco).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tq0_reco)
  ENDIF
  IF(ALLOCATED(ss%tq0_reco)) then
     jsize = SIZE(ss%tq0_reco)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tq0_reco already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tq0_reco(ss%nrho_gas-1))
     ss%tq0_reco = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%tqb0_halo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tqb0_halo)
  ENDIF
  IF(ALLOCATED(ss%tqb0_halo)) then
     jsize = SIZE(ss%tqb0_halo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tqb0_halo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tqb0_halo(ss%nrho_nbi-1))
     ss%tqb0_halo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%tqbe).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tqbe)
  ENDIF
  IF(ALLOCATED(ss%tqbe)) then
     jsize = SIZE(ss%tqbe)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tqbe already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tqbe(ss%nrho_nbi-1))
     ss%tqbe = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%tqbi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tqbi)
  ENDIF
  IF(ALLOCATED(ss%tqbi)) then
     jsize = SIZE(ss%tqbi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tqbi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tqbi(ss%nrho_nbi-1))
     ss%tqbi = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%tqbjxb).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tqbjxb)
  ENDIF
  IF(ALLOCATED(ss%tqbjxb)) then
     jsize = SIZE(ss%tqbjxb)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tqbjxb already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tqbjxb(ss%nrho_nbi-1))
     ss%tqbjxb = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%tqbth).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tqbth)
  ENDIF
  IF(ALLOCATED(ss%tqbth)) then
     jsize = SIZE(ss%tqbth)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tqbth already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tqbth(ss%nrho_nbi-1))
     ss%tqbth = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%tqcx_halo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tqcx_halo)
  ENDIF
  IF(ALLOCATED(ss%tqcx_halo)) then
     jsize = SIZE(ss%tqcx_halo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tqcx_halo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tqcx_halo(ss%nrho_nbi-1))
     ss%tqcx_halo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_gas-1)
  IF(ALLOCATED(ss%tqcx_reco).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tqcx_reco)
  ENDIF
  IF(ALLOCATED(ss%tqcx_reco)) then
     jsize = SIZE(ss%tqcx_reco)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tqcx_reco already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tqcx_reco(ss%nrho_gas-1))
     ss%tqcx_reco = 0.0_rspec
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%ngsc0)
  IF(ALLOCATED(ss%tqioniz).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tqioniz)
  ENDIF
  IF(ALLOCATED(ss%tqioniz)) then
     jsize = SIZE(ss%tqioniz)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tqioniz already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tqioniz(ss%nrho_gas-1,ss%ngsc0))
     ss%tqioniz = 0.0_rspec
     ALLOCATE(ss%id_tqioniz(ss%ngsc0))
     ss%id_tqioniz = 0
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%ngsc0)
  IF(ALLOCATED(ss%tqqcx).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tqqcx)
  ENDIF
  IF(ALLOCATED(ss%tqqcx)) then
     jsize = SIZE(ss%tqqcx)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tqqcx already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tqqcx(ss%nrho_gas-1,ss%ngsc0))
     ss%tqqcx = 0.0_rspec
     ALLOCATE(ss%id_tqqcx(ss%ngsc0))
     ss%id_tqqcx = 0
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%tqsc_halo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tqsc_halo)
  ENDIF
  IF(ALLOCATED(ss%tqsc_halo)) then
     jsize = SIZE(ss%tqsc_halo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tqsc_halo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tqsc_halo(ss%nrho_nbi-1))
     ss%tqsc_halo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_gas-1)
  IF(ALLOCATED(ss%tqsc_reco).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tqsc_reco)
  ENDIF
  IF(ALLOCATED(ss%tqsc_reco)) then
     jsize = SIZE(ss%tqsc_reco)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tqsc_reco already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tqsc_reco(ss%nrho_gas-1))
     ss%tqsc_reco = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth-1)
  IF(ALLOCATED(ss%tq_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tq_anom)
  ENDIF
  IF(ALLOCATED(ss%tq_anom)) then
     jsize = SIZE(ss%tq_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tq_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tq_anom(ss%nrho_anomth-1))
     ss%tq_anom = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%tq_trans).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tq_trans)
  ENDIF
  IF(ALLOCATED(ss%tq_trans)) then
     jsize = SIZE(ss%tq_trans)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tq_trans already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tq_trans(ss%nrho-1))
     ss%tq_trans = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)*(ss%ntrcat)
  IF(ALLOCATED(ss%tq_transcat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tq_transcat)
  ENDIF
  IF(ALLOCATED(ss%tq_transcat)) then
     jsize = SIZE(ss%tq_transcat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tq_transcat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tq_transcat(ss%nrho-1,ss%ntrcat))
     ss%tq_transcat = 0.0_rspec
     ALLOCATE(ss%id_tq_transcat(ss%ntrcat))
     ss%id_tq_transcat = 0
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%trace_flag).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%trace_flag)
  ENDIF
  IF(ALLOCATED(ss%trace_flag)) then
     jsize = SIZE(ss%trace_flag)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: trace_flag already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%trace_flag(ss%nbeam))
     ss%trace_flag = 0
  ENDIF

  isize = (ss%ntrcat)
  IF(ALLOCATED(ss%trcat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%trcat)
  ENDIF
  IF(ALLOCATED(ss%trcat)) then
     jsize = SIZE(ss%trcat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: trcat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%trcat(ss%ntrcat))
     ss%trcat = " "
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%triang).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%triang)
  ENDIF
  IF(ALLOCATED(ss%triang)) then
     jsize = SIZE(ss%triang)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: triang already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%triang(ss%nrho_eq_geo))
     ss%triang = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%triangL).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%triangL)
  ENDIF
  IF(ALLOCATED(ss%triangL)) then
     jsize = SIZE(ss%triangL)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: triangL already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%triangL(ss%nrho_eq_geo))
     ss%triangL = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%triangU).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%triangU)
  ENDIF
  IF(ALLOCATED(ss%triangU)) then
     jsize = SIZE(ss%triangU)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: triangU already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%triangU(ss%nrho_eq_geo))
     ss%triangU = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%triang_miller_L).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%triang_miller_L)
  ENDIF
  IF(ALLOCATED(ss%triang_miller_L)) then
     jsize = SIZE(ss%triang_miller_L)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: triang_miller_L already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%triang_miller_L(ss%nrho_eq_geo))
     ss%triang_miller_L = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%triang_miller_U).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%triang_miller_U)
  ENDIF
  IF(ALLOCATED(ss%triang_miller_U)) then
     jsize = SIZE(ss%triang_miller_U)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: triang_miller_U already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%triang_miller_U(ss%nrho_eq_geo))
     ss%triang_miller_U = 0.0_rspec
  ENDIF

  isize = (ss%ntrcat)
  IF(ALLOCATED(ss%trmodels).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%trmodels)
  ENDIF
  IF(ALLOCATED(ss%trmodels)) then
     jsize = SIZE(ss%trmodels)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: trmodels already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%trmodels(ss%ntrcat))
     ss%trmodels = " "
  ENDIF

  isize = (ss%nrho-1)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%Ts).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Ts)
  ENDIF
  IF(ALLOCATED(ss%Ts)) then
     jsize = SIZE(ss%Ts)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Ts already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Ts(ss%nrho-1,0:ss%nspec_th))
     ss%Ts = 0.0_rspec
     ALLOCATE(ss%id_Ts(0:ss%nspec_th))
     ss%id_Ts = 0
  ENDIF

  isize = (ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%Ts_is_input).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Ts_is_input)
  ENDIF
  IF(ALLOCATED(ss%Ts_is_input)) then
     jsize = SIZE(ss%Ts_is_input)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Ts_is_input already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Ts_is_input(0:ss%nspec_th))
     ss%Ts_is_input = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%tzflux_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tzflux_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%tzflux_ky_tglf)) then
     jsize = SIZE(ss%tzflux_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tzflux_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tzflux_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%tzflux_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_tzflux_ky_tglf(ss%nky_anomth))
     ss%id_tzflux_ky_tglf = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%tz_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%tz_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%tz_ky_tglf)) then
     jsize = SIZE(ss%tz_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: tz_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%tz_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%tz_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_tz_ky_tglf(ss%nky_anomth))
     ss%id_tz_ky_tglf = 0
  ENDIF

  isize = (ss%n_t_einj_standard)
  IF(ALLOCATED(ss%T_einj_standard).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%T_einj_standard)
  ENDIF
  IF(ALLOCATED(ss%T_einj_standard)) then
     jsize = SIZE(ss%T_einj_standard)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: T_einj_standard already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%T_einj_standard(ss%n_t_einj_standard))
     ss%T_einj_standard = 0.0_rspec
  ENDIF

  isize = (ss%n_t_einj_standard)
  IF(ALLOCATED(ss%T_ffull_standard).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%T_ffull_standard)
  ENDIF
  IF(ALLOCATED(ss%T_ffull_standard)) then
     jsize = SIZE(ss%T_ffull_standard)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: T_ffull_standard already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%T_ffull_standard(ss%n_t_einj_standard))
     ss%T_ffull_standard = 0.0_rspec
  ENDIF

  isize = (ss%n_t_einj_standard)
  IF(ALLOCATED(ss%T_fhalf_standard).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%T_fhalf_standard)
  ENDIF
  IF(ALLOCATED(ss%T_fhalf_standard)) then
     jsize = SIZE(ss%T_fhalf_standard)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: T_fhalf_standard already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%T_fhalf_standard(ss%n_t_einj_standard))
     ss%T_fhalf_standard = 0.0_rspec
  ENDIF

  isize = (ss%nrho)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%upwind_pfrac_ns).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%upwind_pfrac_ns)
  ENDIF
  IF(ALLOCATED(ss%upwind_pfrac_ns)) then
     jsize = SIZE(ss%upwind_pfrac_ns)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: upwind_pfrac_ns already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%upwind_pfrac_ns(ss%nrho,0:ss%nspec_th))
     ss%upwind_pfrac_ns = 0.0_rspec
     ALLOCATE(ss%id_upwind_pfrac_ns(0:ss%nspec_th))
     ss%id_upwind_pfrac_ns = 0
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%upwind_pfrac_omega).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%upwind_pfrac_omega)
  ENDIF
  IF(ALLOCATED(ss%upwind_pfrac_omega)) then
     jsize = SIZE(ss%upwind_pfrac_omega)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: upwind_pfrac_omega already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%upwind_pfrac_omega(ss%nrho))
     ss%upwind_pfrac_omega = 0.0_rspec
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%upwind_pfrac_Te).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%upwind_pfrac_Te)
  ENDIF
  IF(ALLOCATED(ss%upwind_pfrac_Te)) then
     jsize = SIZE(ss%upwind_pfrac_Te)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: upwind_pfrac_Te already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%upwind_pfrac_Te(ss%nrho))
     ss%upwind_pfrac_Te = 0.0_rspec
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%upwind_pfrac_Ti).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%upwind_pfrac_Ti)
  ENDIF
  IF(ALLOCATED(ss%upwind_pfrac_Ti)) then
     jsize = SIZE(ss%upwind_pfrac_Ti)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: upwind_pfrac_Ti already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%upwind_pfrac_Ti(ss%nrho))
     ss%upwind_pfrac_Ti = 0.0_rspec
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%vee_trans).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vee_trans)
  ENDIF
  IF(ALLOCATED(ss%vee_trans)) then
     jsize = SIZE(ss%vee_trans)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vee_trans already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vee_trans(ss%nrho))
     ss%vee_trans = 0.0_rspec
  ENDIF

  isize = (ss%nrho)*(ss%ntrcat)
  IF(ALLOCATED(ss%vee_transcat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%vee_transcat)
  ENDIF
  IF(ALLOCATED(ss%vee_transcat)) then
     jsize = SIZE(ss%vee_transcat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: vee_transcat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%vee_transcat(ss%nrho,ss%ntrcat))
     ss%vee_transcat = 0.0_rspec
     ALLOCATE(ss%id_vee_transcat(ss%ntrcat))
     ss%id_vee_transcat = 0
  ENDIF

  isize = (ss%nrho_anom)
  IF(ALLOCATED(ss%velb_fusi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velb_fusi)
  ENDIF
  IF(ALLOCATED(ss%velb_fusi)) then
     jsize = SIZE(ss%velb_fusi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velb_fusi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velb_fusi(ss%nrho_anom))
     ss%velb_fusi = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anom)
  IF(ALLOCATED(ss%velb_nbi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velb_nbi)
  ENDIF
  IF(ALLOCATED(ss%velb_nbi)) then
     jsize = SIZE(ss%velb_nbi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velb_nbi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velb_nbi(ss%nrho_anom))
     ss%velb_nbi = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anom)
  IF(ALLOCATED(ss%velb_rfmi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velb_rfmi)
  ENDIF
  IF(ALLOCATED(ss%velb_rfmi)) then
     jsize = SIZE(ss%velb_rfmi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velb_rfmi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velb_rfmi(ss%nrho_anom))
     ss%velb_rfmi = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velne_nclass).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velne_nclass)
  ENDIF
  IF(ALLOCATED(ss%velne_nclass)) then
     jsize = SIZE(ss%velne_nclass)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velne_nclass already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velne_nclass(ss%nrho_anomth))
     ss%velne_nclass = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velnh_nclass).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velnh_nclass)
  ENDIF
  IF(ALLOCATED(ss%velnh_nclass)) then
     jsize = SIZE(ss%velnh_nclass)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velnh_nclass already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velnh_nclass(ss%nrho_anomth))
     ss%velnh_nclass = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velni_drbm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velni_drbm)
  ENDIF
  IF(ALLOCATED(ss%velni_drbm)) then
     jsize = SIZE(ss%velni_drbm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velni_drbm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velni_drbm(ss%nrho_anomth))
     ss%velni_drbm = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%velns_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velns_anom)
  ENDIF
  IF(ALLOCATED(ss%velns_anom)) then
     jsize = SIZE(ss%velns_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velns_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velns_anom(ss%nrho_anomth,0:ss%nspec_th))
     ss%velns_anom = 0.0_rspec
     ALLOCATE(ss%id_velns_anom(0:ss%nspec_th))
     ss%id_velns_anom = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%velns_glf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velns_glf)
  ENDIF
  IF(ALLOCATED(ss%velns_glf)) then
     jsize = SIZE(ss%velns_glf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velns_glf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velns_glf(ss%nrho_anomth,0:ss%nspec_th))
     ss%velns_glf = 0.0_rspec
     ALLOCATE(ss%id_velns_glf(0:ss%nspec_th))
     ss%id_velns_glf = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%velns_gtcneo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velns_gtcneo)
  ENDIF
  IF(ALLOCATED(ss%velns_gtcneo)) then
     jsize = SIZE(ss%velns_gtcneo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velns_gtcneo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velns_gtcneo(ss%nrho_anomth,0:ss%nspec_th))
     ss%velns_gtcneo = 0.0_rspec
     ALLOCATE(ss%id_velns_gtcneo(0:ss%nspec_th))
     ss%id_velns_gtcneo = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%velns_mmm71).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velns_mmm71)
  ENDIF
  IF(ALLOCATED(ss%velns_mmm71)) then
     jsize = SIZE(ss%velns_mmm71)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velns_mmm71 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velns_mmm71(ss%nrho_anomth,0:ss%nspec_th))
     ss%velns_mmm71 = 0.0_rspec
     ALLOCATE(ss%id_velns_mmm71(0:ss%nspec_th))
     ss%id_velns_mmm71 = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%velns_nclass).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velns_nclass)
  ENDIF
  IF(ALLOCATED(ss%velns_nclass)) then
     jsize = SIZE(ss%velns_nclass)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velns_nclass already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velns_nclass(ss%nrho_anomth,0:ss%nspec_th))
     ss%velns_nclass = 0.0_rspec
     ALLOCATE(ss%id_velns_nclass(0:ss%nspec_th))
     ss%id_velns_nclass = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%velns_neo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velns_neo)
  ENDIF
  IF(ALLOCATED(ss%velns_neo)) then
     jsize = SIZE(ss%velns_neo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velns_neo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velns_neo(ss%nrho_anomth,0:ss%nspec_th))
     ss%velns_neo = 0.0_rspec
     ALLOCATE(ss%id_velns_neo(0:ss%nspec_th))
     ss%id_velns_neo = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%velns_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velns_tglf)
  ENDIF
  IF(ALLOCATED(ss%velns_tglf)) then
     jsize = SIZE(ss%velns_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velns_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velns_tglf(ss%nrho_anomth,0:ss%nspec_th))
     ss%velns_tglf = 0.0_rspec
     ALLOCATE(ss%id_velns_tglf(0:ss%nspec_th))
     ss%id_velns_tglf = 0
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velnx_nclass).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velnx_nclass)
  ENDIF
  IF(ALLOCATED(ss%velnx_nclass)) then
     jsize = SIZE(ss%velnx_nclass)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velnx_nclass already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velnx_nclass(ss%nrho_anomth))
     ss%velnx_nclass = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velpphi_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velpphi_anom)
  ENDIF
  IF(ALLOCATED(ss%velpphi_anom)) then
     jsize = SIZE(ss%velpphi_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velpphi_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velpphi_anom(ss%nrho_anomth))
     ss%velpphi_anom = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velpphi_glf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velpphi_glf)
  ENDIF
  IF(ALLOCATED(ss%velpphi_glf)) then
     jsize = SIZE(ss%velpphi_glf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velpphi_glf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velpphi_glf(ss%nrho_anomth))
     ss%velpphi_glf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velpphi_gtcneo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velpphi_gtcneo)
  ENDIF
  IF(ALLOCATED(ss%velpphi_gtcneo)) then
     jsize = SIZE(ss%velpphi_gtcneo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velpphi_gtcneo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velpphi_gtcneo(ss%nrho_anomth))
     ss%velpphi_gtcneo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velpphi_mmm71).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velpphi_mmm71)
  ENDIF
  IF(ALLOCATED(ss%velpphi_mmm71)) then
     jsize = SIZE(ss%velpphi_mmm71)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velpphi_mmm71 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velpphi_mmm71(ss%nrho_anomth))
     ss%velpphi_mmm71 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%velpphi_neo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%velpphi_neo)
  ENDIF
  IF(ALLOCATED(ss%velpphi_neo)) then
     jsize = SIZE(ss%velpphi_neo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: velpphi_neo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%velpphi_neo(ss%nrho_anomth))
     ss%velpphi_neo = 0.0_rspec
  ENDIF

END SUBROUTINE plasma_state_empty_alloc_8
