! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_empty_alloc_6(ss,iout,ierr)


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

  isize = (ss%nrho_rad-1)
  IF(ALLOCATED(ss%prad_li).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%prad_li)
  ENDIF
  IF(ALLOCATED(ss%prad_li)) then
     jsize = SIZE(ss%prad_li)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: prad_li already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%prad_li(ss%nrho_rad-1))
     ss%prad_li = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)
  IF(ALLOCATED(ss%psc_halo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%psc_halo)
  ENDIF
  IF(ALLOCATED(ss%psc_halo)) then
     jsize = SIZE(ss%psc_halo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: psc_halo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%psc_halo(ss%nrho_nbi-1))
     ss%psc_halo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_gas-1)
  IF(ALLOCATED(ss%psc_reco).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%psc_reco)
  ENDIF
  IF(ALLOCATED(ss%psc_reco)) then
     jsize = SIZE(ss%psc_reco)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: psc_reco already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%psc_reco(ss%nrho_gas-1))
     ss%psc_reco = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%psipol).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%psipol)
  ENDIF
  IF(ALLOCATED(ss%psipol)) then
     jsize = SIZE(ss%psipol)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: psipol already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%psipol(ss%nrho_eq))
     ss%psipol = 0.0_rspec
  ENDIF

  isize = (ss%nr)*(ss%nz)
  IF(ALLOCATED(ss%PsiRZ).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%PsiRZ)
  ENDIF
  IF(ALLOCATED(ss%PsiRZ)) then
     jsize = SIZE(ss%PsiRZ)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: PsiRZ already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%PsiRZ(ss%nr,ss%nz))
     ss%PsiRZ = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%psmom_errck).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%psmom_errck)
  ENDIF
  IF(ALLOCATED(ss%psmom_errck)) then
     jsize = SIZE(ss%psmom_errck)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: psmom_errck already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%psmom_errck(ss%nrho_eq_geo))
     ss%psmom_errck = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)*(ss%npsmom)
  IF(ALLOCATED(ss%psmom_nc).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%psmom_nc)
  ENDIF
  IF(ALLOCATED(ss%psmom_nc)) then
     jsize = SIZE(ss%psmom_nc)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: psmom_nc already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%psmom_nc(ss%nrho_eq_geo,ss%npsmom))
     ss%psmom_nc = 0.0_rspec
     ALLOCATE(ss%id_psmom_nc(ss%npsmom))
     ss%id_psmom_nc = 0
  ENDIF

  isize = (ss%npsmom)
  IF(ALLOCATED(ss%psmom_num).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%psmom_num)
  ENDIF
  IF(ALLOCATED(ss%psmom_num)) then
     jsize = SIZE(ss%psmom_num)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: psmom_num already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%psmom_num(ss%npsmom))
     ss%psmom_num = " "
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%P_eq).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%P_eq)
  ENDIF
  IF(ALLOCATED(ss%P_eq)) then
     jsize = SIZE(ss%P_eq)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: P_eq already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%P_eq(ss%nrho_eq))
     ss%P_eq = 0.0_rspec
  ENDIF

  isize = (ss%nspec_all+1)
  IF(ss%nspec_all.LE.0) isize = 0
  IF(ALLOCATED(ss%qatom_ALL).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qatom_ALL)
  ENDIF
  IF(ALLOCATED(ss%qatom_ALL)) then
     jsize = SIZE(ss%qatom_ALL)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qatom_ALL already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qatom_ALL(0:ss%nspec_all))
     ss%qatom_ALL = 0.0_rspec
  ENDIF

  isize = (ss%nspec_alla+1)
  IF(ss%nspec_alla.LE.0) isize = 0
  IF(ALLOCATED(ss%qatom_ALLA).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qatom_ALLA)
  ENDIF
  IF(ALLOCATED(ss%qatom_ALLA)) then
     jsize = SIZE(ss%qatom_ALLA)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qatom_ALLA already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qatom_ALLA(0:ss%nspec_alla))
     ss%qatom_ALLA = 0.0_rspec
  ENDIF

  isize = (ss%nspec_rfmin)
  IF(ALLOCATED(ss%qatom_RFMIN).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qatom_RFMIN)
  ENDIF
  IF(ALLOCATED(ss%qatom_RFMIN)) then
     jsize = SIZE(ss%qatom_RFMIN)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qatom_RFMIN already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qatom_RFMIN(ss%nspec_rfmin))
     ss%qatom_RFMIN = 0.0_rspec
  ENDIF

  isize = (ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%qatom_S).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qatom_S)
  ENDIF
  IF(ALLOCATED(ss%qatom_S)) then
     jsize = SIZE(ss%qatom_S)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qatom_S already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qatom_S(0:ss%nspec_th))
     ss%qatom_S = 0.0_rspec
  ENDIF

  isize = (ss%nspec_tha+1)
  IF(ss%nspec_tha.LE.0) isize = 0
  IF(ALLOCATED(ss%qatom_SA).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qatom_SA)
  ENDIF
  IF(ALLOCATED(ss%qatom_SA)) then
     jsize = SIZE(ss%qatom_SA)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qatom_SA already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qatom_SA(0:ss%nspec_tha))
     ss%qatom_SA = 0.0_rspec
  ENDIF

  isize = (ss%nspec_fusion)
  IF(ALLOCATED(ss%qatom_SFUS).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qatom_SFUS)
  ENDIF
  IF(ALLOCATED(ss%qatom_SFUS)) then
     jsize = SIZE(ss%qatom_SFUS)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qatom_SFUS already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qatom_SFUS(ss%nspec_fusion))
     ss%qatom_SFUS = 0.0_rspec
  ENDIF

  isize = (ss%nspec_gas)
  IF(ALLOCATED(ss%qatom_SGAS).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qatom_SGAS)
  ENDIF
  IF(ALLOCATED(ss%qatom_SGAS)) then
     jsize = SIZE(ss%qatom_SGAS)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qatom_SGAS already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qatom_SGAS(ss%nspec_gas))
     ss%qatom_SGAS = 0.0_rspec
  ENDIF

  isize = (ss%nspec_imp0)
  IF(ALLOCATED(ss%qatom_SIMP0).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qatom_SIMP0)
  ENDIF
  IF(ALLOCATED(ss%qatom_SIMP0)) then
     jsize = SIZE(ss%qatom_SIMP0)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qatom_SIMP0 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qatom_SIMP0(ss%nspec_imp0))
     ss%qatom_SIMP0 = 0.0_rspec
  ENDIF

  isize = (ss%nspec_impi)
  IF(ALLOCATED(ss%qatom_SIMPI).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qatom_SIMPI)
  ENDIF
  IF(ALLOCATED(ss%qatom_SIMPI)) then
     jsize = SIZE(ss%qatom_SIMPI)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qatom_SIMPI already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qatom_SIMPI(ss%nspec_impi))
     ss%qatom_SIMPI = 0.0_rspec
  ENDIF

  isize = (ss%nspec_beam)
  IF(ALLOCATED(ss%qatom_SNBI).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qatom_SNBI)
  ENDIF
  IF(ALLOCATED(ss%qatom_SNBI)) then
     jsize = SIZE(ss%qatom_SNBI)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qatom_SNBI already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qatom_SNBI(ss%nspec_beam))
     ss%qatom_SNBI = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)*(ss%nspec_beam)
  IF(ALLOCATED(ss%qbeami).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qbeami)
  ENDIF
  IF(ALLOCATED(ss%qbeami)) then
     jsize = SIZE(ss%qbeami)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qbeami already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qbeami(ss%nrho_nbi-1,ss%nspec_beam))
     ss%qbeami = 0.0_rspec
     ALLOCATE(ss%id_qbeami(ss%nspec_beam))
     ss%id_qbeami = 0
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%qcomp_e).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qcomp_e)
  ENDIF
  IF(ALLOCATED(ss%qcomp_e)) then
     jsize = SIZE(ss%qcomp_e)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qcomp_e already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qcomp_e(ss%nrho-1))
     ss%qcomp_e = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%qcomp_i).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qcomp_i)
  ENDIF
  IF(ALLOCATED(ss%qcomp_i)) then
     jsize = SIZE(ss%qcomp_i)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qcomp_i already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qcomp_i(ss%nrho-1))
     ss%qcomp_i = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%qie).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qie)
  ENDIF
  IF(ALLOCATED(ss%qie)) then
     jsize = SIZE(ss%qie)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qie already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qie(ss%nrho-1))
     ss%qie = 0.0_rspec
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%ngsc0)
  IF(ALLOCATED(ss%qioniz).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qioniz)
  ENDIF
  IF(ALLOCATED(ss%qioniz)) then
     jsize = SIZE(ss%qioniz)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qioniz already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qioniz(ss%nrho_gas-1,ss%ngsc0))
     ss%qioniz = 0.0_rspec
     ALLOCATE(ss%id_qioniz(ss%ngsc0))
     ss%id_qioniz = 0
  ENDIF

  isize = (ss%nspec_alla+1)
  IF(ss%nspec_alla.LE.0) isize = 0
  IF(ALLOCATED(ss%ql_operator).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ql_operator)
  ENDIF
  IF(ALLOCATED(ss%ql_operator)) then
     jsize = SIZE(ss%ql_operator)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ql_operator already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ql_operator(0:ss%nspec_alla))
     ss%ql_operator = " "
  ENDIF

  isize = (ss%nrho_gas-1)*(ss%ngsc0)
  IF(ALLOCATED(ss%qqcx).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qqcx)
  ENDIF
  IF(ALLOCATED(ss%qqcx)) then
     jsize = SIZE(ss%qqcx)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qqcx already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qqcx(ss%nrho_gas-1,ss%ngsc0))
     ss%qqcx = 0.0_rspec
     ALLOCATE(ss%id_qqcx(ss%ngsc0))
     ss%id_qqcx = 0
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%qrot_conv).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qrot_conv)
  ENDIF
  IF(ALLOCATED(ss%qrot_conv)) then
     jsize = SIZE(ss%qrot_conv)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qrot_conv already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qrot_conv(ss%nrho-1))
     ss%qrot_conv = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%qrot_diff).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%qrot_diff)
  ENDIF
  IF(ALLOCATED(ss%qrot_diff)) then
     jsize = SIZE(ss%qrot_diff)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: qrot_diff already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%qrot_diff(ss%nrho-1))
     ss%qrot_diff = 0.0_rspec
  ENDIF

  isize = (ss%nspec_all+1)
  IF(ss%nspec_all.LE.0) isize = 0
  IF(ALLOCATED(ss%q_ALL).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%q_ALL)
  ENDIF
  IF(ALLOCATED(ss%q_ALL)) then
     jsize = SIZE(ss%q_ALL)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: q_ALL already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%q_ALL(0:ss%nspec_all))
     ss%q_ALL = 0.0_rspec
  ENDIF

  isize = (ss%nspec_alla+1)
  IF(ss%nspec_alla.LE.0) isize = 0
  IF(ALLOCATED(ss%q_ALLA).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%q_ALLA)
  ENDIF
  IF(ALLOCATED(ss%q_ALLA)) then
     jsize = SIZE(ss%q_ALLA)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: q_ALLA already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%q_ALLA(0:ss%nspec_alla))
     ss%q_ALLA = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%q_eq).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%q_eq)
  ENDIF
  IF(ALLOCATED(ss%q_eq)) then
     jsize = SIZE(ss%q_eq)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: q_eq already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%q_eq(ss%nrho_eq))
     ss%q_eq = 0.0_rspec
  ENDIF

  isize = (ss%nspec_rfmin)
  IF(ALLOCATED(ss%q_RFMIN).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%q_RFMIN)
  ENDIF
  IF(ALLOCATED(ss%q_RFMIN)) then
     jsize = SIZE(ss%q_RFMIN)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: q_RFMIN already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%q_RFMIN(ss%nspec_rfmin))
     ss%q_RFMIN = 0.0_rspec
  ENDIF

  isize = (ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%q_S).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%q_S)
  ENDIF
  IF(ALLOCATED(ss%q_S)) then
     jsize = SIZE(ss%q_S)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: q_S already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%q_S(0:ss%nspec_th))
     ss%q_S = 0.0_rspec
  ENDIF

  isize = (ss%nspec_tha+1)
  IF(ss%nspec_tha.LE.0) isize = 0
  IF(ALLOCATED(ss%q_SA).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%q_SA)
  ENDIF
  IF(ALLOCATED(ss%q_SA)) then
     jsize = SIZE(ss%q_SA)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: q_SA already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%q_SA(0:ss%nspec_tha))
     ss%q_SA = 0.0_rspec
  ENDIF

  isize = (ss%nspec_fusion)
  IF(ALLOCATED(ss%q_SFUS).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%q_SFUS)
  ENDIF
  IF(ALLOCATED(ss%q_SFUS)) then
     jsize = SIZE(ss%q_SFUS)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: q_SFUS already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%q_SFUS(ss%nspec_fusion))
     ss%q_SFUS = 0.0_rspec
  ENDIF

  isize = (ss%nspec_gas)
  IF(ALLOCATED(ss%q_SGAS).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%q_SGAS)
  ENDIF
  IF(ALLOCATED(ss%q_SGAS)) then
     jsize = SIZE(ss%q_SGAS)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: q_SGAS already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%q_SGAS(ss%nspec_gas))
     ss%q_SGAS = 0.0_rspec
  ENDIF

  isize = (ss%nspec_imp0)
  IF(ALLOCATED(ss%q_SIMP0).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%q_SIMP0)
  ENDIF
  IF(ALLOCATED(ss%q_SIMP0)) then
     jsize = SIZE(ss%q_SIMP0)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: q_SIMP0 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%q_SIMP0(ss%nspec_imp0))
     ss%q_SIMP0 = 0.0_rspec
  ENDIF

  isize = (ss%nspec_impi)
  IF(ALLOCATED(ss%q_SIMPI).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%q_SIMPI)
  ENDIF
  IF(ALLOCATED(ss%q_SIMPI)) then
     jsize = SIZE(ss%q_SIMPI)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: q_SIMPI already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%q_SIMPI(ss%nspec_impi))
     ss%q_SIMPI = 0.0_rspec
  ENDIF

  isize = (ss%nspec_beam)
  IF(ALLOCATED(ss%q_SNBI).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%q_SNBI)
  ENDIF
  IF(ALLOCATED(ss%q_SNBI)) then
     jsize = SIZE(ss%q_SNBI)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: q_SNBI already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%q_SNBI(ss%nspec_beam))
     ss%q_SNBI = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%R0_momeq).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%R0_momeq)
  ENDIF
  IF(ALLOCATED(ss%R0_momeq)) then
     jsize = SIZE(ss%R0_momeq)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: R0_momeq already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%R0_momeq(ss%nrho_eq))
     ss%R0_momeq = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%rate_sinb0i).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rate_sinb0i)
  ENDIF
  IF(ALLOCATED(ss%rate_sinb0i)) then
     jsize = SIZE(ss%rate_sinb0i)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rate_sinb0i already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rate_sinb0i(ss%nrho_nbi-1,ss%nspec_gas))
     ss%rate_sinb0i = 0.0_rspec
     ALLOCATE(ss%id_rate_sinb0i(ss%nspec_gas))
     ss%id_rate_sinb0i = 0
  ENDIF

  isize = (ss%nrho_nbi-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%rate_sinb0x).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rate_sinb0x)
  ENDIF
  IF(ALLOCATED(ss%rate_sinb0x)) then
     jsize = SIZE(ss%rate_sinb0x)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rate_sinb0x already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rate_sinb0x(ss%nrho_nbi-1,ss%nspec_gas))
     ss%rate_sinb0x = 0.0_rspec
     ALLOCATE(ss%id_rate_sinb0x(ss%nspec_gas))
     ss%id_rate_sinb0x = 0
  ENDIF

  isize = (ss%nrho_nbi-1)*(ss%nspec_gas)*(ss%nspec_beam)
  IF(ALLOCATED(ss%rate_sinb0xs).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rate_sinb0xs)
  ENDIF
  IF(ALLOCATED(ss%rate_sinb0xs)) then
     jsize = SIZE(ss%rate_sinb0xs)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rate_sinb0xs already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rate_sinb0xs(ss%nrho_nbi-1,ss%nspec_gas,ss%nspec_beam))
     ss%rate_sinb0xs = 0.0_rspec
     ALLOCATE(ss%id_rate_sinb0xs(ss%nspec_gas,ss%nspec_beam))
     ss%id_rate_sinb0xs = 0
  ENDIF

  isize = (ss%nrho_fus-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%rate_sinf0i).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rate_sinf0i)
  ENDIF
  IF(ALLOCATED(ss%rate_sinf0i)) then
     jsize = SIZE(ss%rate_sinf0i)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rate_sinf0i already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rate_sinf0i(ss%nrho_fus-1,ss%nspec_gas))
     ss%rate_sinf0i = 0.0_rspec
     ALLOCATE(ss%id_rate_sinf0i(ss%nspec_gas))
     ss%id_rate_sinf0i = 0
  ENDIF

  isize = (ss%nrho_fus-1)*(ss%nspec_gas)
  IF(ALLOCATED(ss%rate_sinf0x).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rate_sinf0x)
  ENDIF
  IF(ALLOCATED(ss%rate_sinf0x)) then
     jsize = SIZE(ss%rate_sinf0x)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rate_sinf0x already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rate_sinf0x(ss%nrho_fus-1,ss%nspec_gas))
     ss%rate_sinf0x = 0.0_rspec
     ALLOCATE(ss%id_rate_sinf0x(ss%nspec_gas))
     ss%id_rate_sinf0x = 0
  ENDIF

  isize = (ss%nrho_fus-1)*(ss%nspec_gas)*(ss%nspec_fusion)
  IF(ALLOCATED(ss%rate_sinf0xs).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rate_sinf0xs)
  ENDIF
  IF(ALLOCATED(ss%rate_sinf0xs)) then
     jsize = SIZE(ss%rate_sinf0xs)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rate_sinf0xs already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rate_sinf0xs(ss%nrho_fus-1,ss%nspec_gas,ss%nspec_fusion))
     ss%rate_sinf0xs = 0.0_rspec
     ALLOCATE(ss%id_rate_sinf0xs(ss%nspec_gas,ss%nspec_fusion))
     ss%id_rate_sinf0xs = 0
  ENDIF

  isize = (ss%max_num_nphi_vac)*(ss%nicrf_src)
  IF(ALLOCATED(ss%Real_ant_coef).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Real_ant_coef)
  ENDIF
  IF(ALLOCATED(ss%Real_ant_coef)) then
     jsize = SIZE(ss%Real_ant_coef)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Real_ant_coef already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Real_ant_coef(ss%max_num_nphi_vac,ss%nicrf_src))
     ss%Real_ant_coef = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%res_sn).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%res_sn)
  ENDIF
  IF(ALLOCATED(ss%res_sn)) then
     jsize = SIZE(ss%res_sn)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: res_sn already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%res_sn(ss%nrho-1,0:ss%nspec_th))
     ss%res_sn = 0.0_rspec
     ALLOCATE(ss%id_res_sn(0:ss%nspec_th))
     ss%id_res_sn = 0
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%res_te).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%res_te)
  ENDIF
  IF(ALLOCATED(ss%res_te)) then
     jsize = SIZE(ss%res_te)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: res_te already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%res_te(ss%nrho-1))
     ss%res_te = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%res_ti).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%res_ti)
  ENDIF
  IF(ALLOCATED(ss%res_ti)) then
     jsize = SIZE(ss%res_ti)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: res_ti already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%res_ti(ss%nrho-1))
     ss%res_ti = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%res_tq).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%res_tq)
  ENDIF
  IF(ALLOCATED(ss%res_tq)) then
     jsize = SIZE(ss%res_tq)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: res_tq already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%res_tq(ss%nrho-1))
     ss%res_tq = 0.0_rspec
  ENDIF

  isize = (ss%nspec_rfmin)
  IF(ALLOCATED(ss%RFMIN_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%RFMIN_name)
  ENDIF
  IF(ALLOCATED(ss%RFMIN_name)) then
     jsize = SIZE(ss%RFMIN_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: RFMIN_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%RFMIN_name(ss%nspec_rfmin))
     ss%RFMIN_name = " "
  ENDIF

  isize = (ss%nspec_rfmin)
  IF(ALLOCATED(ss%rfmin_to_all).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rfmin_to_all)
  ENDIF
  IF(ALLOCATED(ss%rfmin_to_all)) then
     jsize = SIZE(ss%rfmin_to_all)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rfmin_to_all already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rfmin_to_all(ss%nspec_rfmin))
     ss%rfmin_to_all = 0
  ENDIF

  isize = (ss%nspec_rfmin)
  IF(ALLOCATED(ss%rfmin_to_alla).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rfmin_to_alla)
  ENDIF
  IF(ALLOCATED(ss%rfmin_to_alla)) then
     jsize = SIZE(ss%rfmin_to_alla)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rfmin_to_alla already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rfmin_to_alla(ss%nspec_rfmin))
     ss%rfmin_to_alla = 0
  ENDIF

  isize = (ss%nspec_rfmin)
  IF(ALLOCATED(ss%RFMIN_type).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%RFMIN_type)
  ENDIF
  IF(ALLOCATED(ss%RFMIN_type)) then
     jsize = SIZE(ss%RFMIN_type)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: RFMIN_type already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%RFMIN_type(ss%nspec_rfmin))
     ss%RFMIN_type = 0
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%rho).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho)
  ENDIF
  IF(ALLOCATED(ss%rho)) then
     jsize = SIZE(ss%rho)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho(ss%nrho))
     ss%rho = 0.0_rspec
  ENDIF

  isize = (ss%nlhrf_src)
  IF(ALLOCATED(ss%rhopsi0lh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rhopsi0lh)
  ENDIF
  IF(ALLOCATED(ss%rhopsi0lh)) then
     jsize = SIZE(ss%rhopsi0lh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rhopsi0lh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rhopsi0lh(ss%nlhrf_src))
     ss%rhopsi0lh = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anom)
  IF(ALLOCATED(ss%rho_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_anom)
  ENDIF
  IF(ALLOCATED(ss%rho_anom)) then
     jsize = SIZE(ss%rho_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_anom(ss%nrho_anom))
     ss%rho_anom = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anom2)
  IF(ALLOCATED(ss%rho_anom2).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_anom2)
  ENDIF
  IF(ALLOCATED(ss%rho_anom2)) then
     jsize = SIZE(ss%rho_anom2)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_anom2 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_anom2(ss%nrho_anom2))
     ss%rho_anom2 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%rho_anomth).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_anomth)
  ENDIF
  IF(ALLOCATED(ss%rho_anomth)) then
     jsize = SIZE(ss%rho_anomth)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_anomth already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_anomth(ss%nrho_anomth))
     ss%rho_anomth = 0.0_rspec
  ENDIF

  isize = (ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%rho_bdy_ns).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_bdy_ns)
  ENDIF
  IF(ALLOCATED(ss%rho_bdy_ns)) then
     jsize = SIZE(ss%rho_bdy_ns)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_bdy_ns already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_bdy_ns(0:ss%nspec_th))
     ss%rho_bdy_ns = 0.0_rspec
  ENDIF

  isize = (ss%nrho_ecrf)
  IF(ALLOCATED(ss%rho_ecrf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_ecrf)
  ENDIF
  IF(ALLOCATED(ss%rho_ecrf)) then
     jsize = SIZE(ss%rho_ecrf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_ecrf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_ecrf(ss%nrho_ecrf))
     ss%rho_ecrf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%rho_eq).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_eq)
  ENDIF
  IF(ALLOCATED(ss%rho_eq)) then
     jsize = SIZE(ss%rho_eq)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_eq already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_eq(ss%nrho_eq))
     ss%rho_eq = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%rho_eq_geo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_eq_geo)
  ENDIF
  IF(ALLOCATED(ss%rho_eq_geo)) then
     jsize = SIZE(ss%rho_eq_geo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_eq_geo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_eq_geo(ss%nrho_eq_geo))
     ss%rho_eq_geo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_fus)
  IF(ALLOCATED(ss%rho_fus).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_fus)
  ENDIF
  IF(ALLOCATED(ss%rho_fus)) then
     jsize = SIZE(ss%rho_fus)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_fus already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_fus(ss%nrho_fus))
     ss%rho_fus = 0.0_rspec
  ENDIF

  isize = (ss%nrho_gas)
  IF(ALLOCATED(ss%rho_gas).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_gas)
  ENDIF
  IF(ALLOCATED(ss%rho_gas)) then
     jsize = SIZE(ss%rho_gas)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_gas already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_gas(ss%nrho_gas))
     ss%rho_gas = 0.0_rspec
  ENDIF

  isize = (ss%nrho_icrf)
  IF(ALLOCATED(ss%rho_icrf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_icrf)
  ENDIF
  IF(ALLOCATED(ss%rho_icrf)) then
     jsize = SIZE(ss%rho_icrf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_icrf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_icrf(ss%nrho_icrf))
     ss%rho_icrf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_lhrf)
  IF(ALLOCATED(ss%rho_lhrf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_lhrf)
  ENDIF
  IF(ALLOCATED(ss%rho_lhrf)) then
     jsize = SIZE(ss%rho_lhrf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_lhrf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_lhrf(ss%nrho_lhrf))
     ss%rho_lhrf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_lmhd)
  IF(ALLOCATED(ss%rho_lmhd).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_lmhd)
  ENDIF
  IF(ALLOCATED(ss%rho_lmhd)) then
     jsize = SIZE(ss%rho_lmhd)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_lmhd already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_lmhd(ss%nrho_lmhd))
     ss%rho_lmhd = 0.0_rspec
  ENDIF

  isize = (ss%nrho_mist)
  IF(ALLOCATED(ss%rho_mist).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_mist)
  ENDIF
  IF(ALLOCATED(ss%rho_mist)) then
     jsize = SIZE(ss%rho_mist)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_mist already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_mist(ss%nrho_mist))
     ss%rho_mist = 0.0_rspec
  ENDIF

  isize = (ss%nrho_nbi)
  IF(ALLOCATED(ss%rho_nbi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%rho_nbi)
  ENDIF
  IF(ALLOCATED(ss%rho_nbi)) then
     jsize = SIZE(ss%rho_nbi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: rho_nbi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%rho_nbi(ss%nrho_nbi))
     ss%rho_nbi = 0.0_rspec
  ENDIF

END SUBROUTINE plasma_state_empty_alloc_6
