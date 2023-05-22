! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_merge_6(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)


  ! Merge floating point data
  !   ss3 <-- w1*ss1 + (1-w1)*ss2

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components to merge
                                ! use ps_cc_all to merge all components

  REAL(KIND=rspec) :: w1        ! weight on ss1
  TYPE (plasma_state) :: ss1    ! state data to copy *from* with weight (w1)
  TYPE (plasma_state) :: ss2    ! state data to copy *from* with weight (1-w1)
  TYPE (plasma_state) :: ss3    ! state to which data is copied.
  INTEGER, INTENT(IN) :: icheck ! control for checking match of non-floating-
  !  point data: ps_ignore, ps_warn, or ps_error on mismatch.

  INTEGER, INTENT(IN) :: iout
  INTEGER, INTENT(INOUT) :: ierr  ! completion code (not cleared,set on error)

  !-------------------
  INTEGER :: isize
  !-------------------

  !-----------------------------------------------
  ! copy scalars and arrays that are allocated...

  IF((ALLOCATED(ss3%prad_li)).AND.(cclist(ps_cnum_RAD).EQ.1)) THEN
     IF(ALLOCATED(ss1%prad_li).AND.ALLOCATED(ss2%prad_li)) THEN
        CALL ps_conformal("prad_li",ss2%prad_li,ss3%prad_li,iout,ierr)
        CALL ps_conformal("prad_li",ss1%prad_li,ss3%prad_li,iout,ierr)
        if(ierr.eq.0) ss3%prad_li = w1*ss1%prad_li + (1.0_rspec-w1)*ss2%prad_li
     ELSE IF(ALLOCATED(ss1%prad_li)) THEN
        CALL ps_conformal("prad_li",ss1%prad_li,ss3%prad_li,iout,ierr)
        ss3%prad_li = ss1%prad_li
     ELSE IF(ALLOCATED(ss2%prad_li)) THEN
        CALL ps_conformal("prad_li",ss2%prad_li,ss3%prad_li,iout,ierr)
        ss3%prad_li = ss2%prad_li
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%psc_halo)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%psc_halo).AND.ALLOCATED(ss2%psc_halo)) THEN
        CALL ps_conformal("psc_halo",ss2%psc_halo,ss3%psc_halo,iout,ierr)
        CALL ps_conformal("psc_halo",ss1%psc_halo,ss3%psc_halo,iout,ierr)
        if(ierr.eq.0) ss3%psc_halo = w1*ss1%psc_halo + (1.0_rspec-w1)*ss2%psc_halo
     ELSE IF(ALLOCATED(ss1%psc_halo)) THEN
        CALL ps_conformal("psc_halo",ss1%psc_halo,ss3%psc_halo,iout,ierr)
        ss3%psc_halo = ss1%psc_halo
     ELSE IF(ALLOCATED(ss2%psc_halo)) THEN
        CALL ps_conformal("psc_halo",ss2%psc_halo,ss3%psc_halo,iout,ierr)
        ss3%psc_halo = ss2%psc_halo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%psc_reco)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%psc_reco).AND.ALLOCATED(ss2%psc_reco)) THEN
        CALL ps_conformal("psc_reco",ss2%psc_reco,ss3%psc_reco,iout,ierr)
        CALL ps_conformal("psc_reco",ss1%psc_reco,ss3%psc_reco,iout,ierr)
        if(ierr.eq.0) ss3%psc_reco = w1*ss1%psc_reco + (1.0_rspec-w1)*ss2%psc_reco
     ELSE IF(ALLOCATED(ss1%psc_reco)) THEN
        CALL ps_conformal("psc_reco",ss1%psc_reco,ss3%psc_reco,iout,ierr)
        ss3%psc_reco = ss1%psc_reco
     ELSE IF(ALLOCATED(ss2%psc_reco)) THEN
        CALL ps_conformal("psc_reco",ss2%psc_reco,ss3%psc_reco,iout,ierr)
        ss3%psc_reco = ss2%psc_reco
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%psipol)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%psipol).AND.ALLOCATED(ss2%psipol)) THEN
        CALL ps_conformal("psipol",ss2%psipol,ss3%psipol,iout,ierr)
        CALL ps_conformal("psipol",ss1%psipol,ss3%psipol,iout,ierr)
        if(ierr.eq.0) ss3%psipol = w1*ss1%psipol + (1.0_rspec-w1)*ss2%psipol
     ELSE IF(ALLOCATED(ss1%psipol)) THEN
        CALL ps_conformal("psipol",ss1%psipol,ss3%psipol,iout,ierr)
        ss3%psipol = ss1%psipol
     ELSE IF(ALLOCATED(ss2%psipol)) THEN
        CALL ps_conformal("psipol",ss2%psipol,ss3%psipol,iout,ierr)
        ss3%psipol = ss2%psipol
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%PsiRZ)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%PsiRZ).AND.ALLOCATED(ss2%PsiRZ)) THEN
        CALL ps_conformal("PsiRZ",ss2%PsiRZ,ss3%PsiRZ,iout,ierr)
        CALL ps_conformal("PsiRZ",ss1%PsiRZ,ss3%PsiRZ,iout,ierr)
        if(ierr.eq.0) ss3%PsiRZ = w1*ss1%PsiRZ + (1.0_rspec-w1)*ss2%PsiRZ
     ELSE IF(ALLOCATED(ss1%PsiRZ)) THEN
        CALL ps_conformal("PsiRZ",ss1%PsiRZ,ss3%PsiRZ,iout,ierr)
        ss3%PsiRZ = ss1%PsiRZ
     ELSE IF(ALLOCATED(ss2%PsiRZ)) THEN
        CALL ps_conformal("PsiRZ",ss2%PsiRZ,ss3%PsiRZ,iout,ierr)
        ss3%PsiRZ = ss2%PsiRZ
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     ss3%Psi_to_machine_axis = w1*ss1%Psi_to_machine_axis + (1.0_rspec-w1)*ss2%Psi_to_machine_axis
  ENDIF

  IF((ALLOCATED(ss3%psmom_errck)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%psmom_errck).AND.ALLOCATED(ss2%psmom_errck)) THEN
        CALL ps_conformal("psmom_errck",ss2%psmom_errck,ss3%psmom_errck,iout,ierr)
        CALL ps_conformal("psmom_errck",ss1%psmom_errck,ss3%psmom_errck,iout,ierr)
        if(ierr.eq.0) ss3%psmom_errck = w1*ss1%psmom_errck + (1.0_rspec-w1)*ss2%psmom_errck
     ELSE IF(ALLOCATED(ss1%psmom_errck)) THEN
        CALL ps_conformal("psmom_errck",ss1%psmom_errck,ss3%psmom_errck,iout,ierr)
        ss3%psmom_errck = ss1%psmom_errck
     ELSE IF(ALLOCATED(ss2%psmom_errck)) THEN
        CALL ps_conformal("psmom_errck",ss2%psmom_errck,ss3%psmom_errck,iout,ierr)
        ss3%psmom_errck = ss2%psmom_errck
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%psmom_nc)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%psmom_nc).AND.ALLOCATED(ss2%psmom_nc)) THEN
        CALL ps_conformal("psmom_nc",ss2%psmom_nc,ss3%psmom_nc,iout,ierr)
        CALL ps_conformal("psmom_nc",ss1%psmom_nc,ss3%psmom_nc,iout,ierr)
        if(ierr.eq.0) ss3%psmom_nc = w1*ss1%psmom_nc + (1.0_rspec-w1)*ss2%psmom_nc
     ELSE IF(ALLOCATED(ss1%psmom_nc)) THEN
        CALL ps_conformal("psmom_nc",ss1%psmom_nc,ss3%psmom_nc,iout,ierr)
        ss3%psmom_nc = ss1%psmom_nc
     ELSE IF(ALLOCATED(ss2%psmom_nc)) THEN
        CALL ps_conformal("psmom_nc",ss2%psmom_nc,ss3%psmom_nc,iout,ierr)
        ss3%psmom_nc = ss2%psmom_nc
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%psmom_num)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%psmom_num).AND.ALLOCATED(ss2%psmom_num)) THEN
        CALL ps_diff("psmom_num",ss1%psmom_num,ss2%psmom_num,icheck,iout,ierr)
        CALL ps_conformal("psmom_num",ss1%psmom_num,ss3%psmom_num,iout,ierr)
        if(ierr.eq.0) ss3%psmom_num = ss1%psmom_num
     ELSE IF(ALLOCATED(ss1%psmom_num)) THEN
        CALL ps_conformal("psmom_num",ss1%psmom_num,ss3%psmom_num,iout,ierr)
        ss3%psmom_num = ss1%psmom_num
     ELSE IF(ALLOCATED(ss2%psmom_num)) THEN
        CALL ps_conformal("psmom_num",ss2%psmom_num,ss3%psmom_num,iout,ierr)
        ss3%psmom_num = ss2%psmom_num
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%P_eq)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%P_eq).AND.ALLOCATED(ss2%P_eq)) THEN
        CALL ps_conformal("P_eq",ss2%P_eq,ss3%P_eq,iout,ierr)
        CALL ps_conformal("P_eq",ss1%P_eq,ss3%P_eq,iout,ierr)
        if(ierr.eq.0) ss3%P_eq = w1*ss1%P_eq + (1.0_rspec-w1)*ss2%P_eq
     ELSE IF(ALLOCATED(ss1%P_eq)) THEN
        CALL ps_conformal("P_eq",ss1%P_eq,ss3%P_eq,iout,ierr)
        ss3%P_eq = ss1%P_eq
     ELSE IF(ALLOCATED(ss2%P_eq)) THEN
        CALL ps_conformal("P_eq",ss2%P_eq,ss3%P_eq,iout,ierr)
        ss3%P_eq = ss2%P_eq
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%qatom_ALL)) THEN
     IF(ALLOCATED(ss1%qatom_ALL).AND.ALLOCATED(ss2%qatom_ALL)) THEN
        CALL ps_diff("qatom_ALL",ss1%qatom_ALL,ss2%qatom_ALL,ps_error,iout,ierr)
        CALL ps_conformal("qatom_ALL",ss1%qatom_ALL,ss3%qatom_ALL,iout,ierr)
        if(ierr.eq.0) ss3%qatom_ALL = ss1%qatom_ALL
     ELSE IF(ALLOCATED(ss1%qatom_ALL)) THEN
        CALL ps_conformal("qatom_ALL",ss1%qatom_ALL,ss3%qatom_ALL,iout,ierr)
        ss3%qatom_ALL = ss1%qatom_ALL
     ELSE IF(ALLOCATED(ss2%qatom_ALL)) THEN
        CALL ps_conformal("qatom_ALL",ss2%qatom_ALL,ss3%qatom_ALL,iout,ierr)
        ss3%qatom_ALL = ss2%qatom_ALL
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%qatom_ALLA)) THEN
     IF(ALLOCATED(ss1%qatom_ALLA).AND.ALLOCATED(ss2%qatom_ALLA)) THEN
        CALL ps_diff("qatom_ALLA",ss1%qatom_ALLA,ss2%qatom_ALLA,ps_error,iout,ierr)
        CALL ps_conformal("qatom_ALLA",ss1%qatom_ALLA,ss3%qatom_ALLA,iout,ierr)
        if(ierr.eq.0) ss3%qatom_ALLA = ss1%qatom_ALLA
     ELSE IF(ALLOCATED(ss1%qatom_ALLA)) THEN
        CALL ps_conformal("qatom_ALLA",ss1%qatom_ALLA,ss3%qatom_ALLA,iout,ierr)
        ss3%qatom_ALLA = ss1%qatom_ALLA
     ELSE IF(ALLOCATED(ss2%qatom_ALLA)) THEN
        CALL ps_conformal("qatom_ALLA",ss2%qatom_ALLA,ss3%qatom_ALLA,iout,ierr)
        ss3%qatom_ALLA = ss2%qatom_ALLA
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%qatom_RFMIN)) THEN
     IF(ALLOCATED(ss1%qatom_RFMIN).AND.ALLOCATED(ss2%qatom_RFMIN)) THEN
        CALL ps_diff("qatom_RFMIN",ss1%qatom_RFMIN,ss2%qatom_RFMIN,ps_error,iout,ierr)
        CALL ps_conformal("qatom_RFMIN",ss1%qatom_RFMIN,ss3%qatom_RFMIN,iout,ierr)
        if(ierr.eq.0) ss3%qatom_RFMIN = ss1%qatom_RFMIN
     ELSE IF(ALLOCATED(ss1%qatom_RFMIN)) THEN
        CALL ps_conformal("qatom_RFMIN",ss1%qatom_RFMIN,ss3%qatom_RFMIN,iout,ierr)
        ss3%qatom_RFMIN = ss1%qatom_RFMIN
     ELSE IF(ALLOCATED(ss2%qatom_RFMIN)) THEN
        CALL ps_conformal("qatom_RFMIN",ss2%qatom_RFMIN,ss3%qatom_RFMIN,iout,ierr)
        ss3%qatom_RFMIN = ss2%qatom_RFMIN
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%qatom_S)) THEN
     IF(ALLOCATED(ss1%qatom_S).AND.ALLOCATED(ss2%qatom_S)) THEN
        CALL ps_diff("qatom_S",ss1%qatom_S,ss2%qatom_S,ps_error,iout,ierr)
        CALL ps_conformal("qatom_S",ss1%qatom_S,ss3%qatom_S,iout,ierr)
        if(ierr.eq.0) ss3%qatom_S = ss1%qatom_S
     ELSE IF(ALLOCATED(ss1%qatom_S)) THEN
        CALL ps_conformal("qatom_S",ss1%qatom_S,ss3%qatom_S,iout,ierr)
        ss3%qatom_S = ss1%qatom_S
     ELSE IF(ALLOCATED(ss2%qatom_S)) THEN
        CALL ps_conformal("qatom_S",ss2%qatom_S,ss3%qatom_S,iout,ierr)
        ss3%qatom_S = ss2%qatom_S
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%qatom_SA)) THEN
     IF(ALLOCATED(ss1%qatom_SA).AND.ALLOCATED(ss2%qatom_SA)) THEN
        CALL ps_diff("qatom_SA",ss1%qatom_SA,ss2%qatom_SA,ps_error,iout,ierr)
        CALL ps_conformal("qatom_SA",ss1%qatom_SA,ss3%qatom_SA,iout,ierr)
        if(ierr.eq.0) ss3%qatom_SA = ss1%qatom_SA
     ELSE IF(ALLOCATED(ss1%qatom_SA)) THEN
        CALL ps_conformal("qatom_SA",ss1%qatom_SA,ss3%qatom_SA,iout,ierr)
        ss3%qatom_SA = ss1%qatom_SA
     ELSE IF(ALLOCATED(ss2%qatom_SA)) THEN
        CALL ps_conformal("qatom_SA",ss2%qatom_SA,ss3%qatom_SA,iout,ierr)
        ss3%qatom_SA = ss2%qatom_SA
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%qatom_SFUS)) THEN
     IF(ALLOCATED(ss1%qatom_SFUS).AND.ALLOCATED(ss2%qatom_SFUS)) THEN
        CALL ps_diff("qatom_SFUS",ss1%qatom_SFUS,ss2%qatom_SFUS,ps_error,iout,ierr)
        CALL ps_conformal("qatom_SFUS",ss1%qatom_SFUS,ss3%qatom_SFUS,iout,ierr)
        if(ierr.eq.0) ss3%qatom_SFUS = ss1%qatom_SFUS
     ELSE IF(ALLOCATED(ss1%qatom_SFUS)) THEN
        CALL ps_conformal("qatom_SFUS",ss1%qatom_SFUS,ss3%qatom_SFUS,iout,ierr)
        ss3%qatom_SFUS = ss1%qatom_SFUS
     ELSE IF(ALLOCATED(ss2%qatom_SFUS)) THEN
        CALL ps_conformal("qatom_SFUS",ss2%qatom_SFUS,ss3%qatom_SFUS,iout,ierr)
        ss3%qatom_SFUS = ss2%qatom_SFUS
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%qatom_SGAS)) THEN
     IF(ALLOCATED(ss1%qatom_SGAS).AND.ALLOCATED(ss2%qatom_SGAS)) THEN
        CALL ps_diff("qatom_SGAS",ss1%qatom_SGAS,ss2%qatom_SGAS,ps_error,iout,ierr)
        CALL ps_conformal("qatom_SGAS",ss1%qatom_SGAS,ss3%qatom_SGAS,iout,ierr)
        if(ierr.eq.0) ss3%qatom_SGAS = ss1%qatom_SGAS
     ELSE IF(ALLOCATED(ss1%qatom_SGAS)) THEN
        CALL ps_conformal("qatom_SGAS",ss1%qatom_SGAS,ss3%qatom_SGAS,iout,ierr)
        ss3%qatom_SGAS = ss1%qatom_SGAS
     ELSE IF(ALLOCATED(ss2%qatom_SGAS)) THEN
        CALL ps_conformal("qatom_SGAS",ss2%qatom_SGAS,ss3%qatom_SGAS,iout,ierr)
        ss3%qatom_SGAS = ss2%qatom_SGAS
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%qatom_SIMP0)) THEN
     IF(ALLOCATED(ss1%qatom_SIMP0).AND.ALLOCATED(ss2%qatom_SIMP0)) THEN
        CALL ps_diff("qatom_SIMP0",ss1%qatom_SIMP0,ss2%qatom_SIMP0,ps_error,iout,ierr)
        CALL ps_conformal("qatom_SIMP0",ss1%qatom_SIMP0,ss3%qatom_SIMP0,iout,ierr)
        if(ierr.eq.0) ss3%qatom_SIMP0 = ss1%qatom_SIMP0
     ELSE IF(ALLOCATED(ss1%qatom_SIMP0)) THEN
        CALL ps_conformal("qatom_SIMP0",ss1%qatom_SIMP0,ss3%qatom_SIMP0,iout,ierr)
        ss3%qatom_SIMP0 = ss1%qatom_SIMP0
     ELSE IF(ALLOCATED(ss2%qatom_SIMP0)) THEN
        CALL ps_conformal("qatom_SIMP0",ss2%qatom_SIMP0,ss3%qatom_SIMP0,iout,ierr)
        ss3%qatom_SIMP0 = ss2%qatom_SIMP0
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%qatom_SIMPI)) THEN
     IF(ALLOCATED(ss1%qatom_SIMPI).AND.ALLOCATED(ss2%qatom_SIMPI)) THEN
        CALL ps_diff("qatom_SIMPI",ss1%qatom_SIMPI,ss2%qatom_SIMPI,ps_error,iout,ierr)
        CALL ps_conformal("qatom_SIMPI",ss1%qatom_SIMPI,ss3%qatom_SIMPI,iout,ierr)
        if(ierr.eq.0) ss3%qatom_SIMPI = ss1%qatom_SIMPI
     ELSE IF(ALLOCATED(ss1%qatom_SIMPI)) THEN
        CALL ps_conformal("qatom_SIMPI",ss1%qatom_SIMPI,ss3%qatom_SIMPI,iout,ierr)
        ss3%qatom_SIMPI = ss1%qatom_SIMPI
     ELSE IF(ALLOCATED(ss2%qatom_SIMPI)) THEN
        CALL ps_conformal("qatom_SIMPI",ss2%qatom_SIMPI,ss3%qatom_SIMPI,iout,ierr)
        ss3%qatom_SIMPI = ss2%qatom_SIMPI
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%qatom_SNBI)) THEN
     IF(ALLOCATED(ss1%qatom_SNBI).AND.ALLOCATED(ss2%qatom_SNBI)) THEN
        CALL ps_diff("qatom_SNBI",ss1%qatom_SNBI,ss2%qatom_SNBI,ps_error,iout,ierr)
        CALL ps_conformal("qatom_SNBI",ss1%qatom_SNBI,ss3%qatom_SNBI,iout,ierr)
        if(ierr.eq.0) ss3%qatom_SNBI = ss1%qatom_SNBI
     ELSE IF(ALLOCATED(ss1%qatom_SNBI)) THEN
        CALL ps_conformal("qatom_SNBI",ss1%qatom_SNBI,ss3%qatom_SNBI,iout,ierr)
        ss3%qatom_SNBI = ss1%qatom_SNBI
     ELSE IF(ALLOCATED(ss2%qatom_SNBI)) THEN
        CALL ps_conformal("qatom_SNBI",ss2%qatom_SNBI,ss3%qatom_SNBI,iout,ierr)
        ss3%qatom_SNBI = ss2%qatom_SNBI
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%qbeami)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%qbeami).AND.ALLOCATED(ss2%qbeami)) THEN
        CALL ps_conformal("qbeami",ss2%qbeami,ss3%qbeami,iout,ierr)
        CALL ps_conformal("qbeami",ss1%qbeami,ss3%qbeami,iout,ierr)
        if(ierr.eq.0) ss3%qbeami = w1*ss1%qbeami + (1.0_rspec-w1)*ss2%qbeami
     ELSE IF(ALLOCATED(ss1%qbeami)) THEN
        CALL ps_conformal("qbeami",ss1%qbeami,ss3%qbeami,iout,ierr)
        ss3%qbeami = ss1%qbeami
     ELSE IF(ALLOCATED(ss2%qbeami)) THEN
        CALL ps_conformal("qbeami",ss2%qbeami,ss3%qbeami,iout,ierr)
        ss3%qbeami = ss2%qbeami
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%qcomp_e)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%qcomp_e).AND.ALLOCATED(ss2%qcomp_e)) THEN
        CALL ps_conformal("qcomp_e",ss2%qcomp_e,ss3%qcomp_e,iout,ierr)
        CALL ps_conformal("qcomp_e",ss1%qcomp_e,ss3%qcomp_e,iout,ierr)
        if(ierr.eq.0) ss3%qcomp_e = w1*ss1%qcomp_e + (1.0_rspec-w1)*ss2%qcomp_e
     ELSE IF(ALLOCATED(ss1%qcomp_e)) THEN
        CALL ps_conformal("qcomp_e",ss1%qcomp_e,ss3%qcomp_e,iout,ierr)
        ss3%qcomp_e = ss1%qcomp_e
     ELSE IF(ALLOCATED(ss2%qcomp_e)) THEN
        CALL ps_conformal("qcomp_e",ss2%qcomp_e,ss3%qcomp_e,iout,ierr)
        ss3%qcomp_e = ss2%qcomp_e
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%qcomp_i)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%qcomp_i).AND.ALLOCATED(ss2%qcomp_i)) THEN
        CALL ps_conformal("qcomp_i",ss2%qcomp_i,ss3%qcomp_i,iout,ierr)
        CALL ps_conformal("qcomp_i",ss1%qcomp_i,ss3%qcomp_i,iout,ierr)
        if(ierr.eq.0) ss3%qcomp_i = w1*ss1%qcomp_i + (1.0_rspec-w1)*ss2%qcomp_i
     ELSE IF(ALLOCATED(ss1%qcomp_i)) THEN
        CALL ps_conformal("qcomp_i",ss1%qcomp_i,ss3%qcomp_i,iout,ierr)
        ss3%qcomp_i = ss1%qcomp_i
     ELSE IF(ALLOCATED(ss2%qcomp_i)) THEN
        CALL ps_conformal("qcomp_i",ss2%qcomp_i,ss3%qcomp_i,iout,ierr)
        ss3%qcomp_i = ss2%qcomp_i
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%qie)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%qie).AND.ALLOCATED(ss2%qie)) THEN
        CALL ps_conformal("qie",ss2%qie,ss3%qie,iout,ierr)
        CALL ps_conformal("qie",ss1%qie,ss3%qie,iout,ierr)
        if(ierr.eq.0) ss3%qie = w1*ss1%qie + (1.0_rspec-w1)*ss2%qie
     ELSE IF(ALLOCATED(ss1%qie)) THEN
        CALL ps_conformal("qie",ss1%qie,ss3%qie,iout,ierr)
        ss3%qie = ss1%qie
     ELSE IF(ALLOCATED(ss2%qie)) THEN
        CALL ps_conformal("qie",ss2%qie,ss3%qie,iout,ierr)
        ss3%qie = ss2%qie
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%qioniz)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%qioniz).AND.ALLOCATED(ss2%qioniz)) THEN
        CALL ps_conformal("qioniz",ss2%qioniz,ss3%qioniz,iout,ierr)
        CALL ps_conformal("qioniz",ss1%qioniz,ss3%qioniz,iout,ierr)
        if(ierr.eq.0) ss3%qioniz = w1*ss1%qioniz + (1.0_rspec-w1)*ss2%qioniz
     ELSE IF(ALLOCATED(ss1%qioniz)) THEN
        CALL ps_conformal("qioniz",ss1%qioniz,ss3%qioniz,iout,ierr)
        ss3%qioniz = ss1%qioniz
     ELSE IF(ALLOCATED(ss2%qioniz)) THEN
        CALL ps_conformal("qioniz",ss2%qioniz,ss3%qioniz,iout,ierr)
        ss3%qioniz = ss2%qioniz
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ql_operator)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%ql_operator).AND.ALLOCATED(ss2%ql_operator)) THEN
        CALL ps_diff("ql_operator",ss1%ql_operator,ss2%ql_operator,icheck,iout,ierr)
        CALL ps_conformal("ql_operator",ss1%ql_operator,ss3%ql_operator,iout,ierr)
        if(ierr.eq.0) ss3%ql_operator = ss1%ql_operator
     ELSE IF(ALLOCATED(ss1%ql_operator)) THEN
        CALL ps_conformal("ql_operator",ss1%ql_operator,ss3%ql_operator,iout,ierr)
        ss3%ql_operator = ss1%ql_operator
     ELSE IF(ALLOCATED(ss2%ql_operator)) THEN
        CALL ps_conformal("ql_operator",ss2%ql_operator,ss3%ql_operator,iout,ierr)
        ss3%ql_operator = ss2%ql_operator
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%qqcx)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%qqcx).AND.ALLOCATED(ss2%qqcx)) THEN
        CALL ps_conformal("qqcx",ss2%qqcx,ss3%qqcx,iout,ierr)
        CALL ps_conformal("qqcx",ss1%qqcx,ss3%qqcx,iout,ierr)
        if(ierr.eq.0) ss3%qqcx = w1*ss1%qqcx + (1.0_rspec-w1)*ss2%qqcx
     ELSE IF(ALLOCATED(ss1%qqcx)) THEN
        CALL ps_conformal("qqcx",ss1%qqcx,ss3%qqcx,iout,ierr)
        ss3%qqcx = ss1%qqcx
     ELSE IF(ALLOCATED(ss2%qqcx)) THEN
        CALL ps_conformal("qqcx",ss2%qqcx,ss3%qqcx,iout,ierr)
        ss3%qqcx = ss2%qqcx
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%qrot_conv)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%qrot_conv).AND.ALLOCATED(ss2%qrot_conv)) THEN
        CALL ps_conformal("qrot_conv",ss2%qrot_conv,ss3%qrot_conv,iout,ierr)
        CALL ps_conformal("qrot_conv",ss1%qrot_conv,ss3%qrot_conv,iout,ierr)
        if(ierr.eq.0) ss3%qrot_conv = w1*ss1%qrot_conv + (1.0_rspec-w1)*ss2%qrot_conv
     ELSE IF(ALLOCATED(ss1%qrot_conv)) THEN
        CALL ps_conformal("qrot_conv",ss1%qrot_conv,ss3%qrot_conv,iout,ierr)
        ss3%qrot_conv = ss1%qrot_conv
     ELSE IF(ALLOCATED(ss2%qrot_conv)) THEN
        CALL ps_conformal("qrot_conv",ss2%qrot_conv,ss3%qrot_conv,iout,ierr)
        ss3%qrot_conv = ss2%qrot_conv
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%qrot_diff)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%qrot_diff).AND.ALLOCATED(ss2%qrot_diff)) THEN
        CALL ps_conformal("qrot_diff",ss2%qrot_diff,ss3%qrot_diff,iout,ierr)
        CALL ps_conformal("qrot_diff",ss1%qrot_diff,ss3%qrot_diff,iout,ierr)
        if(ierr.eq.0) ss3%qrot_diff = w1*ss1%qrot_diff + (1.0_rspec-w1)*ss2%qrot_diff
     ELSE IF(ALLOCATED(ss1%qrot_diff)) THEN
        CALL ps_conformal("qrot_diff",ss1%qrot_diff,ss3%qrot_diff,iout,ierr)
        ss3%qrot_diff = ss1%qrot_diff
     ELSE IF(ALLOCATED(ss2%qrot_diff)) THEN
        CALL ps_conformal("qrot_diff",ss2%qrot_diff,ss3%qrot_diff,iout,ierr)
        ss3%qrot_diff = ss2%qrot_diff
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%q_ALL)) THEN
     IF(ALLOCATED(ss1%q_ALL).AND.ALLOCATED(ss2%q_ALL)) THEN
        CALL ps_diff("q_ALL",ss1%q_ALL,ss2%q_ALL,ps_error,iout,ierr)
        CALL ps_conformal("q_ALL",ss1%q_ALL,ss3%q_ALL,iout,ierr)
        if(ierr.eq.0) ss3%q_ALL = ss1%q_ALL
     ELSE IF(ALLOCATED(ss1%q_ALL)) THEN
        CALL ps_conformal("q_ALL",ss1%q_ALL,ss3%q_ALL,iout,ierr)
        ss3%q_ALL = ss1%q_ALL
     ELSE IF(ALLOCATED(ss2%q_ALL)) THEN
        CALL ps_conformal("q_ALL",ss2%q_ALL,ss3%q_ALL,iout,ierr)
        ss3%q_ALL = ss2%q_ALL
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%q_ALLA)) THEN
     IF(ALLOCATED(ss1%q_ALLA).AND.ALLOCATED(ss2%q_ALLA)) THEN
        CALL ps_diff("q_ALLA",ss1%q_ALLA,ss2%q_ALLA,ps_error,iout,ierr)
        CALL ps_conformal("q_ALLA",ss1%q_ALLA,ss3%q_ALLA,iout,ierr)
        if(ierr.eq.0) ss3%q_ALLA = ss1%q_ALLA
     ELSE IF(ALLOCATED(ss1%q_ALLA)) THEN
        CALL ps_conformal("q_ALLA",ss1%q_ALLA,ss3%q_ALLA,iout,ierr)
        ss3%q_ALLA = ss1%q_ALLA
     ELSE IF(ALLOCATED(ss2%q_ALLA)) THEN
        CALL ps_conformal("q_ALLA",ss2%q_ALLA,ss3%q_ALLA,iout,ierr)
        ss3%q_ALLA = ss2%q_ALLA
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%q_eq)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%q_eq).AND.ALLOCATED(ss2%q_eq)) THEN
        CALL ps_conformal("q_eq",ss2%q_eq,ss3%q_eq,iout,ierr)
        CALL ps_conformal("q_eq",ss1%q_eq,ss3%q_eq,iout,ierr)
        if(ierr.eq.0) ss3%q_eq = w1*ss1%q_eq + (1.0_rspec-w1)*ss2%q_eq
     ELSE IF(ALLOCATED(ss1%q_eq)) THEN
        CALL ps_conformal("q_eq",ss1%q_eq,ss3%q_eq,iout,ierr)
        ss3%q_eq = ss1%q_eq
     ELSE IF(ALLOCATED(ss2%q_eq)) THEN
        CALL ps_conformal("q_eq",ss2%q_eq,ss3%q_eq,iout,ierr)
        ss3%q_eq = ss2%q_eq
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%q_RFMIN)) THEN
     IF(ALLOCATED(ss1%q_RFMIN).AND.ALLOCATED(ss2%q_RFMIN)) THEN
        CALL ps_diff("q_RFMIN",ss1%q_RFMIN,ss2%q_RFMIN,ps_error,iout,ierr)
        CALL ps_conformal("q_RFMIN",ss1%q_RFMIN,ss3%q_RFMIN,iout,ierr)
        if(ierr.eq.0) ss3%q_RFMIN = ss1%q_RFMIN
     ELSE IF(ALLOCATED(ss1%q_RFMIN)) THEN
        CALL ps_conformal("q_RFMIN",ss1%q_RFMIN,ss3%q_RFMIN,iout,ierr)
        ss3%q_RFMIN = ss1%q_RFMIN
     ELSE IF(ALLOCATED(ss2%q_RFMIN)) THEN
        CALL ps_conformal("q_RFMIN",ss2%q_RFMIN,ss3%q_RFMIN,iout,ierr)
        ss3%q_RFMIN = ss2%q_RFMIN
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%q_S)) THEN
     IF(ALLOCATED(ss1%q_S).AND.ALLOCATED(ss2%q_S)) THEN
        CALL ps_diff("q_S",ss1%q_S,ss2%q_S,ps_error,iout,ierr)
        CALL ps_conformal("q_S",ss1%q_S,ss3%q_S,iout,ierr)
        if(ierr.eq.0) ss3%q_S = ss1%q_S
     ELSE IF(ALLOCATED(ss1%q_S)) THEN
        CALL ps_conformal("q_S",ss1%q_S,ss3%q_S,iout,ierr)
        ss3%q_S = ss1%q_S
     ELSE IF(ALLOCATED(ss2%q_S)) THEN
        CALL ps_conformal("q_S",ss2%q_S,ss3%q_S,iout,ierr)
        ss3%q_S = ss2%q_S
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%q_SA)) THEN
     IF(ALLOCATED(ss1%q_SA).AND.ALLOCATED(ss2%q_SA)) THEN
        CALL ps_diff("q_SA",ss1%q_SA,ss2%q_SA,ps_error,iout,ierr)
        CALL ps_conformal("q_SA",ss1%q_SA,ss3%q_SA,iout,ierr)
        if(ierr.eq.0) ss3%q_SA = ss1%q_SA
     ELSE IF(ALLOCATED(ss1%q_SA)) THEN
        CALL ps_conformal("q_SA",ss1%q_SA,ss3%q_SA,iout,ierr)
        ss3%q_SA = ss1%q_SA
     ELSE IF(ALLOCATED(ss2%q_SA)) THEN
        CALL ps_conformal("q_SA",ss2%q_SA,ss3%q_SA,iout,ierr)
        ss3%q_SA = ss2%q_SA
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%q_SFUS)) THEN
     IF(ALLOCATED(ss1%q_SFUS).AND.ALLOCATED(ss2%q_SFUS)) THEN
        CALL ps_diff("q_SFUS",ss1%q_SFUS,ss2%q_SFUS,ps_error,iout,ierr)
        CALL ps_conformal("q_SFUS",ss1%q_SFUS,ss3%q_SFUS,iout,ierr)
        if(ierr.eq.0) ss3%q_SFUS = ss1%q_SFUS
     ELSE IF(ALLOCATED(ss1%q_SFUS)) THEN
        CALL ps_conformal("q_SFUS",ss1%q_SFUS,ss3%q_SFUS,iout,ierr)
        ss3%q_SFUS = ss1%q_SFUS
     ELSE IF(ALLOCATED(ss2%q_SFUS)) THEN
        CALL ps_conformal("q_SFUS",ss2%q_SFUS,ss3%q_SFUS,iout,ierr)
        ss3%q_SFUS = ss2%q_SFUS
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%q_SGAS)) THEN
     IF(ALLOCATED(ss1%q_SGAS).AND.ALLOCATED(ss2%q_SGAS)) THEN
        CALL ps_diff("q_SGAS",ss1%q_SGAS,ss2%q_SGAS,ps_error,iout,ierr)
        CALL ps_conformal("q_SGAS",ss1%q_SGAS,ss3%q_SGAS,iout,ierr)
        if(ierr.eq.0) ss3%q_SGAS = ss1%q_SGAS
     ELSE IF(ALLOCATED(ss1%q_SGAS)) THEN
        CALL ps_conformal("q_SGAS",ss1%q_SGAS,ss3%q_SGAS,iout,ierr)
        ss3%q_SGAS = ss1%q_SGAS
     ELSE IF(ALLOCATED(ss2%q_SGAS)) THEN
        CALL ps_conformal("q_SGAS",ss2%q_SGAS,ss3%q_SGAS,iout,ierr)
        ss3%q_SGAS = ss2%q_SGAS
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%q_SIMP0)) THEN
     IF(ALLOCATED(ss1%q_SIMP0).AND.ALLOCATED(ss2%q_SIMP0)) THEN
        CALL ps_diff("q_SIMP0",ss1%q_SIMP0,ss2%q_SIMP0,ps_error,iout,ierr)
        CALL ps_conformal("q_SIMP0",ss1%q_SIMP0,ss3%q_SIMP0,iout,ierr)
        if(ierr.eq.0) ss3%q_SIMP0 = ss1%q_SIMP0
     ELSE IF(ALLOCATED(ss1%q_SIMP0)) THEN
        CALL ps_conformal("q_SIMP0",ss1%q_SIMP0,ss3%q_SIMP0,iout,ierr)
        ss3%q_SIMP0 = ss1%q_SIMP0
     ELSE IF(ALLOCATED(ss2%q_SIMP0)) THEN
        CALL ps_conformal("q_SIMP0",ss2%q_SIMP0,ss3%q_SIMP0,iout,ierr)
        ss3%q_SIMP0 = ss2%q_SIMP0
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%q_SIMPI)) THEN
     IF(ALLOCATED(ss1%q_SIMPI).AND.ALLOCATED(ss2%q_SIMPI)) THEN
        CALL ps_diff("q_SIMPI",ss1%q_SIMPI,ss2%q_SIMPI,ps_error,iout,ierr)
        CALL ps_conformal("q_SIMPI",ss1%q_SIMPI,ss3%q_SIMPI,iout,ierr)
        if(ierr.eq.0) ss3%q_SIMPI = ss1%q_SIMPI
     ELSE IF(ALLOCATED(ss1%q_SIMPI)) THEN
        CALL ps_conformal("q_SIMPI",ss1%q_SIMPI,ss3%q_SIMPI,iout,ierr)
        ss3%q_SIMPI = ss1%q_SIMPI
     ELSE IF(ALLOCATED(ss2%q_SIMPI)) THEN
        CALL ps_conformal("q_SIMPI",ss2%q_SIMPI,ss3%q_SIMPI,iout,ierr)
        ss3%q_SIMPI = ss2%q_SIMPI
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%q_SNBI)) THEN
     IF(ALLOCATED(ss1%q_SNBI).AND.ALLOCATED(ss2%q_SNBI)) THEN
        CALL ps_diff("q_SNBI",ss1%q_SNBI,ss2%q_SNBI,ps_error,iout,ierr)
        CALL ps_conformal("q_SNBI",ss1%q_SNBI,ss3%q_SNBI,iout,ierr)
        if(ierr.eq.0) ss3%q_SNBI = ss1%q_SNBI
     ELSE IF(ALLOCATED(ss1%q_SNBI)) THEN
        CALL ps_conformal("q_SNBI",ss1%q_SNBI,ss3%q_SNBI,iout,ierr)
        ss3%q_SNBI = ss1%q_SNBI
     ELSE IF(ALLOCATED(ss2%q_SNBI)) THEN
        CALL ps_conformal("q_SNBI",ss2%q_SNBI,ss3%q_SNBI,iout,ierr)
        ss3%q_SNBI = ss2%q_SNBI
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%R0_momeq)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%R0_momeq).AND.ALLOCATED(ss2%R0_momeq)) THEN
        CALL ps_conformal("R0_momeq",ss2%R0_momeq,ss3%R0_momeq,iout,ierr)
        CALL ps_conformal("R0_momeq",ss1%R0_momeq,ss3%R0_momeq,iout,ierr)
        if(ierr.eq.0) ss3%R0_momeq = w1*ss1%R0_momeq + (1.0_rspec-w1)*ss2%R0_momeq
     ELSE IF(ALLOCATED(ss1%R0_momeq)) THEN
        CALL ps_conformal("R0_momeq",ss1%R0_momeq,ss3%R0_momeq,iout,ierr)
        ss3%R0_momeq = ss1%R0_momeq
     ELSE IF(ALLOCATED(ss2%R0_momeq)) THEN
        CALL ps_conformal("R0_momeq",ss2%R0_momeq,ss3%R0_momeq,iout,ierr)
        ss3%R0_momeq = ss2%R0_momeq
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_RAD).EQ.1) THEN

     CALL ps_diff("RAD_Code_Info",ss1%RAD_Code_Info,ss2%RAD_Code_Info,icheck,iout,ierr)
     ss3%RAD_Code_Info = ss1%RAD_Code_Info
  ENDIF

  IF(cclist(ps_cnum_RAD).EQ.1) THEN

     CALL ps_diff("RAD_Data_Info",ss1%RAD_Data_Info,ss2%RAD_Data_Info,icheck,iout,ierr)
     ss3%RAD_Data_Info = ss1%RAD_Data_Info
  ENDIF

  IF((ALLOCATED(ss3%rate_sinb0i)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%rate_sinb0i).AND.ALLOCATED(ss2%rate_sinb0i)) THEN
        CALL ps_conformal("rate_sinb0i",ss2%rate_sinb0i,ss3%rate_sinb0i,iout,ierr)
        CALL ps_conformal("rate_sinb0i",ss1%rate_sinb0i,ss3%rate_sinb0i,iout,ierr)
        if(ierr.eq.0) ss3%rate_sinb0i = w1*ss1%rate_sinb0i + (1.0_rspec-w1)*ss2%rate_sinb0i
     ELSE IF(ALLOCATED(ss1%rate_sinb0i)) THEN
        CALL ps_conformal("rate_sinb0i",ss1%rate_sinb0i,ss3%rate_sinb0i,iout,ierr)
        ss3%rate_sinb0i = ss1%rate_sinb0i
     ELSE IF(ALLOCATED(ss2%rate_sinb0i)) THEN
        CALL ps_conformal("rate_sinb0i",ss2%rate_sinb0i,ss3%rate_sinb0i,iout,ierr)
        ss3%rate_sinb0i = ss2%rate_sinb0i
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rate_sinb0x)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%rate_sinb0x).AND.ALLOCATED(ss2%rate_sinb0x)) THEN
        CALL ps_conformal("rate_sinb0x",ss2%rate_sinb0x,ss3%rate_sinb0x,iout,ierr)
        CALL ps_conformal("rate_sinb0x",ss1%rate_sinb0x,ss3%rate_sinb0x,iout,ierr)
        if(ierr.eq.0) ss3%rate_sinb0x = w1*ss1%rate_sinb0x + (1.0_rspec-w1)*ss2%rate_sinb0x
     ELSE IF(ALLOCATED(ss1%rate_sinb0x)) THEN
        CALL ps_conformal("rate_sinb0x",ss1%rate_sinb0x,ss3%rate_sinb0x,iout,ierr)
        ss3%rate_sinb0x = ss1%rate_sinb0x
     ELSE IF(ALLOCATED(ss2%rate_sinb0x)) THEN
        CALL ps_conformal("rate_sinb0x",ss2%rate_sinb0x,ss3%rate_sinb0x,iout,ierr)
        ss3%rate_sinb0x = ss2%rate_sinb0x
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rate_sinb0xs)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%rate_sinb0xs).AND.ALLOCATED(ss2%rate_sinb0xs)) THEN
        CALL ps_conformal("rate_sinb0xs",ss2%rate_sinb0xs,ss3%rate_sinb0xs,iout,ierr)
        CALL ps_conformal("rate_sinb0xs",ss1%rate_sinb0xs,ss3%rate_sinb0xs,iout,ierr)
        if(ierr.eq.0) ss3%rate_sinb0xs = w1*ss1%rate_sinb0xs + (1.0_rspec-w1)*ss2%rate_sinb0xs
     ELSE IF(ALLOCATED(ss1%rate_sinb0xs)) THEN
        CALL ps_conformal("rate_sinb0xs",ss1%rate_sinb0xs,ss3%rate_sinb0xs,iout,ierr)
        ss3%rate_sinb0xs = ss1%rate_sinb0xs
     ELSE IF(ALLOCATED(ss2%rate_sinb0xs)) THEN
        CALL ps_conformal("rate_sinb0xs",ss2%rate_sinb0xs,ss3%rate_sinb0xs,iout,ierr)
        ss3%rate_sinb0xs = ss2%rate_sinb0xs
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rate_sinf0i)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%rate_sinf0i).AND.ALLOCATED(ss2%rate_sinf0i)) THEN
        CALL ps_conformal("rate_sinf0i",ss2%rate_sinf0i,ss3%rate_sinf0i,iout,ierr)
        CALL ps_conformal("rate_sinf0i",ss1%rate_sinf0i,ss3%rate_sinf0i,iout,ierr)
        if(ierr.eq.0) ss3%rate_sinf0i = w1*ss1%rate_sinf0i + (1.0_rspec-w1)*ss2%rate_sinf0i
     ELSE IF(ALLOCATED(ss1%rate_sinf0i)) THEN
        CALL ps_conformal("rate_sinf0i",ss1%rate_sinf0i,ss3%rate_sinf0i,iout,ierr)
        ss3%rate_sinf0i = ss1%rate_sinf0i
     ELSE IF(ALLOCATED(ss2%rate_sinf0i)) THEN
        CALL ps_conformal("rate_sinf0i",ss2%rate_sinf0i,ss3%rate_sinf0i,iout,ierr)
        ss3%rate_sinf0i = ss2%rate_sinf0i
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rate_sinf0x)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%rate_sinf0x).AND.ALLOCATED(ss2%rate_sinf0x)) THEN
        CALL ps_conformal("rate_sinf0x",ss2%rate_sinf0x,ss3%rate_sinf0x,iout,ierr)
        CALL ps_conformal("rate_sinf0x",ss1%rate_sinf0x,ss3%rate_sinf0x,iout,ierr)
        if(ierr.eq.0) ss3%rate_sinf0x = w1*ss1%rate_sinf0x + (1.0_rspec-w1)*ss2%rate_sinf0x
     ELSE IF(ALLOCATED(ss1%rate_sinf0x)) THEN
        CALL ps_conformal("rate_sinf0x",ss1%rate_sinf0x,ss3%rate_sinf0x,iout,ierr)
        ss3%rate_sinf0x = ss1%rate_sinf0x
     ELSE IF(ALLOCATED(ss2%rate_sinf0x)) THEN
        CALL ps_conformal("rate_sinf0x",ss2%rate_sinf0x,ss3%rate_sinf0x,iout,ierr)
        ss3%rate_sinf0x = ss2%rate_sinf0x
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rate_sinf0xs)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%rate_sinf0xs).AND.ALLOCATED(ss2%rate_sinf0xs)) THEN
        CALL ps_conformal("rate_sinf0xs",ss2%rate_sinf0xs,ss3%rate_sinf0xs,iout,ierr)
        CALL ps_conformal("rate_sinf0xs",ss1%rate_sinf0xs,ss3%rate_sinf0xs,iout,ierr)
        if(ierr.eq.0) ss3%rate_sinf0xs = w1*ss1%rate_sinf0xs + (1.0_rspec-w1)*ss2%rate_sinf0xs
     ELSE IF(ALLOCATED(ss1%rate_sinf0xs)) THEN
        CALL ps_conformal("rate_sinf0xs",ss1%rate_sinf0xs,ss3%rate_sinf0xs,iout,ierr)
        ss3%rate_sinf0xs = ss1%rate_sinf0xs
     ELSE IF(ALLOCATED(ss2%rate_sinf0xs)) THEN
        CALL ps_conformal("rate_sinf0xs",ss2%rate_sinf0xs,ss3%rate_sinf0xs,iout,ierr)
        ss3%rate_sinf0xs = ss2%rate_sinf0xs
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Real_ant_coef)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%Real_ant_coef).AND.ALLOCATED(ss2%Real_ant_coef)) THEN
        CALL ps_diff("Real_ant_coef",ss1%Real_ant_coef,ss2%Real_ant_coef,icheck,iout,ierr)
        CALL ps_conformal("Real_ant_coef",ss1%Real_ant_coef,ss3%Real_ant_coef,iout,ierr)
        if(ierr.eq.0) ss3%Real_ant_coef = ss1%Real_ant_coef
     ELSE IF(ALLOCATED(ss1%Real_ant_coef)) THEN
        CALL ps_conformal("Real_ant_coef",ss1%Real_ant_coef,ss3%Real_ant_coef,iout,ierr)
        ss3%Real_ant_coef = ss1%Real_ant_coef
     ELSE IF(ALLOCATED(ss2%Real_ant_coef)) THEN
        CALL ps_conformal("Real_ant_coef",ss2%Real_ant_coef,ss3%Real_ant_coef,iout,ierr)
        ss3%Real_ant_coef = ss2%Real_ant_coef
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%res_sn)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%res_sn).AND.ALLOCATED(ss2%res_sn)) THEN
        CALL ps_conformal("res_sn",ss2%res_sn,ss3%res_sn,iout,ierr)
        CALL ps_conformal("res_sn",ss1%res_sn,ss3%res_sn,iout,ierr)
        if(ierr.eq.0) ss3%res_sn = w1*ss1%res_sn + (1.0_rspec-w1)*ss2%res_sn
     ELSE IF(ALLOCATED(ss1%res_sn)) THEN
        CALL ps_conformal("res_sn",ss1%res_sn,ss3%res_sn,iout,ierr)
        ss3%res_sn = ss1%res_sn
     ELSE IF(ALLOCATED(ss2%res_sn)) THEN
        CALL ps_conformal("res_sn",ss2%res_sn,ss3%res_sn,iout,ierr)
        ss3%res_sn = ss2%res_sn
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%res_te)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%res_te).AND.ALLOCATED(ss2%res_te)) THEN
        CALL ps_conformal("res_te",ss2%res_te,ss3%res_te,iout,ierr)
        CALL ps_conformal("res_te",ss1%res_te,ss3%res_te,iout,ierr)
        if(ierr.eq.0) ss3%res_te = w1*ss1%res_te + (1.0_rspec-w1)*ss2%res_te
     ELSE IF(ALLOCATED(ss1%res_te)) THEN
        CALL ps_conformal("res_te",ss1%res_te,ss3%res_te,iout,ierr)
        ss3%res_te = ss1%res_te
     ELSE IF(ALLOCATED(ss2%res_te)) THEN
        CALL ps_conformal("res_te",ss2%res_te,ss3%res_te,iout,ierr)
        ss3%res_te = ss2%res_te
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%res_ti)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%res_ti).AND.ALLOCATED(ss2%res_ti)) THEN
        CALL ps_conformal("res_ti",ss2%res_ti,ss3%res_ti,iout,ierr)
        CALL ps_conformal("res_ti",ss1%res_ti,ss3%res_ti,iout,ierr)
        if(ierr.eq.0) ss3%res_ti = w1*ss1%res_ti + (1.0_rspec-w1)*ss2%res_ti
     ELSE IF(ALLOCATED(ss1%res_ti)) THEN
        CALL ps_conformal("res_ti",ss1%res_ti,ss3%res_ti,iout,ierr)
        ss3%res_ti = ss1%res_ti
     ELSE IF(ALLOCATED(ss2%res_ti)) THEN
        CALL ps_conformal("res_ti",ss2%res_ti,ss3%res_ti,iout,ierr)
        ss3%res_ti = ss2%res_ti
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%res_tq)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%res_tq).AND.ALLOCATED(ss2%res_tq)) THEN
        CALL ps_conformal("res_tq",ss2%res_tq,ss3%res_tq,iout,ierr)
        CALL ps_conformal("res_tq",ss1%res_tq,ss3%res_tq,iout,ierr)
        if(ierr.eq.0) ss3%res_tq = w1*ss1%res_tq + (1.0_rspec-w1)*ss2%res_tq
     ELSE IF(ALLOCATED(ss1%res_tq)) THEN
        CALL ps_conformal("res_tq",ss1%res_tq,ss3%res_tq,iout,ierr)
        ss3%res_tq = ss1%res_tq
     ELSE IF(ALLOCATED(ss2%res_tq)) THEN
        CALL ps_conformal("res_tq",ss2%res_tq,ss3%res_tq,iout,ierr)
        ss3%res_tq = ss2%res_tq
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%RFMIN_name)) THEN
     IF(ALLOCATED(ss1%RFMIN_name).AND.ALLOCATED(ss2%RFMIN_name)) THEN
        CALL ps_diff("RFMIN_name",ss1%RFMIN_name,ss2%RFMIN_name,ps_error,iout,ierr)
        CALL ps_conformal("RFMIN_name",ss1%RFMIN_name,ss3%RFMIN_name,iout,ierr)
        if(ierr.eq.0) ss3%RFMIN_name = ss1%RFMIN_name
     ELSE IF(ALLOCATED(ss1%RFMIN_name)) THEN
        CALL ps_conformal("RFMIN_name",ss1%RFMIN_name,ss3%RFMIN_name,iout,ierr)
        ss3%RFMIN_name = ss1%RFMIN_name
     ELSE IF(ALLOCATED(ss2%RFMIN_name)) THEN
        CALL ps_conformal("RFMIN_name",ss2%RFMIN_name,ss3%RFMIN_name,iout,ierr)
        ss3%RFMIN_name = ss2%RFMIN_name
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%rfmin_to_all)) THEN
     IF(ALLOCATED(ss1%rfmin_to_all).AND.ALLOCATED(ss2%rfmin_to_all)) THEN
        CALL ps_diff("rfmin_to_all",ss1%rfmin_to_all,ss2%rfmin_to_all,ps_error,iout,ierr)
        CALL ps_conformal("rfmin_to_all",ss1%rfmin_to_all,ss3%rfmin_to_all,iout,ierr)
        if(ierr.eq.0) ss3%rfmin_to_all = ss1%rfmin_to_all
     ELSE IF(ALLOCATED(ss1%rfmin_to_all)) THEN
        CALL ps_conformal("rfmin_to_all",ss1%rfmin_to_all,ss3%rfmin_to_all,iout,ierr)
        ss3%rfmin_to_all = ss1%rfmin_to_all
     ELSE IF(ALLOCATED(ss2%rfmin_to_all)) THEN
        CALL ps_conformal("rfmin_to_all",ss2%rfmin_to_all,ss3%rfmin_to_all,iout,ierr)
        ss3%rfmin_to_all = ss2%rfmin_to_all
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%rfmin_to_alla)) THEN
     IF(ALLOCATED(ss1%rfmin_to_alla).AND.ALLOCATED(ss2%rfmin_to_alla)) THEN
        CALL ps_diff("rfmin_to_alla",ss1%rfmin_to_alla,ss2%rfmin_to_alla,ps_error,iout,ierr)
        CALL ps_conformal("rfmin_to_alla",ss1%rfmin_to_alla,ss3%rfmin_to_alla,iout,ierr)
        if(ierr.eq.0) ss3%rfmin_to_alla = ss1%rfmin_to_alla
     ELSE IF(ALLOCATED(ss1%rfmin_to_alla)) THEN
        CALL ps_conformal("rfmin_to_alla",ss1%rfmin_to_alla,ss3%rfmin_to_alla,iout,ierr)
        ss3%rfmin_to_alla = ss1%rfmin_to_alla
     ELSE IF(ALLOCATED(ss2%rfmin_to_alla)) THEN
        CALL ps_conformal("rfmin_to_alla",ss2%rfmin_to_alla,ss3%rfmin_to_alla,iout,ierr)
        ss3%rfmin_to_alla = ss2%rfmin_to_alla
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%RFMIN_type)) THEN
     IF(ALLOCATED(ss1%RFMIN_type).AND.ALLOCATED(ss2%RFMIN_type)) THEN
        CALL ps_diff("RFMIN_type",ss1%RFMIN_type,ss2%RFMIN_type,ps_error,iout,ierr)
        CALL ps_conformal("RFMIN_type",ss1%RFMIN_type,ss3%RFMIN_type,iout,ierr)
        if(ierr.eq.0) ss3%RFMIN_type = ss1%RFMIN_type
     ELSE IF(ALLOCATED(ss1%RFMIN_type)) THEN
        CALL ps_conformal("RFMIN_type",ss1%RFMIN_type,ss3%RFMIN_type,iout,ierr)
        ss3%RFMIN_type = ss1%RFMIN_type
     ELSE IF(ALLOCATED(ss2%RFMIN_type)) THEN
        CALL ps_conformal("RFMIN_type",ss2%RFMIN_type,ss3%RFMIN_type,iout,ierr)
        ss3%RFMIN_type = ss2%RFMIN_type
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho).AND.ALLOCATED(ss2%rho)) THEN
        CALL ps_diff("rho",ss1%rho,ss2%rho,ps_error,iout,ierr)
        CALL ps_conformal("rho",ss1%rho,ss3%rho,iout,ierr)
        if(ierr.eq.0) ss3%rho = ss1%rho
     ELSE IF(ALLOCATED(ss1%rho)) THEN
        CALL ps_conformal("rho",ss1%rho,ss3%rho,iout,ierr)
        ss3%rho = ss1%rho
     ELSE IF(ALLOCATED(ss2%rho)) THEN
        CALL ps_conformal("rho",ss2%rho,ss3%rho,iout,ierr)
        ss3%rho = ss2%rho
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rhopsi0lh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%rhopsi0lh).AND.ALLOCATED(ss2%rhopsi0lh)) THEN
        CALL ps_diff("rhopsi0lh",ss1%rhopsi0lh,ss2%rhopsi0lh,icheck,iout,ierr)
        CALL ps_conformal("rhopsi0lh",ss1%rhopsi0lh,ss3%rhopsi0lh,iout,ierr)
        if(ierr.eq.0) ss3%rhopsi0lh = ss1%rhopsi0lh
     ELSE IF(ALLOCATED(ss1%rhopsi0lh)) THEN
        CALL ps_conformal("rhopsi0lh",ss1%rhopsi0lh,ss3%rhopsi0lh,iout,ierr)
        ss3%rhopsi0lh = ss1%rhopsi0lh
     ELSE IF(ALLOCATED(ss2%rhopsi0lh)) THEN
        CALL ps_conformal("rhopsi0lh",ss2%rhopsi0lh,ss3%rhopsi0lh,iout,ierr)
        ss3%rhopsi0lh = ss2%rhopsi0lh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_anom).AND.ALLOCATED(ss2%rho_anom)) THEN
        CALL ps_diff("rho_anom",ss1%rho_anom,ss2%rho_anom,ps_error,iout,ierr)
        CALL ps_conformal("rho_anom",ss1%rho_anom,ss3%rho_anom,iout,ierr)
        if(ierr.eq.0) ss3%rho_anom = ss1%rho_anom
     ELSE IF(ALLOCATED(ss1%rho_anom)) THEN
        CALL ps_conformal("rho_anom",ss1%rho_anom,ss3%rho_anom,iout,ierr)
        ss3%rho_anom = ss1%rho_anom
     ELSE IF(ALLOCATED(ss2%rho_anom)) THEN
        CALL ps_conformal("rho_anom",ss2%rho_anom,ss3%rho_anom,iout,ierr)
        ss3%rho_anom = ss2%rho_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_anom2)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_anom2).AND.ALLOCATED(ss2%rho_anom2)) THEN
        CALL ps_diff("rho_anom2",ss1%rho_anom2,ss2%rho_anom2,ps_error,iout,ierr)
        CALL ps_conformal("rho_anom2",ss1%rho_anom2,ss3%rho_anom2,iout,ierr)
        if(ierr.eq.0) ss3%rho_anom2 = ss1%rho_anom2
     ELSE IF(ALLOCATED(ss1%rho_anom2)) THEN
        CALL ps_conformal("rho_anom2",ss1%rho_anom2,ss3%rho_anom2,iout,ierr)
        ss3%rho_anom2 = ss1%rho_anom2
     ELSE IF(ALLOCATED(ss2%rho_anom2)) THEN
        CALL ps_conformal("rho_anom2",ss2%rho_anom2,ss3%rho_anom2,iout,ierr)
        ss3%rho_anom2 = ss2%rho_anom2
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_anomth)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_anomth).AND.ALLOCATED(ss2%rho_anomth)) THEN
        CALL ps_diff("rho_anomth",ss1%rho_anomth,ss2%rho_anomth,ps_error,iout,ierr)
        CALL ps_conformal("rho_anomth",ss1%rho_anomth,ss3%rho_anomth,iout,ierr)
        if(ierr.eq.0) ss3%rho_anomth = ss1%rho_anomth
     ELSE IF(ALLOCATED(ss1%rho_anomth)) THEN
        CALL ps_conformal("rho_anomth",ss1%rho_anomth,ss3%rho_anomth,iout,ierr)
        ss3%rho_anomth = ss1%rho_anomth
     ELSE IF(ALLOCATED(ss2%rho_anomth)) THEN
        CALL ps_conformal("rho_anomth",ss2%rho_anomth,ss3%rho_anomth,iout,ierr)
        ss3%rho_anomth = ss2%rho_anomth
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_bdy_ns)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_bdy_ns).AND.ALLOCATED(ss2%rho_bdy_ns)) THEN
        CALL ps_conformal("rho_bdy_ns",ss2%rho_bdy_ns,ss3%rho_bdy_ns,iout,ierr)
        CALL ps_conformal("rho_bdy_ns",ss1%rho_bdy_ns,ss3%rho_bdy_ns,iout,ierr)
        if(ierr.eq.0) ss3%rho_bdy_ns = w1*ss1%rho_bdy_ns + (1.0_rspec-w1)*ss2%rho_bdy_ns
     ELSE IF(ALLOCATED(ss1%rho_bdy_ns)) THEN
        CALL ps_conformal("rho_bdy_ns",ss1%rho_bdy_ns,ss3%rho_bdy_ns,iout,ierr)
        ss3%rho_bdy_ns = ss1%rho_bdy_ns
     ELSE IF(ALLOCATED(ss2%rho_bdy_ns)) THEN
        CALL ps_conformal("rho_bdy_ns",ss2%rho_bdy_ns,ss3%rho_bdy_ns,iout,ierr)
        ss3%rho_bdy_ns = ss2%rho_bdy_ns
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     ss3%rho_bdy_omegat = w1*ss1%rho_bdy_omegat + (1.0_rspec-w1)*ss2%rho_bdy_omegat
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     ss3%rho_bdy_Te = w1*ss1%rho_bdy_Te + (1.0_rspec-w1)*ss2%rho_bdy_Te
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     ss3%rho_bdy_Ti = w1*ss1%rho_bdy_Ti + (1.0_rspec-w1)*ss2%rho_bdy_Ti
  ENDIF

  IF((ALLOCATED(ss3%rho_ecrf)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_ecrf).AND.ALLOCATED(ss2%rho_ecrf)) THEN
        CALL ps_diff("rho_ecrf",ss1%rho_ecrf,ss2%rho_ecrf,ps_error,iout,ierr)
        CALL ps_conformal("rho_ecrf",ss1%rho_ecrf,ss3%rho_ecrf,iout,ierr)
        if(ierr.eq.0) ss3%rho_ecrf = ss1%rho_ecrf
     ELSE IF(ALLOCATED(ss1%rho_ecrf)) THEN
        CALL ps_conformal("rho_ecrf",ss1%rho_ecrf,ss3%rho_ecrf,iout,ierr)
        ss3%rho_ecrf = ss1%rho_ecrf
     ELSE IF(ALLOCATED(ss2%rho_ecrf)) THEN
        CALL ps_conformal("rho_ecrf",ss2%rho_ecrf,ss3%rho_ecrf,iout,ierr)
        ss3%rho_ecrf = ss2%rho_ecrf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_eq)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_eq).AND.ALLOCATED(ss2%rho_eq)) THEN
        CALL ps_diff("rho_eq",ss1%rho_eq,ss2%rho_eq,ps_error,iout,ierr)
        CALL ps_conformal("rho_eq",ss1%rho_eq,ss3%rho_eq,iout,ierr)
        if(ierr.eq.0) ss3%rho_eq = ss1%rho_eq
     ELSE IF(ALLOCATED(ss1%rho_eq)) THEN
        CALL ps_conformal("rho_eq",ss1%rho_eq,ss3%rho_eq,iout,ierr)
        ss3%rho_eq = ss1%rho_eq
     ELSE IF(ALLOCATED(ss2%rho_eq)) THEN
        CALL ps_conformal("rho_eq",ss2%rho_eq,ss3%rho_eq,iout,ierr)
        ss3%rho_eq = ss2%rho_eq
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_eq_geo)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_eq_geo).AND.ALLOCATED(ss2%rho_eq_geo)) THEN
        CALL ps_diff("rho_eq_geo",ss1%rho_eq_geo,ss2%rho_eq_geo,ps_error,iout,ierr)
        CALL ps_conformal("rho_eq_geo",ss1%rho_eq_geo,ss3%rho_eq_geo,iout,ierr)
        if(ierr.eq.0) ss3%rho_eq_geo = ss1%rho_eq_geo
     ELSE IF(ALLOCATED(ss1%rho_eq_geo)) THEN
        CALL ps_conformal("rho_eq_geo",ss1%rho_eq_geo,ss3%rho_eq_geo,iout,ierr)
        ss3%rho_eq_geo = ss1%rho_eq_geo
     ELSE IF(ALLOCATED(ss2%rho_eq_geo)) THEN
        CALL ps_conformal("rho_eq_geo",ss2%rho_eq_geo,ss3%rho_eq_geo,iout,ierr)
        ss3%rho_eq_geo = ss2%rho_eq_geo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_fus)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_fus).AND.ALLOCATED(ss2%rho_fus)) THEN
        CALL ps_diff("rho_fus",ss1%rho_fus,ss2%rho_fus,ps_error,iout,ierr)
        CALL ps_conformal("rho_fus",ss1%rho_fus,ss3%rho_fus,iout,ierr)
        if(ierr.eq.0) ss3%rho_fus = ss1%rho_fus
     ELSE IF(ALLOCATED(ss1%rho_fus)) THEN
        CALL ps_conformal("rho_fus",ss1%rho_fus,ss3%rho_fus,iout,ierr)
        ss3%rho_fus = ss1%rho_fus
     ELSE IF(ALLOCATED(ss2%rho_fus)) THEN
        CALL ps_conformal("rho_fus",ss2%rho_fus,ss3%rho_fus,iout,ierr)
        ss3%rho_fus = ss2%rho_fus
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_gas)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_gas).AND.ALLOCATED(ss2%rho_gas)) THEN
        CALL ps_diff("rho_gas",ss1%rho_gas,ss2%rho_gas,ps_error,iout,ierr)
        CALL ps_conformal("rho_gas",ss1%rho_gas,ss3%rho_gas,iout,ierr)
        if(ierr.eq.0) ss3%rho_gas = ss1%rho_gas
     ELSE IF(ALLOCATED(ss1%rho_gas)) THEN
        CALL ps_conformal("rho_gas",ss1%rho_gas,ss3%rho_gas,iout,ierr)
        ss3%rho_gas = ss1%rho_gas
     ELSE IF(ALLOCATED(ss2%rho_gas)) THEN
        CALL ps_conformal("rho_gas",ss2%rho_gas,ss3%rho_gas,iout,ierr)
        ss3%rho_gas = ss2%rho_gas
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_icrf)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_icrf).AND.ALLOCATED(ss2%rho_icrf)) THEN
        CALL ps_diff("rho_icrf",ss1%rho_icrf,ss2%rho_icrf,ps_error,iout,ierr)
        CALL ps_conformal("rho_icrf",ss1%rho_icrf,ss3%rho_icrf,iout,ierr)
        if(ierr.eq.0) ss3%rho_icrf = ss1%rho_icrf
     ELSE IF(ALLOCATED(ss1%rho_icrf)) THEN
        CALL ps_conformal("rho_icrf",ss1%rho_icrf,ss3%rho_icrf,iout,ierr)
        ss3%rho_icrf = ss1%rho_icrf
     ELSE IF(ALLOCATED(ss2%rho_icrf)) THEN
        CALL ps_conformal("rho_icrf",ss2%rho_icrf,ss3%rho_icrf,iout,ierr)
        ss3%rho_icrf = ss2%rho_icrf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_lhrf)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_lhrf).AND.ALLOCATED(ss2%rho_lhrf)) THEN
        CALL ps_diff("rho_lhrf",ss1%rho_lhrf,ss2%rho_lhrf,ps_error,iout,ierr)
        CALL ps_conformal("rho_lhrf",ss1%rho_lhrf,ss3%rho_lhrf,iout,ierr)
        if(ierr.eq.0) ss3%rho_lhrf = ss1%rho_lhrf
     ELSE IF(ALLOCATED(ss1%rho_lhrf)) THEN
        CALL ps_conformal("rho_lhrf",ss1%rho_lhrf,ss3%rho_lhrf,iout,ierr)
        ss3%rho_lhrf = ss1%rho_lhrf
     ELSE IF(ALLOCATED(ss2%rho_lhrf)) THEN
        CALL ps_conformal("rho_lhrf",ss2%rho_lhrf,ss3%rho_lhrf,iout,ierr)
        ss3%rho_lhrf = ss2%rho_lhrf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_lmhd)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_lmhd).AND.ALLOCATED(ss2%rho_lmhd)) THEN
        CALL ps_diff("rho_lmhd",ss1%rho_lmhd,ss2%rho_lmhd,ps_error,iout,ierr)
        CALL ps_conformal("rho_lmhd",ss1%rho_lmhd,ss3%rho_lmhd,iout,ierr)
        if(ierr.eq.0) ss3%rho_lmhd = ss1%rho_lmhd
     ELSE IF(ALLOCATED(ss1%rho_lmhd)) THEN
        CALL ps_conformal("rho_lmhd",ss1%rho_lmhd,ss3%rho_lmhd,iout,ierr)
        ss3%rho_lmhd = ss1%rho_lmhd
     ELSE IF(ALLOCATED(ss2%rho_lmhd)) THEN
        CALL ps_conformal("rho_lmhd",ss2%rho_lmhd,ss3%rho_lmhd,iout,ierr)
        ss3%rho_lmhd = ss2%rho_lmhd
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_mist)).AND.(cclist(ps_cnum_RAD).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_mist).AND.ALLOCATED(ss2%rho_mist)) THEN
        CALL ps_diff("rho_mist",ss1%rho_mist,ss2%rho_mist,ps_error,iout,ierr)
        CALL ps_conformal("rho_mist",ss1%rho_mist,ss3%rho_mist,iout,ierr)
        if(ierr.eq.0) ss3%rho_mist = ss1%rho_mist
     ELSE IF(ALLOCATED(ss1%rho_mist)) THEN
        CALL ps_conformal("rho_mist",ss1%rho_mist,ss3%rho_mist,iout,ierr)
        ss3%rho_mist = ss1%rho_mist
     ELSE IF(ALLOCATED(ss2%rho_mist)) THEN
        CALL ps_conformal("rho_mist",ss2%rho_mist,ss3%rho_mist,iout,ierr)
        ss3%rho_mist = ss2%rho_mist
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_nbi)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_nbi).AND.ALLOCATED(ss2%rho_nbi)) THEN
        CALL ps_diff("rho_nbi",ss1%rho_nbi,ss2%rho_nbi,ps_error,iout,ierr)
        CALL ps_conformal("rho_nbi",ss1%rho_nbi,ss3%rho_nbi,iout,ierr)
        if(ierr.eq.0) ss3%rho_nbi = ss1%rho_nbi
     ELSE IF(ALLOCATED(ss1%rho_nbi)) THEN
        CALL ps_conformal("rho_nbi",ss1%rho_nbi,ss3%rho_nbi,iout,ierr)
        ss3%rho_nbi = ss1%rho_nbi
     ELSE IF(ALLOCATED(ss2%rho_nbi)) THEN
        CALL ps_conformal("rho_nbi",ss2%rho_nbi,ss3%rho_nbi,iout,ierr)
        ss3%rho_nbi = ss2%rho_nbi
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_merge_6
