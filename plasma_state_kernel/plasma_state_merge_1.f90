! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_merge_1(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)


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

  IF((ALLOCATED(ss3%chii_ntm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chii_ntm).AND.ALLOCATED(ss2%chii_ntm)) THEN
        CALL ps_conformal("chii_ntm",ss2%chii_ntm,ss3%chii_ntm,iout,ierr)
        CALL ps_conformal("chii_ntm",ss1%chii_ntm,ss3%chii_ntm,iout,ierr)
        if(ierr.eq.0) ss3%chii_ntm = w1*ss1%chii_ntm + (1.0_rspec-w1)*ss2%chii_ntm
     ELSE IF(ALLOCATED(ss1%chii_ntm)) THEN
        CALL ps_conformal("chii_ntm",ss1%chii_ntm,ss3%chii_ntm,iout,ierr)
        ss3%chii_ntm = ss1%chii_ntm
     ELSE IF(ALLOCATED(ss2%chii_ntm)) THEN
        CALL ps_conformal("chii_ntm",ss2%chii_ntm,ss3%chii_ntm,iout,ierr)
        ss3%chii_ntm = ss2%chii_ntm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chii_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chii_tglf).AND.ALLOCATED(ss2%chii_tglf)) THEN
        CALL ps_conformal("chii_tglf",ss2%chii_tglf,ss3%chii_tglf,iout,ierr)
        CALL ps_conformal("chii_tglf",ss1%chii_tglf,ss3%chii_tglf,iout,ierr)
        if(ierr.eq.0) ss3%chii_tglf = w1*ss1%chii_tglf + (1.0_rspec-w1)*ss2%chii_tglf
     ELSE IF(ALLOCATED(ss1%chii_tglf)) THEN
        CALL ps_conformal("chii_tglf",ss1%chii_tglf,ss3%chii_tglf,iout,ierr)
        ss3%chii_tglf = ss1%chii_tglf
     ELSE IF(ALLOCATED(ss2%chii_tglf)) THEN
        CALL ps_conformal("chii_tglf",ss2%chii_tglf,ss3%chii_tglf,iout,ierr)
        ss3%chii_tglf = ss2%chii_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chii_trans)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%chii_trans).AND.ALLOCATED(ss2%chii_trans)) THEN
        CALL ps_conformal("chii_trans",ss2%chii_trans,ss3%chii_trans,iout,ierr)
        CALL ps_conformal("chii_trans",ss1%chii_trans,ss3%chii_trans,iout,ierr)
        if(ierr.eq.0) ss3%chii_trans = w1*ss1%chii_trans + (1.0_rspec-w1)*ss2%chii_trans
     ELSE IF(ALLOCATED(ss1%chii_trans)) THEN
        CALL ps_conformal("chii_trans",ss1%chii_trans,ss3%chii_trans,iout,ierr)
        ss3%chii_trans = ss1%chii_trans
     ELSE IF(ALLOCATED(ss2%chii_trans)) THEN
        CALL ps_conformal("chii_trans",ss2%chii_trans,ss3%chii_trans,iout,ierr)
        ss3%chii_trans = ss2%chii_trans
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chii_transcat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%chii_transcat).AND.ALLOCATED(ss2%chii_transcat)) THEN
        CALL ps_conformal("chii_transcat",ss2%chii_transcat,ss3%chii_transcat,iout,ierr)
        CALL ps_conformal("chii_transcat",ss1%chii_transcat,ss3%chii_transcat,iout,ierr)
        if(ierr.eq.0) ss3%chii_transcat = w1*ss1%chii_transcat + (1.0_rspec-w1)*ss2%chii_transcat
     ELSE IF(ALLOCATED(ss1%chii_transcat)) THEN
        CALL ps_conformal("chii_transcat",ss1%chii_transcat,ss3%chii_transcat,iout,ierr)
        ss3%chii_transcat = ss1%chii_transcat
     ELSE IF(ALLOCATED(ss2%chii_transcat)) THEN
        CALL ps_conformal("chii_transcat",ss2%chii_transcat,ss3%chii_transcat,iout,ierr)
        ss3%chii_transcat = ss2%chii_transcat
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chii_w19)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chii_w19).AND.ALLOCATED(ss2%chii_w19)) THEN
        CALL ps_conformal("chii_w19",ss2%chii_w19,ss3%chii_w19,iout,ierr)
        CALL ps_conformal("chii_w19",ss1%chii_w19,ss3%chii_w19,iout,ierr)
        if(ierr.eq.0) ss3%chii_w19 = w1*ss1%chii_w19 + (1.0_rspec-w1)*ss2%chii_w19
     ELSE IF(ALLOCATED(ss1%chii_w19)) THEN
        CALL ps_conformal("chii_w19",ss1%chii_w19,ss3%chii_w19,iout,ierr)
        ss3%chii_w19 = ss1%chii_w19
     ELSE IF(ALLOCATED(ss2%chii_w19)) THEN
        CALL ps_conformal("chii_w19",ss2%chii_w19,ss3%chii_w19,iout,ierr)
        ss3%chii_w19 = ss2%chii_w19
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chimo_trans)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%chimo_trans).AND.ALLOCATED(ss2%chimo_trans)) THEN
        CALL ps_conformal("chimo_trans",ss2%chimo_trans,ss3%chimo_trans,iout,ierr)
        CALL ps_conformal("chimo_trans",ss1%chimo_trans,ss3%chimo_trans,iout,ierr)
        if(ierr.eq.0) ss3%chimo_trans = w1*ss1%chimo_trans + (1.0_rspec-w1)*ss2%chimo_trans
     ELSE IF(ALLOCATED(ss1%chimo_trans)) THEN
        CALL ps_conformal("chimo_trans",ss1%chimo_trans,ss3%chimo_trans,iout,ierr)
        ss3%chimo_trans = ss1%chimo_trans
     ELSE IF(ALLOCATED(ss2%chimo_trans)) THEN
        CALL ps_conformal("chimo_trans",ss2%chimo_trans,ss3%chimo_trans,iout,ierr)
        ss3%chimo_trans = ss2%chimo_trans
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chimo_transcat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%chimo_transcat).AND.ALLOCATED(ss2%chimo_transcat)) THEN
        CALL ps_conformal("chimo_transcat",ss2%chimo_transcat,ss3%chimo_transcat,iout,ierr)
        CALL ps_conformal("chimo_transcat",ss1%chimo_transcat,ss3%chimo_transcat,iout,ierr)
        if(ierr.eq.0) ss3%chimo_transcat = w1*ss1%chimo_transcat + (1.0_rspec-w1)*ss2%chimo_transcat
     ELSE IF(ALLOCATED(ss1%chimo_transcat)) THEN
        CALL ps_conformal("chimo_transcat",ss1%chimo_transcat,ss3%chimo_transcat,iout,ierr)
        ss3%chimo_transcat = ss1%chimo_transcat
     ELSE IF(ALLOCATED(ss2%chimo_transcat)) THEN
        CALL ps_conformal("chimo_transcat",ss2%chimo_transcat,ss3%chimo_transcat,iout,ierr)
        ss3%chimo_transcat = ss2%chimo_transcat
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chiphi_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chiphi_anom).AND.ALLOCATED(ss2%chiphi_anom)) THEN
        CALL ps_conformal("chiphi_anom",ss2%chiphi_anom,ss3%chiphi_anom,iout,ierr)
        CALL ps_conformal("chiphi_anom",ss1%chiphi_anom,ss3%chiphi_anom,iout,ierr)
        if(ierr.eq.0) ss3%chiphi_anom = w1*ss1%chiphi_anom + (1.0_rspec-w1)*ss2%chiphi_anom
     ELSE IF(ALLOCATED(ss1%chiphi_anom)) THEN
        CALL ps_conformal("chiphi_anom",ss1%chiphi_anom,ss3%chiphi_anom,iout,ierr)
        ss3%chiphi_anom = ss1%chiphi_anom
     ELSE IF(ALLOCATED(ss2%chiphi_anom)) THEN
        CALL ps_conformal("chiphi_anom",ss2%chiphi_anom,ss3%chiphi_anom,iout,ierr)
        ss3%chiphi_anom = ss2%chiphi_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chiphi_drbm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chiphi_drbm).AND.ALLOCATED(ss2%chiphi_drbm)) THEN
        CALL ps_conformal("chiphi_drbm",ss2%chiphi_drbm,ss3%chiphi_drbm,iout,ierr)
        CALL ps_conformal("chiphi_drbm",ss1%chiphi_drbm,ss3%chiphi_drbm,iout,ierr)
        if(ierr.eq.0) ss3%chiphi_drbm = w1*ss1%chiphi_drbm + (1.0_rspec-w1)*ss2%chiphi_drbm
     ELSE IF(ALLOCATED(ss1%chiphi_drbm)) THEN
        CALL ps_conformal("chiphi_drbm",ss1%chiphi_drbm,ss3%chiphi_drbm,iout,ierr)
        ss3%chiphi_drbm = ss1%chiphi_drbm
     ELSE IF(ALLOCATED(ss2%chiphi_drbm)) THEN
        CALL ps_conformal("chiphi_drbm",ss2%chiphi_drbm,ss3%chiphi_drbm,iout,ierr)
        ss3%chiphi_drbm = ss2%chiphi_drbm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chiphi_glf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chiphi_glf).AND.ALLOCATED(ss2%chiphi_glf)) THEN
        CALL ps_conformal("chiphi_glf",ss2%chiphi_glf,ss3%chiphi_glf,iout,ierr)
        CALL ps_conformal("chiphi_glf",ss1%chiphi_glf,ss3%chiphi_glf,iout,ierr)
        if(ierr.eq.0) ss3%chiphi_glf = w1*ss1%chiphi_glf + (1.0_rspec-w1)*ss2%chiphi_glf
     ELSE IF(ALLOCATED(ss1%chiphi_glf)) THEN
        CALL ps_conformal("chiphi_glf",ss1%chiphi_glf,ss3%chiphi_glf,iout,ierr)
        ss3%chiphi_glf = ss1%chiphi_glf
     ELSE IF(ALLOCATED(ss2%chiphi_glf)) THEN
        CALL ps_conformal("chiphi_glf",ss2%chiphi_glf,ss3%chiphi_glf,iout,ierr)
        ss3%chiphi_glf = ss2%chiphi_glf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chiphi_gtcneo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chiphi_gtcneo).AND.ALLOCATED(ss2%chiphi_gtcneo)) THEN
        CALL ps_conformal("chiphi_gtcneo",ss2%chiphi_gtcneo,ss3%chiphi_gtcneo,iout,ierr)
        CALL ps_conformal("chiphi_gtcneo",ss1%chiphi_gtcneo,ss3%chiphi_gtcneo,iout,ierr)
        if(ierr.eq.0) ss3%chiphi_gtcneo = w1*ss1%chiphi_gtcneo + (1.0_rspec-w1)*ss2%chiphi_gtcneo
     ELSE IF(ALLOCATED(ss1%chiphi_gtcneo)) THEN
        CALL ps_conformal("chiphi_gtcneo",ss1%chiphi_gtcneo,ss3%chiphi_gtcneo,iout,ierr)
        ss3%chiphi_gtcneo = ss1%chiphi_gtcneo
     ELSE IF(ALLOCATED(ss2%chiphi_gtcneo)) THEN
        CALL ps_conformal("chiphi_gtcneo",ss2%chiphi_gtcneo,ss3%chiphi_gtcneo,iout,ierr)
        ss3%chiphi_gtcneo = ss2%chiphi_gtcneo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chiphi_mmm71)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chiphi_mmm71).AND.ALLOCATED(ss2%chiphi_mmm71)) THEN
        CALL ps_conformal("chiphi_mmm71",ss2%chiphi_mmm71,ss3%chiphi_mmm71,iout,ierr)
        CALL ps_conformal("chiphi_mmm71",ss1%chiphi_mmm71,ss3%chiphi_mmm71,iout,ierr)
        if(ierr.eq.0) ss3%chiphi_mmm71 = w1*ss1%chiphi_mmm71 + (1.0_rspec-w1)*ss2%chiphi_mmm71
     ELSE IF(ALLOCATED(ss1%chiphi_mmm71)) THEN
        CALL ps_conformal("chiphi_mmm71",ss1%chiphi_mmm71,ss3%chiphi_mmm71,iout,ierr)
        ss3%chiphi_mmm71 = ss1%chiphi_mmm71
     ELSE IF(ALLOCATED(ss2%chiphi_mmm71)) THEN
        CALL ps_conformal("chiphi_mmm71",ss2%chiphi_mmm71,ss3%chiphi_mmm71,iout,ierr)
        ss3%chiphi_mmm71 = ss2%chiphi_mmm71
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chiphi_neo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chiphi_neo).AND.ALLOCATED(ss2%chiphi_neo)) THEN
        CALL ps_conformal("chiphi_neo",ss2%chiphi_neo,ss3%chiphi_neo,iout,ierr)
        CALL ps_conformal("chiphi_neo",ss1%chiphi_neo,ss3%chiphi_neo,iout,ierr)
        if(ierr.eq.0) ss3%chiphi_neo = w1*ss1%chiphi_neo + (1.0_rspec-w1)*ss2%chiphi_neo
     ELSE IF(ALLOCATED(ss1%chiphi_neo)) THEN
        CALL ps_conformal("chiphi_neo",ss1%chiphi_neo,ss3%chiphi_neo,iout,ierr)
        ss3%chiphi_neo = ss1%chiphi_neo
     ELSE IF(ALLOCATED(ss2%chiphi_neo)) THEN
        CALL ps_conformal("chiphi_neo",ss2%chiphi_neo,ss3%chiphi_neo,iout,ierr)
        ss3%chiphi_neo = ss2%chiphi_neo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chiphi_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chiphi_tglf).AND.ALLOCATED(ss2%chiphi_tglf)) THEN
        CALL ps_conformal("chiphi_tglf",ss2%chiphi_tglf,ss3%chiphi_tglf,iout,ierr)
        CALL ps_conformal("chiphi_tglf",ss1%chiphi_tglf,ss3%chiphi_tglf,iout,ierr)
        if(ierr.eq.0) ss3%chiphi_tglf = w1*ss1%chiphi_tglf + (1.0_rspec-w1)*ss2%chiphi_tglf
     ELSE IF(ALLOCATED(ss1%chiphi_tglf)) THEN
        CALL ps_conformal("chiphi_tglf",ss1%chiphi_tglf,ss3%chiphi_tglf,iout,ierr)
        ss3%chiphi_tglf = ss1%chiphi_tglf
     ELSE IF(ALLOCATED(ss2%chiphi_tglf)) THEN
        CALL ps_conformal("chiphi_tglf",ss2%chiphi_tglf,ss3%chiphi_tglf,iout,ierr)
        ss3%chiphi_tglf = ss2%chiphi_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chiphi_w19)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chiphi_w19).AND.ALLOCATED(ss2%chiphi_w19)) THEN
        CALL ps_conformal("chiphi_w19",ss2%chiphi_w19,ss3%chiphi_w19,iout,ierr)
        CALL ps_conformal("chiphi_w19",ss1%chiphi_w19,ss3%chiphi_w19,iout,ierr)
        if(ierr.eq.0) ss3%chiphi_w19 = w1*ss1%chiphi_w19 + (1.0_rspec-w1)*ss2%chiphi_w19
     ELSE IF(ALLOCATED(ss1%chiphi_w19)) THEN
        CALL ps_conformal("chiphi_w19",ss1%chiphi_w19,ss3%chiphi_w19,iout,ierr)
        ss3%chiphi_w19 = ss1%chiphi_w19
     ELSE IF(ALLOCATED(ss2%chiphi_w19)) THEN
        CALL ps_conformal("chiphi_w19",ss2%chiphi_w19,ss3%chiphi_w19,iout,ierr)
        ss3%chiphi_w19 = ss2%chiphi_w19
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chitheta_mmm71)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chitheta_mmm71).AND.ALLOCATED(ss2%chitheta_mmm71)) THEN
        CALL ps_conformal("chitheta_mmm71",ss2%chitheta_mmm71,ss3%chitheta_mmm71,iout,ierr)
        CALL ps_conformal("chitheta_mmm71",ss1%chitheta_mmm71,ss3%chitheta_mmm71,iout,ierr)
        if(ierr.eq.0) ss3%chitheta_mmm71 = w1*ss1%chitheta_mmm71 + (1.0_rspec-w1)*ss2%chitheta_mmm71
     ELSE IF(ALLOCATED(ss1%chitheta_mmm71)) THEN
        CALL ps_conformal("chitheta_mmm71",ss1%chitheta_mmm71,ss3%chitheta_mmm71,iout,ierr)
        ss3%chitheta_mmm71 = ss1%chitheta_mmm71
     ELSE IF(ALLOCATED(ss2%chitheta_mmm71)) THEN
        CALL ps_conformal("chitheta_mmm71",ss2%chitheta_mmm71,ss3%chitheta_mmm71,iout,ierr)
        ss3%chitheta_mmm71 = ss2%chitheta_mmm71
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%circuit_name)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%circuit_name).AND.ALLOCATED(ss2%circuit_name)) THEN
        CALL ps_diff("circuit_name",ss1%circuit_name,ss2%circuit_name,icheck,iout,ierr)
        CALL ps_conformal("circuit_name",ss1%circuit_name,ss3%circuit_name,iout,ierr)
        if(ierr.eq.0) ss3%circuit_name = ss1%circuit_name
     ELSE IF(ALLOCATED(ss1%circuit_name)) THEN
        CALL ps_conformal("circuit_name",ss1%circuit_name,ss3%circuit_name,iout,ierr)
        ss3%circuit_name = ss1%circuit_name
     ELSE IF(ALLOCATED(ss2%circuit_name)) THEN
        CALL ps_conformal("circuit_name",ss2%circuit_name,ss3%circuit_name,iout,ierr)
        ss3%circuit_name = ss2%circuit_name
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%coil_apt)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%coil_apt).AND.ALLOCATED(ss2%coil_apt)) THEN
        CALL ps_conformal("coil_apt",ss2%coil_apt,ss3%coil_apt,iout,ierr)
        CALL ps_conformal("coil_apt",ss1%coil_apt,ss3%coil_apt,iout,ierr)
        if(ierr.eq.0) ss3%coil_apt = w1*ss1%coil_apt + (1.0_rspec-w1)*ss2%coil_apt
     ELSE IF(ALLOCATED(ss1%coil_apt)) THEN
        CALL ps_conformal("coil_apt",ss1%coil_apt,ss3%coil_apt,iout,ierr)
        ss3%coil_apt = ss1%coil_apt
     ELSE IF(ALLOCATED(ss2%coil_apt)) THEN
        CALL ps_conformal("coil_apt",ss2%coil_apt,ss3%coil_apt,iout,ierr)
        ss3%coil_apt = ss2%coil_apt
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%coil_in_circuit)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%coil_in_circuit).AND.ALLOCATED(ss2%coil_in_circuit)) THEN
        CALL ps_diff("coil_in_circuit",ss1%coil_in_circuit,ss2%coil_in_circuit,icheck,iout,ierr)
        CALL ps_conformal("coil_in_circuit",ss1%coil_in_circuit,ss3%coil_in_circuit,iout,ierr)
        if(ierr.eq.0) ss3%coil_in_circuit = ss1%coil_in_circuit
     ELSE IF(ALLOCATED(ss1%coil_in_circuit)) THEN
        CALL ps_conformal("coil_in_circuit",ss1%coil_in_circuit,ss3%coil_in_circuit,iout,ierr)
        ss3%coil_in_circuit = ss1%coil_in_circuit
     ELSE IF(ALLOCATED(ss2%coil_in_circuit)) THEN
        CALL ps_conformal("coil_in_circuit",ss2%coil_in_circuit,ss3%coil_in_circuit,iout,ierr)
        ss3%coil_in_circuit = ss2%coil_in_circuit
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%coil_name)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%coil_name).AND.ALLOCATED(ss2%coil_name)) THEN
        CALL ps_diff("coil_name",ss1%coil_name,ss2%coil_name,icheck,iout,ierr)
        CALL ps_conformal("coil_name",ss1%coil_name,ss3%coil_name,iout,ierr)
        if(ierr.eq.0) ss3%coil_name = ss1%coil_name
     ELSE IF(ALLOCATED(ss1%coil_name)) THEN
        CALL ps_conformal("coil_name",ss1%coil_name,ss3%coil_name,iout,ierr)
        ss3%coil_name = ss1%coil_name
     ELSE IF(ALLOCATED(ss2%coil_name)) THEN
        CALL ps_conformal("coil_name",ss2%coil_name,ss3%coil_name,iout,ierr)
        ss3%coil_name = ss2%coil_name
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%coil_resispt)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%coil_resispt).AND.ALLOCATED(ss2%coil_resispt)) THEN
        CALL ps_diff("coil_resispt",ss1%coil_resispt,ss2%coil_resispt,icheck,iout,ierr)
        CALL ps_conformal("coil_resispt",ss1%coil_resispt,ss3%coil_resispt,iout,ierr)
        if(ierr.eq.0) ss3%coil_resispt = ss1%coil_resispt
     ELSE IF(ALLOCATED(ss1%coil_resispt)) THEN
        CALL ps_conformal("coil_resispt",ss1%coil_resispt,ss3%coil_resispt,iout,ierr)
        ss3%coil_resispt = ss1%coil_resispt
     ELSE IF(ALLOCATED(ss2%coil_resispt)) THEN
        CALL ps_conformal("coil_resispt",ss2%coil_resispt,ss3%coil_resispt,iout,ierr)
        ss3%coil_resispt = ss2%coil_resispt
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%curbeam)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%curbeam).AND.ALLOCATED(ss2%curbeam)) THEN
        CALL ps_conformal("curbeam",ss2%curbeam,ss3%curbeam,iout,ierr)
        CALL ps_conformal("curbeam",ss1%curbeam,ss3%curbeam,iout,ierr)
        if(ierr.eq.0) ss3%curbeam = w1*ss1%curbeam + (1.0_rspec-w1)*ss2%curbeam
     ELSE IF(ALLOCATED(ss1%curbeam)) THEN
        CALL ps_conformal("curbeam",ss1%curbeam,ss3%curbeam,iout,ierr)
        ss3%curbeam = ss1%curbeam
     ELSE IF(ALLOCATED(ss2%curbeam)) THEN
        CALL ps_conformal("curbeam",ss2%curbeam,ss3%curbeam,iout,ierr)
        ss3%curbeam = ss2%curbeam
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%curech)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%curech).AND.ALLOCATED(ss2%curech)) THEN
        CALL ps_conformal("curech",ss2%curech,ss3%curech,iout,ierr)
        CALL ps_conformal("curech",ss1%curech,ss3%curech,iout,ierr)
        if(ierr.eq.0) ss3%curech = w1*ss1%curech + (1.0_rspec-w1)*ss2%curech
     ELSE IF(ALLOCATED(ss1%curech)) THEN
        CALL ps_conformal("curech",ss1%curech,ss3%curech,iout,ierr)
        ss3%curech = ss1%curech
     ELSE IF(ALLOCATED(ss2%curech)) THEN
        CALL ps_conformal("curech",ss2%curech,ss3%curech,iout,ierr)
        ss3%curech = ss2%curech
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%curech_src)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%curech_src).AND.ALLOCATED(ss2%curech_src)) THEN
        CALL ps_conformal("curech_src",ss2%curech_src,ss3%curech_src,iout,ierr)
        CALL ps_conformal("curech_src",ss1%curech_src,ss3%curech_src,iout,ierr)
        if(ierr.eq.0) ss3%curech_src = w1*ss1%curech_src + (1.0_rspec-w1)*ss2%curech_src
     ELSE IF(ALLOCATED(ss1%curech_src)) THEN
        CALL ps_conformal("curech_src",ss1%curech_src,ss3%curech_src,iout,ierr)
        ss3%curech_src = ss1%curech_src
     ELSE IF(ALLOCATED(ss2%curech_src)) THEN
        CALL ps_conformal("curech_src",ss2%curech_src,ss3%curech_src,iout,ierr)
        ss3%curech_src = ss2%curech_src
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%curfusn)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%curfusn).AND.ALLOCATED(ss2%curfusn)) THEN
        CALL ps_conformal("curfusn",ss2%curfusn,ss3%curfusn,iout,ierr)
        CALL ps_conformal("curfusn",ss1%curfusn,ss3%curfusn,iout,ierr)
        if(ierr.eq.0) ss3%curfusn = w1*ss1%curfusn + (1.0_rspec-w1)*ss2%curfusn
     ELSE IF(ALLOCATED(ss1%curfusn)) THEN
        CALL ps_conformal("curfusn",ss1%curfusn,ss3%curfusn,iout,ierr)
        ss3%curfusn = ss1%curfusn
     ELSE IF(ALLOCATED(ss2%curfusn)) THEN
        CALL ps_conformal("curfusn",ss2%curfusn,ss3%curfusn,iout,ierr)
        ss3%curfusn = ss2%curfusn
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%curich)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%curich).AND.ALLOCATED(ss2%curich)) THEN
        CALL ps_conformal("curich",ss2%curich,ss3%curich,iout,ierr)
        CALL ps_conformal("curich",ss1%curich,ss3%curich,iout,ierr)
        if(ierr.eq.0) ss3%curich = w1*ss1%curich + (1.0_rspec-w1)*ss2%curich
     ELSE IF(ALLOCATED(ss1%curich)) THEN
        CALL ps_conformal("curich",ss1%curich,ss3%curich,iout,ierr)
        ss3%curich = ss1%curich
     ELSE IF(ALLOCATED(ss2%curich)) THEN
        CALL ps_conformal("curich",ss2%curich,ss3%curich,iout,ierr)
        ss3%curich = ss2%curich
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%curlh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%curlh).AND.ALLOCATED(ss2%curlh)) THEN
        CALL ps_conformal("curlh",ss2%curlh,ss3%curlh,iout,ierr)
        CALL ps_conformal("curlh",ss1%curlh,ss3%curlh,iout,ierr)
        if(ierr.eq.0) ss3%curlh = w1*ss1%curlh + (1.0_rspec-w1)*ss2%curlh
     ELSE IF(ALLOCATED(ss1%curlh)) THEN
        CALL ps_conformal("curlh",ss1%curlh,ss3%curlh,iout,ierr)
        ss3%curlh = ss1%curlh
     ELSE IF(ALLOCATED(ss2%curlh)) THEN
        CALL ps_conformal("curlh",ss2%curlh,ss3%curlh,iout,ierr)
        ss3%curlh = ss2%curlh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%curlh_src)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%curlh_src).AND.ALLOCATED(ss2%curlh_src)) THEN
        CALL ps_conformal("curlh_src",ss2%curlh_src,ss3%curlh_src,iout,ierr)
        CALL ps_conformal("curlh_src",ss1%curlh_src,ss3%curlh_src,iout,ierr)
        if(ierr.eq.0) ss3%curlh_src = w1*ss1%curlh_src + (1.0_rspec-w1)*ss2%curlh_src
     ELSE IF(ALLOCATED(ss1%curlh_src)) THEN
        CALL ps_conformal("curlh_src",ss1%curlh_src,ss3%curlh_src,iout,ierr)
        ss3%curlh_src = ss1%curlh_src
     ELSE IF(ALLOCATED(ss2%curlh_src)) THEN
        CALL ps_conformal("curlh_src",ss2%curlh_src,ss3%curlh_src,iout,ierr)
        ss3%curlh_src = ss2%curlh_src
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%curmino)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%curmino).AND.ALLOCATED(ss2%curmino)) THEN
        CALL ps_conformal("curmino",ss2%curmino,ss3%curmino,iout,ierr)
        CALL ps_conformal("curmino",ss1%curmino,ss3%curmino,iout,ierr)
        if(ierr.eq.0) ss3%curmino = w1*ss1%curmino + (1.0_rspec-w1)*ss2%curmino
     ELSE IF(ALLOCATED(ss1%curmino)) THEN
        CALL ps_conformal("curmino",ss1%curmino,ss3%curmino,iout,ierr)
        ss3%curmino = ss1%curmino
     ELSE IF(ALLOCATED(ss2%curmino)) THEN
        CALL ps_conformal("curmino",ss2%curmino,ss3%curmino,iout,ierr)
        ss3%curmino = ss2%curmino
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%curr_bootstrap)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%curr_bootstrap).AND.ALLOCATED(ss2%curr_bootstrap)) THEN
        CALL ps_conformal("curr_bootstrap",ss2%curr_bootstrap,ss3%curr_bootstrap,iout,ierr)
        CALL ps_conformal("curr_bootstrap",ss1%curr_bootstrap,ss3%curr_bootstrap,iout,ierr)
        if(ierr.eq.0) ss3%curr_bootstrap = w1*ss1%curr_bootstrap + (1.0_rspec-w1)*ss2%curr_bootstrap
     ELSE IF(ALLOCATED(ss1%curr_bootstrap)) THEN
        CALL ps_conformal("curr_bootstrap",ss1%curr_bootstrap,ss3%curr_bootstrap,iout,ierr)
        ss3%curr_bootstrap = ss1%curr_bootstrap
     ELSE IF(ALLOCATED(ss2%curr_bootstrap)) THEN
        CALL ps_conformal("curr_bootstrap",ss2%curr_bootstrap,ss3%curr_bootstrap,iout,ierr)
        ss3%curr_bootstrap = ss2%curr_bootstrap
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%curr_ohmic)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%curr_ohmic).AND.ALLOCATED(ss2%curr_ohmic)) THEN
        CALL ps_conformal("curr_ohmic",ss2%curr_ohmic,ss3%curr_ohmic,iout,ierr)
        CALL ps_conformal("curr_ohmic",ss1%curr_ohmic,ss3%curr_ohmic,iout,ierr)
        if(ierr.eq.0) ss3%curr_ohmic = w1*ss1%curr_ohmic + (1.0_rspec-w1)*ss2%curr_ohmic
     ELSE IF(ALLOCATED(ss1%curr_ohmic)) THEN
        CALL ps_conformal("curr_ohmic",ss1%curr_ohmic,ss3%curr_ohmic,iout,ierr)
        ss3%curr_ohmic = ss1%curr_ohmic
     ELSE IF(ALLOCATED(ss2%curr_ohmic)) THEN
        CALL ps_conformal("curr_ohmic",ss2%curr_ohmic,ss3%curr_ohmic,iout,ierr)
        ss3%curr_ohmic = ss2%curr_ohmic
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%curt)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%curt).AND.ALLOCATED(ss2%curt)) THEN
        CALL ps_conformal("curt",ss2%curt,ss3%curt,iout,ierr)
        CALL ps_conformal("curt",ss1%curt,ss3%curt,iout,ierr)
        if(ierr.eq.0) ss3%curt = w1*ss1%curt + (1.0_rspec-w1)*ss2%curt
     ELSE IF(ALLOCATED(ss1%curt)) THEN
        CALL ps_conformal("curt",ss1%curt,ss3%curt,iout,ierr)
        ss3%curt = ss1%curt
     ELSE IF(ALLOCATED(ss2%curt)) THEN
        CALL ps_conformal("curt",ss2%curt,ss3%curt,iout,ierr)
        ss3%curt = ss2%curt
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("CUR_Data_Info",ss1%CUR_Data_Info,ss2%CUR_Data_Info,icheck,iout,ierr)
     ss3%CUR_Data_Info = ss1%CUR_Data_Info
  ENDIF

  IF((ALLOCATED(ss3%cur_rw)).AND.(cclist(ps_cnum_RUNAWAY).EQ.1)) THEN
     IF(ALLOCATED(ss1%cur_rw).AND.ALLOCATED(ss2%cur_rw)) THEN
        CALL ps_conformal("cur_rw",ss2%cur_rw,ss3%cur_rw,iout,ierr)
        CALL ps_conformal("cur_rw",ss1%cur_rw,ss3%cur_rw,iout,ierr)
        if(ierr.eq.0) ss3%cur_rw = w1*ss1%cur_rw + (1.0_rspec-w1)*ss2%cur_rw
     ELSE IF(ALLOCATED(ss1%cur_rw)) THEN
        CALL ps_conformal("cur_rw",ss1%cur_rw,ss3%cur_rw,iout,ierr)
        ss3%cur_rw = ss1%cur_rw
     ELSE IF(ALLOCATED(ss2%cur_rw)) THEN
        CALL ps_conformal("cur_rw",ss2%cur_rw,ss3%cur_rw,iout,ierr)
        ss3%cur_rw = ss2%cur_rw
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%d2R_geo_dRHOdTH)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%d2R_geo_dRHOdTH).AND.ALLOCATED(ss2%d2R_geo_dRHOdTH)) THEN
        CALL ps_conformal("d2R_geo_dRHOdTH",ss2%d2R_geo_dRHOdTH,ss3%d2R_geo_dRHOdTH,iout,ierr)
        CALL ps_conformal("d2R_geo_dRHOdTH",ss1%d2R_geo_dRHOdTH,ss3%d2R_geo_dRHOdTH,iout,ierr)
        if(ierr.eq.0) ss3%d2R_geo_dRHOdTH = w1*ss1%d2R_geo_dRHOdTH + (1.0_rspec-w1)*ss2%d2R_geo_dRHOdTH
     ELSE IF(ALLOCATED(ss1%d2R_geo_dRHOdTH)) THEN
        CALL ps_conformal("d2R_geo_dRHOdTH",ss1%d2R_geo_dRHOdTH,ss3%d2R_geo_dRHOdTH,iout,ierr)
        ss3%d2R_geo_dRHOdTH = ss1%d2R_geo_dRHOdTH
     ELSE IF(ALLOCATED(ss2%d2R_geo_dRHOdTH)) THEN
        CALL ps_conformal("d2R_geo_dRHOdTH",ss2%d2R_geo_dRHOdTH,ss3%d2R_geo_dRHOdTH,iout,ierr)
        ss3%d2R_geo_dRHOdTH = ss2%d2R_geo_dRHOdTH
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%d2Z_geo_dRHOdTH)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%d2Z_geo_dRHOdTH).AND.ALLOCATED(ss2%d2Z_geo_dRHOdTH)) THEN
        CALL ps_conformal("d2Z_geo_dRHOdTH",ss2%d2Z_geo_dRHOdTH,ss3%d2Z_geo_dRHOdTH,iout,ierr)
        CALL ps_conformal("d2Z_geo_dRHOdTH",ss1%d2Z_geo_dRHOdTH,ss3%d2Z_geo_dRHOdTH,iout,ierr)
        if(ierr.eq.0) ss3%d2Z_geo_dRHOdTH = w1*ss1%d2Z_geo_dRHOdTH + (1.0_rspec-w1)*ss2%d2Z_geo_dRHOdTH
     ELSE IF(ALLOCATED(ss1%d2Z_geo_dRHOdTH)) THEN
        CALL ps_conformal("d2Z_geo_dRHOdTH",ss1%d2Z_geo_dRHOdTH,ss3%d2Z_geo_dRHOdTH,iout,ierr)
        ss3%d2Z_geo_dRHOdTH = ss1%d2Z_geo_dRHOdTH
     ELSE IF(ALLOCATED(ss2%d2Z_geo_dRHOdTH)) THEN
        CALL ps_conformal("d2Z_geo_dRHOdTH",ss2%d2Z_geo_dRHOdTH,ss3%d2Z_geo_dRHOdTH,iout,ierr)
        ss3%d2Z_geo_dRHOdTH = ss2%d2Z_geo_dRHOdTH
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%db_mtm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%db_mtm).AND.ALLOCATED(ss2%db_mtm)) THEN
        CALL ps_conformal("db_mtm",ss2%db_mtm,ss3%db_mtm,iout,ierr)
        CALL ps_conformal("db_mtm",ss1%db_mtm,ss3%db_mtm,iout,ierr)
        if(ierr.eq.0) ss3%db_mtm = w1*ss1%db_mtm + (1.0_rspec-w1)*ss2%db_mtm
     ELSE IF(ALLOCATED(ss1%db_mtm)) THEN
        CALL ps_conformal("db_mtm",ss1%db_mtm,ss3%db_mtm,iout,ierr)
        ss3%db_mtm = ss1%db_mtm
     ELSE IF(ALLOCATED(ss2%db_mtm)) THEN
        CALL ps_conformal("db_mtm",ss2%db_mtm,ss3%db_mtm,iout,ierr)
        ss3%db_mtm = ss2%db_mtm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dcur_rw_dVloop)).AND.(cclist(ps_cnum_RUNAWAY).EQ.1)) THEN
     IF(ALLOCATED(ss1%dcur_rw_dVloop).AND.ALLOCATED(ss2%dcur_rw_dVloop)) THEN
        CALL ps_conformal("dcur_rw_dVloop",ss2%dcur_rw_dVloop,ss3%dcur_rw_dVloop,iout,ierr)
        CALL ps_conformal("dcur_rw_dVloop",ss1%dcur_rw_dVloop,ss3%dcur_rw_dVloop,iout,ierr)
        if(ierr.eq.0) ss3%dcur_rw_dVloop = w1*ss1%dcur_rw_dVloop + (1.0_rspec-w1)*ss2%dcur_rw_dVloop
     ELSE IF(ALLOCATED(ss1%dcur_rw_dVloop)) THEN
        CALL ps_conformal("dcur_rw_dVloop",ss1%dcur_rw_dVloop,ss3%dcur_rw_dVloop,iout,ierr)
        ss3%dcur_rw_dVloop = ss1%dcur_rw_dVloop
     ELSE IF(ALLOCATED(ss2%dcur_rw_dVloop)) THEN
        CALL ps_conformal("dcur_rw_dVloop",ss2%dcur_rw_dVloop,ss3%dcur_rw_dVloop,iout,ierr)
        ss3%dcur_rw_dVloop = ss2%dcur_rw_dVloop
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%DeltaW_code)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%DeltaW_code).AND.ALLOCATED(ss2%DeltaW_code)) THEN
        CALL ps_diff("DeltaW_code",ss1%DeltaW_code,ss2%DeltaW_code,icheck,iout,ierr)
        CALL ps_conformal("DeltaW_code",ss1%DeltaW_code,ss3%DeltaW_code,iout,ierr)
        if(ierr.eq.0) ss3%DeltaW_code = ss1%DeltaW_code
     ELSE IF(ALLOCATED(ss1%DeltaW_code)) THEN
        CALL ps_conformal("DeltaW_code",ss1%DeltaW_code,ss3%DeltaW_code,iout,ierr)
        ss3%DeltaW_code = ss1%DeltaW_code
     ELSE IF(ALLOCATED(ss2%DeltaW_code)) THEN
        CALL ps_conformal("DeltaW_code",ss2%DeltaW_code,ss3%DeltaW_code,iout,ierr)
        ss3%DeltaW_code = ss2%DeltaW_code
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%DeltaW_mapper)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%DeltaW_mapper).AND.ALLOCATED(ss2%DeltaW_mapper)) THEN
        CALL ps_diff("DeltaW_mapper",ss1%DeltaW_mapper,ss2%DeltaW_mapper,icheck,iout,ierr)
        CALL ps_conformal("DeltaW_mapper",ss1%DeltaW_mapper,ss3%DeltaW_mapper,iout,ierr)
        if(ierr.eq.0) ss3%DeltaW_mapper = ss1%DeltaW_mapper
     ELSE IF(ALLOCATED(ss1%DeltaW_mapper)) THEN
        CALL ps_conformal("DeltaW_mapper",ss1%DeltaW_mapper,ss3%DeltaW_mapper,iout,ierr)
        ss3%DeltaW_mapper = ss1%DeltaW_mapper
     ELSE IF(ALLOCATED(ss2%DeltaW_mapper)) THEN
        CALL ps_conformal("DeltaW_mapper",ss2%DeltaW_mapper,ss3%DeltaW_mapper,iout,ierr)
        ss3%DeltaW_mapper = ss2%DeltaW_mapper
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%DeltaW_stable)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%DeltaW_stable).AND.ALLOCATED(ss2%DeltaW_stable)) THEN
        CALL ps_diff("DeltaW_stable",ss1%DeltaW_stable,ss2%DeltaW_stable,icheck,iout,ierr)
        CALL ps_conformal("DeltaW_stable",ss1%DeltaW_stable,ss3%DeltaW_stable,iout,ierr)
        if(ierr.eq.0) ss3%DeltaW_stable = ss1%DeltaW_stable
     ELSE IF(ALLOCATED(ss1%DeltaW_stable)) THEN
        CALL ps_conformal("DeltaW_stable",ss1%DeltaW_stable,ss3%DeltaW_stable,iout,ierr)
        ss3%DeltaW_stable = ss1%DeltaW_stable
     ELSE IF(ALLOCATED(ss2%DeltaW_stable)) THEN
        CALL ps_conformal("DeltaW_stable",ss2%DeltaW_stable,ss3%DeltaW_stable,iout,ierr)
        ss3%DeltaW_stable = ss2%DeltaW_stable
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%DeltaW_status)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%DeltaW_status).AND.ALLOCATED(ss2%DeltaW_status)) THEN
        CALL ps_diff("DeltaW_status",ss1%DeltaW_status,ss2%DeltaW_status,icheck,iout,ierr)
        CALL ps_conformal("DeltaW_status",ss1%DeltaW_status,ss3%DeltaW_status,iout,ierr)
        if(ierr.eq.0) ss3%DeltaW_status = ss1%DeltaW_status
     ELSE IF(ALLOCATED(ss1%DeltaW_status)) THEN
        CALL ps_conformal("DeltaW_status",ss1%DeltaW_status,ss3%DeltaW_status,iout,ierr)
        ss3%DeltaW_status = ss1%DeltaW_status
     ELSE IF(ALLOCATED(ss2%DeltaW_status)) THEN
        CALL ps_conformal("DeltaW_status",ss2%DeltaW_status,ss3%DeltaW_status,iout,ierr)
        ss3%DeltaW_status = ss2%DeltaW_status
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difb_fusi)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difb_fusi).AND.ALLOCATED(ss2%difb_fusi)) THEN
        CALL ps_conformal("difb_fusi",ss2%difb_fusi,ss3%difb_fusi,iout,ierr)
        CALL ps_conformal("difb_fusi",ss1%difb_fusi,ss3%difb_fusi,iout,ierr)
        if(ierr.eq.0) ss3%difb_fusi = w1*ss1%difb_fusi + (1.0_rspec-w1)*ss2%difb_fusi
     ELSE IF(ALLOCATED(ss1%difb_fusi)) THEN
        CALL ps_conformal("difb_fusi",ss1%difb_fusi,ss3%difb_fusi,iout,ierr)
        ss3%difb_fusi = ss1%difb_fusi
     ELSE IF(ALLOCATED(ss2%difb_fusi)) THEN
        CALL ps_conformal("difb_fusi",ss2%difb_fusi,ss3%difb_fusi,iout,ierr)
        ss3%difb_fusi = ss2%difb_fusi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difb_nbi)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difb_nbi).AND.ALLOCATED(ss2%difb_nbi)) THEN
        CALL ps_conformal("difb_nbi",ss2%difb_nbi,ss3%difb_nbi,iout,ierr)
        CALL ps_conformal("difb_nbi",ss1%difb_nbi,ss3%difb_nbi,iout,ierr)
        if(ierr.eq.0) ss3%difb_nbi = w1*ss1%difb_nbi + (1.0_rspec-w1)*ss2%difb_nbi
     ELSE IF(ALLOCATED(ss1%difb_nbi)) THEN
        CALL ps_conformal("difb_nbi",ss1%difb_nbi,ss3%difb_nbi,iout,ierr)
        ss3%difb_nbi = ss1%difb_nbi
     ELSE IF(ALLOCATED(ss2%difb_nbi)) THEN
        CALL ps_conformal("difb_nbi",ss2%difb_nbi,ss3%difb_nbi,iout,ierr)
        ss3%difb_nbi = ss2%difb_nbi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difb_rfmi)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difb_rfmi).AND.ALLOCATED(ss2%difb_rfmi)) THEN
        CALL ps_conformal("difb_rfmi",ss2%difb_rfmi,ss3%difb_rfmi,iout,ierr)
        CALL ps_conformal("difb_rfmi",ss1%difb_rfmi,ss3%difb_rfmi,iout,ierr)
        if(ierr.eq.0) ss3%difb_rfmi = w1*ss1%difb_rfmi + (1.0_rspec-w1)*ss2%difb_rfmi
     ELSE IF(ALLOCATED(ss1%difb_rfmi)) THEN
        CALL ps_conformal("difb_rfmi",ss1%difb_rfmi,ss3%difb_rfmi,iout,ierr)
        ss3%difb_rfmi = ss1%difb_rfmi
     ELSE IF(ALLOCATED(ss2%difb_rfmi)) THEN
        CALL ps_conformal("difb_rfmi",ss2%difb_rfmi,ss3%difb_rfmi,iout,ierr)
        ss3%difb_rfmi = ss2%difb_rfmi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%diff_trans)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%diff_trans).AND.ALLOCATED(ss2%diff_trans)) THEN
        CALL ps_conformal("diff_trans",ss2%diff_trans,ss3%diff_trans,iout,ierr)
        CALL ps_conformal("diff_trans",ss1%diff_trans,ss3%diff_trans,iout,ierr)
        if(ierr.eq.0) ss3%diff_trans = w1*ss1%diff_trans + (1.0_rspec-w1)*ss2%diff_trans
     ELSE IF(ALLOCATED(ss1%diff_trans)) THEN
        CALL ps_conformal("diff_trans",ss1%diff_trans,ss3%diff_trans,iout,ierr)
        ss3%diff_trans = ss1%diff_trans
     ELSE IF(ALLOCATED(ss2%diff_trans)) THEN
        CALL ps_conformal("diff_trans",ss2%diff_trans,ss3%diff_trans,iout,ierr)
        ss3%diff_trans = ss2%diff_trans
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%diff_transcat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%diff_transcat).AND.ALLOCATED(ss2%diff_transcat)) THEN
        CALL ps_conformal("diff_transcat",ss2%diff_transcat,ss3%diff_transcat,iout,ierr)
        CALL ps_conformal("diff_transcat",ss1%diff_transcat,ss3%diff_transcat,iout,ierr)
        if(ierr.eq.0) ss3%diff_transcat = w1*ss1%diff_transcat + (1.0_rspec-w1)*ss2%diff_transcat
     ELSE IF(ALLOCATED(ss1%diff_transcat)) THEN
        CALL ps_conformal("diff_transcat",ss1%diff_transcat,ss3%diff_transcat,iout,ierr)
        ss3%diff_transcat = ss1%diff_transcat
     ELSE IF(ALLOCATED(ss2%diff_transcat)) THEN
        CALL ps_conformal("diff_transcat",ss2%diff_transcat,ss3%diff_transcat,iout,ierr)
        ss3%diff_transcat = ss2%diff_transcat
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difne_nclass)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difne_nclass).AND.ALLOCATED(ss2%difne_nclass)) THEN
        CALL ps_conformal("difne_nclass",ss2%difne_nclass,ss3%difne_nclass,iout,ierr)
        CALL ps_conformal("difne_nclass",ss1%difne_nclass,ss3%difne_nclass,iout,ierr)
        if(ierr.eq.0) ss3%difne_nclass = w1*ss1%difne_nclass + (1.0_rspec-w1)*ss2%difne_nclass
     ELSE IF(ALLOCATED(ss1%difne_nclass)) THEN
        CALL ps_conformal("difne_nclass",ss1%difne_nclass,ss3%difne_nclass,iout,ierr)
        ss3%difne_nclass = ss1%difne_nclass
     ELSE IF(ALLOCATED(ss2%difne_nclass)) THEN
        CALL ps_conformal("difne_nclass",ss2%difne_nclass,ss3%difne_nclass,iout,ierr)
        ss3%difne_nclass = ss2%difne_nclass
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difnh_nclass)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difnh_nclass).AND.ALLOCATED(ss2%difnh_nclass)) THEN
        CALL ps_conformal("difnh_nclass",ss2%difnh_nclass,ss3%difnh_nclass,iout,ierr)
        CALL ps_conformal("difnh_nclass",ss1%difnh_nclass,ss3%difnh_nclass,iout,ierr)
        if(ierr.eq.0) ss3%difnh_nclass = w1*ss1%difnh_nclass + (1.0_rspec-w1)*ss2%difnh_nclass
     ELSE IF(ALLOCATED(ss1%difnh_nclass)) THEN
        CALL ps_conformal("difnh_nclass",ss1%difnh_nclass,ss3%difnh_nclass,iout,ierr)
        ss3%difnh_nclass = ss1%difnh_nclass
     ELSE IF(ALLOCATED(ss2%difnh_nclass)) THEN
        CALL ps_conformal("difnh_nclass",ss2%difnh_nclass,ss3%difnh_nclass,iout,ierr)
        ss3%difnh_nclass = ss2%difnh_nclass
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difns_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difns_anom).AND.ALLOCATED(ss2%difns_anom)) THEN
        CALL ps_conformal("difns_anom",ss2%difns_anom,ss3%difns_anom,iout,ierr)
        CALL ps_conformal("difns_anom",ss1%difns_anom,ss3%difns_anom,iout,ierr)
        if(ierr.eq.0) ss3%difns_anom = w1*ss1%difns_anom + (1.0_rspec-w1)*ss2%difns_anom
     ELSE IF(ALLOCATED(ss1%difns_anom)) THEN
        CALL ps_conformal("difns_anom",ss1%difns_anom,ss3%difns_anom,iout,ierr)
        ss3%difns_anom = ss1%difns_anom
     ELSE IF(ALLOCATED(ss2%difns_anom)) THEN
        CALL ps_conformal("difns_anom",ss2%difns_anom,ss3%difns_anom,iout,ierr)
        ss3%difns_anom = ss2%difns_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difns_drbm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difns_drbm).AND.ALLOCATED(ss2%difns_drbm)) THEN
        CALL ps_conformal("difns_drbm",ss2%difns_drbm,ss3%difns_drbm,iout,ierr)
        CALL ps_conformal("difns_drbm",ss1%difns_drbm,ss3%difns_drbm,iout,ierr)
        if(ierr.eq.0) ss3%difns_drbm = w1*ss1%difns_drbm + (1.0_rspec-w1)*ss2%difns_drbm
     ELSE IF(ALLOCATED(ss1%difns_drbm)) THEN
        CALL ps_conformal("difns_drbm",ss1%difns_drbm,ss3%difns_drbm,iout,ierr)
        ss3%difns_drbm = ss1%difns_drbm
     ELSE IF(ALLOCATED(ss2%difns_drbm)) THEN
        CALL ps_conformal("difns_drbm",ss2%difns_drbm,ss3%difns_drbm,iout,ierr)
        ss3%difns_drbm = ss2%difns_drbm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difns_glf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difns_glf).AND.ALLOCATED(ss2%difns_glf)) THEN
        CALL ps_conformal("difns_glf",ss2%difns_glf,ss3%difns_glf,iout,ierr)
        CALL ps_conformal("difns_glf",ss1%difns_glf,ss3%difns_glf,iout,ierr)
        if(ierr.eq.0) ss3%difns_glf = w1*ss1%difns_glf + (1.0_rspec-w1)*ss2%difns_glf
     ELSE IF(ALLOCATED(ss1%difns_glf)) THEN
        CALL ps_conformal("difns_glf",ss1%difns_glf,ss3%difns_glf,iout,ierr)
        ss3%difns_glf = ss1%difns_glf
     ELSE IF(ALLOCATED(ss2%difns_glf)) THEN
        CALL ps_conformal("difns_glf",ss2%difns_glf,ss3%difns_glf,iout,ierr)
        ss3%difns_glf = ss2%difns_glf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difns_gtcneo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difns_gtcneo).AND.ALLOCATED(ss2%difns_gtcneo)) THEN
        CALL ps_conformal("difns_gtcneo",ss2%difns_gtcneo,ss3%difns_gtcneo,iout,ierr)
        CALL ps_conformal("difns_gtcneo",ss1%difns_gtcneo,ss3%difns_gtcneo,iout,ierr)
        if(ierr.eq.0) ss3%difns_gtcneo = w1*ss1%difns_gtcneo + (1.0_rspec-w1)*ss2%difns_gtcneo
     ELSE IF(ALLOCATED(ss1%difns_gtcneo)) THEN
        CALL ps_conformal("difns_gtcneo",ss1%difns_gtcneo,ss3%difns_gtcneo,iout,ierr)
        ss3%difns_gtcneo = ss1%difns_gtcneo
     ELSE IF(ALLOCATED(ss2%difns_gtcneo)) THEN
        CALL ps_conformal("difns_gtcneo",ss2%difns_gtcneo,ss3%difns_gtcneo,iout,ierr)
        ss3%difns_gtcneo = ss2%difns_gtcneo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difns_mmm71)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difns_mmm71).AND.ALLOCATED(ss2%difns_mmm71)) THEN
        CALL ps_conformal("difns_mmm71",ss2%difns_mmm71,ss3%difns_mmm71,iout,ierr)
        CALL ps_conformal("difns_mmm71",ss1%difns_mmm71,ss3%difns_mmm71,iout,ierr)
        if(ierr.eq.0) ss3%difns_mmm71 = w1*ss1%difns_mmm71 + (1.0_rspec-w1)*ss2%difns_mmm71
     ELSE IF(ALLOCATED(ss1%difns_mmm71)) THEN
        CALL ps_conformal("difns_mmm71",ss1%difns_mmm71,ss3%difns_mmm71,iout,ierr)
        ss3%difns_mmm71 = ss1%difns_mmm71
     ELSE IF(ALLOCATED(ss2%difns_mmm71)) THEN
        CALL ps_conformal("difns_mmm71",ss2%difns_mmm71,ss3%difns_mmm71,iout,ierr)
        ss3%difns_mmm71 = ss2%difns_mmm71
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difns_nclass)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difns_nclass).AND.ALLOCATED(ss2%difns_nclass)) THEN
        CALL ps_conformal("difns_nclass",ss2%difns_nclass,ss3%difns_nclass,iout,ierr)
        CALL ps_conformal("difns_nclass",ss1%difns_nclass,ss3%difns_nclass,iout,ierr)
        if(ierr.eq.0) ss3%difns_nclass = w1*ss1%difns_nclass + (1.0_rspec-w1)*ss2%difns_nclass
     ELSE IF(ALLOCATED(ss1%difns_nclass)) THEN
        CALL ps_conformal("difns_nclass",ss1%difns_nclass,ss3%difns_nclass,iout,ierr)
        ss3%difns_nclass = ss1%difns_nclass
     ELSE IF(ALLOCATED(ss2%difns_nclass)) THEN
        CALL ps_conformal("difns_nclass",ss2%difns_nclass,ss3%difns_nclass,iout,ierr)
        ss3%difns_nclass = ss2%difns_nclass
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difns_neo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difns_neo).AND.ALLOCATED(ss2%difns_neo)) THEN
        CALL ps_conformal("difns_neo",ss2%difns_neo,ss3%difns_neo,iout,ierr)
        CALL ps_conformal("difns_neo",ss1%difns_neo,ss3%difns_neo,iout,ierr)
        if(ierr.eq.0) ss3%difns_neo = w1*ss1%difns_neo + (1.0_rspec-w1)*ss2%difns_neo
     ELSE IF(ALLOCATED(ss1%difns_neo)) THEN
        CALL ps_conformal("difns_neo",ss1%difns_neo,ss3%difns_neo,iout,ierr)
        ss3%difns_neo = ss1%difns_neo
     ELSE IF(ALLOCATED(ss2%difns_neo)) THEN
        CALL ps_conformal("difns_neo",ss2%difns_neo,ss3%difns_neo,iout,ierr)
        ss3%difns_neo = ss2%difns_neo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difns_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difns_tglf).AND.ALLOCATED(ss2%difns_tglf)) THEN
        CALL ps_conformal("difns_tglf",ss2%difns_tglf,ss3%difns_tglf,iout,ierr)
        CALL ps_conformal("difns_tglf",ss1%difns_tglf,ss3%difns_tglf,iout,ierr)
        if(ierr.eq.0) ss3%difns_tglf = w1*ss1%difns_tglf + (1.0_rspec-w1)*ss2%difns_tglf
     ELSE IF(ALLOCATED(ss1%difns_tglf)) THEN
        CALL ps_conformal("difns_tglf",ss1%difns_tglf,ss3%difns_tglf,iout,ierr)
        ss3%difns_tglf = ss1%difns_tglf
     ELSE IF(ALLOCATED(ss2%difns_tglf)) THEN
        CALL ps_conformal("difns_tglf",ss2%difns_tglf,ss3%difns_tglf,iout,ierr)
        ss3%difns_tglf = ss2%difns_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difns_w19)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difns_w19).AND.ALLOCATED(ss2%difns_w19)) THEN
        CALL ps_conformal("difns_w19",ss2%difns_w19,ss3%difns_w19,iout,ierr)
        CALL ps_conformal("difns_w19",ss1%difns_w19,ss3%difns_w19,iout,ierr)
        if(ierr.eq.0) ss3%difns_w19 = w1*ss1%difns_w19 + (1.0_rspec-w1)*ss2%difns_w19
     ELSE IF(ALLOCATED(ss1%difns_w19)) THEN
        CALL ps_conformal("difns_w19",ss1%difns_w19,ss3%difns_w19,iout,ierr)
        ss3%difns_w19 = ss1%difns_w19
     ELSE IF(ALLOCATED(ss2%difns_w19)) THEN
        CALL ps_conformal("difns_w19",ss2%difns_w19,ss3%difns_w19,iout,ierr)
        ss3%difns_w19 = ss2%difns_w19
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difnx_nclass)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difnx_nclass).AND.ALLOCATED(ss2%difnx_nclass)) THEN
        CALL ps_conformal("difnx_nclass",ss2%difnx_nclass,ss3%difnx_nclass,iout,ierr)
        CALL ps_conformal("difnx_nclass",ss1%difnx_nclass,ss3%difnx_nclass,iout,ierr)
        if(ierr.eq.0) ss3%difnx_nclass = w1*ss1%difnx_nclass + (1.0_rspec-w1)*ss2%difnx_nclass
     ELSE IF(ALLOCATED(ss1%difnx_nclass)) THEN
        CALL ps_conformal("difnx_nclass",ss1%difnx_nclass,ss3%difnx_nclass,iout,ierr)
        ss3%difnx_nclass = ss1%difnx_nclass
     ELSE IF(ALLOCATED(ss2%difnx_nclass)) THEN
        CALL ps_conformal("difnx_nclass",ss2%difnx_nclass,ss3%difnx_nclass,iout,ierr)
        ss3%difnx_nclass = ss2%difnx_nclass
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%difnz_mmm71)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%difnz_mmm71).AND.ALLOCATED(ss2%difnz_mmm71)) THEN
        CALL ps_conformal("difnz_mmm71",ss2%difnz_mmm71,ss3%difnz_mmm71,iout,ierr)
        CALL ps_conformal("difnz_mmm71",ss1%difnz_mmm71,ss3%difnz_mmm71,iout,ierr)
        if(ierr.eq.0) ss3%difnz_mmm71 = w1*ss1%difnz_mmm71 + (1.0_rspec-w1)*ss2%difnz_mmm71
     ELSE IF(ALLOCATED(ss1%difnz_mmm71)) THEN
        CALL ps_conformal("difnz_mmm71",ss1%difnz_mmm71,ss3%difnz_mmm71,iout,ierr)
        ss3%difnz_mmm71 = ss1%difnz_mmm71
     ELSE IF(ALLOCATED(ss2%difnz_mmm71)) THEN
        CALL ps_conformal("difnz_mmm71",ss2%difnz_mmm71,ss3%difnz_mmm71,iout,ierr)
        ss3%difnz_mmm71 = ss2%difnz_mmm71
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dist_fun)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%dist_fun).AND.ALLOCATED(ss2%dist_fun)) THEN
        CALL ps_diff("dist_fun",ss1%dist_fun,ss2%dist_fun,icheck,iout,ierr)
        CALL ps_conformal("dist_fun",ss1%dist_fun,ss3%dist_fun,iout,ierr)
        if(ierr.eq.0) ss3%dist_fun = ss1%dist_fun
     ELSE IF(ALLOCATED(ss1%dist_fun)) THEN
        CALL ps_conformal("dist_fun",ss1%dist_fun,ss3%dist_fun,iout,ierr)
        ss3%dist_fun = ss1%dist_fun
     ELSE IF(ALLOCATED(ss2%dist_fun)) THEN
        CALL ps_conformal("dist_fun",ss2%dist_fun,ss3%dist_fun,iout,ierr)
        ss3%dist_fun = ss2%dist_fun
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_GAS).EQ.1) THEN
     ss3%dn0out = w1*ss1%dn0out + (1.0_rspec-w1)*ss2%dn0out
  ENDIF

  IF((ALLOCATED(ss3%dQedGrdte_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%dQedGrdte_tglf).AND.ALLOCATED(ss2%dQedGrdte_tglf)) THEN
        CALL ps_conformal("dQedGrdte_tglf",ss2%dQedGrdte_tglf,ss3%dQedGrdte_tglf,iout,ierr)
        CALL ps_conformal("dQedGrdte_tglf",ss1%dQedGrdte_tglf,ss3%dQedGrdte_tglf,iout,ierr)
        if(ierr.eq.0) ss3%dQedGrdte_tglf = w1*ss1%dQedGrdte_tglf + (1.0_rspec-w1)*ss2%dQedGrdte_tglf
     ELSE IF(ALLOCATED(ss1%dQedGrdte_tglf)) THEN
        CALL ps_conformal("dQedGrdte_tglf",ss1%dQedGrdte_tglf,ss3%dQedGrdte_tglf,iout,ierr)
        ss3%dQedGrdte_tglf = ss1%dQedGrdte_tglf
     ELSE IF(ALLOCATED(ss2%dQedGrdte_tglf)) THEN
        CALL ps_conformal("dQedGrdte_tglf",ss2%dQedGrdte_tglf,ss3%dQedGrdte_tglf,iout,ierr)
        ss3%dQedGrdte_tglf = ss2%dQedGrdte_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dQedGrdti_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%dQedGrdti_tglf).AND.ALLOCATED(ss2%dQedGrdti_tglf)) THEN
        CALL ps_conformal("dQedGrdti_tglf",ss2%dQedGrdti_tglf,ss3%dQedGrdti_tglf,iout,ierr)
        CALL ps_conformal("dQedGrdti_tglf",ss1%dQedGrdti_tglf,ss3%dQedGrdti_tglf,iout,ierr)
        if(ierr.eq.0) ss3%dQedGrdti_tglf = w1*ss1%dQedGrdti_tglf + (1.0_rspec-w1)*ss2%dQedGrdti_tglf
     ELSE IF(ALLOCATED(ss1%dQedGrdti_tglf)) THEN
        CALL ps_conformal("dQedGrdti_tglf",ss1%dQedGrdti_tglf,ss3%dQedGrdti_tglf,iout,ierr)
        ss3%dQedGrdti_tglf = ss1%dQedGrdti_tglf
     ELSE IF(ALLOCATED(ss2%dQedGrdti_tglf)) THEN
        CALL ps_conformal("dQedGrdti_tglf",ss2%dQedGrdti_tglf,ss3%dQedGrdti_tglf,iout,ierr)
        ss3%dQedGrdti_tglf = ss2%dQedGrdti_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dQidGrdte_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%dQidGrdte_tglf).AND.ALLOCATED(ss2%dQidGrdte_tglf)) THEN
        CALL ps_conformal("dQidGrdte_tglf",ss2%dQidGrdte_tglf,ss3%dQidGrdte_tglf,iout,ierr)
        CALL ps_conformal("dQidGrdte_tglf",ss1%dQidGrdte_tglf,ss3%dQidGrdte_tglf,iout,ierr)
        if(ierr.eq.0) ss3%dQidGrdte_tglf = w1*ss1%dQidGrdte_tglf + (1.0_rspec-w1)*ss2%dQidGrdte_tglf
     ELSE IF(ALLOCATED(ss1%dQidGrdte_tglf)) THEN
        CALL ps_conformal("dQidGrdte_tglf",ss1%dQidGrdte_tglf,ss3%dQidGrdte_tglf,iout,ierr)
        ss3%dQidGrdte_tglf = ss1%dQidGrdte_tglf
     ELSE IF(ALLOCATED(ss2%dQidGrdte_tglf)) THEN
        CALL ps_conformal("dQidGrdte_tglf",ss2%dQidGrdte_tglf,ss3%dQidGrdte_tglf,iout,ierr)
        ss3%dQidGrdte_tglf = ss2%dQidGrdte_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dQidGrdti_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%dQidGrdti_tglf).AND.ALLOCATED(ss2%dQidGrdti_tglf)) THEN
        CALL ps_conformal("dQidGrdti_tglf",ss2%dQidGrdti_tglf,ss3%dQidGrdti_tglf,iout,ierr)
        CALL ps_conformal("dQidGrdti_tglf",ss1%dQidGrdti_tglf,ss3%dQidGrdti_tglf,iout,ierr)
        if(ierr.eq.0) ss3%dQidGrdti_tglf = w1*ss1%dQidGrdti_tglf + (1.0_rspec-w1)*ss2%dQidGrdti_tglf
     ELSE IF(ALLOCATED(ss1%dQidGrdti_tglf)) THEN
        CALL ps_conformal("dQidGrdti_tglf",ss1%dQidGrdti_tglf,ss3%dQidGrdti_tglf,iout,ierr)
        ss3%dQidGrdti_tglf = ss1%dQidGrdti_tglf
     ELSE IF(ALLOCATED(ss2%dQidGrdti_tglf)) THEN
        CALL ps_conformal("dQidGrdti_tglf",ss2%dQidGrdti_tglf,ss3%dQidGrdti_tglf,iout,ierr)
        ss3%dQidGrdti_tglf = ss2%dQidGrdti_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dR0_momeq_dRHO)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%dR0_momeq_dRHO).AND.ALLOCATED(ss2%dR0_momeq_dRHO)) THEN
        CALL ps_conformal("dR0_momeq_dRHO",ss2%dR0_momeq_dRHO,ss3%dR0_momeq_dRHO,iout,ierr)
        CALL ps_conformal("dR0_momeq_dRHO",ss1%dR0_momeq_dRHO,ss3%dR0_momeq_dRHO,iout,ierr)
        if(ierr.eq.0) ss3%dR0_momeq_dRHO = w1*ss1%dR0_momeq_dRHO + (1.0_rspec-w1)*ss2%dR0_momeq_dRHO
     ELSE IF(ALLOCATED(ss1%dR0_momeq_dRHO)) THEN
        CALL ps_conformal("dR0_momeq_dRHO",ss1%dR0_momeq_dRHO,ss3%dR0_momeq_dRHO,iout,ierr)
        ss3%dR0_momeq_dRHO = ss1%dR0_momeq_dRHO
     ELSE IF(ALLOCATED(ss2%dR0_momeq_dRHO)) THEN
        CALL ps_conformal("dR0_momeq_dRHO",ss2%dR0_momeq_dRHO,ss3%dR0_momeq_dRHO,iout,ierr)
        ss3%dR0_momeq_dRHO = ss2%dR0_momeq_dRHO
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dR_geo_dRHO)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%dR_geo_dRHO).AND.ALLOCATED(ss2%dR_geo_dRHO)) THEN
        CALL ps_conformal("dR_geo_dRHO",ss2%dR_geo_dRHO,ss3%dR_geo_dRHO,iout,ierr)
        CALL ps_conformal("dR_geo_dRHO",ss1%dR_geo_dRHO,ss3%dR_geo_dRHO,iout,ierr)
        if(ierr.eq.0) ss3%dR_geo_dRHO = w1*ss1%dR_geo_dRHO + (1.0_rspec-w1)*ss2%dR_geo_dRHO
     ELSE IF(ALLOCATED(ss1%dR_geo_dRHO)) THEN
        CALL ps_conformal("dR_geo_dRHO",ss1%dR_geo_dRHO,ss3%dR_geo_dRHO,iout,ierr)
        ss3%dR_geo_dRHO = ss1%dR_geo_dRHO
     ELSE IF(ALLOCATED(ss2%dR_geo_dRHO)) THEN
        CALL ps_conformal("dR_geo_dRHO",ss2%dR_geo_dRHO,ss3%dR_geo_dRHO,iout,ierr)
        ss3%dR_geo_dRHO = ss2%dR_geo_dRHO
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dR_geo_dTH)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%dR_geo_dTH).AND.ALLOCATED(ss2%dR_geo_dTH)) THEN
        CALL ps_conformal("dR_geo_dTH",ss2%dR_geo_dTH,ss3%dR_geo_dTH,iout,ierr)
        CALL ps_conformal("dR_geo_dTH",ss1%dR_geo_dTH,ss3%dR_geo_dTH,iout,ierr)
        if(ierr.eq.0) ss3%dR_geo_dTH = w1*ss1%dR_geo_dTH + (1.0_rspec-w1)*ss2%dR_geo_dTH
     ELSE IF(ALLOCATED(ss1%dR_geo_dTH)) THEN
        CALL ps_conformal("dR_geo_dTH",ss1%dR_geo_dTH,ss3%dR_geo_dTH,iout,ierr)
        ss3%dR_geo_dTH = ss1%dR_geo_dTH
     ELSE IF(ALLOCATED(ss2%dR_geo_dTH)) THEN
        CALL ps_conformal("dR_geo_dTH",ss2%dR_geo_dTH,ss3%dR_geo_dTH,iout,ierr)
        ss3%dR_geo_dTH = ss2%dR_geo_dTH
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dxRjcos_momeq_dRHO)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%dxRjcos_momeq_dRHO).AND.ALLOCATED(ss2%dxRjcos_momeq_dRHO)) THEN
        CALL ps_conformal("dxRjcos_momeq_dRHO",ss2%dxRjcos_momeq_dRHO,ss3%dxRjcos_momeq_dRHO,iout,ierr)
        CALL ps_conformal("dxRjcos_momeq_dRHO",ss1%dxRjcos_momeq_dRHO,ss3%dxRjcos_momeq_dRHO,iout,ierr)
        if(ierr.eq.0) ss3%dxRjcos_momeq_dRHO = w1*ss1%dxRjcos_momeq_dRHO + (1.0_rspec-w1)*ss2%dxRjcos_momeq_dRHO
     ELSE IF(ALLOCATED(ss1%dxRjcos_momeq_dRHO)) THEN
        CALL ps_conformal("dxRjcos_momeq_dRHO",ss1%dxRjcos_momeq_dRHO,ss3%dxRjcos_momeq_dRHO,iout,ierr)
        ss3%dxRjcos_momeq_dRHO = ss1%dxRjcos_momeq_dRHO
     ELSE IF(ALLOCATED(ss2%dxRjcos_momeq_dRHO)) THEN
        CALL ps_conformal("dxRjcos_momeq_dRHO",ss2%dxRjcos_momeq_dRHO,ss3%dxRjcos_momeq_dRHO,iout,ierr)
        ss3%dxRjcos_momeq_dRHO = ss2%dxRjcos_momeq_dRHO
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dxRjsin_momeq_dRHO)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%dxRjsin_momeq_dRHO).AND.ALLOCATED(ss2%dxRjsin_momeq_dRHO)) THEN
        CALL ps_conformal("dxRjsin_momeq_dRHO",ss2%dxRjsin_momeq_dRHO,ss3%dxRjsin_momeq_dRHO,iout,ierr)
        CALL ps_conformal("dxRjsin_momeq_dRHO",ss1%dxRjsin_momeq_dRHO,ss3%dxRjsin_momeq_dRHO,iout,ierr)
        if(ierr.eq.0) ss3%dxRjsin_momeq_dRHO = w1*ss1%dxRjsin_momeq_dRHO + (1.0_rspec-w1)*ss2%dxRjsin_momeq_dRHO
     ELSE IF(ALLOCATED(ss1%dxRjsin_momeq_dRHO)) THEN
        CALL ps_conformal("dxRjsin_momeq_dRHO",ss1%dxRjsin_momeq_dRHO,ss3%dxRjsin_momeq_dRHO,iout,ierr)
        ss3%dxRjsin_momeq_dRHO = ss1%dxRjsin_momeq_dRHO
     ELSE IF(ALLOCATED(ss2%dxRjsin_momeq_dRHO)) THEN
        CALL ps_conformal("dxRjsin_momeq_dRHO",ss2%dxRjsin_momeq_dRHO,ss3%dxRjsin_momeq_dRHO,iout,ierr)
        ss3%dxRjsin_momeq_dRHO = ss2%dxRjsin_momeq_dRHO
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dxZjcos_momeq_dRHO)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%dxZjcos_momeq_dRHO).AND.ALLOCATED(ss2%dxZjcos_momeq_dRHO)) THEN
        CALL ps_conformal("dxZjcos_momeq_dRHO",ss2%dxZjcos_momeq_dRHO,ss3%dxZjcos_momeq_dRHO,iout,ierr)
        CALL ps_conformal("dxZjcos_momeq_dRHO",ss1%dxZjcos_momeq_dRHO,ss3%dxZjcos_momeq_dRHO,iout,ierr)
        if(ierr.eq.0) ss3%dxZjcos_momeq_dRHO = w1*ss1%dxZjcos_momeq_dRHO + (1.0_rspec-w1)*ss2%dxZjcos_momeq_dRHO
     ELSE IF(ALLOCATED(ss1%dxZjcos_momeq_dRHO)) THEN
        CALL ps_conformal("dxZjcos_momeq_dRHO",ss1%dxZjcos_momeq_dRHO,ss3%dxZjcos_momeq_dRHO,iout,ierr)
        ss3%dxZjcos_momeq_dRHO = ss1%dxZjcos_momeq_dRHO
     ELSE IF(ALLOCATED(ss2%dxZjcos_momeq_dRHO)) THEN
        CALL ps_conformal("dxZjcos_momeq_dRHO",ss2%dxZjcos_momeq_dRHO,ss3%dxZjcos_momeq_dRHO,iout,ierr)
        ss3%dxZjcos_momeq_dRHO = ss2%dxZjcos_momeq_dRHO
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dxZjsin_momeq_dRHO)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%dxZjsin_momeq_dRHO).AND.ALLOCATED(ss2%dxZjsin_momeq_dRHO)) THEN
        CALL ps_conformal("dxZjsin_momeq_dRHO",ss2%dxZjsin_momeq_dRHO,ss3%dxZjsin_momeq_dRHO,iout,ierr)
        CALL ps_conformal("dxZjsin_momeq_dRHO",ss1%dxZjsin_momeq_dRHO,ss3%dxZjsin_momeq_dRHO,iout,ierr)
        if(ierr.eq.0) ss3%dxZjsin_momeq_dRHO = w1*ss1%dxZjsin_momeq_dRHO + (1.0_rspec-w1)*ss2%dxZjsin_momeq_dRHO
     ELSE IF(ALLOCATED(ss1%dxZjsin_momeq_dRHO)) THEN
        CALL ps_conformal("dxZjsin_momeq_dRHO",ss1%dxZjsin_momeq_dRHO,ss3%dxZjsin_momeq_dRHO,iout,ierr)
        ss3%dxZjsin_momeq_dRHO = ss1%dxZjsin_momeq_dRHO
     ELSE IF(ALLOCATED(ss2%dxZjsin_momeq_dRHO)) THEN
        CALL ps_conformal("dxZjsin_momeq_dRHO",ss2%dxZjsin_momeq_dRHO,ss3%dxZjsin_momeq_dRHO,iout,ierr)
        ss3%dxZjsin_momeq_dRHO = ss2%dxZjsin_momeq_dRHO
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dx_fshield)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%dx_fshield).AND.ALLOCATED(ss2%dx_fshield)) THEN
        CALL ps_diff("dx_fshield",ss1%dx_fshield,ss2%dx_fshield,icheck,iout,ierr)
        CALL ps_conformal("dx_fshield",ss1%dx_fshield,ss3%dx_fshield,iout,ierr)
        if(ierr.eq.0) ss3%dx_fshield = ss1%dx_fshield
     ELSE IF(ALLOCATED(ss1%dx_fshield)) THEN
        CALL ps_conformal("dx_fshield",ss1%dx_fshield,ss3%dx_fshield,iout,ierr)
        ss3%dx_fshield = ss1%dx_fshield
     ELSE IF(ALLOCATED(ss2%dx_fshield)) THEN
        CALL ps_conformal("dx_fshield",ss2%dx_fshield,ss3%dx_fshield,iout,ierr)
        ss3%dx_fshield = ss2%dx_fshield
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dZ0_momeq_dRHO)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%dZ0_momeq_dRHO).AND.ALLOCATED(ss2%dZ0_momeq_dRHO)) THEN
        CALL ps_conformal("dZ0_momeq_dRHO",ss2%dZ0_momeq_dRHO,ss3%dZ0_momeq_dRHO,iout,ierr)
        CALL ps_conformal("dZ0_momeq_dRHO",ss1%dZ0_momeq_dRHO,ss3%dZ0_momeq_dRHO,iout,ierr)
        if(ierr.eq.0) ss3%dZ0_momeq_dRHO = w1*ss1%dZ0_momeq_dRHO + (1.0_rspec-w1)*ss2%dZ0_momeq_dRHO
     ELSE IF(ALLOCATED(ss1%dZ0_momeq_dRHO)) THEN
        CALL ps_conformal("dZ0_momeq_dRHO",ss1%dZ0_momeq_dRHO,ss3%dZ0_momeq_dRHO,iout,ierr)
        ss3%dZ0_momeq_dRHO = ss1%dZ0_momeq_dRHO
     ELSE IF(ALLOCATED(ss2%dZ0_momeq_dRHO)) THEN
        CALL ps_conformal("dZ0_momeq_dRHO",ss2%dZ0_momeq_dRHO,ss3%dZ0_momeq_dRHO,iout,ierr)
        ss3%dZ0_momeq_dRHO = ss2%dZ0_momeq_dRHO
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dZ_geo_dRHO)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%dZ_geo_dRHO).AND.ALLOCATED(ss2%dZ_geo_dRHO)) THEN
        CALL ps_conformal("dZ_geo_dRHO",ss2%dZ_geo_dRHO,ss3%dZ_geo_dRHO,iout,ierr)
        CALL ps_conformal("dZ_geo_dRHO",ss1%dZ_geo_dRHO,ss3%dZ_geo_dRHO,iout,ierr)
        if(ierr.eq.0) ss3%dZ_geo_dRHO = w1*ss1%dZ_geo_dRHO + (1.0_rspec-w1)*ss2%dZ_geo_dRHO
     ELSE IF(ALLOCATED(ss1%dZ_geo_dRHO)) THEN
        CALL ps_conformal("dZ_geo_dRHO",ss1%dZ_geo_dRHO,ss3%dZ_geo_dRHO,iout,ierr)
        ss3%dZ_geo_dRHO = ss1%dZ_geo_dRHO
     ELSE IF(ALLOCATED(ss2%dZ_geo_dRHO)) THEN
        CALL ps_conformal("dZ_geo_dRHO",ss2%dZ_geo_dRHO,ss3%dZ_geo_dRHO,iout,ierr)
        ss3%dZ_geo_dRHO = ss2%dZ_geo_dRHO
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%dZ_geo_dTH)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%dZ_geo_dTH).AND.ALLOCATED(ss2%dZ_geo_dTH)) THEN
        CALL ps_conformal("dZ_geo_dTH",ss2%dZ_geo_dTH,ss3%dZ_geo_dTH,iout,ierr)
        CALL ps_conformal("dZ_geo_dTH",ss1%dZ_geo_dTH,ss3%dZ_geo_dTH,iout,ierr)
        if(ierr.eq.0) ss3%dZ_geo_dTH = w1*ss1%dZ_geo_dTH + (1.0_rspec-w1)*ss2%dZ_geo_dTH
     ELSE IF(ALLOCATED(ss1%dZ_geo_dTH)) THEN
        CALL ps_conformal("dZ_geo_dTH",ss1%dZ_geo_dTH,ss3%dZ_geo_dTH,iout,ierr)
        ss3%dZ_geo_dTH = ss1%dZ_geo_dTH
     ELSE IF(ALLOCATED(ss2%dZ_geo_dTH)) THEN
        CALL ps_conformal("dZ_geo_dTH",ss2%dZ_geo_dTH,ss3%dZ_geo_dTH,iout,ierr)
        ss3%dZ_geo_dTH = ss2%dZ_geo_dTH
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_merge_1
