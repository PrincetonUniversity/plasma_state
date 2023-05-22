! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_merge_9(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)


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

  IF((ALLOCATED(ss3%velpphi_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velpphi_tglf).AND.ALLOCATED(ss2%velpphi_tglf)) THEN
        CALL ps_conformal("velpphi_tglf",ss2%velpphi_tglf,ss3%velpphi_tglf,iout,ierr)
        CALL ps_conformal("velpphi_tglf",ss1%velpphi_tglf,ss3%velpphi_tglf,iout,ierr)
        if(ierr.eq.0) ss3%velpphi_tglf = w1*ss1%velpphi_tglf + (1.0_rspec-w1)*ss2%velpphi_tglf
     ELSE IF(ALLOCATED(ss1%velpphi_tglf)) THEN
        CALL ps_conformal("velpphi_tglf",ss1%velpphi_tglf,ss3%velpphi_tglf,iout,ierr)
        ss3%velpphi_tglf = ss1%velpphi_tglf
     ELSE IF(ALLOCATED(ss2%velpphi_tglf)) THEN
        CALL ps_conformal("velpphi_tglf",ss2%velpphi_tglf,ss3%velpphi_tglf,iout,ierr)
        ss3%velpphi_tglf = ss2%velpphi_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velte_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velte_anom).AND.ALLOCATED(ss2%velte_anom)) THEN
        CALL ps_conformal("velte_anom",ss2%velte_anom,ss3%velte_anom,iout,ierr)
        CALL ps_conformal("velte_anom",ss1%velte_anom,ss3%velte_anom,iout,ierr)
        if(ierr.eq.0) ss3%velte_anom = w1*ss1%velte_anom + (1.0_rspec-w1)*ss2%velte_anom
     ELSE IF(ALLOCATED(ss1%velte_anom)) THEN
        CALL ps_conformal("velte_anom",ss1%velte_anom,ss3%velte_anom,iout,ierr)
        ss3%velte_anom = ss1%velte_anom
     ELSE IF(ALLOCATED(ss2%velte_anom)) THEN
        CALL ps_conformal("velte_anom",ss2%velte_anom,ss3%velte_anom,iout,ierr)
        ss3%velte_anom = ss2%velte_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velte_glf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velte_glf).AND.ALLOCATED(ss2%velte_glf)) THEN
        CALL ps_conformal("velte_glf",ss2%velte_glf,ss3%velte_glf,iout,ierr)
        CALL ps_conformal("velte_glf",ss1%velte_glf,ss3%velte_glf,iout,ierr)
        if(ierr.eq.0) ss3%velte_glf = w1*ss1%velte_glf + (1.0_rspec-w1)*ss2%velte_glf
     ELSE IF(ALLOCATED(ss1%velte_glf)) THEN
        CALL ps_conformal("velte_glf",ss1%velte_glf,ss3%velte_glf,iout,ierr)
        ss3%velte_glf = ss1%velte_glf
     ELSE IF(ALLOCATED(ss2%velte_glf)) THEN
        CALL ps_conformal("velte_glf",ss2%velte_glf,ss3%velte_glf,iout,ierr)
        ss3%velte_glf = ss2%velte_glf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velte_gtcneo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velte_gtcneo).AND.ALLOCATED(ss2%velte_gtcneo)) THEN
        CALL ps_conformal("velte_gtcneo",ss2%velte_gtcneo,ss3%velte_gtcneo,iout,ierr)
        CALL ps_conformal("velte_gtcneo",ss1%velte_gtcneo,ss3%velte_gtcneo,iout,ierr)
        if(ierr.eq.0) ss3%velte_gtcneo = w1*ss1%velte_gtcneo + (1.0_rspec-w1)*ss2%velte_gtcneo
     ELSE IF(ALLOCATED(ss1%velte_gtcneo)) THEN
        CALL ps_conformal("velte_gtcneo",ss1%velte_gtcneo,ss3%velte_gtcneo,iout,ierr)
        ss3%velte_gtcneo = ss1%velte_gtcneo
     ELSE IF(ALLOCATED(ss2%velte_gtcneo)) THEN
        CALL ps_conformal("velte_gtcneo",ss2%velte_gtcneo,ss3%velte_gtcneo,iout,ierr)
        ss3%velte_gtcneo = ss2%velte_gtcneo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velte_mmm71)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velte_mmm71).AND.ALLOCATED(ss2%velte_mmm71)) THEN
        CALL ps_conformal("velte_mmm71",ss2%velte_mmm71,ss3%velte_mmm71,iout,ierr)
        CALL ps_conformal("velte_mmm71",ss1%velte_mmm71,ss3%velte_mmm71,iout,ierr)
        if(ierr.eq.0) ss3%velte_mmm71 = w1*ss1%velte_mmm71 + (1.0_rspec-w1)*ss2%velte_mmm71
     ELSE IF(ALLOCATED(ss1%velte_mmm71)) THEN
        CALL ps_conformal("velte_mmm71",ss1%velte_mmm71,ss3%velte_mmm71,iout,ierr)
        ss3%velte_mmm71 = ss1%velte_mmm71
     ELSE IF(ALLOCATED(ss2%velte_mmm71)) THEN
        CALL ps_conformal("velte_mmm71",ss2%velte_mmm71,ss3%velte_mmm71,iout,ierr)
        ss3%velte_mmm71 = ss2%velte_mmm71
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velte_nclass)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velte_nclass).AND.ALLOCATED(ss2%velte_nclass)) THEN
        CALL ps_conformal("velte_nclass",ss2%velte_nclass,ss3%velte_nclass,iout,ierr)
        CALL ps_conformal("velte_nclass",ss1%velte_nclass,ss3%velte_nclass,iout,ierr)
        if(ierr.eq.0) ss3%velte_nclass = w1*ss1%velte_nclass + (1.0_rspec-w1)*ss2%velte_nclass
     ELSE IF(ALLOCATED(ss1%velte_nclass)) THEN
        CALL ps_conformal("velte_nclass",ss1%velte_nclass,ss3%velte_nclass,iout,ierr)
        ss3%velte_nclass = ss1%velte_nclass
     ELSE IF(ALLOCATED(ss2%velte_nclass)) THEN
        CALL ps_conformal("velte_nclass",ss2%velte_nclass,ss3%velte_nclass,iout,ierr)
        ss3%velte_nclass = ss2%velte_nclass
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velte_neo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velte_neo).AND.ALLOCATED(ss2%velte_neo)) THEN
        CALL ps_conformal("velte_neo",ss2%velte_neo,ss3%velte_neo,iout,ierr)
        CALL ps_conformal("velte_neo",ss1%velte_neo,ss3%velte_neo,iout,ierr)
        if(ierr.eq.0) ss3%velte_neo = w1*ss1%velte_neo + (1.0_rspec-w1)*ss2%velte_neo
     ELSE IF(ALLOCATED(ss1%velte_neo)) THEN
        CALL ps_conformal("velte_neo",ss1%velte_neo,ss3%velte_neo,iout,ierr)
        ss3%velte_neo = ss1%velte_neo
     ELSE IF(ALLOCATED(ss2%velte_neo)) THEN
        CALL ps_conformal("velte_neo",ss2%velte_neo,ss3%velte_neo,iout,ierr)
        ss3%velte_neo = ss2%velte_neo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velte_paleo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velte_paleo).AND.ALLOCATED(ss2%velte_paleo)) THEN
        CALL ps_conformal("velte_paleo",ss2%velte_paleo,ss3%velte_paleo,iout,ierr)
        CALL ps_conformal("velte_paleo",ss1%velte_paleo,ss3%velte_paleo,iout,ierr)
        if(ierr.eq.0) ss3%velte_paleo = w1*ss1%velte_paleo + (1.0_rspec-w1)*ss2%velte_paleo
     ELSE IF(ALLOCATED(ss1%velte_paleo)) THEN
        CALL ps_conformal("velte_paleo",ss1%velte_paleo,ss3%velte_paleo,iout,ierr)
        ss3%velte_paleo = ss1%velte_paleo
     ELSE IF(ALLOCATED(ss2%velte_paleo)) THEN
        CALL ps_conformal("velte_paleo",ss2%velte_paleo,ss3%velte_paleo,iout,ierr)
        ss3%velte_paleo = ss2%velte_paleo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velte_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velte_tglf).AND.ALLOCATED(ss2%velte_tglf)) THEN
        CALL ps_conformal("velte_tglf",ss2%velte_tglf,ss3%velte_tglf,iout,ierr)
        CALL ps_conformal("velte_tglf",ss1%velte_tglf,ss3%velte_tglf,iout,ierr)
        if(ierr.eq.0) ss3%velte_tglf = w1*ss1%velte_tglf + (1.0_rspec-w1)*ss2%velte_tglf
     ELSE IF(ALLOCATED(ss1%velte_tglf)) THEN
        CALL ps_conformal("velte_tglf",ss1%velte_tglf,ss3%velte_tglf,iout,ierr)
        ss3%velte_tglf = ss1%velte_tglf
     ELSE IF(ALLOCATED(ss2%velte_tglf)) THEN
        CALL ps_conformal("velte_tglf",ss2%velte_tglf,ss3%velte_tglf,iout,ierr)
        ss3%velte_tglf = ss2%velte_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velti_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velti_anom).AND.ALLOCATED(ss2%velti_anom)) THEN
        CALL ps_conformal("velti_anom",ss2%velti_anom,ss3%velti_anom,iout,ierr)
        CALL ps_conformal("velti_anom",ss1%velti_anom,ss3%velti_anom,iout,ierr)
        if(ierr.eq.0) ss3%velti_anom = w1*ss1%velti_anom + (1.0_rspec-w1)*ss2%velti_anom
     ELSE IF(ALLOCATED(ss1%velti_anom)) THEN
        CALL ps_conformal("velti_anom",ss1%velti_anom,ss3%velti_anom,iout,ierr)
        ss3%velti_anom = ss1%velti_anom
     ELSE IF(ALLOCATED(ss2%velti_anom)) THEN
        CALL ps_conformal("velti_anom",ss2%velti_anom,ss3%velti_anom,iout,ierr)
        ss3%velti_anom = ss2%velti_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velti_glf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velti_glf).AND.ALLOCATED(ss2%velti_glf)) THEN
        CALL ps_conformal("velti_glf",ss2%velti_glf,ss3%velti_glf,iout,ierr)
        CALL ps_conformal("velti_glf",ss1%velti_glf,ss3%velti_glf,iout,ierr)
        if(ierr.eq.0) ss3%velti_glf = w1*ss1%velti_glf + (1.0_rspec-w1)*ss2%velti_glf
     ELSE IF(ALLOCATED(ss1%velti_glf)) THEN
        CALL ps_conformal("velti_glf",ss1%velti_glf,ss3%velti_glf,iout,ierr)
        ss3%velti_glf = ss1%velti_glf
     ELSE IF(ALLOCATED(ss2%velti_glf)) THEN
        CALL ps_conformal("velti_glf",ss2%velti_glf,ss3%velti_glf,iout,ierr)
        ss3%velti_glf = ss2%velti_glf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velti_gtcneo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velti_gtcneo).AND.ALLOCATED(ss2%velti_gtcneo)) THEN
        CALL ps_conformal("velti_gtcneo",ss2%velti_gtcneo,ss3%velti_gtcneo,iout,ierr)
        CALL ps_conformal("velti_gtcneo",ss1%velti_gtcneo,ss3%velti_gtcneo,iout,ierr)
        if(ierr.eq.0) ss3%velti_gtcneo = w1*ss1%velti_gtcneo + (1.0_rspec-w1)*ss2%velti_gtcneo
     ELSE IF(ALLOCATED(ss1%velti_gtcneo)) THEN
        CALL ps_conformal("velti_gtcneo",ss1%velti_gtcneo,ss3%velti_gtcneo,iout,ierr)
        ss3%velti_gtcneo = ss1%velti_gtcneo
     ELSE IF(ALLOCATED(ss2%velti_gtcneo)) THEN
        CALL ps_conformal("velti_gtcneo",ss2%velti_gtcneo,ss3%velti_gtcneo,iout,ierr)
        ss3%velti_gtcneo = ss2%velti_gtcneo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velti_mmm71)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velti_mmm71).AND.ALLOCATED(ss2%velti_mmm71)) THEN
        CALL ps_conformal("velti_mmm71",ss2%velti_mmm71,ss3%velti_mmm71,iout,ierr)
        CALL ps_conformal("velti_mmm71",ss1%velti_mmm71,ss3%velti_mmm71,iout,ierr)
        if(ierr.eq.0) ss3%velti_mmm71 = w1*ss1%velti_mmm71 + (1.0_rspec-w1)*ss2%velti_mmm71
     ELSE IF(ALLOCATED(ss1%velti_mmm71)) THEN
        CALL ps_conformal("velti_mmm71",ss1%velti_mmm71,ss3%velti_mmm71,iout,ierr)
        ss3%velti_mmm71 = ss1%velti_mmm71
     ELSE IF(ALLOCATED(ss2%velti_mmm71)) THEN
        CALL ps_conformal("velti_mmm71",ss2%velti_mmm71,ss3%velti_mmm71,iout,ierr)
        ss3%velti_mmm71 = ss2%velti_mmm71
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velti_nclass)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velti_nclass).AND.ALLOCATED(ss2%velti_nclass)) THEN
        CALL ps_conformal("velti_nclass",ss2%velti_nclass,ss3%velti_nclass,iout,ierr)
        CALL ps_conformal("velti_nclass",ss1%velti_nclass,ss3%velti_nclass,iout,ierr)
        if(ierr.eq.0) ss3%velti_nclass = w1*ss1%velti_nclass + (1.0_rspec-w1)*ss2%velti_nclass
     ELSE IF(ALLOCATED(ss1%velti_nclass)) THEN
        CALL ps_conformal("velti_nclass",ss1%velti_nclass,ss3%velti_nclass,iout,ierr)
        ss3%velti_nclass = ss1%velti_nclass
     ELSE IF(ALLOCATED(ss2%velti_nclass)) THEN
        CALL ps_conformal("velti_nclass",ss2%velti_nclass,ss3%velti_nclass,iout,ierr)
        ss3%velti_nclass = ss2%velti_nclass
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velti_neo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velti_neo).AND.ALLOCATED(ss2%velti_neo)) THEN
        CALL ps_conformal("velti_neo",ss2%velti_neo,ss3%velti_neo,iout,ierr)
        CALL ps_conformal("velti_neo",ss1%velti_neo,ss3%velti_neo,iout,ierr)
        if(ierr.eq.0) ss3%velti_neo = w1*ss1%velti_neo + (1.0_rspec-w1)*ss2%velti_neo
     ELSE IF(ALLOCATED(ss1%velti_neo)) THEN
        CALL ps_conformal("velti_neo",ss1%velti_neo,ss3%velti_neo,iout,ierr)
        ss3%velti_neo = ss1%velti_neo
     ELSE IF(ALLOCATED(ss2%velti_neo)) THEN
        CALL ps_conformal("velti_neo",ss2%velti_neo,ss3%velti_neo,iout,ierr)
        ss3%velti_neo = ss2%velti_neo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velti_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velti_tglf).AND.ALLOCATED(ss2%velti_tglf)) THEN
        CALL ps_conformal("velti_tglf",ss2%velti_tglf,ss3%velti_tglf,iout,ierr)
        CALL ps_conformal("velti_tglf",ss1%velti_tglf,ss3%velti_tglf,iout,ierr)
        if(ierr.eq.0) ss3%velti_tglf = w1*ss1%velti_tglf + (1.0_rspec-w1)*ss2%velti_tglf
     ELSE IF(ALLOCATED(ss1%velti_tglf)) THEN
        CALL ps_conformal("velti_tglf",ss1%velti_tglf,ss3%velti_tglf,iout,ierr)
        ss3%velti_tglf = ss1%velti_tglf
     ELSE IF(ALLOCATED(ss2%velti_tglf)) THEN
        CALL ps_conformal("velti_tglf",ss2%velti_tglf,ss3%velti_tglf,iout,ierr)
        ss3%velti_tglf = ss2%velti_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vie_trans)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vie_trans).AND.ALLOCATED(ss2%vie_trans)) THEN
        CALL ps_conformal("vie_trans",ss2%vie_trans,ss3%vie_trans,iout,ierr)
        CALL ps_conformal("vie_trans",ss1%vie_trans,ss3%vie_trans,iout,ierr)
        if(ierr.eq.0) ss3%vie_trans = w1*ss1%vie_trans + (1.0_rspec-w1)*ss2%vie_trans
     ELSE IF(ALLOCATED(ss1%vie_trans)) THEN
        CALL ps_conformal("vie_trans",ss1%vie_trans,ss3%vie_trans,iout,ierr)
        ss3%vie_trans = ss1%vie_trans
     ELSE IF(ALLOCATED(ss2%vie_trans)) THEN
        CALL ps_conformal("vie_trans",ss2%vie_trans,ss3%vie_trans,iout,ierr)
        ss3%vie_trans = ss2%vie_trans
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vie_transcat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vie_transcat).AND.ALLOCATED(ss2%vie_transcat)) THEN
        CALL ps_conformal("vie_transcat",ss2%vie_transcat,ss3%vie_transcat,iout,ierr)
        CALL ps_conformal("vie_transcat",ss1%vie_transcat,ss3%vie_transcat,iout,ierr)
        if(ierr.eq.0) ss3%vie_transcat = w1*ss1%vie_transcat + (1.0_rspec-w1)*ss2%vie_transcat
     ELSE IF(ALLOCATED(ss1%vie_transcat)) THEN
        CALL ps_conformal("vie_transcat",ss1%vie_transcat,ss3%vie_transcat,iout,ierr)
        ss3%vie_transcat = ss1%vie_transcat
     ELSE IF(ALLOCATED(ss2%vie_transcat)) THEN
        CALL ps_conformal("vie_transcat",ss2%vie_transcat,ss3%vie_transcat,iout,ierr)
        ss3%vie_transcat = ss2%vie_transcat
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vmo_trans)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vmo_trans).AND.ALLOCATED(ss2%vmo_trans)) THEN
        CALL ps_conformal("vmo_trans",ss2%vmo_trans,ss3%vmo_trans,iout,ierr)
        CALL ps_conformal("vmo_trans",ss1%vmo_trans,ss3%vmo_trans,iout,ierr)
        if(ierr.eq.0) ss3%vmo_trans = w1*ss1%vmo_trans + (1.0_rspec-w1)*ss2%vmo_trans
     ELSE IF(ALLOCATED(ss1%vmo_trans)) THEN
        CALL ps_conformal("vmo_trans",ss1%vmo_trans,ss3%vmo_trans,iout,ierr)
        ss3%vmo_trans = ss1%vmo_trans
     ELSE IF(ALLOCATED(ss2%vmo_trans)) THEN
        CALL ps_conformal("vmo_trans",ss2%vmo_trans,ss3%vmo_trans,iout,ierr)
        ss3%vmo_trans = ss2%vmo_trans
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vmo_transcat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vmo_transcat).AND.ALLOCATED(ss2%vmo_transcat)) THEN
        CALL ps_conformal("vmo_transcat",ss2%vmo_transcat,ss3%vmo_transcat,iout,ierr)
        CALL ps_conformal("vmo_transcat",ss1%vmo_transcat,ss3%vmo_transcat,iout,ierr)
        if(ierr.eq.0) ss3%vmo_transcat = w1*ss1%vmo_transcat + (1.0_rspec-w1)*ss2%vmo_transcat
     ELSE IF(ALLOCATED(ss1%vmo_transcat)) THEN
        CALL ps_conformal("vmo_transcat",ss1%vmo_transcat,ss3%vmo_transcat,iout,ierr)
        ss3%vmo_transcat = ss1%vmo_transcat
     ELSE IF(ALLOCATED(ss2%vmo_transcat)) THEN
        CALL ps_conformal("vmo_transcat",ss2%vmo_transcat,ss3%vmo_transcat,iout,ierr)
        ss3%vmo_transcat = ss2%vmo_transcat
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vn_trans)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vn_trans).AND.ALLOCATED(ss2%vn_trans)) THEN
        CALL ps_conformal("vn_trans",ss2%vn_trans,ss3%vn_trans,iout,ierr)
        CALL ps_conformal("vn_trans",ss1%vn_trans,ss3%vn_trans,iout,ierr)
        if(ierr.eq.0) ss3%vn_trans = w1*ss1%vn_trans + (1.0_rspec-w1)*ss2%vn_trans
     ELSE IF(ALLOCATED(ss1%vn_trans)) THEN
        CALL ps_conformal("vn_trans",ss1%vn_trans,ss3%vn_trans,iout,ierr)
        ss3%vn_trans = ss1%vn_trans
     ELSE IF(ALLOCATED(ss2%vn_trans)) THEN
        CALL ps_conformal("vn_trans",ss2%vn_trans,ss3%vn_trans,iout,ierr)
        ss3%vn_trans = ss2%vn_trans
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vn_transcat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vn_transcat).AND.ALLOCATED(ss2%vn_transcat)) THEN
        CALL ps_conformal("vn_transcat",ss2%vn_transcat,ss3%vn_transcat,iout,ierr)
        CALL ps_conformal("vn_transcat",ss1%vn_transcat,ss3%vn_transcat,iout,ierr)
        if(ierr.eq.0) ss3%vn_transcat = w1*ss1%vn_transcat + (1.0_rspec-w1)*ss2%vn_transcat
     ELSE IF(ALLOCATED(ss1%vn_transcat)) THEN
        CALL ps_conformal("vn_transcat",ss1%vn_transcat,ss3%vn_transcat,iout,ierr)
        ss3%vn_transcat = ss1%vn_transcat
     ELSE IF(ALLOCATED(ss2%vn_transcat)) THEN
        CALL ps_conformal("vn_transcat",ss2%vn_transcat,ss3%vn_transcat,iout,ierr)
        ss3%vn_transcat = ss2%vn_transcat
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vol)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%vol).AND.ALLOCATED(ss2%vol)) THEN
        CALL ps_conformal("vol",ss2%vol,ss3%vol,iout,ierr)
        CALL ps_conformal("vol",ss1%vol,ss3%vol,iout,ierr)
        if(ierr.eq.0) ss3%vol = w1*ss1%vol + (1.0_rspec-w1)*ss2%vol
     ELSE IF(ALLOCATED(ss1%vol)) THEN
        CALL ps_conformal("vol",ss1%vol,ss3%vol,iout,ierr)
        ss3%vol = ss1%vol
     ELSE IF(ALLOCATED(ss2%vol)) THEN
        CALL ps_conformal("vol",ss2%vol,ss3%vol,iout,ierr)
        ss3%vol = ss2%vol
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vphi0_av)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%vphi0_av).AND.ALLOCATED(ss2%vphi0_av)) THEN
        CALL ps_conformal("vphi0_av",ss2%vphi0_av,ss3%vphi0_av,iout,ierr)
        CALL ps_conformal("vphi0_av",ss1%vphi0_av,ss3%vphi0_av,iout,ierr)
        if(ierr.eq.0) ss3%vphi0_av = w1*ss1%vphi0_av + (1.0_rspec-w1)*ss2%vphi0_av
     ELSE IF(ALLOCATED(ss1%vphi0_av)) THEN
        CALL ps_conformal("vphi0_av",ss1%vphi0_av,ss3%vphi0_av,iout,ierr)
        ss3%vphi0_av = ss1%vphi0_av
     ELSE IF(ALLOCATED(ss2%vphi0_av)) THEN
        CALL ps_conformal("vphi0_av",ss2%vphi0_av,ss3%vphi0_av,iout,ierr)
        ss3%vphi0_av = ss2%vphi0_av
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("vpol_Data_Info",ss1%vpol_Data_Info,ss2%vpol_Data_Info,icheck,iout,ierr)
     ss3%vpol_Data_Info = ss1%vpol_Data_Info
  ENDIF

  IF((ALLOCATED(ss3%vpol_inmp)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vpol_inmp).AND.ALLOCATED(ss2%vpol_inmp)) THEN
        CALL ps_conformal("vpol_inmp",ss2%vpol_inmp,ss3%vpol_inmp,iout,ierr)
        CALL ps_conformal("vpol_inmp",ss1%vpol_inmp,ss3%vpol_inmp,iout,ierr)
        if(ierr.eq.0) ss3%vpol_inmp = w1*ss1%vpol_inmp + (1.0_rspec-w1)*ss2%vpol_inmp
     ELSE IF(ALLOCATED(ss1%vpol_inmp)) THEN
        CALL ps_conformal("vpol_inmp",ss1%vpol_inmp,ss3%vpol_inmp,iout,ierr)
        ss3%vpol_inmp = ss1%vpol_inmp
     ELSE IF(ALLOCATED(ss2%vpol_inmp)) THEN
        CALL ps_conformal("vpol_inmp",ss2%vpol_inmp,ss3%vpol_inmp,iout,ierr)
        ss3%vpol_inmp = ss2%vpol_inmp
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vpol_is_input)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vpol_is_input).AND.ALLOCATED(ss2%vpol_is_input)) THEN
        CALL ps_diff("vpol_is_input",ss1%vpol_is_input,ss2%vpol_is_input,icheck,iout,ierr)
        CALL ps_conformal("vpol_is_input",ss1%vpol_is_input,ss3%vpol_is_input,iout,ierr)
        if(ierr.eq.0) ss3%vpol_is_input = ss1%vpol_is_input
     ELSE IF(ALLOCATED(ss1%vpol_is_input)) THEN
        CALL ps_conformal("vpol_is_input",ss1%vpol_is_input,ss3%vpol_is_input,iout,ierr)
        ss3%vpol_is_input = ss1%vpol_is_input
     ELSE IF(ALLOCATED(ss2%vpol_is_input)) THEN
        CALL ps_conformal("vpol_is_input",ss2%vpol_is_input,ss3%vpol_is_input,iout,ierr)
        ss3%vpol_is_input = ss2%vpol_is_input
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vpol_omp)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vpol_omp).AND.ALLOCATED(ss2%vpol_omp)) THEN
        CALL ps_conformal("vpol_omp",ss2%vpol_omp,ss3%vpol_omp,iout,ierr)
        CALL ps_conformal("vpol_omp",ss1%vpol_omp,ss3%vpol_omp,iout,ierr)
        if(ierr.eq.0) ss3%vpol_omp = w1*ss1%vpol_omp + (1.0_rspec-w1)*ss2%vpol_omp
     ELSE IF(ALLOCATED(ss1%vpol_omp)) THEN
        CALL ps_conformal("vpol_omp",ss1%vpol_omp,ss3%vpol_omp,iout,ierr)
        ss3%vpol_omp = ss1%vpol_omp
     ELSE IF(ALLOCATED(ss2%vpol_omp)) THEN
        CALL ps_conformal("vpol_omp",ss2%vpol_omp,ss3%vpol_omp,iout,ierr)
        ss3%vpol_omp = ss2%vpol_omp
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%VRep_coil_count)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%VRep_coil_count).AND.ALLOCATED(ss2%VRep_coil_count)) THEN
        CALL ps_diff("VRep_coil_count",ss1%VRep_coil_count,ss2%VRep_coil_count,icheck,iout,ierr)
        CALL ps_conformal("VRep_coil_count",ss1%VRep_coil_count,ss3%VRep_coil_count,iout,ierr)
        if(ierr.eq.0) ss3%VRep_coil_count = ss1%VRep_coil_count
     ELSE IF(ALLOCATED(ss1%VRep_coil_count)) THEN
        CALL ps_conformal("VRep_coil_count",ss1%VRep_coil_count,ss3%VRep_coil_count,iout,ierr)
        ss3%VRep_coil_count = ss1%VRep_coil_count
     ELSE IF(ALLOCATED(ss2%VRep_coil_count)) THEN
        CALL ps_conformal("VRep_coil_count",ss2%VRep_coil_count,ss3%VRep_coil_count,iout,ierr)
        ss3%VRep_coil_count = ss2%VRep_coil_count
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Vrep_coil_spacing)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Vrep_coil_spacing).AND.ALLOCATED(ss2%Vrep_coil_spacing)) THEN
        CALL ps_diff("Vrep_coil_spacing",ss1%Vrep_coil_spacing,ss2%Vrep_coil_spacing,icheck,iout,ierr)
        CALL ps_conformal("Vrep_coil_spacing",ss1%Vrep_coil_spacing,ss3%Vrep_coil_spacing,iout,ierr)
        if(ierr.eq.0) ss3%Vrep_coil_spacing = ss1%Vrep_coil_spacing
     ELSE IF(ALLOCATED(ss1%Vrep_coil_spacing)) THEN
        CALL ps_conformal("Vrep_coil_spacing",ss1%Vrep_coil_spacing,ss3%Vrep_coil_spacing,iout,ierr)
        ss3%Vrep_coil_spacing = ss1%Vrep_coil_spacing
     ELSE IF(ALLOCATED(ss2%Vrep_coil_spacing)) THEN
        CALL ps_conformal("Vrep_coil_spacing",ss2%Vrep_coil_spacing,ss3%Vrep_coil_spacing,iout,ierr)
        ss3%Vrep_coil_spacing = ss2%Vrep_coil_spacing
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Vsize_coil)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Vsize_coil).AND.ALLOCATED(ss2%Vsize_coil)) THEN
        CALL ps_diff("Vsize_coil",ss1%Vsize_coil,ss2%Vsize_coil,icheck,iout,ierr)
        CALL ps_conformal("Vsize_coil",ss1%Vsize_coil,ss3%Vsize_coil,iout,ierr)
        if(ierr.eq.0) ss3%Vsize_coil = ss1%Vsize_coil
     ELSE IF(ALLOCATED(ss1%Vsize_coil)) THEN
        CALL ps_conformal("Vsize_coil",ss1%Vsize_coil,ss3%Vsize_coil,iout,ierr)
        ss3%Vsize_coil = ss1%Vsize_coil
     ELSE IF(ALLOCATED(ss2%Vsize_coil)) THEN
        CALL ps_conformal("Vsize_coil",ss2%Vsize_coil,ss3%Vsize_coil,iout,ierr)
        ss3%Vsize_coil = ss2%Vsize_coil
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     ss3%vsur = w1*ss1%vsur + (1.0_rspec-w1)*ss2%vsur
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("VSUR_Data_Info",ss1%VSUR_Data_Info,ss2%VSUR_Data_Info,icheck,iout,ierr)
     ss3%VSUR_Data_Info = ss1%VSUR_Data_Info
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("vtor_Data_Info",ss1%vtor_Data_Info,ss2%vtor_Data_Info,icheck,iout,ierr)
     ss3%vtor_Data_Info = ss1%vtor_Data_Info
  ENDIF

  IF((ALLOCATED(ss3%vtor_inmp)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vtor_inmp).AND.ALLOCATED(ss2%vtor_inmp)) THEN
        CALL ps_conformal("vtor_inmp",ss2%vtor_inmp,ss3%vtor_inmp,iout,ierr)
        CALL ps_conformal("vtor_inmp",ss1%vtor_inmp,ss3%vtor_inmp,iout,ierr)
        if(ierr.eq.0) ss3%vtor_inmp = w1*ss1%vtor_inmp + (1.0_rspec-w1)*ss2%vtor_inmp
     ELSE IF(ALLOCATED(ss1%vtor_inmp)) THEN
        CALL ps_conformal("vtor_inmp",ss1%vtor_inmp,ss3%vtor_inmp,iout,ierr)
        ss3%vtor_inmp = ss1%vtor_inmp
     ELSE IF(ALLOCATED(ss2%vtor_inmp)) THEN
        CALL ps_conformal("vtor_inmp",ss2%vtor_inmp,ss3%vtor_inmp,iout,ierr)
        ss3%vtor_inmp = ss2%vtor_inmp
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vtor_is_input)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vtor_is_input).AND.ALLOCATED(ss2%vtor_is_input)) THEN
        CALL ps_diff("vtor_is_input",ss1%vtor_is_input,ss2%vtor_is_input,icheck,iout,ierr)
        CALL ps_conformal("vtor_is_input",ss1%vtor_is_input,ss3%vtor_is_input,iout,ierr)
        if(ierr.eq.0) ss3%vtor_is_input = ss1%vtor_is_input
     ELSE IF(ALLOCATED(ss1%vtor_is_input)) THEN
        CALL ps_conformal("vtor_is_input",ss1%vtor_is_input,ss3%vtor_is_input,iout,ierr)
        ss3%vtor_is_input = ss1%vtor_is_input
     ELSE IF(ALLOCATED(ss2%vtor_is_input)) THEN
        CALL ps_conformal("vtor_is_input",ss2%vtor_is_input,ss3%vtor_is_input,iout,ierr)
        ss3%vtor_is_input = ss2%vtor_is_input
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vtor_omp)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vtor_omp).AND.ALLOCATED(ss2%vtor_omp)) THEN
        CALL ps_conformal("vtor_omp",ss2%vtor_omp,ss3%vtor_omp,iout,ierr)
        CALL ps_conformal("vtor_omp",ss1%vtor_omp,ss3%vtor_omp,iout,ierr)
        if(ierr.eq.0) ss3%vtor_omp = w1*ss1%vtor_omp + (1.0_rspec-w1)*ss2%vtor_omp
     ELSE IF(ALLOCATED(ss1%vtor_omp)) THEN
        CALL ps_conformal("vtor_omp",ss1%vtor_omp,ss3%vtor_omp,iout,ierr)
        ss3%vtor_omp = ss1%vtor_omp
     ELSE IF(ALLOCATED(ss2%vtor_omp)) THEN
        CALL ps_conformal("vtor_omp",ss2%vtor_omp,ss3%vtor_omp,iout,ierr)
        ss3%vtor_omp = ss2%vtor_omp
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%V_loop)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%V_loop).AND.ALLOCATED(ss2%V_loop)) THEN
        CALL ps_conformal("V_loop",ss2%V_loop,ss3%V_loop,iout,ierr)
        CALL ps_conformal("V_loop",ss1%V_loop,ss3%V_loop,iout,ierr)
        if(ierr.eq.0) ss3%V_loop = w1*ss1%V_loop + (1.0_rspec-w1)*ss2%V_loop
     ELSE IF(ALLOCATED(ss1%V_loop)) THEN
        CALL ps_conformal("V_loop",ss1%V_loop,ss3%V_loop,iout,ierr)
        ss3%V_loop = ss1%V_loop
     ELSE IF(ALLOCATED(ss2%V_loop)) THEN
        CALL ps_conformal("V_loop",ss2%V_loop,ss3%V_loop,iout,ierr)
        ss3%V_loop = ss2%V_loop
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%v_pars)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%v_pars).AND.ALLOCATED(ss2%v_pars)) THEN
        CALL ps_conformal("v_pars",ss2%v_pars,ss3%v_pars,iout,ierr)
        CALL ps_conformal("v_pars",ss1%v_pars,ss3%v_pars,iout,ierr)
        if(ierr.eq.0) ss3%v_pars = w1*ss1%v_pars + (1.0_rspec-w1)*ss2%v_pars
     ELSE IF(ALLOCATED(ss1%v_pars)) THEN
        CALL ps_conformal("v_pars",ss1%v_pars,ss3%v_pars,iout,ierr)
        ss3%v_pars = ss1%v_pars
     ELSE IF(ALLOCATED(ss2%v_pars)) THEN
        CALL ps_conformal("v_pars",ss2%v_pars,ss3%v_pars,iout,ierr)
        ss3%v_pars = ss2%v_pars
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%v_pers)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%v_pers).AND.ALLOCATED(ss2%v_pers)) THEN
        CALL ps_conformal("v_pers",ss2%v_pers,ss3%v_pers,iout,ierr)
        CALL ps_conformal("v_pers",ss1%v_pers,ss3%v_pers,iout,ierr)
        if(ierr.eq.0) ss3%v_pers = w1*ss1%v_pers + (1.0_rspec-w1)*ss2%v_pers
     ELSE IF(ALLOCATED(ss1%v_pers)) THEN
        CALL ps_conformal("v_pers",ss1%v_pers,ss3%v_pers,iout,ierr)
        ss3%v_pers = ss1%v_pers
     ELSE IF(ALLOCATED(ss2%v_pers)) THEN
        CALL ps_conformal("v_pers",ss2%v_pers,ss3%v_pers,iout,ierr)
        ss3%v_pers = ss2%v_pers
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%wt_nphi)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%wt_nphi).AND.ALLOCATED(ss2%wt_nphi)) THEN
        CALL ps_diff("wt_nphi",ss1%wt_nphi,ss2%wt_nphi,icheck,iout,ierr)
        CALL ps_conformal("wt_nphi",ss1%wt_nphi,ss3%wt_nphi,iout,ierr)
        if(ierr.eq.0) ss3%wt_nphi = ss1%wt_nphi
     ELSE IF(ALLOCATED(ss1%wt_nphi)) THEN
        CALL ps_conformal("wt_nphi",ss1%wt_nphi,ss3%wt_nphi,iout,ierr)
        ss3%wt_nphi = ss1%wt_nphi
     ELSE IF(ALLOCATED(ss2%wt_nphi)) THEN
        CALL ps_conformal("wt_nphi",ss2%wt_nphi,ss3%wt_nphi,iout,ierr)
        ss3%wt_nphi = ss2%wt_nphi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%wt_nphi_abs)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%wt_nphi_abs).AND.ALLOCATED(ss2%wt_nphi_abs)) THEN
        CALL ps_conformal("wt_nphi_abs",ss2%wt_nphi_abs,ss3%wt_nphi_abs,iout,ierr)
        CALL ps_conformal("wt_nphi_abs",ss1%wt_nphi_abs,ss3%wt_nphi_abs,iout,ierr)
        if(ierr.eq.0) ss3%wt_nphi_abs = w1*ss1%wt_nphi_abs + (1.0_rspec-w1)*ss2%wt_nphi_abs
     ELSE IF(ALLOCATED(ss1%wt_nphi_abs)) THEN
        CALL ps_conformal("wt_nphi_abs",ss1%wt_nphi_abs,ss3%wt_nphi_abs,iout,ierr)
        ss3%wt_nphi_abs = ss1%wt_nphi_abs
     ELSE IF(ALLOCATED(ss2%wt_nphi_abs)) THEN
        CALL ps_conformal("wt_nphi_abs",ss2%wt_nphi_abs,ss3%wt_nphi_abs,iout,ierr)
        ss3%wt_nphi_abs = ss2%wt_nphi_abs
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%wt_nphi_ext)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%wt_nphi_ext).AND.ALLOCATED(ss2%wt_nphi_ext)) THEN
        CALL ps_conformal("wt_nphi_ext",ss2%wt_nphi_ext,ss3%wt_nphi_ext,iout,ierr)
        CALL ps_conformal("wt_nphi_ext",ss1%wt_nphi_ext,ss3%wt_nphi_ext,iout,ierr)
        if(ierr.eq.0) ss3%wt_nphi_ext = w1*ss1%wt_nphi_ext + (1.0_rspec-w1)*ss2%wt_nphi_ext
     ELSE IF(ALLOCATED(ss1%wt_nphi_ext)) THEN
        CALL ps_conformal("wt_nphi_ext",ss1%wt_nphi_ext,ss3%wt_nphi_ext,iout,ierr)
        ss3%wt_nphi_ext = ss1%wt_nphi_ext
     ELSE IF(ALLOCATED(ss2%wt_nphi_ext)) THEN
        CALL ps_conformal("wt_nphi_ext",ss2%wt_nphi_ext,ss3%wt_nphi_ext,iout,ierr)
        ss3%wt_nphi_ext = ss2%wt_nphi_ext
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%xRjcos_momeq)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%xRjcos_momeq).AND.ALLOCATED(ss2%xRjcos_momeq)) THEN
        CALL ps_conformal("xRjcos_momeq",ss2%xRjcos_momeq,ss3%xRjcos_momeq,iout,ierr)
        CALL ps_conformal("xRjcos_momeq",ss1%xRjcos_momeq,ss3%xRjcos_momeq,iout,ierr)
        if(ierr.eq.0) ss3%xRjcos_momeq = w1*ss1%xRjcos_momeq + (1.0_rspec-w1)*ss2%xRjcos_momeq
     ELSE IF(ALLOCATED(ss1%xRjcos_momeq)) THEN
        CALL ps_conformal("xRjcos_momeq",ss1%xRjcos_momeq,ss3%xRjcos_momeq,iout,ierr)
        ss3%xRjcos_momeq = ss1%xRjcos_momeq
     ELSE IF(ALLOCATED(ss2%xRjcos_momeq)) THEN
        CALL ps_conformal("xRjcos_momeq",ss2%xRjcos_momeq,ss3%xRjcos_momeq,iout,ierr)
        ss3%xRjcos_momeq = ss2%xRjcos_momeq
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%xRjsin_momeq)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%xRjsin_momeq).AND.ALLOCATED(ss2%xRjsin_momeq)) THEN
        CALL ps_conformal("xRjsin_momeq",ss2%xRjsin_momeq,ss3%xRjsin_momeq,iout,ierr)
        CALL ps_conformal("xRjsin_momeq",ss1%xRjsin_momeq,ss3%xRjsin_momeq,iout,ierr)
        if(ierr.eq.0) ss3%xRjsin_momeq = w1*ss1%xRjsin_momeq + (1.0_rspec-w1)*ss2%xRjsin_momeq
     ELSE IF(ALLOCATED(ss1%xRjsin_momeq)) THEN
        CALL ps_conformal("xRjsin_momeq",ss1%xRjsin_momeq,ss3%xRjsin_momeq,iout,ierr)
        ss3%xRjsin_momeq = ss1%xRjsin_momeq
     ELSE IF(ALLOCATED(ss2%xRjsin_momeq)) THEN
        CALL ps_conformal("xRjsin_momeq",ss2%xRjsin_momeq,ss3%xRjsin_momeq,iout,ierr)
        ss3%xRjsin_momeq = ss2%xRjsin_momeq
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%xs)).AND.(cclist(ps_cnum_RAD).EQ.1)) THEN
     IF(ALLOCATED(ss1%xs).AND.ALLOCATED(ss2%xs)) THEN
        CALL ps_conformal("xs",ss2%xs,ss3%xs,iout,ierr)
        CALL ps_conformal("xs",ss1%xs,ss3%xs,iout,ierr)
        if(ierr.eq.0) ss3%xs = w1*ss1%xs + (1.0_rspec-w1)*ss2%xs
     ELSE IF(ALLOCATED(ss1%xs)) THEN
        CALL ps_conformal("xs",ss1%xs,ss3%xs,iout,ierr)
        ss3%xs = ss1%xs
     ELSE IF(ALLOCATED(ss2%xs)) THEN
        CALL ps_conformal("xs",ss2%xs,ss3%xs,iout,ierr)
        ss3%xs = ss2%xs
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%xs_name)).AND.(cclist(ps_cnum_RAD).EQ.1)) THEN
     IF(ALLOCATED(ss1%xs_name).AND.ALLOCATED(ss2%xs_name)) THEN
        CALL ps_diff("xs_name",ss1%xs_name,ss2%xs_name,icheck,iout,ierr)
        CALL ps_conformal("xs_name",ss1%xs_name,ss3%xs_name,iout,ierr)
        if(ierr.eq.0) ss3%xs_name = ss1%xs_name
     ELSE IF(ALLOCATED(ss1%xs_name)) THEN
        CALL ps_conformal("xs_name",ss1%xs_name,ss3%xs_name,iout,ierr)
        ss3%xs_name = ss1%xs_name
     ELSE IF(ALLOCATED(ss2%xs_name)) THEN
        CALL ps_conformal("xs_name",ss2%xs_name,ss3%xs_name,iout,ierr)
        ss3%xs_name = ss2%xs_name
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%xZjcos_momeq)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%xZjcos_momeq).AND.ALLOCATED(ss2%xZjcos_momeq)) THEN
        CALL ps_conformal("xZjcos_momeq",ss2%xZjcos_momeq,ss3%xZjcos_momeq,iout,ierr)
        CALL ps_conformal("xZjcos_momeq",ss1%xZjcos_momeq,ss3%xZjcos_momeq,iout,ierr)
        if(ierr.eq.0) ss3%xZjcos_momeq = w1*ss1%xZjcos_momeq + (1.0_rspec-w1)*ss2%xZjcos_momeq
     ELSE IF(ALLOCATED(ss1%xZjcos_momeq)) THEN
        CALL ps_conformal("xZjcos_momeq",ss1%xZjcos_momeq,ss3%xZjcos_momeq,iout,ierr)
        ss3%xZjcos_momeq = ss1%xZjcos_momeq
     ELSE IF(ALLOCATED(ss2%xZjcos_momeq)) THEN
        CALL ps_conformal("xZjcos_momeq",ss2%xZjcos_momeq,ss3%xZjcos_momeq,iout,ierr)
        ss3%xZjcos_momeq = ss2%xZjcos_momeq
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%xZjsin_momeq)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%xZjsin_momeq).AND.ALLOCATED(ss2%xZjsin_momeq)) THEN
        CALL ps_conformal("xZjsin_momeq",ss2%xZjsin_momeq,ss3%xZjsin_momeq,iout,ierr)
        CALL ps_conformal("xZjsin_momeq",ss1%xZjsin_momeq,ss3%xZjsin_momeq,iout,ierr)
        if(ierr.eq.0) ss3%xZjsin_momeq = w1*ss1%xZjsin_momeq + (1.0_rspec-w1)*ss2%xZjsin_momeq
     ELSE IF(ALLOCATED(ss1%xZjsin_momeq)) THEN
        CALL ps_conformal("xZjsin_momeq",ss1%xZjsin_momeq,ss3%xZjsin_momeq,iout,ierr)
        ss3%xZjsin_momeq = ss1%xZjsin_momeq
     ELSE IF(ALLOCATED(ss2%xZjsin_momeq)) THEN
        CALL ps_conformal("xZjsin_momeq",ss2%xZjsin_momeq,ss3%xZjsin_momeq,iout,ierr)
        ss3%xZjsin_momeq = ss2%xZjsin_momeq
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("Z0max",ss1%Z0max,ss2%Z0max,icheck,iout,ierr)
     ss3%Z0max = ss1%Z0max
  ENDIF

  IF((ALLOCATED(ss3%Z0_momeq)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Z0_momeq).AND.ALLOCATED(ss2%Z0_momeq)) THEN
        CALL ps_conformal("Z0_momeq",ss2%Z0_momeq,ss3%Z0_momeq,iout,ierr)
        CALL ps_conformal("Z0_momeq",ss1%Z0_momeq,ss3%Z0_momeq,iout,ierr)
        if(ierr.eq.0) ss3%Z0_momeq = w1*ss1%Z0_momeq + (1.0_rspec-w1)*ss2%Z0_momeq
     ELSE IF(ALLOCATED(ss1%Z0_momeq)) THEN
        CALL ps_conformal("Z0_momeq",ss1%Z0_momeq,ss3%Z0_momeq,iout,ierr)
        ss3%Z0_momeq = ss1%Z0_momeq
     ELSE IF(ALLOCATED(ss2%Z0_momeq)) THEN
        CALL ps_conformal("Z0_momeq",ss2%Z0_momeq,ss3%Z0_momeq,iout,ierr)
        ss3%Z0_momeq = ss2%Z0_momeq
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Zbap)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%Zbap).AND.ALLOCATED(ss2%Zbap)) THEN
        CALL ps_diff("Zbap",ss1%Zbap,ss2%Zbap,icheck,iout,ierr)
        CALL ps_conformal("Zbap",ss1%Zbap,ss3%Zbap,iout,ierr)
        if(ierr.eq.0) ss3%Zbap = ss1%Zbap
     ELSE IF(ALLOCATED(ss1%Zbap)) THEN
        CALL ps_conformal("Zbap",ss1%Zbap,ss3%Zbap,iout,ierr)
        ss3%Zbap = ss1%Zbap
     ELSE IF(ALLOCATED(ss2%Zbap)) THEN
        CALL ps_conformal("Zbap",ss2%Zbap,ss3%Zbap,iout,ierr)
        ss3%Zbap = ss2%Zbap
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Zbsc)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%Zbsc).AND.ALLOCATED(ss2%Zbsc)) THEN
        CALL ps_diff("Zbsc",ss1%Zbsc,ss2%Zbsc,icheck,iout,ierr)
        CALL ps_conformal("Zbsc",ss1%Zbsc,ss3%Zbsc,iout,ierr)
        if(ierr.eq.0) ss3%Zbsc = ss1%Zbsc
     ELSE IF(ALLOCATED(ss1%Zbsc)) THEN
        CALL ps_conformal("Zbsc",ss1%Zbsc,ss3%Zbsc,iout,ierr)
        ss3%Zbsc = ss1%Zbsc
     ELSE IF(ALLOCATED(ss2%Zbsc)) THEN
        CALL ps_conformal("Zbsc",ss2%Zbsc,ss3%Zbsc,iout,ierr)
        ss3%Zbsc = ss2%Zbsc
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Zeff)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%Zeff).AND.ALLOCATED(ss2%Zeff)) THEN
        CALL ps_conformal("Zeff",ss2%Zeff,ss3%Zeff,iout,ierr)
        CALL ps_conformal("Zeff",ss1%Zeff,ss3%Zeff,iout,ierr)
        if(ierr.eq.0) ss3%Zeff = w1*ss1%Zeff + (1.0_rspec-w1)*ss2%Zeff
     ELSE IF(ALLOCATED(ss1%Zeff)) THEN
        CALL ps_conformal("Zeff",ss1%Zeff,ss3%Zeff,iout,ierr)
        ss3%Zeff = ss1%Zeff
     ELSE IF(ALLOCATED(ss2%Zeff)) THEN
        CALL ps_conformal("Zeff",ss2%Zeff,ss3%Zeff,iout,ierr)
        ss3%Zeff = ss2%Zeff
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("ZEFF_Data_Info",ss1%ZEFF_Data_Info,ss2%ZEFF_Data_Info,icheck,iout,ierr)
     ss3%ZEFF_Data_Info = ss1%ZEFF_Data_Info
  ENDIF

  IF((ALLOCATED(ss3%Zeff_fi)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%Zeff_fi).AND.ALLOCATED(ss2%Zeff_fi)) THEN
        CALL ps_conformal("Zeff_fi",ss2%Zeff_fi,ss3%Zeff_fi,iout,ierr)
        CALL ps_conformal("Zeff_fi",ss1%Zeff_fi,ss3%Zeff_fi,iout,ierr)
        if(ierr.eq.0) ss3%Zeff_fi = w1*ss1%Zeff_fi + (1.0_rspec-w1)*ss2%Zeff_fi
     ELSE IF(ALLOCATED(ss1%Zeff_fi)) THEN
        CALL ps_conformal("Zeff_fi",ss1%Zeff_fi,ss3%Zeff_fi,iout,ierr)
        ss3%Zeff_fi = ss1%Zeff_fi
     ELSE IF(ALLOCATED(ss2%Zeff_fi)) THEN
        CALL ps_conformal("Zeff_fi",ss2%Zeff_fi,ss3%Zeff_fi,iout,ierr)
        ss3%Zeff_fi = ss2%Zeff_fi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Zeff_th)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%Zeff_th).AND.ALLOCATED(ss2%Zeff_th)) THEN
        CALL ps_conformal("Zeff_th",ss2%Zeff_th,ss3%Zeff_th,iout,ierr)
        CALL ps_conformal("Zeff_th",ss1%Zeff_th,ss3%Zeff_th,iout,ierr)
        if(ierr.eq.0) ss3%Zeff_th = w1*ss1%Zeff_th + (1.0_rspec-w1)*ss2%Zeff_th
     ELSE IF(ALLOCATED(ss1%Zeff_th)) THEN
        CALL ps_conformal("Zeff_th",ss1%Zeff_th,ss3%Zeff_th,iout,ierr)
        ss3%Zeff_th = ss1%Zeff_th
     ELSE IF(ALLOCATED(ss2%Zeff_th)) THEN
        CALL ps_conformal("Zeff_th",ss2%Zeff_th,ss3%Zeff_th,iout,ierr)
        ss3%Zeff_th = ss2%Zeff_th
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("Zimp1",ss1%Zimp1,ss2%Zimp1,icheck,iout,ierr)
     ss3%Zimp1 = ss1%Zimp1
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("Zimp2",ss1%Zimp2,ss2%Zimp2,icheck,iout,ierr)
     ss3%Zimp2 = ss1%Zimp2
  ENDIF

  IF((ALLOCATED(ss3%zlim)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%zlim).AND.ALLOCATED(ss2%zlim)) THEN
        CALL ps_diff("zlim",ss1%zlim,ss2%zlim,icheck,iout,ierr)
        CALL ps_conformal("zlim",ss1%zlim,ss3%zlim,iout,ierr)
        if(ierr.eq.0) ss3%zlim = ss1%zlim
     ELSE IF(ALLOCATED(ss1%zlim)) THEN
        CALL ps_conformal("zlim",ss1%zlim,ss3%zlim,iout,ierr)
        ss3%zlim = ss1%zlim
     ELSE IF(ALLOCATED(ss2%zlim)) THEN
        CALL ps_conformal("zlim",ss2%zlim,ss3%zlim,iout,ierr)
        ss3%zlim = ss2%zlim
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Zloc_coil)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Zloc_coil).AND.ALLOCATED(ss2%Zloc_coil)) THEN
        CALL ps_diff("Zloc_coil",ss1%Zloc_coil,ss2%Zloc_coil,icheck,iout,ierr)
        CALL ps_conformal("Zloc_coil",ss1%Zloc_coil,ss3%Zloc_coil,iout,ierr)
        if(ierr.eq.0) ss3%Zloc_coil = ss1%Zloc_coil
     ELSE IF(ALLOCATED(ss1%Zloc_coil)) THEN
        CALL ps_conformal("Zloc_coil",ss1%Zloc_coil,ss3%Zloc_coil,iout,ierr)
        ss3%Zloc_coil = ss1%Zloc_coil
     ELSE IF(ALLOCATED(ss2%Zloc_coil)) THEN
        CALL ps_conformal("Zloc_coil",ss2%Zloc_coil,ss3%Zloc_coil,iout,ierr)
        ss3%Zloc_coil = ss2%Zloc_coil
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Z_ant)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%Z_ant).AND.ALLOCATED(ss2%Z_ant)) THEN
        CALL ps_diff("Z_ant",ss1%Z_ant,ss2%Z_ant,icheck,iout,ierr)
        CALL ps_conformal("Z_ant",ss1%Z_ant,ss3%Z_ant,iout,ierr)
        if(ierr.eq.0) ss3%Z_ant = ss1%Z_ant
     ELSE IF(ALLOCATED(ss1%Z_ant)) THEN
        CALL ps_conformal("Z_ant",ss1%Z_ant,ss3%Z_ant,iout,ierr)
        ss3%Z_ant = ss1%Z_ant
     ELSE IF(ALLOCATED(ss2%Z_ant)) THEN
        CALL ps_conformal("Z_ant",ss2%Z_ant,ss3%Z_ant,iout,ierr)
        ss3%Z_ant = ss2%Z_ant
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Z_antgeo)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%Z_antgeo).AND.ALLOCATED(ss2%Z_antgeo)) THEN
        CALL ps_diff("Z_antgeo",ss1%Z_antgeo,ss2%Z_antgeo,icheck,iout,ierr)
        CALL ps_conformal("Z_antgeo",ss1%Z_antgeo,ss3%Z_antgeo,iout,ierr)
        if(ierr.eq.0) ss3%Z_antgeo = ss1%Z_antgeo
     ELSE IF(ALLOCATED(ss1%Z_antgeo)) THEN
        CALL ps_conformal("Z_antgeo",ss1%Z_antgeo,ss3%Z_antgeo,iout,ierr)
        ss3%Z_antgeo = ss1%Z_antgeo
     ELSE IF(ALLOCATED(ss2%Z_antgeo)) THEN
        CALL ps_conformal("Z_antgeo",ss2%Z_antgeo,ss3%Z_antgeo,iout,ierr)
        ss3%Z_antgeo = ss2%Z_antgeo
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     ss3%Z_axis = w1*ss1%Z_axis + (1.0_rspec-w1)*ss2%Z_axis
  ENDIF

  IF((ALLOCATED(ss3%Z_EC_launch)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%Z_EC_launch).AND.ALLOCATED(ss2%Z_EC_launch)) THEN
        CALL ps_diff("Z_EC_launch",ss1%Z_EC_launch,ss2%Z_EC_launch,icheck,iout,ierr)
        CALL ps_conformal("Z_EC_launch",ss1%Z_EC_launch,ss3%Z_EC_launch,iout,ierr)
        if(ierr.eq.0) ss3%Z_EC_launch = ss1%Z_EC_launch
     ELSE IF(ALLOCATED(ss1%Z_EC_launch)) THEN
        CALL ps_conformal("Z_EC_launch",ss1%Z_EC_launch,ss3%Z_EC_launch,iout,ierr)
        ss3%Z_EC_launch = ss1%Z_EC_launch
     ELSE IF(ALLOCATED(ss2%Z_EC_launch)) THEN
        CALL ps_conformal("Z_EC_launch",ss2%Z_EC_launch,ss3%Z_EC_launch,iout,ierr)
        ss3%Z_EC_launch = ss2%Z_EC_launch
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Z_geo)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Z_geo).AND.ALLOCATED(ss2%Z_geo)) THEN
        CALL ps_conformal("Z_geo",ss2%Z_geo,ss3%Z_geo,iout,ierr)
        CALL ps_conformal("Z_geo",ss1%Z_geo,ss3%Z_geo,iout,ierr)
        if(ierr.eq.0) ss3%Z_geo = w1*ss1%Z_geo + (1.0_rspec-w1)*ss2%Z_geo
     ELSE IF(ALLOCATED(ss1%Z_geo)) THEN
        CALL ps_conformal("Z_geo",ss1%Z_geo,ss3%Z_geo,iout,ierr)
        ss3%Z_geo = ss1%Z_geo
     ELSE IF(ALLOCATED(ss2%Z_geo)) THEN
        CALL ps_conformal("Z_geo",ss2%Z_geo,ss3%Z_geo,iout,ierr)
        ss3%Z_geo = ss2%Z_geo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Z_grid)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Z_grid).AND.ALLOCATED(ss2%Z_grid)) THEN
        CALL ps_diff("Z_grid",ss1%Z_grid,ss2%Z_grid,ps_error,iout,ierr)
        CALL ps_conformal("Z_grid",ss1%Z_grid,ss3%Z_grid,iout,ierr)
        if(ierr.eq.0) ss3%Z_grid = ss1%Z_grid
     ELSE IF(ALLOCATED(ss1%Z_grid)) THEN
        CALL ps_conformal("Z_grid",ss1%Z_grid,ss3%Z_grid,iout,ierr)
        ss3%Z_grid = ss1%Z_grid
     ELSE IF(ALLOCATED(ss2%Z_grid)) THEN
        CALL ps_conformal("Z_grid",ss2%Z_grid,ss3%Z_grid,iout,ierr)
        ss3%Z_grid = ss2%Z_grid
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN

     CALL ps_diff("Z_max_box",ss1%Z_max_box,ss2%Z_max_box,icheck,iout,ierr)
     ss3%Z_max_box = ss1%Z_max_box
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     ss3%Z_max_lcfs = w1*ss1%Z_max_lcfs + (1.0_rspec-w1)*ss2%Z_max_lcfs
  ENDIF

  IF((ALLOCATED(ss3%Z_midp)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Z_midp).AND.ALLOCATED(ss2%Z_midp)) THEN
        CALL ps_conformal("Z_midp",ss2%Z_midp,ss3%Z_midp,iout,ierr)
        CALL ps_conformal("Z_midp",ss1%Z_midp,ss3%Z_midp,iout,ierr)
        if(ierr.eq.0) ss3%Z_midp = w1*ss1%Z_midp + (1.0_rspec-w1)*ss2%Z_midp
     ELSE IF(ALLOCATED(ss1%Z_midp)) THEN
        CALL ps_conformal("Z_midp",ss1%Z_midp,ss3%Z_midp,iout,ierr)
        ss3%Z_midp = ss1%Z_midp
     ELSE IF(ALLOCATED(ss2%Z_midp)) THEN
        CALL ps_conformal("Z_midp",ss2%Z_midp,ss3%Z_midp,iout,ierr)
        ss3%Z_midp = ss2%Z_midp
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Z_mid_ant)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%Z_mid_ant).AND.ALLOCATED(ss2%Z_mid_ant)) THEN
        CALL ps_diff("Z_mid_ant",ss1%Z_mid_ant,ss2%Z_mid_ant,icheck,iout,ierr)
        CALL ps_conformal("Z_mid_ant",ss1%Z_mid_ant,ss3%Z_mid_ant,iout,ierr)
        if(ierr.eq.0) ss3%Z_mid_ant = ss1%Z_mid_ant
     ELSE IF(ALLOCATED(ss1%Z_mid_ant)) THEN
        CALL ps_conformal("Z_mid_ant",ss1%Z_mid_ant,ss3%Z_mid_ant,iout,ierr)
        ss3%Z_mid_ant = ss1%Z_mid_ant
     ELSE IF(ALLOCATED(ss2%Z_mid_ant)) THEN
        CALL ps_conformal("Z_mid_ant",ss2%Z_mid_ant,ss3%Z_mid_ant,iout,ierr)
        ss3%Z_mid_ant = ss2%Z_mid_ant
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN

     CALL ps_diff("Z_min_box",ss1%Z_min_box,ss2%Z_min_box,icheck,iout,ierr)
     ss3%Z_min_box = ss1%Z_min_box
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     ss3%Z_min_lcfs = w1*ss1%Z_min_lcfs + (1.0_rspec-w1)*ss2%Z_min_lcfs
  ENDIF

  IF((ALLOCATED(ss3%Z_ripple)).AND.(cclist(ps_cnum_RIPPLE).EQ.1)) THEN
     IF(ALLOCATED(ss1%Z_ripple).AND.ALLOCATED(ss2%Z_ripple)) THEN
        CALL ps_diff("Z_ripple",ss1%Z_ripple,ss2%Z_ripple,ps_error,iout,ierr)
        CALL ps_conformal("Z_ripple",ss1%Z_ripple,ss3%Z_ripple,iout,ierr)
        if(ierr.eq.0) ss3%Z_ripple = ss1%Z_ripple
     ELSE IF(ALLOCATED(ss1%Z_ripple)) THEN
        CALL ps_conformal("Z_ripple",ss1%Z_ripple,ss3%Z_ripple,iout,ierr)
        ss3%Z_ripple = ss1%Z_ripple
     ELSE IF(ALLOCATED(ss2%Z_ripple)) THEN
        CALL ps_conformal("Z_ripple",ss2%Z_ripple,ss3%Z_ripple,iout,ierr)
        ss3%Z_ripple = ss2%Z_ripple
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Z_surfMax)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Z_surfMax).AND.ALLOCATED(ss2%Z_surfMax)) THEN
        CALL ps_conformal("Z_surfMax",ss2%Z_surfMax,ss3%Z_surfMax,iout,ierr)
        CALL ps_conformal("Z_surfMax",ss1%Z_surfMax,ss3%Z_surfMax,iout,ierr)
        if(ierr.eq.0) ss3%Z_surfMax = w1*ss1%Z_surfMax + (1.0_rspec-w1)*ss2%Z_surfMax
     ELSE IF(ALLOCATED(ss1%Z_surfMax)) THEN
        CALL ps_conformal("Z_surfMax",ss1%Z_surfMax,ss3%Z_surfMax,iout,ierr)
        ss3%Z_surfMax = ss1%Z_surfMax
     ELSE IF(ALLOCATED(ss2%Z_surfMax)) THEN
        CALL ps_conformal("Z_surfMax",ss2%Z_surfMax,ss3%Z_surfMax,iout,ierr)
        ss3%Z_surfMax = ss2%Z_surfMax
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Z_surfMin)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Z_surfMin).AND.ALLOCATED(ss2%Z_surfMin)) THEN
        CALL ps_conformal("Z_surfMin",ss2%Z_surfMin,ss3%Z_surfMin,iout,ierr)
        CALL ps_conformal("Z_surfMin",ss1%Z_surfMin,ss3%Z_surfMin,iout,ierr)
        if(ierr.eq.0) ss3%Z_surfMin = w1*ss1%Z_surfMin + (1.0_rspec-w1)*ss2%Z_surfMin
     ELSE IF(ALLOCATED(ss1%Z_surfMin)) THEN
        CALL ps_conformal("Z_surfMin",ss1%Z_surfMin,ss3%Z_surfMin,iout,ierr)
        ss3%Z_surfMin = ss1%Z_surfMin
     ELSE IF(ALLOCATED(ss2%Z_surfMin)) THEN
        CALL ps_conformal("Z_surfMin",ss2%Z_surfMin,ss3%Z_surfMin,iout,ierr)
        ss3%Z_surfMin = ss2%Z_surfMin
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_merge_9
