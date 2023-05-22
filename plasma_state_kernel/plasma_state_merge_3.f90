! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_merge_3(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)


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

  IF((ALLOCATED(ss3%grho2)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%grho2).AND.ALLOCATED(ss2%grho2)) THEN
        CALL ps_conformal("grho2",ss2%grho2,ss3%grho2,iout,ierr)
        CALL ps_conformal("grho2",ss1%grho2,ss3%grho2,iout,ierr)
        if(ierr.eq.0) ss3%grho2 = w1*ss1%grho2 + (1.0_rspec-w1)*ss2%grho2
     ELSE IF(ALLOCATED(ss1%grho2)) THEN
        CALL ps_conformal("grho2",ss1%grho2,ss3%grho2,iout,ierr)
        ss3%grho2 = ss1%grho2
     ELSE IF(ALLOCATED(ss2%grho2)) THEN
        CALL ps_conformal("grho2",ss2%grho2,ss3%grho2,iout,ierr)
        ss3%grho2 = ss2%grho2
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%grho2b2i)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%grho2b2i).AND.ALLOCATED(ss2%grho2b2i)) THEN
        CALL ps_conformal("grho2b2i",ss2%grho2b2i,ss3%grho2b2i,iout,ierr)
        CALL ps_conformal("grho2b2i",ss1%grho2b2i,ss3%grho2b2i,iout,ierr)
        if(ierr.eq.0) ss3%grho2b2i = w1*ss1%grho2b2i + (1.0_rspec-w1)*ss2%grho2b2i
     ELSE IF(ALLOCATED(ss1%grho2b2i)) THEN
        CALL ps_conformal("grho2b2i",ss1%grho2b2i,ss3%grho2b2i,iout,ierr)
        ss3%grho2b2i = ss1%grho2b2i
     ELSE IF(ALLOCATED(ss2%grho2b2i)) THEN
        CALL ps_conformal("grho2b2i",ss2%grho2b2i,ss3%grho2b2i,iout,ierr)
        ss3%grho2b2i = ss2%grho2b2i
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%grho2r2i)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%grho2r2i).AND.ALLOCATED(ss2%grho2r2i)) THEN
        CALL ps_conformal("grho2r2i",ss2%grho2r2i,ss3%grho2r2i,iout,ierr)
        CALL ps_conformal("grho2r2i",ss1%grho2r2i,ss3%grho2r2i,iout,ierr)
        if(ierr.eq.0) ss3%grho2r2i = w1*ss1%grho2r2i + (1.0_rspec-w1)*ss2%grho2r2i
     ELSE IF(ALLOCATED(ss1%grho2r2i)) THEN
        CALL ps_conformal("grho2r2i",ss1%grho2r2i,ss3%grho2r2i,iout,ierr)
        ss3%grho2r2i = ss1%grho2r2i
     ELSE IF(ALLOCATED(ss2%grho2r2i)) THEN
        CALL ps_conformal("grho2r2i",ss2%grho2r2i,ss3%grho2r2i,iout,ierr)
        ss3%grho2r2i = ss2%grho2r2i
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%grho2r3i)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%grho2r3i).AND.ALLOCATED(ss2%grho2r3i)) THEN
        CALL ps_conformal("grho2r3i",ss2%grho2r3i,ss3%grho2r3i,iout,ierr)
        CALL ps_conformal("grho2r3i",ss1%grho2r3i,ss3%grho2r3i,iout,ierr)
        if(ierr.eq.0) ss3%grho2r3i = w1*ss1%grho2r3i + (1.0_rspec-w1)*ss2%grho2r3i
     ELSE IF(ALLOCATED(ss1%grho2r3i)) THEN
        CALL ps_conformal("grho2r3i",ss1%grho2r3i,ss3%grho2r3i,iout,ierr)
        ss3%grho2r3i = ss1%grho2r3i
     ELSE IF(ALLOCATED(ss2%grho2r3i)) THEN
        CALL ps_conformal("grho2r3i",ss2%grho2r3i,ss3%grho2r3i,iout,ierr)
        ss3%grho2r3i = ss2%grho2r3i
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%gri)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%gri).AND.ALLOCATED(ss2%gri)) THEN
        CALL ps_conformal("gri",ss2%gri,ss3%gri,iout,ierr)
        CALL ps_conformal("gri",ss1%gri,ss3%gri,iout,ierr)
        if(ierr.eq.0) ss3%gri = w1*ss1%gri + (1.0_rspec-w1)*ss2%gri
     ELSE IF(ALLOCATED(ss1%gri)) THEN
        CALL ps_conformal("gri",ss1%gri,ss3%gri,iout,ierr)
        ss3%gri = ss1%gri
     ELSE IF(ALLOCATED(ss2%gri)) THEN
        CALL ps_conformal("gri",ss2%gri,ss3%gri,iout,ierr)
        ss3%gri = ss2%gri
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%grirhoi)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%grirhoi).AND.ALLOCATED(ss2%grirhoi)) THEN
        CALL ps_conformal("grirhoi",ss2%grirhoi,ss3%grirhoi,iout,ierr)
        CALL ps_conformal("grirhoi",ss1%grirhoi,ss3%grirhoi,iout,ierr)
        if(ierr.eq.0) ss3%grirhoi = w1*ss1%grirhoi + (1.0_rspec-w1)*ss2%grirhoi
     ELSE IF(ALLOCATED(ss1%grirhoi)) THEN
        CALL ps_conformal("grirhoi",ss1%grirhoi,ss3%grirhoi,iout,ierr)
        ss3%grirhoi = ss1%grirhoi
     ELSE IF(ALLOCATED(ss2%grirhoi)) THEN
        CALL ps_conformal("grirhoi",ss2%grirhoi,ss3%grirhoi,iout,ierr)
        ss3%grirhoi = ss2%grirhoi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%growthrate_drbm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%growthrate_drbm).AND.ALLOCATED(ss2%growthrate_drbm)) THEN
        CALL ps_conformal("growthrate_drbm",ss2%growthrate_drbm,ss3%growthrate_drbm,iout,ierr)
        CALL ps_conformal("growthrate_drbm",ss1%growthrate_drbm,ss3%growthrate_drbm,iout,ierr)
        if(ierr.eq.0) ss3%growthrate_drbm = w1*ss1%growthrate_drbm + (1.0_rspec-w1)*ss2%growthrate_drbm
     ELSE IF(ALLOCATED(ss1%growthrate_drbm)) THEN
        CALL ps_conformal("growthrate_drbm",ss1%growthrate_drbm,ss3%growthrate_drbm,iout,ierr)
        ss3%growthrate_drbm = ss1%growthrate_drbm
     ELSE IF(ALLOCATED(ss2%growthrate_drbm)) THEN
        CALL ps_conformal("growthrate_drbm",ss2%growthrate_drbm,ss3%growthrate_drbm,iout,ierr)
        ss3%growthrate_drbm = ss2%growthrate_drbm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%growthrate_etg)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%growthrate_etg).AND.ALLOCATED(ss2%growthrate_etg)) THEN
        CALL ps_conformal("growthrate_etg",ss2%growthrate_etg,ss3%growthrate_etg,iout,ierr)
        CALL ps_conformal("growthrate_etg",ss1%growthrate_etg,ss3%growthrate_etg,iout,ierr)
        if(ierr.eq.0) ss3%growthrate_etg = w1*ss1%growthrate_etg + (1.0_rspec-w1)*ss2%growthrate_etg
     ELSE IF(ALLOCATED(ss1%growthrate_etg)) THEN
        CALL ps_conformal("growthrate_etg",ss1%growthrate_etg,ss3%growthrate_etg,iout,ierr)
        ss3%growthrate_etg = ss1%growthrate_etg
     ELSE IF(ALLOCATED(ss2%growthrate_etg)) THEN
        CALL ps_conformal("growthrate_etg",ss2%growthrate_etg,ss3%growthrate_etg,iout,ierr)
        ss3%growthrate_etg = ss2%growthrate_etg
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%growthrate_glf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%growthrate_glf).AND.ALLOCATED(ss2%growthrate_glf)) THEN
        CALL ps_conformal("growthrate_glf",ss2%growthrate_glf,ss3%growthrate_glf,iout,ierr)
        CALL ps_conformal("growthrate_glf",ss1%growthrate_glf,ss3%growthrate_glf,iout,ierr)
        if(ierr.eq.0) ss3%growthrate_glf = w1*ss1%growthrate_glf + (1.0_rspec-w1)*ss2%growthrate_glf
     ELSE IF(ALLOCATED(ss1%growthrate_glf)) THEN
        CALL ps_conformal("growthrate_glf",ss1%growthrate_glf,ss3%growthrate_glf,iout,ierr)
        ss3%growthrate_glf = ss1%growthrate_glf
     ELSE IF(ALLOCATED(ss2%growthrate_glf)) THEN
        CALL ps_conformal("growthrate_glf",ss2%growthrate_glf,ss3%growthrate_glf,iout,ierr)
        ss3%growthrate_glf = ss2%growthrate_glf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%growthrate_max_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%growthrate_max_tglf).AND.ALLOCATED(ss2%growthrate_max_tglf)) THEN
        CALL ps_conformal("growthrate_max_tglf",ss2%growthrate_max_tglf,ss3%growthrate_max_tglf,iout,ierr)
        CALL ps_conformal("growthrate_max_tglf",ss1%growthrate_max_tglf,ss3%growthrate_max_tglf,iout,ierr)
        if(ierr.eq.0) ss3%growthrate_max_tglf = w1*ss1%growthrate_max_tglf + (1.0_rspec-w1)*ss2%growthrate_max_tglf
     ELSE IF(ALLOCATED(ss1%growthrate_max_tglf)) THEN
        CALL ps_conformal("growthrate_max_tglf",ss1%growthrate_max_tglf,ss3%growthrate_max_tglf,iout,ierr)
        ss3%growthrate_max_tglf = ss1%growthrate_max_tglf
     ELSE IF(ALLOCATED(ss2%growthrate_max_tglf)) THEN
        CALL ps_conformal("growthrate_max_tglf",ss2%growthrate_max_tglf,ss3%growthrate_max_tglf,iout,ierr)
        ss3%growthrate_max_tglf = ss2%growthrate_max_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%growthrate_mmm71)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%growthrate_mmm71).AND.ALLOCATED(ss2%growthrate_mmm71)) THEN
        CALL ps_conformal("growthrate_mmm71",ss2%growthrate_mmm71,ss3%growthrate_mmm71,iout,ierr)
        CALL ps_conformal("growthrate_mmm71",ss1%growthrate_mmm71,ss3%growthrate_mmm71,iout,ierr)
        if(ierr.eq.0) ss3%growthrate_mmm71 = w1*ss1%growthrate_mmm71 + (1.0_rspec-w1)*ss2%growthrate_mmm71
     ELSE IF(ALLOCATED(ss1%growthrate_mmm71)) THEN
        CALL ps_conformal("growthrate_mmm71",ss1%growthrate_mmm71,ss3%growthrate_mmm71,iout,ierr)
        ss3%growthrate_mmm71 = ss1%growthrate_mmm71
     ELSE IF(ALLOCATED(ss2%growthrate_mmm71)) THEN
        CALL ps_conformal("growthrate_mmm71",ss2%growthrate_mmm71,ss3%growthrate_mmm71,iout,ierr)
        ss3%growthrate_mmm71 = ss2%growthrate_mmm71
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%growthrate_mtm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%growthrate_mtm).AND.ALLOCATED(ss2%growthrate_mtm)) THEN
        CALL ps_conformal("growthrate_mtm",ss2%growthrate_mtm,ss3%growthrate_mtm,iout,ierr)
        CALL ps_conformal("growthrate_mtm",ss1%growthrate_mtm,ss3%growthrate_mtm,iout,ierr)
        if(ierr.eq.0) ss3%growthrate_mtm = w1*ss1%growthrate_mtm + (1.0_rspec-w1)*ss2%growthrate_mtm
     ELSE IF(ALLOCATED(ss1%growthrate_mtm)) THEN
        CALL ps_conformal("growthrate_mtm",ss1%growthrate_mtm,ss3%growthrate_mtm,iout,ierr)
        ss3%growthrate_mtm = ss1%growthrate_mtm
     ELSE IF(ALLOCATED(ss2%growthrate_mtm)) THEN
        CALL ps_conformal("growthrate_mtm",ss2%growthrate_mtm,ss3%growthrate_mtm,iout,ierr)
        ss3%growthrate_mtm = ss2%growthrate_mtm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%growthrate_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%growthrate_tglf).AND.ALLOCATED(ss2%growthrate_tglf)) THEN
        CALL ps_conformal("growthrate_tglf",ss2%growthrate_tglf,ss3%growthrate_tglf,iout,ierr)
        CALL ps_conformal("growthrate_tglf",ss1%growthrate_tglf,ss3%growthrate_tglf,iout,ierr)
        if(ierr.eq.0) ss3%growthrate_tglf = w1*ss1%growthrate_tglf + (1.0_rspec-w1)*ss2%growthrate_tglf
     ELSE IF(ALLOCATED(ss1%growthrate_tglf)) THEN
        CALL ps_conformal("growthrate_tglf",ss1%growthrate_tglf,ss3%growthrate_tglf,iout,ierr)
        ss3%growthrate_tglf = ss1%growthrate_tglf
     ELSE IF(ALLOCATED(ss2%growthrate_tglf)) THEN
        CALL ps_conformal("growthrate_tglf",ss2%growthrate_tglf,ss3%growthrate_tglf,iout,ierr)
        ss3%growthrate_tglf = ss2%growthrate_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%growthrate_vpar)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%growthrate_vpar).AND.ALLOCATED(ss2%growthrate_vpar)) THEN
        CALL ps_conformal("growthrate_vpar",ss2%growthrate_vpar,ss3%growthrate_vpar,iout,ierr)
        CALL ps_conformal("growthrate_vpar",ss1%growthrate_vpar,ss3%growthrate_vpar,iout,ierr)
        if(ierr.eq.0) ss3%growthrate_vpar = w1*ss1%growthrate_vpar + (1.0_rspec-w1)*ss2%growthrate_vpar
     ELSE IF(ALLOCATED(ss1%growthrate_vpar)) THEN
        CALL ps_conformal("growthrate_vpar",ss1%growthrate_vpar,ss3%growthrate_vpar,iout,ierr)
        ss3%growthrate_vpar = ss1%growthrate_vpar
     ELSE IF(ALLOCATED(ss2%growthrate_vpar)) THEN
        CALL ps_conformal("growthrate_vpar",ss2%growthrate_vpar,ss3%growthrate_vpar,iout,ierr)
        ss3%growthrate_vpar = ss2%growthrate_vpar
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%gs_name)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%gs_name).AND.ALLOCATED(ss2%gs_name)) THEN
        CALL ps_diff("gs_name",ss1%gs_name,ss2%gs_name,icheck,iout,ierr)
        CALL ps_conformal("gs_name",ss1%gs_name,ss3%gs_name,iout,ierr)
        if(ierr.eq.0) ss3%gs_name = ss1%gs_name
     ELSE IF(ALLOCATED(ss1%gs_name)) THEN
        CALL ps_conformal("gs_name",ss1%gs_name,ss3%gs_name,iout,ierr)
        ss3%gs_name = ss1%gs_name
     ELSE IF(ALLOCATED(ss2%gs_name)) THEN
        CALL ps_conformal("gs_name",ss2%gs_name,ss3%gs_name,iout,ierr)
        ss3%gs_name = ss2%gs_name
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%g_eq)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%g_eq).AND.ALLOCATED(ss2%g_eq)) THEN
        CALL ps_conformal("g_eq",ss2%g_eq,ss3%g_eq,iout,ierr)
        CALL ps_conformal("g_eq",ss1%g_eq,ss3%g_eq,iout,ierr)
        if(ierr.eq.0) ss3%g_eq = w1*ss1%g_eq + (1.0_rspec-w1)*ss2%g_eq
     ELSE IF(ALLOCATED(ss1%g_eq)) THEN
        CALL ps_conformal("g_eq",ss1%g_eq,ss3%g_eq,iout,ierr)
        ss3%g_eq = ss1%g_eq
     ELSE IF(ALLOCATED(ss2%g_eq)) THEN
        CALL ps_conformal("g_eq",ss2%g_eq,ss3%g_eq,iout,ierr)
        ss3%g_eq = ss2%g_eq
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%hegrilh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%hegrilh).AND.ALLOCATED(ss2%hegrilh)) THEN
        CALL ps_diff("hegrilh",ss1%hegrilh,ss2%hegrilh,icheck,iout,ierr)
        CALL ps_conformal("hegrilh",ss1%hegrilh,ss3%hegrilh,iout,ierr)
        if(ierr.eq.0) ss3%hegrilh = ss1%hegrilh
     ELSE IF(ALLOCATED(ss1%hegrilh)) THEN
        CALL ps_conformal("hegrilh",ss1%hegrilh,ss3%hegrilh,iout,ierr)
        ss3%hegrilh = ss1%hegrilh
     ELSE IF(ALLOCATED(ss2%hegrilh)) THEN
        CALL ps_conformal("hegrilh",ss2%hegrilh,ss3%hegrilh,iout,ierr)
        ss3%hegrilh = ss2%hegrilh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Hsize_coil)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Hsize_coil).AND.ALLOCATED(ss2%Hsize_coil)) THEN
        CALL ps_diff("Hsize_coil",ss1%Hsize_coil,ss2%Hsize_coil,icheck,iout,ierr)
        CALL ps_conformal("Hsize_coil",ss1%Hsize_coil,ss3%Hsize_coil,iout,ierr)
        if(ierr.eq.0) ss3%Hsize_coil = ss1%Hsize_coil
     ELSE IF(ALLOCATED(ss1%Hsize_coil)) THEN
        CALL ps_conformal("Hsize_coil",ss1%Hsize_coil,ss3%Hsize_coil,iout,ierr)
        ss3%Hsize_coil = ss1%Hsize_coil
     ELSE IF(ALLOCATED(ss2%Hsize_coil)) THEN
        CALL ps_conformal("Hsize_coil",ss2%Hsize_coil,ss3%Hsize_coil,iout,ierr)
        ss3%Hsize_coil = ss2%Hsize_coil
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%H_einj_standard)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%H_einj_standard).AND.ALLOCATED(ss2%H_einj_standard)) THEN
        CALL ps_diff("H_einj_standard",ss1%H_einj_standard,ss2%H_einj_standard,icheck,iout,ierr)
        CALL ps_conformal("H_einj_standard",ss1%H_einj_standard,ss3%H_einj_standard,iout,ierr)
        if(ierr.eq.0) ss3%H_einj_standard = ss1%H_einj_standard
     ELSE IF(ALLOCATED(ss1%H_einj_standard)) THEN
        CALL ps_conformal("H_einj_standard",ss1%H_einj_standard,ss3%H_einj_standard,iout,ierr)
        ss3%H_einj_standard = ss1%H_einj_standard
     ELSE IF(ALLOCATED(ss2%H_einj_standard)) THEN
        CALL ps_conformal("H_einj_standard",ss2%H_einj_standard,ss3%H_einj_standard,iout,ierr)
        ss3%H_einj_standard = ss2%H_einj_standard
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%H_ffull_standard)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%H_ffull_standard).AND.ALLOCATED(ss2%H_ffull_standard)) THEN
        CALL ps_diff("H_ffull_standard",ss1%H_ffull_standard,ss2%H_ffull_standard,icheck,iout,ierr)
        CALL ps_conformal("H_ffull_standard",ss1%H_ffull_standard,ss3%H_ffull_standard,iout,ierr)
        if(ierr.eq.0) ss3%H_ffull_standard = ss1%H_ffull_standard
     ELSE IF(ALLOCATED(ss1%H_ffull_standard)) THEN
        CALL ps_conformal("H_ffull_standard",ss1%H_ffull_standard,ss3%H_ffull_standard,iout,ierr)
        ss3%H_ffull_standard = ss1%H_ffull_standard
     ELSE IF(ALLOCATED(ss2%H_ffull_standard)) THEN
        CALL ps_conformal("H_ffull_standard",ss2%H_ffull_standard,ss3%H_ffull_standard,iout,ierr)
        ss3%H_ffull_standard = ss2%H_ffull_standard
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%H_fhalf_standard)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%H_fhalf_standard).AND.ALLOCATED(ss2%H_fhalf_standard)) THEN
        CALL ps_diff("H_fhalf_standard",ss1%H_fhalf_standard,ss2%H_fhalf_standard,icheck,iout,ierr)
        CALL ps_conformal("H_fhalf_standard",ss1%H_fhalf_standard,ss3%H_fhalf_standard,iout,ierr)
        if(ierr.eq.0) ss3%H_fhalf_standard = ss1%H_fhalf_standard
     ELSE IF(ALLOCATED(ss1%H_fhalf_standard)) THEN
        CALL ps_conformal("H_fhalf_standard",ss1%H_fhalf_standard,ss3%H_fhalf_standard,iout,ierr)
        ss3%H_fhalf_standard = ss1%H_fhalf_standard
     ELSE IF(ALLOCATED(ss2%H_fhalf_standard)) THEN
        CALL ps_conformal("H_fhalf_standard",ss2%H_fhalf_standard,ss3%H_fhalf_standard,iout,ierr)
        ss3%H_fhalf_standard = ss2%H_fhalf_standard
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%icircuit_coil)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%icircuit_coil).AND.ALLOCATED(ss2%icircuit_coil)) THEN
        CALL ps_diff("icircuit_coil",ss1%icircuit_coil,ss2%icircuit_coil,icheck,iout,ierr)
        CALL ps_conformal("icircuit_coil",ss1%icircuit_coil,ss3%icircuit_coil,iout,ierr)
        if(ierr.eq.0) ss3%icircuit_coil = ss1%icircuit_coil
     ELSE IF(ALLOCATED(ss1%icircuit_coil)) THEN
        CALL ps_conformal("icircuit_coil",ss1%icircuit_coil,ss3%icircuit_coil,iout,ierr)
        ss3%icircuit_coil = ss1%icircuit_coil
     ELSE IF(ALLOCATED(ss2%icircuit_coil)) THEN
        CALL ps_conformal("icircuit_coil",ss2%icircuit_coil,ss3%icircuit_coil,iout,ierr)
        ss3%icircuit_coil = ss2%icircuit_coil
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%icrf_src_name)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%icrf_src_name).AND.ALLOCATED(ss2%icrf_src_name)) THEN
        CALL ps_diff("icrf_src_name",ss1%icrf_src_name,ss2%icrf_src_name,icheck,iout,ierr)
        CALL ps_conformal("icrf_src_name",ss1%icrf_src_name,ss3%icrf_src_name,iout,ierr)
        if(ierr.eq.0) ss3%icrf_src_name = ss1%icrf_src_name
     ELSE IF(ALLOCATED(ss1%icrf_src_name)) THEN
        CALL ps_conformal("icrf_src_name",ss1%icrf_src_name,ss3%icrf_src_name,iout,ierr)
        ss3%icrf_src_name = ss1%icrf_src_name
     ELSE IF(ALLOCATED(ss2%icrf_src_name)) THEN
        CALL ps_conformal("icrf_src_name",ss2%icrf_src_name,ss3%icrf_src_name,iout,ierr)
        ss3%icrf_src_name = ss2%icrf_src_name
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_IC).EQ.1) THEN

     CALL ps_diff("IC_Code_Info",ss1%IC_Code_Info,ss2%IC_Code_Info,icheck,iout,ierr)
     ss3%IC_Code_Info = ss1%IC_Code_Info
  ENDIF

  IF(cclist(ps_cnum_IC).EQ.1) THEN

     CALL ps_diff("IC_Data_Info",ss1%IC_Data_Info,ss2%IC_Data_Info,icheck,iout,ierr)
     ss3%IC_Data_Info = ss1%IC_Data_Info
  ENDIF

  IF((ALLOCATED(ss3%Imag_ant_coef)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%Imag_ant_coef).AND.ALLOCATED(ss2%Imag_ant_coef)) THEN
        CALL ps_diff("Imag_ant_coef",ss1%Imag_ant_coef,ss2%Imag_ant_coef,icheck,iout,ierr)
        CALL ps_conformal("Imag_ant_coef",ss1%Imag_ant_coef,ss3%Imag_ant_coef,iout,ierr)
        if(ierr.eq.0) ss3%Imag_ant_coef = ss1%Imag_ant_coef
     ELSE IF(ALLOCATED(ss1%Imag_ant_coef)) THEN
        CALL ps_conformal("Imag_ant_coef",ss1%Imag_ant_coef,ss3%Imag_ant_coef,iout,ierr)
        ss3%Imag_ant_coef = ss1%Imag_ant_coef
     ELSE IF(ALLOCATED(ss2%Imag_ant_coef)) THEN
        CALL ps_conformal("Imag_ant_coef",ss2%Imag_ant_coef,ss3%Imag_ant_coef,iout,ierr)
        ss3%Imag_ant_coef = ss2%Imag_ant_coef
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     ss3%interchange_R = w1*ss1%interchange_R + (1.0_rspec-w1)*ss2%interchange_R
  ENDIF

  IF((ALLOCATED(ss3%iota)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%iota).AND.ALLOCATED(ss2%iota)) THEN
        CALL ps_conformal("iota",ss2%iota,ss3%iota,iout,ierr)
        CALL ps_conformal("iota",ss1%iota,ss3%iota,iout,ierr)
        if(ierr.eq.0) ss3%iota = w1*ss1%iota + (1.0_rspec-w1)*ss2%iota
     ELSE IF(ALLOCATED(ss1%iota)) THEN
        CALL ps_conformal("iota",ss1%iota,ss3%iota,iout,ierr)
        ss3%iota = ss1%iota
     ELSE IF(ALLOCATED(ss2%iota)) THEN
        CALL ps_conformal("iota",ss2%iota,ss3%iota,iout,ierr)
        ss3%iota = ss2%iota
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%isThermal)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%isThermal).AND.ALLOCATED(ss2%isThermal)) THEN
        CALL ps_diff("isThermal",ss1%isThermal,ss2%isThermal,icheck,iout,ierr)
        CALL ps_conformal("isThermal",ss1%isThermal,ss3%isThermal,iout,ierr)
        if(ierr.eq.0) ss3%isThermal = ss1%isThermal
     ELSE IF(ALLOCATED(ss1%isThermal)) THEN
        CALL ps_conformal("isThermal",ss1%isThermal,ss3%isThermal,iout,ierr)
        ss3%isThermal = ss1%isThermal
     ELSE IF(ALLOCATED(ss2%isThermal)) THEN
        CALL ps_conformal("isThermal",ss2%isThermal,ss3%isThermal,iout,ierr)
        ss3%isThermal = ss2%isThermal
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%is_recycling)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%is_recycling).AND.ALLOCATED(ss2%is_recycling)) THEN
        CALL ps_diff("is_recycling",ss1%is_recycling,ss2%is_recycling,icheck,iout,ierr)
        CALL ps_conformal("is_recycling",ss1%is_recycling,ss3%is_recycling,iout,ierr)
        if(ierr.eq.0) ss3%is_recycling = ss1%is_recycling
     ELSE IF(ALLOCATED(ss1%is_recycling)) THEN
        CALL ps_conformal("is_recycling",ss1%is_recycling,ss3%is_recycling,iout,ierr)
        ss3%is_recycling = ss1%is_recycling
     ELSE IF(ALLOCATED(ss2%is_recycling)) THEN
        CALL ps_conformal("is_recycling",ss2%is_recycling,ss3%is_recycling,iout,ierr)
        ss3%is_recycling = ss2%is_recycling
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_LH).EQ.1) THEN

     CALL ps_diff("i_npoloidal",ss1%i_npoloidal,ss2%i_npoloidal,icheck,iout,ierr)
     ss3%i_npoloidal = ss1%i_npoloidal
  ENDIF

  IF((ALLOCATED(ss3%jdotb)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%jdotb).AND.ALLOCATED(ss2%jdotb)) THEN
        CALL ps_conformal("jdotb",ss2%jdotb,ss3%jdotb,iout,ierr)
        CALL ps_conformal("jdotb",ss1%jdotb,ss3%jdotb,iout,ierr)
        if(ierr.eq.0) ss3%jdotb = w1*ss1%jdotb + (1.0_rspec-w1)*ss2%jdotb
     ELSE IF(ALLOCATED(ss1%jdotb)) THEN
        CALL ps_conformal("jdotb",ss1%jdotb,ss3%jdotb,iout,ierr)
        ss3%jdotb = ss1%jdotb
     ELSE IF(ALLOCATED(ss2%jdotb)) THEN
        CALL ps_conformal("jdotb",ss2%jdotb,ss3%jdotb,iout,ierr)
        ss3%jdotb = ss2%jdotb
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN

     CALL ps_diff("kccw_Bphi",ss1%kccw_Bphi,ss2%kccw_Bphi,icheck,iout,ierr)
     ss3%kccw_Bphi = ss1%kccw_Bphi
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN

     CALL ps_diff("kccw_Jphi",ss1%kccw_Jphi,ss2%kccw_Jphi,icheck,iout,ierr)
     ss3%kccw_Jphi = ss1%kccw_Jphi
  ENDIF

  IF(cclist(ps_cnum_IC).EQ.1) THEN

     CALL ps_diff("kdens_rfmin",ss1%kdens_rfmin,ss2%kdens_rfmin,icheck,iout,ierr)
     ss3%kdens_rfmin = ss1%kdens_rfmin
  ENDIF

  IF((ALLOCATED(ss3%kvolt_nbi)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%kvolt_nbi).AND.ALLOCATED(ss2%kvolt_nbi)) THEN
        CALL ps_conformal("kvolt_nbi",ss2%kvolt_nbi,ss3%kvolt_nbi,iout,ierr)
        CALL ps_conformal("kvolt_nbi",ss1%kvolt_nbi,ss3%kvolt_nbi,iout,ierr)
        if(ierr.eq.0) ss3%kvolt_nbi = w1*ss1%kvolt_nbi + (1.0_rspec-w1)*ss2%kvolt_nbi
     ELSE IF(ALLOCATED(ss1%kvolt_nbi)) THEN
        CALL ps_conformal("kvolt_nbi",ss1%kvolt_nbi,ss3%kvolt_nbi,iout,ierr)
        ss3%kvolt_nbi = ss1%kvolt_nbi
     ELSE IF(ALLOCATED(ss2%kvolt_nbi)) THEN
        CALL ps_conformal("kvolt_nbi",ss2%kvolt_nbi,ss3%kvolt_nbi,iout,ierr)
        ss3%kvolt_nbi = ss2%kvolt_nbi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%kyrhos_drbm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%kyrhos_drbm).AND.ALLOCATED(ss2%kyrhos_drbm)) THEN
        CALL ps_conformal("kyrhos_drbm",ss2%kyrhos_drbm,ss3%kyrhos_drbm,iout,ierr)
        CALL ps_conformal("kyrhos_drbm",ss1%kyrhos_drbm,ss3%kyrhos_drbm,iout,ierr)
        if(ierr.eq.0) ss3%kyrhos_drbm = w1*ss1%kyrhos_drbm + (1.0_rspec-w1)*ss2%kyrhos_drbm
     ELSE IF(ALLOCATED(ss1%kyrhos_drbm)) THEN
        CALL ps_conformal("kyrhos_drbm",ss1%kyrhos_drbm,ss3%kyrhos_drbm,iout,ierr)
        ss3%kyrhos_drbm = ss1%kyrhos_drbm
     ELSE IF(ALLOCATED(ss2%kyrhos_drbm)) THEN
        CALL ps_conformal("kyrhos_drbm",ss2%kyrhos_drbm,ss3%kyrhos_drbm,iout,ierr)
        ss3%kyrhos_drbm = ss2%kyrhos_drbm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%kyrhos_etg)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%kyrhos_etg).AND.ALLOCATED(ss2%kyrhos_etg)) THEN
        CALL ps_conformal("kyrhos_etg",ss2%kyrhos_etg,ss3%kyrhos_etg,iout,ierr)
        CALL ps_conformal("kyrhos_etg",ss1%kyrhos_etg,ss3%kyrhos_etg,iout,ierr)
        if(ierr.eq.0) ss3%kyrhos_etg = w1*ss1%kyrhos_etg + (1.0_rspec-w1)*ss2%kyrhos_etg
     ELSE IF(ALLOCATED(ss1%kyrhos_etg)) THEN
        CALL ps_conformal("kyrhos_etg",ss1%kyrhos_etg,ss3%kyrhos_etg,iout,ierr)
        ss3%kyrhos_etg = ss1%kyrhos_etg
     ELSE IF(ALLOCATED(ss2%kyrhos_etg)) THEN
        CALL ps_conformal("kyrhos_etg",ss2%kyrhos_etg,ss3%kyrhos_etg,iout,ierr)
        ss3%kyrhos_etg = ss2%kyrhos_etg
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%kyrhos_mtm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%kyrhos_mtm).AND.ALLOCATED(ss2%kyrhos_mtm)) THEN
        CALL ps_conformal("kyrhos_mtm",ss2%kyrhos_mtm,ss3%kyrhos_mtm,iout,ierr)
        CALL ps_conformal("kyrhos_mtm",ss1%kyrhos_mtm,ss3%kyrhos_mtm,iout,ierr)
        if(ierr.eq.0) ss3%kyrhos_mtm = w1*ss1%kyrhos_mtm + (1.0_rspec-w1)*ss2%kyrhos_mtm
     ELSE IF(ALLOCATED(ss1%kyrhos_mtm)) THEN
        CALL ps_conformal("kyrhos_mtm",ss1%kyrhos_mtm,ss3%kyrhos_mtm,iout,ierr)
        ss3%kyrhos_mtm = ss1%kyrhos_mtm
     ELSE IF(ALLOCATED(ss2%kyrhos_mtm)) THEN
        CALL ps_conformal("kyrhos_mtm",ss2%kyrhos_mtm,ss3%kyrhos_mtm,iout,ierr)
        ss3%kyrhos_mtm = ss2%kyrhos_mtm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%kyspectrum_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%kyspectrum_tglf).AND.ALLOCATED(ss2%kyspectrum_tglf)) THEN
        CALL ps_conformal("kyspectrum_tglf",ss2%kyspectrum_tglf,ss3%kyspectrum_tglf,iout,ierr)
        CALL ps_conformal("kyspectrum_tglf",ss1%kyspectrum_tglf,ss3%kyspectrum_tglf,iout,ierr)
        if(ierr.eq.0) ss3%kyspectrum_tglf = w1*ss1%kyspectrum_tglf + (1.0_rspec-w1)*ss2%kyspectrum_tglf
     ELSE IF(ALLOCATED(ss1%kyspectrum_tglf)) THEN
        CALL ps_conformal("kyspectrum_tglf",ss1%kyspectrum_tglf,ss3%kyspectrum_tglf,iout,ierr)
        ss3%kyspectrum_tglf = ss1%kyspectrum_tglf
     ELSE IF(ALLOCATED(ss2%kyspectrum_tglf)) THEN
        CALL ps_conformal("kyspectrum_tglf",ss2%kyspectrum_tglf,ss3%kyspectrum_tglf,iout,ierr)
        ss3%kyspectrum_tglf = ss2%kyspectrum_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Lbscap)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%Lbscap).AND.ALLOCATED(ss2%Lbscap)) THEN
        CALL ps_diff("Lbscap",ss1%Lbscap,ss2%Lbscap,icheck,iout,ierr)
        CALL ps_conformal("Lbscap",ss1%Lbscap,ss3%Lbscap,iout,ierr)
        if(ierr.eq.0) ss3%Lbscap = ss1%Lbscap
     ELSE IF(ALLOCATED(ss1%Lbscap)) THEN
        CALL ps_conformal("Lbscap",ss1%Lbscap,ss3%Lbscap,iout,ierr)
        ss3%Lbscap = ss1%Lbscap
     ELSE IF(ALLOCATED(ss2%Lbscap)) THEN
        CALL ps_conformal("Lbscap",ss2%Lbscap,ss3%Lbscap,iout,ierr)
        ss3%Lbscap = ss2%Lbscap
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Lbscap2)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%Lbscap2).AND.ALLOCATED(ss2%Lbscap2)) THEN
        CALL ps_diff("Lbscap2",ss1%Lbscap2,ss2%Lbscap2,icheck,iout,ierr)
        CALL ps_conformal("Lbscap2",ss1%Lbscap2,ss3%Lbscap2,iout,ierr)
        if(ierr.eq.0) ss3%Lbscap2 = ss1%Lbscap2
     ELSE IF(ALLOCATED(ss1%Lbscap2)) THEN
        CALL ps_conformal("Lbscap2",ss1%Lbscap2,ss3%Lbscap2,iout,ierr)
        ss3%Lbscap2 = ss1%Lbscap2
     ELSE IF(ALLOCATED(ss2%Lbscap2)) THEN
        CALL ps_conformal("Lbscap2",ss2%Lbscap2,ss3%Lbscap2,iout,ierr)
        ss3%Lbscap2 = ss2%Lbscap2
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Lbsctan)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%Lbsctan).AND.ALLOCATED(ss2%Lbsctan)) THEN
        CALL ps_diff("Lbsctan",ss1%Lbsctan,ss2%Lbsctan,icheck,iout,ierr)
        CALL ps_conformal("Lbsctan",ss1%Lbsctan,ss3%Lbsctan,iout,ierr)
        if(ierr.eq.0) ss3%Lbsctan = ss1%Lbsctan
     ELSE IF(ALLOCATED(ss1%Lbsctan)) THEN
        CALL ps_conformal("Lbsctan",ss1%Lbsctan,ss3%Lbsctan,iout,ierr)
        ss3%Lbsctan = ss1%Lbsctan
     ELSE IF(ALLOCATED(ss2%Lbsctan)) THEN
        CALL ps_conformal("Lbsctan",ss2%Lbsctan,ss3%Lbsctan,iout,ierr)
        ss3%Lbsctan = ss2%Lbsctan
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%lhrf_src_name)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%lhrf_src_name).AND.ALLOCATED(ss2%lhrf_src_name)) THEN
        CALL ps_diff("lhrf_src_name",ss1%lhrf_src_name,ss2%lhrf_src_name,icheck,iout,ierr)
        CALL ps_conformal("lhrf_src_name",ss1%lhrf_src_name,ss3%lhrf_src_name,iout,ierr)
        if(ierr.eq.0) ss3%lhrf_src_name = ss1%lhrf_src_name
     ELSE IF(ALLOCATED(ss1%lhrf_src_name)) THEN
        CALL ps_conformal("lhrf_src_name",ss1%lhrf_src_name,ss3%lhrf_src_name,iout,ierr)
        ss3%lhrf_src_name = ss1%lhrf_src_name
     ELSE IF(ALLOCATED(ss2%lhrf_src_name)) THEN
        CALL ps_conformal("lhrf_src_name",ss2%lhrf_src_name,ss3%lhrf_src_name,iout,ierr)
        ss3%lhrf_src_name = ss2%lhrf_src_name
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_LH).EQ.1) THEN

     CALL ps_diff("LH_Code_Info",ss1%LH_Code_Info,ss2%LH_Code_Info,icheck,iout,ierr)
     ss3%LH_Code_Info = ss1%LH_Code_Info
  ENDIF

  IF(cclist(ps_cnum_LH).EQ.1) THEN

     CALL ps_diff("LH_Data_Info",ss1%LH_Data_Info,ss2%LH_Data_Info,icheck,iout,ierr)
     ss3%LH_Data_Info = ss1%LH_Data_Info
  ENDIF

  IF(cclist(ps_cnum_LMHD).EQ.1) THEN

     CALL ps_diff("LMHD_eq_code",ss1%LMHD_eq_code,ss2%LMHD_eq_code,icheck,iout,ierr)
     ss3%LMHD_eq_code = ss1%LMHD_eq_code
  ENDIF

  IF(cclist(ps_cnum_LMHD).EQ.1) THEN

     CALL ps_diff("LMHD_status",ss1%LMHD_status,ss2%LMHD_status,icheck,iout,ierr)
     ss3%LMHD_status = ss1%LMHD_status
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("lock_machine_descr",ss1%lock_machine_descr,ss2%lock_machine_descr,icheck,iout,ierr)
     ss3%lock_machine_descr = ss1%lock_machine_descr
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("lock_shot_config",ss1%lock_shot_config,ss2%lock_shot_config,icheck,iout,ierr)
     ss3%lock_shot_config = ss1%lock_shot_config
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("lock_sim_init",ss1%lock_sim_init,ss2%lock_sim_init,icheck,iout,ierr)
     ss3%lock_sim_init = ss1%lock_sim_init
  ENDIF

  IF((ALLOCATED(ss3%Lpol)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Lpol).AND.ALLOCATED(ss2%Lpol)) THEN
        CALL ps_conformal("Lpol",ss2%Lpol,ss3%Lpol,iout,ierr)
        CALL ps_conformal("Lpol",ss1%Lpol,ss3%Lpol,iout,ierr)
        if(ierr.eq.0) ss3%Lpol = w1*ss1%Lpol + (1.0_rspec-w1)*ss2%Lpol
     ELSE IF(ALLOCATED(ss1%Lpol)) THEN
        CALL ps_conformal("Lpol",ss1%Lpol,ss3%Lpol,iout,ierr)
        ss3%Lpol = ss1%Lpol
     ELSE IF(ALLOCATED(ss2%Lpol)) THEN
        CALL ps_conformal("Lpol",ss2%Lpol,ss3%Lpol,iout,ierr)
        ss3%Lpol = ss2%Lpol
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Mercier_stable)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%Mercier_stable).AND.ALLOCATED(ss2%Mercier_stable)) THEN
        CALL ps_diff("Mercier_stable",ss1%Mercier_stable,ss2%Mercier_stable,icheck,iout,ierr)
        CALL ps_conformal("Mercier_stable",ss1%Mercier_stable,ss3%Mercier_stable,iout,ierr)
        if(ierr.eq.0) ss3%Mercier_stable = ss1%Mercier_stable
     ELSE IF(ALLOCATED(ss1%Mercier_stable)) THEN
        CALL ps_conformal("Mercier_stable",ss1%Mercier_stable,ss3%Mercier_stable,iout,ierr)
        ss3%Mercier_stable = ss1%Mercier_stable
     ELSE IF(ALLOCATED(ss2%Mercier_stable)) THEN
        CALL ps_conformal("Mercier_stable",ss2%Mercier_stable,ss3%Mercier_stable,iout,ierr)
        ss3%Mercier_stable = ss2%Mercier_stable
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_LMHD).EQ.1) THEN

     CALL ps_diff("MHD_eq_status",ss1%MHD_eq_status,ss2%MHD_eq_status,icheck,iout,ierr)
     ss3%MHD_eq_status = ss1%MHD_eq_status
  ENDIF

  IF(ALLOCATED(ss3%m_ALL)) THEN
     IF(ALLOCATED(ss1%m_ALL).AND.ALLOCATED(ss2%m_ALL)) THEN
        CALL ps_diff("m_ALL",ss1%m_ALL,ss2%m_ALL,ps_error,iout,ierr)
        CALL ps_conformal("m_ALL",ss1%m_ALL,ss3%m_ALL,iout,ierr)
        if(ierr.eq.0) ss3%m_ALL = ss1%m_ALL
     ELSE IF(ALLOCATED(ss1%m_ALL)) THEN
        CALL ps_conformal("m_ALL",ss1%m_ALL,ss3%m_ALL,iout,ierr)
        ss3%m_ALL = ss1%m_ALL
     ELSE IF(ALLOCATED(ss2%m_ALL)) THEN
        CALL ps_conformal("m_ALL",ss2%m_ALL,ss3%m_ALL,iout,ierr)
        ss3%m_ALL = ss2%m_ALL
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%m_ALLA)) THEN
     IF(ALLOCATED(ss1%m_ALLA).AND.ALLOCATED(ss2%m_ALLA)) THEN
        CALL ps_diff("m_ALLA",ss1%m_ALLA,ss2%m_ALLA,ps_error,iout,ierr)
        CALL ps_conformal("m_ALLA",ss1%m_ALLA,ss3%m_ALLA,iout,ierr)
        if(ierr.eq.0) ss3%m_ALLA = ss1%m_ALLA
     ELSE IF(ALLOCATED(ss1%m_ALLA)) THEN
        CALL ps_conformal("m_ALLA",ss1%m_ALLA,ss3%m_ALLA,iout,ierr)
        ss3%m_ALLA = ss1%m_ALLA
     ELSE IF(ALLOCATED(ss2%m_ALLA)) THEN
        CALL ps_conformal("m_ALLA",ss2%m_ALLA,ss3%m_ALLA,iout,ierr)
        ss3%m_ALLA = ss2%m_ALLA
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%m_RFMIN)) THEN
     IF(ALLOCATED(ss1%m_RFMIN).AND.ALLOCATED(ss2%m_RFMIN)) THEN
        CALL ps_diff("m_RFMIN",ss1%m_RFMIN,ss2%m_RFMIN,ps_error,iout,ierr)
        CALL ps_conformal("m_RFMIN",ss1%m_RFMIN,ss3%m_RFMIN,iout,ierr)
        if(ierr.eq.0) ss3%m_RFMIN = ss1%m_RFMIN
     ELSE IF(ALLOCATED(ss1%m_RFMIN)) THEN
        CALL ps_conformal("m_RFMIN",ss1%m_RFMIN,ss3%m_RFMIN,iout,ierr)
        ss3%m_RFMIN = ss1%m_RFMIN
     ELSE IF(ALLOCATED(ss2%m_RFMIN)) THEN
        CALL ps_conformal("m_RFMIN",ss2%m_RFMIN,ss3%m_RFMIN,iout,ierr)
        ss3%m_RFMIN = ss2%m_RFMIN
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%m_S)) THEN
     IF(ALLOCATED(ss1%m_S).AND.ALLOCATED(ss2%m_S)) THEN
        CALL ps_diff("m_S",ss1%m_S,ss2%m_S,ps_error,iout,ierr)
        CALL ps_conformal("m_S",ss1%m_S,ss3%m_S,iout,ierr)
        if(ierr.eq.0) ss3%m_S = ss1%m_S
     ELSE IF(ALLOCATED(ss1%m_S)) THEN
        CALL ps_conformal("m_S",ss1%m_S,ss3%m_S,iout,ierr)
        ss3%m_S = ss1%m_S
     ELSE IF(ALLOCATED(ss2%m_S)) THEN
        CALL ps_conformal("m_S",ss2%m_S,ss3%m_S,iout,ierr)
        ss3%m_S = ss2%m_S
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%m_SA)) THEN
     IF(ALLOCATED(ss1%m_SA).AND.ALLOCATED(ss2%m_SA)) THEN
        CALL ps_diff("m_SA",ss1%m_SA,ss2%m_SA,ps_error,iout,ierr)
        CALL ps_conformal("m_SA",ss1%m_SA,ss3%m_SA,iout,ierr)
        if(ierr.eq.0) ss3%m_SA = ss1%m_SA
     ELSE IF(ALLOCATED(ss1%m_SA)) THEN
        CALL ps_conformal("m_SA",ss1%m_SA,ss3%m_SA,iout,ierr)
        ss3%m_SA = ss1%m_SA
     ELSE IF(ALLOCATED(ss2%m_SA)) THEN
        CALL ps_conformal("m_SA",ss2%m_SA,ss3%m_SA,iout,ierr)
        ss3%m_SA = ss2%m_SA
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%m_SFUS)) THEN
     IF(ALLOCATED(ss1%m_SFUS).AND.ALLOCATED(ss2%m_SFUS)) THEN
        CALL ps_diff("m_SFUS",ss1%m_SFUS,ss2%m_SFUS,ps_error,iout,ierr)
        CALL ps_conformal("m_SFUS",ss1%m_SFUS,ss3%m_SFUS,iout,ierr)
        if(ierr.eq.0) ss3%m_SFUS = ss1%m_SFUS
     ELSE IF(ALLOCATED(ss1%m_SFUS)) THEN
        CALL ps_conformal("m_SFUS",ss1%m_SFUS,ss3%m_SFUS,iout,ierr)
        ss3%m_SFUS = ss1%m_SFUS
     ELSE IF(ALLOCATED(ss2%m_SFUS)) THEN
        CALL ps_conformal("m_SFUS",ss2%m_SFUS,ss3%m_SFUS,iout,ierr)
        ss3%m_SFUS = ss2%m_SFUS
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%m_SGAS)) THEN
     IF(ALLOCATED(ss1%m_SGAS).AND.ALLOCATED(ss2%m_SGAS)) THEN
        CALL ps_diff("m_SGAS",ss1%m_SGAS,ss2%m_SGAS,ps_error,iout,ierr)
        CALL ps_conformal("m_SGAS",ss1%m_SGAS,ss3%m_SGAS,iout,ierr)
        if(ierr.eq.0) ss3%m_SGAS = ss1%m_SGAS
     ELSE IF(ALLOCATED(ss1%m_SGAS)) THEN
        CALL ps_conformal("m_SGAS",ss1%m_SGAS,ss3%m_SGAS,iout,ierr)
        ss3%m_SGAS = ss1%m_SGAS
     ELSE IF(ALLOCATED(ss2%m_SGAS)) THEN
        CALL ps_conformal("m_SGAS",ss2%m_SGAS,ss3%m_SGAS,iout,ierr)
        ss3%m_SGAS = ss2%m_SGAS
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%m_SIMP0)) THEN
     IF(ALLOCATED(ss1%m_SIMP0).AND.ALLOCATED(ss2%m_SIMP0)) THEN
        CALL ps_diff("m_SIMP0",ss1%m_SIMP0,ss2%m_SIMP0,ps_error,iout,ierr)
        CALL ps_conformal("m_SIMP0",ss1%m_SIMP0,ss3%m_SIMP0,iout,ierr)
        if(ierr.eq.0) ss3%m_SIMP0 = ss1%m_SIMP0
     ELSE IF(ALLOCATED(ss1%m_SIMP0)) THEN
        CALL ps_conformal("m_SIMP0",ss1%m_SIMP0,ss3%m_SIMP0,iout,ierr)
        ss3%m_SIMP0 = ss1%m_SIMP0
     ELSE IF(ALLOCATED(ss2%m_SIMP0)) THEN
        CALL ps_conformal("m_SIMP0",ss2%m_SIMP0,ss3%m_SIMP0,iout,ierr)
        ss3%m_SIMP0 = ss2%m_SIMP0
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%m_SIMPI)) THEN
     IF(ALLOCATED(ss1%m_SIMPI).AND.ALLOCATED(ss2%m_SIMPI)) THEN
        CALL ps_diff("m_SIMPI",ss1%m_SIMPI,ss2%m_SIMPI,ps_error,iout,ierr)
        CALL ps_conformal("m_SIMPI",ss1%m_SIMPI,ss3%m_SIMPI,iout,ierr)
        if(ierr.eq.0) ss3%m_SIMPI = ss1%m_SIMPI
     ELSE IF(ALLOCATED(ss1%m_SIMPI)) THEN
        CALL ps_conformal("m_SIMPI",ss1%m_SIMPI,ss3%m_SIMPI,iout,ierr)
        ss3%m_SIMPI = ss1%m_SIMPI
     ELSE IF(ALLOCATED(ss2%m_SIMPI)) THEN
        CALL ps_conformal("m_SIMPI",ss2%m_SIMPI,ss3%m_SIMPI,iout,ierr)
        ss3%m_SIMPI = ss2%m_SIMPI
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%m_SNBI)) THEN
     IF(ALLOCATED(ss1%m_SNBI).AND.ALLOCATED(ss2%m_SNBI)) THEN
        CALL ps_diff("m_SNBI",ss1%m_SNBI,ss2%m_SNBI,ps_error,iout,ierr)
        CALL ps_conformal("m_SNBI",ss1%m_SNBI,ss3%m_SNBI,iout,ierr)
        if(ierr.eq.0) ss3%m_SNBI = ss1%m_SNBI
     ELSE IF(ALLOCATED(ss1%m_SNBI)) THEN
        CALL ps_conformal("m_SNBI",ss1%m_SNBI,ss3%m_SNBI,iout,ierr)
        ss3%m_SNBI = ss1%m_SNBI
     ELSE IF(ALLOCATED(ss2%m_SNBI)) THEN
        CALL ps_conformal("m_SNBI",ss2%m_SNBI,ss3%m_SNBI,iout,ierr)
        ss3%m_SNBI = ss2%m_SNBI
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%n0norm)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%n0norm).AND.ALLOCATED(ss2%n0norm)) THEN
        CALL ps_conformal("n0norm",ss2%n0norm,ss3%n0norm,iout,ierr)
        CALL ps_conformal("n0norm",ss1%n0norm,ss3%n0norm,iout,ierr)
        if(ierr.eq.0) ss3%n0norm = w1*ss1%n0norm + (1.0_rspec-w1)*ss2%n0norm
     ELSE IF(ALLOCATED(ss1%n0norm)) THEN
        CALL ps_conformal("n0norm",ss1%n0norm,ss3%n0norm,iout,ierr)
        ss3%n0norm = ss1%n0norm
     ELSE IF(ALLOCATED(ss2%n0norm)) THEN
        CALL ps_conformal("n0norm",ss2%n0norm,ss3%n0norm,iout,ierr)
        ss3%n0norm = ss2%n0norm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%n0_halo)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%n0_halo).AND.ALLOCATED(ss2%n0_halo)) THEN
        CALL ps_conformal("n0_halo",ss2%n0_halo,ss3%n0_halo,iout,ierr)
        CALL ps_conformal("n0_halo",ss1%n0_halo,ss3%n0_halo,iout,ierr)
        if(ierr.eq.0) ss3%n0_halo = w1*ss1%n0_halo + (1.0_rspec-w1)*ss2%n0_halo
     ELSE IF(ALLOCATED(ss1%n0_halo)) THEN
        CALL ps_conformal("n0_halo",ss1%n0_halo,ss3%n0_halo,iout,ierr)
        ss3%n0_halo = ss1%n0_halo
     ELSE IF(ALLOCATED(ss2%n0_halo)) THEN
        CALL ps_conformal("n0_halo",ss2%n0_halo,ss3%n0_halo,iout,ierr)
        ss3%n0_halo = ss2%n0_halo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%n0_reco)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%n0_reco).AND.ALLOCATED(ss2%n0_reco)) THEN
        CALL ps_conformal("n0_reco",ss2%n0_reco,ss3%n0_reco,iout,ierr)
        CALL ps_conformal("n0_reco",ss1%n0_reco,ss3%n0_reco,iout,ierr)
        if(ierr.eq.0) ss3%n0_reco = w1*ss1%n0_reco + (1.0_rspec-w1)*ss2%n0_reco
     ELSE IF(ALLOCATED(ss1%n0_reco)) THEN
        CALL ps_conformal("n0_reco",ss1%n0_reco,ss3%n0_reco,iout,ierr)
        ss3%n0_reco = ss1%n0_reco
     ELSE IF(ALLOCATED(ss2%n0_reco)) THEN
        CALL ps_conformal("n0_reco",ss2%n0_reco,ss3%n0_reco,iout,ierr)
        ss3%n0_reco = ss2%n0_reco
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nbap2_shape)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%nbap2_shape).AND.ALLOCATED(ss2%nbap2_shape)) THEN
        CALL ps_diff("nbap2_shape",ss1%nbap2_shape,ss2%nbap2_shape,icheck,iout,ierr)
        CALL ps_conformal("nbap2_shape",ss1%nbap2_shape,ss3%nbap2_shape,iout,ierr)
        if(ierr.eq.0) ss3%nbap2_shape = ss1%nbap2_shape
     ELSE IF(ALLOCATED(ss1%nbap2_shape)) THEN
        CALL ps_conformal("nbap2_shape",ss1%nbap2_shape,ss3%nbap2_shape,iout,ierr)
        ss3%nbap2_shape = ss1%nbap2_shape
     ELSE IF(ALLOCATED(ss2%nbap2_shape)) THEN
        CALL ps_conformal("nbap2_shape",ss2%nbap2_shape,ss3%nbap2_shape,iout,ierr)
        ss3%nbap2_shape = ss2%nbap2_shape
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nbap_shape)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%nbap_shape).AND.ALLOCATED(ss2%nbap_shape)) THEN
        CALL ps_diff("nbap_shape",ss1%nbap_shape,ss2%nbap_shape,icheck,iout,ierr)
        CALL ps_conformal("nbap_shape",ss1%nbap_shape,ss3%nbap_shape,iout,ierr)
        if(ierr.eq.0) ss3%nbap_shape = ss1%nbap_shape
     ELSE IF(ALLOCATED(ss1%nbap_shape)) THEN
        CALL ps_conformal("nbap_shape",ss1%nbap_shape,ss3%nbap_shape,iout,ierr)
        ss3%nbap_shape = ss1%nbap_shape
     ELSE IF(ALLOCATED(ss2%nbap_shape)) THEN
        CALL ps_conformal("nbap_shape",ss2%nbap_shape,ss3%nbap_shape,iout,ierr)
        ss3%nbap_shape = ss2%nbap_shape
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_merge_3
