! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_contents_profs_1(itype,icomponent,ss,w,isize)


  ! fill window structure with requested table of contents

  USE plasma_state_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: itype   ! type of table of contents
                                 ! values such as ps_grid_dims or ps_profiles

  INTEGER, INTENT(IN) :: icomponent(ps_ccount) ! component flags
             ! icomponent(j).eq.1 => component j active
             ! e.g. icomponent(ps_cnum_EQ).eq.1 => EQ component is active

  TYPE (plasma_state) :: ss      ! state examined
  TYPE (ps_list_window) :: w     ! list window structure filled

  INTEGER, INTENT(INOUT) :: isize  ! incremented size...

  !----------------

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_neo
        w%list_elems(isize) = "chii_neo"
        w%elem_descr(isize) = "Ti-equation, neoclassical diffusivity from NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chii_neo)) THEN
           IF(ss%hash_codes(ps_phash_chii_neo).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_ntm
        w%list_elems(isize) = "chii_ntm"
        w%elem_descr(isize) = "Ti-equation, neoclassical diffusivity from NTM model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chii_ntm)) THEN
           IF(ss%hash_codes(ps_phash_chii_ntm).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_tglf
        w%list_elems(isize) = "chii_tglf"
        w%elem_descr(isize) = "Ti-equation, diffusivity from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chii_tglf)) THEN
           IF(ss%hash_codes(ps_phash_chii_tglf).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_trans
        w%list_elems(isize) = "chii_trans"
        w%elem_descr(isize) = "ion thermal diffusivity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%chii_trans)) THEN
           IF(ss%hash_codes(ps_phash_chii_trans).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_transcat
        w%list_elems(isize) = "chii_transcat"
        w%elem_descr(isize) = "Ion energy diffusivity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho,ntrcat)"
        IF(ALLOCATED(ss%chii_transcat)) THEN
           IF(ss%hash_codes(ps_phash_chii_transcat).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_w19
        w%list_elems(isize) = "chii_w19"
        w%elem_descr(isize) = "Ti-equation, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chii_w19)) THEN
           IF(ss%hash_codes(ps_phash_chii_w19).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chimo_trans
        w%list_elems(isize) = "chimo_trans"
        w%elem_descr(isize) = "angular momentum diffusivity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%chimo_trans)) THEN
           IF(ss%hash_codes(ps_phash_chimo_trans).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chimo_transcat
        w%list_elems(isize) = "chimo_transcat"
        w%elem_descr(isize) = "Angular momentum diffusivity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho,ntrcat)"
        IF(ALLOCATED(ss%chimo_transcat)) THEN
           IF(ss%hash_codes(ps_phash_chimo_transcat).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_anom
        w%list_elems(isize) = "chiphi_anom"
        w%elem_descr(isize) = "momentum-equation, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chiphi_anom)) THEN
           IF(ss%hash_codes(ps_phash_chiphi_anom).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_drbm
        w%list_elems(isize) = "chiphi_drbm"
        w%elem_descr(isize) = "momentum-equation, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chiphi_drbm)) THEN
           IF(ss%hash_codes(ps_phash_chiphi_drbm).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_glf
        w%list_elems(isize) = "chiphi_glf"
        w%elem_descr(isize) = "momentum-equation, diffusivity from GLF23 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chiphi_glf)) THEN
           IF(ss%hash_codes(ps_phash_chiphi_glf).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_gtcneo
        w%list_elems(isize) = "chiphi_gtcneo"
        w%elem_descr(isize) = "momentum-equation, neoclassical diffusivity from GTC_NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chiphi_gtcneo)) THEN
           IF(ss%hash_codes(ps_phash_chiphi_gtcneo).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_mmm71
        w%list_elems(isize) = "chiphi_mmm71"
        w%elem_descr(isize) = "momentum-equation, diffusivity term, toroidal"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chiphi_mmm71)) THEN
           IF(ss%hash_codes(ps_phash_chiphi_mmm71).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_neo
        w%list_elems(isize) = "chiphi_neo"
        w%elem_descr(isize) = "momentum-equation, neoclassical diffusivity from NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chiphi_neo)) THEN
           IF(ss%hash_codes(ps_phash_chiphi_neo).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_tglf
        w%list_elems(isize) = "chiphi_tglf"
        w%elem_descr(isize) = "momentum-equation, diffusivity from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chiphi_tglf)) THEN
           IF(ss%hash_codes(ps_phash_chiphi_tglf).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_w19
        w%list_elems(isize) = "chiphi_w19"
        w%elem_descr(isize) = "momentum-equation, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chiphi_w19)) THEN
           IF(ss%hash_codes(ps_phash_chiphi_w19).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chitheta_mmm71
        w%list_elems(isize) = "chitheta_mmm71"
        w%elem_descr(isize) = "momentum-equation, diffusivity term, poloidal"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chitheta_mmm71)) THEN
           IF(ss%hash_codes(ps_phash_chitheta_mmm71).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curbeam
        w%list_elems(isize) = "curbeam"
        w%elem_descr(isize) = "beam ion driven current (shielded)"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%curbeam)) THEN
           IF(ss%hash_codes(ps_phash_curbeam).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DA"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curech
        w%list_elems(isize) = "curech"
        w%elem_descr(isize) = "ECH current drive"
        w%component(isize) = "EC"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho_ecrf-1)"
        IF(ALLOCATED(ss%curech)) THEN
           IF(ss%hash_codes(ps_phash_curech).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DA"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curech_src
        w%list_elems(isize) = "curech_src"
        w%elem_descr(isize) = "ECH current drive (by antenna)"
        w%component(isize) = "EC"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho_ecrf-1,necrf_src)"
        IF(ALLOCATED(ss%curech_src)) THEN
           IF(ss%hash_codes(ps_phash_curech_src).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DA"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curfusn
        w%list_elems(isize) = "curfusn"
        w%elem_descr(isize) = "fusion ion driven current (shielded)"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho_fus-1)"
        IF(ALLOCATED(ss%curfusn)) THEN
           IF(ss%hash_codes(ps_phash_curfusn).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DA"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curich
        w%list_elems(isize) = "curich"
        w%elem_descr(isize) = "total driven current (all IC srcs)"
        w%component(isize) = "IC"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho_icrf-1)"
        IF(ALLOCATED(ss%curich)) THEN
           IF(ss%hash_codes(ps_phash_curich).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DA"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curlh
        w%list_elems(isize) = "curlh"
        w%elem_descr(isize) = "LH current drive"
        w%component(isize) = "LH"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho_lhrf-1)"
        IF(ALLOCATED(ss%curlh)) THEN
           IF(ss%hash_codes(ps_phash_curlh).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DA"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curlh_src
        w%list_elems(isize) = "curlh_src"
        w%elem_descr(isize) = "LH current drive (by antenna)"
        w%component(isize) = "LH"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho_lhrf-1,nlhrf_src)"
        IF(ALLOCATED(ss%curlh_src)) THEN
           IF(ss%hash_codes(ps_phash_curlh_src).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DA"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curmino
        w%list_elems(isize) = "curmino"
        w%elem_descr(isize) = "minority ion driven cur (shielded)."
        w%component(isize) = "IC"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho_icrf-1)"
        IF(ALLOCATED(ss%curmino)) THEN
           IF(ss%hash_codes(ps_phash_curmino).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DA"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curr_bootstrap
        w%list_elems(isize) = "curr_bootstrap"
        w%elem_descr(isize) = "Neoclassical bootstrap current"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%curr_bootstrap)) THEN
           IF(ss%hash_codes(ps_phash_curr_bootstrap).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DA"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curr_ohmic
        w%list_elems(isize) = "curr_ohmic"
        w%elem_descr(isize) = "Ohmic current"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%curr_ohmic)) THEN
           IF(ss%hash_codes(ps_phash_curr_ohmic).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DA"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curt
        w%list_elems(isize) = "curt"
        w%elem_descr(isize) = "total enclosed toroidal current"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho_eq)"
        IF(ALLOCATED(ss%curt)) THEN
           IF(ss%hash_codes(ps_phash_curt).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "SPLINE_00"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! cur_rw
        w%list_elems(isize) = "cur_rw"
        w%elem_descr(isize) = "current carried by fast electrons"
        w%component(isize) = "RUNAWAY"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho_rw-1)"
        IF(ALLOCATED(ss%cur_rw)) THEN
           IF(ss%hash_codes(ps_phash_cur_rw).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DA"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! db_mtm
        w%list_elems(isize) = "db_mtm"
        w%elem_descr(isize) = "dB/B from MTM model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%db_mtm)) THEN
           IF(ss%hash_codes(ps_phash_db_mtm).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! dcur_rw_dVloop
        w%list_elems(isize) = "dcur_rw_dVloop"
        w%elem_descr(isize) = "sensitivity to loop volts"
        w%component(isize) = "RUNAWAY"
        w%elem_units(isize) = "A/Volt"
        w%c256val(isize) = "(nrho_rw-1)"
        IF(ALLOCATED(ss%dcur_rw_dVloop)) THEN
           IF(ss%hash_codes(ps_phash_dcur_rw_dVloop).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DA"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difb_fusi
        w%list_elems(isize) = "difb_fusi"
        w%elem_descr(isize) = "fusion ion anomalous diffusivity"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anom)"
        IF(ALLOCATED(ss%difb_fusi)) THEN
           IF(ss%hash_codes(ps_phash_difb_fusi).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difb_nbi
        w%list_elems(isize) = "difb_nbi"
        w%elem_descr(isize) = "beam ion anomalous diffusivity"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anom)"
        IF(ALLOCATED(ss%difb_nbi)) THEN
           IF(ss%hash_codes(ps_phash_difb_nbi).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difb_rfmi
        w%list_elems(isize) = "difb_rfmi"
        w%elem_descr(isize) = "RF minority ion anomalous diffusivity"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anom)"
        IF(ALLOCATED(ss%difb_rfmi)) THEN
           IF(ss%hash_codes(ps_phash_difb_rfmi).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! diff_trans
        w%list_elems(isize) = "diff_trans"
        w%elem_descr(isize) = "specie ptcl diffusivity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho,0:nspec_th)"
        IF(ALLOCATED(ss%diff_trans)) THEN
           IF(ss%hash_codes(ps_phash_diff_trans).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! diff_transcat
        w%list_elems(isize) = "diff_transcat"
        w%elem_descr(isize) = "specie diffusivity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho,0:nspec_th,ntrcat)"
        IF(ALLOCATED(ss%diff_transcat)) THEN
           IF(ss%hash_codes(ps_phash_diff_transcat).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difne_nclass
        w%list_elems(isize) = "difne_nclass"
        w%elem_descr(isize) = "electron-density-equations, neoclassical diffusivity from NCLASS model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%difne_nclass)) THEN
           IF(ss%hash_codes(ps_phash_difne_nclass).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF
END SUBROUTINE plasma_state_contents_profs_1
