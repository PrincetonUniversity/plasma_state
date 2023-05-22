! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_contents_profs_3(itype,icomponent,ss,w,isize)


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
        isize = isize + 1  ! freq_glf
        w%list_elems(isize) = "freq_glf"
        w%elem_descr(isize) = "frequency from GLF23 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%freq_glf)) THEN
           IF(ss%hash_codes(ps_phash_freq_glf).LT.0) THEN
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
        isize = isize + 1  ! freq_max_tglf
        w%list_elems(isize) = "freq_max_tglf"
        w%elem_descr(isize) = "frequency corresponding maximum growthrate from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%freq_max_tglf)) THEN
           IF(ss%hash_codes(ps_phash_freq_max_tglf).LT.0) THEN
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
        isize = isize + 1  ! freq_mmm71
        w%list_elems(isize) = "freq_mmm71"
        w%elem_descr(isize) = "frequency from mmm71 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_anomth,nky_mmm)"
        IF(ALLOCATED(ss%freq_mmm71)) THEN
           IF(ss%hash_codes(ps_phash_freq_mmm71).LT.0) THEN
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
        isize = isize + 1  ! freq_mtm
        w%list_elems(isize) = "freq_mtm"
        w%elem_descr(isize) = "frequency from MTM model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%freq_mtm)) THEN
           IF(ss%hash_codes(ps_phash_freq_mtm).LT.0) THEN
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
        isize = isize + 1  ! freq_tglf
        w%list_elems(isize) = "freq_tglf"
        w%elem_descr(isize) = "frequency from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%freq_tglf)) THEN
           IF(ss%hash_codes(ps_phash_freq_tglf).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gamma_nc
        w%list_elems(isize) = "gamma_nc"
        w%elem_descr(isize) = "NC gamma"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m^-1"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%gamma_nc)) THEN
           IF(ss%hash_codes(ps_phash_gamma_nc).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gb1
        w%list_elems(isize) = "gb1"
        w%elem_descr(isize) = "<|B|>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%gb1)) THEN
           IF(ss%hash_codes(ps_phash_gb1).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gb2
        w%list_elems(isize) = "gb2"
        w%elem_descr(isize) = "<B^2>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T^2"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%gb2)) THEN
           IF(ss%hash_codes(ps_phash_gb2).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gb2i
        w%list_elems(isize) = "gb2i"
        w%elem_descr(isize) = "<1/B^2>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T^-2"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%gb2i)) THEN
           IF(ss%hash_codes(ps_phash_gb2i).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gbr2
        w%list_elems(isize) = "gbr2"
        w%elem_descr(isize) = "<|B|*R^2>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T*m^2"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%gbr2)) THEN
           IF(ss%hash_codes(ps_phash_gbr2).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gncfb2h
        w%list_elems(isize) = "gncfb2h"
        w%elem_descr(isize) = "<(B^-2)*((1-H)^0.5-(1/3)*(1-H)^1.5)>; H=B/Bmax"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T^-2"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%gncfb2h)) THEN
           IF(ss%hash_codes(ps_phash_gncfb2h).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gncfh
        w%list_elems(isize) = "gncfh"
        w%elem_descr(isize) = "<H^-2*(1-SQRT(1-H)*(1+H/2))>; H=B/Bmax"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%gncfh)) THEN
           IF(ss%hash_codes(ps_phash_gncfh).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gr1
        w%list_elems(isize) = "gr1"
        w%elem_descr(isize) = "<R>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%gr1)) THEN
           IF(ss%hash_codes(ps_phash_gr1).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gr2
        w%list_elems(isize) = "gr2"
        w%elem_descr(isize) = "<R^2>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m^2"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%gr2)) THEN
           IF(ss%hash_codes(ps_phash_gr2).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gr2i
        w%list_elems(isize) = "gr2i"
        w%elem_descr(isize) = "<R^-2>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m^-2"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%gr2i)) THEN
           IF(ss%hash_codes(ps_phash_gr2i).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gr2rho2
        w%list_elems(isize) = "gr2rho2"
        w%elem_descr(isize) = "<R^2*|grad(rho)|^2>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%gr2rho2)) THEN
           IF(ss%hash_codes(ps_phash_gr2rho2).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gr3i
        w%list_elems(isize) = "gr3i"
        w%elem_descr(isize) = "<R^-3>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%gr3i)) THEN
           IF(ss%hash_codes(ps_phash_gr3i).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! grho1
        w%list_elems(isize) = "grho1"
        w%elem_descr(isize) = "<|grad(rho)|>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m^-1"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%grho1)) THEN
           IF(ss%hash_codes(ps_phash_grho1).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! grho2
        w%list_elems(isize) = "grho2"
        w%elem_descr(isize) = "<|grad(rho)|^2>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m^-2"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%grho2)) THEN
           IF(ss%hash_codes(ps_phash_grho2).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! grho2b2i
        w%list_elems(isize) = "grho2b2i"
        w%elem_descr(isize) = "<|grad(rho)|^2/B^2>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T^-2*m^-2"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%grho2b2i)) THEN
           IF(ss%hash_codes(ps_phash_grho2b2i).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! grho2r2i
        w%list_elems(isize) = "grho2r2i"
        w%elem_descr(isize) = "<|grad(rho)|^2/R^2>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m^-4"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%grho2r2i)) THEN
           IF(ss%hash_codes(ps_phash_grho2r2i).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! grho2r3i
        w%list_elems(isize) = "grho2r3i"
        w%elem_descr(isize) = "<|grad(rho)|^2/R^3>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m^-5"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%grho2r3i)) THEN
           IF(ss%hash_codes(ps_phash_grho2r3i).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gri
        w%list_elems(isize) = "gri"
        w%elem_descr(isize) = "<1/R>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m^-2"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%gri)) THEN
           IF(ss%hash_codes(ps_phash_gri).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! grirhoi
        w%list_elems(isize) = "grirhoi"
        w%elem_descr(isize) = "<1/(R*|grad(rho)|)>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%grirhoi)) THEN
           IF(ss%hash_codes(ps_phash_grirhoi).LT.0) THEN
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
        isize = isize + 1  ! growthrate_drbm
        w%list_elems(isize) = "growthrate_drbm"
        w%elem_descr(isize) = "growthrate from DRBM model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%growthrate_drbm)) THEN
           IF(ss%hash_codes(ps_phash_growthrate_drbm).LT.0) THEN
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
        isize = isize + 1  ! growthrate_etg
        w%list_elems(isize) = "growthrate_etg"
        w%elem_descr(isize) = "growthrate from ETG model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%growthrate_etg)) THEN
           IF(ss%hash_codes(ps_phash_growthrate_etg).LT.0) THEN
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
        isize = isize + 1  ! growthrate_glf
        w%list_elems(isize) = "growthrate_glf"
        w%elem_descr(isize) = "growthrate from GLF23 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%growthrate_glf)) THEN
           IF(ss%hash_codes(ps_phash_growthrate_glf).LT.0) THEN
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
        isize = isize + 1  ! growthrate_max_tglf
        w%list_elems(isize) = "growthrate_max_tglf"
        w%elem_descr(isize) = "maximum growthrate from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%growthrate_max_tglf)) THEN
           IF(ss%hash_codes(ps_phash_growthrate_max_tglf).LT.0) THEN
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
        isize = isize + 1  ! growthrate_mmm71
        w%list_elems(isize) = "growthrate_mmm71"
        w%elem_descr(isize) = "growthrate from mmm71 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_anomth,nky_mmm)"
        IF(ALLOCATED(ss%growthrate_mmm71)) THEN
           IF(ss%hash_codes(ps_phash_growthrate_mmm71).LT.0) THEN
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
        isize = isize + 1  ! growthrate_mtm
        w%list_elems(isize) = "growthrate_mtm"
        w%elem_descr(isize) = "growthrate from MTM model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%growthrate_mtm)) THEN
           IF(ss%hash_codes(ps_phash_growthrate_mtm).LT.0) THEN
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
        isize = isize + 1  ! growthrate_tglf
        w%list_elems(isize) = "growthrate_tglf"
        w%elem_descr(isize) = "growthrate from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%growthrate_tglf)) THEN
           IF(ss%hash_codes(ps_phash_growthrate_tglf).LT.0) THEN
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
        isize = isize + 1  ! growthrate_vpar
        w%list_elems(isize) = "growthrate_vpar"
        w%elem_descr(isize) = "growthrate from parallel velocity shear"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%growthrate_vpar)) THEN
           IF(ss%hash_codes(ps_phash_growthrate_vpar).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! g_eq
        w%list_elems(isize) = "g_eq"
        w%elem_descr(isize) = "equilibrium R*|B_phi|"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T*m"
        w%c256val(isize) = "(nrho_eq)"
        IF(ALLOCATED(ss%g_eq)) THEN
           IF(ss%hash_codes(ps_phash_g_eq).LT.0) THEN
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

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! iota
        w%list_elems(isize) = "iota"
        w%elem_descr(isize) = "iota profile (1/q)"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%iota)) THEN
           IF(ss%hash_codes(ps_phash_iota).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! jdotb
        w%list_elems(isize) = "jdotb"
        w%elem_descr(isize) = "<J.B>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "A*T/m^2"
        w%c256val(isize) = "(nrho_eq)"
        IF(ALLOCATED(ss%jdotb)) THEN
           IF(ss%hash_codes(ps_phash_jdotb).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "HERMITE_00"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! kyrhos_drbm
        w%list_elems(isize) = "kyrhos_drbm"
        w%elem_descr(isize) = "ky*rhos at max growth rate for DRBM model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%kyrhos_drbm)) THEN
           IF(ss%hash_codes(ps_phash_kyrhos_drbm).LT.0) THEN
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
        isize = isize + 1  ! kyrhos_etg
        w%list_elems(isize) = "kyrhos_etg"
        w%elem_descr(isize) = "ky*rhos at max growth rate for ETG model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%kyrhos_etg)) THEN
           IF(ss%hash_codes(ps_phash_kyrhos_etg).LT.0) THEN
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
END SUBROUTINE plasma_state_contents_profs_3
