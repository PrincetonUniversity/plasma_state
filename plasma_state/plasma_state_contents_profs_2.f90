! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_contents_profs_2(itype,icomponent,ss,w,isize)


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
        isize = isize + 1  ! difnh_nclass
        w%list_elems(isize) = "difnh_nclass"
        w%elem_descr(isize) = "main-hydrogenic-density-equations, neoclassical diffusivity from NCLASS model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%difnh_nclass)) THEN
           IF(ss%hash_codes(ps_phash_difnh_nclass).LT.0) THEN
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
        isize = isize + 1  ! difns_anom
        w%list_elems(isize) = "difns_anom"
        w%elem_descr(isize) = "density-equations, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%difns_anom)) THEN
           IF(ss%hash_codes(ps_phash_difns_anom).LT.0) THEN
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
        isize = isize + 1  ! difns_drbm
        w%list_elems(isize) = "difns_drbm"
        w%elem_descr(isize) = "density-equations, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%difns_drbm)) THEN
           IF(ss%hash_codes(ps_phash_difns_drbm).LT.0) THEN
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
        isize = isize + 1  ! difns_glf
        w%list_elems(isize) = "difns_glf"
        w%elem_descr(isize) = "density-equations, diffusivity from GLF23 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%difns_glf)) THEN
           IF(ss%hash_codes(ps_phash_difns_glf).LT.0) THEN
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
        isize = isize + 1  ! difns_gtcneo
        w%list_elems(isize) = "difns_gtcneo"
        w%elem_descr(isize) = "density-equations, neoclassical diffusivity from GTC_NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%difns_gtcneo)) THEN
           IF(ss%hash_codes(ps_phash_difns_gtcneo).LT.0) THEN
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
        isize = isize + 1  ! difns_mmm71
        w%list_elems(isize) = "difns_mmm71"
        w%elem_descr(isize) = "density-equations, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%difns_mmm71)) THEN
           IF(ss%hash_codes(ps_phash_difns_mmm71).LT.0) THEN
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
        isize = isize + 1  ! difns_nclass
        w%list_elems(isize) = "difns_nclass"
        w%elem_descr(isize) = "density-equations, neoclassical diffusivity from NCLASS model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%difns_nclass)) THEN
           IF(ss%hash_codes(ps_phash_difns_nclass).LT.0) THEN
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
        isize = isize + 1  ! difns_neo
        w%list_elems(isize) = "difns_neo"
        w%elem_descr(isize) = "density-equations, neoclassical diffusivity from NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%difns_neo)) THEN
           IF(ss%hash_codes(ps_phash_difns_neo).LT.0) THEN
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
        isize = isize + 1  ! difns_tglf
        w%list_elems(isize) = "difns_tglf"
        w%elem_descr(isize) = "density-equations, diffusivity from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%difns_tglf)) THEN
           IF(ss%hash_codes(ps_phash_difns_tglf).LT.0) THEN
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
        isize = isize + 1  ! difns_w19
        w%list_elems(isize) = "difns_w19"
        w%elem_descr(isize) = "density-equations, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%difns_w19)) THEN
           IF(ss%hash_codes(ps_phash_difns_w19).LT.0) THEN
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
        isize = isize + 1  ! difnx_nclass
        w%list_elems(isize) = "difnx_nclass"
        w%elem_descr(isize) = "main-impurity-density-equations, neoclassical diffusivity from NCLASS model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%difnx_nclass)) THEN
           IF(ss%hash_codes(ps_phash_difnx_nclass).LT.0) THEN
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
        isize = isize + 1  ! difnz_mmm71
        w%list_elems(isize) = "difnz_mmm71"
        w%elem_descr(isize) = "impurity ion diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%difnz_mmm71)) THEN
           IF(ss%hash_codes(ps_phash_difnz_mmm71).LT.0) THEN
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
        isize = isize + 1  ! dQedGrdte_tglf
        w%list_elems(isize) = "dQedGrdte_tglf"
        w%elem_descr(isize) = "Te flux gradient term from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "1/(m*s)"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%dQedGrdte_tglf)) THEN
           IF(ss%hash_codes(ps_phash_dQedGrdte_tglf).LT.0) THEN
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
        isize = isize + 1  ! dQedGrdti_tglf
        w%list_elems(isize) = "dQedGrdti_tglf"
        w%elem_descr(isize) = "Te flux gradient term from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "1/(m*s)"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%dQedGrdti_tglf)) THEN
           IF(ss%hash_codes(ps_phash_dQedGrdti_tglf).LT.0) THEN
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
        isize = isize + 1  ! dQidGrdte_tglf
        w%list_elems(isize) = "dQidGrdte_tglf"
        w%elem_descr(isize) = "Ti flux gradient term from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "1/(m*s)"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%dQidGrdte_tglf)) THEN
           IF(ss%hash_codes(ps_phash_dQidGrdte_tglf).LT.0) THEN
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
        isize = isize + 1  ! dQidGrdti_tglf
        w%list_elems(isize) = "dQidGrdti_tglf"
        w%elem_descr(isize) = "Ti flux gradient term from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "1/(m*s)"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%dQidGrdti_tglf)) THEN
           IF(ss%hash_codes(ps_phash_dQidGrdti_tglf).LT.0) THEN
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
        isize = isize + 1  ! ee_mobility_factor
        w%list_elems(isize) = "ee_mobility_factor"
        w%elem_descr(isize) = "chi_e/chi_i"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%ee_mobility_factor)) THEN
           IF(ss%hash_codes(ps_phash_ee_mobility_factor).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "HERMITE"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! elong
        w%list_elems(isize) = "elong"
        w%elem_descr(isize) = "elongation (b/a)"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%elong)) THEN
           IF(ss%hash_codes(ps_phash_elong).LT.0) THEN
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
        isize = isize + 1  ! eperp_beami
        w%list_elems(isize) = "eperp_beami"
        w%elem_descr(isize) = "beam species <Eperp>, lab frame"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho_nbi-1,nspec_beam)"
        IF(ALLOCATED(ss%eperp_beami)) THEN
           IF(ss%hash_codes(ps_phash_eperp_beami).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*NBEAMI"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! eperp_fusi
        w%list_elems(isize) = "eperp_fusi"
        w%elem_descr(isize) = "fusion ion <Eperp>, lab frame"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho_fus-1,nspec_fusion)"
        IF(ALLOCATED(ss%eperp_fusi)) THEN
           IF(ss%hash_codes(ps_phash_eperp_fusi).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*NFUSI"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! eperp_mini
        w%list_elems(isize) = "eperp_mini"
        w%elem_descr(isize) = "minority species <Eperp>, lab frame"
        w%component(isize) = "IC"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho_icrf-1,nspec_rfmin)"
        IF(ALLOCATED(ss%eperp_mini)) THEN
           IF(ss%hash_codes(ps_phash_eperp_mini).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*NMINI"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! eperp_rw
        w%list_elems(isize) = "eperp_rw"
        w%elem_descr(isize) = "runaway electron <Eperp>"
        w%component(isize) = "RUNAWAY"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho_rw-1)"
        IF(ALLOCATED(ss%eperp_rw)) THEN
           IF(ss%hash_codes(ps_phash_eperp_rw).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*NRW"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! epll_beami
        w%list_elems(isize) = "epll_beami"
        w%elem_descr(isize) = "beam species <Epll>, lab frame"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho_nbi-1,nspec_beam)"
        IF(ALLOCATED(ss%epll_beami)) THEN
           IF(ss%hash_codes(ps_phash_epll_beami).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*NBEAMI"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! epll_fusi
        w%list_elems(isize) = "epll_fusi"
        w%elem_descr(isize) = "fusion ion <Epll>, lab frame"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho_fus-1,nspec_fusion)"
        IF(ALLOCATED(ss%epll_fusi)) THEN
           IF(ss%hash_codes(ps_phash_epll_fusi).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*NFUSI"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! epll_mini
        w%list_elems(isize) = "epll_mini"
        w%elem_descr(isize) = "minority species <Epll>, lab frame"
        w%component(isize) = "IC"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho_icrf-1,nspec_rfmin)"
        IF(ALLOCATED(ss%epll_mini)) THEN
           IF(ss%hash_codes(ps_phash_epll_mini).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*NMINI"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! epll_rw
        w%list_elems(isize) = "epll_rw"
        w%elem_descr(isize) = "runaway electron <Epll>"
        w%component(isize) = "RUNAWAY"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho_rw-1)"
        IF(ALLOCATED(ss%epll_rw)) THEN
           IF(ss%hash_codes(ps_phash_epll_rw).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*NRW"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Epot
        w%list_elems(isize) = "Epot"
        w%elem_descr(isize) = "radial electrostatic potential"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%Epot)) THEN
           IF(ss%hash_codes(ps_phash_Epot).LT.0) THEN
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
        isize = isize + 1  ! eta_parallel
        w%list_elems(isize) = "eta_parallel"
        w%elem_descr(isize) = "parallel electrical resistivity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "ohm*m"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%eta_parallel)) THEN
           IF(ss%hash_codes(ps_phash_eta_parallel).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "HERMITE"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! fidiff_bpass_co
        w%list_elems(isize) = "fidiff_bpass_co"
        w%elem_descr(isize) = "fusion ion diffusivity: co barely passing orbits"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nefi_anom2,nrho_anom2,nspec_fusion)"
        IF(ALLOCATED(ss%fidiff_bpass_co)) THEN
           IF(ss%hash_codes(ps_phash_fidiff_bpass_co).LT.0) THEN
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
        isize = isize + 1  ! fidiff_bpass_ctr
        w%list_elems(isize) = "fidiff_bpass_ctr"
        w%elem_descr(isize) = "fusion ion diffusivity: ctr barely passing orbits"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nefi_anom2,nrho_anom2,nspec_fusion)"
        IF(ALLOCATED(ss%fidiff_bpass_ctr)) THEN
           IF(ss%hash_codes(ps_phash_fidiff_bpass_ctr).LT.0) THEN
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
        isize = isize + 1  ! fidiff_btrap
        w%list_elems(isize) = "fidiff_btrap"
        w%elem_descr(isize) = "fusion ion diffusivity: barely trapped orbits"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nefi_anom2,nrho_anom2,nspec_fusion)"
        IF(ALLOCATED(ss%fidiff_btrap)) THEN
           IF(ss%hash_codes(ps_phash_fidiff_btrap).LT.0) THEN
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
        isize = isize + 1  ! fidiff_dpass_co
        w%list_elems(isize) = "fidiff_dpass_co"
        w%elem_descr(isize) = "fusion ion diffusivity: co passing |vpll/v| -> 1"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nefi_anom2,nrho_anom2,nspec_fusion)"
        IF(ALLOCATED(ss%fidiff_dpass_co)) THEN
           IF(ss%hash_codes(ps_phash_fidiff_dpass_co).LT.0) THEN
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
        isize = isize + 1  ! fidiff_dpass_ctr
        w%list_elems(isize) = "fidiff_dpass_ctr"
        w%elem_descr(isize) = "fusion ion diffusivity: ctr passing |vpll/v| -> 1"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nefi_anom2,nrho_anom2,nspec_fusion)"
        IF(ALLOCATED(ss%fidiff_dpass_ctr)) THEN
           IF(ss%hash_codes(ps_phash_fidiff_dpass_ctr).LT.0) THEN
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
        isize = isize + 1  ! fidiff_dtrap
        w%list_elems(isize) = "fidiff_dtrap"
        w%elem_descr(isize) = "fusion ion diffusivity: deeply trapped orbits"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nefi_anom2,nrho_anom2,nspec_fusion)"
        IF(ALLOCATED(ss%fidiff_dtrap)) THEN
           IF(ss%hash_codes(ps_phash_fidiff_dtrap).LT.0) THEN
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
        isize = isize + 1  ! Fi_depletion
        w%list_elems(isize) = "Fi_depletion"
        w%elem_descr(isize) = "fast ion depletion: sum[nj*Zj/ne]"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%Fi_depletion)) THEN
           IF(ss%hash_codes(ps_phash_Fi_depletion).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*NS(:,PS_ELEC_INDEX)"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! freq_drbm
        w%list_elems(isize) = "freq_drbm"
        w%elem_descr(isize) = "frequency from DRBM model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%freq_drbm)) THEN
           IF(ss%hash_codes(ps_phash_freq_drbm).LT.0) THEN
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
        isize = isize + 1  ! freq_etg
        w%list_elems(isize) = "freq_etg"
        w%elem_descr(isize) = "frequency from ETG model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%freq_etg)) THEN
           IF(ss%hash_codes(ps_phash_freq_etg).LT.0) THEN
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
END SUBROUTINE plasma_state_contents_profs_2
