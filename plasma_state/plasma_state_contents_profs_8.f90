! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_contents_profs_8(itype,icomponent,ss,w,isize)


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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! triang_miller_L
        w%list_elems(isize) = "triang_miller_L"
        w%elem_descr(isize) = "Miller lower triangularity"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%triang_miller_L)) THEN
           IF(ss%hash_codes(ps_phash_triang_miller_L).LT.0) THEN
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
        isize = isize + 1  ! triang_miller_U
        w%list_elems(isize) = "triang_miller_U"
        w%elem_descr(isize) = "Miller upper triangularity"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%triang_miller_U)) THEN
           IF(ss%hash_codes(ps_phash_triang_miller_U).LT.0) THEN
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
        isize = isize + 1  ! Ts
        w%list_elems(isize) = "Ts"
        w%elem_descr(isize) = "thermal specie temperature"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho-1,0:nspec_th)"
        IF(ALLOCATED(ss%Ts)) THEN
           IF(ss%hash_codes(ps_phash_Ts).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*NS"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tzflux_ky_tglf
        w%list_elems(isize) = "tzflux_ky_tglf"
        w%elem_descr(isize) = "impurity density flux spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%tzflux_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_tzflux_ky_tglf).LT.0) THEN
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
        isize = isize + 1  ! tz_ky_tglf
        w%list_elems(isize) = "tz_ky_tglf"
        w%elem_descr(isize) = "impurity density fluctuation spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%tz_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_tz_ky_tglf).LT.0) THEN
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
        isize = isize + 1  ! upwind_pfrac_ns
        w%list_elems(isize) = "upwind_pfrac_ns"
        w%elem_descr(isize) = "specie density forward upwind fractions"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho,0:nspec_th)"
        IF(ALLOCATED(ss%upwind_pfrac_ns)) THEN
           IF(ss%hash_codes(ps_phash_upwind_pfrac_ns).LT.0) THEN
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
        isize = isize + 1  ! upwind_pfrac_omega
        w%list_elems(isize) = "upwind_pfrac_omega"
        w%elem_descr(isize) = "omega (ang. velocity) forward upwind fraction"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%upwind_pfrac_omega)) THEN
           IF(ss%hash_codes(ps_phash_upwind_pfrac_omega).LT.0) THEN
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
        isize = isize + 1  ! upwind_pfrac_Te
        w%list_elems(isize) = "upwind_pfrac_Te"
        w%elem_descr(isize) = "Te (electron energy advection) forward upwind fraction"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%upwind_pfrac_Te)) THEN
           IF(ss%hash_codes(ps_phash_upwind_pfrac_Te).LT.0) THEN
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
        isize = isize + 1  ! upwind_pfrac_Ti
        w%list_elems(isize) = "upwind_pfrac_Ti"
        w%elem_descr(isize) = "Ti (ion energy advection) forward upwind fraction"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%upwind_pfrac_Ti)) THEN
           IF(ss%hash_codes(ps_phash_upwind_pfrac_Ti).LT.0) THEN
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
        isize = isize + 1  ! vee_trans
        w%list_elems(isize) = "vee_trans"
        w%elem_descr(isize) = "electron energy radial velocity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%vee_trans)) THEN
           IF(ss%hash_codes(ps_phash_vee_trans).LT.0) THEN
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
        isize = isize + 1  ! vee_transcat
        w%list_elems(isize) = "vee_transcat"
        w%elem_descr(isize) = "Electron energy radial velocity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho,ntrcat)"
        IF(ALLOCATED(ss%vee_transcat)) THEN
           IF(ss%hash_codes(ps_phash_vee_transcat).LT.0) THEN
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
        isize = isize + 1  ! velb_fusi
        w%list_elems(isize) = "velb_fusi"
        w%elem_descr(isize) = "fusion ion anom. radial velocity"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anom)"
        IF(ALLOCATED(ss%velb_fusi)) THEN
           IF(ss%hash_codes(ps_phash_velb_fusi).LT.0) THEN
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
        isize = isize + 1  ! velb_nbi
        w%list_elems(isize) = "velb_nbi"
        w%elem_descr(isize) = "beam ion anom. radial velocity"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anom)"
        IF(ALLOCATED(ss%velb_nbi)) THEN
           IF(ss%hash_codes(ps_phash_velb_nbi).LT.0) THEN
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
        isize = isize + 1  ! velb_rfmi
        w%list_elems(isize) = "velb_rfmi"
        w%elem_descr(isize) = "RF minority ion anom. radial velocity"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anom)"
        IF(ALLOCATED(ss%velb_rfmi)) THEN
           IF(ss%hash_codes(ps_phash_velb_rfmi).LT.0) THEN
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
        isize = isize + 1  ! velne_nclass
        w%list_elems(isize) = "velne_nclass"
        w%elem_descr(isize) = "electron-density-equations, advective velocity term from NCLASS model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velne_nclass)) THEN
           IF(ss%hash_codes(ps_phash_velne_nclass).LT.0) THEN
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
        isize = isize + 1  ! velnh_nclass
        w%list_elems(isize) = "velnh_nclass"
        w%elem_descr(isize) = "main-hydrogenic-density-equations, advective velocity term from NCLASS model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velnh_nclass)) THEN
           IF(ss%hash_codes(ps_phash_velnh_nclass).LT.0) THEN
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
        isize = isize + 1  ! velni_drbm
        w%list_elems(isize) = "velni_drbm"
        w%elem_descr(isize) = "ion convective velocity"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velni_drbm)) THEN
           IF(ss%hash_codes(ps_phash_velni_drbm).LT.0) THEN
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
        isize = isize + 1  ! velns_anom
        w%list_elems(isize) = "velns_anom"
        w%elem_descr(isize) = "density-equations, advective velocity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%velns_anom)) THEN
           IF(ss%hash_codes(ps_phash_velns_anom).LT.0) THEN
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
        isize = isize + 1  ! velns_glf
        w%list_elems(isize) = "velns_glf"
        w%elem_descr(isize) = "density-equations, advective velocity term from GLF23 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%velns_glf)) THEN
           IF(ss%hash_codes(ps_phash_velns_glf).LT.0) THEN
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
        isize = isize + 1  ! velns_gtcneo
        w%list_elems(isize) = "velns_gtcneo"
        w%elem_descr(isize) = "density-equations, advective velocity term from GTC_NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%velns_gtcneo)) THEN
           IF(ss%hash_codes(ps_phash_velns_gtcneo).LT.0) THEN
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
        isize = isize + 1  ! velns_mmm71
        w%list_elems(isize) = "velns_mmm71"
        w%elem_descr(isize) = "density-equations, advective velocity term from MMM71 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%velns_mmm71)) THEN
           IF(ss%hash_codes(ps_phash_velns_mmm71).LT.0) THEN
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
        isize = isize + 1  ! velns_nclass
        w%list_elems(isize) = "velns_nclass"
        w%elem_descr(isize) = "density-equations, advective velocity term from NCLASS model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%velns_nclass)) THEN
           IF(ss%hash_codes(ps_phash_velns_nclass).LT.0) THEN
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
        isize = isize + 1  ! velns_neo
        w%list_elems(isize) = "velns_neo"
        w%elem_descr(isize) = "density-equations, advective velocity term from NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%velns_neo)) THEN
           IF(ss%hash_codes(ps_phash_velns_neo).LT.0) THEN
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
        isize = isize + 1  ! velns_tglf
        w%list_elems(isize) = "velns_tglf"
        w%elem_descr(isize) = "density-equations, advective velocity term from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth,0:nspec_th)"
        IF(ALLOCATED(ss%velns_tglf)) THEN
           IF(ss%hash_codes(ps_phash_velns_tglf).LT.0) THEN
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
        isize = isize + 1  ! velnx_nclass
        w%list_elems(isize) = "velnx_nclass"
        w%elem_descr(isize) = "main-impurity-density-equations, advective velocity term from NCLASS model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velnx_nclass)) THEN
           IF(ss%hash_codes(ps_phash_velnx_nclass).LT.0) THEN
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
        isize = isize + 1  ! velpphi_anom
        w%list_elems(isize) = "velpphi_anom"
        w%elem_descr(isize) = "momentum-equation, advective velocity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velpphi_anom)) THEN
           IF(ss%hash_codes(ps_phash_velpphi_anom).LT.0) THEN
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
        isize = isize + 1  ! velpphi_glf
        w%list_elems(isize) = "velpphi_glf"
        w%elem_descr(isize) = "momentum-equation, advective velocity term from GLF23 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velpphi_glf)) THEN
           IF(ss%hash_codes(ps_phash_velpphi_glf).LT.0) THEN
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
        isize = isize + 1  ! velpphi_gtcneo
        w%list_elems(isize) = "velpphi_gtcneo"
        w%elem_descr(isize) = "momentum-equation, advective velocity term from GTC_NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velpphi_gtcneo)) THEN
           IF(ss%hash_codes(ps_phash_velpphi_gtcneo).LT.0) THEN
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
        isize = isize + 1  ! velpphi_mmm71
        w%list_elems(isize) = "velpphi_mmm71"
        w%elem_descr(isize) = "momentum-equation, advective velocity term from MMM71 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velpphi_mmm71)) THEN
           IF(ss%hash_codes(ps_phash_velpphi_mmm71).LT.0) THEN
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
        isize = isize + 1  ! velpphi_neo
        w%list_elems(isize) = "velpphi_neo"
        w%elem_descr(isize) = "momentum-equation, advective velocity term from NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velpphi_neo)) THEN
           IF(ss%hash_codes(ps_phash_velpphi_neo).LT.0) THEN
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
        isize = isize + 1  ! velpphi_tglf
        w%list_elems(isize) = "velpphi_tglf"
        w%elem_descr(isize) = "momentum-equation, advective velocity term from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velpphi_tglf)) THEN
           IF(ss%hash_codes(ps_phash_velpphi_tglf).LT.0) THEN
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
        isize = isize + 1  ! velte_anom
        w%list_elems(isize) = "velte_anom"
        w%elem_descr(isize) = "Te-equation, advective velocity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velte_anom)) THEN
           IF(ss%hash_codes(ps_phash_velte_anom).LT.0) THEN
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
        isize = isize + 1  ! velte_glf
        w%list_elems(isize) = "velte_glf"
        w%elem_descr(isize) = "Te-equation, advective velocity term from GLF23 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velte_glf)) THEN
           IF(ss%hash_codes(ps_phash_velte_glf).LT.0) THEN
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
        isize = isize + 1  ! velte_gtcneo
        w%list_elems(isize) = "velte_gtcneo"
        w%elem_descr(isize) = "Te-equation, advective velocity term from GTC_NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velte_gtcneo)) THEN
           IF(ss%hash_codes(ps_phash_velte_gtcneo).LT.0) THEN
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
        isize = isize + 1  ! velte_mmm71
        w%list_elems(isize) = "velte_mmm71"
        w%elem_descr(isize) = "Te-equation, advective velocity term from MMM71 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velte_mmm71)) THEN
           IF(ss%hash_codes(ps_phash_velte_mmm71).LT.0) THEN
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
        isize = isize + 1  ! velte_nclass
        w%list_elems(isize) = "velte_nclass"
        w%elem_descr(isize) = "Te-equation, advective velocity term from NCLASS model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velte_nclass)) THEN
           IF(ss%hash_codes(ps_phash_velte_nclass).LT.0) THEN
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
        isize = isize + 1  ! velte_neo
        w%list_elems(isize) = "velte_neo"
        w%elem_descr(isize) = "Te-equation, advective velocity term from NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velte_neo)) THEN
           IF(ss%hash_codes(ps_phash_velte_neo).LT.0) THEN
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
END SUBROUTINE plasma_state_contents_profs_8
