! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_contents_profs_0(itype,icomponent,ss,w,isize)


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
        isize = isize + 1  ! anom_evar
        w%list_elems(isize) = "anom_evar"
        w%elem_descr(isize) = "anomalous transport energy variation"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nefi_anom)"
        IF(ALLOCATED(ss%anom_evar)) THEN
           IF(ss%hash_codes(ps_phash_anom_evar).LT.0) THEN
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
        isize = isize + 1  ! area
        w%list_elems(isize) = "area"
        w%elem_descr(isize) = "enclosed area"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m^2"
        w%c256val(isize) = "(nrho_eq)"
        IF(ALLOCATED(ss%area)) THEN
           IF(ss%hash_codes(ps_phash_area).LT.0) THEN
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

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! balstab
        w%list_elems(isize) = "balstab"
        w%elem_descr(isize) = "Ballooning/Mercier stability indicator"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_lmhd)"
        IF(ALLOCATED(ss%balstab)) THEN
           IF(ss%hash_codes(ps_phash_balstab).LT.0) THEN
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
        isize = isize + 1  ! bidiff_bpass_co
        w%list_elems(isize) = "bidiff_bpass_co"
        w%elem_descr(isize) = "beam ion diffusivity: co barely passing orbits"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nefi_anom2,nrho_anom2,nspec_beam)"
        IF(ALLOCATED(ss%bidiff_bpass_co)) THEN
           IF(ss%hash_codes(ps_phash_bidiff_bpass_co).LT.0) THEN
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
        isize = isize + 1  ! bidiff_bpass_ctr
        w%list_elems(isize) = "bidiff_bpass_ctr"
        w%elem_descr(isize) = "beam ion diffusivity: ctr barely passing orbits"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nefi_anom2,nrho_anom2,nspec_beam)"
        IF(ALLOCATED(ss%bidiff_bpass_ctr)) THEN
           IF(ss%hash_codes(ps_phash_bidiff_bpass_ctr).LT.0) THEN
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
        isize = isize + 1  ! bidiff_btrap
        w%list_elems(isize) = "bidiff_btrap"
        w%elem_descr(isize) = "beam ion diffusivity: barely trapped orbits"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nefi_anom2,nrho_anom2,nspec_beam)"
        IF(ALLOCATED(ss%bidiff_btrap)) THEN
           IF(ss%hash_codes(ps_phash_bidiff_btrap).LT.0) THEN
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
        isize = isize + 1  ! bidiff_dpass_co
        w%list_elems(isize) = "bidiff_dpass_co"
        w%elem_descr(isize) = "beam ion diffusivity: co passing |vpll/v| -> 1"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nefi_anom2,nrho_anom2,nspec_beam)"
        IF(ALLOCATED(ss%bidiff_dpass_co)) THEN
           IF(ss%hash_codes(ps_phash_bidiff_dpass_co).LT.0) THEN
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
        isize = isize + 1  ! bidiff_dpass_ctr
        w%list_elems(isize) = "bidiff_dpass_ctr"
        w%elem_descr(isize) = "beam ion diffusivity: ctr passing |vpll/v| -> 1"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nefi_anom2,nrho_anom2,nspec_beam)"
        IF(ALLOCATED(ss%bidiff_dpass_ctr)) THEN
           IF(ss%hash_codes(ps_phash_bidiff_dpass_ctr).LT.0) THEN
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
        isize = isize + 1  ! bidiff_dtrap
        w%list_elems(isize) = "bidiff_dtrap"
        w%elem_descr(isize) = "beam ion diffusivity: deeply trapped orbits"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nefi_anom2,nrho_anom2,nspec_beam)"
        IF(ALLOCATED(ss%bidiff_dtrap)) THEN
           IF(ss%hash_codes(ps_phash_bidiff_dtrap).LT.0) THEN
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
        isize = isize + 1  ! BphiRZ
        w%list_elems(isize) = "BphiRZ"
        w%elem_descr(isize) = "Toroidal field"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T"
        w%c256val(isize) = "(nr,nz)"
        IF(ALLOCATED(ss%BphiRZ)) THEN
           IF(ss%hash_codes(ps_phash_BphiRZ).LT.0) THEN
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
        isize = isize + 1  ! BRRZ
        w%list_elems(isize) = "BRRZ"
        w%elem_descr(isize) = "R component of poloidal field"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T"
        w%c256val(isize) = "(nr,nz)"
        IF(ALLOCATED(ss%BRRZ)) THEN
           IF(ss%hash_codes(ps_phash_BRRZ).LT.0) THEN
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
        isize = isize + 1  ! BZRZ
        w%list_elems(isize) = "BZRZ"
        w%elem_descr(isize) = "Z component of poloidal field"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T"
        w%c256val(isize) = "(nr,nz)"
        IF(ALLOCATED(ss%BZRZ)) THEN
           IF(ss%hash_codes(ps_phash_BZRZ).LT.0) THEN
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
        isize = isize + 1  ! B_surfMax
        w%list_elems(isize) = "B_surfMax"
        w%elem_descr(isize) = "max mod(B) on flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%B_surfMax)) THEN
           IF(ss%hash_codes(ps_phash_B_surfMax).LT.0) THEN
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
        isize = isize + 1  ! B_surfMin
        w%list_elems(isize) = "B_surfMin"
        w%elem_descr(isize) = "min mod(B) on flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%B_surfMin)) THEN
           IF(ss%hash_codes(ps_phash_B_surfMin).LT.0) THEN
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

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! cdicrf
        w%list_elems(isize) = "cdicrf"
        w%elem_descr(isize) = "ICRF current drive"
        w%component(isize) = "IC"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho_icrf-1,nicrf_src)"
        IF(ALLOCATED(ss%cdicrf)) THEN
           IF(ss%hash_codes(ps_phash_cdicrf).LT.0) THEN
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
        isize = isize + 1  ! cdicrf_nphi
        w%list_elems(isize) = "cdicrf_nphi"
        w%elem_descr(isize) = "ICRF current drive"
        w%component(isize) = "IC"
        w%elem_units(isize) = "A"
        w%c256val(isize) = "(nrho_icrf-1,max_num_nphi,nicrf_src)"
        IF(ALLOCATED(ss%cdicrf_nphi)) THEN
           IF(ss%hash_codes(ps_phash_cdicrf_nphi).LT.0) THEN
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
        isize = isize + 1  ! chie_anom
        w%list_elems(isize) = "chie_anom"
        w%elem_descr(isize) = "Te-equation, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chie_anom)) THEN
           IF(ss%hash_codes(ps_phash_chie_anom).LT.0) THEN
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
        isize = isize + 1  ! chie_drbm
        w%list_elems(isize) = "chie_drbm"
        w%elem_descr(isize) = "Te-equation, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chie_drbm)) THEN
           IF(ss%hash_codes(ps_phash_chie_drbm).LT.0) THEN
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
        isize = isize + 1  ! chie_etg
        w%list_elems(isize) = "chie_etg"
        w%elem_descr(isize) = "Te-equation, diffusivity term ETG model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chie_etg)) THEN
           IF(ss%hash_codes(ps_phash_chie_etg).LT.0) THEN
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
        isize = isize + 1  ! chie_glf
        w%list_elems(isize) = "chie_glf"
        w%elem_descr(isize) = "Te-equation, diffusivity from GLF23 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chie_glf)) THEN
           IF(ss%hash_codes(ps_phash_chie_glf).LT.0) THEN
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
        isize = isize + 1  ! chie_gtcneo
        w%list_elems(isize) = "chie_gtcneo"
        w%elem_descr(isize) = "Te-equation, neoclassical diffusivity from GTC_NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chie_gtcneo)) THEN
           IF(ss%hash_codes(ps_phash_chie_gtcneo).LT.0) THEN
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
        isize = isize + 1  ! chie_mmm71
        w%list_elems(isize) = "chie_mmm71"
        w%elem_descr(isize) = "Te-equation, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chie_mmm71)) THEN
           IF(ss%hash_codes(ps_phash_chie_mmm71).LT.0) THEN
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
        isize = isize + 1  ! chie_mtm
        w%list_elems(isize) = "chie_mtm"
        w%elem_descr(isize) = "Te-equation, thermal diffusivity term MTM model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chie_mtm)) THEN
           IF(ss%hash_codes(ps_phash_chie_mtm).LT.0) THEN
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
        isize = isize + 1  ! chie_nclass
        w%list_elems(isize) = "chie_nclass"
        w%elem_descr(isize) = "Te-equation, neoclassical diffusivity from NCLASS model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chie_nclass)) THEN
           IF(ss%hash_codes(ps_phash_chie_nclass).LT.0) THEN
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
        isize = isize + 1  ! chie_neo
        w%list_elems(isize) = "chie_neo"
        w%elem_descr(isize) = "Te-equation, neoclassical diffusivity from NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chie_neo)) THEN
           IF(ss%hash_codes(ps_phash_chie_neo).LT.0) THEN
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
        isize = isize + 1  ! chie_ntm
        w%list_elems(isize) = "chie_ntm"
        w%elem_descr(isize) = "Te-equation, neoclassical diffusivity from NTM model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chie_ntm)) THEN
           IF(ss%hash_codes(ps_phash_chie_ntm).LT.0) THEN
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
        isize = isize + 1  ! chie_paleo
        w%list_elems(isize) = "chie_paleo"
        w%elem_descr(isize) = "Te-equation, diffusivity term PALEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chie_paleo)) THEN
           IF(ss%hash_codes(ps_phash_chie_paleo).LT.0) THEN
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
        isize = isize + 1  ! chie_tglf
        w%list_elems(isize) = "chie_tglf"
        w%elem_descr(isize) = "Te-equation, diffusivity from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chie_tglf)) THEN
           IF(ss%hash_codes(ps_phash_chie_tglf).LT.0) THEN
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
        isize = isize + 1  ! chie_trans
        w%list_elems(isize) = "chie_trans"
        w%elem_descr(isize) = "electron thermal diffusivity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%chie_trans)) THEN
           IF(ss%hash_codes(ps_phash_chie_trans).LT.0) THEN
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
        isize = isize + 1  ! chie_transcat
        w%list_elems(isize) = "chie_transcat"
        w%elem_descr(isize) = "Electron energy diffusivity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho,ntrcat)"
        IF(ALLOCATED(ss%chie_transcat)) THEN
           IF(ss%hash_codes(ps_phash_chie_transcat).LT.0) THEN
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
        isize = isize + 1  ! chie_w19
        w%list_elems(isize) = "chie_w19"
        w%elem_descr(isize) = "Te-equation, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chie_w19)) THEN
           IF(ss%hash_codes(ps_phash_chie_w19).LT.0) THEN
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
        isize = isize + 1  ! chii_anom
        w%list_elems(isize) = "chii_anom"
        w%elem_descr(isize) = "Ti-equation, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chii_anom)) THEN
           IF(ss%hash_codes(ps_phash_chii_anom).LT.0) THEN
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
        isize = isize + 1  ! chii_drbm
        w%list_elems(isize) = "chii_drbm"
        w%elem_descr(isize) = "Ti-equation, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chii_drbm)) THEN
           IF(ss%hash_codes(ps_phash_chii_drbm).LT.0) THEN
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
        isize = isize + 1  ! chii_glf
        w%list_elems(isize) = "chii_glf"
        w%elem_descr(isize) = "Ti-equation, diffusivity from GLF23 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chii_glf)) THEN
           IF(ss%hash_codes(ps_phash_chii_glf).LT.0) THEN
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
        isize = isize + 1  ! chii_gtcneo
        w%list_elems(isize) = "chii_gtcneo"
        w%elem_descr(isize) = "Ti-equation, neoclassical diffusivity from GTC_NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chii_gtcneo)) THEN
           IF(ss%hash_codes(ps_phash_chii_gtcneo).LT.0) THEN
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
        isize = isize + 1  ! chii_mmm71
        w%list_elems(isize) = "chii_mmm71"
        w%elem_descr(isize) = "Ti-equation, diffusivity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chii_mmm71)) THEN
           IF(ss%hash_codes(ps_phash_chii_mmm71).LT.0) THEN
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
        isize = isize + 1  ! chii_nclass
        w%list_elems(isize) = "chii_nclass"
        w%elem_descr(isize) = "Ti-equation, neoclassical diffusivity from NCALSS model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/s"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%chii_nclass)) THEN
           IF(ss%hash_codes(ps_phash_chii_nclass).LT.0) THEN
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
END SUBROUTINE plasma_state_contents_profs_0
