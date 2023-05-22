! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_contents_profs_9(itype,icomponent,ss,w,isize)


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
        isize = isize + 1  ! velte_paleo
        w%list_elems(isize) = "velte_paleo"
        w%elem_descr(isize) = "Te-equation, convection term from PALEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velte_paleo)) THEN
           IF(ss%hash_codes(ps_phash_velte_paleo).LT.0) THEN
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
        isize = isize + 1  ! velte_tglf
        w%list_elems(isize) = "velte_tglf"
        w%elem_descr(isize) = "Te-equation, advective velocity term from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velte_tglf)) THEN
           IF(ss%hash_codes(ps_phash_velte_tglf).LT.0) THEN
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
        isize = isize + 1  ! velti_anom
        w%list_elems(isize) = "velti_anom"
        w%elem_descr(isize) = "Ti-equation, advective velocity term"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velti_anom)) THEN
           IF(ss%hash_codes(ps_phash_velti_anom).LT.0) THEN
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
        isize = isize + 1  ! velti_glf
        w%list_elems(isize) = "velti_glf"
        w%elem_descr(isize) = "Ti-equation, advective velocity term from GLF23 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velti_glf)) THEN
           IF(ss%hash_codes(ps_phash_velti_glf).LT.0) THEN
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
        isize = isize + 1  ! velti_gtcneo
        w%list_elems(isize) = "velti_gtcneo"
        w%elem_descr(isize) = "Ti-equation, advective velocity term from GTC_NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velti_gtcneo)) THEN
           IF(ss%hash_codes(ps_phash_velti_gtcneo).LT.0) THEN
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
        isize = isize + 1  ! velti_mmm71
        w%list_elems(isize) = "velti_mmm71"
        w%elem_descr(isize) = "Ti-equation, advective velocity term from MMM71 model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velti_mmm71)) THEN
           IF(ss%hash_codes(ps_phash_velti_mmm71).LT.0) THEN
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
        isize = isize + 1  ! velti_nclass
        w%list_elems(isize) = "velti_nclass"
        w%elem_descr(isize) = "Ti-equation, advective velocity term from NCLASS model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velti_nclass)) THEN
           IF(ss%hash_codes(ps_phash_velti_nclass).LT.0) THEN
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
        isize = isize + 1  ! velti_neo
        w%list_elems(isize) = "velti_neo"
        w%elem_descr(isize) = "Ti-equation, advective velocity term from NEO model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velti_neo)) THEN
           IF(ss%hash_codes(ps_phash_velti_neo).LT.0) THEN
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
        isize = isize + 1  ! velti_tglf
        w%list_elems(isize) = "velti_tglf"
        w%elem_descr(isize) = "Ti-equation, advective velocity term from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%velti_tglf)) THEN
           IF(ss%hash_codes(ps_phash_velti_tglf).LT.0) THEN
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
        isize = isize + 1  ! vie_trans
        w%list_elems(isize) = "vie_trans"
        w%elem_descr(isize) = "ion energy radial velocity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%vie_trans)) THEN
           IF(ss%hash_codes(ps_phash_vie_trans).LT.0) THEN
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
        isize = isize + 1  ! vie_transcat
        w%list_elems(isize) = "vie_transcat"
        w%elem_descr(isize) = "Ion energy radial velocity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho,ntrcat)"
        IF(ALLOCATED(ss%vie_transcat)) THEN
           IF(ss%hash_codes(ps_phash_vie_transcat).LT.0) THEN
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
        isize = isize + 1  ! vmo_trans
        w%list_elems(isize) = "vmo_trans"
        w%elem_descr(isize) = "angular momentum radial velocity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%vmo_trans)) THEN
           IF(ss%hash_codes(ps_phash_vmo_trans).LT.0) THEN
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
        isize = isize + 1  ! vmo_transcat
        w%list_elems(isize) = "vmo_transcat"
        w%elem_descr(isize) = "Angular momentum radial velocity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho,ntrcat)"
        IF(ALLOCATED(ss%vmo_transcat)) THEN
           IF(ss%hash_codes(ps_phash_vmo_transcat).LT.0) THEN
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
        isize = isize + 1  ! vn_trans
        w%list_elems(isize) = "vn_trans"
        w%elem_descr(isize) = "specie radial velocity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho,0:nspec_th)"
        IF(ALLOCATED(ss%vn_trans)) THEN
           IF(ss%hash_codes(ps_phash_vn_trans).LT.0) THEN
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
        isize = isize + 1  ! vn_transcat
        w%list_elems(isize) = "vn_transcat"
        w%elem_descr(isize) = "specie radial velocity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho,0:nspec_th,ntrcat)"
        IF(ALLOCATED(ss%vn_transcat)) THEN
           IF(ss%hash_codes(ps_phash_vn_transcat).LT.0) THEN
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
        isize = isize + 1  ! vol
        w%list_elems(isize) = "vol"
        w%elem_descr(isize) = "enclosed volume"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m^3"
        w%c256val(isize) = "(nrho_eq)"
        IF(ALLOCATED(ss%vol)) THEN
           IF(ss%hash_codes(ps_phash_vol).LT.0) THEN
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
        isize = isize + 1  ! vpol_inmp
        w%list_elems(isize) = "vpol_inmp"
        w%elem_descr(isize) = "poloidal velocity, inner half midplane"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho-1,0:nspec_th)"
        IF(ALLOCATED(ss%vpol_inmp)) THEN
           IF(ss%hash_codes(ps_phash_vpol_inmp).LT.0) THEN
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

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vpol_omp
        w%list_elems(isize) = "vpol_omp"
        w%elem_descr(isize) = "poloidal velocity, outer half midplane"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho-1,0:nspec_th)"
        IF(ALLOCATED(ss%vpol_omp)) THEN
           IF(ss%hash_codes(ps_phash_vpol_omp).LT.0) THEN
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

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vtor_inmp
        w%list_elems(isize) = "vtor_inmp"
        w%elem_descr(isize) = "toroidal velocity, inner half midplane"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho-1,0:nspec_th)"
        IF(ALLOCATED(ss%vtor_inmp)) THEN
           IF(ss%hash_codes(ps_phash_vtor_inmp).LT.0) THEN
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

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vtor_omp
        w%list_elems(isize) = "vtor_omp"
        w%elem_descr(isize) = "toroidal velocity, outer half midplane"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho-1,0:nspec_th)"
        IF(ALLOCATED(ss%vtor_omp)) THEN
           IF(ss%hash_codes(ps_phash_vtor_omp).LT.0) THEN
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

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! V_loop
        w%list_elems(isize) = "V_loop"
        w%elem_descr(isize) = "loop voltage"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "Volts"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%V_loop)) THEN
           IF(ss%hash_codes(ps_phash_V_loop).LT.0) THEN
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
        isize = isize + 1  ! v_pars
        w%list_elems(isize) = "v_pars"
        w%elem_descr(isize) = "thermal specie <vpll>"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho-1,0:nspec_th)"
        IF(ALLOCATED(ss%v_pars)) THEN
           IF(ss%hash_codes(ps_phash_v_pars).LT.0) THEN
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

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! v_pers
        w%list_elems(isize) = "v_pers"
        w%elem_descr(isize) = "thermal specie <vper>"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho-1,0:nspec_th)"
        IF(ALLOCATED(ss%v_pers)) THEN
           IF(ss%hash_codes(ps_phash_v_pers).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! xRjcos_momeq
        w%list_elems(isize) = "xRjcos_momeq"
        w%elem_descr(isize) = "scaled R cos moments"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq,neqmom)"
        IF(ALLOCATED(ss%xRjcos_momeq)) THEN
           IF(ss%hash_codes(ps_phash_xRjcos_momeq).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "HERMITE_EXPLICIT"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! xRjsin_momeq
        w%list_elems(isize) = "xRjsin_momeq"
        w%elem_descr(isize) = "scaled R sin moments"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq,neqmom)"
        IF(ALLOCATED(ss%xRjsin_momeq)) THEN
           IF(ss%hash_codes(ps_phash_xRjsin_momeq).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "HERMITE_EXPLICIT"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! xZjcos_momeq
        w%list_elems(isize) = "xZjcos_momeq"
        w%elem_descr(isize) = "scaled Z cos moments"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq,neqmom)"
        IF(ALLOCATED(ss%xZjcos_momeq)) THEN
           IF(ss%hash_codes(ps_phash_xZjcos_momeq).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "HERMITE_EXPLICIT"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! xZjsin_momeq
        w%list_elems(isize) = "xZjsin_momeq"
        w%elem_descr(isize) = "scaled Z sin moments"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq,neqmom)"
        IF(ALLOCATED(ss%xZjsin_momeq)) THEN
           IF(ss%hash_codes(ps_phash_xZjsin_momeq).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "HERMITE_EXPLICIT"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Z0_momeq
        w%list_elems(isize) = "Z0_momeq"
        w%elem_descr(isize) = "Z0 of flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq)"
        IF(ALLOCATED(ss%Z0_momeq)) THEN
           IF(ss%hash_codes(ps_phash_Z0_momeq).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "HERMITE_EXPLICIT"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Zeff
        w%list_elems(isize) = "Zeff"
        w%elem_descr(isize) = "Zeff (sum[nj*Zj**2]/ne)"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%Zeff)) THEN
           IF(ss%hash_codes(ps_phash_Zeff).LT.0) THEN
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

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Zeff_fi
        w%list_elems(isize) = "Zeff_fi"
        w%elem_descr(isize) = "Fast ion contribution to Zeff"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%Zeff_fi)) THEN
           IF(ss%hash_codes(ps_phash_Zeff_fi).LT.0) THEN
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

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Zeff_th
        w%list_elems(isize) = "Zeff_th"
        w%elem_descr(isize) = "Thermal ion contribution to Zeff"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%Zeff_th)) THEN
           IF(ss%hash_codes(ps_phash_Zeff_th).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Z_geo
        w%list_elems(isize) = "Z_geo"
        w%elem_descr(isize) = "flux surfaces Z(rho,theta)"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq,nth_eq)"
        IF(ALLOCATED(ss%Z_geo)) THEN
           IF(ss%hash_codes(ps_phash_Z_geo).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "HERMITE_EXPLICIT"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Z_midp
        w%list_elems(isize) = "Z_midp"
        w%elem_descr(isize) = "flux surface midplane elevation"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%Z_midp)) THEN
           IF(ss%hash_codes(ps_phash_Z_midp).LT.0) THEN
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
        isize = isize + 1  ! Z_surfMax
        w%list_elems(isize) = "Z_surfMax"
        w%elem_descr(isize) = "max Z on flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%Z_surfMax)) THEN
           IF(ss%hash_codes(ps_phash_Z_surfMax).LT.0) THEN
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
        isize = isize + 1  ! Z_surfMin
        w%list_elems(isize) = "Z_surfMin"
        w%elem_descr(isize) = "min Z on flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%Z_surfMin)) THEN
           IF(ss%hash_codes(ps_phash_Z_surfMin).LT.0) THEN
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
END SUBROUTINE plasma_state_contents_profs_9
