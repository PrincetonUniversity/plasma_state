! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_contents_profs_6(itype,icomponent,ss,w,isize)


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

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qioniz
        w%list_elems(isize) = "qioniz"
        w%elem_descr(isize) = "ionization power per unit influx (sc0)"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "W/(#/sec)"
        w%c256val(isize) = "(nrho_gas-1,ngsc0)"
        IF(ALLOCATED(ss%qioniz)) THEN
           IF(ss%hash_codes(ps_phash_qioniz).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qqcx
        w%list_elems(isize) = "qqcx"
        w%elem_descr(isize) = "cx power per unit influx per (T0-Ti)"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "W/(#/sec)/keV"
        w%c256val(isize) = "(nrho_gas-1,ngsc0)"
        IF(ALLOCATED(ss%qqcx)) THEN
           IF(ss%hash_codes(ps_phash_qqcx).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qrot_conv
        w%list_elems(isize) = "qrot_conv"
        w%elem_descr(isize) = "rotational energy dissipation (convective)"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%qrot_conv)) THEN
           IF(ss%hash_codes(ps_phash_qrot_conv).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qrot_diff
        w%list_elems(isize) = "qrot_diff"
        w%elem_descr(isize) = "rotational energy dissipation (diffusive)"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%qrot_diff)) THEN
           IF(ss%hash_codes(ps_phash_qrot_diff).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! q_eq
        w%list_elems(isize) = "q_eq"
        w%elem_descr(isize) = "equilibrium q profile"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq)"
        IF(ALLOCATED(ss%q_eq)) THEN
           IF(ss%hash_codes(ps_phash_q_eq).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "SPLINE"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! R0_momeq
        w%list_elems(isize) = "R0_momeq"
        w%elem_descr(isize) = "R0 of flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq)"
        IF(ALLOCATED(ss%R0_momeq)) THEN
           IF(ss%hash_codes(ps_phash_R0_momeq).LT.0) THEN
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

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! rate_sinb0i
        w%list_elems(isize) = "rate_sinb0i"
        w%elem_descr(isize) = "sink rate due to ionization by beam ions"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_nbi-1,nspec_gas)"
        IF(ALLOCATED(ss%rate_sinb0i)) THEN
           IF(ss%hash_codes(ps_phash_rate_sinb0i).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! rate_sinb0x
        w%list_elems(isize) = "rate_sinb0x"
        w%elem_descr(isize) = "sink rate due to CX with beam ions (total)"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_nbi-1,nspec_gas)"
        IF(ALLOCATED(ss%rate_sinb0x)) THEN
           IF(ss%hash_codes(ps_phash_rate_sinb0x).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! rate_sinb0xs
        w%list_elems(isize) = "rate_sinb0xs"
        w%elem_descr(isize) = "sink rate due to CX with beam ions"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_nbi-1,nspec_gas,nspec_beam)"
        IF(ALLOCATED(ss%rate_sinb0xs)) THEN
           IF(ss%hash_codes(ps_phash_rate_sinb0xs).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! rate_sinf0i
        w%list_elems(isize) = "rate_sinf0i"
        w%elem_descr(isize) = "sink rate due to ionization by fusion ions"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_fus-1,nspec_gas)"
        IF(ALLOCATED(ss%rate_sinf0i)) THEN
           IF(ss%hash_codes(ps_phash_rate_sinf0i).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! rate_sinf0x
        w%list_elems(isize) = "rate_sinf0x"
        w%elem_descr(isize) = "sink rate due to CX with fusion ions (total)"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_fus-1,nspec_gas)"
        IF(ALLOCATED(ss%rate_sinf0x)) THEN
           IF(ss%hash_codes(ps_phash_rate_sinf0x).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! rate_sinf0xs
        w%list_elems(isize) = "rate_sinf0xs"
        w%elem_descr(isize) = "sink rate due to CX with fusion ions"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "(nrho_fus-1,nspec_gas,nspec_fusion)"
        IF(ALLOCATED(ss%rate_sinf0xs)) THEN
           IF(ss%hash_codes(ps_phash_rate_sinf0xs).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! res_sn
        w%list_elems(isize) = "res_sn"
        w%elem_descr(isize) = "residuals for particle transport eqn"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "Norm"
        w%c256val(isize) = "(nrho-1,0:nspec_th)"
        IF(ALLOCATED(ss%res_sn)) THEN
           IF(ss%hash_codes(ps_phash_res_sn).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! res_te
        w%list_elems(isize) = "res_te"
        w%elem_descr(isize) = "residuals for electron transport eqn"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "Norm"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%res_te)) THEN
           IF(ss%hash_codes(ps_phash_res_te).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! res_ti
        w%list_elems(isize) = "res_ti"
        w%elem_descr(isize) = "residuals for ion transport eqn"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "Norm"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%res_ti)) THEN
           IF(ss%hash_codes(ps_phash_res_ti).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! res_tq
        w%list_elems(isize) = "res_tq"
        w%elem_descr(isize) = "rediduals for angular momentum transport eqn"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "Norm"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%res_tq)) THEN
           IF(ss%hash_codes(ps_phash_res_tq).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Rmajor_mean
        w%list_elems(isize) = "Rmajor_mean"
        w%elem_descr(isize) = "Rmajor_mean = (R_Surfmax+R_surfmin)/2"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%Rmajor_mean)) THEN
           IF(ss%hash_codes(ps_phash_Rmajor_mean).LT.0) THEN
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
        isize = isize + 1  ! rMinor_mean
        w%list_elems(isize) = "rMinor_mean"
        w%elem_descr(isize) = "rMinor_mean = (R_Surfmax-R_surfmin)/2"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%rMinor_mean)) THEN
           IF(ss%hash_codes(ps_phash_rMinor_mean).LT.0) THEN
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
        isize = isize + 1  ! R_geo
        w%list_elems(isize) = "R_geo"
        w%elem_descr(isize) = "flux surfaces R(rho,theta)"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq,nth_eq)"
        IF(ALLOCATED(ss%R_geo)) THEN
           IF(ss%hash_codes(ps_phash_R_geo).LT.0) THEN
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
        isize = isize + 1  ! R_midp_in
        w%list_elems(isize) = "R_midp_in"
        w%elem_descr(isize) = "R (high field side) midplane intercept"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%R_midp_in)) THEN
           IF(ss%hash_codes(ps_phash_R_midp_in).LT.0) THEN
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
        isize = isize + 1  ! R_midp_out
        w%list_elems(isize) = "R_midp_out"
        w%elem_descr(isize) = "R (low field side) midplane intercept"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%R_midp_out)) THEN
           IF(ss%hash_codes(ps_phash_R_midp_out).LT.0) THEN
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
        isize = isize + 1  ! R_surfMax
        w%list_elems(isize) = "R_surfMax"
        w%elem_descr(isize) = "max R on flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%R_surfMax)) THEN
           IF(ss%hash_codes(ps_phash_R_surfMax).LT.0) THEN
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
        isize = isize + 1  ! R_surfMin
        w%list_elems(isize) = "R_surfMin"
        w%elem_descr(isize) = "min R on flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%R_surfMin)) THEN
           IF(ss%hash_codes(ps_phash_R_surfMin).LT.0) THEN
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

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! s0reco
        w%list_elems(isize) = "s0reco"
        w%elem_descr(isize) = "recombination neutral sce"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho_gas-1,nspec_gas)"
        IF(ALLOCATED(ss%s0reco)) THEN
           IF(ss%hash_codes(ps_phash_s0reco).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! s0reco_e
        w%list_elems(isize) = "s0reco_e"
        w%elem_descr(isize) = "net electron source associated with recombination:"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho_gas-1)"
        IF(ALLOCATED(ss%s0reco_e)) THEN
           IF(ss%hash_codes(ps_phash_s0reco_e).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! s0reco_recap
        w%list_elems(isize) = "s0reco_recap"
        w%elem_descr(isize) = "recombination neutrals recapture"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho_gas-1,nspec_gas)"
        IF(ALLOCATED(ss%s0reco_recap)) THEN
           IF(ss%hash_codes(ps_phash_s0reco_recap).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sb0halo
        w%list_elems(isize) = "sb0halo"
        w%elem_descr(isize) = "beam dep cx sink"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho_nbi-1,nspec_gas)"
        IF(ALLOCATED(ss%sb0halo)) THEN
           IF(ss%hash_codes(ps_phash_sb0halo).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sb0halo_recap
        w%list_elems(isize) = "sb0halo_recap"
        w%elem_descr(isize) = "cx neutral recapture"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho_nbi-1,nspec_gas)"
        IF(ALLOCATED(ss%sb0halo_recap)) THEN
           IF(ss%hash_codes(ps_phash_sb0halo_recap).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sbedep
        w%list_elems(isize) = "sbedep"
        w%elem_descr(isize) = "beam deposition cold electron sce"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%sbedep)) THEN
           IF(ss%hash_codes(ps_phash_sbedep).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sbehalo
        w%list_elems(isize) = "sbehalo"
        w%elem_descr(isize) = "cold electron sce due to"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%sbehalo)) THEN
           IF(ss%hash_codes(ps_phash_sbehalo).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sbsce
        w%list_elems(isize) = "sbsce"
        w%elem_descr(isize) = "net source, thermal ions & electrons"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho_nbi-1,0:nspec_tha)"
        IF(ALLOCATED(ss%sbsce)) THEN
           IF(ss%hash_codes(ps_phash_sbsce).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sbtherm
        w%list_elems(isize) = "sbtherm"
        w%elem_descr(isize) = "beam ion thermalization"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho_nbi-1,nspec_beam)"
        IF(ALLOCATED(ss%sbtherm)) THEN
           IF(ss%hash_codes(ps_phash_sbtherm).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sc_anom
        w%list_elems(isize) = "sc_anom"
        w%elem_descr(isize) = "anomalous sources"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho_anomth-1,0:nspec_th)"
        IF(ALLOCATED(ss%sc_anom)) THEN
           IF(ss%hash_codes(ps_phash_sc_anom).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sfsce
        w%list_elems(isize) = "sfsce"
        w%elem_descr(isize) = "net thermalization source of fusion ions"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho_fus-1,0:nspec_tha)"
        IF(ALLOCATED(ss%sfsce)) THEN
           IF(ss%hash_codes(ps_phash_sfsce).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sftherm
        w%list_elems(isize) = "sftherm"
        w%elem_descr(isize) = "fusion ion thermalization"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho_fus-1,nspec_fusion)"
        IF(ALLOCATED(ss%sftherm)) THEN
           IF(ss%hash_codes(ps_phash_sftherm).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sii_xs
        w%list_elems(isize) = "sii_xs"
        w%elem_descr(isize) = "species source/sinks driven by xs(1:nxs)"
        w%component(isize) = "RAD"
        w%elem_units(isize) = "(#/sec)/(#/sec)"
        w%c256val(isize) = "(nrho_mist-1,0:nspec_th,nxs)"
        IF(ALLOCATED(ss%sii_xs)) THEN
           IF(ss%hash_codes(ps_phash_sii_xs).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sn_trans
        w%list_elems(isize) = "sn_trans"
        w%elem_descr(isize) = "particle transport (loss)"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho-1,0:nspec_th)"
        IF(ALLOCATED(ss%sn_trans)) THEN
           IF(ss%hash_codes(ps_phash_sn_trans).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF
END SUBROUTINE plasma_state_contents_profs_6
