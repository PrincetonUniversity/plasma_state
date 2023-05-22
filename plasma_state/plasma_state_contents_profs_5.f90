! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_contents_profs_5(itype,icomponent,ss,w,isize)


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
        isize = isize + 1  ! pe_anom
        w%list_elems(isize) = "pe_anom"
        w%elem_descr(isize) = "anomalous electron heating"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_anomth-1)"
        IF(ALLOCATED(ss%pe_anom)) THEN
           IF(ss%hash_codes(ps_phash_pe_anom).LT.0) THEN
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
        isize = isize + 1  ! pe_trans
        w%list_elems(isize) = "pe_trans"
        w%elem_descr(isize) = "electron transport (loss) power"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%pe_trans)) THEN
           IF(ss%hash_codes(ps_phash_pe_trans).LT.0) THEN
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
        isize = isize + 1  ! pe_transcat
        w%list_elems(isize) = "pe_transcat"
        w%elem_descr(isize) = "electron transport"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho-1,ntrcat)"
        IF(ALLOCATED(ss%pe_transcat)) THEN
           IF(ss%hash_codes(ps_phash_pe_transcat).LT.0) THEN
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
        isize = isize + 1  ! pfuse
        w%list_elems(isize) = "pfuse"
        w%elem_descr(isize) = "electron heating by all fusion ions"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_fus-1)"
        IF(ALLOCATED(ss%pfuse)) THEN
           IF(ss%hash_codes(ps_phash_pfuse).LT.0) THEN
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
        isize = isize + 1  ! pfusi
        w%list_elems(isize) = "pfusi"
        w%elem_descr(isize) = "thermal ion heating by all fusion ions"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_fus-1)"
        IF(ALLOCATED(ss%pfusi)) THEN
           IF(ss%hash_codes(ps_phash_pfusi).LT.0) THEN
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
        isize = isize + 1  ! pfusth
        w%list_elems(isize) = "pfusth"
        w%elem_descr(isize) = "thermalization of fusion ions"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_fus-1)"
        IF(ALLOCATED(ss%pfusth)) THEN
           IF(ss%hash_codes(ps_phash_pfusth).LT.0) THEN
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
        isize = isize + 1  ! phiflux_ky_tglf
        w%list_elems(isize) = "phiflux_ky_tglf"
        w%elem_descr(isize) = "momentum fluctuation spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%phiflux_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_phiflux_ky_tglf).LT.0) THEN
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
        isize = isize + 1  ! phit
        w%list_elems(isize) = "phit"
        w%elem_descr(isize) = "toroidal flux vs. rho"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "Wb"
        w%c256val(isize) = "(nrho_eq)"
        IF(ALLOCATED(ss%phit)) THEN
           IF(ss%hash_codes(ps_phash_phit).LT.0) THEN
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

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! picrf_nphi_srcs
        w%list_elems(isize) = "picrf_nphi_srcs"
        w%elem_descr(isize) = "direct ICRF power deposition"
        w%component(isize) = "IC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_icrf-1,max_num_nphi,nicrf_src,0:nspec_alla)"
        IF(ALLOCATED(ss%picrf_nphi_srcs)) THEN
           IF(ss%hash_codes(ps_phash_picrf_nphi_srcs).LT.0) THEN
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

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! picrf_srcs
        w%list_elems(isize) = "picrf_srcs"
        w%elem_descr(isize) = "direct ICRF power deposition"
        w%component(isize) = "IC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_icrf-1,nicrf_src,0:nspec_alla)"
        IF(ALLOCATED(ss%picrf_srcs)) THEN
           IF(ss%hash_codes(ps_phash_picrf_srcs).LT.0) THEN
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

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! picrf_totals
        w%list_elems(isize) = "picrf_totals"
        w%elem_descr(isize) = "direct ICRF power deposition"
        w%component(isize) = "IC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_icrf-1,0:nspec_alla)"
        IF(ALLOCATED(ss%picrf_totals)) THEN
           IF(ss%hash_codes(ps_phash_picrf_totals).LT.0) THEN
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

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! picth
        w%list_elems(isize) = "picth"
        w%elem_descr(isize) = "direct thermal ion heating by ICRF"
        w%component(isize) = "IC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_icrf-1)"
        IF(ALLOCATED(ss%picth)) THEN
           IF(ss%hash_codes(ps_phash_picth).LT.0) THEN
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

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pilh
        w%list_elems(isize) = "pilh"
        w%elem_descr(isize) = "ion heating by LH"
        w%component(isize) = "LH"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_lhrf-1)"
        IF(ALLOCATED(ss%pilh)) THEN
           IF(ss%hash_codes(ps_phash_pilh).LT.0) THEN
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

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pilh_src
        w%list_elems(isize) = "pilh_src"
        w%elem_descr(isize) = "LH ion heating by antenna"
        w%component(isize) = "LH"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_lhrf-1,nlhrf_src)"
        IF(ALLOCATED(ss%pilh_src)) THEN
           IF(ss%hash_codes(ps_phash_pilh_src).LT.0) THEN
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
        isize = isize + 1  ! pi_anom
        w%list_elems(isize) = "pi_anom"
        w%elem_descr(isize) = "anomalous ion heating"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_anomth-1)"
        IF(ALLOCATED(ss%pi_anom)) THEN
           IF(ss%hash_codes(ps_phash_pi_anom).LT.0) THEN
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
        isize = isize + 1  ! pi_trans
        w%list_elems(isize) = "pi_trans"
        w%elem_descr(isize) = "ion transport (loss) power"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%pi_trans)) THEN
           IF(ss%hash_codes(ps_phash_pi_trans).LT.0) THEN
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
        isize = isize + 1  ! pi_transcat
        w%list_elems(isize) = "pi_transcat"
        w%elem_descr(isize) = "ion transport"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho-1,ntrcat)"
        IF(ALLOCATED(ss%pi_transcat)) THEN
           IF(ss%hash_codes(ps_phash_pi_transcat).LT.0) THEN
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

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pmine
        w%list_elems(isize) = "pmine"
        w%elem_descr(isize) = "electron heating by minority ions"
        w%component(isize) = "IC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_icrf-1)"
        IF(ALLOCATED(ss%pmine)) THEN
           IF(ss%hash_codes(ps_phash_pmine).LT.0) THEN
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

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pmini
        w%list_elems(isize) = "pmini"
        w%elem_descr(isize) = "thermal ion heating by minority ions"
        w%component(isize) = "IC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_icrf-1)"
        IF(ALLOCATED(ss%pmini)) THEN
           IF(ss%hash_codes(ps_phash_pmini).LT.0) THEN
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

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pminth
        w%list_elems(isize) = "pminth"
        w%elem_descr(isize) = "(de)thermalization of minority ions"
        w%component(isize) = "IC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_icrf-1)"
        IF(ALLOCATED(ss%pminth)) THEN
           IF(ss%hash_codes(ps_phash_pminth).LT.0) THEN
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
        isize = isize + 1  ! pohme
        w%list_elems(isize) = "pohme"
        w%elem_descr(isize) = "Ohmic heating of electrons"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%pohme)) THEN
           IF(ss%hash_codes(ps_phash_pohme).LT.0) THEN
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
        isize = isize + 1  ! pot_ky_tglf
        w%list_elems(isize) = "pot_ky_tglf"
        w%elem_descr(isize) = "potential fluctuation spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%pot_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_pot_ky_tglf).LT.0) THEN
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

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! prad
        w%list_elems(isize) = "prad"
        w%elem_descr(isize) = "total radiated power"
        w%component(isize) = "RAD"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_rad-1)"
        IF(ALLOCATED(ss%prad)) THEN
           IF(ss%hash_codes(ps_phash_prad).LT.0) THEN
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
        isize = isize + 1  ! prad_br
        w%list_elems(isize) = "prad_br"
        w%elem_descr(isize) = "bremsstrahlung contribution"
        w%component(isize) = "RAD"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_rad-1)"
        IF(ALLOCATED(ss%prad_br)) THEN
           IF(ss%hash_codes(ps_phash_prad_br).LT.0) THEN
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
        isize = isize + 1  ! prad_cy
        w%list_elems(isize) = "prad_cy"
        w%elem_descr(isize) = "cyclotron radiation contribution"
        w%component(isize) = "RAD"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_rad-1)"
        IF(ALLOCATED(ss%prad_cy)) THEN
           IF(ss%hash_codes(ps_phash_prad_cy).LT.0) THEN
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
        isize = isize + 1  ! prad_li
        w%list_elems(isize) = "prad_li"
        w%elem_descr(isize) = "line radiation contribution"
        w%component(isize) = "RAD"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_rad-1)"
        IF(ALLOCATED(ss%prad_li)) THEN
           IF(ss%hash_codes(ps_phash_prad_li).LT.0) THEN
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
        isize = isize + 1  ! psc_halo
        w%list_elems(isize) = "psc_halo"
        w%elem_descr(isize) = "beam halo power: recapture - pb0_halo"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%psc_halo)) THEN
           IF(ss%hash_codes(ps_phash_psc_halo).LT.0) THEN
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
        isize = isize + 1  ! psc_reco
        w%list_elems(isize) = "psc_reco"
        w%elem_descr(isize) = "reco power: recapture - p0_reco"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_gas-1)"
        IF(ALLOCATED(ss%psc_reco)) THEN
           IF(ss%hash_codes(ps_phash_psc_reco).LT.0) THEN
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
        isize = isize + 1  ! psipol
        w%list_elems(isize) = "psipol"
        w%elem_descr(isize) = "poloidal flux vs. rho"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "Wb/rad"
        w%c256val(isize) = "(nrho_eq)"
        IF(ALLOCATED(ss%psipol)) THEN
           IF(ss%hash_codes(ps_phash_psipol).LT.0) THEN
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
        isize = isize + 1  ! PsiRZ
        w%list_elems(isize) = "PsiRZ"
        w%elem_descr(isize) = "Psi(R,Z)"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "Wb/rad"
        w%c256val(isize) = "(nr,nz)"
        IF(ALLOCATED(ss%PsiRZ)) THEN
           IF(ss%hash_codes(ps_phash_PsiRZ).LT.0) THEN
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
        isize = isize + 1  ! psmom_errck
        w%list_elems(isize) = "psmom_errck"
        w%elem_descr(isize) = "PS moments sum convergence"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%psmom_errck)) THEN
           IF(ss%hash_codes(ps_phash_psmom_errck).LT.0) THEN
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
        isize = isize + 1  ! psmom_nc
        w%list_elems(isize) = "psmom_nc"
        w%elem_descr(isize) = "PS moments"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m^-2"
        w%c256val(isize) = "(nrho_eq_geo,npsmom)"
        IF(ALLOCATED(ss%psmom_nc)) THEN
           IF(ss%hash_codes(ps_phash_psmom_nc).LT.0) THEN
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
        isize = isize + 1  ! P_eq
        w%list_elems(isize) = "P_eq"
        w%elem_descr(isize) = "equilibrium scalar pressure"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "Pa"
        w%c256val(isize) = "(nrho_eq)"
        IF(ALLOCATED(ss%P_eq)) THEN
           IF(ss%hash_codes(ps_phash_P_eq).LT.0) THEN
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

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qbeami
        w%list_elems(isize) = "qbeami"
        w%elem_descr(isize) = "average beam ion charge"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "C"
        w%c256val(isize) = "(nrho_nbi-1,nspec_beam)"
        IF(ALLOCATED(ss%qbeami)) THEN
           IF(ss%hash_codes(ps_phash_qbeami).LT.0) THEN
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

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qcomp_e
        w%list_elems(isize) = "qcomp_e"
        w%elem_descr(isize) = "Compressional electron heating"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%qcomp_e)) THEN
           IF(ss%hash_codes(ps_phash_qcomp_e).LT.0) THEN
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
        isize = isize + 1  ! qcomp_i
        w%list_elems(isize) = "qcomp_i"
        w%elem_descr(isize) = "Compressional ion heating"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%qcomp_i)) THEN
           IF(ss%hash_codes(ps_phash_qcomp_i).LT.0) THEN
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
        isize = isize + 1  ! qie
        w%list_elems(isize) = "qie"
        w%elem_descr(isize) = "Ion->Electron heat coupling"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%qie)) THEN
           IF(ss%hash_codes(ps_phash_qie).LT.0) THEN
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
END SUBROUTINE plasma_state_contents_profs_5
