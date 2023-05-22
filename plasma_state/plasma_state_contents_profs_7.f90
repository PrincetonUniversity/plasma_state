! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_contents_profs_7(itype,icomponent,ss,w,isize)


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

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sn_transcat
        w%list_elems(isize) = "sn_transcat"
        w%elem_descr(isize) = "species transport"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho-1,0:nspec_th,ntrcat)"
        IF(ALLOCATED(ss%sn_transcat)) THEN
           IF(ss%hash_codes(ps_phash_sn_transcat).LT.0) THEN
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
        isize = isize + 1  ! sprof0
        w%list_elems(isize) = "sprof0"
        w%elem_descr(isize) = "gas species source/sinks per unit species influx"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "(#/sec)/(#/sec)"
        w%c256val(isize) = "(nrho_gas-1,nspec_gas,ngsc0)"
        IF(ALLOCATED(ss%sprof0)) THEN
           IF(ss%hash_codes(ps_phash_sprof0).LT.0) THEN
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
        isize = isize + 1  ! sprof0e
        w%list_elems(isize) = "sprof0e"
        w%elem_descr(isize) = "electron source per unit neutral species influx"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "(#/sec)/(#/sec)"
        w%c256val(isize) = "(nrho_gas-1,ngsc0)"
        IF(ALLOCATED(ss%sprof0e)) THEN
           IF(ss%hash_codes(ps_phash_sprof0e).LT.0) THEN
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

  IF(icomponent(ps_cnum_PEL).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! spsce
        w%list_elems(isize) = "spsce"
        w%elem_descr(isize) = "net source, thermal ions & electrons"
        w%component(isize) = "PEL"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nrho_pel-1,0:nspec_tha)"
        IF(ALLOCATED(ss%spsce)) THEN
           IF(ss%hash_codes(ps_phash_spsce).LT.0) THEN
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
        isize = isize + 1  ! squareLO
        w%list_elems(isize) = "squareLO"
        w%elem_descr(isize) = "lower outer squareness"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%squareLO)) THEN
           IF(ss%hash_codes(ps_phash_squareLO).LT.0) THEN
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
        isize = isize + 1  ! squareUO
        w%list_elems(isize) = "squareUO"
        w%elem_descr(isize) = "upper outer squareness"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%squareUO)) THEN
           IF(ss%hash_codes(ps_phash_squareUO).LT.0) THEN
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
        isize = isize + 1  ! surf
        w%list_elems(isize) = "surf"
        w%elem_descr(isize) = "area of flux surfaces"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m^2"
        w%c256val(isize) = "(nrho_eq)"
        IF(ALLOCATED(ss%surf)) THEN
           IF(ss%hash_codes(ps_phash_surf).LT.0) THEN
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

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! T0cx
        w%list_elems(isize) = "T0cx"
        w%elem_descr(isize) = "effective neutral temperature for CX power"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho_gas-1,ngsc0)"
        IF(ALLOCATED(ss%T0cx)) THEN
           IF(ss%hash_codes(ps_phash_T0cx).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*QQCX"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! T0sc0
        w%list_elems(isize) = "T0sc0"
        w%elem_descr(isize) = "neutral temperature (2/3)<E0>"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho_gas-1,nspec_gas,ngsc0)"
        IF(ALLOCATED(ss%T0sc0)) THEN
           IF(ss%hash_codes(ps_phash_T0sc0).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*N0NORM"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! T0_halo
        w%list_elems(isize) = "T0_halo"
        w%elem_descr(isize) = "halo neutral temperature (2/3)<E0>"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho_nbi-1,nspec_gas)"
        IF(ALLOCATED(ss%T0_halo)) THEN
           IF(ss%hash_codes(ps_phash_T0_halo).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*N0_HALO"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! T0_reco
        w%list_elems(isize) = "T0_reco"
        w%elem_descr(isize) = "reco neutral temperature (2/3)<E0>"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho_gas-1,nspec_gas)"
        IF(ALLOCATED(ss%T0_reco)) THEN
           IF(ss%hash_codes(ps_phash_T0_reco).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*N0_RECO"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! teflux_ky_tglf
        w%list_elems(isize) = "teflux_ky_tglf"
        w%elem_descr(isize) = "electron density flux spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%teflux_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_teflux_ky_tglf).LT.0) THEN
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
        isize = isize + 1  ! te_ky_tglf
        w%list_elems(isize) = "te_ky_tglf"
        w%elem_descr(isize) = "electron density fluctuation spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%te_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_te_ky_tglf).LT.0) THEN
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

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tfrip2_log
        w%list_elems(isize) = "tfrip2_log"
        w%elem_descr(isize) = "2nd component of TF ripple"
        w%component(isize) = "RIPPLE"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nr_rip,nz_rip)"
        IF(ALLOCATED(ss%tfrip2_log)) THEN
           IF(ss%hash_codes(ps_phash_tfrip2_log).LT.0) THEN
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

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tfrip_log
        w%list_elems(isize) = "tfrip_log"
        w%elem_descr(isize) = "TF ripple: log((Bmax-Bmin)/(Bmax+Bmin))"
        w%component(isize) = "RIPPLE"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nr_rip,nz_rip)"
        IF(ALLOCATED(ss%tfrip_log)) THEN
           IF(ss%hash_codes(ps_phash_tfrip_log).LT.0) THEN
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
        isize = isize + 1  ! Ti
        w%list_elems(isize) = "Ti"
        w%elem_descr(isize) = "ion temperature"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%Ti)) THEN
           IF(ss%hash_codes(ps_phash_Ti).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*NI"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tiflux_ky_tglf
        w%list_elems(isize) = "tiflux_ky_tglf"
        w%elem_descr(isize) = "ion density flux spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%tiflux_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_tiflux_ky_tglf).LT.0) THEN
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
        isize = isize + 1  ! ti_ky_tglf
        w%list_elems(isize) = "ti_ky_tglf"
        w%elem_descr(isize) = "ion density fluctuation spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%ti_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_ti_ky_tglf).LT.0) THEN
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
        isize = isize + 1  ! tmodel
        w%list_elems(isize) = "tmodel"
        w%elem_descr(isize) = "target temperature profile"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nrho_anomth-1)"
        IF(ALLOCATED(ss%tmodel)) THEN
           IF(ss%hash_codes(ps_phash_tmodel).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*NMODEL"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tq0_reco
        w%list_elems(isize) = "tq0_reco"
        w%elem_descr(isize) = "torque of recombination neutral source"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_gas-1)"
        IF(ALLOCATED(ss%tq0_reco)) THEN
           IF(ss%hash_codes(ps_phash_tq0_reco).LT.0) THEN
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
        isize = isize + 1  ! tqb0_halo
        w%list_elems(isize) = "tqb0_halo"
        w%elem_descr(isize) = "torque of halo neutral source"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%tqb0_halo)) THEN
           IF(ss%hash_codes(ps_phash_tqb0_halo).LT.0) THEN
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
        isize = isize + 1  ! tqbe
        w%list_elems(isize) = "tqbe"
        w%elem_descr(isize) = "collisional beam torque to electrons"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%tqbe)) THEN
           IF(ss%hash_codes(ps_phash_tqbe).LT.0) THEN
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
        isize = isize + 1  ! tqbi
        w%list_elems(isize) = "tqbi"
        w%elem_descr(isize) = "collisional beam torque to thermal ions"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%tqbi)) THEN
           IF(ss%hash_codes(ps_phash_tqbi).LT.0) THEN
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
        isize = isize + 1  ! tqbjxb
        w%list_elems(isize) = "tqbjxb"
        w%elem_descr(isize) = "JxB beam torque"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%tqbjxb)) THEN
           IF(ss%hash_codes(ps_phash_tqbjxb).LT.0) THEN
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
        isize = isize + 1  ! tqbth
        w%list_elems(isize) = "tqbth"
        w%elem_descr(isize) = "momentum in thermalization of beam ions"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%tqbth)) THEN
           IF(ss%hash_codes(ps_phash_tqbth).LT.0) THEN
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
        isize = isize + 1  ! tqcx_halo
        w%list_elems(isize) = "tqcx_halo"
        w%elem_descr(isize) = "beam halo driven CX momentum torque"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%tqcx_halo)) THEN
           IF(ss%hash_codes(ps_phash_tqcx_halo).LT.0) THEN
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
        isize = isize + 1  ! tqcx_reco
        w%list_elems(isize) = "tqcx_reco"
        w%elem_descr(isize) = "reco driven CX momentum torque"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_gas-1)"
        IF(ALLOCATED(ss%tqcx_reco)) THEN
           IF(ss%hash_codes(ps_phash_tqcx_reco).LT.0) THEN
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
        isize = isize + 1  ! tqioniz
        w%list_elems(isize) = "tqioniz"
        w%elem_descr(isize) = "ionization torque per unit influx (sc0)"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "Nt*m/(#/sec)"
        w%c256val(isize) = "(nrho_gas-1,ngsc0)"
        IF(ALLOCATED(ss%tqioniz)) THEN
           IF(ss%hash_codes(ps_phash_tqioniz).LT.0) THEN
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
        isize = isize + 1  ! tqqcx
        w%list_elems(isize) = "tqqcx"
        w%elem_descr(isize) = "cx torque per unit influx per (omeg0-omegi)"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "Nt*m/(#/sec)/(rad/sec)"
        w%c256val(isize) = "(nrho_gas-1,ngsc0)"
        IF(ALLOCATED(ss%tqqcx)) THEN
           IF(ss%hash_codes(ps_phash_tqqcx).LT.0) THEN
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
        isize = isize + 1  ! tqsc_halo
        w%list_elems(isize) = "tqsc_halo"
        w%elem_descr(isize) = "beam halo torque: recapture - tqb0_halo"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%tqsc_halo)) THEN
           IF(ss%hash_codes(ps_phash_tqsc_halo).LT.0) THEN
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
        isize = isize + 1  ! tqsc_reco
        w%list_elems(isize) = "tqsc_reco"
        w%elem_descr(isize) = "reco torque: recapture - tq0_reco"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_gas-1)"
        IF(ALLOCATED(ss%tqsc_reco)) THEN
           IF(ss%hash_codes(ps_phash_tqsc_reco).LT.0) THEN
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
        isize = isize + 1  ! tq_anom
        w%list_elems(isize) = "tq_anom"
        w%elem_descr(isize) = "anomalous torque"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_anomth-1)"
        IF(ALLOCATED(ss%tq_anom)) THEN
           IF(ss%hash_codes(ps_phash_tq_anom).LT.0) THEN
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
        isize = isize + 1  ! tq_trans
        w%list_elems(isize) = "tq_trans"
        w%elem_descr(isize) = "angular momentum transport (loss)"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%tq_trans)) THEN
           IF(ss%hash_codes(ps_phash_tq_trans).LT.0) THEN
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
        isize = isize + 1  ! tq_transcat
        w%list_elems(isize) = "tq_transcat"
        w%elem_descr(isize) = "toroidal angular momentum transport"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho-1,ntrcat)"
        IF(ALLOCATED(ss%tq_transcat)) THEN
           IF(ss%hash_codes(ps_phash_tq_transcat).LT.0) THEN
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
        isize = isize + 1  ! triang
        w%list_elems(isize) = "triang"
        w%elem_descr(isize) = "triangularity (symmetrized)"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%triang)) THEN
           IF(ss%hash_codes(ps_phash_triang).LT.0) THEN
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
        isize = isize + 1  ! triangL
        w%list_elems(isize) = "triangL"
        w%elem_descr(isize) = "lower triangularity"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%triangL)) THEN
           IF(ss%hash_codes(ps_phash_triangL).LT.0) THEN
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
        isize = isize + 1  ! triangU
        w%list_elems(isize) = "triangU"
        w%elem_descr(isize) = "upper triangularity"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%triangU)) THEN
           IF(ss%hash_codes(ps_phash_triangU).LT.0) THEN
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
END SUBROUTINE plasma_state_contents_profs_7
