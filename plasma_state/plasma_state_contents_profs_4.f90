! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_contents_profs_4(itype,icomponent,ss,w,isize)


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
        isize = isize + 1  ! kyrhos_mtm
        w%list_elems(isize) = "kyrhos_mtm"
        w%elem_descr(isize) = "ky*rhos at max growth rate for MTM model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%kyrhos_mtm)) THEN
           IF(ss%hash_codes(ps_phash_kyrhos_mtm).LT.0) THEN
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
        isize = isize + 1  ! kyspectrum_tglf
        w%list_elems(isize) = "kyspectrum_tglf"
        w%elem_descr(isize) = "ky-spectrum from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%kyspectrum_tglf)) THEN
           IF(ss%hash_codes(ps_phash_kyspectrum_tglf).LT.0) THEN
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
        isize = isize + 1  ! Lpol
        w%list_elems(isize) = "Lpol"
        w%elem_descr(isize) = "poloidal path length"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq)"
        IF(ALLOCATED(ss%Lpol)) THEN
           IF(ss%hash_codes(ps_phash_Lpol).LT.0) THEN
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
        isize = isize + 1  ! n0norm
        w%list_elems(isize) = "n0norm"
        w%elem_descr(isize) = "neutral density per unit gas influx (sc0)"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "(m^-3)/(#/sec)"
        w%c256val(isize) = "(nrho_gas-1,nspec_gas,ngsc0)"
        IF(ALLOCATED(ss%n0norm)) THEN
           IF(ss%hash_codes(ps_phash_n0norm).LT.0) THEN
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
        isize = isize + 1  ! n0_halo
        w%list_elems(isize) = "n0_halo"
        w%elem_descr(isize) = "halo thermal neutral density"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(nrho_nbi-1,nspec_gas)"
        IF(ALLOCATED(ss%n0_halo)) THEN
           IF(ss%hash_codes(ps_phash_n0_halo).LT.0) THEN
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

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! n0_reco
        w%list_elems(isize) = "n0_reco"
        w%elem_descr(isize) = "reco thermal neutral density"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(nrho_gas-1,nspec_gas)"
        IF(ALLOCATED(ss%n0_reco)) THEN
           IF(ss%hash_codes(ps_phash_n0_reco).LT.0) THEN
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
        isize = isize + 1  ! nbeami
        w%list_elems(isize) = "nbeami"
        w%elem_descr(isize) = "beam species density"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(nrho_nbi-1,nspec_beam)"
        IF(ALLOCATED(ss%nbeami)) THEN
           IF(ss%hash_codes(ps_phash_nbeami).LT.0) THEN
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

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! neflux_ky_tglf
        w%list_elems(isize) = "neflux_ky_tglf"
        w%elem_descr(isize) = "electron density flux spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%neflux_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_neflux_ky_tglf).LT.0) THEN
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
        isize = isize + 1  ! ne_ky_tglf
        w%list_elems(isize) = "ne_ky_tglf"
        w%elem_descr(isize) = "electron density fluctuation spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%ne_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_ne_ky_tglf).LT.0) THEN
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

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! nfusi
        w%list_elems(isize) = "nfusi"
        w%elem_descr(isize) = "fusion ion density"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(nrho_fus-1,nspec_fusion)"
        IF(ALLOCATED(ss%nfusi)) THEN
           IF(ss%hash_codes(ps_phash_nfusi).LT.0) THEN
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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ngradb2_av
        w%list_elems(isize) = "ngradb2_av"
        w%elem_descr(isize) = "<(B.grad(B)/|B|)^2>"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T^2*m^-2"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%ngradb2_av)) THEN
           IF(ss%hash_codes(ps_phash_ngradb2_av).LT.0) THEN
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
        isize = isize + 1  ! ni
        w%list_elems(isize) = "ni"
        w%elem_descr(isize) = "total thermal ion density"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%ni)) THEN
           IF(ss%hash_codes(ps_phash_ni).LT.0) THEN
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

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! niflux_ky_tglf
        w%list_elems(isize) = "niflux_ky_tglf"
        w%elem_descr(isize) = "ion density flux spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%niflux_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_niflux_ky_tglf).LT.0) THEN
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
        isize = isize + 1  ! ni_ky_tglf
        w%list_elems(isize) = "ni_ky_tglf"
        w%elem_descr(isize) = "ion density fluctuation spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%ni_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_ni_ky_tglf).LT.0) THEN
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
        isize = isize + 1  ! nmini
        w%list_elems(isize) = "nmini"
        w%elem_descr(isize) = "minority species density"
        w%component(isize) = "IC"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(nrho_icrf-1,nspec_rfmin)"
        IF(ALLOCATED(ss%nmini)) THEN
           IF(ss%hash_codes(ps_phash_nmini).LT.0) THEN
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

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! nmodel
        w%list_elems(isize) = "nmodel"
        w%elem_descr(isize) = "target density profile"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(nrho_anomth-1)"
        IF(ALLOCATED(ss%nmodel)) THEN
           IF(ss%hash_codes(ps_phash_nmodel).LT.0) THEN
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

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! nrw
        w%list_elems(isize) = "nrw"
        w%elem_descr(isize) = "runaway electron density"
        w%component(isize) = "RUNAWAY"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(nrho_rw-1)"
        IF(ALLOCATED(ss%nrw)) THEN
           IF(ss%hash_codes(ps_phash_nrw).LT.0) THEN
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
        isize = isize + 1  ! ns
        w%list_elems(isize) = "ns"
        w%elem_descr(isize) = "thermal specie density"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(nrho-1,0:nspec_th)"
        IF(ALLOCATED(ss%ns)) THEN
           IF(ss%hash_codes(ps_phash_ns).LT.0) THEN
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

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! nzflux_ky_tglf
        w%list_elems(isize) = "nzflux_ky_tglf"
        w%elem_descr(isize) = "impurity density flux spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%nzflux_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_nzflux_ky_tglf).LT.0) THEN
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
        isize = isize + 1  ! nz_ky_tglf
        w%list_elems(isize) = "nz_ky_tglf"
        w%elem_descr(isize) = "impurity density fluctuation spectrum"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth,nky_anomth)"
        IF(ALLOCATED(ss%nz_ky_tglf)) THEN
           IF(ss%hash_codes(ps_phash_nz_ky_tglf).LT.0) THEN
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
        isize = isize + 1  ! omeg0cx
        w%list_elems(isize) = "omeg0cx"
        w%elem_descr(isize) = "effective neutral angular velocity for CX torque"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "rad/sec"
        w%c256val(isize) = "(nrho_gas-1,ngsc0)"
        IF(ALLOCATED(ss%omeg0cx)) THEN
           IF(ss%hash_codes(ps_phash_omeg0cx).LT.0) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
        w%val_field(isize) = "STEP*TQQCX"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! omeg0sc0
        w%list_elems(isize) = "omeg0sc0"
        w%elem_descr(isize) = "toroidal angular velocity"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "(rad/sec)"
        w%c256val(isize) = "(nrho_gas-1,nspec_gas,ngsc0)"
        IF(ALLOCATED(ss%omeg0sc0)) THEN
           IF(ss%hash_codes(ps_phash_omeg0sc0).LT.0) THEN
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
        isize = isize + 1  ! omeg0_halo
        w%list_elems(isize) = "omeg0_halo"
        w%elem_descr(isize) = "halo neutral toroidal angular velocity"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "rad/sec"
        w%c256val(isize) = "(nrho_nbi-1,nspec_gas)"
        IF(ALLOCATED(ss%omeg0_halo)) THEN
           IF(ss%hash_codes(ps_phash_omeg0_halo).LT.0) THEN
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
        isize = isize + 1  ! omeg0_reco
        w%list_elems(isize) = "omeg0_reco"
        w%elem_descr(isize) = "reco neutral toroidal angular velocity"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "rad/sec"
        w%c256val(isize) = "(nrho_gas-1,nspec_gas)"
        IF(ALLOCATED(ss%omeg0_reco)) THEN
           IF(ss%hash_codes(ps_phash_omeg0_reco).LT.0) THEN
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

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! omegat
        w%list_elems(isize) = "omegat"
        w%elem_descr(isize) = "ion toroidal angular velocity"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "rad/sec"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%omegat)) THEN
           IF(ss%hash_codes(ps_phash_omegat).LT.0) THEN
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
        isize = isize + 1  ! omega_exb
        w%list_elems(isize) = "omega_exb"
        w%elem_descr(isize) = "ExB flow shear rate"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "GB"
        w%c256val(isize) = "(nrho_anomth)"
        IF(ALLOCATED(ss%omega_exb)) THEN
           IF(ss%hash_codes(ps_phash_omega_exb).LT.0) THEN
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
        isize = isize + 1  ! p0_reco
        w%list_elems(isize) = "p0_reco"
        w%elem_descr(isize) = "power in recombination neutral source"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_gas-1)"
        IF(ALLOCATED(ss%p0_reco)) THEN
           IF(ss%hash_codes(ps_phash_p0_reco).LT.0) THEN
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
        isize = isize + 1  ! pb0_halo
        w%list_elems(isize) = "pb0_halo"
        w%elem_descr(isize) = "power in halo neutral source"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%pb0_halo)) THEN
           IF(ss%hash_codes(ps_phash_pb0_halo).LT.0) THEN
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
        isize = isize + 1  ! pbe
        w%list_elems(isize) = "pbe"
        w%elem_descr(isize) = "electron heating by all beam ions"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%pbe)) THEN
           IF(ss%hash_codes(ps_phash_pbe).LT.0) THEN
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
        isize = isize + 1  ! pbi
        w%list_elems(isize) = "pbi"
        w%elem_descr(isize) = "thermal ion heating by all beam ions"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%pbi)) THEN
           IF(ss%hash_codes(ps_phash_pbi).LT.0) THEN
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
        isize = isize + 1  ! pbth
        w%list_elems(isize) = "pbth"
        w%elem_descr(isize) = "thermalization of beam ions"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%pbth)) THEN
           IF(ss%hash_codes(ps_phash_pbth).LT.0) THEN
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
        isize = isize + 1  ! pcx_halo
        w%list_elems(isize) = "pcx_halo"
        w%elem_descr(isize) = "beam halo driven CX power"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%pcx_halo)) THEN
           IF(ss%hash_codes(ps_phash_pcx_halo).LT.0) THEN
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
        isize = isize + 1  ! pcx_reco
        w%list_elems(isize) = "pcx_reco"
        w%elem_descr(isize) = "reco driven CX power"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_gas-1)"
        IF(ALLOCATED(ss%pcx_reco)) THEN
           IF(ss%hash_codes(ps_phash_pcx_reco).LT.0) THEN
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

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! peech
        w%list_elems(isize) = "peech"
        w%elem_descr(isize) = "electron heating by ECH"
        w%component(isize) = "EC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_ecrf-1)"
        IF(ALLOCATED(ss%peech)) THEN
           IF(ss%hash_codes(ps_phash_peech).LT.0) THEN
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

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! peech_src
        w%list_elems(isize) = "peech_src"
        w%elem_descr(isize) = "ECH heating (by antenna)"
        w%component(isize) = "EC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_ecrf-1,necrf_src)"
        IF(ALLOCATED(ss%peech_src)) THEN
           IF(ss%hash_codes(ps_phash_peech_src).LT.0) THEN
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
        isize = isize + 1  ! pelh
        w%list_elems(isize) = "pelh"
        w%elem_descr(isize) = "electron heating by LH"
        w%component(isize) = "LH"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_lhrf-1)"
        IF(ALLOCATED(ss%pelh)) THEN
           IF(ss%hash_codes(ps_phash_pelh).LT.0) THEN
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
        isize = isize + 1  ! pelh_src
        w%list_elems(isize) = "pelh_src"
        w%elem_descr(isize) = "LH electron heating by antenna"
        w%component(isize) = "LH"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_lhrf-1,nlhrf_src)"
        IF(ALLOCATED(ss%pelh_src)) THEN
           IF(ss%hash_codes(ps_phash_pelh_src).LT.0) THEN
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
END SUBROUTINE plasma_state_contents_profs_4
