! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_contents(itype,icomponent,ss,w,ierr)

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

  INTEGER, INTENT(OUT) :: ierr   ! status code on exit, 0=normal

  !local:

  INTEGER :: iout,isize,id,inum
  INTEGER :: i1,i2,i3,i4

  !----------------
  iout = SWIM_err_out_file
  ierr = 0

  isize = 0

  ! Enumeration Dimensions

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! nky_anomth
        w%list_elems(isize) = "nky_anomth"
        w%elem_descr(isize) = "number of points, ky grid ( anomalous data over ky spectrum)"
        w%component(isize) = "ANOM"
        w%ival(isize) = ss%nky_anomth
        w%val_field(isize) = "nky_anomth"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! nky_mmm
        w%list_elems(isize) = "nky_mmm"
        w%elem_descr(isize) = "number of points, ky grid for MMM model"
        w%component(isize) = "ANOM"
        w%ival(isize) = ss%nky_mmm
        w%val_field(isize) = "nky_mmm"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! max_nrz_antgeo
        w%list_elems(isize) = "max_nrz_antgeo"
        w%elem_descr(isize) = "Maximum size of variable length enumeration: NRZ_ANTGEO"
        w%component(isize) = "IC"
        w%ival(isize) = ss%max_nrz_antgeo
        w%val_field(isize) = "nrz_antgeo(nicrf_src)"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! max_nspectrumlh_src
        w%list_elems(isize) = "max_nspectrumlh_src"
        w%elem_descr(isize) = "Maximum size of variable length enumeration: NSPECTRUMLH_SRC"
        w%component(isize) = "LH"
        w%ival(isize) = ss%max_nspectrumlh_src
        w%val_field(isize) = "nspectrumlh_src(nlhrf_src)"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! max_num_nphi
        w%list_elems(isize) = "max_num_nphi"
        w%elem_descr(isize) = "Maximum size of variable length enumeration: NUM_NPHI"
        w%component(isize) = "IC"
        w%ival(isize) = ss%max_num_nphi
        w%val_field(isize) = "num_nphi(nicrf_src)"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! max_num_nphi_vac
        w%list_elems(isize) = "max_num_nphi_vac"
        w%elem_descr(isize) = "Maximum size of variable length enumeration: NUM_NPHI_VAC"
        w%component(isize) = "IC"
        w%ival(isize) = ss%max_num_nphi_vac
        w%val_field(isize) = "num_nphi_vac(nicrf_src)"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! num_rzlim
        w%list_elems(isize) = "num_rzlim"
        w%elem_descr(isize) = "number of points, axisymmetric (R,Z) limiter"
        w%component(isize) = "EQ"
        w%ival(isize) = ss%num_rzlim
        w%val_field(isize) = "num_rzlim"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! n_D_einj_standard
        w%list_elems(isize) = "n_D_einj_standard"
        w%elem_descr(isize) = "no. pts in D beam energy fractions table"
        w%component(isize) = "NBI"
        w%ival(isize) = ss%n_d_einj_standard
        w%val_field(isize) = "n_D_einj_standard"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! n_H_einj_standard
        w%list_elems(isize) = "n_H_einj_standard"
        w%elem_descr(isize) = "no. pts in H beam energy fractions table"
        w%component(isize) = "NBI"
        w%ival(isize) = ss%n_h_einj_standard
        w%val_field(isize) = "n_H_einj_standard"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! max_n_straps
        w%list_elems(isize) = "max_n_straps"
        w%elem_descr(isize) = "Maximum size of variable length enumeration: N_STRAPS"
        w%component(isize) = "IC"
        w%ival(isize) = ss%max_n_straps
        w%val_field(isize) = "n_straps(nicrf_src)"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! n_T_einj_standard
        w%list_elems(isize) = "n_T_einj_standard"
        w%elem_descr(isize) = "no. pts in T beam energy fractions table"
        w%component(isize) = "NBI"
        w%ival(isize) = ss%n_t_einj_standard
        w%val_field(isize) = "n_T_einj_standard"
     ENDIF
  ENDIF
  ! Species Lists

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! ALL
        w%list_elems(isize) = "ALL"
        w%elem_descr(isize) = "all species"
        w%component(isize) = "PLASMA"
        w%val_field(isize) = "(0:nspec_all)"
        IF(ss%nspec_all.EQ.0) then
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = ss%nspec_all +1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! ALLA
        w%list_elems(isize) = "ALLA"
        w%elem_descr(isize) = "all species abridged"
        w%component(isize) = "PLASMA"
        w%val_field(isize) = "(0:nspec_alla)"
        IF(ss%nspec_alla.EQ.0) then
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = ss%nspec_alla +1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! SNBI
        w%list_elems(isize) = "SNBI"
        w%elem_descr(isize) = "beam ion species"
        w%component(isize) = "NBI"
        w%val_field(isize) = "(nspec_beam)"
        w%ival(isize) = ss%nspec_beam
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! SFUS
        w%list_elems(isize) = "SFUS"
        w%elem_descr(isize) = "fusion product ion species"
        w%component(isize) = "FUS"
        w%val_field(isize) = "(nspec_fusion)"
        w%ival(isize) = ss%nspec_fusion
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! SGAS
        w%list_elems(isize) = "SGAS"
        w%elem_descr(isize) = "atomic neutral species in plasma core"
        w%component(isize) = "GAS"
        w%val_field(isize) = "(nspec_gas)"
        w%ival(isize) = ss%nspec_gas
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! SIMP0
        w%list_elems(isize) = "SIMP0"
        w%elem_descr(isize) = "atomic impurity species in simulation"
        w%component(isize) = "GAS"
        w%val_field(isize) = "(nspec_imp0)"
        w%ival(isize) = ss%nspec_imp0
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! SIMPI
        w%list_elems(isize) = "SIMPI"
        w%elem_descr(isize) = "impurity ion species in simulation"
        w%component(isize) = "PLASMA"
        w%val_field(isize) = "(nspec_impi)"
        w%ival(isize) = ss%nspec_impi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! RFMIN
        w%list_elems(isize) = "RFMIN"
        w%elem_descr(isize) = "ICRF minority species"
        w%component(isize) = "IC"
        w%val_field(isize) = "(nspec_rfmin)"
        w%ival(isize) = ss%nspec_rfmin
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! S
        w%list_elems(isize) = "S"
        w%elem_descr(isize) = "thermal species list"
        w%component(isize) = "PLASMA"
        w%val_field(isize) = "(0:nspec_th)"
        IF(ss%nspec_th.EQ.0) then
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = ss%nspec_th +1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! SA
        w%list_elems(isize) = "SA"
        w%elem_descr(isize) = "thermal species abridged"
        w%component(isize) = "PLASMA"
        w%val_field(isize) = "(0:nspec_tha)"
        IF(ss%nspec_tha.EQ.0) then
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = ss%nspec_tha +1
        ENDIF
     ENDIF
  ENDIF

  ! Item Lists

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! Tor_mode_label
        w%list_elems(isize) = "Tor_mode_label"
        w%elem_descr(isize) = "Toroidal modes to monitor"
        w%component(isize) = "LMHD"
        w%val_field(isize) = "(mmodes)"
        w%ival(isize) = ss%mmodes
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! Balloon_code
        w%list_elems(isize) = "Balloon_code"
        w%elem_descr(isize) = "name(s) of Ballooning code(s) used"
        w%component(isize) = "LMHD"
        w%val_field(isize) = "(nballoon)"
        w%ival(isize) = ss%nballoon
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! nbi_src_name
        w%list_elems(isize) = "nbi_src_name"
        w%elem_descr(isize) = "number & name of neutral beams"
        w%component(isize) = "NBI"
        w%val_field(isize) = "(nbeam)"
        w%ival(isize) = ss%nbeam
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! circuit_name
        w%list_elems(isize) = "circuit_name"
        w%elem_descr(isize) = "Number & names of circuits"
        w%component(isize) = "EQ"
        w%val_field(isize) = "(ncircuits)"
        w%ival(isize) = ss%ncircuits
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! coil_name
        w%list_elems(isize) = "coil_name"
        w%elem_descr(isize) = "Number & name of axisymmetric coils"
        w%component(isize) = "EQ"
        w%val_field(isize) = "(ncoils)"
        w%ival(isize) = ss%ncoils
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! DeltaW_code
        w%list_elems(isize) = "DeltaW_code"
        w%elem_descr(isize) = "name(s) of delta(W) code(s) used"
        w%component(isize) = "LMHD"
        w%val_field(isize) = "(ndelta_w)"
        w%ival(isize) = ss%ndelta_w
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! ecrf_src_name
        w%list_elems(isize) = "ecrf_src_name"
        w%elem_descr(isize) = "number & name of ECRF sources"
        w%component(isize) = "EC"
        w%val_field(isize) = "(necrf_src)"
        w%ival(isize) = ss%necrf_src
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! eqmom_num
        w%list_elems(isize) = "eqmom_num"
        w%elem_descr(isize) = "Equilibrium moments"
        w%component(isize) = "EQ"
        w%val_field(isize) = "(neqmom)"
        w%ival(isize) = ss%neqmom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! gs_name
        w%list_elems(isize) = "gs_name"
        w%elem_descr(isize) = "number & name of edge neutral gas sources"
        w%component(isize) = "GAS"
        w%val_field(isize) = "(ngsc0)"
        w%ival(isize) = ss%ngsc0
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! icrf_src_name
        w%list_elems(isize) = "icrf_src_name"
        w%elem_descr(isize) = "number & name of ICRF sources"
        w%component(isize) = "IC"
        w%val_field(isize) = "(nicrf_src)"
        w%ival(isize) = ss%nicrf_src
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! lhrf_src_name
        w%list_elems(isize) = "lhrf_src_name"
        w%elem_descr(isize) = "number & name of LHRF sources"
        w%component(isize) = "LH"
        w%val_field(isize) = "(nlhrf_src)"
        w%ival(isize) = ss%nlhrf_src
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! Newcomb_code
        w%list_elems(isize) = "Newcomb_code"
        w%elem_descr(isize) = "name(s) of Newcomb code(s) used"
        w%component(isize) = "LMHD"
        w%val_field(isize) = "(nnewcomb)"
        w%ival(isize) = ss%nnewcomb
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! psmom_num
        w%list_elems(isize) = "psmom_num"
        w%elem_descr(isize) = "Pfirsch-Schlutter moments"
        w%component(isize) = "EQ"
        w%val_field(isize) = "(npsmom)"
        w%ival(isize) = ss%npsmom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! trcat
        w%list_elems(isize) = "trcat"
        w%elem_descr(isize) = "Application defined list of"
        w%component(isize) = "PLASMA"
        w%val_field(isize) = "(ntrcat)"
        w%ival(isize) = ss%ntrcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! xs_name
        w%list_elems(isize) = "xs_name"
        w%elem_descr(isize) = "number & name of impurity sources"
        w%component(isize) = "RAD"
        w%val_field(isize) = "(nxs)"
        w%ival(isize) = ss%nxs
     ENDIF
  ENDIF

  ! Grids

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! E_anom
        w%list_elems(isize) = "E_anom"
        w%elem_descr(isize) = "(optional) energy grid (1d profiles)"
        w%component(isize) = "ANOM"
        w%val_field(isize) = "(nefi_anom)"
        w%ival(isize) = ss%nefi_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! E_anom2
        w%list_elems(isize) = "E_anom2"
        w%elem_descr(isize) = "energy grid (2d anomalous transport profiles)"
        w%component(isize) = "ANOM"
        w%val_field(isize) = "(nefi_anom2)"
        w%ival(isize) = ss%nefi_anom2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! R_grid
        w%list_elems(isize) = "R_grid"
        w%elem_descr(isize) = "R grid"
        w%component(isize) = "EQ"
        w%val_field(isize) = "(nr)"
        w%ival(isize) = ss%nr
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho
        w%list_elems(isize) = "rho"
        w%elem_descr(isize) = "rho grid (PLASMA)"
        w%component(isize) = "PLASMA"
        w%val_field(isize) = "(nrho)"
        w%ival(isize) = ss%nrho
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_anom
        w%list_elems(isize) = "rho_anom"
        w%elem_descr(isize) = "rho grid (1d anomalous transport profiles)"
        w%component(isize) = "ANOM"
        w%val_field(isize) = "(nrho_anom)"
        w%ival(isize) = ss%nrho_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_anom2
        w%list_elems(isize) = "rho_anom2"
        w%elem_descr(isize) = "rho grid (2d anomalous transport profiles)"
        w%component(isize) = "ANOM"
        w%val_field(isize) = "(nrho_anom2)"
        w%ival(isize) = ss%nrho_anom2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_anomth
        w%list_elems(isize) = "rho_anomth"
        w%elem_descr(isize) = "rho grid (1d anomalous data for 1d"
        w%component(isize) = "ANOM"
        w%val_field(isize) = "(nrho_anomth)"
        w%ival(isize) = ss%nrho_anomth
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_ecrf
        w%list_elems(isize) = "rho_ecrf"
        w%elem_descr(isize) = "rho grid -- ECRF"
        w%component(isize) = "EC"
        w%val_field(isize) = "(nrho_ecrf)"
        w%ival(isize) = ss%nrho_ecrf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_eq
        w%list_elems(isize) = "rho_eq"
        w%elem_descr(isize) = "rho grid (EQ)"
        w%component(isize) = "EQ"
        w%val_field(isize) = "(nrho_eq)"
        w%ival(isize) = ss%nrho_eq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_eq_geo
        w%list_elems(isize) = "rho_eq_geo"
        w%elem_descr(isize) = "rho grid (EQ flux surface averages)"
        w%component(isize) = "EQ"
        w%val_field(isize) = "(nrho_eq_geo)"
        w%ival(isize) = ss%nrho_eq_geo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_fus
        w%list_elems(isize) = "rho_fus"
        w%elem_descr(isize) = "rho grid -- Fusion products model"
        w%component(isize) = "FUS"
        w%val_field(isize) = "(nrho_fus)"
        w%ival(isize) = ss%nrho_fus
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_gas
        w%list_elems(isize) = "rho_gas"
        w%elem_descr(isize) = "rho grid (GAS -- neutral gas model)"
        w%component(isize) = "GAS"
        w%val_field(isize) = "(nrho_gas)"
        w%ival(isize) = ss%nrho_gas
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_icrf
        w%list_elems(isize) = "rho_icrf"
        w%elem_descr(isize) = "rho grid (RF)"
        w%component(isize) = "IC"
        w%val_field(isize) = "(nrho_icrf)"
        w%ival(isize) = ss%nrho_icrf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_lhrf
        w%list_elems(isize) = "rho_lhrf"
        w%elem_descr(isize) = "rho grid -- LHRF"
        w%component(isize) = "LH"
        w%val_field(isize) = "(nrho_lhrf)"
        w%ival(isize) = ss%nrho_lhrf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_lmhd
        w%list_elems(isize) = "rho_lmhd"
        w%elem_descr(isize) = "rho grid (LMHD component)"
        w%component(isize) = "LMHD"
        w%val_field(isize) = "(nrho_lmhd)"
        w%ival(isize) = ss%nrho_lmhd
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_mist
        w%list_elems(isize) = "rho_mist"
        w%elem_descr(isize) = "rho grid (PLASMA) impurity-driven sources"
        w%component(isize) = "RAD"
        w%val_field(isize) = "(nrho_mist)"
        w%ival(isize) = ss%nrho_mist
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_nbi
        w%list_elems(isize) = "rho_nbi"
        w%elem_descr(isize) = "rho grid -- NBI"
        w%component(isize) = "NBI"
        w%val_field(isize) = "(nrho_nbi)"
        w%ival(isize) = ss%nrho_nbi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PEL).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_pel
        w%list_elems(isize) = "rho_pel"
        w%elem_descr(isize) = "rho grid -- PEL"
        w%component(isize) = "PEL"
        w%val_field(isize) = "(nrho_pel)"
        w%ival(isize) = ss%nrho_pel
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_rad
        w%list_elems(isize) = "rho_rad"
        w%elem_descr(isize) = "rho grid (RAD)"
        w%component(isize) = "RAD"
        w%val_field(isize) = "(nrho_rad)"
        w%ival(isize) = ss%nrho_rad
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! rho_rw
        w%list_elems(isize) = "rho_rw"
        w%elem_descr(isize) = "rho grid -- RUNAWAY electrons"
        w%component(isize) = "RUNAWAY"
        w%val_field(isize) = "(nrho_rw)"
        w%ival(isize) = ss%nrho_rw
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! R_ripple
        w%list_elems(isize) = "R_ripple"
        w%elem_descr(isize) = "R grid for TF ripple field(s)"
        w%component(isize) = "RIPPLE"
        w%val_field(isize) = "(nr_rip)"
        w%ival(isize) = ss%nr_rip
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! th_eq
        w%list_elems(isize) = "th_eq"
        w%elem_descr(isize) = "theta grid (EQ)"
        w%component(isize) = "EQ"
        w%val_field(isize) = "(nth_eq)"
        w%ival(isize) = ss%nth_eq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! Z_grid
        w%list_elems(isize) = "Z_grid"
        w%elem_descr(isize) = "Z grid"
        w%component(isize) = "EQ"
        w%val_field(isize) = "(nz)"
        w%ival(isize) = ss%nz
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! Z_ripple
        w%list_elems(isize) = "Z_ripple"
        w%elem_descr(isize) = "Z grid for TF ripple field(s)"
        w%component(isize) = "RIPPLE"
        w%val_field(isize) = "(nz_rip)"
        w%ival(isize) = ss%nz_rip
     ENDIF
  ENDIF

  ! Machine Description

  CALL plasma_state_contents_mdescr(itype,icomponent,ss,w,isize)


  ! Shot Configuration

  CALL plasma_state_contents_sconfig(itype,icomponent,ss,w,isize)


  ! Simulation Init

  CALL plasma_state_contents_sini(itype,icomponent,ss,w,isize)


  ! State Data

  CALL plasma_state_contents_sdata(itype,icomponent,ss,w,isize)


  ! Profiles (split large file for faster compiling)

  CALL plasma_state_contents_profs_0(itype,icomponent,ss,w,isize)
  CALL plasma_state_contents_profs_1(itype,icomponent,ss,w,isize)
  CALL plasma_state_contents_profs_2(itype,icomponent,ss,w,isize)
  CALL plasma_state_contents_profs_3(itype,icomponent,ss,w,isize)
  CALL plasma_state_contents_profs_4(itype,icomponent,ss,w,isize)
  CALL plasma_state_contents_profs_5(itype,icomponent,ss,w,isize)
  CALL plasma_state_contents_profs_6(itype,icomponent,ss,w,isize)
  CALL plasma_state_contents_profs_7(itype,icomponent,ss,w,isize)
  CALL plasma_state_contents_profs_8(itype,icomponent,ss,w,isize)
  CALL plasma_state_contents_profs_9(itype,icomponent,ss,w,isize)


  ! Profile groups

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ANOM_DIFFUS
        w%list_elems(isize) = "ANOM_DIFFUS"
        w%elem_descr(isize) = "{difb_nbi,difb_fusi,difb_rfmi}"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho_anom)"
        IF(ALLOCATED(ss%difb_rfmi)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ANOM_VELOC
        w%list_elems(isize) = "ANOM_VELOC"
        w%elem_descr(isize) = "{velb_nbi,velb_fusi,velb_rfmi}"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho_anom)"
        IF(ALLOCATED(ss%velb_rfmi)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! BEAM_HALO_POWER
        w%list_elems(isize) = "BEAM_HALO_POWER"
        w%elem_descr(isize) = "{pb0_halo,psc_halo,pcx_halo}"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%pcx_halo)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! BEAM_HALO_TORQUE
        w%list_elems(isize) = "BEAM_HALO_TORQUE"
        w%elem_descr(isize) = "{tqb0_halo,tqsc_halo,tqcx_halo}"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%tqcx_halo)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! BEAM_HEATING
        w%list_elems(isize) = "BEAM_HEATING"
        w%elem_descr(isize) = "{pbe,pbi,pbth}"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%pbth)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! BEAM_TORQUE
        w%list_elems(isize) = "BEAM_TORQUE"
        w%elem_descr(isize) = "{tqbe,tqbi,tqbjxb,tqbth}"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_nbi-1)"
        IF(ALLOCATED(ss%tqbth)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! BMINMAX
        w%list_elems(isize) = "BMINMAX"
        w%elem_descr(isize) = "{B_surfMax,B_surfMin}"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%B_surfMin)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! COMPRESSION
        w%list_elems(isize) = "COMPRESSION"
        w%elem_descr(isize) = "{qcomp_e,qcomp_i}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%qcomp_i)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! DIFFUSIVITY
        w%list_elems(isize) = "DIFFUSIVITY"
        w%elem_descr(isize) = "{chie_trans,chii_trans,chimo_trans}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m^2/sec"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%chimo_trans)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! EQ_BAXIS
        w%list_elems(isize) = "EQ_BAXIS"
        w%elem_descr(isize) = "{B_axis,B_axis_vac}"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! EQ_BLCFS
        w%list_elems(isize) = "EQ_BLCFS"
        w%elem_descr(isize) = "{B_min_lcfs,B_max_lcfs}"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! EQ_BSIGNS
        w%list_elems(isize) = "EQ_BSIGNS"
        w%elem_descr(isize) = "{kccw_Bphi,kccw_Jphi}"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! EQ_FILES
        w%list_elems(isize) = "EQ_FILES"
        w%elem_descr(isize) = "{eqdsk_file}"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! EQ_RZAXIS
        w%list_elems(isize) = "EQ_RZAXIS"
        w%elem_descr(isize) = "{R_axis,Z_axis}"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EQ_RZBOX
        w%list_elems(isize) = "EQ_RZBOX"
        w%elem_descr(isize) = "{R_min_box,R_max_box,Z_min_box,Z_max_box}"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! EQ_RZLCFS
        w%list_elems(isize) = "EQ_RZLCFS"
        w%elem_descr(isize) = "{R_min_lcfs,R_max_lcfs,Z_min_lcfs,Z_max_lcfs}"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! FUSION_HEATING
        w%list_elems(isize) = "FUSION_HEATING"
        w%elem_descr(isize) = "{pfuse,pfusi,pfusth}"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_fus-1)"
        IF(ALLOCATED(ss%pfusth)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! ICRF_CONTROLS
        w%list_elems(isize) = "ICRF_CONTROLS"
        w%elem_descr(isize) = "{kdens_rfmin}"
        w%component(isize) = "IC"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! LMHD_FLAGS
        w%list_elems(isize) = "LMHD_FLAGS"
        w%elem_descr(isize) = "{MHD_eq_status,LMHD_status}"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! LOCK_VARIABLES
        w%list_elems(isize) = "LOCK_VARIABLES"
        w%elem_descr(isize) = "{lock_machine_descr,lock_shot_config,lock_sim_init}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! LOWER_HYBRID_HEATING
        w%list_elems(isize) = "LOWER_HYBRID_HEATING"
        w%elem_descr(isize) = "{pelh,pilh}"
        w%component(isize) = "LH"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_lhrf-1)"
        IF(ALLOCATED(ss%pilh)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! MIDPLANE
        w%list_elems(isize) = "MIDPLANE"
        w%elem_descr(isize) = "{Z_midp,R_midp_in,R_midp_out}"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%R_midp_out)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! MINORITY_HEATING
        w%list_elems(isize) = "MINORITY_HEATING"
        w%elem_descr(isize) = "{pmine,pmini,pminth}"
        w%component(isize) = "IC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_icrf-1)"
        IF(ALLOCATED(ss%pminth)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RADIAL_VELOCITY
        w%list_elems(isize) = "RADIAL_VELOCITY"
        w%elem_descr(isize) = "{vee_trans,vie_trans,vmo_trans}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%vmo_trans)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RADIATED_POWERS
        w%list_elems(isize) = "RADIATED_POWERS"
        w%elem_descr(isize) = "{prad,prad_cy,prad_br,prad_li}"
        w%component(isize) = "RAD"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_rad-1)"
        IF(ALLOCATED(ss%prad_li)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RECO_POWER
        w%list_elems(isize) = "RECO_POWER"
        w%elem_descr(isize) = "{p0_reco,psc_reco,pcx_reco}"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho_gas-1)"
        IF(ALLOCATED(ss%pcx_reco)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RECO_TORQUE
        w%list_elems(isize) = "RECO_TORQUE"
        w%elem_descr(isize) = "{tq0_reco,tqsc_reco,tqcx_reco}"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "Nt*m"
        w%c256val(isize) = "(nrho_gas-1)"
        IF(ALLOCATED(ss%tqcx_reco)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RESIDUAL
        w%list_elems(isize) = "RESIDUAL"
        w%elem_descr(isize) = "{res_te,res_ti}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "Norm"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%res_ti)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! RHO_BDY_SOLVE
        w%list_elems(isize) = "RHO_BDY_SOLVE"
        w%elem_descr(isize) = "{rho_bdy_Te,rho_bdy_Ti,rho_bdy_omegat}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RIPPLE_FIELDS
        w%list_elems(isize) = "RIPPLE_FIELDS"
        w%elem_descr(isize) = "{tfrip_log,tfrip2_log}"
        w%component(isize) = "RIPPLE"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nr_rip,nz_rip)"
        IF(ALLOCATED(ss%tfrip2_log)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RMAJMIN
        w%list_elems(isize) = "RMAJMIN"
        w%elem_descr(isize) = "{Rmajor_mean,rMinor_mean}"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%rMinor_mean)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ROTATION_DISSIPATION
        w%list_elems(isize) = "ROTATION_DISSIPATION"
        w%elem_descr(isize) = "{qrot_conv,qrot_diff}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%qrot_diff)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RZMINMAX
        w%list_elems(isize) = "RZMINMAX"
        w%elem_descr(isize) = "{R_surfMax,R_surfMin,Z_surfMax,Z_surfMin}"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%Z_surfMin)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! SURFACE_VOLTAGE
        w%list_elems(isize) = "SURFACE_VOLTAGE"
        w%elem_descr(isize) = "{vsur}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "Volts"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! SURFSHAPE
        w%list_elems(isize) = "SURFSHAPE"
        w%elem_descr(isize) = "{elong,triang,triangU,triangL,squareUO,squareLO}"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%squareLO)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! TEMPERATURE_BCS
        w%list_elems(isize) = "TEMPERATURE_BCS"
        w%elem_descr(isize) = "{Te_bdy,Ti_bdy}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "KeV"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! TIME_RANGE
        w%list_elems(isize) = "TIME_RANGE"
        w%elem_descr(isize) = "{tinit,tfinal}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "sec"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! TIME_STEP
        w%list_elems(isize) = "TIME_STEP"
        w%elem_descr(isize) = "{t0,t1}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "sec"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! TRANSPORT
        w%list_elems(isize) = "TRANSPORT"
        w%elem_descr(isize) = "{pe_trans,pi_trans}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%pi_trans)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*DV"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! TRIANG_MILLER
        w%list_elems(isize) = "TRIANG_MILLER"
        w%elem_descr(isize) = "{triang_miller_U,triang_miller_L}"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho_eq_geo)"
        IF(ALLOCATED(ss%triang_miller_L)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! UPWIND_FORWARD_FRACTIONS
        w%list_elems(isize) = "UPWIND_FORWARD_FRACTIONS"
        w%elem_descr(isize) = "{upwind_pfrac_Te,upwind_pfrac_Ti,upwind_pfrac_omega}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho)"
        IF(ALLOCATED(ss%upwind_pfrac_omega)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "PCLIN"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ZEFF_GROUP
        w%list_elems(isize) = "ZEFF_GROUP"
        w%elem_descr(isize) = "{Zeff,Zeff_th,Zeff_fi}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nrho-1)"
        IF(ALLOCATED(ss%Zeff_fi)) THEN
           w%ival(isize) = 2
        ELSE
           w%ival(isize) = -2
        ENDIF
        w%val_field(isize) = "STEP*NS(:,PS_ELEC_INDEX)"
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! ZIMP_SA
        w%list_elems(isize) = "ZIMP_SA"
        w%elem_descr(isize) = "{Zimp1,Zimp2}"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        w%ival(isize) = 2
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_contents
