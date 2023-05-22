
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_contents_sdata(itype,icomponent,ss,w,isize)


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

  !local:

  INTEGER :: id,inum
  INTEGER :: i1,i2,i3,i4

  !----------------

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Balloon_stable
        w%list_elems(isize) = "Balloon_stable"
        w%elem_descr(isize) = "0: stable everywhere; =1: unstable somewhere"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nballoon)"
        IF(ALLOCATED(ss%Balloon_stable)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nballoon
              IF((ss%Balloon_stable(I1).NE.0).AND.(ss%Balloon_stable(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Balloon_status
        w%list_elems(isize) = "Balloon_status"
        w%elem_descr(isize) = "completion status: Ballooning codes"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nballoon)"
        IF(ALLOCATED(ss%Balloon_status)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nballoon
              IF((ss%Balloon_status(I1).NE.0).AND.(ss%Balloon_status(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! B_axis
        w%list_elems(isize) = "B_axis"
        w%elem_descr(isize) = "|B| at magnetic axis"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T"
        w%c256val(isize) = "scalar"
        IF(ss%B_axis.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! B_axis_vac
        w%list_elems(isize) = "B_axis_vac"
        w%elem_descr(isize) = "vacuum B field at axis"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T"
        w%c256val(isize) = "scalar"
        IF(ss%B_axis_vac.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! B_max_lcfs
        w%list_elems(isize) = "B_max_lcfs"
        w%elem_descr(isize) = "|B|_max of last closed flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T"
        w%c256val(isize) = "scalar"
        IF(ss%B_max_lcfs.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! B_min_lcfs
        w%list_elems(isize) = "B_min_lcfs"
        w%elem_descr(isize) = "|B|_min of last closed flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "T"
        w%c256val(isize) = "scalar"
        IF(ss%B_min_lcfs.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! coil_apt
        w%list_elems(isize) = "coil_apt"
        w%elem_descr(isize) = "SIGNED circuit current"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "amps/turn"
        w%c256val(isize) = "(ncircuits)"
        IF(ALLOCATED(ss%coil_apt)) THEN
           IF(MAXVAL(ABS(ss%coil_apt)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! DeltaW_stable
        w%list_elems(isize) = "DeltaW_stable"
        w%elem_descr(isize) = "0: stable; 1: unstable;"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(mmodes,ndelta_w)"
        IF(ALLOCATED(ss%DeltaW_stable)) THEN
           w%ival(isize) = 0
           DO I2 = 1,ss%ndelta_w
              DO I1 = 1,ss%mmodes
                 IF((ss%DeltaW_stable(I1,I2).NE.0).AND.(ss%DeltaW_stable(I1,I2).NE.ps_unInit)) THEN
                    w%ival(isize) = 1
                    exit
                 ENDIF
              ENDDO   ! I1
           ENDDO   ! I2
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! DeltaW_status
        w%list_elems(isize) = "DeltaW_status"
        w%elem_descr(isize) = "completion status: Delta(W) codes"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(ndelta_w)"
        IF(ALLOCATED(ss%DeltaW_status)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%ndelta_w
              IF((ss%DeltaW_status(I1).NE.0).AND.(ss%DeltaW_status(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! dist_fun
        w%list_elems(isize) = "dist_fun"
        w%elem_descr(isize) = "distribution function filenames"
        w%component(isize) = "IC"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(0:nspec_alla)"
        IF(ALLOCATED(ss%dist_fun)) THEN
           w%ival(isize) = 0
           DO I1 = 0,ss%nspec_alla
              IF(ss%dist_fun(I1).NE." ") THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! dn0out
        w%list_elems(isize) = "dn0out"
        w%elem_descr(isize) = "Hydrogenic neutral density in scrape off region"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "scalar"
        IF(ss%dn0out.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! e0_av
        w%list_elems(isize) = "e0_av"
        w%elem_descr(isize) = 'average energy of neutral sources, "(3/2)*T0"'
        w%component(isize) = "GAS"
        w%elem_units(isize) = "KeV"
        w%c256val(isize) = "(ngsc0)"
        IF(ALLOCATED(ss%e0_av)) THEN
           IF(MAXVAL(ABS(ss%e0_av)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! eldot
        w%list_elems(isize) = "eldot"
        w%elem_descr(isize) = "[d(Philim)/dt]/[2*Philim]"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "1/sec"
        w%c256val(isize) = "scalar"
        IF(ss%eldot.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! eqdsk_file
        w%list_elems(isize) = "eqdsk_file"
        w%elem_descr(isize) = "EFIT G-eqdsk file: psi(R,Z), g(psi), etc."
        w%component(isize) = "EQ"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%eqdsk_file.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! frac_full
        w%list_elems(isize) = "frac_full"
        w%elem_descr(isize) = "fraction of beam current at full voltage"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%frac_full)) THEN
           IF(MAXVAL(ABS(ss%frac_full)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! frac_half
        w%list_elems(isize) = "frac_half"
        w%elem_descr(isize) = "fraction of beam current at half voltage"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%frac_half)) THEN
           IF(MAXVAL(ABS(ss%frac_half)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! gamma_lmhd
        w%list_elems(isize) = "gamma_lmhd"
        w%elem_descr(isize) = "ideal growth rate"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(mmodes,ndelta_w)"
        IF(ALLOCATED(ss%gamma_lmhd)) THEN
           IF(MAXVAL(ABS(ss%gamma_lmhd)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! interchange_R
        w%list_elems(isize) = "interchange_R"
        w%elem_descr(isize) = "norm rho of the location of the interchange mode from TGLF model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "scalar"
        IF(ss%interchange_R.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! isThermal
        w%list_elems(isize) = "isThermal"
        w%elem_descr(isize) = "=1 if minority specie is thermalized"
        w%component(isize) = "IC"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nspec_rfmin)"
        IF(ALLOCATED(ss%isThermal)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nspec_rfmin
              IF((ss%isThermal(I1).NE.0).AND.(ss%isThermal(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! kvolt_nbi
        w%list_elems(isize) = "kvolt_nbi"
        w%elem_descr(isize) = "energy of each beam source **keV**"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%kvolt_nbi)) THEN
           IF(MAXVAL(ABS(ss%kvolt_nbi)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! LMHD_status
        w%list_elems(isize) = "LMHD_status"
        w%elem_descr(isize) = "0: OK; =1: at least one code error occurred."
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%LMHD_status.EQ.0).OR.(ss%LMHD_status.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! lock_machine_descr
        w%list_elems(isize) = "lock_machine_descr"
        w%elem_descr(isize) = "Machine_Description section lock"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%lock_machine_descr.EQ.0).OR.(ss%lock_machine_descr.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! lock_shot_config
        w%list_elems(isize) = "lock_shot_config"
        w%elem_descr(isize) = "Shot_Configuration section lock"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%lock_shot_config.EQ.0).OR.(ss%lock_shot_config.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! lock_sim_init
        w%list_elems(isize) = "lock_sim_init"
        w%elem_descr(isize) = "Simulation_Init section lock"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%lock_sim_init.EQ.0).OR.(ss%lock_sim_init.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Mercier_stable
        w%list_elems(isize) = "Mercier_stable"
        w%elem_descr(isize) = "0: stable everywhere; =1: unstable somewhere"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nballoon)"
        IF(ALLOCATED(ss%Mercier_stable)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nballoon
              IF((ss%Mercier_stable(I1).NE.0).AND.(ss%Mercier_stable(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! MHD_eq_status
        w%list_elems(isize) = "MHD_eq_status"
        w%elem_descr(isize) = "0: OK; =1: MHD equilibrium refinement failed"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%MHD_eq_status.EQ.0).OR.(ss%MHD_eq_status.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! nbeami_bdy
        w%list_elems(isize) = "nbeami_bdy"
        w%elem_descr(isize) = "beam ion densities at/beyond boundary"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(nspec_beam)"
        IF(ALLOCATED(ss%nbeami_bdy)) THEN
           IF(MAXVAL(ABS(ss%nbeami_bdy)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! nebar
        w%list_elems(isize) = "nebar"
        w%elem_descr(isize) = "line-averaged electron density"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "#/m^2"
        w%c256val(isize) = "scalar"
        IF(ss%nebar.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Newcomb_stable
        w%list_elems(isize) = "Newcomb_stable"
        w%elem_descr(isize) = "0: stable; 1: unstable;"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(mmodes,nnewcomb)"
        IF(ALLOCATED(ss%Newcomb_stable)) THEN
           w%ival(isize) = 0
           DO I2 = 1,ss%nnewcomb
              DO I1 = 1,ss%mmodes
                 IF((ss%Newcomb_stable(I1,I2).NE.0).AND.(ss%Newcomb_stable(I1,I2).NE.ps_unInit)) THEN
                    w%ival(isize) = 1
                    exit
                 ENDIF
              ENDDO   ! I1
           ENDDO   ! I2
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Newcomb_status
        w%list_elems(isize) = "Newcomb_status"
        w%elem_descr(isize) = "completion status: Newcomb codes"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nnewcomb)"
        IF(ALLOCATED(ss%Newcomb_status)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nnewcomb
              IF((ss%Newcomb_status(I1).NE.0).AND.(ss%Newcomb_status(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! nfusi_bdy
        w%list_elems(isize) = "nfusi_bdy"
        w%elem_descr(isize) = "fusion ion densities at/beyond boundary"
        w%component(isize) = "IC"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(nspec_fusion)"
        IF(ALLOCATED(ss%nfusi_bdy)) THEN
           IF(MAXVAL(ABS(ss%nfusi_bdy)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! nmdifb
        w%list_elems(isize) = "nmdifb"
        w%elem_descr(isize) = "type of fast ion diffusivity"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%nmdifb.EQ.0).OR.(ss%nmdifb.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! nmini_bdy
        w%list_elems(isize) = "nmini_bdy"
        w%elem_descr(isize) = "RF minority ion densities at/beyond boundary"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(nspec_rfmin)"
        IF(ALLOCATED(ss%nmini_bdy)) THEN
           IF(MAXVAL(ABS(ss%nmini_bdy)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! nmodel_bdy
        w%list_elems(isize) = "nmodel_bdy"
        w%elem_descr(isize) = "model density, boundary value"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "scalar"
        IF(ss%nmodel_bdy.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! ns_bdy
        w%list_elems(isize) = "ns_bdy"
        w%elem_descr(isize) = "specie densities at/beyond boundary"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "m^-3"
        w%c256val(isize) = "(0:nspec_th)"
        IF(ALLOCATED(ss%ns_bdy)) THEN
           IF(MAXVAL(ABS(ss%ns_bdy)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! ns_is_input
        w%list_elems(isize) = "ns_is_input"
        w%elem_descr(isize) = "specie density measurement flag"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(0:nspec_th)"
        IF(ALLOCATED(ss%ns_is_input)) THEN
           w%ival(isize) = 0
           DO I1 = 0,ss%nspec_th
              IF((ss%ns_is_input(I1).NE.0).AND.(ss%ns_is_input(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! omegat_bdy
        w%list_elems(isize) = "omegat_bdy"
        w%elem_descr(isize) = "toroidal angular velocity at/beyond boundary"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "rad/sec"
        w%c256val(isize) = "scalar"
        IF(ss%omegat_bdy.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! picrf_abs
        w%list_elems(isize) = "picrf_abs"
        w%elem_descr(isize) = "RF power absorbed inside plasma"
        w%component(isize) = "IC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nicrf_src)"
        IF(ALLOCATED(ss%picrf_abs)) THEN
           IF(MAXVAL(ABS(ss%picrf_abs)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! picrf_ext
        w%list_elems(isize) = "picrf_ext"
        w%elem_descr(isize) = "RF power deposited outside plasma"
        w%component(isize) = "IC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nicrf_src)"
        IF(ALLOCATED(ss%picrf_ext)) THEN
           IF(MAXVAL(ABS(ss%picrf_ext)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! power_ec
        w%list_elems(isize) = "power_ec"
        w%elem_descr(isize) = "power on each ECRF source"
        w%component(isize) = "EC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%power_ec)) THEN
           IF(MAXVAL(ABS(ss%power_ec)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! power_ic
        w%list_elems(isize) = "power_ic"
        w%elem_descr(isize) = "power on each ICRF source"
        w%component(isize) = "IC"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nicrf_src)"
        IF(ALLOCATED(ss%power_ic)) THEN
           IF(MAXVAL(ABS(ss%power_ic)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! power_lh
        w%list_elems(isize) = "power_lh"
        w%elem_descr(isize) = "power on each LHRF source"
        w%component(isize) = "LH"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nlhrf_src)"
        IF(ALLOCATED(ss%power_lh)) THEN
           IF(MAXVAL(ABS(ss%power_lh)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! power_nbi
        w%list_elems(isize) = "power_nbi"
        w%elem_descr(isize) = "power on each beam source"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%power_nbi)) THEN
           IF(MAXVAL(ABS(ss%power_nbi)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! power_nbi_trace
        w%list_elems(isize) = "power_nbi_trace"
        w%elem_descr(isize) = "power injected as trace element (if any)"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "W"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%power_nbi_trace)) THEN
           IF(MAXVAL(ABS(ss%power_nbi_trace)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Psi_to_machine_axis
        w%list_elems(isize) = "Psi_to_machine_axis"
        w%elem_descr(isize) = "delta(Psi) from magnetic axis to machine axis"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "Wb/rad"
        w%c256val(isize) = "scalar"
        IF(ss%Psi_to_machine_axis.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! ql_operator
        w%list_elems(isize) = "ql_operator"
        w%elem_descr(isize) = "quasilinear operator filenames"
        w%component(isize) = "IC"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(0:nspec_alla)"
        IF(ALLOCATED(ss%ql_operator)) THEN
           w%ival(isize) = 0
           DO I1 = 0,ss%nspec_alla
              IF(ss%ql_operator(I1).NE." ") THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! rho_bdy_ns
        w%list_elems(isize) = "rho_bdy_ns"
        w%elem_descr(isize) = "locations beyond which densities are known"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(0:nspec_th)"
        IF(ALLOCATED(ss%rho_bdy_ns)) THEN
           IF(MAXVAL(ABS(ss%rho_bdy_ns)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! rho_bdy_omegat
        w%list_elems(isize) = "rho_bdy_omegat"
        w%elem_descr(isize) = "location beyond which angular velocity is known"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "scalar"
        IF(ss%rho_bdy_omegat.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! rho_bdy_Te
        w%list_elems(isize) = "rho_bdy_Te"
        w%elem_descr(isize) = "location beyond which Te is known"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "scalar"
        IF(ss%rho_bdy_Te.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! rho_bdy_Ti
        w%list_elems(isize) = "rho_bdy_Ti"
        w%elem_descr(isize) = "location beyond which Ti is known"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "scalar"
        IF(ss%rho_bdy_Ti.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! R_axis
        w%list_elems(isize) = "R_axis"
        w%elem_descr(isize) = "R of magnetic axis"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "scalar"
        IF(ss%R_axis.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! R_max_lcfs
        w%list_elems(isize) = "R_max_lcfs"
        w%elem_descr(isize) = "R_max of last closed flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "scalar"
        IF(ss%R_max_lcfs.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! R_min_lcfs
        w%list_elems(isize) = "R_min_lcfs"
        w%elem_descr(isize) = "R_min of last closed flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "scalar"
        IF(ss%R_min_lcfs.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! sc0
        w%list_elems(isize) = "sc0"
        w%elem_descr(isize) = "edge neutral sources (atoms/sec)"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(ngsc0)"
        IF(ALLOCATED(ss%sc0)) THEN
           IF(MAXVAL(ABS(ss%sc0)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! t0
        w%list_elems(isize) = "t0"
        w%elem_descr(isize) = "time at beginning of IPS macro timestep"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "sec"
        w%c256val(isize) = "scalar"
        IF(ss%t0.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! t1
        w%list_elems(isize) = "t1"
        w%elem_descr(isize) = "time at end of IPS macro timestep"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "sec"
        w%c256val(isize) = "scalar"
        IF(ss%t1.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Te_bdy
        w%list_elems(isize) = "Te_bdy"
        w%elem_descr(isize) = "Electron temperature at/beyond boundary"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "KeV"
        w%c256val(isize) = "scalar"
        IF(ss%Te_bdy.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Ti_bdy
        w%list_elems(isize) = "Ti_bdy"
        w%elem_descr(isize) = "Ion temperature at/beyond boundary"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "KeV"
        w%c256val(isize) = "scalar"
        IF(ss%Ti_bdy.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! tmodel_bdy
        w%list_elems(isize) = "tmodel_bdy"
        w%elem_descr(isize) = "model temperature, boundary value"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "scalar"
        IF(ss%tmodel_bdy.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Tornum_balcrit
        w%list_elems(isize) = "Tornum_balcrit"
        w%elem_descr(isize) = "critical toroidal mode number for ballooning"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nballoon)"
        IF(ALLOCATED(ss%Tornum_balcrit)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nballoon
              IF((ss%Tornum_balcrit(I1).NE.0).AND.(ss%Tornum_balcrit(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Ts_is_input
        w%list_elems(isize) = "Ts_is_input"
        w%elem_descr(isize) = "specie temperature measurement flag"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(0:nspec_th)"
        IF(ALLOCATED(ss%Ts_is_input)) THEN
           w%ival(isize) = 0
           DO I1 = 0,ss%nspec_th
              IF((ss%Ts_is_input(I1).NE.0).AND.(ss%Ts_is_input(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! vphi0_av
        w%list_elems(isize) = "vphi0_av"
        w%elem_descr(isize) = "average toroidal velocity of neutral source"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "m/sec"
        w%c256val(isize) = "(ngsc0)"
        IF(ALLOCATED(ss%vphi0_av)) THEN
           IF(MAXVAL(ABS(ss%vphi0_av)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! vpol_is_input
        w%list_elems(isize) = "vpol_is_input"
        w%elem_descr(isize) = "specie outer midplane poloidal velocit flag"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(0:nspec_th)"
        IF(ALLOCATED(ss%vpol_is_input)) THEN
           w%ival(isize) = 0
           DO I1 = 0,ss%nspec_th
              IF((ss%vpol_is_input(I1).NE.0).AND.(ss%vpol_is_input(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! vsur
        w%list_elems(isize) = "vsur"
        w%elem_descr(isize) = "toroidal voltage at surface"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "Volts"
        w%c256val(isize) = "scalar"
        IF(ss%vsur.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! vtor_is_input
        w%list_elems(isize) = "vtor_is_input"
        w%elem_descr(isize) = "specie outer midplane toroidal velocit flag"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(0:nspec_th)"
        IF(ALLOCATED(ss%vtor_is_input)) THEN
           w%ival(isize) = 0
           DO I1 = 0,ss%nspec_th
              IF((ss%vtor_is_input(I1).NE.0).AND.(ss%vtor_is_input(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! wt_nphi_abs
        w%list_elems(isize) = "wt_nphi_abs"
        w%elem_descr(isize) = "absorbed spectrum n_phi weight"
        w%component(isize) = "IC"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(max_num_nphi,nicrf_src)"
        IF(ALLOCATED(ss%wt_nphi_abs)) THEN
           IF(MAXVAL(ABS(ss%wt_nphi_abs)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! wt_nphi_ext
        w%list_elems(isize) = "wt_nphi_ext"
        w%elem_descr(isize) = "fraction of power deposited outside"
        w%component(isize) = "IC"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(max_num_nphi,nicrf_src)"
        IF(ALLOCATED(ss%wt_nphi_ext)) THEN
           IF(MAXVAL(ABS(ss%wt_nphi_ext)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! xs
        w%list_elems(isize) = "xs"
        w%elem_descr(isize) = "impurity source magnitude (atoms/sec)"
        w%component(isize) = "RAD"
        w%elem_units(isize) = "#/sec"
        w%c256val(isize) = "(nxs)"
        IF(ALLOCATED(ss%xs)) THEN
           IF(MAXVAL(ABS(ss%xs)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Z_axis
        w%list_elems(isize) = "Z_axis"
        w%elem_descr(isize) = "Z of magnetic axis"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "scalar"
        IF(ss%Z_axis.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Z_max_lcfs
        w%list_elems(isize) = "Z_max_lcfs"
        w%elem_descr(isize) = "Z_max of last closed flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "scalar"
        IF(ss%Z_max_lcfs.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Z_min_lcfs
        w%list_elems(isize) = "Z_min_lcfs"
        w%elem_descr(isize) = "Z_min of last closed flux surface"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "scalar"
        IF(ss%Z_min_lcfs.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF
END SUBROUTINE plasma_state_contents_sdata
