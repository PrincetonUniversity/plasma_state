
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_contents_sini(itype,icomponent,ss,w,isize)


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

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! alla_index
        w%list_elems(isize) = "alla_index"
        w%elem_descr(isize) = "species index in original lists"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(0:nspec_alla)"
        IF(ALLOCATED(ss%alla_index)) THEN
           w%ival(isize) = 0
           DO I1 = 0,ss%nspec_alla
              IF((ss%alla_index(I1).NE.0).AND.(ss%alla_index(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! all_index
        w%list_elems(isize) = "all_index"
        w%elem_descr(isize) = "species index in original lists"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(0:nspec_all)"
        IF(ALLOCATED(ss%all_index)) THEN
           w%ival(isize) = 0
           DO I1 = 0,ss%nspec_all
              IF((ss%all_index(I1).NE.0).AND.(ss%all_index(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! ANOM_Code_Info
        w%list_elems(isize) = "ANOM_Code_Info"
        w%elem_descr(isize) = "Information: code implementing ANOM component"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%ANOM_Code_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Balloon_mapper
        w%list_elems(isize) = "Balloon_mapper"
        w%elem_descr(isize) = "mapper code used for each ballooning code"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(nballoon)"
        IF(ALLOCATED(ss%Balloon_mapper)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nballoon
              IF(ss%Balloon_mapper(I1).NE." ") THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! CUR_Data_Info
        w%list_elems(isize) = "CUR_Data_Info"
        w%elem_descr(isize) = "information on source of plasma current data"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%CUR_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! DeltaW_mapper
        w%list_elems(isize) = "DeltaW_mapper"
        w%elem_descr(isize) = "mapper code used for each delta(W) code"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(ndelta_w)"
        IF(ALLOCATED(ss%DeltaW_mapper)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%ndelta_w
              IF(ss%DeltaW_mapper(I1).NE." ") THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! EC_Code_Info
        w%list_elems(isize) = "EC_Code_Info"
        w%elem_descr(isize) = "Information: code implementing EC component"
        w%component(isize) = "EC"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%EC_Code_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! EC_Data_Info
        w%list_elems(isize) = "EC_Data_Info"
        w%elem_descr(isize) = "information on source of ECRF power data"
        w%component(isize) = "EC"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%EC_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! EQ_Code_Info
        w%list_elems(isize) = "EQ_Code_Info"
        w%elem_descr(isize) = "Information: code implementing EQ component"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%EQ_Code_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! EQ_Data_Info
        w%list_elems(isize) = "EQ_Data_Info"
        w%elem_descr(isize) = "Information: source of EQ input data"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%EQ_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! fracmin
        w%list_elems(isize) = "fracmin"
        w%elem_descr(isize) = 'minority density fraction if kdens_rfmin="fraction"'
        w%component(isize) = "IC"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nspec_rfmin)"
        IF(ALLOCATED(ss%fracmin)) THEN
           IF(MAXVAL(ABS(ss%fracmin)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! FUS_Code_Info
        w%list_elems(isize) = "FUS_Code_Info"
        w%elem_descr(isize) = "Information: code implementing FUS component"
        w%component(isize) = "FUS"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%FUS_Code_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! GAS_Code_Info
        w%list_elems(isize) = "GAS_Code_Info"
        w%elem_descr(isize) = "Information: code implementing GAS component"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%GAS_Code_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Global_label
        w%list_elems(isize) = "Global_label"
        w%elem_descr(isize) = "Run Label"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%Global_label.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! icircuit_coil
        w%list_elems(isize) = "icircuit_coil"
        w%elem_descr(isize) = "index of circuit to which each coil belongs"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(ncoils)"
        IF(ALLOCATED(ss%icircuit_coil)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%ncoils
              IF((ss%icircuit_coil(I1).NE.0).AND.(ss%icircuit_coil(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! IC_Code_Info
        w%list_elems(isize) = "IC_Code_Info"
        w%elem_descr(isize) = "Information: code implementing IC component"
        w%component(isize) = "IC"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%IC_Code_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! IC_Data_Info
        w%list_elems(isize) = "IC_Data_Info"
        w%elem_descr(isize) = "information on source of ICRF power data"
        w%component(isize) = "IC"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%IC_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! kdens_rfmin
        w%list_elems(isize) = "kdens_rfmin"
        w%elem_descr(isize) = '= "fraction" => nmini(i) = fracmin(i)*ne'
        w%component(isize) = "IC"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%kdens_rfmin.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! LH_Code_Info
        w%list_elems(isize) = "LH_Code_Info"
        w%elem_descr(isize) = "Information: code implementing LH component"
        w%component(isize) = "LH"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%LH_Code_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! LH_Data_Info
        w%list_elems(isize) = "LH_Data_Info"
        w%elem_descr(isize) = "Information on source of LHRF power data"
        w%component(isize) = "LH"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%LH_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! LMHD_eq_code
        w%list_elems(isize) = "LMHD_eq_code"
        w%elem_descr(isize) = "name of MHD equilibrium refinement code used"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%LMHD_eq_code.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! NBI_Code_Info
        w%list_elems(isize) = "NBI_Code_Info"
        w%elem_descr(isize) = "Information: code implementing NBI component"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%NBI_Code_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! NBI_Data_Info
        w%list_elems(isize) = "NBI_Data_Info"
        w%elem_descr(isize) = "information on source of beam power & voltage data"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%NBI_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Newcomb_mapper
        w%list_elems(isize) = "Newcomb_mapper"
        w%elem_descr(isize) = "mapper code used for each Newcomb code"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(nnewcomb)"
        IF(ALLOCATED(ss%Newcomb_mapper)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nnewcomb
              IF(ss%Newcomb_mapper(I1).NE." ") THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! nky_anomth
        w%list_elems(isize) = "nky_anomth"
        w%elem_descr(isize) = "number of points, ky grid ( anomalous data over ky spectrum)"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%nky_anomth.EQ.0).OR.(ss%nky_anomth.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! nky_mmm
        w%list_elems(isize) = "nky_mmm"
        w%elem_descr(isize) = "number of points, ky grid for MMM model"
        w%component(isize) = "ANOM"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%nky_mmm.EQ.0).OR.(ss%nky_mmm.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! nmom
        w%list_elems(isize) = "nmom"
        w%elem_descr(isize) = "number of Fourier (R,Z) moments"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%nmom.EQ.0).OR.(ss%nmom.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! ns_Data_Info
        w%list_elems(isize) = "ns_Data_Info"
        w%elem_descr(isize) = "information on source of Density data"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%ns_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PEL).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! PEL_Code_Info
        w%list_elems(isize) = "PEL_Code_Info"
        w%elem_descr(isize) = "Information: code implementing PELLET component"
        w%component(isize) = "PEL"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%PEL_Code_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PEL).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! PEL_Data_Info
        w%list_elems(isize) = "PEL_Data_Info"
        w%elem_descr(isize) = "Information: on source of pellet injection"
        w%component(isize) = "PEL"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%PEL_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! PLASMA_Code_Info
        w%list_elems(isize) = "PLASMA_Code_Info"
        w%elem_descr(isize) = "Information: code managing PLASMA component"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%PLASMA_Code_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! RAD_Code_Info
        w%list_elems(isize) = "RAD_Code_Info"
        w%elem_descr(isize) = "Information: code implementing RAD component"
        w%component(isize) = "RAD"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%RAD_Code_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! RAD_Data_Info
        w%list_elems(isize) = "RAD_Data_Info"
        w%elem_descr(isize) = "Information on source of radiated power data"
        w%component(isize) = "RAD"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%RAD_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! rfmin_to_all
        w%list_elems(isize) = "rfmin_to_all"
        w%elem_descr(isize) = 'map rfmin index to "all" list'
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nspec_rfmin)"
        IF(ALLOCATED(ss%rfmin_to_all)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nspec_rfmin
              IF((ss%rfmin_to_all(I1).NE.0).AND.(ss%rfmin_to_all(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! rfmin_to_alla
        w%list_elems(isize) = "rfmin_to_alla"
        w%elem_descr(isize) = 'map rfmin index to "alla" list'
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nspec_rfmin)"
        IF(ALLOCATED(ss%rfmin_to_alla)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nspec_rfmin
              IF((ss%rfmin_to_alla(I1).NE.0).AND.(ss%rfmin_to_alla(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! RUNAWAY_Code_Info
        w%list_elems(isize) = "RUNAWAY_Code_Info"
        w%elem_descr(isize) = "Information: code implementing RUNAWAY component"
        w%component(isize) = "RUNAWAY"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%RUNAWAY_Code_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! RunID
        w%list_elems(isize) = "RunID"
        w%elem_descr(isize) = "Run ID"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%RunID.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! sa_index
        w%list_elems(isize) = "sa_index"
        w%elem_descr(isize) = "species index in original lists"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(0:nspec_all)"
        IF(ALLOCATED(ss%sa_index)) THEN
           w%ival(isize) = 0
           DO I1 = 0,ss%nspec_all
              IF((ss%sa_index(I1).NE.0).AND.(ss%sa_index(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! sc0_to_sgas
        w%list_elems(isize) = "sc0_to_sgas"
        w%elem_descr(isize) = "index map from neutral source to neutral species"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(ngsc0)"
        IF(ALLOCATED(ss%sc0_to_sgas)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%ngsc0
              IF((ss%sc0_to_sgas(I1).NE.0).AND.(ss%sc0_to_sgas(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! sfus_to_all
        w%list_elems(isize) = "sfus_to_all"
        w%elem_descr(isize) = 'map sfus index to "all" list'
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nspec_fusion)"
        IF(ALLOCATED(ss%sfus_to_all)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nspec_fusion
              IF((ss%sfus_to_all(I1).NE.0).AND.(ss%sfus_to_all(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! sfus_to_alla
        w%list_elems(isize) = "sfus_to_alla"
        w%elem_descr(isize) = 'map sfus index to "alla" list'
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nspec_fusion)"
        IF(ALLOCATED(ss%sfus_to_alla)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nspec_fusion
              IF((ss%sfus_to_alla(I1).NE.0).AND.(ss%sfus_to_alla(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! sgas_to_s
        w%list_elems(isize) = "sgas_to_s"
        w%elem_descr(isize) = 'map sgas index to "s" list'
        w%component(isize) = "GAS"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nspec_gas)"
        IF(ALLOCATED(ss%sgas_to_s)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nspec_gas
              IF((ss%sgas_to_s(I1).NE.0).AND.(ss%sgas_to_s(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! simp0_to_s
        w%list_elems(isize) = "simp0_to_s"
        w%elem_descr(isize) = 'map simp0 index to "s" list'
        w%component(isize) = "GAS"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nspec_imp0)"
        IF(ALLOCATED(ss%simp0_to_s)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nspec_imp0
              IF((ss%simp0_to_s(I1).NE.0).AND.(ss%simp0_to_s(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! simpi_to_s
        w%list_elems(isize) = "simpi_to_s"
        w%elem_descr(isize) = 'map simpi index to "s" list'
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nspec_impi)"
        IF(ALLOCATED(ss%simpi_to_s)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nspec_impi
              IF((ss%simpi_to_s(I1).NE.0).AND.(ss%simpi_to_s(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! snbi_to_all
        w%list_elems(isize) = "snbi_to_all"
        w%elem_descr(isize) = 'map snbi index to "all" list'
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nspec_beam)"
        IF(ALLOCATED(ss%snbi_to_all)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nspec_beam
              IF((ss%snbi_to_all(I1).NE.0).AND.(ss%snbi_to_all(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! snbi_to_alla
        w%list_elems(isize) = "snbi_to_alla"
        w%elem_descr(isize) = 'map snbi index to "alla" list'
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nspec_beam)"
        IF(ALLOCATED(ss%snbi_to_alla)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nspec_beam
              IF((ss%snbi_to_alla(I1).NE.0).AND.(ss%snbi_to_alla(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! s_to_sa
        w%list_elems(isize) = "s_to_sa"
        w%elem_descr(isize) = "address in sa species list"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(0:nspec_th)"
        IF(ALLOCATED(ss%s_to_sa)) THEN
           w%ival(isize) = 0
           DO I1 = 0,ss%nspec_th
              IF((ss%s_to_sa(I1).NE.0).AND.(ss%s_to_sa(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! tfinal
        w%list_elems(isize) = "tfinal"
        w%elem_descr(isize) = "stop time of simulation (may be adjustable)"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "sec"
        w%c256val(isize) = "scalar"
        IF(ss%tfinal.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! TF_Data_Info
        w%list_elems(isize) = "TF_Data_Info"
        w%elem_descr(isize) = "information on source of toroidal field data"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%TF_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! tinit
        w%list_elems(isize) = "tinit"
        w%elem_descr(isize) = "start time of simulation"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "sec"
        w%c256val(isize) = "scalar"
        IF(ss%tinit.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Tor_mode_No
        w%list_elems(isize) = "Tor_mode_No"
        w%elem_descr(isize) = "(from input) Toroidal modes monitored"
        w%component(isize) = "LMHD"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(mmodes)"
        IF(ALLOCATED(ss%Tor_mode_No)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%mmodes
              IF((ss%Tor_mode_No(I1).NE.0).AND.(ss%Tor_mode_No(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! trace_flag
        w%list_elems(isize) = "trace_flag"
        w%elem_descr(isize) = "trace element flag"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%trace_flag)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nbeam
              IF((ss%trace_flag(I1).NE.0).AND.(ss%trace_flag(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! trmodels
        w%list_elems(isize) = "trmodels"
        w%elem_descr(isize) = "short description or ID of transport models"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(ntrcat)"
        IF(ALLOCATED(ss%trmodels)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%ntrcat
              IF(ss%trmodels(I1).NE." ") THEN
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
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Ts_Data_Info
        w%list_elems(isize) = "Ts_Data_Info"
        w%elem_descr(isize) = "information on source of Temperature data"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%Ts_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! vpol_Data_Info
        w%list_elems(isize) = "vpol_Data_Info"
        w%elem_descr(isize) = "information on source of poloidal velocity data"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%vpol_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! VSUR_Data_Info
        w%list_elems(isize) = "VSUR_Data_Info"
        w%elem_descr(isize) = "information on source of surface voltage data"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%VSUR_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! vtor_Data_Info
        w%list_elems(isize) = "vtor_Data_Info"
        w%elem_descr(isize) = "information on source of toroidal velocity data"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%vtor_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Z0max
        w%list_elems(isize) = "Z0max"
        w%elem_descr(isize) = "maximum Z of non-impurity species"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%Z0max.EQ.0).OR.(ss%Z0max.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! ZEFF_Data_Info
        w%list_elems(isize) = "ZEFF_Data_Info"
        w%elem_descr(isize) = "information on source of Zeff data"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%ZEFF_Data_Info.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Zimp1
        w%list_elems(isize) = "Zimp1"
        w%elem_descr(isize) = "lower limit on Z of impurity in reduced species list SA;"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%Zimp1.EQ.0).OR.(ss%Zimp1.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Zimp2
        w%list_elems(isize) = "Zimp2"
        w%elem_descr(isize) = "upper limit on Z of impurity in SA species list;"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%Zimp2.EQ.0).OR.(ss%Zimp2.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF
END SUBROUTINE plasma_state_contents_sini
