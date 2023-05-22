
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_contents_mdescr(itype,icomponent,ss,w,isize)


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

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Ang_Href_coil
        w%list_elems(isize) = "Ang_Href_coil"
        w%elem_descr(isize) = "angle w.r.t. horizontal of coil base"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "degrees"
        w%c256val(isize) = "(ncoils)"
        IF(ALLOCATED(ss%Ang_Href_coil)) THEN
           IF(MAXVAL(ABS(ss%Ang_Href_coil)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Ang_Vref_coil
        w%list_elems(isize) = "Ang_Vref_coil"
        w%elem_descr(isize) = "angle w.r.t. vertical of coil sides"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "degrees"
        w%c256val(isize) = "(ncoils)"
        IF(ALLOCATED(ss%Ang_Vref_coil)) THEN
           IF(MAXVAL(ABS(ss%Ang_Vref_coil)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ant_model
        w%list_elems(isize) = "ant_model"
        w%elem_descr(isize) = "antenna model filenames (1 per antenna source)"
        w%component(isize) = "IC"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(nicrf_src)"
        IF(ALLOCATED(ss%ant_model)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nicrf_src
              IF(ss%ant_model(I1).NE." ") THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap2_halfheight
        w%list_elems(isize) = "ap2_halfheight"
        w%elem_descr(isize) = "aperture half-height"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%ap2_halfheight)) THEN
           IF(MAXVAL(ABS(ss%ap2_halfheight)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap2_halfwidth
        w%list_elems(isize) = "ap2_halfwidth"
        w%elem_descr(isize) = "2nd aperture half-width"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%ap2_halfwidth)) THEN
           IF(MAXVAL(ABS(ss%ap2_halfwidth)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap2_horiz_offset
        w%list_elems(isize) = "ap2_horiz_offset"
        w%elem_descr(isize) = "horizontal offset, 2nd aperture"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%ap2_horiz_offset)) THEN
           IF(MAXVAL(ABS(ss%ap2_horiz_offset)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap2_vert_offset
        w%list_elems(isize) = "ap2_vert_offset"
        w%elem_descr(isize) = "vertical offset, 2nd aperture"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%ap2_vert_offset)) THEN
           IF(MAXVAL(ABS(ss%ap2_vert_offset)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap_halfheight
        w%list_elems(isize) = "ap_halfheight"
        w%elem_descr(isize) = "aperture half-height"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%ap_halfheight)) THEN
           IF(MAXVAL(ABS(ss%ap_halfheight)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap_halfwidth
        w%list_elems(isize) = "ap_halfwidth"
        w%elem_descr(isize) = "aperture half-width"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%ap_halfwidth)) THEN
           IF(MAXVAL(ABS(ss%ap_halfwidth)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap_horiz_offset
        w%list_elems(isize) = "ap_horiz_offset"
        w%elem_descr(isize) = "horizontal offset of aperture"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%ap_horiz_offset)) THEN
           IF(MAXVAL(ABS(ss%ap_horiz_offset)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap_vert_offset
        w%list_elems(isize) = "ap_vert_offset"
        w%elem_descr(isize) = "vertical offset of aperture"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%ap_vert_offset)) THEN
           IF(MAXVAL(ABS(ss%ap_vert_offset)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! beam_type
        w%list_elems(isize) = "beam_type"
        w%elem_descr(isize) = "beam type:"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%beam_type)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nbeam
              IF(ss%beam_type(I1).NE." ") THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! b_halfHeight
        w%list_elems(isize) = "b_halfHeight"
        w%elem_descr(isize) = "beam half-height at source"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%b_halfHeight)) THEN
           IF(MAXVAL(ABS(ss%b_halfHeight)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! b_halfwidth
        w%list_elems(isize) = "b_halfwidth"
        w%elem_descr(isize) = "beam half-width at source"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%b_halfwidth)) THEN
           IF(MAXVAL(ABS(ss%b_halfwidth)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! b_Hdivergence
        w%list_elems(isize) = "b_Hdivergence"
        w%elem_descr(isize) = "horizontal divergence (D) of beam"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "degrees"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%b_Hdivergence)) THEN
           IF(MAXVAL(ABS(ss%b_Hdivergence)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! b_Hfocal_length
        w%list_elems(isize) = "b_Hfocal_length"
        w%elem_descr(isize) = "horizontal focal length"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%b_Hfocal_length)) THEN
           IF(MAXVAL(ABS(ss%b_Hfocal_length)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! b_Vdivergence
        w%list_elems(isize) = "b_Vdivergence"
        w%elem_descr(isize) = "vertical divergence (D) of beam"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "degrees"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%b_Vdivergence)) THEN
           IF(MAXVAL(ABS(ss%b_Vdivergence)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! b_Vfocal_length
        w%list_elems(isize) = "b_Vfocal_length"
        w%elem_descr(isize) = "vertical focal length"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%b_Vfocal_length)) THEN
           IF(MAXVAL(ABS(ss%b_Vfocal_length)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! coil_in_circuit
        w%list_elems(isize) = "coil_in_circuit"
        w%elem_descr(isize) = "circuit to which each coil belongs"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(ncoils)"
        IF(ALLOCATED(ss%coil_in_circuit)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%ncoils
              IF(ss%coil_in_circuit(I1).NE." ") THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! coil_resispt
        w%list_elems(isize) = "coil_resispt"
        w%elem_descr(isize) = "coil resistance per turn"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "ohms/turn"
        w%c256val(isize) = "(ncoils)"
        IF(ALLOCATED(ss%coil_resispt)) THEN
           IF(MAXVAL(ABS(ss%coil_resispt)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! dx_fshield
        w%list_elems(isize) = "dx_fshield"
        w%elem_descr(isize) = "distance, antenna to Faraday shield"
        w%component(isize) = "IC"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nicrf_src)"
        IF(ALLOCATED(ss%dx_fshield)) THEN
           IF(MAXVAL(ABS(ss%dx_fshield)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! D_einj_standard
        w%list_elems(isize) = "D_einj_standard"
        w%elem_descr(isize) = "D beam table energies"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(n_d_einj_standard)"
        IF(ALLOCATED(ss%D_einj_standard)) THEN
           IF(MAXVAL(ABS(ss%D_einj_standard)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! D_ffull_standard
        w%list_elems(isize) = "D_ffull_standard"
        w%elem_descr(isize) = "D beam full energy fractions"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(n_d_einj_standard)"
        IF(ALLOCATED(ss%D_ffull_standard)) THEN
           IF(MAXVAL(ABS(ss%D_ffull_standard)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! D_fhalf_standard
        w%list_elems(isize) = "D_fhalf_standard"
        w%elem_descr(isize) = "D beam half energy fractions"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(n_d_einj_standard)"
        IF(ALLOCATED(ss%D_fhalf_standard)) THEN
           IF(MAXVAL(ABS(ss%D_fhalf_standard)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EC_Beam_Elongation
        w%list_elems(isize) = "EC_Beam_Elongation"
        w%elem_descr(isize) = "EC beam ellipticity"
        w%component(isize) = "EC"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%EC_Beam_Elongation)) THEN
           IF(MAXVAL(ABS(ss%EC_Beam_Elongation)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EC_Half_Power_Angle
        w%list_elems(isize) = "EC_Half_Power_Angle"
        w%elem_descr(isize) = "Divergence of EC beam"
        w%component(isize) = "EC"
        w%elem_units(isize) = "degrees"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%EC_Half_Power_Angle)) THEN
           IF(MAXVAL(ABS(ss%EC_Half_Power_Angle)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EC_PRINC_CURV_HOR
        w%list_elems(isize) = "EC_PRINC_CURV_HOR"
        w%elem_descr(isize) = "principal curvature in horizontal direction"
        w%component(isize) = "EC"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%EC_PRINC_CURV_HOR)) THEN
           IF(MAXVAL(ABS(ss%EC_PRINC_CURV_HOR)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EC_PRINC_CURV_VERT
        w%list_elems(isize) = "EC_PRINC_CURV_VERT"
        w%elem_descr(isize) = "principal curvature in vertical direction"
        w%component(isize) = "EC"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%EC_PRINC_CURV_VERT)) THEN
           IF(MAXVAL(ABS(ss%EC_PRINC_CURV_VERT)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EC_PRINC_WIDTH_HOR
        w%list_elems(isize) = "EC_PRINC_WIDTH_HOR"
        w%elem_descr(isize) = "principal beam width in horizontal direction"
        w%component(isize) = "EC"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR)) THEN
           IF(MAXVAL(ABS(ss%EC_PRINC_WIDTH_HOR)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EC_PRINC_WIDTH_VERT
        w%list_elems(isize) = "EC_PRINC_WIDTH_VERT"
        w%elem_descr(isize) = "principal beam width in vertical direction"
        w%component(isize) = "EC"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT)) THEN
           IF(MAXVAL(ABS(ss%EC_PRINC_WIDTH_VERT)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Einj_max
        w%list_elems(isize) = "Einj_max"
        w%elem_descr(isize) = "maximum injection energy"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%Einj_max)) THEN
           IF(MAXVAL(ABS(ss%Einj_max)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Einj_min
        w%list_elems(isize) = "Einj_min"
        w%elem_descr(isize) = "minimum injection energy"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%Einj_min)) THEN
           IF(MAXVAL(ABS(ss%Einj_min)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! geometry
        w%list_elems(isize) = "geometry"
        w%elem_descr(isize) = "path to file(s) vac. vessel & coil descr."
        w%component(isize) = "EQ"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%geometry.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Hsize_coil
        w%list_elems(isize) = "Hsize_coil"
        w%elem_descr(isize) = "(Rotatable) Horizontal coil base size"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(ncoils)"
        IF(ALLOCATED(ss%Hsize_coil)) THEN
           IF(MAXVAL(ABS(ss%Hsize_coil)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! H_einj_standard
        w%list_elems(isize) = "H_einj_standard"
        w%elem_descr(isize) = "H beam table energies"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(n_h_einj_standard)"
        IF(ALLOCATED(ss%H_einj_standard)) THEN
           IF(MAXVAL(ABS(ss%H_einj_standard)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! H_ffull_standard
        w%list_elems(isize) = "H_ffull_standard"
        w%elem_descr(isize) = "H beam full energy fractions"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(n_h_einj_standard)"
        IF(ALLOCATED(ss%H_ffull_standard)) THEN
           IF(MAXVAL(ABS(ss%H_ffull_standard)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! H_fhalf_standard
        w%list_elems(isize) = "H_fhalf_standard"
        w%elem_descr(isize) = "H beam half energy fractions"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(n_h_einj_standard)"
        IF(ALLOCATED(ss%H_fhalf_standard)) THEN
           IF(MAXVAL(ABS(ss%H_fhalf_standard)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Lbscap
        w%list_elems(isize) = "Lbscap"
        w%elem_descr(isize) = "distance, source to aperture"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%Lbscap)) THEN
           IF(MAXVAL(ABS(ss%Lbscap)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Lbscap2
        w%list_elems(isize) = "Lbscap2"
        w%elem_descr(isize) = "distance, source to 2nd aperture"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%Lbscap2)) THEN
           IF(MAXVAL(ABS(ss%Lbscap2)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Lbsctan
        w%list_elems(isize) = "Lbsctan"
        w%elem_descr(isize) = "distance, source to tangency point"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%Lbsctan)) THEN
           IF(MAXVAL(ABS(ss%Lbsctan)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! nbap2_shape
        w%list_elems(isize) = "nbap2_shape"
        w%elem_descr(isize) = "keyword for optional 2nd aperture"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%nbap2_shape)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nbeam
              IF(ss%nbap2_shape(I1).NE." ") THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! nbap_shape
        w%list_elems(isize) = "nbap_shape"
        w%elem_descr(isize) = "shape keyword for beam aperture"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%nbap_shape)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nbeam
              IF(ss%nbap_shape(I1).NE." ") THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! nbshape
        w%list_elems(isize) = "nbshape"
        w%elem_descr(isize) = "shape keyword for beam source grid"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%nbshape)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nbeam
              IF(ss%nbshape(I1).NE." ") THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! nrz_antgeo
        w%list_elems(isize) = "nrz_antgeo"
        w%elem_descr(isize) = "number of (R,Z) points, antenna geometries"
        w%component(isize) = "IC"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nicrf_src)"
        IF(ALLOCATED(ss%nrz_antgeo)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nicrf_src
              IF((ss%nrz_antgeo(I1).NE.0).AND.(ss%nrz_antgeo(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ntf_coil2
        w%list_elems(isize) = "ntf_coil2"
        w%elem_descr(isize) = "number of TF coils involved in inducing 2nd component"
        w%component(isize) = "RIPPLE"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%ntf_coil2.EQ.0).OR.(ss%ntf_coil2.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ntf_coils
        w%list_elems(isize) = "ntf_coils"
        w%elem_descr(isize) = "number of TF coils"
        w%component(isize) = "RIPPLE"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%ntf_coils.EQ.0).OR.(ss%ntf_coils.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Nturns
        w%list_elems(isize) = "Nturns"
        w%elem_descr(isize) = "SIGNED number of conductor turns wrapped in each coil"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(ncoils)"
        IF(ALLOCATED(ss%Nturns)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%ncoils
              IF((ss%Nturns(I1).NE.0).AND.(ss%Nturns(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! num_rzlim
        w%list_elems(isize) = "num_rzlim"
        w%elem_descr(isize) = "number of points, axisymmetric (R,Z) limiter"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%num_rzlim.EQ.0).OR.(ss%num_rzlim.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! n_D_einj_standard
        w%list_elems(isize) = "n_D_einj_standard"
        w%elem_descr(isize) = "no. pts in D beam energy fractions table"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%n_D_einj_standard.EQ.0).OR.(ss%n_D_einj_standard.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! n_H_einj_standard
        w%list_elems(isize) = "n_H_einj_standard"
        w%elem_descr(isize) = "no. pts in H beam energy fractions table"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%n_H_einj_standard.EQ.0).OR.(ss%n_H_einj_standard.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! n_T_einj_standard
        w%list_elems(isize) = "n_T_einj_standard"
        w%elem_descr(isize) = "no. pts in T beam energy fractions table"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%n_T_einj_standard.EQ.0).OR.(ss%n_T_einj_standard.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Phibsc
        w%list_elems(isize) = "Phibsc"
        w%elem_descr(isize) = "toroidal location of beam source"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "degrees"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%Phibsc)) THEN
           IF(MAXVAL(ABS(ss%Phibsc)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Phi_EC_launch
        w%list_elems(isize) = "Phi_EC_launch"
        w%elem_descr(isize) = "Phi of center of launcher"
        w%component(isize) = "EC"
        w%elem_units(isize) = "degrees"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%Phi_EC_launch)) THEN
           IF(MAXVAL(ABS(ss%Phi_EC_launch)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! rlim
        w%list_elems(isize) = "rlim"
        w%elem_descr(isize) = "R points in closed (R,Z) contour sequence"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(num_rzlim)"
        IF(ALLOCATED(ss%rlim)) THEN
           IF(MAXVAL(ABS(ss%rlim)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Rloc_coil
        w%list_elems(isize) = "Rloc_coil"
        w%elem_descr(isize) = "R location of coil, lower left corner"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(ncoils)"
        IF(ALLOCATED(ss%Rloc_coil)) THEN
           IF(MAXVAL(ABS(ss%Rloc_coil)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! R_antgeo
        w%list_elems(isize) = "R_antgeo"
        w%elem_descr(isize) = "antenna geo: R pts"
        w%component(isize) = "IC"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(max_nrz_antgeo,nicrf_src)"
        IF(ALLOCATED(ss%R_antgeo)) THEN
           IF(MAXVAL(ABS(ss%R_antgeo)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! R_EC_launch
        w%list_elems(isize) = "R_EC_launch"
        w%elem_descr(isize) = "R of center of launcher"
        w%component(isize) = "EC"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%R_EC_launch)) THEN
           IF(MAXVAL(ABS(ss%R_EC_launch)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! R_max_box
        w%list_elems(isize) = "R_max_box"
        w%elem_descr(isize) = "R_max of bounding box"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "scalar"
        IF(ss%R_max_box.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! R_min_box
        w%list_elems(isize) = "R_min_box"
        w%elem_descr(isize) = "R_min of bounding box"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "scalar"
        IF(ss%R_min_box.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! sRtcen
        w%list_elems(isize) = "sRtcen"
        w%elem_descr(isize) = "signed tangency radius"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%sRtcen)) THEN
           IF(MAXVAL(ABS(ss%sRtcen)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! tokamak_id
        w%list_elems(isize) = "tokamak_id"
        w%elem_descr(isize) = "Tokamak (Machine Description) ID"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "scalar"
        IF(ss%tokamak_id.EQ." ") THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! T_einj_standard
        w%list_elems(isize) = "T_einj_standard"
        w%elem_descr(isize) = "T beam table energies"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "keV"
        w%c256val(isize) = "(n_t_einj_standard)"
        IF(ALLOCATED(ss%T_einj_standard)) THEN
           IF(MAXVAL(ABS(ss%T_einj_standard)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! T_ffull_standard
        w%list_elems(isize) = "T_ffull_standard"
        w%elem_descr(isize) = "T beam full energy fractions"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(n_t_einj_standard)"
        IF(ALLOCATED(ss%T_ffull_standard)) THEN
           IF(MAXVAL(ABS(ss%T_ffull_standard)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! T_fhalf_standard
        w%list_elems(isize) = "T_fhalf_standard"
        w%elem_descr(isize) = "T beam half energy fractions"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(n_t_einj_standard)"
        IF(ALLOCATED(ss%T_fhalf_standard)) THEN
           IF(MAXVAL(ABS(ss%T_fhalf_standard)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! VRep_coil_count
        w%list_elems(isize) = "VRep_coil_count"
        w%elem_descr(isize) = "replication count, vertical coil stack"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(ncoils)"
        IF(ALLOCATED(ss%VRep_coil_count)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%ncoils
              IF((ss%VRep_coil_count(I1).NE.0).AND.(ss%VRep_coil_count(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Vrep_coil_spacing
        w%list_elems(isize) = "Vrep_coil_spacing"
        w%elem_descr(isize) = "replicated coil separation"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(ncoils)"
        IF(ALLOCATED(ss%Vrep_coil_spacing)) THEN
           IF(MAXVAL(ABS(ss%Vrep_coil_spacing)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Vsize_coil
        w%list_elems(isize) = "Vsize_coil"
        w%elem_descr(isize) = "(Rotatable) Vertical coil size"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(ncoils)"
        IF(ALLOCATED(ss%Vsize_coil)) THEN
           IF(MAXVAL(ABS(ss%Vsize_coil)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Zbap
        w%list_elems(isize) = "Zbap"
        w%elem_descr(isize) = "elevation of beam centerline at aperture"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%Zbap)) THEN
           IF(MAXVAL(ABS(ss%Zbap)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Zbsc
        w%list_elems(isize) = "Zbsc"
        w%elem_descr(isize) = "elevation of beam source"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%Zbsc)) THEN
           IF(MAXVAL(ABS(ss%Zbsc)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! zlim
        w%list_elems(isize) = "zlim"
        w%elem_descr(isize) = "Z points in closed (R,Z) contour sequence"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(num_rzlim)"
        IF(ALLOCATED(ss%zlim)) THEN
           IF(MAXVAL(ABS(ss%zlim)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Zloc_coil
        w%list_elems(isize) = "Zloc_coil"
        w%elem_descr(isize) = "Z location of coil, lower left corner"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(ncoils)"
        IF(ALLOCATED(ss%Zloc_coil)) THEN
           IF(MAXVAL(ABS(ss%Zloc_coil)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Z_antgeo
        w%list_elems(isize) = "Z_antgeo"
        w%elem_descr(isize) = "antenna geo: Z pts"
        w%component(isize) = "IC"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(max_nrz_antgeo,nicrf_src)"
        IF(ALLOCATED(ss%Z_antgeo)) THEN
           IF(MAXVAL(ABS(ss%Z_antgeo)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Z_EC_launch
        w%list_elems(isize) = "Z_EC_launch"
        w%elem_descr(isize) = "Z of center of launcher"
        w%component(isize) = "EC"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%Z_EC_launch)) THEN
           IF(MAXVAL(ABS(ss%Z_EC_launch)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Z_max_box
        w%list_elems(isize) = "Z_max_box"
        w%elem_descr(isize) = "Z_max of bounding box"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "scalar"
        IF(ss%Z_max_box.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Z_min_box
        w%list_elems(isize) = "Z_min_box"
        w%elem_descr(isize) = "Z_min of bounding box"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "scalar"
        IF(ss%Z_min_box.EQ.0.0_rspec) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF
END SUBROUTINE plasma_state_contents_mdescr
