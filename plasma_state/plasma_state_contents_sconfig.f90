
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_contents_sconfig(itype,icomponent,ss,w,isize)


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

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! amaxlh
        w%list_elems(isize) = "amaxlh"
        w%elem_descr(isize) = "width of the spectrum peak"
        w%component(isize) = "LH"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(max_nspectrumlh_src,nlhrf_src)"
        IF(ALLOCATED(ss%amaxlh)) THEN
           IF(MAXVAL(ABS(ss%amaxlh)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! aminlh
        w%list_elems(isize) = "aminlh"
        w%elem_descr(isize) = "parallel mode number, center of the spectrum peak"
        w%component(isize) = "LH"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(max_nspectrumlh_src,nlhrf_src)"
        IF(ALLOCATED(ss%aminlh)) THEN
           IF(MAXVAL(ABS(ss%aminlh)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! apolmaxlh
        w%list_elems(isize) = "apolmaxlh"
        w%elem_descr(isize) = "poloidal mode number right of lumps"
        w%component(isize) = "LH"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(max_nspectrumlh_src,nlhrf_src)"
        IF(ALLOCATED(ss%apolmaxlh)) THEN
           IF(MAXVAL(ABS(ss%apolmaxlh)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! apolminlh
        w%list_elems(isize) = "apolminlh"
        w%elem_descr(isize) = "poloidal mode number left of lumps"
        w%component(isize) = "LH"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(max_nspectrumlh_src,nlhrf_src)"
        IF(ALLOCATED(ss%apolminlh)) THEN
           IF(MAXVAL(ABS(ss%apolminlh)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! atormaxlh
        w%list_elems(isize) = "atormaxlh"
        w%elem_descr(isize) = "toroidal mode number right of lumps"
        w%component(isize) = "LH"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(max_nspectrumlh_src,nlhrf_src)"
        IF(ALLOCATED(ss%atormaxlh)) THEN
           IF(MAXVAL(ABS(ss%atormaxlh)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! atorminlh
        w%list_elems(isize) = "atorminlh"
        w%elem_descr(isize) = "toroidal mode number left of lumps"
        w%component(isize) = "LH"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(max_nspectrumlh_src,nlhrf_src)"
        IF(ALLOCATED(ss%atorminlh)) THEN
           IF(MAXVAL(ABS(ss%atorminlh)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! EC_Omode_Fraction
        w%list_elems(isize) = "EC_Omode_Fraction"
        w%elem_descr(isize) = "O-mode fraction of EC source"
        w%component(isize) = "EC"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%EC_Omode_Fraction)) THEN
           IF(MAXVAL(ABS(ss%EC_Omode_Fraction)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! EC_phi_aim
        w%list_elems(isize) = "EC_phi_aim"
        w%elem_descr(isize) = "toroidal aiming angle"
        w%component(isize) = "EC"
        w%elem_units(isize) = "degrees"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%EC_phi_aim)) THEN
           IF(MAXVAL(ABS(ss%EC_phi_aim)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! EC_theta_aim
        w%list_elems(isize) = "EC_theta_aim"
        w%elem_descr(isize) = "poloidal aiming angle"
        w%component(isize) = "EC"
        w%elem_units(isize) = "degrees"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%EC_theta_aim)) THEN
           IF(MAXVAL(ABS(ss%EC_theta_aim)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! fracpslh
        w%list_elems(isize) = "fracpslh"
        w%elem_descr(isize) = "fraction of power on each peak (the sum over all peaks must be 1)"
        w%component(isize) = "LH"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(max_nspectrumlh_src,nlhrf_src)"
        IF(ALLOCATED(ss%fracpslh)) THEN
           IF(MAXVAL(ABS(ss%fracpslh)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! freq_ec
        w%list_elems(isize) = "freq_ec"
        w%elem_descr(isize) = "frequency of EC source"
        w%component(isize) = "EC"
        w%elem_units(isize) = "Hz"
        w%c256val(isize) = "(necrf_src)"
        IF(ALLOCATED(ss%freq_ec)) THEN
           IF(MAXVAL(ABS(ss%freq_ec)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! freq_ic
        w%list_elems(isize) = "freq_ic"
        w%elem_descr(isize) = "frequency on each ICRF source"
        w%component(isize) = "IC"
        w%elem_units(isize) = "Hz"
        w%c256val(isize) = "(nicrf_src)"
        IF(ALLOCATED(ss%freq_ic)) THEN
           IF(MAXVAL(ABS(ss%freq_ic)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! freq_lh
        w%list_elems(isize) = "freq_lh"
        w%elem_descr(isize) = "frequency on each LHRF source"
        w%component(isize) = "LH"
        w%elem_units(isize) = "Hz"
        w%c256val(isize) = "(nlhrf_src)"
        IF(ALLOCATED(ss%freq_lh)) THEN
           IF(MAXVAL(ABS(ss%freq_lh)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! gas_atom
        w%list_elems(isize) = "gas_atom"
        w%elem_descr(isize) = "name of species introduced by each gas source"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(ngsc0)"
        IF(ALLOCATED(ss%gas_atom)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%ngsc0
              IF(ss%gas_atom(I1).NE." ") THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! hegrilh
        w%list_elems(isize) = "hegrilh"
        w%elem_descr(isize) = "need description"
        w%component(isize) = "LH"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nlhrf_src)"
        IF(ALLOCATED(ss%hegrilh)) THEN
           IF(MAXVAL(ABS(ss%hegrilh)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! Imag_ant_coef
        w%list_elems(isize) = "Imag_ant_coef"
        w%elem_descr(isize) = "imag part of Fourier Coef"
        w%component(isize) = "IC"
        w%elem_units(isize) = "Amps/m"
        w%c256val(isize) = "(max_num_nphi_vac,nicrf_src)"
        IF(ALLOCATED(ss%Imag_ant_coef)) THEN
           IF(MAXVAL(ABS(ss%Imag_ant_coef)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! is_recycling
        w%list_elems(isize) = "is_recycling"
        w%elem_descr(isize) = "0/1; =1 to for recycling sources"
        w%component(isize) = "GAS"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(ngsc0)"
        IF(ALLOCATED(ss%is_recycling)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%ngsc0
              IF((ss%is_recycling(I1).NE.0).AND.(ss%is_recycling(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! i_npoloidal
        w%list_elems(isize) = "i_npoloidal"
        w%elem_descr(isize) = "need description"
        w%component(isize) = "LH"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%i_npoloidal.EQ.0).OR.(ss%i_npoloidal.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! kccw_Bphi
        w%list_elems(isize) = "kccw_Bphi"
        w%elem_descr(isize) = "B_phi orientation: +1 means CCW viewed from above"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%kccw_Bphi.EQ.0).OR.(ss%kccw_Bphi.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! kccw_Jphi
        w%list_elems(isize) = "kccw_Jphi"
        w%elem_descr(isize) = "J_phi orientation: +1 means CCW viewed from above"
        w%component(isize) = "EQ"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%kccw_Jphi.EQ.0).OR.(ss%kccw_Jphi.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nbion
        w%list_elems(isize) = "nbion"
        w%elem_descr(isize) = "name of species injected by each beam."
        w%component(isize) = "NBI"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%nbion)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nbeam
              IF(ss%nbion(I1).NE." ") THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nbion_trace
        w%list_elems(isize) = "nbion_trace"
        w%elem_descr(isize) = "(if non-blank) beam trace element"
        w%component(isize) = "NBI"
        w%elem_units(isize) = "character"
        w%c256val(isize) = "(nbeam)"
        IF(ALLOCATED(ss%nbion_trace)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nbeam
              IF(ss%nbion_trace(I1).NE." ") THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nnkparlh
        w%list_elems(isize) = "nnkparlh"
        w%elem_descr(isize) = "need description"
        w%component(isize) = "LH"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(max_nspectrumlh_src,nlhrf_src)"
        IF(ALLOCATED(ss%nnkparlh)) THEN
           w%ival(isize) = 0
           DO I2 = 1,ss%nlhrf_src
              DO I1 = 1,ss%max_nspectrumlh_src
                 IF((ss%nnkparlh(I1,I2).NE.0).AND.(ss%nnkparlh(I1,I2).NE.ps_unInit)) THEN
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

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nnkpollh
        w%list_elems(isize) = "nnkpollh"
        w%elem_descr(isize) = "need description"
        w%component(isize) = "LH"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(max_nspectrumlh_src,nlhrf_src)"
        IF(ALLOCATED(ss%nnkpollh)) THEN
           w%ival(isize) = 0
           DO I2 = 1,ss%nlhrf_src
              DO I1 = 1,ss%max_nspectrumlh_src
                 IF((ss%nnkpollh(I1,I2).NE.0).AND.(ss%nnkpollh(I1,I2).NE.ps_unInit)) THEN
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

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nnktorlh
        w%list_elems(isize) = "nnktorlh"
        w%elem_descr(isize) = "need description"
        w%component(isize) = "LH"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(max_nspectrumlh_src,nlhrf_src)"
        IF(ALLOCATED(ss%nnktorlh)) THEN
           w%ival(isize) = 0
           DO I2 = 1,ss%nlhrf_src
              DO I1 = 1,ss%max_nspectrumlh_src
                 IF((ss%nnktorlh(I1,I2).NE.0).AND.(ss%nnktorlh(I1,I2).NE.ps_unInit)) THEN
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

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nphi
        w%list_elems(isize) = "nphi"
        w%elem_descr(isize) = "n_phi wave spectrum from antenna"
        w%component(isize) = "IC"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(max_num_nphi,nicrf_src)"
        IF(ALLOCATED(ss%nphi)) THEN
           w%ival(isize) = 0
           DO I2 = 1,ss%nicrf_src
              DO I1 = 1,ss%max_num_nphi
                 IF((ss%nphi(I1,I2).NE.0).AND.(ss%nphi(I1,I2).NE.ps_unInit)) THEN
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

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nspectrumlh_src
        w%list_elems(isize) = "nspectrumlh_src"
        w%elem_descr(isize) = "number of spectrum peaks"
        w%component(isize) = "LH"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nlhrf_src)"
        IF(ALLOCATED(ss%nspectrumlh_src)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nlhrf_src
              IF((ss%nspectrumlh_src(I1).NE.0).AND.(ss%nspectrumlh_src(I1).NE.ps_unInit)) THEN
                 w%ival(isize) = 1
                 exit
              ENDIF
           ENDDO   ! I1
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nthinlh
        w%list_elems(isize) = "nthinlh"
        w%elem_descr(isize) = "need description"
        w%component(isize) = "LH"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nlhrf_src)"
        IF(ALLOCATED(ss%nthinlh)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nlhrf_src
              IF((ss%nthinlh(I1).NE.0).AND.(ss%nthinlh(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! num_nphi
        w%list_elems(isize) = "num_nphi"
        w%elem_descr(isize) = "number of non-zero n_phi values"
        w%component(isize) = "IC"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nicrf_src)"
        IF(ALLOCATED(ss%num_nphi)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nicrf_src
              IF((ss%num_nphi(I1).NE.0).AND.(ss%num_nphi(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! num_nphi_vac
        w%list_elems(isize) = "num_nphi_vac"
        w%elem_descr(isize) = "number of non-zero n_phi values"
        w%component(isize) = "IC"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nicrf_src)"
        IF(ALLOCATED(ss%num_nphi_vac)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nicrf_src
              IF((ss%num_nphi_vac(I1).NE.0).AND.(ss%num_nphi_vac(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! n_straps
        w%list_elems(isize) = "n_straps"
        w%elem_descr(isize) = "number of straps in the antenna"
        w%component(isize) = "IC"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "(nicrf_src)"
        IF(ALLOCATED(ss%n_straps)) THEN
           w%ival(isize) = 0
           DO I1 = 1,ss%nicrf_src
              IF((ss%n_straps(I1).NE.0).AND.(ss%n_straps(I1).NE.ps_unInit)) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! Real_ant_coef
        w%list_elems(isize) = "Real_ant_coef"
        w%elem_descr(isize) = "real part of Fourier Coef"
        w%component(isize) = "IC"
        w%elem_units(isize) = "Amps/m"
        w%c256val(isize) = "(max_num_nphi_vac,nicrf_src)"
        IF(ALLOCATED(ss%Real_ant_coef)) THEN
           IF(MAXVAL(ABS(ss%Real_ant_coef)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! rhopsi0lh
        w%list_elems(isize) = "rhopsi0lh"
        w%elem_descr(isize) = "need description"
        w%component(isize) = "LH"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nlhrf_src)"
        IF(ALLOCATED(ss%rhopsi0lh)) THEN
           IF(MAXVAL(ABS(ss%rhopsi0lh)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! R_ant
        w%list_elems(isize) = "R_ant"
        w%elem_descr(isize) = "major radius of antenna"
        w%component(isize) = "IC"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nicrf_src)"
        IF(ALLOCATED(ss%R_ant)) THEN
           IF(MAXVAL(ABS(ss%R_ant)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! shot_number
        w%list_elems(isize) = "shot_number"
        w%elem_descr(isize) = "integer shot number"
        w%component(isize) = "PLASMA"
        w%elem_units(isize) = "integer"
        w%c256val(isize) = "scalar"
        IF((ss%shot_number.EQ.0).OR.(ss%shot_number.EQ.ps_unInit)) THEN
           w%ival(isize) = 0
        ELSE
           w%ival(isize) = 1
        ENDIF
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! thegrilh
        w%list_elems(isize) = "thegrilh"
        w%elem_descr(isize) = "need description"
        w%component(isize) = "LH"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(nlhrf_src)"
        IF(ALLOCATED(ss%thegrilh)) THEN
           IF(MAXVAL(ABS(ss%thegrilh)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! wt_nphi
        w%list_elems(isize) = "wt_nphi"
        w%elem_descr(isize) = "vacuum spectrum n_phi weight"
        w%component(isize) = "IC"
        w%elem_units(isize) = "-"
        w%c256val(isize) = "(max_num_nphi,nicrf_src)"
        IF(ALLOCATED(ss%wt_nphi)) THEN
           IF(MAXVAL(ABS(ss%wt_nphi)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! Z_ant
        w%list_elems(isize) = "Z_ant"
        w%elem_descr(isize) = "height of antenna"
        w%component(isize) = "IC"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nicrf_src)"
        IF(ALLOCATED(ss%Z_ant)) THEN
           IF(MAXVAL(ABS(ss%Z_ant)).EQ.0.0_rspec) THEN
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
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! Z_mid_ant
        w%list_elems(isize) = "Z_mid_ant"
        w%elem_descr(isize) = "center of antenna relative to TF coil midplane"
        w%component(isize) = "IC"
        w%elem_units(isize) = "m"
        w%c256val(isize) = "(nicrf_src)"
        IF(ALLOCATED(ss%Z_mid_ant)) THEN
           IF(MAXVAL(ABS(ss%Z_mid_ant)).EQ.0.0_rspec) THEN
              w%ival(isize) = 0
           ELSE
              w%ival(isize) = 1
           ENDIF
        ELSE
           w%ival(isize) = -1
        ENDIF
     ENDIF
  ENDIF
END SUBROUTINE plasma_state_contents_sconfig
