! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


subroutine ps_mdescr_namelist_read(init,filename,tmp_filename,g_filename,ss, &
                                   ierr)

  ! read machine description namelist, copy into state
  ! if state is not empty, namelist input must not change existing
  ! non-zero array dimensions.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  LOGICAL, INTENT(IN) :: init                  ! .TRUE. to pre-initialize
  ! state variables prior to namelist read (caution -- could cause problems
  ! if partial machine description is already in the state).

  CHARACTER*(*), INTENT(IN) :: filename        ! file to contain namelist
  CHARACTER*(*), INTENT(IN) :: tmp_filename    ! namelist (comments removed)
  !  (leave tmp_filename blank to get internally generated filename)
  CHARACTER*(*), INTENT(IN) :: g_filename      ! if non-blank: G-EQDSK filename

  ! if G-eqdsk filename is provided, limiter and bounding box in G-EQDSK
  ! file will override that given in the machine description namelist.;

  TYPE (plasma_state) :: ss     ! state containing data to read
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------------------
  INTEGER :: iout,iertmp,isize,isiz0,ii
  INTEGER :: ilun_namel
  !-------------------------------
  CHARACTER*32 :: tokamak_id
  CHARACTER*256 :: geometry
  INTEGER :: num_rzlim
  REAL(KIND=rspec), dimension(ps_max_static) :: rlim
  REAL(KIND=rspec), dimension(ps_max_static) :: zlim
  REAL(KIND=rspec) :: R_min_box
  REAL(KIND=rspec) :: R_max_box
  REAL(KIND=rspec) :: Z_min_box
  REAL(KIND=rspec) :: Z_max_box
  CHARACTER*32, dimension(ps_max_static) :: circuit_name
  CHARACTER*32, dimension(ps_max_static) :: coil_name
  CHARACTER*32, dimension(ps_max_static) :: coil_in_circuit
  REAL(KIND=rspec), dimension(ps_max_static) :: Rloc_coil
  REAL(KIND=rspec), dimension(ps_max_static) :: Zloc_coil
  REAL(KIND=rspec), dimension(ps_max_static) :: Hsize_coil
  REAL(KIND=rspec), dimension(ps_max_static) :: Ang_Href_coil
  REAL(KIND=rspec), dimension(ps_max_static) :: Vsize_coil
  REAL(KIND=rspec), dimension(ps_max_static) :: Ang_Vref_coil
  INTEGER, dimension(ps_max_static) :: Nturns
  INTEGER, dimension(ps_max_static) :: VRep_coil_count
  REAL(KIND=rspec), dimension(ps_max_static) :: Vrep_coil_spacing
  REAL(KIND=rspec), dimension(ps_max_static) :: coil_resispt
  CHARACTER*32, dimension(ps_max_static) :: nbi_src_name
  CHARACTER*32, dimension(ps_max_static) :: beam_type
  REAL(KIND=rspec), dimension(ps_max_static) :: sRtcen
  REAL(KIND=rspec), dimension(ps_max_static) :: Lbsctan
  REAL(KIND=rspec), dimension(ps_max_static) :: Zbsc
  REAL(KIND=rspec), dimension(ps_max_static) :: Phibsc
  REAL(KIND=rspec), dimension(ps_max_static) :: Lbscap
  REAL(KIND=rspec), dimension(ps_max_static) :: Zbap
  CHARACTER*32, dimension(ps_max_static) :: nbshape
  REAL(KIND=rspec), dimension(ps_max_static) :: b_halfwidth
  REAL(KIND=rspec), dimension(ps_max_static) :: b_halfHeight
  REAL(KIND=rspec), dimension(ps_max_static) :: b_Hfocal_length
  REAL(KIND=rspec), dimension(ps_max_static) :: b_Vfocal_length
  REAL(KIND=rspec), dimension(ps_max_static) :: b_Hdivergence
  REAL(KIND=rspec), dimension(ps_max_static) :: b_Vdivergence
  CHARACTER*32, dimension(ps_max_static) :: nbap_shape
  REAL(KIND=rspec), dimension(ps_max_static) :: ap_halfwidth
  REAL(KIND=rspec), dimension(ps_max_static) :: ap_halfheight
  REAL(KIND=rspec), dimension(ps_max_static) :: ap_horiz_offset
  REAL(KIND=rspec), dimension(ps_max_static) :: ap_vert_offset
  CHARACTER*32, dimension(ps_max_static) :: nbap2_shape
  REAL(KIND=rspec), dimension(ps_max_static) :: Lbscap2
  REAL(KIND=rspec), dimension(ps_max_static) :: ap2_halfwidth
  REAL(KIND=rspec), dimension(ps_max_static) :: ap2_halfheight
  REAL(KIND=rspec), dimension(ps_max_static) :: ap2_horiz_offset
  REAL(KIND=rspec), dimension(ps_max_static) :: ap2_vert_offset
  REAL(KIND=rspec), dimension(ps_max_static) :: Einj_min
  REAL(KIND=rspec), dimension(ps_max_static) :: Einj_max
  INTEGER :: n_H_einj_standard
  INTEGER :: n_D_einj_standard
  INTEGER :: n_T_einj_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: H_einj_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: H_ffull_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: H_fhalf_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: D_einj_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: D_ffull_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: D_fhalf_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: T_einj_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: T_ffull_standard
  REAL(KIND=rspec), dimension(ps_max_static) :: T_fhalf_standard
  CHARACTER*32, dimension(ps_max_static_2d) :: icrf_src_name
  CHARACTER*256, dimension(ps_max_static_2d) :: ant_model
  INTEGER, dimension(ps_max_static_2d) :: nrz_antgeo
  INTEGER :: max_nrz_antgeo
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: R_antgeo
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: Z_antgeo
  REAL(KIND=rspec), dimension(ps_max_static_2d) :: dx_fshield
  CHARACTER*32, dimension(ps_max_static) :: ecrf_src_name
  REAL(KIND=rspec), dimension(ps_max_static) :: R_EC_launch
  REAL(KIND=rspec), dimension(ps_max_static) :: Z_EC_launch
  REAL(KIND=rspec), dimension(ps_max_static) :: Phi_EC_launch
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_PRINC_WIDTH_HOR
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_PRINC_WIDTH_VERT
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_PRINC_CURV_HOR
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_PRINC_CURV_VERT
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_Half_Power_Angle
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_Beam_Elongation
  CHARACTER*32, dimension(ps_max_static) :: lhrf_src_name
  INTEGER :: ntf_coils
  INTEGER :: ntf_coil2

  NAMELIST /mdescr/ &
       tokamak_id, &
       geometry, &
       num_rzlim, &
       rlim, &
       zlim, &
       R_min_box, &
       R_max_box, &
       Z_min_box, &
       Z_max_box, &
       circuit_name, &
       coil_name, &
       coil_in_circuit, &
       Rloc_coil, &
       Zloc_coil, &
       Hsize_coil, &
       Ang_Href_coil, &
       Vsize_coil, &
       Ang_Vref_coil, &
       Nturns, &
       VRep_coil_count, &
       Vrep_coil_spacing, &
       coil_resispt, &
       nbi_src_name, &
       beam_type, &
       sRtcen, &
       Lbsctan, &
       Zbsc, &
       Phibsc, &
       Lbscap, &
       Zbap, &
       nbshape, &
       b_halfwidth, &
       b_halfHeight, &
       b_Hfocal_length, &
       b_Vfocal_length, &
       b_Hdivergence, &
       b_Vdivergence, &
       nbap_shape, &
       ap_halfwidth, &
       ap_halfheight, &
       ap_horiz_offset, &
       ap_vert_offset, &
       nbap2_shape, &
       Lbscap2, &
       ap2_halfwidth, &
       ap2_halfheight, &
       ap2_horiz_offset, &
       ap2_vert_offset, &
       Einj_min, &
       Einj_max, &
       n_H_einj_standard, &
       n_D_einj_standard, &
       n_T_einj_standard, &
       H_einj_standard, &
       H_ffull_standard, &
       H_fhalf_standard, &
       D_einj_standard, &
       D_ffull_standard, &
       D_fhalf_standard, &
       T_einj_standard, &
       T_ffull_standard, &
       T_fhalf_standard, &
       icrf_src_name, &
       ant_model, &
       nrz_antgeo, &
       max_nrz_antgeo, &
       R_antgeo, &
       Z_antgeo, &
       dx_fshield, &
       ecrf_src_name, &
       R_EC_launch, &
       Z_EC_launch, &
       Phi_EC_launch, &
       EC_PRINC_WIDTH_HOR, &
       EC_PRINC_WIDTH_VERT, &
       EC_PRINC_CURV_HOR, &
       EC_PRINC_CURV_VERT, &
       EC_Half_Power_Angle, &
       EC_Beam_Elongation, &
       lhrf_src_name, &
       ntf_coils, &
       ntf_coil2
  !---------------------------------------

  iout = SWIM_err_out_file

  ierr = 0

  ! initialize local namelist variables to state values
  IF(init) ss%tokamak_id = " "
  tokamak_id = ss%tokamak_id
  IF(init) ss%geometry = " "
  geometry = ss%geometry
  IF(init) ss%num_rzlim = 0
  num_rzlim = ss%num_rzlim
  rlim = 0.0_rspec
  IF(ALLOCATED(ss%rlim)) THEN
     isize = size(rlim)
     IF(init) THEN
        ss%rlim(1:isize) = rlim(1:isize)
     ELSE
        rlim(1:isize) = ss%rlim(1:isize)
     ENDIF
  ENDIF
  zlim = 0.0_rspec
  IF(ALLOCATED(ss%zlim)) THEN
     isize = size(zlim)
     IF(init) THEN
        ss%zlim(1:isize) = zlim(1:isize)
     ELSE
        zlim(1:isize) = ss%zlim(1:isize)
     ENDIF
  ENDIF
  IF(init) ss%R_min_box = 0.0_rspec
  R_min_box = ss%R_min_box
  IF(init) ss%R_max_box = 0.0_rspec
  R_max_box = ss%R_max_box
  IF(init) ss%Z_min_box = 0.0_rspec
  Z_min_box = ss%Z_min_box
  IF(init) ss%Z_max_box = 0.0_rspec
  Z_max_box = ss%Z_max_box
  circuit_name = " "
  IF(ALLOCATED(ss%circuit_name)) THEN
     isize = size(circuit_name)
     IF(init) THEN
        ss%circuit_name(1:isize) = circuit_name(1:isize)
     ELSE
        circuit_name(1:isize) = ss%circuit_name(1:isize)
     ENDIF
  ENDIF
  coil_name = " "
  IF(ALLOCATED(ss%coil_name)) THEN
     isize = size(coil_name)
     IF(init) THEN
        ss%coil_name(1:isize) = coil_name(1:isize)
     ELSE
        coil_name(1:isize) = ss%coil_name(1:isize)
     ENDIF
  ENDIF
  coil_in_circuit = " "
  IF(ALLOCATED(ss%coil_in_circuit)) THEN
     isize = size(coil_in_circuit)
     IF(init) THEN
        ss%coil_in_circuit(1:isize) = coil_in_circuit(1:isize)
     ELSE
        coil_in_circuit(1:isize) = ss%coil_in_circuit(1:isize)
     ENDIF
  ENDIF
  Rloc_coil = 0.0_rspec
  IF(ALLOCATED(ss%Rloc_coil)) THEN
     isize = size(Rloc_coil)
     IF(init) THEN
        ss%Rloc_coil(1:isize) = Rloc_coil(1:isize)
     ELSE
        Rloc_coil(1:isize) = ss%Rloc_coil(1:isize)
     ENDIF
  ENDIF
  Zloc_coil = 0.0_rspec
  IF(ALLOCATED(ss%Zloc_coil)) THEN
     isize = size(Zloc_coil)
     IF(init) THEN
        ss%Zloc_coil(1:isize) = Zloc_coil(1:isize)
     ELSE
        Zloc_coil(1:isize) = ss%Zloc_coil(1:isize)
     ENDIF
  ENDIF
  Hsize_coil = 0.0_rspec
  IF(ALLOCATED(ss%Hsize_coil)) THEN
     isize = size(Hsize_coil)
     IF(init) THEN
        ss%Hsize_coil(1:isize) = Hsize_coil(1:isize)
     ELSE
        Hsize_coil(1:isize) = ss%Hsize_coil(1:isize)
     ENDIF
  ENDIF
  Ang_Href_coil = 0.0_rspec
  IF(ALLOCATED(ss%Ang_Href_coil)) THEN
     isize = size(Ang_Href_coil)
     IF(init) THEN
        ss%Ang_Href_coil(1:isize) = Ang_Href_coil(1:isize)
     ELSE
        Ang_Href_coil(1:isize) = ss%Ang_Href_coil(1:isize)
     ENDIF
  ENDIF
  Vsize_coil = 0.0_rspec
  IF(ALLOCATED(ss%Vsize_coil)) THEN
     isize = size(Vsize_coil)
     IF(init) THEN
        ss%Vsize_coil(1:isize) = Vsize_coil(1:isize)
     ELSE
        Vsize_coil(1:isize) = ss%Vsize_coil(1:isize)
     ENDIF
  ENDIF
  Ang_Vref_coil = 0.0_rspec
  IF(ALLOCATED(ss%Ang_Vref_coil)) THEN
     isize = size(Ang_Vref_coil)
     IF(init) THEN
        ss%Ang_Vref_coil(1:isize) = Ang_Vref_coil(1:isize)
     ELSE
        Ang_Vref_coil(1:isize) = ss%Ang_Vref_coil(1:isize)
     ENDIF
  ENDIF
  Nturns = ps_unInit
  IF(ALLOCATED(ss%Nturns)) THEN
     isize = size(Nturns)
     IF(init) THEN
        ss%Nturns(1:isize) = Nturns(1:isize)
     ELSE
        Nturns(1:isize) = ss%Nturns(1:isize)
     ENDIF
  ENDIF
  VRep_coil_count = ps_unInit
  IF(ALLOCATED(ss%VRep_coil_count)) THEN
     isize = size(VRep_coil_count)
     IF(init) THEN
        ss%VRep_coil_count(1:isize) = VRep_coil_count(1:isize)
     ELSE
        VRep_coil_count(1:isize) = ss%VRep_coil_count(1:isize)
     ENDIF
  ENDIF
  Vrep_coil_spacing = 0.0_rspec
  IF(ALLOCATED(ss%Vrep_coil_spacing)) THEN
     isize = size(Vrep_coil_spacing)
     IF(init) THEN
        ss%Vrep_coil_spacing(1:isize) = Vrep_coil_spacing(1:isize)
     ELSE
        Vrep_coil_spacing(1:isize) = ss%Vrep_coil_spacing(1:isize)
     ENDIF
  ENDIF
  coil_resispt = 0.0_rspec
  IF(ALLOCATED(ss%coil_resispt)) THEN
     isize = size(coil_resispt)
     IF(init) THEN
        ss%coil_resispt(1:isize) = coil_resispt(1:isize)
     ELSE
        coil_resispt(1:isize) = ss%coil_resispt(1:isize)
     ENDIF
  ENDIF
  nbi_src_name = " "
  IF(ALLOCATED(ss%nbi_src_name)) THEN
     isize = size(nbi_src_name)
     IF(init) THEN
        ss%nbi_src_name(1:isize) = nbi_src_name(1:isize)
     ELSE
        nbi_src_name(1:isize) = ss%nbi_src_name(1:isize)
     ENDIF
  ENDIF
  beam_type = " "
  IF(ALLOCATED(ss%beam_type)) THEN
     isize = size(beam_type)
     IF(init) THEN
        ss%beam_type(1:isize) = beam_type(1:isize)
     ELSE
        beam_type(1:isize) = ss%beam_type(1:isize)
     ENDIF
  ENDIF
  sRtcen = 0.0_rspec
  IF(ALLOCATED(ss%sRtcen)) THEN
     isize = size(sRtcen)
     IF(init) THEN
        ss%sRtcen(1:isize) = sRtcen(1:isize)
     ELSE
        sRtcen(1:isize) = ss%sRtcen(1:isize)
     ENDIF
  ENDIF
  Lbsctan = 0.0_rspec
  IF(ALLOCATED(ss%Lbsctan)) THEN
     isize = size(Lbsctan)
     IF(init) THEN
        ss%Lbsctan(1:isize) = Lbsctan(1:isize)
     ELSE
        Lbsctan(1:isize) = ss%Lbsctan(1:isize)
     ENDIF
  ENDIF
  Zbsc = 0.0_rspec
  IF(ALLOCATED(ss%Zbsc)) THEN
     isize = size(Zbsc)
     IF(init) THEN
        ss%Zbsc(1:isize) = Zbsc(1:isize)
     ELSE
        Zbsc(1:isize) = ss%Zbsc(1:isize)
     ENDIF
  ENDIF
  Phibsc = 0.0_rspec
  IF(ALLOCATED(ss%Phibsc)) THEN
     isize = size(Phibsc)
     IF(init) THEN
        ss%Phibsc(1:isize) = Phibsc(1:isize)
     ELSE
        Phibsc(1:isize) = ss%Phibsc(1:isize)
     ENDIF
  ENDIF
  Lbscap = 0.0_rspec
  IF(ALLOCATED(ss%Lbscap)) THEN
     isize = size(Lbscap)
     IF(init) THEN
        ss%Lbscap(1:isize) = Lbscap(1:isize)
     ELSE
        Lbscap(1:isize) = ss%Lbscap(1:isize)
     ENDIF
  ENDIF
  Zbap = 0.0_rspec
  IF(ALLOCATED(ss%Zbap)) THEN
     isize = size(Zbap)
     IF(init) THEN
        ss%Zbap(1:isize) = Zbap(1:isize)
     ELSE
        Zbap(1:isize) = ss%Zbap(1:isize)
     ENDIF
  ENDIF
  nbshape = " "
  IF(ALLOCATED(ss%nbshape)) THEN
     isize = size(nbshape)
     IF(init) THEN
        ss%nbshape(1:isize) = nbshape(1:isize)
     ELSE
        nbshape(1:isize) = ss%nbshape(1:isize)
     ENDIF
  ENDIF
  b_halfwidth = 0.0_rspec
  IF(ALLOCATED(ss%b_halfwidth)) THEN
     isize = size(b_halfwidth)
     IF(init) THEN
        ss%b_halfwidth(1:isize) = b_halfwidth(1:isize)
     ELSE
        b_halfwidth(1:isize) = ss%b_halfwidth(1:isize)
     ENDIF
  ENDIF
  b_halfHeight = 0.0_rspec
  IF(ALLOCATED(ss%b_halfHeight)) THEN
     isize = size(b_halfHeight)
     IF(init) THEN
        ss%b_halfHeight(1:isize) = b_halfHeight(1:isize)
     ELSE
        b_halfHeight(1:isize) = ss%b_halfHeight(1:isize)
     ENDIF
  ENDIF
  b_Hfocal_length = 0.0_rspec
  IF(ALLOCATED(ss%b_Hfocal_length)) THEN
     isize = size(b_Hfocal_length)
     IF(init) THEN
        ss%b_Hfocal_length(1:isize) = b_Hfocal_length(1:isize)
     ELSE
        b_Hfocal_length(1:isize) = ss%b_Hfocal_length(1:isize)
     ENDIF
  ENDIF
  b_Vfocal_length = 0.0_rspec
  IF(ALLOCATED(ss%b_Vfocal_length)) THEN
     isize = size(b_Vfocal_length)
     IF(init) THEN
        ss%b_Vfocal_length(1:isize) = b_Vfocal_length(1:isize)
     ELSE
        b_Vfocal_length(1:isize) = ss%b_Vfocal_length(1:isize)
     ENDIF
  ENDIF
  b_Hdivergence = 0.0_rspec
  IF(ALLOCATED(ss%b_Hdivergence)) THEN
     isize = size(b_Hdivergence)
     IF(init) THEN
        ss%b_Hdivergence(1:isize) = b_Hdivergence(1:isize)
     ELSE
        b_Hdivergence(1:isize) = ss%b_Hdivergence(1:isize)
     ENDIF
  ENDIF
  b_Vdivergence = 0.0_rspec
  IF(ALLOCATED(ss%b_Vdivergence)) THEN
     isize = size(b_Vdivergence)
     IF(init) THEN
        ss%b_Vdivergence(1:isize) = b_Vdivergence(1:isize)
     ELSE
        b_Vdivergence(1:isize) = ss%b_Vdivergence(1:isize)
     ENDIF
  ENDIF
  nbap_shape = " "
  IF(ALLOCATED(ss%nbap_shape)) THEN
     isize = size(nbap_shape)
     IF(init) THEN
        ss%nbap_shape(1:isize) = nbap_shape(1:isize)
     ELSE
        nbap_shape(1:isize) = ss%nbap_shape(1:isize)
     ENDIF
  ENDIF
  ap_halfwidth = 0.0_rspec
  IF(ALLOCATED(ss%ap_halfwidth)) THEN
     isize = size(ap_halfwidth)
     IF(init) THEN
        ss%ap_halfwidth(1:isize) = ap_halfwidth(1:isize)
     ELSE
        ap_halfwidth(1:isize) = ss%ap_halfwidth(1:isize)
     ENDIF
  ENDIF
  ap_halfheight = 0.0_rspec
  IF(ALLOCATED(ss%ap_halfheight)) THEN
     isize = size(ap_halfheight)
     IF(init) THEN
        ss%ap_halfheight(1:isize) = ap_halfheight(1:isize)
     ELSE
        ap_halfheight(1:isize) = ss%ap_halfheight(1:isize)
     ENDIF
  ENDIF
  ap_horiz_offset = 0.0_rspec
  IF(ALLOCATED(ss%ap_horiz_offset)) THEN
     isize = size(ap_horiz_offset)
     IF(init) THEN
        ss%ap_horiz_offset(1:isize) = ap_horiz_offset(1:isize)
     ELSE
        ap_horiz_offset(1:isize) = ss%ap_horiz_offset(1:isize)
     ENDIF
  ENDIF
  ap_vert_offset = 0.0_rspec
  IF(ALLOCATED(ss%ap_vert_offset)) THEN
     isize = size(ap_vert_offset)
     IF(init) THEN
        ss%ap_vert_offset(1:isize) = ap_vert_offset(1:isize)
     ELSE
        ap_vert_offset(1:isize) = ss%ap_vert_offset(1:isize)
     ENDIF
  ENDIF
  nbap2_shape = " "
  IF(ALLOCATED(ss%nbap2_shape)) THEN
     isize = size(nbap2_shape)
     IF(init) THEN
        ss%nbap2_shape(1:isize) = nbap2_shape(1:isize)
     ELSE
        nbap2_shape(1:isize) = ss%nbap2_shape(1:isize)
     ENDIF
  ENDIF
  Lbscap2 = 0.0_rspec
  IF(ALLOCATED(ss%Lbscap2)) THEN
     isize = size(Lbscap2)
     IF(init) THEN
        ss%Lbscap2(1:isize) = Lbscap2(1:isize)
     ELSE
        Lbscap2(1:isize) = ss%Lbscap2(1:isize)
     ENDIF
  ENDIF
  ap2_halfwidth = 0.0_rspec
  IF(ALLOCATED(ss%ap2_halfwidth)) THEN
     isize = size(ap2_halfwidth)
     IF(init) THEN
        ss%ap2_halfwidth(1:isize) = ap2_halfwidth(1:isize)
     ELSE
        ap2_halfwidth(1:isize) = ss%ap2_halfwidth(1:isize)
     ENDIF
  ENDIF
  ap2_halfheight = 0.0_rspec
  IF(ALLOCATED(ss%ap2_halfheight)) THEN
     isize = size(ap2_halfheight)
     IF(init) THEN
        ss%ap2_halfheight(1:isize) = ap2_halfheight(1:isize)
     ELSE
        ap2_halfheight(1:isize) = ss%ap2_halfheight(1:isize)
     ENDIF
  ENDIF
  ap2_horiz_offset = 0.0_rspec
  IF(ALLOCATED(ss%ap2_horiz_offset)) THEN
     isize = size(ap2_horiz_offset)
     IF(init) THEN
        ss%ap2_horiz_offset(1:isize) = ap2_horiz_offset(1:isize)
     ELSE
        ap2_horiz_offset(1:isize) = ss%ap2_horiz_offset(1:isize)
     ENDIF
  ENDIF
  ap2_vert_offset = 0.0_rspec
  IF(ALLOCATED(ss%ap2_vert_offset)) THEN
     isize = size(ap2_vert_offset)
     IF(init) THEN
        ss%ap2_vert_offset(1:isize) = ap2_vert_offset(1:isize)
     ELSE
        ap2_vert_offset(1:isize) = ss%ap2_vert_offset(1:isize)
     ENDIF
  ENDIF
  Einj_min = 0.0_rspec
  IF(ALLOCATED(ss%Einj_min)) THEN
     isize = size(Einj_min)
     IF(init) THEN
        ss%Einj_min(1:isize) = Einj_min(1:isize)
     ELSE
        Einj_min(1:isize) = ss%Einj_min(1:isize)
     ENDIF
  ENDIF
  Einj_max = 0.0_rspec
  IF(ALLOCATED(ss%Einj_max)) THEN
     isize = size(Einj_max)
     IF(init) THEN
        ss%Einj_max(1:isize) = Einj_max(1:isize)
     ELSE
        Einj_max(1:isize) = ss%Einj_max(1:isize)
     ENDIF
  ENDIF
  IF(init) ss%n_H_einj_standard = 0
  n_H_einj_standard = ss%n_H_einj_standard
  IF(init) ss%n_D_einj_standard = 0
  n_D_einj_standard = ss%n_D_einj_standard
  IF(init) ss%n_T_einj_standard = 0
  n_T_einj_standard = ss%n_T_einj_standard
  H_einj_standard = 0.0_rspec
  IF(ALLOCATED(ss%H_einj_standard)) THEN
     isize = size(H_einj_standard)
     IF(init) THEN
        ss%H_einj_standard(1:isize) = H_einj_standard(1:isize)
     ELSE
        H_einj_standard(1:isize) = ss%H_einj_standard(1:isize)
     ENDIF
  ENDIF
  H_ffull_standard = 0.0_rspec
  IF(ALLOCATED(ss%H_ffull_standard)) THEN
     isize = size(H_ffull_standard)
     IF(init) THEN
        ss%H_ffull_standard(1:isize) = H_ffull_standard(1:isize)
     ELSE
        H_ffull_standard(1:isize) = ss%H_ffull_standard(1:isize)
     ENDIF
  ENDIF
  H_fhalf_standard = 0.0_rspec
  IF(ALLOCATED(ss%H_fhalf_standard)) THEN
     isize = size(H_fhalf_standard)
     IF(init) THEN
        ss%H_fhalf_standard(1:isize) = H_fhalf_standard(1:isize)
     ELSE
        H_fhalf_standard(1:isize) = ss%H_fhalf_standard(1:isize)
     ENDIF
  ENDIF
  D_einj_standard = 0.0_rspec
  IF(ALLOCATED(ss%D_einj_standard)) THEN
     isize = size(D_einj_standard)
     IF(init) THEN
        ss%D_einj_standard(1:isize) = D_einj_standard(1:isize)
     ELSE
        D_einj_standard(1:isize) = ss%D_einj_standard(1:isize)
     ENDIF
  ENDIF
  D_ffull_standard = 0.0_rspec
  IF(ALLOCATED(ss%D_ffull_standard)) THEN
     isize = size(D_ffull_standard)
     IF(init) THEN
        ss%D_ffull_standard(1:isize) = D_ffull_standard(1:isize)
     ELSE
        D_ffull_standard(1:isize) = ss%D_ffull_standard(1:isize)
     ENDIF
  ENDIF
  D_fhalf_standard = 0.0_rspec
  IF(ALLOCATED(ss%D_fhalf_standard)) THEN
     isize = size(D_fhalf_standard)
     IF(init) THEN
        ss%D_fhalf_standard(1:isize) = D_fhalf_standard(1:isize)
     ELSE
        D_fhalf_standard(1:isize) = ss%D_fhalf_standard(1:isize)
     ENDIF
  ENDIF
  T_einj_standard = 0.0_rspec
  IF(ALLOCATED(ss%T_einj_standard)) THEN
     isize = size(T_einj_standard)
     IF(init) THEN
        ss%T_einj_standard(1:isize) = T_einj_standard(1:isize)
     ELSE
        T_einj_standard(1:isize) = ss%T_einj_standard(1:isize)
     ENDIF
  ENDIF
  T_ffull_standard = 0.0_rspec
  IF(ALLOCATED(ss%T_ffull_standard)) THEN
     isize = size(T_ffull_standard)
     IF(init) THEN
        ss%T_ffull_standard(1:isize) = T_ffull_standard(1:isize)
     ELSE
        T_ffull_standard(1:isize) = ss%T_ffull_standard(1:isize)
     ENDIF
  ENDIF
  T_fhalf_standard = 0.0_rspec
  IF(ALLOCATED(ss%T_fhalf_standard)) THEN
     isize = size(T_fhalf_standard)
     IF(init) THEN
        ss%T_fhalf_standard(1:isize) = T_fhalf_standard(1:isize)
     ELSE
        T_fhalf_standard(1:isize) = ss%T_fhalf_standard(1:isize)
     ENDIF
  ENDIF
  icrf_src_name = " "
  IF(ALLOCATED(ss%icrf_src_name)) THEN
     isize = size(icrf_src_name)
     IF(init) THEN
        ss%icrf_src_name(1:isize) = icrf_src_name(1:isize)
     ELSE
        icrf_src_name(1:isize) = ss%icrf_src_name(1:isize)
     ENDIF
  ENDIF
  ant_model = " "
  IF(ALLOCATED(ss%ant_model)) THEN
     isize = size(ant_model)
     IF(init) THEN
        ss%ant_model(1:isize) = ant_model(1:isize)
     ELSE
        ant_model(1:isize) = ss%ant_model(1:isize)
     ENDIF
  ENDIF
  nrz_antgeo = ps_unInit
  IF(ALLOCATED(ss%nrz_antgeo)) THEN
     isize = size(nrz_antgeo)
     IF(init) THEN
        ss%nrz_antgeo(1:isize) = nrz_antgeo(1:isize)
     ELSE
        nrz_antgeo(1:isize) = ss%nrz_antgeo(1:isize)
     ENDIF
  ENDIF
  IF(init) ss%max_nrz_antgeo = 0
  max_nrz_antgeo = ss%max_nrz_antgeo
  R_antgeo = 0.0_rspec
  IF(ALLOCATED(ss%R_antgeo)) THEN
     isiz0 = size(R_antgeo,1)
     isize = size(R_antgeo,2)
     IF(init) THEN
        ss%R_antgeo(1:isiz0,1:isize) = R_antgeo(1:isiz0,1:isize)
     ELSE
        R_antgeo(1:isiz0,1:isize) = ss%R_antgeo(1:isiz0,1:isize)
     ENDIF
  ENDIF
  Z_antgeo = 0.0_rspec
  IF(ALLOCATED(ss%Z_antgeo)) THEN
     isiz0 = size(Z_antgeo,1)
     isize = size(Z_antgeo,2)
     IF(init) THEN
        ss%Z_antgeo(1:isiz0,1:isize) = Z_antgeo(1:isiz0,1:isize)
     ELSE
        Z_antgeo(1:isiz0,1:isize) = ss%Z_antgeo(1:isiz0,1:isize)
     ENDIF
  ENDIF
  dx_fshield = 0.0_rspec
  IF(ALLOCATED(ss%dx_fshield)) THEN
     isize = size(dx_fshield)
     IF(init) THEN
        ss%dx_fshield(1:isize) = dx_fshield(1:isize)
     ELSE
        dx_fshield(1:isize) = ss%dx_fshield(1:isize)
     ENDIF
  ENDIF
  ecrf_src_name = " "
  IF(ALLOCATED(ss%ecrf_src_name)) THEN
     isize = size(ecrf_src_name)
     IF(init) THEN
        ss%ecrf_src_name(1:isize) = ecrf_src_name(1:isize)
     ELSE
        ecrf_src_name(1:isize) = ss%ecrf_src_name(1:isize)
     ENDIF
  ENDIF
  R_EC_launch = 0.0_rspec
  IF(ALLOCATED(ss%R_EC_launch)) THEN
     isize = size(R_EC_launch)
     IF(init) THEN
        ss%R_EC_launch(1:isize) = R_EC_launch(1:isize)
     ELSE
        R_EC_launch(1:isize) = ss%R_EC_launch(1:isize)
     ENDIF
  ENDIF
  Z_EC_launch = 0.0_rspec
  IF(ALLOCATED(ss%Z_EC_launch)) THEN
     isize = size(Z_EC_launch)
     IF(init) THEN
        ss%Z_EC_launch(1:isize) = Z_EC_launch(1:isize)
     ELSE
        Z_EC_launch(1:isize) = ss%Z_EC_launch(1:isize)
     ENDIF
  ENDIF
  Phi_EC_launch = 0.0_rspec
  IF(ALLOCATED(ss%Phi_EC_launch)) THEN
     isize = size(Phi_EC_launch)
     IF(init) THEN
        ss%Phi_EC_launch(1:isize) = Phi_EC_launch(1:isize)
     ELSE
        Phi_EC_launch(1:isize) = ss%Phi_EC_launch(1:isize)
     ENDIF
  ENDIF
  EC_PRINC_WIDTH_HOR = 0.0_rspec
  IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR)) THEN
     isize = size(EC_PRINC_WIDTH_HOR)
     IF(init) THEN
        ss%EC_PRINC_WIDTH_HOR(1:isize) = EC_PRINC_WIDTH_HOR(1:isize)
     ELSE
        EC_PRINC_WIDTH_HOR(1:isize) = ss%EC_PRINC_WIDTH_HOR(1:isize)
     ENDIF
  ENDIF
  EC_PRINC_WIDTH_VERT = 0.0_rspec
  IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT)) THEN
     isize = size(EC_PRINC_WIDTH_VERT)
     IF(init) THEN
        ss%EC_PRINC_WIDTH_VERT(1:isize) = EC_PRINC_WIDTH_VERT(1:isize)
     ELSE
        EC_PRINC_WIDTH_VERT(1:isize) = ss%EC_PRINC_WIDTH_VERT(1:isize)
     ENDIF
  ENDIF
  EC_PRINC_CURV_HOR = 0.0_rspec
  IF(ALLOCATED(ss%EC_PRINC_CURV_HOR)) THEN
     isize = size(EC_PRINC_CURV_HOR)
     IF(init) THEN
        ss%EC_PRINC_CURV_HOR(1:isize) = EC_PRINC_CURV_HOR(1:isize)
     ELSE
        EC_PRINC_CURV_HOR(1:isize) = ss%EC_PRINC_CURV_HOR(1:isize)
     ENDIF
  ENDIF
  EC_PRINC_CURV_VERT = 0.0_rspec
  IF(ALLOCATED(ss%EC_PRINC_CURV_VERT)) THEN
     isize = size(EC_PRINC_CURV_VERT)
     IF(init) THEN
        ss%EC_PRINC_CURV_VERT(1:isize) = EC_PRINC_CURV_VERT(1:isize)
     ELSE
        EC_PRINC_CURV_VERT(1:isize) = ss%EC_PRINC_CURV_VERT(1:isize)
     ENDIF
  ENDIF
  EC_Half_Power_Angle = 0.0_rspec
  IF(ALLOCATED(ss%EC_Half_Power_Angle)) THEN
     isize = size(EC_Half_Power_Angle)
     IF(init) THEN
        ss%EC_Half_Power_Angle(1:isize) = EC_Half_Power_Angle(1:isize)
     ELSE
        EC_Half_Power_Angle(1:isize) = ss%EC_Half_Power_Angle(1:isize)
     ENDIF
  ENDIF
  EC_Beam_Elongation = 0.0_rspec
  IF(ALLOCATED(ss%EC_Beam_Elongation)) THEN
     isize = size(EC_Beam_Elongation)
     IF(init) THEN
        ss%EC_Beam_Elongation(1:isize) = EC_Beam_Elongation(1:isize)
     ELSE
        EC_Beam_Elongation(1:isize) = ss%EC_Beam_Elongation(1:isize)
     ENDIF
  ENDIF
  lhrf_src_name = " "
  IF(ALLOCATED(ss%lhrf_src_name)) THEN
     isize = size(lhrf_src_name)
     IF(init) THEN
        ss%lhrf_src_name(1:isize) = lhrf_src_name(1:isize)
     ELSE
        lhrf_src_name(1:isize) = ss%lhrf_src_name(1:isize)
     ENDIF
  ENDIF
  IF(init) ss%ntf_coils = 0
  ntf_coils = ss%ntf_coils
  IF(init) ss%ntf_coil2 = 0
  ntf_coil2 = ss%ntf_coil2

  IF(ierr.GT.0) RETURN

  ! OK: open namelist file, strip comments.
  !     namelist to be read on ilun_namel after this call:

  CALL ps_namel_strip(filename,tmp_filename,ilun_namel,iout,ierr)

  IF(ierr.NE.0) THEN
     WRITE(iout,*) ' ?namelist file OPEN or READ error: '//trim(filename)
     RETURN
  ENDIF

  READ(UNIT=ilun_namel,NML=mdescr)

  CLOSE(UNIT=ilun_namel,status="DELETE")

  ! check list quantities; infer dimensions

  ss%num_rzlim = num_rzlim

  CALL ps_namrd_ilist_chk("circuit_name", &
       ps_max_static,circuit_name,ss%NCIRCUITS,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  CALL ps_namrd_ilist_chk("coil_name", &
       ps_max_static,coil_name,ss%NCOILS,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  CALL ps_namrd_ilist_chk("nbi_src_name", &
       ps_max_static,nbi_src_name,ss%NBEAM,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  ss%n_H_einj_standard = n_H_einj_standard

  ss%n_D_einj_standard = n_D_einj_standard

  ss%n_T_einj_standard = n_T_einj_standard

  CALL ps_namrd_ilist_chk("icrf_src_name", &
       ps_max_static_2d,icrf_src_name,ss%NICRF_SRC,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  ss%max_nrz_antgeo = max_nrz_antgeo

  CALL ps_namrd_ilist_chk("ecrf_src_name", &
       ps_max_static,ecrf_src_name,ss%NECRF_SRC,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  CALL ps_namrd_ilist_chk("lhrf_src_name", &
       ps_max_static,lhrf_src_name,ss%NLHRF_SRC,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  IF(ierr.NE.0) THEN
     WRITE(iout,*) " ?mdescr namelist list check errors after read."
     RETURN
  ENDIF

  ! OK, allocate arrays per dimensioning in namelist
  CALL plasma_state_empty_alloc(ss,ierr)
  IF(ierr.NE.0) RETURN

  ! Now pick up any vector dimensions

  isize = ss%NCOILS
  ss%Nturns(1:isize) = Nturns(1:isize)

  isize = ss%NCOILS
  ss%VRep_coil_count(1:isize) = VRep_coil_count(1:isize)

  isize = ss%NICRF_SRC
  ss%nrz_antgeo(1:isize) = nrz_antgeo(1:isize)

  ! OK, allocate arrays per vector dimensioning in namelist
  CALL plasma_state_empty_alloc(ss,ierr)
  IF(ierr.NE.0) RETURN

  ! Now copy namelist variables into state
  ss%tokamak_id = tokamak_id
  ss%geometry = geometry

  IF(ALLOCATED(ss%rlim)) THEN
     isize = ss%NUM_RZLIM
     ss%rlim(1:isize) = rlim(1:isize)
  ENDIF

  IF(ALLOCATED(ss%zlim)) THEN
     isize = ss%NUM_RZLIM
     ss%zlim(1:isize) = zlim(1:isize)
  ENDIF
  ss%R_min_box = R_min_box
  ss%R_max_box = R_max_box
  ss%Z_min_box = Z_min_box
  ss%Z_max_box = Z_max_box

  IF(ALLOCATED(ss%circuit_name)) THEN
     isize = ss%NCIRCUITS
     ss%circuit_name(1:isize) = circuit_name(1:isize)
  ENDIF

  IF(ALLOCATED(ss%coil_name)) THEN
     isize = ss%NCOILS
     ss%coil_name(1:isize) = coil_name(1:isize)
  ENDIF

  IF(ALLOCATED(ss%coil_in_circuit)) THEN
     isize = ss%NCOILS
     ss%coil_in_circuit(1:isize) = coil_in_circuit(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Rloc_coil)) THEN
     isize = ss%NCOILS
     ss%Rloc_coil(1:isize) = Rloc_coil(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Zloc_coil)) THEN
     isize = ss%NCOILS
     ss%Zloc_coil(1:isize) = Zloc_coil(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Hsize_coil)) THEN
     isize = ss%NCOILS
     ss%Hsize_coil(1:isize) = Hsize_coil(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Ang_Href_coil)) THEN
     isize = ss%NCOILS
     ss%Ang_Href_coil(1:isize) = Ang_Href_coil(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Vsize_coil)) THEN
     isize = ss%NCOILS
     ss%Vsize_coil(1:isize) = Vsize_coil(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Ang_Vref_coil)) THEN
     isize = ss%NCOILS
     ss%Ang_Vref_coil(1:isize) = Ang_Vref_coil(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Nturns)) THEN
     isize = ss%NCOILS
     ss%Nturns(1:isize) = Nturns(1:isize)
  ENDIF

  IF(ALLOCATED(ss%VRep_coil_count)) THEN
     isize = ss%NCOILS
     ss%VRep_coil_count(1:isize) = VRep_coil_count(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Vrep_coil_spacing)) THEN
     isize = ss%NCOILS
     ss%Vrep_coil_spacing(1:isize) = Vrep_coil_spacing(1:isize)
  ENDIF

  IF(ALLOCATED(ss%coil_resispt)) THEN
     isize = ss%NCOILS
     ss%coil_resispt(1:isize) = coil_resispt(1:isize)
  ENDIF

  IF(ALLOCATED(ss%nbi_src_name)) THEN
     isize = ss%NBEAM
     ss%nbi_src_name(1:isize) = nbi_src_name(1:isize)
  ENDIF

  IF(ALLOCATED(ss%beam_type)) THEN
     isize = ss%NBEAM
     ss%beam_type(1:isize) = beam_type(1:isize)
  ENDIF

  IF(ALLOCATED(ss%sRtcen)) THEN
     isize = ss%NBEAM
     ss%sRtcen(1:isize) = sRtcen(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Lbsctan)) THEN
     isize = ss%NBEAM
     ss%Lbsctan(1:isize) = Lbsctan(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Zbsc)) THEN
     isize = ss%NBEAM
     ss%Zbsc(1:isize) = Zbsc(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Phibsc)) THEN
     isize = ss%NBEAM
     ss%Phibsc(1:isize) = Phibsc(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Lbscap)) THEN
     isize = ss%NBEAM
     ss%Lbscap(1:isize) = Lbscap(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Zbap)) THEN
     isize = ss%NBEAM
     ss%Zbap(1:isize) = Zbap(1:isize)
  ENDIF

  IF(ALLOCATED(ss%nbshape)) THEN
     isize = ss%NBEAM
     ss%nbshape(1:isize) = nbshape(1:isize)
  ENDIF

  IF(ALLOCATED(ss%b_halfwidth)) THEN
     isize = ss%NBEAM
     ss%b_halfwidth(1:isize) = b_halfwidth(1:isize)
  ENDIF

  IF(ALLOCATED(ss%b_halfHeight)) THEN
     isize = ss%NBEAM
     ss%b_halfHeight(1:isize) = b_halfHeight(1:isize)
  ENDIF

  IF(ALLOCATED(ss%b_Hfocal_length)) THEN
     isize = ss%NBEAM
     ss%b_Hfocal_length(1:isize) = b_Hfocal_length(1:isize)
  ENDIF

  IF(ALLOCATED(ss%b_Vfocal_length)) THEN
     isize = ss%NBEAM
     ss%b_Vfocal_length(1:isize) = b_Vfocal_length(1:isize)
  ENDIF

  IF(ALLOCATED(ss%b_Hdivergence)) THEN
     isize = ss%NBEAM
     ss%b_Hdivergence(1:isize) = b_Hdivergence(1:isize)
  ENDIF

  IF(ALLOCATED(ss%b_Vdivergence)) THEN
     isize = ss%NBEAM
     ss%b_Vdivergence(1:isize) = b_Vdivergence(1:isize)
  ENDIF

  IF(ALLOCATED(ss%nbap_shape)) THEN
     isize = ss%NBEAM
     ss%nbap_shape(1:isize) = nbap_shape(1:isize)
  ENDIF

  IF(ALLOCATED(ss%ap_halfwidth)) THEN
     isize = ss%NBEAM
     ss%ap_halfwidth(1:isize) = ap_halfwidth(1:isize)
  ENDIF

  IF(ALLOCATED(ss%ap_halfheight)) THEN
     isize = ss%NBEAM
     ss%ap_halfheight(1:isize) = ap_halfheight(1:isize)
  ENDIF

  IF(ALLOCATED(ss%ap_horiz_offset)) THEN
     isize = ss%NBEAM
     ss%ap_horiz_offset(1:isize) = ap_horiz_offset(1:isize)
  ENDIF

  IF(ALLOCATED(ss%ap_vert_offset)) THEN
     isize = ss%NBEAM
     ss%ap_vert_offset(1:isize) = ap_vert_offset(1:isize)
  ENDIF

  IF(ALLOCATED(ss%nbap2_shape)) THEN
     isize = ss%NBEAM
     ss%nbap2_shape(1:isize) = nbap2_shape(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Lbscap2)) THEN
     isize = ss%NBEAM
     ss%Lbscap2(1:isize) = Lbscap2(1:isize)
  ENDIF

  IF(ALLOCATED(ss%ap2_halfwidth)) THEN
     isize = ss%NBEAM
     ss%ap2_halfwidth(1:isize) = ap2_halfwidth(1:isize)
  ENDIF

  IF(ALLOCATED(ss%ap2_halfheight)) THEN
     isize = ss%NBEAM
     ss%ap2_halfheight(1:isize) = ap2_halfheight(1:isize)
  ENDIF

  IF(ALLOCATED(ss%ap2_horiz_offset)) THEN
     isize = ss%NBEAM
     ss%ap2_horiz_offset(1:isize) = ap2_horiz_offset(1:isize)
  ENDIF

  IF(ALLOCATED(ss%ap2_vert_offset)) THEN
     isize = ss%NBEAM
     ss%ap2_vert_offset(1:isize) = ap2_vert_offset(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Einj_min)) THEN
     isize = ss%NBEAM
     ss%Einj_min(1:isize) = Einj_min(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Einj_max)) THEN
     isize = ss%NBEAM
     ss%Einj_max(1:isize) = Einj_max(1:isize)
  ENDIF

  IF(ALLOCATED(ss%H_einj_standard)) THEN
     isize = ss%N_H_EINJ_STANDARD
     ss%H_einj_standard(1:isize) = H_einj_standard(1:isize)
  ENDIF

  IF(ALLOCATED(ss%H_ffull_standard)) THEN
     isize = ss%N_H_EINJ_STANDARD
     ss%H_ffull_standard(1:isize) = H_ffull_standard(1:isize)
  ENDIF

  IF(ALLOCATED(ss%H_fhalf_standard)) THEN
     isize = ss%N_H_EINJ_STANDARD
     ss%H_fhalf_standard(1:isize) = H_fhalf_standard(1:isize)
  ENDIF

  IF(ALLOCATED(ss%D_einj_standard)) THEN
     isize = ss%N_D_EINJ_STANDARD
     ss%D_einj_standard(1:isize) = D_einj_standard(1:isize)
  ENDIF

  IF(ALLOCATED(ss%D_ffull_standard)) THEN
     isize = ss%N_D_EINJ_STANDARD
     ss%D_ffull_standard(1:isize) = D_ffull_standard(1:isize)
  ENDIF

  IF(ALLOCATED(ss%D_fhalf_standard)) THEN
     isize = ss%N_D_EINJ_STANDARD
     ss%D_fhalf_standard(1:isize) = D_fhalf_standard(1:isize)
  ENDIF

  IF(ALLOCATED(ss%T_einj_standard)) THEN
     isize = ss%N_T_EINJ_STANDARD
     ss%T_einj_standard(1:isize) = T_einj_standard(1:isize)
  ENDIF

  IF(ALLOCATED(ss%T_ffull_standard)) THEN
     isize = ss%N_T_EINJ_STANDARD
     ss%T_ffull_standard(1:isize) = T_ffull_standard(1:isize)
  ENDIF

  IF(ALLOCATED(ss%T_fhalf_standard)) THEN
     isize = ss%N_T_EINJ_STANDARD
     ss%T_fhalf_standard(1:isize) = T_fhalf_standard(1:isize)
  ENDIF

  IF(ALLOCATED(ss%icrf_src_name)) THEN
     isize = ss%NICRF_SRC
     ss%icrf_src_name(1:isize) = icrf_src_name(1:isize)
  ENDIF

  IF(ALLOCATED(ss%ant_model)) THEN
     isize = ss%NICRF_SRC
     ss%ant_model(1:isize) = ant_model(1:isize)
  ENDIF

  IF(ALLOCATED(ss%nrz_antgeo)) THEN
     isize = ss%NICRF_SRC
     ss%nrz_antgeo(1:isize) = nrz_antgeo(1:isize)
  ENDIF

  IF(ALLOCATED(ss%R_antgeo)) THEN
     isiz0 = maxval(ss%NRZ_ANTGEO)
     isize = ss%NICRF_SRC
     ss%R_antgeo(1:isiz0,1:isize) = R_antgeo(1:isiz0,1:isize)
  ENDIF

  IF(ALLOCATED(ss%Z_antgeo)) THEN
     isiz0 = maxval(ss%NRZ_ANTGEO)
     isize = ss%NICRF_SRC
     ss%Z_antgeo(1:isiz0,1:isize) = Z_antgeo(1:isiz0,1:isize)
  ENDIF

  IF(ALLOCATED(ss%dx_fshield)) THEN
     isize = ss%NICRF_SRC
     ss%dx_fshield(1:isize) = dx_fshield(1:isize)
  ENDIF

  IF(ALLOCATED(ss%ecrf_src_name)) THEN
     isize = ss%NECRF_SRC
     ss%ecrf_src_name(1:isize) = ecrf_src_name(1:isize)
  ENDIF

  IF(ALLOCATED(ss%R_EC_launch)) THEN
     isize = ss%NECRF_SRC
     ss%R_EC_launch(1:isize) = R_EC_launch(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Z_EC_launch)) THEN
     isize = ss%NECRF_SRC
     ss%Z_EC_launch(1:isize) = Z_EC_launch(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Phi_EC_launch)) THEN
     isize = ss%NECRF_SRC
     ss%Phi_EC_launch(1:isize) = Phi_EC_launch(1:isize)
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR)) THEN
     isize = ss%NECRF_SRC
     ss%EC_PRINC_WIDTH_HOR(1:isize) = EC_PRINC_WIDTH_HOR(1:isize)
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT)) THEN
     isize = ss%NECRF_SRC
     ss%EC_PRINC_WIDTH_VERT(1:isize) = EC_PRINC_WIDTH_VERT(1:isize)
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_CURV_HOR)) THEN
     isize = ss%NECRF_SRC
     ss%EC_PRINC_CURV_HOR(1:isize) = EC_PRINC_CURV_HOR(1:isize)
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_CURV_VERT)) THEN
     isize = ss%NECRF_SRC
     ss%EC_PRINC_CURV_VERT(1:isize) = EC_PRINC_CURV_VERT(1:isize)
  ENDIF

  IF(ALLOCATED(ss%EC_Half_Power_Angle)) THEN
     isize = ss%NECRF_SRC
     ss%EC_Half_Power_Angle(1:isize) = EC_Half_Power_Angle(1:isize)
  ENDIF

  IF(ALLOCATED(ss%EC_Beam_Elongation)) THEN
     isize = ss%NECRF_SRC
     ss%EC_Beam_Elongation(1:isize) = EC_Beam_Elongation(1:isize)
  ENDIF

  IF(ALLOCATED(ss%lhrf_src_name)) THEN
     isize = ss%NLHRF_SRC
     ss%lhrf_src_name(1:isize) = lhrf_src_name(1:isize)
  ENDIF
  ss%ntf_coils = ntf_coils
  ss%ntf_coil2 = ntf_coil2


  IF(g_filename.NE.' ') THEN
     CALL ps_mdescr_gfile_read(g_filename,ss,ierr)
     IF(ierr.NE.0) THEN
        WRITE(iout,*) ' ?ps_mdescr_namelist_read: gfile read error.'
     ENDIF
  ENDIF

END SUBROUTINE ps_mdescr_namelist_read
