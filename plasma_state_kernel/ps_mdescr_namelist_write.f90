! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


subroutine ps_mdescr_namelist_write(filename,ss,ierr)

  ! from current contents of state, write machine description namelist

  USE plasma_state_definition_mod
  USE ps_namwr_mod
  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN) :: filename        ! file to contain namelist
  TYPE (plasma_state) :: ss     ! state containing data to write
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------------------
  INTEGER :: iout, ilun_namel
  INTEGER, DIMENSION(:), ALLOCATABLE :: isize_vec
  !-------------------------------

  iout = SWIM_err_out_file

  CALL find_io_unit(ilun_namel)

  OPEN(UNIT=ilun_namel,file=filename,status='unknown',iostat=ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) ' ?ps_mdescr_namelist_write: OPEN failure on file: '// &
          TRIM(filename)
     RETURN
  ENDIF

  WRITE(unit=ilun_namel,fmt='(" &mdescr")')

  CALL ps_namwr(ilun_namel, &
       "Tokamak (Machine Description) ID","character", &
       "tokamak_id", ss%tokamak_id)

  CALL ps_namwr(ilun_namel, &
       "path to file(s) vac. vessel & coil descr.","character", &
       "geometry", ss%geometry)

  CALL ps_namwr(ilun_namel, &
       "number of points, axisymmetric (R,Z) limiter","integer", &
       "num_rzlim", ss%num_rzlim)

  IF(ALLOCATED(ss%rlim)) THEN
     CALL ps_namwr(ilun_namel, &
          "R points in closed (R,Z) contour sequence","m", &
          "rlim", ss%NUM_RZLIM, ss%rlim)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "R points in closed (R,Z) contour sequence","m", "rlim")
  ENDIF

  IF(ALLOCATED(ss%zlim)) THEN
     CALL ps_namwr(ilun_namel, &
          "Z points in closed (R,Z) contour sequence","m", &
          "zlim", ss%NUM_RZLIM, ss%zlim)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "Z points in closed (R,Z) contour sequence","m", "zlim")
  ENDIF

  CALL ps_namwr(ilun_namel, &
       "R_min of bounding box","m", &
       "R_min_box", ss%R_min_box)

  CALL ps_namwr(ilun_namel, &
       "R_max of bounding box","m", &
       "R_max_box", ss%R_max_box)

  CALL ps_namwr(ilun_namel, &
       "Z_min of bounding box","m", &
       "Z_min_box", ss%Z_min_box)

  CALL ps_namwr(ilun_namel, &
       "Z_max of bounding box","m", &
       "Z_max_box", ss%Z_max_box)

  IF(ALLOCATED(ss%circuit_name)) THEN
     CALL ps_namwr(ilun_namel, &
          "Number & names of circuits","character", &
          "circuit_name", ss%NCIRCUITS, ss%circuit_name)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "Number & names of circuits","character", "circuit_name")
  ENDIF

  IF(ALLOCATED(ss%coil_name)) THEN
     CALL ps_namwr(ilun_namel, &
          "Number & name of axisymmetric coils","character", &
          "coil_name", ss%NCOILS, ss%coil_name)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "Number & name of axisymmetric coils","character", "coil_name")
  ENDIF

  IF(ALLOCATED(ss%coil_in_circuit)) THEN
     CALL ps_namwr(ilun_namel, &
          "circuit to which each coil belongs","character", &
          "coil_in_circuit", ss%NCOILS, ss%coil_in_circuit)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "circuit to which each coil belongs","character", "coil_in_circuit")
  ENDIF

  IF(ALLOCATED(ss%Rloc_coil)) THEN
     CALL ps_namwr(ilun_namel, &
          "R location of coil, lower left corner","m", &
          "Rloc_coil", ss%NCOILS, ss%Rloc_coil)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "R location of coil, lower left corner","m", "Rloc_coil")
  ENDIF

  IF(ALLOCATED(ss%Zloc_coil)) THEN
     CALL ps_namwr(ilun_namel, &
          "Z location of coil, lower left corner","m", &
          "Zloc_coil", ss%NCOILS, ss%Zloc_coil)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "Z location of coil, lower left corner","m", "Zloc_coil")
  ENDIF

  IF(ALLOCATED(ss%Hsize_coil)) THEN
     CALL ps_namwr(ilun_namel, &
          "(Rotatable) Horizontal coil base size","m", &
          "Hsize_coil", ss%NCOILS, ss%Hsize_coil)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "(Rotatable) Horizontal coil base size","m", "Hsize_coil")
  ENDIF

  IF(ALLOCATED(ss%Ang_Href_coil)) THEN
     CALL ps_namwr(ilun_namel, &
          "angle w.r.t. horizontal of coil base","degrees", &
          "Ang_Href_coil", ss%NCOILS, ss%Ang_Href_coil)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "angle w.r.t. horizontal of coil base","degrees", "Ang_Href_coil")
  ENDIF

  IF(ALLOCATED(ss%Vsize_coil)) THEN
     CALL ps_namwr(ilun_namel, &
          "(Rotatable) Vertical coil size","m", &
          "Vsize_coil", ss%NCOILS, ss%Vsize_coil)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "(Rotatable) Vertical coil size","m", "Vsize_coil")
  ENDIF

  IF(ALLOCATED(ss%Ang_Vref_coil)) THEN
     CALL ps_namwr(ilun_namel, &
          "angle w.r.t. vertical of coil sides","degrees", &
          "Ang_Vref_coil", ss%NCOILS, ss%Ang_Vref_coil)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "angle w.r.t. vertical of coil sides","degrees", "Ang_Vref_coil")
  ENDIF

  IF(ALLOCATED(ss%Nturns)) THEN
     CALL ps_namwr(ilun_namel, &
          "SIGNED number of conductor turns wrapped in each coil","integer", &
          "Nturns", ss%NCOILS, ss%Nturns)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "SIGNED number of conductor turns wrapped in each coil","integer", "Nturns")
  ENDIF

  IF(ALLOCATED(ss%VRep_coil_count)) THEN
     CALL ps_namwr(ilun_namel, &
          "replication count, vertical coil stack","integer", &
          "VRep_coil_count", ss%NCOILS, ss%VRep_coil_count)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "replication count, vertical coil stack","integer", "VRep_coil_count")
  ENDIF

  IF(ALLOCATED(ss%Vrep_coil_spacing)) THEN
     CALL ps_namwr(ilun_namel, &
          "replicated coil separation","m", &
          "Vrep_coil_spacing", ss%NCOILS, ss%Vrep_coil_spacing)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "replicated coil separation","m", "Vrep_coil_spacing")
  ENDIF

  IF(ALLOCATED(ss%coil_resispt)) THEN
     CALL ps_namwr(ilun_namel, &
          "coil resistance per turn","ohms/turn", &
          "coil_resispt", ss%NCOILS, ss%coil_resispt)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "coil resistance per turn","ohms/turn", "coil_resispt")
  ENDIF

  IF(ALLOCATED(ss%nbi_src_name)) THEN
     CALL ps_namwr(ilun_namel, &
          "number & name of neutral beams","character", &
          "nbi_src_name", ss%NBEAM, ss%nbi_src_name)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "number & name of neutral beams","character", "nbi_src_name")
  ENDIF

  IF(ALLOCATED(ss%beam_type)) THEN
     CALL ps_namwr(ilun_namel, &
          "beam type:","character", &
          "beam_type", ss%NBEAM, ss%beam_type)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "beam type:","character", "beam_type")
  ENDIF

  IF(ALLOCATED(ss%sRtcen)) THEN
     CALL ps_namwr(ilun_namel, &
          "signed tangency radius","m", &
          "sRtcen", ss%NBEAM, ss%sRtcen)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "signed tangency radius","m", "sRtcen")
  ENDIF

  IF(ALLOCATED(ss%Lbsctan)) THEN
     CALL ps_namwr(ilun_namel, &
          "distance, source to tangency point","m", &
          "Lbsctan", ss%NBEAM, ss%Lbsctan)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "distance, source to tangency point","m", "Lbsctan")
  ENDIF

  IF(ALLOCATED(ss%Zbsc)) THEN
     CALL ps_namwr(ilun_namel, &
          "elevation of beam source","m", &
          "Zbsc", ss%NBEAM, ss%Zbsc)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "elevation of beam source","m", "Zbsc")
  ENDIF

  IF(ALLOCATED(ss%Phibsc)) THEN
     CALL ps_namwr(ilun_namel, &
          "toroidal location of beam source","degrees", &
          "Phibsc", ss%NBEAM, ss%Phibsc)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "toroidal location of beam source","degrees", "Phibsc")
  ENDIF

  IF(ALLOCATED(ss%Lbscap)) THEN
     CALL ps_namwr(ilun_namel, &
          "distance, source to aperture","m", &
          "Lbscap", ss%NBEAM, ss%Lbscap)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "distance, source to aperture","m", "Lbscap")
  ENDIF

  IF(ALLOCATED(ss%Zbap)) THEN
     CALL ps_namwr(ilun_namel, &
          "elevation of beam centerline at aperture","m", &
          "Zbap", ss%NBEAM, ss%Zbap)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "elevation of beam centerline at aperture","m", "Zbap")
  ENDIF

  IF(ALLOCATED(ss%nbshape)) THEN
     CALL ps_namwr(ilun_namel, &
          "shape keyword for beam source grid","character", &
          "nbshape", ss%NBEAM, ss%nbshape)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "shape keyword for beam source grid","character", "nbshape")
  ENDIF

  IF(ALLOCATED(ss%b_halfwidth)) THEN
     CALL ps_namwr(ilun_namel, &
          "beam half-width at source","m", &
          "b_halfwidth", ss%NBEAM, ss%b_halfwidth)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "beam half-width at source","m", "b_halfwidth")
  ENDIF

  IF(ALLOCATED(ss%b_halfHeight)) THEN
     CALL ps_namwr(ilun_namel, &
          "beam half-height at source","m", &
          "b_halfHeight", ss%NBEAM, ss%b_halfHeight)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "beam half-height at source","m", "b_halfHeight")
  ENDIF

  IF(ALLOCATED(ss%b_Hfocal_length)) THEN
     CALL ps_namwr(ilun_namel, &
          "horizontal focal length","m", &
          "b_Hfocal_length", ss%NBEAM, ss%b_Hfocal_length)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "horizontal focal length","m", "b_Hfocal_length")
  ENDIF

  IF(ALLOCATED(ss%b_Vfocal_length)) THEN
     CALL ps_namwr(ilun_namel, &
          "vertical focal length","m", &
          "b_Vfocal_length", ss%NBEAM, ss%b_Vfocal_length)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "vertical focal length","m", "b_Vfocal_length")
  ENDIF

  IF(ALLOCATED(ss%b_Hdivergence)) THEN
     CALL ps_namwr(ilun_namel, &
          "horizontal divergence (D) of beam","degrees", &
          "b_Hdivergence", ss%NBEAM, ss%b_Hdivergence)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "horizontal divergence (D) of beam","degrees", "b_Hdivergence")
  ENDIF

  IF(ALLOCATED(ss%b_Vdivergence)) THEN
     CALL ps_namwr(ilun_namel, &
          "vertical divergence (D) of beam","degrees", &
          "b_Vdivergence", ss%NBEAM, ss%b_Vdivergence)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "vertical divergence (D) of beam","degrees", "b_Vdivergence")
  ENDIF

  IF(ALLOCATED(ss%nbap_shape)) THEN
     CALL ps_namwr(ilun_namel, &
          "shape keyword for beam aperture","character", &
          "nbap_shape", ss%NBEAM, ss%nbap_shape)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "shape keyword for beam aperture","character", "nbap_shape")
  ENDIF

  IF(ALLOCATED(ss%ap_halfwidth)) THEN
     CALL ps_namwr(ilun_namel, &
          "aperture half-width","m", &
          "ap_halfwidth", ss%NBEAM, ss%ap_halfwidth)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "aperture half-width","m", "ap_halfwidth")
  ENDIF

  IF(ALLOCATED(ss%ap_halfheight)) THEN
     CALL ps_namwr(ilun_namel, &
          "aperture half-height","m", &
          "ap_halfheight", ss%NBEAM, ss%ap_halfheight)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "aperture half-height","m", "ap_halfheight")
  ENDIF

  IF(ALLOCATED(ss%ap_horiz_offset)) THEN
     CALL ps_namwr(ilun_namel, &
          "horizontal offset of aperture","m", &
          "ap_horiz_offset", ss%NBEAM, ss%ap_horiz_offset)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "horizontal offset of aperture","m", "ap_horiz_offset")
  ENDIF

  IF(ALLOCATED(ss%ap_vert_offset)) THEN
     CALL ps_namwr(ilun_namel, &
          "vertical offset of aperture","m", &
          "ap_vert_offset", ss%NBEAM, ss%ap_vert_offset)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "vertical offset of aperture","m", "ap_vert_offset")
  ENDIF

  IF(ALLOCATED(ss%nbap2_shape)) THEN
     CALL ps_namwr(ilun_namel, &
          "keyword for optional 2nd aperture","character", &
          "nbap2_shape", ss%NBEAM, ss%nbap2_shape)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "keyword for optional 2nd aperture","character", "nbap2_shape")
  ENDIF

  IF(ALLOCATED(ss%Lbscap2)) THEN
     CALL ps_namwr(ilun_namel, &
          "distance, source to 2nd aperture","m", &
          "Lbscap2", ss%NBEAM, ss%Lbscap2)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "distance, source to 2nd aperture","m", "Lbscap2")
  ENDIF

  IF(ALLOCATED(ss%ap2_halfwidth)) THEN
     CALL ps_namwr(ilun_namel, &
          "2nd aperture half-width","m", &
          "ap2_halfwidth", ss%NBEAM, ss%ap2_halfwidth)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "2nd aperture half-width","m", "ap2_halfwidth")
  ENDIF

  IF(ALLOCATED(ss%ap2_halfheight)) THEN
     CALL ps_namwr(ilun_namel, &
          "aperture half-height","m", &
          "ap2_halfheight", ss%NBEAM, ss%ap2_halfheight)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "aperture half-height","m", "ap2_halfheight")
  ENDIF

  IF(ALLOCATED(ss%ap2_horiz_offset)) THEN
     CALL ps_namwr(ilun_namel, &
          "horizontal offset, 2nd aperture","m", &
          "ap2_horiz_offset", ss%NBEAM, ss%ap2_horiz_offset)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "horizontal offset, 2nd aperture","m", "ap2_horiz_offset")
  ENDIF

  IF(ALLOCATED(ss%ap2_vert_offset)) THEN
     CALL ps_namwr(ilun_namel, &
          "vertical offset, 2nd aperture","m", &
          "ap2_vert_offset", ss%NBEAM, ss%ap2_vert_offset)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "vertical offset, 2nd aperture","m", "ap2_vert_offset")
  ENDIF

  IF(ALLOCATED(ss%Einj_min)) THEN
     CALL ps_namwr(ilun_namel, &
          "minimum injection energy","keV", &
          "Einj_min", ss%NBEAM, ss%Einj_min)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "minimum injection energy","keV", "Einj_min")
  ENDIF

  IF(ALLOCATED(ss%Einj_max)) THEN
     CALL ps_namwr(ilun_namel, &
          "maximum injection energy","keV", &
          "Einj_max", ss%NBEAM, ss%Einj_max)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "maximum injection energy","keV", "Einj_max")
  ENDIF

  CALL ps_namwr(ilun_namel, &
       "no. pts in H beam energy fractions table","integer", &
       "n_H_einj_standard", ss%n_H_einj_standard)

  CALL ps_namwr(ilun_namel, &
       "no. pts in D beam energy fractions table","integer", &
       "n_D_einj_standard", ss%n_D_einj_standard)

  CALL ps_namwr(ilun_namel, &
       "no. pts in T beam energy fractions table","integer", &
       "n_T_einj_standard", ss%n_T_einj_standard)

  IF(ALLOCATED(ss%H_einj_standard)) THEN
     CALL ps_namwr(ilun_namel, &
          "H beam table energies","keV", &
          "H_einj_standard", ss%N_H_EINJ_STANDARD, ss%H_einj_standard)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "H beam table energies","keV", "H_einj_standard")
  ENDIF

  IF(ALLOCATED(ss%H_ffull_standard)) THEN
     CALL ps_namwr(ilun_namel, &
          "H beam full energy fractions","-", &
          "H_ffull_standard", ss%N_H_EINJ_STANDARD, ss%H_ffull_standard)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "H beam full energy fractions","-", "H_ffull_standard")
  ENDIF

  IF(ALLOCATED(ss%H_fhalf_standard)) THEN
     CALL ps_namwr(ilun_namel, &
          "H beam half energy fractions","-", &
          "H_fhalf_standard", ss%N_H_EINJ_STANDARD, ss%H_fhalf_standard)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "H beam half energy fractions","-", "H_fhalf_standard")
  ENDIF

  IF(ALLOCATED(ss%D_einj_standard)) THEN
     CALL ps_namwr(ilun_namel, &
          "D beam table energies","keV", &
          "D_einj_standard", ss%N_D_EINJ_STANDARD, ss%D_einj_standard)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "D beam table energies","keV", "D_einj_standard")
  ENDIF

  IF(ALLOCATED(ss%D_ffull_standard)) THEN
     CALL ps_namwr(ilun_namel, &
          "D beam full energy fractions","-", &
          "D_ffull_standard", ss%N_D_EINJ_STANDARD, ss%D_ffull_standard)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "D beam full energy fractions","-", "D_ffull_standard")
  ENDIF

  IF(ALLOCATED(ss%D_fhalf_standard)) THEN
     CALL ps_namwr(ilun_namel, &
          "D beam half energy fractions","-", &
          "D_fhalf_standard", ss%N_D_EINJ_STANDARD, ss%D_fhalf_standard)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "D beam half energy fractions","-", "D_fhalf_standard")
  ENDIF

  IF(ALLOCATED(ss%T_einj_standard)) THEN
     CALL ps_namwr(ilun_namel, &
          "T beam table energies","keV", &
          "T_einj_standard", ss%N_T_EINJ_STANDARD, ss%T_einj_standard)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "T beam table energies","keV", "T_einj_standard")
  ENDIF

  IF(ALLOCATED(ss%T_ffull_standard)) THEN
     CALL ps_namwr(ilun_namel, &
          "T beam full energy fractions","-", &
          "T_ffull_standard", ss%N_T_EINJ_STANDARD, ss%T_ffull_standard)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "T beam full energy fractions","-", "T_ffull_standard")
  ENDIF

  IF(ALLOCATED(ss%T_fhalf_standard)) THEN
     CALL ps_namwr(ilun_namel, &
          "T beam half energy fractions","-", &
          "T_fhalf_standard", ss%N_T_EINJ_STANDARD, ss%T_fhalf_standard)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "T beam half energy fractions","-", "T_fhalf_standard")
  ENDIF

  IF(ALLOCATED(ss%icrf_src_name)) THEN
     CALL ps_namwr(ilun_namel, &
          "number & name of ICRF sources","character", &
          "icrf_src_name", ss%NICRF_SRC, ss%icrf_src_name)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "number & name of ICRF sources","character", "icrf_src_name")
  ENDIF

  IF(ALLOCATED(ss%ant_model)) THEN
     CALL ps_namwr(ilun_namel, &
          "antenna model filenames (1 per antenna source)","character", &
          "ant_model", ss%NICRF_SRC, ss%ant_model)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "antenna model filenames (1 per antenna source)","character", "ant_model")
  ENDIF

  IF(ALLOCATED(ss%nrz_antgeo)) THEN
     CALL ps_namwr(ilun_namel, &
          "number of (R,Z) points, antenna geometries","integer", &
          "nrz_antgeo", ss%NICRF_SRC, ss%nrz_antgeo)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "number of (R,Z) points, antenna geometries","integer", "nrz_antgeo")
  ENDIF

  CALL ps_namwr(ilun_namel, &
       "Maximum size of variable length enumeration: NRZ_ANTGEO","integer", &
       "max_nrz_antgeo", ss%max_nrz_antgeo)

  IF(ALLOCATED(ss%R_antgeo)) THEN
     ALLOCATE(isize_vec(ss%NICRF_SRC))
     isize_vec = ss%NRZ_ANTGEO
     CALL ps_namwr(ilun_namel, &
          "antenna geo: R pts","m", &
          "R_antgeo", isize_vec, ss%R_antgeo)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "antenna geo: R pts","m", "R_antgeo")
  ENDIF

  IF(ALLOCATED(ss%Z_antgeo)) THEN
     ALLOCATE(isize_vec(ss%NICRF_SRC))
     isize_vec = ss%NRZ_ANTGEO
     CALL ps_namwr(ilun_namel, &
          "antenna geo: Z pts","m", &
          "Z_antgeo", isize_vec, ss%Z_antgeo)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "antenna geo: Z pts","m", "Z_antgeo")
  ENDIF

  IF(ALLOCATED(ss%dx_fshield)) THEN
     CALL ps_namwr(ilun_namel, &
          "distance, antenna to Faraday shield","m", &
          "dx_fshield", ss%NICRF_SRC, ss%dx_fshield)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "distance, antenna to Faraday shield","m", "dx_fshield")
  ENDIF

  IF(ALLOCATED(ss%ecrf_src_name)) THEN
     CALL ps_namwr(ilun_namel, &
          "number & name of ECRF sources","character", &
          "ecrf_src_name", ss%NECRF_SRC, ss%ecrf_src_name)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "number & name of ECRF sources","character", "ecrf_src_name")
  ENDIF

  IF(ALLOCATED(ss%R_EC_launch)) THEN
     CALL ps_namwr(ilun_namel, &
          "R of center of launcher","m", &
          "R_EC_launch", ss%NECRF_SRC, ss%R_EC_launch)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "R of center of launcher","m", "R_EC_launch")
  ENDIF

  IF(ALLOCATED(ss%Z_EC_launch)) THEN
     CALL ps_namwr(ilun_namel, &
          "Z of center of launcher","m", &
          "Z_EC_launch", ss%NECRF_SRC, ss%Z_EC_launch)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "Z of center of launcher","m", "Z_EC_launch")
  ENDIF

  IF(ALLOCATED(ss%Phi_EC_launch)) THEN
     CALL ps_namwr(ilun_namel, &
          "Phi of center of launcher","degrees", &
          "Phi_EC_launch", ss%NECRF_SRC, ss%Phi_EC_launch)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "Phi of center of launcher","degrees", "Phi_EC_launch")
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR)) THEN
     CALL ps_namwr(ilun_namel, &
          "principal beam width in horizontal direction","m", &
          "EC_PRINC_WIDTH_HOR", ss%NECRF_SRC, ss%EC_PRINC_WIDTH_HOR)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "principal beam width in horizontal direction","m", "EC_PRINC_WIDTH_HOR")
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT)) THEN
     CALL ps_namwr(ilun_namel, &
          "principal beam width in vertical direction","m", &
          "EC_PRINC_WIDTH_VERT", ss%NECRF_SRC, ss%EC_PRINC_WIDTH_VERT)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "principal beam width in vertical direction","m", "EC_PRINC_WIDTH_VERT")
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_CURV_HOR)) THEN
     CALL ps_namwr(ilun_namel, &
          "principal curvature in horizontal direction","m", &
          "EC_PRINC_CURV_HOR", ss%NECRF_SRC, ss%EC_PRINC_CURV_HOR)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "principal curvature in horizontal direction","m", "EC_PRINC_CURV_HOR")
  ENDIF

  IF(ALLOCATED(ss%EC_PRINC_CURV_VERT)) THEN
     CALL ps_namwr(ilun_namel, &
          "principal curvature in vertical direction","m", &
          "EC_PRINC_CURV_VERT", ss%NECRF_SRC, ss%EC_PRINC_CURV_VERT)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "principal curvature in vertical direction","m", "EC_PRINC_CURV_VERT")
  ENDIF

  IF(ALLOCATED(ss%EC_Half_Power_Angle)) THEN
     CALL ps_namwr(ilun_namel, &
          "Divergence of EC beam","degrees", &
          "EC_Half_Power_Angle", ss%NECRF_SRC, ss%EC_Half_Power_Angle)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "Divergence of EC beam","degrees", "EC_Half_Power_Angle")
  ENDIF

  IF(ALLOCATED(ss%EC_Beam_Elongation)) THEN
     CALL ps_namwr(ilun_namel, &
          "EC beam ellipticity","-", &
          "EC_Beam_Elongation", ss%NECRF_SRC, ss%EC_Beam_Elongation)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "EC beam ellipticity","-", "EC_Beam_Elongation")
  ENDIF

  IF(ALLOCATED(ss%lhrf_src_name)) THEN
     CALL ps_namwr(ilun_namel, &
          "number & name of LHRF sources","character", &
          "lhrf_src_name", ss%NLHRF_SRC, ss%lhrf_src_name)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "number & name of LHRF sources","character", "lhrf_src_name")
  ENDIF

  CALL ps_namwr(ilun_namel, &
       "number of TF coils","integer", &
       "ntf_coils", ss%ntf_coils)

  CALL ps_namwr(ilun_namel, &
       "number of TF coils involved in inducing 2nd component","integer", &
       "ntf_coil2", ss%ntf_coil2)


  WRITE(ilun_namel,'(" /")')
  CLOSE(UNIT=ilun_namel)

END SUBROUTINE ps_mdescr_namelist_write
