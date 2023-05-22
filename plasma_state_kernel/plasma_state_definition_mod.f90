! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


MODULE plasma_state_definition_mod

#include "ps_container_size.h"

  USE plasma_state_constants_mod
  USE plasma_state_util_mod

  ! plasma_state header:

  use swim_global_data_mod, only: rspec, swim_err_out_file, swim_error
  implicit NONE


  !--------------------------------------------
  !
  !  PLASMA STATE DATA TYPE
  !
  !--------------------------------------------

  !  assumed size (#integer words) of a plasma_state_container instance...
  !  Note: use MACRO defined in #include file!

  INTEGER, PARAMETER :: ps_container_size = __PS_CONTAINER_SIZE

  TYPE :: plasma_state_container

     TYPE (plasma_state), pointer :: ps_ptr => NULL()

  END TYPE plasma_state_container

  TYPE :: plasma_state

     ! version of the software (i.e. that wrote state data to a file)

     CHARACTER*32 :: version_ID

     ! ---------------------------------------
     ! internal tags for use of software that manages collections of
     ! plasma states.  These quantities are not read from or written to
     ! disk files not copied between state objects!  these must be
     ! maintained by the module or software which instantiates state
     ! objects.

     INTEGER :: itag = ps_unInit
     CHARACTER*32 :: chtag = ' '

     ! this is an opaque container for an interpolation object-- a
     ! separate such object to be associated with each state instance.
     !  Note: use MACRO defined in #include file!

     INTEGER :: iobj(ps_container_size) = __PS_CONTAINER_INIT

     ! as of Jan. 2008 "xplasma" provides the interpolation capability
     ! but in principle this could be replaced.  This object will imbed
     ! a pointer, and it cannot be safely copied, nor read from nor written
     ! to disk files!  See "plasma_state_mod.f90" for usage.

     ! ---------------------------------------
     ! zone weight mapping index
     ! if ID_ZONWT(id) is ZERO, the weighting is just volume or area
     ! depending on the profile type; if it is non-zero, it is an ID
     ! of a profile over the same grid that is to be used for weighting
     ! (for example, when Temperatures are rezoned a density weighting
     ! would be used; ID_ZONWT(id) will point to the correct density profile).
     ! This is updated when the state is updated or read from file.

     INTEGER, dimension(:), allocatable :: ID_ZONWT

     ! ---------------------------
     ! hash codes
     ! These are used to detect that a profile, grid, or set of variables
     ! contains changes (e.g. when just read in from a file).

     INTEGER, dimension(ps_hash_total) :: hash_codes      ! file copy
     INTEGER, dimension(ps_hash_total) :: new_hash_codes  ! updated..
     INTEGER, dimension(ps_hash_total) :: saved_hash_codes  ! saved..

     ! ---------------------------
     ! count of active dimensions in each component

     INTEGER, dimension(ps_ccount) :: active_dims

     ! --------------------------------------------------------------------


     !===============================================
     ! Section: MACHINE_DESCRIPTION

     !----------------------------
     ! Component: ANOM

     !----------------------------
     ! Component: EC

     ! ** Item List Dimension **

     INTEGER :: necrf_src = 0
       !item list dimension of ecrf_src_name (ecrf_source)
       !L|ecrf_source   ecrf_src_name(necrf_src)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: ecrf_src_name
       !number & name of ECRF sources
       !L|ecrf_source   ecrf_src_name(necrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: EC_Beam_Elongation
       !EC beam ellipticity
       !The conical EC beam shape can be elongated
       !"vertically" (elongation > 1) or "horizontally"
       !(elongation < 1).  Here "vertical" means in the
       !direction normal to the plane contaiing the beam
       !centerline and the +phi unit vector; "horizontal"
       !means normal to the plane of the beam centerline
       !and the "vertical" direction.  If this quantity is
       !defaulted to zero, it is reset to 1.0 exactly, i.e.
       !for a beam with circular cross section.
       !R|units=-   EC_Beam_Elongation(necrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: EC_Half_Power_Angle
       !Divergence of EC beam
       !beam at half-power, this half-angle away from
       !direction of beam centerline which is started
       !at the launcher (treated as a point source);
       !See EC_theta_aim(necrf_src) & EC_phi_aim(necrf_src)
       !in the shot configuration section.
       !R|units=degrees  EC_Half_Power_Angle(necrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: EC_PRINC_CURV_HOR
       !principal curvature in horizontal direction
       !R|units=m  EC_PRINC_CURV_HOR(necrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: EC_PRINC_CURV_VERT
       !principal curvature in vertical direction
       !R|units=m  EC_PRINC_CURV_VERT(necrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: EC_PRINC_WIDTH_HOR
       !principal beam width in horizontal direction
       !R|units=m  EC_PRINC_WIDTH_HOR(necrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: EC_PRINC_WIDTH_VERT
       !principal beam width in vertical direction
       !R|units=m  EC_PRINC_WIDTH_VERT(necrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Phi_EC_launch
       !Phi of center of launcher
       !this may be left at zero for axisymmetric simulations.
       !R|units=degrees  Phi_EC_launch(necrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: R_EC_launch
       !R of center of launcher
       !R|units=m   R_EC_launch(necrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Z_EC_launch
       !Z of center of launcher
       !R|units=m   Z_EC_launch(necrf_src)

     !----------------------------
     ! Component: EQ

     ! ** Item List Dimension **

     INTEGER :: ncircuits = 0
       !item list dimension of circuit_name (pf_circuits)
       !L|pf_circuits  circuit_name(ncircuits)

     ! ** Item List Dimension **

     INTEGER :: ncoils = 0
       !item list dimension of coil_name (pf_coils)
       !L|pf_coils     coil_name(ncoils)

     ! ** (Ad hoc) Array Dimension **

     INTEGER :: num_rzlim = 0
       !number of points, axisymmetric (R,Z) limiter
       !dimension for rlim & zlim -- closed axisymmetric piecewise linear
       !contour describing axisymmetric limiter or vacuum vessel wall
       !expect rlim(1)=rlim(num_rzlim) and zlim(1)=zlim(num_rzlim)
       !I|ENUM  num_rzlim

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Ang_Href_coil
       !angle w.r.t. horizontal of coil base
       !zero means, base is alligned horizontally
       !+10 (degrees) means, tilted up to the right;
       !-10 (degrees) means, tilted down to the right
       !R|units=degrees Ang_Href_coil(ncoils)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Ang_Vref_coil
       !angle w.r.t. vertical of coil sides
       !zero means, walls are alligned vertically
       !+10 (degrees) means, tilted up and to left 10 degrees from vertical
       !-10 (degrees) means, tilted up and to right, 10 degrees from vertical
       !R|units=degrees Ang_Vref_coil(ncoils)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: circuit_name
       !Number & names of circuits
       !connecting coils affecting poloidal field-- axisymmetric coils,
       !or eddy currents represented as cancelling coil pairs
       !L|pf_circuits  circuit_name(ncircuits)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: coil_in_circuit
       !circuit to which each coil belongs
       !(each must exactly match a name in the circuit_name(...) list).
       !N  coil_in_circuit(ncoils)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: coil_name
       !Number & name of axisymmetric coils
       !L|pf_coils     coil_name(ncoils)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: coil_resispt
       !coil resistance per turn
       !total resistance (ohms) of coil j: abs(Nturns(j))*coil_resispt(j)
       !R|units=ohms/turn  coil_resispt(ncoils)

     CHARACTER*256 :: geometry = " "
       !path to file(s) vac. vessel & coil descr.
       !F  geometry

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Hsize_coil
       !(Rotatable) Horizontal coil base size
       !R|units=m  Hsize_coil(ncoils)

     INTEGER, DIMENSION(:), ALLOCATABLE :: Nturns
       !SIGNED number of conductor turns wrapped in each coil
       !+ means, a positive current flows counter-clockwise viewed from above;
       !- means, a positive current flows clockwise viewed from above;
       !I  Nturns(ncoils)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rlim
       !R points in closed (R,Z) contour sequence
       !R|units=m  rlim(num_rzlim)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Rloc_coil
       !R location of coil, lower left corner
       !R|units=m  Rloc_coil(ncoils)

     REAL(KIND=rspec) :: R_max_box = 0.0_rspec
       !R_max of bounding box
       !R  R_max_box

     REAL(KIND=rspec) :: R_min_box = 0.0_rspec
       !R_min of bounding box
       !R  R_min_box

     INTEGER, DIMENSION(:), ALLOCATABLE :: VRep_coil_count
       !replication count, vertical coil stack
       !default: 1
       !I  VRep_coil_count(ncoils)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Vrep_coil_spacing
       !replicated coil separation
       !(only meaningful for coils {k} with VRep_coil_count(k) > 1).
       !R|units=m Vrep_coil_spacing(ncoils)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Vsize_coil
       !(Rotatable) Vertical coil size
       !R|units=m  Vsize_coil(ncoils)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: zlim
       !Z points in closed (R,Z) contour sequence
       !R|units=m  zlim(num_rzlim)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Zloc_coil
       !Z location of coil, lower left corner
       !R|units=m  Zloc_coil(ncoils)

     REAL(KIND=rspec) :: Z_max_box = 0.0_rspec
       !Z_max of bounding box
       !R  Z_max_box

     REAL(KIND=rspec) :: Z_min_box = 0.0_rspec
       !Z_min of bounding box
       !R  Z_min_box

     !----------------------------
     ! Component: FUS

     !----------------------------
     ! Component: GAS

     !----------------------------
     ! Component: IC

     ! ** (Ad hoc) Array Dimension **

     INTEGER :: max_nrz_antgeo = 0
       !Maximum size of variable length enumeration: NRZ_ANTGEO
       !(derived from NRZ_ANTGEO)

     ! ** Item List Dimension **

     INTEGER :: nicrf_src = 0
       !item list dimension of icrf_src_name (icrf_source)
       !L|icrf_source   icrf_src_name(nicrf_src)

     CHARACTER*256, DIMENSION(:), ALLOCATABLE :: ant_model
       !antenna model filenames (1 per antenna source)
       !F ant_model(nicrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: dx_fshield
       !distance, antenna to Faraday shield
       !R|units=m dx_fshield(nicrf_src)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: icrf_src_name
       !number & name of ICRF sources
       !L|icrf_source   icrf_src_name(nicrf_src)

     INTEGER, DIMENSION(:), ALLOCATABLE :: nrz_antgeo
       !number of (R,Z) points, antenna geometries
       !I|ENUM nrz_antgeo(nicrf_src)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: R_antgeo
       !antenna geo: R pts
       !R|units=m R_antgeo(nrz_antgeo(),nicrf_src)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: Z_antgeo
       !antenna geo: Z pts
       !R|units=m Z_antgeo(nrz_antgeo(),nicrf_src)

     !----------------------------
     ! Component: LH

     ! ** Item List Dimension **

     INTEGER :: nlhrf_src = 0
       !item list dimension of lhrf_src_name (lhrf_source)
       !L|lhrf_source   lhrf_src_name(nlhrf_src)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: lhrf_src_name
       !number & name of LHRF sources
       !L|lhrf_source   lhrf_src_name(nlhrf_src)

     !----------------------------
     ! Component: LMHD

     !----------------------------
     ! Component: NBI

     ! ** Item List Dimension **

     INTEGER :: nbeam = 0
       !item list dimension of nbi_src_name (neutral_beams)
       !L|neutral_beams nbi_src_name(nbeam)

     ! ** (Ad hoc) Array Dimension **

     INTEGER :: n_D_einj_standard = 0
       !no. pts in D beam energy fractions table
       !I|ENUM n_D_einj_standard

     ! ** (Ad hoc) Array Dimension **

     INTEGER :: n_H_einj_standard = 0
       !no. pts in H beam energy fractions table
       !I|ENUM n_H_einj_standard

     ! ** (Ad hoc) Array Dimension **

     INTEGER :: n_T_einj_standard = 0
       !no. pts in T beam energy fractions table
       !I|ENUM n_T_einj_standard

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: ap2_halfheight
       !aperture half-height
       !for nbap_shape(j)="rectangle", this is the vertical size
       !specification; this quantity ignored for nbap_shape(j)="circle", or
       !if there is no 2nd aperture.
       !R|units=m         ap2_halfheight(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: ap2_halfwidth
       !2nd aperture half-width
       !for nbap_shape(j)="rectangle", this is the horizontal size
       !specification; for nbap_shape(j)="circle", ap_halfwidth(j) is
       !the radius; ignored if there is no 2nd aperture.
       !R|units=m         ap2_halfwidth(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: ap2_horiz_offset
       !horizontal offset, 2nd aperture
       !opening relative to beam centerline; positive value means shift of
       !aperture away from machine axis i.e. to make a beam with a larger
       !tangency radius
       !R|units=m         ap2_horiz_offset(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: ap2_vert_offset
       !vertical offset, 2nd aperture
       !opening relative to beam centerline: positive value means the
       !aperture opening is shifted upward relative to the beam centerline
       !R|units=m         ap2_vert_offset(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: ap_halfheight
       !aperture half-height
       !for nbap_shape(j)="rectangle", this is the vertical size
       !specification; this quantity ignored for nbap_shape(j)="circle".
       !R|units=m         ap_halfheight(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: ap_halfwidth
       !aperture half-width
       !for nbap_shape(j)="rectangle", this is the horizontal size
       !specification; for nbap_shape(j)="circle", ap_halfwidth(j) is
       !the radius.
       !R|units=m         ap_halfwidth(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: ap_horiz_offset
       !horizontal offset of aperture
       !opening relative to beam centerline; positive value means shift of
       !aperture away from machine axis i.e. to make a beam with a larger
       !tangency radius
       !R|units=m         ap_horiz_offset(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: ap_vert_offset
       !vertical offset of aperture
       !opening relative to beam centerline: positive value means the
       !aperture opening is shifted upward relative to the beam centerline
       !R|units=m         ap_vert_offset(nbeam)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: beam_type
       !beam type:
       !"standard", H/D/T beam using standard full-half-third energy
       !injection fractions; energy dependence of standard beam energy
       !fractions are defined as part of the machine description
       !"negative_ion", H/D/T beam 100% injected at full energy
       !"Helium" for Helium or He3 beam, energy fraction is 100% full energy.
       !"Impurity" for injection of one of: {Ne,Ar,Xe,Kr}: 100% full energy.
       !N     beam_type(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: b_halfHeight
       !beam half-height at source
       !If nbshape(j)="Rectangle" this is the vertical half-height of the
       !beam at its source grid.  This quantity ignored for
       !nbshape(j)="circle".
       !R|units=m       b_halfHeight(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: b_halfwidth
       !beam half-width at source
       !If nbshape(j)="Rectangle" this is the horizontal half-width of
       !the source rectangle for beam j; if nbshape(j)="circle" it is
       !the radius of the circular source for beam j, in meters.
       !R|units=m       b_halfwidth(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: b_Hdivergence
       !horizontal divergence (D) of beam
       !for nbshape(j)="circle" sources this is the ONLY divergence.
       !--> definition of D (b_Hdivergence(...) given in DEGREES):
       !for alpha the random angle of deviation from the direction of
       !aiming, P(alpha) ~ exp(-alpha**2/D**2); P(D) = (1/e)*P(0)
       !I.e. D is the angle of deviation from the aiming angle, in
       !either direction, for a (1/e) reduction in probability of
       !launch of a beam particle.
       !R|units=degrees   b_Hdivergence(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: b_Hfocal_length
       !horizontal focal length
       !for nbshape(j)="circle" sources this is the only focal length.
       !R|units=m       b_Hfocal_length(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: b_Vdivergence
       !vertical divergence (D) of beam
       !used for nbshape(j)="Rectangle" sources ONLY.
       !--> definition of D (b_Vdivergence(...) given in DEGREES):
       !for alpha the random angle of deviation from the direction of
       !aiming, P(alpha) ~ exp(-alpha**2/D**2); P(D) = (1/e)*P(0)
       !I.e. D is the angle of deviation from the aiming angle, in
       !either direction, for a (1/e) reduction in probability of
       !launch of a beam particle.
       !R|units=degrees   b_Vdivergence(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: b_Vfocal_length
       !vertical focal length
       !used for nbshape(j)="Rectangle" sources only.
       !R|units=m       b_Vfocal_length(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: D_einj_standard
       !D beam table energies
       !for full-half-third energy beam current fractions data
       !R|units=keV D_einj_standard(n_D_einj_standard)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: D_ffull_standard
       !D beam full energy fractions
       !R|units=- D_ffull_standard(n_D_einj_standard)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: D_fhalf_standard
       !D beam half energy fractions
       !R|units=- D_fhalf_standard(n_D_einj_standard)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Einj_max
       !maximum injection energy
       !maximum energy of the full energy fractional component of each beam
       !Some codes use 1.5*maxval(Einj_max) as an upper limit on a lab frame
       !energy grid for beam ions slowing down distribution functions.
       !R|units=keV       Einj_max(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Einj_min
       !minimum injection energy
       !minimum energy of the full energy fractional component of each beam
       !R|units=keV       Einj_min(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: H_einj_standard
       !H beam table energies
       !for full-half-third energy beam current fractions data
       !R|units=keV H_einj_standard(n_H_einj_standard)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: H_ffull_standard
       !H beam full energy fractions
       !R|units=- H_ffull_standard(n_H_einj_standard)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: H_fhalf_standard
       !H beam half energy fractions
       !R|units=- H_fhalf_standard(n_H_einj_standard)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Lbscap
       !distance, source to aperture
       !Distance of beam centerline from source to aperture into vacuum vessel
       !R|units=m       Lbscap(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Lbscap2
       !distance, source to 2nd aperture
       !Distance of beam centerline from source to center of 2nd aperture
       !into vacuum vessel.
       !R|units=m       Lbscap2(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Lbsctan
       !distance, source to tangency point
       !This is the length of the centerline from the center of the neutral
       !beam's original ion source to the centerline tangency point, |sRtcen|
       !from the machine axis.  The length is given in meters.
       !R|units=m       Lbsctan(nbeam)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: nbap2_shape
       !keyword for optional 2nd aperture
       !For beam j, nbap2_shape= blank or 'None': no 2nd aperture
       !Other possible values:  "Rectangle"; "Circle".
       !The beam centerline is assumed to pass through the center of the
       !aperture.
       !N     nbap2_shape(nbeam)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: nbap_shape
       !shape keyword for beam aperture
       !For beam j, nbap_shape(j)="Rectangle" indicates a rectangular
       !aperture; nbap_shape(j)="Circle" indicates a circular aperture.
       !The beam centerline is assumed to pass through the center of the
       !aperture.
       !N     nbap_shape(nbeam)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: nbi_src_name
       !number & name of neutral beams
       !L|neutral_beams nbi_src_name(nbeam)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: nbshape
       !shape keyword for beam source grid
       !For beam j, nbshape(j)="Rectangle" indicates a rectangular shaped
       !source; nbshape(j)="Circle" indicates a circular shaped source.
       !(The keyword must be spelled correctly but upper or lower case
       !letters may be used interchangeably).  The plane of
       !the source is assumed perpendicular to the beam centerline.
       !N     nbshape(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Phibsc
       !toroidal location of beam source
       !Angle of location of beam centerline at source, with respect to some
       !reference Phi=0 vertical plane.  Phi is assumed to increase in the
       !counter-clockwise direction of the torus when viewed from above, i.e.
       !(R,phi,Z) form a right handed coordinate system.  The setting of
       !Phibsc does not affect beam heating in axisymmetric simulations.
       !R|units=degrees       Phibsc(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: sRtcen
       !signed tangency radius
       !This is the major radius of tangency of the neutral beam centerlines--
       !i.e. the distance of closest approach of each beam centerline to the
       !machine axis, in meters.
       !Sign convention: + for beams which impart angular momentum in the
       !counter-clockwise direction around the torus as viewed from above.
       !R|units=m       sRtcen(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: T_einj_standard
       !T beam table energies
       !for full-half-third energy beam current fractions data
       !R|units=keV T_einj_standard(n_T_einj_standard)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: T_ffull_standard
       !T beam full energy fractions
       !R|units=- T_ffull_standard(n_T_einj_standard)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: T_fhalf_standard
       !T beam half energy fractions
       !R|units=- T_fhalf_standard(n_T_einj_standard)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Zbap
       !elevation of beam centerline at aperture
       !At a distance (Lbscap) from the beam source, the beam centerline
       !passes above (+) or below (-) machine midplane as defined by Z=0
       !in the equilibrium specification.  If defaulted, the centerline
       !is centered on the midplane (Z=0) when it crosses the plane of the
       !aperture.  If both Zbap(...) and Zbsc(...) are defaulted, the
       !entire beam centerline lies in the midplane.
       !Note that Z at the beam centerline tangency radius is:
       !Ztan = Zbsc + (Lbsctan/Lbscap)*(Zbap-Zbsc)
       !R|units=m       Zbap(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Zbsc
       !elevation of beam source
       !Elevation of beam centerline at source (+ above) (- below)
       !machine midplane as defined by Z=0 in the equilibrium specification
       !(the default, 0.0, centers beam source in the machine midplane).
       !R|units=m       Zbsc(nbeam)

     !----------------------------
     ! Component: PEL

     !----------------------------
     ! Component: PLASMA

     CHARACTER*32 :: tokamak_id = " "
       !Tokamak (Machine Description) ID
       !<tokid>_<revid> within a 32 character string.
       !N  tokamak_id

     !----------------------------
     ! Component: RAD

     !----------------------------
     ! Component: RIPPLE

     INTEGER :: ntf_coil2 = ps_unInit
       !number of TF coils involved in inducing 2nd component
       !of TF ripple field (typically ntf_coil2 = ntf_coils/2).
       !(This 2nd TF ripple field is sometimes introduced in
       !tokamak experiments to study the effects of TF ripple
       !induced fast ion loss).
       !I ntf_coil2

     INTEGER :: ntf_coils = ps_unInit
       !number of TF coils
       !the number of toroidal field (TF) coils is also the
       !number of periods in the main TF ripple field
       !I ntf_coils

     !----------------------------
     ! Component: RUNAWAY

     !===============================================
     ! Section: SHOT_CONFIGURATION

     !----------------------------
     ! Component: ANOM

     !----------------------------
     ! Component: EC

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: EC_Omode_Fraction
       !O-mode fraction of EC source
       !Power(O-mode)/(Power(O-mode)+Power(X-mode))
       !R|units=-   EC_Omode_Fraction(necrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: EC_phi_aim
       !toroidal aiming angle
       !0 degrees-> component of beam in +R direction
       !90 degrees-> component of beam in +phi direction
       !180 degrees-> component of beam in -R direction
       !270 degrees or -90 degrees-> component in -phi direction
       !An EC beam with a component in the +phi direction
       !will push electrons in the +phi direction, counter
       !clockwise (CCW) around the machine axis as viewed
       !from above; and hence drive current in a clockwise
       !(CW) direction.  See shot configuration variable
       !kCCW_Jphi for orientation of main plasma current.
       !See also EC_theta_aim(...).
       !R|units=degrees  EC_phi_aim(necrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: EC_theta_aim
       !poloidal aiming angle
       != angle from +Z direction:
       !0 degrees-> straight up;
       !+/-  45 degrees-> on a cone in an upward direction
       !+/-  90 degrees-> in a horizontal direction...
       !+/- 135 degrees-> on a cone in a downward direction
       !+/- 180 degrees-> straight down
       !(for values other than 0,+/- 180, see alse EC_phi_aim).
       !R|units=degrees  EC_theta_aim(necrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: freq_ec
       !frequency of EC source
       !R|units=Hz freq_ec(necrf_src)

     !----------------------------
     ! Component: EQ

     INTEGER :: kccw_Bphi = 0
       !B_phi orientation: +1 means CCW viewed from above
       !-1 means clockwise (CW) viewed from above.
       !I  kccw_Bphi = 0

     INTEGER :: kccw_Jphi = 0
       !J_phi orientation: +1 means CCW viewed from above
       !I  kccw_Jphi = 0

     !----------------------------
     ! Component: FUS

     ! ** Species List Dimension **

     INTEGER :: nspec_fusion = 0
       !species list dimension of SFUS (fusion_ion)
       !S|fusion_ion   SFUS(nspec_fusion)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: m_SFUS
       !SFUS specie mass
       !S|fusion_ion   SFUS(nspec_fusion)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qatom_SFUS
       !SFUS atomic number
       !S|fusion_ion   SFUS(nspec_fusion)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: q_SFUS
       !SFUS specie charge
       !S|fusion_ion   SFUS(nspec_fusion)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: SFUS_name
       !fusion product ion species
       !S|fusion_ion   SFUS(nspec_fusion)

     INTEGER, DIMENSION(:), ALLOCATABLE :: SFUS_type
       !SFUS specie types
       !S|fusion_ion   SFUS(nspec_fusion)

     !----------------------------
     ! Component: GAS

     ! ** Item List Dimension **

     INTEGER :: ngsc0 = 0
       !item list dimension of gs_name (gas_source)
       !L|gas_source    gs_name(ngsc0)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: gas_atom
       !name of species introduced by each gas source
       !Possible values are: "H", "D", "T", "He3", and "He4" at present.
       !Each must correspond to an actual species in the SGAS list.
       !N  gas_atom(ngsc0)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: gs_name
       !number & name of edge neutral gas sources
       !L|gas_source    gs_name(ngsc0)

     INTEGER, DIMENSION(:), ALLOCATABLE :: is_recycling
       !0/1; =1 to for recycling sources
       !i.e. sources proportional to ion outfluxes
       !I  is_recycling(ngsc0)

     !----------------------------
     ! Component: IC

     ! ** (Ad hoc) Array Dimension **

     INTEGER :: max_num_nphi = 0
       !Maximum size of variable length enumeration: NUM_NPHI
       !(derived from NUM_NPHI)

     ! ** (Ad hoc) Array Dimension **

     INTEGER :: max_num_nphi_vac = 0
       !Maximum size of variable length enumeration: NUM_NPHI_VAC
       !(derived from NUM_NPHI_VAC)

     ! ** (Ad hoc) Array Dimension **

     INTEGER :: max_n_straps = 0
       !Maximum size of variable length enumeration: N_STRAPS
       !(derived from N_STRAPS)

     ! ** Species List Dimension **

     INTEGER :: nspec_rfmin = 0
       !species list dimension of RFMIN (RF_minority)
       !S|RF_minority  RFMIN(nspec_rfmin)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: freq_ic
       !frequency on each ICRF source
       !R|units=Hz freq_ic(nicrf_src)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: Imag_ant_coef
       !imag part of Fourier Coef
       !R|units=Amps/m Imag_ant_coef(num_nphi_vac(),nicrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: m_RFMIN
       !RFMIN specie mass
       !S|RF_minority  RFMIN(nspec_rfmin)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: nphi
       !n_phi wave spectrum from antenna
       !I nphi(num_nphi(),nicrf_src)

     INTEGER, DIMENSION(:), ALLOCATABLE :: num_nphi
       !number of non-zero n_phi values
       !counting both positive and negative
       !n_phi values
       !I|ENUM:nphi  num_nphi(nicrf_src)

     INTEGER, DIMENSION(:), ALLOCATABLE :: num_nphi_vac
       !number of non-zero n_phi values
       !counting both positive and negative
       !n_phi values for the retained vacuum
       !spectrum--will typicall be a few hundred
       !or less and is a super set of num_phi
       !I|ENUM num_nphi_vac(nicrf_src)

     INTEGER, DIMENSION(:), ALLOCATABLE :: n_straps
       !number of straps in the antenna
       !I|ENUM n_straps(nicrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qatom_RFMIN
       !RFMIN atomic number
       !S|RF_minority  RFMIN(nspec_rfmin)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: q_RFMIN
       !RFMIN specie charge
       !S|RF_minority  RFMIN(nspec_rfmin)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: Real_ant_coef
       !real part of Fourier Coef
       !R|units=Amps/m Real_ant_coef(num_nphi_vac(),nicrf_src)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: RFMIN_name
       !ICRF minority species
       !S|RF_minority  RFMIN(nspec_rfmin)

     INTEGER, DIMENSION(:), ALLOCATABLE :: RFMIN_type
       !RFMIN specie types
       !S|RF_minority  RFMIN(nspec_rfmin)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: R_ant
       !major radius of antenna
       !R|units=m R_ant(nicrf_src)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: wt_nphi
       !vacuum spectrum n_phi weight
       !power_ic(iant)*wt_nphi(ind,iant)
       !gives "vacuum" power @nphi(ind,iant)
       !"ind" covers range 1:num_nphi(iant).
       !see also wt_nphi_abs(...)
       !R|units=- wt_nphi(num_nphi(),nicrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Z_ant
       !height of antenna
       !R|units=m Z_ant(nicrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Z_mid_ant
       !center of antenna relative to TF coil midplane
       !machine midplane as defined by Z=0 in the equilibrium specification
       !same reference as used for beams
       !default of 0.0 could be used, but ant. is often offset
       !R|units=m Z_mid_ant(nicrf_src)

     !----------------------------
     ! Component: LH

     ! ** (Ad hoc) Array Dimension **

     INTEGER :: max_nspectrumlh_src = 0
       !Maximum size of variable length enumeration: NSPECTRUMLH_SRC
       !(derived from NSPECTRUMLH_SRC)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: amaxlh
       !width of the spectrum peak
       !R|units=- amaxlh(nspectrumlh_src(),nlhrf_src)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: aminlh
       !parallel mode number, center of the spectrum peak
       !R|units=- aminlh(nspectrumlh_src(),nlhrf_src)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: apolmaxlh
       !poloidal mode number right of lumps
       !R|units=- apolmaxlh(nspectrumlh_src(),nlhrf_src)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: apolminlh
       !poloidal mode number left of lumps
       !R|units=- apolminlh(nspectrumlh_src(),nlhrf_src)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: atormaxlh
       !toroidal mode number right of lumps
       !R|units=- atormaxlh(nspectrumlh_src(),nlhrf_src)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: atorminlh
       !toroidal mode number left of lumps
       !R|units=- atorminlh(nspectrumlh_src(),nlhrf_src)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: fracpslh
       !fraction of power on each peak (the sum over all peaks must be 1)
       !R|units=- fracpslh(nspectrumlh_src(),nlhrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: freq_lh
       !frequency on each LHRF source
       !R|units=Hz freq_lh(nlhrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: hegrilh
       !need description
       !R|units=- hegrilh(nlhrf_src)

     INTEGER :: i_npoloidal = ps_unInit
       !need description
       !I i_npoloidal

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: nnkparlh
       !need description
       !I nnkparlh(nspectrumlh_src(),nlhrf_src)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: nnkpollh
       !need description
       !I nnkpollh(nspectrumlh_src(),nlhrf_src)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: nnktorlh
       !need description
       !I nnktorlh(nspectrumlh_src(),nlhrf_src)

     INTEGER, DIMENSION(:), ALLOCATABLE :: nspectrumlh_src
       !number of spectrum peaks
       !I|ENUM nspectrumlh_src(nlhrf_src)

     INTEGER, DIMENSION(:), ALLOCATABLE :: nthinlh
       !need description
       !I nthinlh(nlhrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rhopsi0lh
       !need description
       !R|units=- rhopsi0lh(nlhrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: thegrilh
       !need description
       !R|units=- thegrilh(nlhrf_src)

     !----------------------------
     ! Component: LMHD

     !----------------------------
     ! Component: NBI

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: nbion
       !name of species injected by each beam.
       !Possible values are: "H", "D", "T", "He3", and "He4" at present.
       !DMC Apr 2010: added: "Ne", "Ar", "Kr", "Xe"
       !Each beam can inject one species.  Multiple beams (or usually
       !all beams) can inject the same species.
       !the SNBI species list will be constructed from nbion elements;
       !in many experiments this will consist only of the single element "D".
       !N  nbion(nbeam)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: nbion_trace
       !(if non-blank) beam trace element
       !Possible values same as nbion; nbion(ib).ne.nbion_trace(ib) enforced.
       !This allows a second trace element to be injected with any neutral
       !beam-- as in trace tritium beam experiments, or, heating beams doped
       !with a noble gas species.  See: power_nbi_trace(...)
       !N  nbion_trace(nbeam)

     !----------------------------
     ! Component: PEL

     !----------------------------
     ! Component: PLASMA

     ! ** Species List Dimension **

     INTEGER :: nspec_th = 0
       !species list dimension of S (thermal_specie)
       !S|thermal_specie  S(0:nspec_th)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: m_S
       !S specie mass
       !S|thermal_specie  S(0:nspec_th)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qatom_S
       !S atomic number
       !S|thermal_specie  S(0:nspec_th)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: q_S
       !S specie charge
       !S|thermal_specie  S(0:nspec_th)

     INTEGER :: shot_number = ps_unInit
       !integer shot number
       !I shot_number

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: S_name
       !thermal species list
       !index 0 for electrons
       !S|thermal_specie  S(0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: S_type
       !S specie types
       !S|thermal_specie  S(0:nspec_th)

     !----------------------------
     ! Component: RAD

     ! ** Item List Dimension **

     INTEGER :: nxs = 0
       !item list dimension of xs_name (impurity_source)
       !L|impurity_source  xs_name(nxs)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: xs_name
       !number & name of impurity sources
       !L|impurity_source  xs_name(nxs)

     !----------------------------
     ! Component: RIPPLE

     !----------------------------
     ! Component: RUNAWAY

     !===============================================
     ! Section: SIMULATION_INIT

     !----------------------------
     ! Component: ANOM

     ! ** E Grid Dimension **

     INTEGER :: nefi_anom = 0
       !grid dimension of E_anom (E coordinate)
       !G  E_anom(nefi_anom)

     ! ** E Grid Dimension **

     INTEGER :: nefi_anom2 = 0
       !grid dimension of E_anom2 (E coordinate)
       !G  E_anom2(nefi_anom2)

     ! ** (Ad hoc) Array Dimension **

     INTEGER :: nky_anomth = 0
       !number of points, ky grid ( anomalous data over ky spectrum)
       !I|ENUM  nky_anomth

     ! ** (Ad hoc) Array Dimension **

     INTEGER :: nky_mmm = 0
       !number of points, ky grid for MMM model
       !I|ENUM  nky_mmm

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_anom = 0
       !grid dimension of rho_anom (RHO coordinate)
       !G  rho_anom(nrho_anom)

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_anom2 = 0
       !grid dimension of rho_anom2 (RHO coordinate)
       !G  rho_anom2(nrho_anom2)

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_anomth = 0
       !grid dimension of rho_anomth (RHO coordinate)
       !G  rho_anomth(nrho_anomth)

     CHARACTER*80 :: ANOM_Code_Info = " "
       !Information: code implementing ANOM component
       !C*80   ANOM_Code_Info

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: E_anom
       !(optional) energy grid (1d profiles)
       !for separated variation of fast ion
       !anomalous transport with fast ion energy
       !G  E_anom(nefi_anom)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: E_anom2
       !energy grid (2d anomalous transport profiles)
       !for Db(E,rho), separate profiles for various
       !pitch related limiting orbit topologies:
       !deeply trapped
       !barely trapped
       !barely passing  co/ctr
       !vpll/v -> 1 limit  co/ctr
       !G  E_anom2(nefi_anom2)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_anom
       !rho grid (1d anomalous transport profiles)
       !G  rho_anom(nrho_anom)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_anom2
       !rho grid (2d anomalous transport profiles)
       !G  rho_anom2(nrho_anom2)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_anomth
       !rho grid (1d anomalous data for 1d
       !transport models)
       !G  rho_anomth(nrho_anomth)

     !----------------------------
     ! Component: EC

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_ecrf = 0
       !grid dimension of rho_ecrf (RHO coordinate)
       !G  rho_ecrf(nrho_ecrf)

     CHARACTER*80 :: EC_Code_Info = " "
       !Information: code implementing EC component
       !C*80   EC_Code_Info

     CHARACTER*80 :: EC_Data_Info = " "
       !information on source of ECRF power data
       !C*80   EC_Data_Info

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_ecrf
       !rho grid -- ECRF
       !G  rho_ecrf(nrho_ecrf)

     !----------------------------
     ! Component: EQ

     ! ** Item List Dimension **

     INTEGER :: neqmom = 0
       !item list dimension of eqmom_num (EQ_moments)
       !L|EQ_moments  eqmom_num(neqmom)

     ! ** Item List Dimension **

     INTEGER :: npsmom = 0
       !item list dimension of psmom_num (PS_moments)
       !L|PS_moments  psmom_num(npsmom)

     ! ** R Grid Dimension **

     INTEGER :: nr = 0
       !grid dimension of R_grid (R coordinate)
       !G   R_grid(nR)

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_eq = 0
       !grid dimension of rho_eq (RHO coordinate)
       !G   rho_eq(nrho_eq)

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_eq_geo = 0
       !grid dimension of rho_eq_geo (RHO coordinate)
       !G   rho_eq_geo(nrho_eq_geo)

     ! ** TH Grid Dimension **

     INTEGER :: nth_eq = 0
       !grid dimension of th_eq (TH coordinate)
       !G|CCW   th_eq(nth_eq)

     ! ** Z Grid Dimension **

     INTEGER :: nz = 0
       !grid dimension of Z_grid (Z coordinate)
       !G   Z_grid(nZ)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: eqmom_num
       !Equilibrium moments
       !neqmom = nmom; eqmom_num(1)='1', eqmom_num(2)='2', etc...
       !L|EQ_moments  eqmom_num(neqmom)

     CHARACTER*80 :: EQ_Code_Info = " "
       !Information: code implementing EQ component
       !C*80   EQ_Code_Info

     CHARACTER*80 :: EQ_Data_Info = " "
       !Information: source of EQ input data
       !C*80   EQ_Data_Info

     INTEGER, DIMENSION(:), ALLOCATABLE :: icircuit_coil
       !index of circuit to which each coil belongs
       !(derived from machine description): coil j, named coil_name(j),
       !belongs to the circuit named circuit_name(icircuit_coil(j)).
       !I  icircuit_coil(ncoils)

     INTEGER :: nmom = 16
       !number of Fourier (R,Z) moments
       !requested for Fourier Spline representation
       !R(x,th) = R0(x) + sum x*(xRj(x)*cos(j*th) + xRj~(x)*sin(j*th))
       !Z(x,th) = Z0(x) + sum x*(xZj~(x)*cos(j*th) + xZj(x)*sin(j*th))
       !I  nmom = 16

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: psmom_num
       !Pfirsch-Schlutter moments
       !if call to compute PS moments occurs and this has not been
       !initialized, npsmom=16 will be set, and psmom_num(1)='1',
       !psmom_num(2)='2', ..., psmom_num(16)='16'
       !L|PS_moments  psmom_num(npsmom)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_eq
       !rho grid (EQ)
       !G   rho_eq(nrho_eq)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_eq_geo
       !rho grid (EQ flux surface averages)
       !this grid is built from rho_eq: nrho_eq_geo = 2*nrho_eq - 1
       !rho_eq_geo(1:nrho_eq_geo:2) = rho_eq(1:nrho_eq)
       !rho_eq_geo(2:nrho_eq_geo:2) =
       !(rho_eq(1:nrho_eq-1)+rho_eq(2:nrho_eq))/2
       !i.e. a doubly fine grid for metric flux surface averages stored
       !as piecewise linear radidal profiles.
       !G   rho_eq_geo(nrho_eq_geo)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: R_grid
       !R grid
       !G   R_grid(nR)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: th_eq
       !theta grid (EQ)
       !G|CCW   th_eq(nth_eq)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Z_grid
       !Z grid
       !G   Z_grid(nZ)

     !----------------------------
     ! Component: FUS

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_fus = 0
       !grid dimension of rho_fus (RHO coordinate)
       !G  rho_fus(nrho_fus)

     CHARACTER*80 :: FUS_Code_Info = " "
       !Information: code implementing FUS component
       !C*80   FUS_Code_Info

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_fus
       !rho grid -- Fusion products model
       !G  rho_fus(nrho_fus)

     !----------------------------
     ! Component: GAS

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_gas = 0
       !grid dimension of rho_gas (RHO coordinate)
       !G  rho_gas(nrho_gas)

     ! ** Species List Dimension **

     INTEGER :: nspec_gas = 0
       !species list dimension of SGAS (neutral_gas)
       !S|neutral_gas  SGAS(nspec_gas)

     ! ** Species List Dimension **

     INTEGER :: nspec_imp0 = 0
       !species list dimension of SIMP0 (impurity_atoms)
       !S|impurity_atoms SIMP0(nspec_imp0)

     CHARACTER*80 :: GAS_Code_Info = " "
       !Information: code implementing GAS component
       !C*80   GAS_Code_Info

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: m_SGAS
       !SGAS specie mass
       !S|neutral_gas  SGAS(nspec_gas)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: m_SIMP0
       !SIMP0 specie mass
       !S|impurity_atoms SIMP0(nspec_imp0)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qatom_SGAS
       !SGAS atomic number
       !S|neutral_gas  SGAS(nspec_gas)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qatom_SIMP0
       !SIMP0 atomic number
       !S|impurity_atoms SIMP0(nspec_imp0)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: q_SGAS
       !SGAS specie charge
       !S|neutral_gas  SGAS(nspec_gas)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: q_SIMP0
       !SIMP0 specie charge
       !S|impurity_atoms SIMP0(nspec_imp0)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_gas
       !rho grid (GAS -- neutral gas model)
       !G  rho_gas(nrho_gas)

     INTEGER, DIMENSION(:), ALLOCATABLE :: sc0_to_sgas
       !index map from neutral source to neutral species
       !(computed from GAS_ATOM(ngsc0)).
       !this indicates the species injected by the source
       !although charge exchange induces a source/sink
       !profile of additional species proportional to
       !each source.
       !I  sc0_to_sgas(ngsc0)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: SGAS_name
       !atomic neutral species in plasma core
       !S|neutral_gas  SGAS(nspec_gas)

     INTEGER, DIMENSION(:), ALLOCATABLE :: sgas_to_s
       !map sgas index to "s" list
       !I  sgas_to_s(nspec_gas)

     INTEGER, DIMENSION(:), ALLOCATABLE :: SGAS_type
       !SGAS specie types
       !S|neutral_gas  SGAS(nspec_gas)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: SIMP0_name
       !atomic impurity species in simulation
       !S|impurity_atoms SIMP0(nspec_imp0)

     INTEGER, DIMENSION(:), ALLOCATABLE :: simp0_to_s
       !map simp0 index to "s" list
       !I  simp0_to_s(nspec_imp0)

     INTEGER, DIMENSION(:), ALLOCATABLE :: SIMP0_type
       !SIMP0 specie types
       !S|impurity_atoms SIMP0(nspec_imp0)

     !----------------------------
     ! Component: IC

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_icrf = 0
       !grid dimension of rho_icrf (RHO coordinate)
       !G  rho_icrf(nrho_icrf)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: fracmin
       !minority density fraction if kdens_rfmin="fraction"
       !if so, it must have non-zero positive value for each minority ion specie.
       !sum(fracmin) << 1 is expected.
       !R|units=- fracmin(nspec_rfmin)

     CHARACTER*80 :: IC_Code_Info = " "
       !Information: code implementing IC component
       !C*80   IC_Code_Info

     CHARACTER*80 :: IC_Data_Info = " "
       !information on source of ICRF power data
       !C*80   IC_Data_Info

     CHARACTER*32 :: kdens_rfmin = "data"
       != "fraction" => nmini(i) = fracmin(i)*ne
       != any other value (e.g. "data" or "model") then
       !the minority density is set by some other
       !means (e.g. read in from another data source).
       !N  kdens_rfmin = "data"

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_icrf
       !rho grid (RF)
       !G  rho_icrf(nrho_icrf)

     !----------------------------
     ! Component: LH

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_lhrf = 0
       !grid dimension of rho_lhrf (RHO coordinate)
       !G  rho_lhrf(nrho_lhrf)

     CHARACTER*80 :: LH_Code_Info = " "
       !Information: code implementing LH component
       !C*80   LH_Code_Info

     CHARACTER*80 :: LH_Data_Info = " "
       !Information on source of LHRF power data
       !C*80   LH_Data_Info

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_lhrf
       !rho grid -- LHRF
       !G  rho_lhrf(nrho_lhrf)

     !----------------------------
     ! Component: LMHD

     ! ** Item List Dimension **

     INTEGER :: mmodes = 0
       !item list dimension of Tor_mode_label (toroidal_modes)
       !L|toroidal_modes Tor_mode_label(mmodes)

     ! ** Item List Dimension **

     INTEGER :: nballoon = 0
       !item list dimension of Balloon_code (Balloon_codes)
       !L|Balloon_codes  Balloon_code(nballoon)

     ! ** Item List Dimension **

     INTEGER :: ndelta_w = 0
       !item list dimension of DeltaW_code (Delta_W_codes)
       !L|Delta_W_codes  DeltaW_code(ndelta_w)

     ! ** Item List Dimension **

     INTEGER :: nnewcomb = 0
       !item list dimension of Newcomb_code (Newcomb_codes)
       !L|Newcomb_codes  Newcomb_code(nnewcomb)

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_lmhd = 0
       !grid dimension of rho_lmhd (RHO coordinate)
       !G  rho_lmhd(nrho_lmhd)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: Balloon_code
       !name(s) of Ballooning code(s) used
       !L|Balloon_codes  Balloon_code(nballoon)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: Balloon_mapper
       !mapper code used for each ballooning code
       !N  Balloon_mapper(nballoon)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: DeltaW_code
       !name(s) of delta(W) code(s) used
       !L|Delta_W_codes  DeltaW_code(ndelta_w)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: DeltaW_mapper
       !mapper code used for each delta(W) code
       !N  DeltaW_mapper(ndelta_W)

     CHARACTER*32 :: LMHD_eq_code = " "
       !name of MHD equilibrium refinement code used
       !N  LMHD_eq_code

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: Newcomb_code
       !name(s) of Newcomb code(s) used
       !L|Newcomb_codes  Newcomb_code(nnewcomb)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: Newcomb_mapper
       !mapper code used for each Newcomb code
       !N  Newcomb_mapper(nnewcomb)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_lmhd
       !rho grid (LMHD component)
       !G  rho_lmhd(nrho_lmhd)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: Tor_mode_label
       !Toroidal modes to monitor
       !L|toroidal_modes Tor_mode_label(mmodes)

     INTEGER, DIMENSION(:), ALLOCATABLE :: Tor_mode_No
       !(from input) Toroidal modes monitored
       !set at initialization, would not vary in time
       !likely just: 1,2,3,...,mmodes
       !I   Tor_mode_No(mmodes)

     !----------------------------
     ! Component: NBI

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_nbi = 0
       !grid dimension of rho_nbi (RHO coordinate)
       !G  rho_nbi(nrho_nbi)

     ! ** Species List Dimension **

     INTEGER :: nspec_beam = 0
       !species list dimension of SNBI (beam_ion)
       !S|beam_ion     SNBI(nspec_beam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: m_SNBI
       !SNBI specie mass
       !S|beam_ion     SNBI(nspec_beam)

     CHARACTER*80 :: NBI_Code_Info = " "
       !Information: code implementing NBI component
       !C*80   NBI_Code_Info

     CHARACTER*80 :: NBI_Data_Info = " "
       !information on source of beam power & voltage data
       !C*80   NBI_Data_Info

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qatom_SNBI
       !SNBI atomic number
       !S|beam_ion     SNBI(nspec_beam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: q_SNBI
       !SNBI specie charge
       !S|beam_ion     SNBI(nspec_beam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_nbi
       !rho grid -- NBI
       !G  rho_nbi(nrho_nbi)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: SNBI_name
       !beam ion species
       !this species list to be constructed from nbion(1:nbeam)
       !and augmented optionally by nbion_trace(1:nbeam)
       !S|beam_ion     SNBI(nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: SNBI_type
       !SNBI specie types
       !S|beam_ion     SNBI(nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: trace_flag
       !trace element flag
       !0 means, beam has no trace element;
       !1 means, trace element, matching injection energy fractions
       !2 means, trace element injected at 100% full energy
       !(1 will be set if the trace element is an isotope of
       !of the beam's main injection specie; 2 if not).
       !I  trace_flag(nbeam)

     !----------------------------
     ! Component: PEL

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_pel = 0
       !grid dimension of rho_pel (RHO coordinate)
       !G  rho_pel(nrho_pel)

     CHARACTER*80 :: PEL_Code_Info = " "
       !Information: code implementing PELLET component
       !C*80   PEL_Code_Info

     CHARACTER*80 :: PEL_Data_Info = " "
       !Information: on source of pellet injection
       !C*80   PEL_Data_Info

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_pel
       !rho grid -- PEL
       !G  rho_pel(nrho_pel)

     !----------------------------
     ! Component: PLASMA

     ! ** RHO Grid Dimension **

     INTEGER :: nrho = 0
       !grid dimension of rho (RHO coordinate)
       !G   rho(nrho)

     ! ** Species List Dimension **

     INTEGER :: nspec_all = 0
       !species list dimension of ALL (specie)
       !S|specie  ALL(0:nspec_all)

     ! ** Species List Dimension **

     INTEGER :: nspec_alla = 0
       !species list dimension of ALLA (specie)
       !S|specie  ALLA(0:nspec_alla)

     ! ** Species List Dimension **

     INTEGER :: nspec_impi = 0
       !species list dimension of SIMPI (impurity_atoms)
       !S|impurity_atoms SIMPI(nspec_impi)

     ! ** Species List Dimension **

     INTEGER :: nspec_tha = 0
       !species list dimension of SA (thermal_specie)
       !S|thermal_specie  SA(0:nspec_tha)

     ! ** Item List Dimension **

     INTEGER :: ntrcat = 0
       !item list dimension of trcat (TR_categories)
       !L|TR_categories  trcat(ntrcat)

     INTEGER, DIMENSION(:), ALLOCATABLE :: alla_index
       !species index in original lists
       !alla_index(j)=-1 indicates that element (j) of the "alla" list is
       !derived from a composite of ions in the "s" list (using Zeff
       !and quasineutrality equations).
       !otherwise for thermal ions alla_index(j) gives the index of the
       !species in "S".  For example, if index 2 corresponds to Deuterium
       !thermal ions in "alla", then alla_index(2) gives the index in "S"
       !for Deuterium thermal ions.  For non-thermal ions, ion species j
       !corresponds to species index:
       !alla_index(j) in "SNBI" if all_type(j) = ps_beam_ion
       !alla_index(j) in "RFMIN" if all_type(j) = ps_rf_minority
       !alla_index(j) in "SFUS" if all_type(j) = ps_fusion_ion
       !I alla_index(0:nspec_alla)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: ALLA_name
       !all species abridged
       !(index 0 for electrons)
       !S|specie  ALLA(0:nspec_alla)

     INTEGER, DIMENSION(:), ALLOCATABLE :: ALLA_type
       !ALLA specie types
       !S|specie  ALLA(0:nspec_alla)

     INTEGER, DIMENSION(:), ALLOCATABLE :: all_index
       !species index in original lists
       !ion species j in "all" corresponds to ion species:
       !all_index(j) in "s" if all_type(j) = ps_electron, ps_therm_ion, or
       !ps_impurity
       !all_index(j) in "snbi" if all_type(j) = ps_beam_ion
       !all_index(j) in "rfmin" if all_type(j) = ps_rf_minority
       !all_index(j) in "sfus" if all_type(j) = ps_fusion_ion
       !I all_index(0:nspec_all)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: ALL_name
       !all species
       !index 0 for electrons
       !this list is the union of lists "s", "snbi", "rfmin", and "sfus"
       !S|specie  ALL(0:nspec_all)

     INTEGER, DIMENSION(:), ALLOCATABLE :: ALL_type
       !ALL specie types
       !S|specie  ALL(0:nspec_all)

     CHARACTER*80 :: CUR_Data_Info = " "
       !information on source of plasma current data
       !C*80   CUR_Data_Info

     CHARACTER*80 :: Global_label = " "
       !Run Label
       !imbedded blanks allowed; <= 80 characters
       !C*80   Global_label

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: m_ALL
       !ALL specie mass
       !S|specie  ALL(0:nspec_all)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: m_ALLA
       !ALLA specie mass
       !S|specie  ALLA(0:nspec_alla)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: m_SA
       !SA specie mass
       !S|thermal_specie  SA(0:nspec_tha)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: m_SIMPI
       !SIMPI specie mass
       !S|impurity_atoms SIMPI(nspec_impi)

     CHARACTER*80 :: ns_Data_Info = " "
       !information on source of Density data
       !C*80   ns_Data_Info

     CHARACTER*80 :: PLASMA_Code_Info = " "
       !Information: code managing PLASMA component
       !(advance of specie temperatures & densities etc)
       !C*80   PLASMA_Code_Info

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qatom_ALL
       !ALL atomic number
       !S|specie  ALL(0:nspec_all)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qatom_ALLA
       !ALLA atomic number
       !S|specie  ALLA(0:nspec_alla)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qatom_SA
       !SA atomic number
       !S|thermal_specie  SA(0:nspec_tha)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qatom_SIMPI
       !SIMPI atomic number
       !S|impurity_atoms SIMPI(nspec_impi)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: q_ALL
       !ALL specie charge
       !S|specie  ALL(0:nspec_all)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: q_ALLA
       !ALLA specie charge
       !S|specie  ALLA(0:nspec_alla)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: q_SA
       !SA specie charge
       !S|thermal_specie  SA(0:nspec_tha)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: q_SIMPI
       !SIMPI specie charge
       !S|impurity_atoms SIMPI(nspec_impi)

     INTEGER, DIMENSION(:), ALLOCATABLE :: rfmin_to_all
       !map rfmin index to "all" list
       !I  rfmin_to_all(nspec_rfmin)

     INTEGER, DIMENSION(:), ALLOCATABLE :: rfmin_to_alla
       !map rfmin index to "alla" list
       !I  rfmin_to_alla(nspec_rfmin)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho
       !rho grid (PLASMA)
       !G   rho(nrho)

     CHARACTER*32 :: RunID = " "
       !Run ID
       !one word, no imbedded blanks; <= 32 characters
       !N      RunID

     INTEGER, DIMENSION(:), ALLOCATABLE :: sa_index
       !species index in original lists
       !sa_index(j)=-1 indicates that element (j) of the "SA" list is
       !derived from a composite of ions in the "S" list (using Zeff
       !and quasineutrality equations).
       !otherwise sa_index(j) gives the index of the species in "S"
       !For example, if index 2 corresponds to Deuterium ions in "SA", then
       !sa_index(2) gives the index in "S" for Deuterium ions.
       !For non-impurities usually sa_index(j)=j but this could be altered
       !if He3+1 or He4+1 (less than fully stripped He ions) are present in
       !the model.
       !I sa_index(0:nspec_all)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: SA_name
       !thermal species abridged
       !(index 0 for electrons)
       !S|thermal_specie  SA(0:nspec_tha)

     INTEGER, DIMENSION(:), ALLOCATABLE :: SA_type
       !SA specie types
       !S|thermal_specie  SA(0:nspec_tha)

     INTEGER, DIMENSION(:), ALLOCATABLE :: sfus_to_all
       !map sfus index to "all" list
       !I  sfus_to_all(nspec_fusion)

     INTEGER, DIMENSION(:), ALLOCATABLE :: sfus_to_alla
       !map sfus index to "alla" list
       !I  sfus_to_alla(nspec_fusion)

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: SIMPI_name
       !impurity ion species in simulation
       !S|impurity_atoms SIMPI(nspec_impi)

     INTEGER, DIMENSION(:), ALLOCATABLE :: simpi_to_s
       !map simpi index to "s" list
       !I  simpi_to_s(nspec_impi)

     INTEGER, DIMENSION(:), ALLOCATABLE :: SIMPI_type
       !SIMPI specie types
       !S|impurity_atoms SIMPI(nspec_impi)

     INTEGER, DIMENSION(:), ALLOCATABLE :: snbi_to_all
       !map snbi index to "all" list
       !I  snbi_to_all(nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: snbi_to_alla
       !map snbi index to "alla" list
       !I  snbi_to_alla(nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: s_to_sa
       !address in sa species list
       !if species s(j) is in list SA, s_to_sa(j) gives its address in SA.
       !if species s(j) is NOT in list SA, s_to_sa(j)=-1.
       !I s_to_sa(0:nspec_th)

     REAL(KIND=rspec) :: tfinal = 0.0_rspec
       !stop time of simulation (may be adjustable)
       !R  tfinal

     CHARACTER*80 :: TF_Data_Info = " "
       !information on source of toroidal field data
       !C*80   TF_Data_Info

     REAL(KIND=rspec) :: tinit = 0.0_rspec
       !start time of simulation
       !R  tinit

     CHARACTER*32, DIMENSION(:), ALLOCATABLE :: trcat
       !Application defined list of
       !transport categories (possible examples: NC for neoclassical,
       !TURB for turbulent, SAW for sawtooth induced, ...); used to
       !dimension profile data provided to quantify transport rates
       !according to underlying physics
       !L|TR_categories  trcat(ntrcat)

     CHARACTER*256, DIMENSION(:), ALLOCATABLE :: trmodels
       !short description or ID of transport models
       !C*256 trmodels(ntrcat)

     CHARACTER*80 :: Ts_Data_Info = " "
       !information on source of Temperature data
       !C*80   Ts_Data_Info

     CHARACTER*80 :: vpol_Data_Info = " "
       !information on source of poloidal velocity data
       !C*80   vpol_Data_Info

     CHARACTER*80 :: VSUR_Data_Info = " "
       !information on source of surface voltage data
       !C*80   VSUR_Data_Info

     CHARACTER*80 :: vtor_Data_Info = " "
       !information on source of toroidal velocity data
       !C*80   vtor_Data_Info

     INTEGER :: Z0max = 2
       !maximum Z of non-impurity species
       !(default is 2: Helium).  Ion species with Z <= Z0max
       !are considered "non-impurities" and are present in
       !SGAS, a list of recycling neutral gas species.
       !IS  Z0max = 2

     CHARACTER*80 :: ZEFF_Data_Info = " "
       !information on source of Zeff data
       !C*80   ZEFF_Data_Info

     INTEGER :: Zimp1 = 2
       !lower limit on Z of impurity in reduced species list SA;
       !by default the lowest available Z impurity is chosen.
       !After the SA list is defined, this value is reset to
       !match the lower Z impurity selected for the SA list.
       !IS  Zimp1 = 2

     INTEGER :: Zimp2 = 1000
       !upper limit on Z of impurity in SA species list;
       !by default the highest Z impurity available is chosen.
       !After the SA list is defined, this value is reset to
       !match the higher Z impurity selected for the SA list.
       !IS  Zimp2 = 1000

     !----------------------------
     ! Component: RAD

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_mist = 0
       !grid dimension of rho_mist (RHO coordinate)
       !G  rho_mist(nrho_mist)

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_rad = 0
       !grid dimension of rho_rad (RHO coordinate)
       !G  rho_rad(nrho_rad)

     CHARACTER*80 :: RAD_Code_Info = " "
       !Information: code implementing RAD component
       !C*80   RAD_Code_Info

     CHARACTER*80 :: RAD_Data_Info = " "
       !Information on source of radiated power data
       !C*80   RAD_Data_Info

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_mist
       !rho grid (PLASMA) impurity-driven sources
       !G  rho_mist(nrho_mist)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_rad
       !rho grid (RAD)
       !G  rho_rad(nrho_rad)

     !----------------------------
     ! Component: RIPPLE

     ! ** R Grid Dimension **

     INTEGER :: nr_rip = 0
       !grid dimension of R_ripple (R coordinate)
       !G  R_ripple(nR_rip)

     ! ** Z Grid Dimension **

     INTEGER :: nz_rip = 0
       !grid dimension of Z_ripple (Z coordinate)
       !G  Z_ripple(nZ_rip)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: R_ripple
       !R grid for TF ripple field(s)
       !G  R_ripple(nR_rip)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Z_ripple
       !Z grid for TF ripple field(s)
       !G  Z_ripple(nZ_rip)

     !----------------------------
     ! Component: RUNAWAY

     ! ** RHO Grid Dimension **

     INTEGER :: nrho_rw = 0
       !grid dimension of rho_rw (RHO coordinate)
       !G  rho_rw(nrho_rw)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_rw
       !rho grid -- RUNAWAY electrons
       !G  rho_rw(nrho_rw)

     CHARACTER*80 :: RUNAWAY_Code_Info = " "
       !Information: code implementing RUNAWAY component
       !C*80   RUNAWAY_Code_Info

     !===============================================
     ! Section: STATE_DATA

     !----------------------------
     ! Component: ANOM

     REAL(KIND=rspec) :: interchange_R = 0.0_rspec
       !norm rho of the location of the interchange mode from TGLF model
       !R|units=-      interchange_R

     INTEGER :: nmdifb = 0
       !type of fast ion diffusivity
       !=4: 2D fast ion diffusivity from ufiles
       !=6: fast ion diffusivity from dep code
       !I   nmdifb = 0

     REAL(KIND=rspec) :: nmodel_bdy = 0.0_rspec
       !model density, boundary value
       !R|units=m^-3   nmodel_bdy

     REAL(KIND=rspec) :: tmodel_bdy = 0.0_rspec
       !model temperature, boundary value
       !R|units=keV    tmodel_bdy

     !----------------------------
     ! Component: EC

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: power_ec
       !power on each ECRF source
       !R|units=W  power_ec(necrf_src)

     !----------------------------
     ! Component: EQ

     REAL(KIND=rspec) :: B_axis = 0.0_rspec
       !|B| at magnetic axis
       !R  B_axis

     REAL(KIND=rspec) :: B_axis_vac = 0.0_rspec
       !vacuum B field at axis
       !R  B_axis_vac

     REAL(KIND=rspec) :: B_max_lcfs = 0.0_rspec
       !|B|_max of last closed flux surface
       !R  B_max_lcfs

     REAL(KIND=rspec) :: B_min_lcfs = 0.0_rspec
       !|B|_min of last closed flux surface
       !R  B_min_lcfs

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: coil_apt
       !SIGNED circuit current
       !amps/turn within each coil attached to the indicated circult:
       !SIGN: + means current flows counter-clockwise, viewed from above, for
       !a coil (j) with Nturns(j) > 0; direction opposite if Nturns(j) < 0
       !- means the reverse.
       !The SIGNED current in coil (j) is Nturns(j)*coil_apt(icircuit_coil(j));
       !a positive current flows counter-clockwise viewed from above.
       !Any element of Nturns(...) and any element of coil_apt(...) can be
       !either positive or negative.
       !R|units=amps/turn coil_apt(ncircuits)

     CHARACTER*256 :: eqdsk_file = " "
       !EFIT G-eqdsk file: psi(R,Z), g(psi), etc.
       !F  eqdsk_file

     REAL(KIND=rspec) :: Psi_to_machine_axis = 0.0_rspec
       !delta(Psi) from magnetic axis to machine axis
       !subtract this from Psi(R,Z) to get a Psi value
       !that approaches zero as R-> zero: Psi ~ R*A_phi
       !(as in the EFIT free boundary representation).
       !Note: prescribed boundary code runs (e.g.
       !traditional TRANSP runs) cannot know this
       !number; so Plasma States created from such run
       !data will leave this at zero.  Plasma States
       !using free boundary MHD equilibrium information
       !will always have a non-zero value here.
       !R|units=Wb/rad   Psi_to_machine_axis

     REAL(KIND=rspec) :: R_axis = 0.0_rspec
       !R of magnetic axis
       !R  R_axis

     REAL(KIND=rspec) :: R_max_lcfs = 0.0_rspec
       !R_max of last closed flux surface
       !R  R_max_lcfs

     REAL(KIND=rspec) :: R_min_lcfs = 0.0_rspec
       !R_min of last closed flux surface
       !R  R_min_lcfs

     REAL(KIND=rspec) :: Z_axis = 0.0_rspec
       !Z of magnetic axis
       !R  Z_axis

     REAL(KIND=rspec) :: Z_max_lcfs = 0.0_rspec
       !Z_max of last closed flux surface
       !R  Z_max_lcfs

     REAL(KIND=rspec) :: Z_min_lcfs = 0.0_rspec
       !Z_min of last closed flux surface
       !R  Z_min_lcfs

     !----------------------------
     ! Component: FUS

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: nmini_bdy
       !RF minority ion densities at/beyond boundary
       !R|units=m^-3  nmini_bdy(nspec_rfmin)

     !----------------------------
     ! Component: GAS

     REAL(KIND=rspec) :: dn0out = 0.0_rspec
       !Hydrogenic neutral density in scrape off region
       !This is a simplified 0d specification of neutral density
       !beyond the plasma boundary, that can be used to deplete
       !fast ion orbits that go beyond the boundary via charge
       !exchange, in some models.
       !R|units=m^-3  dn0out

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: e0_av
       !average energy of neutral sources, "(3/2)*T0"
       ![in reference frame of (vphi0_av)]; some models
       !impose a minimum value such as 0.005 KeV.  The
       !value can effect penetration of neutrals and
       !depth of edge-driven charge exchange loss, but
       !(depending on plasma parameters) the effect
       !is often weak.
       !R|units=KeV   e0_av(ngsc0)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: sc0
       !edge neutral sources (atoms/sec)
       !these numbers normalize numerous GAS profiles
       !R|units=#/sec  sc0(ngsc0)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: vphi0_av
       !average toroidal velocity of neutral source
       !(some models may assume this is zero).
       !R|units=m/sec vphi0_av(ngsc0)

     !----------------------------
     ! Component: IC

     CHARACTER*256, DIMENSION(:), ALLOCATABLE :: dist_fun
       !distribution function filenames
       !F  dist_fun(0:nspec_alla)

     INTEGER, DIMENSION(:), ALLOCATABLE :: isThermal
       !=1 if minority specie is thermalized
       !=0: never set; =2: non-thermal.
       !I  isThermal(nspec_rfmin)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: nfusi_bdy
       !fusion ion densities at/beyond boundary
       !R|units=m^-3  nfusi_bdy(nspec_fusion)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: picrf_abs
       !RF power absorbed inside plasma
       !R|units=W picrf_abs(nicrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: picrf_ext
       !RF power deposited outside plasma
       !R|units=W picrf_ext(nicrf_src)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: power_ic
       !power on each ICRF source
       !R|units=W  power_ic(nicrf_src)

     CHARACTER*256, DIMENSION(:), ALLOCATABLE :: ql_operator
       !quasilinear operator filenames
       !F  ql_operator(0:nspec_alla)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: wt_nphi_abs
       !absorbed spectrum n_phi weight
       !power_ic(iant)*wt_nphi_abs(ind,iant)
       !gives power in toroidal mode
       !nphi(ind,iant)
       !"ind" covers range 1:num_nphi(iant).
       !see vacuum spectrum wt_nphi(...)
       !R|units=- wt_nphi_abs(num_nphi(),nicrf_src)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: wt_nphi_ext
       !fraction of power deposited outside
       !plasma vs. nphi, antenna #.
       !R|units=- wt_nphi_ext(num_nphi(),nicrf_src)

     !----------------------------
     ! Component: LH

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: power_lh
       !power on each LHRF source
       !R|units=W  power_lh(nlhrf_src)

     !----------------------------
     ! Component: LMHD

     INTEGER, DIMENSION(:), ALLOCATABLE :: Balloon_stable
       !0: stable everywhere; =1: unstable somewhere
       !I   Balloon_stable(nballoon)

     INTEGER, DIMENSION(:), ALLOCATABLE :: Balloon_status
       !completion status: Ballooning codes
       !** see also: balstab profile, below **
       !I   Balloon_status(nballoon)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: DeltaW_stable
       !0: stable; 1: unstable;
       !element (i,j) indicates if toroidal mode number
       !Tor_mode_No(i) is stable or unstable according to
       !the code DeltaW_code(j)
       !I   DeltaW_stable(mmodes,ndelta_W)

     INTEGER, DIMENSION(:), ALLOCATABLE :: DeltaW_status
       !completion status: Delta(W) codes
       !I   DeltaW_status(ndelta_W)

     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: gamma_lmhd
       !ideal growth rate
       !from a delta-W linear MHD code, normalized to the
       !Alfven time Rq/V_a where:
       !R -- major radius of magnetic axis
       !V_a -- alfven speed at magnetic axis
       !q = qmin  (minimum q anywhere in equilibrium)
       !R|units=-  gamma_lmhd(mmodes,ndelta_W)

     INTEGER :: LMHD_status = ps_unInit
       !0: OK; =1: at least one code error occurred.
       !I   LMHD_status

     INTEGER, DIMENSION(:), ALLOCATABLE :: Mercier_stable
       !0: stable everywhere; =1: unstable somewhere
       !I   Mercier_stable(nballoon)

     INTEGER :: MHD_eq_status = ps_unInit
       !0: OK; =1: MHD equilibrium refinement failed
       !if this is non-zero, the entire LMHD calculation is abandoned!!
       !I   MHD_eq_status

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: Newcomb_stable
       !0: stable; 1: unstable;
       !element (i,j) indicates if toroidal mode number
       !Tor_mode_No(i) is stable or unstable according to
       !the code Newcomb_code(j)
       !I   Newcomb_stable(mmodes,nnewcomb)

     INTEGER, DIMENSION(:), ALLOCATABLE :: Newcomb_status
       !completion status: Newcomb codes
       !I   Newcomb_status(nnewcomb)

     INTEGER, DIMENSION(:), ALLOCATABLE :: Tornum_balcrit
       !critical toroidal mode number for ballooning
       !unstable cases (-1 if could not be found).
       !I   Tornum_balcrit(nballoon)

     !----------------------------
     ! Component: NBI

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: frac_full
       !fraction of beam current at full voltage
       !R|units=-  frac_full(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: frac_half
       !fraction of beam current at half voltage
       !1-frac_full(ib)-frac_half(ib) = fraction at (1/3) voltage
       !R|units=-  frac_half(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: kvolt_nbi
       !energy of each beam source **keV**
       !R|units=keV kvolt_nbi(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: nbeami_bdy
       !beam ion densities at/beyond boundary
       !R|units=m^-3  nbeami_bdy(nspec_beam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: power_nbi
       !power on each beam source
       !R|units=W  power_nbi(nbeam)

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: power_nbi_trace
       !power injected as trace element (if any)
       !total power on beam ib is power_nbi(ib) + power_nbi_trace(ib)
       !R|units=W  power_nbi_trace(nbeam)

     !----------------------------
     ! Component: PEL

     !----------------------------
     ! Component: PLASMA

     REAL(KIND=rspec) :: eldot = 0.0_rspec
       ![d(Philim)/dt]/[2*Philim]
       !R|units=1/sec  eldot

     INTEGER :: lock_machine_descr = 0
       !Machine_Description section lock
       !I  lock_machine_descr = 0

     INTEGER :: lock_shot_config = 0
       !Shot_Configuration section lock
       !I  lock_shot_config = 0

     INTEGER :: lock_sim_init = 0
       !Simulation_Init section lock
       !I  lock_sim_init = 0

     REAL(KIND=rspec) :: nebar = 0.0_rspec
       !line-averaged electron density
       !R|units=#/m^2 nebar

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: ns_bdy
       !specie densities at/beyond boundary
       !R|units=m^-3 ns_bdy(0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: ns_is_input
       !specie density measurement flag
       !I ns_is_input(0:nspec_th)

     REAL(KIND=rspec) :: omegat_bdy = 0.0_rspec
       !toroidal angular velocity at/beyond boundary
       !R|units=rad/sec  omegat_bdy

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rho_bdy_ns
       !locations beyond which densities are known
       !R|units=- rho_bdy_ns(0:nspec_th)

     REAL(KIND=rspec) :: rho_bdy_omegat = 0.0_rspec
       !location beyond which angular velocity is known
       !R rho_bdy_omegat

     REAL(KIND=rspec) :: rho_bdy_Te = 0.0_rspec
       !location beyond which Te is known
       !R rho_bdy_Te

     REAL(KIND=rspec) :: rho_bdy_Ti = 0.0_rspec
       !location beyond which Ti is known
       !R rho_bdy_Ti

     REAL(KIND=rspec) :: t0 = 0.0_rspec
       !time at beginning of IPS macro timestep
       !R  t0

     REAL(KIND=rspec) :: t1 = 0.0_rspec
       !time at end of IPS macro timestep
       !R  t1

     REAL(KIND=rspec) :: Te_bdy = 0.0_rspec
       !Electron temperature at/beyond boundary
       !R Te_bdy

     REAL(KIND=rspec) :: Ti_bdy = 0.0_rspec
       !Ion temperature at/beyond boundary
       !R Ti_bdy

     INTEGER, DIMENSION(:), ALLOCATABLE :: Ts_is_input
       !specie temperature measurement flag
       !I Ts_is_input(0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: vpol_is_input
       !specie outer midplane poloidal velocit flag
       !I vpol_is_input(0:nspec_th)

     REAL(KIND=rspec) :: vsur = 0.0_rspec
       !toroidal voltage at surface
       !R  vsur

     INTEGER, DIMENSION(:), ALLOCATABLE :: vtor_is_input
       !specie outer midplane toroidal velocit flag
       !I vtor_is_input(0:nspec_th)

     !----------------------------
     ! Component: RAD

     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: xs
       !impurity source magnitude (atoms/sec)
       !these numbers normalize "sii_xs" source profiles
       !R|units=#/sec    xs(nxs)

     !----------------------------
     ! Component: RIPPLE

     !----------------------------
     ! Component: RUNAWAY

     !===============================================
     ! Section: STATE_PROFILES

     !----------------------------
     ! Component: ANOM

     INTEGER :: id_anom_evar = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: anom_evar
       !anomalous transport energy variation
       !=1 to use transport profiles as given; O(1) expected.
       !if undefined, use transport profiles as given.
       !R|pclin|units=- anom_evar(nefi_anom)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_bidiff_bpass_co
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: bidiff_bpass_co
       !beam ion diffusivity: co barely passing orbits
       !R|units=m^2/sec|pclin  bidiff_bpass_co(nefi_anom2,nrho_anom2,nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_bidiff_bpass_ctr
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: bidiff_bpass_ctr
       !beam ion diffusivity: ctr barely passing orbits
       !R|units=m^2/sec|pclin  bidiff_bpass_ctr(nefi_anom2,nrho_anom2,nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_bidiff_btrap
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: bidiff_btrap
       !beam ion diffusivity: barely trapped orbits
       !R|units=m^2/sec|pclin  bidiff_btrap(nefi_anom2,nrho_anom2,nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_bidiff_dpass_co
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: bidiff_dpass_co
       !beam ion diffusivity: co passing |vpll/v| -> 1
       !R|units=m^2/sec|pclin  bidiff_dpass_co(nefi_anom2,nrho_anom2,nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_bidiff_dpass_ctr
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: bidiff_dpass_ctr
       !beam ion diffusivity: ctr passing |vpll/v| -> 1
       !R|units=m^2/sec|pclin  bidiff_dpass_ctr(nefi_anom2,nrho_anom2,nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_bidiff_dtrap
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: bidiff_dtrap
       !beam ion diffusivity: deeply trapped orbits
       !R|units=m^2/sec|pclin  bidiff_dtrap(nefi_anom2,nrho_anom2,nspec_beam)

     INTEGER :: id_chie_anom = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_anom
       !Te-equation, diffusivity term
       !R|units=m^2/sec|pclin  chie_anom(nrho_anomth)

     INTEGER :: id_chie_drbm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_drbm
       !Te-equation, diffusivity term
       !R|units=m^2/sec|pclin  chie_drbm(nrho_anomth)

     INTEGER :: id_chie_etg = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_etg
       !Te-equation, diffusivity term ETG model
       !R|units=m^2/sec|pclin chie_etg(nrho_anomth)

     INTEGER :: id_chie_glf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_glf
       !Te-equation, diffusivity from GLF23 model
       !R|units=m^2/s|pclin chie_glf(nrho_anomth)

     INTEGER :: id_chie_gtcneo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_gtcneo
       !Te-equation, neoclassical diffusivity from GTC_NEO model
       !R|units=m^2/s|pclin chie_gtcneo(nrho_anomth)

     INTEGER :: id_chie_mmm71 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_mmm71
       !Te-equation, diffusivity term
       !R|units=m^2/sec|pclin  chie_mmm71(nrho_anomth)

     INTEGER :: id_chie_mtm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_mtm
       !Te-equation, thermal diffusivity term MTM model
       !R|units=m^2/sec|pclin chie_mtm(nrho_anomth)

     INTEGER :: id_chie_nclass = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_nclass
       !Te-equation, neoclassical diffusivity from NCLASS model
       !R|units=m^2/s|pclin chie_nclass(nrho_anomth)

     INTEGER :: id_chie_neo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_neo
       !Te-equation, neoclassical diffusivity from NEO model
       !R|units=m^2/s|pclin chie_neo(nrho_anomth)

     INTEGER :: id_chie_ntm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_ntm
       !Te-equation, neoclassical diffusivity from NTM model
       !R|units=m^2/s|pclin chie_ntm(nrho_anomth)

     INTEGER :: id_chie_paleo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_paleo
       !Te-equation, diffusivity term PALEO model
       !R|units=m^2/sec|pclin chie_paleo(nrho_anomth)

     INTEGER :: id_chie_tglf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_tglf
       !Te-equation, diffusivity from TGLF model
       !R|units=m^2/s|pclin chie_tglf(nrho_anomth)

     INTEGER :: id_chie_w19 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_w19
       !Te-equation, diffusivity term
       !R|units=m^2/sec|pclin  chie_w19(nrho_anomth)

     INTEGER :: id_chii_anom = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chii_anom
       !Ti-equation, diffusivity term
       !R|units=m^2/sec|pclin  chii_anom(nrho_anomth)

     INTEGER :: id_chii_drbm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chii_drbm
       !Ti-equation, diffusivity term
       !R|units=m^2/sec|pclin  chii_drbm(nrho_anomth)

     INTEGER :: id_chii_glf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chii_glf
       !Ti-equation, diffusivity from GLF23 model
       !R|units=m^2/s|pclin chii_glf(nrho_anomth)

     INTEGER :: id_chii_gtcneo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chii_gtcneo
       !Ti-equation, neoclassical diffusivity from GTC_NEO model
       !R|units=m^2/s|pclin chii_gtcneo(nrho_anomth)

     INTEGER :: id_chii_mmm71 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chii_mmm71
       !Ti-equation, diffusivity term
       !R|units=m^2/sec|pclin  chii_mmm71(nrho_anomth)

     INTEGER :: id_chii_nclass = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chii_nclass
       !Ti-equation, neoclassical diffusivity from NCALSS model
       !R|units=m^2/s|pclin chii_nclass(nrho_anomth)

     INTEGER :: id_chii_neo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chii_neo
       !Ti-equation, neoclassical diffusivity from NEO model
       !R|units=m^2/s|pclin chii_neo(nrho_anomth)

     INTEGER :: id_chii_ntm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chii_ntm
       !Ti-equation, neoclassical diffusivity from NTM model
       !R|units=m^2/s|pclin chii_ntm(nrho_anomth)

     INTEGER :: id_chii_tglf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chii_tglf
       !Ti-equation, diffusivity from TGLF model
       !R|units=m^2/s|pclin chii_tglf(nrho_anomth)

     INTEGER :: id_chii_w19 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chii_w19
       !Ti-equation, diffusivity term
       !R|units=m^2/sec|pclin  chii_w19(nrho_anomth)

     INTEGER :: id_chiphi_anom = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chiphi_anom
       !momentum-equation, diffusivity term
       !R|units=m^2/sec|pclin  chiphi_anom(nrho_anomth)

     INTEGER :: id_chiphi_drbm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chiphi_drbm
       !momentum-equation, diffusivity term
       !R|units=m^2/sec|pclin  chiphi_drbm(nrho_anomth)

     INTEGER :: id_chiphi_glf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chiphi_glf
       !momentum-equation, diffusivity from GLF23 model
       !R|units=m^2/s|pclin chiphi_glf(nrho_anomth)

     INTEGER :: id_chiphi_gtcneo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chiphi_gtcneo
       !momentum-equation, neoclassical diffusivity from GTC_NEO model
       !R|units=m^2/s|pclin chiphi_gtcneo(nrho_anomth)

     INTEGER :: id_chiphi_mmm71 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chiphi_mmm71
       !momentum-equation, diffusivity term, toroidal
       !R|units=m^2/sec|pclin  chiphi_mmm71(nrho_anomth)

     INTEGER :: id_chiphi_neo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chiphi_neo
       !momentum-equation, neoclassical diffusivity from NEO model
       !R|units=m^2/s|pclin chiphi_neo(nrho_anomth)

     INTEGER :: id_chiphi_tglf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chiphi_tglf
       !momentum-equation, diffusivity from TGLF model
       !R|units=m^2/s|pclin chiphi_tglf(nrho_anomth)

     INTEGER :: id_chiphi_w19 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chiphi_w19
       !momentum-equation, diffusivity term
       !R|units=m^2/sec|pclin  chiphi_w19(nrho_anomth)

     INTEGER :: id_chitheta_mmm71 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chitheta_mmm71
       !momentum-equation, diffusivity term, poloidal
       !R|units=m^2/sec|pclin  chitheta_mmm71(nrho_anomth)

     INTEGER :: id_db_mtm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: db_mtm
       !dB/B from MTM model
       !R|units=-|pclin db_mtm(nrho_anomth)

     INTEGER :: id_difb_fusi = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: difb_fusi
       !fusion ion anomalous diffusivity
       !R|pclin  difb_fusi(nrho_anom)

     INTEGER :: id_difb_nbi = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: difb_nbi
       !beam ion anomalous diffusivity
       !R|pclin  difb_nbi(nrho_anom)

     INTEGER :: id_difb_rfmi = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: difb_rfmi
       !RF minority ion anomalous diffusivity
       !R|pclin  difb_rfmi(nrho_anom)

     INTEGER :: id_difne_nclass = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: difne_nclass
       !electron-density-equations, neoclassical diffusivity from NCLASS model
       !R|units=m^2/s|pclin difne_nclass(nrho_anomth)

     INTEGER :: id_difnh_nclass = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: difnh_nclass
       !main-hydrogenic-density-equations, neoclassical diffusivity from NCLASS model
       !R|units=m^2/s|pclin difnh_nclass(nrho_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_difns_anom
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: difns_anom
       !density-equations, diffusivity term
       !R|units=m^2/sec|pclin  difns_anom(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_difns_drbm
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: difns_drbm
       !density-equations, diffusivity term
       !R|units=m^2/sec|pclin  difns_drbm(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_difns_glf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: difns_glf
       !density-equations, diffusivity from GLF23 model
       !R|units=m^2/s|pclin difns_glf(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_difns_gtcneo
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: difns_gtcneo
       !density-equations, neoclassical diffusivity from GTC_NEO model
       !R|units=m^2/s|pclin difns_gtcneo(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_difns_mmm71
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: difns_mmm71
       !density-equations, diffusivity term
       !R|units=m^2/sec|pclin  difns_mmm71(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_difns_nclass
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: difns_nclass
       !density-equations, neoclassical diffusivity from NCLASS model
       !R|units=m^2/s|pclin difns_nclass(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_difns_neo
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: difns_neo
       !density-equations, neoclassical diffusivity from NEO model
       !R|units=m^2/s|pclin difns_neo(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_difns_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: difns_tglf
       !density-equations, diffusivity from TGLF model
       !R|units=m^2/s|pclin difns_tglf(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_difns_w19
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: difns_w19
       !density-equations, diffusivity term
       !R|units=m^2/sec|pclin  difns_w19(nrho_anomth,0:nspec_th)

     INTEGER :: id_difnx_nclass = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: difnx_nclass
       !main-impurity-density-equations, neoclassical diffusivity from NCLASS model
       !R|units=m^2/s|pclin difnx_nclass(nrho_anomth)

     INTEGER :: id_difnz_mmm71 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: difnz_mmm71
       !impurity ion diffusivity term
       !R|units=m^2/sec|pclin  difnz_mmm71(nrho_anomth)

     INTEGER :: id_dQedGrdte_tglf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: dQedGrdte_tglf
       !Te flux gradient term from TGLF model
       !R|units=1/(m*s)|pclin dQedGrdte_tglf(nrho_anomth)

     INTEGER :: id_dQedGrdti_tglf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: dQedGrdti_tglf
       !Te flux gradient term from TGLF model
       !R|units=1/(m*s)|pclin dQedGrdti_tglf(nrho_anomth)

     INTEGER :: id_dQidGrdte_tglf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: dQidGrdte_tglf
       !Ti flux gradient term from TGLF model
       !R|units=1/(m*s)|pclin dQidGrdte_tglf(nrho_anomth)

     INTEGER :: id_dQidGrdti_tglf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: dQidGrdti_tglf
       !Ti flux gradient term from TGLF model
       !R|units=1/(m*s)|pclin dQidGrdti_tglf(nrho_anomth)

     INTEGER :: id_ee_mobility_factor = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: ee_mobility_factor
       !chi_e/chi_i
       !ratio of electron heat diffusivity to ion heat diffusivity, a model input
       !(does not effect relative non-diffusive heat pinch velocities).
       !R|units=-|Hermite  ee_mobility_factor(nrho_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_fidiff_bpass_co
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: fidiff_bpass_co
       !fusion ion diffusivity: co barely passing orbits
       !R|units=m^2/sec|pclin  fidiff_bpass_co(nefi_anom2,nrho_anom2,nspec_fusion)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_fidiff_bpass_ctr
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: fidiff_bpass_ctr
       !fusion ion diffusivity: ctr barely passing orbits
       !R|units=m^2/sec|pclin  fidiff_bpass_ctr(nefi_anom2,nrho_anom2,nspec_fusion)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_fidiff_btrap
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: fidiff_btrap
       !fusion ion diffusivity: barely trapped orbits
       !R|units=m^2/sec|pclin  fidiff_btrap(nefi_anom2,nrho_anom2,nspec_fusion)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_fidiff_dpass_co
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: fidiff_dpass_co
       !fusion ion diffusivity: co passing |vpll/v| -> 1
       !R|units=m^2/sec|pclin  fidiff_dpass_co(nefi_anom2,nrho_anom2,nspec_fusion)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_fidiff_dpass_ctr
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: fidiff_dpass_ctr
       !fusion ion diffusivity: ctr passing |vpll/v| -> 1
       !R|units=m^2/sec|pclin  fidiff_dpass_ctr(nefi_anom2,nrho_anom2,nspec_fusion)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_fidiff_dtrap
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: fidiff_dtrap
       !fusion ion diffusivity: deeply trapped orbits
       !R|units=m^2/sec|pclin  fidiff_dtrap(nefi_anom2,nrho_anom2,nspec_fusion)

     INTEGER :: id_freq_drbm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: freq_drbm
       !frequency from DRBM model
       !R|units=1/sec|pclin freq_drbm(nrho_anomth)

     INTEGER :: id_freq_etg = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: freq_etg
       !frequency from ETG model
       !R|units=1/sec|pclin freq_etg(nrho_anomth)

     INTEGER :: id_freq_glf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: freq_glf
       !frequency from GLF23 model
       !R|units=GB|pclin freq_glf(nrho_anomth)

     INTEGER :: id_freq_max_tglf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: freq_max_tglf
       !frequency corresponding maximum growthrate from TGLF model
       !R|units=GB|pclin freq_max_tglf(nrho_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_freq_mmm71
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: freq_mmm71
       !frequency from mmm71 model
       !R|units=1/sec|pclin freq_mmm71(nrho_anomth,nky_mmm)

     INTEGER :: id_freq_mtm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: freq_mtm
       !frequency from MTM model
       !R|units=1/sec|pclin freq_mtm(nrho_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_freq_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: freq_tglf
       !frequency from TGLF model
       !R|units=GB|pclin freq_tglf(nrho_anomth,nky_anomth)

     INTEGER :: id_growthrate_drbm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: growthrate_drbm
       !growthrate from DRBM model
       !R|units=1/sec|pclin growthrate_drbm(nrho_anomth)

     INTEGER :: id_growthrate_etg = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: growthrate_etg
       !growthrate from ETG model
       !R|units=1/sec|pclin growthrate_etg(nrho_anomth)

     INTEGER :: id_growthrate_glf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: growthrate_glf
       !growthrate from GLF23 model
       !R|units=GB|pclin growthrate_glf(nrho_anomth)

     INTEGER :: id_growthrate_max_tglf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: growthrate_max_tglf
       !maximum growthrate from TGLF model
       !R|units=GB|pclin growthrate_max_tglf(nrho_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_growthrate_mmm71
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: growthrate_mmm71
       !growthrate from mmm71 model
       !R|units=1/sec|pclin growthrate_mmm71(nrho_anomth,nky_mmm)

     INTEGER :: id_growthrate_mtm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: growthrate_mtm
       !growthrate from MTM model
       !R|units=1/sec|pclin growthrate_mtm(nrho_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_growthrate_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: growthrate_tglf
       !growthrate from TGLF model
       !R|units=GB|pclin growthrate_tglf(nrho_anomth,nky_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_growthrate_vpar
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: growthrate_vpar
       !growthrate from parallel velocity shear
       !R|units=GB|pclin growthrate_vpar(nrho_anomth,0:nspec_th)

     INTEGER :: id_kyrhos_drbm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: kyrhos_drbm
       !ky*rhos at max growth rate for DRBM model
       !R|units=-|pclin kyrhos_drbm(nrho_anomth)

     INTEGER :: id_kyrhos_etg = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: kyrhos_etg
       !ky*rhos at max growth rate for ETG model
       !R|units=-|pclin kyrhos_etg(nrho_anomth)

     INTEGER :: id_kyrhos_mtm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: kyrhos_mtm
       !ky*rhos at max growth rate for MTM model
       !R|units=-|pclin kyrhos_mtm(nrho_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_kyspectrum_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: kyspectrum_tglf
       !ky-spectrum from TGLF model
       !R|units=-|pclin kyspectrum_tglf(nrho_anomth,nky_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_neflux_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: neflux_ky_tglf
       !electron density flux spectrum
       !R|units=GB|pclin neflux_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_ne_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: ne_ky_tglf
       !electron density fluctuation spectrum
       !R|units=GB|pclin ne_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_niflux_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: niflux_ky_tglf
       !ion density flux spectrum
       !R|units=GB|pclin niflux_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_ni_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: ni_ky_tglf
       !ion density fluctuation spectrum
       !R|units=GB|pclin ni_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER :: id_nmodel = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: nmodel
       !target density profile
       !from experiment or "profile consistency" model
       !R|units=m^-3|step  nmodel(~nrho_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_nzflux_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: nzflux_ky_tglf
       !impurity density flux spectrum
       !R|units=GB|pclin nzflux_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_nz_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: nz_ky_tglf
       !impurity density fluctuation spectrum
       !R|units=GB|pclin nz_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER :: id_omega_exb = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: omega_exb
       !ExB flow shear rate
       !R|units=GB|pclin omega_exb(nrho_anomth)

     INTEGER :: id_pe_anom = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pe_anom
       !anomalous electron heating
       !R|units=W|step*dV  pe_anom(~nrho_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_phiflux_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: phiflux_ky_tglf
       !momentum fluctuation spectrum
       !R|units=GB|pclin phiflux_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER :: id_pi_anom = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pi_anom
       !anomalous ion heating
       !R|units=W|step*dV  pi_anom(~nrho_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_pot_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: pot_ky_tglf
       !potential fluctuation spectrum
       !R|units=GB|pclin pot_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_sc_anom
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: sc_anom
       !anomalous sources
       !R|units=#/sec|step*dV sc_anom(~nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_teflux_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: teflux_ky_tglf
       !electron density flux spectrum
       !R|units=GB|pclin teflux_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_te_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: te_ky_tglf
       !electron density fluctuation spectrum
       !R|units=GB|pclin te_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_tiflux_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: tiflux_ky_tglf
       !ion density flux spectrum
       !R|units=GB|pclin tiflux_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_ti_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: ti_ky_tglf
       !ion density fluctuation spectrum
       !R|units=GB|pclin ti_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER :: id_tmodel = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: tmodel
       !target temperature profile
       !from experiment or "profile consistency" model
       !R|units=keV|step*nmodel   tmodel(~nrho_anomth)

     INTEGER :: id_tq_anom = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: tq_anom
       !anomalous torque
       !R|units=Nt*m|step*dV  tq_anom(~nrho_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_tzflux_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: tzflux_ky_tglf
       !impurity density flux spectrum
       !R|units=GB|pclin tzflux_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_tz_ky_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: tz_ky_tglf
       !impurity density fluctuation spectrum
       !R|units=GB|pclin tz_ky_tglf(nrho_anomth,nky_anomth)

     INTEGER :: id_velb_fusi = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velb_fusi
       !fusion ion anom. radial velocity
       !R|pclin  velb_fusi(nrho_anom)

     INTEGER :: id_velb_nbi = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velb_nbi
       !beam ion anom. radial velocity
       !R|pclin  velb_nbi(nrho_anom)

     INTEGER :: id_velb_rfmi = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velb_rfmi
       !RF minority ion anom. radial velocity
       !R|pclin  velb_rfmi(nrho_anom)

     INTEGER :: id_velne_nclass = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velne_nclass
       !electron-density-equations, advective velocity term from NCLASS model
       !R|units=m/sec|pclin velne_nclass(nrho_anomth)

     INTEGER :: id_velnh_nclass = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velnh_nclass
       !main-hydrogenic-density-equations, advective velocity term from NCLASS model
       !R|units=m/sec|pclin velnh_nclass(nrho_anomth)

     INTEGER :: id_velni_drbm = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velni_drbm
       !ion convective velocity
       !R|units=m/s|pclin velni_drbm(nrho_anomth)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_velns_anom
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: velns_anom
       !density-equations, advective velocity term
       !R|units=m/sec|pclin  velns_anom(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_velns_glf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: velns_glf
       !density-equations, advective velocity term from GLF23 model
       !R|units=m/sec|pclin  velns_glf(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_velns_gtcneo
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: velns_gtcneo
       !density-equations, advective velocity term from GTC_NEO model
       !R|units=m/sec|pclin  velns_gtcneo(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_velns_mmm71
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: velns_mmm71
       !density-equations, advective velocity term from MMM71 model
       !R|units=m/sec|pclin  velns_mmm71(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_velns_nclass
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: velns_nclass
       !density-equations, advective velocity term from NCLASS model
       !R|units=m/sec|pclin velns_nclass(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_velns_neo
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: velns_neo
       !density-equations, advective velocity term from NEO model
       !R|units=m/sec|pclin  velns_neo(nrho_anomth,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_velns_tglf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: velns_tglf
       !density-equations, advective velocity term from TGLF model
       !R|units=m/sec|pclin velns_tglf(nrho_anomth,0:nspec_th)

     INTEGER :: id_velnx_nclass = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velnx_nclass
       !main-impurity-density-equations, advective velocity term from NCLASS model
       !R|units=m/sec|pclin velnx_nclass(nrho_anomth)

     INTEGER :: id_velpphi_anom = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velpphi_anom
       !momentum-equation, advective velocity term
       !R|units=m/sec|pclin  velpphi_anom(nrho_anomth)

     INTEGER :: id_velpphi_glf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velpphi_glf
       !momentum-equation, advective velocity term from GLF23 model
       !R|units=m/sec|pclin  velpphi_glf(nrho_anomth)

     INTEGER :: id_velpphi_gtcneo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velpphi_gtcneo
       !momentum-equation, advective velocity term from GTC_NEO model
       !R|units=m/sec|pclin  velpphi_gtcneo(nrho_anomth)

     INTEGER :: id_velpphi_mmm71 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velpphi_mmm71
       !momentum-equation, advective velocity term from MMM71 model
       !R|units=m/sec|pclin  velpphi_mmm71(nrho_anomth)

     INTEGER :: id_velpphi_neo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velpphi_neo
       !momentum-equation, advective velocity term from NEO model
       !R|units=m/sec|pclin  velpphi_neo(nrho_anomth)

     INTEGER :: id_velpphi_tglf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velpphi_tglf
       !momentum-equation, advective velocity term from TGLF model
       !R|units=m/sec|pclin velpphi_tglf(nrho_anomth)

     INTEGER :: id_velte_anom = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velte_anom
       !Te-equation, advective velocity term
       !R|units=m/sec|pclin  velte_anom(nrho_anomth)

     INTEGER :: id_velte_glf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velte_glf
       !Te-equation, advective velocity term from GLF23 model
       !R|units=m/sec|pclin  velte_glf(nrho_anomth)

     INTEGER :: id_velte_gtcneo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velte_gtcneo
       !Te-equation, advective velocity term from GTC_NEO model
       !R|units=m/sec|pclin  velte_gtcneo(nrho_anomth)

     INTEGER :: id_velte_mmm71 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velte_mmm71
       !Te-equation, advective velocity term from MMM71 model
       !R|units=m/sec|pclin  velte_mmm71(nrho_anomth)

     INTEGER :: id_velte_nclass = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velte_nclass
       !Te-equation, advective velocity term from NCLASS model
       !R|units=m/sec|pclin velte_nclass(nrho_anomth)

     INTEGER :: id_velte_neo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velte_neo
       !Te-equation, advective velocity term from NEO model
       !R|units=m/sec|pclin  velte_neo(nrho_anomth)

     INTEGER :: id_velte_paleo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velte_paleo
       !Te-equation, convection term from PALEO model
       !R|units=m/sec|pclin   velte_paleo(nrho_anomth)

     INTEGER :: id_velte_tglf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velte_tglf
       !Te-equation, advective velocity term from TGLF model
       !R|units=m/sec|pclin velte_tglf(nrho_anomth)

     INTEGER :: id_velti_anom = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velti_anom
       !Ti-equation, advective velocity term
       !R|units=m/sec|pclin  velti_anom(nrho_anomth)

     INTEGER :: id_velti_glf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velti_glf
       !Ti-equation, advective velocity term from GLF23 model
       !R|units=m/sec|pclin  velti_glf(nrho_anomth)

     INTEGER :: id_velti_gtcneo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velti_gtcneo
       !Ti-equation, advective velocity term from GTC_NEO model
       !R|units=m/sec|pclin  velti_gtcneo(nrho_anomth)

     INTEGER :: id_velti_mmm71 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velti_mmm71
       !Ti-equation, advective velocity term from MMM71 model
       !R|units=m/sec|pclin  velti_mmm71(nrho_anomth)

     INTEGER :: id_velti_nclass = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velti_nclass
       !Ti-equation, advective velocity term from NCLASS model
       !R|units=m/sec|pclin velti_nclass(nrho_anomth)

     INTEGER :: id_velti_neo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velti_neo
       !Ti-equation, advective velocity term from NEO model
       !R|units=m/sec|pclin  velti_neo(nrho_anomth)

     INTEGER :: id_velti_tglf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: velti_tglf
       !Ti-equation, advective velocity term from TGLF model
       !R|units=m/sec|pclin velti_tglf(nrho_anomth)

     !----------------------------
     ! Component: EC

     INTEGER :: id_curech = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: curech
       !ECH current drive
       !R|step*dA|units=A   curech(~nrho_ecrf)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_curech_src
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: curech_src
       !ECH current drive (by antenna)
       !R|step*dA|units=A   curech_src(~nrho_ecrf,necrf_src)

     INTEGER :: id_peech = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: peech
       !electron heating by ECH
       !R|step*dV|units=W   peech(~nrho_ecrf)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_peech_src
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: peech_src
       !ECH heating (by antenna)
       !R|step*dV|units=W   peech_src(~nrho_ecrf,necrf_src)

     !----------------------------
     ! Component: EQ

     INTEGER :: id_area = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: area
       !enclosed area
       !R|units=m^2|Spline_00 area(nrho_eq)

     INTEGER :: id_BphiRZ = 0
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: BphiRZ
       !Toroidal field
       !R|units=T|Hermite      BphiRZ(nR,nZ)

     INTEGER :: id_BRRZ = 0
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: BRRZ
       !R component of poloidal field
       !R|units=T|Hermite      BRRZ(nR,nZ)

     INTEGER :: id_BZRZ = 0
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: BZRZ
       !Z component of poloidal field
       !R|units=T|Hermite      BZRZ(nR,nZ)

     INTEGER :: id_B_surfMax = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: B_surfMax
       !max mod(B) on flux surface
       !R|pclin  B_surfMax(nrho_eq_geo)

     INTEGER :: id_B_surfMin = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: B_surfMin
       !min mod(B) on flux surface
       !R|pclin  B_surfMin(nrho_eq_geo)

     INTEGER :: id_curt = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: curt
       !total enclosed toroidal current
       !this includes <j.B> contributions and <j x B> contributions and
       !is related to the poloidal field:
       !mu0*I(rho) = rho*phit(nrho_eq)/(q(rho)*2*pi*pi)
       !* grho2r2i(rho) * (dVol/drho)
       !(From Ampere's Law, mu0*I_phi = loop integral (B_pol*dl)
       != loop integral (dl * grad(psi)/R)
       != rho*phit(nrho_eq)/(q(rho)*pi) *
       !loop integral (dl * grad(rho)/R)
       !(2*pi) * loop integral (dl * grad(rho)/rho)
       != loop integral (dl * 2*pi*R/grad(rho) * grad(rho)**2/R**2)
       != grho2r2i(rho) * dVol/drho
       !derivation depends on the definition of the radial coord.:
       !rho = sqrt(phit(rho)/phit(rho=1))
       !curt(nrho_eq) gives the total current, >0 by convention;
       !see kccw_Jphi, +/-1, for direction of toroidal current.
       !R|units=A|Spline_00   curt(nrho_eq)

       ! (Hermite derivative):
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: d2R_geo_dRHOdTH
       !flux surfaces R(rho,theta)
       !R|units=m|Hermite_explicit R_geo(nrho_eq,nth_eq)

       ! (Hermite derivative):
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: d2Z_geo_dRHOdTH
       !flux surfaces Z(rho,theta)
       !R|units=m|Hermite_explicit Z_geo(nrho_eq,nth_eq)

       ! (Hermite derivative):
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: dR0_momeq_dRHO
       !R0 of flux surface
       !Fourier Spline representation
       !R|units=m|Hermite_explicit  R0_momeq(nrho_eq)

       ! (Hermite derivative):
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: dR_geo_dRHO
       !flux surfaces R(rho,theta)
       !R|units=m|Hermite_explicit R_geo(nrho_eq,nth_eq)

       ! (Hermite derivative):
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: dR_geo_dTH
       !flux surfaces R(rho,theta)
       !R|units=m|Hermite_explicit R_geo(nrho_eq,nth_eq)

       ! (Hermite derivative):
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: dxRjcos_momeq_dRHO
       !scaled R cos moments
       !Fourier Spline representation
       !R|units=m|Hermite_explicit  xRjcos_momeq(nrho_eq,neqmom)

       ! (Hermite derivative):
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: dxRjsin_momeq_dRHO
       !scaled R sin moments
       !Fourier Spline representation
       !R|units=m|Hermite_explicit  xRjsin_momeq(nrho_eq,neqmom)

       ! (Hermite derivative):
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: dxZjcos_momeq_dRHO
       !scaled Z cos moments
       !Fourier Spline representation
       !R|units=m|Hermite_explicit  xZjcos_momeq(nrho_eq,neqmom)

       ! (Hermite derivative):
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: dxZjsin_momeq_dRHO
       !scaled Z sin moments
       !Fourier Spline representation
       !R|units=m|Hermite_explicit  xZjsin_momeq(nrho_eq,neqmom)

       ! (Hermite derivative):
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: dZ0_momeq_dRHO
       !Z0 of flux surface
       !Fourier Spline representation
       !R|units=m|Hermite_explicit  Z0_momeq(nrho_eq)

       ! (Hermite derivative):
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: dZ_geo_dRHO
       !flux surfaces Z(rho,theta)
       !R|units=m|Hermite_explicit Z_geo(nrho_eq,nth_eq)

       ! (Hermite derivative):
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: dZ_geo_dTH
       !flux surfaces Z(rho,theta)
       !R|units=m|Hermite_explicit Z_geo(nrho_eq,nth_eq)

     INTEGER :: id_elong = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: elong
       !elongation (b/a)
       !(Zmax-Zmin)/(Rmax-Rmin) on each surface
       !R|pclin  elong(nrho_eq_geo)

     INTEGER :: id_gamma_nc = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: gamma_nc
       !NC gamma
       !2pi/int(0 to 2pi)[dtheta*(B/B.grad(theta))]
       !R|units=m^-1|pclin   gamma_nc(nrho_eq_geo)

     INTEGER :: id_gb1 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: gb1
       !<|B|>
       !R|units=T|pclin      gb1(nrho_eq_geo)

     INTEGER :: id_gb2 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: gb2
       !<B^2>
       !R|units=T^2|pclin    gb2(nrho_eq_geo)

     INTEGER :: id_gb2i = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: gb2i
       !<1/B^2>
       !R|units=T^-2|pclin   gb2i(nrho_eq_geo)

     INTEGER :: id_gbr2 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: gbr2
       !<|B|*R^2>
       !R|units=T*m^2|pclin  gbr2(nrho_eq_geo)

     INTEGER :: id_gncfb2h = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: gncfb2h
       !<(B^-2)*((1-H)^0.5-(1/3)*(1-H)^1.5)>; H=B/Bmax
       !R|units=T^-2|pclin  gncfb2h(nrho_eq_geo)

     INTEGER :: id_gncfh = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: gncfh
       !<H^-2*(1-SQRT(1-H)*(1+H/2))>; H=B/Bmax
       !R|units=-|pclin  gncfh(nrho_eq_geo)

     INTEGER :: id_gr1 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: gr1
       !<R>
       !R|units=m|pclin   gr1(nrho_eq_geo)

     INTEGER :: id_gr2 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: gr2
       !<R^2>
       !R|units=m^2|pclin gr2(nrho_eq_geo)

     INTEGER :: id_gr2i = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: gr2i
       !<R^-2>
       !R|units=m^-2|pclin gr2i(nrho_eq_geo)

     INTEGER :: id_gr2rho2 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: gr2rho2
       !<R^2*|grad(rho)|^2>
       !R|units=-|pclin gr2rho2(nrho_eq_geo)

     INTEGER :: id_gr3i = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: gr3i
       !<R^-3>
       !R|units=m^-3|pclin gr3i(nrho_eq_geo)

     INTEGER :: id_grho1 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: grho1
       !<|grad(rho)|>
       !R|units=m^-1|pclin grho1(nrho_eq_geo)

     INTEGER :: id_grho2 = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: grho2
       !<|grad(rho)|^2>
       !R|units=m^-2|pclin grho2(nrho_eq_geo)

     INTEGER :: id_grho2b2i = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: grho2b2i
       !<|grad(rho)|^2/B^2>
       !R|units=T^-2*m^-2|pclin grho2b2i(nrho_eq_geo)

     INTEGER :: id_grho2r2i = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: grho2r2i
       !<|grad(rho)|^2/R^2>
       !R|units=m^-4|pclin grho2r2i(nrho_eq_geo)

     INTEGER :: id_grho2r3i = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: grho2r3i
       !<|grad(rho)|^2/R^3>
       !R|units=m^-5|pclin grho2r3i(nrho_eq_geo)

     INTEGER :: id_gri = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: gri
       !<1/R>
       !R|units=m^-2|pclin gri(nrho_eq_geo)

     INTEGER :: id_grirhoi = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: grirhoi
       !<1/(R*|grad(rho)|)>
       !R|units=-|pclin grirhoi(nrho_eq_geo)

     INTEGER :: id_g_eq = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: g_eq
       !equilibrium R*|B_phi|
       !By convention this profile is always positive; direction
       !of B_phi is taken from element kccw_Bphi:
       !B_phi = kccw_Bphi*g/R
       !R|units=T*m|Spline_00      g_eq(nrho_eq)

     INTEGER :: id_jdotb = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: jdotb
       !<J.B>
       !Parallel current * mod(B) -- note sign convention:
       !Local positive values indicate local current flow
       !in the direction of the main toroidal current; i.e. actually
       !kccw_Jphi * kccw_Bphi * <J.B> is stored.  So, even if the
       !main toroidal current and the toroidal field are anti-parallel,
       !jdotb is generally positive, but there can be localized regions
       !of negative values, usually near the plasma edge.
       !From a parallel Ampere's Law expression, <J.B> can be expressed
       !as a combination of derivatives of toroidal current and toroidal
       !field functions curt(rho) and g(rho):
       !<J.B> = 2pi*[g*d(curt)/drho-curt*dg/drho]/(dVol/drho)
       !R|units=A*T/m^2|Hermite_00     jdotb(nrho_eq)

     INTEGER :: id_Lpol = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Lpol
       !poloidal path length
       !R|units=m|Spline Lpol(nrho_eq)

     INTEGER :: id_ngradb2_av = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: ngradb2_av
       !<(B.grad(B)/|B|)^2>
       !average of square of grad(B) along field line
       !R|units=T^2*m^-2|pclin   ngradb2_av(nrho_eq_geo)

     INTEGER :: id_phit = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: phit
       !toroidal flux vs. rho
       !By convention, this profile always positive; 0 on axis.
       !See kccw_Bphi for specification of direction of toroidal field.
       !R|units=Wb|Spline_00       phit(nrho_eq)

     INTEGER :: id_psipol = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: psipol
       !poloidal flux vs. rho
       !By convention, this profile always positive; 0 on axis.
       !-- see kccw_Jphi; see also psi_to_machine_axis
       !for specification of direction (sign) of toroidal current and
       !hence the poloidal magnetic field.
       !R|units=Wb/rad|Spline_00   psipol(nrho_eq)

     INTEGER :: id_PsiRZ = 0
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: PsiRZ
       !Psi(R,Z)
       !Note on Psi(rho) and Psi(R,Z) both: convention is to set
       !Psi=0 at the magnetic axis and increasing towards the plasma
       !boundary.  Direction of poloidal field is taken from element
       !kccw_Jphi:
       !
       !BR = kccw_Jphi*(1/R)*d(Psi/dZ)
       !BZ =-kccw_Jphi*(1/R)*d(Psi/dR)
       !
       !see also: Psi_to_machine_axis.
       !R|units=Wb/rad|Spline  PsiRZ(nR,nZ)

     INTEGER :: id_psmom_errck = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: psmom_errck
       !PS moments sum convergence
       !|ngradb2_av(:)-sum[j=1:npsmom]psmom_nc(:,j)/gb2(:)|/maxval(|ngradb2_av(:)|)
       !R|units=-|pclin       psmom_errck(nrho_eq_geo)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_psmom_nc
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: psmom_nc
       !PS moments
       !(1:npsmom) Pfirsch-Schlutter moments (NCLASS)
       !convergence check: sum[j=1:infinity](psmom_nc(:,j))= gb2(:)*ngradb2_av(:)
       !R|units=m^-2|pclin    psmom_nc(nrho_eq_geo,npsmom)

     INTEGER :: id_P_eq = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: P_eq
       !equilibrium scalar pressure
       !R|units=Pa|Spline_00       P_eq(nrho_eq)

     INTEGER :: id_q_eq = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: q_eq
       !equilibrium q profile
       !R|units=-|Spline           q_eq(nrho_eq)

     INTEGER :: id_R0_momeq = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: R0_momeq
       !R0 of flux surface
       !Fourier Spline representation
       !R|units=m|Hermite_explicit  R0_momeq(nrho_eq)

     INTEGER :: id_Rmajor_mean = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Rmajor_mean
       !Rmajor_mean = (R_Surfmax+R_surfmin)/2
       !R|pclin  Rmajor_mean(nrho_eq_geo)

     INTEGER :: id_rMinor_mean = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: rMinor_mean
       !rMinor_mean = (R_Surfmax-R_surfmin)/2
       !R|pclin  rMinor_mean(nrho_eq_geo)

     INTEGER :: id_R_geo = 0
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: R_geo
       !flux surfaces R(rho,theta)
       !R|units=m|Hermite_explicit R_geo(nrho_eq,nth_eq)

     INTEGER :: id_R_midp_in = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: R_midp_in
       !R (high field side) midplane intercept
       !R|pclin  R_midp_in(nrho_eq_geo)

     INTEGER :: id_R_midp_out = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: R_midp_out
       !R (low field side) midplane intercept
       !R|pclin  R_midp_out(nrho_eq_geo)

     INTEGER :: id_R_surfMax = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: R_surfMax
       !max R on flux surface
       !R|pclin  R_surfMax(nrho_eq_geo)

     INTEGER :: id_R_surfMin = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: R_surfMin
       !min R on flux surface
       !R|pclin  R_surfMin(nrho_eq_geo)

     INTEGER :: id_squareLO = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: squareLO
       !lower outer squareness
       !lower half of plasma, large major radius side
       !definition per C. T. Holcomb et al,
       !Physics of Plasmas 15, 056116 (2009)
       !R|pclin  squareLO(nrho_eq_geo)

     INTEGER :: id_squareUO = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: squareUO
       !upper outer squareness
       !upper half of plasma, large major radius side
       !definition per C. T. Holcomb et al,
       !Physics of Plasmas 15, 056116 (2009)
       !R|pclin  squareUO(nrho_eq_geo)

     INTEGER :: id_surf = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: surf
       !area of flux surfaces
       !R|units=m^2|Spline surf(nrho_eq)

     INTEGER :: id_triang = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: triang
       !triangularity (symmetrized)
       !((Rmin+Rmax)/2 - min(R(Zmax),R(Zmin)))/((Rmax-Rmin)/2)
       !R|pclin  triang(nrho_eq_geo)

     INTEGER :: id_triangL = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: triangL
       !lower triangularity
       !((Rmin+Rmax)/2 - R(Zmin))/((Rmax-Rmin)/2)
       !R|pclin  triangL(nrho_eq_geo)

     INTEGER :: id_triangU = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: triangU
       !upper triangularity
       !((Rmin+Rmax)/2 - R(Zmax))/((Rmax-Rmin)/2)
       !R|pclin  triangU(nrho_eq_geo)

     INTEGER :: id_triang_miller_L = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: triang_miller_L
       !Miller lower triangularity
       !Sin(aCos((R(Zmin)-Rmajor_mean)/rMinor_mean)-pi/2).
       !R|pclin  triang_miller_L(nrho_eq_geo)

     INTEGER :: id_triang_miller_U = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: triang_miller_U
       !Miller upper triangularity
       !Sin(aCos((R(Zmax)-Rmajor_mean)/rMinor_mean)-pi/2).
       !R|pclin  triang_miller_U(nrho_eq_geo)

     INTEGER :: id_vol = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: vol
       !enclosed volume
       !R|units=m^3|Spline_00  vol(nrho_eq)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_xRjcos_momeq
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: xRjcos_momeq
       !scaled R cos moments
       !Fourier Spline representation
       !R|units=m|Hermite_explicit  xRjcos_momeq(nrho_eq,neqmom)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_xRjsin_momeq
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: xRjsin_momeq
       !scaled R sin moments
       !Fourier Spline representation
       !R|units=m|Hermite_explicit  xRjsin_momeq(nrho_eq,neqmom)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_xZjcos_momeq
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: xZjcos_momeq
       !scaled Z cos moments
       !Fourier Spline representation
       !R|units=m|Hermite_explicit  xZjcos_momeq(nrho_eq,neqmom)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_xZjsin_momeq
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: xZjsin_momeq
       !scaled Z sin moments
       !Fourier Spline representation
       !R|units=m|Hermite_explicit  xZjsin_momeq(nrho_eq,neqmom)

     INTEGER :: id_Z0_momeq = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Z0_momeq
       !Z0 of flux surface
       !Fourier Spline representation
       !R|units=m|Hermite_explicit  Z0_momeq(nrho_eq)

     INTEGER :: id_Z_geo = 0
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: Z_geo
       !flux surfaces Z(rho,theta)
       !R|units=m|Hermite_explicit Z_geo(nrho_eq,nth_eq)

     INTEGER :: id_Z_midp = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Z_midp
       !flux surface midplane elevation
       !R|pclin  Z_midp(nrho_eq_geo)

     INTEGER :: id_Z_surfMax = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Z_surfMax
       !max Z on flux surface
       !R|pclin  Z_surfMax(nrho_eq_geo)

     INTEGER :: id_Z_surfMin = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Z_surfMin
       !min Z on flux surface
       !R|pclin  Z_surfMin(nrho_eq_geo)

     !----------------------------
     ! Component: FUS

     INTEGER :: id_curfusn = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: curfusn
       !fusion ion driven current (shielded)
       !R|step*dA|units=A   curfusn(~nrho_fus)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_eperp_fusi
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: eperp_fusi
       !fusion ion <Eperp>, lab frame
       !R|units=keV|alias=eperp_|step*nfusi eperp_fusi(~nrho_fus,nspec_fusion)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_epll_fusi
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: epll_fusi
       !fusion ion <Epll>, lab frame
       !R|units=keV|alias=epll_|step*nfusi  epll_fusi(~nrho_fus,nspec_fusion)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_nfusi
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: nfusi
       !fusion ion density
       !R|units=m^-3|alias=n|step  nfusi(~nrho_fus,nspec_fusion)

     INTEGER :: id_pfuse = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pfuse
       !electron heating by all fusion ions
       !R|step*dV   pfuse(~nrho_fus)

     INTEGER :: id_pfusi = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pfusi
       !thermal ion heating by all fusion ions
       !R|step*dV   pfusi(~nrho_fus)

     INTEGER :: id_pfusth = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pfusth
       !thermalization of fusion ions
       !R|step*dV   pfusth(~nrho_fus)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_rate_sinf0i
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: rate_sinf0i
       !sink rate due to ionization by fusion ions
       !R|step|units=1/sec       rate_sinf0i(~nrho_fus,nspec_gas)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_rate_sinf0x
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: rate_sinf0x
       !sink rate due to CX with fusion ions (total)
       !R|step|units=1/sec       rate_sinf0x(~nrho_fus,nspec_gas)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: id_rate_sinf0xs
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: rate_sinf0xs
       !sink rate due to CX with fusion ions
       !R|step|units=1/sec       rate_sinf0xs(~nrho_fus,nspec_gas,nspec_fusion)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_sfsce
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: sfsce
       !net thermalization source of fusion ions
       !sources due to fusion ions:  fusion ion thermalization;
       !corrections due to charge exchange effects (usually small).
       !R|step*dV|units=#/sec   sfsce(~nrho_fus,0:nspec_tha)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_sftherm
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: sftherm
       !fusion ion thermalization
       !R|step*dV|units=#/sec   sftherm(~nrho_fus,nspec_fusion)

     !----------------------------
     ! Component: GAS

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: id_n0norm
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: n0norm
       !neutral density per unit gas influx (sc0)
       !for each neutral specie due to each source
       !R|step|units=(m^-3)/(#/sec)    n0norm(~nrho_gas,nspec_gas,ngsc0)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_n0_reco
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: n0_reco
       !reco thermal neutral density
       !R|units=m^-3|step         n0_reco(~nrho_gas,nspec_gas)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_omeg0cx
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: omeg0cx
       !effective neutral angular velocity for CX torque
       !(use with tqqcx)
       !R|step*tqqcx|units=rad/sec  omeg0cx(~nrho_gas,ngsc0)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: id_omeg0sc0
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: omeg0sc0
       !toroidal angular velocity
       !for each neutral specie due to each influx (sc0).
       !R|step*n0norm|units=(rad/sec)  omeg0sc0(~nrho_gas,nspec_gas,ngsc0)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_omeg0_reco
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: omeg0_reco
       !reco neutral toroidal angular velocity
       !R|units=rad/sec|step*n0_reco  omeg0_reco(~nrho_gas,nspec_gas)

     INTEGER :: id_p0_reco = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: p0_reco
       !power in recombination neutral source
       !(loss from thermal ions)
       !R|step*dV   p0_reco(~nrho_gas)

     INTEGER :: id_pcx_reco = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pcx_reco
       !reco driven CX power
       !(>0 => local source to thermal ions)
       !(no associated particle source/sink)
       !R|step*dV   pcx_reco(~nrho_gas)

     INTEGER :: id_psc_reco = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: psc_reco
       !reco power: recapture - p0_reco
       !(net source/sink to thermal ions)
       !(associated particle source/sink exists)
       !R|step*dV   psc_reco(~nrho_gas)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_qioniz
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: qioniz
       !ionization power per unit influx (sc0)
       !R|step*dV|units=W/(#/sec)     qioniz(~nrho_gas,ngsc0)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_qqcx
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: qqcx
       !cx power per unit influx per (T0-Ti)
       !R|step*dV|units=W/(#/sec)/keV  qqcx(~nrho_gas,ngsc0)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_s0reco
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: s0reco
       !recombination neutral sce
       !psc_reco =~ (3/2)*Ti*(s0reco_recap-s0reco)*1.602e-16
       !note: electron sink = sum (Z(i)*s0reco(:,i))
       !where Z(i) is the number of electrons carried by a neutral atom
       !of species (i).
       !R|step*dV|units=#/sec s0reco(~nrho_gas,nspec_gas)

     INTEGER :: id_s0reco_e = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: s0reco_e
       !net electron source associated with recombination:
       !sum[-Z(i)*s0reco(:,i) + Z(i)*s0reco_recap(:,i)]
       !R|step*dV|units=#/sec s0reco_e(~nrho_gas)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_s0reco_recap
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: s0reco_recap
       !recombination neutrals recapture
       !recapture of recombination source thermal neutrals -- ion source
       !psc_reco =~ (3/2)*Ti*(sb0reco_recap-sb0reco)*1.602e-16
       !R|step*dV|units=#/sec s0reco_recap(~nrho_gas,nspec_gas)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: id_sprof0
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: sprof0
       !gas species source/sinks per unit species influx
       !due to charge exchange, single species neutral
       !source (sc0(i)) leads to multi species ion sce.
       !remark (Jan. 2009):
       !charge exchange can affect impurity charge
       !states -- may need to add profiles to track
       !this interaction, in the future...
       !R|step*dV|units=(#/sec)/(#/sec)  sprof0(~nrho_gas,nspec_gas,ngsc0)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_sprof0e
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: sprof0e
       !electron source per unit neutral species influx
       !R|step*dV|units=(#/sec)/(#/sec)  sprof0e(~nrho_gas,ngsc0)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_T0cx
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: T0cx
       !effective neutral temperature for CX power
       !(use with qqcx)
       !R|step*qqcx|units=keV  T0cx(~nrho_gas,ngsc0)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: id_T0sc0
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: T0sc0
       !neutral temperature (2/3)<E0>
       !in co-rotating frame for each neutral specie
       !due each neutral influx (sc0)
       !R|step*n0norm|units=keV        T0sc0(~nrho_gas,nspec_gas,ngsc0)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_T0_reco
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: T0_reco
       !reco neutral temperature (2/3)<E0>
       !R|units=keV|step*n0_reco  T0_reco(~nrho_gas,nspec_gas)

     INTEGER :: id_tq0_reco = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: tq0_reco
       !torque of recombination neutral source
       !(momentum loss from thermal ions)
       !R|step*dV   tq0_reco(~nrho_gas)

     INTEGER :: id_tqcx_reco = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: tqcx_reco
       !reco driven CX momentum torque
       !(positive value = momentum source)
       !(no associated particle source/sink)
       !R|step*dV   tqcx_reco(~nrho_gas)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_tqioniz
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: tqioniz
       !ionization torque per unit influx (sc0)
       !R|step*dV|units=Nt*m/(#/sec)     tqioniz(~nrho_gas,ngsc0)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_tqqcx
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: tqqcx
       !cx torque per unit influx per (omeg0-omegi)
       !R|step*dV|units=Nt*m/(#/sec)/(rad/sec)  tqqcx(~nrho_gas,ngsc0)

     INTEGER :: id_tqsc_reco = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: tqsc_reco
       !reco torque: recapture - tq0_reco
       !(net source/sink to thermal ions)
       !(associated particle source/sink exists)
       !R|step*dV   tqsc_reco(~nrho_gas)

     !----------------------------
     ! Component: IC

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_cdicrf
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: cdicrf
       !ICRF current drive
       !(summed over nphi)
       !R|units=A|step*dA   cdicrf(~nrho_icrf,nicrf_src)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: id_cdicrf_nphi
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: cdicrf_nphi
       !ICRF current drive
       !contribution from each n_phi on each antenna
       !R|units=A|step*dA   cdicrf_nphi(~nrho_icrf,num_nphi(),nicrf_src)

     INTEGER :: id_curich = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: curich
       !total driven current (all IC srcs)
       !R|units=A|step*dA   curich(~nrho_icrf)

     INTEGER :: id_curmino = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: curmino
       !minority ion driven cur (shielded).
       !R|step*dA|units=A   curmino(~nrho_icrf)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_eperp_mini
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: eperp_mini
       !minority species <Eperp>, lab frame
       !R|units=keV|alias=eperp_|step*nmini eperp_mini(~nrho_icrf,nspec_rfmin)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_epll_mini
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: epll_mini
       !minority species <Epll>, lab frame
       !R|units=keV|alias=epll_|step*nmini  epll_mini(~nrho_icrf,nspec_rfmin)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_nmini
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: nmini
       !minority species density
       !R|units=m^-3|alias=n|step  nmini(~nrho_icrf,nspec_rfmin)

     INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: id_picrf_nphi_srcs
     REAL(KIND=rspec), DIMENSION(:,:,:,:), ALLOCATABLE :: picrf_nphi_srcs
       !direct ICRF power deposition
       !contribution from each n_phi on each antenna
       !R|units=W|alias=picrf_nphi_|step*dV   picrf_nphi_srcs(~nrho_icrf,num_nphi(),nicrf_src,0:nspec_alla)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: id_picrf_srcs
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: picrf_srcs
       !direct ICRF power deposition
       !(sum over nphi)
       !R|units=W|alias=picrfs_|step*dV   picrf_srcs(~nrho_icrf,nicrf_src,0:nspec_alla)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_picrf_totals
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: picrf_totals
       !direct ICRF power deposition
       !(sum over IC sources & nphi)
       !R|units=W|alias=picrf_|step*dV    picrf_totals(~nrho_icrf,0:nspec_alla)

     INTEGER :: id_picth = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: picth
       !direct thermal ion heating by ICRF
       !(sum over thermal ion species & IC sources)
       !R|step*dV|units=W   picth(~nrho_icrf)

     INTEGER :: id_pmine = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pmine
       !electron heating by minority ions
       !R|step*dV   pmine(~nrho_icrf)

     INTEGER :: id_pmini = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pmini
       !thermal ion heating by minority ions
       !R|step*dV   pmini(~nrho_icrf)

     INTEGER :: id_pminth = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pminth
       !(de)thermalization of minority ions
       !R|step*dV   pminth(~nrho_icrf)

     !----------------------------
     ! Component: LH

     INTEGER :: id_curlh = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: curlh
       !LH current drive
       !R|step*dA|units=A   curlh(~nrho_lhrf)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_curlh_src
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: curlh_src
       !LH current drive (by antenna)
       !R|step*dA|units=A   curlh_src(~nrho_lhrf,nlhrf_src)

     INTEGER :: id_pelh = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pelh
       !electron heating by LH
       !R|step*dV   pelh(~nrho_lhrf)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_pelh_src
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: pelh_src
       !LH electron heating by antenna
       !R|step*dV|units=W   pelh_src(~nrho_lhrf,nlhrf_src)

     INTEGER :: id_pilh = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pilh
       !ion heating by LH
       !R|step*dV   pilh(~nrho_lhrf)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_pilh_src
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: pilh_src
       !LH ion heating by antenna
       !R|step*dV|units=W   pilh_src(~nrho_lhrf,nlhrf_src)

     !----------------------------
     ! Component: LMHD

     INTEGER :: id_balstab = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: balstab
       !Ballooning/Mercier stability indicator
       != 0.0: stable;
       != 1.0: Ballooning unstable;
       != 2.0: Mercier unstable
       != 3.0: Both Ballooning and Mercier unstable
       !(note a floating point profile is used; interpolation to another
       !grid would yield non-integer values that if non-zero would indicate
       !instability in the vicinity).
       !R|units=-|pclin balstab(nrho_lmhd)

     !----------------------------
     ! Component: NBI

     INTEGER :: id_curbeam = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: curbeam
       !beam ion driven current (shielded)
       !R|step*dA|units=A   curbeam(~nrho_nbi)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_eperp_beami
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: eperp_beami
       !beam species <Eperp>, lab frame
       !R|units=keV|alias=eperp_|step*nbeami eperp_beami(~nrho_nbi,nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_epll_beami
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: epll_beami
       !beam species <Epll>, lab frame
       !R|units=keV|alias=epll_|step*nbeami  epll_beami(~nrho_nbi,nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_n0_halo
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: n0_halo
       !halo thermal neutral density
       !R|units=m^-3|step         n0_halo(~nrho_nbi,nspec_gas)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_nbeami
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: nbeami
       !beam species density
       !R|units=m^-3|alias=n|step  nbeami(~nrho_nbi,nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_omeg0_halo
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: omeg0_halo
       !halo neutral toroidal angular velocity
       !R|units=rad/sec|step*n0_halo  omeg0_halo(~nrho_nbi,nspec_gas)

     INTEGER :: id_pb0_halo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pb0_halo
       !power in halo neutral source
       !(loss from thermal ions)
       !R|step*dV   pb0_halo(~nrho_nbi)

     INTEGER :: id_pbe = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pbe
       !electron heating by all beam ions
       !R|step*dV   pbe(~nrho_nbi)

     INTEGER :: id_pbi = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pbi
       !thermal ion heating by all beam ions
       !R|step*dV   pbi(~nrho_nbi)

     INTEGER :: id_pbth = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pbth
       !thermalization of beam ions
       !R|step*dV   pbth(~nrho_nbi)

     INTEGER :: id_pcx_halo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pcx_halo
       !beam halo driven CX power
       !(>0 means local source)
       !(no associated particle source/sink)
       !R|step*dV   pcx_halo(~nrho_nbi)

     INTEGER :: id_psc_halo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: psc_halo
       !beam halo power: recapture - pb0_halo
       !(net source/sink to thermal ions)
       !(associated particle source/sink exists)
       !R|step*dV   psc_halo(~nrho_nbi)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_qbeami
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: qbeami
       !average beam ion charge
       !Note, although H & He beam ions will be fully stripped,
       !Impurity beams (Ne, Xe, Ar, Kr) may not be...
       !<state>%q_snbi(1:<state>%nspec_beam) will contain the
       !density weighted volume average charge over the
       !profile: ps_xe for H beams, 2*ps_xe for He beams...
       !R|units=C|step*nbeami qbeami(~nrho_nbi,nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_rate_sinb0i
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: rate_sinb0i
       !sink rate due to ionization by beam ions
       !R|step|units=1/sec       rate_sinb0i(~nrho_nbi,nspec_gas)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_rate_sinb0x
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: rate_sinb0x
       !sink rate due to CX with beam ions (total)
       !R|step|units=1/sec       rate_sinb0x(~nrho_nbi,nspec_gas)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: id_rate_sinb0xs
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: rate_sinb0xs
       !sink rate due to CX with beam ions
       !R|step|units=1/sec       rate_sinb0xs(~nrho_nbi,nspec_gas,nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_sb0halo
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: sb0halo
       !beam dep cx sink
       !thermal neutrals created by fast neutral deposition and/or recapture
       !of fast neutrals
       !psc_halo =~ (3/2)*Ti*(sb0halo_recap-sb0halo)*1.602e-16
       !R|step*dV|units=#/sec sb0halo(~nrho_nbi,nspec_gas)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_sb0halo_recap
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: sb0halo_recap
       !cx neutral recapture
       !recapture of thermal neutrals -- ion source
       !psc_halo =~ (3/2)*Ti*(sb0halo_recap-sb0halo)*1.602e-16
       !(cross species CX source/sink contributions may occur)
       !note: electron recapture, cold source = sum (Z(i)*sb0halo_recap(:,i))
       !where Z(i) is the number of electrons carried by a neutral atom
       !of species (i).  => this is to be included in sbsce(:,0).
       !R|step*dV|units=#/sec sb0halo_recap(~nrho_nbi,nspec_gas)

     INTEGER :: id_sbedep = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: sbedep
       !beam deposition cold electron sce
       !there is also a contribution due to recapture of charge-exchanged
       !fast neutrals from partially slowed down beam ions.
       !R|step*dV|units=#/sec sbedep(~nrho_nbi)

     INTEGER :: id_sbehalo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: sbehalo
       !cold electron sce due to
       !recapture of beam halo charge exchange thermal neutrals
       != sum Z(i)*sb0halo_recap(i), if electrons do not go into
       !an impurity charge state spectrum.
       !R|step*dV|units=#/sec sbehalo(~nrho_nbi)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_sbsce
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: sbsce
       !net source, thermal ions & electrons
       !...cold electron deposition; ion thermalization;
       !corrections for charge exchange (beam deposition halo, mainly),
       !halo recapture
       !for thermal ions:
       !sbsce = sbtherm - sb0halo + sb0halo_recap
       !(caution: sbtherm is defined over beam species list;
       !sbsce over thermal species list;
       !sb0halo & sb0halo_recap over the neutral gas species list).
       !for electrons:
       !sbsce = sbedep + sbehalo
       !R|step*dV|units=#/sec   sbsce(~nrho_nbi,0:nspec_tha)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_sbtherm
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: sbtherm
       !beam ion thermalization
       !source of slowed down, thermalized beam ions
       !R|step*dV|units=#/sec sbtherm(~nrho_nbi,nspec_beam)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_T0_halo
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: T0_halo
       !halo neutral temperature (2/3)<E0>
       !R|units=keV|step*n0_halo  T0_halo(~nrho_nbi,nspec_gas)

     INTEGER :: id_tqb0_halo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: tqb0_halo
       !torque of halo neutral source
       !(loss from thermal ions)
       !R|step*dV   tqb0_halo(~nrho_nbi)

     INTEGER :: id_tqbe = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: tqbe
       !collisional beam torque to electrons
       !R|step*dV   tqbe(~nrho_nbi)

     INTEGER :: id_tqbi = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: tqbi
       !collisional beam torque to thermal ions
       !R|step*dV   tqbi(~nrho_nbi)

     INTEGER :: id_tqbjxb = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: tqbjxb
       !JxB beam torque
       !R|step*dV   tqbjxb(~nrho_nbi)

     INTEGER :: id_tqbth = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: tqbth
       !momentum in thermalization of beam ions
       !R|step*dV   tqbth(~nrho_nbi)

     INTEGER :: id_tqcx_halo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: tqcx_halo
       !beam halo driven CX momentum torque
       !(positive value = momentum source)
       !(no associated particle source/sink)
       !R|step*dV   tqcx_halo(~nrho_nbi)

     INTEGER :: id_tqsc_halo = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: tqsc_halo
       !beam halo torque: recapture - tqb0_halo
       !(net source/sink to thermal ions)
       !(associated particle source/sink exists)
       !R|step*dV   tqsc_halo(~nrho_nbi)

     !----------------------------
     ! Component: PEL

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_spsce
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: spsce
       !net source, thermal ions & electrons
       !R|step*dV|units=#/sec   spsce(~nrho_pel,0:nspec_tha)

     !----------------------------
     ! Component: PLASMA

     INTEGER :: id_chie_trans = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chie_trans
       !electron thermal diffusivity
       !R|pclin chie_trans(nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_chie_transcat
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: chie_transcat
       !Electron energy diffusivity
       !R|units=m^2/sec|pclin chie_transcat(nrho,ntrcat)

     INTEGER :: id_chii_trans = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chii_trans
       !ion thermal diffusivity
       !R|pclin chii_trans(nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_chii_transcat
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: chii_transcat
       !Ion energy diffusivity
       !R|units=m^2/sec|pclin chii_transcat(nrho,ntrcat)

     INTEGER :: id_chimo_trans = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: chimo_trans
       !angular momentum diffusivity
       !R|pclin chimo_trans(nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_chimo_transcat
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: chimo_transcat
       !Angular momentum diffusivity
       !R|units=m^2/sec|pclin chimo_transcat(nrho,ntrcat)

     INTEGER :: id_curr_bootstrap = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: curr_bootstrap
       !Neoclassical bootstrap current
       !R|units=A|step*dA   curr_bootstrap(~nrho)

     INTEGER :: id_curr_ohmic = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: curr_ohmic
       !Ohmic current
       !i.e. inductive current driven by toroidal loop voltage
       !in the toroidal direction, Amps/zone:  sum of:
       !curr_ohmic + curr_bootstrap + RF driven currents +
       !fast ion driven currents + runaway electron current +
       !diamagnetic current => curt (total enclosed toroidal
       !current) in the EQ component.
       !R|units=A|step*dA   curr_ohmic(~nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_diff_trans
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: diff_trans
       !specie ptcl diffusivity
       !R|units=m^2/sec|pclin diff_trans(nrho,0:nspec_th)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: id_diff_transcat
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: diff_transcat
       !specie diffusivity
       !R|units=m^2/sec|pclin diff_transcat(nrho,0:nspec_th,ntrcat)

     INTEGER :: id_Epot = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Epot
       !radial electrostatic potential
       !at zone bdys
       !R|units=keV|pclin  Epot(nrho)

     INTEGER :: id_eta_parallel = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: eta_parallel
       !parallel electrical resistivity
       !R|units=ohm*m|Hermite eta_parallel(nrho)

     INTEGER :: id_Fi_depletion = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Fi_depletion
       !fast ion depletion: sum[nj*Zj/ne]
       !R|units=-|step*ns(:,ps_elec_index) Fi_depletion(~nrho)

     INTEGER :: id_iota = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: iota
       !iota profile (1/q)
       !R|units=-|Spline_00  iota(nrho)

     INTEGER :: id_ni = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: ni
       !total thermal ion density
       !should match sum ns(:,1:nspec_th)
       !R|units=m^-3|step  ni(~nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_ns
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: ns
       !thermal specie density
       !R|units=m^-3|alias=n|step  ns(~nrho,0:nspec_th)

     INTEGER :: id_omegat = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: omegat
       !ion toroidal angular velocity
       !single profile for all thermal
       !ion species; should be consistent
       !with ns & v_pars.
       !R|units=rad/sec|step*ni  omegat(~nrho)

     INTEGER :: id_pe_trans = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pe_trans
       !electron transport (loss) power
       !R|step*dV   pe_trans(~nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_pe_transcat
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: pe_transcat
       !electron transport
       !R|step*dV|units=W  pe_transcat(~nrho,ntrcat)

     INTEGER :: id_pi_trans = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pi_trans
       !ion transport (loss) power
       !R|step*dV   pi_trans(~nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_pi_transcat
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: pi_transcat
       !ion transport
       !R|step*dV|units=W  pi_transcat(~nrho,ntrcat)

     INTEGER :: id_pohme = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: pohme
       !Ohmic heating of electrons
       !R|units=W|step*dV   pohme(~nrho)

     INTEGER :: id_qcomp_e = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qcomp_e
       !Compressional electron heating
       !R|step*dV   qcomp_e(~nrho)

     INTEGER :: id_qcomp_i = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qcomp_i
       !Compressional ion heating
       !R|step*dV   qcomp_i(~nrho)

     INTEGER :: id_qie = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qie
       !Ion->Electron heat coupling
       !+: power flow from ions to electrons
       !-: power flow from electrons to ions
       !R|units=W|step*dV   qie(~nrho)

     INTEGER :: id_qrot_conv = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qrot_conv
       !rotational energy dissipation (convective)
       !R|step*dV   qrot_conv(~nrho)

     INTEGER :: id_qrot_diff = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: qrot_diff
       !rotational energy dissipation (diffusive)
       !R|step*dV   qrot_diff(~nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_res_sn
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: res_sn
       !residuals for particle transport eqn
       !R|units=Norm|step*dV res_sn(~nrho,0:nspec_th)

     INTEGER :: id_res_te = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: res_te
       !residuals for electron transport eqn
       !R|step*dV res_te(~nrho)

     INTEGER :: id_res_ti = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: res_ti
       !residuals for ion transport eqn
       !R|step*dV res_ti(~nrho)

     INTEGER :: id_res_tq = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: res_tq
       !rediduals for angular momentum transport eqn
       !R|units=Norm|step*dV res_tq(~nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_sn_trans
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: sn_trans
       !particle transport (loss)
       !electrons & thermal ion species
       !R|units=#/sec|step*dV sn_trans(~nrho,0:nspec_th)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: id_sn_transcat
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: sn_transcat
       !species transport
       !R|step*dV|units=#/sec  sn_transcat(~nrho,0:nspec_th,ntrcat)

     INTEGER :: id_Ti = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Ti
       !ion temperature
       !single profile for all thermal
       !ion species; should be consistent
       !with ns & Ts
       !R|units=keV|step*ni Ti(~nrho)

     INTEGER :: id_tq_trans = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: tq_trans
       !angular momentum transport (loss)
       !toroidal angular momentum
       !R|units=Nt*m|step*dV  tq_trans(~nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_tq_transcat
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: tq_transcat
       !toroidal angular momentum transport
       !R|step*dV|units=Nt*m  tq_transcat(~nrho,ntrcat)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_Ts
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: Ts
       !thermal specie temperature
       !R|units=keV|alias=T|step*ns  Ts(~nrho,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_upwind_pfrac_ns
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: upwind_pfrac_ns
       !specie density forward upwind fractions
       !R|units=-|pclin upwind_pfrac_ns(nrho,0:nspec_th)

     INTEGER :: id_upwind_pfrac_omega = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: upwind_pfrac_omega
       !omega (ang. velocity) forward upwind fraction
       !R|pclin upwind_pfrac_omega(nrho)

     INTEGER :: id_upwind_pfrac_Te = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: upwind_pfrac_Te
       !Te (electron energy advection) forward upwind fraction
       !R|pclin upwind_pfrac_Te(nrho)

     INTEGER :: id_upwind_pfrac_Ti = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: upwind_pfrac_Ti
       !Ti (ion energy advection) forward upwind fraction
       !R|pclin upwind_pfrac_Ti(nrho)

     INTEGER :: id_vee_trans = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: vee_trans
       !electron energy radial velocity
       !R|pclin   vee_trans(nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_vee_transcat
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: vee_transcat
       !Electron energy radial velocity
       !R|units=m/sec|pclin vee_transcat(nrho,ntrcat)

     INTEGER :: id_vie_trans = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: vie_trans
       !ion energy radial velocity
       !R|pclin   vie_trans(nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_vie_transcat
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: vie_transcat
       !Ion energy radial velocity
       !R|units=m/sec|pclin vie_transcat(nrho,ntrcat)

     INTEGER :: id_vmo_trans = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: vmo_trans
       !angular momentum radial velocity
       !R|pclin   vmo_trans(nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_vmo_transcat
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: vmo_transcat
       !Angular momentum radial velocity
       !R|units=m/sec|pclin vmo_transcat(nrho,ntrcat)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_vn_trans
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: vn_trans
       !specie radial velocity
       !R|units=m/sec|pclin   vn_trans(nrho,0:nspec_th)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: id_vn_transcat
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: vn_transcat
       !specie radial velocity
       !R|units=m/sec|pclin vn_transcat(nrho,0:nspec_th,ntrcat)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_vpol_inmp
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: vpol_inmp
       !poloidal velocity, inner half midplane
       !R|units=m/sec|step*ns vpol_inmp(~nrho,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_vpol_omp
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: vpol_omp
       !poloidal velocity, outer half midplane
       !R|units=m/sec|step*ns vpol_omp(~nrho,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_vtor_inmp
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: vtor_inmp
       !toroidal velocity, inner half midplane
       !R|units=m/sec|step*ns vtor_inmp(~nrho,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_vtor_omp
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: vtor_omp
       !toroidal velocity, outer half midplane
       !R|units=m/sec|step*ns vtor_omp(~nrho,0:nspec_th)

     INTEGER :: id_V_loop = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: V_loop
       !loop voltage
       !R|units=Volts|pclin	V_loop(nrho)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_v_pars
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: v_pars
       !thermal specie <vpll>
       !R|units=m/sec|alias=vpll_|step*ns v_pars(~nrho,0:nspec_th)

     INTEGER, DIMENSION(:), ALLOCATABLE :: id_v_pers
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: v_pers
       !thermal specie <vper>
       !R|units=m/sec|alias=vper_|step*ns v_pers(~nrho,0:nspec_th)

     INTEGER :: id_Zeff = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Zeff
       !Zeff (sum[nj*Zj**2]/ne)
       !R|step*ns(:,ps_elec_index)  Zeff(~nrho)

     INTEGER :: id_Zeff_fi = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Zeff_fi
       !Fast ion contribution to Zeff
       !R|step*ns(:,ps_elec_index)  Zeff_fi(~nrho)

     INTEGER :: id_Zeff_th = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: Zeff_th
       !Thermal ion contribution to Zeff
       !R|step*ns(:,ps_elec_index)  Zeff_th(~nrho)

     !----------------------------
     ! Component: RAD

     INTEGER :: id_prad = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: prad
       !total radiated power
       !R|step*dV  prad(~nrho_rad)

     INTEGER :: id_prad_br = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: prad_br
       !bremsstrahlung contribution
       !R|step*dV  prad_br(~nrho_rad)

     INTEGER :: id_prad_cy = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: prad_cy
       !cyclotron radiation contribution
       !R|step*dV  prad_cy(~nrho_rad)

     INTEGER :: id_prad_li = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: prad_li
       !line radiation contribution
       !R|step*dV  prad_li(~nrho_rad)

     INTEGER, DIMENSION(:,:), ALLOCATABLE :: id_sii_xs
     REAL(KIND=rspec), DIMENSION(:,:,:), ALLOCATABLE :: sii_xs
       !species source/sinks driven by xs(1:nxs)
       !R|step*dV|units=(#/sec)/(#/sec) sii_xs(~nrho_mist,0:nspec_th,nxs)

     !----------------------------
     ! Component: RIPPLE

     INTEGER :: id_tfrip2_log = 0
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: tfrip2_log
       !2nd component of TF ripple
       !log((Bmax-Bmin)/(Bmax+Bmin))
       !This field is sometimes introduced
       !time dependently in experiments.
       !R|pclin  tfrip2_log(nR_rip,nZ_rip)

     INTEGER :: id_tfrip_log = 0
     REAL(KIND=rspec), DIMENSION(:,:), ALLOCATABLE :: tfrip_log
       !TF ripple: log((Bmax-Bmin)/(Bmax+Bmin))
       !This field is likely time invariant,
       !an attribute of the tokamak TF coils.
       !R|pclin  tfrip_log(nR_rip,nZ_rip)

     !----------------------------
     ! Component: RUNAWAY

     INTEGER :: id_cur_rw = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: cur_rw
       !current carried by fast electrons
       !R|step*dA|units=A   cur_rw(~nrho_rw)

     INTEGER :: id_dcur_rw_dVloop = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: dcur_rw_dVloop
       !sensitivity to loop volts
       !R|step*dA|units=A/Volt dcur_rw_dVloop(~nrho_rw)

     INTEGER :: id_eperp_rw = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: eperp_rw
       !runaway electron <Eperp>
       !R|units=keV|step*nrw  eperp_rw(~nrho_rw)

     INTEGER :: id_epll_rw = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: epll_rw
       !runaway electron <Epll>
       !R|units=keV|step*nrw  epll_rw(~nrho_rw)

     INTEGER :: id_nrw = 0
     REAL(KIND=rspec), DIMENSION(:), ALLOCATABLE :: nrw
       !runaway electron density
       !nrw/ne << 1 is assumed
       !R|units=m^-3|step   nrw(~nrho_rw)


  END TYPE plasma_state

  !-------------------------------------------------------------------------
  !   ps_list_window TYPE definition
  !      this is an aid to state visualization software
  !-------------------------------------------------------------------------

  TYPE :: ps_list_window

    CHARACTER*32 :: list_name = ' '   ! name of list
    INTEGER :: size = 0               ! size of list (# of elements)

    CHARACTER*32, DIMENSION(:), POINTER :: list_elems => NULL()
                                           ! sorted list element names

    CHARACTER*32, DIMENSION(:), POINTER :: elem_units => NULL()
                                           ! element units labels

    CHARACTER*256, DIMENSION(:), POINTER :: elem_descr => NULL()
                                           ! element descriptions

    CHARACTER*32, DIMENSION(:), POINTER :: component => NULL()
                                           ! component membership of element

    CHARACTER*32, DIMENSION(:), POINTER :: val_field => NULL()
                                           ! element value as character string

    CHARACTER*256, DIMENSION(:), POINTER :: c256val => NULL()
                                           ! CHAR*256 value (e.g. file path)

    INTEGER, DIMENSION(:), POINTER :: ival => NULL()
                                           ! INTEGER value

    REAL(KIND=rspec), DIMENSION(:), POINTER :: fval => NULL()
                                           ! REAL(KIND=rspec) value

    INTEGER, DIMENSION(:), POINTER :: vecsizes => NULL()
                                           ! vector sizes

    INTEGER, DIMENSION(:,:), POINTER :: ivec => NULL()
                                           ! INTEGER vectors

    REAL(KIND=rspec), DIMENSION(:,:), POINTER :: fvec => NULL()
                                           ! REAL(KIND=rspec) vectors

    !  species list data...

    INTEGER, DIMENSION(:), POINTER :: spectype => NULL()
                                           ! type of species e.g. ps_therm_ion

    INTEGER, DIMENSION(:), POINTER :: qval => NULL()
                                           ! ion charge

    INTEGER, DIMENSION(:), POINTER :: qatom => NULL()
                                           ! atomic number

    REAL(KIND=rspec), DIMENSION(:), POINTER :: awght => NULL()
                                           ! atomic weight
  END TYPE ps_list_window

  CONTAINS

    SUBROUTINE load_ps_container(iarray,ptr)
      INTEGER :: iarray(*)
      TYPE (plasma_state_container) :: ptr

      ptr = transfer(iarray(1:ps_container_size),ptr)
    END SUBROUTINE load_ps_container

    SUBROUTINE unload_ps_container(ptr,iarray)
      TYPE (plasma_state_container) :: ptr
      INTEGER :: iarray(*)

      iarray(1:ps_container_size) = transfer(ptr,iarray(1:ps_container_size))
    END SUBROUTINE unload_ps_container

    SUBROUTINE free_window(w)

      TYPE (ps_list_window) :: w

      w%list_name = ' '
      w%size = 0

      if(associated(w%list_elems)) deallocate(w%list_elems)
      if(associated(w%elem_descr)) deallocate(w%elem_descr)
      if(associated(w%component)) deallocate(w%component)
      if(associated(w%elem_units)) deallocate(w%elem_units)
      if(associated(w%val_field)) deallocate(w%val_field)
      if(associated(w%c256val)) deallocate(w%c256val)
      if(associated(w%ival)) deallocate(w%ival)
      if(associated(w%fval)) deallocate(w%fval)
      if(associated(w%vecsizes)) deallocate(w%vecsizes)
      if(associated(w%ivec)) deallocate(w%ivec)
      if(associated(w%fvec)) deallocate(w%fvec)
      if(associated(w%spectype)) deallocate(w%spectype)
      if(associated(w%qval)) deallocate(w%qval)
      if(associated(w%qatom)) deallocate(w%qatom)
      if(associated(w%awght)) deallocate(w%awght)

    END SUBROUTINE free_window

    SUBROUTINE clone_window(w1,w2)

      TYPE (ps_list_window) :: w1,w2

      call free_window(w2)

      w2%list_name = w1%list_name
      w2%size = w1%size

      if(associated(w1%list_elems)) then
         allocate(w2%list_elems(size(w1%list_elems)))
         w2%list_elems = w1%list_elems
      endif

      if(associated(w1%elem_descr))  then
         allocate(w2%elem_descr(size(w1%elem_descr)))
         w2%elem_descr = w1%elem_descr
      endif

      if(associated(w1%component))  then
         allocate(w2%component(size(w1%component)))
         w2%component = w1%component
      endif

      if(associated(w1%elem_units))  then
         allocate(w2%elem_units(size(w1%elem_units)))
         w2%elem_units = w1%elem_units
      endif

      if(associated(w1%val_field))  then
         allocate(w2%val_field(size(w1%val_field)))
         w2%val_field = w1%val_field
      endif

      if(associated(w1%c256val))  then
         allocate(w2%c256val(size(w1%c256val)))
         w2%c256val = w1%c256val
      endif

      if(associated(w1%ival))  then
         allocate(w2%ival(size(w1%ival)))
         w2%ival = w1%ival
      endif

      if(associated(w1%fval))  then
         allocate(w2%fval(size(w1%fval)))
         w2%fval = w1%fval
      endif

      if(associated(w1%vecsizes))  then
         allocate(w2%vecsizes(size(w1%vecsizes)))
         w2%vecsizes = w1%vecsizes
      endif

      if(associated(w1%ivec))  then
         allocate(w2%ivec(size(w1%ivec,1),size(w1%ivec,2)))
         w2%ivec = w1%ivec
      endif

      if(associated(w1%fvec))  then
         allocate(w2%fvec(size(w1%fvec,1),size(w1%fvec,2)))
         w2%fvec = w1%fvec
      endif

      if(associated(w1%spectype))  then
         allocate(w2%spectype(size(w1%spectype)))
         w2%spectype = w1%spectype
      endif

      if(associated(w1%qval))  then
         allocate(w2%qval(size(w1%qval)))
         w2%qval = w1%qval
      endif

      if(associated(w1%qatom))  then
         allocate(w2%qatom(size(w1%qatom)))
         w2%qatom = w1%qatom
      endif

      if(associated(w1%awght))  then
         allocate(w2%awght(size(w1%awght)))
         w2%awght = w1%awght
      endif

    END SUBROUTINE clone_window

  !-------------------------------------------------------------------------
  !
  !   Data declarations -- NONE
  !
  !-------------------------------------------------------------------------

END MODULE plasma_state_definition_mod
