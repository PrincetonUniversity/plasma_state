program plasma_state_test

  !--------------------------------------------------------------------
  ! Demo program for use of plasma state component
  !
  ! Plasma State Vsn 2.00 -- December 10, 2007 (D. McCune)
  !
  ! (Original version D. McCune 9 Oct 2006)
  !
  !--------------------------------------------------------------------
  ! RB: added timers to code
  ! Wed May 16 12:20:10 EDT 2007
  !
  !--------------------------------------------------------------------
  !
  ! This routine demonstrates use of the IPS state interface.
  !
  !-----------------------------------
  ! The state contents are specified in "plasma_state_spec.dat".  This is
  ! input to a code generator which produces the state file i/o routines;
  ! an "internal" plasma state component interface is also defined; an
  ! implementation is provided using xplasma2.  The internal interface 
  ! is not currently documented.
  !
  !   Procedure for modifying the contents of state:
  !     1.  edit "plasma_state_spec.dat"
  !     2.  run the code generator script.
  !
  ! After the state definition is modified, old state files can still
  ! be examined (e.g. with a plotting tool) but may not be usable for
  ! simulation restarts.
  !
  ! DMC: I/O of version 2.00 state is based on NetCDF self describing
  ! file with separate variable for each state element (instead of 
  ! XPLASMA NetCDF I/O).  XPLASMA NetCDF files based on earlier state 
  ! software will not be readable.  Compatibility of future versions
  ! with files from earlier versions after 2.00 should be OK, but with 
  ! missing individual data items.
  !-----------------------------------
  !
  ! State elements are traditional f77 integer, floating point, and character
  ! string scalars and arrays all contained within a large container data 
  ! type "plasma_state".  Two instances of this container data type are
  ! declared in the module "plasma_state_mod":
  !
  !   ps -- the current state (timestep now being computed).
  !   psp -- the prior or "committed" state (completed, prior timestep)
  !
  ! Elements of the state can be referenced directly using the standard
  ! f95 syntax-- e.g. ps%nrho is the size of one of the radial flux 
  ! coordinate "rho" grids in the state.
  !
  ! State elements can be directly modified by codes that use the plasma
  ! state-- although this should be done carefully.  Generally, items in 
  ! the state are meant to be shared between multiple components of the 
  ! IPS; conventions for use of the state data will need to evolve.
  !
  ! States will be mapped to NetCDF files.  The module "plasma_state_mod"
  ! defines two variables for these filenames:
  !
  !   CHARACTER*256 :: state_file = 'plasma_state.cdf'
  !   CHARACTER*256 :: prior_file = 'prior_state.cdf'
  !
  ! RB: actually, those two names are set up in the call to ps_init_tag. 
  !
  ! The assigned default values can of course be modified.
  !-----------------------------------
  ! Version 2.00 software:
  ! Non-profile state data is split into sections:
  !
  ! set up during initialization; subsequently time invariant:
  !    MACHINE_DESCRIPTION   (e.g. limiter description; beam geometries)
  !    SHOT_CONFIGURATION    (e.g. species lists)
  !    SIMULATION_INIT       (e.g. grid resolutions)
  !
  ! initial condition set; time evolved after initialization:
  !    STATE_DATA
  !
  ! There will be a possibility for exceptions to the time invariance
  ! of initialization sections, but generally, special code will be
  ! needed.  For example, if a grid is redefined, all profiles using 
  ! the grid would require interpolation.
  !-----------------------------------
  ! In addition to direct access to state data elements, the module
  !
  !    use plasma_state_mod
  !
  ! defines a subroutine interface which allows states to be loaded from
  ! or written out to files, support various steps during initialization,
  ! support profile interpolation, etc... For details, see the comments
  ! in plasma_state_mod.f90
  !
  ! Many of these routines are exercised here.  The code in this test
  ! program may serve as a suitable template for some applications, but,
  ! some will require a different approach.
  !-----------------------------------
  ! Profile IDs:  each state array that is defined over one or more coordinate
  !   grids is assigned an ID variable or array with name ID_<name>.  These IDs
  !   are needed to refer to specific profiles for interpolation or rezoning.
  !
  !   Examples mapping from plasma_state_spec.dat to plasma state object "ps":
  !
  !      R|units=ohm*m|Hermite eta_parallel(nrho)  
  !                            ! parallel electrical resistivity
  !
  !      -> ps%eta_parallel(...)  (1d allocated REAL(KIND=rspec) array)
  !                        (1:ps%nrho)
  !
  !      -> ps%id_eta_parallel    INTEGER scalar
  !
  !      R|units=m^-3|step  ns(~nrho,0:nspec_th)       ! thermal specie density
  !
  !      -> ps%ns(...)     (2d allocated REAL(KIND=rspec) array
  !      -> ps%id_ns(...)  (1d allocated INTEGER array)
  !
  !              ps%ns(1:ps%nrho-1,0:ps%nspec_th)
  !              ps%id_ns(0:ps%nspec_th)
  !
  ! Because this data is a zone oriented "step function", the number of
  ! radial points is ps%nrho-1; each zone is between two successive grid
  ! points.
  !
  ! The ID variables are handles used for interpolation.  For details on
  ! available interpolation calls, see the comments in "plasma_state_mod.f90".
  !
  ! Some examples are presented in this code.
  ! 
  !-----------------------------------
  ! Implementation of interpolation and rezoning-- the "visible" state
  ! elements ps%* and psp%* are not the entire state.  When interpolation
  ! and rezoning operations are carried out, additional hidden information--
  ! such as spline coefficients-- is accessed depending on the interpolation
  ! method given in the state specification.
  !
  ! Interpolation methods for profiles are part of the state specification
  ! "plasma_state_spec.dat" and are handled by generated code.
  !
  ! However, since applications have direct access to the profile data, they
  ! can separately define their own interpolation methods if needed.
  !-----------------------------------

  ! define state object and interface...

  USE plasma_state_mod

  IMPLICIT NONE

  !------------------------------------
  !  local
  INTEGER :: ierr,ier1,ier2,ier3
  INTEGER :: iout = 6

  !  for this simple demo, all profiles will vary as:
  !    (<core_value>-<edge_value>)*(1-rho**2)**2 + <edge_value> -- cf profgen

  REAL(KIND=rspec) :: time0 = 0.1_rspec      ! s -- time of start of "run".
  REAL(KIND=rspec) :: delta_t = 0.01_rspec   ! s -- initial time step size

  REAL(KIND=rspec) :: T0 = 5.0_rspec   ! KeV at core
  REAL(KIND=rspec) :: Ta = 0.1_rspec   ! KeV at edge

  REAL(KIND=rspec) :: den0 = 1.0e20_rspec  ! #/m^3 electrons at core
  REAL(KIND=rspec) :: dena = 0.2e20_rspec  ! #/m^3 electrons at edge

  REAL(KIND=rspec) :: Zeff0 = 1.2_rspec    ! Z_eff at core
  REAL(KIND=rspec) :: Zeffa = 2.0_rspec    ! Z_eff at edge

  REAL(KIND=rspec) :: Aimp_atom = 12.011_rspec ! Carbon A

  REAL(KIND=rspec) :: frac_mino = 0.01_rspec  ! n_minority/n_e
  integer :: iZ_mino = 1   ! Z of minority (H)
  integer :: iA_mino = 1   ! A of minority (H)

  integer :: iz_imp = 6               ! Z of impurity (C)
  integer :: iz_imp2 = 26             ! Z of 2nd impurity (Fe)
  real*8 :: zimp

  REAL(KIND=rspec), dimension(:), allocatable :: Zeff,rho_zc

  integer :: i,ii,jj,ith,ict,inum,nrho,nrho_zc,isave
  integer :: id,ids(2)
  integer :: icomp,idiff,i_nonzero

  REAL(KIND=rspec) :: zne_adj,zzne_adj,zrho,zdvol
  REAL(KIND=rspec) :: ZERO= 0.0_rspec
  REAL(KIND=rspec) :: ONE = 1.0_rspec
  REAL(KIND=rspec) :: vpp0,ppp0,zansR0,zansZ0
  REAL(KIND=rspec) :: test_Rmin,test_Rmax,zdiff1,zdiff2
  REAL(KIND=rspec) :: test_Zmin,test_Zmax

  REAL(KIND=rspec), dimension(:), allocatable :: zdvola,zdvolc
  REAL(KIND=rspec), dimension(:), allocatable :: rho_intrp,th_intrp
  REAL(KIND=rspec), dimension(:), allocatable :: dvdrho,dpsidrho
  REAL(KIND=rspec), dimension(:,:), allocatable :: buf2

  REAL(KIND=rspec), dimension(:), allocatable :: rho_bdys,dvols
  REAL(KIND=rspec), dimension(:,:), allocatable :: my_ns,my_ts,my_ns2,my_ts2
  REAL(KIND=rspec), dimension(:), allocatable :: my_ns1,my_ts1
  REAL(KIND=rspec), dimension(:,:,:), allocatable :: my_profa,my_profb,my_profc
  REAL(KIND=rspec), dimension(:), allocatable :: zvols,ttest1,ttest2,ttest3
  REAL(KIND=rspec), dimension(:), allocatable :: volref_ns,volref_nt
  REAL(KIND=rspec), dimension(:), allocatable :: volint_ns,volint_nt
  REAL(KIND=rspec), dimension(:), allocatable :: maxdiff_ns,maxdiff_nt
  REAL(KIND=rspec), dimension(:), allocatable :: dmaxn,dmaxt,zb_test

  REAL(KIND=rspec), dimension(:,:), allocatable :: tha_test
  REAL(KIND=rspec) :: test_voltot(1)

  REAL(KIND=rspec), parameter :: small_n=1.0_rspec
  REAL(KIND=rspec), parameter :: small_T=1.0e-20_rspec
  
  TYPE (plasma_state) :: my_state       ! used for un-initialized state test

  !-----------------------
  !  unified plasma parameter arrays (on ps%nspec_alla list)
  !    abridged thermal species + all fast species combined.
  integer :: ipts,nrho_user,nrhop1
  REAL(KIND=rspec), dimension(:), allocatable :: rho_user, rho_bdy_rezon
  REAL(KIND=rspec), dimension(:), allocatable :: dense,tempe
  REAL(KIND=rspec), dimension(:,:), allocatable :: densi,tempi
  REAL(KIND=rspec), dimension(:), allocatable :: wk_eperp,wk_epll,wk_dens
  !-----------------------

  !-----------------
  ! Timing variables
  !-----------------
  REAL(KIND=rspec) :: t1, t2, t3, t4, t5, t_overall

  integer :: iarg,narg,iwarn
  character*32 argval

  integer :: cclist(ps_ccount)

  !------------------------------------
  ! When starting a simulation, there is no prior state.
  ! To create a state from scratch:
  !   a) specify state array dimensions (could be e.g. by a namelist read)
  !   b) apply a global label to the plasma state data (e.g. name of a program)
  !      set the initial time.
  !   c) allocate state arrays
  !   d) fill in names and values where appropriate for all non-coordinate
  !      array dimensions:  species lists, RF antenna lists, NB injector
  !      lists, ...
  !   e) acquire initial MHD equilibrium
  !   f) acquire initial conditions-- e.g. temperatures and densities for
  !      each species.
  !   g) store and commit the initial state-- now both a current and prior
  !      state exist, and time dependent simulation can commence.
  !
  ! Elements of the current state are in ps%<element-names>(indices,...)
  ! Elements of the prior state are in  psp%<element-names>(indices,...)
  !------------------------------------

  !==========================================================================
  !  Contents of "ps" are defined in "plasma_state_spec.dat"

  !==========================================================================
  ! (a) array dimensions -- set ONLY ONCE at start of simulation...
  !     (the values here are chosen somewhat arbitrarily).

  ! activate some extra debugging output
  ! default value of ps_debug is 0, for quiet operation
  ! set ps_debug = 2 or 3 for even more extra output

  write(iout,*) ' ==> plasma_state_test'
  write(iout,*) '     generated plasma state software version: ',trim(ps_version_id)
  write(iout,*) ' '

  t_overall = mytime()

  ps_debug = 2

  ! DMC 6 June 2007 -- add command argument to allow modification of
  ! debug switch:   "debug=0", "debug=1", "debug=2", or "debug=3"

  narg = command_argument_count()
  do iarg=1,narg
     call get_command_argument(iarg,argval)
     call ulower(argval)  ! force lowercase
     if(argval.eq.'debug=0') ps_debug = 0
     if(argval.eq.'debug=1') ps_debug = 1
     if(argval.eq.'debug=2') ps_debug = 2
     if(argval.eq.'debug=3') ps_debug = 3
  enddo
  write(iout,'(1x,a,1x,i1/)') ' plasma_state_test debug level: ',ps_debug

  ! PLASMA...

  ps%nspec_th = 7     ! 7 thermal ion species, 6 impurity-charge-states
  ps%nspec_rfmin = 1  ! 1 RF minority specie

  !  ps%nspec will be computed when the state is allocated
  !    see state specification "plasma_state_spec.dat".

  ps%nrho = 101       ! OK, a grid for the plasma component
  ps%nrho_rw = 26     ! OK, a grid for the runaway electron component

  ! ICRF...

! ps%nicrf_src = 1    ! ONE antenna...
  ps%nicrf_src = 2    ! two antennas  LAB..
  CALL ps_alloc_plasma_state(ierr)
  call ckerr('ps_alloc_plasma_state')
    
  ps%ant_model(1) = 'icrf_antenna model_1'
  ps%ant_model(2) = 'icrf_antenna model_2'
  ps%n_straps(1) = 3  ! straps for first source
  ps%n_straps(2) = 4  ! straps for second source
  ps%num_nphi_vac(1) = 128 ! vac modes for antenna
  ps%num_nphi_vac(2) = 128 ! vac modes for antenna
  ps%num_nphi(1) = 12 ! used modes for antenna
  ps%num_nphi(2) = 12 ! used  modes for antenna
  CALL ps_alloc_plasma_state(ierr)
  call ckerr('ps_alloc_plasma_state')
  ps%nrho_icrf = 61   ! OK, a grid for the IC component

  ps%nphi(1:12,1) = (/ (i, i=0,11) /)  ! for unique label
  ps%nphi(1:12,2) = (/ (i, i=0,11) /)
  ps%wt_nphi      = 1._rspec           ! nonzero weight required by namelist check

  ! Equilibrium -- R and Z grid sizes will be reset when first G-eqdsk is read.
  !   (The G-eqdsk R and Z grids are used).

  ps%nrho_eq = 51     ! Grids for EQ
  ps%nth_eq = 101
  ps%nR = 0
  ps%nZ = 0

  !----------------------------------------------------------------
  ! (b) Global label for state data (as will appear in state files)
  !     Here the program name "plasma_state_test" is used, but it is likely
  !     better to use a runid -- ID string to uniquely identify a specific
  !     run or simulation
  ! RB: added the version id to the state label.
  !----------------------------------------------------------------

  ps%Global_Label = 'plasma_state_test_'//trim(ps_version_id)//' (test)'
  ps%RunID = 'Test'

  ps%Tokamak_ID = 'Dummy'
  ps%Shot_Number = 12345

  !----------------------------------------------------------------
  ! NOTE: for this application I use a very simple error handling method:
  !   if ierr.NE.0 print a message and exit, in contained subroutine "ckerr".
  !   Real applications will require something more than this.
  !----------------------------------------------------------------

  !  the initial times.  The first state does not really represent a timestep
  !  so both the start time ps%t0 and the final time ps%t1 are set:

  ps%t0 = time0
  ps%t1 = time0

  !-------------------------
  ! (c) Initial allocation of state arrays.  All integers and floats are
  ! initialized to ZERO; all character strings to blank

  ! Note, it is possible to call this several times, providing additional
  ! non-zero array dimensions on each call, BUT: once a non-zero dimension
  ! has been supplied, the rule for now is that it cannot be changed!  This
  ! routine enforces the rule by checking all arrays prior to allocation,
  ! and checking the sizes of arrays already allocated against their
  ! corresponding array dimension variables in the state.  Generated 
  ! code is used to do this.

  CALL ps_alloc_plasma_state(ierr)
  call ckerr('ps_alloc_plasma_state')

  !==========================================================================
  ! (d) Define names of list elements.
  !
  ! Explanation:
  ! Array dimensions come in two flavor:
  !    1.  Grid dimensions -- e.g. along rho, theta, R, Z
  !    2.  List dimensions -- e.g. species index, RF antenna index, ...
  ! Each element of each list dimension must have a tag name associated
  ! with it, so that distinct names can be provided for every profile
  ! over the grid dimensions.  For example, the state array:
  !
  !  R|units=MW/m^3|step prf_srcs(~nrho_icrf,nicrf_src,0:nspec)
  !                    ! RF power deposition
  !
  ! defines (ps%nicrf_src * (ps%nspec + 1)) profiles vs. rho.  In the state
  ! output files these will be gathered as a list "PRF_SRCS" containing
  ! elements with such names as:
  !    "PRF_SRCS_ANT1_E" (heating from Antenna#1 to electrons)
  !    "PRF_SRCS_ANT1_D" (heating from Antenna#1 to deuterons)
  !    "PRF_SRCS_ANT1_C6" (heating from Antenna#1 to Carbon impurity)
  !    "PRF_SRCS_ANT3_HMINO" (heating from Antenna#3 to H-minority)
  !       ...
  !       ...
  !    "PRF_SRCS_ANT3_HMINO" (heating from Antenna#3 to H-minority)
  ! I.e. if there are 3 antennas and 4 species {e,D,C,Hmino}, there would
  ! be 4 profiles, each separately named and available (e.g. for plotting).
  !
  ! But, these names can only be generated if the tag names for all elements
  ! of the list dimensions are defined first.
  !
  ! (The plasma state cannot be saved to a file until all tag names are 
  ! defined; duplicate tags within a list dimension are not allowed).
  !
  ! (The names are used in state visualization tools such as "cstate").
  !
  !-------------------------
  ! 1. Define species lists tag names...
  !
  !  The procedure here is to load the charge and mass of each species
  !  and then call PS_LABEL_SPECIES(...).
  !
  !  In the state, integer atomic numbers, ion charges, and ion masses
  !  are mapped to REAL(KIND=rspec) charges (C) and masses (kg).
  !
  !  If starting from integer atomic #, charge #, and AMU mass, it is
  !  convenient to use PS_SPECIES_CONVERT(...) to do the conversion
  !
  !  CALL ps_species_convert(atomic#, charge#, AMU-nearest-integer, &  ! input
  !                 atomic-charge, ion-charge, mass, ierr)             ! output
  !
  !  The AMU-nearest-integer argument is only used for H and He isotopes;
  !  it is ignored for all other elements...
  !
  !  various combinations of the first three arguments and their meaning:
  !
  !  (-1,-1,0) -- electrons -- 2nd and 3rd arguments ignored.
  !
  !  (1,1,1) -- H ion  --> "H"
  !  (1,1,2) -- D ion  --> "D"
  !  (1,1,3) -- T ion  --> "T"
  !  (2,2,3) -- He3 (fully stripped) --> "He3"
  !  (2,2,4) -- He4 (fully stripped) --> "He4"
  !  (2,1,4) -- He4 charge +1 --> "He4_1"
  !  (6,6,0) -- C (Carbon) (fully stripped) -- AMU (3rd) arg ignored. --> "C"
  !  (6,4,0) -- C (Carbon) charge +4 -- AMU (3rd) arg ignored. --> "C_4"
  !
  !  DMC -- multiple charge states defined but only C+6 will be given
  !         non-zero density (to exercise naming logic for derived species
  !         lists in presence of multiple impurities and charge states)

  CALL ps_species_convert(-1, -1, 0, &
       ps%qatom_s(0), ps%q_s(0), ps%m_s(0), ierr)  ! electron (species 0)
  CALL ckerr('ps_species_convert')

  CALL ps_species_convert(1, 1, 2, &
       ps%qatom_s(1), ps%q_s(1), ps%m_s(1), ierr)  ! Deuterium (species 1)
  CALL ckerr('ps_species_convert')

  CALL ps_species_convert(iz_imp, iz_imp, 0, &
       ps%qatom_s(2), ps%q_s(2), ps%m_s(2), ierr)  ! Carbon +6 (species 2)
  CALL ckerr('ps_species_convert')

  CALL ps_species_convert(iz_imp, iz_imp-1, 0, &
       ps%qatom_s(3), ps%q_s(3), ps%m_s(3), ierr)  ! Carbon +5 charge state
  CALL ckerr('ps_species_convert')

  CALL ps_species_convert(iz_imp, iz_imp-2, 0, &
       ps%qatom_s(4), ps%q_s(4), ps%m_s(4), ierr)  ! Carbon +4 charge state
  CALL ckerr('ps_species_convert')

  CALL ps_species_convert(iz_imp2, iz_imp2, 0, &
       ps%qatom_s(5), ps%q_s(5), ps%m_s(5), ierr)  ! Iron +26 (species 5)
  CALL ckerr('ps_species_convert')

  CALL ps_species_convert(iz_imp2, iz_imp2-1, 0, &
       ps%qatom_s(6), ps%q_s(6), ps%m_s(6), ierr)  ! Iron +25 charge state
  CALL ckerr('ps_species_convert')

  CALL ps_species_convert(iz_imp2, iz_imp2-2, 0, &
       ps%qatom_s(7), ps%q_s(7), ps%m_s(7), ierr)  ! Iron +24 charge state
  CALL ckerr('ps_species_convert')

  !  *** fast species ***
  !  ALL (1:nspec_nonMax) MUST BE LABELED!
  !    suffix argument set -- fast species
  !
  !  (I have used suffixes "mino", "beam", and "fusn" for RF minority, beam,
  !  and fusion product ions respectively -- dmc).
  !
  !  Hydrogen minority -- "mino" suffix suggested...
  !  element (1) of the "rfmin" species list.

  CALL ps_species_convert(1, 1, 1, &
       ps%qatom_rfmin(1), ps%q_rfmin(1), ps%m_rfmin(1), ierr)  ! Hydrogen
  CALL ckerr('ps_species_convert')

  !  ** AFTER all species are defined, they need to be labeled:
  !     Here error checks are performed: e.g. if fewer species were defined
  !     than were promised e.g. by the setting of ps%nspec_th.

  !  This routine is called only after the following species lists have
  !  had their qatom, q, and m arrays completely filled out:
  !
  !     Thermal species "s" -- mandatory
  !     Neutral beam species "snbi" -- only required if beams are present.
  !     RF minority species "rfmin" -- only required if RF minorities present.
  !     Fusion product species "sfus" -- only required if there are fusion 
  !                                    products.
  !
  !  In this test application, only "s" and "rfmin" species lists have been
  !  allocated and filled out.

  CALL ps_label_species(ierr)
  CALL ckerr('ps_label_species')

  !----------------------------------
  !  form combined species list -- some arrays e.g. RF power coupling will
  !    want to be defined over a combined list with all species, thermal
  !    and non-thermal alike.  So:

  CALL ps_merge_species_lists(ierr)
  CALL ckerr('ps_merge_species_lists')

  !  The above call creates the combined species list "all", and:
  !    "abridged" thermal species list "sa"
  !    "abridged" all species list "alla"

  !  The abridged lists merge all impurities down to 2 impurities which
  !  will have their densities computed by the Zeff and Quasi-neutrality 
  !  equations from the full list.  Here, "impurity" is defined as a thermal
  !  specie with Z >= Zimp; by default Zimp=3 but this is adjustable by an
  !  optional argument in the PS_LABEL_SPECIES call.
  
  !----------------------------------
  !  Form list of neutral gas species e.g. for a gas-fueling model
  !  This is formed from thermal ion species with Z<=Zmax; by default Zmax=2
  !  but this can be reset by an optional argument (for details see the
  !  comments in plasma_state_mod.f90).

  CALL ps_neutral_species(ierr)
  CALL ckerr('ps_neutral_species')
  !----------------------------------

  !  Print out combined species list...

  do ii=0,ps%nspec_all
     write(iout,1001) ii,ps%all_type(ii), &
          trim(ps%all_name(ii)),ps%q_all(ii),ps%m_all(ii)
  enddo
1001 format(' Specie index & type: ',i2,1x,i2,1x, &
          '"',a,'" charge & mass: ',2(1pe12.5,1x))

  !------------------------------------
  ! (d) continued: lists other than species lists...
  ! Define antenna list
  ! ALL (1:nicrf_src) MUST BE LABELED!

  ps%icrf_src_name(1) = 'Ant_1'
  ps%icrf_src_name(2) = 'Ant_2'

  ! additional lists -- e.g. of neutral beams -- could be added to the state
  ! and defined...

  !==========================================================================
  ! (e) Initialize (rho,theta) grids -- use local CONTAIN'ed routine below...

  CALL rho_grid(ps%nrho_rw,   ps%rho_rw)
  CALL rho_grid(ps%nrho_icrf, ps%rho_icrf)
  CALL rho_grid(ps%nrho,      ps%rho)

  ! Note: all grids must be defined before the state can be written to file.
  ! The following step defines the equilibrium and its grids.

  !==========================================================================
  ! (f) initial equilibrium
  !    Here I simply take an existing (old NSTX) equilibrium G-eqdsk file
  !    The R and Z grids will be allocated (along with associated profile
  !    arrays on the first call to "ps_update_equilibrium".  It is assumed,
  !    at least for now, that the range and resolution of the R and Z grids,
  !    once established, will not change in time.

  !    A number of profiles corresponding to metric quantities and flux
  !    surface averages are also computed in this call

  !  this routine also initializes the ps%rho_eq and ps%th_eq grids
  !  on its first call
  
  ps%V_loop = 2.0
  
  write(iout,'(1x,a,1pe12.5)') 'loop voltage at center: ',ps%V_loop(1)
  write(iout,'(1x,a,1pe12.5)') 'loop voltage at edge:   ',ps%V_loop(ps%nrho)
  
  ! "slightly" non-default value of bdy_crat provided; default is 0.075d0.

  t1 = mytime()
  write(iout,*) ' '
  write(iout,'(1x,a)') ' -- building plasma state equilibrium from g file -- '
  CALL ps_update_equilibrium(ierr, g_filepath='g104403.nstx', &
       bdy_crat = 0.075001d0 )
  call ckerr('ps_update_equilibrium')

  t1 = mytime() - t1
  write(iout,'(1x,a,t34,1pe10.3)') 'Time for ps_update_equilibrium: ', t1

  call ps_momtest(ps,iout,ierr)
  call ckerr('ps_momtest')

  !----------------------
  ! (f2) flux surface averages, etc., derived from equilibrium
  !      stuff like <|grad(rho)|^2/R^2> that ends up used in transport
  !      equations; neoclassical moments; elongation; triangularity...

  t1 = mytime()
  write(iout,*) ' '
  write(iout,'(1x,a)') ' -- computing equilibrium derived profiles -- '

  ! ** the derived quantities are written on a different grid than the
  !    primary equilibriumk quantities **

  ! in this instance I choose to use the same grid for the geometric profiles
  ! as was used for the original equilibrium.

  ps%nrho_eq_geo = ps%nrho_eq

  CALL ps_alloc_plasma_state(ierr)
  call ckerr('ps_alloc_plasma_state')

  ps%rho_eq_geo = ps%rho_eq  ! use same grid

  !----
  ! the grid is also used for the computation, but, a coarser grid could
  ! be used if performance becomes an issue...
  !----

  write(iout,*) ' '
  write(iout,*) ' (ps_mhdeq_verify call as debug test-- not required)'
  call ps_mhdeq_verify(ierr,pcur=ps%curt(ps%nrho_eq))
  call ckerr('ps_mhdeq_verify')
  
  write(iout,*) ' '
  write(iout,*) ' -- flux surface computations -- '
  call ps_mhdeq_derive('Everything',ierr)
  call ckerr('ps_mhdeq_derive')

  t1 = mytime() - t1
  write(iout,'(1x,a,t34,1pe10.3)') 'Time for ps_mhdeq_derive: ', t1

  !==========================================================================
  ! (f1) machine description and shot configuration sections completed
  ! setting locks causes further changes to be flagged as errors.  Here
  ! also, calls are made to exerciste machine description and shot 
  ! configuration namelist I/O routines.

  CALL ps_update_hashCodes(ierr, mdescr_lock=.TRUE., sconfig_lock=.TRUE.)

  write(iout,*) ' '
  write(iout,*) ' plasma_state_test: locked machine description (mdescr).'
  write(iout,*) ' plasma_state_test: locked shot configuration (sconfig).'

  write(iout,*) '   mdescr namelist write test...'
  call ps_mdescr_write('mdescr_sample_namelist.dat',ierr)
  call ckerr('ps_mdescr_write')

  write(iout,*) '   sconfig namelist read test...'
  call ps_sconfig_write('sconfig_sample_namelist.dat',ierr)
  call ckerr('ps_sconfig_write')

  ! read these namelists into a separate state object

  write(iout,*) '   mdescr namelist read test...'
  call ps_mdescr_read('mdescr_sample_namelist.dat',ierr, state=aux)
  call ckerr('ps_mdescr_write')

  write(iout,*) '   sconfig namelist read test...'
  call ps_sconfig_read('sconfig_sample_namelist.dat',ierr, state=aux)
  call ckerr('ps_sconfig_write')

  ! store to file
  CALL ps_store_plasma_state(ierr, state=aux)

  write(iout,*) ' namelist I/O tests completed.'

  !==========================================================================
  ! (g) initial profiles

  !------------------------------------------------
  !  densities -- ps%ns(1:ps%nrho-1,0:ps%nspec_th)
  !     step function zone oriented data

  !  zone centered grid (construct local copy)

  nrho_zc = ps%nrho - 1
  allocate(rho_zc(nrho_zc))
  rho_zc = (ps%rho(1:nrho_zc) + ps%rho(2:ps%nrho))/2

  !   electron density
  call profgen(den0, dena, rho_zc, ps%ns(:,0))

  !   D and impurity density...
  !   will use ne*Zeff = sum[i]((Z[i]^2*n[i])
  !        and      ne = sum[i](Z[i]*n[i])

  !   and account for depletion by minority ion species

  allocate(Zeff(nrho_zc))

  call profgen(Zeff0, Zeffa, rho_zc, Zeff)  ! Zeff profile

  Zimp = ps%q_s(2)/ps_xe  ! Zimp (=6)

  do i=1,nrho_zc
     !  depletion due to RF minority species
     zne_adj = (ONE-frac_mino*iZ_mino)*ps%ns(i,0)
     zzne_adj = ps%ns(i,0)*Zeff(i) - iZ_mino**2*frac_mino*ps%ns(i,0)

     !  thermal species  sum n*Z = zne_adj
     !                   sum n*Z**2 = zzne_adj

     !  in this little test program there are only 2 ion species, so this
     !  is 2 eqns in 2 unknowns.  In real codes, a mix of multiple non-
     !  impurity species becomes part of the initial condition...

     !         nD + nimp*Zimp = zne_adj
     !         nD + nimp*Zimp*Zimp = zzne_adj

     !         nimp = (zzne_adj-zne_adj)/(Zimp*(Zimp-1))
     !         nD   = (zne_adj*Zimp-zzne_adj)/(Zimp-1)

     ps%ns(i,1) = (zne_adj*Zimp-zzne_adj)/(Zimp-ONE)   ! nD
     ps%ns(i,2) = (zzne_adj-zne_adj)/(Zimp*(Zimp-ONE)) ! nimp (C)
     ps%Zeff(i) = Zeff(i)
  enddo

  i_nonZero=2  ! non-zero densities up to this index only...

  !------------------------------------------------
  !  Temperatures -- ps%Ts(1:ps%nrho-1,0:ps%nspec_th)
  !     step function zone oriented data

  !  Here, all receive the same temperature

  do ith=0,ps%nspec_th
     call profgen(T0, Ta, rho_zc, ps%TS(:,ith))
  enddo
  
  !  Compute test volume integrals, based on profile of EQ volumes
  allocate(zvols(ps%nrho),volref_ns(0:ps%nspec_th),volref_nt(0:ps%nspec_th))
  allocate(maxdiff_ns(0:ps%nspec_th),maxdiff_nt(0:ps%nspec_th))
  allocate(dmaxn(0:ps%nspec_th),dmaxt(0:ps%nspec_th))

  !  Use interpolation call *** get volume on "nrho" grid
  call ps_intrp_1d(ps%rho, ps%id_vol, zvols, ierr)
  call ckerr('ps_intrp_1d')

  do ith=0,ps%nspec_th
     maxdiff_ns(ith)=ZERO
     maxdiff_nt(ith)=ZERO

     volref_ns(ith)=ZERO
     volref_nt(ith)=ZERO
     do i=1,ps%nrho-1
        zdvol = zvols(i+1)-zvols(i)
        volref_ns(ith)=volref_ns(ith) + zdvol*ps%ns(i,ith)
        volref_nt(ith)=volref_nt(ith) + zdvol*ps%ns(i,ith)*ps%Ts(i,ith)
     enddo
  enddo

  !  store plasma parameters so these can be used to fill minority 
  !  profiles -- involves interpolation to a different grid

  isave = ps_debug
  ps_debug = 0

  call ps_state_memory_update(ierr,eqcheck=.FALSE.)
  call ckerr('ps_state_memory_update')

  ps_debug = isave

  !---------------------------------------
  !  Minority parameters... derived from plasma parameters, ad hoc

  call ps_rho_rezone(ps%rho_icrf, ps%id_ns(0), ps%nmini(:,1), ierr)
  call ckerr('ps_rho_rezone (set up minority Eperp)')
  ps%nmini(:,1) = ps%nmini(:,1) * frac_mino

  call ps_rho_rezone(ps%rho_icrf, ps%id_Ts(1), ps%eperp_mini(:,1), ierr)
  call ckerr('ps_rho_rezone (set up minority Eperp)')

  ps%eperp_mini(:,1) = ps%eperp_mini(:,1)*10.0_rspec
  ps%epll_mini(:,1) = ps%eperp_mini(:,1)/2.0_rspec
  write(iout,'(1x,a,3(1x,1pe12.5)/)') &
       ' minority density, Eperp, Epll on axis:',ps%nmini(1,1), &
       ps%eperp_mini(1,1),ps%epll_mini(1,1)

  !---------------------------------------

  !------------------------------------
  write(iout,'(1x,a)') ' '
  write(iout,'(1x,a)') ' I/O tests ...'
  write(iout,'(1x,a)') ' '
  write(iout,'(1x,a)') ' -- storing plasma state -- '

  t1 = mytime()
  CALL ps_store_plasma_state(ierr)
  call ckerr('ps_store_plasma_state')
  t1 = mytime() - t1
  write(iout,'(1x,a,t34,1pe10.3)') 'Time for ps_store_plasma_state: ', t1

  write(iout,'(1x,a)') ' '
  write(iout,'(1x,a)') ' -- committing plasma state -- '
  t2 = mytime()
  CALL ps_commit_plasma_state(ierr)
  call ckerr('ps_commit_plasma_state')
  t2 = mytime() - t2
  write(iout,'(1x,a,t34,1pe10.3)') 'Time for ps_commit_plasma_state: ', t2

  !  look for differences btw ps and psp...
  write(iout,*) ' '
  CALL plasma_state_hash_diff(ps,'ps',psp,'psp','*',icomp,idiff)
  if(idiff.eq.0) then
     write(iout,*) ' --> no differences, as expected.'
  else
     write(iout,*) ' -->>>> Unexpected differences !!! <<<<<--'
  endif

  write(iout,'(1x,a)') ' '
  write(iout,'(1x,a)') ' -- clearing plasma state profiles to ZERO -- '
  t3 = mytime()
  CALL ps_clear_profs(ierr)
  call ckerr('ps_clear_profs')
  t3 = mytime() - t3
  write(iout,'(1x,a,t34,1pe10.3)') 'Time for ps_clear_profs: ', t3

  write(iout,'(1x,a)') ' '
  write(iout,'(1x,a)') ' -- get (restore) plasma state from file -- '
  t4 = mytime()
  CALL ps_get_plasma_state(ierr)
  call ckerr('ps_get_plasma_state')
  t4 = mytime() - t4
  write(iout,'(1x,a,t34,1pe10.3)') 'Time for ps_get_plasma_state: ', t4

  !  look for differences btw ps and psp...
  CALL plasma_state_hash_diff(ps,'ps',psp,'psp','*',icomp,idiff)
  if(idiff.eq.0) then
     write(iout,*) ' --> no differences, as expected.'
  else
     write(iout,*) ' -->>>> Unexpected differences !!! <<<<<--'
  endif
  
  write(iout,'(1x,a)') ' '
  write(iout,'(1x,a)') ' -- storing plasma state -- '
  t5 = mytime()
  CALL ps_store_plasma_state(ierr)
  call ckerr('ps_store_plasma_state')
  t5 = mytime() - t5
  write(iout,'(1x,a,t34,1pe10.3)') 'Time for ps_store_plasma_state: ', t5

  write(iout,'(1x,a)') ' -- current and prior state files should be identical.'

  !  look for differences btw ps and psp...
  CALL plasma_state_hash_diff(ps,'ps',psp,'psp','*',icomp,idiff)
  if(idiff.eq.0) then
     write(iout,*) ' --> no differences, as expected.'
  else
     write(iout,*) ' -->>>> Unexpected differences !!! <<<<<--'
  endif

  !------------------------
  !  partial copy

  write(iout,*) ' '
  write(iout,*) ' partial copy test (PLASMA component only; no MHD equilibrium)'
  write(iout,*) ' '

  call ps_cclist_remove('*',cclist,iwarn)
  call ps_cclist_add('PLASMA',cclist,iwarn)

  !  cclist contains the PLASMA component only.

  call ps_copy_plasma_state(ps, aux, ierr, cclist=cclist)
  call ckerr('ps_copy_plasma_state')

  call ps_store_plasma_state(ierr, filename='partial_state.cdf', state=aux)
  call ckerr('ps_store_plasma_state')

  !------------------------------------

  write(iout,*) ' '
  write(iout,*) '  Test using local, un-initialized state-- warning expected.'
  CALL ps_get_plasma_state(ierr,state=my_state, filename='partial_state.cdf')
  call ckerr('ps_get_plasma_state')
  write(iout,*) ' '

  !------------------------------------
  !  new dmc Dec 2006

  write(iout,'(1x,a)') ' '
  write(iout,'(1x,a)') ' limiter data test ... '
  write(iout,'(1x,a)') ' '

  !  from version 2.00 on, the plasma state contains elements describing
  !  an axisymmetric limiter...

  write(iout,'(1x,a,1x,i4)') &
       ' Number of points in closed axisymmetric limiter contour: ', &
       ps%num_rzlim

  do ipts=1,2
     write(iout,2001) ipts,ps%rlim(ipts),ps%zlim(ipts)
  enddo

  do ipts=ps%num_rzlim-1,ps%num_rzlim
     write(iout,2001) ipts,ps%rlim(ipts),ps%zlim(ipts)
  enddo

2001 format(3x,' limiter point #',i3,' (R,Z) = (',1pe13.6,',',1pe13.6,')')

  write(iout,'(1x,a,2(1x,1pe16.9))') &
       ' limiter Rmin,Rmax: ',minval(ps%rlim),maxval(ps%rlim)
  write(iout,'(1x,a,2(1x,1pe16.9))') &
       ' limiter Zmin,Zmax: ',minval(ps%zlim),maxval(ps%zlim)

  !------------------------------------
  !  new dmc Feb 2008

  write(iout,'(1x,a)') ' '
  write(iout,'(1x,a)') ' abridged species list data fetch test ... '
  write(iout,'(1x,a)') ' '

  !  fetch temperatures, densities, and flows: abridged species

  allocate(tha_test(ps%nrho-1,ps%nspec_tha))

  call ps_tha_fetch(ierr,ns=tha_test,iwarn=iwarn)
  call ckerr('ps_tha_fetch(ns)')
  write(iout,'(a,2(1x,1pe16.9))') &
       ' "abridged" impurity densities, 1st zone: ', &
       tha_test(1,ps%nspec_tha-1),tha_test(1,ps%nspec_tha)

  call ps_tha_fetch(ierr,Ts=tha_test,iwarn=iwarn)
  call ckerr('ps_tha_fetch(Ts)')
  write(iout,'(a,2(1x,1pe16.9))') &
       ' "abridged" impurity temperatures, 1st zone: ', &
       tha_test(1,ps%nspec_tha-1),tha_test(1,ps%nspec_tha)

  call ps_tha_fetch(ierr,v_pars=tha_test,iwarn=iwarn)
  call ckerr('ps_tha_fetch(v_pars)')

  deallocate(tha_test)

  allocate(tha_test(1,ps%nspec_tha))
  call ps_tha_rezone((/ 0.0_rspec, 1.0_rspec /),ierr, ns=tha_test, &
       zonevol=test_voltot)
  call ckerr('ps_tha_rezone(ns)')

  tha_test=tha_test*test_voltot(1)
  write(iout,'(a,2(1x,1pe16.9))') &
       ' "abridged" impurity density volume integrals:', &
       tha_test(1,ps%nspec_tha-1),tha_test(1,ps%nspec_tha)

  call ps_tha_rezone((/ 0.0_rspec, 1.0_rspec /),ierr, Ts=tha_test)
  call ckerr('ps_tha_rezone(Ts)')
  write(iout,'(a,2(1x,1pe16.9))') &
       ' "abridged" impurity temperature averages: ', &
       tha_test(1,ps%nspec_tha-1),tha_test(1,ps%nspec_tha)

  call ps_tha_rezone((/ 0.0_rspec, 1.0_rspec /),ierr, v_pars=tha_test)
  call ckerr('ps_tha_rezone(v_pars)')

  deallocate(tha_test)

  !  Collective Ti fetch/rezone test
  allocate(ttest1(ps%nrho-1),ttest2(ps%nrho-1),ttest3(ps%nrho-1))
  ttest1 = ps%ts(:,1)
  call ps_ti_fetch(ierr,state=ps,ti=ttest2); call ckerr('ps_ti_fetch')
  call ps_ti_rezone(ps%rho,ierr,state=ps,ti=ttest3); call ckerr('ps_ti_rezone')

  do i=1,ps%nrho-1
     zdiff1=abs(ttest2(i)-ttest1(i))/max(ttest2(i),ttest1(i))
     zdiff2=abs(ttest3(i)-ttest1(i))/max(ttest3(i),ttest1(i))
     if(max(zdiff1,zdiff2).gt.(1.0d-12)) then
        write(iout,*) ' ?? PS_TI_FETCH/PS_TI_REZONE result outside tolerance:'
        write(iout,*) '    zone ',i,' original, fetch, rezone values: ', &
             ttest1(i),ttest2(i),ttest3(i)
     endif
  enddo
  deallocate(ttest1,ttest2,ttest3)
  !------------------------------------
  write(iout,'(1x,a)') ' '
  write(iout,'(1x,a)') ' 1d interpolation test ... '
  write(iout,'(1x,a)') ' '

  allocate(zb_test(ps%nrho))
  call ps_zone2bdy_rezone(ps%id_Ts(0),zb_test,ierr)
  call ckerr('ps_zone2bdy_rezone')

  write(iout,'("  Te on axis: ",1pe12.5)') zb_test(1)
  do i=2,7
     write(iout,'("  Te at zone center #",i1,": ",1pe12.5)') i-1,ps%Ts(i-1,0)
     write(iout,'("  Te at zone bdy    #",i1,": ",1pe12.5)') i,zb_test(i)
  enddo
  write(iout,*) '  ... ... '
  do i=ps%nrho-5,ps%nrho
     write(iout,'("  Te at zone center #",i3,": ",1pe12.5)') i-1,ps%Ts(i-1,0)
     write(iout,'("  Te at zone bdy    #",i3,": ",1pe12.5)') i,zb_test(i)
  enddo

  inum=11
  allocate(rho_intrp(inum),dvdrho(inum),dpsidrho(inum))
  allocate(buf2(inum-1,2))

  do i=1,inum
     rho_intrp(i) = (i-1)*ONE/(inum-1)
  enddo

  ! scalar interpolations -- refer to current state by default

  call ps_intrp_1d(ZERO,ps%id_vol,vpp0,ierr, ideriv=2)    ! d2(Vol)/drho2 @axis
  call ckerr('ps_intrp_1d')
  call ps_intrp_1d(ZERO,ps%id_psipol,ppp0,ierr, ideriv=2) ! d2(Psi)/drho2 @axis
  call ckerr('ps_intrp_1d')
  
  write(iout,'(1x,a,1x,1pe16.9)') ' @axis: d2[Vol]/d2[Psi] = ', vpp0/ppp0

  ! scalar interpolations -- to refer to prior state:

  call ps_intrp_1d(ZERO,psp%id_vol,vpp0,ierr, ideriv=2, state=psp)
  call ckerr('ps_intrp_1d')
  call ps_intrp_1d(ZERO,psp%id_psipol,ppp0,ierr, ideriv=2, state=psp)
  call ckerr('ps_intrp_1d')
  
  write(iout,'(1x,a,1x,1pe16.9,1x,a)') &
       ' @axis: d2[Vol]/d2[Psi] = ', vpp0/ppp0,' (prior state)'

  dvdrho(1)=vpp0   ! OK for ratio
  dpsidrho(1)=ppp0 ! OK for ratio

  ! one call can also do two interpolations to a common target vector,
  ! yielding a 2d array of values; array & vector sizes must be consistent.
  ! (If not errors are reported).
  
  ids(1)=ps%id_vol
  ids(2)=ps%id_psipol

  ! evaluate 1st derivative of both profiles.

  call ps_intrp_1d(rho_intrp(2:inum),ids,buf2,ierr, ideriv=1)
  call ckerr('ps_intrp_1d')

  ! a vector iderivs(:) could also have been specified, giving seperate 
  ! control of derivative for each profile id...

  dvdrho(2:inum)=buf2(:,1)
  dpsidrho(2:inum)=buf2(:,2)

  write(iout,'(1x,a)') ' '
  write(iout,'(1x,a)') '  #   rho             dVol/dPsi'
  do i=1,inum
     write(iout,'(1x,i2,1x,2(1pe15.8,1x))') &
          i,rho_intrp(i),dvdrho(i)/dpsidrho(i)
  enddo

  deallocate(rho_intrp,dvdrho,dpsidrho,buf2)

  !------------------------------------
  write(iout,'(1x,a)') ' '
  write(iout,'(1x,a)') ' 2d interpolation test ... '
  write(iout,'(1x,a)') ' '

  call ps_intrp_2d(ZERO,ZERO,ps%id_R_geo,zansR0,ierr)
  call ckerr('ps_intrp_2d')
  call ps_intrp_2d(ZERO,ZERO,ps%id_Z_geo,zansZ0,ierr)
  call ckerr('ps_intrp_2d')

  write(iout,'(1x,a,1x,2(1pe16.9,1x))') ' [R,Z]axis (state scalars):', &
       ps%R_axis,ps%Z_axis

  write(iout,'(1x,a,1x,2(1pe16.9,1x))') ' [R,Z]axis (interpolation):', &
       zansR0,zansZ0

  ids(1) = ps%id_R_geo
  ids(2) = ps%id_Z_geo

  inum=1001
  allocate(rho_intrp(inum),th_intrp(inum),buf2(inum,2))

  rho_intrp = ONE  ! replicated 1001 times...
  do i=1,inum
     th_intrp(i) = (i-1)*6.2831853071795862_rspec/(inum-1)
  enddo

  ! Here a 1000-pt bdy (R,Z) contour is evaluated

  call ps_intrp_2d(rho_intrp,th_intrp,ids,buf2,ierr)
  call ckerr('ps_intrp_2d')

  test_Rmin = minval(buf2(:,1))  
  test_Rmax = maxval(buf2(:,1))

  test_Zmin = minval(buf2(:,2))  
  test_Zmax = maxval(buf2(:,2))

  write(iout,'(1x,a)') ' '
  write(iout,'(1x,a)') ' Last Closed Flux Surface Extrema:'
  write(iout,'(1x,a)') ' (1000 pt [R,Z] surface contour was evaluated by interpolation):'
  write(iout,'(1x,a)') ' '
  write(iout,'(1x,a,1x,2(1pe16.9,1x))') ' Rmin,Rmax (state scalars): ', &
       ps%R_min_lcfs,ps%R_max_lcfs
  write(iout,'(1x,a,1x,2(1pe16.9,1x))') ' Rmin,Rmax (1000 pt search):', &
       test_Rmin,test_Rmax
  write(iout,'(1x,a,1x,2(1pe16.9,1x))') ' Zmin,Zmax (state scalars): ', &
       ps%Z_min_lcfs,ps%Z_max_lcfs
  write(iout,'(1x,a,1x,2(1pe16.9,1x))') ' Zmin,Zmax (1000 pt search):', &
       test_Zmin,test_Zmax

  deallocate(rho_intrp,th_intrp,buf2)

  !------------------------------------
  !  demonstration of rho-rezoning
  !  when the density "ns" is rezoned, its volume integral is preserved
  !  when the temperature "Ts" is rezoned, the volume integrals of (ns*Ts)
  !  are conserved species by species.  The weighting of Ts is defined in
  !  "plasma_state_spec.dat".

  inum=1
  do ict = 1,3

     inum = inum + 10

     allocate(rho_bdys(inum),dvols(inum-1))  ! grid; volumes zone by zone

     allocate(my_ns(inum-1,0:ps%nspec_th))    ! densities...
     allocate(my_Ts(inum-1,0:ps%nspec_th))    ! Temperatures...

     allocate(my_ns2(inum-1,0:ps%nspec_th))   ! densities (user_rezone)
     allocate(my_Ts2(inum-1,0:ps%nspec_th))   ! Temperatures (user_rezone)

     allocate(volint_ns(0:ps%nspec_th),volint_nt(0:ps%nspec_th))

     do i=1,inum
        rho_bdys(i) = (i-1)*ONE/(inum-1)
     enddo

     call ps_rho_rezone(rho_bdys, ps%id_ns, my_ns, ierr, zonediff = dvols)
     call ckerr('ps_rho_rezone')
     call ps_rho_rezone(rho_bdys, ps%id_Ts, my_Ts, ierr)
     call ckerr('ps_rho_rezone')

     call ps_user_rezone(ps%rho, rho_bdys, ps%ns, my_ns2, ierr, &
          interp=-1)
     call ckerr('ps_user_rezone')

     call ps_user_rezone(ps%rho, rho_bdys, ps%Ts, my_ts2, ierr, &
          interp=-1, w_orig = ps%ns)
     call ckerr('ps_user_rezone')

     write(iout,'(1x,a)') ' '
     write(iout,'(1x,a,i3,a)') ' test with ',inum-1,' zones:'
     write(iout,'(1x,a)') ' zone  rho            ne             Te             nD             Ti '

     volint_ns = ZERO
     volint_nt = ZERO

     dmaxn = ZERO
     dmaxt = ZERO

     do i=1,inum-1
        do ith=0,ps%nspec_th
           dmaxn(ith) = max(dmaxn(ith), &
                abs(my_ns(i,ith)-my_ns2(i,ith))/max(small_n,my_ns(i,ith)))
           if(my_ns(i,ith).eq.0.0_rspec) then
              dmaxt(ith) = 0.0_rspec
           else
              dmaxt(ith) = max(dmaxt(ith), &
                   abs(my_Ts(i,ith)-my_Ts2(i,ith))/max(small_T,my_Ts(i,ith)))
           endif
        enddo
        zrho = (rho_bdys(i) + rho_bdys(i+1))/2
        write(iout,'(1x,i2,1x,5(1x,1pe14.7))') i,zrho, &
             my_ns(i,0),my_Ts(i,0),my_ns(i,1),my_Ts(i,1)

        volint_ns = volint_ns + dvols(i)*my_ns(i,:)
        volint_nt = volint_nt + dvols(i)*my_ns(i,:)*my_Ts(i,:)

     enddo

     write(iout,'(1x,a,3(1x,1pe16.9))') 'VOLINT_ns: ',VOLINT_ns(0:i_nonZero)
     write(iout,'(1x,a,3(1x,1pe16.9))') 'Volint_nt: ',Volint_nt(0:i_nonZero)
     write(iout,'(1x,a,2(1x,1pe11.4))') &
          ' ps_rho_rezone vs. ps_user_rezone (0 diff expected): ', &
          maxval(dmaxn),maxval(dmaxt)

     do ith=0,ps%nspec_th
        maxdiff_ns(ith)=max(maxdiff_ns(ith), &
             abs(volint_ns(ith)-volref_ns(ith))/max(small_n,volref_ns(ith)))
        if(volref_ns(ith).eq.0.0_rspec) then
           maxdiff_nt(ith)= 0.0_rspec
        else
           maxdiff_nt(ith)=max(maxdiff_nt(ith), &
                abs(volint_nt(ith)-volref_nt(ith))/max(small_n,volref_nt(ith)))
        endif
     enddo

     if(ict.lt.3) then
        deallocate(rho_bdys,dvols,my_ns,my_ts, &
             my_ns2,my_ts2,volint_ns,volint_nt)
     endif
  enddo

  allocate(my_ns1(inum-1),my_Ts1(inum-1))
  call ps_user_rezone(ps%rho, rho_bdys, ps%ns(:,1), my_ns1, ierr)
  call ckerr('ps_user_rezone')
  call ps_user_rezone(ps%rho, rho_bdys, ps%Ts(:,1), my_Ts1, ierr, &
       w_orig = ps%ns(:,1))
  call ckerr('ps_user_rezone')

  dmaxn = ZERO
  dmaxt = ZERO

  do i=1,inum-1
     dmaxn(1) = max(dmaxn(1),abs(my_ns(i,1)-my_ns1(i))/max(small_n,my_ns(i,1)))
     dmaxt(1) = max(dmaxt(1),abs(my_Ts(i,1)-my_Ts1(i))/max(small_T,my_Ts(i,1)))
  enddo
  write(iout,'(1x,a,2(1x,1pe11.4))') &
       ' ps_rho_rezone vs. ps_user_rezone (0 diff expected): ', &
       maxval(dmaxn),maxval(dmaxt)

  allocate(my_profa(ps%nrho-1,0:ps%nspec_th,2))
  allocate(my_profb(ps%nrho-1,0:ps%nspec_th,2))
  allocate(my_profc(inum-1,0:ps%nspec_th,2))

  my_profa(:,:,1) = ps%ns
  my_profa(:,:,2) = ps%Ts
  my_profb(:,:,1) = ZERO
  my_profb(:,:,2) = ps%ns

  call ps_user_rezone(ps%rho, rho_bdys, my_profa, my_profc, ierr, &
       w_orig = my_profb)
  call ckerr('ps_user_rezone')

  dmaxn = ZERO
  dmaxt = ZERO

  do i=1,inum-1
     do ith=0,ps%nspec_th
        dmaxn(ith) = max(dmaxn(ith), &
             abs(my_ns(i,ith)-my_profc(i,ith,1))/max(small_n,my_ns(i,ith)))
        if(my_ns(i,ith).eq.0.0_rspec) then
           dmaxt(ith) = 0.0_rspec
        else
           dmaxt(ith) = max(dmaxt(ith), &
                abs(my_Ts(i,ith)-my_profc(i,ith,2))/max(small_T,my_Ts(i,ith)))
        endif
     enddo
  enddo
  write(iout,'(1x,a,2(1x,1pe11.4))') &
       ' ps_rho_rezone vs. ps_user_rezone (0 diff expected): ', &
       maxval(dmaxn),maxval(dmaxt)

  allocate(zdvola(ps%nrho-1),zdvolc(size(rho_bdys)-1))

  call ps_rho_metric('USER','DVOL_EQ',zdvola,ierr, user_rho_grid = ps%rho)
  call ckerr('ps_rho_metric')
  call ps_rho_metric('USER','DVOL_EQ',zdvolc,ierr, user_rho_grid = rho_bdys)
  call ckerr('ps_rho_metric')

  do ith=0,ps%nspec_th
     my_profa(:,ith,1) = my_profa(:,ith,1)*zdvola
     my_profa(:,ith,2) = my_profa(:,ith,2)*zdvola
     my_profb(:,ith,1) = my_profb(:,ith,1)*zdvola
     my_profb(:,ith,2) = my_profb(:,ith,2)*zdvola
  enddo

  call ps_user_rezone(ps%rho, rho_bdys, my_profa, my_profc, ierr, &
       w_orig = my_profb, nonorm = .TRUE.)
  call ckerr('ps_user_rezone')

  dmaxn = ZERO
  dmaxt = ZERO

  do i=1,inum-1
     do ith=0,ps%nspec_th
        dmaxn(ith) = max(dmaxn(ith), &
             abs(my_ns(i,ith)-my_profc(i,ith,1)/zdvolc(i))/max(small_n,my_ns(i,ith)))
        if(my_ns(i,ith).eq.0.0_rspec) then
           dmaxt(ith) = 0.0_rspec
        else
           dmaxt(ith) = max(dmaxt(ith), &
                abs(my_Ts(i,ith)-my_profc(i,ith,2))/max(small_T,my_Ts(i,ith)))
        endif
     enddo
  enddo
  write(iout,*) '   ps_user_rezone nonorm=T (arrays #/zone in place of #/m^3)'
  write(iout,'(1x,a,2(1x,1pe11.4))') &
       ' ps_rho_rezone vs. ps_user_rezone (near 0 diff exp): ', &
       maxval(dmaxn),maxval(dmaxt)
 
  do ith=0,ps%nspec_th
     write(iout,'(1x,a)') ' '
     if(ith.le.i_nonZero) then
        write(iout,'(1x,a,a)') ' species: ',ps%s_name(ith)
        write(iout,'(1x,a,1pe16.9)') &
             ' Max deviation, volint(n): ',maxdiff_ns(ith)
        write(iout,'(1x,a,1pe16.9)') &
             ' Max deviation, volint(n*T): ',maxdiff_nt(ith)
     else
        write(iout,'(1x,a,a)') ' species: '//trim(ps%s_name(ith))// &
             ': ZERO density'
     endif
  enddo

  !------------------------------------
  !  May 2008: demonstrate
  !  creation of single arrays of all the densities and temperatures
  !  for the nspec_alla list:
  !       abridged thermal ion species + all fast species
  !
  !  rezoned to a boundary oriented grid; use rho_bdy_rezone(...) for
  !  this rezoning.
  !

  nrho_user=201
  nrhop1=nrho_user+1
  allocate(rho_user(nrho_user),rho_bdy_rezon(nrhop1))

  do ii=1,nrho_user
     rho_user(ii) = (ii-1)*1.0_rspec/(nrho_user-1)
  enddo

  !  rho_bdy_rezon(...) has a half width zone at the center and edge, and a
  !  full width zone around each interior boundary in rho_user(...).
  !  size(rho_bdy_rezon) = size(rho_user) + 1

  rho_bdy_rezon(1)=rho_user(1)
  rho_bdy_rezon(nrhop1)=rho_user(nrho_user)
  rho_bdy_rezon(2:nrho_user) = &
       0.5_rspec*(rho_user(1:nrho_user-1)+rho_user(2:nrho_user))

  allocate(dense(nrho_user),tempe(nrho_user))
  allocate(densi(nrho_user,ps%nspec_alla),tempi(nrho_user,ps%nspec_alla))

  call ps_rho_rezone(rho_bdy_rezon, ps%id_ns(0), dense, ierr, zonesmoo=.TRUE.)
  call ckerr('ps_rho_rezone (U1)')

  call ps_rho_rezone(rho_bdy_rezon, ps%id_Ts(0), tempe, ierr, zonesmoo=.TRUE.)
  call ckerr('ps_rho_rezone (U2)')

  !  get the abridged thermal species list first...

  call ps_tha_rezone(rho_bdy_rezon, ierr, zonesmoo=.TRUE., &
       ns = densi(:,1:ps%nspec_tha), Ts = tempi(:,1:ps%nspec_tha))
  call ckerr('ps_tha_rezone')

  !  get the fast species; in general these will be on different grids.

  allocate(wk_dens(nrho_user),wk_eperp(nrho_user),wk_epll(nrho_user))

  do ii=ps%nspec_tha + 1, ps%nspec_alla
     jj = ps%alla_index(ii)   ! index in "all species" list
     !       jj=-1 if ps%alla_type(ii).eq.ps_impurity, but this should not
     !       occur as here we loop over fast ions only.  Still, I test this...
     if(jj.le.0) then
        write(iout,*) ' ? unexpected alla_index(ii) value: ',jj
     else
        !  OK, jj=alla_index(ii) points to the element index of whatever
        !  fast species list element corresponds to element ii of the alla list

        if(ps%alla_type(ii).eq.ps_beam_ion) then
           call ps_rho_rezone(rho_bdy_rezon, ps%id_nbeami(jj), wk_dens, &
                ier1, zonesmoo=.TRUE.)
           call ps_rho_rezone(rho_bdy_rezon, ps%id_eperp_beami(jj), wk_eperp, &
                ier2, zonesmoo=.TRUE.)
           call ps_rho_rezone(rho_bdy_rezon, ps%id_epll_beami(jj), wk_epll, &
                ier3, zonesmoo=.TRUE.)

        else if(ps%alla_type(ii).eq.ps_rf_minority) then
           call ps_rho_rezone(rho_bdy_rezon, ps%id_nmini(jj), wk_dens, &
                ier1, zonesmoo=.TRUE.)
           call ps_rho_rezone(rho_bdy_rezon, ps%id_eperp_mini(jj), wk_eperp, &
                ier2, zonesmoo=.TRUE.)
           call ps_rho_rezone(rho_bdy_rezon, ps%id_epll_mini(jj), wk_epll, &
                ier3, zonesmoo=.TRUE.)

        else if(ps%alla_type(ii).eq.ps_fusion_ion) then
           call ps_rho_rezone(rho_bdy_rezon, ps%id_nfusi(jj), wk_dens, &
                ier1, zonesmoo=.TRUE.)
           call ps_rho_rezone(rho_bdy_rezon, ps%id_eperp_fusi(jj), wk_eperp, &
                ier2, zonesmoo=.TRUE.)
           call ps_rho_rezone(rho_bdy_rezon, ps%id_epll_fusi(jj), wk_epll, &
                ier3, zonesmoo=.TRUE.)

        endif

        ierr = ier1 + ier2 + ier3
        call ckerr('ps_rho_rezone (fast specie)')

        densi(:,ii) = wk_dens
        tempi(:,ii) = (2.0_rspec/3.0_rspec)*(wk_eperp + wk_epll)

     endif
  enddo

  write(iout,'(A,1pe12.5)') &
       '  density      Temperature   Species @ rho_user(7) = ',rho_user(7)
  write(iout,'(2(1x,1pe12.5),3x,a)') dense(7),tempe(7), &
       trim(ps%alla_name(0))
  do ii=1,ps%nspec_alla
     write(iout,'(2(1x,1pe12.5),3x,a)') densi(7,ii),tempi(7,ii), &
          trim(ps%alla_name(ii))
  enddo

  !------------------------------------
  !  ps_rho_metric test...

  if(allocated(zvols)) deallocate(zvols)

  allocate(zvols(ps%nrho - 1))  ! use for volume elements here...

  write(iout,*) ' '
  write(iout,*) ' -------------------'
  write(iout,*) ' ps_rho_metric test.'
  write(iout,*) ' '

  zvols = 0
  call ps_rho_metric('RHO','DVOL',zvols,ierr)
  call ckerr('ps_rho_metric')
  write(iout,'(1x,a,1x,1pe16.9)') ' volume of 3rd zone: ',zvols(3)

  zvols = 0
  call ps_rho_metric('RHO','DAREA',zvols,ierr)
  call ckerr('ps_rho_metric')
  write(iout,'(1x,a,1x,1pe16.9)') ' area of 3rd zone: ',zvols(3)

  write(iout,*) &
       ' ... recomputation with "user" rho grid = ps%rho: should match.'

  zvols = 0
  call ps_rho_metric('USER_RHO','DVOL',zvols,ierr, user_rho_grid = ps%rho)
  call ckerr('ps_rho_metric')
  write(iout,'(1x,a,1x,1pe16.9)') ' volume of 3rd zone: ',zvols(3)

  zvols = 0
  call ps_rho_metric('USER_RHO','DAREA',zvols,ierr, user_rho_grid = ps%rho)
  call ckerr('ps_rho_metric')
  write(iout,'(1x,a,1x,1pe16.9)') ' area of 3rd zone: ',zvols(3)

  write(iout,*) ' '
  write(iout,*) ' -------------------'
  write(iout,*) ' partial update test'
  write(iout,*) ' '

  ! state updates (updates only) can be propagated through files
  ! here form an update consisting of 3 elements: ts, t0, t1

  aux%ts(:,0) = 1.01_rspec*aux%ts(:,0)  ! increase Te by 1%
  aux%t0 = aux%t0 + 0.001_rspec
  aux%t1 = aux%t1 + 0.001_rspec

  ! write update file: PLASMA & EQ components only

  call ps_cclist_remove('*',cclist,iwarn)
  call ps_cclist_add('PLASMA',cclist,iwarn)
  call ps_cclist_add('EQ',cclist,iwarn)

  call ps_update_report(ierr,state=aux,cclist=cclist, &
       report_filename='update_test.list')
  call ckerr('ps_update_report (1)')

  call ps_write_update_file('update_test.cdf',ierr,state=aux, &
       cclist=cclist, iwarn=iwarn)
  call ckerr('ps_write_update_file (1)')

  ! read update file, but do not update hash codes (update_complete=.FALSE.)
  ! in normal usage, several updates might be read in sequence, with
  ! update_complete=.FALSE. until the last one is read with 
  ! update_complete=.TRUE. after which the interpolation structures are 
  ! checked and hash codes are updated.

  ! here, by not updating the hash codes, I can write another update file
  ! (it uses the hash codes to determine what has been updated).

  call ps_read_update_file('update_test.cdf',ierr,state=ps, &
       update_complete=.FALSE.)
  call ckerr('ps_read_update_file')

  ! ps state by default: ps_state_test.cdf

  call ps_update_report(ierr,report_filename='ps_update_test1.list')
  call ckerr('ps_update_report (2)')

  call ps_write_update_file('ps_update_test1.cdf',ierr)
  call ckerr('ps_write_update_file (2)')

  ! ps state by default: ps_state_test.cdf
  ! since the update was already written, writing a 2nd time shows 0 changes...

  call ps_update_report(ierr,report_filename='ps_update_test2.list')
  call ckerr('ps_update_report (3)')

  call ps_write_update_file('ps_update_test2.cdf',ierr)
  call ckerr('ps_write_update_file (3)')

  write(iout,*) ' plasma_state_test: update file test completed.'

  ! test merging data from 2 states into a third state (ps,psp) -> (aux)
  ! just the equilibrium (EQ) component data is merged.
  ! array dimensions for the EQ component are copied into (aux) first.

  call ps_cclist_remove('*',cclist,iwarn)
  call ps_cclist_add('EQ',cclist,iwarn)

  call ps_copy_dims(ps,aux,0,ierr,cclist=cclist)
  call ckerr('ps_copy_dims')

  call ps_merge_plasma_state(0.1_rspec,psp,ps,ierr, cclist=cclist, &
       new_state = aux, icheck=ps_warn)
  call ckerr('ps_merge_plasma_state')

  write(iout,*) ' plasma_state_test: merge test completed.'

  !  DMC: add this Apr 2011 -- source code, see below...
  call pspro_test

  write(iout,*) ' '
  t_overall = mytime() - t_overall
  write(iout,'(1x,a,t34,1pe10.3)') 'Time for overall execution: ', t_overall

  !------------------------------------
  !------------------------------------

  CONTAINS
    !------------------------------------
    SUBROUTINE rho_grid(nrho,rho)

      ! generate evenly spaced rho grid

      integer, intent(in) :: nrho    ! # of pts covering [0:1]
      REAL(KIND=rspec) :: rho(nrho)  ! the rho grid being generated

      REAL(KIND=rspec), parameter :: ONE = 1.0_rspec

      integer :: irho

      do irho = 1,nrho
         rho(irho) = (irho-1)*ONE/(nrho-1)
      enddo

    END SUBROUTINE rho_grid

    !------------------------------------
    SUBROUTINE th_grid(nth,th)

      ! generate evenly spaced theta grid [-pi:pi]

      integer, intent(in) :: nth    ! # of pts covering the range
      REAL(KIND=rspec) :: th(nth)   ! the theta grid being generated

      REAL(KIND=rspec), parameter :: PI = 3.1415926535897931_rspec

      integer :: ith

      do ith = 1,nth
         th(ith) = -PI + (ith-1)*2*PI/(nth-1)
      enddo

    END SUBROUTINE th_grid

    !------------------------------------
    SUBROUTINE profgen(f0,fa,rho,fans)

      !  quick & dirty profile generator

      REAL(KIND=rspec), intent(in) :: f0,fa  ! core and edge values
      REAL(KIND=rspec), dimension(:), intent(in) :: rho  ! normalized sqrt tor. flux
      REAL(KIND=rspec), dimension(:), intent(out) :: fans  ! formula output

      fans = (f0-fa)*(1.0_rspec-rho**2)**2 + fa

    END SUBROUTINE profgen

    REAL(KIND=rspec) FUNCTION mytime()

      real*8 :: ztime

      call cpu_time(ztime)

      mytime = ztime

    END FUNCTION mytime

    SUBROUTINE ckerr(sbrtn)
      character*(*), intent(in) :: sbrtn

      IF(ierr.NE.0) then
         write(6,*) ' ?plasma_state_test: error in call: '//trim(sbrtn)
         stop
      ENDIF
    END SUBROUTINE ckerr

end program plasma_state_test

subroutine pspro_test

  USE pspro
  USE plasma_state_mod
  implicit NONE

  !  test pspro routines...

  integer :: jintrp,ii,jjii,ierr

  real*8 :: rho(9),cloge(9,3),eta_sp(9,3),eta_nc(9,3)

  write(6,*) ' '
  write(6,*) ' Plasma_state_test: ---> pspro tests... '
  write(6,*) ' '

  ! prelim: get iota(bar) = 1/q
  !   NOTE: pspro routines call ps_state_memory_update; iota(bar)
  !         interpolation will be available

  call ps_intrp_1d(ps%rho,ps%id_q_eq,ps%iota,ierr, state=ps)
  do ii=1,ps%nrho
     if(ps%iota(ii).gt.0.0_rspec) then
        ps%iota(ii) = 1.0_rspec/ps%iota(ii)  ! q -> 1/q
     else
        write(6,*) ' ?pspro_test: singular q value: ',ps%iota(ii)
        ps%iota(ii) = 100.0_rspec
     endif
  enddo


  do ii=1,9
     rho(ii) = 0.1d0*ii
  enddo

  do jintrp=0,2
     jjii=jintrp+1

     call pspro_cloge(ps,rho,cloge(:,jjii),ierr, intrp=jintrp)
     if(ierr.ne.0) write(6,*) '  *** pspro_cloge: ierr = ',ierr

     call pspro_resis_hhb(ps,rho,eta_sp(:,jjii),eta_nc(:,jjii),ierr, &
          intrp=jintrp)
     if(ierr.ne.0) write(6,*) '  *** pspro_resis_hhb: ierr = ',ierr
  enddo

  write(6,*) ' '
  write(6,*) ' Electron coulomb log -- input profile interpolations varied:'
  write(6,*) '   #     rho        cloge(0)      cloge(1)      cloge(2)'
  do ii=1,9
     write(6,'(3x,i1,4(2x,1pe12.5))') ii,rho(ii),cloge(ii,1:3)
  enddo

  write(6,*) ' '
  write(6,*) ' HHB NC resistivity (Ohm/m) -- input profile interpolations varied:'
  write(6,*) '   #     rho        eta_nc(0)     eta_nc(1)     eta_nc(2)'
  do ii=1,9
     write(6,'(3x,i1,4(2x,1pe12.5))') ii,rho(ii),eta_nc(ii,1:3)
  enddo

end subroutine pspro_test
