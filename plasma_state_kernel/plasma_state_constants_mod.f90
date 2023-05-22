! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


MODULE plasma_state_constants_mod

  ! plasma_state header:

  use swim_global_data_mod, only: rspec, swim_err_out_file, swim_error
  implicit NONE


  PRIVATE

  !--------------------------------------------
  !
  !  PLASMA STATE MODULE -- CONSTANTS (PARAMETER statements)
  !
  !--------------------------------------------

  CHARACTER*32, PARAMETER, PUBLIC :: ps_version_id = "2.056"


  !---------------------------------------
  ! module parameters (constants):


  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_me = 9.1094e-31_rspec
         ! electron mass, kg

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_mp = 1.6726e-27_rspec
         ! proton mass, kg

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_xe = 1.6022e-19_rspec
         ! Joule/ptcl-eV (also, electron charge)

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_epslon = 1.0e-34_rspec
         ! small number

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_epsinv = 1.0e+34_rspec
         ! large number

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_pi = 3.1415926535897931_rspec
         ! pi

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_twopi = 6.2831853071795862_rspec
         ! 2*pi

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_zero = 0.0_rspec
         ! Zero

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_one = 1.0_rspec
         ! one

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_two = 2.0_rspec
         ! two

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_three = 3.0_rspec
         ! three

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_half = 0.5_rspec
         ! one-half

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_onethird = 0.3333333333333333_rspec
         ! one-third

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_twothird = 0.6666666666666667_rspec
         ! two-third

  REAL(KIND=rspec), PARAMETER, PUBLIC :: ps_mu0 = 1.2566e-6_rspec
         ! mu0

  INTEGER, PARAMETER, PUBLIC :: ps_electron = -1

  INTEGER, PARAMETER, PUBLIC :: ps_therm_ion = 1

  INTEGER, PARAMETER, PUBLIC :: ps_impurity = 2

  INTEGER, PARAMETER, PUBLIC :: ps_tokamakium = 3

  INTEGER, PARAMETER, PUBLIC :: ps_beam_ion = 4

  INTEGER, PARAMETER, PUBLIC :: ps_rf_minority = 5

  INTEGER, PARAMETER, PUBLIC :: ps_fusion_ion = 6

  INTEGER, PARAMETER, PUBLIC :: ps_elec_index = 0

  INTEGER, PARAMETER, PUBLIC :: ps_ccw_select = 1
         ! used as indicator of counter-clockwise oriented theta

  INTEGER, PARAMETER, PUBLIC :: ps_ccw_reverse = 2
         ! used as indicator of clockwise oriented theta

  INTEGER, PARAMETER, PUBLIC :: ps_stepdv = -3
         ! step function *dV in plasma state object

  INTEGER, PARAMETER, PUBLIC :: ps_stepda = -2
         ! step function *dA in plasma state object

  INTEGER, PARAMETER, PUBLIC :: ps_stepfun = -1
         ! step function

  INTEGER, PARAMETER, PUBLIC :: ps_pclin = 0
         ! piecewise linear function

  INTEGER, PARAMETER, PUBLIC :: ps_hermite = 1
         ! Hermite cubic interpolant

  INTEGER, PARAMETER, PUBLIC :: ps_spline = 2
         ! Cubic spline interpolant

  INTEGER, PARAMETER, PUBLIC :: ps_bc_dflt = -1
         ! Default boundary condition: "not a knot" for splines

  INTEGER, PARAMETER, PUBLIC :: ps_bc_grad00 = 0
         ! d/drho -> 0 as rho -> 0

  INTEGER, PARAMETER, PUBLIC :: ps_bc_xxx = 99
         ! This indicates Hermite item with all explicit nodal
         ! derivitaves as input

  INTEGER, PARAMETER, PUBLIC :: ps_uninit = -123456789
         ! value for un-initialized integer scalars

  INTEGER, PARAMETER, PUBLIC :: ps_components = 1

  INTEGER, PARAMETER, PUBLIC :: ps_array_dims = 2

  INTEGER, PARAMETER, PUBLIC :: ps_species_lists = 3

  INTEGER, PARAMETER, PUBLIC :: ps_data_sets = 4

  INTEGER, PARAMETER, PUBLIC :: ps_profile_sets = 5

  INTEGER, PARAMETER, PUBLIC :: ps_data = 6

  INTEGER, PARAMETER, PUBLIC :: ps_profiles = 7

  INTEGER, PARAMETER, PUBLIC :: ps_item_lists = 8

  INTEGER, PARAMETER, PUBLIC :: ps_grid_dims = 9

  INTEGER, PARAMETER, PUBLIC :: ps_machine_descr = 10

  INTEGER, PARAMETER, PUBLIC :: ps_shot_config = 11

  INTEGER, PARAMETER, PUBLIC :: ps_sim_control = 12

  INTEGER, PARAMETER, PUBLIC :: ps_enum_dims = 13

  INTEGER, PARAMETER, PUBLIC :: ps_unlocked = 0

  INTEGER, PARAMETER, PUBLIC :: ps_monitor = 1

  INTEGER, PARAMETER, PUBLIC :: ps_locked = 2

  INTEGER, PARAMETER, PUBLIC :: ps_nocheck = 0
         ! skip hash checks

  INTEGER, PARAMETER, PUBLIC :: ps_chkall = 1
         ! check profiles & dimensions & scalars

  INTEGER, PARAMETER, PUBLIC :: ps_chkprofs = 2
         ! check profiles & dimensions; skip scalars

  INTEGER, PARAMETER, PUBLIC :: ps_stable = 0
         ! constant indicating a stable linear MHD mode

  INTEGER, PARAMETER, PUBLIC :: ps_normal = 0
         ! LMHD code exit status: normal

  INTEGER, PARAMETER, PUBLIC :: ps_eqfail = 1
         ! LMHD code exit status: equilibrium refinement failed

  INTEGER, PARAMETER, PUBLIC :: ps_mapfail = 2
         ! LMHD code exit status: mapper failed
         ! >2 may indicate various failures peculiar to each LMHD code itself

  INTEGER, PARAMETER, PUBLIC :: ps_max_static = 500

  INTEGER, PARAMETER, PUBLIC :: ps_max_static_2d = 128

  !---------------------------------------
  ! CCW parameters (constants):

  INTEGER, PARAMETER, PUBLIC :: ps_ccw_nth_eq = ps_ccw_select

END MODULE plasma_state_constants_mod
