! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_contents_alloc(itype,icomponent,w,ierr)

  !  get counts on various tables of contents

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: itype   ! type of table of contents
                                 ! values such as ps_grid_dims or ps_profiles

  INTEGER, INTENT(IN) :: icomponent(ps_ccount) ! component flags
             ! icomponent(j).eq.1 => component j active
             ! e.g. icomponent(ps_cnum_EQ).eq.1 => EQ component is active

  TYPE (ps_list_window) :: w     ! list window structure allocated

  INTEGER, INTENT(OUT) :: ierr   ! status completion code (0 = OK)

  !local:

  INTEGER :: iout,isize

  !----------------
  iout = SWIM_err_out_file
  ierr = 0

  call free_window(w)

  isize = 0

  if(itype.eq.ps_grid_dims) then
     ! grid
     w%list_name='Grids'
  else if(itype.eq.ps_species_lists) then
     ! species list
     w%list_name='Species_Lists'
  else if(itype.eq.ps_item_lists) then
     ! item list
     w%list_name='Item_Lists'
  else if(itype.eq.ps_machine_descr) then
     ! machine description
     w%list_name='Machine_Description'
  else if(itype.eq.ps_shot_config) then
     ! shot configuration
     w%list_name='Shot_Configuration'
  else if(itype.eq.ps_sim_control) then
     ! simulation initialization data
     w%list_name='Simulation_Init'
  else if(itype.eq.ps_data) then
     ! time varying state data
     w%list_name='State_Data'
  else if(itype.eq.ps_profiles) then
     ! profile data
     w%list_name='Profiles'
  else if(itype.eq.ps_enum_dims) then
     ! enumeration dimensions
     w%list_name='Enumeration_Dimensions'
  else
     write(iout,'(a,i5,a)') '?ps_contents_alloc: itype = ',itype,' invalid.'
     ierr = 1
     return
  endif


  ! Enumeration Dimensions

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! nky_anomth
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! nky_mmm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! nrz_antgeo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! nspectrumlh_src
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! num_nphi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! num_nphi_vac
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! num_rzlim
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! n_D_einj_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! n_H_einj_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! n_straps
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_enum_dims) THEN
        isize = isize + 1  ! n_T_einj_standard
     ENDIF
  ENDIF
  ! Species Lists

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! nspec_all
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! nspec_alla
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! nspec_beam
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! nspec_fusion
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! nspec_gas
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! nspec_imp0
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! nspec_impi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! nspec_rfmin
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! nspec_th
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_species_lists) THEN
        isize = isize + 1  ! nspec_tha
     ENDIF
  ENDIF

  ! Item Lists

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! mmodes
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! nballoon
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! nbeam
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! ncircuits
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! ncoils
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! ndelta_w
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! necrf_src
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! neqmom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! ngsc0
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! nicrf_src
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! nlhrf_src
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! nnewcomb
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! npsmom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! ntrcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_item_lists) THEN
        isize = isize + 1  ! nxs
     ENDIF
  ENDIF

  ! Grids

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nefi_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nefi_anom2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nr
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_anom2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_anomth
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_ecrf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_eq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_eq_geo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_fus
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_gas
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_icrf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_lhrf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_lmhd
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_mist
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_nbi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PEL).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_pel
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_rad
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nrho_rw
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nr_rip
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nth_eq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nz
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_grid_dims) THEN
        isize = isize + 1  ! nz_rip
     ENDIF
  ENDIF

  ! Machine Description

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Ang_Href_coil
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Ang_Vref_coil
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ant_model
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap2_halfheight
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap2_halfwidth
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap2_horiz_offset
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap2_vert_offset
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap_halfheight
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap_halfwidth
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap_horiz_offset
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ap_vert_offset
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! beam_type
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! b_halfHeight
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! b_halfwidth
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! b_Hdivergence
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! b_Hfocal_length
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! b_Vdivergence
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! b_Vfocal_length
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! coil_in_circuit
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! coil_resispt
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! dx_fshield
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! D_einj_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! D_ffull_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! D_fhalf_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EC_Beam_Elongation
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EC_Half_Power_Angle
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EC_PRINC_CURV_HOR
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EC_PRINC_CURV_VERT
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EC_PRINC_WIDTH_HOR
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EC_PRINC_WIDTH_VERT
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Einj_max
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Einj_min
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! geometry
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Hsize_coil
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! H_einj_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! H_ffull_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! H_fhalf_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Lbscap
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Lbscap2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Lbsctan
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! nbap2_shape
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! nbap_shape
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! nbshape
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! nrz_antgeo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ntf_coil2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! ntf_coils
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Nturns
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! num_rzlim
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! n_D_einj_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! n_H_einj_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! n_T_einj_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Phibsc
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Phi_EC_launch
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! rlim
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Rloc_coil
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! R_antgeo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! R_EC_launch
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! R_max_box
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! R_min_box
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! sRtcen
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! tokamak_id
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! T_einj_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! T_ffull_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! T_fhalf_standard
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! VRep_coil_count
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Vrep_coil_spacing
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Vsize_coil
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Zbap
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Zbsc
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! zlim
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Zloc_coil
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Z_antgeo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Z_EC_launch
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Z_max_box
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! Z_min_box
     ENDIF
  ENDIF

  ! Shot Configuration

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! amaxlh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! aminlh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! apolmaxlh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! apolminlh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! atormaxlh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! atorminlh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! EC_Omode_Fraction
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! EC_phi_aim
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! EC_theta_aim
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! fracpslh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! freq_ec
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! freq_ic
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! freq_lh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! gas_atom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! hegrilh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! Imag_ant_coef
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! is_recycling
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! i_npoloidal
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! kccw_Bphi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! kccw_Jphi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nbion
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nbion_trace
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nnkparlh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nnkpollh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nnktorlh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nphi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nspectrumlh_src
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! nthinlh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! num_nphi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! num_nphi_vac
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! n_straps
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! Real_ant_coef
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! rhopsi0lh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! R_ant
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! shot_number
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! thegrilh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! wt_nphi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! Z_ant
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! Z_mid_ant
     ENDIF
  ENDIF

  ! Simulation Init

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! alla_index
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! all_index
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! ANOM_Code_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Balloon_mapper
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! CUR_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! DeltaW_mapper
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! EC_Code_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! EC_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! EQ_Code_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! EQ_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! fracmin
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! FUS_Code_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! GAS_Code_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Global_label
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! icircuit_coil
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! IC_Code_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! IC_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! kdens_rfmin
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! LH_Code_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! LH_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! LMHD_eq_code
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! NBI_Code_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! NBI_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Newcomb_mapper
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! nky_anomth
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! nky_mmm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! nmom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! ns_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PEL).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! PEL_Code_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PEL).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! PEL_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! PLASMA_Code_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! RAD_Code_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! RAD_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! rfmin_to_all
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! rfmin_to_alla
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! RUNAWAY_Code_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! RunID
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! sa_index
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! sc0_to_sgas
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! sfus_to_all
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! sfus_to_alla
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! sgas_to_s
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! simp0_to_s
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! simpi_to_s
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! snbi_to_all
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! snbi_to_alla
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! s_to_sa
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! tfinal
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! TF_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! tinit
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Tor_mode_No
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! trace_flag
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! trmodels
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Ts_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! vpol_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! VSUR_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! vtor_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Z0max
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! ZEFF_Data_Info
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Zimp1
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! Zimp2
     ENDIF
  ENDIF

  ! State Data

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Balloon_stable
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Balloon_status
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! B_axis
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! B_axis_vac
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! B_max_lcfs
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! B_min_lcfs
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! coil_apt
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! DeltaW_stable
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! DeltaW_status
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! dist_fun
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! dn0out
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! e0_av
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! eldot
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! eqdsk_file
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! frac_full
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! frac_half
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! gamma_lmhd
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! interchange_R
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! isThermal
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! kvolt_nbi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! LMHD_status
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! lock_machine_descr
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! lock_shot_config
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! lock_sim_init
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Mercier_stable
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! MHD_eq_status
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! nbeami_bdy
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! nebar
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Newcomb_stable
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Newcomb_status
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! nfusi_bdy
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! nmdifb
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! nmini_bdy
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! nmodel_bdy
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! ns_bdy
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! ns_is_input
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! omegat_bdy
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! picrf_abs
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! picrf_ext
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! power_ec
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! power_ic
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! power_lh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! power_nbi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! power_nbi_trace
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Psi_to_machine_axis
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! ql_operator
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! rho_bdy_ns
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! rho_bdy_omegat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! rho_bdy_Te
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! rho_bdy_Ti
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! R_axis
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! R_max_lcfs
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! R_min_lcfs
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! sc0
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! t0
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! t1
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Te_bdy
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Ti_bdy
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! tmodel_bdy
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Tornum_balcrit
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Ts_is_input
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! vphi0_av
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! vpol_is_input
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! vsur
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! vtor_is_input
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! wt_nphi_abs
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! wt_nphi_ext
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! xs
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Z_axis
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Z_max_lcfs
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! Z_min_lcfs
     ENDIF
  ENDIF

  ! Profiles

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! anom_evar
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! area
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! balstab
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! bidiff_bpass_co
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! bidiff_bpass_ctr
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! bidiff_btrap
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! bidiff_dpass_co
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! bidiff_dpass_ctr
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! bidiff_dtrap
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! BphiRZ
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! BRRZ
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! BZRZ
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! B_surfMax
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! B_surfMin
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! cdicrf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! cdicrf_nphi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_drbm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_etg
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_glf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_gtcneo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_mmm71
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_mtm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_nclass
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_neo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_ntm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_paleo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_trans
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_transcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chie_w19
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_drbm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_glf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_gtcneo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_mmm71
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_nclass
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_neo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_ntm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_trans
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_transcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chii_w19
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chimo_trans
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chimo_transcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_drbm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_glf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_gtcneo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_mmm71
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_neo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chiphi_w19
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! chitheta_mmm71
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curbeam
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curech
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curech_src
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curfusn
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curich
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curlh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curlh_src
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curmino
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curr_bootstrap
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curr_ohmic
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! curt
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! cur_rw
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! db_mtm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! dcur_rw_dVloop
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difb_fusi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difb_nbi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difb_rfmi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! diff_trans
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! diff_transcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difne_nclass
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difnh_nclass
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difns_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difns_drbm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difns_glf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difns_gtcneo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difns_mmm71
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difns_nclass
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difns_neo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difns_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difns_w19
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difnx_nclass
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! difnz_mmm71
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! dQedGrdte_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! dQedGrdti_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! dQidGrdte_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! dQidGrdti_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ee_mobility_factor
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! elong
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! eperp_beami
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! eperp_fusi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! eperp_mini
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! eperp_rw
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! epll_beami
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! epll_fusi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! epll_mini
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! epll_rw
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Epot
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! eta_parallel
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! fidiff_bpass_co
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! fidiff_bpass_ctr
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! fidiff_btrap
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! fidiff_dpass_co
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! fidiff_dpass_ctr
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! fidiff_dtrap
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Fi_depletion
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! freq_drbm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! freq_etg
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! freq_glf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! freq_max_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! freq_mmm71
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! freq_mtm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! freq_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gamma_nc
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gb1
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gb2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gb2i
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gbr2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gncfb2h
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gncfh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gr1
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gr2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gr2i
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gr2rho2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gr3i
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! grho1
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! grho2
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! grho2b2i
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! grho2r2i
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! grho2r3i
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! gri
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! grirhoi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! growthrate_drbm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! growthrate_etg
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! growthrate_glf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! growthrate_max_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! growthrate_mmm71
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! growthrate_mtm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! growthrate_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! growthrate_vpar
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! g_eq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! iota
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! jdotb
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! kyrhos_drbm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! kyrhos_etg
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! kyrhos_mtm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! kyspectrum_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Lpol
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! n0norm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! n0_halo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! n0_reco
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! nbeami
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! neflux_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ne_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! nfusi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ngradb2_av
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ni
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! niflux_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ni_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! nmini
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! nmodel
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RUNAWAY).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! nrw
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ns
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! nzflux_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! nz_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! omeg0cx
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! omeg0sc0
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! omeg0_halo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! omeg0_reco
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! omegat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! omega_exb
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! p0_reco
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pb0_halo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pbe
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pbi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pbth
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pcx_halo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pcx_reco
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! peech
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! peech_src
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pelh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pelh_src
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pe_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pe_trans
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pe_transcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pfuse
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pfusi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pfusth
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! phiflux_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! phit
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! picrf_nphi_srcs
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! picrf_srcs
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! picrf_totals
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! picth
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pilh
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pilh_src
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pi_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pi_trans
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pi_transcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pmine
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pmini
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pminth
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pohme
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! pot_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! prad
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! prad_br
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! prad_cy
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! prad_li
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! psc_halo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! psc_reco
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! psipol
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! PsiRZ
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! psmom_errck
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! psmom_nc
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! P_eq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qbeami
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qcomp_e
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qcomp_i
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qie
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qioniz
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qqcx
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qrot_conv
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! qrot_diff
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! q_eq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! R0_momeq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! rate_sinb0i
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! rate_sinb0x
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! rate_sinb0xs
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! rate_sinf0i
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! rate_sinf0x
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! rate_sinf0xs
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! res_sn
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! res_te
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! res_ti
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! res_tq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Rmajor_mean
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! rMinor_mean
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! R_geo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! R_midp_in
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! R_midp_out
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! R_surfMax
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! R_surfMin
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! s0reco
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! s0reco_e
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! s0reco_recap
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sb0halo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sb0halo_recap
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sbedep
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sbehalo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sbsce
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sbtherm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sc_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sfsce
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sftherm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sii_xs
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sn_trans
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sn_transcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sprof0
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! sprof0e
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PEL).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! spsce
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! squareLO
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! squareUO
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! surf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! T0cx
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! T0sc0
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! T0_halo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! T0_reco
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! teflux_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! te_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tfrip2_log
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tfrip_log
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Ti
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tiflux_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ti_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tmodel
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tq0_reco
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tqb0_halo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tqbe
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tqbi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tqbjxb
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tqbth
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tqcx_halo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tqcx_reco
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tqioniz
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tqqcx
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tqsc_halo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tqsc_reco
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tq_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tq_trans
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tq_transcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! triang
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! triangL
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! triangU
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! triang_miller_L
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! triang_miller_U
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Ts
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tzflux_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! tz_ky_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! upwind_pfrac_ns
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! upwind_pfrac_omega
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! upwind_pfrac_Te
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! upwind_pfrac_Ti
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vee_trans
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vee_transcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velb_fusi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velb_nbi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velb_rfmi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velne_nclass
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velnh_nclass
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velni_drbm
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velns_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velns_glf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velns_gtcneo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velns_mmm71
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velns_nclass
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velns_neo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velns_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velnx_nclass
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velpphi_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velpphi_glf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velpphi_gtcneo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velpphi_mmm71
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velpphi_neo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velpphi_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velte_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velte_glf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velte_gtcneo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velte_mmm71
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velte_nclass
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velte_neo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velte_paleo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velte_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velti_anom
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velti_glf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velti_gtcneo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velti_mmm71
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velti_nclass
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velti_neo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! velti_tglf
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vie_trans
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vie_transcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vmo_trans
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vmo_transcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vn_trans
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vn_transcat
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vol
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vpol_inmp
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vpol_omp
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vtor_inmp
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! vtor_omp
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! V_loop
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! v_pars
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! v_pers
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! xRjcos_momeq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! xRjsin_momeq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! xZjcos_momeq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! xZjsin_momeq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Z0_momeq
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Zeff
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Zeff_fi
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Zeff_th
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Z_geo
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Z_midp
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Z_surfMax
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! Z_surfMin
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ANOM_DIFFUS
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_ANOM).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ANOM_VELOC
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! BEAM_HALO_POWER
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! BEAM_HALO_TORQUE
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! BEAM_HEATING
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_NBI).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! BEAM_TORQUE
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! BMINMAX
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! COMPRESSION
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! DIFFUSIVITY
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! EQ_BAXIS
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! EQ_BLCFS
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_shot_config) THEN
        isize = isize + 1  ! EQ_BSIGNS
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! EQ_FILES
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! EQ_RZAXIS
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_machine_descr) THEN
        isize = isize + 1  ! EQ_RZBOX
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! EQ_RZLCFS
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_FUS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! FUSION_HEATING
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! ICRF_CONTROLS
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LMHD).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! LMHD_FLAGS
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! LOCK_VARIABLES
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_LH).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! LOWER_HYBRID_HEATING
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! MIDPLANE
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_IC).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! MINORITY_HEATING
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RADIAL_VELOCITY
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RAD).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RADIATED_POWERS
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RECO_POWER
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_GAS).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RECO_TORQUE
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RESIDUAL
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! RHO_BDY_SOLVE
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_RIPPLE).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RIPPLE_FIELDS
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RMAJMIN
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ROTATION_DISSIPATION
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! RZMINMAX
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! SURFACE_VOLTAGE
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! SURFSHAPE
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! TEMPERATURE_BCS
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! TIME_RANGE
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_data) THEN
        isize = isize + 1  ! TIME_STEP
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! TRANSPORT
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_EQ).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! TRIANG_MILLER
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! UPWIND_FORWARD_FRACTIONS
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_profiles) THEN
        isize = isize + 1  ! ZEFF_GROUP
     ENDIF
  ENDIF

  IF(icomponent(ps_cnum_PLASMA).EQ.1) THEN
     IF(itype.EQ.ps_sim_control) THEN
        isize = isize + 1  ! ZIMP_SA
     ENDIF
  ENDIF


  w%size = isize

  allocate(w%list_elems(isize)); w%list_elems = ' '
  allocate(w%elem_descr(isize)); w%elem_descr = ' '
  allocate(w%ival(isize)); w%ival = 0
  allocate(w%component(isize)); w%component = ' '

  allocate(w%vecsizes(isize)); w%vecsizes = 0

  if((itype.eq.ps_data).or.(itype.eq.ps_profiles).or. &
       (itype.eq.ps_machine_descr).or.(itype.eq.ps_shot_config).or. &
       (itype.eq.ps_sim_control)) then

     allocate(w%c256val(isize)); w%c256val = ' '
     allocate(w%elem_units(isize)); w%elem_units = ' '
     if(itype.eq.ps_profiles) then
        allocate(w%val_field(isize)); w%val_field = ' '
     endif

  else

     allocate(w%val_field(isize)); w%val_field = ' '

  endif

END SUBROUTINE ps_contents_alloc
