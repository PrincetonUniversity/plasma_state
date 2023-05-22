! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


subroutine ps_sconfig_namelist_write(filename,ss,ierr)

  ! from current contents of state, write shot configuration namelist

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
     WRITE(iout,*) ' ?ps_sconfig_namelist_write: OPEN failure on file: '// &
          TRIM(filename)
     RETURN
  ENDIF

  WRITE(unit=ilun_namel,fmt='(" &sconfig")')

  CALL ps_namwr(ilun_namel, &
       "integer shot number","integer", &
       "shot_number", ss%shot_number)

  IF(ALLOCATED(ss%S_name)) THEN
     CALL ps_namwr(ilun_namel, &
          "thermal species list","species_list", &
          "S", ss%NSPEC_TH, ss%S_name(1:ss%NSPEC_TH), &
          ss%qatom_S(1:ss%NSPEC_TH), ss%q_S(1:ss%NSPEC_TH), ss%m_S(1:ss%NSPEC_TH))
  ELSE
     CALL ps_namwr(ilun_namel, &
          "thermal species list","species_list", "S")
  ENDIF

  IF(ALLOCATED(ss%nbion)) THEN
     CALL ps_namwr(ilun_namel, &
          "name of species injected by each beam.","character", &
          "nbion", ss%NBEAM, ss%nbion)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "name of species injected by each beam.","character", "nbion")
  ENDIF

  IF(ALLOCATED(ss%nbion_trace)) THEN
     CALL ps_namwr(ilun_namel, &
          "(if non-blank) beam trace element","character", &
          "nbion_trace", ss%NBEAM, ss%nbion_trace)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "(if non-blank) beam trace element","character", "nbion_trace")
  ENDIF

  IF(ALLOCATED(ss%SFUS_name)) THEN
     CALL ps_namwr(ilun_namel, &
          "fusion product ion species","species_list", &
          "SFUS", ss%NSPEC_FUSION, ss%SFUS_name(1:ss%NSPEC_FUSION), &
          ss%qatom_SFUS(1:ss%NSPEC_FUSION), ss%q_SFUS(1:ss%NSPEC_FUSION), ss%m_SFUS(1:ss%NSPEC_FUSION))
  ELSE
     CALL ps_namwr(ilun_namel, &
          "fusion product ion species","species_list", "SFUS")
  ENDIF

  IF(ALLOCATED(ss%RFMIN_name)) THEN
     CALL ps_namwr(ilun_namel, &
          "ICRF minority species","species_list", &
          "RFMIN", ss%NSPEC_RFMIN, ss%RFMIN_name(1:ss%NSPEC_RFMIN), &
          ss%qatom_RFMIN(1:ss%NSPEC_RFMIN), ss%q_RFMIN(1:ss%NSPEC_RFMIN), ss%m_RFMIN(1:ss%NSPEC_RFMIN))
  ELSE
     CALL ps_namwr(ilun_namel, &
          "ICRF minority species","species_list", "RFMIN")
  ENDIF

  IF(ALLOCATED(ss%freq_ic)) THEN
     CALL ps_namwr(ilun_namel, &
          "frequency on each ICRF source","Hz", &
          "freq_ic", ss%NICRF_SRC, ss%freq_ic)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "frequency on each ICRF source","Hz", "freq_ic")
  ENDIF

  IF(ALLOCATED(ss%n_straps)) THEN
     CALL ps_namwr(ilun_namel, &
          "number of straps in the antenna","integer", &
          "n_straps", ss%NICRF_SRC, ss%n_straps)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "number of straps in the antenna","integer", "n_straps")
  ENDIF

  CALL ps_namwr(ilun_namel, &
       "Maximum size of variable length enumeration: N_STRAPS","integer", &
       "max_n_straps", ss%max_n_straps)

  IF(ALLOCATED(ss%R_ant)) THEN
     CALL ps_namwr(ilun_namel, &
          "major radius of antenna","m", &
          "R_ant", ss%NICRF_SRC, ss%R_ant)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "major radius of antenna","m", "R_ant")
  ENDIF

  IF(ALLOCATED(ss%Z_ant)) THEN
     CALL ps_namwr(ilun_namel, &
          "height of antenna","m", &
          "Z_ant", ss%NICRF_SRC, ss%Z_ant)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "height of antenna","m", "Z_ant")
  ENDIF

  IF(ALLOCATED(ss%Z_mid_ant)) THEN
     CALL ps_namwr(ilun_namel, &
          "center of antenna relative to TF coil midplane","m", &
          "Z_mid_ant", ss%NICRF_SRC, ss%Z_mid_ant)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "center of antenna relative to TF coil midplane","m", "Z_mid_ant")
  ENDIF

  IF(ALLOCATED(ss%num_nphi_vac)) THEN
     CALL ps_namwr(ilun_namel, &
          "number of non-zero n_phi values","integer", &
          "num_nphi_vac", ss%NICRF_SRC, ss%num_nphi_vac)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "number of non-zero n_phi values","integer", "num_nphi_vac")
  ENDIF

  CALL ps_namwr(ilun_namel, &
       "Maximum size of variable length enumeration: NUM_NPHI_VAC","integer", &
       "max_num_nphi_vac", ss%max_num_nphi_vac)

  IF(ALLOCATED(ss%Real_ant_coef)) THEN
     ALLOCATE(isize_vec(ss%NICRF_SRC))
     isize_vec = ss%NUM_NPHI_VAC
     CALL ps_namwr(ilun_namel, &
          "real part of Fourier Coef","Amps/m", &
          "Real_ant_coef", isize_vec, ss%Real_ant_coef)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "real part of Fourier Coef","Amps/m", "Real_ant_coef")
  ENDIF

  IF(ALLOCATED(ss%Imag_ant_coef)) THEN
     ALLOCATE(isize_vec(ss%NICRF_SRC))
     isize_vec = ss%NUM_NPHI_VAC
     CALL ps_namwr(ilun_namel, &
          "imag part of Fourier Coef","Amps/m", &
          "Imag_ant_coef", isize_vec, ss%Imag_ant_coef)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "imag part of Fourier Coef","Amps/m", "Imag_ant_coef")
  ENDIF

  IF(ALLOCATED(ss%num_nphi)) THEN
     CALL ps_namwr(ilun_namel, &
          "number of non-zero n_phi values","integer", &
          "num_nphi", ss%NICRF_SRC, ss%num_nphi)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "number of non-zero n_phi values","integer", "num_nphi")
  ENDIF

  CALL ps_namwr(ilun_namel, &
       "Maximum size of variable length enumeration: NUM_NPHI","integer", &
       "max_num_nphi", ss%max_num_nphi)

  IF(ALLOCATED(ss%nphi)) THEN
     ALLOCATE(isize_vec(ss%NICRF_SRC))
     isize_vec = ss%NUM_NPHI
     CALL ps_namwr(ilun_namel, &
          "n_phi wave spectrum from antenna","integer", &
          "nphi", isize_vec, ss%nphi)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "n_phi wave spectrum from antenna","integer", "nphi")
  ENDIF

  IF(ALLOCATED(ss%wt_nphi)) THEN
     ALLOCATE(isize_vec(ss%NICRF_SRC))
     isize_vec = ss%NUM_NPHI
     CALL ps_namwr(ilun_namel, &
          "vacuum spectrum n_phi weight","-", &
          "wt_nphi", isize_vec, ss%wt_nphi)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "vacuum spectrum n_phi weight","-", "wt_nphi")
  ENDIF

  CALL ps_namwr(ilun_namel, &
       "B_phi orientation: +1 means CCW viewed from above","integer", &
       "kccw_Bphi", ss%kccw_Bphi)

  CALL ps_namwr(ilun_namel, &
       "J_phi orientation: +1 means CCW viewed from above","integer", &
       "kccw_Jphi", ss%kccw_Jphi)

  IF(ALLOCATED(ss%gs_name)) THEN
     CALL ps_namwr(ilun_namel, &
          "number & name of edge neutral gas sources","character", &
          "gs_name", ss%NGSC0, ss%gs_name)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "number & name of edge neutral gas sources","character", "gs_name")
  ENDIF

  IF(ALLOCATED(ss%gas_atom)) THEN
     CALL ps_namwr(ilun_namel, &
          "name of species introduced by each gas source","character", &
          "gas_atom", ss%NGSC0, ss%gas_atom)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "name of species introduced by each gas source","character", "gas_atom")
  ENDIF

  IF(ALLOCATED(ss%is_recycling)) THEN
     CALL ps_namwr(ilun_namel, &
          "0/1; =1 to for recycling sources","integer", &
          "is_recycling", ss%NGSC0, ss%is_recycling)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "0/1; =1 to for recycling sources","integer", "is_recycling")
  ENDIF

  IF(ALLOCATED(ss%xs_name)) THEN
     CALL ps_namwr(ilun_namel, &
          "number & name of impurity sources","character", &
          "xs_name", ss%NXS, ss%xs_name)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "number & name of impurity sources","character", "xs_name")
  ENDIF

  IF(ALLOCATED(ss%freq_ec)) THEN
     CALL ps_namwr(ilun_namel, &
          "frequency of EC source","Hz", &
          "freq_ec", ss%NECRF_SRC, ss%freq_ec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "frequency of EC source","Hz", "freq_ec")
  ENDIF

  IF(ALLOCATED(ss%EC_theta_aim)) THEN
     CALL ps_namwr(ilun_namel, &
          "poloidal aiming angle","degrees", &
          "EC_theta_aim", ss%NECRF_SRC, ss%EC_theta_aim)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "poloidal aiming angle","degrees", "EC_theta_aim")
  ENDIF

  IF(ALLOCATED(ss%EC_phi_aim)) THEN
     CALL ps_namwr(ilun_namel, &
          "toroidal aiming angle","degrees", &
          "EC_phi_aim", ss%NECRF_SRC, ss%EC_phi_aim)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "toroidal aiming angle","degrees", "EC_phi_aim")
  ENDIF

  IF(ALLOCATED(ss%EC_Omode_Fraction)) THEN
     CALL ps_namwr(ilun_namel, &
          "O-mode fraction of EC source","-", &
          "EC_Omode_Fraction", ss%NECRF_SRC, ss%EC_Omode_Fraction)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "O-mode fraction of EC source","-", "EC_Omode_Fraction")
  ENDIF

  IF(ALLOCATED(ss%freq_lh)) THEN
     CALL ps_namwr(ilun_namel, &
          "frequency on each LHRF source","Hz", &
          "freq_lh", ss%NLHRF_SRC, ss%freq_lh)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "frequency on each LHRF source","Hz", "freq_lh")
  ENDIF

  IF(ALLOCATED(ss%nspectrumlh_src)) THEN
     CALL ps_namwr(ilun_namel, &
          "number of spectrum peaks","integer", &
          "nspectrumlh_src", ss%NLHRF_SRC, ss%nspectrumlh_src)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "number of spectrum peaks","integer", "nspectrumlh_src")
  ENDIF

  CALL ps_namwr(ilun_namel, &
       "Maximum size of variable length enumeration: NSPECTRUMLH_SRC","integer", &
       "max_nspectrumlh_src", ss%max_nspectrumlh_src)

  IF(ALLOCATED(ss%aminlh)) THEN
     ALLOCATE(isize_vec(ss%NLHRF_SRC))
     isize_vec = ss%NSPECTRUMLH_SRC
     CALL ps_namwr(ilun_namel, &
          "parallel mode number, center of the spectrum peak","-", &
          "aminlh", isize_vec, ss%aminlh)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "parallel mode number, center of the spectrum peak","-", "aminlh")
  ENDIF

  IF(ALLOCATED(ss%amaxlh)) THEN
     ALLOCATE(isize_vec(ss%NLHRF_SRC))
     isize_vec = ss%NSPECTRUMLH_SRC
     CALL ps_namwr(ilun_namel, &
          "width of the spectrum peak","-", &
          "amaxlh", isize_vec, ss%amaxlh)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "width of the spectrum peak","-", "amaxlh")
  ENDIF

  IF(ALLOCATED(ss%atorminlh)) THEN
     ALLOCATE(isize_vec(ss%NLHRF_SRC))
     isize_vec = ss%NSPECTRUMLH_SRC
     CALL ps_namwr(ilun_namel, &
          "toroidal mode number left of lumps","-", &
          "atorminlh", isize_vec, ss%atorminlh)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "toroidal mode number left of lumps","-", "atorminlh")
  ENDIF

  IF(ALLOCATED(ss%atormaxlh)) THEN
     ALLOCATE(isize_vec(ss%NLHRF_SRC))
     isize_vec = ss%NSPECTRUMLH_SRC
     CALL ps_namwr(ilun_namel, &
          "toroidal mode number right of lumps","-", &
          "atormaxlh", isize_vec, ss%atormaxlh)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "toroidal mode number right of lumps","-", "atormaxlh")
  ENDIF

  IF(ALLOCATED(ss%apolminlh)) THEN
     ALLOCATE(isize_vec(ss%NLHRF_SRC))
     isize_vec = ss%NSPECTRUMLH_SRC
     CALL ps_namwr(ilun_namel, &
          "poloidal mode number left of lumps","-", &
          "apolminlh", isize_vec, ss%apolminlh)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "poloidal mode number left of lumps","-", "apolminlh")
  ENDIF

  IF(ALLOCATED(ss%apolmaxlh)) THEN
     ALLOCATE(isize_vec(ss%NLHRF_SRC))
     isize_vec = ss%NSPECTRUMLH_SRC
     CALL ps_namwr(ilun_namel, &
          "poloidal mode number right of lumps","-", &
          "apolmaxlh", isize_vec, ss%apolmaxlh)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "poloidal mode number right of lumps","-", "apolmaxlh")
  ENDIF

  IF(ALLOCATED(ss%fracpslh)) THEN
     ALLOCATE(isize_vec(ss%NLHRF_SRC))
     isize_vec = ss%NSPECTRUMLH_SRC
     CALL ps_namwr(ilun_namel, &
          "fraction of power on each peak (the sum over all peaks must be 1)","-", &
          "fracpslh", isize_vec, ss%fracpslh)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "fraction of power on each peak (the sum over all peaks must be 1)","-", "fracpslh")
  ENDIF

  CALL ps_namwr(ilun_namel, &
       "need description","integer", &
       "i_npoloidal", ss%i_npoloidal)

  IF(ALLOCATED(ss%rhopsi0lh)) THEN
     CALL ps_namwr(ilun_namel, &
          "need description","-", &
          "rhopsi0lh", ss%NLHRF_SRC, ss%rhopsi0lh)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "need description","-", "rhopsi0lh")
  ENDIF

  IF(ALLOCATED(ss%thegrilh)) THEN
     CALL ps_namwr(ilun_namel, &
          "need description","-", &
          "thegrilh", ss%NLHRF_SRC, ss%thegrilh)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "need description","-", "thegrilh")
  ENDIF

  IF(ALLOCATED(ss%hegrilh)) THEN
     CALL ps_namwr(ilun_namel, &
          "need description","-", &
          "hegrilh", ss%NLHRF_SRC, ss%hegrilh)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "need description","-", "hegrilh")
  ENDIF

  IF(ALLOCATED(ss%nthinlh)) THEN
     CALL ps_namwr(ilun_namel, &
          "need description","integer", &
          "nthinlh", ss%NLHRF_SRC, ss%nthinlh)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "need description","integer", "nthinlh")
  ENDIF

  IF(ALLOCATED(ss%nnkparlh)) THEN
     ALLOCATE(isize_vec(ss%NLHRF_SRC))
     isize_vec = ss%NSPECTRUMLH_SRC
     CALL ps_namwr(ilun_namel, &
          "need description","integer", &
          "nnkparlh", isize_vec, ss%nnkparlh)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "need description","integer", "nnkparlh")
  ENDIF

  IF(ALLOCATED(ss%nnktorlh)) THEN
     ALLOCATE(isize_vec(ss%NLHRF_SRC))
     isize_vec = ss%NSPECTRUMLH_SRC
     CALL ps_namwr(ilun_namel, &
          "need description","integer", &
          "nnktorlh", isize_vec, ss%nnktorlh)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "need description","integer", "nnktorlh")
  ENDIF

  IF(ALLOCATED(ss%nnkpollh)) THEN
     ALLOCATE(isize_vec(ss%NLHRF_SRC))
     isize_vec = ss%NSPECTRUMLH_SRC
     CALL ps_namwr(ilun_namel, &
          "need description","integer", &
          "nnkpollh", isize_vec, ss%nnkpollh)
     DEALLOCATE(isize_vec)
  ELSE
     CALL ps_namwr(ilun_namel, &
          "need description","integer", "nnkpollh")
  ENDIF


  WRITE(ilun_namel,'(" /")')
  CLOSE(UNIT=ilun_namel)

END SUBROUTINE ps_sconfig_namelist_write
