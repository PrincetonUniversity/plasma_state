! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_hash_diff(ss1,lbl1,ss2,lbl2,sstr,icomp,idiff)

  ! summarize difference between two states, based on hash code comparison.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss1        ! 1st state
  CHARACTER*(*), intent(in) :: lbl1 ! label for 1st state

  TYPE (plasma_state) :: ss2        ! 2nd state
  CHARACTER*(*), intent(in) :: lbl2 ! label for 2nd state

  CHARACTER*(*), intent(in) :: sstr ! search string
  !  if section, dimension, or profile name contains sstr as a substring
  !  check the hash difference for that item.

  !  ==> if sstr = "*", check the hash difference for ALL items.

  INTEGER, intent(out) :: icomp     ! number of hash comparisons done
  INTEGER, intent(out) :: idiff     ! number of DIFFERENCES found

  !-------------------
  INTEGER :: iout,ils,iaddr1,iaddr2
  CHARACTER*32 ss
  !-------------------

  iout = SWIM_err_out_file

  ss = sstr
  call uupper(ss)
  ils = len(trim(ss))

  icomp = 0  ! number hash pairs compared
  idiff = 0  ! number that were different
  !--------------------------------------------
  ! section hash codes

  IF((ss(1:ils).EQ."*").OR.(INDEX("MACHINE_DESCRIPTION",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_shash_machine_description
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
          (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("SECTION","MACHINE_DESCRIPTION")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SHOT_CONFIGURATION",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_shash_shot_configuration
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
          (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("SECTION","SHOT_CONFIGURATION")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SIMULATION_INIT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_shash_simulation_init
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
          (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("SECTION","SIMULATION_INIT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("STATE_DATA",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_shash_state_data
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
          (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("SECTION","STATE_DATA")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("STATE_PROFILES",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_shash_state_profiles
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
          (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("SECTION","STATE_PROFILES")
     ENDIF
  ENDIF

  !--------------------------------------------
  ! dimension hash codes

  IF((ss(1:ils).EQ."*").OR.(INDEX("MAX_NRZ_ANTGEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_max_nrz_antgeo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","MAX_NRZ_ANTGEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("MAX_NSPECTRUMLH_SRC",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_max_nspectrumlh_src
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","MAX_NSPECTRUMLH_SRC")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("MAX_NUM_NPHI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_max_num_nphi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","MAX_NUM_NPHI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("MAX_NUM_NPHI_VAC",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_max_num_nphi_vac
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","MAX_NUM_NPHI_VAC")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("MAX_N_STRAPS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_max_n_straps
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","MAX_N_STRAPS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("MMODES",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_mmodes
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","MMODES")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NBALLOON",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nballoon
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NBALLOON")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NBEAM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nbeam
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NBEAM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NCIRCUITS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_ncircuits
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NCIRCUITS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NCOILS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_ncoils
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NCOILS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NDELTA_W",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_ndelta_w
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NDELTA_W")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NECRF_SRC",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_necrf_src
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NECRF_SRC")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NEFI_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nefi_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NEFI_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NEFI_ANOM2",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nefi_anom2
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NEFI_ANOM2")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NEQMOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_neqmom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NEQMOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NGSC0",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_ngsc0
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NGSC0")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NICRF_SRC",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nicrf_src
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NICRF_SRC")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NKY_ANOMTH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nky_anomth
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NKY_ANOMTH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NKY_MMM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nky_mmm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NKY_MMM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NLHRF_SRC",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nlhrf_src
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NLHRF_SRC")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NNEWCOMB",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nnewcomb
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NNEWCOMB")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NPSMOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_npsmom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NPSMOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NR",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nr
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NR")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_ANOM2",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_anom2
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_ANOM2")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_ANOMTH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_anomth
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_ANOMTH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_ECRF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_ecrf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_ECRF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_EQ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_eq
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_EQ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_EQ_GEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_eq_geo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_EQ_GEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_FUS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_fus
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_FUS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_GAS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_gas
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_GAS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_ICRF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_icrf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_ICRF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_LHRF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_lhrf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_LHRF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_LMHD",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_lmhd
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_LMHD")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_MIST",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_mist
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_MIST")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_NBI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_nbi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_NBI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_PEL",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_pel
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_PEL")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_RAD",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_rad
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_RAD")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRHO_RW",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nrho_rw
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NRHO_RW")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NR_RIP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nr_rip
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NR_RIP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NSPEC_ALL",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nspec_all
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NSPEC_ALL")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NSPEC_ALLA",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nspec_alla
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NSPEC_ALLA")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NSPEC_BEAM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nspec_beam
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NSPEC_BEAM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NSPEC_FUSION",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nspec_fusion
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NSPEC_FUSION")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NSPEC_GAS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nspec_gas
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NSPEC_GAS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NSPEC_IMP0",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nspec_imp0
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NSPEC_IMP0")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NSPEC_IMPI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nspec_impi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NSPEC_IMPI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NSPEC_RFMIN",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nspec_rfmin
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NSPEC_RFMIN")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NSPEC_TH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nspec_th
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NSPEC_TH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NSPEC_THA",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nspec_tha
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NSPEC_THA")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NTH_EQ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nth_eq
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NTH_EQ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NTRCAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_ntrcat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NTRCAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NUM_RZLIM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_num_rzlim
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NUM_RZLIM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NXS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nxs
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NXS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NZ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nz
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NZ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NZ_RIP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_nz_rip
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","NZ_RIP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("N_D_EINJ_STANDARD",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_n_d_einj_standard
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","N_D_EINJ_STANDARD")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("N_H_EINJ_STANDARD",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_n_h_einj_standard
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","N_H_EINJ_STANDARD")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("N_T_EINJ_STANDARD",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_dhash_n_t_einj_standard
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("DIMENSION","N_T_EINJ_STANDARD")
     ENDIF
  ENDIF

  !--------------------------------------------
  ! profile hash codes

  IF((ss(1:ils).EQ."*").OR.(INDEX("ANOM_EVAR",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_anom_evar
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","ANOM_EVAR")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("AREA",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_area
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","AREA")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("BALSTAB",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_balstab
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","BALSTAB")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("BIDIFF_BPASS_CO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_bidiff_bpass_co
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","BIDIFF_BPASS_CO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("BIDIFF_BPASS_CTR",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_bidiff_bpass_ctr
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","BIDIFF_BPASS_CTR")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("BIDIFF_BTRAP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_bidiff_btrap
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","BIDIFF_BTRAP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("BIDIFF_DPASS_CO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_bidiff_dpass_co
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","BIDIFF_DPASS_CO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("BIDIFF_DPASS_CTR",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_bidiff_dpass_ctr
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","BIDIFF_DPASS_CTR")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("BIDIFF_DTRAP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_bidiff_dtrap
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","BIDIFF_DTRAP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("BPHIRZ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_bphirz
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","BPHIRZ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("BRRZ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_brrz
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","BRRZ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("BZRZ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_bzrz
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","BZRZ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("B_SURFMAX",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_b_surfmax
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","B_SURFMAX")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("B_SURFMIN",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_b_surfmin
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","B_SURFMIN")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CDICRF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_cdicrf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CDICRF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CDICRF_NPHI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_cdicrf_nphi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CDICRF_NPHI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_DRBM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_drbm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_DRBM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_ETG",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_etg
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_ETG")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_GLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_glf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_GLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_GTCNEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_gtcneo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_GTCNEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_MMM71",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_mmm71
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_MMM71")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_MTM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_mtm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_MTM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_NCLASS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_nclass
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_NCLASS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_NEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_neo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_NEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_NTM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_ntm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_NTM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_PALEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_paleo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_PALEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_TRANS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_trans
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_TRANS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_TRANSCAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_transcat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_TRANSCAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIE_W19",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chie_w19
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIE_W19")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHII_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chii_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHII_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHII_DRBM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chii_drbm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHII_DRBM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHII_GLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chii_glf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHII_GLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHII_GTCNEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chii_gtcneo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHII_GTCNEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHII_MMM71",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chii_mmm71
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHII_MMM71")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHII_NCLASS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chii_nclass
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHII_NCLASS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHII_NEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chii_neo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHII_NEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHII_NTM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chii_ntm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHII_NTM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHII_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chii_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHII_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHII_TRANS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chii_trans
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHII_TRANS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHII_TRANSCAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chii_transcat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHII_TRANSCAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHII_W19",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chii_w19
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHII_W19")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIMO_TRANS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chimo_trans
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIMO_TRANS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIMO_TRANSCAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chimo_transcat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIMO_TRANSCAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIPHI_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chiphi_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIPHI_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIPHI_DRBM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chiphi_drbm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIPHI_DRBM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIPHI_GLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chiphi_glf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIPHI_GLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIPHI_GTCNEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chiphi_gtcneo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIPHI_GTCNEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIPHI_MMM71",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chiphi_mmm71
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIPHI_MMM71")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIPHI_NEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chiphi_neo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIPHI_NEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIPHI_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chiphi_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIPHI_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHIPHI_W19",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chiphi_w19
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHIPHI_W19")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CHITHETA_MMM71",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_chitheta_mmm71
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CHITHETA_MMM71")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CURBEAM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_curbeam
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CURBEAM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CURECH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_curech
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CURECH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CURECH_SRC",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_curech_src
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CURECH_SRC")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CURFUSN",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_curfusn
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CURFUSN")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CURICH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_curich
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CURICH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CURLH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_curlh
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CURLH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CURLH_SRC",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_curlh_src
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CURLH_SRC")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CURMINO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_curmino
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CURMINO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CURR_BOOTSTRAP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_curr_bootstrap
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CURR_BOOTSTRAP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CURR_OHMIC",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_curr_ohmic
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CURR_OHMIC")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CURT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_curt
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CURT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("CUR_RW",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_cur_rw
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","CUR_RW")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("D2R_GEO_DRHODTH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_d2r_geo_drhodth
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","D2R_GEO_DRHODTH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("D2Z_GEO_DRHODTH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_d2z_geo_drhodth
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","D2Z_GEO_DRHODTH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DB_MTM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_db_mtm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DB_MTM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DCUR_RW_DVLOOP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dcur_rw_dvloop
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DCUR_RW_DVLOOP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFB_FUSI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difb_fusi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFB_FUSI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFB_NBI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difb_nbi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFB_NBI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFB_RFMI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difb_rfmi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFB_RFMI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFF_TRANS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_diff_trans
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFF_TRANS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFF_TRANSCAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_diff_transcat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFF_TRANSCAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFNE_NCLASS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difne_nclass
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFNE_NCLASS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFNH_NCLASS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difnh_nclass
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFNH_NCLASS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFNS_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difns_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFNS_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFNS_DRBM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difns_drbm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFNS_DRBM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFNS_GLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difns_glf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFNS_GLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFNS_GTCNEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difns_gtcneo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFNS_GTCNEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFNS_MMM71",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difns_mmm71
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFNS_MMM71")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFNS_NCLASS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difns_nclass
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFNS_NCLASS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFNS_NEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difns_neo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFNS_NEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFNS_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difns_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFNS_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFNS_W19",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difns_w19
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFNS_W19")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFNX_NCLASS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difnx_nclass
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFNX_NCLASS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DIFNZ_MMM71",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_difnz_mmm71
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DIFNZ_MMM71")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DQEDGRDTE_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dqedgrdte_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DQEDGRDTE_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DQEDGRDTI_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dqedgrdti_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DQEDGRDTI_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DQIDGRDTE_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dqidgrdte_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DQIDGRDTE_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DQIDGRDTI_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dqidgrdti_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DQIDGRDTI_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DR0_MOMEQ_DRHO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dr0_momeq_drho
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DR0_MOMEQ_DRHO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DR_GEO_DRHO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dr_geo_drho
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DR_GEO_DRHO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DR_GEO_DTH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dr_geo_dth
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DR_GEO_DTH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DXRJCOS_MOMEQ_DRHO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dxrjcos_momeq_drho
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DXRJCOS_MOMEQ_DRHO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DXRJSIN_MOMEQ_DRHO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dxrjsin_momeq_drho
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DXRJSIN_MOMEQ_DRHO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DXZJCOS_MOMEQ_DRHO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dxzjcos_momeq_drho
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DXZJCOS_MOMEQ_DRHO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DXZJSIN_MOMEQ_DRHO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dxzjsin_momeq_drho
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DXZJSIN_MOMEQ_DRHO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DZ0_MOMEQ_DRHO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dz0_momeq_drho
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DZ0_MOMEQ_DRHO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DZ_GEO_DRHO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dz_geo_drho
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DZ_GEO_DRHO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("DZ_GEO_DTH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_dz_geo_dth
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","DZ_GEO_DTH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("EE_MOBILITY_FACTOR",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_ee_mobility_factor
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","EE_MOBILITY_FACTOR")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("ELONG",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_elong
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","ELONG")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("EPERP_BEAMI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_eperp_beami
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","EPERP_BEAMI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("EPERP_FUSI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_eperp_fusi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","EPERP_FUSI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("EPERP_MINI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_eperp_mini
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","EPERP_MINI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("EPERP_RW",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_eperp_rw
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","EPERP_RW")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("EPLL_BEAMI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_epll_beami
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","EPLL_BEAMI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("EPLL_FUSI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_epll_fusi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","EPLL_FUSI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("EPLL_MINI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_epll_mini
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","EPLL_MINI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("EPLL_RW",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_epll_rw
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","EPLL_RW")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("EPOT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_epot
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","EPOT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("ETA_PARALLEL",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_eta_parallel
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","ETA_PARALLEL")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FIDIFF_BPASS_CO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_fidiff_bpass_co
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FIDIFF_BPASS_CO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FIDIFF_BPASS_CTR",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_fidiff_bpass_ctr
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FIDIFF_BPASS_CTR")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FIDIFF_BTRAP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_fidiff_btrap
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FIDIFF_BTRAP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FIDIFF_DPASS_CO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_fidiff_dpass_co
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FIDIFF_DPASS_CO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FIDIFF_DPASS_CTR",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_fidiff_dpass_ctr
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FIDIFF_DPASS_CTR")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FIDIFF_DTRAP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_fidiff_dtrap
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FIDIFF_DTRAP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FI_DEPLETION",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_fi_depletion
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FI_DEPLETION")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FREQ_DRBM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_freq_drbm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FREQ_DRBM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FREQ_ETG",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_freq_etg
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FREQ_ETG")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FREQ_GLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_freq_glf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FREQ_GLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FREQ_MAX_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_freq_max_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FREQ_MAX_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FREQ_MMM71",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_freq_mmm71
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FREQ_MMM71")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FREQ_MTM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_freq_mtm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FREQ_MTM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("FREQ_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_freq_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","FREQ_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GAMMA_NC",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_gamma_nc
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GAMMA_NC")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GB1",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_gb1
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GB1")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GB2",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_gb2
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GB2")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GB2I",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_gb2i
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GB2I")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GBR2",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_gbr2
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GBR2")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GNCFB2H",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_gncfb2h
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GNCFB2H")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GNCFH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_gncfh
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GNCFH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GR1",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_gr1
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GR1")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GR2",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_gr2
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GR2")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GR2I",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_gr2i
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GR2I")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GR2RHO2",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_gr2rho2
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GR2RHO2")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GR3I",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_gr3i
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GR3I")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GRHO1",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_grho1
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GRHO1")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GRHO2",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_grho2
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GRHO2")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GRHO2B2I",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_grho2b2i
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GRHO2B2I")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GRHO2R2I",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_grho2r2i
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GRHO2R2I")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GRHO2R3I",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_grho2r3i
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GRHO2R3I")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GRI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_gri
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GRI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GRIRHOI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_grirhoi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GRIRHOI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GROWTHRATE_DRBM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_growthrate_drbm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GROWTHRATE_DRBM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GROWTHRATE_ETG",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_growthrate_etg
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GROWTHRATE_ETG")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GROWTHRATE_GLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_growthrate_glf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GROWTHRATE_GLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GROWTHRATE_MAX_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_growthrate_max_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GROWTHRATE_MAX_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GROWTHRATE_MMM71",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_growthrate_mmm71
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GROWTHRATE_MMM71")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GROWTHRATE_MTM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_growthrate_mtm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GROWTHRATE_MTM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GROWTHRATE_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_growthrate_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GROWTHRATE_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("GROWTHRATE_VPAR",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_growthrate_vpar
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","GROWTHRATE_VPAR")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("G_EQ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_g_eq
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","G_EQ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("IOTA",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_iota
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","IOTA")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("JDOTB",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_jdotb
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","JDOTB")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("KYRHOS_DRBM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_kyrhos_drbm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","KYRHOS_DRBM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("KYRHOS_ETG",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_kyrhos_etg
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","KYRHOS_ETG")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("KYRHOS_MTM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_kyrhos_mtm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","KYRHOS_MTM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("KYSPECTRUM_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_kyspectrum_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","KYSPECTRUM_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("LPOL",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_lpol
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","LPOL")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("N0NORM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_n0norm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","N0NORM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("N0_HALO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_n0_halo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","N0_HALO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("N0_RECO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_n0_reco
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","N0_RECO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NBEAMI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_nbeami
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NBEAMI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NEFLUX_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_neflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NEFLUX_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NE_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_ne_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NE_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NFUSI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_nfusi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NFUSI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NGRADB2_AV",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_ngradb2_av
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NGRADB2_AV")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_ni
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NIFLUX_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_niflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NIFLUX_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NI_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_ni_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NI_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NMINI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_nmini
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NMINI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NMODEL",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_nmodel
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NMODEL")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NRW",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_nrw
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NRW")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_ns
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NZFLUX_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_nzflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NZFLUX_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("NZ_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_nz_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","NZ_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("OMEG0CX",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_omeg0cx
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","OMEG0CX")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("OMEG0SC0",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_omeg0sc0
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","OMEG0SC0")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("OMEG0_HALO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_omeg0_halo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","OMEG0_HALO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("OMEG0_RECO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_omeg0_reco
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","OMEG0_RECO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("OMEGAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_omegat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","OMEGAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("OMEGA_EXB",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_omega_exb
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","OMEGA_EXB")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("P0_RECO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_p0_reco
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","P0_RECO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PB0_HALO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pb0_halo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PB0_HALO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PBE",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pbe
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PBE")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PBI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pbi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PBI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PBTH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pbth
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PBTH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PCX_HALO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pcx_halo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PCX_HALO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PCX_RECO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pcx_reco
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PCX_RECO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PEECH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_peech
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PEECH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PEECH_SRC",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_peech_src
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PEECH_SRC")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PELH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pelh
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PELH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PELH_SRC",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pelh_src
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PELH_SRC")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PE_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pe_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PE_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PE_TRANS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pe_trans
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PE_TRANS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PE_TRANSCAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pe_transcat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PE_TRANSCAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PFUSE",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pfuse
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PFUSE")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PFUSI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pfusi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PFUSI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PFUSTH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pfusth
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PFUSTH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PHIFLUX_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_phiflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PHIFLUX_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PHIT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_phit
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PHIT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PICRF_NPHI_SRCS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_picrf_nphi_srcs
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PICRF_NPHI_SRCS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PICRF_SRCS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_picrf_srcs
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PICRF_SRCS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PICRF_TOTALS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_picrf_totals
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PICRF_TOTALS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PICTH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_picth
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PICTH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PILH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pilh
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PILH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PILH_SRC",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pilh_src
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PILH_SRC")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PI_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pi_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PI_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PI_TRANS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pi_trans
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PI_TRANS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PI_TRANSCAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pi_transcat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PI_TRANSCAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PMINE",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pmine
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PMINE")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PMINI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pmini
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PMINI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PMINTH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pminth
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PMINTH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("POHME",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pohme
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","POHME")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("POT_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_pot_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","POT_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PRAD",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_prad
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PRAD")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PRAD_BR",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_prad_br
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PRAD_BR")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PRAD_CY",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_prad_cy
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PRAD_CY")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PRAD_LI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_prad_li
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PRAD_LI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PSC_HALO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_psc_halo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PSC_HALO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PSC_RECO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_psc_reco
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PSC_RECO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PSIPOL",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_psipol
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PSIPOL")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PSIRZ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_psirz
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PSIRZ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PSMOM_ERRCK",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_psmom_errck
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PSMOM_ERRCK")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("PSMOM_NC",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_psmom_nc
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","PSMOM_NC")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("P_EQ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_p_eq
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","P_EQ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("QBEAMI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_qbeami
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","QBEAMI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("QCOMP_E",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_qcomp_e
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","QCOMP_E")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("QCOMP_I",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_qcomp_i
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","QCOMP_I")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("QIE",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_qie
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","QIE")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("QIONIZ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_qioniz
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","QIONIZ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("QQCX",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_qqcx
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","QQCX")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("QROT_CONV",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_qrot_conv
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","QROT_CONV")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("QROT_DIFF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_qrot_diff
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","QROT_DIFF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("Q_EQ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_q_eq
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","Q_EQ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("R0_MOMEQ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_r0_momeq
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","R0_MOMEQ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("RATE_SINB0I",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_rate_sinb0i
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","RATE_SINB0I")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("RATE_SINB0X",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_rate_sinb0x
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","RATE_SINB0X")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("RATE_SINB0XS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_rate_sinb0xs
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","RATE_SINB0XS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("RATE_SINF0I",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_rate_sinf0i
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","RATE_SINF0I")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("RATE_SINF0X",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_rate_sinf0x
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","RATE_SINF0X")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("RATE_SINF0XS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_rate_sinf0xs
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","RATE_SINF0XS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("RES_SN",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_res_sn
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","RES_SN")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("RES_TE",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_res_te
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","RES_TE")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("RES_TI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_res_ti
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","RES_TI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("RES_TQ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_res_tq
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","RES_TQ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("RMAJOR_MEAN",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_rmajor_mean
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","RMAJOR_MEAN")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("RMINOR_MEAN",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_rminor_mean
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","RMINOR_MEAN")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("R_GEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_r_geo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","R_GEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("R_MIDP_IN",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_r_midp_in
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","R_MIDP_IN")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("R_MIDP_OUT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_r_midp_out
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","R_MIDP_OUT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("R_SURFMAX",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_r_surfmax
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","R_SURFMAX")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("R_SURFMIN",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_r_surfmin
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","R_SURFMIN")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("S0RECO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_s0reco
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","S0RECO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("S0RECO_E",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_s0reco_e
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","S0RECO_E")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("S0RECO_RECAP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_s0reco_recap
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","S0RECO_RECAP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SB0HALO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sb0halo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SB0HALO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SB0HALO_RECAP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sb0halo_recap
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SB0HALO_RECAP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SBEDEP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sbedep
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SBEDEP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SBEHALO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sbehalo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SBEHALO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SBSCE",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sbsce
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SBSCE")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SBTHERM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sbtherm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SBTHERM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SC_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sc_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SC_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SFSCE",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sfsce
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SFSCE")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SFTHERM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sftherm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SFTHERM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SII_XS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sii_xs
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SII_XS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SN_TRANS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sn_trans
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SN_TRANS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SN_TRANSCAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sn_transcat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SN_TRANSCAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SPROF0",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sprof0
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SPROF0")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SPROF0E",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_sprof0e
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SPROF0E")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SPSCE",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_spsce
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SPSCE")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SQUARELO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_squarelo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SQUARELO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SQUAREUO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_squareuo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SQUAREUO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("SURF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_surf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","SURF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("T0CX",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_t0cx
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","T0CX")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("T0SC0",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_t0sc0
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","T0SC0")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("T0_HALO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_t0_halo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","T0_HALO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("T0_RECO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_t0_reco
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","T0_RECO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TEFLUX_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_teflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TEFLUX_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TE_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_te_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TE_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TFRIP2_LOG",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tfrip2_log
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TFRIP2_LOG")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TFRIP_LOG",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tfrip_log
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TFRIP_LOG")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_ti
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TIFLUX_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tiflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TIFLUX_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TI_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_ti_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TI_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TMODEL",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tmodel
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TMODEL")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQ0_RECO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tq0_reco
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQ0_RECO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQB0_HALO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tqb0_halo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQB0_HALO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQBE",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tqbe
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQBE")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQBI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tqbi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQBI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQBJXB",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tqbjxb
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQBJXB")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQBTH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tqbth
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQBTH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQCX_HALO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tqcx_halo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQCX_HALO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQCX_RECO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tqcx_reco
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQCX_RECO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQIONIZ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tqioniz
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQIONIZ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQQCX",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tqqcx
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQQCX")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQSC_HALO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tqsc_halo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQSC_HALO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQSC_RECO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tqsc_reco
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQSC_RECO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQ_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tq_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQ_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQ_TRANS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tq_trans
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQ_TRANS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TQ_TRANSCAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tq_transcat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TQ_TRANSCAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TRIANG",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_triang
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TRIANG")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TRIANGL",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_triangl
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TRIANGL")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TRIANGU",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_triangu
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TRIANGU")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TRIANG_MILLER_L",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_triang_miller_l
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TRIANG_MILLER_L")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TRIANG_MILLER_U",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_triang_miller_u
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TRIANG_MILLER_U")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_ts
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TZFLUX_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tzflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TZFLUX_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("TZ_KY_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_tz_ky_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","TZ_KY_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("UPWIND_PFRAC_NS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_upwind_pfrac_ns
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","UPWIND_PFRAC_NS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("UPWIND_PFRAC_OMEGA",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_upwind_pfrac_omega
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","UPWIND_PFRAC_OMEGA")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("UPWIND_PFRAC_TE",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_upwind_pfrac_te
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","UPWIND_PFRAC_TE")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("UPWIND_PFRAC_TI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_upwind_pfrac_ti
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","UPWIND_PFRAC_TI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VEE_TRANS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_vee_trans
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VEE_TRANS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VEE_TRANSCAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_vee_transcat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VEE_TRANSCAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELB_FUSI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velb_fusi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELB_FUSI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELB_NBI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velb_nbi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELB_NBI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELB_RFMI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velb_rfmi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELB_RFMI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELNE_NCLASS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velne_nclass
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELNE_NCLASS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELNH_NCLASS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velnh_nclass
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELNH_NCLASS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELNI_DRBM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velni_drbm
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELNI_DRBM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELNS_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velns_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELNS_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELNS_GLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velns_glf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELNS_GLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELNS_GTCNEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velns_gtcneo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELNS_GTCNEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELNS_MMM71",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velns_mmm71
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELNS_MMM71")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELNS_NCLASS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velns_nclass
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELNS_NCLASS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELNS_NEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velns_neo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELNS_NEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELNS_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velns_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELNS_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELNX_NCLASS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velnx_nclass
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELNX_NCLASS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELPPHI_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velpphi_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELPPHI_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELPPHI_GLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velpphi_glf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELPPHI_GLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELPPHI_GTCNEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velpphi_gtcneo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELPPHI_GTCNEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELPPHI_MMM71",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velpphi_mmm71
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELPPHI_MMM71")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELPPHI_NEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velpphi_neo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELPPHI_NEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELPPHI_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velpphi_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELPPHI_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTE_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velte_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTE_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTE_GLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velte_glf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTE_GLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTE_GTCNEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velte_gtcneo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTE_GTCNEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTE_MMM71",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velte_mmm71
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTE_MMM71")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTE_NCLASS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velte_nclass
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTE_NCLASS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTE_NEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velte_neo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTE_NEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTE_PALEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velte_paleo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTE_PALEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTE_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velte_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTE_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTI_ANOM",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velti_anom
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTI_ANOM")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTI_GLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velti_glf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTI_GLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTI_GTCNEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velti_gtcneo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTI_GTCNEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTI_MMM71",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velti_mmm71
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTI_MMM71")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTI_NCLASS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velti_nclass
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTI_NCLASS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTI_NEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velti_neo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTI_NEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VELTI_TGLF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_velti_tglf
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VELTI_TGLF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VIE_TRANS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_vie_trans
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VIE_TRANS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VIE_TRANSCAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_vie_transcat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VIE_TRANSCAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VMO_TRANS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_vmo_trans
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VMO_TRANS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VMO_TRANSCAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_vmo_transcat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VMO_TRANSCAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VN_TRANS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_vn_trans
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VN_TRANS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VN_TRANSCAT",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_vn_transcat
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VN_TRANSCAT")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VOL",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_vol
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VOL")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VPOL_INMP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_vpol_inmp
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VPOL_INMP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VPOL_OMP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_vpol_omp
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VPOL_OMP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VTOR_INMP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_vtor_inmp
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VTOR_INMP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("VTOR_OMP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_vtor_omp
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","VTOR_OMP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("V_LOOP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_v_loop
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","V_LOOP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("V_PARS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_v_pars
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","V_PARS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("V_PERS",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_v_pers
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","V_PERS")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("XRJCOS_MOMEQ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_xrjcos_momeq
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","XRJCOS_MOMEQ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("XRJSIN_MOMEQ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_xrjsin_momeq
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","XRJSIN_MOMEQ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("XZJCOS_MOMEQ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_xzjcos_momeq
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","XZJCOS_MOMEQ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("XZJSIN_MOMEQ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_xzjsin_momeq
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","XZJSIN_MOMEQ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("Z0_MOMEQ",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_z0_momeq
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","Z0_MOMEQ")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("ZEFF",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_zeff
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","ZEFF")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("ZEFF_FI",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_zeff_fi
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","ZEFF_FI")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("ZEFF_TH",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_zeff_th
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","ZEFF_TH")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("Z_GEO",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_z_geo
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","Z_GEO")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("Z_MIDP",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_z_midp
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","Z_MIDP")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("Z_SURFMAX",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_z_surfmax
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","Z_SURFMAX")
     ENDIF
  ENDIF

  IF((ss(1:ils).EQ."*").OR.(INDEX("Z_SURFMIN",ss(1:ils)).GT.0)) THEN
     icomp = icomp + 1
     iaddr1 = ps_phash_z_surfmin
     iaddr2 = iaddr1 + 1
     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &
        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN
        idiff = idiff + 1
        if(idiff.eq.1) call diff_header
        call diff_report("PROFILE","Z_SURFMIN")
     ENDIF
  ENDIF

  IF(icomp.EQ.0) THEN
     write(iout,*) ' --> plasma_state_hash_diff: no comparison, sstr="'// &
          trim(sstr)//'".'
  ELSE IF(idiff.EQ.0) THEN
     write(iout,*) ' --> plasma_state_hash_diff: no hash differences between states:'
     write(iout,*) '    "'//trim(lbl1)//'" and "'//trim(lbl2)//'".'
     write(iout,*) '    (sstr="'//trim(sstr)//'"; ',icomp,' hashes compared).'
  ELSE
     write(iout,*) ' --> plasma_state_hash_diff: ',idiff,' hash difference(s) between states:'
     write(iout,*) '    "'//trim(lbl1)//'" and "'//trim(lbl2)//'".'
     write(iout,*) '    (sstr="'//trim(sstr)//'"; ',icomp,' hashes compared).'
  ENDIF
CONTAINS
  subroutine diff_header
     write(iout,*) ' ----------------------- '
     write(iout,*) ' plasma_state_hash_diff detects hash code differences:'
  end subroutine diff_header

  subroutine diff_report(typestr,namestr)
     character*(*) :: typestr
     character*(*) :: namestr
     write(iout,*) '     '//trim(typestr)//' hash for '//trim(namestr)//' is different.'
  end subroutine diff_report

END SUBROUTINE plasma_state_hash_diff
