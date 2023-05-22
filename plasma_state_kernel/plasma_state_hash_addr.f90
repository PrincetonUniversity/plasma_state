! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


LOGICAL FUNCTION plasma_state_hash_match(ss,iaddr,ierr)

  ! return .TRUE. if hash codes match, implying no change in data
  ! this routine assumes hash codes are up-to-date:
  ! ...CALL plasma_hash_state_update(ss,ierr) first!!!

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss       ! state data to check
  INTEGER, INTENT(IN) :: iaddr    ! address of hash codes to check
  INTEGER, INTENT(OUT) :: ierr    ! completion code (0=OK)

  ! non-zero ierr value indicates iaddr out of range

  !-------------------
  INTEGER :: iout
  !-------------------

  ierr = 0
  plasma_state_hash_match = .FALSE.

  iout = SWIM_err_out_file

  IF((iaddr.LE.0).or.(iaddr.GE.ps_hash_total)) THEN
     WRITE(iout,*) ' ?plasma_state_hash_match: iaddr out of expected range.'
     WRITE(iout,*) '  iaddr = ',iaddr,'; expected range: 1 to ',ps_hash_total-1
     ierr=9999
     RETURN
  ENDIF

  plasma_state_hash_match =(ss%hash_codes(iaddr).eq.ss%new_hash_codes(iaddr)) &
                   .AND.(ss%hash_codes(iaddr+1).eq.ss%new_hash_codes(iaddr+1))

END FUNCTION plasma_state_hash_match

SUBROUTINE plasma_state_hash_mark(ss,iaddr,ierr)

  ! mark indicated item as up-to-date, by copying its hash codes

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss       ! state data to check
  INTEGER, INTENT(IN) :: iaddr    ! address of hash codes to check
  INTEGER, INTENT(OUT) :: ierr    ! completion code (0=OK)

  ! non-zero ierr value indicates iaddr out of range

  !-------------------
  INTEGER :: iout
  !-------------------

  ierr = 0
  iout = SWIM_err_out_file

  IF((iaddr.LE.0).or.(iaddr.GE.ps_hash_total)) THEN
     WRITE(iout,*) ' ?plasma_state_hash_mark: iaddr out of expected range.'
     WRITE(iout,*) '  iaddr = ',iaddr,'; expected range: 1 to ',ps_hash_total-1
     ierr=9999
     RETURN
  ENDIF

  ss%hash_codes(iaddr) = ss%new_hash_codes(iaddr)
  ss%hash_codes(iaddr+1) = ss%new_hash_codes(iaddr+1)

END SUBROUTINE plasma_state_hash_mark

SUBROUTINE plasma_state_hash_addr(htype,hname,iaddr)

  ! return base hash address for named entity of indicated type
  !   iaddr -- base; iaddr+1 -- 2ndary hash address

  ! (at present this routine does not know about component-specific
  ! section hash codes)

  USE plasma_state_definition_mod
  IMPLICIT NONE

  ! character string inputs are copied into local variables (C*32)
  ! which are converted to uppercase

  CHARACTER*(*), INTENT(IN) :: htype  ! hash entity type:
  !                             "SECTION" or "DIMENSION" or "PROFILE"

  CHARACTER*(*), INTENT(IN) :: hname  ! hash entity name:
  !                      example for "SECTION":  "MACHINE_DESCRIPTION"
  !                      example for "DIMENSION":  "RHO_EQ"
  !                      example for "PROFILE":  "TS"

  INTEGER, INTENT(OUT) :: iaddr       ! base hash address
  !  0 is returned & warning issued if htype & name do not map to a known
  !  quantity

  !----------------------------------
  CHARACTER*32 :: htypu,hnamu
  INTEGER :: iout
  !----------------------------------

  iout = SWIM_err_out_file

  !  fold input arguments to upper case

  htypu = htype
  hnamu = hname

  call uupper(htypu)
  call uupper(hnamu)

  iaddr = 0

  !--------------------------------------------
  ! section hash codes

  IF((htypu.EQ."SECTION").AND.(hnamu.EQ."MACHINE_DESCRIPTION")) THEN
     iaddr = ps_shash_machine_description
     RETURN
  ENDIF

  IF((htypu.EQ."SECTION").AND.(hnamu.EQ."SHOT_CONFIGURATION")) THEN
     iaddr = ps_shash_shot_configuration
     RETURN
  ENDIF

  IF((htypu.EQ."SECTION").AND.(hnamu.EQ."SIMULATION_INIT")) THEN
     iaddr = ps_shash_simulation_init
     RETURN
  ENDIF

  IF((htypu.EQ."SECTION").AND.(hnamu.EQ."STATE_DATA")) THEN
     iaddr = ps_shash_state_data
     RETURN
  ENDIF

  IF((htypu.EQ."SECTION").AND.(hnamu.EQ."STATE_PROFILES")) THEN
     iaddr = ps_shash_state_profiles
     RETURN
  ENDIF

  !--------------------------------------------
  ! dimension hash codes

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."MAX_NRZ_ANTGEO")) THEN
     iaddr = ps_dhash_max_nrz_antgeo
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."MAX_NSPECTRUMLH_SRC")) THEN
     iaddr = ps_dhash_max_nspectrumlh_src
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."MAX_NUM_NPHI")) THEN
     iaddr = ps_dhash_max_num_nphi
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."MAX_NUM_NPHI_VAC")) THEN
     iaddr = ps_dhash_max_num_nphi_vac
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."MAX_N_STRAPS")) THEN
     iaddr = ps_dhash_max_n_straps
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."MMODES")) THEN
     iaddr = ps_dhash_mmodes
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NBALLOON")) THEN
     iaddr = ps_dhash_nballoon
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NBEAM")) THEN
     iaddr = ps_dhash_nbeam
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NCIRCUITS")) THEN
     iaddr = ps_dhash_ncircuits
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NCOILS")) THEN
     iaddr = ps_dhash_ncoils
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NDELTA_W")) THEN
     iaddr = ps_dhash_ndelta_w
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NECRF_SRC")) THEN
     iaddr = ps_dhash_necrf_src
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NEFI_ANOM")) THEN
     iaddr = ps_dhash_nefi_anom
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NEFI_ANOM2")) THEN
     iaddr = ps_dhash_nefi_anom2
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NEQMOM")) THEN
     iaddr = ps_dhash_neqmom
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NGSC0")) THEN
     iaddr = ps_dhash_ngsc0
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NICRF_SRC")) THEN
     iaddr = ps_dhash_nicrf_src
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NKY_ANOMTH")) THEN
     iaddr = ps_dhash_nky_anomth
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NKY_MMM")) THEN
     iaddr = ps_dhash_nky_mmm
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NLHRF_SRC")) THEN
     iaddr = ps_dhash_nlhrf_src
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NNEWCOMB")) THEN
     iaddr = ps_dhash_nnewcomb
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NPSMOM")) THEN
     iaddr = ps_dhash_npsmom
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NR")) THEN
     iaddr = ps_dhash_nr
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO")) THEN
     iaddr = ps_dhash_nrho
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_ANOM")) THEN
     iaddr = ps_dhash_nrho_anom
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_ANOM2")) THEN
     iaddr = ps_dhash_nrho_anom2
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_ANOMTH")) THEN
     iaddr = ps_dhash_nrho_anomth
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_ECRF")) THEN
     iaddr = ps_dhash_nrho_ecrf
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_EQ")) THEN
     iaddr = ps_dhash_nrho_eq
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_EQ_GEO")) THEN
     iaddr = ps_dhash_nrho_eq_geo
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_FUS")) THEN
     iaddr = ps_dhash_nrho_fus
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_GAS")) THEN
     iaddr = ps_dhash_nrho_gas
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_ICRF")) THEN
     iaddr = ps_dhash_nrho_icrf
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_LHRF")) THEN
     iaddr = ps_dhash_nrho_lhrf
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_LMHD")) THEN
     iaddr = ps_dhash_nrho_lmhd
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_MIST")) THEN
     iaddr = ps_dhash_nrho_mist
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_NBI")) THEN
     iaddr = ps_dhash_nrho_nbi
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_PEL")) THEN
     iaddr = ps_dhash_nrho_pel
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_RAD")) THEN
     iaddr = ps_dhash_nrho_rad
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NRHO_RW")) THEN
     iaddr = ps_dhash_nrho_rw
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NR_RIP")) THEN
     iaddr = ps_dhash_nr_rip
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NSPEC_ALL")) THEN
     iaddr = ps_dhash_nspec_all
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NSPEC_ALLA")) THEN
     iaddr = ps_dhash_nspec_alla
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NSPEC_BEAM")) THEN
     iaddr = ps_dhash_nspec_beam
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NSPEC_FUSION")) THEN
     iaddr = ps_dhash_nspec_fusion
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NSPEC_GAS")) THEN
     iaddr = ps_dhash_nspec_gas
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NSPEC_IMP0")) THEN
     iaddr = ps_dhash_nspec_imp0
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NSPEC_IMPI")) THEN
     iaddr = ps_dhash_nspec_impi
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NSPEC_RFMIN")) THEN
     iaddr = ps_dhash_nspec_rfmin
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NSPEC_TH")) THEN
     iaddr = ps_dhash_nspec_th
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NSPEC_THA")) THEN
     iaddr = ps_dhash_nspec_tha
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NTH_EQ")) THEN
     iaddr = ps_dhash_nth_eq
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NTRCAT")) THEN
     iaddr = ps_dhash_ntrcat
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NUM_RZLIM")) THEN
     iaddr = ps_dhash_num_rzlim
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NXS")) THEN
     iaddr = ps_dhash_nxs
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NZ")) THEN
     iaddr = ps_dhash_nz
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."NZ_RIP")) THEN
     iaddr = ps_dhash_nz_rip
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."N_D_EINJ_STANDARD")) THEN
     iaddr = ps_dhash_n_d_einj_standard
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."N_H_EINJ_STANDARD")) THEN
     iaddr = ps_dhash_n_h_einj_standard
     RETURN
  ENDIF

  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."N_T_EINJ_STANDARD")) THEN
     iaddr = ps_dhash_n_t_einj_standard
     RETURN
  ENDIF

  !--------------------------------------------
  ! profile hash codes

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."ANOM_EVAR")) THEN
     iaddr = ps_phash_anom_evar
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."AREA")) THEN
     iaddr = ps_phash_area
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."BALSTAB")) THEN
     iaddr = ps_phash_balstab
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."BIDIFF_BPASS_CO")) THEN
     iaddr = ps_phash_bidiff_bpass_co
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."BIDIFF_BPASS_CTR")) THEN
     iaddr = ps_phash_bidiff_bpass_ctr
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."BIDIFF_BTRAP")) THEN
     iaddr = ps_phash_bidiff_btrap
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."BIDIFF_DPASS_CO")) THEN
     iaddr = ps_phash_bidiff_dpass_co
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."BIDIFF_DPASS_CTR")) THEN
     iaddr = ps_phash_bidiff_dpass_ctr
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."BIDIFF_DTRAP")) THEN
     iaddr = ps_phash_bidiff_dtrap
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."BPHIRZ")) THEN
     iaddr = ps_phash_bphirz
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."BRRZ")) THEN
     iaddr = ps_phash_brrz
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."BZRZ")) THEN
     iaddr = ps_phash_bzrz
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."B_SURFMAX")) THEN
     iaddr = ps_phash_b_surfmax
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."B_SURFMIN")) THEN
     iaddr = ps_phash_b_surfmin
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CDICRF")) THEN
     iaddr = ps_phash_cdicrf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CDICRF_NPHI")) THEN
     iaddr = ps_phash_cdicrf_nphi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_ANOM")) THEN
     iaddr = ps_phash_chie_anom
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_DRBM")) THEN
     iaddr = ps_phash_chie_drbm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_ETG")) THEN
     iaddr = ps_phash_chie_etg
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_GLF")) THEN
     iaddr = ps_phash_chie_glf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_GTCNEO")) THEN
     iaddr = ps_phash_chie_gtcneo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_MMM71")) THEN
     iaddr = ps_phash_chie_mmm71
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_MTM")) THEN
     iaddr = ps_phash_chie_mtm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_NCLASS")) THEN
     iaddr = ps_phash_chie_nclass
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_NEO")) THEN
     iaddr = ps_phash_chie_neo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_NTM")) THEN
     iaddr = ps_phash_chie_ntm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_PALEO")) THEN
     iaddr = ps_phash_chie_paleo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_TGLF")) THEN
     iaddr = ps_phash_chie_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_TRANS")) THEN
     iaddr = ps_phash_chie_trans
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_TRANSCAT")) THEN
     iaddr = ps_phash_chie_transcat
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIE_W19")) THEN
     iaddr = ps_phash_chie_w19
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHII_ANOM")) THEN
     iaddr = ps_phash_chii_anom
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHII_DRBM")) THEN
     iaddr = ps_phash_chii_drbm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHII_GLF")) THEN
     iaddr = ps_phash_chii_glf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHII_GTCNEO")) THEN
     iaddr = ps_phash_chii_gtcneo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHII_MMM71")) THEN
     iaddr = ps_phash_chii_mmm71
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHII_NCLASS")) THEN
     iaddr = ps_phash_chii_nclass
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHII_NEO")) THEN
     iaddr = ps_phash_chii_neo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHII_NTM")) THEN
     iaddr = ps_phash_chii_ntm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHII_TGLF")) THEN
     iaddr = ps_phash_chii_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHII_TRANS")) THEN
     iaddr = ps_phash_chii_trans
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHII_TRANSCAT")) THEN
     iaddr = ps_phash_chii_transcat
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHII_W19")) THEN
     iaddr = ps_phash_chii_w19
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIMO_TRANS")) THEN
     iaddr = ps_phash_chimo_trans
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIMO_TRANSCAT")) THEN
     iaddr = ps_phash_chimo_transcat
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIPHI_ANOM")) THEN
     iaddr = ps_phash_chiphi_anom
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIPHI_DRBM")) THEN
     iaddr = ps_phash_chiphi_drbm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIPHI_GLF")) THEN
     iaddr = ps_phash_chiphi_glf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIPHI_GTCNEO")) THEN
     iaddr = ps_phash_chiphi_gtcneo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIPHI_MMM71")) THEN
     iaddr = ps_phash_chiphi_mmm71
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIPHI_NEO")) THEN
     iaddr = ps_phash_chiphi_neo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIPHI_TGLF")) THEN
     iaddr = ps_phash_chiphi_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHIPHI_W19")) THEN
     iaddr = ps_phash_chiphi_w19
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CHITHETA_MMM71")) THEN
     iaddr = ps_phash_chitheta_mmm71
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CURBEAM")) THEN
     iaddr = ps_phash_curbeam
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CURECH")) THEN
     iaddr = ps_phash_curech
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CURECH_SRC")) THEN
     iaddr = ps_phash_curech_src
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CURFUSN")) THEN
     iaddr = ps_phash_curfusn
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CURICH")) THEN
     iaddr = ps_phash_curich
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CURLH")) THEN
     iaddr = ps_phash_curlh
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CURLH_SRC")) THEN
     iaddr = ps_phash_curlh_src
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CURMINO")) THEN
     iaddr = ps_phash_curmino
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CURR_BOOTSTRAP")) THEN
     iaddr = ps_phash_curr_bootstrap
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CURR_OHMIC")) THEN
     iaddr = ps_phash_curr_ohmic
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CURT")) THEN
     iaddr = ps_phash_curt
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."CUR_RW")) THEN
     iaddr = ps_phash_cur_rw
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."D2R_GEO_DRHODTH")) THEN
     iaddr = ps_phash_d2r_geo_drhodth
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."D2Z_GEO_DRHODTH")) THEN
     iaddr = ps_phash_d2z_geo_drhodth
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DB_MTM")) THEN
     iaddr = ps_phash_db_mtm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DCUR_RW_DVLOOP")) THEN
     iaddr = ps_phash_dcur_rw_dvloop
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFB_FUSI")) THEN
     iaddr = ps_phash_difb_fusi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFB_NBI")) THEN
     iaddr = ps_phash_difb_nbi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFB_RFMI")) THEN
     iaddr = ps_phash_difb_rfmi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFF_TRANS")) THEN
     iaddr = ps_phash_diff_trans
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFF_TRANSCAT")) THEN
     iaddr = ps_phash_diff_transcat
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFNE_NCLASS")) THEN
     iaddr = ps_phash_difne_nclass
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFNH_NCLASS")) THEN
     iaddr = ps_phash_difnh_nclass
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFNS_ANOM")) THEN
     iaddr = ps_phash_difns_anom
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFNS_DRBM")) THEN
     iaddr = ps_phash_difns_drbm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFNS_GLF")) THEN
     iaddr = ps_phash_difns_glf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFNS_GTCNEO")) THEN
     iaddr = ps_phash_difns_gtcneo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFNS_MMM71")) THEN
     iaddr = ps_phash_difns_mmm71
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFNS_NCLASS")) THEN
     iaddr = ps_phash_difns_nclass
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFNS_NEO")) THEN
     iaddr = ps_phash_difns_neo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFNS_TGLF")) THEN
     iaddr = ps_phash_difns_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFNS_W19")) THEN
     iaddr = ps_phash_difns_w19
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFNX_NCLASS")) THEN
     iaddr = ps_phash_difnx_nclass
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DIFNZ_MMM71")) THEN
     iaddr = ps_phash_difnz_mmm71
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DQEDGRDTE_TGLF")) THEN
     iaddr = ps_phash_dqedgrdte_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DQEDGRDTI_TGLF")) THEN
     iaddr = ps_phash_dqedgrdti_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DQIDGRDTE_TGLF")) THEN
     iaddr = ps_phash_dqidgrdte_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DQIDGRDTI_TGLF")) THEN
     iaddr = ps_phash_dqidgrdti_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DR0_MOMEQ_DRHO")) THEN
     iaddr = ps_phash_dr0_momeq_drho
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DR_GEO_DRHO")) THEN
     iaddr = ps_phash_dr_geo_drho
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DR_GEO_DTH")) THEN
     iaddr = ps_phash_dr_geo_dth
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DXRJCOS_MOMEQ_DRHO")) THEN
     iaddr = ps_phash_dxrjcos_momeq_drho
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DXRJSIN_MOMEQ_DRHO")) THEN
     iaddr = ps_phash_dxrjsin_momeq_drho
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DXZJCOS_MOMEQ_DRHO")) THEN
     iaddr = ps_phash_dxzjcos_momeq_drho
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DXZJSIN_MOMEQ_DRHO")) THEN
     iaddr = ps_phash_dxzjsin_momeq_drho
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DZ0_MOMEQ_DRHO")) THEN
     iaddr = ps_phash_dz0_momeq_drho
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DZ_GEO_DRHO")) THEN
     iaddr = ps_phash_dz_geo_drho
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."DZ_GEO_DTH")) THEN
     iaddr = ps_phash_dz_geo_dth
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."EE_MOBILITY_FACTOR")) THEN
     iaddr = ps_phash_ee_mobility_factor
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."ELONG")) THEN
     iaddr = ps_phash_elong
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."EPERP_BEAMI")) THEN
     iaddr = ps_phash_eperp_beami
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."EPERP_FUSI")) THEN
     iaddr = ps_phash_eperp_fusi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."EPERP_MINI")) THEN
     iaddr = ps_phash_eperp_mini
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."EPERP_RW")) THEN
     iaddr = ps_phash_eperp_rw
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."EPLL_BEAMI")) THEN
     iaddr = ps_phash_epll_beami
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."EPLL_FUSI")) THEN
     iaddr = ps_phash_epll_fusi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."EPLL_MINI")) THEN
     iaddr = ps_phash_epll_mini
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."EPLL_RW")) THEN
     iaddr = ps_phash_epll_rw
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."EPOT")) THEN
     iaddr = ps_phash_epot
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."ETA_PARALLEL")) THEN
     iaddr = ps_phash_eta_parallel
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FIDIFF_BPASS_CO")) THEN
     iaddr = ps_phash_fidiff_bpass_co
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FIDIFF_BPASS_CTR")) THEN
     iaddr = ps_phash_fidiff_bpass_ctr
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FIDIFF_BTRAP")) THEN
     iaddr = ps_phash_fidiff_btrap
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FIDIFF_DPASS_CO")) THEN
     iaddr = ps_phash_fidiff_dpass_co
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FIDIFF_DPASS_CTR")) THEN
     iaddr = ps_phash_fidiff_dpass_ctr
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FIDIFF_DTRAP")) THEN
     iaddr = ps_phash_fidiff_dtrap
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FI_DEPLETION")) THEN
     iaddr = ps_phash_fi_depletion
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FREQ_DRBM")) THEN
     iaddr = ps_phash_freq_drbm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FREQ_ETG")) THEN
     iaddr = ps_phash_freq_etg
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FREQ_GLF")) THEN
     iaddr = ps_phash_freq_glf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FREQ_MAX_TGLF")) THEN
     iaddr = ps_phash_freq_max_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FREQ_MMM71")) THEN
     iaddr = ps_phash_freq_mmm71
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FREQ_MTM")) THEN
     iaddr = ps_phash_freq_mtm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."FREQ_TGLF")) THEN
     iaddr = ps_phash_freq_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GAMMA_NC")) THEN
     iaddr = ps_phash_gamma_nc
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GB1")) THEN
     iaddr = ps_phash_gb1
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GB2")) THEN
     iaddr = ps_phash_gb2
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GB2I")) THEN
     iaddr = ps_phash_gb2i
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GBR2")) THEN
     iaddr = ps_phash_gbr2
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GNCFB2H")) THEN
     iaddr = ps_phash_gncfb2h
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GNCFH")) THEN
     iaddr = ps_phash_gncfh
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GR1")) THEN
     iaddr = ps_phash_gr1
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GR2")) THEN
     iaddr = ps_phash_gr2
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GR2I")) THEN
     iaddr = ps_phash_gr2i
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GR2RHO2")) THEN
     iaddr = ps_phash_gr2rho2
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GR3I")) THEN
     iaddr = ps_phash_gr3i
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GRHO1")) THEN
     iaddr = ps_phash_grho1
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GRHO2")) THEN
     iaddr = ps_phash_grho2
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GRHO2B2I")) THEN
     iaddr = ps_phash_grho2b2i
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GRHO2R2I")) THEN
     iaddr = ps_phash_grho2r2i
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GRHO2R3I")) THEN
     iaddr = ps_phash_grho2r3i
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GRI")) THEN
     iaddr = ps_phash_gri
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GRIRHOI")) THEN
     iaddr = ps_phash_grirhoi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GROWTHRATE_DRBM")) THEN
     iaddr = ps_phash_growthrate_drbm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GROWTHRATE_ETG")) THEN
     iaddr = ps_phash_growthrate_etg
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GROWTHRATE_GLF")) THEN
     iaddr = ps_phash_growthrate_glf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GROWTHRATE_MAX_TGLF")) THEN
     iaddr = ps_phash_growthrate_max_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GROWTHRATE_MMM71")) THEN
     iaddr = ps_phash_growthrate_mmm71
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GROWTHRATE_MTM")) THEN
     iaddr = ps_phash_growthrate_mtm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GROWTHRATE_TGLF")) THEN
     iaddr = ps_phash_growthrate_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."GROWTHRATE_VPAR")) THEN
     iaddr = ps_phash_growthrate_vpar
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."G_EQ")) THEN
     iaddr = ps_phash_g_eq
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."IOTA")) THEN
     iaddr = ps_phash_iota
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."JDOTB")) THEN
     iaddr = ps_phash_jdotb
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."KYRHOS_DRBM")) THEN
     iaddr = ps_phash_kyrhos_drbm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."KYRHOS_ETG")) THEN
     iaddr = ps_phash_kyrhos_etg
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."KYRHOS_MTM")) THEN
     iaddr = ps_phash_kyrhos_mtm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."KYSPECTRUM_TGLF")) THEN
     iaddr = ps_phash_kyspectrum_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."LPOL")) THEN
     iaddr = ps_phash_lpol
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."N0NORM")) THEN
     iaddr = ps_phash_n0norm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."N0_HALO")) THEN
     iaddr = ps_phash_n0_halo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."N0_RECO")) THEN
     iaddr = ps_phash_n0_reco
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NBEAMI")) THEN
     iaddr = ps_phash_nbeami
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NEFLUX_KY_TGLF")) THEN
     iaddr = ps_phash_neflux_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NE_KY_TGLF")) THEN
     iaddr = ps_phash_ne_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NFUSI")) THEN
     iaddr = ps_phash_nfusi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NGRADB2_AV")) THEN
     iaddr = ps_phash_ngradb2_av
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NI")) THEN
     iaddr = ps_phash_ni
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NIFLUX_KY_TGLF")) THEN
     iaddr = ps_phash_niflux_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NI_KY_TGLF")) THEN
     iaddr = ps_phash_ni_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NMINI")) THEN
     iaddr = ps_phash_nmini
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NMODEL")) THEN
     iaddr = ps_phash_nmodel
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NRW")) THEN
     iaddr = ps_phash_nrw
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NS")) THEN
     iaddr = ps_phash_ns
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NZFLUX_KY_TGLF")) THEN
     iaddr = ps_phash_nzflux_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."NZ_KY_TGLF")) THEN
     iaddr = ps_phash_nz_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."OMEG0CX")) THEN
     iaddr = ps_phash_omeg0cx
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."OMEG0SC0")) THEN
     iaddr = ps_phash_omeg0sc0
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."OMEG0_HALO")) THEN
     iaddr = ps_phash_omeg0_halo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."OMEG0_RECO")) THEN
     iaddr = ps_phash_omeg0_reco
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."OMEGAT")) THEN
     iaddr = ps_phash_omegat
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."OMEGA_EXB")) THEN
     iaddr = ps_phash_omega_exb
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."P0_RECO")) THEN
     iaddr = ps_phash_p0_reco
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PB0_HALO")) THEN
     iaddr = ps_phash_pb0_halo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PBE")) THEN
     iaddr = ps_phash_pbe
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PBI")) THEN
     iaddr = ps_phash_pbi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PBTH")) THEN
     iaddr = ps_phash_pbth
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PCX_HALO")) THEN
     iaddr = ps_phash_pcx_halo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PCX_RECO")) THEN
     iaddr = ps_phash_pcx_reco
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PEECH")) THEN
     iaddr = ps_phash_peech
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PEECH_SRC")) THEN
     iaddr = ps_phash_peech_src
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PELH")) THEN
     iaddr = ps_phash_pelh
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PELH_SRC")) THEN
     iaddr = ps_phash_pelh_src
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PE_ANOM")) THEN
     iaddr = ps_phash_pe_anom
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PE_TRANS")) THEN
     iaddr = ps_phash_pe_trans
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PE_TRANSCAT")) THEN
     iaddr = ps_phash_pe_transcat
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PFUSE")) THEN
     iaddr = ps_phash_pfuse
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PFUSI")) THEN
     iaddr = ps_phash_pfusi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PFUSTH")) THEN
     iaddr = ps_phash_pfusth
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PHIFLUX_KY_TGLF")) THEN
     iaddr = ps_phash_phiflux_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PHIT")) THEN
     iaddr = ps_phash_phit
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PICRF_NPHI_SRCS")) THEN
     iaddr = ps_phash_picrf_nphi_srcs
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PICRF_SRCS")) THEN
     iaddr = ps_phash_picrf_srcs
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PICRF_TOTALS")) THEN
     iaddr = ps_phash_picrf_totals
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PICTH")) THEN
     iaddr = ps_phash_picth
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PILH")) THEN
     iaddr = ps_phash_pilh
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PILH_SRC")) THEN
     iaddr = ps_phash_pilh_src
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PI_ANOM")) THEN
     iaddr = ps_phash_pi_anom
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PI_TRANS")) THEN
     iaddr = ps_phash_pi_trans
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PI_TRANSCAT")) THEN
     iaddr = ps_phash_pi_transcat
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PMINE")) THEN
     iaddr = ps_phash_pmine
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PMINI")) THEN
     iaddr = ps_phash_pmini
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PMINTH")) THEN
     iaddr = ps_phash_pminth
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."POHME")) THEN
     iaddr = ps_phash_pohme
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."POT_KY_TGLF")) THEN
     iaddr = ps_phash_pot_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PRAD")) THEN
     iaddr = ps_phash_prad
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PRAD_BR")) THEN
     iaddr = ps_phash_prad_br
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PRAD_CY")) THEN
     iaddr = ps_phash_prad_cy
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PRAD_LI")) THEN
     iaddr = ps_phash_prad_li
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PSC_HALO")) THEN
     iaddr = ps_phash_psc_halo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PSC_RECO")) THEN
     iaddr = ps_phash_psc_reco
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PSIPOL")) THEN
     iaddr = ps_phash_psipol
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PSIRZ")) THEN
     iaddr = ps_phash_psirz
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PSMOM_ERRCK")) THEN
     iaddr = ps_phash_psmom_errck
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."PSMOM_NC")) THEN
     iaddr = ps_phash_psmom_nc
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."P_EQ")) THEN
     iaddr = ps_phash_p_eq
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."QBEAMI")) THEN
     iaddr = ps_phash_qbeami
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."QCOMP_E")) THEN
     iaddr = ps_phash_qcomp_e
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."QCOMP_I")) THEN
     iaddr = ps_phash_qcomp_i
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."QIE")) THEN
     iaddr = ps_phash_qie
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."QIONIZ")) THEN
     iaddr = ps_phash_qioniz
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."QQCX")) THEN
     iaddr = ps_phash_qqcx
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."QROT_CONV")) THEN
     iaddr = ps_phash_qrot_conv
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."QROT_DIFF")) THEN
     iaddr = ps_phash_qrot_diff
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."Q_EQ")) THEN
     iaddr = ps_phash_q_eq
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."R0_MOMEQ")) THEN
     iaddr = ps_phash_r0_momeq
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."RATE_SINB0I")) THEN
     iaddr = ps_phash_rate_sinb0i
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."RATE_SINB0X")) THEN
     iaddr = ps_phash_rate_sinb0x
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."RATE_SINB0XS")) THEN
     iaddr = ps_phash_rate_sinb0xs
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."RATE_SINF0I")) THEN
     iaddr = ps_phash_rate_sinf0i
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."RATE_SINF0X")) THEN
     iaddr = ps_phash_rate_sinf0x
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."RATE_SINF0XS")) THEN
     iaddr = ps_phash_rate_sinf0xs
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."RES_SN")) THEN
     iaddr = ps_phash_res_sn
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."RES_TE")) THEN
     iaddr = ps_phash_res_te
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."RES_TI")) THEN
     iaddr = ps_phash_res_ti
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."RES_TQ")) THEN
     iaddr = ps_phash_res_tq
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."RMAJOR_MEAN")) THEN
     iaddr = ps_phash_rmajor_mean
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."RMINOR_MEAN")) THEN
     iaddr = ps_phash_rminor_mean
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."R_GEO")) THEN
     iaddr = ps_phash_r_geo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."R_MIDP_IN")) THEN
     iaddr = ps_phash_r_midp_in
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."R_MIDP_OUT")) THEN
     iaddr = ps_phash_r_midp_out
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."R_SURFMAX")) THEN
     iaddr = ps_phash_r_surfmax
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."R_SURFMIN")) THEN
     iaddr = ps_phash_r_surfmin
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."S0RECO")) THEN
     iaddr = ps_phash_s0reco
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."S0RECO_E")) THEN
     iaddr = ps_phash_s0reco_e
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."S0RECO_RECAP")) THEN
     iaddr = ps_phash_s0reco_recap
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SB0HALO")) THEN
     iaddr = ps_phash_sb0halo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SB0HALO_RECAP")) THEN
     iaddr = ps_phash_sb0halo_recap
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SBEDEP")) THEN
     iaddr = ps_phash_sbedep
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SBEHALO")) THEN
     iaddr = ps_phash_sbehalo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SBSCE")) THEN
     iaddr = ps_phash_sbsce
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SBTHERM")) THEN
     iaddr = ps_phash_sbtherm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SC_ANOM")) THEN
     iaddr = ps_phash_sc_anom
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SFSCE")) THEN
     iaddr = ps_phash_sfsce
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SFTHERM")) THEN
     iaddr = ps_phash_sftherm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SII_XS")) THEN
     iaddr = ps_phash_sii_xs
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SN_TRANS")) THEN
     iaddr = ps_phash_sn_trans
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SN_TRANSCAT")) THEN
     iaddr = ps_phash_sn_transcat
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SPROF0")) THEN
     iaddr = ps_phash_sprof0
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SPROF0E")) THEN
     iaddr = ps_phash_sprof0e
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SPSCE")) THEN
     iaddr = ps_phash_spsce
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SQUARELO")) THEN
     iaddr = ps_phash_squarelo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SQUAREUO")) THEN
     iaddr = ps_phash_squareuo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."SURF")) THEN
     iaddr = ps_phash_surf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."T0CX")) THEN
     iaddr = ps_phash_t0cx
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."T0SC0")) THEN
     iaddr = ps_phash_t0sc0
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."T0_HALO")) THEN
     iaddr = ps_phash_t0_halo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."T0_RECO")) THEN
     iaddr = ps_phash_t0_reco
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TEFLUX_KY_TGLF")) THEN
     iaddr = ps_phash_teflux_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TE_KY_TGLF")) THEN
     iaddr = ps_phash_te_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TFRIP2_LOG")) THEN
     iaddr = ps_phash_tfrip2_log
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TFRIP_LOG")) THEN
     iaddr = ps_phash_tfrip_log
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TI")) THEN
     iaddr = ps_phash_ti
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TIFLUX_KY_TGLF")) THEN
     iaddr = ps_phash_tiflux_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TI_KY_TGLF")) THEN
     iaddr = ps_phash_ti_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TMODEL")) THEN
     iaddr = ps_phash_tmodel
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQ0_RECO")) THEN
     iaddr = ps_phash_tq0_reco
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQB0_HALO")) THEN
     iaddr = ps_phash_tqb0_halo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQBE")) THEN
     iaddr = ps_phash_tqbe
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQBI")) THEN
     iaddr = ps_phash_tqbi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQBJXB")) THEN
     iaddr = ps_phash_tqbjxb
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQBTH")) THEN
     iaddr = ps_phash_tqbth
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQCX_HALO")) THEN
     iaddr = ps_phash_tqcx_halo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQCX_RECO")) THEN
     iaddr = ps_phash_tqcx_reco
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQIONIZ")) THEN
     iaddr = ps_phash_tqioniz
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQQCX")) THEN
     iaddr = ps_phash_tqqcx
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQSC_HALO")) THEN
     iaddr = ps_phash_tqsc_halo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQSC_RECO")) THEN
     iaddr = ps_phash_tqsc_reco
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQ_ANOM")) THEN
     iaddr = ps_phash_tq_anom
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQ_TRANS")) THEN
     iaddr = ps_phash_tq_trans
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TQ_TRANSCAT")) THEN
     iaddr = ps_phash_tq_transcat
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TRIANG")) THEN
     iaddr = ps_phash_triang
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TRIANGL")) THEN
     iaddr = ps_phash_triangl
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TRIANGU")) THEN
     iaddr = ps_phash_triangu
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TRIANG_MILLER_L")) THEN
     iaddr = ps_phash_triang_miller_l
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TRIANG_MILLER_U")) THEN
     iaddr = ps_phash_triang_miller_u
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TS")) THEN
     iaddr = ps_phash_ts
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TZFLUX_KY_TGLF")) THEN
     iaddr = ps_phash_tzflux_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."TZ_KY_TGLF")) THEN
     iaddr = ps_phash_tz_ky_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."UPWIND_PFRAC_NS")) THEN
     iaddr = ps_phash_upwind_pfrac_ns
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."UPWIND_PFRAC_OMEGA")) THEN
     iaddr = ps_phash_upwind_pfrac_omega
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."UPWIND_PFRAC_TE")) THEN
     iaddr = ps_phash_upwind_pfrac_te
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."UPWIND_PFRAC_TI")) THEN
     iaddr = ps_phash_upwind_pfrac_ti
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VEE_TRANS")) THEN
     iaddr = ps_phash_vee_trans
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VEE_TRANSCAT")) THEN
     iaddr = ps_phash_vee_transcat
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELB_FUSI")) THEN
     iaddr = ps_phash_velb_fusi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELB_NBI")) THEN
     iaddr = ps_phash_velb_nbi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELB_RFMI")) THEN
     iaddr = ps_phash_velb_rfmi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELNE_NCLASS")) THEN
     iaddr = ps_phash_velne_nclass
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELNH_NCLASS")) THEN
     iaddr = ps_phash_velnh_nclass
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELNI_DRBM")) THEN
     iaddr = ps_phash_velni_drbm
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELNS_ANOM")) THEN
     iaddr = ps_phash_velns_anom
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELNS_GLF")) THEN
     iaddr = ps_phash_velns_glf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELNS_GTCNEO")) THEN
     iaddr = ps_phash_velns_gtcneo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELNS_MMM71")) THEN
     iaddr = ps_phash_velns_mmm71
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELNS_NCLASS")) THEN
     iaddr = ps_phash_velns_nclass
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELNS_NEO")) THEN
     iaddr = ps_phash_velns_neo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELNS_TGLF")) THEN
     iaddr = ps_phash_velns_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELNX_NCLASS")) THEN
     iaddr = ps_phash_velnx_nclass
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELPPHI_ANOM")) THEN
     iaddr = ps_phash_velpphi_anom
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELPPHI_GLF")) THEN
     iaddr = ps_phash_velpphi_glf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELPPHI_GTCNEO")) THEN
     iaddr = ps_phash_velpphi_gtcneo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELPPHI_MMM71")) THEN
     iaddr = ps_phash_velpphi_mmm71
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELPPHI_NEO")) THEN
     iaddr = ps_phash_velpphi_neo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELPPHI_TGLF")) THEN
     iaddr = ps_phash_velpphi_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTE_ANOM")) THEN
     iaddr = ps_phash_velte_anom
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTE_GLF")) THEN
     iaddr = ps_phash_velte_glf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTE_GTCNEO")) THEN
     iaddr = ps_phash_velte_gtcneo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTE_MMM71")) THEN
     iaddr = ps_phash_velte_mmm71
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTE_NCLASS")) THEN
     iaddr = ps_phash_velte_nclass
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTE_NEO")) THEN
     iaddr = ps_phash_velte_neo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTE_PALEO")) THEN
     iaddr = ps_phash_velte_paleo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTE_TGLF")) THEN
     iaddr = ps_phash_velte_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTI_ANOM")) THEN
     iaddr = ps_phash_velti_anom
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTI_GLF")) THEN
     iaddr = ps_phash_velti_glf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTI_GTCNEO")) THEN
     iaddr = ps_phash_velti_gtcneo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTI_MMM71")) THEN
     iaddr = ps_phash_velti_mmm71
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTI_NCLASS")) THEN
     iaddr = ps_phash_velti_nclass
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTI_NEO")) THEN
     iaddr = ps_phash_velti_neo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VELTI_TGLF")) THEN
     iaddr = ps_phash_velti_tglf
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VIE_TRANS")) THEN
     iaddr = ps_phash_vie_trans
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VIE_TRANSCAT")) THEN
     iaddr = ps_phash_vie_transcat
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VMO_TRANS")) THEN
     iaddr = ps_phash_vmo_trans
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VMO_TRANSCAT")) THEN
     iaddr = ps_phash_vmo_transcat
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VN_TRANS")) THEN
     iaddr = ps_phash_vn_trans
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VN_TRANSCAT")) THEN
     iaddr = ps_phash_vn_transcat
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VOL")) THEN
     iaddr = ps_phash_vol
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VPOL_INMP")) THEN
     iaddr = ps_phash_vpol_inmp
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VPOL_OMP")) THEN
     iaddr = ps_phash_vpol_omp
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VTOR_INMP")) THEN
     iaddr = ps_phash_vtor_inmp
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."VTOR_OMP")) THEN
     iaddr = ps_phash_vtor_omp
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."V_LOOP")) THEN
     iaddr = ps_phash_v_loop
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."V_PARS")) THEN
     iaddr = ps_phash_v_pars
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."V_PERS")) THEN
     iaddr = ps_phash_v_pers
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."XRJCOS_MOMEQ")) THEN
     iaddr = ps_phash_xrjcos_momeq
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."XRJSIN_MOMEQ")) THEN
     iaddr = ps_phash_xrjsin_momeq
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."XZJCOS_MOMEQ")) THEN
     iaddr = ps_phash_xzjcos_momeq
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."XZJSIN_MOMEQ")) THEN
     iaddr = ps_phash_xzjsin_momeq
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."Z0_MOMEQ")) THEN
     iaddr = ps_phash_z0_momeq
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."ZEFF")) THEN
     iaddr = ps_phash_zeff
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."ZEFF_FI")) THEN
     iaddr = ps_phash_zeff_fi
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."ZEFF_TH")) THEN
     iaddr = ps_phash_zeff_th
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."Z_GEO")) THEN
     iaddr = ps_phash_z_geo
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."Z_MIDP")) THEN
     iaddr = ps_phash_z_midp
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."Z_SURFMAX")) THEN
     iaddr = ps_phash_z_surfmax
     RETURN
  ENDIF

  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."Z_SURFMIN")) THEN
     iaddr = ps_phash_z_surfmin
     RETURN
  ENDIF

  WRITE(iout,*) ' %plasma_state_hash_addr: no address found for:'
  WRITE(iout,*) '  htype = "'//trim(htypu)//'"; hname = "'//trim(hnamu)//'".'

  iaddr = 0

END SUBROUTINE plasma_state_hash_addr
