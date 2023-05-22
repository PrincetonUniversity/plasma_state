! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_report_exec(ss,cclist,isect,ireverse,iout,ierr)

  ! generate report on Plasma State quantities that have been modified,
  ! based on hash code analysis.

  ! report for elements in state (ss)

  ! covering sections per (isect) and components per (cclist)

  ! if ireverse.eq.1 write list of items NOT updated;
  !    otherwise write list of updated items.

  ! write ascii report to fortran logical unit (iout)

  ! return status code (ierr)

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state containing data to write
  INTEGER, INTENT(IN) :: cclist(ps_ccount) ! component-by-component control

  INTEGER, INTENT(IN) :: isect  ! section code
    ! =1: State_Profiles section ONLY
    ! =2: State_Profiles & State_Data sections
    ! =3: All sections

  INTEGER, INTENT(IN) :: ireverse  ! =1 to list NOT-updated quantities
  ! otherwise, list updated quantities.

  INTEGER, INTENT(IN) :: iout   ! I/O unit for output
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-----------------------
  LOGICAL :: plasma_state_hash_match,ichange

  INTEGER :: jout,jsect,iaddr,iertmp
  CHARACTER*20 :: hdr

  LOGICAL :: iwrite

  !-----------------------

  jout = SWIM_err_out_file
  jsect = max(3,min(1,isect))

  call plasma_state_hash_code(ss,cclist,ierr)
  if(ierr.NE.0) then
     write(iout,*) ' ?ps_update_report_exec: unexpected hash code error! '
     write(jout,*) ' ?ps_update_report_exec: unexpected hash code error! '
     return
  endif

  if(ireverse.EQ.1) then
     hdr = "NOT updated:"
  else
     hdr = "updated:"
  endif


  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_alla_index
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  alla_index (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_alla
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ALLA_name (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_alla
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ALLA_type (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_all_index
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  all_index (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_all
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ALL_name (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_all
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ALL_type (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_amaxlh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  amaxlh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_aminlh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  aminlh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Ang_Href_coil
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Ang_Href_coil (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Ang_Vref_coil
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Ang_Vref_coil (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ANOM_Code_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ANOM_Code_Info (ANOM, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_anom_evar
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  anom_evar (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ant_model
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ant_model (IC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ap2_halfheight
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ap2_halfheight (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ap2_halfwidth
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ap2_halfwidth (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ap2_horiz_offset
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ap2_horiz_offset (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ap2_vert_offset
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ap2_vert_offset (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_apolmaxlh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  apolmaxlh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_apolminlh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  apolminlh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ap_halfheight
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ap_halfheight (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ap_halfwidth
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ap_halfwidth (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ap_horiz_offset
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ap_horiz_offset (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ap_vert_offset
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ap_vert_offset (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_area
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  area (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_atormaxlh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  atormaxlh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_atorminlh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  atorminlh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nballoon
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Balloon_code (LMHD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Balloon_mapper
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Balloon_mapper (LMHD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_Balloon_stable
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Balloon_stable (LMHD, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_Balloon_status
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Balloon_status (LMHD, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_balstab
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  balstab (LMHD, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_beam_type
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  beam_type (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_bidiff_bpass_co
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  bidiff_bpass_co (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_bidiff_bpass_ctr
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  bidiff_bpass_ctr (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_bidiff_btrap
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  bidiff_btrap (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_bidiff_dpass_co
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  bidiff_dpass_co (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_bidiff_dpass_ctr
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  bidiff_dpass_ctr (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_bidiff_dtrap
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  bidiff_dtrap (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_BphiRZ
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  BphiRZ (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_BRRZ
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  BRRZ (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_BZRZ
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  BZRZ (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_B_axis
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  B_axis (EQ, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_B_axis_vac
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  B_axis_vac (EQ, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_b_halfHeight
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  b_halfHeight (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_b_halfwidth
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  b_halfwidth (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_b_Hdivergence
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  b_Hdivergence (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_b_Hfocal_length
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  b_Hfocal_length (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_B_max_lcfs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  B_max_lcfs (EQ, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_B_min_lcfs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  B_min_lcfs (EQ, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_B_surfMax
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  B_surfMax (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_B_surfMin
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  B_surfMin (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_b_Vdivergence
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  b_Vdivergence (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_b_Vfocal_length
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  b_Vfocal_length (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_cdicrf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  cdicrf (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_cdicrf_nphi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  cdicrf_nphi (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_anom (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_drbm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_drbm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_etg
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_etg (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_glf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_glf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_gtcneo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_gtcneo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_mmm71
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_mmm71 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_mtm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_mtm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_nclass
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_nclass (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_neo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_neo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_ntm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_ntm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_paleo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_paleo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_trans
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_trans (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_transcat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_transcat (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chie_w19
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chie_w19 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chii_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chii_anom (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chii_drbm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chii_drbm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chii_glf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chii_glf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chii_gtcneo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chii_gtcneo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chii_mmm71
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chii_mmm71 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chii_nclass
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chii_nclass (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chii_neo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chii_neo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chii_ntm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chii_ntm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chii_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chii_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chii_trans
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chii_trans (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chii_transcat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chii_transcat (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chii_w19
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chii_w19 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chimo_trans
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chimo_trans (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chimo_transcat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chimo_transcat (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chiphi_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chiphi_anom (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chiphi_drbm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chiphi_drbm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chiphi_glf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chiphi_glf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chiphi_gtcneo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chiphi_gtcneo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chiphi_mmm71
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chiphi_mmm71 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chiphi_neo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chiphi_neo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chiphi_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chiphi_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chiphi_w19
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chiphi_w19 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_chitheta_mmm71
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  chitheta_mmm71 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_ncircuits
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  circuit_name (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_coil_apt
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  coil_apt (EQ, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_coil_in_circuit
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  coil_in_circuit (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_ncoils
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  coil_name (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_coil_resispt
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  coil_resispt (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_curbeam
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  curbeam (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_curech
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  curech (EC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_curech_src
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  curech_src (EC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_curfusn
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  curfusn (FUS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_curich
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  curich (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_curlh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  curlh (LH, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_curlh_src
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  curlh_src (LH, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_curmino
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  curmino (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_curr_bootstrap
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  curr_bootstrap (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_curr_ohmic
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  curr_ohmic (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_curt
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  curt (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_CUR_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  CUR_Data_Info (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_RUNAWAY).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_cur_rw
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  cur_rw (RUNAWAY, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_d2R_geo_dRHOdTH
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  d2R_geo_dRHOdTH (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_d2Z_geo_dRHOdTH
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  d2Z_geo_dRHOdTH (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_db_mtm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  db_mtm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_RUNAWAY).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dcur_rw_dVloop
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dcur_rw_dVloop (RUNAWAY, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_ndelta_w
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  DeltaW_code (LMHD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_DeltaW_mapper
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  DeltaW_mapper (LMHD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_DeltaW_stable
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  DeltaW_stable (LMHD, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_DeltaW_status
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  DeltaW_status (LMHD, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difb_fusi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difb_fusi (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difb_nbi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difb_nbi (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difb_rfmi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difb_rfmi (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_diff_trans
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  diff_trans (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_diff_transcat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  diff_transcat (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difne_nclass
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difne_nclass (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difnh_nclass
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difnh_nclass (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difns_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difns_anom (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difns_drbm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difns_drbm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difns_glf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difns_glf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difns_gtcneo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difns_gtcneo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difns_mmm71
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difns_mmm71 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difns_nclass
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difns_nclass (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difns_neo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difns_neo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difns_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difns_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difns_w19
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difns_w19 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difnx_nclass
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difnx_nclass (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_difnz_mmm71
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  difnz_mmm71 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_dist_fun
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dist_fun (IC, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_dn0out
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dn0out (GAS, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dQedGrdte_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dQedGrdte_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dQedGrdti_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dQedGrdti_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dQidGrdte_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dQidGrdte_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dQidGrdti_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dQidGrdti_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dR0_momeq_dRHO
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dR0_momeq_dRHO (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dR_geo_dRHO
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dR_geo_dRHO (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dR_geo_dTH
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dR_geo_dTH (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dxRjcos_momeq_dRHO
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dxRjcos_momeq_dRHO (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dxRjsin_momeq_dRHO
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dxRjsin_momeq_dRHO (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dxZjcos_momeq_dRHO
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dxZjcos_momeq_dRHO (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dxZjsin_momeq_dRHO
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dxZjsin_momeq_dRHO (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_dx_fshield
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dx_fshield (IC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dZ0_momeq_dRHO
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dZ0_momeq_dRHO (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dZ_geo_dRHO
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dZ_geo_dRHO (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_dZ_geo_dTH
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  dZ_geo_dTH (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_D_einj_standard
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  D_einj_standard (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_D_ffull_standard
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  D_ffull_standard (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_D_fhalf_standard
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  D_fhalf_standard (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_e0_av
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  e0_av (GAS, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_necrf_src
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ecrf_src_name (EC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_EC_Beam_Elongation
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  EC_Beam_Elongation (EC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_EC_Code_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  EC_Code_Info (EC, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_EC_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  EC_Data_Info (EC, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_EC_Half_Power_Angle
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  EC_Half_Power_Angle (EC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_EC_Omode_Fraction
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  EC_Omode_Fraction (EC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_EC_phi_aim
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  EC_phi_aim (EC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_EC_PRINC_CURV_HOR
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  EC_PRINC_CURV_HOR (EC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_EC_PRINC_CURV_VERT
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  EC_PRINC_CURV_VERT (EC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_EC_PRINC_WIDTH_HOR
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  EC_PRINC_WIDTH_HOR (EC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_EC_PRINC_WIDTH_VERT
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  EC_PRINC_WIDTH_VERT (EC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_EC_theta_aim
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  EC_theta_aim (EC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_ee_mobility_factor
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ee_mobility_factor (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Einj_max
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Einj_max (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Einj_min
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Einj_min (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_eldot
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  eldot (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_elong
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  elong (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_eperp_beami
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  eperp_beami (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_eperp_fusi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  eperp_fusi (FUS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_eperp_mini
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  eperp_mini (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_RUNAWAY).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_eperp_rw
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  eperp_rw (RUNAWAY, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_epll_beami
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  epll_beami (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_epll_fusi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  epll_fusi (FUS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_epll_mini
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  epll_mini (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_RUNAWAY).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_epll_rw
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  epll_rw (RUNAWAY, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Epot
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Epot (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_eqdsk_file
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  eqdsk_file (EQ, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_neqmom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  eqmom_num (EQ, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_EQ_Code_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  EQ_Code_Info (EQ, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_EQ_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  EQ_Data_Info (EQ, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_eta_parallel
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  eta_parallel (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nefi_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  E_anom (ANOM, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nefi_anom2
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  E_anom2 (ANOM, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_fidiff_bpass_co
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  fidiff_bpass_co (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_fidiff_bpass_ctr
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  fidiff_bpass_ctr (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_fidiff_btrap
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  fidiff_btrap (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_fidiff_dpass_co
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  fidiff_dpass_co (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_fidiff_dpass_ctr
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  fidiff_dpass_ctr (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_fidiff_dtrap
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  fidiff_dtrap (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Fi_depletion
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Fi_depletion (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_fracmin
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  fracmin (IC, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_fracpslh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  fracpslh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_frac_full
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  frac_full (NBI, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_frac_half
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  frac_half (NBI, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_freq_drbm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  freq_drbm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_freq_ec
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  freq_ec (EC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_freq_etg
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  freq_etg (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_freq_glf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  freq_glf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_freq_ic
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  freq_ic (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_freq_lh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  freq_lh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_freq_max_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  freq_max_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_freq_mmm71
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  freq_mmm71 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_freq_mtm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  freq_mtm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_freq_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  freq_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_FUS_Code_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  FUS_Code_Info (FUS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_gamma_lmhd
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gamma_lmhd (LMHD, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_gamma_nc
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gamma_nc (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_gas_atom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gas_atom (GAS, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_GAS_Code_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  GAS_Code_Info (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_gb1
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gb1 (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_gb2
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gb2 (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_gb2i
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gb2i (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_gbr2
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gbr2 (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_geometry
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  geometry (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Global_label
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Global_label (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_gncfb2h
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gncfb2h (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_gncfh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gncfh (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_gr1
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gr1 (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_gr2
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gr2 (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_gr2i
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gr2i (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_gr2rho2
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gr2rho2 (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_gr3i
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gr3i (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_grho1
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  grho1 (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_grho2
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  grho2 (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_grho2b2i
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  grho2b2i (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_grho2r2i
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  grho2r2i (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_grho2r3i
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  grho2r3i (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_gri
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gri (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_grirhoi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  grirhoi (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_growthrate_drbm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  growthrate_drbm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_growthrate_etg
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  growthrate_etg (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_growthrate_glf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  growthrate_glf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_growthrate_max_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  growthrate_max_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_growthrate_mmm71
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  growthrate_mmm71 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_growthrate_mtm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  growthrate_mtm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_growthrate_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  growthrate_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_growthrate_vpar
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  growthrate_vpar (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_ngsc0
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  gs_name (GAS, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_g_eq
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  g_eq (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_hegrilh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  hegrilh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Hsize_coil
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Hsize_coil (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_H_einj_standard
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  H_einj_standard (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_H_ffull_standard
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  H_ffull_standard (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_H_fhalf_standard
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  H_fhalf_standard (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_icircuit_coil
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  icircuit_coil (EQ, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nicrf_src
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  icrf_src_name (IC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_IC_Code_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  IC_Code_Info (IC, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_IC_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  IC_Data_Info (IC, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Imag_ant_coef
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Imag_ant_coef (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_interchange_R
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  interchange_R (ANOM, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_iota
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  iota (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_isThermal
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  isThermal (IC, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_is_recycling
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  is_recycling (GAS, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_i_npoloidal
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  i_npoloidal (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_jdotb
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  jdotb (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_kccw_Bphi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  kccw_Bphi (EQ, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_kccw_Jphi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  kccw_Jphi (EQ, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_kdens_rfmin
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  kdens_rfmin (IC, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_kvolt_nbi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  kvolt_nbi (NBI, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_kyrhos_drbm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  kyrhos_drbm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_kyrhos_etg
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  kyrhos_etg (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_kyrhos_mtm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  kyrhos_mtm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_kyspectrum_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  kyspectrum_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Lbscap
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Lbscap (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Lbscap2
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Lbscap2 (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Lbsctan
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Lbsctan (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nlhrf_src
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  lhrf_src_name (LH, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_LH_Code_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  LH_Code_Info (LH, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_LH_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  LH_Data_Info (LH, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_LMHD_eq_code
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  LMHD_eq_code (LMHD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_LMHD_status
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  LMHD_status (LMHD, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_lock_machine_descr
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  lock_machine_descr (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_lock_shot_config
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  lock_shot_config (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_lock_sim_init
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  lock_sim_init (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Lpol
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Lpol (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_Mercier_stable
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Mercier_stable (LMHD, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_MHD_eq_status
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  MHD_eq_status (LMHD, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_all
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  m_ALL (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_alla
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  m_ALLA (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_rfmin
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  m_RFMIN (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_th
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  m_S (PLASMA, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_tha
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  m_SA (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_fusion
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  m_SFUS (FUS, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_gas
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  m_SGAS (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_imp0
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  m_SIMP0 (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_impi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  m_SIMPI (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_beam
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  m_SNBI (NBI, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_n0norm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  n0norm (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_n0_halo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  n0_halo (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_n0_reco
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  n0_reco (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_nbap2_shape
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nbap2_shape (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_nbap_shape
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nbap_shape (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_nbeami
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nbeami (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_nbeami_bdy
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nbeami_bdy (NBI, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_nbion
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nbion (NBI, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_nbion_trace
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nbion_trace (NBI, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_NBI_Code_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  NBI_Code_Info (NBI, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_NBI_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  NBI_Data_Info (NBI, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nbeam
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nbi_src_name (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_nbshape
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nbshape (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_nebar
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nebar (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_neflux_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  neflux_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nnewcomb
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Newcomb_code (LMHD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Newcomb_mapper
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Newcomb_mapper (LMHD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_Newcomb_stable
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Newcomb_stable (LMHD, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_Newcomb_status
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Newcomb_status (LMHD, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_ne_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ne_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_nfusi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nfusi (FUS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_nfusi_bdy
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nfusi_bdy (IC, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_ngradb2_av
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ngradb2_av (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_ni
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ni (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_niflux_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  niflux_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_ni_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ni_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_nmdifb
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nmdifb (ANOM, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_nmini
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nmini (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_nmini_bdy
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nmini_bdy (FUS, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_nmodel
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nmodel (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_nmodel_bdy
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nmodel_bdy (ANOM, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_nmom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nmom (EQ, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_nnkparlh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nnkparlh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_nnkpollh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nnkpollh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_nnktorlh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nnktorlh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_nphi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nphi (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_RUNAWAY).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_nrw
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nrw (RUNAWAY, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_nrz_antgeo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nrz_antgeo (IC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_ns
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ns (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_nspectrumlh_src
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nspectrumlh_src (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_ns_bdy
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ns_bdy (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ns_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ns_Data_Info (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_ns_is_input
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ns_is_input (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_RIPPLE).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ntf_coil2
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ntf_coil2 (RIPPLE, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_RIPPLE).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ntf_coils
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ntf_coils (RIPPLE, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_nthinlh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nthinlh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Nturns
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Nturns (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_num_nphi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  num_nphi (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_num_nphi_vac
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  num_nphi_vac (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_nzflux_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nzflux_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_nz_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  nz_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_n_straps
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  n_straps (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_omeg0cx
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  omeg0cx (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_omeg0sc0
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  omeg0sc0 (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_omeg0_halo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  omeg0_halo (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_omeg0_reco
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  omeg0_reco (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_omegat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  omegat (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_omegat_bdy
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  omegat_bdy (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_omega_exb
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  omega_exb (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_p0_reco
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  p0_reco (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pb0_halo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pb0_halo (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pbe
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pbe (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pbi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pbi (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pbth
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pbth (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pcx_halo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pcx_halo (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pcx_reco
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pcx_reco (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_peech
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  peech (EC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_peech_src
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  peech_src (EC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pelh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pelh (LH, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pelh_src
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pelh_src (LH, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PEL).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_PEL_Code_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  PEL_Code_Info (PEL, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PEL).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_PEL_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  PEL_Data_Info (PEL, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pe_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pe_anom (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pe_trans
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pe_trans (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pe_transcat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pe_transcat (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pfuse
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pfuse (FUS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pfusi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pfusi (FUS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pfusth
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pfusth (FUS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Phibsc
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Phibsc (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_phiflux_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  phiflux_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_phit
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  phit (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Phi_EC_launch
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Phi_EC_launch (EC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_picrf_abs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  picrf_abs (IC, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_picrf_ext
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  picrf_ext (IC, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_picrf_nphi_srcs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  picrf_nphi_srcs (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_picrf_srcs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  picrf_srcs (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_picrf_totals
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  picrf_totals (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_picth
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  picth (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pilh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pilh (LH, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pilh_src
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pilh_src (LH, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pi_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pi_anom (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pi_trans
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pi_trans (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pi_transcat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pi_transcat (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_PLASMA_Code_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  PLASMA_Code_Info (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pmine
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pmine (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pmini
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pmini (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pminth
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pminth (IC, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pohme
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pohme (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_pot_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  pot_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_power_ec
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  power_ec (EC, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_power_ic
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  power_ic (IC, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_power_lh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  power_lh (LH, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_power_nbi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  power_nbi (NBI, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_power_nbi_trace
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  power_nbi_trace (NBI, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_RAD).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_prad
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  prad (RAD, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_RAD).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_prad_br
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  prad_br (RAD, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_RAD).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_prad_cy
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  prad_cy (RAD, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_RAD).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_prad_li
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  prad_li (RAD, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_psc_halo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  psc_halo (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_psc_reco
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  psc_reco (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_psipol
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  psipol (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_PsiRZ
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  PsiRZ (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_Psi_to_machine_axis
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Psi_to_machine_axis (EQ, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_psmom_errck
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  psmom_errck (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_psmom_nc
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  psmom_nc (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_npsmom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  psmom_num (EQ, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_P_eq
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  P_eq (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_all
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qatom_ALL (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_alla
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qatom_ALLA (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_rfmin
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qatom_RFMIN (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_th
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qatom_S (PLASMA, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_tha
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qatom_SA (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_fusion
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qatom_SFUS (FUS, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_gas
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qatom_SGAS (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_imp0
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qatom_SIMP0 (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_impi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qatom_SIMPI (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_beam
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qatom_SNBI (NBI, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_qbeami
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qbeami (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_qcomp_e
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qcomp_e (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_qcomp_i
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qcomp_i (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_qie
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qie (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_qioniz
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qioniz (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_ql_operator
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ql_operator (IC, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_qqcx
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qqcx (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_qrot_conv
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qrot_conv (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_qrot_diff
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  qrot_diff (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_all
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  q_ALL (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_alla
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  q_ALLA (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_q_eq
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  q_eq (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_rfmin
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  q_RFMIN (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_th
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  q_S (PLASMA, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_tha
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  q_SA (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_fusion
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  q_SFUS (FUS, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_gas
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  q_SGAS (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_imp0
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  q_SIMP0 (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_impi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  q_SIMPI (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_beam
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  q_SNBI (NBI, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_R0_momeq
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R0_momeq (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_RAD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_RAD_Code_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  RAD_Code_Info (RAD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_RAD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_RAD_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  RAD_Data_Info (RAD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_rate_sinb0i
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rate_sinb0i (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_rate_sinb0x
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rate_sinb0x (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_rate_sinb0xs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rate_sinb0xs (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_rate_sinf0i
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rate_sinf0i (FUS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_rate_sinf0x
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rate_sinf0x (FUS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_rate_sinf0xs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rate_sinf0xs (FUS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Real_ant_coef
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Real_ant_coef (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_res_sn
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  res_sn (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_res_te
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  res_te (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_res_ti
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  res_ti (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_res_tq
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  res_tq (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_rfmin
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  RFMIN_name (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_rfmin_to_all
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rfmin_to_all (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_rfmin_to_alla
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rfmin_to_alla (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_rfmin
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  RFMIN_type (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_rhopsi0lh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rhopsi0lh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_anom (ANOM, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_anom2
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_anom2 (ANOM, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_anomth
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_anomth (ANOM, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_rho_bdy_ns
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_bdy_ns (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_rho_bdy_omegat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_bdy_omegat (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_rho_bdy_Te
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_bdy_Te (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_rho_bdy_Ti
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_bdy_Ti (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_ecrf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_ecrf (EC, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_eq
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_eq (EQ, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_eq_geo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_eq_geo (EQ, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_fus
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_fus (FUS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_gas
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_gas (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_icrf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_icrf (IC, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_lhrf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_lhrf (LH, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_lmhd
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_lmhd (LMHD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_RAD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_mist
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_mist (RAD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_nbi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_nbi (NBI, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PEL).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_pel
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_pel (PEL, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_RAD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_rad
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_rad (RAD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_RUNAWAY).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nrho_rw
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rho_rw (RUNAWAY, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_rlim
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rlim (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Rloc_coil
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Rloc_coil (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Rmajor_mean
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Rmajor_mean (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_rMinor_mean
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  rMinor_mean (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_RUNAWAY).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_RUNAWAY_Code_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  RUNAWAY_Code_Info (RUNAWAY, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_RunID
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  RunID (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_R_ant
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_ant (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_R_antgeo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_antgeo (IC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_R_axis
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_axis (EQ, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_R_EC_launch
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_EC_launch (EC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_R_geo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_geo (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nr
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_grid (EQ, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_R_max_box
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_max_box (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_R_max_lcfs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_max_lcfs (EQ, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_R_midp_in
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_midp_in (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_R_midp_out
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_midp_out (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_R_min_box
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_min_box (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_R_min_lcfs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_min_lcfs (EQ, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_RIPPLE).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nr_rip
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_ripple (RIPPLE, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_R_surfMax
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_surfMax (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_R_surfMin
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  R_surfMin (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_s0reco
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  s0reco (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_s0reco_e
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  s0reco_e (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_s0reco_recap
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  s0reco_recap (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_sa_index
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sa_index (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_tha
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  SA_name (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_tha
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  SA_type (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sb0halo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sb0halo (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sb0halo_recap
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sb0halo_recap (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sbedep
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sbedep (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sbehalo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sbehalo (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sbsce
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sbsce (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sbtherm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sbtherm (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_sc0
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sc0 (GAS, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_sc0_to_sgas
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sc0_to_sgas (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sc_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sc_anom (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sfsce
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sfsce (FUS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sftherm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sftherm (FUS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_fusion
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  SFUS_name (FUS, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_sfus_to_all
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sfus_to_all (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_sfus_to_alla
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sfus_to_alla (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_FUS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_fusion
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  SFUS_type (FUS, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_gas
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  SGAS_name (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_sgas_to_s
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sgas_to_s (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_gas
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  SGAS_type (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_shot_number
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  shot_number (PLASMA, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_RAD).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sii_xs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sii_xs (RAD, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_imp0
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  SIMP0_name (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_simp0_to_s
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  simp0_to_s (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_imp0
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  SIMP0_type (GAS, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_impi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  SIMPI_name (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_simpi_to_s
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  simpi_to_s (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_impi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  SIMPI_type (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_beam
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  SNBI_name (NBI, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_snbi_to_all
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  snbi_to_all (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_snbi_to_alla
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  snbi_to_alla (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_beam
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  SNBI_type (NBI, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sn_trans
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sn_trans (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sn_transcat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sn_transcat (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sprof0
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sprof0 (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_sprof0e
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sprof0e (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PEL).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_spsce
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  spsce (PEL, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_squareLO
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  squareLO (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_squareUO
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  squareUO (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_sRtcen
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  sRtcen (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_surf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  surf (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_th
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  S_name (PLASMA, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_s_to_sa
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  s_to_sa (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nspec_th
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  S_type (PLASMA, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_t0
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  t0 (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_T0cx
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  T0cx (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_T0sc0
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  T0sc0 (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_T0_halo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  T0_halo (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_T0_reco
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  T0_reco (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_t1
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  t1 (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_teflux_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  teflux_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_Te_bdy
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Te_bdy (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_te_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  te_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_tfinal
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tfinal (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_RIPPLE).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tfrip2_log
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tfrip2_log (RIPPLE, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_RIPPLE).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tfrip_log
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tfrip_log (RIPPLE, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_TF_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  TF_Data_Info (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LH).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_thegrilh
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  thegrilh (LH, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nth_eq
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  th_eq (EQ, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Ti
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Ti (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tiflux_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tiflux_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_tinit
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tinit (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_Ti_bdy
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Ti_bdy (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_ti_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ti_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tmodel
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tmodel (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_tmodel_bdy
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tmodel_bdy (ANOM, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_tokamak_id
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tokamak_id (PLASMA, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_Tornum_balcrit
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Tornum_balcrit (LMHD, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_mmodes
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Tor_mode_label (LMHD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_LMHD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Tor_mode_No
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Tor_mode_No (LMHD, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tq0_reco
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tq0_reco (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tqb0_halo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tqb0_halo (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tqbe
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tqbe (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tqbi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tqbi (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tqbjxb
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tqbjxb (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tqbth
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tqbth (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tqcx_halo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tqcx_halo (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tqcx_reco
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tqcx_reco (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tqioniz
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tqioniz (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tqqcx
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tqqcx (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tqsc_halo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tqsc_halo (NBI, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tqsc_reco
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tqsc_reco (GAS, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tq_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tq_anom (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tq_trans
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tq_trans (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tq_transcat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tq_transcat (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_trace_flag
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  trace_flag (NBI, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_ntrcat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  trcat (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_triang
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  triang (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_triangL
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  triangL (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_triangU
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  triangU (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_triang_miller_L
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  triang_miller_L (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_triang_miller_U
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  triang_miller_U (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_trmodels
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  trmodels (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Ts
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Ts (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Ts_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Ts_Data_Info (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_Ts_is_input
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Ts_is_input (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tzflux_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tzflux_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_tz_ky_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  tz_ky_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_T_einj_standard
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  T_einj_standard (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_T_ffull_standard
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  T_ffull_standard (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_T_fhalf_standard
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  T_fhalf_standard (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_upwind_pfrac_ns
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  upwind_pfrac_ns (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_upwind_pfrac_omega
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  upwind_pfrac_omega (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_upwind_pfrac_Te
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  upwind_pfrac_Te (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_upwind_pfrac_Ti
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  upwind_pfrac_Ti (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_vee_trans
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vee_trans (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_vee_transcat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vee_transcat (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velb_fusi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velb_fusi (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velb_nbi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velb_nbi (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velb_rfmi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velb_rfmi (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velne_nclass
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velne_nclass (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velnh_nclass
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velnh_nclass (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velni_drbm
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velni_drbm (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velns_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velns_anom (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velns_glf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velns_glf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velns_gtcneo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velns_gtcneo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velns_mmm71
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velns_mmm71 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velns_nclass
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velns_nclass (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velns_neo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velns_neo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velns_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velns_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velnx_nclass
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velnx_nclass (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velpphi_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velpphi_anom (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velpphi_glf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velpphi_glf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velpphi_gtcneo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velpphi_gtcneo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velpphi_mmm71
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velpphi_mmm71 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velpphi_neo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velpphi_neo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velpphi_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velpphi_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velte_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velte_anom (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velte_glf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velte_glf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velte_gtcneo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velte_gtcneo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velte_mmm71
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velte_mmm71 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velte_nclass
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velte_nclass (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velte_neo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velte_neo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velte_paleo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velte_paleo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velte_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velte_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velti_anom
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velti_anom (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velti_glf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velti_glf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velti_gtcneo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velti_gtcneo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velti_mmm71
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velti_mmm71 (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velti_nclass
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velti_nclass (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velti_neo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velti_neo (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_ANOM).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_velti_tglf
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  velti_tglf (ANOM, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_vie_trans
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vie_trans (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_vie_transcat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vie_transcat (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_vmo_trans
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vmo_trans (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_vmo_transcat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vmo_transcat (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_vn_trans
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vn_trans (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_vn_transcat
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vn_transcat (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_vol
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vol (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_GAS).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_vphi0_av
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vphi0_av (GAS, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_vpol_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vpol_Data_Info (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_vpol_inmp
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vpol_inmp (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_vpol_is_input
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vpol_is_input (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_vpol_omp
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vpol_omp (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_VRep_coil_count
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  VRep_coil_count (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Vrep_coil_spacing
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Vrep_coil_spacing (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Vsize_coil
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Vsize_coil (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_vsur
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vsur (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_VSUR_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  VSUR_Data_Info (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_vtor_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vtor_Data_Info (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_vtor_inmp
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vtor_inmp (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_vtor_is_input
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vtor_is_input (PLASMA, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_vtor_omp
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  vtor_omp (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_V_loop
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  V_loop (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_v_pars
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  v_pars (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_v_pers
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  v_pers (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_wt_nphi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  wt_nphi (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_wt_nphi_abs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  wt_nphi_abs (IC, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_wt_nphi_ext
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  wt_nphi_ext (IC, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_xRjcos_momeq
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  xRjcos_momeq (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_xRjsin_momeq
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  xRjsin_momeq (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_RAD).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_xs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  xs (RAD, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_RAD).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nxs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  xs_name (RAD, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_xZjcos_momeq
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  xZjcos_momeq (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_xZjsin_momeq
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  xZjsin_momeq (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Z0max
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z0max (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Z0_momeq
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z0_momeq (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Zbap
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Zbap (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_NBI).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Zbsc
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Zbsc (NBI, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Zeff
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Zeff (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_ZEFF_Data_Info
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  ZEFF_Data_Info (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Zeff_fi
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Zeff_fi (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Zeff_th
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Zeff_th (PLASMA, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Zimp1
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Zimp1 (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_PLASMA).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Zimp2
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Zimp2 (PLASMA, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_zlim
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  zlim (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Zloc_coil
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Zloc_coil (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Z_ant
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_ant (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Z_antgeo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_antgeo (IC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_Z_axis
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_axis (EQ, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Z_EC_launch
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_EC_launch (EC, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Z_geo
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_geo (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nz
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_grid (EQ, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Z_max_box
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_max_box (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_Z_max_lcfs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_max_lcfs (EQ, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Z_midp
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_midp (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_IC).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Z_mid_ant
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_mid_ant (IC, SHOT_CONFIGURATION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_shash_Z_min_box
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_min_box (EQ, MACHINE_DESCRIPTION)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.2)) THEN
     iaddr = ps_shash_Z_min_lcfs
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_min_lcfs (EQ, STATE_DATA)"
     endif
  endif

  IF((cclist(ps_cnum_RIPPLE).EQ.1).AND.(jsect.GE.3)) THEN
     iaddr = ps_dhash_nz_rip
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_ripple (RIPPLE, SIMULATION_INIT)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Z_surfMax
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_surfMax (EQ, STATE_PROFILES)"
     endif
  endif

  IF((cclist(ps_cnum_EQ).EQ.1).AND.(jsect.GE.1)) THEN
     iaddr = ps_phash_Z_surfMin
     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)
     if(ireverse.NE.1) iwrite = .NOT.iwrite
     if(iwrite) then
        write(iout,"(1x,A)") trim(hdr)//"  Z_surfMin (EQ, STATE_PROFILES)"
     endif
  endif


END SUBROUTINE ps_update_report_exec

