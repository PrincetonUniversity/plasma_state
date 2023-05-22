! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_hash_code(ss,cclist,ierr)

  ! update hash codes for state object ss: write to ss%new_hash_codes(...).
  !   hash codes for grids & profiles that are unallocated: ps_unInit
  !   hash codes for profiles that are all ZERO: -1
  ! otherwise hash codes are a pair positive integers, one evaluated by
  ! walking "forward" through the data; one by walking backwards through
  ! the data.
  !
  ! set cclist(j)=0 to suppress hash code update for all items owned
  !    by component (j) = ps_cnum_<component>.
  ! set cclist(j)=ps_chkProfs=2 to do hash code update only for grid and
  !    profile quantities owned by component (j) = ps_cnum_<component>.

  USE plasma_state_definition_mod
  USE plasma_hash_code
  IMPLICIT NONE

  TYPE (plasma_state) :: ss
  INTEGER, INTENT(IN) :: cclist(ps_ccount)
  INTEGER, INTENT(OUT) :: ierr    ! completion code (0=OK)

  ! if ierr is set, a change detected in a locked section
  ! in this case, messages are written as well.

  !-------------------
  INTEGER :: iout,ilock,iaddr1,iaddr2,isize,r8size,iertot,ii
  INTEGER :: kf1,kf2      ! working hash codes
  LOGICAL :: idiff,plasma_state_hash_match
  logical :: iprofs_only,icheck_any,icheck_all
  !-------------------

  ierr = 0
  iout = SWIM_err_out_file

  iertot = 0

  icheck_any=.FALSE.
  iprofs_only=.TRUE.
  icheck_all=.TRUE.

  ! check whether to skip scalar hash updates

  do ii=1,ps_ccount
     if(cclist(ii).ne.ps_chkAll) icheck_all=.FALSE.

     if(cclist(ii).ne.ps_nocheck) then
        icheck_any=.TRUE.
        if(cclist(ii).eq.ps_chkAll) then
           iprofs_only=.FALSE.
        else if(cclist(ii).eq.ps_chkProfs) then
           continue
        else
           ierr=1
        endif
     endif
  enddo

  if(ierr.eq.1) then
     write(iout,*) ' ?plasma_state_hash_code: illegal element value in'
     write(iout,*) '  CCLIST array: ',cclist
     write(iout,*) '  legal values are: ',ps_nocheck,ps_chkAll,ps_chkProfs
     write(iout,*) '    (ps_nocheck,ps_chkAll,ps_chkProfs).'
     return
  endif

  if(.NOT.icheck_any) return

  if(iprofs_only) go to 1000

  !--------------------------------------------
  ! section hash codes

  !----------------------
  !  section: MACHINE_DESCRIPTION

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Ang_Href_coil
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Ang_Href_coil)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Ang_Href_coil,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Ang_Vref_coil
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Ang_Vref_coil)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Ang_Vref_coil,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ant_model
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ant_model)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%ant_model,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ap2_halfheight
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ap2_halfheight)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%ap2_halfheight,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ap2_halfwidth
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ap2_halfwidth)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%ap2_halfwidth,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ap2_horiz_offset
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ap2_horiz_offset)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%ap2_horiz_offset,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ap2_vert_offset
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ap2_vert_offset)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%ap2_vert_offset,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ap_halfheight
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ap_halfheight)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%ap_halfheight,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ap_halfwidth
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ap_halfwidth)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%ap_halfwidth,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ap_horiz_offset
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ap_horiz_offset)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%ap_horiz_offset,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ap_vert_offset
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ap_vert_offset)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%ap_vert_offset,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_beam_type
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%beam_type)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%beam_type,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_b_halfHeight
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%b_halfHeight)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%b_halfHeight,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_b_halfwidth
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%b_halfwidth)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%b_halfwidth,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_b_Hdivergence
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%b_Hdivergence)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%b_Hdivergence,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_b_Hfocal_length
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%b_Hfocal_length)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%b_Hfocal_length,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_b_Vdivergence
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%b_Vdivergence)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%b_Vdivergence,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_b_Vfocal_length
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%b_Vfocal_length)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%b_Vfocal_length,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_coil_in_circuit
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%coil_in_circuit)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%coil_in_circuit,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_coil_resispt
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%coil_resispt)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%coil_resispt,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_dx_fshield
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dx_fshield)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%dx_fshield,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_D_einj_standard
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%D_einj_standard)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%D_einj_standard,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_D_ffull_standard
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%D_ffull_standard)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%D_ffull_standard,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_D_fhalf_standard
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%D_fhalf_standard)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%D_fhalf_standard,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_EC_Beam_Elongation
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%EC_Beam_Elongation)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%EC_Beam_Elongation,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_EC_Half_Power_Angle
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%EC_Half_Power_Angle)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%EC_Half_Power_Angle,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_EC_PRINC_CURV_HOR
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%EC_PRINC_CURV_HOR)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%EC_PRINC_CURV_HOR,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_EC_PRINC_CURV_VERT
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%EC_PRINC_CURV_VERT)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%EC_PRINC_CURV_VERT,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_EC_PRINC_WIDTH_HOR
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%EC_PRINC_WIDTH_HOR,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_EC_PRINC_WIDTH_VERT
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%EC_PRINC_WIDTH_VERT,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Einj_max
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Einj_max)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Einj_max,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Einj_min
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Einj_min)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Einj_min,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_geometry
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%geometry,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Hsize_coil
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Hsize_coil)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Hsize_coil,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_H_einj_standard
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%H_einj_standard)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%H_einj_standard,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_H_ffull_standard
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%H_ffull_standard)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%H_ffull_standard,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_H_fhalf_standard
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%H_fhalf_standard)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%H_fhalf_standard,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Lbscap
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Lbscap)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Lbscap,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Lbscap2
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Lbscap2)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Lbscap2,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Lbsctan
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Lbsctan)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Lbsctan,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nbap2_shape
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nbap2_shape)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nbap2_shape,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nbap_shape
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nbap_shape)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nbap_shape,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nbshape
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nbshape)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nbshape,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nrz_antgeo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nrz_antgeo)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nrz_antgeo,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_RIPPLE).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ntf_coil2
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%ntf_coil2,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_RIPPLE).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ntf_coils
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%ntf_coils,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Nturns
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Nturns)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Nturns,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Phibsc
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Phibsc)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Phibsc,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Phi_EC_launch
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Phi_EC_launch)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Phi_EC_launch,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_rlim
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rlim)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%rlim,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Rloc_coil
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Rloc_coil)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Rloc_coil,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_R_antgeo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%R_antgeo)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%R_antgeo,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_R_EC_launch
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%R_EC_launch)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%R_EC_launch,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_R_max_box
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%R_max_box,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_R_min_box
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%R_min_box,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_sRtcen
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sRtcen)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%sRtcen,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_tokamak_id
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%tokamak_id,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_T_einj_standard
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%T_einj_standard)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%T_einj_standard,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_T_ffull_standard
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%T_ffull_standard)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%T_ffull_standard,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_T_fhalf_standard
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%T_fhalf_standard)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%T_fhalf_standard,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_VRep_coil_count
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%VRep_coil_count)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%VRep_coil_count,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Vrep_coil_spacing
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Vrep_coil_spacing)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Vrep_coil_spacing,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Vsize_coil
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Vsize_coil)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Vsize_coil,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Zbap
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Zbap)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Zbap,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Zbsc
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Zbsc)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Zbsc,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_zlim
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%zlim)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%zlim,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Zloc_coil
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Zloc_coil)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Zloc_coil,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Z_antgeo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Z_antgeo)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Z_antgeo,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Z_EC_launch
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Z_EC_launch)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Z_EC_launch,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Z_max_box
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%Z_max_box,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Z_min_box
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%Z_min_box,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  IF(icheck_all) THEN
     iaddr1 = ps_shash_machine_description
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     IF(ALLOCATED(ss%ANG_HREF_COIL)) THEN
        call hashCode(ss%ANG_HREF_COIL,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%ANG_VREF_COIL)) THEN
        call hashCode(ss%ANG_VREF_COIL,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%ANT_MODEL)) THEN
        call hashCode(ss%ANT_MODEL,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%AP2_HALFHEIGHT)) THEN
        call hashCode(ss%AP2_HALFHEIGHT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%AP2_HALFWIDTH)) THEN
        call hashCode(ss%AP2_HALFWIDTH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%AP2_HORIZ_OFFSET)) THEN
        call hashCode(ss%AP2_HORIZ_OFFSET,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%AP2_VERT_OFFSET)) THEN
        call hashCode(ss%AP2_VERT_OFFSET,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%AP_HALFHEIGHT)) THEN
        call hashCode(ss%AP_HALFHEIGHT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%AP_HALFWIDTH)) THEN
        call hashCode(ss%AP_HALFWIDTH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%AP_HORIZ_OFFSET)) THEN
        call hashCode(ss%AP_HORIZ_OFFSET,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%AP_VERT_OFFSET)) THEN
        call hashCode(ss%AP_VERT_OFFSET,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%BEAM_TYPE)) THEN
        call hashCode(ss%BEAM_TYPE,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%B_HALFHEIGHT)) THEN
        call hashCode(ss%B_HALFHEIGHT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%B_HALFWIDTH)) THEN
        call hashCode(ss%B_HALFWIDTH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%B_HDIVERGENCE)) THEN
        call hashCode(ss%B_HDIVERGENCE,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%B_HFOCAL_LENGTH)) THEN
        call hashCode(ss%B_HFOCAL_LENGTH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%B_VDIVERGENCE)) THEN
        call hashCode(ss%B_VDIVERGENCE,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%B_VFOCAL_LENGTH)) THEN
        call hashCode(ss%B_VFOCAL_LENGTH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%COIL_IN_CIRCUIT)) THEN
        call hashCode(ss%COIL_IN_CIRCUIT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%COIL_RESISPT)) THEN
        call hashCode(ss%COIL_RESISPT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%DX_FSHIELD)) THEN
        call hashCode(ss%DX_FSHIELD,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%D_EINJ_STANDARD)) THEN
        call hashCode(ss%D_EINJ_STANDARD,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%D_FFULL_STANDARD)) THEN
        call hashCode(ss%D_FFULL_STANDARD,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%D_FHALF_STANDARD)) THEN
        call hashCode(ss%D_FHALF_STANDARD,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%EC_BEAM_ELONGATION)) THEN
        call hashCode(ss%EC_BEAM_ELONGATION,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%EC_HALF_POWER_ANGLE)) THEN
        call hashCode(ss%EC_HALF_POWER_ANGLE,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%EC_PRINC_CURV_HOR)) THEN
        call hashCode(ss%EC_PRINC_CURV_HOR,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%EC_PRINC_CURV_VERT)) THEN
        call hashCode(ss%EC_PRINC_CURV_VERT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR)) THEN
        call hashCode(ss%EC_PRINC_WIDTH_HOR,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT)) THEN
        call hashCode(ss%EC_PRINC_WIDTH_VERT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%EINJ_MAX)) THEN
        call hashCode(ss%EINJ_MAX,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%EINJ_MIN)) THEN
        call hashCode(ss%EINJ_MIN,kf1,kf2)
     ENDIF
     call hashCode(ss%GEOMETRY,kf1,kf2)
     IF(ALLOCATED(ss%HSIZE_COIL)) THEN
        call hashCode(ss%HSIZE_COIL,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%H_EINJ_STANDARD)) THEN
        call hashCode(ss%H_EINJ_STANDARD,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%H_FFULL_STANDARD)) THEN
        call hashCode(ss%H_FFULL_STANDARD,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%H_FHALF_STANDARD)) THEN
        call hashCode(ss%H_FHALF_STANDARD,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%LBSCAP)) THEN
        call hashCode(ss%LBSCAP,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%LBSCAP2)) THEN
        call hashCode(ss%LBSCAP2,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%LBSCTAN)) THEN
        call hashCode(ss%LBSCTAN,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NBAP2_SHAPE)) THEN
        call hashCode(ss%NBAP2_SHAPE,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NBAP_SHAPE)) THEN
        call hashCode(ss%NBAP_SHAPE,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NBSHAPE)) THEN
        call hashCode(ss%NBSHAPE,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NRZ_ANTGEO)) THEN
        call hashCode(ss%NRZ_ANTGEO,kf1,kf2)
     ENDIF
     call hashCode(ss%NTF_COIL2,kf1,kf2)
     call hashCode(ss%NTF_COILS,kf1,kf2)
     IF(ALLOCATED(ss%NTURNS)) THEN
        call hashCode(ss%NTURNS,kf1,kf2)
     ENDIF
     call hashCode(ss%NUM_RZLIM,kf1,kf2)
     call hashCode(ss%N_D_EINJ_STANDARD,kf1,kf2)
     call hashCode(ss%N_H_EINJ_STANDARD,kf1,kf2)
     call hashCode(ss%N_T_EINJ_STANDARD,kf1,kf2)
     IF(ALLOCATED(ss%PHIBSC)) THEN
        call hashCode(ss%PHIBSC,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%PHI_EC_LAUNCH)) THEN
        call hashCode(ss%PHI_EC_LAUNCH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%RLIM)) THEN
        call hashCode(ss%RLIM,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%RLOC_COIL)) THEN
        call hashCode(ss%RLOC_COIL,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%R_ANTGEO)) THEN
        call hashCode(ss%R_ANTGEO,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%R_EC_LAUNCH)) THEN
        call hashCode(ss%R_EC_LAUNCH,kf1,kf2)
     ENDIF
     call hashCode(ss%R_MAX_BOX,kf1,kf2)
     call hashCode(ss%R_MIN_BOX,kf1,kf2)
     IF(ALLOCATED(ss%SRTCEN)) THEN
        call hashCode(ss%SRTCEN,kf1,kf2)
     ENDIF
     call hashCode(ss%TOKAMAK_ID,kf1,kf2)
     IF(ALLOCATED(ss%T_EINJ_STANDARD)) THEN
        call hashCode(ss%T_EINJ_STANDARD,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%T_FFULL_STANDARD)) THEN
        call hashCode(ss%T_FFULL_STANDARD,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%T_FHALF_STANDARD)) THEN
        call hashCode(ss%T_FHALF_STANDARD,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%VREP_COIL_COUNT)) THEN
        call hashCode(ss%VREP_COIL_COUNT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%VREP_COIL_SPACING)) THEN
        call hashCode(ss%VREP_COIL_SPACING,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%VSIZE_COIL)) THEN
        call hashCode(ss%VSIZE_COIL,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%ZBAP)) THEN
        call hashCode(ss%ZBAP,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%ZBSC)) THEN
        call hashCode(ss%ZBSC,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%ZLIM)) THEN
        call hashCode(ss%ZLIM,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%ZLOC_COIL)) THEN
        call hashCode(ss%ZLOC_COIL,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%Z_ANTGEO)) THEN
        call hashCode(ss%Z_ANTGEO,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%Z_EC_LAUNCH)) THEN
        call hashCode(ss%Z_EC_LAUNCH,kf1,kf2)
     ENDIF
     call hashCode(ss%Z_MAX_BOX,kf1,kf2)
     call hashCode(ss%Z_MIN_BOX,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  ilock = ss%lock_machine_descr
  CALL chk_hash("SECTION","machine_description")

  !----------------------
  !  section: SHOT_CONFIGURATION

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_amaxlh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%amaxlh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%amaxlh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_aminlh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%aminlh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%aminlh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_apolmaxlh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%apolmaxlh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%apolmaxlh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_apolminlh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%apolminlh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%apolminlh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_atormaxlh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%atormaxlh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%atormaxlh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_atorminlh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%atorminlh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%atorminlh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_EC_Omode_Fraction
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%EC_Omode_Fraction)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%EC_Omode_Fraction,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_EC_phi_aim
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%EC_phi_aim)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%EC_phi_aim,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_EC_theta_aim
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%EC_theta_aim)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%EC_theta_aim,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_fracpslh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%fracpslh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%fracpslh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_freq_ec
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%freq_ec)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%freq_ec,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_freq_ic
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%freq_ic)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%freq_ic,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_freq_lh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%freq_lh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%freq_lh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_GAS).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_gas_atom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gas_atom)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%gas_atom,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_hegrilh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%hegrilh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%hegrilh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Imag_ant_coef
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Imag_ant_coef)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Imag_ant_coef,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_GAS).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_is_recycling
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%is_recycling)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%is_recycling,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_i_npoloidal
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%i_npoloidal,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_kccw_Bphi
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%kccw_Bphi,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_kccw_Jphi
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%kccw_Jphi,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nbion
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nbion)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nbion,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nbion_trace
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nbion_trace)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nbion_trace,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nnkparlh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nnkparlh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nnkparlh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nnkpollh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nnkpollh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nnkpollh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nnktorlh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nnktorlh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nnktorlh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nphi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nphi)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nphi,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nspectrumlh_src
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nspectrumlh_src)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nspectrumlh_src,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nthinlh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nthinlh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nthinlh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_num_nphi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%num_nphi)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%num_nphi,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_num_nphi_vac
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%num_nphi_vac)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%num_nphi_vac,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_n_straps
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%n_straps)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%n_straps,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Real_ant_coef
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Real_ant_coef)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Real_ant_coef,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_rhopsi0lh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rhopsi0lh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%rhopsi0lh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_R_ant
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%R_ant)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%R_ant,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_shot_number
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%shot_number,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_thegrilh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%thegrilh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%thegrilh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_wt_nphi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%wt_nphi)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%wt_nphi,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Z_ant
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Z_ant)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Z_ant,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Z_mid_ant
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Z_mid_ant)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Z_mid_ant,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  IF(icheck_all) THEN
     iaddr1 = ps_shash_shot_configuration
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     IF(ALLOCATED(ss%AMAXLH)) THEN
        call hashCode(ss%AMAXLH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%AMINLH)) THEN
        call hashCode(ss%AMINLH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%APOLMAXLH)) THEN
        call hashCode(ss%APOLMAXLH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%APOLMINLH)) THEN
        call hashCode(ss%APOLMINLH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%ATORMAXLH)) THEN
        call hashCode(ss%ATORMAXLH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%ATORMINLH)) THEN
        call hashCode(ss%ATORMINLH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%EC_OMODE_FRACTION)) THEN
        call hashCode(ss%EC_OMODE_FRACTION,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%EC_PHI_AIM)) THEN
        call hashCode(ss%EC_PHI_AIM,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%EC_THETA_AIM)) THEN
        call hashCode(ss%EC_THETA_AIM,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%FRACPSLH)) THEN
        call hashCode(ss%FRACPSLH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%FREQ_EC)) THEN
        call hashCode(ss%FREQ_EC,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%FREQ_IC)) THEN
        call hashCode(ss%FREQ_IC,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%FREQ_LH)) THEN
        call hashCode(ss%FREQ_LH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%GAS_ATOM)) THEN
        call hashCode(ss%GAS_ATOM,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%HEGRILH)) THEN
        call hashCode(ss%HEGRILH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%IMAG_ANT_COEF)) THEN
        call hashCode(ss%IMAG_ANT_COEF,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%IS_RECYCLING)) THEN
        call hashCode(ss%IS_RECYCLING,kf1,kf2)
     ENDIF
     call hashCode(ss%I_NPOLOIDAL,kf1,kf2)
     call hashCode(ss%KCCW_BPHI,kf1,kf2)
     call hashCode(ss%KCCW_JPHI,kf1,kf2)
     IF(ALLOCATED(ss%NBION)) THEN
        call hashCode(ss%NBION,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NBION_TRACE)) THEN
        call hashCode(ss%NBION_TRACE,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NNKPARLH)) THEN
        call hashCode(ss%NNKPARLH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NNKPOLLH)) THEN
        call hashCode(ss%NNKPOLLH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NNKTORLH)) THEN
        call hashCode(ss%NNKTORLH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NPHI)) THEN
        call hashCode(ss%NPHI,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NSPECTRUMLH_SRC)) THEN
        call hashCode(ss%NSPECTRUMLH_SRC,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NTHINLH)) THEN
        call hashCode(ss%NTHINLH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NUM_NPHI)) THEN
        call hashCode(ss%NUM_NPHI,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NUM_NPHI_VAC)) THEN
        call hashCode(ss%NUM_NPHI_VAC,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%N_STRAPS)) THEN
        call hashCode(ss%N_STRAPS,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%REAL_ANT_COEF)) THEN
        call hashCode(ss%REAL_ANT_COEF,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%RHOPSI0LH)) THEN
        call hashCode(ss%RHOPSI0LH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%R_ANT)) THEN
        call hashCode(ss%R_ANT,kf1,kf2)
     ENDIF
     call hashCode(ss%SHOT_NUMBER,kf1,kf2)
     IF(ALLOCATED(ss%THEGRILH)) THEN
        call hashCode(ss%THEGRILH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%WT_NPHI)) THEN
        call hashCode(ss%WT_NPHI,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%Z_ANT)) THEN
        call hashCode(ss%Z_ANT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%Z_MID_ANT)) THEN
        call hashCode(ss%Z_MID_ANT,kf1,kf2)
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  ilock = ss%lock_shot_config
  CALL chk_hash("SECTION","shot_configuration")

  !----------------------
  !  section: SIMULATION_INIT

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_alla_index
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%alla_index)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%alla_index,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_all_index
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%all_index)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%all_index,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_ANOM).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ANOM_Code_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%ANOM_Code_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Balloon_mapper
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Balloon_mapper)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Balloon_mapper,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_CUR_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%CUR_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_DeltaW_mapper
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%DeltaW_mapper)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%DeltaW_mapper,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_EC_Code_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%EC_Code_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_EC_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%EC_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_EQ_Code_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%EQ_Code_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_EQ_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%EQ_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_fracmin
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%fracmin)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%fracmin,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_FUS).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_FUS_Code_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%FUS_Code_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_GAS).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_GAS_Code_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%GAS_Code_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Global_label
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%Global_label,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_icircuit_coil
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%icircuit_coil)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%icircuit_coil,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_IC_Code_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%IC_Code_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_IC_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%IC_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_kdens_rfmin
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%kdens_rfmin,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_LH_Code_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%LH_Code_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_LH_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%LH_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_LMHD_eq_code
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%LMHD_eq_code,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_NBI_Code_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%NBI_Code_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_NBI_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%NBI_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Newcomb_mapper
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Newcomb_mapper)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Newcomb_mapper,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nmom
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%nmom,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ns_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%ns_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PEL).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_PEL_Code_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%PEL_Code_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PEL).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_PEL_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%PEL_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_PLASMA_Code_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%PLASMA_Code_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_RAD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_RAD_Code_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%RAD_Code_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_RAD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_RAD_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%RAD_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_rfmin_to_all
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rfmin_to_all)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%rfmin_to_all,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_rfmin_to_alla
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rfmin_to_alla)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%rfmin_to_alla,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_RUNAWAY).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_RUNAWAY_Code_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%RUNAWAY_Code_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_RunID
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%RunID,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_sa_index
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sa_index)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%sa_index,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_GAS).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_sc0_to_sgas
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sc0_to_sgas)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%sc0_to_sgas,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_sfus_to_all
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sfus_to_all)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%sfus_to_all,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_sfus_to_alla
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sfus_to_alla)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%sfus_to_alla,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_GAS).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_sgas_to_s
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sgas_to_s)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%sgas_to_s,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_GAS).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_simp0_to_s
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%simp0_to_s)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%simp0_to_s,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_simpi_to_s
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%simpi_to_s)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%simpi_to_s,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_snbi_to_all
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%snbi_to_all)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%snbi_to_all,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_snbi_to_alla
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%snbi_to_alla)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%snbi_to_alla,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_s_to_sa
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%s_to_sa)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%s_to_sa,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_tfinal
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%tfinal,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_TF_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%TF_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_tinit
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%tinit,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Tor_mode_No
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Tor_mode_No)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Tor_mode_No,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_trace_flag
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%trace_flag)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%trace_flag,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_trmodels
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%trmodels)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%trmodels,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Ts_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%Ts_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_vpol_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%vpol_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_VSUR_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%VSUR_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_vtor_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%vtor_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Z0max
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%Z0max,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ZEFF_Data_Info
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%ZEFF_Data_Info,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Zimp1
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%Zimp1,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Zimp2
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%Zimp2,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  IF(icheck_all) THEN
     iaddr1 = ps_shash_simulation_init
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     IF(ALLOCATED(ss%ALLA_INDEX)) THEN
        call hashCode(ss%ALLA_INDEX,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%ALL_INDEX)) THEN
        call hashCode(ss%ALL_INDEX,kf1,kf2)
     ENDIF
     call hashCode(ss%ANOM_CODE_INFO,kf1,kf2)
     IF(ALLOCATED(ss%BALLOON_MAPPER)) THEN
        call hashCode(ss%BALLOON_MAPPER,kf1,kf2)
     ENDIF
     call hashCode(ss%CUR_DATA_INFO,kf1,kf2)
     IF(ALLOCATED(ss%DELTAW_MAPPER)) THEN
        call hashCode(ss%DELTAW_MAPPER,kf1,kf2)
     ENDIF
     call hashCode(ss%EC_CODE_INFO,kf1,kf2)
     call hashCode(ss%EC_DATA_INFO,kf1,kf2)
     call hashCode(ss%EQ_CODE_INFO,kf1,kf2)
     call hashCode(ss%EQ_DATA_INFO,kf1,kf2)
     IF(ALLOCATED(ss%FRACMIN)) THEN
        call hashCode(ss%FRACMIN,kf1,kf2)
     ENDIF
     call hashCode(ss%FUS_CODE_INFO,kf1,kf2)
     call hashCode(ss%GAS_CODE_INFO,kf1,kf2)
     call hashCode(ss%GLOBAL_LABEL,kf1,kf2)
     IF(ALLOCATED(ss%ICIRCUIT_COIL)) THEN
        call hashCode(ss%ICIRCUIT_COIL,kf1,kf2)
     ENDIF
     call hashCode(ss%IC_CODE_INFO,kf1,kf2)
     call hashCode(ss%IC_DATA_INFO,kf1,kf2)
     call hashCode(ss%KDENS_RFMIN,kf1,kf2)
     call hashCode(ss%LH_CODE_INFO,kf1,kf2)
     call hashCode(ss%LH_DATA_INFO,kf1,kf2)
     call hashCode(ss%LMHD_EQ_CODE,kf1,kf2)
     call hashCode(ss%NBI_CODE_INFO,kf1,kf2)
     call hashCode(ss%NBI_DATA_INFO,kf1,kf2)
     IF(ALLOCATED(ss%NEWCOMB_MAPPER)) THEN
        call hashCode(ss%NEWCOMB_MAPPER,kf1,kf2)
     ENDIF
     call hashCode(ss%NKY_ANOMTH,kf1,kf2)
     call hashCode(ss%NKY_MMM,kf1,kf2)
     call hashCode(ss%NMOM,kf1,kf2)
     call hashCode(ss%NS_DATA_INFO,kf1,kf2)
     call hashCode(ss%PEL_CODE_INFO,kf1,kf2)
     call hashCode(ss%PEL_DATA_INFO,kf1,kf2)
     call hashCode(ss%PLASMA_CODE_INFO,kf1,kf2)
     call hashCode(ss%RAD_CODE_INFO,kf1,kf2)
     call hashCode(ss%RAD_DATA_INFO,kf1,kf2)
     IF(ALLOCATED(ss%RFMIN_TO_ALL)) THEN
        call hashCode(ss%RFMIN_TO_ALL,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%RFMIN_TO_ALLA)) THEN
        call hashCode(ss%RFMIN_TO_ALLA,kf1,kf2)
     ENDIF
     call hashCode(ss%RUNAWAY_CODE_INFO,kf1,kf2)
     call hashCode(ss%RUNID,kf1,kf2)
     IF(ALLOCATED(ss%SA_INDEX)) THEN
        call hashCode(ss%SA_INDEX,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%SC0_TO_SGAS)) THEN
        call hashCode(ss%SC0_TO_SGAS,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%SFUS_TO_ALL)) THEN
        call hashCode(ss%SFUS_TO_ALL,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%SFUS_TO_ALLA)) THEN
        call hashCode(ss%SFUS_TO_ALLA,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%SGAS_TO_S)) THEN
        call hashCode(ss%SGAS_TO_S,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%SIMP0_TO_S)) THEN
        call hashCode(ss%SIMP0_TO_S,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%SIMPI_TO_S)) THEN
        call hashCode(ss%SIMPI_TO_S,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%SNBI_TO_ALL)) THEN
        call hashCode(ss%SNBI_TO_ALL,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%SNBI_TO_ALLA)) THEN
        call hashCode(ss%SNBI_TO_ALLA,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%S_TO_SA)) THEN
        call hashCode(ss%S_TO_SA,kf1,kf2)
     ENDIF
     call hashCode(ss%TFINAL,kf1,kf2)
     call hashCode(ss%TF_DATA_INFO,kf1,kf2)
     call hashCode(ss%TINIT,kf1,kf2)
     IF(ALLOCATED(ss%TOR_MODE_NO)) THEN
        call hashCode(ss%TOR_MODE_NO,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%TRACE_FLAG)) THEN
        call hashCode(ss%TRACE_FLAG,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%TRMODELS)) THEN
        call hashCode(ss%TRMODELS,kf1,kf2)
     ENDIF
     call hashCode(ss%TS_DATA_INFO,kf1,kf2)
     call hashCode(ss%VPOL_DATA_INFO,kf1,kf2)
     call hashCode(ss%VSUR_DATA_INFO,kf1,kf2)
     call hashCode(ss%VTOR_DATA_INFO,kf1,kf2)
     call hashCode(ss%Z0MAX,kf1,kf2)
     call hashCode(ss%ZEFF_DATA_INFO,kf1,kf2)
     call hashCode(ss%ZIMP1,kf1,kf2)
     call hashCode(ss%ZIMP2,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  ilock = ss%lock_sim_init
  CALL chk_hash("SECTION","simulation_init")

  !----------------------
  !  section: STATE_DATA

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Balloon_stable
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Balloon_stable)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Balloon_stable,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Balloon_status
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Balloon_status)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Balloon_status,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_B_axis
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%B_axis,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_B_axis_vac
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%B_axis_vac,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_B_max_lcfs
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%B_max_lcfs,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_B_min_lcfs
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%B_min_lcfs,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_coil_apt
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%coil_apt)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%coil_apt,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_DeltaW_stable
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%DeltaW_stable)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%DeltaW_stable,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_DeltaW_status
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%DeltaW_status)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%DeltaW_status,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_dist_fun
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dist_fun)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%dist_fun,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_GAS).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_dn0out
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%dn0out,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_GAS).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_e0_av
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%e0_av)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%e0_av,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_eldot
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%eldot,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_eqdsk_file
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%eqdsk_file,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_frac_full
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%frac_full)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%frac_full,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_frac_half
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%frac_half)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%frac_half,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_gamma_lmhd
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gamma_lmhd)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%gamma_lmhd,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_ANOM).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_interchange_R
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%interchange_R,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_isThermal
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%isThermal)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%isThermal,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_kvolt_nbi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%kvolt_nbi)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%kvolt_nbi,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_LMHD_status
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%LMHD_status,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_lock_machine_descr
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%lock_machine_descr,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_lock_shot_config
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%lock_shot_config,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_lock_sim_init
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%lock_sim_init,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Mercier_stable
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Mercier_stable)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Mercier_stable,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_MHD_eq_status
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%MHD_eq_status,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nbeami_bdy
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nbeami_bdy)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nbeami_bdy,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nebar
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%nebar,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Newcomb_stable
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Newcomb_stable)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Newcomb_stable,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Newcomb_status
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Newcomb_status)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Newcomb_status,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nfusi_bdy
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nfusi_bdy)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nfusi_bdy,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_ANOM).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nmdifb
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%nmdifb,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_FUS).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nmini_bdy
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nmini_bdy)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%nmini_bdy,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_ANOM).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_nmodel_bdy
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%nmodel_bdy,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ns_bdy
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ns_bdy)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%ns_bdy,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ns_is_input
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ns_is_input)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%ns_is_input,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_omegat_bdy
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%omegat_bdy,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_picrf_abs
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%picrf_abs)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%picrf_abs,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_picrf_ext
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%picrf_ext)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%picrf_ext,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_power_ec
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%power_ec)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%power_ec,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_power_ic
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%power_ic)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%power_ic,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LH).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_power_lh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%power_lh)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%power_lh,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_power_nbi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%power_nbi)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%power_nbi,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_NBI).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_power_nbi_trace
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%power_nbi_trace)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%power_nbi_trace,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Psi_to_machine_axis
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%Psi_to_machine_axis,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_ql_operator
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ql_operator)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%ql_operator,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_rho_bdy_ns
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_bdy_ns)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%rho_bdy_ns,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_rho_bdy_omegat
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%rho_bdy_omegat,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_rho_bdy_Te
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%rho_bdy_Te,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_rho_bdy_Ti
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%rho_bdy_Ti,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_R_axis
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%R_axis,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_R_max_lcfs
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%R_max_lcfs,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_R_min_lcfs
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%R_min_lcfs,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_GAS).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_sc0
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sc0)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%sc0,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_t0
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%t0,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_t1
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%t1,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Te_bdy
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%Te_bdy,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Ti_bdy
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%Ti_bdy,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_ANOM).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_tmodel_bdy
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%tmodel_bdy,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_LMHD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Tornum_balcrit
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Tornum_balcrit)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Tornum_balcrit,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Ts_is_input
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Ts_is_input)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%Ts_is_input,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_GAS).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_vphi0_av
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vphi0_av)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%vphi0_av,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_vpol_is_input
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vpol_is_input)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%vpol_is_input,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_vsur
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%vsur,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_PLASMA).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_vtor_is_input
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vtor_is_input)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%vtor_is_input,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_wt_nphi_abs
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%wt_nphi_abs)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%wt_nphi_abs,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_IC).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_wt_nphi_ext
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%wt_nphi_ext)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%wt_nphi_ext,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_RAD).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_xs
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%xs)) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%xs,kf1,kf2)
     ELSE
        kf1 = 0; kf2 = 0
     ENDIF

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Z_axis
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%Z_axis,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Z_max_lcfs
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%Z_max_lcfs,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  if(cclist(ps_cnum_EQ).eq.ps_chkAll) THEN
     iaddr1 = ps_shash_Z_min_lcfs
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     call hashCode(ss%Z_min_lcfs,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  IF(icheck_all) THEN
     iaddr1 = ps_shash_state_data
     iaddr2 = iaddr1 + 1
     kf1 = 17; kf2 = 23
     IF(ALLOCATED(ss%BALLOON_STABLE)) THEN
        call hashCode(ss%BALLOON_STABLE,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%BALLOON_STATUS)) THEN
        call hashCode(ss%BALLOON_STATUS,kf1,kf2)
     ENDIF
     call hashCode(ss%B_AXIS,kf1,kf2)
     call hashCode(ss%B_AXIS_VAC,kf1,kf2)
     call hashCode(ss%B_MAX_LCFS,kf1,kf2)
     call hashCode(ss%B_MIN_LCFS,kf1,kf2)
     IF(ALLOCATED(ss%COIL_APT)) THEN
        call hashCode(ss%COIL_APT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%DELTAW_STABLE)) THEN
        call hashCode(ss%DELTAW_STABLE,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%DELTAW_STATUS)) THEN
        call hashCode(ss%DELTAW_STATUS,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%DIST_FUN)) THEN
        call hashCode(ss%DIST_FUN,kf1,kf2)
     ENDIF
     call hashCode(ss%DN0OUT,kf1,kf2)
     IF(ALLOCATED(ss%E0_AV)) THEN
        call hashCode(ss%E0_AV,kf1,kf2)
     ENDIF
     call hashCode(ss%ELDOT,kf1,kf2)
     call hashCode(ss%EQDSK_FILE,kf1,kf2)
     IF(ALLOCATED(ss%FRAC_FULL)) THEN
        call hashCode(ss%FRAC_FULL,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%FRAC_HALF)) THEN
        call hashCode(ss%FRAC_HALF,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%GAMMA_LMHD)) THEN
        call hashCode(ss%GAMMA_LMHD,kf1,kf2)
     ENDIF
     call hashCode(ss%INTERCHANGE_R,kf1,kf2)
     IF(ALLOCATED(ss%ISTHERMAL)) THEN
        call hashCode(ss%ISTHERMAL,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%KVOLT_NBI)) THEN
        call hashCode(ss%KVOLT_NBI,kf1,kf2)
     ENDIF
     call hashCode(ss%LMHD_STATUS,kf1,kf2)
     call hashCode(ss%LOCK_MACHINE_DESCR,kf1,kf2)
     call hashCode(ss%LOCK_SHOT_CONFIG,kf1,kf2)
     call hashCode(ss%LOCK_SIM_INIT,kf1,kf2)
     IF(ALLOCATED(ss%MERCIER_STABLE)) THEN
        call hashCode(ss%MERCIER_STABLE,kf1,kf2)
     ENDIF
     call hashCode(ss%MHD_EQ_STATUS,kf1,kf2)
     IF(ALLOCATED(ss%NBEAMI_BDY)) THEN
        call hashCode(ss%NBEAMI_BDY,kf1,kf2)
     ENDIF
     call hashCode(ss%NEBAR,kf1,kf2)
     IF(ALLOCATED(ss%NEWCOMB_STABLE)) THEN
        call hashCode(ss%NEWCOMB_STABLE,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NEWCOMB_STATUS)) THEN
        call hashCode(ss%NEWCOMB_STATUS,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NFUSI_BDY)) THEN
        call hashCode(ss%NFUSI_BDY,kf1,kf2)
     ENDIF
     call hashCode(ss%NMDIFB,kf1,kf2)
     IF(ALLOCATED(ss%NMINI_BDY)) THEN
        call hashCode(ss%NMINI_BDY,kf1,kf2)
     ENDIF
     call hashCode(ss%NMODEL_BDY,kf1,kf2)
     IF(ALLOCATED(ss%NS_BDY)) THEN
        call hashCode(ss%NS_BDY,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%NS_IS_INPUT)) THEN
        call hashCode(ss%NS_IS_INPUT,kf1,kf2)
     ENDIF
     call hashCode(ss%OMEGAT_BDY,kf1,kf2)
     IF(ALLOCATED(ss%PICRF_ABS)) THEN
        call hashCode(ss%PICRF_ABS,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%PICRF_EXT)) THEN
        call hashCode(ss%PICRF_EXT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%POWER_EC)) THEN
        call hashCode(ss%POWER_EC,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%POWER_IC)) THEN
        call hashCode(ss%POWER_IC,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%POWER_LH)) THEN
        call hashCode(ss%POWER_LH,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%POWER_NBI)) THEN
        call hashCode(ss%POWER_NBI,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%POWER_NBI_TRACE)) THEN
        call hashCode(ss%POWER_NBI_TRACE,kf1,kf2)
     ENDIF
     call hashCode(ss%PSI_TO_MACHINE_AXIS,kf1,kf2)
     IF(ALLOCATED(ss%QL_OPERATOR)) THEN
        call hashCode(ss%QL_OPERATOR,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%RHO_BDY_NS)) THEN
        call hashCode(ss%RHO_BDY_NS,kf1,kf2)
     ENDIF
     call hashCode(ss%RHO_BDY_OMEGAT,kf1,kf2)
     call hashCode(ss%RHO_BDY_TE,kf1,kf2)
     call hashCode(ss%RHO_BDY_TI,kf1,kf2)
     call hashCode(ss%R_AXIS,kf1,kf2)
     call hashCode(ss%R_MAX_LCFS,kf1,kf2)
     call hashCode(ss%R_MIN_LCFS,kf1,kf2)
     IF(ALLOCATED(ss%SC0)) THEN
        call hashCode(ss%SC0,kf1,kf2)
     ENDIF
     call hashCode(ss%T0,kf1,kf2)
     call hashCode(ss%T1,kf1,kf2)
     call hashCode(ss%TE_BDY,kf1,kf2)
     call hashCode(ss%TI_BDY,kf1,kf2)
     call hashCode(ss%TMODEL_BDY,kf1,kf2)
     IF(ALLOCATED(ss%TORNUM_BALCRIT)) THEN
        call hashCode(ss%TORNUM_BALCRIT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%TS_IS_INPUT)) THEN
        call hashCode(ss%TS_IS_INPUT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%VPHI0_AV)) THEN
        call hashCode(ss%VPHI0_AV,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%VPOL_IS_INPUT)) THEN
        call hashCode(ss%VPOL_IS_INPUT,kf1,kf2)
     ENDIF
     call hashCode(ss%VSUR,kf1,kf2)
     IF(ALLOCATED(ss%VTOR_IS_INPUT)) THEN
        call hashCode(ss%VTOR_IS_INPUT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%WT_NPHI_ABS)) THEN
        call hashCode(ss%WT_NPHI_ABS,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%WT_NPHI_EXT)) THEN
        call hashCode(ss%WT_NPHI_EXT,kf1,kf2)
     ENDIF
     IF(ALLOCATED(ss%XS)) THEN
        call hashCode(ss%XS,kf1,kf2)
     ENDIF
     call hashCode(ss%Z_AXIS,kf1,kf2)
     call hashCode(ss%Z_MAX_LCFS,kf1,kf2)
     call hashCode(ss%Z_MIN_LCFS,kf1,kf2)

     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2
  ENDIF

  ilock = ps_unlocked
  CALL chk_hash("SECTION","state_data")

  !----------------------
  !  section: STATE_PROFILES

  iaddr1 = ps_shash_state_profiles
  iaddr2 = iaddr1 + 1
  ss%new_hash_codes(iaddr1) = 0
  ss%new_hash_codes(iaddr2) = 0

  ilock = ps_unlocked
  CALL chk_hash("SECTION","state_profiles")

  !--------------------------------------------
  ! dimension hash codes

1000 continue  ! jump here to skip scalar hashes

  !----------------------
  ! dimension: MAX_NRZ_ANTGEO
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_max_nrz_antgeo
     iaddr2 = iaddr1 + 1
     kf1 = ps_unInit; kf2 = ps_unInit
     IF(ss%MAX_NRZ_ANTGEO.NE.0) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%MAX_NRZ_ANTGEO,kf1,kf2)
     ENDIF

     ilock = ss%lock_machine_descr
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","max_nrz_antgeo")
  ENDIF

  !----------------------
  ! dimension: MAX_NSPECTRUMLH_SRC
  IF(cclist(ps_cnum_LH).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_max_nspectrumlh_src
     iaddr2 = iaddr1 + 1
     kf1 = ps_unInit; kf2 = ps_unInit
     IF(ss%MAX_NSPECTRUMLH_SRC.NE.0) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%MAX_NSPECTRUMLH_SRC,kf1,kf2)
     ENDIF

     ilock = ss%lock_shot_config
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","max_nspectrumlh_src")
  ENDIF

  !----------------------
  ! dimension: MAX_NUM_NPHI
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_max_num_nphi
     iaddr2 = iaddr1 + 1
     kf1 = ps_unInit; kf2 = ps_unInit
     IF(ss%MAX_NUM_NPHI.NE.0) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%MAX_NUM_NPHI,kf1,kf2)
     ENDIF

     ilock = ss%lock_shot_config
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","max_num_nphi")
  ENDIF

  !----------------------
  ! dimension: MAX_NUM_NPHI_VAC
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_max_num_nphi_vac
     iaddr2 = iaddr1 + 1
     kf1 = ps_unInit; kf2 = ps_unInit
     IF(ss%MAX_NUM_NPHI_VAC.NE.0) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%MAX_NUM_NPHI_VAC,kf1,kf2)
     ENDIF

     ilock = ss%lock_shot_config
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","max_num_nphi_vac")
  ENDIF

  !----------------------
  ! dimension: MAX_N_STRAPS
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_max_n_straps
     iaddr2 = iaddr1 + 1
     kf1 = ps_unInit; kf2 = ps_unInit
     IF(ss%MAX_N_STRAPS.NE.0) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%MAX_N_STRAPS,kf1,kf2)
     ENDIF

     ilock = ss%lock_shot_config
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","max_n_straps")
  ENDIF

  !----------------------
  ! dimension: MMODES
  IF(cclist(ps_cnum_LMHD).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_mmodes
     iaddr2 = iaddr1 + 1
     IF(ss%MMODES.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%Tor_mode_label,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","mmodes")
  ENDIF

  !----------------------
  ! dimension: NBALLOON
  IF(cclist(ps_cnum_LMHD).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nballoon
     iaddr2 = iaddr1 + 1
     IF(ss%NBALLOON.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%Balloon_code,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nballoon")
  ENDIF

  !----------------------
  ! dimension: NBEAM
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nbeam
     iaddr2 = iaddr1 + 1
     IF(ss%NBEAM.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%nbi_src_name,kf1,kf2)
     ENDIF

     ilock = ss%lock_machine_descr
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nbeam")
  ENDIF

  !----------------------
  ! dimension: NCIRCUITS
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_ncircuits
     iaddr2 = iaddr1 + 1
     IF(ss%NCIRCUITS.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%circuit_name,kf1,kf2)
     ENDIF

     ilock = ss%lock_machine_descr
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","ncircuits")
  ENDIF

  !----------------------
  ! dimension: NCOILS
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_ncoils
     iaddr2 = iaddr1 + 1
     IF(ss%NCOILS.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%coil_name,kf1,kf2)
     ENDIF

     ilock = ss%lock_machine_descr
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","ncoils")
  ENDIF

  !----------------------
  ! dimension: NDELTA_W
  IF(cclist(ps_cnum_LMHD).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_ndelta_w
     iaddr2 = iaddr1 + 1
     IF(ss%NDELTA_W.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%DeltaW_code,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","ndelta_w")
  ENDIF

  !----------------------
  ! dimension: NECRF_SRC
  IF(cclist(ps_cnum_EC).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_necrf_src
     iaddr2 = iaddr1 + 1
     IF(ss%NECRF_SRC.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%ecrf_src_name,kf1,kf2)
     ENDIF

     ilock = ss%lock_machine_descr
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","necrf_src")
  ENDIF

  !----------------------
  ! dimension: NEFI_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nefi_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%E_anom)) THEN
        IF(MAXVAL(ABS(ss%E_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%E_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nefi_anom")
  ENDIF

  !----------------------
  ! dimension: NEFI_ANOM2
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nefi_anom2
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%E_anom2)) THEN
        IF(MAXVAL(ABS(ss%E_anom2)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%E_anom2,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nefi_anom2")
  ENDIF

  !----------------------
  ! dimension: NEQMOM
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_neqmom
     iaddr2 = iaddr1 + 1
     IF(ss%NEQMOM.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%eqmom_num,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","neqmom")
  ENDIF

  !----------------------
  ! dimension: NGSC0
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_ngsc0
     iaddr2 = iaddr1 + 1
     IF(ss%NGSC0.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%gs_name,kf1,kf2)
     ENDIF

     ilock = ss%lock_shot_config
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","ngsc0")
  ENDIF

  !----------------------
  ! dimension: NICRF_SRC
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nicrf_src
     iaddr2 = iaddr1 + 1
     IF(ss%NICRF_SRC.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%icrf_src_name,kf1,kf2)
     ENDIF

     ilock = ss%lock_machine_descr
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nicrf_src")
  ENDIF

  !----------------------
  ! dimension: NKY_ANOMTH
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nky_anomth
     iaddr2 = iaddr1 + 1
     kf1 = ps_unInit; kf2 = ps_unInit
     IF(ss%NKY_ANOMTH.NE.0) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%NKY_ANOMTH,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nky_anomth")
  ENDIF

  !----------------------
  ! dimension: NKY_MMM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nky_mmm
     iaddr2 = iaddr1 + 1
     kf1 = ps_unInit; kf2 = ps_unInit
     IF(ss%NKY_MMM.NE.0) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%NKY_MMM,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nky_mmm")
  ENDIF

  !----------------------
  ! dimension: NLHRF_SRC
  IF(cclist(ps_cnum_LH).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nlhrf_src
     iaddr2 = iaddr1 + 1
     IF(ss%NLHRF_SRC.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%lhrf_src_name,kf1,kf2)
     ENDIF

     ilock = ss%lock_machine_descr
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nlhrf_src")
  ENDIF

  !----------------------
  ! dimension: NNEWCOMB
  IF(cclist(ps_cnum_LMHD).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nnewcomb
     iaddr2 = iaddr1 + 1
     IF(ss%NNEWCOMB.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%Newcomb_code,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nnewcomb")
  ENDIF

  !----------------------
  ! dimension: NPSMOM
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_npsmom
     iaddr2 = iaddr1 + 1
     IF(ss%NPSMOM.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%psmom_num,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","npsmom")
  ENDIF

  !----------------------
  ! dimension: NR
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nr
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%R_grid)) THEN
        IF(MAXVAL(ABS(ss%R_grid)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%R_grid,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nr")
  ENDIF

  !----------------------
  ! dimension: NRHO
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho)) THEN
        IF(MAXVAL(ABS(ss%rho)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho")
  ENDIF

  !----------------------
  ! dimension: NRHO_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_anom)) THEN
        IF(MAXVAL(ABS(ss%rho_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_anom")
  ENDIF

  !----------------------
  ! dimension: NRHO_ANOM2
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_anom2
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_anom2)) THEN
        IF(MAXVAL(ABS(ss%rho_anom2)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_anom2,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_anom2")
  ENDIF

  !----------------------
  ! dimension: NRHO_ANOMTH
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_anomth
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_anomth)) THEN
        IF(MAXVAL(ABS(ss%rho_anomth)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_anomth,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_anomth")
  ENDIF

  !----------------------
  ! dimension: NRHO_ECRF
  IF(cclist(ps_cnum_EC).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_ecrf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_ecrf)) THEN
        IF(MAXVAL(ABS(ss%rho_ecrf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_ecrf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_ecrf")
  ENDIF

  !----------------------
  ! dimension: NRHO_EQ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_eq
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_eq)) THEN
        IF(MAXVAL(ABS(ss%rho_eq)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_eq,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_eq")
  ENDIF

  !----------------------
  ! dimension: NRHO_EQ_GEO
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_eq_geo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_eq_geo)) THEN
        IF(MAXVAL(ABS(ss%rho_eq_geo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_eq_geo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_eq_geo")
  ENDIF

  !----------------------
  ! dimension: NRHO_FUS
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_fus
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_fus)) THEN
        IF(MAXVAL(ABS(ss%rho_fus)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_fus,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_fus")
  ENDIF

  !----------------------
  ! dimension: NRHO_GAS
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_gas
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_gas)) THEN
        IF(MAXVAL(ABS(ss%rho_gas)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_gas,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_gas")
  ENDIF

  !----------------------
  ! dimension: NRHO_ICRF
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_icrf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_icrf)) THEN
        IF(MAXVAL(ABS(ss%rho_icrf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_icrf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_icrf")
  ENDIF

  !----------------------
  ! dimension: NRHO_LHRF
  IF(cclist(ps_cnum_LH).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_lhrf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_lhrf)) THEN
        IF(MAXVAL(ABS(ss%rho_lhrf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_lhrf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_lhrf")
  ENDIF

  !----------------------
  ! dimension: NRHO_LMHD
  IF(cclist(ps_cnum_LMHD).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_lmhd
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_lmhd)) THEN
        IF(MAXVAL(ABS(ss%rho_lmhd)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_lmhd,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_lmhd")
  ENDIF

  !----------------------
  ! dimension: NRHO_MIST
  IF(cclist(ps_cnum_RAD).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_mist
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_mist)) THEN
        IF(MAXVAL(ABS(ss%rho_mist)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_mist,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_mist")
  ENDIF

  !----------------------
  ! dimension: NRHO_NBI
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_nbi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_nbi)) THEN
        IF(MAXVAL(ABS(ss%rho_nbi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_nbi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_nbi")
  ENDIF

  !----------------------
  ! dimension: NRHO_PEL
  IF(cclist(ps_cnum_PEL).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_pel
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_pel)) THEN
        IF(MAXVAL(ABS(ss%rho_pel)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_pel,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_pel")
  ENDIF

  !----------------------
  ! dimension: NRHO_RAD
  IF(cclist(ps_cnum_RAD).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_rad
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_rad)) THEN
        IF(MAXVAL(ABS(ss%rho_rad)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_rad,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_rad")
  ENDIF

  !----------------------
  ! dimension: NRHO_RW
  IF(cclist(ps_cnum_RUNAWAY).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nrho_rw
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rho_rw)) THEN
        IF(MAXVAL(ABS(ss%rho_rw)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rho_rw,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nrho_rw")
  ENDIF

  !----------------------
  ! dimension: NR_RIP
  IF(cclist(ps_cnum_RIPPLE).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nr_rip
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%R_ripple)) THEN
        IF(MAXVAL(ABS(ss%R_ripple)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%R_ripple,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nr_rip")
  ENDIF

  !----------------------
  ! dimension: NSPEC_ALL
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nspec_all
     iaddr2 = iaddr1 + 1
     IF(ss%NSPEC_ALL.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%ALL_name,kf1,kf2)
        call hashCode(ss%ALL_type,kf1,kf2)
        call hashCode(ss%q_ALL,kf1,kf2)
        call hashCode(ss%qatom_ALL,kf1,kf2)
        call hashCode(ss%m_ALL,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nspec_all")
  ENDIF

  !----------------------
  ! dimension: NSPEC_ALLA
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nspec_alla
     iaddr2 = iaddr1 + 1
     IF(ss%NSPEC_ALLA.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%ALLA_name,kf1,kf2)
        call hashCode(ss%ALLA_type,kf1,kf2)
        call hashCode(ss%q_ALLA,kf1,kf2)
        call hashCode(ss%qatom_ALLA,kf1,kf2)
        call hashCode(ss%m_ALLA,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nspec_alla")
  ENDIF

  !----------------------
  ! dimension: NSPEC_BEAM
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nspec_beam
     iaddr2 = iaddr1 + 1
     IF(ss%NSPEC_BEAM.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%SNBI_name,kf1,kf2)
        call hashCode(ss%SNBI_type,kf1,kf2)
        call hashCode(ss%q_SNBI,kf1,kf2)
        call hashCode(ss%qatom_SNBI,kf1,kf2)
        call hashCode(ss%m_SNBI,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nspec_beam")
  ENDIF

  !----------------------
  ! dimension: NSPEC_FUSION
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nspec_fusion
     iaddr2 = iaddr1 + 1
     IF(ss%NSPEC_FUSION.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%SFUS_name,kf1,kf2)
        call hashCode(ss%SFUS_type,kf1,kf2)
        call hashCode(ss%q_SFUS,kf1,kf2)
        call hashCode(ss%qatom_SFUS,kf1,kf2)
        call hashCode(ss%m_SFUS,kf1,kf2)
     ENDIF

     ilock = ss%lock_shot_config
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nspec_fusion")
  ENDIF

  !----------------------
  ! dimension: NSPEC_GAS
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nspec_gas
     iaddr2 = iaddr1 + 1
     IF(ss%NSPEC_GAS.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%SGAS_name,kf1,kf2)
        call hashCode(ss%SGAS_type,kf1,kf2)
        call hashCode(ss%q_SGAS,kf1,kf2)
        call hashCode(ss%qatom_SGAS,kf1,kf2)
        call hashCode(ss%m_SGAS,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nspec_gas")
  ENDIF

  !----------------------
  ! dimension: NSPEC_IMP0
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nspec_imp0
     iaddr2 = iaddr1 + 1
     IF(ss%NSPEC_IMP0.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%SIMP0_name,kf1,kf2)
        call hashCode(ss%SIMP0_type,kf1,kf2)
        call hashCode(ss%q_SIMP0,kf1,kf2)
        call hashCode(ss%qatom_SIMP0,kf1,kf2)
        call hashCode(ss%m_SIMP0,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nspec_imp0")
  ENDIF

  !----------------------
  ! dimension: NSPEC_IMPI
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nspec_impi
     iaddr2 = iaddr1 + 1
     IF(ss%NSPEC_IMPI.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%SIMPI_name,kf1,kf2)
        call hashCode(ss%SIMPI_type,kf1,kf2)
        call hashCode(ss%q_SIMPI,kf1,kf2)
        call hashCode(ss%qatom_SIMPI,kf1,kf2)
        call hashCode(ss%m_SIMPI,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nspec_impi")
  ENDIF

  !----------------------
  ! dimension: NSPEC_RFMIN
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nspec_rfmin
     iaddr2 = iaddr1 + 1
     IF(ss%NSPEC_RFMIN.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%RFMIN_name,kf1,kf2)
        call hashCode(ss%RFMIN_type,kf1,kf2)
        call hashCode(ss%q_RFMIN,kf1,kf2)
        call hashCode(ss%qatom_RFMIN,kf1,kf2)
        call hashCode(ss%m_RFMIN,kf1,kf2)
     ENDIF

     ilock = ss%lock_shot_config
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nspec_rfmin")
  ENDIF

  !----------------------
  ! dimension: NSPEC_TH
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nspec_th
     iaddr2 = iaddr1 + 1
     IF(ss%NSPEC_TH.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%S_name,kf1,kf2)
        call hashCode(ss%S_type,kf1,kf2)
        call hashCode(ss%q_S,kf1,kf2)
        call hashCode(ss%qatom_S,kf1,kf2)
        call hashCode(ss%m_S,kf1,kf2)
     ENDIF

     ilock = ss%lock_shot_config
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nspec_th")
  ENDIF

  !----------------------
  ! dimension: NSPEC_THA
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nspec_tha
     iaddr2 = iaddr1 + 1
     IF(ss%NSPEC_THA.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%SA_name,kf1,kf2)
        call hashCode(ss%SA_type,kf1,kf2)
        call hashCode(ss%q_SA,kf1,kf2)
        call hashCode(ss%qatom_SA,kf1,kf2)
        call hashCode(ss%m_SA,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nspec_tha")
  ENDIF

  !----------------------
  ! dimension: NTH_EQ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nth_eq
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%th_eq)) THEN
        IF(MAXVAL(ABS(ss%th_eq)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%th_eq,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nth_eq")
  ENDIF

  !----------------------
  ! dimension: NTRCAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_ntrcat
     iaddr2 = iaddr1 + 1
     IF(ss%NTRCAT.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%trcat,kf1,kf2)
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","ntrcat")
  ENDIF

  !----------------------
  ! dimension: NUM_RZLIM
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_num_rzlim
     iaddr2 = iaddr1 + 1
     kf1 = ps_unInit; kf2 = ps_unInit
     IF(ss%NUM_RZLIM.NE.0) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%NUM_RZLIM,kf1,kf2)
     ENDIF

     ilock = ss%lock_machine_descr
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","num_rzlim")
  ENDIF

  !----------------------
  ! dimension: NXS
  IF(cclist(ps_cnum_RAD).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nxs
     iaddr2 = iaddr1 + 1
     IF(ss%NXS.EQ.0) THEN
        kf1 = ps_unInit; kf2 = ps_unInit
     ELSE
        kf1 = 17; kf2 = 23
        call hashCode(ss%xs_name,kf1,kf2)
     ENDIF

     ilock = ss%lock_shot_config
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nxs")
  ENDIF

  !----------------------
  ! dimension: NZ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nz
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Z_grid)) THEN
        IF(MAXVAL(ABS(ss%Z_grid)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Z_grid,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nz")
  ENDIF

  !----------------------
  ! dimension: NZ_RIP
  IF(cclist(ps_cnum_RIPPLE).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_nz_rip
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Z_ripple)) THEN
        IF(MAXVAL(ABS(ss%Z_ripple)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Z_ripple,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF

     ilock = ss%lock_sim_init
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","nz_rip")
  ENDIF

  !----------------------
  ! dimension: N_D_EINJ_STANDARD
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_n_d_einj_standard
     iaddr2 = iaddr1 + 1
     kf1 = ps_unInit; kf2 = ps_unInit
     IF(ss%N_D_EINJ_STANDARD.NE.0) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%N_D_EINJ_STANDARD,kf1,kf2)
     ENDIF

     ilock = ss%lock_machine_descr
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","n_d_einj_standard")
  ENDIF

  !----------------------
  ! dimension: N_H_EINJ_STANDARD
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_n_h_einj_standard
     iaddr2 = iaddr1 + 1
     kf1 = ps_unInit; kf2 = ps_unInit
     IF(ss%N_H_EINJ_STANDARD.NE.0) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%N_H_EINJ_STANDARD,kf1,kf2)
     ENDIF

     ilock = ss%lock_machine_descr
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","n_h_einj_standard")
  ENDIF

  !----------------------
  ! dimension: N_T_EINJ_STANDARD
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_dhash_n_t_einj_standard
     iaddr2 = iaddr1 + 1
     kf1 = ps_unInit; kf2 = ps_unInit
     IF(ss%N_T_EINJ_STANDARD.NE.0) THEN
        kf1 = 17; kf2 = 23
        call hashCode(ss%N_T_EINJ_STANDARD,kf1,kf2)
     ENDIF

     ilock = ss%lock_machine_descr
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

     CALL chk_hash("DIMENSION","n_t_einj_standard")
  ENDIF

  !--------------------------------------------
  ! profile hash codes

  !----------------------
  ! profile: ANOM_EVAR
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_anom_evar
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%anom_evar)) THEN
        IF(MAXVAL(ABS(ss%anom_evar)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%anom_evar,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: AREA
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_area
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%area)) THEN
        IF(MAXVAL(ABS(ss%area)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%area,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: BALSTAB
  IF(cclist(ps_cnum_LMHD).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_balstab
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%balstab)) THEN
        IF(MAXVAL(ABS(ss%balstab)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%balstab,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: BIDIFF_BPASS_CO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_bidiff_bpass_co
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%bidiff_bpass_co)) THEN
        IF(MAXVAL(ABS(ss%bidiff_bpass_co)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%bidiff_bpass_co,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: BIDIFF_BPASS_CTR
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_bidiff_bpass_ctr
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%bidiff_bpass_ctr)) THEN
        IF(MAXVAL(ABS(ss%bidiff_bpass_ctr)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%bidiff_bpass_ctr,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: BIDIFF_BTRAP
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_bidiff_btrap
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%bidiff_btrap)) THEN
        IF(MAXVAL(ABS(ss%bidiff_btrap)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%bidiff_btrap,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: BIDIFF_DPASS_CO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_bidiff_dpass_co
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%bidiff_dpass_co)) THEN
        IF(MAXVAL(ABS(ss%bidiff_dpass_co)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%bidiff_dpass_co,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: BIDIFF_DPASS_CTR
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_bidiff_dpass_ctr
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%bidiff_dpass_ctr)) THEN
        IF(MAXVAL(ABS(ss%bidiff_dpass_ctr)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%bidiff_dpass_ctr,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: BIDIFF_DTRAP
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_bidiff_dtrap
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%bidiff_dtrap)) THEN
        IF(MAXVAL(ABS(ss%bidiff_dtrap)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%bidiff_dtrap,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: BPHIRZ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_bphirz
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%BphiRZ)) THEN
        IF(MAXVAL(ABS(ss%BphiRZ)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%BphiRZ,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: BRRZ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_brrz
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%BRRZ)) THEN
        IF(MAXVAL(ABS(ss%BRRZ)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%BRRZ,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: BZRZ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_bzrz
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%BZRZ)) THEN
        IF(MAXVAL(ABS(ss%BZRZ)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%BZRZ,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: B_SURFMAX
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_b_surfmax
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%B_surfMax)) THEN
        IF(MAXVAL(ABS(ss%B_surfMax)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%B_surfMax,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: B_SURFMIN
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_b_surfmin
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%B_surfMin)) THEN
        IF(MAXVAL(ABS(ss%B_surfMin)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%B_surfMin,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CDICRF
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_cdicrf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%cdicrf)) THEN
        IF(MAXVAL(ABS(ss%cdicrf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%cdicrf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CDICRF_NPHI
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_cdicrf_nphi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%cdicrf_nphi)) THEN
        IF(MAXVAL(ABS(ss%cdicrf_nphi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%cdicrf_nphi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_anom)) THEN
        IF(MAXVAL(ABS(ss%chie_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_DRBM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_drbm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_drbm)) THEN
        IF(MAXVAL(ABS(ss%chie_drbm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_drbm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_ETG
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_etg
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_etg)) THEN
        IF(MAXVAL(ABS(ss%chie_etg)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_etg,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_GLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_glf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_glf)) THEN
        IF(MAXVAL(ABS(ss%chie_glf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_glf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_GTCNEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_gtcneo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_gtcneo)) THEN
        IF(MAXVAL(ABS(ss%chie_gtcneo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_gtcneo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_MMM71
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_mmm71
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_mmm71)) THEN
        IF(MAXVAL(ABS(ss%chie_mmm71)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_mmm71,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_MTM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_mtm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_mtm)) THEN
        IF(MAXVAL(ABS(ss%chie_mtm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_mtm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_NCLASS
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_nclass
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_nclass)) THEN
        IF(MAXVAL(ABS(ss%chie_nclass)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_nclass,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_NEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_neo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_neo)) THEN
        IF(MAXVAL(ABS(ss%chie_neo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_neo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_NTM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_ntm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_ntm)) THEN
        IF(MAXVAL(ABS(ss%chie_ntm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_ntm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_PALEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_paleo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_paleo)) THEN
        IF(MAXVAL(ABS(ss%chie_paleo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_paleo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_tglf)) THEN
        IF(MAXVAL(ABS(ss%chie_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_TRANS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_trans
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_trans)) THEN
        IF(MAXVAL(ABS(ss%chie_trans)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_trans,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_TRANSCAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_transcat
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_transcat)) THEN
        IF(MAXVAL(ABS(ss%chie_transcat)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_transcat,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIE_W19
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chie_w19
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chie_w19)) THEN
        IF(MAXVAL(ABS(ss%chie_w19)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chie_w19,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHII_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chii_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chii_anom)) THEN
        IF(MAXVAL(ABS(ss%chii_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chii_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHII_DRBM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chii_drbm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chii_drbm)) THEN
        IF(MAXVAL(ABS(ss%chii_drbm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chii_drbm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHII_GLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chii_glf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chii_glf)) THEN
        IF(MAXVAL(ABS(ss%chii_glf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chii_glf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHII_GTCNEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chii_gtcneo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chii_gtcneo)) THEN
        IF(MAXVAL(ABS(ss%chii_gtcneo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chii_gtcneo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHII_MMM71
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chii_mmm71
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chii_mmm71)) THEN
        IF(MAXVAL(ABS(ss%chii_mmm71)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chii_mmm71,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHII_NCLASS
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chii_nclass
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chii_nclass)) THEN
        IF(MAXVAL(ABS(ss%chii_nclass)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chii_nclass,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHII_NEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chii_neo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chii_neo)) THEN
        IF(MAXVAL(ABS(ss%chii_neo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chii_neo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHII_NTM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chii_ntm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chii_ntm)) THEN
        IF(MAXVAL(ABS(ss%chii_ntm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chii_ntm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHII_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chii_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chii_tglf)) THEN
        IF(MAXVAL(ABS(ss%chii_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chii_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHII_TRANS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chii_trans
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chii_trans)) THEN
        IF(MAXVAL(ABS(ss%chii_trans)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chii_trans,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHII_TRANSCAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chii_transcat
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chii_transcat)) THEN
        IF(MAXVAL(ABS(ss%chii_transcat)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chii_transcat,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHII_W19
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chii_w19
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chii_w19)) THEN
        IF(MAXVAL(ABS(ss%chii_w19)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chii_w19,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIMO_TRANS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chimo_trans
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chimo_trans)) THEN
        IF(MAXVAL(ABS(ss%chimo_trans)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chimo_trans,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIMO_TRANSCAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chimo_transcat
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chimo_transcat)) THEN
        IF(MAXVAL(ABS(ss%chimo_transcat)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chimo_transcat,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIPHI_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chiphi_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chiphi_anom)) THEN
        IF(MAXVAL(ABS(ss%chiphi_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chiphi_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIPHI_DRBM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chiphi_drbm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chiphi_drbm)) THEN
        IF(MAXVAL(ABS(ss%chiphi_drbm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chiphi_drbm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIPHI_GLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chiphi_glf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chiphi_glf)) THEN
        IF(MAXVAL(ABS(ss%chiphi_glf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chiphi_glf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIPHI_GTCNEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chiphi_gtcneo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chiphi_gtcneo)) THEN
        IF(MAXVAL(ABS(ss%chiphi_gtcneo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chiphi_gtcneo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIPHI_MMM71
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chiphi_mmm71
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chiphi_mmm71)) THEN
        IF(MAXVAL(ABS(ss%chiphi_mmm71)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chiphi_mmm71,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIPHI_NEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chiphi_neo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chiphi_neo)) THEN
        IF(MAXVAL(ABS(ss%chiphi_neo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chiphi_neo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIPHI_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chiphi_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chiphi_tglf)) THEN
        IF(MAXVAL(ABS(ss%chiphi_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chiphi_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHIPHI_W19
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chiphi_w19
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chiphi_w19)) THEN
        IF(MAXVAL(ABS(ss%chiphi_w19)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chiphi_w19,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CHITHETA_MMM71
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_chitheta_mmm71
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%chitheta_mmm71)) THEN
        IF(MAXVAL(ABS(ss%chitheta_mmm71)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%chitheta_mmm71,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CURBEAM
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_curbeam
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%curbeam)) THEN
        IF(MAXVAL(ABS(ss%curbeam)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%curbeam,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CURECH
  IF(cclist(ps_cnum_EC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_curech
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%curech)) THEN
        IF(MAXVAL(ABS(ss%curech)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%curech,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CURECH_SRC
  IF(cclist(ps_cnum_EC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_curech_src
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%curech_src)) THEN
        IF(MAXVAL(ABS(ss%curech_src)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%curech_src,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CURFUSN
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_curfusn
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%curfusn)) THEN
        IF(MAXVAL(ABS(ss%curfusn)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%curfusn,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CURICH
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_curich
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%curich)) THEN
        IF(MAXVAL(ABS(ss%curich)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%curich,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CURLH
  IF(cclist(ps_cnum_LH).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_curlh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%curlh)) THEN
        IF(MAXVAL(ABS(ss%curlh)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%curlh,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CURLH_SRC
  IF(cclist(ps_cnum_LH).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_curlh_src
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%curlh_src)) THEN
        IF(MAXVAL(ABS(ss%curlh_src)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%curlh_src,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CURMINO
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_curmino
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%curmino)) THEN
        IF(MAXVAL(ABS(ss%curmino)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%curmino,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CURR_BOOTSTRAP
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_curr_bootstrap
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%curr_bootstrap)) THEN
        IF(MAXVAL(ABS(ss%curr_bootstrap)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%curr_bootstrap,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CURR_OHMIC
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_curr_ohmic
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%curr_ohmic)) THEN
        IF(MAXVAL(ABS(ss%curr_ohmic)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%curr_ohmic,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CURT
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_curt
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%curt)) THEN
        IF(MAXVAL(ABS(ss%curt)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%curt,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: CUR_RW
  IF(cclist(ps_cnum_RUNAWAY).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_cur_rw
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%cur_rw)) THEN
        IF(MAXVAL(ABS(ss%cur_rw)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%cur_rw,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: D2R_GEO_DRHODTH
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_d2r_geo_drhodth
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%d2R_geo_dRHOdTH)) THEN
        IF(MAXVAL(ABS(ss%d2R_geo_dRHOdTH)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%d2R_geo_dRHOdTH,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: D2Z_GEO_DRHODTH
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_d2z_geo_drhodth
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%d2Z_geo_dRHOdTH)) THEN
        IF(MAXVAL(ABS(ss%d2Z_geo_dRHOdTH)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%d2Z_geo_dRHOdTH,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DB_MTM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_db_mtm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%db_mtm)) THEN
        IF(MAXVAL(ABS(ss%db_mtm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%db_mtm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DCUR_RW_DVLOOP
  IF(cclist(ps_cnum_RUNAWAY).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dcur_rw_dvloop
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dcur_rw_dVloop)) THEN
        IF(MAXVAL(ABS(ss%dcur_rw_dVloop)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dcur_rw_dVloop,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFB_FUSI
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difb_fusi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difb_fusi)) THEN
        IF(MAXVAL(ABS(ss%difb_fusi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difb_fusi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFB_NBI
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difb_nbi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difb_nbi)) THEN
        IF(MAXVAL(ABS(ss%difb_nbi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difb_nbi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFB_RFMI
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difb_rfmi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difb_rfmi)) THEN
        IF(MAXVAL(ABS(ss%difb_rfmi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difb_rfmi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFF_TRANS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_diff_trans
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%diff_trans)) THEN
        IF(MAXVAL(ABS(ss%diff_trans)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%diff_trans,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFF_TRANSCAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_diff_transcat
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%diff_transcat)) THEN
        IF(MAXVAL(ABS(ss%diff_transcat)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%diff_transcat,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFNE_NCLASS
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difne_nclass
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difne_nclass)) THEN
        IF(MAXVAL(ABS(ss%difne_nclass)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difne_nclass,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFNH_NCLASS
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difnh_nclass
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difnh_nclass)) THEN
        IF(MAXVAL(ABS(ss%difnh_nclass)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difnh_nclass,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFNS_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difns_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difns_anom)) THEN
        IF(MAXVAL(ABS(ss%difns_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difns_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFNS_DRBM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difns_drbm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difns_drbm)) THEN
        IF(MAXVAL(ABS(ss%difns_drbm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difns_drbm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFNS_GLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difns_glf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difns_glf)) THEN
        IF(MAXVAL(ABS(ss%difns_glf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difns_glf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFNS_GTCNEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difns_gtcneo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difns_gtcneo)) THEN
        IF(MAXVAL(ABS(ss%difns_gtcneo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difns_gtcneo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFNS_MMM71
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difns_mmm71
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difns_mmm71)) THEN
        IF(MAXVAL(ABS(ss%difns_mmm71)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difns_mmm71,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFNS_NCLASS
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difns_nclass
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difns_nclass)) THEN
        IF(MAXVAL(ABS(ss%difns_nclass)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difns_nclass,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFNS_NEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difns_neo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difns_neo)) THEN
        IF(MAXVAL(ABS(ss%difns_neo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difns_neo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFNS_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difns_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difns_tglf)) THEN
        IF(MAXVAL(ABS(ss%difns_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difns_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFNS_W19
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difns_w19
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difns_w19)) THEN
        IF(MAXVAL(ABS(ss%difns_w19)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difns_w19,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFNX_NCLASS
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difnx_nclass
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difnx_nclass)) THEN
        IF(MAXVAL(ABS(ss%difnx_nclass)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difnx_nclass,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DIFNZ_MMM71
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_difnz_mmm71
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%difnz_mmm71)) THEN
        IF(MAXVAL(ABS(ss%difnz_mmm71)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%difnz_mmm71,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DQEDGRDTE_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dqedgrdte_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dQedGrdte_tglf)) THEN
        IF(MAXVAL(ABS(ss%dQedGrdte_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dQedGrdte_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DQEDGRDTI_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dqedgrdti_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dQedGrdti_tglf)) THEN
        IF(MAXVAL(ABS(ss%dQedGrdti_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dQedGrdti_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DQIDGRDTE_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dqidgrdte_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dQidGrdte_tglf)) THEN
        IF(MAXVAL(ABS(ss%dQidGrdte_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dQidGrdte_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DQIDGRDTI_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dqidgrdti_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dQidGrdti_tglf)) THEN
        IF(MAXVAL(ABS(ss%dQidGrdti_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dQidGrdti_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DR0_MOMEQ_DRHO
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dr0_momeq_drho
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dR0_momeq_dRHO)) THEN
        IF(MAXVAL(ABS(ss%dR0_momeq_dRHO)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dR0_momeq_dRHO,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DR_GEO_DRHO
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dr_geo_drho
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dR_geo_dRHO)) THEN
        IF(MAXVAL(ABS(ss%dR_geo_dRHO)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dR_geo_dRHO,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DR_GEO_DTH
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dr_geo_dth
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dR_geo_dTH)) THEN
        IF(MAXVAL(ABS(ss%dR_geo_dTH)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dR_geo_dTH,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DXRJCOS_MOMEQ_DRHO
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dxrjcos_momeq_drho
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dxRjcos_momeq_dRHO)) THEN
        IF(MAXVAL(ABS(ss%dxRjcos_momeq_dRHO)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dxRjcos_momeq_dRHO,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DXRJSIN_MOMEQ_DRHO
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dxrjsin_momeq_drho
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dxRjsin_momeq_dRHO)) THEN
        IF(MAXVAL(ABS(ss%dxRjsin_momeq_dRHO)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dxRjsin_momeq_dRHO,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DXZJCOS_MOMEQ_DRHO
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dxzjcos_momeq_drho
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dxZjcos_momeq_dRHO)) THEN
        IF(MAXVAL(ABS(ss%dxZjcos_momeq_dRHO)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dxZjcos_momeq_dRHO,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DXZJSIN_MOMEQ_DRHO
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dxzjsin_momeq_drho
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dxZjsin_momeq_dRHO)) THEN
        IF(MAXVAL(ABS(ss%dxZjsin_momeq_dRHO)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dxZjsin_momeq_dRHO,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DZ0_MOMEQ_DRHO
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dz0_momeq_drho
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dZ0_momeq_dRHO)) THEN
        IF(MAXVAL(ABS(ss%dZ0_momeq_dRHO)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dZ0_momeq_dRHO,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DZ_GEO_DRHO
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dz_geo_drho
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dZ_geo_dRHO)) THEN
        IF(MAXVAL(ABS(ss%dZ_geo_dRHO)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dZ_geo_dRHO,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: DZ_GEO_DTH
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_dz_geo_dth
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%dZ_geo_dTH)) THEN
        IF(MAXVAL(ABS(ss%dZ_geo_dTH)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%dZ_geo_dTH,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: EE_MOBILITY_FACTOR
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_ee_mobility_factor
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ee_mobility_factor)) THEN
        IF(MAXVAL(ABS(ss%ee_mobility_factor)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%ee_mobility_factor,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: ELONG
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_elong
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%elong)) THEN
        IF(MAXVAL(ABS(ss%elong)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%elong,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: EPERP_BEAMI
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_eperp_beami
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%eperp_beami)) THEN
        IF(MAXVAL(ABS(ss%eperp_beami)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%eperp_beami,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: EPERP_FUSI
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_eperp_fusi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%eperp_fusi)) THEN
        IF(MAXVAL(ABS(ss%eperp_fusi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%eperp_fusi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: EPERP_MINI
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_eperp_mini
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%eperp_mini)) THEN
        IF(MAXVAL(ABS(ss%eperp_mini)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%eperp_mini,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: EPERP_RW
  IF(cclist(ps_cnum_RUNAWAY).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_eperp_rw
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%eperp_rw)) THEN
        IF(MAXVAL(ABS(ss%eperp_rw)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%eperp_rw,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: EPLL_BEAMI
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_epll_beami
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%epll_beami)) THEN
        IF(MAXVAL(ABS(ss%epll_beami)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%epll_beami,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: EPLL_FUSI
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_epll_fusi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%epll_fusi)) THEN
        IF(MAXVAL(ABS(ss%epll_fusi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%epll_fusi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: EPLL_MINI
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_epll_mini
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%epll_mini)) THEN
        IF(MAXVAL(ABS(ss%epll_mini)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%epll_mini,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: EPLL_RW
  IF(cclist(ps_cnum_RUNAWAY).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_epll_rw
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%epll_rw)) THEN
        IF(MAXVAL(ABS(ss%epll_rw)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%epll_rw,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: EPOT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_epot
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Epot)) THEN
        IF(MAXVAL(ABS(ss%Epot)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Epot,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: ETA_PARALLEL
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_eta_parallel
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%eta_parallel)) THEN
        IF(MAXVAL(ABS(ss%eta_parallel)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%eta_parallel,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FIDIFF_BPASS_CO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_fidiff_bpass_co
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%fidiff_bpass_co)) THEN
        IF(MAXVAL(ABS(ss%fidiff_bpass_co)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%fidiff_bpass_co,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FIDIFF_BPASS_CTR
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_fidiff_bpass_ctr
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%fidiff_bpass_ctr)) THEN
        IF(MAXVAL(ABS(ss%fidiff_bpass_ctr)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%fidiff_bpass_ctr,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FIDIFF_BTRAP
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_fidiff_btrap
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%fidiff_btrap)) THEN
        IF(MAXVAL(ABS(ss%fidiff_btrap)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%fidiff_btrap,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FIDIFF_DPASS_CO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_fidiff_dpass_co
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%fidiff_dpass_co)) THEN
        IF(MAXVAL(ABS(ss%fidiff_dpass_co)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%fidiff_dpass_co,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FIDIFF_DPASS_CTR
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_fidiff_dpass_ctr
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%fidiff_dpass_ctr)) THEN
        IF(MAXVAL(ABS(ss%fidiff_dpass_ctr)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%fidiff_dpass_ctr,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FIDIFF_DTRAP
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_fidiff_dtrap
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%fidiff_dtrap)) THEN
        IF(MAXVAL(ABS(ss%fidiff_dtrap)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%fidiff_dtrap,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FI_DEPLETION
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_fi_depletion
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Fi_depletion)) THEN
        IF(MAXVAL(ABS(ss%Fi_depletion)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Fi_depletion,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FREQ_DRBM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_freq_drbm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%freq_drbm)) THEN
        IF(MAXVAL(ABS(ss%freq_drbm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%freq_drbm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FREQ_ETG
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_freq_etg
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%freq_etg)) THEN
        IF(MAXVAL(ABS(ss%freq_etg)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%freq_etg,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FREQ_GLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_freq_glf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%freq_glf)) THEN
        IF(MAXVAL(ABS(ss%freq_glf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%freq_glf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FREQ_MAX_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_freq_max_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%freq_max_tglf)) THEN
        IF(MAXVAL(ABS(ss%freq_max_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%freq_max_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FREQ_MMM71
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_freq_mmm71
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%freq_mmm71)) THEN
        IF(MAXVAL(ABS(ss%freq_mmm71)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%freq_mmm71,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FREQ_MTM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_freq_mtm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%freq_mtm)) THEN
        IF(MAXVAL(ABS(ss%freq_mtm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%freq_mtm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: FREQ_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_freq_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%freq_tglf)) THEN
        IF(MAXVAL(ABS(ss%freq_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%freq_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GAMMA_NC
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_gamma_nc
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gamma_nc)) THEN
        IF(MAXVAL(ABS(ss%gamma_nc)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%gamma_nc,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GB1
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_gb1
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gb1)) THEN
        IF(MAXVAL(ABS(ss%gb1)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%gb1,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GB2
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_gb2
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gb2)) THEN
        IF(MAXVAL(ABS(ss%gb2)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%gb2,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GB2I
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_gb2i
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gb2i)) THEN
        IF(MAXVAL(ABS(ss%gb2i)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%gb2i,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GBR2
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_gbr2
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gbr2)) THEN
        IF(MAXVAL(ABS(ss%gbr2)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%gbr2,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GNCFB2H
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_gncfb2h
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gncfb2h)) THEN
        IF(MAXVAL(ABS(ss%gncfb2h)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%gncfb2h,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GNCFH
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_gncfh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gncfh)) THEN
        IF(MAXVAL(ABS(ss%gncfh)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%gncfh,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GR1
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_gr1
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gr1)) THEN
        IF(MAXVAL(ABS(ss%gr1)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%gr1,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GR2
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_gr2
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gr2)) THEN
        IF(MAXVAL(ABS(ss%gr2)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%gr2,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GR2I
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_gr2i
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gr2i)) THEN
        IF(MAXVAL(ABS(ss%gr2i)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%gr2i,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GR2RHO2
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_gr2rho2
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gr2rho2)) THEN
        IF(MAXVAL(ABS(ss%gr2rho2)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%gr2rho2,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GR3I
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_gr3i
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gr3i)) THEN
        IF(MAXVAL(ABS(ss%gr3i)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%gr3i,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GRHO1
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_grho1
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%grho1)) THEN
        IF(MAXVAL(ABS(ss%grho1)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%grho1,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GRHO2
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_grho2
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%grho2)) THEN
        IF(MAXVAL(ABS(ss%grho2)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%grho2,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GRHO2B2I
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_grho2b2i
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%grho2b2i)) THEN
        IF(MAXVAL(ABS(ss%grho2b2i)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%grho2b2i,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GRHO2R2I
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_grho2r2i
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%grho2r2i)) THEN
        IF(MAXVAL(ABS(ss%grho2r2i)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%grho2r2i,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GRHO2R3I
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_grho2r3i
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%grho2r3i)) THEN
        IF(MAXVAL(ABS(ss%grho2r3i)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%grho2r3i,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GRI
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_gri
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%gri)) THEN
        IF(MAXVAL(ABS(ss%gri)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%gri,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GRIRHOI
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_grirhoi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%grirhoi)) THEN
        IF(MAXVAL(ABS(ss%grirhoi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%grirhoi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GROWTHRATE_DRBM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_growthrate_drbm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%growthrate_drbm)) THEN
        IF(MAXVAL(ABS(ss%growthrate_drbm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%growthrate_drbm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GROWTHRATE_ETG
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_growthrate_etg
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%growthrate_etg)) THEN
        IF(MAXVAL(ABS(ss%growthrate_etg)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%growthrate_etg,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GROWTHRATE_GLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_growthrate_glf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%growthrate_glf)) THEN
        IF(MAXVAL(ABS(ss%growthrate_glf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%growthrate_glf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GROWTHRATE_MAX_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_growthrate_max_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%growthrate_max_tglf)) THEN
        IF(MAXVAL(ABS(ss%growthrate_max_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%growthrate_max_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GROWTHRATE_MMM71
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_growthrate_mmm71
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%growthrate_mmm71)) THEN
        IF(MAXVAL(ABS(ss%growthrate_mmm71)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%growthrate_mmm71,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GROWTHRATE_MTM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_growthrate_mtm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%growthrate_mtm)) THEN
        IF(MAXVAL(ABS(ss%growthrate_mtm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%growthrate_mtm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GROWTHRATE_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_growthrate_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%growthrate_tglf)) THEN
        IF(MAXVAL(ABS(ss%growthrate_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%growthrate_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: GROWTHRATE_VPAR
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_growthrate_vpar
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%growthrate_vpar)) THEN
        IF(MAXVAL(ABS(ss%growthrate_vpar)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%growthrate_vpar,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: G_EQ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_g_eq
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%g_eq)) THEN
        IF(MAXVAL(ABS(ss%g_eq)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%g_eq,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: IOTA
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_iota
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%iota)) THEN
        IF(MAXVAL(ABS(ss%iota)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%iota,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: JDOTB
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_jdotb
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%jdotb)) THEN
        IF(MAXVAL(ABS(ss%jdotb)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%jdotb,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: KYRHOS_DRBM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_kyrhos_drbm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%kyrhos_drbm)) THEN
        IF(MAXVAL(ABS(ss%kyrhos_drbm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%kyrhos_drbm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: KYRHOS_ETG
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_kyrhos_etg
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%kyrhos_etg)) THEN
        IF(MAXVAL(ABS(ss%kyrhos_etg)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%kyrhos_etg,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: KYRHOS_MTM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_kyrhos_mtm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%kyrhos_mtm)) THEN
        IF(MAXVAL(ABS(ss%kyrhos_mtm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%kyrhos_mtm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: KYSPECTRUM_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_kyspectrum_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%kyspectrum_tglf)) THEN
        IF(MAXVAL(ABS(ss%kyspectrum_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%kyspectrum_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: LPOL
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_lpol
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Lpol)) THEN
        IF(MAXVAL(ABS(ss%Lpol)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Lpol,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: N0NORM
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_n0norm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%n0norm)) THEN
        IF(MAXVAL(ABS(ss%n0norm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%n0norm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: N0_HALO
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_n0_halo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%n0_halo)) THEN
        IF(MAXVAL(ABS(ss%n0_halo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%n0_halo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: N0_RECO
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_n0_reco
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%n0_reco)) THEN
        IF(MAXVAL(ABS(ss%n0_reco)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%n0_reco,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NBEAMI
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_nbeami
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nbeami)) THEN
        IF(MAXVAL(ABS(ss%nbeami)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%nbeami,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NEFLUX_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_neflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%neflux_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%neflux_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%neflux_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NE_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_ne_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ne_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%ne_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%ne_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NFUSI
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_nfusi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nfusi)) THEN
        IF(MAXVAL(ABS(ss%nfusi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%nfusi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NGRADB2_AV
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_ngradb2_av
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ngradb2_av)) THEN
        IF(MAXVAL(ABS(ss%ngradb2_av)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%ngradb2_av,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NI
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_ni
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ni)) THEN
        IF(MAXVAL(ABS(ss%ni)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%ni,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NIFLUX_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_niflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%niflux_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%niflux_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%niflux_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NI_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_ni_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ni_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%ni_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%ni_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NMINI
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_nmini
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nmini)) THEN
        IF(MAXVAL(ABS(ss%nmini)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%nmini,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NMODEL
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_nmodel
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nmodel)) THEN
        IF(MAXVAL(ABS(ss%nmodel)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%nmodel,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NRW
  IF(cclist(ps_cnum_RUNAWAY).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_nrw
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nrw)) THEN
        IF(MAXVAL(ABS(ss%nrw)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%nrw,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_ns
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ns)) THEN
        IF(MAXVAL(ABS(ss%ns)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%ns,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NZFLUX_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_nzflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nzflux_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%nzflux_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%nzflux_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: NZ_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_nz_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%nz_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%nz_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%nz_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: OMEG0CX
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_omeg0cx
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%omeg0cx)) THEN
        IF(MAXVAL(ABS(ss%omeg0cx)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%omeg0cx,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: OMEG0SC0
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_omeg0sc0
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%omeg0sc0)) THEN
        IF(MAXVAL(ABS(ss%omeg0sc0)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%omeg0sc0,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: OMEG0_HALO
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_omeg0_halo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%omeg0_halo)) THEN
        IF(MAXVAL(ABS(ss%omeg0_halo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%omeg0_halo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: OMEG0_RECO
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_omeg0_reco
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%omeg0_reco)) THEN
        IF(MAXVAL(ABS(ss%omeg0_reco)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%omeg0_reco,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: OMEGAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_omegat
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%omegat)) THEN
        IF(MAXVAL(ABS(ss%omegat)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%omegat,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: OMEGA_EXB
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_omega_exb
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%omega_exb)) THEN
        IF(MAXVAL(ABS(ss%omega_exb)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%omega_exb,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: P0_RECO
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_p0_reco
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%p0_reco)) THEN
        IF(MAXVAL(ABS(ss%p0_reco)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%p0_reco,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PB0_HALO
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pb0_halo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pb0_halo)) THEN
        IF(MAXVAL(ABS(ss%pb0_halo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pb0_halo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PBE
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pbe
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pbe)) THEN
        IF(MAXVAL(ABS(ss%pbe)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pbe,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PBI
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pbi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pbi)) THEN
        IF(MAXVAL(ABS(ss%pbi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pbi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PBTH
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pbth
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pbth)) THEN
        IF(MAXVAL(ABS(ss%pbth)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pbth,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PCX_HALO
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pcx_halo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pcx_halo)) THEN
        IF(MAXVAL(ABS(ss%pcx_halo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pcx_halo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PCX_RECO
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pcx_reco
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pcx_reco)) THEN
        IF(MAXVAL(ABS(ss%pcx_reco)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pcx_reco,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PEECH
  IF(cclist(ps_cnum_EC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_peech
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%peech)) THEN
        IF(MAXVAL(ABS(ss%peech)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%peech,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PEECH_SRC
  IF(cclist(ps_cnum_EC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_peech_src
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%peech_src)) THEN
        IF(MAXVAL(ABS(ss%peech_src)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%peech_src,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PELH
  IF(cclist(ps_cnum_LH).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pelh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pelh)) THEN
        IF(MAXVAL(ABS(ss%pelh)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pelh,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PELH_SRC
  IF(cclist(ps_cnum_LH).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pelh_src
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pelh_src)) THEN
        IF(MAXVAL(ABS(ss%pelh_src)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pelh_src,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PE_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pe_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pe_anom)) THEN
        IF(MAXVAL(ABS(ss%pe_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pe_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PE_TRANS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pe_trans
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pe_trans)) THEN
        IF(MAXVAL(ABS(ss%pe_trans)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pe_trans,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PE_TRANSCAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pe_transcat
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pe_transcat)) THEN
        IF(MAXVAL(ABS(ss%pe_transcat)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pe_transcat,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PFUSE
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pfuse
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pfuse)) THEN
        IF(MAXVAL(ABS(ss%pfuse)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pfuse,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PFUSI
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pfusi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pfusi)) THEN
        IF(MAXVAL(ABS(ss%pfusi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pfusi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PFUSTH
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pfusth
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pfusth)) THEN
        IF(MAXVAL(ABS(ss%pfusth)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pfusth,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PHIFLUX_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_phiflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%phiflux_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%phiflux_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%phiflux_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PHIT
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_phit
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%phit)) THEN
        IF(MAXVAL(ABS(ss%phit)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%phit,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PICRF_NPHI_SRCS
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_picrf_nphi_srcs
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%picrf_nphi_srcs)) THEN
        IF(MAXVAL(ABS(ss%picrf_nphi_srcs)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%picrf_nphi_srcs,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PICRF_SRCS
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_picrf_srcs
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%picrf_srcs)) THEN
        IF(MAXVAL(ABS(ss%picrf_srcs)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%picrf_srcs,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PICRF_TOTALS
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_picrf_totals
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%picrf_totals)) THEN
        IF(MAXVAL(ABS(ss%picrf_totals)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%picrf_totals,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PICTH
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_picth
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%picth)) THEN
        IF(MAXVAL(ABS(ss%picth)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%picth,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PILH
  IF(cclist(ps_cnum_LH).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pilh
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pilh)) THEN
        IF(MAXVAL(ABS(ss%pilh)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pilh,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PILH_SRC
  IF(cclist(ps_cnum_LH).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pilh_src
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pilh_src)) THEN
        IF(MAXVAL(ABS(ss%pilh_src)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pilh_src,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PI_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pi_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pi_anom)) THEN
        IF(MAXVAL(ABS(ss%pi_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pi_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PI_TRANS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pi_trans
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pi_trans)) THEN
        IF(MAXVAL(ABS(ss%pi_trans)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pi_trans,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PI_TRANSCAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pi_transcat
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pi_transcat)) THEN
        IF(MAXVAL(ABS(ss%pi_transcat)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pi_transcat,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PMINE
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pmine
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pmine)) THEN
        IF(MAXVAL(ABS(ss%pmine)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pmine,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PMINI
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pmini
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pmini)) THEN
        IF(MAXVAL(ABS(ss%pmini)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pmini,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PMINTH
  IF(cclist(ps_cnum_IC).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pminth
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pminth)) THEN
        IF(MAXVAL(ABS(ss%pminth)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pminth,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: POHME
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pohme
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pohme)) THEN
        IF(MAXVAL(ABS(ss%pohme)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pohme,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: POT_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_pot_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%pot_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%pot_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%pot_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PRAD
  IF(cclist(ps_cnum_RAD).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_prad
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%prad)) THEN
        IF(MAXVAL(ABS(ss%prad)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%prad,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PRAD_BR
  IF(cclist(ps_cnum_RAD).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_prad_br
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%prad_br)) THEN
        IF(MAXVAL(ABS(ss%prad_br)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%prad_br,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PRAD_CY
  IF(cclist(ps_cnum_RAD).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_prad_cy
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%prad_cy)) THEN
        IF(MAXVAL(ABS(ss%prad_cy)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%prad_cy,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PRAD_LI
  IF(cclist(ps_cnum_RAD).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_prad_li
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%prad_li)) THEN
        IF(MAXVAL(ABS(ss%prad_li)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%prad_li,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PSC_HALO
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_psc_halo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%psc_halo)) THEN
        IF(MAXVAL(ABS(ss%psc_halo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%psc_halo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PSC_RECO
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_psc_reco
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%psc_reco)) THEN
        IF(MAXVAL(ABS(ss%psc_reco)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%psc_reco,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PSIPOL
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_psipol
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%psipol)) THEN
        IF(MAXVAL(ABS(ss%psipol)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%psipol,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PSIRZ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_psirz
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%PsiRZ)) THEN
        IF(MAXVAL(ABS(ss%PsiRZ)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%PsiRZ,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PSMOM_ERRCK
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_psmom_errck
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%psmom_errck)) THEN
        IF(MAXVAL(ABS(ss%psmom_errck)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%psmom_errck,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: PSMOM_NC
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_psmom_nc
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%psmom_nc)) THEN
        IF(MAXVAL(ABS(ss%psmom_nc)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%psmom_nc,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: P_EQ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_p_eq
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%P_eq)) THEN
        IF(MAXVAL(ABS(ss%P_eq)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%P_eq,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: QBEAMI
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_qbeami
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%qbeami)) THEN
        IF(MAXVAL(ABS(ss%qbeami)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%qbeami,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: QCOMP_E
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_qcomp_e
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%qcomp_e)) THEN
        IF(MAXVAL(ABS(ss%qcomp_e)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%qcomp_e,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: QCOMP_I
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_qcomp_i
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%qcomp_i)) THEN
        IF(MAXVAL(ABS(ss%qcomp_i)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%qcomp_i,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: QIE
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_qie
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%qie)) THEN
        IF(MAXVAL(ABS(ss%qie)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%qie,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: QIONIZ
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_qioniz
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%qioniz)) THEN
        IF(MAXVAL(ABS(ss%qioniz)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%qioniz,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: QQCX
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_qqcx
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%qqcx)) THEN
        IF(MAXVAL(ABS(ss%qqcx)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%qqcx,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: QROT_CONV
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_qrot_conv
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%qrot_conv)) THEN
        IF(MAXVAL(ABS(ss%qrot_conv)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%qrot_conv,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: QROT_DIFF
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_qrot_diff
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%qrot_diff)) THEN
        IF(MAXVAL(ABS(ss%qrot_diff)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%qrot_diff,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: Q_EQ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_q_eq
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%q_eq)) THEN
        IF(MAXVAL(ABS(ss%q_eq)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%q_eq,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: R0_MOMEQ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_r0_momeq
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%R0_momeq)) THEN
        IF(MAXVAL(ABS(ss%R0_momeq)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%R0_momeq,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: RATE_SINB0I
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_rate_sinb0i
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rate_sinb0i)) THEN
        IF(MAXVAL(ABS(ss%rate_sinb0i)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rate_sinb0i,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: RATE_SINB0X
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_rate_sinb0x
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rate_sinb0x)) THEN
        IF(MAXVAL(ABS(ss%rate_sinb0x)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rate_sinb0x,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: RATE_SINB0XS
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_rate_sinb0xs
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rate_sinb0xs)) THEN
        IF(MAXVAL(ABS(ss%rate_sinb0xs)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rate_sinb0xs,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: RATE_SINF0I
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_rate_sinf0i
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rate_sinf0i)) THEN
        IF(MAXVAL(ABS(ss%rate_sinf0i)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rate_sinf0i,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: RATE_SINF0X
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_rate_sinf0x
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rate_sinf0x)) THEN
        IF(MAXVAL(ABS(ss%rate_sinf0x)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rate_sinf0x,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: RATE_SINF0XS
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_rate_sinf0xs
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rate_sinf0xs)) THEN
        IF(MAXVAL(ABS(ss%rate_sinf0xs)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rate_sinf0xs,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: RES_SN
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_res_sn
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%res_sn)) THEN
        IF(MAXVAL(ABS(ss%res_sn)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%res_sn,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: RES_TE
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_res_te
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%res_te)) THEN
        IF(MAXVAL(ABS(ss%res_te)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%res_te,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: RES_TI
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_res_ti
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%res_ti)) THEN
        IF(MAXVAL(ABS(ss%res_ti)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%res_ti,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: RES_TQ
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_res_tq
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%res_tq)) THEN
        IF(MAXVAL(ABS(ss%res_tq)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%res_tq,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: RMAJOR_MEAN
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_rmajor_mean
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Rmajor_mean)) THEN
        IF(MAXVAL(ABS(ss%Rmajor_mean)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Rmajor_mean,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: RMINOR_MEAN
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_rminor_mean
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%rMinor_mean)) THEN
        IF(MAXVAL(ABS(ss%rMinor_mean)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%rMinor_mean,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: R_GEO
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_r_geo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%R_geo)) THEN
        IF(MAXVAL(ABS(ss%R_geo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%R_geo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: R_MIDP_IN
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_r_midp_in
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%R_midp_in)) THEN
        IF(MAXVAL(ABS(ss%R_midp_in)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%R_midp_in,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: R_MIDP_OUT
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_r_midp_out
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%R_midp_out)) THEN
        IF(MAXVAL(ABS(ss%R_midp_out)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%R_midp_out,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: R_SURFMAX
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_r_surfmax
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%R_surfMax)) THEN
        IF(MAXVAL(ABS(ss%R_surfMax)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%R_surfMax,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: R_SURFMIN
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_r_surfmin
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%R_surfMin)) THEN
        IF(MAXVAL(ABS(ss%R_surfMin)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%R_surfMin,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: S0RECO
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_s0reco
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%s0reco)) THEN
        IF(MAXVAL(ABS(ss%s0reco)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%s0reco,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: S0RECO_E
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_s0reco_e
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%s0reco_e)) THEN
        IF(MAXVAL(ABS(ss%s0reco_e)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%s0reco_e,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: S0RECO_RECAP
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_s0reco_recap
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%s0reco_recap)) THEN
        IF(MAXVAL(ABS(ss%s0reco_recap)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%s0reco_recap,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SB0HALO
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sb0halo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sb0halo)) THEN
        IF(MAXVAL(ABS(ss%sb0halo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sb0halo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SB0HALO_RECAP
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sb0halo_recap
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sb0halo_recap)) THEN
        IF(MAXVAL(ABS(ss%sb0halo_recap)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sb0halo_recap,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SBEDEP
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sbedep
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sbedep)) THEN
        IF(MAXVAL(ABS(ss%sbedep)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sbedep,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SBEHALO
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sbehalo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sbehalo)) THEN
        IF(MAXVAL(ABS(ss%sbehalo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sbehalo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SBSCE
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sbsce
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sbsce)) THEN
        IF(MAXVAL(ABS(ss%sbsce)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sbsce,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SBTHERM
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sbtherm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sbtherm)) THEN
        IF(MAXVAL(ABS(ss%sbtherm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sbtherm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SC_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sc_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sc_anom)) THEN
        IF(MAXVAL(ABS(ss%sc_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sc_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SFSCE
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sfsce
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sfsce)) THEN
        IF(MAXVAL(ABS(ss%sfsce)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sfsce,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SFTHERM
  IF(cclist(ps_cnum_FUS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sftherm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sftherm)) THEN
        IF(MAXVAL(ABS(ss%sftherm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sftherm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SII_XS
  IF(cclist(ps_cnum_RAD).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sii_xs
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sii_xs)) THEN
        IF(MAXVAL(ABS(ss%sii_xs)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sii_xs,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SN_TRANS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sn_trans
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sn_trans)) THEN
        IF(MAXVAL(ABS(ss%sn_trans)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sn_trans,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SN_TRANSCAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sn_transcat
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sn_transcat)) THEN
        IF(MAXVAL(ABS(ss%sn_transcat)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sn_transcat,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SPROF0
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sprof0
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sprof0)) THEN
        IF(MAXVAL(ABS(ss%sprof0)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sprof0,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SPROF0E
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_sprof0e
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%sprof0e)) THEN
        IF(MAXVAL(ABS(ss%sprof0e)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%sprof0e,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SPSCE
  IF(cclist(ps_cnum_PEL).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_spsce
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%spsce)) THEN
        IF(MAXVAL(ABS(ss%spsce)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%spsce,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SQUARELO
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_squarelo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%squareLO)) THEN
        IF(MAXVAL(ABS(ss%squareLO)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%squareLO,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SQUAREUO
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_squareuo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%squareUO)) THEN
        IF(MAXVAL(ABS(ss%squareUO)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%squareUO,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: SURF
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_surf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%surf)) THEN
        IF(MAXVAL(ABS(ss%surf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%surf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: T0CX
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_t0cx
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%T0cx)) THEN
        IF(MAXVAL(ABS(ss%T0cx)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%T0cx,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: T0SC0
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_t0sc0
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%T0sc0)) THEN
        IF(MAXVAL(ABS(ss%T0sc0)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%T0sc0,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: T0_HALO
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_t0_halo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%T0_halo)) THEN
        IF(MAXVAL(ABS(ss%T0_halo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%T0_halo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: T0_RECO
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_t0_reco
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%T0_reco)) THEN
        IF(MAXVAL(ABS(ss%T0_reco)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%T0_reco,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TEFLUX_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_teflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%teflux_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%teflux_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%teflux_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TE_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_te_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%te_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%te_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%te_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TFRIP2_LOG
  IF(cclist(ps_cnum_RIPPLE).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tfrip2_log
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tfrip2_log)) THEN
        IF(MAXVAL(ABS(ss%tfrip2_log)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tfrip2_log,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TFRIP_LOG
  IF(cclist(ps_cnum_RIPPLE).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tfrip_log
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tfrip_log)) THEN
        IF(MAXVAL(ABS(ss%tfrip_log)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tfrip_log,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TI
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_ti
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Ti)) THEN
        IF(MAXVAL(ABS(ss%Ti)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Ti,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TIFLUX_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tiflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tiflux_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%tiflux_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tiflux_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TI_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_ti_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%ti_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%ti_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%ti_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TMODEL
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tmodel
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tmodel)) THEN
        IF(MAXVAL(ABS(ss%tmodel)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tmodel,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQ0_RECO
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tq0_reco
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tq0_reco)) THEN
        IF(MAXVAL(ABS(ss%tq0_reco)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tq0_reco,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQB0_HALO
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tqb0_halo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tqb0_halo)) THEN
        IF(MAXVAL(ABS(ss%tqb0_halo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tqb0_halo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQBE
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tqbe
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tqbe)) THEN
        IF(MAXVAL(ABS(ss%tqbe)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tqbe,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQBI
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tqbi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tqbi)) THEN
        IF(MAXVAL(ABS(ss%tqbi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tqbi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQBJXB
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tqbjxb
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tqbjxb)) THEN
        IF(MAXVAL(ABS(ss%tqbjxb)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tqbjxb,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQBTH
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tqbth
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tqbth)) THEN
        IF(MAXVAL(ABS(ss%tqbth)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tqbth,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQCX_HALO
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tqcx_halo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tqcx_halo)) THEN
        IF(MAXVAL(ABS(ss%tqcx_halo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tqcx_halo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQCX_RECO
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tqcx_reco
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tqcx_reco)) THEN
        IF(MAXVAL(ABS(ss%tqcx_reco)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tqcx_reco,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQIONIZ
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tqioniz
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tqioniz)) THEN
        IF(MAXVAL(ABS(ss%tqioniz)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tqioniz,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQQCX
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tqqcx
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tqqcx)) THEN
        IF(MAXVAL(ABS(ss%tqqcx)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tqqcx,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQSC_HALO
  IF(cclist(ps_cnum_NBI).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tqsc_halo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tqsc_halo)) THEN
        IF(MAXVAL(ABS(ss%tqsc_halo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tqsc_halo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQSC_RECO
  IF(cclist(ps_cnum_GAS).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tqsc_reco
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tqsc_reco)) THEN
        IF(MAXVAL(ABS(ss%tqsc_reco)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tqsc_reco,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQ_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tq_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tq_anom)) THEN
        IF(MAXVAL(ABS(ss%tq_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tq_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQ_TRANS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tq_trans
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tq_trans)) THEN
        IF(MAXVAL(ABS(ss%tq_trans)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tq_trans,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TQ_TRANSCAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tq_transcat
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tq_transcat)) THEN
        IF(MAXVAL(ABS(ss%tq_transcat)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tq_transcat,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TRIANG
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_triang
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%triang)) THEN
        IF(MAXVAL(ABS(ss%triang)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%triang,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TRIANGL
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_triangl
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%triangL)) THEN
        IF(MAXVAL(ABS(ss%triangL)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%triangL,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TRIANGU
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_triangu
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%triangU)) THEN
        IF(MAXVAL(ABS(ss%triangU)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%triangU,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TRIANG_MILLER_L
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_triang_miller_l
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%triang_miller_L)) THEN
        IF(MAXVAL(ABS(ss%triang_miller_L)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%triang_miller_L,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TRIANG_MILLER_U
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_triang_miller_u
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%triang_miller_U)) THEN
        IF(MAXVAL(ABS(ss%triang_miller_U)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%triang_miller_U,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_ts
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Ts)) THEN
        IF(MAXVAL(ABS(ss%Ts)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Ts,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TZFLUX_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tzflux_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tzflux_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%tzflux_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tzflux_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: TZ_KY_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_tz_ky_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%tz_ky_tglf)) THEN
        IF(MAXVAL(ABS(ss%tz_ky_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%tz_ky_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: UPWIND_PFRAC_NS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_upwind_pfrac_ns
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%upwind_pfrac_ns)) THEN
        IF(MAXVAL(ABS(ss%upwind_pfrac_ns)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%upwind_pfrac_ns,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: UPWIND_PFRAC_OMEGA
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_upwind_pfrac_omega
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%upwind_pfrac_omega)) THEN
        IF(MAXVAL(ABS(ss%upwind_pfrac_omega)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%upwind_pfrac_omega,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: UPWIND_PFRAC_TE
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_upwind_pfrac_te
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%upwind_pfrac_Te)) THEN
        IF(MAXVAL(ABS(ss%upwind_pfrac_Te)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%upwind_pfrac_Te,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: UPWIND_PFRAC_TI
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_upwind_pfrac_ti
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%upwind_pfrac_Ti)) THEN
        IF(MAXVAL(ABS(ss%upwind_pfrac_Ti)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%upwind_pfrac_Ti,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VEE_TRANS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_vee_trans
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vee_trans)) THEN
        IF(MAXVAL(ABS(ss%vee_trans)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%vee_trans,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VEE_TRANSCAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_vee_transcat
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vee_transcat)) THEN
        IF(MAXVAL(ABS(ss%vee_transcat)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%vee_transcat,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELB_FUSI
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velb_fusi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velb_fusi)) THEN
        IF(MAXVAL(ABS(ss%velb_fusi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velb_fusi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELB_NBI
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velb_nbi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velb_nbi)) THEN
        IF(MAXVAL(ABS(ss%velb_nbi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velb_nbi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELB_RFMI
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velb_rfmi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velb_rfmi)) THEN
        IF(MAXVAL(ABS(ss%velb_rfmi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velb_rfmi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELNE_NCLASS
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velne_nclass
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velne_nclass)) THEN
        IF(MAXVAL(ABS(ss%velne_nclass)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velne_nclass,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELNH_NCLASS
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velnh_nclass
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velnh_nclass)) THEN
        IF(MAXVAL(ABS(ss%velnh_nclass)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velnh_nclass,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELNI_DRBM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velni_drbm
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velni_drbm)) THEN
        IF(MAXVAL(ABS(ss%velni_drbm)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velni_drbm,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELNS_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velns_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velns_anom)) THEN
        IF(MAXVAL(ABS(ss%velns_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velns_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELNS_GLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velns_glf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velns_glf)) THEN
        IF(MAXVAL(ABS(ss%velns_glf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velns_glf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELNS_GTCNEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velns_gtcneo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velns_gtcneo)) THEN
        IF(MAXVAL(ABS(ss%velns_gtcneo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velns_gtcneo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELNS_MMM71
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velns_mmm71
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velns_mmm71)) THEN
        IF(MAXVAL(ABS(ss%velns_mmm71)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velns_mmm71,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELNS_NCLASS
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velns_nclass
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velns_nclass)) THEN
        IF(MAXVAL(ABS(ss%velns_nclass)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velns_nclass,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELNS_NEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velns_neo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velns_neo)) THEN
        IF(MAXVAL(ABS(ss%velns_neo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velns_neo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELNS_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velns_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velns_tglf)) THEN
        IF(MAXVAL(ABS(ss%velns_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velns_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELNX_NCLASS
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velnx_nclass
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velnx_nclass)) THEN
        IF(MAXVAL(ABS(ss%velnx_nclass)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velnx_nclass,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELPPHI_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velpphi_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velpphi_anom)) THEN
        IF(MAXVAL(ABS(ss%velpphi_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velpphi_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELPPHI_GLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velpphi_glf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velpphi_glf)) THEN
        IF(MAXVAL(ABS(ss%velpphi_glf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velpphi_glf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELPPHI_GTCNEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velpphi_gtcneo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velpphi_gtcneo)) THEN
        IF(MAXVAL(ABS(ss%velpphi_gtcneo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velpphi_gtcneo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELPPHI_MMM71
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velpphi_mmm71
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velpphi_mmm71)) THEN
        IF(MAXVAL(ABS(ss%velpphi_mmm71)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velpphi_mmm71,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELPPHI_NEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velpphi_neo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velpphi_neo)) THEN
        IF(MAXVAL(ABS(ss%velpphi_neo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velpphi_neo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELPPHI_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velpphi_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velpphi_tglf)) THEN
        IF(MAXVAL(ABS(ss%velpphi_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velpphi_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTE_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velte_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velte_anom)) THEN
        IF(MAXVAL(ABS(ss%velte_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velte_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTE_GLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velte_glf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velte_glf)) THEN
        IF(MAXVAL(ABS(ss%velte_glf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velte_glf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTE_GTCNEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velte_gtcneo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velte_gtcneo)) THEN
        IF(MAXVAL(ABS(ss%velte_gtcneo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velte_gtcneo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTE_MMM71
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velte_mmm71
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velte_mmm71)) THEN
        IF(MAXVAL(ABS(ss%velte_mmm71)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velte_mmm71,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTE_NCLASS
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velte_nclass
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velte_nclass)) THEN
        IF(MAXVAL(ABS(ss%velte_nclass)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velte_nclass,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTE_NEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velte_neo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velte_neo)) THEN
        IF(MAXVAL(ABS(ss%velte_neo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velte_neo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTE_PALEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velte_paleo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velte_paleo)) THEN
        IF(MAXVAL(ABS(ss%velte_paleo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velte_paleo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTE_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velte_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velte_tglf)) THEN
        IF(MAXVAL(ABS(ss%velte_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velte_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTI_ANOM
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velti_anom
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velti_anom)) THEN
        IF(MAXVAL(ABS(ss%velti_anom)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velti_anom,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTI_GLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velti_glf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velti_glf)) THEN
        IF(MAXVAL(ABS(ss%velti_glf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velti_glf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTI_GTCNEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velti_gtcneo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velti_gtcneo)) THEN
        IF(MAXVAL(ABS(ss%velti_gtcneo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velti_gtcneo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTI_MMM71
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velti_mmm71
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velti_mmm71)) THEN
        IF(MAXVAL(ABS(ss%velti_mmm71)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velti_mmm71,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTI_NCLASS
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velti_nclass
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velti_nclass)) THEN
        IF(MAXVAL(ABS(ss%velti_nclass)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velti_nclass,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTI_NEO
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velti_neo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velti_neo)) THEN
        IF(MAXVAL(ABS(ss%velti_neo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velti_neo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VELTI_TGLF
  IF(cclist(ps_cnum_ANOM).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_velti_tglf
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%velti_tglf)) THEN
        IF(MAXVAL(ABS(ss%velti_tglf)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%velti_tglf,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VIE_TRANS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_vie_trans
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vie_trans)) THEN
        IF(MAXVAL(ABS(ss%vie_trans)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%vie_trans,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VIE_TRANSCAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_vie_transcat
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vie_transcat)) THEN
        IF(MAXVAL(ABS(ss%vie_transcat)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%vie_transcat,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VMO_TRANS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_vmo_trans
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vmo_trans)) THEN
        IF(MAXVAL(ABS(ss%vmo_trans)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%vmo_trans,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VMO_TRANSCAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_vmo_transcat
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vmo_transcat)) THEN
        IF(MAXVAL(ABS(ss%vmo_transcat)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%vmo_transcat,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VN_TRANS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_vn_trans
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vn_trans)) THEN
        IF(MAXVAL(ABS(ss%vn_trans)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%vn_trans,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VN_TRANSCAT
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_vn_transcat
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vn_transcat)) THEN
        IF(MAXVAL(ABS(ss%vn_transcat)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%vn_transcat,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VOL
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_vol
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vol)) THEN
        IF(MAXVAL(ABS(ss%vol)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%vol,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VPOL_INMP
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_vpol_inmp
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vpol_inmp)) THEN
        IF(MAXVAL(ABS(ss%vpol_inmp)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%vpol_inmp,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VPOL_OMP
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_vpol_omp
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vpol_omp)) THEN
        IF(MAXVAL(ABS(ss%vpol_omp)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%vpol_omp,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VTOR_INMP
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_vtor_inmp
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vtor_inmp)) THEN
        IF(MAXVAL(ABS(ss%vtor_inmp)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%vtor_inmp,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: VTOR_OMP
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_vtor_omp
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%vtor_omp)) THEN
        IF(MAXVAL(ABS(ss%vtor_omp)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%vtor_omp,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: V_LOOP
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_v_loop
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%V_loop)) THEN
        IF(MAXVAL(ABS(ss%V_loop)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%V_loop,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: V_PARS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_v_pars
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%v_pars)) THEN
        IF(MAXVAL(ABS(ss%v_pars)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%v_pars,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: V_PERS
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_v_pers
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%v_pers)) THEN
        IF(MAXVAL(ABS(ss%v_pers)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%v_pers,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: XRJCOS_MOMEQ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_xrjcos_momeq
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%xRjcos_momeq)) THEN
        IF(MAXVAL(ABS(ss%xRjcos_momeq)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%xRjcos_momeq,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: XRJSIN_MOMEQ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_xrjsin_momeq
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%xRjsin_momeq)) THEN
        IF(MAXVAL(ABS(ss%xRjsin_momeq)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%xRjsin_momeq,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: XZJCOS_MOMEQ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_xzjcos_momeq
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%xZjcos_momeq)) THEN
        IF(MAXVAL(ABS(ss%xZjcos_momeq)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%xZjcos_momeq,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: XZJSIN_MOMEQ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_xzjsin_momeq
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%xZjsin_momeq)) THEN
        IF(MAXVAL(ABS(ss%xZjsin_momeq)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%xZjsin_momeq,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: Z0_MOMEQ
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_z0_momeq
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Z0_momeq)) THEN
        IF(MAXVAL(ABS(ss%Z0_momeq)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Z0_momeq,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: ZEFF
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_zeff
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Zeff)) THEN
        IF(MAXVAL(ABS(ss%Zeff)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Zeff,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: ZEFF_FI
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_zeff_fi
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Zeff_fi)) THEN
        IF(MAXVAL(ABS(ss%Zeff_fi)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Zeff_fi,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: ZEFF_TH
  IF(cclist(ps_cnum_PLASMA).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_zeff_th
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Zeff_th)) THEN
        IF(MAXVAL(ABS(ss%Zeff_th)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Zeff_th,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: Z_GEO
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_z_geo
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Z_geo)) THEN
        IF(MAXVAL(ABS(ss%Z_geo)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Z_geo,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: Z_MIDP
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_z_midp
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Z_midp)) THEN
        IF(MAXVAL(ABS(ss%Z_midp)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Z_midp,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: Z_SURFMAX
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_z_surfmax
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Z_surfMax)) THEN
        IF(MAXVAL(ABS(ss%Z_surfMax)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Z_surfMax,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  !----------------------
  ! profile: Z_SURFMIN
  IF(cclist(ps_cnum_EQ).GE.ps_chkAll) THEN
     iaddr1 = ps_phash_z_surfmin
     iaddr2 = iaddr1 + 1
     IF(ALLOCATED(ss%Z_surfMin)) THEN
        IF(MAXVAL(ABS(ss%Z_surfMin)).EQ.0.0_rspec) THEN
           kf1 = -1; kf2 = -1
        ELSE
           kf1 = 17; kf2 = 23
           call hashCode(ss%Z_surfMin,kf1,kf2)
        ENDIF
     ELSE
        kf1 = ps_unInit; kf2 = ps_unInit
     ENDIF
     ss%new_hash_codes(iaddr1) = kf1
     ss%new_hash_codes(iaddr2) = kf2

  ENDIF

  IF(iertot.GT.0) THEN
     WRITE(iout,*) " ?plasma_state_hash_code: change lock violation(s)."
     ierr=9999
  ELSE
     ierr = 0
  ENDIF

  CONTAINS
     SUBROUTINE chk_hash(htype,hname)
        CHARACTER*(*), INTENT(in) :: htype
        CHARACTER*(*), INTENT(in) :: hname

        integer :: iwarn
        logical :: ichange,plasma_state_hash_match

        ichange = .not.plasma_state_hash_match(ss,iaddr1,ierr)

        iwarn = 0
        IF(ichange) THEN
           IF(ilock.EQ.1) THEN
              iwarn = 1
           ELSE IF(ilock.EQ.2) THEN
              iwarn = 2
              iertot = iertot + 1
           ENDIF
        ENDIF

        IF(iwarn.EQ.1) THEN
           WRITE(iout,*) &
                ' %hash code monitored change detected: type="'// &
                trim(htype)//'" name="'//trim(hname)//'".'
        ELSE IF(iwarn.EQ.2) THEN
           WRITE(iout,*) &
                ' ?hash code change LOCK violation: type="'// &
                trim(htype)//'" name="'//trim(hname)//'".'
        ENDIF
     END SUBROUTINE chk_hash

END SUBROUTINE plasma_state_hash_code
