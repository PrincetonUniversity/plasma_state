! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_merge_0(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)


  ! Merge floating point data
  !   ss3 <-- w1*ss1 + (1-w1)*ss2

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components to merge
                                ! use ps_cc_all to merge all components

  REAL(KIND=rspec) :: w1        ! weight on ss1
  TYPE (plasma_state) :: ss1    ! state data to copy *from* with weight (w1)
  TYPE (plasma_state) :: ss2    ! state data to copy *from* with weight (1-w1)
  TYPE (plasma_state) :: ss3    ! state to which data is copied.
  INTEGER, INTENT(IN) :: icheck ! control for checking match of non-floating-
  !  point data: ps_ignore, ps_warn, or ps_error on mismatch.

  INTEGER, INTENT(IN) :: iout
  INTEGER, INTENT(INOUT) :: ierr  ! completion code (not cleared,set on error)

  !-------------------
  INTEGER :: isize
  !-------------------

  !-----------------------------------------------
  ! copy scalars and arrays that are allocated...

  IF(ALLOCATED(ss3%alla_index)) THEN
     IF(ALLOCATED(ss1%alla_index).AND.ALLOCATED(ss2%alla_index)) THEN
        CALL ps_diff("alla_index",ss1%alla_index,ss2%alla_index,ps_error,iout,ierr)
        CALL ps_conformal("alla_index",ss1%alla_index,ss3%alla_index,iout,ierr)
        if(ierr.eq.0) ss3%alla_index = ss1%alla_index
     ELSE IF(ALLOCATED(ss1%alla_index)) THEN
        CALL ps_conformal("alla_index",ss1%alla_index,ss3%alla_index,iout,ierr)
        ss3%alla_index = ss1%alla_index
     ELSE IF(ALLOCATED(ss2%alla_index)) THEN
        CALL ps_conformal("alla_index",ss2%alla_index,ss3%alla_index,iout,ierr)
        ss3%alla_index = ss2%alla_index
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%ALLA_name)) THEN
     IF(ALLOCATED(ss1%ALLA_name).AND.ALLOCATED(ss2%ALLA_name)) THEN
        CALL ps_diff("ALLA_name",ss1%ALLA_name,ss2%ALLA_name,ps_error,iout,ierr)
        CALL ps_conformal("ALLA_name",ss1%ALLA_name,ss3%ALLA_name,iout,ierr)
        if(ierr.eq.0) ss3%ALLA_name = ss1%ALLA_name
     ELSE IF(ALLOCATED(ss1%ALLA_name)) THEN
        CALL ps_conformal("ALLA_name",ss1%ALLA_name,ss3%ALLA_name,iout,ierr)
        ss3%ALLA_name = ss1%ALLA_name
     ELSE IF(ALLOCATED(ss2%ALLA_name)) THEN
        CALL ps_conformal("ALLA_name",ss2%ALLA_name,ss3%ALLA_name,iout,ierr)
        ss3%ALLA_name = ss2%ALLA_name
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%ALLA_type)) THEN
     IF(ALLOCATED(ss1%ALLA_type).AND.ALLOCATED(ss2%ALLA_type)) THEN
        CALL ps_diff("ALLA_type",ss1%ALLA_type,ss2%ALLA_type,ps_error,iout,ierr)
        CALL ps_conformal("ALLA_type",ss1%ALLA_type,ss3%ALLA_type,iout,ierr)
        if(ierr.eq.0) ss3%ALLA_type = ss1%ALLA_type
     ELSE IF(ALLOCATED(ss1%ALLA_type)) THEN
        CALL ps_conformal("ALLA_type",ss1%ALLA_type,ss3%ALLA_type,iout,ierr)
        ss3%ALLA_type = ss1%ALLA_type
     ELSE IF(ALLOCATED(ss2%ALLA_type)) THEN
        CALL ps_conformal("ALLA_type",ss2%ALLA_type,ss3%ALLA_type,iout,ierr)
        ss3%ALLA_type = ss2%ALLA_type
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%all_index)) THEN
     IF(ALLOCATED(ss1%all_index).AND.ALLOCATED(ss2%all_index)) THEN
        CALL ps_diff("all_index",ss1%all_index,ss2%all_index,ps_error,iout,ierr)
        CALL ps_conformal("all_index",ss1%all_index,ss3%all_index,iout,ierr)
        if(ierr.eq.0) ss3%all_index = ss1%all_index
     ELSE IF(ALLOCATED(ss1%all_index)) THEN
        CALL ps_conformal("all_index",ss1%all_index,ss3%all_index,iout,ierr)
        ss3%all_index = ss1%all_index
     ELSE IF(ALLOCATED(ss2%all_index)) THEN
        CALL ps_conformal("all_index",ss2%all_index,ss3%all_index,iout,ierr)
        ss3%all_index = ss2%all_index
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%ALL_name)) THEN
     IF(ALLOCATED(ss1%ALL_name).AND.ALLOCATED(ss2%ALL_name)) THEN
        CALL ps_diff("ALL_name",ss1%ALL_name,ss2%ALL_name,ps_error,iout,ierr)
        CALL ps_conformal("ALL_name",ss1%ALL_name,ss3%ALL_name,iout,ierr)
        if(ierr.eq.0) ss3%ALL_name = ss1%ALL_name
     ELSE IF(ALLOCATED(ss1%ALL_name)) THEN
        CALL ps_conformal("ALL_name",ss1%ALL_name,ss3%ALL_name,iout,ierr)
        ss3%ALL_name = ss1%ALL_name
     ELSE IF(ALLOCATED(ss2%ALL_name)) THEN
        CALL ps_conformal("ALL_name",ss2%ALL_name,ss3%ALL_name,iout,ierr)
        ss3%ALL_name = ss2%ALL_name
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%ALL_type)) THEN
     IF(ALLOCATED(ss1%ALL_type).AND.ALLOCATED(ss2%ALL_type)) THEN
        CALL ps_diff("ALL_type",ss1%ALL_type,ss2%ALL_type,ps_error,iout,ierr)
        CALL ps_conformal("ALL_type",ss1%ALL_type,ss3%ALL_type,iout,ierr)
        if(ierr.eq.0) ss3%ALL_type = ss1%ALL_type
     ELSE IF(ALLOCATED(ss1%ALL_type)) THEN
        CALL ps_conformal("ALL_type",ss1%ALL_type,ss3%ALL_type,iout,ierr)
        ss3%ALL_type = ss1%ALL_type
     ELSE IF(ALLOCATED(ss2%ALL_type)) THEN
        CALL ps_conformal("ALL_type",ss2%ALL_type,ss3%ALL_type,iout,ierr)
        ss3%ALL_type = ss2%ALL_type
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%amaxlh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%amaxlh).AND.ALLOCATED(ss2%amaxlh)) THEN
        CALL ps_diff("amaxlh",ss1%amaxlh,ss2%amaxlh,icheck,iout,ierr)
        CALL ps_conformal("amaxlh",ss1%amaxlh,ss3%amaxlh,iout,ierr)
        if(ierr.eq.0) ss3%amaxlh = ss1%amaxlh
     ELSE IF(ALLOCATED(ss1%amaxlh)) THEN
        CALL ps_conformal("amaxlh",ss1%amaxlh,ss3%amaxlh,iout,ierr)
        ss3%amaxlh = ss1%amaxlh
     ELSE IF(ALLOCATED(ss2%amaxlh)) THEN
        CALL ps_conformal("amaxlh",ss2%amaxlh,ss3%amaxlh,iout,ierr)
        ss3%amaxlh = ss2%amaxlh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%aminlh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%aminlh).AND.ALLOCATED(ss2%aminlh)) THEN
        CALL ps_diff("aminlh",ss1%aminlh,ss2%aminlh,icheck,iout,ierr)
        CALL ps_conformal("aminlh",ss1%aminlh,ss3%aminlh,iout,ierr)
        if(ierr.eq.0) ss3%aminlh = ss1%aminlh
     ELSE IF(ALLOCATED(ss1%aminlh)) THEN
        CALL ps_conformal("aminlh",ss1%aminlh,ss3%aminlh,iout,ierr)
        ss3%aminlh = ss1%aminlh
     ELSE IF(ALLOCATED(ss2%aminlh)) THEN
        CALL ps_conformal("aminlh",ss2%aminlh,ss3%aminlh,iout,ierr)
        ss3%aminlh = ss2%aminlh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Ang_Href_coil)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Ang_Href_coil).AND.ALLOCATED(ss2%Ang_Href_coil)) THEN
        CALL ps_diff("Ang_Href_coil",ss1%Ang_Href_coil,ss2%Ang_Href_coil,icheck,iout,ierr)
        CALL ps_conformal("Ang_Href_coil",ss1%Ang_Href_coil,ss3%Ang_Href_coil,iout,ierr)
        if(ierr.eq.0) ss3%Ang_Href_coil = ss1%Ang_Href_coil
     ELSE IF(ALLOCATED(ss1%Ang_Href_coil)) THEN
        CALL ps_conformal("Ang_Href_coil",ss1%Ang_Href_coil,ss3%Ang_Href_coil,iout,ierr)
        ss3%Ang_Href_coil = ss1%Ang_Href_coil
     ELSE IF(ALLOCATED(ss2%Ang_Href_coil)) THEN
        CALL ps_conformal("Ang_Href_coil",ss2%Ang_Href_coil,ss3%Ang_Href_coil,iout,ierr)
        ss3%Ang_Href_coil = ss2%Ang_Href_coil
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Ang_Vref_coil)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Ang_Vref_coil).AND.ALLOCATED(ss2%Ang_Vref_coil)) THEN
        CALL ps_diff("Ang_Vref_coil",ss1%Ang_Vref_coil,ss2%Ang_Vref_coil,icheck,iout,ierr)
        CALL ps_conformal("Ang_Vref_coil",ss1%Ang_Vref_coil,ss3%Ang_Vref_coil,iout,ierr)
        if(ierr.eq.0) ss3%Ang_Vref_coil = ss1%Ang_Vref_coil
     ELSE IF(ALLOCATED(ss1%Ang_Vref_coil)) THEN
        CALL ps_conformal("Ang_Vref_coil",ss1%Ang_Vref_coil,ss3%Ang_Vref_coil,iout,ierr)
        ss3%Ang_Vref_coil = ss1%Ang_Vref_coil
     ELSE IF(ALLOCATED(ss2%Ang_Vref_coil)) THEN
        CALL ps_conformal("Ang_Vref_coil",ss2%Ang_Vref_coil,ss3%Ang_Vref_coil,iout,ierr)
        ss3%Ang_Vref_coil = ss2%Ang_Vref_coil
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_ANOM).EQ.1) THEN

     CALL ps_diff("ANOM_Code_Info",ss1%ANOM_Code_Info,ss2%ANOM_Code_Info,icheck,iout,ierr)
     ss3%ANOM_Code_Info = ss1%ANOM_Code_Info
  ENDIF

  IF((ALLOCATED(ss3%anom_evar)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%anom_evar).AND.ALLOCATED(ss2%anom_evar)) THEN
        CALL ps_conformal("anom_evar",ss2%anom_evar,ss3%anom_evar,iout,ierr)
        CALL ps_conformal("anom_evar",ss1%anom_evar,ss3%anom_evar,iout,ierr)
        if(ierr.eq.0) ss3%anom_evar = w1*ss1%anom_evar + (1.0_rspec-w1)*ss2%anom_evar
     ELSE IF(ALLOCATED(ss1%anom_evar)) THEN
        CALL ps_conformal("anom_evar",ss1%anom_evar,ss3%anom_evar,iout,ierr)
        ss3%anom_evar = ss1%anom_evar
     ELSE IF(ALLOCATED(ss2%anom_evar)) THEN
        CALL ps_conformal("anom_evar",ss2%anom_evar,ss3%anom_evar,iout,ierr)
        ss3%anom_evar = ss2%anom_evar
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ant_model)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%ant_model).AND.ALLOCATED(ss2%ant_model)) THEN
        CALL ps_diff("ant_model",ss1%ant_model,ss2%ant_model,icheck,iout,ierr)
        CALL ps_conformal("ant_model",ss1%ant_model,ss3%ant_model,iout,ierr)
        if(ierr.eq.0) ss3%ant_model = ss1%ant_model
     ELSE IF(ALLOCATED(ss1%ant_model)) THEN
        CALL ps_conformal("ant_model",ss1%ant_model,ss3%ant_model,iout,ierr)
        ss3%ant_model = ss1%ant_model
     ELSE IF(ALLOCATED(ss2%ant_model)) THEN
        CALL ps_conformal("ant_model",ss2%ant_model,ss3%ant_model,iout,ierr)
        ss3%ant_model = ss2%ant_model
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ap2_halfheight)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%ap2_halfheight).AND.ALLOCATED(ss2%ap2_halfheight)) THEN
        CALL ps_diff("ap2_halfheight",ss1%ap2_halfheight,ss2%ap2_halfheight,icheck,iout,ierr)
        CALL ps_conformal("ap2_halfheight",ss1%ap2_halfheight,ss3%ap2_halfheight,iout,ierr)
        if(ierr.eq.0) ss3%ap2_halfheight = ss1%ap2_halfheight
     ELSE IF(ALLOCATED(ss1%ap2_halfheight)) THEN
        CALL ps_conformal("ap2_halfheight",ss1%ap2_halfheight,ss3%ap2_halfheight,iout,ierr)
        ss3%ap2_halfheight = ss1%ap2_halfheight
     ELSE IF(ALLOCATED(ss2%ap2_halfheight)) THEN
        CALL ps_conformal("ap2_halfheight",ss2%ap2_halfheight,ss3%ap2_halfheight,iout,ierr)
        ss3%ap2_halfheight = ss2%ap2_halfheight
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ap2_halfwidth)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%ap2_halfwidth).AND.ALLOCATED(ss2%ap2_halfwidth)) THEN
        CALL ps_diff("ap2_halfwidth",ss1%ap2_halfwidth,ss2%ap2_halfwidth,icheck,iout,ierr)
        CALL ps_conformal("ap2_halfwidth",ss1%ap2_halfwidth,ss3%ap2_halfwidth,iout,ierr)
        if(ierr.eq.0) ss3%ap2_halfwidth = ss1%ap2_halfwidth
     ELSE IF(ALLOCATED(ss1%ap2_halfwidth)) THEN
        CALL ps_conformal("ap2_halfwidth",ss1%ap2_halfwidth,ss3%ap2_halfwidth,iout,ierr)
        ss3%ap2_halfwidth = ss1%ap2_halfwidth
     ELSE IF(ALLOCATED(ss2%ap2_halfwidth)) THEN
        CALL ps_conformal("ap2_halfwidth",ss2%ap2_halfwidth,ss3%ap2_halfwidth,iout,ierr)
        ss3%ap2_halfwidth = ss2%ap2_halfwidth
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ap2_horiz_offset)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%ap2_horiz_offset).AND.ALLOCATED(ss2%ap2_horiz_offset)) THEN
        CALL ps_diff("ap2_horiz_offset",ss1%ap2_horiz_offset,ss2%ap2_horiz_offset,icheck,iout,ierr)
        CALL ps_conformal("ap2_horiz_offset",ss1%ap2_horiz_offset,ss3%ap2_horiz_offset,iout,ierr)
        if(ierr.eq.0) ss3%ap2_horiz_offset = ss1%ap2_horiz_offset
     ELSE IF(ALLOCATED(ss1%ap2_horiz_offset)) THEN
        CALL ps_conformal("ap2_horiz_offset",ss1%ap2_horiz_offset,ss3%ap2_horiz_offset,iout,ierr)
        ss3%ap2_horiz_offset = ss1%ap2_horiz_offset
     ELSE IF(ALLOCATED(ss2%ap2_horiz_offset)) THEN
        CALL ps_conformal("ap2_horiz_offset",ss2%ap2_horiz_offset,ss3%ap2_horiz_offset,iout,ierr)
        ss3%ap2_horiz_offset = ss2%ap2_horiz_offset
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ap2_vert_offset)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%ap2_vert_offset).AND.ALLOCATED(ss2%ap2_vert_offset)) THEN
        CALL ps_diff("ap2_vert_offset",ss1%ap2_vert_offset,ss2%ap2_vert_offset,icheck,iout,ierr)
        CALL ps_conformal("ap2_vert_offset",ss1%ap2_vert_offset,ss3%ap2_vert_offset,iout,ierr)
        if(ierr.eq.0) ss3%ap2_vert_offset = ss1%ap2_vert_offset
     ELSE IF(ALLOCATED(ss1%ap2_vert_offset)) THEN
        CALL ps_conformal("ap2_vert_offset",ss1%ap2_vert_offset,ss3%ap2_vert_offset,iout,ierr)
        ss3%ap2_vert_offset = ss1%ap2_vert_offset
     ELSE IF(ALLOCATED(ss2%ap2_vert_offset)) THEN
        CALL ps_conformal("ap2_vert_offset",ss2%ap2_vert_offset,ss3%ap2_vert_offset,iout,ierr)
        ss3%ap2_vert_offset = ss2%ap2_vert_offset
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%apolmaxlh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%apolmaxlh).AND.ALLOCATED(ss2%apolmaxlh)) THEN
        CALL ps_diff("apolmaxlh",ss1%apolmaxlh,ss2%apolmaxlh,icheck,iout,ierr)
        CALL ps_conformal("apolmaxlh",ss1%apolmaxlh,ss3%apolmaxlh,iout,ierr)
        if(ierr.eq.0) ss3%apolmaxlh = ss1%apolmaxlh
     ELSE IF(ALLOCATED(ss1%apolmaxlh)) THEN
        CALL ps_conformal("apolmaxlh",ss1%apolmaxlh,ss3%apolmaxlh,iout,ierr)
        ss3%apolmaxlh = ss1%apolmaxlh
     ELSE IF(ALLOCATED(ss2%apolmaxlh)) THEN
        CALL ps_conformal("apolmaxlh",ss2%apolmaxlh,ss3%apolmaxlh,iout,ierr)
        ss3%apolmaxlh = ss2%apolmaxlh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%apolminlh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%apolminlh).AND.ALLOCATED(ss2%apolminlh)) THEN
        CALL ps_diff("apolminlh",ss1%apolminlh,ss2%apolminlh,icheck,iout,ierr)
        CALL ps_conformal("apolminlh",ss1%apolminlh,ss3%apolminlh,iout,ierr)
        if(ierr.eq.0) ss3%apolminlh = ss1%apolminlh
     ELSE IF(ALLOCATED(ss1%apolminlh)) THEN
        CALL ps_conformal("apolminlh",ss1%apolminlh,ss3%apolminlh,iout,ierr)
        ss3%apolminlh = ss1%apolminlh
     ELSE IF(ALLOCATED(ss2%apolminlh)) THEN
        CALL ps_conformal("apolminlh",ss2%apolminlh,ss3%apolminlh,iout,ierr)
        ss3%apolminlh = ss2%apolminlh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ap_halfheight)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%ap_halfheight).AND.ALLOCATED(ss2%ap_halfheight)) THEN
        CALL ps_diff("ap_halfheight",ss1%ap_halfheight,ss2%ap_halfheight,icheck,iout,ierr)
        CALL ps_conformal("ap_halfheight",ss1%ap_halfheight,ss3%ap_halfheight,iout,ierr)
        if(ierr.eq.0) ss3%ap_halfheight = ss1%ap_halfheight
     ELSE IF(ALLOCATED(ss1%ap_halfheight)) THEN
        CALL ps_conformal("ap_halfheight",ss1%ap_halfheight,ss3%ap_halfheight,iout,ierr)
        ss3%ap_halfheight = ss1%ap_halfheight
     ELSE IF(ALLOCATED(ss2%ap_halfheight)) THEN
        CALL ps_conformal("ap_halfheight",ss2%ap_halfheight,ss3%ap_halfheight,iout,ierr)
        ss3%ap_halfheight = ss2%ap_halfheight
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ap_halfwidth)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%ap_halfwidth).AND.ALLOCATED(ss2%ap_halfwidth)) THEN
        CALL ps_diff("ap_halfwidth",ss1%ap_halfwidth,ss2%ap_halfwidth,icheck,iout,ierr)
        CALL ps_conformal("ap_halfwidth",ss1%ap_halfwidth,ss3%ap_halfwidth,iout,ierr)
        if(ierr.eq.0) ss3%ap_halfwidth = ss1%ap_halfwidth
     ELSE IF(ALLOCATED(ss1%ap_halfwidth)) THEN
        CALL ps_conformal("ap_halfwidth",ss1%ap_halfwidth,ss3%ap_halfwidth,iout,ierr)
        ss3%ap_halfwidth = ss1%ap_halfwidth
     ELSE IF(ALLOCATED(ss2%ap_halfwidth)) THEN
        CALL ps_conformal("ap_halfwidth",ss2%ap_halfwidth,ss3%ap_halfwidth,iout,ierr)
        ss3%ap_halfwidth = ss2%ap_halfwidth
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ap_horiz_offset)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%ap_horiz_offset).AND.ALLOCATED(ss2%ap_horiz_offset)) THEN
        CALL ps_diff("ap_horiz_offset",ss1%ap_horiz_offset,ss2%ap_horiz_offset,icheck,iout,ierr)
        CALL ps_conformal("ap_horiz_offset",ss1%ap_horiz_offset,ss3%ap_horiz_offset,iout,ierr)
        if(ierr.eq.0) ss3%ap_horiz_offset = ss1%ap_horiz_offset
     ELSE IF(ALLOCATED(ss1%ap_horiz_offset)) THEN
        CALL ps_conformal("ap_horiz_offset",ss1%ap_horiz_offset,ss3%ap_horiz_offset,iout,ierr)
        ss3%ap_horiz_offset = ss1%ap_horiz_offset
     ELSE IF(ALLOCATED(ss2%ap_horiz_offset)) THEN
        CALL ps_conformal("ap_horiz_offset",ss2%ap_horiz_offset,ss3%ap_horiz_offset,iout,ierr)
        ss3%ap_horiz_offset = ss2%ap_horiz_offset
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ap_vert_offset)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%ap_vert_offset).AND.ALLOCATED(ss2%ap_vert_offset)) THEN
        CALL ps_diff("ap_vert_offset",ss1%ap_vert_offset,ss2%ap_vert_offset,icheck,iout,ierr)
        CALL ps_conformal("ap_vert_offset",ss1%ap_vert_offset,ss3%ap_vert_offset,iout,ierr)
        if(ierr.eq.0) ss3%ap_vert_offset = ss1%ap_vert_offset
     ELSE IF(ALLOCATED(ss1%ap_vert_offset)) THEN
        CALL ps_conformal("ap_vert_offset",ss1%ap_vert_offset,ss3%ap_vert_offset,iout,ierr)
        ss3%ap_vert_offset = ss1%ap_vert_offset
     ELSE IF(ALLOCATED(ss2%ap_vert_offset)) THEN
        CALL ps_conformal("ap_vert_offset",ss2%ap_vert_offset,ss3%ap_vert_offset,iout,ierr)
        ss3%ap_vert_offset = ss2%ap_vert_offset
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%area)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%area).AND.ALLOCATED(ss2%area)) THEN
        CALL ps_conformal("area",ss2%area,ss3%area,iout,ierr)
        CALL ps_conformal("area",ss1%area,ss3%area,iout,ierr)
        if(ierr.eq.0) ss3%area = w1*ss1%area + (1.0_rspec-w1)*ss2%area
     ELSE IF(ALLOCATED(ss1%area)) THEN
        CALL ps_conformal("area",ss1%area,ss3%area,iout,ierr)
        ss3%area = ss1%area
     ELSE IF(ALLOCATED(ss2%area)) THEN
        CALL ps_conformal("area",ss2%area,ss3%area,iout,ierr)
        ss3%area = ss2%area
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%atormaxlh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%atormaxlh).AND.ALLOCATED(ss2%atormaxlh)) THEN
        CALL ps_diff("atormaxlh",ss1%atormaxlh,ss2%atormaxlh,icheck,iout,ierr)
        CALL ps_conformal("atormaxlh",ss1%atormaxlh,ss3%atormaxlh,iout,ierr)
        if(ierr.eq.0) ss3%atormaxlh = ss1%atormaxlh
     ELSE IF(ALLOCATED(ss1%atormaxlh)) THEN
        CALL ps_conformal("atormaxlh",ss1%atormaxlh,ss3%atormaxlh,iout,ierr)
        ss3%atormaxlh = ss1%atormaxlh
     ELSE IF(ALLOCATED(ss2%atormaxlh)) THEN
        CALL ps_conformal("atormaxlh",ss2%atormaxlh,ss3%atormaxlh,iout,ierr)
        ss3%atormaxlh = ss2%atormaxlh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%atorminlh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%atorminlh).AND.ALLOCATED(ss2%atorminlh)) THEN
        CALL ps_diff("atorminlh",ss1%atorminlh,ss2%atorminlh,icheck,iout,ierr)
        CALL ps_conformal("atorminlh",ss1%atorminlh,ss3%atorminlh,iout,ierr)
        if(ierr.eq.0) ss3%atorminlh = ss1%atorminlh
     ELSE IF(ALLOCATED(ss1%atorminlh)) THEN
        CALL ps_conformal("atorminlh",ss1%atorminlh,ss3%atorminlh,iout,ierr)
        ss3%atorminlh = ss1%atorminlh
     ELSE IF(ALLOCATED(ss2%atorminlh)) THEN
        CALL ps_conformal("atorminlh",ss2%atorminlh,ss3%atorminlh,iout,ierr)
        ss3%atorminlh = ss2%atorminlh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Balloon_code)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%Balloon_code).AND.ALLOCATED(ss2%Balloon_code)) THEN
        CALL ps_diff("Balloon_code",ss1%Balloon_code,ss2%Balloon_code,icheck,iout,ierr)
        CALL ps_conformal("Balloon_code",ss1%Balloon_code,ss3%Balloon_code,iout,ierr)
        if(ierr.eq.0) ss3%Balloon_code = ss1%Balloon_code
     ELSE IF(ALLOCATED(ss1%Balloon_code)) THEN
        CALL ps_conformal("Balloon_code",ss1%Balloon_code,ss3%Balloon_code,iout,ierr)
        ss3%Balloon_code = ss1%Balloon_code
     ELSE IF(ALLOCATED(ss2%Balloon_code)) THEN
        CALL ps_conformal("Balloon_code",ss2%Balloon_code,ss3%Balloon_code,iout,ierr)
        ss3%Balloon_code = ss2%Balloon_code
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Balloon_mapper)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%Balloon_mapper).AND.ALLOCATED(ss2%Balloon_mapper)) THEN
        CALL ps_diff("Balloon_mapper",ss1%Balloon_mapper,ss2%Balloon_mapper,icheck,iout,ierr)
        CALL ps_conformal("Balloon_mapper",ss1%Balloon_mapper,ss3%Balloon_mapper,iout,ierr)
        if(ierr.eq.0) ss3%Balloon_mapper = ss1%Balloon_mapper
     ELSE IF(ALLOCATED(ss1%Balloon_mapper)) THEN
        CALL ps_conformal("Balloon_mapper",ss1%Balloon_mapper,ss3%Balloon_mapper,iout,ierr)
        ss3%Balloon_mapper = ss1%Balloon_mapper
     ELSE IF(ALLOCATED(ss2%Balloon_mapper)) THEN
        CALL ps_conformal("Balloon_mapper",ss2%Balloon_mapper,ss3%Balloon_mapper,iout,ierr)
        ss3%Balloon_mapper = ss2%Balloon_mapper
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Balloon_stable)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%Balloon_stable).AND.ALLOCATED(ss2%Balloon_stable)) THEN
        CALL ps_diff("Balloon_stable",ss1%Balloon_stable,ss2%Balloon_stable,icheck,iout,ierr)
        CALL ps_conformal("Balloon_stable",ss1%Balloon_stable,ss3%Balloon_stable,iout,ierr)
        if(ierr.eq.0) ss3%Balloon_stable = ss1%Balloon_stable
     ELSE IF(ALLOCATED(ss1%Balloon_stable)) THEN
        CALL ps_conformal("Balloon_stable",ss1%Balloon_stable,ss3%Balloon_stable,iout,ierr)
        ss3%Balloon_stable = ss1%Balloon_stable
     ELSE IF(ALLOCATED(ss2%Balloon_stable)) THEN
        CALL ps_conformal("Balloon_stable",ss2%Balloon_stable,ss3%Balloon_stable,iout,ierr)
        ss3%Balloon_stable = ss2%Balloon_stable
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Balloon_status)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%Balloon_status).AND.ALLOCATED(ss2%Balloon_status)) THEN
        CALL ps_diff("Balloon_status",ss1%Balloon_status,ss2%Balloon_status,icheck,iout,ierr)
        CALL ps_conformal("Balloon_status",ss1%Balloon_status,ss3%Balloon_status,iout,ierr)
        if(ierr.eq.0) ss3%Balloon_status = ss1%Balloon_status
     ELSE IF(ALLOCATED(ss1%Balloon_status)) THEN
        CALL ps_conformal("Balloon_status",ss1%Balloon_status,ss3%Balloon_status,iout,ierr)
        ss3%Balloon_status = ss1%Balloon_status
     ELSE IF(ALLOCATED(ss2%Balloon_status)) THEN
        CALL ps_conformal("Balloon_status",ss2%Balloon_status,ss3%Balloon_status,iout,ierr)
        ss3%Balloon_status = ss2%Balloon_status
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%balstab)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%balstab).AND.ALLOCATED(ss2%balstab)) THEN
        CALL ps_conformal("balstab",ss2%balstab,ss3%balstab,iout,ierr)
        CALL ps_conformal("balstab",ss1%balstab,ss3%balstab,iout,ierr)
        if(ierr.eq.0) ss3%balstab = w1*ss1%balstab + (1.0_rspec-w1)*ss2%balstab
     ELSE IF(ALLOCATED(ss1%balstab)) THEN
        CALL ps_conformal("balstab",ss1%balstab,ss3%balstab,iout,ierr)
        ss3%balstab = ss1%balstab
     ELSE IF(ALLOCATED(ss2%balstab)) THEN
        CALL ps_conformal("balstab",ss2%balstab,ss3%balstab,iout,ierr)
        ss3%balstab = ss2%balstab
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%beam_type)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%beam_type).AND.ALLOCATED(ss2%beam_type)) THEN
        CALL ps_diff("beam_type",ss1%beam_type,ss2%beam_type,icheck,iout,ierr)
        CALL ps_conformal("beam_type",ss1%beam_type,ss3%beam_type,iout,ierr)
        if(ierr.eq.0) ss3%beam_type = ss1%beam_type
     ELSE IF(ALLOCATED(ss1%beam_type)) THEN
        CALL ps_conformal("beam_type",ss1%beam_type,ss3%beam_type,iout,ierr)
        ss3%beam_type = ss1%beam_type
     ELSE IF(ALLOCATED(ss2%beam_type)) THEN
        CALL ps_conformal("beam_type",ss2%beam_type,ss3%beam_type,iout,ierr)
        ss3%beam_type = ss2%beam_type
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%bidiff_bpass_co)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%bidiff_bpass_co).AND.ALLOCATED(ss2%bidiff_bpass_co)) THEN
        CALL ps_conformal("bidiff_bpass_co",ss2%bidiff_bpass_co,ss3%bidiff_bpass_co,iout,ierr)
        CALL ps_conformal("bidiff_bpass_co",ss1%bidiff_bpass_co,ss3%bidiff_bpass_co,iout,ierr)
        if(ierr.eq.0) ss3%bidiff_bpass_co = w1*ss1%bidiff_bpass_co + (1.0_rspec-w1)*ss2%bidiff_bpass_co
     ELSE IF(ALLOCATED(ss1%bidiff_bpass_co)) THEN
        CALL ps_conformal("bidiff_bpass_co",ss1%bidiff_bpass_co,ss3%bidiff_bpass_co,iout,ierr)
        ss3%bidiff_bpass_co = ss1%bidiff_bpass_co
     ELSE IF(ALLOCATED(ss2%bidiff_bpass_co)) THEN
        CALL ps_conformal("bidiff_bpass_co",ss2%bidiff_bpass_co,ss3%bidiff_bpass_co,iout,ierr)
        ss3%bidiff_bpass_co = ss2%bidiff_bpass_co
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%bidiff_bpass_ctr)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%bidiff_bpass_ctr).AND.ALLOCATED(ss2%bidiff_bpass_ctr)) THEN
        CALL ps_conformal("bidiff_bpass_ctr",ss2%bidiff_bpass_ctr,ss3%bidiff_bpass_ctr,iout,ierr)
        CALL ps_conformal("bidiff_bpass_ctr",ss1%bidiff_bpass_ctr,ss3%bidiff_bpass_ctr,iout,ierr)
        if(ierr.eq.0) ss3%bidiff_bpass_ctr = w1*ss1%bidiff_bpass_ctr + (1.0_rspec-w1)*ss2%bidiff_bpass_ctr
     ELSE IF(ALLOCATED(ss1%bidiff_bpass_ctr)) THEN
        CALL ps_conformal("bidiff_bpass_ctr",ss1%bidiff_bpass_ctr,ss3%bidiff_bpass_ctr,iout,ierr)
        ss3%bidiff_bpass_ctr = ss1%bidiff_bpass_ctr
     ELSE IF(ALLOCATED(ss2%bidiff_bpass_ctr)) THEN
        CALL ps_conformal("bidiff_bpass_ctr",ss2%bidiff_bpass_ctr,ss3%bidiff_bpass_ctr,iout,ierr)
        ss3%bidiff_bpass_ctr = ss2%bidiff_bpass_ctr
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%bidiff_btrap)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%bidiff_btrap).AND.ALLOCATED(ss2%bidiff_btrap)) THEN
        CALL ps_conformal("bidiff_btrap",ss2%bidiff_btrap,ss3%bidiff_btrap,iout,ierr)
        CALL ps_conformal("bidiff_btrap",ss1%bidiff_btrap,ss3%bidiff_btrap,iout,ierr)
        if(ierr.eq.0) ss3%bidiff_btrap = w1*ss1%bidiff_btrap + (1.0_rspec-w1)*ss2%bidiff_btrap
     ELSE IF(ALLOCATED(ss1%bidiff_btrap)) THEN
        CALL ps_conformal("bidiff_btrap",ss1%bidiff_btrap,ss3%bidiff_btrap,iout,ierr)
        ss3%bidiff_btrap = ss1%bidiff_btrap
     ELSE IF(ALLOCATED(ss2%bidiff_btrap)) THEN
        CALL ps_conformal("bidiff_btrap",ss2%bidiff_btrap,ss3%bidiff_btrap,iout,ierr)
        ss3%bidiff_btrap = ss2%bidiff_btrap
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%bidiff_dpass_co)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%bidiff_dpass_co).AND.ALLOCATED(ss2%bidiff_dpass_co)) THEN
        CALL ps_conformal("bidiff_dpass_co",ss2%bidiff_dpass_co,ss3%bidiff_dpass_co,iout,ierr)
        CALL ps_conformal("bidiff_dpass_co",ss1%bidiff_dpass_co,ss3%bidiff_dpass_co,iout,ierr)
        if(ierr.eq.0) ss3%bidiff_dpass_co = w1*ss1%bidiff_dpass_co + (1.0_rspec-w1)*ss2%bidiff_dpass_co
     ELSE IF(ALLOCATED(ss1%bidiff_dpass_co)) THEN
        CALL ps_conformal("bidiff_dpass_co",ss1%bidiff_dpass_co,ss3%bidiff_dpass_co,iout,ierr)
        ss3%bidiff_dpass_co = ss1%bidiff_dpass_co
     ELSE IF(ALLOCATED(ss2%bidiff_dpass_co)) THEN
        CALL ps_conformal("bidiff_dpass_co",ss2%bidiff_dpass_co,ss3%bidiff_dpass_co,iout,ierr)
        ss3%bidiff_dpass_co = ss2%bidiff_dpass_co
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%bidiff_dpass_ctr)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%bidiff_dpass_ctr).AND.ALLOCATED(ss2%bidiff_dpass_ctr)) THEN
        CALL ps_conformal("bidiff_dpass_ctr",ss2%bidiff_dpass_ctr,ss3%bidiff_dpass_ctr,iout,ierr)
        CALL ps_conformal("bidiff_dpass_ctr",ss1%bidiff_dpass_ctr,ss3%bidiff_dpass_ctr,iout,ierr)
        if(ierr.eq.0) ss3%bidiff_dpass_ctr = w1*ss1%bidiff_dpass_ctr + (1.0_rspec-w1)*ss2%bidiff_dpass_ctr
     ELSE IF(ALLOCATED(ss1%bidiff_dpass_ctr)) THEN
        CALL ps_conformal("bidiff_dpass_ctr",ss1%bidiff_dpass_ctr,ss3%bidiff_dpass_ctr,iout,ierr)
        ss3%bidiff_dpass_ctr = ss1%bidiff_dpass_ctr
     ELSE IF(ALLOCATED(ss2%bidiff_dpass_ctr)) THEN
        CALL ps_conformal("bidiff_dpass_ctr",ss2%bidiff_dpass_ctr,ss3%bidiff_dpass_ctr,iout,ierr)
        ss3%bidiff_dpass_ctr = ss2%bidiff_dpass_ctr
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%bidiff_dtrap)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%bidiff_dtrap).AND.ALLOCATED(ss2%bidiff_dtrap)) THEN
        CALL ps_conformal("bidiff_dtrap",ss2%bidiff_dtrap,ss3%bidiff_dtrap,iout,ierr)
        CALL ps_conformal("bidiff_dtrap",ss1%bidiff_dtrap,ss3%bidiff_dtrap,iout,ierr)
        if(ierr.eq.0) ss3%bidiff_dtrap = w1*ss1%bidiff_dtrap + (1.0_rspec-w1)*ss2%bidiff_dtrap
     ELSE IF(ALLOCATED(ss1%bidiff_dtrap)) THEN
        CALL ps_conformal("bidiff_dtrap",ss1%bidiff_dtrap,ss3%bidiff_dtrap,iout,ierr)
        ss3%bidiff_dtrap = ss1%bidiff_dtrap
     ELSE IF(ALLOCATED(ss2%bidiff_dtrap)) THEN
        CALL ps_conformal("bidiff_dtrap",ss2%bidiff_dtrap,ss3%bidiff_dtrap,iout,ierr)
        ss3%bidiff_dtrap = ss2%bidiff_dtrap
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%BphiRZ)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%BphiRZ).AND.ALLOCATED(ss2%BphiRZ)) THEN
        CALL ps_conformal("BphiRZ",ss2%BphiRZ,ss3%BphiRZ,iout,ierr)
        CALL ps_conformal("BphiRZ",ss1%BphiRZ,ss3%BphiRZ,iout,ierr)
        if(ierr.eq.0) ss3%BphiRZ = w1*ss1%BphiRZ + (1.0_rspec-w1)*ss2%BphiRZ
     ELSE IF(ALLOCATED(ss1%BphiRZ)) THEN
        CALL ps_conformal("BphiRZ",ss1%BphiRZ,ss3%BphiRZ,iout,ierr)
        ss3%BphiRZ = ss1%BphiRZ
     ELSE IF(ALLOCATED(ss2%BphiRZ)) THEN
        CALL ps_conformal("BphiRZ",ss2%BphiRZ,ss3%BphiRZ,iout,ierr)
        ss3%BphiRZ = ss2%BphiRZ
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%BRRZ)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%BRRZ).AND.ALLOCATED(ss2%BRRZ)) THEN
        CALL ps_conformal("BRRZ",ss2%BRRZ,ss3%BRRZ,iout,ierr)
        CALL ps_conformal("BRRZ",ss1%BRRZ,ss3%BRRZ,iout,ierr)
        if(ierr.eq.0) ss3%BRRZ = w1*ss1%BRRZ + (1.0_rspec-w1)*ss2%BRRZ
     ELSE IF(ALLOCATED(ss1%BRRZ)) THEN
        CALL ps_conformal("BRRZ",ss1%BRRZ,ss3%BRRZ,iout,ierr)
        ss3%BRRZ = ss1%BRRZ
     ELSE IF(ALLOCATED(ss2%BRRZ)) THEN
        CALL ps_conformal("BRRZ",ss2%BRRZ,ss3%BRRZ,iout,ierr)
        ss3%BRRZ = ss2%BRRZ
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%BZRZ)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%BZRZ).AND.ALLOCATED(ss2%BZRZ)) THEN
        CALL ps_conformal("BZRZ",ss2%BZRZ,ss3%BZRZ,iout,ierr)
        CALL ps_conformal("BZRZ",ss1%BZRZ,ss3%BZRZ,iout,ierr)
        if(ierr.eq.0) ss3%BZRZ = w1*ss1%BZRZ + (1.0_rspec-w1)*ss2%BZRZ
     ELSE IF(ALLOCATED(ss1%BZRZ)) THEN
        CALL ps_conformal("BZRZ",ss1%BZRZ,ss3%BZRZ,iout,ierr)
        ss3%BZRZ = ss1%BZRZ
     ELSE IF(ALLOCATED(ss2%BZRZ)) THEN
        CALL ps_conformal("BZRZ",ss2%BZRZ,ss3%BZRZ,iout,ierr)
        ss3%BZRZ = ss2%BZRZ
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     ss3%B_axis = w1*ss1%B_axis + (1.0_rspec-w1)*ss2%B_axis
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     ss3%B_axis_vac = w1*ss1%B_axis_vac + (1.0_rspec-w1)*ss2%B_axis_vac
  ENDIF

  IF((ALLOCATED(ss3%b_halfHeight)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%b_halfHeight).AND.ALLOCATED(ss2%b_halfHeight)) THEN
        CALL ps_diff("b_halfHeight",ss1%b_halfHeight,ss2%b_halfHeight,icheck,iout,ierr)
        CALL ps_conformal("b_halfHeight",ss1%b_halfHeight,ss3%b_halfHeight,iout,ierr)
        if(ierr.eq.0) ss3%b_halfHeight = ss1%b_halfHeight
     ELSE IF(ALLOCATED(ss1%b_halfHeight)) THEN
        CALL ps_conformal("b_halfHeight",ss1%b_halfHeight,ss3%b_halfHeight,iout,ierr)
        ss3%b_halfHeight = ss1%b_halfHeight
     ELSE IF(ALLOCATED(ss2%b_halfHeight)) THEN
        CALL ps_conformal("b_halfHeight",ss2%b_halfHeight,ss3%b_halfHeight,iout,ierr)
        ss3%b_halfHeight = ss2%b_halfHeight
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%b_halfwidth)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%b_halfwidth).AND.ALLOCATED(ss2%b_halfwidth)) THEN
        CALL ps_diff("b_halfwidth",ss1%b_halfwidth,ss2%b_halfwidth,icheck,iout,ierr)
        CALL ps_conformal("b_halfwidth",ss1%b_halfwidth,ss3%b_halfwidth,iout,ierr)
        if(ierr.eq.0) ss3%b_halfwidth = ss1%b_halfwidth
     ELSE IF(ALLOCATED(ss1%b_halfwidth)) THEN
        CALL ps_conformal("b_halfwidth",ss1%b_halfwidth,ss3%b_halfwidth,iout,ierr)
        ss3%b_halfwidth = ss1%b_halfwidth
     ELSE IF(ALLOCATED(ss2%b_halfwidth)) THEN
        CALL ps_conformal("b_halfwidth",ss2%b_halfwidth,ss3%b_halfwidth,iout,ierr)
        ss3%b_halfwidth = ss2%b_halfwidth
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%b_Hdivergence)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%b_Hdivergence).AND.ALLOCATED(ss2%b_Hdivergence)) THEN
        CALL ps_diff("b_Hdivergence",ss1%b_Hdivergence,ss2%b_Hdivergence,icheck,iout,ierr)
        CALL ps_conformal("b_Hdivergence",ss1%b_Hdivergence,ss3%b_Hdivergence,iout,ierr)
        if(ierr.eq.0) ss3%b_Hdivergence = ss1%b_Hdivergence
     ELSE IF(ALLOCATED(ss1%b_Hdivergence)) THEN
        CALL ps_conformal("b_Hdivergence",ss1%b_Hdivergence,ss3%b_Hdivergence,iout,ierr)
        ss3%b_Hdivergence = ss1%b_Hdivergence
     ELSE IF(ALLOCATED(ss2%b_Hdivergence)) THEN
        CALL ps_conformal("b_Hdivergence",ss2%b_Hdivergence,ss3%b_Hdivergence,iout,ierr)
        ss3%b_Hdivergence = ss2%b_Hdivergence
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%b_Hfocal_length)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%b_Hfocal_length).AND.ALLOCATED(ss2%b_Hfocal_length)) THEN
        CALL ps_diff("b_Hfocal_length",ss1%b_Hfocal_length,ss2%b_Hfocal_length,icheck,iout,ierr)
        CALL ps_conformal("b_Hfocal_length",ss1%b_Hfocal_length,ss3%b_Hfocal_length,iout,ierr)
        if(ierr.eq.0) ss3%b_Hfocal_length = ss1%b_Hfocal_length
     ELSE IF(ALLOCATED(ss1%b_Hfocal_length)) THEN
        CALL ps_conformal("b_Hfocal_length",ss1%b_Hfocal_length,ss3%b_Hfocal_length,iout,ierr)
        ss3%b_Hfocal_length = ss1%b_Hfocal_length
     ELSE IF(ALLOCATED(ss2%b_Hfocal_length)) THEN
        CALL ps_conformal("b_Hfocal_length",ss2%b_Hfocal_length,ss3%b_Hfocal_length,iout,ierr)
        ss3%b_Hfocal_length = ss2%b_Hfocal_length
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     ss3%B_max_lcfs = w1*ss1%B_max_lcfs + (1.0_rspec-w1)*ss2%B_max_lcfs
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     ss3%B_min_lcfs = w1*ss1%B_min_lcfs + (1.0_rspec-w1)*ss2%B_min_lcfs
  ENDIF

  IF((ALLOCATED(ss3%B_surfMax)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%B_surfMax).AND.ALLOCATED(ss2%B_surfMax)) THEN
        CALL ps_conformal("B_surfMax",ss2%B_surfMax,ss3%B_surfMax,iout,ierr)
        CALL ps_conformal("B_surfMax",ss1%B_surfMax,ss3%B_surfMax,iout,ierr)
        if(ierr.eq.0) ss3%B_surfMax = w1*ss1%B_surfMax + (1.0_rspec-w1)*ss2%B_surfMax
     ELSE IF(ALLOCATED(ss1%B_surfMax)) THEN
        CALL ps_conformal("B_surfMax",ss1%B_surfMax,ss3%B_surfMax,iout,ierr)
        ss3%B_surfMax = ss1%B_surfMax
     ELSE IF(ALLOCATED(ss2%B_surfMax)) THEN
        CALL ps_conformal("B_surfMax",ss2%B_surfMax,ss3%B_surfMax,iout,ierr)
        ss3%B_surfMax = ss2%B_surfMax
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%B_surfMin)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%B_surfMin).AND.ALLOCATED(ss2%B_surfMin)) THEN
        CALL ps_conformal("B_surfMin",ss2%B_surfMin,ss3%B_surfMin,iout,ierr)
        CALL ps_conformal("B_surfMin",ss1%B_surfMin,ss3%B_surfMin,iout,ierr)
        if(ierr.eq.0) ss3%B_surfMin = w1*ss1%B_surfMin + (1.0_rspec-w1)*ss2%B_surfMin
     ELSE IF(ALLOCATED(ss1%B_surfMin)) THEN
        CALL ps_conformal("B_surfMin",ss1%B_surfMin,ss3%B_surfMin,iout,ierr)
        ss3%B_surfMin = ss1%B_surfMin
     ELSE IF(ALLOCATED(ss2%B_surfMin)) THEN
        CALL ps_conformal("B_surfMin",ss2%B_surfMin,ss3%B_surfMin,iout,ierr)
        ss3%B_surfMin = ss2%B_surfMin
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%b_Vdivergence)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%b_Vdivergence).AND.ALLOCATED(ss2%b_Vdivergence)) THEN
        CALL ps_diff("b_Vdivergence",ss1%b_Vdivergence,ss2%b_Vdivergence,icheck,iout,ierr)
        CALL ps_conformal("b_Vdivergence",ss1%b_Vdivergence,ss3%b_Vdivergence,iout,ierr)
        if(ierr.eq.0) ss3%b_Vdivergence = ss1%b_Vdivergence
     ELSE IF(ALLOCATED(ss1%b_Vdivergence)) THEN
        CALL ps_conformal("b_Vdivergence",ss1%b_Vdivergence,ss3%b_Vdivergence,iout,ierr)
        ss3%b_Vdivergence = ss1%b_Vdivergence
     ELSE IF(ALLOCATED(ss2%b_Vdivergence)) THEN
        CALL ps_conformal("b_Vdivergence",ss2%b_Vdivergence,ss3%b_Vdivergence,iout,ierr)
        ss3%b_Vdivergence = ss2%b_Vdivergence
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%b_Vfocal_length)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%b_Vfocal_length).AND.ALLOCATED(ss2%b_Vfocal_length)) THEN
        CALL ps_diff("b_Vfocal_length",ss1%b_Vfocal_length,ss2%b_Vfocal_length,icheck,iout,ierr)
        CALL ps_conformal("b_Vfocal_length",ss1%b_Vfocal_length,ss3%b_Vfocal_length,iout,ierr)
        if(ierr.eq.0) ss3%b_Vfocal_length = ss1%b_Vfocal_length
     ELSE IF(ALLOCATED(ss1%b_Vfocal_length)) THEN
        CALL ps_conformal("b_Vfocal_length",ss1%b_Vfocal_length,ss3%b_Vfocal_length,iout,ierr)
        ss3%b_Vfocal_length = ss1%b_Vfocal_length
     ELSE IF(ALLOCATED(ss2%b_Vfocal_length)) THEN
        CALL ps_conformal("b_Vfocal_length",ss2%b_Vfocal_length,ss3%b_Vfocal_length,iout,ierr)
        ss3%b_Vfocal_length = ss2%b_Vfocal_length
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%cdicrf)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%cdicrf).AND.ALLOCATED(ss2%cdicrf)) THEN
        CALL ps_conformal("cdicrf",ss2%cdicrf,ss3%cdicrf,iout,ierr)
        CALL ps_conformal("cdicrf",ss1%cdicrf,ss3%cdicrf,iout,ierr)
        if(ierr.eq.0) ss3%cdicrf = w1*ss1%cdicrf + (1.0_rspec-w1)*ss2%cdicrf
     ELSE IF(ALLOCATED(ss1%cdicrf)) THEN
        CALL ps_conformal("cdicrf",ss1%cdicrf,ss3%cdicrf,iout,ierr)
        ss3%cdicrf = ss1%cdicrf
     ELSE IF(ALLOCATED(ss2%cdicrf)) THEN
        CALL ps_conformal("cdicrf",ss2%cdicrf,ss3%cdicrf,iout,ierr)
        ss3%cdicrf = ss2%cdicrf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%cdicrf_nphi)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%cdicrf_nphi).AND.ALLOCATED(ss2%cdicrf_nphi)) THEN
        CALL ps_conformal("cdicrf_nphi",ss2%cdicrf_nphi,ss3%cdicrf_nphi,iout,ierr)
        CALL ps_conformal("cdicrf_nphi",ss1%cdicrf_nphi,ss3%cdicrf_nphi,iout,ierr)
        if(ierr.eq.0) ss3%cdicrf_nphi = w1*ss1%cdicrf_nphi + (1.0_rspec-w1)*ss2%cdicrf_nphi
     ELSE IF(ALLOCATED(ss1%cdicrf_nphi)) THEN
        CALL ps_conformal("cdicrf_nphi",ss1%cdicrf_nphi,ss3%cdicrf_nphi,iout,ierr)
        ss3%cdicrf_nphi = ss1%cdicrf_nphi
     ELSE IF(ALLOCATED(ss2%cdicrf_nphi)) THEN
        CALL ps_conformal("cdicrf_nphi",ss2%cdicrf_nphi,ss3%cdicrf_nphi,iout,ierr)
        ss3%cdicrf_nphi = ss2%cdicrf_nphi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_anom).AND.ALLOCATED(ss2%chie_anom)) THEN
        CALL ps_conformal("chie_anom",ss2%chie_anom,ss3%chie_anom,iout,ierr)
        CALL ps_conformal("chie_anom",ss1%chie_anom,ss3%chie_anom,iout,ierr)
        if(ierr.eq.0) ss3%chie_anom = w1*ss1%chie_anom + (1.0_rspec-w1)*ss2%chie_anom
     ELSE IF(ALLOCATED(ss1%chie_anom)) THEN
        CALL ps_conformal("chie_anom",ss1%chie_anom,ss3%chie_anom,iout,ierr)
        ss3%chie_anom = ss1%chie_anom
     ELSE IF(ALLOCATED(ss2%chie_anom)) THEN
        CALL ps_conformal("chie_anom",ss2%chie_anom,ss3%chie_anom,iout,ierr)
        ss3%chie_anom = ss2%chie_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_drbm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_drbm).AND.ALLOCATED(ss2%chie_drbm)) THEN
        CALL ps_conformal("chie_drbm",ss2%chie_drbm,ss3%chie_drbm,iout,ierr)
        CALL ps_conformal("chie_drbm",ss1%chie_drbm,ss3%chie_drbm,iout,ierr)
        if(ierr.eq.0) ss3%chie_drbm = w1*ss1%chie_drbm + (1.0_rspec-w1)*ss2%chie_drbm
     ELSE IF(ALLOCATED(ss1%chie_drbm)) THEN
        CALL ps_conformal("chie_drbm",ss1%chie_drbm,ss3%chie_drbm,iout,ierr)
        ss3%chie_drbm = ss1%chie_drbm
     ELSE IF(ALLOCATED(ss2%chie_drbm)) THEN
        CALL ps_conformal("chie_drbm",ss2%chie_drbm,ss3%chie_drbm,iout,ierr)
        ss3%chie_drbm = ss2%chie_drbm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_etg)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_etg).AND.ALLOCATED(ss2%chie_etg)) THEN
        CALL ps_conformal("chie_etg",ss2%chie_etg,ss3%chie_etg,iout,ierr)
        CALL ps_conformal("chie_etg",ss1%chie_etg,ss3%chie_etg,iout,ierr)
        if(ierr.eq.0) ss3%chie_etg = w1*ss1%chie_etg + (1.0_rspec-w1)*ss2%chie_etg
     ELSE IF(ALLOCATED(ss1%chie_etg)) THEN
        CALL ps_conformal("chie_etg",ss1%chie_etg,ss3%chie_etg,iout,ierr)
        ss3%chie_etg = ss1%chie_etg
     ELSE IF(ALLOCATED(ss2%chie_etg)) THEN
        CALL ps_conformal("chie_etg",ss2%chie_etg,ss3%chie_etg,iout,ierr)
        ss3%chie_etg = ss2%chie_etg
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_glf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_glf).AND.ALLOCATED(ss2%chie_glf)) THEN
        CALL ps_conformal("chie_glf",ss2%chie_glf,ss3%chie_glf,iout,ierr)
        CALL ps_conformal("chie_glf",ss1%chie_glf,ss3%chie_glf,iout,ierr)
        if(ierr.eq.0) ss3%chie_glf = w1*ss1%chie_glf + (1.0_rspec-w1)*ss2%chie_glf
     ELSE IF(ALLOCATED(ss1%chie_glf)) THEN
        CALL ps_conformal("chie_glf",ss1%chie_glf,ss3%chie_glf,iout,ierr)
        ss3%chie_glf = ss1%chie_glf
     ELSE IF(ALLOCATED(ss2%chie_glf)) THEN
        CALL ps_conformal("chie_glf",ss2%chie_glf,ss3%chie_glf,iout,ierr)
        ss3%chie_glf = ss2%chie_glf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_gtcneo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_gtcneo).AND.ALLOCATED(ss2%chie_gtcneo)) THEN
        CALL ps_conformal("chie_gtcneo",ss2%chie_gtcneo,ss3%chie_gtcneo,iout,ierr)
        CALL ps_conformal("chie_gtcneo",ss1%chie_gtcneo,ss3%chie_gtcneo,iout,ierr)
        if(ierr.eq.0) ss3%chie_gtcneo = w1*ss1%chie_gtcneo + (1.0_rspec-w1)*ss2%chie_gtcneo
     ELSE IF(ALLOCATED(ss1%chie_gtcneo)) THEN
        CALL ps_conformal("chie_gtcneo",ss1%chie_gtcneo,ss3%chie_gtcneo,iout,ierr)
        ss3%chie_gtcneo = ss1%chie_gtcneo
     ELSE IF(ALLOCATED(ss2%chie_gtcneo)) THEN
        CALL ps_conformal("chie_gtcneo",ss2%chie_gtcneo,ss3%chie_gtcneo,iout,ierr)
        ss3%chie_gtcneo = ss2%chie_gtcneo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_mmm71)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_mmm71).AND.ALLOCATED(ss2%chie_mmm71)) THEN
        CALL ps_conformal("chie_mmm71",ss2%chie_mmm71,ss3%chie_mmm71,iout,ierr)
        CALL ps_conformal("chie_mmm71",ss1%chie_mmm71,ss3%chie_mmm71,iout,ierr)
        if(ierr.eq.0) ss3%chie_mmm71 = w1*ss1%chie_mmm71 + (1.0_rspec-w1)*ss2%chie_mmm71
     ELSE IF(ALLOCATED(ss1%chie_mmm71)) THEN
        CALL ps_conformal("chie_mmm71",ss1%chie_mmm71,ss3%chie_mmm71,iout,ierr)
        ss3%chie_mmm71 = ss1%chie_mmm71
     ELSE IF(ALLOCATED(ss2%chie_mmm71)) THEN
        CALL ps_conformal("chie_mmm71",ss2%chie_mmm71,ss3%chie_mmm71,iout,ierr)
        ss3%chie_mmm71 = ss2%chie_mmm71
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_mtm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_mtm).AND.ALLOCATED(ss2%chie_mtm)) THEN
        CALL ps_conformal("chie_mtm",ss2%chie_mtm,ss3%chie_mtm,iout,ierr)
        CALL ps_conformal("chie_mtm",ss1%chie_mtm,ss3%chie_mtm,iout,ierr)
        if(ierr.eq.0) ss3%chie_mtm = w1*ss1%chie_mtm + (1.0_rspec-w1)*ss2%chie_mtm
     ELSE IF(ALLOCATED(ss1%chie_mtm)) THEN
        CALL ps_conformal("chie_mtm",ss1%chie_mtm,ss3%chie_mtm,iout,ierr)
        ss3%chie_mtm = ss1%chie_mtm
     ELSE IF(ALLOCATED(ss2%chie_mtm)) THEN
        CALL ps_conformal("chie_mtm",ss2%chie_mtm,ss3%chie_mtm,iout,ierr)
        ss3%chie_mtm = ss2%chie_mtm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_nclass)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_nclass).AND.ALLOCATED(ss2%chie_nclass)) THEN
        CALL ps_conformal("chie_nclass",ss2%chie_nclass,ss3%chie_nclass,iout,ierr)
        CALL ps_conformal("chie_nclass",ss1%chie_nclass,ss3%chie_nclass,iout,ierr)
        if(ierr.eq.0) ss3%chie_nclass = w1*ss1%chie_nclass + (1.0_rspec-w1)*ss2%chie_nclass
     ELSE IF(ALLOCATED(ss1%chie_nclass)) THEN
        CALL ps_conformal("chie_nclass",ss1%chie_nclass,ss3%chie_nclass,iout,ierr)
        ss3%chie_nclass = ss1%chie_nclass
     ELSE IF(ALLOCATED(ss2%chie_nclass)) THEN
        CALL ps_conformal("chie_nclass",ss2%chie_nclass,ss3%chie_nclass,iout,ierr)
        ss3%chie_nclass = ss2%chie_nclass
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_neo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_neo).AND.ALLOCATED(ss2%chie_neo)) THEN
        CALL ps_conformal("chie_neo",ss2%chie_neo,ss3%chie_neo,iout,ierr)
        CALL ps_conformal("chie_neo",ss1%chie_neo,ss3%chie_neo,iout,ierr)
        if(ierr.eq.0) ss3%chie_neo = w1*ss1%chie_neo + (1.0_rspec-w1)*ss2%chie_neo
     ELSE IF(ALLOCATED(ss1%chie_neo)) THEN
        CALL ps_conformal("chie_neo",ss1%chie_neo,ss3%chie_neo,iout,ierr)
        ss3%chie_neo = ss1%chie_neo
     ELSE IF(ALLOCATED(ss2%chie_neo)) THEN
        CALL ps_conformal("chie_neo",ss2%chie_neo,ss3%chie_neo,iout,ierr)
        ss3%chie_neo = ss2%chie_neo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_ntm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_ntm).AND.ALLOCATED(ss2%chie_ntm)) THEN
        CALL ps_conformal("chie_ntm",ss2%chie_ntm,ss3%chie_ntm,iout,ierr)
        CALL ps_conformal("chie_ntm",ss1%chie_ntm,ss3%chie_ntm,iout,ierr)
        if(ierr.eq.0) ss3%chie_ntm = w1*ss1%chie_ntm + (1.0_rspec-w1)*ss2%chie_ntm
     ELSE IF(ALLOCATED(ss1%chie_ntm)) THEN
        CALL ps_conformal("chie_ntm",ss1%chie_ntm,ss3%chie_ntm,iout,ierr)
        ss3%chie_ntm = ss1%chie_ntm
     ELSE IF(ALLOCATED(ss2%chie_ntm)) THEN
        CALL ps_conformal("chie_ntm",ss2%chie_ntm,ss3%chie_ntm,iout,ierr)
        ss3%chie_ntm = ss2%chie_ntm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_paleo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_paleo).AND.ALLOCATED(ss2%chie_paleo)) THEN
        CALL ps_conformal("chie_paleo",ss2%chie_paleo,ss3%chie_paleo,iout,ierr)
        CALL ps_conformal("chie_paleo",ss1%chie_paleo,ss3%chie_paleo,iout,ierr)
        if(ierr.eq.0) ss3%chie_paleo = w1*ss1%chie_paleo + (1.0_rspec-w1)*ss2%chie_paleo
     ELSE IF(ALLOCATED(ss1%chie_paleo)) THEN
        CALL ps_conformal("chie_paleo",ss1%chie_paleo,ss3%chie_paleo,iout,ierr)
        ss3%chie_paleo = ss1%chie_paleo
     ELSE IF(ALLOCATED(ss2%chie_paleo)) THEN
        CALL ps_conformal("chie_paleo",ss2%chie_paleo,ss3%chie_paleo,iout,ierr)
        ss3%chie_paleo = ss2%chie_paleo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_tglf).AND.ALLOCATED(ss2%chie_tglf)) THEN
        CALL ps_conformal("chie_tglf",ss2%chie_tglf,ss3%chie_tglf,iout,ierr)
        CALL ps_conformal("chie_tglf",ss1%chie_tglf,ss3%chie_tglf,iout,ierr)
        if(ierr.eq.0) ss3%chie_tglf = w1*ss1%chie_tglf + (1.0_rspec-w1)*ss2%chie_tglf
     ELSE IF(ALLOCATED(ss1%chie_tglf)) THEN
        CALL ps_conformal("chie_tglf",ss1%chie_tglf,ss3%chie_tglf,iout,ierr)
        ss3%chie_tglf = ss1%chie_tglf
     ELSE IF(ALLOCATED(ss2%chie_tglf)) THEN
        CALL ps_conformal("chie_tglf",ss2%chie_tglf,ss3%chie_tglf,iout,ierr)
        ss3%chie_tglf = ss2%chie_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_trans)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_trans).AND.ALLOCATED(ss2%chie_trans)) THEN
        CALL ps_conformal("chie_trans",ss2%chie_trans,ss3%chie_trans,iout,ierr)
        CALL ps_conformal("chie_trans",ss1%chie_trans,ss3%chie_trans,iout,ierr)
        if(ierr.eq.0) ss3%chie_trans = w1*ss1%chie_trans + (1.0_rspec-w1)*ss2%chie_trans
     ELSE IF(ALLOCATED(ss1%chie_trans)) THEN
        CALL ps_conformal("chie_trans",ss1%chie_trans,ss3%chie_trans,iout,ierr)
        ss3%chie_trans = ss1%chie_trans
     ELSE IF(ALLOCATED(ss2%chie_trans)) THEN
        CALL ps_conformal("chie_trans",ss2%chie_trans,ss3%chie_trans,iout,ierr)
        ss3%chie_trans = ss2%chie_trans
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_transcat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_transcat).AND.ALLOCATED(ss2%chie_transcat)) THEN
        CALL ps_conformal("chie_transcat",ss2%chie_transcat,ss3%chie_transcat,iout,ierr)
        CALL ps_conformal("chie_transcat",ss1%chie_transcat,ss3%chie_transcat,iout,ierr)
        if(ierr.eq.0) ss3%chie_transcat = w1*ss1%chie_transcat + (1.0_rspec-w1)*ss2%chie_transcat
     ELSE IF(ALLOCATED(ss1%chie_transcat)) THEN
        CALL ps_conformal("chie_transcat",ss1%chie_transcat,ss3%chie_transcat,iout,ierr)
        ss3%chie_transcat = ss1%chie_transcat
     ELSE IF(ALLOCATED(ss2%chie_transcat)) THEN
        CALL ps_conformal("chie_transcat",ss2%chie_transcat,ss3%chie_transcat,iout,ierr)
        ss3%chie_transcat = ss2%chie_transcat
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chie_w19)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chie_w19).AND.ALLOCATED(ss2%chie_w19)) THEN
        CALL ps_conformal("chie_w19",ss2%chie_w19,ss3%chie_w19,iout,ierr)
        CALL ps_conformal("chie_w19",ss1%chie_w19,ss3%chie_w19,iout,ierr)
        if(ierr.eq.0) ss3%chie_w19 = w1*ss1%chie_w19 + (1.0_rspec-w1)*ss2%chie_w19
     ELSE IF(ALLOCATED(ss1%chie_w19)) THEN
        CALL ps_conformal("chie_w19",ss1%chie_w19,ss3%chie_w19,iout,ierr)
        ss3%chie_w19 = ss1%chie_w19
     ELSE IF(ALLOCATED(ss2%chie_w19)) THEN
        CALL ps_conformal("chie_w19",ss2%chie_w19,ss3%chie_w19,iout,ierr)
        ss3%chie_w19 = ss2%chie_w19
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chii_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chii_anom).AND.ALLOCATED(ss2%chii_anom)) THEN
        CALL ps_conformal("chii_anom",ss2%chii_anom,ss3%chii_anom,iout,ierr)
        CALL ps_conformal("chii_anom",ss1%chii_anom,ss3%chii_anom,iout,ierr)
        if(ierr.eq.0) ss3%chii_anom = w1*ss1%chii_anom + (1.0_rspec-w1)*ss2%chii_anom
     ELSE IF(ALLOCATED(ss1%chii_anom)) THEN
        CALL ps_conformal("chii_anom",ss1%chii_anom,ss3%chii_anom,iout,ierr)
        ss3%chii_anom = ss1%chii_anom
     ELSE IF(ALLOCATED(ss2%chii_anom)) THEN
        CALL ps_conformal("chii_anom",ss2%chii_anom,ss3%chii_anom,iout,ierr)
        ss3%chii_anom = ss2%chii_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chii_drbm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chii_drbm).AND.ALLOCATED(ss2%chii_drbm)) THEN
        CALL ps_conformal("chii_drbm",ss2%chii_drbm,ss3%chii_drbm,iout,ierr)
        CALL ps_conformal("chii_drbm",ss1%chii_drbm,ss3%chii_drbm,iout,ierr)
        if(ierr.eq.0) ss3%chii_drbm = w1*ss1%chii_drbm + (1.0_rspec-w1)*ss2%chii_drbm
     ELSE IF(ALLOCATED(ss1%chii_drbm)) THEN
        CALL ps_conformal("chii_drbm",ss1%chii_drbm,ss3%chii_drbm,iout,ierr)
        ss3%chii_drbm = ss1%chii_drbm
     ELSE IF(ALLOCATED(ss2%chii_drbm)) THEN
        CALL ps_conformal("chii_drbm",ss2%chii_drbm,ss3%chii_drbm,iout,ierr)
        ss3%chii_drbm = ss2%chii_drbm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chii_glf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chii_glf).AND.ALLOCATED(ss2%chii_glf)) THEN
        CALL ps_conformal("chii_glf",ss2%chii_glf,ss3%chii_glf,iout,ierr)
        CALL ps_conformal("chii_glf",ss1%chii_glf,ss3%chii_glf,iout,ierr)
        if(ierr.eq.0) ss3%chii_glf = w1*ss1%chii_glf + (1.0_rspec-w1)*ss2%chii_glf
     ELSE IF(ALLOCATED(ss1%chii_glf)) THEN
        CALL ps_conformal("chii_glf",ss1%chii_glf,ss3%chii_glf,iout,ierr)
        ss3%chii_glf = ss1%chii_glf
     ELSE IF(ALLOCATED(ss2%chii_glf)) THEN
        CALL ps_conformal("chii_glf",ss2%chii_glf,ss3%chii_glf,iout,ierr)
        ss3%chii_glf = ss2%chii_glf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chii_gtcneo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chii_gtcneo).AND.ALLOCATED(ss2%chii_gtcneo)) THEN
        CALL ps_conformal("chii_gtcneo",ss2%chii_gtcneo,ss3%chii_gtcneo,iout,ierr)
        CALL ps_conformal("chii_gtcneo",ss1%chii_gtcneo,ss3%chii_gtcneo,iout,ierr)
        if(ierr.eq.0) ss3%chii_gtcneo = w1*ss1%chii_gtcneo + (1.0_rspec-w1)*ss2%chii_gtcneo
     ELSE IF(ALLOCATED(ss1%chii_gtcneo)) THEN
        CALL ps_conformal("chii_gtcneo",ss1%chii_gtcneo,ss3%chii_gtcneo,iout,ierr)
        ss3%chii_gtcneo = ss1%chii_gtcneo
     ELSE IF(ALLOCATED(ss2%chii_gtcneo)) THEN
        CALL ps_conformal("chii_gtcneo",ss2%chii_gtcneo,ss3%chii_gtcneo,iout,ierr)
        ss3%chii_gtcneo = ss2%chii_gtcneo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chii_mmm71)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chii_mmm71).AND.ALLOCATED(ss2%chii_mmm71)) THEN
        CALL ps_conformal("chii_mmm71",ss2%chii_mmm71,ss3%chii_mmm71,iout,ierr)
        CALL ps_conformal("chii_mmm71",ss1%chii_mmm71,ss3%chii_mmm71,iout,ierr)
        if(ierr.eq.0) ss3%chii_mmm71 = w1*ss1%chii_mmm71 + (1.0_rspec-w1)*ss2%chii_mmm71
     ELSE IF(ALLOCATED(ss1%chii_mmm71)) THEN
        CALL ps_conformal("chii_mmm71",ss1%chii_mmm71,ss3%chii_mmm71,iout,ierr)
        ss3%chii_mmm71 = ss1%chii_mmm71
     ELSE IF(ALLOCATED(ss2%chii_mmm71)) THEN
        CALL ps_conformal("chii_mmm71",ss2%chii_mmm71,ss3%chii_mmm71,iout,ierr)
        ss3%chii_mmm71 = ss2%chii_mmm71
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chii_nclass)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chii_nclass).AND.ALLOCATED(ss2%chii_nclass)) THEN
        CALL ps_conformal("chii_nclass",ss2%chii_nclass,ss3%chii_nclass,iout,ierr)
        CALL ps_conformal("chii_nclass",ss1%chii_nclass,ss3%chii_nclass,iout,ierr)
        if(ierr.eq.0) ss3%chii_nclass = w1*ss1%chii_nclass + (1.0_rspec-w1)*ss2%chii_nclass
     ELSE IF(ALLOCATED(ss1%chii_nclass)) THEN
        CALL ps_conformal("chii_nclass",ss1%chii_nclass,ss3%chii_nclass,iout,ierr)
        ss3%chii_nclass = ss1%chii_nclass
     ELSE IF(ALLOCATED(ss2%chii_nclass)) THEN
        CALL ps_conformal("chii_nclass",ss2%chii_nclass,ss3%chii_nclass,iout,ierr)
        ss3%chii_nclass = ss2%chii_nclass
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%chii_neo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%chii_neo).AND.ALLOCATED(ss2%chii_neo)) THEN
        CALL ps_conformal("chii_neo",ss2%chii_neo,ss3%chii_neo,iout,ierr)
        CALL ps_conformal("chii_neo",ss1%chii_neo,ss3%chii_neo,iout,ierr)
        if(ierr.eq.0) ss3%chii_neo = w1*ss1%chii_neo + (1.0_rspec-w1)*ss2%chii_neo
     ELSE IF(ALLOCATED(ss1%chii_neo)) THEN
        CALL ps_conformal("chii_neo",ss1%chii_neo,ss3%chii_neo,iout,ierr)
        ss3%chii_neo = ss1%chii_neo
     ELSE IF(ALLOCATED(ss2%chii_neo)) THEN
        CALL ps_conformal("chii_neo",ss2%chii_neo,ss3%chii_neo,iout,ierr)
        ss3%chii_neo = ss2%chii_neo
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_merge_0
