! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_merge_7(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)


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

  IF((ALLOCATED(ss3%rho_pel)).AND.(cclist(ps_cnum_PEL).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_pel).AND.ALLOCATED(ss2%rho_pel)) THEN
        CALL ps_diff("rho_pel",ss1%rho_pel,ss2%rho_pel,ps_error,iout,ierr)
        CALL ps_conformal("rho_pel",ss1%rho_pel,ss3%rho_pel,iout,ierr)
        if(ierr.eq.0) ss3%rho_pel = ss1%rho_pel
     ELSE IF(ALLOCATED(ss1%rho_pel)) THEN
        CALL ps_conformal("rho_pel",ss1%rho_pel,ss3%rho_pel,iout,ierr)
        ss3%rho_pel = ss1%rho_pel
     ELSE IF(ALLOCATED(ss2%rho_pel)) THEN
        CALL ps_conformal("rho_pel",ss2%rho_pel,ss3%rho_pel,iout,ierr)
        ss3%rho_pel = ss2%rho_pel
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_rad)).AND.(cclist(ps_cnum_RAD).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_rad).AND.ALLOCATED(ss2%rho_rad)) THEN
        CALL ps_diff("rho_rad",ss1%rho_rad,ss2%rho_rad,ps_error,iout,ierr)
        CALL ps_conformal("rho_rad",ss1%rho_rad,ss3%rho_rad,iout,ierr)
        if(ierr.eq.0) ss3%rho_rad = ss1%rho_rad
     ELSE IF(ALLOCATED(ss1%rho_rad)) THEN
        CALL ps_conformal("rho_rad",ss1%rho_rad,ss3%rho_rad,iout,ierr)
        ss3%rho_rad = ss1%rho_rad
     ELSE IF(ALLOCATED(ss2%rho_rad)) THEN
        CALL ps_conformal("rho_rad",ss2%rho_rad,ss3%rho_rad,iout,ierr)
        ss3%rho_rad = ss2%rho_rad
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rho_rw)).AND.(cclist(ps_cnum_RUNAWAY).EQ.1)) THEN
     IF(ALLOCATED(ss1%rho_rw).AND.ALLOCATED(ss2%rho_rw)) THEN
        CALL ps_diff("rho_rw",ss1%rho_rw,ss2%rho_rw,ps_error,iout,ierr)
        CALL ps_conformal("rho_rw",ss1%rho_rw,ss3%rho_rw,iout,ierr)
        if(ierr.eq.0) ss3%rho_rw = ss1%rho_rw
     ELSE IF(ALLOCATED(ss1%rho_rw)) THEN
        CALL ps_conformal("rho_rw",ss1%rho_rw,ss3%rho_rw,iout,ierr)
        ss3%rho_rw = ss1%rho_rw
     ELSE IF(ALLOCATED(ss2%rho_rw)) THEN
        CALL ps_conformal("rho_rw",ss2%rho_rw,ss3%rho_rw,iout,ierr)
        ss3%rho_rw = ss2%rho_rw
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rlim)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%rlim).AND.ALLOCATED(ss2%rlim)) THEN
        CALL ps_diff("rlim",ss1%rlim,ss2%rlim,icheck,iout,ierr)
        CALL ps_conformal("rlim",ss1%rlim,ss3%rlim,iout,ierr)
        if(ierr.eq.0) ss3%rlim = ss1%rlim
     ELSE IF(ALLOCATED(ss1%rlim)) THEN
        CALL ps_conformal("rlim",ss1%rlim,ss3%rlim,iout,ierr)
        ss3%rlim = ss1%rlim
     ELSE IF(ALLOCATED(ss2%rlim)) THEN
        CALL ps_conformal("rlim",ss2%rlim,ss3%rlim,iout,ierr)
        ss3%rlim = ss2%rlim
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Rloc_coil)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Rloc_coil).AND.ALLOCATED(ss2%Rloc_coil)) THEN
        CALL ps_diff("Rloc_coil",ss1%Rloc_coil,ss2%Rloc_coil,icheck,iout,ierr)
        CALL ps_conformal("Rloc_coil",ss1%Rloc_coil,ss3%Rloc_coil,iout,ierr)
        if(ierr.eq.0) ss3%Rloc_coil = ss1%Rloc_coil
     ELSE IF(ALLOCATED(ss1%Rloc_coil)) THEN
        CALL ps_conformal("Rloc_coil",ss1%Rloc_coil,ss3%Rloc_coil,iout,ierr)
        ss3%Rloc_coil = ss1%Rloc_coil
     ELSE IF(ALLOCATED(ss2%Rloc_coil)) THEN
        CALL ps_conformal("Rloc_coil",ss2%Rloc_coil,ss3%Rloc_coil,iout,ierr)
        ss3%Rloc_coil = ss2%Rloc_coil
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Rmajor_mean)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%Rmajor_mean).AND.ALLOCATED(ss2%Rmajor_mean)) THEN
        CALL ps_conformal("Rmajor_mean",ss2%Rmajor_mean,ss3%Rmajor_mean,iout,ierr)
        CALL ps_conformal("Rmajor_mean",ss1%Rmajor_mean,ss3%Rmajor_mean,iout,ierr)
        if(ierr.eq.0) ss3%Rmajor_mean = w1*ss1%Rmajor_mean + (1.0_rspec-w1)*ss2%Rmajor_mean
     ELSE IF(ALLOCATED(ss1%Rmajor_mean)) THEN
        CALL ps_conformal("Rmajor_mean",ss1%Rmajor_mean,ss3%Rmajor_mean,iout,ierr)
        ss3%Rmajor_mean = ss1%Rmajor_mean
     ELSE IF(ALLOCATED(ss2%Rmajor_mean)) THEN
        CALL ps_conformal("Rmajor_mean",ss2%Rmajor_mean,ss3%Rmajor_mean,iout,ierr)
        ss3%Rmajor_mean = ss2%Rmajor_mean
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%rMinor_mean)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%rMinor_mean).AND.ALLOCATED(ss2%rMinor_mean)) THEN
        CALL ps_conformal("rMinor_mean",ss2%rMinor_mean,ss3%rMinor_mean,iout,ierr)
        CALL ps_conformal("rMinor_mean",ss1%rMinor_mean,ss3%rMinor_mean,iout,ierr)
        if(ierr.eq.0) ss3%rMinor_mean = w1*ss1%rMinor_mean + (1.0_rspec-w1)*ss2%rMinor_mean
     ELSE IF(ALLOCATED(ss1%rMinor_mean)) THEN
        CALL ps_conformal("rMinor_mean",ss1%rMinor_mean,ss3%rMinor_mean,iout,ierr)
        ss3%rMinor_mean = ss1%rMinor_mean
     ELSE IF(ALLOCATED(ss2%rMinor_mean)) THEN
        CALL ps_conformal("rMinor_mean",ss2%rMinor_mean,ss3%rMinor_mean,iout,ierr)
        ss3%rMinor_mean = ss2%rMinor_mean
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_RUNAWAY).EQ.1) THEN

     CALL ps_diff("RUNAWAY_Code_Info",ss1%RUNAWAY_Code_Info,ss2%RUNAWAY_Code_Info,icheck,iout,ierr)
     ss3%RUNAWAY_Code_Info = ss1%RUNAWAY_Code_Info
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("RunID",ss1%RunID,ss2%RunID,icheck,iout,ierr)
     ss3%RunID = ss1%RunID
  ENDIF

  IF((ALLOCATED(ss3%R_ant)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%R_ant).AND.ALLOCATED(ss2%R_ant)) THEN
        CALL ps_diff("R_ant",ss1%R_ant,ss2%R_ant,icheck,iout,ierr)
        CALL ps_conformal("R_ant",ss1%R_ant,ss3%R_ant,iout,ierr)
        if(ierr.eq.0) ss3%R_ant = ss1%R_ant
     ELSE IF(ALLOCATED(ss1%R_ant)) THEN
        CALL ps_conformal("R_ant",ss1%R_ant,ss3%R_ant,iout,ierr)
        ss3%R_ant = ss1%R_ant
     ELSE IF(ALLOCATED(ss2%R_ant)) THEN
        CALL ps_conformal("R_ant",ss2%R_ant,ss3%R_ant,iout,ierr)
        ss3%R_ant = ss2%R_ant
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%R_antgeo)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%R_antgeo).AND.ALLOCATED(ss2%R_antgeo)) THEN
        CALL ps_diff("R_antgeo",ss1%R_antgeo,ss2%R_antgeo,icheck,iout,ierr)
        CALL ps_conformal("R_antgeo",ss1%R_antgeo,ss3%R_antgeo,iout,ierr)
        if(ierr.eq.0) ss3%R_antgeo = ss1%R_antgeo
     ELSE IF(ALLOCATED(ss1%R_antgeo)) THEN
        CALL ps_conformal("R_antgeo",ss1%R_antgeo,ss3%R_antgeo,iout,ierr)
        ss3%R_antgeo = ss1%R_antgeo
     ELSE IF(ALLOCATED(ss2%R_antgeo)) THEN
        CALL ps_conformal("R_antgeo",ss2%R_antgeo,ss3%R_antgeo,iout,ierr)
        ss3%R_antgeo = ss2%R_antgeo
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     ss3%R_axis = w1*ss1%R_axis + (1.0_rspec-w1)*ss2%R_axis
  ENDIF

  IF((ALLOCATED(ss3%R_EC_launch)).AND.(cclist(ps_cnum_EC).EQ.1)) THEN
     IF(ALLOCATED(ss1%R_EC_launch).AND.ALLOCATED(ss2%R_EC_launch)) THEN
        CALL ps_diff("R_EC_launch",ss1%R_EC_launch,ss2%R_EC_launch,icheck,iout,ierr)
        CALL ps_conformal("R_EC_launch",ss1%R_EC_launch,ss3%R_EC_launch,iout,ierr)
        if(ierr.eq.0) ss3%R_EC_launch = ss1%R_EC_launch
     ELSE IF(ALLOCATED(ss1%R_EC_launch)) THEN
        CALL ps_conformal("R_EC_launch",ss1%R_EC_launch,ss3%R_EC_launch,iout,ierr)
        ss3%R_EC_launch = ss1%R_EC_launch
     ELSE IF(ALLOCATED(ss2%R_EC_launch)) THEN
        CALL ps_conformal("R_EC_launch",ss2%R_EC_launch,ss3%R_EC_launch,iout,ierr)
        ss3%R_EC_launch = ss2%R_EC_launch
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%R_geo)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%R_geo).AND.ALLOCATED(ss2%R_geo)) THEN
        CALL ps_conformal("R_geo",ss2%R_geo,ss3%R_geo,iout,ierr)
        CALL ps_conformal("R_geo",ss1%R_geo,ss3%R_geo,iout,ierr)
        if(ierr.eq.0) ss3%R_geo = w1*ss1%R_geo + (1.0_rspec-w1)*ss2%R_geo
     ELSE IF(ALLOCATED(ss1%R_geo)) THEN
        CALL ps_conformal("R_geo",ss1%R_geo,ss3%R_geo,iout,ierr)
        ss3%R_geo = ss1%R_geo
     ELSE IF(ALLOCATED(ss2%R_geo)) THEN
        CALL ps_conformal("R_geo",ss2%R_geo,ss3%R_geo,iout,ierr)
        ss3%R_geo = ss2%R_geo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%R_grid)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%R_grid).AND.ALLOCATED(ss2%R_grid)) THEN
        CALL ps_diff("R_grid",ss1%R_grid,ss2%R_grid,ps_error,iout,ierr)
        CALL ps_conformal("R_grid",ss1%R_grid,ss3%R_grid,iout,ierr)
        if(ierr.eq.0) ss3%R_grid = ss1%R_grid
     ELSE IF(ALLOCATED(ss1%R_grid)) THEN
        CALL ps_conformal("R_grid",ss1%R_grid,ss3%R_grid,iout,ierr)
        ss3%R_grid = ss1%R_grid
     ELSE IF(ALLOCATED(ss2%R_grid)) THEN
        CALL ps_conformal("R_grid",ss2%R_grid,ss3%R_grid,iout,ierr)
        ss3%R_grid = ss2%R_grid
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN

     CALL ps_diff("R_max_box",ss1%R_max_box,ss2%R_max_box,icheck,iout,ierr)
     ss3%R_max_box = ss1%R_max_box
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     ss3%R_max_lcfs = w1*ss1%R_max_lcfs + (1.0_rspec-w1)*ss2%R_max_lcfs
  ENDIF

  IF((ALLOCATED(ss3%R_midp_in)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%R_midp_in).AND.ALLOCATED(ss2%R_midp_in)) THEN
        CALL ps_conformal("R_midp_in",ss2%R_midp_in,ss3%R_midp_in,iout,ierr)
        CALL ps_conformal("R_midp_in",ss1%R_midp_in,ss3%R_midp_in,iout,ierr)
        if(ierr.eq.0) ss3%R_midp_in = w1*ss1%R_midp_in + (1.0_rspec-w1)*ss2%R_midp_in
     ELSE IF(ALLOCATED(ss1%R_midp_in)) THEN
        CALL ps_conformal("R_midp_in",ss1%R_midp_in,ss3%R_midp_in,iout,ierr)
        ss3%R_midp_in = ss1%R_midp_in
     ELSE IF(ALLOCATED(ss2%R_midp_in)) THEN
        CALL ps_conformal("R_midp_in",ss2%R_midp_in,ss3%R_midp_in,iout,ierr)
        ss3%R_midp_in = ss2%R_midp_in
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%R_midp_out)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%R_midp_out).AND.ALLOCATED(ss2%R_midp_out)) THEN
        CALL ps_conformal("R_midp_out",ss2%R_midp_out,ss3%R_midp_out,iout,ierr)
        CALL ps_conformal("R_midp_out",ss1%R_midp_out,ss3%R_midp_out,iout,ierr)
        if(ierr.eq.0) ss3%R_midp_out = w1*ss1%R_midp_out + (1.0_rspec-w1)*ss2%R_midp_out
     ELSE IF(ALLOCATED(ss1%R_midp_out)) THEN
        CALL ps_conformal("R_midp_out",ss1%R_midp_out,ss3%R_midp_out,iout,ierr)
        ss3%R_midp_out = ss1%R_midp_out
     ELSE IF(ALLOCATED(ss2%R_midp_out)) THEN
        CALL ps_conformal("R_midp_out",ss2%R_midp_out,ss3%R_midp_out,iout,ierr)
        ss3%R_midp_out = ss2%R_midp_out
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN

     CALL ps_diff("R_min_box",ss1%R_min_box,ss2%R_min_box,icheck,iout,ierr)
     ss3%R_min_box = ss1%R_min_box
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN
     ss3%R_min_lcfs = w1*ss1%R_min_lcfs + (1.0_rspec-w1)*ss2%R_min_lcfs
  ENDIF

  IF((ALLOCATED(ss3%R_ripple)).AND.(cclist(ps_cnum_RIPPLE).EQ.1)) THEN
     IF(ALLOCATED(ss1%R_ripple).AND.ALLOCATED(ss2%R_ripple)) THEN
        CALL ps_diff("R_ripple",ss1%R_ripple,ss2%R_ripple,ps_error,iout,ierr)
        CALL ps_conformal("R_ripple",ss1%R_ripple,ss3%R_ripple,iout,ierr)
        if(ierr.eq.0) ss3%R_ripple = ss1%R_ripple
     ELSE IF(ALLOCATED(ss1%R_ripple)) THEN
        CALL ps_conformal("R_ripple",ss1%R_ripple,ss3%R_ripple,iout,ierr)
        ss3%R_ripple = ss1%R_ripple
     ELSE IF(ALLOCATED(ss2%R_ripple)) THEN
        CALL ps_conformal("R_ripple",ss2%R_ripple,ss3%R_ripple,iout,ierr)
        ss3%R_ripple = ss2%R_ripple
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%R_surfMax)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%R_surfMax).AND.ALLOCATED(ss2%R_surfMax)) THEN
        CALL ps_conformal("R_surfMax",ss2%R_surfMax,ss3%R_surfMax,iout,ierr)
        CALL ps_conformal("R_surfMax",ss1%R_surfMax,ss3%R_surfMax,iout,ierr)
        if(ierr.eq.0) ss3%R_surfMax = w1*ss1%R_surfMax + (1.0_rspec-w1)*ss2%R_surfMax
     ELSE IF(ALLOCATED(ss1%R_surfMax)) THEN
        CALL ps_conformal("R_surfMax",ss1%R_surfMax,ss3%R_surfMax,iout,ierr)
        ss3%R_surfMax = ss1%R_surfMax
     ELSE IF(ALLOCATED(ss2%R_surfMax)) THEN
        CALL ps_conformal("R_surfMax",ss2%R_surfMax,ss3%R_surfMax,iout,ierr)
        ss3%R_surfMax = ss2%R_surfMax
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%R_surfMin)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%R_surfMin).AND.ALLOCATED(ss2%R_surfMin)) THEN
        CALL ps_conformal("R_surfMin",ss2%R_surfMin,ss3%R_surfMin,iout,ierr)
        CALL ps_conformal("R_surfMin",ss1%R_surfMin,ss3%R_surfMin,iout,ierr)
        if(ierr.eq.0) ss3%R_surfMin = w1*ss1%R_surfMin + (1.0_rspec-w1)*ss2%R_surfMin
     ELSE IF(ALLOCATED(ss1%R_surfMin)) THEN
        CALL ps_conformal("R_surfMin",ss1%R_surfMin,ss3%R_surfMin,iout,ierr)
        ss3%R_surfMin = ss1%R_surfMin
     ELSE IF(ALLOCATED(ss2%R_surfMin)) THEN
        CALL ps_conformal("R_surfMin",ss2%R_surfMin,ss3%R_surfMin,iout,ierr)
        ss3%R_surfMin = ss2%R_surfMin
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%s0reco)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%s0reco).AND.ALLOCATED(ss2%s0reco)) THEN
        CALL ps_conformal("s0reco",ss2%s0reco,ss3%s0reco,iout,ierr)
        CALL ps_conformal("s0reco",ss1%s0reco,ss3%s0reco,iout,ierr)
        if(ierr.eq.0) ss3%s0reco = w1*ss1%s0reco + (1.0_rspec-w1)*ss2%s0reco
     ELSE IF(ALLOCATED(ss1%s0reco)) THEN
        CALL ps_conformal("s0reco",ss1%s0reco,ss3%s0reco,iout,ierr)
        ss3%s0reco = ss1%s0reco
     ELSE IF(ALLOCATED(ss2%s0reco)) THEN
        CALL ps_conformal("s0reco",ss2%s0reco,ss3%s0reco,iout,ierr)
        ss3%s0reco = ss2%s0reco
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%s0reco_e)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%s0reco_e).AND.ALLOCATED(ss2%s0reco_e)) THEN
        CALL ps_conformal("s0reco_e",ss2%s0reco_e,ss3%s0reco_e,iout,ierr)
        CALL ps_conformal("s0reco_e",ss1%s0reco_e,ss3%s0reco_e,iout,ierr)
        if(ierr.eq.0) ss3%s0reco_e = w1*ss1%s0reco_e + (1.0_rspec-w1)*ss2%s0reco_e
     ELSE IF(ALLOCATED(ss1%s0reco_e)) THEN
        CALL ps_conformal("s0reco_e",ss1%s0reco_e,ss3%s0reco_e,iout,ierr)
        ss3%s0reco_e = ss1%s0reco_e
     ELSE IF(ALLOCATED(ss2%s0reco_e)) THEN
        CALL ps_conformal("s0reco_e",ss2%s0reco_e,ss3%s0reco_e,iout,ierr)
        ss3%s0reco_e = ss2%s0reco_e
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%s0reco_recap)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%s0reco_recap).AND.ALLOCATED(ss2%s0reco_recap)) THEN
        CALL ps_conformal("s0reco_recap",ss2%s0reco_recap,ss3%s0reco_recap,iout,ierr)
        CALL ps_conformal("s0reco_recap",ss1%s0reco_recap,ss3%s0reco_recap,iout,ierr)
        if(ierr.eq.0) ss3%s0reco_recap = w1*ss1%s0reco_recap + (1.0_rspec-w1)*ss2%s0reco_recap
     ELSE IF(ALLOCATED(ss1%s0reco_recap)) THEN
        CALL ps_conformal("s0reco_recap",ss1%s0reco_recap,ss3%s0reco_recap,iout,ierr)
        ss3%s0reco_recap = ss1%s0reco_recap
     ELSE IF(ALLOCATED(ss2%s0reco_recap)) THEN
        CALL ps_conformal("s0reco_recap",ss2%s0reco_recap,ss3%s0reco_recap,iout,ierr)
        ss3%s0reco_recap = ss2%s0reco_recap
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%sa_index)) THEN
     IF(ALLOCATED(ss1%sa_index).AND.ALLOCATED(ss2%sa_index)) THEN
        CALL ps_diff("sa_index",ss1%sa_index,ss2%sa_index,ps_error,iout,ierr)
        CALL ps_conformal("sa_index",ss1%sa_index,ss3%sa_index,iout,ierr)
        if(ierr.eq.0) ss3%sa_index = ss1%sa_index
     ELSE IF(ALLOCATED(ss1%sa_index)) THEN
        CALL ps_conformal("sa_index",ss1%sa_index,ss3%sa_index,iout,ierr)
        ss3%sa_index = ss1%sa_index
     ELSE IF(ALLOCATED(ss2%sa_index)) THEN
        CALL ps_conformal("sa_index",ss2%sa_index,ss3%sa_index,iout,ierr)
        ss3%sa_index = ss2%sa_index
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%SA_name)) THEN
     IF(ALLOCATED(ss1%SA_name).AND.ALLOCATED(ss2%SA_name)) THEN
        CALL ps_diff("SA_name",ss1%SA_name,ss2%SA_name,ps_error,iout,ierr)
        CALL ps_conformal("SA_name",ss1%SA_name,ss3%SA_name,iout,ierr)
        if(ierr.eq.0) ss3%SA_name = ss1%SA_name
     ELSE IF(ALLOCATED(ss1%SA_name)) THEN
        CALL ps_conformal("SA_name",ss1%SA_name,ss3%SA_name,iout,ierr)
        ss3%SA_name = ss1%SA_name
     ELSE IF(ALLOCATED(ss2%SA_name)) THEN
        CALL ps_conformal("SA_name",ss2%SA_name,ss3%SA_name,iout,ierr)
        ss3%SA_name = ss2%SA_name
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%SA_type)) THEN
     IF(ALLOCATED(ss1%SA_type).AND.ALLOCATED(ss2%SA_type)) THEN
        CALL ps_diff("SA_type",ss1%SA_type,ss2%SA_type,ps_error,iout,ierr)
        CALL ps_conformal("SA_type",ss1%SA_type,ss3%SA_type,iout,ierr)
        if(ierr.eq.0) ss3%SA_type = ss1%SA_type
     ELSE IF(ALLOCATED(ss1%SA_type)) THEN
        CALL ps_conformal("SA_type",ss1%SA_type,ss3%SA_type,iout,ierr)
        ss3%SA_type = ss1%SA_type
     ELSE IF(ALLOCATED(ss2%SA_type)) THEN
        CALL ps_conformal("SA_type",ss2%SA_type,ss3%SA_type,iout,ierr)
        ss3%SA_type = ss2%SA_type
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sb0halo)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%sb0halo).AND.ALLOCATED(ss2%sb0halo)) THEN
        CALL ps_conformal("sb0halo",ss2%sb0halo,ss3%sb0halo,iout,ierr)
        CALL ps_conformal("sb0halo",ss1%sb0halo,ss3%sb0halo,iout,ierr)
        if(ierr.eq.0) ss3%sb0halo = w1*ss1%sb0halo + (1.0_rspec-w1)*ss2%sb0halo
     ELSE IF(ALLOCATED(ss1%sb0halo)) THEN
        CALL ps_conformal("sb0halo",ss1%sb0halo,ss3%sb0halo,iout,ierr)
        ss3%sb0halo = ss1%sb0halo
     ELSE IF(ALLOCATED(ss2%sb0halo)) THEN
        CALL ps_conformal("sb0halo",ss2%sb0halo,ss3%sb0halo,iout,ierr)
        ss3%sb0halo = ss2%sb0halo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sb0halo_recap)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%sb0halo_recap).AND.ALLOCATED(ss2%sb0halo_recap)) THEN
        CALL ps_conformal("sb0halo_recap",ss2%sb0halo_recap,ss3%sb0halo_recap,iout,ierr)
        CALL ps_conformal("sb0halo_recap",ss1%sb0halo_recap,ss3%sb0halo_recap,iout,ierr)
        if(ierr.eq.0) ss3%sb0halo_recap = w1*ss1%sb0halo_recap + (1.0_rspec-w1)*ss2%sb0halo_recap
     ELSE IF(ALLOCATED(ss1%sb0halo_recap)) THEN
        CALL ps_conformal("sb0halo_recap",ss1%sb0halo_recap,ss3%sb0halo_recap,iout,ierr)
        ss3%sb0halo_recap = ss1%sb0halo_recap
     ELSE IF(ALLOCATED(ss2%sb0halo_recap)) THEN
        CALL ps_conformal("sb0halo_recap",ss2%sb0halo_recap,ss3%sb0halo_recap,iout,ierr)
        ss3%sb0halo_recap = ss2%sb0halo_recap
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sbedep)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%sbedep).AND.ALLOCATED(ss2%sbedep)) THEN
        CALL ps_conformal("sbedep",ss2%sbedep,ss3%sbedep,iout,ierr)
        CALL ps_conformal("sbedep",ss1%sbedep,ss3%sbedep,iout,ierr)
        if(ierr.eq.0) ss3%sbedep = w1*ss1%sbedep + (1.0_rspec-w1)*ss2%sbedep
     ELSE IF(ALLOCATED(ss1%sbedep)) THEN
        CALL ps_conformal("sbedep",ss1%sbedep,ss3%sbedep,iout,ierr)
        ss3%sbedep = ss1%sbedep
     ELSE IF(ALLOCATED(ss2%sbedep)) THEN
        CALL ps_conformal("sbedep",ss2%sbedep,ss3%sbedep,iout,ierr)
        ss3%sbedep = ss2%sbedep
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sbehalo)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%sbehalo).AND.ALLOCATED(ss2%sbehalo)) THEN
        CALL ps_conformal("sbehalo",ss2%sbehalo,ss3%sbehalo,iout,ierr)
        CALL ps_conformal("sbehalo",ss1%sbehalo,ss3%sbehalo,iout,ierr)
        if(ierr.eq.0) ss3%sbehalo = w1*ss1%sbehalo + (1.0_rspec-w1)*ss2%sbehalo
     ELSE IF(ALLOCATED(ss1%sbehalo)) THEN
        CALL ps_conformal("sbehalo",ss1%sbehalo,ss3%sbehalo,iout,ierr)
        ss3%sbehalo = ss1%sbehalo
     ELSE IF(ALLOCATED(ss2%sbehalo)) THEN
        CALL ps_conformal("sbehalo",ss2%sbehalo,ss3%sbehalo,iout,ierr)
        ss3%sbehalo = ss2%sbehalo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sbsce)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%sbsce).AND.ALLOCATED(ss2%sbsce)) THEN
        CALL ps_conformal("sbsce",ss2%sbsce,ss3%sbsce,iout,ierr)
        CALL ps_conformal("sbsce",ss1%sbsce,ss3%sbsce,iout,ierr)
        if(ierr.eq.0) ss3%sbsce = w1*ss1%sbsce + (1.0_rspec-w1)*ss2%sbsce
     ELSE IF(ALLOCATED(ss1%sbsce)) THEN
        CALL ps_conformal("sbsce",ss1%sbsce,ss3%sbsce,iout,ierr)
        ss3%sbsce = ss1%sbsce
     ELSE IF(ALLOCATED(ss2%sbsce)) THEN
        CALL ps_conformal("sbsce",ss2%sbsce,ss3%sbsce,iout,ierr)
        ss3%sbsce = ss2%sbsce
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sbtherm)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%sbtherm).AND.ALLOCATED(ss2%sbtherm)) THEN
        CALL ps_conformal("sbtherm",ss2%sbtherm,ss3%sbtherm,iout,ierr)
        CALL ps_conformal("sbtherm",ss1%sbtherm,ss3%sbtherm,iout,ierr)
        if(ierr.eq.0) ss3%sbtherm = w1*ss1%sbtherm + (1.0_rspec-w1)*ss2%sbtherm
     ELSE IF(ALLOCATED(ss1%sbtherm)) THEN
        CALL ps_conformal("sbtherm",ss1%sbtherm,ss3%sbtherm,iout,ierr)
        ss3%sbtherm = ss1%sbtherm
     ELSE IF(ALLOCATED(ss2%sbtherm)) THEN
        CALL ps_conformal("sbtherm",ss2%sbtherm,ss3%sbtherm,iout,ierr)
        ss3%sbtherm = ss2%sbtherm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sc0)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%sc0).AND.ALLOCATED(ss2%sc0)) THEN
        CALL ps_conformal("sc0",ss2%sc0,ss3%sc0,iout,ierr)
        CALL ps_conformal("sc0",ss1%sc0,ss3%sc0,iout,ierr)
        if(ierr.eq.0) ss3%sc0 = w1*ss1%sc0 + (1.0_rspec-w1)*ss2%sc0
     ELSE IF(ALLOCATED(ss1%sc0)) THEN
        CALL ps_conformal("sc0",ss1%sc0,ss3%sc0,iout,ierr)
        ss3%sc0 = ss1%sc0
     ELSE IF(ALLOCATED(ss2%sc0)) THEN
        CALL ps_conformal("sc0",ss2%sc0,ss3%sc0,iout,ierr)
        ss3%sc0 = ss2%sc0
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sc0_to_sgas)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%sc0_to_sgas).AND.ALLOCATED(ss2%sc0_to_sgas)) THEN
        CALL ps_diff("sc0_to_sgas",ss1%sc0_to_sgas,ss2%sc0_to_sgas,icheck,iout,ierr)
        CALL ps_conformal("sc0_to_sgas",ss1%sc0_to_sgas,ss3%sc0_to_sgas,iout,ierr)
        if(ierr.eq.0) ss3%sc0_to_sgas = ss1%sc0_to_sgas
     ELSE IF(ALLOCATED(ss1%sc0_to_sgas)) THEN
        CALL ps_conformal("sc0_to_sgas",ss1%sc0_to_sgas,ss3%sc0_to_sgas,iout,ierr)
        ss3%sc0_to_sgas = ss1%sc0_to_sgas
     ELSE IF(ALLOCATED(ss2%sc0_to_sgas)) THEN
        CALL ps_conformal("sc0_to_sgas",ss2%sc0_to_sgas,ss3%sc0_to_sgas,iout,ierr)
        ss3%sc0_to_sgas = ss2%sc0_to_sgas
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sc_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%sc_anom).AND.ALLOCATED(ss2%sc_anom)) THEN
        CALL ps_conformal("sc_anom",ss2%sc_anom,ss3%sc_anom,iout,ierr)
        CALL ps_conformal("sc_anom",ss1%sc_anom,ss3%sc_anom,iout,ierr)
        if(ierr.eq.0) ss3%sc_anom = w1*ss1%sc_anom + (1.0_rspec-w1)*ss2%sc_anom
     ELSE IF(ALLOCATED(ss1%sc_anom)) THEN
        CALL ps_conformal("sc_anom",ss1%sc_anom,ss3%sc_anom,iout,ierr)
        ss3%sc_anom = ss1%sc_anom
     ELSE IF(ALLOCATED(ss2%sc_anom)) THEN
        CALL ps_conformal("sc_anom",ss2%sc_anom,ss3%sc_anom,iout,ierr)
        ss3%sc_anom = ss2%sc_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sfsce)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%sfsce).AND.ALLOCATED(ss2%sfsce)) THEN
        CALL ps_conformal("sfsce",ss2%sfsce,ss3%sfsce,iout,ierr)
        CALL ps_conformal("sfsce",ss1%sfsce,ss3%sfsce,iout,ierr)
        if(ierr.eq.0) ss3%sfsce = w1*ss1%sfsce + (1.0_rspec-w1)*ss2%sfsce
     ELSE IF(ALLOCATED(ss1%sfsce)) THEN
        CALL ps_conformal("sfsce",ss1%sfsce,ss3%sfsce,iout,ierr)
        ss3%sfsce = ss1%sfsce
     ELSE IF(ALLOCATED(ss2%sfsce)) THEN
        CALL ps_conformal("sfsce",ss2%sfsce,ss3%sfsce,iout,ierr)
        ss3%sfsce = ss2%sfsce
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sftherm)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%sftherm).AND.ALLOCATED(ss2%sftherm)) THEN
        CALL ps_conformal("sftherm",ss2%sftherm,ss3%sftherm,iout,ierr)
        CALL ps_conformal("sftherm",ss1%sftherm,ss3%sftherm,iout,ierr)
        if(ierr.eq.0) ss3%sftherm = w1*ss1%sftherm + (1.0_rspec-w1)*ss2%sftherm
     ELSE IF(ALLOCATED(ss1%sftherm)) THEN
        CALL ps_conformal("sftherm",ss1%sftherm,ss3%sftherm,iout,ierr)
        ss3%sftherm = ss1%sftherm
     ELSE IF(ALLOCATED(ss2%sftherm)) THEN
        CALL ps_conformal("sftherm",ss2%sftherm,ss3%sftherm,iout,ierr)
        ss3%sftherm = ss2%sftherm
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%SFUS_name)) THEN
     IF(ALLOCATED(ss1%SFUS_name).AND.ALLOCATED(ss2%SFUS_name)) THEN
        CALL ps_diff("SFUS_name",ss1%SFUS_name,ss2%SFUS_name,ps_error,iout,ierr)
        CALL ps_conformal("SFUS_name",ss1%SFUS_name,ss3%SFUS_name,iout,ierr)
        if(ierr.eq.0) ss3%SFUS_name = ss1%SFUS_name
     ELSE IF(ALLOCATED(ss1%SFUS_name)) THEN
        CALL ps_conformal("SFUS_name",ss1%SFUS_name,ss3%SFUS_name,iout,ierr)
        ss3%SFUS_name = ss1%SFUS_name
     ELSE IF(ALLOCATED(ss2%SFUS_name)) THEN
        CALL ps_conformal("SFUS_name",ss2%SFUS_name,ss3%SFUS_name,iout,ierr)
        ss3%SFUS_name = ss2%SFUS_name
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%sfus_to_all)) THEN
     IF(ALLOCATED(ss1%sfus_to_all).AND.ALLOCATED(ss2%sfus_to_all)) THEN
        CALL ps_diff("sfus_to_all",ss1%sfus_to_all,ss2%sfus_to_all,ps_error,iout,ierr)
        CALL ps_conformal("sfus_to_all",ss1%sfus_to_all,ss3%sfus_to_all,iout,ierr)
        if(ierr.eq.0) ss3%sfus_to_all = ss1%sfus_to_all
     ELSE IF(ALLOCATED(ss1%sfus_to_all)) THEN
        CALL ps_conformal("sfus_to_all",ss1%sfus_to_all,ss3%sfus_to_all,iout,ierr)
        ss3%sfus_to_all = ss1%sfus_to_all
     ELSE IF(ALLOCATED(ss2%sfus_to_all)) THEN
        CALL ps_conformal("sfus_to_all",ss2%sfus_to_all,ss3%sfus_to_all,iout,ierr)
        ss3%sfus_to_all = ss2%sfus_to_all
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%sfus_to_alla)) THEN
     IF(ALLOCATED(ss1%sfus_to_alla).AND.ALLOCATED(ss2%sfus_to_alla)) THEN
        CALL ps_diff("sfus_to_alla",ss1%sfus_to_alla,ss2%sfus_to_alla,ps_error,iout,ierr)
        CALL ps_conformal("sfus_to_alla",ss1%sfus_to_alla,ss3%sfus_to_alla,iout,ierr)
        if(ierr.eq.0) ss3%sfus_to_alla = ss1%sfus_to_alla
     ELSE IF(ALLOCATED(ss1%sfus_to_alla)) THEN
        CALL ps_conformal("sfus_to_alla",ss1%sfus_to_alla,ss3%sfus_to_alla,iout,ierr)
        ss3%sfus_to_alla = ss1%sfus_to_alla
     ELSE IF(ALLOCATED(ss2%sfus_to_alla)) THEN
        CALL ps_conformal("sfus_to_alla",ss2%sfus_to_alla,ss3%sfus_to_alla,iout,ierr)
        ss3%sfus_to_alla = ss2%sfus_to_alla
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%SFUS_type)) THEN
     IF(ALLOCATED(ss1%SFUS_type).AND.ALLOCATED(ss2%SFUS_type)) THEN
        CALL ps_diff("SFUS_type",ss1%SFUS_type,ss2%SFUS_type,ps_error,iout,ierr)
        CALL ps_conformal("SFUS_type",ss1%SFUS_type,ss3%SFUS_type,iout,ierr)
        if(ierr.eq.0) ss3%SFUS_type = ss1%SFUS_type
     ELSE IF(ALLOCATED(ss1%SFUS_type)) THEN
        CALL ps_conformal("SFUS_type",ss1%SFUS_type,ss3%SFUS_type,iout,ierr)
        ss3%SFUS_type = ss1%SFUS_type
     ELSE IF(ALLOCATED(ss2%SFUS_type)) THEN
        CALL ps_conformal("SFUS_type",ss2%SFUS_type,ss3%SFUS_type,iout,ierr)
        ss3%SFUS_type = ss2%SFUS_type
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%SGAS_name)) THEN
     IF(ALLOCATED(ss1%SGAS_name).AND.ALLOCATED(ss2%SGAS_name)) THEN
        CALL ps_diff("SGAS_name",ss1%SGAS_name,ss2%SGAS_name,ps_error,iout,ierr)
        CALL ps_conformal("SGAS_name",ss1%SGAS_name,ss3%SGAS_name,iout,ierr)
        if(ierr.eq.0) ss3%SGAS_name = ss1%SGAS_name
     ELSE IF(ALLOCATED(ss1%SGAS_name)) THEN
        CALL ps_conformal("SGAS_name",ss1%SGAS_name,ss3%SGAS_name,iout,ierr)
        ss3%SGAS_name = ss1%SGAS_name
     ELSE IF(ALLOCATED(ss2%SGAS_name)) THEN
        CALL ps_conformal("SGAS_name",ss2%SGAS_name,ss3%SGAS_name,iout,ierr)
        ss3%SGAS_name = ss2%SGAS_name
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%sgas_to_s)) THEN
     IF(ALLOCATED(ss1%sgas_to_s).AND.ALLOCATED(ss2%sgas_to_s)) THEN
        CALL ps_diff("sgas_to_s",ss1%sgas_to_s,ss2%sgas_to_s,ps_error,iout,ierr)
        CALL ps_conformal("sgas_to_s",ss1%sgas_to_s,ss3%sgas_to_s,iout,ierr)
        if(ierr.eq.0) ss3%sgas_to_s = ss1%sgas_to_s
     ELSE IF(ALLOCATED(ss1%sgas_to_s)) THEN
        CALL ps_conformal("sgas_to_s",ss1%sgas_to_s,ss3%sgas_to_s,iout,ierr)
        ss3%sgas_to_s = ss1%sgas_to_s
     ELSE IF(ALLOCATED(ss2%sgas_to_s)) THEN
        CALL ps_conformal("sgas_to_s",ss2%sgas_to_s,ss3%sgas_to_s,iout,ierr)
        ss3%sgas_to_s = ss2%sgas_to_s
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%SGAS_type)) THEN
     IF(ALLOCATED(ss1%SGAS_type).AND.ALLOCATED(ss2%SGAS_type)) THEN
        CALL ps_diff("SGAS_type",ss1%SGAS_type,ss2%SGAS_type,ps_error,iout,ierr)
        CALL ps_conformal("SGAS_type",ss1%SGAS_type,ss3%SGAS_type,iout,ierr)
        if(ierr.eq.0) ss3%SGAS_type = ss1%SGAS_type
     ELSE IF(ALLOCATED(ss1%SGAS_type)) THEN
        CALL ps_conformal("SGAS_type",ss1%SGAS_type,ss3%SGAS_type,iout,ierr)
        ss3%SGAS_type = ss1%SGAS_type
     ELSE IF(ALLOCATED(ss2%SGAS_type)) THEN
        CALL ps_conformal("SGAS_type",ss2%SGAS_type,ss3%SGAS_type,iout,ierr)
        ss3%SGAS_type = ss2%SGAS_type
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("shot_number",ss1%shot_number,ss2%shot_number,icheck,iout,ierr)
     ss3%shot_number = ss1%shot_number
  ENDIF

  IF((ALLOCATED(ss3%sii_xs)).AND.(cclist(ps_cnum_RAD).EQ.1)) THEN
     IF(ALLOCATED(ss1%sii_xs).AND.ALLOCATED(ss2%sii_xs)) THEN
        CALL ps_conformal("sii_xs",ss2%sii_xs,ss3%sii_xs,iout,ierr)
        CALL ps_conformal("sii_xs",ss1%sii_xs,ss3%sii_xs,iout,ierr)
        if(ierr.eq.0) ss3%sii_xs = w1*ss1%sii_xs + (1.0_rspec-w1)*ss2%sii_xs
     ELSE IF(ALLOCATED(ss1%sii_xs)) THEN
        CALL ps_conformal("sii_xs",ss1%sii_xs,ss3%sii_xs,iout,ierr)
        ss3%sii_xs = ss1%sii_xs
     ELSE IF(ALLOCATED(ss2%sii_xs)) THEN
        CALL ps_conformal("sii_xs",ss2%sii_xs,ss3%sii_xs,iout,ierr)
        ss3%sii_xs = ss2%sii_xs
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%SIMP0_name)) THEN
     IF(ALLOCATED(ss1%SIMP0_name).AND.ALLOCATED(ss2%SIMP0_name)) THEN
        CALL ps_diff("SIMP0_name",ss1%SIMP0_name,ss2%SIMP0_name,ps_error,iout,ierr)
        CALL ps_conformal("SIMP0_name",ss1%SIMP0_name,ss3%SIMP0_name,iout,ierr)
        if(ierr.eq.0) ss3%SIMP0_name = ss1%SIMP0_name
     ELSE IF(ALLOCATED(ss1%SIMP0_name)) THEN
        CALL ps_conformal("SIMP0_name",ss1%SIMP0_name,ss3%SIMP0_name,iout,ierr)
        ss3%SIMP0_name = ss1%SIMP0_name
     ELSE IF(ALLOCATED(ss2%SIMP0_name)) THEN
        CALL ps_conformal("SIMP0_name",ss2%SIMP0_name,ss3%SIMP0_name,iout,ierr)
        ss3%SIMP0_name = ss2%SIMP0_name
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%simp0_to_s)) THEN
     IF(ALLOCATED(ss1%simp0_to_s).AND.ALLOCATED(ss2%simp0_to_s)) THEN
        CALL ps_diff("simp0_to_s",ss1%simp0_to_s,ss2%simp0_to_s,ps_error,iout,ierr)
        CALL ps_conformal("simp0_to_s",ss1%simp0_to_s,ss3%simp0_to_s,iout,ierr)
        if(ierr.eq.0) ss3%simp0_to_s = ss1%simp0_to_s
     ELSE IF(ALLOCATED(ss1%simp0_to_s)) THEN
        CALL ps_conformal("simp0_to_s",ss1%simp0_to_s,ss3%simp0_to_s,iout,ierr)
        ss3%simp0_to_s = ss1%simp0_to_s
     ELSE IF(ALLOCATED(ss2%simp0_to_s)) THEN
        CALL ps_conformal("simp0_to_s",ss2%simp0_to_s,ss3%simp0_to_s,iout,ierr)
        ss3%simp0_to_s = ss2%simp0_to_s
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%SIMP0_type)) THEN
     IF(ALLOCATED(ss1%SIMP0_type).AND.ALLOCATED(ss2%SIMP0_type)) THEN
        CALL ps_diff("SIMP0_type",ss1%SIMP0_type,ss2%SIMP0_type,ps_error,iout,ierr)
        CALL ps_conformal("SIMP0_type",ss1%SIMP0_type,ss3%SIMP0_type,iout,ierr)
        if(ierr.eq.0) ss3%SIMP0_type = ss1%SIMP0_type
     ELSE IF(ALLOCATED(ss1%SIMP0_type)) THEN
        CALL ps_conformal("SIMP0_type",ss1%SIMP0_type,ss3%SIMP0_type,iout,ierr)
        ss3%SIMP0_type = ss1%SIMP0_type
     ELSE IF(ALLOCATED(ss2%SIMP0_type)) THEN
        CALL ps_conformal("SIMP0_type",ss2%SIMP0_type,ss3%SIMP0_type,iout,ierr)
        ss3%SIMP0_type = ss2%SIMP0_type
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%SIMPI_name)) THEN
     IF(ALLOCATED(ss1%SIMPI_name).AND.ALLOCATED(ss2%SIMPI_name)) THEN
        CALL ps_diff("SIMPI_name",ss1%SIMPI_name,ss2%SIMPI_name,ps_error,iout,ierr)
        CALL ps_conformal("SIMPI_name",ss1%SIMPI_name,ss3%SIMPI_name,iout,ierr)
        if(ierr.eq.0) ss3%SIMPI_name = ss1%SIMPI_name
     ELSE IF(ALLOCATED(ss1%SIMPI_name)) THEN
        CALL ps_conformal("SIMPI_name",ss1%SIMPI_name,ss3%SIMPI_name,iout,ierr)
        ss3%SIMPI_name = ss1%SIMPI_name
     ELSE IF(ALLOCATED(ss2%SIMPI_name)) THEN
        CALL ps_conformal("SIMPI_name",ss2%SIMPI_name,ss3%SIMPI_name,iout,ierr)
        ss3%SIMPI_name = ss2%SIMPI_name
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%simpi_to_s)) THEN
     IF(ALLOCATED(ss1%simpi_to_s).AND.ALLOCATED(ss2%simpi_to_s)) THEN
        CALL ps_diff("simpi_to_s",ss1%simpi_to_s,ss2%simpi_to_s,ps_error,iout,ierr)
        CALL ps_conformal("simpi_to_s",ss1%simpi_to_s,ss3%simpi_to_s,iout,ierr)
        if(ierr.eq.0) ss3%simpi_to_s = ss1%simpi_to_s
     ELSE IF(ALLOCATED(ss1%simpi_to_s)) THEN
        CALL ps_conformal("simpi_to_s",ss1%simpi_to_s,ss3%simpi_to_s,iout,ierr)
        ss3%simpi_to_s = ss1%simpi_to_s
     ELSE IF(ALLOCATED(ss2%simpi_to_s)) THEN
        CALL ps_conformal("simpi_to_s",ss2%simpi_to_s,ss3%simpi_to_s,iout,ierr)
        ss3%simpi_to_s = ss2%simpi_to_s
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%SIMPI_type)) THEN
     IF(ALLOCATED(ss1%SIMPI_type).AND.ALLOCATED(ss2%SIMPI_type)) THEN
        CALL ps_diff("SIMPI_type",ss1%SIMPI_type,ss2%SIMPI_type,ps_error,iout,ierr)
        CALL ps_conformal("SIMPI_type",ss1%SIMPI_type,ss3%SIMPI_type,iout,ierr)
        if(ierr.eq.0) ss3%SIMPI_type = ss1%SIMPI_type
     ELSE IF(ALLOCATED(ss1%SIMPI_type)) THEN
        CALL ps_conformal("SIMPI_type",ss1%SIMPI_type,ss3%SIMPI_type,iout,ierr)
        ss3%SIMPI_type = ss1%SIMPI_type
     ELSE IF(ALLOCATED(ss2%SIMPI_type)) THEN
        CALL ps_conformal("SIMPI_type",ss2%SIMPI_type,ss3%SIMPI_type,iout,ierr)
        ss3%SIMPI_type = ss2%SIMPI_type
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%SNBI_name)) THEN
     IF(ALLOCATED(ss1%SNBI_name).AND.ALLOCATED(ss2%SNBI_name)) THEN
        CALL ps_diff("SNBI_name",ss1%SNBI_name,ss2%SNBI_name,ps_error,iout,ierr)
        CALL ps_conformal("SNBI_name",ss1%SNBI_name,ss3%SNBI_name,iout,ierr)
        if(ierr.eq.0) ss3%SNBI_name = ss1%SNBI_name
     ELSE IF(ALLOCATED(ss1%SNBI_name)) THEN
        CALL ps_conformal("SNBI_name",ss1%SNBI_name,ss3%SNBI_name,iout,ierr)
        ss3%SNBI_name = ss1%SNBI_name
     ELSE IF(ALLOCATED(ss2%SNBI_name)) THEN
        CALL ps_conformal("SNBI_name",ss2%SNBI_name,ss3%SNBI_name,iout,ierr)
        ss3%SNBI_name = ss2%SNBI_name
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%snbi_to_all)) THEN
     IF(ALLOCATED(ss1%snbi_to_all).AND.ALLOCATED(ss2%snbi_to_all)) THEN
        CALL ps_diff("snbi_to_all",ss1%snbi_to_all,ss2%snbi_to_all,ps_error,iout,ierr)
        CALL ps_conformal("snbi_to_all",ss1%snbi_to_all,ss3%snbi_to_all,iout,ierr)
        if(ierr.eq.0) ss3%snbi_to_all = ss1%snbi_to_all
     ELSE IF(ALLOCATED(ss1%snbi_to_all)) THEN
        CALL ps_conformal("snbi_to_all",ss1%snbi_to_all,ss3%snbi_to_all,iout,ierr)
        ss3%snbi_to_all = ss1%snbi_to_all
     ELSE IF(ALLOCATED(ss2%snbi_to_all)) THEN
        CALL ps_conformal("snbi_to_all",ss2%snbi_to_all,ss3%snbi_to_all,iout,ierr)
        ss3%snbi_to_all = ss2%snbi_to_all
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%snbi_to_alla)) THEN
     IF(ALLOCATED(ss1%snbi_to_alla).AND.ALLOCATED(ss2%snbi_to_alla)) THEN
        CALL ps_diff("snbi_to_alla",ss1%snbi_to_alla,ss2%snbi_to_alla,ps_error,iout,ierr)
        CALL ps_conformal("snbi_to_alla",ss1%snbi_to_alla,ss3%snbi_to_alla,iout,ierr)
        if(ierr.eq.0) ss3%snbi_to_alla = ss1%snbi_to_alla
     ELSE IF(ALLOCATED(ss1%snbi_to_alla)) THEN
        CALL ps_conformal("snbi_to_alla",ss1%snbi_to_alla,ss3%snbi_to_alla,iout,ierr)
        ss3%snbi_to_alla = ss1%snbi_to_alla
     ELSE IF(ALLOCATED(ss2%snbi_to_alla)) THEN
        CALL ps_conformal("snbi_to_alla",ss2%snbi_to_alla,ss3%snbi_to_alla,iout,ierr)
        ss3%snbi_to_alla = ss2%snbi_to_alla
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%SNBI_type)) THEN
     IF(ALLOCATED(ss1%SNBI_type).AND.ALLOCATED(ss2%SNBI_type)) THEN
        CALL ps_diff("SNBI_type",ss1%SNBI_type,ss2%SNBI_type,ps_error,iout,ierr)
        CALL ps_conformal("SNBI_type",ss1%SNBI_type,ss3%SNBI_type,iout,ierr)
        if(ierr.eq.0) ss3%SNBI_type = ss1%SNBI_type
     ELSE IF(ALLOCATED(ss1%SNBI_type)) THEN
        CALL ps_conformal("SNBI_type",ss1%SNBI_type,ss3%SNBI_type,iout,ierr)
        ss3%SNBI_type = ss1%SNBI_type
     ELSE IF(ALLOCATED(ss2%SNBI_type)) THEN
        CALL ps_conformal("SNBI_type",ss2%SNBI_type,ss3%SNBI_type,iout,ierr)
        ss3%SNBI_type = ss2%SNBI_type
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sn_trans)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%sn_trans).AND.ALLOCATED(ss2%sn_trans)) THEN
        CALL ps_conformal("sn_trans",ss2%sn_trans,ss3%sn_trans,iout,ierr)
        CALL ps_conformal("sn_trans",ss1%sn_trans,ss3%sn_trans,iout,ierr)
        if(ierr.eq.0) ss3%sn_trans = w1*ss1%sn_trans + (1.0_rspec-w1)*ss2%sn_trans
     ELSE IF(ALLOCATED(ss1%sn_trans)) THEN
        CALL ps_conformal("sn_trans",ss1%sn_trans,ss3%sn_trans,iout,ierr)
        ss3%sn_trans = ss1%sn_trans
     ELSE IF(ALLOCATED(ss2%sn_trans)) THEN
        CALL ps_conformal("sn_trans",ss2%sn_trans,ss3%sn_trans,iout,ierr)
        ss3%sn_trans = ss2%sn_trans
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sn_transcat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%sn_transcat).AND.ALLOCATED(ss2%sn_transcat)) THEN
        CALL ps_conformal("sn_transcat",ss2%sn_transcat,ss3%sn_transcat,iout,ierr)
        CALL ps_conformal("sn_transcat",ss1%sn_transcat,ss3%sn_transcat,iout,ierr)
        if(ierr.eq.0) ss3%sn_transcat = w1*ss1%sn_transcat + (1.0_rspec-w1)*ss2%sn_transcat
     ELSE IF(ALLOCATED(ss1%sn_transcat)) THEN
        CALL ps_conformal("sn_transcat",ss1%sn_transcat,ss3%sn_transcat,iout,ierr)
        ss3%sn_transcat = ss1%sn_transcat
     ELSE IF(ALLOCATED(ss2%sn_transcat)) THEN
        CALL ps_conformal("sn_transcat",ss2%sn_transcat,ss3%sn_transcat,iout,ierr)
        ss3%sn_transcat = ss2%sn_transcat
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sprof0)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%sprof0).AND.ALLOCATED(ss2%sprof0)) THEN
        CALL ps_conformal("sprof0",ss2%sprof0,ss3%sprof0,iout,ierr)
        CALL ps_conformal("sprof0",ss1%sprof0,ss3%sprof0,iout,ierr)
        if(ierr.eq.0) ss3%sprof0 = w1*ss1%sprof0 + (1.0_rspec-w1)*ss2%sprof0
     ELSE IF(ALLOCATED(ss1%sprof0)) THEN
        CALL ps_conformal("sprof0",ss1%sprof0,ss3%sprof0,iout,ierr)
        ss3%sprof0 = ss1%sprof0
     ELSE IF(ALLOCATED(ss2%sprof0)) THEN
        CALL ps_conformal("sprof0",ss2%sprof0,ss3%sprof0,iout,ierr)
        ss3%sprof0 = ss2%sprof0
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sprof0e)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%sprof0e).AND.ALLOCATED(ss2%sprof0e)) THEN
        CALL ps_conformal("sprof0e",ss2%sprof0e,ss3%sprof0e,iout,ierr)
        CALL ps_conformal("sprof0e",ss1%sprof0e,ss3%sprof0e,iout,ierr)
        if(ierr.eq.0) ss3%sprof0e = w1*ss1%sprof0e + (1.0_rspec-w1)*ss2%sprof0e
     ELSE IF(ALLOCATED(ss1%sprof0e)) THEN
        CALL ps_conformal("sprof0e",ss1%sprof0e,ss3%sprof0e,iout,ierr)
        ss3%sprof0e = ss1%sprof0e
     ELSE IF(ALLOCATED(ss2%sprof0e)) THEN
        CALL ps_conformal("sprof0e",ss2%sprof0e,ss3%sprof0e,iout,ierr)
        ss3%sprof0e = ss2%sprof0e
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%spsce)).AND.(cclist(ps_cnum_PEL).EQ.1)) THEN
     IF(ALLOCATED(ss1%spsce).AND.ALLOCATED(ss2%spsce)) THEN
        CALL ps_conformal("spsce",ss2%spsce,ss3%spsce,iout,ierr)
        CALL ps_conformal("spsce",ss1%spsce,ss3%spsce,iout,ierr)
        if(ierr.eq.0) ss3%spsce = w1*ss1%spsce + (1.0_rspec-w1)*ss2%spsce
     ELSE IF(ALLOCATED(ss1%spsce)) THEN
        CALL ps_conformal("spsce",ss1%spsce,ss3%spsce,iout,ierr)
        ss3%spsce = ss1%spsce
     ELSE IF(ALLOCATED(ss2%spsce)) THEN
        CALL ps_conformal("spsce",ss2%spsce,ss3%spsce,iout,ierr)
        ss3%spsce = ss2%spsce
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%squareLO)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%squareLO).AND.ALLOCATED(ss2%squareLO)) THEN
        CALL ps_conformal("squareLO",ss2%squareLO,ss3%squareLO,iout,ierr)
        CALL ps_conformal("squareLO",ss1%squareLO,ss3%squareLO,iout,ierr)
        if(ierr.eq.0) ss3%squareLO = w1*ss1%squareLO + (1.0_rspec-w1)*ss2%squareLO
     ELSE IF(ALLOCATED(ss1%squareLO)) THEN
        CALL ps_conformal("squareLO",ss1%squareLO,ss3%squareLO,iout,ierr)
        ss3%squareLO = ss1%squareLO
     ELSE IF(ALLOCATED(ss2%squareLO)) THEN
        CALL ps_conformal("squareLO",ss2%squareLO,ss3%squareLO,iout,ierr)
        ss3%squareLO = ss2%squareLO
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%squareUO)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%squareUO).AND.ALLOCATED(ss2%squareUO)) THEN
        CALL ps_conformal("squareUO",ss2%squareUO,ss3%squareUO,iout,ierr)
        CALL ps_conformal("squareUO",ss1%squareUO,ss3%squareUO,iout,ierr)
        if(ierr.eq.0) ss3%squareUO = w1*ss1%squareUO + (1.0_rspec-w1)*ss2%squareUO
     ELSE IF(ALLOCATED(ss1%squareUO)) THEN
        CALL ps_conformal("squareUO",ss1%squareUO,ss3%squareUO,iout,ierr)
        ss3%squareUO = ss1%squareUO
     ELSE IF(ALLOCATED(ss2%squareUO)) THEN
        CALL ps_conformal("squareUO",ss2%squareUO,ss3%squareUO,iout,ierr)
        ss3%squareUO = ss2%squareUO
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%sRtcen)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%sRtcen).AND.ALLOCATED(ss2%sRtcen)) THEN
        CALL ps_diff("sRtcen",ss1%sRtcen,ss2%sRtcen,icheck,iout,ierr)
        CALL ps_conformal("sRtcen",ss1%sRtcen,ss3%sRtcen,iout,ierr)
        if(ierr.eq.0) ss3%sRtcen = ss1%sRtcen
     ELSE IF(ALLOCATED(ss1%sRtcen)) THEN
        CALL ps_conformal("sRtcen",ss1%sRtcen,ss3%sRtcen,iout,ierr)
        ss3%sRtcen = ss1%sRtcen
     ELSE IF(ALLOCATED(ss2%sRtcen)) THEN
        CALL ps_conformal("sRtcen",ss2%sRtcen,ss3%sRtcen,iout,ierr)
        ss3%sRtcen = ss2%sRtcen
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%surf)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%surf).AND.ALLOCATED(ss2%surf)) THEN
        CALL ps_conformal("surf",ss2%surf,ss3%surf,iout,ierr)
        CALL ps_conformal("surf",ss1%surf,ss3%surf,iout,ierr)
        if(ierr.eq.0) ss3%surf = w1*ss1%surf + (1.0_rspec-w1)*ss2%surf
     ELSE IF(ALLOCATED(ss1%surf)) THEN
        CALL ps_conformal("surf",ss1%surf,ss3%surf,iout,ierr)
        ss3%surf = ss1%surf
     ELSE IF(ALLOCATED(ss2%surf)) THEN
        CALL ps_conformal("surf",ss2%surf,ss3%surf,iout,ierr)
        ss3%surf = ss2%surf
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%S_name)) THEN
     IF(ALLOCATED(ss1%S_name).AND.ALLOCATED(ss2%S_name)) THEN
        CALL ps_diff("S_name",ss1%S_name,ss2%S_name,ps_error,iout,ierr)
        CALL ps_conformal("S_name",ss1%S_name,ss3%S_name,iout,ierr)
        if(ierr.eq.0) ss3%S_name = ss1%S_name
     ELSE IF(ALLOCATED(ss1%S_name)) THEN
        CALL ps_conformal("S_name",ss1%S_name,ss3%S_name,iout,ierr)
        ss3%S_name = ss1%S_name
     ELSE IF(ALLOCATED(ss2%S_name)) THEN
        CALL ps_conformal("S_name",ss2%S_name,ss3%S_name,iout,ierr)
        ss3%S_name = ss2%S_name
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%s_to_sa)) THEN
     IF(ALLOCATED(ss1%s_to_sa).AND.ALLOCATED(ss2%s_to_sa)) THEN
        CALL ps_diff("s_to_sa",ss1%s_to_sa,ss2%s_to_sa,ps_error,iout,ierr)
        CALL ps_conformal("s_to_sa",ss1%s_to_sa,ss3%s_to_sa,iout,ierr)
        if(ierr.eq.0) ss3%s_to_sa = ss1%s_to_sa
     ELSE IF(ALLOCATED(ss1%s_to_sa)) THEN
        CALL ps_conformal("s_to_sa",ss1%s_to_sa,ss3%s_to_sa,iout,ierr)
        ss3%s_to_sa = ss1%s_to_sa
     ELSE IF(ALLOCATED(ss2%s_to_sa)) THEN
        CALL ps_conformal("s_to_sa",ss2%s_to_sa,ss3%s_to_sa,iout,ierr)
        ss3%s_to_sa = ss2%s_to_sa
     ENDIF
  ENDIF

  IF(ALLOCATED(ss3%S_type)) THEN
     IF(ALLOCATED(ss1%S_type).AND.ALLOCATED(ss2%S_type)) THEN
        CALL ps_diff("S_type",ss1%S_type,ss2%S_type,ps_error,iout,ierr)
        CALL ps_conformal("S_type",ss1%S_type,ss3%S_type,iout,ierr)
        if(ierr.eq.0) ss3%S_type = ss1%S_type
     ELSE IF(ALLOCATED(ss1%S_type)) THEN
        CALL ps_conformal("S_type",ss1%S_type,ss3%S_type,iout,ierr)
        ss3%S_type = ss1%S_type
     ELSE IF(ALLOCATED(ss2%S_type)) THEN
        CALL ps_conformal("S_type",ss2%S_type,ss3%S_type,iout,ierr)
        ss3%S_type = ss2%S_type
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     ss3%t0 = w1*ss1%t0 + (1.0_rspec-w1)*ss2%t0
  ENDIF

  IF((ALLOCATED(ss3%T0cx)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%T0cx).AND.ALLOCATED(ss2%T0cx)) THEN
        CALL ps_conformal("T0cx",ss2%T0cx,ss3%T0cx,iout,ierr)
        CALL ps_conformal("T0cx",ss1%T0cx,ss3%T0cx,iout,ierr)
        if(ierr.eq.0) ss3%T0cx = w1*ss1%T0cx + (1.0_rspec-w1)*ss2%T0cx
     ELSE IF(ALLOCATED(ss1%T0cx)) THEN
        CALL ps_conformal("T0cx",ss1%T0cx,ss3%T0cx,iout,ierr)
        ss3%T0cx = ss1%T0cx
     ELSE IF(ALLOCATED(ss2%T0cx)) THEN
        CALL ps_conformal("T0cx",ss2%T0cx,ss3%T0cx,iout,ierr)
        ss3%T0cx = ss2%T0cx
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%T0sc0)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%T0sc0).AND.ALLOCATED(ss2%T0sc0)) THEN
        CALL ps_conformal("T0sc0",ss2%T0sc0,ss3%T0sc0,iout,ierr)
        CALL ps_conformal("T0sc0",ss1%T0sc0,ss3%T0sc0,iout,ierr)
        if(ierr.eq.0) ss3%T0sc0 = w1*ss1%T0sc0 + (1.0_rspec-w1)*ss2%T0sc0
     ELSE IF(ALLOCATED(ss1%T0sc0)) THEN
        CALL ps_conformal("T0sc0",ss1%T0sc0,ss3%T0sc0,iout,ierr)
        ss3%T0sc0 = ss1%T0sc0
     ELSE IF(ALLOCATED(ss2%T0sc0)) THEN
        CALL ps_conformal("T0sc0",ss2%T0sc0,ss3%T0sc0,iout,ierr)
        ss3%T0sc0 = ss2%T0sc0
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%T0_halo)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%T0_halo).AND.ALLOCATED(ss2%T0_halo)) THEN
        CALL ps_conformal("T0_halo",ss2%T0_halo,ss3%T0_halo,iout,ierr)
        CALL ps_conformal("T0_halo",ss1%T0_halo,ss3%T0_halo,iout,ierr)
        if(ierr.eq.0) ss3%T0_halo = w1*ss1%T0_halo + (1.0_rspec-w1)*ss2%T0_halo
     ELSE IF(ALLOCATED(ss1%T0_halo)) THEN
        CALL ps_conformal("T0_halo",ss1%T0_halo,ss3%T0_halo,iout,ierr)
        ss3%T0_halo = ss1%T0_halo
     ELSE IF(ALLOCATED(ss2%T0_halo)) THEN
        CALL ps_conformal("T0_halo",ss2%T0_halo,ss3%T0_halo,iout,ierr)
        ss3%T0_halo = ss2%T0_halo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%T0_reco)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%T0_reco).AND.ALLOCATED(ss2%T0_reco)) THEN
        CALL ps_conformal("T0_reco",ss2%T0_reco,ss3%T0_reco,iout,ierr)
        CALL ps_conformal("T0_reco",ss1%T0_reco,ss3%T0_reco,iout,ierr)
        if(ierr.eq.0) ss3%T0_reco = w1*ss1%T0_reco + (1.0_rspec-w1)*ss2%T0_reco
     ELSE IF(ALLOCATED(ss1%T0_reco)) THEN
        CALL ps_conformal("T0_reco",ss1%T0_reco,ss3%T0_reco,iout,ierr)
        ss3%T0_reco = ss1%T0_reco
     ELSE IF(ALLOCATED(ss2%T0_reco)) THEN
        CALL ps_conformal("T0_reco",ss2%T0_reco,ss3%T0_reco,iout,ierr)
        ss3%T0_reco = ss2%T0_reco
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_merge_7
