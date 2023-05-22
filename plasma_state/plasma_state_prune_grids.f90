! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_prune_grids(cclist,ss,ierr)

  ! for each component deactivated in cclist(:), remove component grids
  ! and any profiles which use these grids

  ! if the EQ component is deactivated, remove equilibrium-derived quantities.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components
                                !   cclist(j)=0 => remove grids of component j

  TYPE (plasma_state) :: ss     ! state data to copy *from*
  TYPE (plasma_state) :: ss2    ! state data to copy *to*
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------
  INTEGER :: iout,iertmp
  !-------------------

  iout = SWIM_err_out_file

  ierr = 0

  IF(cclist(ps_cnum_EQ).eq.0) call ps_purge_equilibrium(ss%iobj, iout, ierr)


  IF(cclist(ps_cnum_ANOM).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_ANOM,"E_ANOM",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_ANOM).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_ANOM,"E_ANOM2",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_PLASMA).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_PLASMA,"RHO",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_ANOM).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_ANOM,"RHO_ANOM",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_ANOM).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_ANOM,"RHO_ANOM2",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_ANOM).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_ANOM,"RHO_ANOMTH",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_EC).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_EC,"RHO_ECRF",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_EQ).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_EQ,"RHO_EQ",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_EQ).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_EQ,"RHO_EQ_GEO",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_FUS).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_FUS,"RHO_FUS",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_GAS).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_GAS,"RHO_GAS",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_IC).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_IC,"RHO_ICRF",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_LH).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_LH,"RHO_LHRF",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_LMHD).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_LMHD,"RHO_LMHD",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_RAD).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_RAD,"RHO_MIST",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_NBI).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_NBI,"RHO_NBI",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_PEL).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_PEL,"RHO_PEL",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_RAD).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_RAD,"RHO_RAD",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_RUNAWAY).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_RUNAWAY,"RHO_RW",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_EQ).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_EQ,"R_GRID",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_RIPPLE).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_RIPPLE,"R_RIPPLE",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_EQ).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_EQ,"TH_EQ",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_EQ).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_EQ,"Z_GRID",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  IF(cclist(ps_cnum_RIPPLE).eq.0) THEN
     CALL ps_remove_grid(ss%iobj,ps_cnum_RIPPLE,"Z_RIPPLE",iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF


END SUBROUTINE plasma_state_prune_grids
