
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_grids(ss,xhash,iout,ierr)

  ! called from plasma_state_xload: check grids; store in xplasma structure

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:
  INTEGER :: isize,icomponent,iertmp
  CHARACTER*80 :: plabel
  INTEGER :: ahash(ps_max_static)
  !-----------------------

  ahash(1) = ps_dhash_NEFI_ANOM
  isize = ss%nefi_anom
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_ANOM
     plabel = "(optional) energy grid (1d profiles)"
     CALL ps_grid_store(ss%iobj,"E","E_ANOM",icomponent,plabel, &
          isize,0,ss%E_anom, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NEFI_ANOM2
  isize = ss%nefi_anom2
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_ANOM
     plabel = "energy grid (2d anomalous transport profiles)"
     CALL ps_grid_store(ss%iobj,"E","E_ANOM2",icomponent,plabel, &
          isize,0,ss%E_anom2, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NR
  isize = ss%nr
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_EQ
     plabel = "R grid"
     CALL ps_grid_store(ss%iobj,"R","R_GRID",icomponent,plabel, &
          isize,0,ss%R_grid, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO
  isize = ss%nrho
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_PLASMA
     plabel = "rho grid (PLASMA)"
     CALL ps_grid_store(ss%iobj,"RHO","RHO",icomponent,plabel, &
          isize,0,ss%rho, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_ANOM
  isize = ss%nrho_anom
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_ANOM
     plabel = "rho grid (1d anomalous transport profiles)"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_ANOM",icomponent,plabel, &
          isize,0,ss%rho_anom, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_ANOM2
  isize = ss%nrho_anom2
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_ANOM
     plabel = "rho grid (2d anomalous transport profiles)"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_ANOM2",icomponent,plabel, &
          isize,0,ss%rho_anom2, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_ANOMTH
  isize = ss%nrho_anomth
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_ANOM
     plabel = "rho grid (1d anomalous data for 1d"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_ANOMTH",icomponent,plabel, &
          isize,0,ss%rho_anomth, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_ECRF
  isize = ss%nrho_ecrf
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_EC
     plabel = "rho grid -- ECRF"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_ECRF",icomponent,plabel, &
          isize,0,ss%rho_ecrf, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_EQ
  isize = ss%nrho_eq
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_EQ
     plabel = "rho grid (EQ)"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_EQ",icomponent,plabel, &
          isize,0,ss%rho_eq, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_EQ_GEO
  isize = ss%nrho_eq_geo
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_EQ
     plabel = "rho grid (EQ flux surface averages)"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_EQ_GEO",icomponent,plabel, &
          isize,0,ss%rho_eq_geo, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_FUS
  isize = ss%nrho_fus
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_FUS
     plabel = "rho grid -- Fusion products model"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_FUS",icomponent,plabel, &
          isize,0,ss%rho_fus, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_GAS
  isize = ss%nrho_gas
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_GAS
     plabel = "rho grid (GAS -- neutral gas model)"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_GAS",icomponent,plabel, &
          isize,0,ss%rho_gas, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_ICRF
  isize = ss%nrho_icrf
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_IC
     plabel = "rho grid (RF)"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_ICRF",icomponent,plabel, &
          isize,0,ss%rho_icrf, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_LHRF
  isize = ss%nrho_lhrf
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_LH
     plabel = "rho grid -- LHRF"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_LHRF",icomponent,plabel, &
          isize,0,ss%rho_lhrf, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_LMHD
  isize = ss%nrho_lmhd
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_LMHD
     plabel = "rho grid (LMHD component)"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_LMHD",icomponent,plabel, &
          isize,0,ss%rho_lmhd, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_MIST
  isize = ss%nrho_mist
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_RAD
     plabel = "rho grid (PLASMA) impurity-driven sources"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_MIST",icomponent,plabel, &
          isize,0,ss%rho_mist, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_NBI
  isize = ss%nrho_nbi
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_NBI
     plabel = "rho grid -- NBI"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_NBI",icomponent,plabel, &
          isize,0,ss%rho_nbi, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_PEL
  isize = ss%nrho_pel
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_PEL
     plabel = "rho grid -- PEL"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_PEL",icomponent,plabel, &
          isize,0,ss%rho_pel, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_RAD
  isize = ss%nrho_rad
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_RAD
     plabel = "rho grid (RAD)"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_RAD",icomponent,plabel, &
          isize,0,ss%rho_rad, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NRHO_RW
  isize = ss%nrho_rw
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_RUNAWAY
     plabel = "rho grid -- RUNAWAY electrons"
     CALL ps_grid_store(ss%iobj,"RHO","RHO_RW",icomponent,plabel, &
          isize,0,ss%rho_rw, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NR_RIP
  isize = ss%nr_rip
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_RIPPLE
     plabel = "R grid for TF ripple field(s)"
     CALL ps_grid_store(ss%iobj,"R","R_RIPPLE",icomponent,plabel, &
          isize,0,ss%R_ripple, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NTH_EQ
  isize = ss%nth_eq
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_EQ
     plabel = "theta grid (EQ)"
     CALL ps_grid_store(ss%iobj,"TH","TH_EQ",icomponent,plabel, &
          isize,ps_ccw_select,ss%th_eq, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NZ
  isize = ss%nz
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_EQ
     plabel = "Z grid"
     CALL ps_grid_store(ss%iobj,"Z","Z_GRID",icomponent,plabel, &
          isize,0,ss%Z_grid, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NZ_RIP
  isize = ss%nz_rip
  IF((isize.GT.0).AND.hcheck(1,1)) THEN
     icomponent = ps_cnum_RIPPLE
     plabel = "Z grid for TF ripple field(s)"
     CALL ps_grid_store(ss%iobj,"Z","Z_RIPPLE",icomponent,plabel, &
          isize,0,ss%Z_ripple, iout,iertmp)
     ierr=MAX(ierr,iertmp)
  ENDIF

CONTAINS

  LOGICAL FUNCTION hcheck(inum,idchk)
     INTEGER, intent(in) :: inum
     INTEGER, intent(in) :: idchk

     !  check all hash codes: set function value TRUE if any show changes.
     !  also set TRUE if idchk = 0

     INTEGER :: ia1,ia2,ii

     if(idchk.eq.0) then
        hcheck=.TRUE.
        return
     endif

     hcheck = .FALSE.
     DO ii=1,inum
        hcheck = .TRUE.
        ia1 = ahash(ii)
        ia2 = ia1 + 1
        IF(ss%hash_codes(ia1).NE.ss%new_hash_codes(ia1)) EXIT
        IF(ss%hash_codes(ia2).NE.ss%new_hash_codes(ia2)) EXIT
        IF(xhash(ia1).NE.ss%new_hash_codes(ia1)) EXIT
        IF(xhash(ia2).NE.ss%new_hash_codes(ia2)) EXIT
        hcheck = .FALSE.
     ENDDO
  END FUNCTION hcheck

  SUBROUTINE lefpack(str)
     CHARACTER*(*), intent(inout) :: str
     !  left pack non-blank characters in str

     INTEGER :: ii,jj
     ii=0
     DO jj=1,len(str)
        IF(str(jj:jj).NE.' ') THEN
           ii = ii + 1
           str(ii:ii) = str(jj:jj)
        ENDIF
     ENDDO
     str(ii+1:) = ' '
  END SUBROUTINE lefpack

END SUBROUTINE plasma_state_xload_grids
