! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_read_6(ss,icdf,iout,iucount,iertot)


  ! Read updated data

  USE plasma_state_definition_mod
  USE plasma_hash_code
  USE netcdf
  IMPLICIT NONE

  TYPE (plasma_state) :: ss  ! state containing array elements to be allocated
  INTEGER, INTENT(IN) :: icdf  ! NetCDF file ID
  INTEGER, INTENT(IN) :: iout  ! LUN for messages
  INTEGER, INTENT(INOUT) :: iucount ! update count
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !-------------------

  INTEGER :: varid,status
  INTEGER :: ival,jval,ii
  INTEGER :: prehash1,prehash2
  INTEGER :: posthash1,posthash2
  CHARACTER*80 :: nc_errmsg
  LOGICAL :: izero

  !-------------------


  status = nf90_inq_varid(icdf,"prad_li",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%prad_li)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: prad_li"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"psc_halo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%psc_halo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: psc_halo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"psc_reco",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%psc_reco)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: psc_reco"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"psipol",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%psipol)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: psipol"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"PsiRZ",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%PsiRZ)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: PsiRZ"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Psi_to_machine_axis",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Psi_to_machine_axis)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Psi_to_machine_axis"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"psmom_errck",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%psmom_errck)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: psmom_errck"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"psmom_nc",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%psmom_nc)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: psmom_nc"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"psmom_num",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%psmom_num)
     status = nf90_get_var(icdf,varid,ss%psmom_num)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: psmom_num"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("psmom_num",ss%psmom_num)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"P_eq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%P_eq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: P_eq"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_ALL",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%qatom_ALL)
     status = nf90_get_var(icdf,varid,ss%qatom_ALL)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qatom_ALL"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("qatom_ALL",ss%qatom_ALL)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_ALLA",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%qatom_ALLA)
     status = nf90_get_var(icdf,varid,ss%qatom_ALLA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qatom_ALLA"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("qatom_ALLA",ss%qatom_ALLA)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_RFMIN",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%qatom_RFMIN)
     status = nf90_get_var(icdf,varid,ss%qatom_RFMIN)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qatom_RFMIN"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("qatom_RFMIN",ss%qatom_RFMIN)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_S",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%qatom_S)
     status = nf90_get_var(icdf,varid,ss%qatom_S)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qatom_S"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("qatom_S",ss%qatom_S)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_SA",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%qatom_SA)
     status = nf90_get_var(icdf,varid,ss%qatom_SA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qatom_SA"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("qatom_SA",ss%qatom_SA)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_SFUS",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%qatom_SFUS)
     status = nf90_get_var(icdf,varid,ss%qatom_SFUS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qatom_SFUS"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("qatom_SFUS",ss%qatom_SFUS)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_SGAS",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%qatom_SGAS)
     status = nf90_get_var(icdf,varid,ss%qatom_SGAS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qatom_SGAS"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("qatom_SGAS",ss%qatom_SGAS)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_SIMP0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%qatom_SIMP0)
     status = nf90_get_var(icdf,varid,ss%qatom_SIMP0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qatom_SIMP0"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("qatom_SIMP0",ss%qatom_SIMP0)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_SIMPI",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%qatom_SIMPI)
     status = nf90_get_var(icdf,varid,ss%qatom_SIMPI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qatom_SIMPI"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("qatom_SIMPI",ss%qatom_SIMPI)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qatom_SNBI",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%qatom_SNBI)
     status = nf90_get_var(icdf,varid,ss%qatom_SNBI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qatom_SNBI"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("qatom_SNBI",ss%qatom_SNBI)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qbeami",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%qbeami)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qbeami"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qcomp_e",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%qcomp_e)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qcomp_e"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qcomp_i",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%qcomp_i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qcomp_i"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qie",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%qie)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qie"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qioniz",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%qioniz)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qioniz"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ql_operator",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ql_operator)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ql_operator"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qqcx",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%qqcx)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qqcx"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qrot_conv",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%qrot_conv)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qrot_conv"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"qrot_diff",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%qrot_diff)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: qrot_diff"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_ALL",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%q_ALL)
     status = nf90_get_var(icdf,varid,ss%q_ALL)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: q_ALL"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("q_ALL",ss%q_ALL)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_ALLA",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%q_ALLA)
     status = nf90_get_var(icdf,varid,ss%q_ALLA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: q_ALLA"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("q_ALLA",ss%q_ALLA)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_eq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%q_eq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: q_eq"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_RFMIN",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%q_RFMIN)
     status = nf90_get_var(icdf,varid,ss%q_RFMIN)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: q_RFMIN"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("q_RFMIN",ss%q_RFMIN)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_S",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%q_S)
     status = nf90_get_var(icdf,varid,ss%q_S)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: q_S"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("q_S",ss%q_S)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_SA",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%q_SA)
     status = nf90_get_var(icdf,varid,ss%q_SA)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: q_SA"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("q_SA",ss%q_SA)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_SFUS",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%q_SFUS)
     status = nf90_get_var(icdf,varid,ss%q_SFUS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: q_SFUS"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("q_SFUS",ss%q_SFUS)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_SGAS",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%q_SGAS)
     status = nf90_get_var(icdf,varid,ss%q_SGAS)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: q_SGAS"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("q_SGAS",ss%q_SGAS)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_SIMP0",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%q_SIMP0)
     status = nf90_get_var(icdf,varid,ss%q_SIMP0)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: q_SIMP0"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("q_SIMP0",ss%q_SIMP0)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_SIMPI",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%q_SIMPI)
     status = nf90_get_var(icdf,varid,ss%q_SIMPI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: q_SIMPI"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("q_SIMPI",ss%q_SIMPI)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"q_SNBI",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%q_SNBI)
     status = nf90_get_var(icdf,varid,ss%q_SNBI)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: q_SNBI"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("q_SNBI",ss%q_SNBI)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"R0_momeq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%R0_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: R0_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RAD_Code_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%RAD_Code_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: RAD_Code_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RAD_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%RAD_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: RAD_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rate_sinb0i",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rate_sinb0i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rate_sinb0i"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rate_sinb0x",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rate_sinb0x)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rate_sinb0x"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rate_sinb0xs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rate_sinb0xs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rate_sinb0xs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rate_sinf0i",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rate_sinf0i)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rate_sinf0i"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rate_sinf0x",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rate_sinf0x)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rate_sinf0x"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rate_sinf0xs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rate_sinf0xs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rate_sinf0xs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Real_ant_coef",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Real_ant_coef)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Real_ant_coef"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"res_sn",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%res_sn)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: res_sn"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"res_te",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%res_te)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: res_te"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"res_ti",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%res_ti)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: res_ti"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"res_tq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%res_tq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: res_tq"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RFMIN_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%RFMIN_name)
     status = nf90_get_var(icdf,varid,ss%RFMIN_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: RFMIN_name"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("RFMIN_name",ss%RFMIN_name)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rfmin_to_all",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rfmin_to_all)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rfmin_to_all"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rfmin_to_alla",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rfmin_to_alla)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rfmin_to_alla"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RFMIN_type",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL ichk_zero(ss%RFMIN_type)
     status = nf90_get_var(icdf,varid,ss%RFMIN_type)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: RFMIN_type"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL ichk_hash("RFMIN_type",ss%RFMIN_type)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho)
     status = nf90_get_var(icdf,varid,ss%rho)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho",ss%rho)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rhopsi0lh",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rhopsi0lh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rhopsi0lh"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_anom)
     status = nf90_get_var(icdf,varid,ss%rho_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_anom"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_anom",ss%rho_anom)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_anom2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_anom2)
     status = nf90_get_var(icdf,varid,ss%rho_anom2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_anom2"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_anom2",ss%rho_anom2)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_anomth",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_anomth)
     status = nf90_get_var(icdf,varid,ss%rho_anomth)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_anomth"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_anomth",ss%rho_anomth)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_ns",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rho_bdy_ns)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_bdy_ns"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_omegat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rho_bdy_omegat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_bdy_omegat"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_Te",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rho_bdy_Te)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_bdy_Te"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_Ti",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%rho_bdy_Ti)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_bdy_Ti"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_ecrf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_ecrf)
     status = nf90_get_var(icdf,varid,ss%rho_ecrf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_ecrf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_ecrf",ss%rho_ecrf)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_eq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_eq)
     status = nf90_get_var(icdf,varid,ss%rho_eq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_eq"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_eq",ss%rho_eq)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_eq_geo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_eq_geo)
     status = nf90_get_var(icdf,varid,ss%rho_eq_geo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_eq_geo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_eq_geo",ss%rho_eq_geo)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_fus",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_fus)
     status = nf90_get_var(icdf,varid,ss%rho_fus)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_fus"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_fus",ss%rho_fus)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_gas",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_gas)
     status = nf90_get_var(icdf,varid,ss%rho_gas)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_gas"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_gas",ss%rho_gas)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_icrf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_icrf)
     status = nf90_get_var(icdf,varid,ss%rho_icrf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_icrf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_icrf",ss%rho_icrf)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_lhrf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_lhrf)
     status = nf90_get_var(icdf,varid,ss%rho_lhrf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_lhrf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_lhrf",ss%rho_lhrf)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_lmhd",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_lmhd)
     status = nf90_get_var(icdf,varid,ss%rho_lmhd)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_lmhd"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_lmhd",ss%rho_lmhd)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_mist",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_mist)
     status = nf90_get_var(icdf,varid,ss%rho_mist)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_mist"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_mist",ss%rho_mist)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_nbi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%rho_nbi)
     status = nf90_get_var(icdf,varid,ss%rho_nbi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: rho_nbi"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("rho_nbi",ss%rho_nbi)
     ENDIF
  ENDIF

CONTAINS

  subroutine ichk_zero(iarray)
     integer, dimension(:), intent(in) :: iarray

     integer :: ii

     izero=.TRUE.
     do ii=1,size(iarray)
        if(iarray(ii).ne.0) then
           izero=.FALSE.
           exit
        endif
     enddo

     if(.not.izero) then
        prehash1=17
        prehash2=23
        call hashCode(iarray,prehash1,prehash2)
     endif

  end subroutine ichk_zero

  subroutine rchk_zero(rarray)
     real(kind=rspec), dimension(:), intent(in) :: rarray

     integer :: ii

     izero=.TRUE.
     do ii=1,size(rarray)
        if(rarray(ii).ne.0.0_rspec) then
           izero=.FALSE.
           exit
        endif
     enddo

     if(.not.izero) then
        prehash1=17
        prehash2=23
        call hashCode(rarray,prehash1,prehash2)
     endif

  end subroutine rchk_zero

  subroutine cchk_zero(c32array)
     character*32, dimension(:), intent(in) :: c32array

     integer :: ii

     izero=.TRUE.
     do ii=1,size(c32array)
        if(c32array(ii).ne." ") then
           izero=.FALSE.
           exit
        endif
     enddo

     if(.not.izero) then
        prehash1=17
        prehash2=23
        call hashCode(c32array,prehash1,prehash2)
     endif

  end subroutine cchk_zero

  subroutine ichk_hash(itemName,iarray)
     character*(*), intent(in) :: itemName
     integer, dimension(:), intent(in) :: iarray

     posthash1=17
     posthash2=23
     call hashCode(iarray,posthash1,posthash2)

     if((prehash1.ne.posthash1).OR.(prehash2.ne.posthash2)) then
        write(iout,*) ' ?ps_update_file_read: value change detected in dimension quantity: '//trim(itemName)
        iertot = iertot + 1
     endif

  end subroutine ichk_hash

  subroutine rchk_hash(itemName,rarray)
     character*(*), intent(in) :: itemName
     real(kind=rspec), dimension(:), intent(in) :: rarray

     posthash1=17
     posthash2=23
     call hashCode(rarray,posthash1,posthash2)

     if((prehash1.ne.posthash1).OR.(prehash2.ne.posthash2)) then
        write(iout,*) ' ?ps_update_file_read: value change detected in dimension quantity: '//trim(itemName)
        iertot = iertot + 1
     endif

  end subroutine rchk_hash

  subroutine cchk_hash(itemName,c32array)
     character*(*), intent(in) :: itemName
     character*32, dimension(:), intent(in) :: c32array

     posthash1=17
     posthash2=23
     call hashCode(c32array,posthash1,posthash2)

     if((prehash1.ne.posthash1).OR.(prehash2.ne.posthash2)) then
        write(iout,*) ' ?ps_update_file_read: value change detected in dimension quantity: '//trim(itemName)
        iertot = iertot + 1
     endif

  end subroutine cchk_hash

  SUBROUTINE ichk_dim(ival,jval,dname)
     INTEGER, intent(in) :: ival  ! file dimension value
     INTEGER, intent(in) :: jval  ! state dimension value
     CHARACTER*(*), intent(in) :: dname

     IF(ival.ne.jval) THEN
        WRITE(iout,*) ' ?ps_update_file_read: update file dimension not consistent with state:'
        WRITE(iout,*) '  file value:  ',ival
        WRITE(iout,*) '  state value: ',jval
        WRITE(iout,*) '  dimension name: '//trim(dname)
        iertot = iertot + 1
     ENDIF
  END SUBROUTINE ichk_dim

  SUBROUTINE ncerr(status)
     INTEGER, INTENT(in) :: status

     nc_errmsg = NF90_strerror(status)
     write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
  END SUBROUTINE ncerr

  SUBROUTINE denull(str)
     ! convert null characters to blank
     CHARACTER*(*) :: str
     INTEGER :: i

     do i=1,len(str)
        if(ichar(str(i:i)).eq.0) str(i:i)=' '
     enddo
  END SUBROUTINE denull

END SUBROUTINE ps_update_file_read_6
