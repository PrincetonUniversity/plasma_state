! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_get_6(ss,icdf,version_change,iout,iertot)


  !  read subset of variables and arrays from NetCDF file

  USE plasma_state_definition_mod
  USE netcdf
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to read
  INTEGER, INTENT(IN) :: icdf   ! NetCDF file ID
  LOGICAL, INTENT(IN) :: version_change  ! flag file/software version mismatch
  INTEGER, INTENT(IN) :: iout   ! message LUN
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !--------------

  INTEGER :: varid,dimid,status,ilen,jlen,idims(20),indim
  INTEGER :: ii,iflag_alloc2,iflag_loc2,isize
  CHARACTER*80 :: nc_errmsg

  !--------------


  !-----------------------------------------------
  ! NetCDF reads...

  IF(ALLOCATED(ss%prad_li)) THEN
     status = nf90_inq_varid(icdf,"prad_li",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"prad_li")
     ELSE
        status = nf90_get_var(icdf,varid,ss%prad_li)
        IF(status.NE.NF90_NOERR) THEN
           ss%prad_li = 0.0_rspec
           CALL ncerr_name(2,status,"prad_li")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%psc_halo)) THEN
     status = nf90_inq_varid(icdf,"psc_halo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"psc_halo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%psc_halo)
        IF(status.NE.NF90_NOERR) THEN
           ss%psc_halo = 0.0_rspec
           CALL ncerr_name(2,status,"psc_halo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%psc_reco)) THEN
     status = nf90_inq_varid(icdf,"psc_reco",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"psc_reco")
     ELSE
        status = nf90_get_var(icdf,varid,ss%psc_reco)
        IF(status.NE.NF90_NOERR) THEN
           ss%psc_reco = 0.0_rspec
           CALL ncerr_name(2,status,"psc_reco")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%psipol)) THEN
     status = nf90_inq_varid(icdf,"psipol",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"psipol")
     ELSE
        status = nf90_get_var(icdf,varid,ss%psipol)
        IF(status.NE.NF90_NOERR) THEN
           ss%psipol = 0.0_rspec
           CALL ncerr_name(2,status,"psipol")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%PsiRZ)) THEN
     status = nf90_inq_varid(icdf,"PsiRZ",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"PsiRZ")
     ELSE
        status = nf90_get_var(icdf,varid,ss%PsiRZ)
        IF(status.NE.NF90_NOERR) THEN
           ss%PsiRZ = 0.0_rspec
           CALL ncerr_name(2,status,"PsiRZ")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Psi_to_machine_axis",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"Psi_to_machine_axis")
  ELSE
     status = nf90_get_var(icdf,varid,ss%Psi_to_machine_axis)
     IF(status.NE.NF90_NOERR) THEN
        ss%Psi_to_machine_axis = 0.0_rspec
        CALL ncerr_name(2,status,"Psi_to_machine_axis")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%psmom_errck)) THEN
     status = nf90_inq_varid(icdf,"psmom_errck",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"psmom_errck")
     ELSE
        status = nf90_get_var(icdf,varid,ss%psmom_errck)
        IF(status.NE.NF90_NOERR) THEN
           ss%psmom_errck = 0.0_rspec
           CALL ncerr_name(2,status,"psmom_errck")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%psmom_nc)) THEN
     status = nf90_inq_varid(icdf,"psmom_nc",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"psmom_nc")
     ELSE
        status = nf90_get_var(icdf,varid,ss%psmom_nc)
        IF(status.NE.NF90_NOERR) THEN
           ss%psmom_nc = 0.0_rspec
           CALL ncerr_name(2,status,"psmom_nc")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%psmom_num)) THEN
     status = nf90_inq_varid(icdf,"psmom_num",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"psmom_num")
     ELSE
        status = nf90_get_var(icdf,varid,ss%psmom_num)
        IF(status.NE.NF90_NOERR) THEN
           ss%psmom_num = " "
           CALL ncerr_name(2,status,"psmom_num")
        ELSE
           CALL denull_vec(ss%psmom_num,ss%npsmom)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%P_eq)) THEN
     status = nf90_inq_varid(icdf,"P_eq",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"P_eq")
     ELSE
        status = nf90_get_var(icdf,varid,ss%P_eq)
        IF(status.NE.NF90_NOERR) THEN
           ss%P_eq = 0.0_rspec
           CALL ncerr_name(2,status,"P_eq")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_ALL)) THEN
     status = nf90_inq_varid(icdf,"qatom_ALL",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qatom_ALL")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qatom_ALL)
        IF(status.NE.NF90_NOERR) THEN
           ss%qatom_ALL = 0.0_rspec
           CALL ncerr_name(2,status,"qatom_ALL")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_ALLA)) THEN
     status = nf90_inq_varid(icdf,"qatom_ALLA",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qatom_ALLA")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qatom_ALLA)
        IF(status.NE.NF90_NOERR) THEN
           ss%qatom_ALLA = 0.0_rspec
           CALL ncerr_name(2,status,"qatom_ALLA")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_RFMIN)) THEN
     status = nf90_inq_varid(icdf,"qatom_RFMIN",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qatom_RFMIN")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qatom_RFMIN)
        IF(status.NE.NF90_NOERR) THEN
           ss%qatom_RFMIN = 0.0_rspec
           CALL ncerr_name(2,status,"qatom_RFMIN")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_S)) THEN
     status = nf90_inq_varid(icdf,"qatom_S",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qatom_S")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qatom_S)
        IF(status.NE.NF90_NOERR) THEN
           ss%qatom_S = 0.0_rspec
           CALL ncerr_name(2,status,"qatom_S")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_SA)) THEN
     status = nf90_inq_varid(icdf,"qatom_SA",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qatom_SA")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qatom_SA)
        IF(status.NE.NF90_NOERR) THEN
           ss%qatom_SA = 0.0_rspec
           CALL ncerr_name(2,status,"qatom_SA")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_SFUS)) THEN
     status = nf90_inq_varid(icdf,"qatom_SFUS",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qatom_SFUS")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qatom_SFUS)
        IF(status.NE.NF90_NOERR) THEN
           ss%qatom_SFUS = 0.0_rspec
           CALL ncerr_name(2,status,"qatom_SFUS")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_SGAS)) THEN
     status = nf90_inq_varid(icdf,"qatom_SGAS",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qatom_SGAS")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qatom_SGAS)
        IF(status.NE.NF90_NOERR) THEN
           ss%qatom_SGAS = 0.0_rspec
           CALL ncerr_name(2,status,"qatom_SGAS")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_SIMP0)) THEN
     status = nf90_inq_varid(icdf,"qatom_SIMP0",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qatom_SIMP0")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qatom_SIMP0)
        IF(status.NE.NF90_NOERR) THEN
           ss%qatom_SIMP0 = 0.0_rspec
           CALL ncerr_name(2,status,"qatom_SIMP0")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_SIMPI)) THEN
     status = nf90_inq_varid(icdf,"qatom_SIMPI",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qatom_SIMPI")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qatom_SIMPI)
        IF(status.NE.NF90_NOERR) THEN
           ss%qatom_SIMPI = 0.0_rspec
           CALL ncerr_name(2,status,"qatom_SIMPI")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qatom_SNBI)) THEN
     status = nf90_inq_varid(icdf,"qatom_SNBI",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qatom_SNBI")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qatom_SNBI)
        IF(status.NE.NF90_NOERR) THEN
           ss%qatom_SNBI = 0.0_rspec
           CALL ncerr_name(2,status,"qatom_SNBI")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qbeami)) THEN
     status = nf90_inq_varid(icdf,"qbeami",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qbeami")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qbeami)
        IF(status.NE.NF90_NOERR) THEN
           ss%qbeami = 0.0_rspec
           CALL ncerr_name(2,status,"qbeami")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qcomp_e)) THEN
     status = nf90_inq_varid(icdf,"qcomp_e",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qcomp_e")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qcomp_e)
        IF(status.NE.NF90_NOERR) THEN
           ss%qcomp_e = 0.0_rspec
           CALL ncerr_name(2,status,"qcomp_e")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qcomp_i)) THEN
     status = nf90_inq_varid(icdf,"qcomp_i",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qcomp_i")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qcomp_i)
        IF(status.NE.NF90_NOERR) THEN
           ss%qcomp_i = 0.0_rspec
           CALL ncerr_name(2,status,"qcomp_i")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qie)) THEN
     status = nf90_inq_varid(icdf,"qie",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qie")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qie)
        IF(status.NE.NF90_NOERR) THEN
           ss%qie = 0.0_rspec
           CALL ncerr_name(2,status,"qie")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qioniz)) THEN
     status = nf90_inq_varid(icdf,"qioniz",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qioniz")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qioniz)
        IF(status.NE.NF90_NOERR) THEN
           ss%qioniz = 0.0_rspec
           CALL ncerr_name(2,status,"qioniz")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ql_operator)) THEN
     status = nf90_inq_varid(icdf,"ql_operator",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ql_operator")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ql_operator)
        IF(status.NE.NF90_NOERR) THEN
           ss%ql_operator = " "
           CALL ncerr_name(2,status,"ql_operator")
        ELSE
           CALL denull_vec(ss%ql_operator,ss%nspec_alla+1)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qqcx)) THEN
     status = nf90_inq_varid(icdf,"qqcx",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qqcx")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qqcx)
        IF(status.NE.NF90_NOERR) THEN
           ss%qqcx = 0.0_rspec
           CALL ncerr_name(2,status,"qqcx")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qrot_conv)) THEN
     status = nf90_inq_varid(icdf,"qrot_conv",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qrot_conv")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qrot_conv)
        IF(status.NE.NF90_NOERR) THEN
           ss%qrot_conv = 0.0_rspec
           CALL ncerr_name(2,status,"qrot_conv")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%qrot_diff)) THEN
     status = nf90_inq_varid(icdf,"qrot_diff",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"qrot_diff")
     ELSE
        status = nf90_get_var(icdf,varid,ss%qrot_diff)
        IF(status.NE.NF90_NOERR) THEN
           ss%qrot_diff = 0.0_rspec
           CALL ncerr_name(2,status,"qrot_diff")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_ALL)) THEN
     status = nf90_inq_varid(icdf,"q_ALL",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"q_ALL")
     ELSE
        status = nf90_get_var(icdf,varid,ss%q_ALL)
        IF(status.NE.NF90_NOERR) THEN
           ss%q_ALL = 0.0_rspec
           CALL ncerr_name(2,status,"q_ALL")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_ALLA)) THEN
     status = nf90_inq_varid(icdf,"q_ALLA",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"q_ALLA")
     ELSE
        status = nf90_get_var(icdf,varid,ss%q_ALLA)
        IF(status.NE.NF90_NOERR) THEN
           ss%q_ALLA = 0.0_rspec
           CALL ncerr_name(2,status,"q_ALLA")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_eq)) THEN
     status = nf90_inq_varid(icdf,"q_eq",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"q_eq")
     ELSE
        status = nf90_get_var(icdf,varid,ss%q_eq)
        IF(status.NE.NF90_NOERR) THEN
           ss%q_eq = 0.0_rspec
           CALL ncerr_name(2,status,"q_eq")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_RFMIN)) THEN
     status = nf90_inq_varid(icdf,"q_RFMIN",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"q_RFMIN")
     ELSE
        status = nf90_get_var(icdf,varid,ss%q_RFMIN)
        IF(status.NE.NF90_NOERR) THEN
           ss%q_RFMIN = 0.0_rspec
           CALL ncerr_name(2,status,"q_RFMIN")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_S)) THEN
     status = nf90_inq_varid(icdf,"q_S",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"q_S")
     ELSE
        status = nf90_get_var(icdf,varid,ss%q_S)
        IF(status.NE.NF90_NOERR) THEN
           ss%q_S = 0.0_rspec
           CALL ncerr_name(2,status,"q_S")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_SA)) THEN
     status = nf90_inq_varid(icdf,"q_SA",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"q_SA")
     ELSE
        status = nf90_get_var(icdf,varid,ss%q_SA)
        IF(status.NE.NF90_NOERR) THEN
           ss%q_SA = 0.0_rspec
           CALL ncerr_name(2,status,"q_SA")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_SFUS)) THEN
     status = nf90_inq_varid(icdf,"q_SFUS",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"q_SFUS")
     ELSE
        status = nf90_get_var(icdf,varid,ss%q_SFUS)
        IF(status.NE.NF90_NOERR) THEN
           ss%q_SFUS = 0.0_rspec
           CALL ncerr_name(2,status,"q_SFUS")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_SGAS)) THEN
     status = nf90_inq_varid(icdf,"q_SGAS",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"q_SGAS")
     ELSE
        status = nf90_get_var(icdf,varid,ss%q_SGAS)
        IF(status.NE.NF90_NOERR) THEN
           ss%q_SGAS = 0.0_rspec
           CALL ncerr_name(2,status,"q_SGAS")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_SIMP0)) THEN
     status = nf90_inq_varid(icdf,"q_SIMP0",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"q_SIMP0")
     ELSE
        status = nf90_get_var(icdf,varid,ss%q_SIMP0)
        IF(status.NE.NF90_NOERR) THEN
           ss%q_SIMP0 = 0.0_rspec
           CALL ncerr_name(2,status,"q_SIMP0")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_SIMPI)) THEN
     status = nf90_inq_varid(icdf,"q_SIMPI",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"q_SIMPI")
     ELSE
        status = nf90_get_var(icdf,varid,ss%q_SIMPI)
        IF(status.NE.NF90_NOERR) THEN
           ss%q_SIMPI = 0.0_rspec
           CALL ncerr_name(2,status,"q_SIMPI")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%q_SNBI)) THEN
     status = nf90_inq_varid(icdf,"q_SNBI",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"q_SNBI")
     ELSE
        status = nf90_get_var(icdf,varid,ss%q_SNBI)
        IF(status.NE.NF90_NOERR) THEN
           ss%q_SNBI = 0.0_rspec
           CALL ncerr_name(2,status,"q_SNBI")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%R0_momeq)) THEN
     status = nf90_inq_varid(icdf,"R0_momeq",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"R0_momeq")
     ELSE
        status = nf90_get_var(icdf,varid,ss%R0_momeq)
        IF(status.NE.NF90_NOERR) THEN
           ss%R0_momeq = 0.0_rspec
           CALL ncerr_name(2,status,"R0_momeq")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RAD_Code_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"RAD_Code_Info")
  ELSE
     ilen = len(ss%RAD_Code_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%RAD_Code_Info = " "
     status = nf90_get_var(icdf,varid,ss%RAD_Code_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%RAD_Code_Info = " "
        CALL ncerr_name(2,status,"RAD_Code_Info")
     ELSE
        CALL denull(ss%RAD_Code_Info)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"RAD_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"RAD_Data_Info")
  ELSE
     ilen = len(ss%RAD_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%RAD_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%RAD_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%RAD_Data_Info = " "
        CALL ncerr_name(2,status,"RAD_Data_Info")
     ELSE
        CALL denull(ss%RAD_Data_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rate_sinb0i)) THEN
     status = nf90_inq_varid(icdf,"rate_sinb0i",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rate_sinb0i")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rate_sinb0i)
        IF(status.NE.NF90_NOERR) THEN
           ss%rate_sinb0i = 0.0_rspec
           CALL ncerr_name(2,status,"rate_sinb0i")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rate_sinb0x)) THEN
     status = nf90_inq_varid(icdf,"rate_sinb0x",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rate_sinb0x")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rate_sinb0x)
        IF(status.NE.NF90_NOERR) THEN
           ss%rate_sinb0x = 0.0_rspec
           CALL ncerr_name(2,status,"rate_sinb0x")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rate_sinb0xs)) THEN
     status = nf90_inq_varid(icdf,"rate_sinb0xs",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rate_sinb0xs")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rate_sinb0xs)
        IF(status.NE.NF90_NOERR) THEN
           ss%rate_sinb0xs = 0.0_rspec
           CALL ncerr_name(2,status,"rate_sinb0xs")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rate_sinf0i)) THEN
     status = nf90_inq_varid(icdf,"rate_sinf0i",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rate_sinf0i")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rate_sinf0i)
        IF(status.NE.NF90_NOERR) THEN
           ss%rate_sinf0i = 0.0_rspec
           CALL ncerr_name(2,status,"rate_sinf0i")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rate_sinf0x)) THEN
     status = nf90_inq_varid(icdf,"rate_sinf0x",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rate_sinf0x")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rate_sinf0x)
        IF(status.NE.NF90_NOERR) THEN
           ss%rate_sinf0x = 0.0_rspec
           CALL ncerr_name(2,status,"rate_sinf0x")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rate_sinf0xs)) THEN
     status = nf90_inq_varid(icdf,"rate_sinf0xs",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rate_sinf0xs")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rate_sinf0xs)
        IF(status.NE.NF90_NOERR) THEN
           ss%rate_sinf0xs = 0.0_rspec
           CALL ncerr_name(2,status,"rate_sinf0xs")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Real_ant_coef)) THEN
     status = nf90_inq_varid(icdf,"Real_ant_coef",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Real_ant_coef")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Real_ant_coef)
        IF(status.NE.NF90_NOERR) THEN
           ss%Real_ant_coef = 0.0_rspec
           CALL ncerr_name(2,status,"Real_ant_coef")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%res_sn)) THEN
     status = nf90_inq_varid(icdf,"res_sn",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"res_sn")
     ELSE
        status = nf90_get_var(icdf,varid,ss%res_sn)
        IF(status.NE.NF90_NOERR) THEN
           ss%res_sn = 0.0_rspec
           CALL ncerr_name(2,status,"res_sn")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%res_te)) THEN
     status = nf90_inq_varid(icdf,"res_te",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"res_te")
     ELSE
        status = nf90_get_var(icdf,varid,ss%res_te)
        IF(status.NE.NF90_NOERR) THEN
           ss%res_te = 0.0_rspec
           CALL ncerr_name(2,status,"res_te")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%res_ti)) THEN
     status = nf90_inq_varid(icdf,"res_ti",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"res_ti")
     ELSE
        status = nf90_get_var(icdf,varid,ss%res_ti)
        IF(status.NE.NF90_NOERR) THEN
           ss%res_ti = 0.0_rspec
           CALL ncerr_name(2,status,"res_ti")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%res_tq)) THEN
     status = nf90_inq_varid(icdf,"res_tq",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"res_tq")
     ELSE
        status = nf90_get_var(icdf,varid,ss%res_tq)
        IF(status.NE.NF90_NOERR) THEN
           ss%res_tq = 0.0_rspec
           CALL ncerr_name(2,status,"res_tq")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%RFMIN_name)) THEN
     status = nf90_inq_varid(icdf,"RFMIN_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"RFMIN_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%RFMIN_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%RFMIN_name = " "
           CALL ncerr_name(2,status,"RFMIN_name")
        ELSE
           CALL denull_vec(ss%RFMIN_name,ss%nspec_rfmin)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rfmin_to_all)) THEN
     status = nf90_inq_varid(icdf,"rfmin_to_all",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rfmin_to_all")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rfmin_to_all)
        IF(status.NE.NF90_NOERR) THEN
           ss%rfmin_to_all = 0
           CALL ncerr_name(2,status,"rfmin_to_all")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rfmin_to_alla)) THEN
     status = nf90_inq_varid(icdf,"rfmin_to_alla",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rfmin_to_alla")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rfmin_to_alla)
        IF(status.NE.NF90_NOERR) THEN
           ss%rfmin_to_alla = 0
           CALL ncerr_name(2,status,"rfmin_to_alla")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%RFMIN_type)) THEN
     status = nf90_inq_varid(icdf,"RFMIN_type",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"RFMIN_type")
     ELSE
        status = nf90_get_var(icdf,varid,ss%RFMIN_type)
        IF(status.NE.NF90_NOERR) THEN
           ss%RFMIN_type = 0
           CALL ncerr_name(2,status,"RFMIN_type")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho)) THEN
     status = nf90_inq_varid(icdf,"rho",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho = 0.0_rspec
           CALL ncerr_name(2,status,"rho")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rhopsi0lh)) THEN
     status = nf90_inq_varid(icdf,"rhopsi0lh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rhopsi0lh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rhopsi0lh)
        IF(status.NE.NF90_NOERR) THEN
           ss%rhopsi0lh = 0.0_rspec
           CALL ncerr_name(2,status,"rhopsi0lh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_anom)) THEN
     status = nf90_inq_varid(icdf,"rho_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_anom = 0.0_rspec
           CALL ncerr_name(2,status,"rho_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_anom2)) THEN
     status = nf90_inq_varid(icdf,"rho_anom2",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_anom2")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_anom2)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_anom2 = 0.0_rspec
           CALL ncerr_name(2,status,"rho_anom2")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_anomth)) THEN
     status = nf90_inq_varid(icdf,"rho_anomth",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_anomth")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_anomth)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_anomth = 0.0_rspec
           CALL ncerr_name(2,status,"rho_anomth")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_bdy_ns)) THEN
     status = nf90_inq_varid(icdf,"rho_bdy_ns",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_bdy_ns")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_bdy_ns)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_bdy_ns = 0.0_rspec
           CALL ncerr_name(2,status,"rho_bdy_ns")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_omegat",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"rho_bdy_omegat")
  ELSE
     status = nf90_get_var(icdf,varid,ss%rho_bdy_omegat)
     IF(status.NE.NF90_NOERR) THEN
        ss%rho_bdy_omegat = 0.0_rspec
        CALL ncerr_name(2,status,"rho_bdy_omegat")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_Te",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"rho_bdy_Te")
  ELSE
     status = nf90_get_var(icdf,varid,ss%rho_bdy_Te)
     IF(status.NE.NF90_NOERR) THEN
        ss%rho_bdy_Te = 0.0_rspec
        CALL ncerr_name(2,status,"rho_bdy_Te")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"rho_bdy_Ti",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"rho_bdy_Ti")
  ELSE
     status = nf90_get_var(icdf,varid,ss%rho_bdy_Ti)
     IF(status.NE.NF90_NOERR) THEN
        ss%rho_bdy_Ti = 0.0_rspec
        CALL ncerr_name(2,status,"rho_bdy_Ti")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_ecrf)) THEN
     status = nf90_inq_varid(icdf,"rho_ecrf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_ecrf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_ecrf)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_ecrf = 0.0_rspec
           CALL ncerr_name(2,status,"rho_ecrf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_eq)) THEN
     status = nf90_inq_varid(icdf,"rho_eq",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_eq")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_eq)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_eq = 0.0_rspec
           CALL ncerr_name(2,status,"rho_eq")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_eq_geo)) THEN
     status = nf90_inq_varid(icdf,"rho_eq_geo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_eq_geo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_eq_geo)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_eq_geo = 0.0_rspec
           CALL ncerr_name(2,status,"rho_eq_geo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_fus)) THEN
     status = nf90_inq_varid(icdf,"rho_fus",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_fus")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_fus)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_fus = 0.0_rspec
           CALL ncerr_name(2,status,"rho_fus")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_gas)) THEN
     status = nf90_inq_varid(icdf,"rho_gas",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_gas")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_gas)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_gas = 0.0_rspec
           CALL ncerr_name(2,status,"rho_gas")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_icrf)) THEN
     status = nf90_inq_varid(icdf,"rho_icrf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_icrf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_icrf)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_icrf = 0.0_rspec
           CALL ncerr_name(2,status,"rho_icrf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_lhrf)) THEN
     status = nf90_inq_varid(icdf,"rho_lhrf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_lhrf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_lhrf)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_lhrf = 0.0_rspec
           CALL ncerr_name(2,status,"rho_lhrf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_lmhd)) THEN
     status = nf90_inq_varid(icdf,"rho_lmhd",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_lmhd")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_lmhd)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_lmhd = 0.0_rspec
           CALL ncerr_name(2,status,"rho_lmhd")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_mist)) THEN
     status = nf90_inq_varid(icdf,"rho_mist",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_mist")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_mist)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_mist = 0.0_rspec
           CALL ncerr_name(2,status,"rho_mist")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%rho_nbi)) THEN
     status = nf90_inq_varid(icdf,"rho_nbi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"rho_nbi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%rho_nbi)
        IF(status.NE.NF90_NOERR) THEN
           ss%rho_nbi = 0.0_rspec
           CALL ncerr_name(2,status,"rho_nbi")
        ENDIF
     ENDIF
  ENDIF

  CONTAINS
     SUBROUTINE denull_vec(vstr,isize)
        INTEGER, intent(in) :: isize
        CHARACTER*(*) :: vstr(isize)
        INTEGER :: i

        do i=1,isize
           call denull(vstr(i))
        enddo
     END SUBROUTINE denull_vec

     SUBROUTINE denull(str)
        ! convert null characters to blank
        CHARACTER*(*) :: str
        INTEGER :: i

        do i=1,len(str)
           if(ichar(str(i:i)).eq.0) str(i:i)=' '
        enddo
     END SUBROUTINE denull

     SUBROUTINE ncerr_name(itype,status,name)
        INTEGER, INTENT(in) :: itype
        INTEGER, INTENT(in) :: status
        CHARACTER*(*), INTENT(in) :: name

        if(itype.eq.1) then
           write(iout,*) ' %plasma_state_get: warning: variable name = "'// &
                  trim(name)//'":'
        else
           write(iout,*) ' ?plasma_state_get: error: variable name = "'// &
                  trim(name)//'":'
           if(version_change) then
              write(iout,*) '   => version change RECOVERY: variable reset to initial value.'
           else
              iertot = iertot + 1
           endif
        endif
        call ncerr(status)
     END SUBROUTINE ncerr_name

     SUBROUTINE ncerr(status)
        INTEGER, INTENT(in) :: status

        nc_errmsg = NF90_strerror(status)
        write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
     END SUBROUTINE ncerr

END SUBROUTINE plasma_state_get_6
