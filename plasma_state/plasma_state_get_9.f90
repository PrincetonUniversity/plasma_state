! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_get_9(ss,icdf,version_change,iout,iertot)


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

  IF(ALLOCATED(ss%velpphi_tglf)) THEN
     status = nf90_inq_varid(icdf,"velpphi_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velpphi_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velpphi_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%velpphi_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"velpphi_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_anom)) THEN
     status = nf90_inq_varid(icdf,"velte_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velte_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velte_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%velte_anom = 0.0_rspec
           CALL ncerr_name(2,status,"velte_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_glf)) THEN
     status = nf90_inq_varid(icdf,"velte_glf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velte_glf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velte_glf)
        IF(status.NE.NF90_NOERR) THEN
           ss%velte_glf = 0.0_rspec
           CALL ncerr_name(2,status,"velte_glf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"velte_gtcneo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velte_gtcneo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velte_gtcneo)
        IF(status.NE.NF90_NOERR) THEN
           ss%velte_gtcneo = 0.0_rspec
           CALL ncerr_name(2,status,"velte_gtcneo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_mmm71)) THEN
     status = nf90_inq_varid(icdf,"velte_mmm71",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velte_mmm71")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velte_mmm71)
        IF(status.NE.NF90_NOERR) THEN
           ss%velte_mmm71 = 0.0_rspec
           CALL ncerr_name(2,status,"velte_mmm71")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_nclass)) THEN
     status = nf90_inq_varid(icdf,"velte_nclass",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velte_nclass")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velte_nclass)
        IF(status.NE.NF90_NOERR) THEN
           ss%velte_nclass = 0.0_rspec
           CALL ncerr_name(2,status,"velte_nclass")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_neo)) THEN
     status = nf90_inq_varid(icdf,"velte_neo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velte_neo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velte_neo)
        IF(status.NE.NF90_NOERR) THEN
           ss%velte_neo = 0.0_rspec
           CALL ncerr_name(2,status,"velte_neo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_paleo)) THEN
     status = nf90_inq_varid(icdf,"velte_paleo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velte_paleo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velte_paleo)
        IF(status.NE.NF90_NOERR) THEN
           ss%velte_paleo = 0.0_rspec
           CALL ncerr_name(2,status,"velte_paleo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velte_tglf)) THEN
     status = nf90_inq_varid(icdf,"velte_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velte_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velte_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%velte_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"velte_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_anom)) THEN
     status = nf90_inq_varid(icdf,"velti_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velti_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velti_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%velti_anom = 0.0_rspec
           CALL ncerr_name(2,status,"velti_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_glf)) THEN
     status = nf90_inq_varid(icdf,"velti_glf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velti_glf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velti_glf)
        IF(status.NE.NF90_NOERR) THEN
           ss%velti_glf = 0.0_rspec
           CALL ncerr_name(2,status,"velti_glf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"velti_gtcneo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velti_gtcneo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velti_gtcneo)
        IF(status.NE.NF90_NOERR) THEN
           ss%velti_gtcneo = 0.0_rspec
           CALL ncerr_name(2,status,"velti_gtcneo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_mmm71)) THEN
     status = nf90_inq_varid(icdf,"velti_mmm71",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velti_mmm71")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velti_mmm71)
        IF(status.NE.NF90_NOERR) THEN
           ss%velti_mmm71 = 0.0_rspec
           CALL ncerr_name(2,status,"velti_mmm71")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_nclass)) THEN
     status = nf90_inq_varid(icdf,"velti_nclass",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velti_nclass")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velti_nclass)
        IF(status.NE.NF90_NOERR) THEN
           ss%velti_nclass = 0.0_rspec
           CALL ncerr_name(2,status,"velti_nclass")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_neo)) THEN
     status = nf90_inq_varid(icdf,"velti_neo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velti_neo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velti_neo)
        IF(status.NE.NF90_NOERR) THEN
           ss%velti_neo = 0.0_rspec
           CALL ncerr_name(2,status,"velti_neo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velti_tglf)) THEN
     status = nf90_inq_varid(icdf,"velti_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velti_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velti_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%velti_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"velti_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vie_trans)) THEN
     status = nf90_inq_varid(icdf,"vie_trans",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vie_trans")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vie_trans)
        IF(status.NE.NF90_NOERR) THEN
           ss%vie_trans = 0.0_rspec
           CALL ncerr_name(2,status,"vie_trans")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vie_transcat)) THEN
     status = nf90_inq_varid(icdf,"vie_transcat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vie_transcat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vie_transcat)
        IF(status.NE.NF90_NOERR) THEN
           ss%vie_transcat = 0.0_rspec
           CALL ncerr_name(2,status,"vie_transcat")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vmo_trans)) THEN
     status = nf90_inq_varid(icdf,"vmo_trans",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vmo_trans")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vmo_trans)
        IF(status.NE.NF90_NOERR) THEN
           ss%vmo_trans = 0.0_rspec
           CALL ncerr_name(2,status,"vmo_trans")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vmo_transcat)) THEN
     status = nf90_inq_varid(icdf,"vmo_transcat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vmo_transcat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vmo_transcat)
        IF(status.NE.NF90_NOERR) THEN
           ss%vmo_transcat = 0.0_rspec
           CALL ncerr_name(2,status,"vmo_transcat")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vn_trans)) THEN
     status = nf90_inq_varid(icdf,"vn_trans",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vn_trans")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vn_trans)
        IF(status.NE.NF90_NOERR) THEN
           ss%vn_trans = 0.0_rspec
           CALL ncerr_name(2,status,"vn_trans")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vn_transcat)) THEN
     status = nf90_inq_varid(icdf,"vn_transcat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vn_transcat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vn_transcat)
        IF(status.NE.NF90_NOERR) THEN
           ss%vn_transcat = 0.0_rspec
           CALL ncerr_name(2,status,"vn_transcat")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vol)) THEN
     status = nf90_inq_varid(icdf,"vol",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vol")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vol)
        IF(status.NE.NF90_NOERR) THEN
           ss%vol = 0.0_rspec
           CALL ncerr_name(2,status,"vol")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vphi0_av)) THEN
     status = nf90_inq_varid(icdf,"vphi0_av",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vphi0_av")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vphi0_av)
        IF(status.NE.NF90_NOERR) THEN
           ss%vphi0_av = 0.0_rspec
           CALL ncerr_name(2,status,"vphi0_av")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vpol_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"vpol_Data_Info")
  ELSE
     ilen = len(ss%vpol_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%vpol_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%vpol_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%vpol_Data_Info = " "
        CALL ncerr_name(2,status,"vpol_Data_Info")
     ELSE
        CALL denull(ss%vpol_Data_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vpol_inmp)) THEN
     status = nf90_inq_varid(icdf,"vpol_inmp",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vpol_inmp")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vpol_inmp)
        IF(status.NE.NF90_NOERR) THEN
           ss%vpol_inmp = 0.0_rspec
           CALL ncerr_name(2,status,"vpol_inmp")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vpol_is_input)) THEN
     status = nf90_inq_varid(icdf,"vpol_is_input",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vpol_is_input")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vpol_is_input)
        IF(status.NE.NF90_NOERR) THEN
           ss%vpol_is_input = 0
           CALL ncerr_name(2,status,"vpol_is_input")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vpol_omp)) THEN
     status = nf90_inq_varid(icdf,"vpol_omp",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vpol_omp")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vpol_omp)
        IF(status.NE.NF90_NOERR) THEN
           ss%vpol_omp = 0.0_rspec
           CALL ncerr_name(2,status,"vpol_omp")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%VRep_coil_count)) THEN
     status = nf90_inq_varid(icdf,"VRep_coil_count",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"VRep_coil_count")
     ELSE
        status = nf90_get_var(icdf,varid,ss%VRep_coil_count)
        IF(status.NE.NF90_NOERR) THEN
           ss%VRep_coil_count = 0
           CALL ncerr_name(2,status,"VRep_coil_count")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Vrep_coil_spacing)) THEN
     status = nf90_inq_varid(icdf,"Vrep_coil_spacing",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Vrep_coil_spacing")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Vrep_coil_spacing)
        IF(status.NE.NF90_NOERR) THEN
           ss%Vrep_coil_spacing = 0.0_rspec
           CALL ncerr_name(2,status,"Vrep_coil_spacing")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Vsize_coil)) THEN
     status = nf90_inq_varid(icdf,"Vsize_coil",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Vsize_coil")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Vsize_coil)
        IF(status.NE.NF90_NOERR) THEN
           ss%Vsize_coil = 0.0_rspec
           CALL ncerr_name(2,status,"Vsize_coil")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vsur",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"vsur")
  ELSE
     status = nf90_get_var(icdf,varid,ss%vsur)
     IF(status.NE.NF90_NOERR) THEN
        ss%vsur = 0.0_rspec
        CALL ncerr_name(2,status,"vsur")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"VSUR_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"VSUR_Data_Info")
  ELSE
     ilen = len(ss%VSUR_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%VSUR_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%VSUR_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%VSUR_Data_Info = " "
        CALL ncerr_name(2,status,"VSUR_Data_Info")
     ELSE
        CALL denull(ss%VSUR_Data_Info)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vtor_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"vtor_Data_Info")
  ELSE
     ilen = len(ss%vtor_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%vtor_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%vtor_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%vtor_Data_Info = " "
        CALL ncerr_name(2,status,"vtor_Data_Info")
     ELSE
        CALL denull(ss%vtor_Data_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vtor_inmp)) THEN
     status = nf90_inq_varid(icdf,"vtor_inmp",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vtor_inmp")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vtor_inmp)
        IF(status.NE.NF90_NOERR) THEN
           ss%vtor_inmp = 0.0_rspec
           CALL ncerr_name(2,status,"vtor_inmp")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vtor_is_input)) THEN
     status = nf90_inq_varid(icdf,"vtor_is_input",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vtor_is_input")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vtor_is_input)
        IF(status.NE.NF90_NOERR) THEN
           ss%vtor_is_input = 0
           CALL ncerr_name(2,status,"vtor_is_input")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vtor_omp)) THEN
     status = nf90_inq_varid(icdf,"vtor_omp",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vtor_omp")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vtor_omp)
        IF(status.NE.NF90_NOERR) THEN
           ss%vtor_omp = 0.0_rspec
           CALL ncerr_name(2,status,"vtor_omp")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%V_loop)) THEN
     status = nf90_inq_varid(icdf,"V_loop",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"V_loop")
     ELSE
        status = nf90_get_var(icdf,varid,ss%V_loop)
        IF(status.NE.NF90_NOERR) THEN
           ss%V_loop = 0.0_rspec
           CALL ncerr_name(2,status,"V_loop")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%v_pars)) THEN
     status = nf90_inq_varid(icdf,"v_pars",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"v_pars")
     ELSE
        status = nf90_get_var(icdf,varid,ss%v_pars)
        IF(status.NE.NF90_NOERR) THEN
           ss%v_pars = 0.0_rspec
           CALL ncerr_name(2,status,"v_pars")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%v_pers)) THEN
     status = nf90_inq_varid(icdf,"v_pers",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"v_pers")
     ELSE
        status = nf90_get_var(icdf,varid,ss%v_pers)
        IF(status.NE.NF90_NOERR) THEN
           ss%v_pers = 0.0_rspec
           CALL ncerr_name(2,status,"v_pers")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%wt_nphi)) THEN
     status = nf90_inq_varid(icdf,"wt_nphi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"wt_nphi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%wt_nphi)
        IF(status.NE.NF90_NOERR) THEN
           ss%wt_nphi = 0.0_rspec
           CALL ncerr_name(2,status,"wt_nphi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%wt_nphi_abs)) THEN
     status = nf90_inq_varid(icdf,"wt_nphi_abs",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"wt_nphi_abs")
     ELSE
        status = nf90_get_var(icdf,varid,ss%wt_nphi_abs)
        IF(status.NE.NF90_NOERR) THEN
           ss%wt_nphi_abs = 0.0_rspec
           CALL ncerr_name(2,status,"wt_nphi_abs")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%wt_nphi_ext)) THEN
     status = nf90_inq_varid(icdf,"wt_nphi_ext",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"wt_nphi_ext")
     ELSE
        status = nf90_get_var(icdf,varid,ss%wt_nphi_ext)
        IF(status.NE.NF90_NOERR) THEN
           ss%wt_nphi_ext = 0.0_rspec
           CALL ncerr_name(2,status,"wt_nphi_ext")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%xRjcos_momeq)) THEN
     status = nf90_inq_varid(icdf,"xRjcos_momeq",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"xRjcos_momeq")
     ELSE
        status = nf90_get_var(icdf,varid,ss%xRjcos_momeq)
        IF(status.NE.NF90_NOERR) THEN
           ss%xRjcos_momeq = 0.0_rspec
           CALL ncerr_name(2,status,"xRjcos_momeq")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%xRjsin_momeq)) THEN
     status = nf90_inq_varid(icdf,"xRjsin_momeq",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"xRjsin_momeq")
     ELSE
        status = nf90_get_var(icdf,varid,ss%xRjsin_momeq)
        IF(status.NE.NF90_NOERR) THEN
           ss%xRjsin_momeq = 0.0_rspec
           CALL ncerr_name(2,status,"xRjsin_momeq")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%xs)) THEN
     status = nf90_inq_varid(icdf,"xs",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"xs")
     ELSE
        status = nf90_get_var(icdf,varid,ss%xs)
        IF(status.NE.NF90_NOERR) THEN
           ss%xs = 0.0_rspec
           CALL ncerr_name(2,status,"xs")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%xs_name)) THEN
     status = nf90_inq_varid(icdf,"xs_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"xs_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%xs_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%xs_name = " "
           CALL ncerr_name(2,status,"xs_name")
        ELSE
           CALL denull_vec(ss%xs_name,ss%nxs)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%xZjcos_momeq)) THEN
     status = nf90_inq_varid(icdf,"xZjcos_momeq",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"xZjcos_momeq")
     ELSE
        status = nf90_get_var(icdf,varid,ss%xZjcos_momeq)
        IF(status.NE.NF90_NOERR) THEN
           ss%xZjcos_momeq = 0.0_rspec
           CALL ncerr_name(2,status,"xZjcos_momeq")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%xZjsin_momeq)) THEN
     status = nf90_inq_varid(icdf,"xZjsin_momeq",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"xZjsin_momeq")
     ELSE
        status = nf90_get_var(icdf,varid,ss%xZjsin_momeq)
        IF(status.NE.NF90_NOERR) THEN
           ss%xZjsin_momeq = 0.0_rspec
           CALL ncerr_name(2,status,"xZjsin_momeq")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z0max",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"Z0max")
  ELSE
     status = nf90_get_var(icdf,varid,ss%Z0max)
     IF(status.NE.NF90_NOERR) THEN
        ss%Z0max = 2
        CALL ncerr_name(2,status,"Z0max")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z0_momeq)) THEN
     status = nf90_inq_varid(icdf,"Z0_momeq",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Z0_momeq")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Z0_momeq)
        IF(status.NE.NF90_NOERR) THEN
           ss%Z0_momeq = 0.0_rspec
           CALL ncerr_name(2,status,"Z0_momeq")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Zbap)) THEN
     status = nf90_inq_varid(icdf,"Zbap",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Zbap")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Zbap)
        IF(status.NE.NF90_NOERR) THEN
           ss%Zbap = 0.0_rspec
           CALL ncerr_name(2,status,"Zbap")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Zbsc)) THEN
     status = nf90_inq_varid(icdf,"Zbsc",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Zbsc")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Zbsc)
        IF(status.NE.NF90_NOERR) THEN
           ss%Zbsc = 0.0_rspec
           CALL ncerr_name(2,status,"Zbsc")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Zeff)) THEN
     status = nf90_inq_varid(icdf,"Zeff",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Zeff")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Zeff)
        IF(status.NE.NF90_NOERR) THEN
           ss%Zeff = 0.0_rspec
           CALL ncerr_name(2,status,"Zeff")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ZEFF_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"ZEFF_Data_Info")
  ELSE
     ilen = len(ss%ZEFF_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%ZEFF_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%ZEFF_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%ZEFF_Data_Info = " "
        CALL ncerr_name(2,status,"ZEFF_Data_Info")
     ELSE
        CALL denull(ss%ZEFF_Data_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Zeff_fi)) THEN
     status = nf90_inq_varid(icdf,"Zeff_fi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Zeff_fi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Zeff_fi)
        IF(status.NE.NF90_NOERR) THEN
           ss%Zeff_fi = 0.0_rspec
           CALL ncerr_name(2,status,"Zeff_fi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Zeff_th)) THEN
     status = nf90_inq_varid(icdf,"Zeff_th",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Zeff_th")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Zeff_th)
        IF(status.NE.NF90_NOERR) THEN
           ss%Zeff_th = 0.0_rspec
           CALL ncerr_name(2,status,"Zeff_th")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zimp1",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"Zimp1")
  ELSE
     status = nf90_get_var(icdf,varid,ss%Zimp1)
     IF(status.NE.NF90_NOERR) THEN
        ss%Zimp1 = 2
        CALL ncerr_name(2,status,"Zimp1")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zimp2",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"Zimp2")
  ELSE
     status = nf90_get_var(icdf,varid,ss%Zimp2)
     IF(status.NE.NF90_NOERR) THEN
        ss%Zimp2 = 1000
        CALL ncerr_name(2,status,"Zimp2")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%zlim)) THEN
     status = nf90_inq_varid(icdf,"zlim",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"zlim")
     ELSE
        status = nf90_get_var(icdf,varid,ss%zlim)
        IF(status.NE.NF90_NOERR) THEN
           ss%zlim = 0.0_rspec
           CALL ncerr_name(2,status,"zlim")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Zloc_coil)) THEN
     status = nf90_inq_varid(icdf,"Zloc_coil",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Zloc_coil")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Zloc_coil)
        IF(status.NE.NF90_NOERR) THEN
           ss%Zloc_coil = 0.0_rspec
           CALL ncerr_name(2,status,"Zloc_coil")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_ant)) THEN
     status = nf90_inq_varid(icdf,"Z_ant",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Z_ant")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Z_ant)
        IF(status.NE.NF90_NOERR) THEN
           ss%Z_ant = 0.0_rspec
           CALL ncerr_name(2,status,"Z_ant")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_antgeo)) THEN
     status = nf90_inq_varid(icdf,"Z_antgeo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Z_antgeo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Z_antgeo)
        IF(status.NE.NF90_NOERR) THEN
           ss%Z_antgeo = 0.0_rspec
           CALL ncerr_name(2,status,"Z_antgeo")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_axis",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"Z_axis")
  ELSE
     status = nf90_get_var(icdf,varid,ss%Z_axis)
     IF(status.NE.NF90_NOERR) THEN
        ss%Z_axis = 0.0_rspec
        CALL ncerr_name(2,status,"Z_axis")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_EC_launch)) THEN
     status = nf90_inq_varid(icdf,"Z_EC_launch",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Z_EC_launch")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Z_EC_launch)
        IF(status.NE.NF90_NOERR) THEN
           ss%Z_EC_launch = 0.0_rspec
           CALL ncerr_name(2,status,"Z_EC_launch")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_geo)) THEN
     status = nf90_inq_varid(icdf,"Z_geo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Z_geo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Z_geo)
        IF(status.NE.NF90_NOERR) THEN
           ss%Z_geo = 0.0_rspec
           CALL ncerr_name(2,status,"Z_geo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_grid)) THEN
     status = nf90_inq_varid(icdf,"Z_grid",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Z_grid")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Z_grid)
        IF(status.NE.NF90_NOERR) THEN
           ss%Z_grid = 0.0_rspec
           CALL ncerr_name(2,status,"Z_grid")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_max_box",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"Z_max_box")
  ELSE
     status = nf90_get_var(icdf,varid,ss%Z_max_box)
     IF(status.NE.NF90_NOERR) THEN
        ss%Z_max_box = 0.0_rspec
        CALL ncerr_name(2,status,"Z_max_box")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_max_lcfs",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"Z_max_lcfs")
  ELSE
     status = nf90_get_var(icdf,varid,ss%Z_max_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        ss%Z_max_lcfs = 0.0_rspec
        CALL ncerr_name(2,status,"Z_max_lcfs")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_midp)) THEN
     status = nf90_inq_varid(icdf,"Z_midp",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Z_midp")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Z_midp)
        IF(status.NE.NF90_NOERR) THEN
           ss%Z_midp = 0.0_rspec
           CALL ncerr_name(2,status,"Z_midp")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_mid_ant)) THEN
     status = nf90_inq_varid(icdf,"Z_mid_ant",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Z_mid_ant")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Z_mid_ant)
        IF(status.NE.NF90_NOERR) THEN
           ss%Z_mid_ant = 0.0_rspec
           CALL ncerr_name(2,status,"Z_mid_ant")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_min_box",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"Z_min_box")
  ELSE
     status = nf90_get_var(icdf,varid,ss%Z_min_box)
     IF(status.NE.NF90_NOERR) THEN
        ss%Z_min_box = 0.0_rspec
        CALL ncerr_name(2,status,"Z_min_box")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_min_lcfs",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"Z_min_lcfs")
  ELSE
     status = nf90_get_var(icdf,varid,ss%Z_min_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        ss%Z_min_lcfs = 0.0_rspec
        CALL ncerr_name(2,status,"Z_min_lcfs")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_ripple)) THEN
     status = nf90_inq_varid(icdf,"Z_ripple",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Z_ripple")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Z_ripple)
        IF(status.NE.NF90_NOERR) THEN
           ss%Z_ripple = 0.0_rspec
           CALL ncerr_name(2,status,"Z_ripple")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_surfMax)) THEN
     status = nf90_inq_varid(icdf,"Z_surfMax",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Z_surfMax")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Z_surfMax)
        IF(status.NE.NF90_NOERR) THEN
           ss%Z_surfMax = 0.0_rspec
           CALL ncerr_name(2,status,"Z_surfMax")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Z_surfMin)) THEN
     status = nf90_inq_varid(icdf,"Z_surfMin",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Z_surfMin")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Z_surfMin)
        IF(status.NE.NF90_NOERR) THEN
           ss%Z_surfMin = 0.0_rspec
           CALL ncerr_name(2,status,"Z_surfMin")
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

END SUBROUTINE plasma_state_get_9
