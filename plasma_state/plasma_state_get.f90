! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_get(filename,ss,ierr)

  ! get (i.e. read from file) plasma state
  ! this overwrites any prior contents that might have been in state "ss".

  USE plasma_state_definition_mod
  USE netcdf
  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN) :: filename        ! state file
  TYPE (plasma_state) :: ss     ! state data to read
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !--------------

  INTEGER :: icdf,iout,varid,dimid,status,iertot,ilen,jlen,idims(20),indim
  INTEGER :: ii,iflag_alloc2,iflag_loc2,isize
  LOGICAL :: version_change
  CHARACTER*80 :: nc_errmsg
  CHARACTER*32 :: dimname
  INTEGER ilocka(3)

  !--------------
  ! Initialization...

  iout = SWIM_err_out_file
  version_change = .FALSE.

  iertot = 0
  CALL ps_init_xpl(ss%iobj,iout,ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) ' ?plasma_state_get: "ps_init_xpl" error prior to file read.'
     iertot = iertot + 1
  ENDIF

  CALL plasma_state_erase(ss,ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) ' ?plasma_state_get: "plasma_state_erase" error prior to file read.'
     iertot = iertot + 1
  ENDIF

  IF(iertot.GT.0) THEN
     ierr=9999
     RETURN
  ENDIF

  ! ********* OPEN the NetCDF file **********

  status = nf90_open(trim(filename),NF90_NOWRITE,icdf)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?plasma_state_get: file open failure: "'//trim(filename)//'".'
     ierr=9999
     RETURN
  ENDIF

  ! ********* READ header information
  ! ...check for version_id mismatch

  status = nf90_inq_varid(icdf,"ps_partial_update",varid)
  if(status.EQ.NF90_NOERR) THEN
     WRITE(iout,*) ' ?plasma_state_get: file contains "ps_partial_update"!'
     WRITE(iout,*) '  This routine can only read full states, not partial states.'
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  status = nf90_inq_varid(icdf,"version_id",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?plasma_state_get: "version_id" not found.'
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  status = nf90_get_var(icdf,varid,ss%version_id)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?plasma_state_get: "version_id" read error.'
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF
  CALL denull(ss%version_id)

  IF(ss%version_id.NE.ps_version_id) THEN
     WRITE(iout,*) ' '
     WRITE(iout,*) ' %plasma_state_get: WARNING, version_id mismatch:'
     WRITE(iout,*) '  Version_ID of current software: "'//trim(ps_version_id)//'".'
     WRITE(iout,*) '  Version_ID found in file:       "'//trim(ss%version_id)//'".'
     WRITE(iout,*) ' '
     WRITE(iout,*) '  ...Hash code checks disabled.'
     WRITE(iout,*) '  ...Error handling modified.'
     WRITE(iout,*) ' '
     version_change = .TRUE.
     ss%hash_codes = 0   ! these cannot be used in case of version_change
  ENDIF

  IF(.not.version_change) THEN

     ! hash codes -- these are used to enforce "locks" on data sections
     ! that are meant to be time invariant after initialization...

     status = nf90_inq_varid(icdf,"hash_codes",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr(status)
        WRITE(iout,*) ' ?plasma_state_get: "hash_codes" not found.'
        ierr=9999
        status = nf90_close(icdf)
        RETURN
     ENDIF

     status = nf90_get_var(icdf,varid,ss%hash_codes)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr(status)
        WRITE(iout,*) ' ?plasma_state_get: "hash_codes" read error.'
        ierr=9999
        status = nf90_close(icdf)
        RETURN
     ENDIF
  ENDIF

  !----------------------------------------------------

  ! *** (scalar) Array Dimensions ***
  ss%active_dims = 0

  ss%max_nrz_antgeo = 0
  status = nf90_inq_dimid(icdf,"dim_max_nrz_antgeo",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%max_nrz_antgeo)
     IF(status.NE.NF90_NOERR) THEN
        ss%max_nrz_antgeo = 0
        CALL ncerr_name(2,status,"max_nrz_antgeo")
     ENDIF
  ENDIF
  IF(ss%max_nrz_antgeo.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
  ENDIF

  ss%max_nspectrumlh_src = 0
  status = nf90_inq_dimid(icdf,"dim_max_nspectrumlh_src",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%max_nspectrumlh_src)
     IF(status.NE.NF90_NOERR) THEN
        ss%max_nspectrumlh_src = 0
        CALL ncerr_name(2,status,"max_nspectrumlh_src")
     ENDIF
  ENDIF
  IF(ss%max_nspectrumlh_src.GT.0) THEN
     ss%active_dims(ps_cnum_LH) = ss%active_dims(ps_cnum_LH) + 1
  ENDIF

  ss%max_num_nphi = 0
  status = nf90_inq_dimid(icdf,"dim_max_num_nphi",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%max_num_nphi)
     IF(status.NE.NF90_NOERR) THEN
        ss%max_num_nphi = 0
        CALL ncerr_name(2,status,"max_num_nphi")
     ENDIF
  ENDIF
  IF(ss%max_num_nphi.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
  ENDIF

  ss%max_num_nphi_vac = 0
  status = nf90_inq_dimid(icdf,"dim_max_num_nphi_vac",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%max_num_nphi_vac)
     IF(status.NE.NF90_NOERR) THEN
        ss%max_num_nphi_vac = 0
        CALL ncerr_name(2,status,"max_num_nphi_vac")
     ENDIF
  ENDIF
  IF(ss%max_num_nphi_vac.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
  ENDIF

  ss%max_n_straps = 0
  status = nf90_inq_dimid(icdf,"dim_max_n_straps",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%max_n_straps)
     IF(status.NE.NF90_NOERR) THEN
        ss%max_n_straps = 0
        CALL ncerr_name(2,status,"max_n_straps")
     ENDIF
  ENDIF
  IF(ss%max_n_straps.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
  ENDIF

  ss%mmodes = 0
  status = nf90_inq_dimid(icdf,"dim_mmodes",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%mmodes)
     IF(status.NE.NF90_NOERR) THEN
        ss%mmodes = 0
        CALL ncerr_name(2,status,"mmodes")
     ENDIF
  ENDIF
  IF(ss%mmodes.GT.0) THEN
     ss%active_dims(ps_cnum_LMHD) = ss%active_dims(ps_cnum_LMHD) + 1
  ENDIF

  ss%nballoon = 0
  status = nf90_inq_dimid(icdf,"dim_nballoon",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nballoon)
     IF(status.NE.NF90_NOERR) THEN
        ss%nballoon = 0
        CALL ncerr_name(2,status,"nballoon")
     ENDIF
  ENDIF
  IF(ss%nballoon.GT.0) THEN
     ss%active_dims(ps_cnum_LMHD) = ss%active_dims(ps_cnum_LMHD) + 1
  ENDIF

  ss%nbeam = 0
  status = nf90_inq_dimid(icdf,"dim_nbeam",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nbeam)
     IF(status.NE.NF90_NOERR) THEN
        ss%nbeam = 0
        CALL ncerr_name(2,status,"nbeam")
     ENDIF
  ENDIF
  IF(ss%nbeam.GT.0) THEN
     ss%active_dims(ps_cnum_NBI) = ss%active_dims(ps_cnum_NBI) + 1
  ENDIF

  ss%ncircuits = 0
  status = nf90_inq_dimid(icdf,"dim_ncircuits",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%ncircuits)
     IF(status.NE.NF90_NOERR) THEN
        ss%ncircuits = 0
        CALL ncerr_name(2,status,"ncircuits")
     ENDIF
  ENDIF
  IF(ss%ncircuits.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
  ENDIF

  ss%ncoils = 0
  status = nf90_inq_dimid(icdf,"dim_ncoils",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%ncoils)
     IF(status.NE.NF90_NOERR) THEN
        ss%ncoils = 0
        CALL ncerr_name(2,status,"ncoils")
     ENDIF
  ENDIF
  IF(ss%ncoils.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
  ENDIF

  ss%ndelta_w = 0
  status = nf90_inq_dimid(icdf,"dim_ndelta_w",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%ndelta_w)
     IF(status.NE.NF90_NOERR) THEN
        ss%ndelta_w = 0
        CALL ncerr_name(2,status,"ndelta_w")
     ENDIF
  ENDIF
  IF(ss%ndelta_w.GT.0) THEN
     ss%active_dims(ps_cnum_LMHD) = ss%active_dims(ps_cnum_LMHD) + 1
  ENDIF

  ss%necrf_src = 0
  status = nf90_inq_dimid(icdf,"dim_necrf_src",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%necrf_src)
     IF(status.NE.NF90_NOERR) THEN
        ss%necrf_src = 0
        CALL ncerr_name(2,status,"necrf_src")
     ENDIF
  ENDIF
  IF(ss%necrf_src.GT.0) THEN
     ss%active_dims(ps_cnum_EC) = ss%active_dims(ps_cnum_EC) + 1
  ENDIF

  ss%nefi_anom = 0
  status = nf90_inq_dimid(icdf,"dim_nefi_anom",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nefi_anom)
     IF(status.NE.NF90_NOERR) THEN
        ss%nefi_anom = 0
        CALL ncerr_name(2,status,"nefi_anom")
     ENDIF
  ENDIF
  IF(ss%nefi_anom.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
  ENDIF

  ss%nefi_anom2 = 0
  status = nf90_inq_dimid(icdf,"dim_nefi_anom2",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nefi_anom2)
     IF(status.NE.NF90_NOERR) THEN
        ss%nefi_anom2 = 0
        CALL ncerr_name(2,status,"nefi_anom2")
     ENDIF
  ENDIF
  IF(ss%nefi_anom2.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
  ENDIF

  ss%neqmom = 0
  status = nf90_inq_dimid(icdf,"dim_neqmom",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%neqmom)
     IF(status.NE.NF90_NOERR) THEN
        ss%neqmom = 0
        CALL ncerr_name(2,status,"neqmom")
     ENDIF
  ENDIF
  IF(ss%neqmom.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
  ENDIF

  ss%ngsc0 = 0
  status = nf90_inq_dimid(icdf,"dim_ngsc0",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%ngsc0)
     IF(status.NE.NF90_NOERR) THEN
        ss%ngsc0 = 0
        CALL ncerr_name(2,status,"ngsc0")
     ENDIF
  ENDIF
  IF(ss%ngsc0.GT.0) THEN
     ss%active_dims(ps_cnum_GAS) = ss%active_dims(ps_cnum_GAS) + 1
  ENDIF

  ss%nicrf_src = 0
  status = nf90_inq_dimid(icdf,"dim_nicrf_src",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nicrf_src)
     IF(status.NE.NF90_NOERR) THEN
        ss%nicrf_src = 0
        CALL ncerr_name(2,status,"nicrf_src")
     ENDIF
  ENDIF
  IF(ss%nicrf_src.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
  ENDIF

  ss%nky_anomth = 0
  status = nf90_inq_dimid(icdf,"dim_nky_anomth",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nky_anomth)
     IF(status.NE.NF90_NOERR) THEN
        ss%nky_anomth = 0
        CALL ncerr_name(2,status,"nky_anomth")
     ENDIF
  ENDIF
  IF(ss%nky_anomth.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
  ENDIF

  ss%nky_mmm = 0
  status = nf90_inq_dimid(icdf,"dim_nky_mmm",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nky_mmm)
     IF(status.NE.NF90_NOERR) THEN
        ss%nky_mmm = 0
        CALL ncerr_name(2,status,"nky_mmm")
     ENDIF
  ENDIF
  IF(ss%nky_mmm.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
  ENDIF

  ss%nlhrf_src = 0
  status = nf90_inq_dimid(icdf,"dim_nlhrf_src",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nlhrf_src)
     IF(status.NE.NF90_NOERR) THEN
        ss%nlhrf_src = 0
        CALL ncerr_name(2,status,"nlhrf_src")
     ENDIF
  ENDIF
  IF(ss%nlhrf_src.GT.0) THEN
     ss%active_dims(ps_cnum_LH) = ss%active_dims(ps_cnum_LH) + 1
  ENDIF

  ss%nnewcomb = 0
  status = nf90_inq_dimid(icdf,"dim_nnewcomb",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nnewcomb)
     IF(status.NE.NF90_NOERR) THEN
        ss%nnewcomb = 0
        CALL ncerr_name(2,status,"nnewcomb")
     ENDIF
  ENDIF
  IF(ss%nnewcomb.GT.0) THEN
     ss%active_dims(ps_cnum_LMHD) = ss%active_dims(ps_cnum_LMHD) + 1
  ENDIF

  ss%npsmom = 0
  status = nf90_inq_dimid(icdf,"dim_npsmom",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%npsmom)
     IF(status.NE.NF90_NOERR) THEN
        ss%npsmom = 0
        CALL ncerr_name(2,status,"npsmom")
     ENDIF
  ENDIF
  IF(ss%npsmom.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
  ENDIF

  ss%nr = 0
  status = nf90_inq_dimid(icdf,"dim_nr",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nr)
     IF(status.NE.NF90_NOERR) THEN
        ss%nr = 0
        CALL ncerr_name(2,status,"nr")
     ENDIF
  ENDIF
  IF(ss%nr.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
  ENDIF

  ss%nrho = 0
  status = nf90_inq_dimid(icdf,"dim_nrho",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho = 0
        CALL ncerr_name(2,status,"nrho")
     ENDIF
  ENDIF
  IF(ss%nrho.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
  ENDIF

  ss%nrho_anom = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_anom",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_anom)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_anom = 0
        CALL ncerr_name(2,status,"nrho_anom")
     ENDIF
  ENDIF
  IF(ss%nrho_anom.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
  ENDIF

  ss%nrho_anom2 = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_anom2",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_anom2)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_anom2 = 0
        CALL ncerr_name(2,status,"nrho_anom2")
     ENDIF
  ENDIF
  IF(ss%nrho_anom2.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
  ENDIF

  ss%nrho_anomth = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_anomth",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_anomth)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_anomth = 0
        CALL ncerr_name(2,status,"nrho_anomth")
     ENDIF
  ENDIF
  IF(ss%nrho_anomth.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
  ENDIF

  ss%nrho_ecrf = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_ecrf",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_ecrf)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_ecrf = 0
        CALL ncerr_name(2,status,"nrho_ecrf")
     ENDIF
  ENDIF
  IF(ss%nrho_ecrf.GT.0) THEN
     ss%active_dims(ps_cnum_EC) = ss%active_dims(ps_cnum_EC) + 1
  ENDIF

  ss%nrho_eq = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_eq",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_eq)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_eq = 0
        CALL ncerr_name(2,status,"nrho_eq")
     ENDIF
  ENDIF
  IF(ss%nrho_eq.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
  ENDIF

  ss%nrho_eq_geo = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_eq_geo)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_eq_geo = 0
        CALL ncerr_name(2,status,"nrho_eq_geo")
     ENDIF
  ENDIF
  IF(ss%nrho_eq_geo.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
  ENDIF

  ss%nrho_fus = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_fus",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_fus)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_fus = 0
        CALL ncerr_name(2,status,"nrho_fus")
     ENDIF
  ENDIF
  IF(ss%nrho_fus.GT.0) THEN
     ss%active_dims(ps_cnum_FUS) = ss%active_dims(ps_cnum_FUS) + 1
  ENDIF

  ss%nrho_gas = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_gas",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_gas)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_gas = 0
        CALL ncerr_name(2,status,"nrho_gas")
     ENDIF
  ENDIF
  IF(ss%nrho_gas.GT.0) THEN
     ss%active_dims(ps_cnum_GAS) = ss%active_dims(ps_cnum_GAS) + 1
  ENDIF

  ss%nrho_icrf = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_icrf",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_icrf)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_icrf = 0
        CALL ncerr_name(2,status,"nrho_icrf")
     ENDIF
  ENDIF
  IF(ss%nrho_icrf.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
  ENDIF

  ss%nrho_lhrf = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_lhrf",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_lhrf)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_lhrf = 0
        CALL ncerr_name(2,status,"nrho_lhrf")
     ENDIF
  ENDIF
  IF(ss%nrho_lhrf.GT.0) THEN
     ss%active_dims(ps_cnum_LH) = ss%active_dims(ps_cnum_LH) + 1
  ENDIF

  ss%nrho_lmhd = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_lmhd",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_lmhd)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_lmhd = 0
        CALL ncerr_name(2,status,"nrho_lmhd")
     ENDIF
  ENDIF
  IF(ss%nrho_lmhd.GT.0) THEN
     ss%active_dims(ps_cnum_LMHD) = ss%active_dims(ps_cnum_LMHD) + 1
  ENDIF

  ss%nrho_mist = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_mist",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_mist)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_mist = 0
        CALL ncerr_name(2,status,"nrho_mist")
     ENDIF
  ENDIF
  IF(ss%nrho_mist.GT.0) THEN
     ss%active_dims(ps_cnum_RAD) = ss%active_dims(ps_cnum_RAD) + 1
  ENDIF

  ss%nrho_nbi = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_nbi",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_nbi)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_nbi = 0
        CALL ncerr_name(2,status,"nrho_nbi")
     ENDIF
  ENDIF
  IF(ss%nrho_nbi.GT.0) THEN
     ss%active_dims(ps_cnum_NBI) = ss%active_dims(ps_cnum_NBI) + 1
  ENDIF

  ss%nrho_pel = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_pel",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_pel)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_pel = 0
        CALL ncerr_name(2,status,"nrho_pel")
     ENDIF
  ENDIF
  IF(ss%nrho_pel.GT.0) THEN
     ss%active_dims(ps_cnum_PEL) = ss%active_dims(ps_cnum_PEL) + 1
  ENDIF

  ss%nrho_rad = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_rad",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_rad)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_rad = 0
        CALL ncerr_name(2,status,"nrho_rad")
     ENDIF
  ENDIF
  IF(ss%nrho_rad.GT.0) THEN
     ss%active_dims(ps_cnum_RAD) = ss%active_dims(ps_cnum_RAD) + 1
  ENDIF

  ss%nrho_rw = 0
  status = nf90_inq_dimid(icdf,"dim_nrho_rw",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nrho_rw)
     IF(status.NE.NF90_NOERR) THEN
        ss%nrho_rw = 0
        CALL ncerr_name(2,status,"nrho_rw")
     ENDIF
  ENDIF
  IF(ss%nrho_rw.GT.0) THEN
     ss%active_dims(ps_cnum_RUNAWAY) = ss%active_dims(ps_cnum_RUNAWAY) + 1
  ENDIF

  ss%nr_rip = 0
  status = nf90_inq_dimid(icdf,"dim_nr_rip",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nr_rip)
     IF(status.NE.NF90_NOERR) THEN
        ss%nr_rip = 0
        CALL ncerr_name(2,status,"nr_rip")
     ENDIF
  ENDIF
  IF(ss%nr_rip.GT.0) THEN
     ss%active_dims(ps_cnum_RIPPLE) = ss%active_dims(ps_cnum_RIPPLE) + 1
  ENDIF

  ss%nspec_all = 0
  status = nf90_inq_dimid(icdf,"dim_nspec_all",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nspec_all)
     IF(status.NE.NF90_NOERR) THEN
        ss%nspec_all = 0
        CALL ncerr_name(2,status,"nspec_all")
     ENDIF
  ENDIF
  IF(ss%nspec_all.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
  ENDIF

  ss%nspec_alla = 0
  status = nf90_inq_dimid(icdf,"dim_nspec_alla",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nspec_alla)
     IF(status.NE.NF90_NOERR) THEN
        ss%nspec_alla = 0
        CALL ncerr_name(2,status,"nspec_alla")
     ENDIF
  ENDIF
  IF(ss%nspec_alla.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
  ENDIF

  ss%nspec_beam = 0
  status = nf90_inq_dimid(icdf,"dim_nspec_beam",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nspec_beam)
     IF(status.NE.NF90_NOERR) THEN
        ss%nspec_beam = 0
        CALL ncerr_name(2,status,"nspec_beam")
     ENDIF
  ENDIF
  IF(ss%nspec_beam.GT.0) THEN
     ss%active_dims(ps_cnum_NBI) = ss%active_dims(ps_cnum_NBI) + 1
  ENDIF

  ss%nspec_fusion = 0
  status = nf90_inq_dimid(icdf,"dim_nspec_fusion",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nspec_fusion)
     IF(status.NE.NF90_NOERR) THEN
        ss%nspec_fusion = 0
        CALL ncerr_name(2,status,"nspec_fusion")
     ENDIF
  ENDIF
  IF(ss%nspec_fusion.GT.0) THEN
     ss%active_dims(ps_cnum_FUS) = ss%active_dims(ps_cnum_FUS) + 1
  ENDIF

  ss%nspec_gas = 0
  status = nf90_inq_dimid(icdf,"dim_nspec_gas",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nspec_gas)
     IF(status.NE.NF90_NOERR) THEN
        ss%nspec_gas = 0
        CALL ncerr_name(2,status,"nspec_gas")
     ENDIF
  ENDIF
  IF(ss%nspec_gas.GT.0) THEN
     ss%active_dims(ps_cnum_GAS) = ss%active_dims(ps_cnum_GAS) + 1
  ENDIF

  ss%nspec_imp0 = 0
  status = nf90_inq_dimid(icdf,"dim_nspec_imp0",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nspec_imp0)
     IF(status.NE.NF90_NOERR) THEN
        ss%nspec_imp0 = 0
        CALL ncerr_name(2,status,"nspec_imp0")
     ENDIF
  ENDIF
  IF(ss%nspec_imp0.GT.0) THEN
     ss%active_dims(ps_cnum_GAS) = ss%active_dims(ps_cnum_GAS) + 1
  ENDIF

  ss%nspec_impi = 0
  status = nf90_inq_dimid(icdf,"dim_nspec_impi",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nspec_impi)
     IF(status.NE.NF90_NOERR) THEN
        ss%nspec_impi = 0
        CALL ncerr_name(2,status,"nspec_impi")
     ENDIF
  ENDIF
  IF(ss%nspec_impi.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
  ENDIF

  ss%nspec_rfmin = 0
  status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nspec_rfmin)
     IF(status.NE.NF90_NOERR) THEN
        ss%nspec_rfmin = 0
        CALL ncerr_name(2,status,"nspec_rfmin")
     ENDIF
  ENDIF
  IF(ss%nspec_rfmin.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
  ENDIF

  ss%nspec_th = 0
  status = nf90_inq_dimid(icdf,"dim_nspec_th",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nspec_th)
     IF(status.NE.NF90_NOERR) THEN
        ss%nspec_th = 0
        CALL ncerr_name(2,status,"nspec_th")
     ENDIF
  ENDIF
  IF(ss%nspec_th.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
  ENDIF

  ss%nspec_tha = 0
  status = nf90_inq_dimid(icdf,"dim_nspec_tha",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nspec_tha)
     IF(status.NE.NF90_NOERR) THEN
        ss%nspec_tha = 0
        CALL ncerr_name(2,status,"nspec_tha")
     ENDIF
  ENDIF
  IF(ss%nspec_tha.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
  ENDIF

  ss%nth_eq = 0
  status = nf90_inq_dimid(icdf,"dim_nth_eq",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nth_eq)
     IF(status.NE.NF90_NOERR) THEN
        ss%nth_eq = 0
        CALL ncerr_name(2,status,"nth_eq")
     ENDIF
  ENDIF
  IF(ss%nth_eq.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
  ENDIF

  ss%ntrcat = 0
  status = nf90_inq_dimid(icdf,"dim_ntrcat",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%ntrcat)
     IF(status.NE.NF90_NOERR) THEN
        ss%ntrcat = 0
        CALL ncerr_name(2,status,"ntrcat")
     ENDIF
  ENDIF
  IF(ss%ntrcat.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
  ENDIF

  ss%num_rzlim = 0
  status = nf90_inq_dimid(icdf,"dim_num_rzlim",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%num_rzlim)
     IF(status.NE.NF90_NOERR) THEN
        ss%num_rzlim = 0
        CALL ncerr_name(2,status,"num_rzlim")
     ENDIF
  ENDIF
  IF(ss%num_rzlim.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
  ENDIF

  ss%nxs = 0
  status = nf90_inq_dimid(icdf,"dim_nxs",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nxs)
     IF(status.NE.NF90_NOERR) THEN
        ss%nxs = 0
        CALL ncerr_name(2,status,"nxs")
     ENDIF
  ENDIF
  IF(ss%nxs.GT.0) THEN
     ss%active_dims(ps_cnum_RAD) = ss%active_dims(ps_cnum_RAD) + 1
  ENDIF

  ss%nz = 0
  status = nf90_inq_dimid(icdf,"dim_nz",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nz)
     IF(status.NE.NF90_NOERR) THEN
        ss%nz = 0
        CALL ncerr_name(2,status,"nz")
     ENDIF
  ENDIF
  IF(ss%nz.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
  ENDIF

  ss%nz_rip = 0
  status = nf90_inq_dimid(icdf,"dim_nz_rip",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%nz_rip)
     IF(status.NE.NF90_NOERR) THEN
        ss%nz_rip = 0
        CALL ncerr_name(2,status,"nz_rip")
     ENDIF
  ENDIF
  IF(ss%nz_rip.GT.0) THEN
     ss%active_dims(ps_cnum_RIPPLE) = ss%active_dims(ps_cnum_RIPPLE) + 1
  ENDIF

  ss%n_D_einj_standard = 0
  status = nf90_inq_dimid(icdf,"dim_n_d_einj_standard",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%n_D_einj_standard)
     IF(status.NE.NF90_NOERR) THEN
        ss%n_D_einj_standard = 0
        CALL ncerr_name(2,status,"n_D_einj_standard")
     ENDIF
  ENDIF
  IF(ss%n_D_einj_standard.GT.0) THEN
     ss%active_dims(ps_cnum_NBI) = ss%active_dims(ps_cnum_NBI) + 1
  ENDIF

  ss%n_H_einj_standard = 0
  status = nf90_inq_dimid(icdf,"dim_n_h_einj_standard",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%n_H_einj_standard)
     IF(status.NE.NF90_NOERR) THEN
        ss%n_H_einj_standard = 0
        CALL ncerr_name(2,status,"n_H_einj_standard")
     ENDIF
  ENDIF
  IF(ss%n_H_einj_standard.GT.0) THEN
     ss%active_dims(ps_cnum_NBI) = ss%active_dims(ps_cnum_NBI) + 1
  ENDIF

  ss%n_T_einj_standard = 0
  status = nf90_inq_dimid(icdf,"dim_n_t_einj_standard",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ss%n_T_einj_standard)
     IF(status.NE.NF90_NOERR) THEN
        ss%n_T_einj_standard = 0
        CALL ncerr_name(2,status,"n_T_einj_standard")
     ENDIF
  ENDIF
  IF(ss%n_T_einj_standard.GT.0) THEN
     ss%active_dims(ps_cnum_NBI) = ss%active_dims(ps_cnum_NBI) + 1
  ENDIF

  IF(iertot.NE.0) THEN
     WRITE(iout,*) " ?plasma_state_get: there were errors reading dimensions..."
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  ! DIMENSIONS read in OK; allocate state arrays...

  CALL plasma_state_empty_alloc(ss,ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) " ?plasma_state_get: plasma_state_empty_alloc error."
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  !----------------------------------------
  ! call subroutines to read variables and arrays

  call plasma_state_get_0(ss,icdf,version_change,iout,iertot)
  call plasma_state_get_1(ss,icdf,version_change,iout,iertot)
  call plasma_state_get_2(ss,icdf,version_change,iout,iertot)
  call plasma_state_get_3(ss,icdf,version_change,iout,iertot)
  call plasma_state_get_4(ss,icdf,version_change,iout,iertot)
  call plasma_state_get_5(ss,icdf,version_change,iout,iertot)
  call plasma_state_get_6(ss,icdf,version_change,iout,iertot)
  call plasma_state_get_7(ss,icdf,version_change,iout,iertot)
  call plasma_state_get_8(ss,icdf,version_change,iout,iertot)
  call plasma_state_get_9(ss,icdf,version_change,iout,iertot)

  !----------------------------------------
  ! Close the NetCDF file

  status = nf90_close(icdf)

  IF(iertot.NE.0) THEN
     WRITE(iout,*) " ?plasma_state_get: there were errors reading data..."
     ierr=9999
     RETURN
  ENDIF

  !----------------------------------------
  ! Store in xplasma; check equilibrium & hash codes

  IF(version_change) THEN
     ilocka(1) = ss%lock_machine_descr
     ss%lock_machine_descr = ps_unlocked
     ilocka(2) = ss%lock_shot_config
     ss%lock_shot_config = ps_unlocked
     ilocka(3) = ss%lock_sim_init
     ss%lock_sim_init = ps_unlocked
  ENDIF

  CALL plasma_state_store(" ",ss,1,ierr)

  IF(version_change) THEN
     ss%lock_machine_descr = ilocka(1)
     ss%lock_shot_config = ilocka(2)
     ss%lock_sim_init = ilocka(3)
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

END SUBROUTINE plasma_state_get
