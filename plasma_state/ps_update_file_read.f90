! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_read(filename,ss,ierr)

  ! get (i.e. read from file) plasma state updates
  ! apply updates to state (ss)

  USE plasma_state_definition_mod
  USE plasma_hash_code
  USE netcdf
  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN) :: filename        ! file containing updates
  TYPE (plasma_state) :: ss     ! state to be updated
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !--------------

  INTEGER :: icdf,iout,varid,dimid,status,iertot
  INTEGER :: ival,jval,icount,iucount,ii
  INTEGER :: prehash1,prehash2
  INTEGER :: posthash1,posthash2
  CHARACTER*80 :: nc_errmsg
  LOGICAL :: izero

  !--------------
  ! Initialization...

  iout = SWIM_err_out_file

  iertot = 0

  iucount = 0

  ! ********* OPEN the NetCDF file **********

  status = nf90_open(trim(filename),NF90_NOWRITE,icdf)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?ps_update_file_read: file open failure: "'//trim(filename)//'".'
     ierr=9999
     RETURN
  ENDIF

  ! ********* READ header information
  ! ...check for version_id mismatch

  status = nf90_inq_varid(icdf,"ps_partial_update",varid)
  if(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) ' ?ps_update_file_read: did not find: "ps_partial_update"!'
     WRITE(iout,*) '  This routine can only read state update files, which'
     WRITE(iout,*) '  must contain this marker variable.'
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  status = nf90_inq_varid(icdf,"version_id",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?ps_update_file_read: "version_id" not found.'
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  status = nf90_get_var(icdf,varid,ss%version_id)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?ps_update_file_read: "version_id" read error.'
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF
  CALL denull(ss%version_id)

  IF(ss%version_id.NE.ps_version_id) THEN
     WRITE(iout,*) ' %ps_update_file_read: warning: version_id mismatch:'
     WRITE(iout,*) '  Version_ID of current software: "'//trim(ps_version_id)//'".'
     WRITE(iout,*) '  Version_ID found in file:       "'//trim(ss%version_id)//'".'
     WRITE(iout,*) '  (...recovery attempt in progress...)'
  ENDIF

  !--------------------------------------------------------------

  ! *** Array Dimensions ***
  icount = 0
  iertot = 0

  status = nf90_inq_dimid(icdf,"dim_max_nrz_antgeo",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%max_nrz_antgeo.EQ.0) THEN
        ss%max_nrz_antgeo = ival
        icount = icount + 1
     ENDIF
     jval = ss%max_nrz_antgeo
     CALL ichk_dim(ival,jval,"max_nrz_antgeo")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_max_nspectrumlh_src",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%max_nspectrumlh_src.EQ.0) THEN
        ss%max_nspectrumlh_src = ival
        icount = icount + 1
     ENDIF
     jval = ss%max_nspectrumlh_src
     CALL ichk_dim(ival,jval,"max_nspectrumlh_src")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_max_num_nphi",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%max_num_nphi.EQ.0) THEN
        ss%max_num_nphi = ival
        icount = icount + 1
     ENDIF
     jval = ss%max_num_nphi
     CALL ichk_dim(ival,jval,"max_num_nphi")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_max_num_nphi_vac",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%max_num_nphi_vac.EQ.0) THEN
        ss%max_num_nphi_vac = ival
        icount = icount + 1
     ENDIF
     jval = ss%max_num_nphi_vac
     CALL ichk_dim(ival,jval,"max_num_nphi_vac")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_max_n_straps",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%max_n_straps.EQ.0) THEN
        ss%max_n_straps = ival
        icount = icount + 1
     ENDIF
     jval = ss%max_n_straps
     CALL ichk_dim(ival,jval,"max_n_straps")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_mmodes",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%mmodes.EQ.0) THEN
        ss%mmodes = ival
        icount = icount + 1
     ENDIF
     jval = ss%mmodes
     CALL ichk_dim(ival,jval,"mmodes")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nballoon",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nballoon.EQ.0) THEN
        ss%nballoon = ival
        icount = icount + 1
     ENDIF
     jval = ss%nballoon
     CALL ichk_dim(ival,jval,"nballoon")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nbeam",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nbeam.EQ.0) THEN
        ss%nbeam = ival
        icount = icount + 1
     ENDIF
     jval = ss%nbeam
     CALL ichk_dim(ival,jval,"nbeam")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_ncircuits",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%ncircuits.EQ.0) THEN
        ss%ncircuits = ival
        icount = icount + 1
     ENDIF
     jval = ss%ncircuits
     CALL ichk_dim(ival,jval,"ncircuits")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_ncoils",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%ncoils.EQ.0) THEN
        ss%ncoils = ival
        icount = icount + 1
     ENDIF
     jval = ss%ncoils
     CALL ichk_dim(ival,jval,"ncoils")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_ndelta_w",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%ndelta_w.EQ.0) THEN
        ss%ndelta_w = ival
        icount = icount + 1
     ENDIF
     jval = ss%ndelta_w
     CALL ichk_dim(ival,jval,"ndelta_w")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_necrf_src",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%necrf_src.EQ.0) THEN
        ss%necrf_src = ival
        icount = icount + 1
     ENDIF
     jval = ss%necrf_src
     CALL ichk_dim(ival,jval,"necrf_src")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nefi_anom",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nefi_anom.EQ.0) THEN
        ss%nefi_anom = ival
        icount = icount + 1
     ENDIF
     jval = ss%nefi_anom
     CALL ichk_dim(ival,jval,"nefi_anom")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nefi_anom2",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nefi_anom2.EQ.0) THEN
        ss%nefi_anom2 = ival
        icount = icount + 1
     ENDIF
     jval = ss%nefi_anom2
     CALL ichk_dim(ival,jval,"nefi_anom2")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_neqmom",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%neqmom.EQ.0) THEN
        ss%neqmom = ival
        icount = icount + 1
     ENDIF
     jval = ss%neqmom
     CALL ichk_dim(ival,jval,"neqmom")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_ngsc0",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%ngsc0.EQ.0) THEN
        ss%ngsc0 = ival
        icount = icount + 1
     ENDIF
     jval = ss%ngsc0
     CALL ichk_dim(ival,jval,"ngsc0")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nicrf_src",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nicrf_src.EQ.0) THEN
        ss%nicrf_src = ival
        icount = icount + 1
     ENDIF
     jval = ss%nicrf_src
     CALL ichk_dim(ival,jval,"nicrf_src")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nky_anomth",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nky_anomth.EQ.0) THEN
        ss%nky_anomth = ival
        icount = icount + 1
     ENDIF
     jval = ss%nky_anomth
     CALL ichk_dim(ival,jval,"nky_anomth")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nky_mmm",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nky_mmm.EQ.0) THEN
        ss%nky_mmm = ival
        icount = icount + 1
     ENDIF
     jval = ss%nky_mmm
     CALL ichk_dim(ival,jval,"nky_mmm")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nlhrf_src",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nlhrf_src.EQ.0) THEN
        ss%nlhrf_src = ival
        icount = icount + 1
     ENDIF
     jval = ss%nlhrf_src
     CALL ichk_dim(ival,jval,"nlhrf_src")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nnewcomb",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nnewcomb.EQ.0) THEN
        ss%nnewcomb = ival
        icount = icount + 1
     ENDIF
     jval = ss%nnewcomb
     CALL ichk_dim(ival,jval,"nnewcomb")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_npsmom",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%npsmom.EQ.0) THEN
        ss%npsmom = ival
        icount = icount + 1
     ENDIF
     jval = ss%npsmom
     CALL ichk_dim(ival,jval,"npsmom")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nr",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nr.EQ.0) THEN
        ss%nr = ival
        icount = icount + 1
     ENDIF
     jval = ss%nr
     CALL ichk_dim(ival,jval,"nr")
  ENDIF

  status = nf90_inq_dimid(icdf,"dm1_nrho",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival + 1
     IF(ss%nrho.EQ.0) THEN
        ss%nrho = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho
     CALL ichk_dim(ival,jval,"nrho")
  ENDIF
  status = nf90_inq_dimid(icdf,"dim_nrho",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho.EQ.0) THEN
        ss%nrho = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho
     CALL ichk_dim(ival,jval,"nrho")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nrho_anom",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_anom.EQ.0) THEN
        ss%nrho_anom = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_anom
     CALL ichk_dim(ival,jval,"nrho_anom")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nrho_anom2",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_anom2.EQ.0) THEN
        ss%nrho_anom2 = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_anom2
     CALL ichk_dim(ival,jval,"nrho_anom2")
  ENDIF

  status = nf90_inq_dimid(icdf,"dm1_nrho_anomth",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival + 1
     IF(ss%nrho_anomth.EQ.0) THEN
        ss%nrho_anomth = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_anomth
     CALL ichk_dim(ival,jval,"nrho_anomth")
  ENDIF
  status = nf90_inq_dimid(icdf,"dim_nrho_anomth",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_anomth.EQ.0) THEN
        ss%nrho_anomth = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_anomth
     CALL ichk_dim(ival,jval,"nrho_anomth")
  ENDIF

  status = nf90_inq_dimid(icdf,"dm1_nrho_ecrf",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival + 1
     IF(ss%nrho_ecrf.EQ.0) THEN
        ss%nrho_ecrf = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_ecrf
     CALL ichk_dim(ival,jval,"nrho_ecrf")
  ENDIF
  status = nf90_inq_dimid(icdf,"dim_nrho_ecrf",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_ecrf.EQ.0) THEN
        ss%nrho_ecrf = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_ecrf
     CALL ichk_dim(ival,jval,"nrho_ecrf")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nrho_eq",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_eq.EQ.0) THEN
        ss%nrho_eq = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_eq
     CALL ichk_dim(ival,jval,"nrho_eq")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_eq_geo.EQ.0) THEN
        ss%nrho_eq_geo = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_eq_geo
     CALL ichk_dim(ival,jval,"nrho_eq_geo")
  ENDIF

  status = nf90_inq_dimid(icdf,"dm1_nrho_fus",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival + 1
     IF(ss%nrho_fus.EQ.0) THEN
        ss%nrho_fus = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_fus
     CALL ichk_dim(ival,jval,"nrho_fus")
  ENDIF
  status = nf90_inq_dimid(icdf,"dim_nrho_fus",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_fus.EQ.0) THEN
        ss%nrho_fus = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_fus
     CALL ichk_dim(ival,jval,"nrho_fus")
  ENDIF

  status = nf90_inq_dimid(icdf,"dm1_nrho_gas",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival + 1
     IF(ss%nrho_gas.EQ.0) THEN
        ss%nrho_gas = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_gas
     CALL ichk_dim(ival,jval,"nrho_gas")
  ENDIF
  status = nf90_inq_dimid(icdf,"dim_nrho_gas",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_gas.EQ.0) THEN
        ss%nrho_gas = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_gas
     CALL ichk_dim(ival,jval,"nrho_gas")
  ENDIF

  status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival + 1
     IF(ss%nrho_icrf.EQ.0) THEN
        ss%nrho_icrf = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_icrf
     CALL ichk_dim(ival,jval,"nrho_icrf")
  ENDIF
  status = nf90_inq_dimid(icdf,"dim_nrho_icrf",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_icrf.EQ.0) THEN
        ss%nrho_icrf = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_icrf
     CALL ichk_dim(ival,jval,"nrho_icrf")
  ENDIF

  status = nf90_inq_dimid(icdf,"dm1_nrho_lhrf",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival + 1
     IF(ss%nrho_lhrf.EQ.0) THEN
        ss%nrho_lhrf = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_lhrf
     CALL ichk_dim(ival,jval,"nrho_lhrf")
  ENDIF
  status = nf90_inq_dimid(icdf,"dim_nrho_lhrf",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_lhrf.EQ.0) THEN
        ss%nrho_lhrf = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_lhrf
     CALL ichk_dim(ival,jval,"nrho_lhrf")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nrho_lmhd",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_lmhd.EQ.0) THEN
        ss%nrho_lmhd = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_lmhd
     CALL ichk_dim(ival,jval,"nrho_lmhd")
  ENDIF

  status = nf90_inq_dimid(icdf,"dm1_nrho_mist",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival + 1
     IF(ss%nrho_mist.EQ.0) THEN
        ss%nrho_mist = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_mist
     CALL ichk_dim(ival,jval,"nrho_mist")
  ENDIF
  status = nf90_inq_dimid(icdf,"dim_nrho_mist",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_mist.EQ.0) THEN
        ss%nrho_mist = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_mist
     CALL ichk_dim(ival,jval,"nrho_mist")
  ENDIF

  status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival + 1
     IF(ss%nrho_nbi.EQ.0) THEN
        ss%nrho_nbi = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_nbi
     CALL ichk_dim(ival,jval,"nrho_nbi")
  ENDIF
  status = nf90_inq_dimid(icdf,"dim_nrho_nbi",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_nbi.EQ.0) THEN
        ss%nrho_nbi = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_nbi
     CALL ichk_dim(ival,jval,"nrho_nbi")
  ENDIF

  status = nf90_inq_dimid(icdf,"dm1_nrho_pel",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival + 1
     IF(ss%nrho_pel.EQ.0) THEN
        ss%nrho_pel = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_pel
     CALL ichk_dim(ival,jval,"nrho_pel")
  ENDIF
  status = nf90_inq_dimid(icdf,"dim_nrho_pel",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_pel.EQ.0) THEN
        ss%nrho_pel = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_pel
     CALL ichk_dim(ival,jval,"nrho_pel")
  ENDIF

  status = nf90_inq_dimid(icdf,"dm1_nrho_rad",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival + 1
     IF(ss%nrho_rad.EQ.0) THEN
        ss%nrho_rad = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_rad
     CALL ichk_dim(ival,jval,"nrho_rad")
  ENDIF
  status = nf90_inq_dimid(icdf,"dim_nrho_rad",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_rad.EQ.0) THEN
        ss%nrho_rad = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_rad
     CALL ichk_dim(ival,jval,"nrho_rad")
  ENDIF

  status = nf90_inq_dimid(icdf,"dm1_nrho_rw",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival + 1
     IF(ss%nrho_rw.EQ.0) THEN
        ss%nrho_rw = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_rw
     CALL ichk_dim(ival,jval,"nrho_rw")
  ENDIF
  status = nf90_inq_dimid(icdf,"dim_nrho_rw",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nrho_rw.EQ.0) THEN
        ss%nrho_rw = ival
        icount = icount + 1
     ENDIF
     jval = ss%nrho_rw
     CALL ichk_dim(ival,jval,"nrho_rw")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nr_rip",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nr_rip.EQ.0) THEN
        ss%nr_rip = ival
        icount = icount + 1
     ENDIF
     jval = ss%nr_rip
     CALL ichk_dim(ival,jval,"nr_rip")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nspec_all",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nspec_all.EQ.0) THEN
        ss%nspec_all = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_all
     CALL ichk_dim(ival,jval,"nspec_all")
  ENDIF
  status = nf90_inq_dimid(icdf,"dp1_nspec_all",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival - 1
     IF(ss%nspec_all.EQ.0) THEN
        ss%nspec_all = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_all
     CALL ichk_dim(ival,jval,"nspec_all")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nspec_alla",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nspec_alla.EQ.0) THEN
        ss%nspec_alla = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_alla
     CALL ichk_dim(ival,jval,"nspec_alla")
  ENDIF
  status = nf90_inq_dimid(icdf,"dp1_nspec_alla",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival - 1
     IF(ss%nspec_alla.EQ.0) THEN
        ss%nspec_alla = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_alla
     CALL ichk_dim(ival,jval,"nspec_alla")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nspec_beam",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nspec_beam.EQ.0) THEN
        ss%nspec_beam = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_beam
     CALL ichk_dim(ival,jval,"nspec_beam")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nspec_fusion",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nspec_fusion.EQ.0) THEN
        ss%nspec_fusion = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_fusion
     CALL ichk_dim(ival,jval,"nspec_fusion")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nspec_gas",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nspec_gas.EQ.0) THEN
        ss%nspec_gas = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_gas
     CALL ichk_dim(ival,jval,"nspec_gas")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nspec_imp0",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nspec_imp0.EQ.0) THEN
        ss%nspec_imp0 = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_imp0
     CALL ichk_dim(ival,jval,"nspec_imp0")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nspec_impi",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nspec_impi.EQ.0) THEN
        ss%nspec_impi = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_impi
     CALL ichk_dim(ival,jval,"nspec_impi")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nspec_rfmin.EQ.0) THEN
        ss%nspec_rfmin = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_rfmin
     CALL ichk_dim(ival,jval,"nspec_rfmin")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nspec_th",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nspec_th.EQ.0) THEN
        ss%nspec_th = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_th
     CALL ichk_dim(ival,jval,"nspec_th")
  ENDIF
  status = nf90_inq_dimid(icdf,"dp1_nspec_th",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival - 1
     IF(ss%nspec_th.EQ.0) THEN
        ss%nspec_th = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_th
     CALL ichk_dim(ival,jval,"nspec_th")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nspec_tha",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nspec_tha.EQ.0) THEN
        ss%nspec_tha = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_tha
     CALL ichk_dim(ival,jval,"nspec_tha")
  ENDIF
  status = nf90_inq_dimid(icdf,"dp1_nspec_tha",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     ival = ival - 1
     IF(ss%nspec_tha.EQ.0) THEN
        ss%nspec_tha = ival
        icount = icount + 1
     ENDIF
     jval = ss%nspec_tha
     CALL ichk_dim(ival,jval,"nspec_tha")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nth_eq",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nth_eq.EQ.0) THEN
        ss%nth_eq = ival
        icount = icount + 1
     ENDIF
     jval = ss%nth_eq
     CALL ichk_dim(ival,jval,"nth_eq")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_ntrcat",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%ntrcat.EQ.0) THEN
        ss%ntrcat = ival
        icount = icount + 1
     ENDIF
     jval = ss%ntrcat
     CALL ichk_dim(ival,jval,"ntrcat")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_num_rzlim",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%num_rzlim.EQ.0) THEN
        ss%num_rzlim = ival
        icount = icount + 1
     ENDIF
     jval = ss%num_rzlim
     CALL ichk_dim(ival,jval,"num_rzlim")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nxs",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nxs.EQ.0) THEN
        ss%nxs = ival
        icount = icount + 1
     ENDIF
     jval = ss%nxs
     CALL ichk_dim(ival,jval,"nxs")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nz",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nz.EQ.0) THEN
        ss%nz = ival
        icount = icount + 1
     ENDIF
     jval = ss%nz
     CALL ichk_dim(ival,jval,"nz")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_nz_rip",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%nz_rip.EQ.0) THEN
        ss%nz_rip = ival
        icount = icount + 1
     ENDIF
     jval = ss%nz_rip
     CALL ichk_dim(ival,jval,"nz_rip")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_n_d_einj_standard",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%n_D_einj_standard.EQ.0) THEN
        ss%n_D_einj_standard = ival
        icount = icount + 1
     ENDIF
     jval = ss%n_D_einj_standard
     CALL ichk_dim(ival,jval,"n_D_einj_standard")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_n_h_einj_standard",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%n_H_einj_standard.EQ.0) THEN
        ss%n_H_einj_standard = ival
        icount = icount + 1
     ENDIF
     jval = ss%n_H_einj_standard
     CALL ichk_dim(ival,jval,"n_H_einj_standard")
  ENDIF

  status = nf90_inq_dimid(icdf,"dim_n_t_einj_standard",dimid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_inquire_dimension(icdf,dimid,len=ival)
     IF(ss%n_T_einj_standard.EQ.0) THEN
        ss%n_T_einj_standard = ival
        icount = icount + 1
     ENDIF
     jval = ss%n_T_einj_standard
     CALL ichk_dim(ival,jval,"n_T_einj_standard")
  ENDIF

  IF(iertot.GT.0) THEN
     WRITE(iout,*) " ?ps_update_file_read: error with dimension values."
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  ! DIMENSIONS read in OK; allocate state arrays...

  IF(icount.GT.0) THEN
     CALL plasma_state_empty_alloc(ss,ierr)
     IF(ierr.NE.0) THEN
        WRITE(iout,*) " ?ps_update_file_read: plasma_state_empty_alloc error."
        ierr=9999
        status = nf90_close(icdf)
        RETURN
     ENDIF
  ENDIF

  !---------------------------------------------
  ! call subroutines to read data and arrays...

  call ps_update_file_read_0(ss,icdf,iout,iucount,iertot)
  call ps_update_file_read_1(ss,icdf,iout,iucount,iertot)
  call ps_update_file_read_2(ss,icdf,iout,iucount,iertot)
  call ps_update_file_read_3(ss,icdf,iout,iucount,iertot)
  call ps_update_file_read_4(ss,icdf,iout,iucount,iertot)
  call ps_update_file_read_5(ss,icdf,iout,iucount,iertot)
  call ps_update_file_read_6(ss,icdf,iout,iucount,iertot)
  call ps_update_file_read_7(ss,icdf,iout,iucount,iertot)
  call ps_update_file_read_8(ss,icdf,iout,iucount,iertot)
  call ps_update_file_read_9(ss,icdf,iout,iucount,iertot)


  IF(iertot.GT.0) THEN
     WRITE(iout,*) ' ?ps_update_file_read: an error occurred.'
     ierr=9999
  ELSE
     WRITE(iout,*) ' *ps_update_file_read: OK, found data for', iucount, &
          ' state elements.'
     ierr=0
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

END SUBROUTINE ps_update_file_read
