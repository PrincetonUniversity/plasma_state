! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_store(filename,ss,icheck,ierr)

  ! store (i.e. write to file) plasma state

  USE plasma_state_definition_mod
  USE netcdf
  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN) :: filename        ! state file
  TYPE (plasma_state) :: ss     ! state data to write
  INTEGER, INTENT(in) :: icheck ! =0 to skip equilibrium check
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-----------------------
  ! if filename is blank, plasma state is loaded into xplasma
  ! and hash codes are updated, but, no NetCDF file is written.
  !-----------------------

  INTEGER :: icdf,iout,varid,dimid,status,iertot,ival,ii,isize
  INTEGER :: idims(20),indim
  CHARACTER*80 :: nc_errmsg
  CHARACTER*32 :: dimname

  !-----------------------
  ! Initialization...

  iout = SWIM_err_out_file

  ss%version_id = ps_version_id

  !------------------------------------
  ! reload equilibrium and other data in xplasma, with checks, as needed...
  ! ...hand maintained code orchestrates:
  !     0. plasma_state_empty_alloc; update ps_active_dims(...)
  !     1. compute new_hash_codes
  !     2. xplasma update of equilibrium as needed based on hash codes
  !     3. xplasma update of data and profiles as needed
  !     4. "zonwt" update
  !     5. update of derived quantities in xplasma.

  CALL ps_xload_check(ss,iout,icheck,ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) " ?plasma_state_store: error in ps_xload_check."
     ierr = 9999
     RETURN
  ENDIF

  CALL plasma_state_xload(ss,ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) " ?plasma_state_store: error in plasma_state_xload."
     ierr = 9999
     RETURN
  ENDIF

  !------------------------------------
  ! in this routine, everything is written out, so no need to keep
  ! old hash codes.

  ss%hash_codes = ss%new_hash_codes

  !------------------------------------
  ! if no filename is provided, we are done!

  IF(filename.eq.' ') RETURN
  !------------------------------------
  ! if a filename is provided, create (clobber!) a new NetCDF file

  status = nf90_create(trim(filename),NF90_CLOBBER,icdf)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?plasma_state_store: file create failure: "'//trim(filename)//'".'
     ierr=9999
     RETURN
  ENDIF

  !------------------------------------

  ! *** Array Dimensions ***

  ss%active_dims = 0
  iertot = 0

  IF(ss%max_nrz_antgeo.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
     ival = ss%max_nrz_antgeo
     status = nf90_def_dim(icdf,"dim_max_nrz_antgeo",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_max_nrz_antgeo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%max_nspectrumlh_src.GT.0) THEN
     ss%active_dims(ps_cnum_LH) = ss%active_dims(ps_cnum_LH) + 1
     ival = ss%max_nspectrumlh_src
     status = nf90_def_dim(icdf,"dim_max_nspectrumlh_src",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_max_nspectrumlh_src"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%max_num_nphi.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
     ival = ss%max_num_nphi
     status = nf90_def_dim(icdf,"dim_max_num_nphi",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_max_num_nphi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%max_num_nphi_vac.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
     ival = ss%max_num_nphi_vac
     status = nf90_def_dim(icdf,"dim_max_num_nphi_vac",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_max_num_nphi_vac"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%max_n_straps.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
     ival = ss%max_n_straps
     status = nf90_def_dim(icdf,"dim_max_n_straps",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_max_n_straps"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%mmodes.GT.0) THEN
     ss%active_dims(ps_cnum_LMHD) = ss%active_dims(ps_cnum_LMHD) + 1
     ival = ss%mmodes
     status = nf90_def_dim(icdf,"dim_mmodes",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_mmodes"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nballoon.GT.0) THEN
     ss%active_dims(ps_cnum_LMHD) = ss%active_dims(ps_cnum_LMHD) + 1
     ival = ss%nballoon
     status = nf90_def_dim(icdf,"dim_nballoon",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nballoon"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nbeam.GT.0) THEN
     ss%active_dims(ps_cnum_NBI) = ss%active_dims(ps_cnum_NBI) + 1
     ival = ss%nbeam
     status = nf90_def_dim(icdf,"dim_nbeam",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nbeam"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%ncircuits.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
     ival = ss%ncircuits
     status = nf90_def_dim(icdf,"dim_ncircuits",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_ncircuits"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%ncoils.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
     ival = ss%ncoils
     status = nf90_def_dim(icdf,"dim_ncoils",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_ncoils"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%ndelta_w.GT.0) THEN
     ss%active_dims(ps_cnum_LMHD) = ss%active_dims(ps_cnum_LMHD) + 1
     ival = ss%ndelta_w
     status = nf90_def_dim(icdf,"dim_ndelta_w",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_ndelta_w"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%necrf_src.GT.0) THEN
     ss%active_dims(ps_cnum_EC) = ss%active_dims(ps_cnum_EC) + 1
     ival = ss%necrf_src
     status = nf90_def_dim(icdf,"dim_necrf_src",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_necrf_src"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nefi_anom.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
     ival = ss%nefi_anom
     status = nf90_def_dim(icdf,"dim_nefi_anom",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nefi_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nefi_anom2.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
     ival = ss%nefi_anom2
     status = nf90_def_dim(icdf,"dim_nefi_anom2",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nefi_anom2"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%neqmom.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
     ival = ss%neqmom
     status = nf90_def_dim(icdf,"dim_neqmom",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_neqmom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%ngsc0.GT.0) THEN
     ss%active_dims(ps_cnum_GAS) = ss%active_dims(ps_cnum_GAS) + 1
     ival = ss%ngsc0
     status = nf90_def_dim(icdf,"dim_ngsc0",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_ngsc0"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nicrf_src.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
     ival = ss%nicrf_src
     status = nf90_def_dim(icdf,"dim_nicrf_src",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nicrf_src"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nky_anomth.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
     ival = ss%nky_anomth
     status = nf90_def_dim(icdf,"dim_nky_anomth",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nky_anomth"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nky_mmm.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
     ival = ss%nky_mmm
     status = nf90_def_dim(icdf,"dim_nky_mmm",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nky_mmm"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nlhrf_src.GT.0) THEN
     ss%active_dims(ps_cnum_LH) = ss%active_dims(ps_cnum_LH) + 1
     ival = ss%nlhrf_src
     status = nf90_def_dim(icdf,"dim_nlhrf_src",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nlhrf_src"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nnewcomb.GT.0) THEN
     ss%active_dims(ps_cnum_LMHD) = ss%active_dims(ps_cnum_LMHD) + 1
     ival = ss%nnewcomb
     status = nf90_def_dim(icdf,"dim_nnewcomb",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nnewcomb"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%npsmom.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
     ival = ss%npsmom
     status = nf90_def_dim(icdf,"dim_npsmom",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_npsmom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nr.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
     ival = ss%nr
     status = nf90_def_dim(icdf,"dim_nr",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nr"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
     ival = ss%nrho - 1
     status = nf90_def_dim(icdf,"dm1_nrho",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dm1_nrho"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nrho
     status = nf90_def_dim(icdf,"dim_nrho",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_anom.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
     ival = ss%nrho_anom
     status = nf90_def_dim(icdf,"dim_nrho_anom",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_anom"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_anom2.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
     ival = ss%nrho_anom2
     status = nf90_def_dim(icdf,"dim_nrho_anom2",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_anom2"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_anomth.GT.0) THEN
     ss%active_dims(ps_cnum_ANOM) = ss%active_dims(ps_cnum_ANOM) + 1
     ival = ss%nrho_anomth - 1
     status = nf90_def_dim(icdf,"dm1_nrho_anomth",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dm1_nrho_anomth"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nrho_anomth
     status = nf90_def_dim(icdf,"dim_nrho_anomth",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_anomth"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_ecrf.GT.0) THEN
     ss%active_dims(ps_cnum_EC) = ss%active_dims(ps_cnum_EC) + 1
     ival = ss%nrho_ecrf - 1
     status = nf90_def_dim(icdf,"dm1_nrho_ecrf",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dm1_nrho_ecrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nrho_ecrf
     status = nf90_def_dim(icdf,"dim_nrho_ecrf",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_ecrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_eq.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
     ival = ss%nrho_eq
     status = nf90_def_dim(icdf,"dim_nrho_eq",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_eq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_eq_geo.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
     ival = ss%nrho_eq_geo
     status = nf90_def_dim(icdf,"dim_nrho_eq_geo",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_eq_geo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_fus.GT.0) THEN
     ss%active_dims(ps_cnum_FUS) = ss%active_dims(ps_cnum_FUS) + 1
     ival = ss%nrho_fus - 1
     status = nf90_def_dim(icdf,"dm1_nrho_fus",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dm1_nrho_fus"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nrho_fus
     status = nf90_def_dim(icdf,"dim_nrho_fus",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_fus"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_gas.GT.0) THEN
     ss%active_dims(ps_cnum_GAS) = ss%active_dims(ps_cnum_GAS) + 1
     ival = ss%nrho_gas - 1
     status = nf90_def_dim(icdf,"dm1_nrho_gas",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dm1_nrho_gas"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nrho_gas
     status = nf90_def_dim(icdf,"dim_nrho_gas",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_gas"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_icrf.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
     ival = ss%nrho_icrf - 1
     status = nf90_def_dim(icdf,"dm1_nrho_icrf",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dm1_nrho_icrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nrho_icrf
     status = nf90_def_dim(icdf,"dim_nrho_icrf",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_icrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_lhrf.GT.0) THEN
     ss%active_dims(ps_cnum_LH) = ss%active_dims(ps_cnum_LH) + 1
     ival = ss%nrho_lhrf - 1
     status = nf90_def_dim(icdf,"dm1_nrho_lhrf",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dm1_nrho_lhrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nrho_lhrf
     status = nf90_def_dim(icdf,"dim_nrho_lhrf",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_lhrf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_lmhd.GT.0) THEN
     ss%active_dims(ps_cnum_LMHD) = ss%active_dims(ps_cnum_LMHD) + 1
     ival = ss%nrho_lmhd
     status = nf90_def_dim(icdf,"dim_nrho_lmhd",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_lmhd"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_mist.GT.0) THEN
     ss%active_dims(ps_cnum_RAD) = ss%active_dims(ps_cnum_RAD) + 1
     ival = ss%nrho_mist - 1
     status = nf90_def_dim(icdf,"dm1_nrho_mist",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dm1_nrho_mist"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nrho_mist
     status = nf90_def_dim(icdf,"dim_nrho_mist",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_mist"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_nbi.GT.0) THEN
     ss%active_dims(ps_cnum_NBI) = ss%active_dims(ps_cnum_NBI) + 1
     ival = ss%nrho_nbi - 1
     status = nf90_def_dim(icdf,"dm1_nrho_nbi",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dm1_nrho_nbi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nrho_nbi
     status = nf90_def_dim(icdf,"dim_nrho_nbi",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_nbi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_pel.GT.0) THEN
     ss%active_dims(ps_cnum_PEL) = ss%active_dims(ps_cnum_PEL) + 1
     ival = ss%nrho_pel - 1
     status = nf90_def_dim(icdf,"dm1_nrho_pel",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dm1_nrho_pel"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nrho_pel
     status = nf90_def_dim(icdf,"dim_nrho_pel",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_pel"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_rad.GT.0) THEN
     ss%active_dims(ps_cnum_RAD) = ss%active_dims(ps_cnum_RAD) + 1
     ival = ss%nrho_rad - 1
     status = nf90_def_dim(icdf,"dm1_nrho_rad",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dm1_nrho_rad"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nrho_rad
     status = nf90_def_dim(icdf,"dim_nrho_rad",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_rad"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nrho_rw.GT.0) THEN
     ss%active_dims(ps_cnum_RUNAWAY) = ss%active_dims(ps_cnum_RUNAWAY) + 1
     ival = ss%nrho_rw - 1
     status = nf90_def_dim(icdf,"dm1_nrho_rw",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dm1_nrho_rw"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nrho_rw
     status = nf90_def_dim(icdf,"dim_nrho_rw",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nrho_rw"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nr_rip.GT.0) THEN
     ss%active_dims(ps_cnum_RIPPLE) = ss%active_dims(ps_cnum_RIPPLE) + 1
     ival = ss%nr_rip
     status = nf90_def_dim(icdf,"dim_nr_rip",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nr_rip"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nspec_all.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
     ival = ss%nspec_all
     status = nf90_def_dim(icdf,"dim_nspec_all",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nspec_all"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nspec_all + 1
     status = nf90_def_dim(icdf,"dp1_nspec_all",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dp1_nspec_all"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nspec_alla.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
     ival = ss%nspec_alla
     status = nf90_def_dim(icdf,"dim_nspec_alla",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nspec_alla"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nspec_alla + 1
     status = nf90_def_dim(icdf,"dp1_nspec_alla",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dp1_nspec_alla"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nspec_beam.GT.0) THEN
     ss%active_dims(ps_cnum_NBI) = ss%active_dims(ps_cnum_NBI) + 1
     ival = ss%nspec_beam
     status = nf90_def_dim(icdf,"dim_nspec_beam",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nspec_beam"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nspec_fusion.GT.0) THEN
     ss%active_dims(ps_cnum_FUS) = ss%active_dims(ps_cnum_FUS) + 1
     ival = ss%nspec_fusion
     status = nf90_def_dim(icdf,"dim_nspec_fusion",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nspec_fusion"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nspec_gas.GT.0) THEN
     ss%active_dims(ps_cnum_GAS) = ss%active_dims(ps_cnum_GAS) + 1
     ival = ss%nspec_gas
     status = nf90_def_dim(icdf,"dim_nspec_gas",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nspec_gas"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nspec_imp0.GT.0) THEN
     ss%active_dims(ps_cnum_GAS) = ss%active_dims(ps_cnum_GAS) + 1
     ival = ss%nspec_imp0
     status = nf90_def_dim(icdf,"dim_nspec_imp0",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nspec_imp0"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nspec_impi.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
     ival = ss%nspec_impi
     status = nf90_def_dim(icdf,"dim_nspec_impi",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nspec_impi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nspec_rfmin.GT.0) THEN
     ss%active_dims(ps_cnum_IC) = ss%active_dims(ps_cnum_IC) + 1
     ival = ss%nspec_rfmin
     status = nf90_def_dim(icdf,"dim_nspec_rfmin",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nspec_rfmin"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nspec_th.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
     ival = ss%nspec_th
     status = nf90_def_dim(icdf,"dim_nspec_th",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nspec_th"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nspec_th + 1
     status = nf90_def_dim(icdf,"dp1_nspec_th",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dp1_nspec_th"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nspec_tha.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
     ival = ss%nspec_tha
     status = nf90_def_dim(icdf,"dim_nspec_tha",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nspec_tha"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
     ival = ss%nspec_tha + 1
     status = nf90_def_dim(icdf,"dp1_nspec_tha",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dp1_nspec_tha"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nth_eq.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
     ival = ss%nth_eq
     status = nf90_def_dim(icdf,"dim_nth_eq",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nth_eq"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%ntrcat.GT.0) THEN
     ss%active_dims(ps_cnum_PLASMA) = ss%active_dims(ps_cnum_PLASMA) + 1
     ival = ss%ntrcat
     status = nf90_def_dim(icdf,"dim_ntrcat",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_ntrcat"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%num_rzlim.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
     ival = ss%num_rzlim
     status = nf90_def_dim(icdf,"dim_num_rzlim",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_num_rzlim"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nxs.GT.0) THEN
     ss%active_dims(ps_cnum_RAD) = ss%active_dims(ps_cnum_RAD) + 1
     ival = ss%nxs
     status = nf90_def_dim(icdf,"dim_nxs",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nxs"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nz.GT.0) THEN
     ss%active_dims(ps_cnum_EQ) = ss%active_dims(ps_cnum_EQ) + 1
     ival = ss%nz
     status = nf90_def_dim(icdf,"dim_nz",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nz"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%nz_rip.GT.0) THEN
     ss%active_dims(ps_cnum_RIPPLE) = ss%active_dims(ps_cnum_RIPPLE) + 1
     ival = ss%nz_rip
     status = nf90_def_dim(icdf,"dim_nz_rip",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_nz_rip"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%n_D_einj_standard.GT.0) THEN
     ss%active_dims(ps_cnum_NBI) = ss%active_dims(ps_cnum_NBI) + 1
     ival = ss%n_D_einj_standard
     status = nf90_def_dim(icdf,"dim_n_d_einj_standard",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_n_d_einj_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%n_H_einj_standard.GT.0) THEN
     ss%active_dims(ps_cnum_NBI) = ss%active_dims(ps_cnum_NBI) + 1
     ival = ss%n_H_einj_standard
     status = nf90_def_dim(icdf,"dim_n_h_einj_standard",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_n_h_einj_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ss%n_T_einj_standard.GT.0) THEN
     ss%active_dims(ps_cnum_NBI) = ss%active_dims(ps_cnum_NBI) + 1
     ival = ss%n_T_einj_standard
     status = nf90_def_dim(icdf,"dim_n_t_einj_standard",ival,dimid)
     IF(status.NE.NF90_NOERR) THEN
        write(iout,*) " error defining dimension: dim_n_t_einj_standard"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_def_dim(icdf,"ps_clen_32",32,dimid)
  IF(status.NE.NF90_NOERR) THEN
     write(iout,*) " error defining dimension: ps_clen_32"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_def_dim(icdf,"ps_clen_256",256,dimid)
  IF(status.NE.NF90_NOERR) THEN
     write(iout,*) " error defining dimension: ps_clen_256"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_def_dim(icdf,"ps_clen_80",80,dimid)
  IF(status.NE.NF90_NOERR) THEN
     write(iout,*) " error defining dimension: ps_clen_80"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  ! hash codes dimension
  status = nf90_def_dim(icdf,"ps_hash_dim",ps_hash_total,dimid)
  IF(status.NE.NF90_NOERR) THEN
     write(iout,*) " error defining dimension:  ps_hash_dim"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  ! hash codes array
  idims(1)=dimid
  status = nf90_def_var(icdf,"hash_codes",NF90_INT, &
            dimids = idims(1:1), varid=varid)
  IF(status.NE.NF90_NOERR) THEN
     write(iout,*) " error defining:  hash_codes array variable."
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(iertot.GT.0) THEN
     WRITE(iout,*) " ?plasma_state_store: error defining NetCDF dimensions."
     ierr = 9999
     status = nf90_close(icdf)
     RETURN
  ELSE
     ierr = 0
  ENDIF

  !-------------------------------------------------
  !  define variables with attributes


  status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
  status = nf90_def_var(icdf,"version_id",NF90_CHAR, &
            dimids = idims(1:1), varid=varid)
  IF(status.NE.NF90_NOERR) THEN
     write(iout,*) " error defining:  version_id character string."
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  !  define variables with attributes -- subroutine calls

  call plasma_state_store_def_0(ss,icdf,iout,iertot)
  call plasma_state_store_def_1(ss,icdf,iout,iertot)
  call plasma_state_store_def_2(ss,icdf,iout,iertot)
  call plasma_state_store_def_3(ss,icdf,iout,iertot)
  call plasma_state_store_def_4(ss,icdf,iout,iertot)
  call plasma_state_store_def_5(ss,icdf,iout,iertot)
  call plasma_state_store_def_6(ss,icdf,iout,iertot)
  call plasma_state_store_def_7(ss,icdf,iout,iertot)
  call plasma_state_store_def_8(ss,icdf,iout,iertot)
  call plasma_state_store_def_9(ss,icdf,iout,iertot)

  IF(iertot.GT.0) THEN
     WRITE(iout,*) " ?plasma_state_store: error defining NetCDF variables."
     ierr = 9999
     status = nf90_close(icdf)
     RETURN
  ELSE
     ierr = 0
  ENDIF

  !------------------------------------
  ! transfer the data to NetCDF

  status = nf90_enddef(icdf)  ! exit definition mode

  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: nf90_enddef error"
     CALL ncerr(status)
     ierr = 9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  iertot = 0

  status = nf90_inq_varid(icdf,"version_id",varid)
  status = nf90_put_var(icdf,varid,ps_version_id)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing version_id"
     CALL ncerr(status)
     ierr = 9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  status = nf90_inq_varid(icdf,"hash_codes",varid)
  status = nf90_put_var(icdf,varid,ss%hash_codes)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing hash_codes"
     CALL ncerr(status)
     ierr = 9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  !  write variables -- subroutine calls

  call plasma_state_store_0(ss,icdf,iout,iertot)
  call plasma_state_store_1(ss,icdf,iout,iertot)
  call plasma_state_store_2(ss,icdf,iout,iertot)
  call plasma_state_store_3(ss,icdf,iout,iertot)
  call plasma_state_store_4(ss,icdf,iout,iertot)
  call plasma_state_store_5(ss,icdf,iout,iertot)
  call plasma_state_store_6(ss,icdf,iout,iertot)
  call plasma_state_store_7(ss,icdf,iout,iertot)
  call plasma_state_store_8(ss,icdf,iout,iertot)
  call plasma_state_store_9(ss,icdf,iout,iertot)

  status = nf90_close(icdf)

  IF(iertot.GT.0) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing NetCDF variables."
     ierr = 9999
     RETURN
  ELSE
     ierr = 0
  ENDIF

  CONTAINS


     SUBROUTINE ncerr(status)
        INTEGER, INTENT(in) :: status

        nc_errmsg = NF90_strerror(status)
        write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
     END SUBROUTINE ncerr

END SUBROUTINE plasma_state_store
