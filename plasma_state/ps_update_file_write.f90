! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_write(filename,ss,cclist,iwarn,ierr)

  ! store (i.e. write to file) plasma state updates
  ! determine what to write as follows:
  !   (a) scalar data:  each item with a change in value is written; if any
  !       element of an array of scalars has changes the entire array is
  !       written.
  !   (b) profile data:  each array that contains ANY changes is written;
  !       Note the entire array is written if there are changes to any parts.
  !   (c) array dimensions needed to define arrays to be written... are also
  !       written.
  !
  ! dimensioning information necessary to support this data is also written.
  ! attributes are NOT written-- these are in the full state files only.

  USE plasma_state_definition_mod
  USE netcdf
  USE ps_nidlist_defn
  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN) :: filename        ! update file
  TYPE (plasma_state) :: ss     ! state containing data to write
  INTEGER, INTENT(IN) :: cclist(ps_ccount) ! component-by-component control

  INTEGER, INTENT(OUT) :: iwarn ! number of modified items not written
  !  this would be due to a modification in an item belonging to component XX
  !  for which cclist(ps_cnum_XX).ne.1; warning messages are printed.

  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-----------------------
  LOGICAL :: plasma_state_hash_match,ichange

  TYPE (nidlist) :: nids

  INTEGER :: icdf,iout,varid,pvarid,vvarid,dimid,status,iertot,iertmp,iaddr
  INTEGER :: idims(20),indim,ival,iucount,ii
  CHARACTER*80 :: nc_errmsg


  !-----------------------
  ! Initialization...

  nids%nid_dim_max_nrz_antgeo = 0
  nids%nid_dim_max_nspectrumlh_src = 0
  nids%nid_dim_max_num_nphi = 0
  nids%nid_dim_max_num_nphi_vac = 0
  nids%nid_dim_max_n_straps = 0
  nids%nid_dim_mmodes = 0
  nids%nid_dim_nballoon = 0
  nids%nid_dim_nbeam = 0
  nids%nid_dim_ncircuits = 0
  nids%nid_dim_ncoils = 0
  nids%nid_dim_ndelta_w = 0
  nids%nid_dim_necrf_src = 0
  nids%nid_dim_nefi_anom = 0
  nids%nid_dim_nefi_anom2 = 0
  nids%nid_dim_neqmom = 0
  nids%nid_dim_ngsc0 = 0
  nids%nid_dim_nicrf_src = 0
  nids%nid_dim_nky_anomth = 0
  nids%nid_dim_nky_mmm = 0
  nids%nid_dim_nlhrf_src = 0
  nids%nid_dim_nnewcomb = 0
  nids%nid_dim_npsmom = 0
  nids%nid_dim_nr = 0
  nids%nid_dm1_nrho = 0
  nids%nid_dim_nrho = 0
  nids%nid_dim_nrho_anom = 0
  nids%nid_dim_nrho_anom2 = 0
  nids%nid_dm1_nrho_anomth = 0
  nids%nid_dim_nrho_anomth = 0
  nids%nid_dm1_nrho_ecrf = 0
  nids%nid_dim_nrho_ecrf = 0
  nids%nid_dim_nrho_eq = 0
  nids%nid_dim_nrho_eq_geo = 0
  nids%nid_dm1_nrho_fus = 0
  nids%nid_dim_nrho_fus = 0
  nids%nid_dm1_nrho_gas = 0
  nids%nid_dim_nrho_gas = 0
  nids%nid_dm1_nrho_icrf = 0
  nids%nid_dim_nrho_icrf = 0
  nids%nid_dm1_nrho_lhrf = 0
  nids%nid_dim_nrho_lhrf = 0
  nids%nid_dim_nrho_lmhd = 0
  nids%nid_dm1_nrho_mist = 0
  nids%nid_dim_nrho_mist = 0
  nids%nid_dm1_nrho_nbi = 0
  nids%nid_dim_nrho_nbi = 0
  nids%nid_dm1_nrho_pel = 0
  nids%nid_dim_nrho_pel = 0
  nids%nid_dm1_nrho_rad = 0
  nids%nid_dim_nrho_rad = 0
  nids%nid_dm1_nrho_rw = 0
  nids%nid_dim_nrho_rw = 0
  nids%nid_dim_nr_rip = 0
  nids%nid_dim_nspec_all = 0
  nids%nid_dp1_nspec_all = 0
  nids%nid_dim_nspec_alla = 0
  nids%nid_dp1_nspec_alla = 0
  nids%nid_dim_nspec_beam = 0
  nids%nid_dim_nspec_fusion = 0
  nids%nid_dim_nspec_gas = 0
  nids%nid_dim_nspec_imp0 = 0
  nids%nid_dim_nspec_impi = 0
  nids%nid_dim_nspec_rfmin = 0
  nids%nid_dim_nspec_th = 0
  nids%nid_dp1_nspec_th = 0
  nids%nid_dim_nspec_tha = 0
  nids%nid_dp1_nspec_tha = 0
  nids%nid_dim_nth_eq = 0
  nids%nid_dim_ntrcat = 0
  nids%nid_dim_num_rzlim = 0
  nids%nid_dim_nxs = 0
  nids%nid_dim_nz = 0
  nids%nid_dim_nz_rip = 0
  nids%nid_dim_n_d_einj_standard = 0
  nids%nid_dim_n_h_einj_standard = 0
  nids%nid_dim_n_t_einj_standard = 0
  nids%nid_clen_32 = 0
  nids%nid_clen_256 = 0
  nids%nid_clen_80 = 0

  iout = SWIM_err_out_file

  iucount = 0

  iwarn = 0

  !------------------------------------
  ! reload equilibrium and other data in xplasma, with checks, as needed...
  ! ...hand maintained code orchestrates:
  !     0. plasma_state_empty_alloc; update ps_active_dims(...)
  !     1. compute new_hash_codes
  !     2. xplasma update of equilibrium as needed based on hash codes
  !     3. xplasma update of data and profiles as needed
  !     4. "zonwt" update
  !     5. update of derived quantities in xplasma.

  CALL ps_xload_check(ss,iout,1,ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) " ?ps_update_file_write: error in ps_xload_check."
     ierr = 9999
     RETURN
  ENDIF

  !------------------------------------
  ! open the updates file...

  status = nf90_create(trim(filename),NF90_CLOBBER,icdf)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?ps_update_file_write: file create failure: "'//trim(filename)//'".'
     ierr=9999
     RETURN
  ENDIF

  status = nf90_def_var(icdf,"ps_partial_update",NF90_INT, varid=pvarid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?ps_update_file_write: "ps_partial_update" variable definition error."'
     ierr=9999
     RETURN
  ENDIF

  !------------------------------------
  iertot = 0

  status = nf90_def_dim(icdf,"ps_clen_32",32,dimid)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " error defining dimension: ps_clen_32"
     CALL ncerr(status)
     iertot = iertot + 1
  ELSE
     nids%nid_clen_32 = dimid
  ENDIF

  status = nf90_def_dim(icdf,"ps_clen_256",256,dimid)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " error defining dimension: ps_clen_256"
     CALL ncerr(status)
     iertot = iertot + 1
  ELSE
     nids%nid_clen_256 = dimid
  ENDIF

  status = nf90_def_dim(icdf,"ps_clen_80",80,dimid)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " error defining dimension: ps_clen_80"
     CALL ncerr(status)
     iertot = iertot + 1
  ELSE
     nids%nid_clen_80 = dimid
  ENDIF

  IF(iertot.GT.0) THEN
     WRITE(iout,*) ' ?ps_update_file_write: character length dimension error.'
     ierr=9999
     RETURN
  ENDIF

  idims(1) = nids%nid_clen_32
  status = nf90_def_var(icdf,"version_id",NF90_CHAR, &
           dimids=idims(1:1), varid=vvarid)
  IF(status.NE.NF90_NOERR) THEN
     write(iout,*) " error defining: version_id character string."
     call ncerr(status)
     iertot = iertot + 1
  ENDIF

  !---------------------------------------------
  ! call subroutines to define data and arrays...

  call ps_update_file_write_def_0(ss,icdf,cclist,nids,iout,iwarn,iertot)
  call ps_update_file_write_def_1(ss,icdf,cclist,nids,iout,iwarn,iertot)
  call ps_update_file_write_def_2(ss,icdf,cclist,nids,iout,iwarn,iertot)
  call ps_update_file_write_def_3(ss,icdf,cclist,nids,iout,iwarn,iertot)
  call ps_update_file_write_def_4(ss,icdf,cclist,nids,iout,iwarn,iertot)
  call ps_update_file_write_def_5(ss,icdf,cclist,nids,iout,iwarn,iertot)
  call ps_update_file_write_def_6(ss,icdf,cclist,nids,iout,iwarn,iertot)
  call ps_update_file_write_def_7(ss,icdf,cclist,nids,iout,iwarn,iertot)
  call ps_update_file_write_def_8(ss,icdf,cclist,nids,iout,iwarn,iertot)
  call ps_update_file_write_def_9(ss,icdf,cclist,nids,iout,iwarn,iertot)

  IF(iertot.GT.0) THEN
     WRITE(iout,*) " ?ps_update_file_write: error defining NetCDF variables."
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
     WRITE(iout,*) " ?ps_update_file_write: nf90_enddef error"
     CALL ncerr(status)
     ierr = 9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  iertot = 0

  status = nf90_put_var(icdf,vvarid,ps_version_id)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?ps_update_file_write: error storing ps_version_id"
     CALL ncerr(status)
     ierr = 9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  ival = 1
  status = nf90_put_var(icdf,pvarid,ival)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?ps_update_file_write: error storing ps_partial_update marker"
     CALL ncerr(status)
     ierr = 9999
     status = nf90_close(icdf)
     RETURN
  ENDIF
  call ps_update_file_write_0(ss,icdf,cclist,iucount,iout,iwarn,iertot)
  call ps_update_file_write_1(ss,icdf,cclist,iucount,iout,iwarn,iertot)
  call ps_update_file_write_2(ss,icdf,cclist,iucount,iout,iwarn,iertot)
  call ps_update_file_write_3(ss,icdf,cclist,iucount,iout,iwarn,iertot)
  call ps_update_file_write_4(ss,icdf,cclist,iucount,iout,iwarn,iertot)
  call ps_update_file_write_5(ss,icdf,cclist,iucount,iout,iwarn,iertot)
  call ps_update_file_write_6(ss,icdf,cclist,iucount,iout,iwarn,iertot)
  call ps_update_file_write_7(ss,icdf,cclist,iucount,iout,iwarn,iertot)
  call ps_update_file_write_8(ss,icdf,cclist,iucount,iout,iwarn,iertot)
  call ps_update_file_write_9(ss,icdf,cclist,iucount,iout,iwarn,iertot)


  status = nf90_close(icdf)

  IF(iertot.GT.0) THEN
     WRITE(iout,*) " ?ps_update_file_write: error storing NetCDF variables."
     ierr = 9999
     RETURN
  ELSE
     ierr = 0
     WRITE(iout,*) " *ps_update_file_write: normal exit, ",iucount, &
          " update elements written."
  ENDIF

  CONTAINS


     SUBROUTINE define_dim(dimid,dname,idval)
        INTEGER, intent(out) :: dimid
        CHARACTER*(*), INTENT(in) :: dname
        INTEGER, intent(in) :: idval

        status = nf90_def_dim(icdf,dname,idval,dimid)
        IF(status.NE.NF90_NOERR) THEN
           WRITE(iout,*) ' ?ps_update_file_write: error defining dimension "'// &
                 trim(dname)//'".'
           IERR=9999
        ENDIF
     END SUBROUTINE define_dim



     SUBROUTINE ncerr(status)
        INTEGER, INTENT(in) :: status

        nc_errmsg = NF90_strerror(status)
        write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
     END SUBROUTINE ncerr



     SUBROUTINE ccwarn(varnam,compnam)

        CHARACTER*(*), intent(in) :: varnam   ! name of variable or array
        CHARACTER*(*), intent(in) :: compnam  ! name of component

        if(iwarn.eq.0) then
           write(iout,*) ' '
           write(iout,*) ' %ps_update_file_write: modified state elements'
           write(iout,*) '  exist which were not written due to the "cclist"'
           write(iout,*) '  input variable which screens out elements from'
           write(iout,*) '  selected components: cclist = ',cclist,';'
           write(iout,*) '  element values of 1 indicate components that are'
           write(iout,*) '  enabled for write.'
           write(iout,*) ' '
        endif

        iwarn = iwarn + 1

        write(iout,*) ' %ps_update_file_write: modified: ss%'// &
             trim(varnam)//' ('//trim(compnam)//' component) NOT WRITTEN.'

     END SUBROUTINE ccwarn

END SUBROUTINE ps_update_file_write
