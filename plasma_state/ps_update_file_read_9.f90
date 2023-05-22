! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE ps_update_file_read_9(ss,icdf,iout,iucount,iertot)


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


  status = nf90_inq_varid(icdf,"velpphi_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velpphi_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velpphi_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velte_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velte_anom"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_glf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velte_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velte_glf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_gtcneo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velte_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velte_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_mmm71",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velte_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velte_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_nclass",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velte_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velte_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_neo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velte_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velte_neo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_paleo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velte_paleo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velte_paleo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velte_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velte_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velte_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_anom",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velti_anom)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velti_anom"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_glf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velti_glf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velti_glf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_gtcneo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velti_gtcneo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velti_gtcneo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_mmm71",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velti_mmm71)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velti_mmm71"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_nclass",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velti_nclass)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velti_nclass"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_neo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velti_neo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velti_neo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"velti_tglf",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%velti_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: velti_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vie_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vie_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vie_trans"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vie_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vie_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vie_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vmo_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vmo_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vmo_trans"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vmo_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vmo_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vmo_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vn_trans",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vn_trans)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vn_trans"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vn_transcat",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vn_transcat)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vn_transcat"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vol",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vol)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vol"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vphi0_av",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vphi0_av)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vphi0_av"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vpol_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vpol_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vpol_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vpol_inmp",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vpol_inmp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vpol_inmp"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vpol_is_input",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vpol_is_input)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vpol_is_input"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vpol_omp",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vpol_omp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vpol_omp"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"VRep_coil_count",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%VRep_coil_count)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: VRep_coil_count"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Vrep_coil_spacing",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Vrep_coil_spacing)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Vrep_coil_spacing"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Vsize_coil",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Vsize_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Vsize_coil"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vsur",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vsur)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vsur"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"VSUR_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%VSUR_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: VSUR_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vtor_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vtor_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vtor_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vtor_inmp",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vtor_inmp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vtor_inmp"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vtor_is_input",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vtor_is_input)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vtor_is_input"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"vtor_omp",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%vtor_omp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: vtor_omp"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"V_loop",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%V_loop)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: V_loop"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"v_pars",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%v_pars)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: v_pars"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"v_pers",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%v_pers)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: v_pers"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"wt_nphi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%wt_nphi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: wt_nphi"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"wt_nphi_abs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%wt_nphi_abs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: wt_nphi_abs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"wt_nphi_ext",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%wt_nphi_ext)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: wt_nphi_ext"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"xRjcos_momeq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%xRjcos_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: xRjcos_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"xRjsin_momeq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%xRjsin_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: xRjsin_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"xs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%xs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: xs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"xs_name",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL cchk_zero(ss%xs_name)
     status = nf90_get_var(icdf,varid,ss%xs_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: xs_name"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL cchk_hash("xs_name",ss%xs_name)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"xZjcos_momeq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%xZjcos_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: xZjcos_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"xZjsin_momeq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%xZjsin_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: xZjsin_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z0max",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z0max)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z0max"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z0_momeq",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z0_momeq)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z0_momeq"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zbap",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Zbap)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Zbap"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zbsc",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Zbsc)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Zbsc"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zeff",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Zeff)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Zeff"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"ZEFF_Data_Info",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%ZEFF_Data_Info)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ZEFF_Data_Info"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zeff_fi",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Zeff_fi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Zeff_fi"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zeff_th",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Zeff_th)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Zeff_th"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zimp1",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Zimp1)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Zimp1"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zimp2",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Zimp2)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Zimp2"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"zlim",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%zlim)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: zlim"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Zloc_coil",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Zloc_coil)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Zloc_coil"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_ant",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z_ant)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_ant"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_antgeo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z_antgeo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_antgeo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_axis",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z_axis)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_axis"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_EC_launch",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z_EC_launch)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_EC_launch"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_geo",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z_geo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_geo"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_grid",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%Z_grid)
     status = nf90_get_var(icdf,varid,ss%Z_grid)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_grid"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("Z_grid",ss%Z_grid)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_max_box",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z_max_box)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_max_box"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_max_lcfs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z_max_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_max_lcfs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_midp",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z_midp)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_midp"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_mid_ant",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z_mid_ant)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_mid_ant"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_min_box",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z_min_box)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_min_box"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_min_lcfs",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z_min_lcfs)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_min_lcfs"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_ripple",varid)
  IF(status.EQ.NF90_NOERR) THEN
     CALL rchk_zero(ss%Z_ripple)
     status = nf90_get_var(icdf,varid,ss%Z_ripple)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_ripple"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
     IF(.NOT.izero) THEN
        CALL rchk_hash("Z_ripple",ss%Z_ripple)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_surfMax",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z_surfMax)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_surfMax"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Z_surfMin",varid)
  IF(status.EQ.NF90_NOERR) THEN
     status = nf90_get_var(icdf,varid,ss%Z_surfMin)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?ps_update_file_read: error reading variable: Z_surfMin"
        CALL ncerr(status)
        iertot = iertot + 1
        izero=.TRUE.
     ELSE
        iucount = iucount + 1
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

END SUBROUTINE ps_update_file_read_9
