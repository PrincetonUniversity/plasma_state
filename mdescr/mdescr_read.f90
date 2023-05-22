! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)



SUBROUTINE mdescr_read(filename,ierr)

  ! read "mdescr" namelist -- generate a temporary filename

  character*(*), intent(in) :: filename
  integer, intent(out) :: ierr

  character*120 tmp_filename
  integer :: ilen

  call tmpfile_d('mdescr',tmp_filename,ilen)

  call mdescr_read_tmpnam(filename,tmp_filename,ierr)

END SUBROUTINE mdescr_read

SUBROUTINE mdescr_read_tmpnam(filename,tmp_filename,ierr)

  ! read "mdescr" namelist -- temporary filename provided for copy
  ! with comments stripped out

  USE mdescr_data
  implicit NONE

  character*(*), intent(in) :: filename
  character*(*), intent(in) :: tmp_filename
  integer, intent(out) :: ierr

  !-------------------------------
  integer :: ilun_namel,iertmp
  !-------------------------------

  ierr = 0

  ! OK: open namelist file, strip comments.
  !     namelist to be read on ilun_namel after this call:

  CALL ps_namel_strip(filename,tmp_filename,ilun_namel,iout,ierr)

  IF(ierr.NE.0) THEN
     WRITE(iout,*) ' ?namelist file OPEN or READ error: '//trim(filename)
     RETURN
  ENDIF

  READ(UNIT=ilun_namel,NML=mdescr)

  CLOSE(UNIT=ilun_namel,status="DELETE")


  CALL ps_namrd_ilist_chk("circuit_name", &
       ps_max_static,circuit_name,NCIRCUITS,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  CALL ps_namrd_ilist_chk("coil_name", &
       ps_max_static,coil_name,NCOILS,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  CALL ps_namrd_ilist_chk("nbi_src_name", &
       ps_max_static,nbi_src_name,NBEAM,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  CALL ps_namrd_ilist_chk("icrf_src_name", &
       ps_max_static_2d,icrf_src_name,NICRF_SRC,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  CALL ps_namrd_ilist_chk("ecrf_src_name", &
       ps_max_static,ecrf_src_name,NECRF_SRC,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  CALL ps_namrd_ilist_chk("lhrf_src_name", &
       ps_max_static,lhrf_src_name,NLHRF_SRC,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  IF(ierr.NE.0) THEN
     WRITE(iout,*) " ?mdescr namelist list check errors after read."
     RETURN
  ENDIF

END SUBROUTINE mdescr_read_tmpnam
