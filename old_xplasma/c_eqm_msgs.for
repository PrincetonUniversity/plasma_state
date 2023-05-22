 
 
!----------------------------------------------------------
! cface  -- generated (f90 fixed form) C interface to:  EQM_MSGS
      SUBROUTINE  C_EQM_MSGS(
     > ILUN,C_ZFILE_LEN,C_ZFILE,IERR)
 
      use iso_c_binding, only: c_char
      external EQM_MSGS
 
! argument declarations
      INTEGER ILUN
 ! scalar char*(*) type, input only:
      INTEGER C_ZFILE_LEN
      character(kind=c_char) C_ZFILE(C_ZFILE_LEN)
 
      INTEGER IERR
 
! local (automatic array) declarations
      CHARACTER(C_ZFILE_LEN) :: ZFILE_act
 
! executable code:  copy for input
 
      call cstring(ZFILE_act, C_ZFILE,'2F')
! call to original routine:  EQM_MSGS
 
      CALL EQM_MSGS(
     > ILUN,ZFILE_act,IERR)
 
! copy back outputs if modified.
 
! exit
      return
      end
