SUBROUTINE scrSINCOS(ZTHETA,JANG,SNTHTK,CSTHTK)
  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: zero, one
  implicit none
  real(fp), intent(in) :: ztheta
  integer, intent(in) :: jang
  real(fp), dimension(jang), intent(out) :: csthtk
  real(fp), dimension(jang), intent(out) :: snthtk
  real(fp) :: zcosp
  real(fp) :: zsinp
  real(fp) :: zthetap
  integer :: i
 
  !
  !	THIS SUBROUTINE CALCULATES:
  !		SIN(ZTHETA)
  !		COS(ZTHETA)
  !		SIN(2*ZTHETA)
  !		COS(2*ZTHETA)
  !	ETC., UP TO JANG*ZTHETA
  !
  !
  !		ZTHETA IS INPUT ANGLE
  !		JANG IS HIGHEST N*ZTHETA TO CALCULATE
  !		SNTHTK IS THE ARRAY CONTAINING SIN(N*ZTHETA)
  !		CSTHTK IS THE ARRAY CONTAINING COS(N*ZTHETA)
  !
  !
  !  LOCAL MEMORY (DMC 6 JUL 1994)
  !
  DATA ZTHETAP/zero/
  DATA ZSINP/zero/
  DATA ZCOSP/one/
  !
  SAVE ZTHETAP,ZSINP,ZCOSP
  !
  !--------------------------------------------------------------------
  !
  !  DMC -- USE LOCAL MEMORY FOR SPEED
  !
  IF(ZTHETA.NE.ZTHETAP) THEN
    !
    !  EVALUATE SIN,COS
    !
    SNTHTK(1)=SIN(ZTHETA)
    CSTHTK(1)=COS(ZTHETA)
    ZTHETAP=ZTHETA
    ZSINP=SNTHTK(1)
    ZCOSP=CSTHTK(1)
  ELSE
    !
    !  REUSE PREVIOUS RESULTS
    !
    SNTHTK(1)=ZSINP
    CSTHTK(1)=ZCOSP
  ENDIF
  !
  DO I=2,JANG
    SNTHTK(I)=SNTHTK(I-1)*CSTHTK(1)+CSTHTK(I-1)*SNTHTK(1)
    CSTHTK(I)=CSTHTK(I-1)*CSTHTK(1)-SNTHTK(I-1)*SNTHTK(1)
  END DO
  !
  RETURN
END SUBROUTINE scrSINCOS
