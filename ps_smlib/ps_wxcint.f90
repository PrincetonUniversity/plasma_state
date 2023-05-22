!******************** START FILE WXCINT.FOR ; GROUP FILTR6 ******************
!
!  WXCINT
!
!  COMPUTE C*INTEGRAL OF WEIGHTING FUNCTION FROM X1 TO X2
!
!  DENOMINATOR IN WEIGHTED AVERAGE FORMULA
!
!  W(T)=1-ABS((XCEN-T)/DELTA)   XCEN-DELTA .LE. T .LE. XCEN+DELTA
!
!  TRIANGULAR SHAPED WEIGHTING FUNCTION CENTERED AT XCEN
!    X1 SHOULD BE .LT. X2 AND X1 AND X2 SHOULD
!    BE WITHIN DELTA OF XCEN
!
function ps_wxcint(X1,X2,C,XCEN,DELTA)
  use iso_c_binding, only: fp => c_double
  implicit none
  REAL(fp) :: ps_wxcint
  ! Arguments
  real(fp), intent(in) :: x1, x2, c
  real(fp), intent(in) :: xcen
  real(fp), intent(in) :: delta
  ! Local variables
  real(fp) :: xa, xb, zdi
  !
  ps_wxcint=C
  XA=max(-DELTA,(X1-XCEN))
  XB=min(DELTA,(X2-XCEN))
  IF((XA.GT.DELTA).OR.(XB.LT.-DELTA)) GO TO 1000
  IF(XB.LT.XA) GO TO 1000
  !
  !  NUMERICS CHECK; DELTA <<< X1
  IF(XCEN.EQ.(XCEN+DELTA)) RETURN
  !
  ZDI=1.0_fp/DELTA
  IF((XA.GT.0.0).OR.(XB.LT.0.0)) GO TO 100
  !
  ps_wxcint=C*ZDI*((XB-XA)-0.5_fp*(XA*XA+XB*XB)*ZDI)
  RETURN
  !
100 CONTINUE
  IF(XA.GT.0.0) ps_wxcint=C*ZDI*((XB-XA)-0.5_fp*(XA+XB)*(XB-XA)*ZDI)
  IF(XB.LT.0.0) ps_wxcint=C*ZDI*((XB-XA)-0.5_fp*(XA+XB)*(XA-XB)*ZDI)
  RETURN
  !
1000 CONTINUE
  write(6,9000)
9000 FORMAT(' SUBROUTINE WXCINT FROM R8FILFN6 FROM R8FILTR6.FOR:'/ &
          ' ILLEGAL INTEGRATION LIMITS')
  call flush(6)
  stop
end function ps_wxcint
