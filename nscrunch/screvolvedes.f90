SUBROUTINE screvolvedes(g11)
  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: half, one
  use scrunch_inc1
  implicit none
  integer :: i
  real(fp) :: fac
  real(fp) :: b1
  real(fp) :: otav
  real(fp) :: dtau
  real(fp) :: g11
  real(fp) :: ftest
  real(fp) :: bmax

  bmax = 0.15_fp
  ftest=gnorm/g11
  dtau=abs(one-ftest)
  g11=gnorm
  otav=dtau/delt
  dtau=delt*otav+.001_fp
  if(dtau.gt.bmax)dtau=bmax
  b1=one-half*dtau
  fac=one/(one+half*dtau)
  do i=1,n2
    xdot(i)=fac*(xdot(i)*b1-delt*gvec(i))
    xvec(i)=xvec(i)+xdot(i)*delt
  end do
  return
end SUBROUTINE screvolvedes
