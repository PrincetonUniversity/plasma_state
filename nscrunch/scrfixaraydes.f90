SUBROUTINE scrfixaraydes
  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: zero, one, two
  use scrunch_inc1
  implicit none
  integer :: m
  integer :: n
  integer :: nn0
  integer :: ntor
  integer :: l
  !**********************************************************************
  !       This routine store toroidal and poloidal mode number arrays
  !**********************************************************************
  l = 0
  ntor = max(1,nphi-1)
  nn0 = 1 - (ntor+1)/2
  do n=1,ntor
    nn(n) = (nn0 + (n-1))*nfp
  end do
  do 40 m=1,mpolx
    mm(m)=m-1
    do 40 n=1,ntor
      if(mm(m).eq.0 .and. nn(n) .lt. 0) go to 40
      l=l+1
30    continue
      m1(l)=mm(m)
      n1(l)=nn(n)
40 continue
  mpnt=l
  dnorm=two/nthetax
  do m=1,mpolx
    dm1(m)=m-1
    faccon(m) = .125_fp*dnorm/(one+dm1(m))**pexp
    if(m.eq.1) cycle
    xmpq(m,1) = dm1(m)**pexp
    xmpq(m,2) = dm1(m)**qexp
    xmpq(m,4) = xmpq(m,1)
    if(m.le.2) xmpq(m,4) = zero
    xmpq(m,3) = sqrt(xmpq(m,4))
  end do
  return
end subroutine scrfixaraydes
