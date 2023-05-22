SUBROUTINE scrfunct1des(rmc,rms,zmc,zms,xpts,grc,grs, &
     gzc,gzs,gpts,fsq,r10,mmax)
  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: zero, half, one
  use scrunch_inc1
  use scrunch_inc2
  implicit none
  intrinsic DOT_PRODUCT
  real(fp) :: fac
  integer :: i
  real(fp) :: tcon
  real(fp), intent(in) :: r10
  real(fp), intent(out) :: fsq
  real(fp) :: t1
  real(fp) :: t2
  integer, intent(in) :: mmax
  integer :: m
  integer :: l
  integer :: j
  real(fp) :: denom
  real(fp), dimension(mpol) :: yc, ys
  real(fp), dimension(ntheta) :: gcon, gtt, r1, z1, rt1, zt1, arg, rcon, zcon
  real(fp), dimension(ntheta,mpol) :: cosa, sina
  real(fp), dimension(*), intent(in) :: rmc, rms, zmc, zms, xpts
  real(fp), dimension(*), intent(inout) :: grc, grs, gzc, gzs, gpts
  !
  !	FORCES: dW/dRmn = SUM(i)[ R(i) - RIN3D(i)]*cos(m*u[i]) ....
  !		dW/dZmn = SUM(i)[ Z(i) - ZIN3D(i)]*sin(m*u[i]) ....
  !		dW/du[i]=    rt(i)*[R(i)-RIN3D(i)] + zt(i)*[Z(i) - ZIN3D(i)]
  !	THE NORM ON THE ANGLE FORCE (dW/du[i]) FOLLOWS FROM NEWTON S
  !	LAW AND IS APPROXIMATELY GTT = RT**2 + ZT**2
  !
  denom = zero
  gnorm = zero
  specw = zero
  do j = 1,nthetax
    r1(j) = -rin3d(j,1)
    z1(j) = -zin3d(j,1)
    rcon(j) = zero
    zcon(j) = zero
    rt1(j)  = zero
    zt1(j)  = zero
  end do
  do l = 1,n2
    grc(l) = zero
  end do
  !**********************************************************************
  !       COMPUTE SPECTRAL WIDTH OF CURVE
  !**********************************************************************
  do m = 2,mmax
    t2 = (rmc(m)*rmc(m)+zmc(m)*zmc(m)+ &
         rms(m)*rms(m)+zms(m)*zms(m))*xmpq(m,1)
    denom = denom+t2
    specw = specw+xmpq(m,2)*t2
  end do
  specw = specw/denom
  !**********************************************************************
  !       COMPUTE CURVE AND CONSTRAINT FORCES
  !**********************************************************************
  do m = 1,mmax
    do l = 1,nthetax
      arg(l)    = dm1(m) * xpts(l)
      cosa(l,m) = cos(arg(l))
      sina(l,m) = sin(arg(l))
      gtt(l)  = rmc(m)*cosa(l,m) + rms(m)*sina(l,m)
      gcon(l) = zmc(m)*cosa(l,m) + zms(m)*sina(l,m)
      r1(l)   = r1(l)  + gtt(l)
      z1(l)   = z1(l)  + gcon(l)
      rt1(l)  = rt1(l) + dm1(m)*(rms(m)*cosa(l,m) - rmc(m)*sina(l,m))
      zt1(l)  = zt1(l) + dm1(m)*(zms(m)*cosa(l,m) - zmc(m)*sina(l,m))
      rcon(l) = rcon(l) + xmpq(m,4)*gtt(l)
      zcon(l) = zcon(l) + xmpq(m,4)*gcon(l)
    end do
  end do
  do l = 1,nthetax
    gtt(l)  = rt1(l)*rt1(l) + zt1(l)*zt1(l)
    gpts(l) = r1(l)*rt1(l)+z1(l)*zt1(l)
    gcon(l) = rcon(l)*rt1(l) + zcon(l)*zt1(l)
  end do
  t1 = -one
  do l = 1,nthetax
    t1 = max(t1,gtt(l))
  end do
  !**********************************************************************
  !       COMPUTE MEAN-SQUARE DEVIATION BETWEEN POINTS AND FIT
  !**********************************************************************
  fsq = zero
  do l = 1,nthetax
    gpts(l) = gpts(l)/t1
    fsq = fsq + half*(r1(l)**2 + z1(l)**2)
  end do
  fsq = sqrt(dnorm*fsq)/r10
  !**********************************************************************
  !       FILTER CONSTRAINT FORCE TO REMOVE ALIASING
  !**********************************************************************
  tcon = one/(t1*sqrt(one+xmpq(mmax,3)))
  do m = 2,mmax-1
    yc(m) = DOT_PRODUCT(cosa(1:nthetax,m),gcon(1:nthetax))*faccon(m)*tcon
    ys(m) = DOT_PRODUCT(sina(1:nthetax,m),gcon(1:nthetax))*faccon(m)*tcon
  end do
  do l = 1,nthetax
    gcon(l) = zero
  end do
  do m = 2,mmax-1
    do l = 1,nthetax
      gcon(l) = gcon(l) + yc(m)*cosa(l,m) + ys(m)*sina(l,m)
    end do
  end do
  !**********************************************************************
  !       ADD CURVE AND CONSTRAINT FORCES
  !**********************************************************************
  do m = 1,mmax
    do l = 1,nthetax
      rcon(l) = r1(l)  + gcon(l)*rt1(l)*xmpq(m,3)
      zcon(l) = z1(l)  + gcon(l)*zt1(l)*xmpq(m,3)
      grc(m)  = grc(m) + cosa(l,m)*rcon(l)
      if ((lasym.and. .not.lzakharov).or.m.eq.1) &
           gzc(m)  = gzc(m) + cosa(l,m)*zcon(l)
      if (lasym) grs(m)  = grs(m) + sina(l,m)*rcon(l)
      if (.not.lzakharov .or. m.le.2) gzs(m)  = gzs(m) + &
           sina(l,m)*zcon(l)
    end do
  end do
  !**********************************************************************
  !       COMPUTE m=1 CONSTRAINT (ZC(m=1)=RS(m=1)) to the Forces
  !**********************************************************************
  if(.not.lzakharov) then
    gzc(2) = grs(2) + gzc(2)
    grs(2) = gzc(2)
    grc(1) = half*grc(1)
  end if
 
  do i = 1,mpol4
    grc(i) = dnorm*grc(i)
  enddo
  do j=1,mmax
    gnorm = gnorm + grc(j)*grc(j) + gzc(j)*gzc(j) &
         + grs(j)*grs(j) + gzs(j)*gzs(j)
  end do
  gnorm = gnorm/r10**2
  do j = 1,nthetax
    gnorm = gnorm + dnorm*gpts(j)*gpts(j)
  end do
  do m = 2,mmax
    fac = one/(one + tcon*xmpq(m,3))
    grc(m) = grc(m)*fac
    gzc(m) = gzc(m)*fac
    grs(m) = grs(m)*fac
    gzs(m) = gzs(m)*fac
  end do

end SUBROUTINE scrfunct1des
