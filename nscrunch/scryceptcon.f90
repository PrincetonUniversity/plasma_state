function scrYceptcon(x,Rtest,Ztest)
  !
  !  evaluate y(x):  theta line fit in some reference frame
  !  also return the (x,y) rotated back to (R,Z) space.
  !
  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: zero, two
  use mintrp
  implicit none
  real(fp) :: scrYceptcon
  real(fp), intent(in)  :: x
  real(fp), intent(out) :: rtest
  real(fp), intent(out) :: ztest
  real(fp) :: zdydx
  real(fp) :: zdx
  real(fp) :: zref
  real(fp) :: rref
  real(fp) :: zsin
  real(fp) :: zcos
  real(fp) :: y
  real(fp) :: zxx
  real(fp) :: zx
  !
  !  for spvec (moption=3)...
  !
  integer :: ict(3)
  integer :: iwarn
  integer :: ier
  data ict/1,0,0/
  !
  !---------------------------------------------
  !
  zx=max(xspl(1),min(xmax,x))
  if(zx.gt.xbrk) then
    zxx=zx-xbrk
    y=d2+zxx*(c2+zxx*(b2+zxx*a2))
    zcos=zcosa2
    zsin=zsina2
    rref=r1
    zref=z1
    zx=zxx
  else
    if(moption.eq.2) then
      y=d+zx*(c+zx*(b+zx*a))
    else
      call spvec(ict,1,zx,1,y,5,xpkg,yspl,iwarn,ier)
      if(ier.ne.0) then
        call scraborter(6,'?scryceptcon: spvec error.')
        return
      end if
    end if
    zcos=zcosa
    zsin=zsina
    rref=r0
    zref=z0
  end if
  !
  !  linear extrapolation
  !
  if(x.gt.xmax) then
    zdx=x-xmax
    if(xmax.gt.xbrk) then
      zx=xmax-xbrk
      zdydx= c2 + two*zx*(b2 + 1.5_fp*zx*a2)
    else
      zx=x
      zdydx= c + two*xmax*(b + 1.5_fp*xmax*a)
    end if
  else
    zdx=zero
    zdydx=zero
  end if
  !
  y=y+zdx*zdydx
  scrYceptcon=y
  !
  !  transform to (R,Z)
  !
  Rtest = rref + zcos*(zx+zdx) - zsin*y
  Ztest = zref + zsin*(zx+zdx) + zcos*y
  !
  return
end function scrYceptcon
