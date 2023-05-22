!-------------------------------------------
function scrXceptcon(x,zsign)
  !
  !  return an estimate of the distance of a point from a flux surface
  !  represented as a series of (R,Y) points
  !
  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: zero, one, two
  use mintrp
  implicit none
  real(fp) :: scrXceptcon
  real(fp), intent(in) :: x
  real(fp), intent(inout) :: zsign
  real(fp) :: dist
  real(fp) :: zsing
  real(fp) :: zcosg
  real(fp) :: zmu
  real(fp) :: dvdu
  real(fp) :: v
  real(fp) :: u
  real(fp) :: zb
  real(fp) :: za
  real(fp) :: zc
  real(fp) :: dvdu4
  real(fp) :: dvdu1
  real(fp) :: vloc4
  real(fp) :: uloc4
  real(fp) :: vloc1
  real(fp) :: uloc1
  real(fp) :: vtest
  real(fp) :: utest
  real(fp) :: zsinb
  real(fp) :: zcosb
  integer :: ii
  integer :: i
  real(fp) :: zd2save
  real(fp) :: zd1save
  integer :: ktp
  real(fp) :: zdot
  real(fp) :: zminus
  real(fp) :: rminus
  real(fp) :: zplus
  real(fp) :: rplus
  real(fp) :: zd
  integer :: inth
  real(fp) :: zdmin
  real(fp) :: ztest
  real(fp) :: rtest
  real(fp) :: scrYceptcon
  real(fp) :: y
  integer :: ith
  integer :: idbct
  !
  real(fp) :: rloc(4),zloc(4)
  !
  integer :: lunmsg
  logical :: idebug
  data idbct/0/
  !-----------------------------
  !
  !  evaluate y from x (from saved low order fit)
  !
  idebug=.false.
  call scrgetlun(lunmsg)
  !
  !dbg      if((ksurf.eq.45).and.(abs(r2-292.374).le.0.005).and.
  !dbg     >   (abs(z2+89.547).le.0.005)) then
  !dbg	idebug=.true.
  !dbg	idbct=idbct+1
  !dbg      else
  !dbg	idebug=.false.
  !dbg      endif
  !
  if(idbct.eq.1) then
    do ith=1,ntheta0
      write(lunmsg,*) ith,rin(ith,ksurf),zin(ith,ksurf)
    enddo
  endif
  !
  y=scrYceptcon(x,Rtest,Ztest)
  !
  !  first find nearest point on contour
  !
  zdmin=1.0e30_fp
  inth=ntheta0-1
  !
  !  note that rin(1,ksurf)=rin(ntheta(ksurf),ksurf) is the convention
  !            zin(1,ksurf)=zin(ntheta(ksurf),ksurf)
  !  for this representation of a closed contour
  !
  do ith=1,inth
    zd = (Rtest-rin(ith,ksurf))**2  + (Ztest-zin(ith,ksurf))**2
    if(zd.lt.zdmin) then
      kth=ith
      zdmin=zd
    endif
  enddo
  !
  !  now choose of the neighbouring two the nearest interval
  rplus=rin(kth+1,ksurf)
  zplus=zin(kth+1,ksurf)
  if(kth.eq.1) then
    rminus=rin(inth,ksurf)
    zminus=zin(inth,ksurf)
  else
    rminus=rin(kth-1,ksurf)
    zminus=zin(kth-1,ksurf)
  endif
  !
  !  direction vector going from kth-1 to kth+1
  !   dotted with (Rtest,Ztest)-(R(kth),Z(kth))
  !
  zdot=(Rtest-rin(kth,ksurf))*(rplus-rminus) + &
       (Ztest-zin(kth,ksurf))*(zplus-zminus)
  !
  !  if negative, choose the prior interval
  if(zdot.lt.zero) then
    kth=kth-1
    if(kth.eq.0) kth=inth
  endif
  !
  !  distances to endpoints of the chosen interval
  !
  ktp=kth+1
  zd1save=(Rtest-rin(kth,ksurf))**2+(Ztest-zin(kth,ksurf))**2
  zd2save=(Rtest-rin(ktp,ksurf))**2+(Ztest-zin(ktp,ksurf))**2
  !
  zd1save=sqrt(zd1save)
  zd2save=sqrt(zd2save)
  !
  !  store 4 nearby points -- describing 3 nearest contour segments with
  !  the 1 nearest one in the middle
  !
  do i=1,4
    ii=kth-2+i                     ! kth-1 thru kth+2
    if(ii.le.0) ii=ii+inth
    if(ii.gt.inth) ii=ii-inth
    rloc(i)=rin(ii,ksurf)
    zloc(i)=zin(ii,ksurf)
  enddo
  !
  if(idebug) then
    write(lunmsg,'('' kth='',i5)') kth
    do i=1,4
      write(lunmsg,1001) i,rloc(i),zloc(i)
1001  format(' i=',i5,' rloc(i)=',1pe13.6,' zloc(i)=',1pe13.6)
    enddo
  endif
  !
  !  rotate and translate to a metric equivalent coordinate system (u,v)
  !  with origin (rloc(2),zloc(2)) u direction (rloc(3)-rloc(2),zloc(3)-zloc(2))
  !
  zd=sqrt((rloc(3)-rloc(2))**2+(zloc(3)-zloc(2))**2)
  zcosb=(rloc(3)-rloc(2))/zd
  zsinb=(zloc(3)-zloc(2))/zd
  !
  utest= zcosb*(Rtest-rloc(2)) +zsinb*(Ztest-zloc(2))
  vtest=-zsinb*(Rtest-rloc(2)) +zcosb*(Ztest-zloc(2))
  !
  uloc1= zcosb*(rloc(1)-rloc(2)) +zsinb*(zloc(1)-zloc(2))
  vloc1=-zsinb*(rloc(1)-rloc(2)) +zcosb*(zloc(1)-zloc(2))
  !
  uloc4= zcosb*(rloc(4)-rloc(2)) +zsinb*(zloc(4)-zloc(2))
  vloc4=-zsinb*(rloc(4)-rloc(2)) +zcosb*(zloc(4)-zloc(2))
  !
  !  in (u,v) space the 4 nearby points are:
  !    (uloc1,vloc1), (0,0), (d,0), (uloc4,vloc4)
  !
  !  with uloc1 < 0 < d < uloc4, because the change of orientation per
  !  contour segment is assumed small.
  !
  !  so, form a cubic v(u) satisfying v(0)=0, v(d)=0,
  !      and v'(0) and v'(d) numerically evaluated
  !
  !  this is a form of Hermite interpolation
  !
  dvdu1=-vloc1/(zd-uloc1)
  dvdu4=vloc4/uloc4
  !
  !  v(u) = zA*u**3 + zB*u**2 + zC*u
  !
  zC=dvdu1
  zA=(dvdu4+dvdu1)/(zd*zd)
  zB=-(two*dvdu1+dvdu4)/zd
  !
  !  estimate u from ratio of distances from segment end point to
  !  test point; parameter runs from 0 to d
  !
  u=zd*zd1save/(zd1save+zd2save)
  v=u*(zC+u*(ZB+u*ZA))
  !
  !  and the derivative
  !
  dvdu=zC+two*u*(zB+1.5_fp*u*zA)
  !
  !  unit normal to curve in u,v space
  !
  zmu=sqrt(1.0D0+dvdu**2)
  zcosg=1.0D0/zmu
  zsing=dvdu/zmu
  !
  !  signed distance estimate, test point to point on curve
  !
  dist=-zsing*(utest-u) +zcosg*(vtest-v)
  !
  !  done
  !
  zsign=one
  scrXceptcon=dist
  !
  if(idebug) then
    write(lunmsg,1002) x,dist
1002 format('  scrXceptcon:  x=',1pe13.6,' dist=',1pe13.6)
  endif
  !
  return
end function scrXceptcon
