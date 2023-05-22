SUBROUTINE scrgctnorm(r,y,n,i,vec,inorm)
  !
  !  return normal vector, normalized to unit length if inorm=1
  !
  !  inputs:  (R(j),Y(j)), j going from 1 to n, describes a closed contour,
  !           with periodicity, i.e. one can think of
  !                  (R(n+1),Y(n+1)) = (R(1),Y(1))
  !           i     -- the point where the normal vector is desired
  !           inorm -- flags whether output vector is to be normalized
  !
  !  output:  vec(2) gives the numerically evaluated vector normal to the
  !           contour at point i.
  !
  use iso_c_binding, only: fp => c_double
  implicit none
  real(fp) :: zd
  integer :: inorm
  real(fp) :: zdely
  real(fp) :: zdelr
  integer :: imins
  integer :: i
  integer :: iplus
  integer :: n
  real(fp), dimension(n) :: r, y
  real(fp), dimension(2) :: vec
  !
  !-----------------------------
  !
  iplus=i+1
  if(iplus.gt.n) iplus=iplus-n
  imins=i-1
  if(imins.lt.1) imins=imins+n
  !
  zdelr=r(iplus)-r(imins)
  zdely=y(iplus)-y(imins)
  !
  vec(1)=zdely
  vec(2)=-zdelr
  !
  if(inorm.eq.1) then
    zd=sqrt(vec(1)**2+vec(2)**2)
    vec(1)=vec(1)/zd
    vec(2)=vec(2)/zd
  endif
  !
  return
end SUBROUTINE scrgctnorm
