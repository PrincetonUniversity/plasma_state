SUBROUTINE scrorderdes(rval,zval,xaxis,yaxis,ier)
  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: zero, half, twopi
  use scrunch_inc1
  implicit none
  real(fp), intent(inout), dimension(*) :: rval
  real(fp), intent(inout), dimension(*) :: zval
  real(fp), intent(in) :: xaxis
  real(fp), intent(in) :: yaxis
  integer, intent(inout) :: ier
  real(fp) :: dy
  real(fp) :: dx
  real(fp) :: y
  real(fp) :: x
  real(fp) :: residue
  real(fp) :: save_z
  real(fp) :: save_r
  integer :: next
  integer :: j
  real(fp) :: shortest
  integer :: i1
  integer :: ip1
  integer :: i
  real(fp) :: old_dist
  real(fp) :: new_dist
  real(fp), dimension(ntheta) :: tempr
  real(fp), dimension(ntheta) :: tempz
  !**********************************************************************
  !       Program ORDER : orders points on a magnetic surface at a
  !       fixed toroidal plane and assigns right-handed circulation
  !       around flux surface. XAXIS, YAXIS:  Polar-type axis (must lie
  !       inside curve to check sign of rotation)
  !**********************************************************************
  old_dist = 1.e20_fp
  do i = 1,nthetax-1
    ip1 = i + 1
    i1 = i
    shortest = 1.e20_fp
15  continue
    do j = ip1,nthetax
      if(i1.gt.1)old_dist = (rval(i1-1)-rval(j))**2 &
           + (zval(i1-1)-zval(j))**2
      new_dist =(rval(i1)-rval(j))**2 + (zval(i1)-zval(j))**2
      if((new_dist.le.old_dist).and. &
           (new_dist.lt.shortest)) then
        next = j
        shortest = new_dist
      end if
    end do
    !**********************************************************************
    !       Swap nearest point (next) with current point (ip1)
    !**********************************************************************
    if(shortest.ge.1.e10_fp) then
      save_r = rval(i-1)
      rval(i-1) = rval(i)
      rval(i)= save_r
      save_z = zval(i-1)
      zval(i-1) = zval(i)
      zval(i)= save_z
      i1 = i1 - 1
      ip1 = ip1 - 1
      go to 15
    end if
    save_r = rval(ip1)
    rval(ip1) = rval(next)
    rval(next)= save_r
    save_z = zval(ip1)
    zval(ip1) = zval(next)
    zval(next)= save_z
  end do
  !**********************************************************************
  !       Check that xaxis,yaxis is inside surface and
  !       ascertain that the angle rotates counterclockwise
  !       using Cauchy's theorem in "complex"-plane
  !**********************************************************************
  residue = zero
  do i = 1,nthetax-1
    x = half*(rval(i)+rval(i+1)) - xaxis
    y = half*(zval(i)+zval(i+1)) - yaxis
    dx= rval(i+1) - rval(i)
    dy= zval(i+1) - zval(i)
    residue = residue + (x*dy - y*dx)/(x**2 + y**2 + 1.e-10_fp)
  end do
  x = half*(rval(1)+rval(nthetax)) - xaxis
  y = half*(zval(1)+zval(nthetax)) - yaxis
  dx= rval(1) - rval(nthetax)
  dy= zval(1) - zval(nthetax)
  residue = residue + (x*dy - y*dx)/(x**2 + y**2 + 1.e-10_fp)

  if( residue .lt.(-.9_fp*twopi))then
    do i = 2,nthetax
      j = nthetax - i + 2
      tempr(i) = rval(j)
      tempz(i) = zval(j)
    end do
    do i = 2,nthetax
      rval(i) = tempr(i)
      zval(i) = tempz(i)
    end do
  else if( abs(residue) .lt. (.9_fp*twopi) )then
    write(lunmsg,*)' The magnetic axis is not enclosed by boundary '
    ier=11
    return
  end if

  return
end SUBROUTINE scrorderdes
