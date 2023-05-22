SUBROUTINE scrgetangldes(rval,zval,xpts,rcenter,zcenter)
  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: zero, twopi
  use scrunch_inc1
  implicit none
  real(fp) :: del_ang
  real(fp) :: denom
  real(fp) :: dnum
  real(fp) :: yc
  real(fp) :: xc
  integer :: iterate
  integer :: j
  integer :: i
  real(fp), dimension(ntheta,*), intent(in) :: rval, zval
  real(fp), dimension(ntheta,*), intent(out) :: xpts
  real(fp), dimension(*), intent(in) :: rcenter, zcenter
  real(fp), dimension(nphi) :: rcos, rsin, zcos, zsin, phi_ang
  !**********************************************************************
  !       Compute angle offset consistent with constraint Z1n = Z1,-n
  !       Note: This is done iteratively, since elongation is unknown
  !**********************************************************************
  do i = 1,nphi
    do j = 1,nthetax
      xpts(j,i) = twopi*(j-1)/DBLE(nthetax)
    end do
  end do
  do iterate = 1,5
    do i = 1,nphi
      rcos(i) = zero
      rsin(i) = zero
      zcos(i) = zero
      zsin(i) = zero
      do j = 1,nthetax
        xc = rval(j,i) - rcenter(i)
        yc = zval(j,i) - zcenter(i)
        rcos(i) = rcos(i) + cos(xpts(j,i))*xc
        rsin(i) = rsin(i) + sin(xpts(j,i))*xc
        zcos(i) = zcos(i) + cos(xpts(j,i))*yc
        zsin(i) = zsin(i) + sin(xpts(j,i))*yc
      end do
    end do
    !**********************************************************************
    !       Compute new angles starting from offset phi_ang(i)
    !**********************************************************************
    dnum = zero
    denom= zero
    do i = 1,nphi
      dnum = dnum + zsin(i)
      denom= denom+ rcos(i)
    enddo
    elongate = dnum/denom
    del_ang = zero
    do i = 1,nphi
      phi_ang(i) = atan2(elongate*zcos(i)-rsin(i), &
           elongate*zsin(i)+rcos(i))
      del_ang = max(del_ang,abs(phi_ang(i)))
      do j = 1,nthetax
        xpts(j,i) = xpts(j,i) + phi_ang(i)
      end do
    end do
    if(del_ang.lt.0.02_fp) go to 40
  end do
40 if (ioflagc) write(lunmsg,*)' Average elongation = ',elongate
end SUBROUTINE scrgetangldes
