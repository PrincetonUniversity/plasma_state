SUBROUTINE scrprintdes
  use iso_c_binding, only: fp => c_double
  use scrunch_inc1
  use scrunch_inc2
  implicit none
  integer :: mn
  real(fp), dimension(nphi2,1) :: rmc, rms, zmc, zms
  real(fp), dimension(nphi2) :: integrate, argi
  if (ioflagc) write(lunmsg,30)
30 format('c  mb  nb    RmncB       RmnsB       ZmncB       ZmnsB')
  do mn=1,mpnt
    if (ioflagc) write(lunmsg,80) mn-1,0,xvec(mn),xvec(mpol+mn), &
         xvec(mpol2+mn),xvec(mpol3+mn)
    rmomb(mn,1)=xvec(mn)
    rmomb(mn,2)=xvec(mpol+mn)
    zmomb(mn,1)=xvec(mpol2+mn)
    zmomb(mn,2)=xvec(mpol3+mn)
  end do
80 format(i5,i4,4e12.4)
  return
end SUBROUTINE scrprintdes

