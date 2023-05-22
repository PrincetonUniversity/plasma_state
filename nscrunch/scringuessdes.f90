SUBROUTINE scringuessdes (ier)
  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: zero
  use scrunch_inc1
  implicit none
  integer :: ier
  integer :: j
  integer :: i
  !**********************************************************************
  !       This subroutine obtains initial guesses for the centroid at each
  !       toroidal plane. By default, the polar axis is set equal to this
  !       centroid.  It is imperative that the polar axis be enclosed by
  !       the surface (otherwise the computed theta angles will not span
  !       [0,2pi]). For certain complex cross-sections, this simple estimate
  !       of the polar axis may fail, and the user must supply values for
  !       raxis3d(i),zaxis3d(i).  In addition, for non-starlike domains, the
  !       points along the curve must be monitonically increasing as a
  !       function of the arclength from any fixed point on the curve. This
  !       ordering is attempted by the subroutine ORDER, but may fail in
  !       general if too few theta points are used.
  !**********************************************************************
  !       COMPUTE CENTROID
  !**********************************************************************
  do i = 1,nphi
    r0n(i) = zero
    z0n(i) = zero
    do j = 1,nthetax
      r0n(i) = r0n(i)+rin3d(j,i)/DBLE(nthetax)
      z0n(i) = z0n(i)+zin3d(j,i)/DBLE(nthetax)
    end do
    raxis3d(i) = r0n(i)
    zaxis3d(i) = z0n(i)
  end do
  !**********************************************************************
  !       ORDER POINTS ON FLUX SURFACE AT EACH TOROIDAL ANGLES
  !**********************************************************************
  if (ioflagc) write(lunmsg,*)'ORDERING SURFACE POINTS'
  DO i = 1,nphi
    CALL scrorderdes(rin3d(1,i),zin3d(1,i), &
         raxis3d(i),zaxis3d(i),ier)
  END DO
  !**********************************************************************
  !       COMPUTE OPTIMUM ANGLE OFFSETS FOR M=1 MODES
  !**********************************************************************
  CALL scrgetangldes(rin3d,zin3d,angle,r0n,z0n)
end SUBROUTINE scringuessdes
