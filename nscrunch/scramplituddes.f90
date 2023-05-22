SUBROUTINE scramplituddes(rcenter,zcenter,angin,rmc,rms, &
     zmc,zms,xpts,xin,yin,ier)
  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: half, two
  use scrunch_inc1
  use scrunch_inc2
  implicit none
  integer :: ier
  integer :: m
  real(fp) :: yi
  real(fp) :: xi
  real(fp) :: arg
  integer :: j
  real(fp) :: xmult
  real(fp), intent(in) :: zcenter
  real(fp), intent(in) :: rcenter
  real(fp), intent(inout), dimension(*) :: rmc,rms,zmc,zms,xpts
  real(fp), intent(in), dimension(*) :: angin,xin,yin
  !*****************************************************************
  !       This subroutine assigns initial guesses for angles and
  !       Fourier mode amplitudes to the appropriate components of
  !       the xvec array ... use Fourier Analysis to project out
  !       each coefficient, below.
  !*****************************************************************
  rmc(1) = rcenter
  zmc(1) = zcenter
  xpts(1:ntheta) = angin(1:ntheta)
  xmult = two/DBLE(nthetax)
  do j = 1,nthetax
    arg = angin(j)
    xi = xmult*(xin(j) - rcenter)
    yi = xmult*(yin(j) - zcenter)
    do m = 2,mpolx
      rmc(m) = rmc(m) + cos((m-1)*arg)*xi
      if (lasym) rms(m) = rms(m) + sin((m-1)*arg)*xi
      if (lasym.and. .not.lzakharov) &
           zmc(m) = zmc(m) + cos((m-1)*arg)*yi
      if (.not.lzakharov.or. m.eq.2) zms(m) = zms(m) + &
           sin((m-1)*arg)*yi
    end do
  end do
  !
  !	INITIALIZE RMS = ZMC FOR M=1
  !
  rms(2) = half*(rms(2) + zmc(2))
  if (.not. lzakharov) zmc(2) = rms(2)

end SUBROUTINE scramplituddes

