SUBROUTINE scrphasead(rm,ym,immax,imom,alpha)
  !
  !  adjust moments set by subtracting a phase shift alpha from the
  !  theta parametrization
  !
  use iso_c_binding, only: fp => c_double
  implicit none
  real(fp) :: yjn2
  real(fp) :: yjn1
  real(fp) :: rjn2
  real(fp) :: rjn1
  integer :: im
  integer, intent(in) :: imom
  real(fp), intent(in) :: alpha
  integer, intent(in) :: immax
  real(fp), dimension(0:immax,2), intent(inout) :: rm, ym
  !
  real(fp) :: zsn(100),zcs(100)
  !
  !------------------------------------
  !
  call scrsincos(alpha,imom,zsn,zcs)
  !
  !  0'th moments are unaffected
  !
  do im=1,imom
    rjn1= rm(im,1)*zcs(im) +rm(im,2)*zsn(im)
    rjn2=-rm(im,1)*zsn(im) +rm(im,2)*zcs(im)
    yjn1= ym(im,1)*zcs(im) +ym(im,2)*zsn(im)
    yjn2=-ym(im,1)*zsn(im) +ym(im,2)*zcs(im)
    rm(im,1)=rjn1
    rm(im,2)=rjn2
    ym(im,1)=yjn1
    ym(im,2)=yjn2
  end do

  return
end SUBROUTINE scrphasead
