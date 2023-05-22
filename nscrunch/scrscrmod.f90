MODULE mintrp
  use iso_c_binding, only: fp => c_double
  implicit none
  integer :: nu, ncmax
!
  real(fp), dimension(:,:), allocatable :: rin ! (nu,ncmax)
  real(fp), dimension(:,:), allocatable :: zin ! (nu,ncmax)
  integer :: ntheta0, numc                 ! no of points in contours
  real(fp) :: raxis,zaxis                   ! the axis location
  real(fp) :: facmid, facedg, facdx0
  !															
  !  recommended value:  moption=3
  integer :: moption                    ! control flag
  !
  !  rotated low order polynomial description of theta line
  !  (for internal use only)
  !
  real(fp) :: r0,z0,r1,z1,r2,z2
  real(fp) :: a,b,c,d,xbrk,a2,b2,c2,d2,xmax,zcosa,zsina
  real(fp) :: zcosa2,zsina2
  !
  real(fp) :: xspl(5),yspl(4,5),xpkg(5,4) ! for moption=3
  !
  !  surface whose intercepts are wanted; last nearby theta interval
  !  (for internal use only)
  !
  integer :: ksurf,kth
  !
  !--------------------------------------------------------------------
END MODULE mintrp

MODULE scrunch_inc1
  use iso_c_binding, only: fp => c_double
  ! begin SCRUNCH_INC1
  ! make mpol = mpolDIM + 1 , where mpolDIM is defined in SCRUNCH_INC0
  implicit none
  integer :: mnd
  integer :: mpnt
  integer :: nphi2
  integer :: nphi
  integer :: mpol
  integer :: nznt
  integer :: ntheta
  integer :: mpol2
  integer :: mpol3
  integer :: mpol4
  integer :: n2
  integer :: nfp
  integer :: nthetax
  integer :: mpolx
  integer :: nresets
  parameter(nphi=1,nphi2=1+nphi/2)
  logical ioflagc
  integer, dimension(:), allocatable :: mm ! (mpol)
  integer :: nn(nphi)
  integer, dimension(:), allocatable :: m1 ! (mnd)
  integer, dimension(:), allocatable :: n1 ! (mnd)
  real(fp), dimension(:), allocatable :: dm1 ! (mpol)
  real(fp) :: r0n(nphi)
  real(fp) :: z0n(nphi)
  real(fp) :: raxis3d(nphi)
  real(fp) :: zaxis3d(nphi)
  real(fp), dimension(:,:), allocatable :: rin3d ! (ntheta,nphi)
  real(fp), dimension(:,:), allocatable :: zin3d ! (ntheta,nphi)
  real(fp), dimension(:,:), allocatable :: angle ! (ntheta,nphi)
  real(fp), dimension(:), allocatable :: faccon ! (mpol)
  real(fp), dimension(:,:), allocatable :: xmpq ! (mpol,4)
  real(fp) :: gnorm
  real(fp) :: specw
  real(fp) :: delt
  real(fp) :: deltf
  real(fp) :: pexp
  real(fp) :: qexp
  real(fp) :: dnorm
  real(fp) :: elongate
  real(fp), dimension(:,:,:), allocatable :: result ! (nphi2,mpol,4)
  real(fp), dimension(:), allocatable :: xvec ! (n2)
  real(fp), dimension(:), allocatable :: gvec ! (n2)
  real(fp), dimension(:), allocatable :: xdot ! (n2)
  real(fp), dimension(:), allocatable :: xstore ! (n2)
 
  integer :: lunmsg = 6
 
END MODULE scrunch_inc1

MODULE scrunch_inc2
  use iso_c_binding, only: fp => c_double
  implicit none
  logical :: lasym
  logical :: lzakharov
  real(fp), dimension(:,:), allocatable :: rmomb ! (mpol,2)
  real(fp), dimension(:,:), allocatable :: zmomb ! (mpol,2)
  real(fp) :: rcent
  real(fp) :: zcent
END MODULE scrunch_inc2
