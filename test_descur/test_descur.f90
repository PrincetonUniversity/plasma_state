program test_descur

  !  read debug file produced by nscrunch/scrdescur.for (icon.ne.0)

  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: zpi => pi
  implicit NONE

  !----------------------------------------------------------------
  !  descur stuff
  real(fp), dimension(:), allocatable :: rmb,zmb
  real(fp), dimension(:,:), allocatable :: rmbout,zmbout
  integer :: ncmax,nu,mpol, ns,jmax,imom
  integer :: nmb,mpolin,icon,nmbout
  logical :: lasym0,lzsmlonly,ioflag
  integer :: ier
  real(fp) :: fsq
  !----------------------------------------------------------------
  !  plotting stuff
  real(fp),dimension(:,:),allocatable :: rplo,zplo
  real(fp),dimension(:),allocatable :: zsin,zcos
  integer, parameter :: inz=2
  real(fp) :: zindx(inz),zth
  integer :: nnc(inz),iself(inz),iz,ith,im
  character(len=10), dimension(inz) :: lbsel
  character(len=17), dimension(5) :: kylab
  character(len=60), dimension(3) :: mnlab
  character(len=4) :: zint
  !----------------------------------------------------------------
  !  misc initialization

  call uinitg("TEST_DESCUR")

  !----------------------------------------------------------------
  !  read scrdescur debug file; allocate arrays as dimensions are
  !  read in...

  open(unit=99,file='scrdescur_debug.dat',status='old',form='unformatted')

  read(99) ncmax,nu,mpol
  ns=ncmax
  jmax=nu
  imom=mpol-1
  call scrpreset(jmax,ns,imom)   ! in nscrunch modules set dimensions
  call scrmemory(ier)            ! allocate arrays
  if(ier.ne.0) then
    call ps_bad_exit
  end if
  call scrclear                  ! clear to zero

  icon=0
  read(99) nmb,mpolin
  allocate(rmb(nmb),zmb(nmb))

  read(99) rmb
  read(99) zmb

  read(99) lasym0
  lzsmlonly=.FALSE.

  close(unit=99)

  nmbout=mpolin-1
  allocate(rmbout(0:nmbout,2),zmbout(0:nmbout,2))

  !------------------------
  !  descur call

  ioflag=.TRUE.

  call scrdescur(rmb,zmb,nmb,mpolin,icon,ioflag, &
       fsq,lasym0,lzsmlonly,nmbout,rmbout,zmbout,ier)

  write(6,*) ' %test_descur:  scrdescur returned ier=',ier,' fsq=',fsq
  write(6,*) '  (ier=1 is normal).'

  !------------------------
  !  plot stuff

  allocate(rplo(4*nmb,inz),zplo(4*nmb,inz)); rplo=0.0; zplo=0.0
  allocate(zsin(nmbout),zcos(nmbout))

  rplo(1:nmb,1)=rmb; zplo(1:nmb,1)=zmb
  do ith=1,4*nmb
     zth=(ith-1)*2*zpi/(4*nmb-1)
     call scrsincos(zth,nmbout,zsin,zcos)
     rplo(ith,2)=rmbout(0,1)
     zplo(ith,2)=zmbout(0,1)
     do im=1,nmbout
        rplo(ith,2)=rplo(ith,2)+rmbout(im,1)*zcos(im)+rmbout(im,2)*zsin(im)
        zplo(ith,2)=zplo(ith,2)+zmbout(im,1)*zcos(im)+zmbout(im,2)*zsin(im)
     enddo
  enddo

  do iz=1,inz
     zindx(iz)=iz
     nnc(iz)=nmb
     iself(iz)=iz
  enddo
  nnc(2)=4*nmb

  kylab=' '
  kylab(1)='flux surface'
  kylab(2)='contour & fit'

  lbsel(1)=' original '
  lbsel(2)='descur fft'

  mnlab=' '
  mnlab(1)='test_descur debug plot'
  mnlab(2)='original contour and "scrdescur" fft fit'
  write(zint,'(I4)') nmb
  mnlab(3)='no. of pts = '//zint

  call r8_grfmm1(rplo,zplo,4*nmb,nnc,inz,zindx,inz,iself,inz, &
       'm','m',mnlab,kylab,lbsel)

end program test_descur
