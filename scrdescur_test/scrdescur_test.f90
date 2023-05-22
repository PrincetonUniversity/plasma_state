program scrdescur_test

  ! debug driver for scrdescur; read debug data written by icon.gt.0 option
  ! of nscrunch/scrdescur subroutine

  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: pi
  implicit none

  !-----------------------
  real(fp), dimension(:), allocatable :: rmb,zmb
  real(fp), dimension(:,:), allocatable :: rmbout,zmbout
  real(fp) :: fsq
  integer :: ncmax,nu,mpol
  integer :: nmb,mpolin,nmbout
  logical :: lasym0,lzsmlonly,ioflag
  integer :: ier
  !-----------------------

  integer :: ilun = 77
  integer :: iout = 6

  logical :: lxout
  !-----------------------
  !  debug plotting...
  !    (this originally the __DEBUG code from nscrunch/scrdesreg.f90)


  real(fp) :: r0atan,z0atan,shif2pi
  real(fp), dimension(:), allocatable  :: thatan,datan
  !!  real(fp) :: thatan(nmb),datan(nmb)

  real(fp), dimension(:), allocatable :: thb2,db2

  integer, parameter :: nthspl=301
  real(fp) :: thvmec(nthspl),thvma(nthspl),dvmec(nthspl), &
       rvmec(nthspl),zvmec(nthspl)
  real(fp) :: thvmcc(nthspl-1),dthth(nthspl-1),ddth(nthspl-1)

  real(fp), dimension(:), allocatable :: sinm,cosm

  integer :: i,j
  !!  real(fp) :: sinm(mpolin-1),cosm(mpolin-1)

  !-----------------------

  write(iout,*) ' '
  write(iout,*) '   scrdescur_test  -- DMC Apr 2010 -- special purpose debug '
  write(iout,*) ' '

  open(unit=ilun,file='scrdescur_debug.dat',status='old',form='unformatted')

  read(ilun) ncmax,nu,mpol
  write(iout,*) '  ncmax= ',ncmax,'  nu= ',nu,'  mpol=',mpol

  read(ilun) nmb,mpolin
  write(iout,*) '  nmb= ',nmb,'  mpolin= ',mpolin

  allocate(rmb(nmb),zmb(nmb))

  read(ilun) rmb(1:nmb)
  read(ilun) zmb(1:nmb)

  write(iout,*) '  min & max of rmb(...): ',minval(rmb),maxval(rmb)
  write(iout,*) '  min & max of zmb(...): ',minval(zmb),maxval(zmb)

  read(ilun) lasym0
  write(iout,*) '  Lasym0: ',lasym0

  read(ilun) nmbout
  close(unit=ilun)

  write(iout,*) ' -----------------> file read OK.'

  call uinitg("scrdescur_test")

  if(lxout(0)) then
     write(iout,*) ' enter comma "," not to change... '
  endif

  call ureadi(' new nmbout ? ',nmbout)
  nmbout = max(3,nmbout)

  write(iout,*) '  nmbout: ',nmbout
  mpolin=nmbout+1

  call scrpreset(nu,ncmax,nmbout)
  call scrmemory(ier)
  if(ier.ne.0) then
    call ps_bad_exit
  end if
  call scrclear

  allocate(rmbout(0:nmbout,2),zmbout(0:nmbout,2))

  write(iout,*) ' ---> scrDESCUR call... '

  ioflag = .TRUE.
  lzsmlonly = .FALSE.

  call scrDESCUR(rmb,zmb,nmb,mpolin,0,ioflag, &
       fsq,lasym0,lzsmlonly,nmbout,rmbout,zmbout,ier)

  write(iout,*) ' ---> scrDESCUR call completed, fsq=',fsq,' ier=',ier

  !-------------------------------
  ! debug plotting

  allocate(sinm(mpolin-1),cosm(mpolin-1))
  allocate(thatan(nmb),datan(nmb))

  z0atan=zmb(1)
  r0atan=sum(rmb(1:nmb))/nmb
  call bdtan(nmb,rmb,zmb,thatan,datan)

  do i=1,nthspl
     thvmec(i)=(i-1)*2*PI/(nthspl-1)
     call r8sincos(thvmec(i),mpolin-1,sinm,cosm)
     rvmec(i)=rmbout(0,1)
     zvmec(i)=zmbout(0,1)
     do j=1,mpolin-1
        rvmec(i)=rvmec(i)+cosm(j)*rmbout(j,1)+sinm(j)*rmbout(j,2)
        zvmec(i)=zvmec(i)+cosm(j)*zmbout(j,1)+sinm(j)*zmbout(j,2)
     enddo
  enddo
  call r8_grafc2(rmb,zmb,nmb, rvmec,zvmec,nthspl, 'm', 'm', &
       'scrDESREG debug plot','zmb(rmb), zvmec(rvmec)','should be similar')
  call bdtan(nthspl,rvmec,zvmec,thvma,dvmec)
  call r8_grafc2(thatan,datan,nmb, thvma,dvmec,nthspl, 'rad', 'm', &
       'scrDESREG debug plot','d(th)orig, d(th)vmec','should be similar')
  do i=1,nthspl-1
     thvmcc(i)=(thvmec(i)+thvmec(i+1))/2
     dthth(i)=(thvma(i+1)-thvma(i))/(thvmec(i+1)-thvmec(i))
     ddth(i)=(dvmec(i+1)-dvmec(i))/(thvmec(i+1)-thvmec(i))
  enddo
  call r8_grafx2(thvmcc,ddth,dthth,nthspl-1,'rad','mixed', &
       'scrDESREG debug plot','d(dvmec)/dth & d(thvmec)/dth',' ')

  stop

CONTAINS

  subroutine bdtan(nmb,rmb,zmb,thatan,datan)
    integer :: nmb
    real(fp) :: rmb(nmb),zmb(nmb),datan(nmb),thatan(nmb)

    shif2pi=0
    do i=1,nmb
       datan(i)=sqrt((rmb(i)-r0atan)**2+(zmb(i)-z0atan)**2)
       thatan(i)=atan2((zmb(i)-z0atan),(rmb(i)-r0atan)) + shif2pi
       if(i.gt.1) then
          if(thatan(i).lt.thatan(i-1)-PI) then
             shif2pi=shif2pi+2*PI
             thatan(i)=thatan(i)+shif2pi
          endif
       endif
    enddo
  end subroutine bdtan

end program scrdescur_test
