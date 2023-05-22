subroutine ps_momtest(ss,iout,ierr)

  ! Test routine: compare xplasma and plasma state Fourier Spline (FS)
  ! equilibrium representations -- they should match

  ! Then, using Plasma State tools compare FS and bicubic spline evaluations
  ! for flux surface values: {R,Z,dR/drho,dZ/drho,dR/dth,dZ/dth}

  use plasma_state_mod
  use xplasma_definitions
  implicit NONE

  TYPE (plasma_state) :: ss     ! state instance from which to load
  INTEGER, intent(in) :: iout   ! I/O channel for messages

  INTEGER, intent(out) :: ierr  ! completion status (0=OK)

  !-----------------------------
  !  type definition -- enable acquisition of xplasma pointer from PS object

  INTEGER, PARAMETER :: R8=SELECTED_REAL_KIND(12,100)

  type :: interp_object
     type (xplasma), pointer :: s_xpobj => NULL()
  end type interp_object

  type (interp_object) :: xobj
  type (xplasma), pointer :: s

  integer :: imom,kmom,iflag,inrho,ict,irho,ids(4)
  real*8, dimension(:), allocatable :: rhobuf
  real*8, dimension(:,:), allocatable :: test1,test2
  integer :: imax0,imax1
  real*8 :: max_diff0, max_diff1
  real*8 :: cpt1,cpt2

  real*8, parameter :: tol_ref0 = 1.0e-11_R8
  real*8, parameter :: tol_ref1 = 1.0e-7_R8

  !-----------------------------
  !  for evaluation comparisons

  integer, parameter :: nkm_vals = 4
  integer, parameter :: nth = 201
  integer, parameter :: nsurfs = 10 

  integer :: isurf,ith,ik,imoms

  integer :: ikm_vals(nkm_vals) = (/ 0, 4, 16, 64 /)

  real*8 :: rhosurfs(nsurfs) = &
       (/ 0.1_R8,  0.3_R8,  0.5_R8,  0.7_R8,  0.9_R8, &
          0.95_R8, 0.97_R8, 0.98_R8, 0.99_R8, 1.0_R8 /)

  real*8 :: rhovec(nth),thvec(nth)
  real*8 :: Rvec(nth,nkm_vals),Zvec(nth,nkm_vals)
  real*8 :: Rvec2(nth,nkm_vals),Zvec2(nth,nkm_vals)
  real*8 :: dRdrho(nth,nkm_vals),dZdrho(nth,nkm_vals)
  real*8 :: dRdth(nth,nkm_vals),dZdth(nth,nkm_vals)

  real*8 :: RR1,RRmin,RRmax,ZZmin,ZZmax
  real*8 :: val_norm    ! val_norm = (RRmax+RRmin)/2
  real*8 :: deriv_norm  ! deriv_norm = max((RRmax-RRmin),(ZZmax-ZZmin))/2
  real*8 :: diftabv(nsurfs,nkm_vals-1),diftabd(nsurfs,nkm_vals-1)
  real*8 :: dR,dZ,dd(4)
  !-----------------------------

  !=====================================================================
  !  check that state has associated xplasma object; set pointer.

  call cpu_time(cpt1)

  write(iout,*) ' '
  if(ps_debug.ge.2) then
     write(iout,*) ' Entering: ps_momtest; ss%global_label = "'// &
          trim(ss%global_label)//'".'
  else
     write(iout,*) ' (ps_momtest...) '
  endif

  xobj = transfer(ss%iobj,xobj)
  s => xobj%s_xpobj

  call xpmom_info(s,ierr, kmom=kmom)
  if(ierr.ne.0) then
     call xplasma_error(s,ierr,iout)
     write(iout,*) ' ?xplasma error in ps_momtest, early exit.'
     return
  endif

  iflag=0
  if(ss%neqmom.eq.0) then
     write(iout,*) ' %ps_momtest: no FS moments in passed Plasma State object'
     iflag=1
  endif

  if(kmom.eq.0) then
     write(iout,*) ' %ps_momtest: no FS moments in state contained xplasma object'
     iflag=1
  endif

  if(iflag.eq.1) return

  if(kmom.ne.ss%neqmom) then
     ierr=1
     write(iout,*) ' ?ps_momtest: FS moments number mismatch btw Plasma State'
     write(iout,*) '  and contained xplasma object: ',ss%neqmom,kmom
     return
  endif

  ! construct test grid & buffers

  inrho = 2*ss%nrho_eq - 1
  allocate(rhobuf(inrho),test1(inrho,4),test2(inrho,4))

  ict=0
  do irho=1,inrho,2
     ict=ict+1
     rhobuf(irho) = ss%rho_eq(ict)
     if(ict.gt.1) then
        rhobuf(irho-1) = 0.5d0*(rhobuf(irho)+rhobuf(irho-2))
     endif
  enddo

  ! OK: compare 0th moments

  imax0 = -1
  imax1 = -1
  max_diff0 = 0.0d0
  max_diff1 = 0.0d0

  ids(1)=ss%id_R0_momeq
  ids(2)=ss%id_Z0_momeq
  ids(3)=0
  ids(4)=0

  call diff(0)
  if(ierr.ne.0) return

  ! compare higher moments

  do imom=1,kmom
     ids(1)=ss%id_xRjcos_momeq(imom)
     ids(2)=ss%id_xZjcos_momeq(imom)
     ids(3)=ss%id_xRjsin_momeq(imom)
     ids(4)=ss%id_xZjsin_momeq(imom)
     call diff(imom)
     if(ierr.ne.0) return
  enddo

  if(max_diff0.gt.tol_ref0) then
     write(iout,*) ' ?xp_momtest: max coefficient value difference exceeds reference'
     write(iout,'(1x,a,i4,a,1pe12.5)') '  moment #',imax0,':',max_diff0
     ierr = 1
  else
     write(iout,'(a,1pe12.5)') ' %xp_momtest: max coeff value diff OK: ', &
          max_diff0
  endif

  if(max_diff1.gt.tol_ref1) then
     write(iout,*) ' ?xp_momtest: max coefficient derivative difference exceeds reference'
     write(iout,'(1x,a,i4,a,1pe12.5)') '  moment #',imax0,':',max_diff1
     ierr = 1
  else
     write(iout,'(a,1pe12.5)') ' %xp_momtest: max coeff deriv diff OK: ', &
          max_diff1
  endif
  !--------------------------------------------------
  !  evaluation comparisons

  do ith=1,nth
     thvec(ith) = (ith-1)*6.2831853071795862_R8/(nth-1)
  enddo

  diftabv = 0.0_R8
  diftabd = 0.0_R8

  do isurf = 1,nsurfs
     rhovec(1:nth) = rhosurfs(isurf)

     do ik=1,nkm_vals
        imoms=ikm_vals(ik)

        call ps_rz_eval(rhovec,thvec,ierr, state=ss, fs_kmax=imoms, &
             R=Rvec(:,ik), Z=Zvec(:,ik))
        if(ierr.ne.0) return

        call ps_rz_eval(rhovec,thvec,ierr, state=ss, fs_kmax=imoms, &
             R=Rvec2(:,ik), Z=Zvec2(:,ik), &
             dRdrho=dRdrho(:,ik), dZdrho=dZdrho(:,ik), &
             dRdth=dRdth(:,ik), dZdth=dZdth(:,ik))
        if(ierr.ne.0) return

        ! preliminary check

        max_diff0 = 0.0_R8
        if((isurf.eq.1).and.(imoms.eq.0)) then
           RRmin=Rvec(1,ik)
           RRmax=RRmin
           RR1=max(1.0_R8,RRmin)
           ZZmin=Zvec(1,ik)
           ZZmax=ZZmin
        endif

        do ith=1,nth
           if(imoms.eq.0) then
              RRmin=min(RRmin,Rvec(ith,ik))
              RRmax=max(RRmax,Rvec(ith,ik))
              ZZmin=min(ZZmin,Zvec(ith,ik))
              ZZmax=max(ZZmax,Zvec(ith,ik))
           endif
           max_diff0 = max(max_diff0,abs(Rvec(ith,ik)-Rvec2(ith,ik))/RR1)
           max_diff0 = max(max_diff0,abs(Zvec(ith,ik)-Zvec2(ith,ik))/RR1)
           if(ik.gt.1) then
              dR=abs(Rvec(ith,ik)-Rvec(ith,1))
              dZ=abs(Zvec(ith,ik)-Zvec(ith,1))

              dd(1)=abs(dRdrho(ith,ik)-dRdrho(ith,1))
              dd(2)=abs(dZdrho(ith,ik)-dZdrho(ith,1))
              dd(3)=abs(dRdth(ith,ik)-dRdth(ith,1))
              dd(4)=abs(dZdth(ith,ik)-dZdth(ith,1))

              diftabv(isurf,ik-1)=max(diftabv(isurf,ik-1),dR)
              diftabv(isurf,ik-1)=max(diftabv(isurf,ik-1),dZ)
              diftabd(isurf,ik-1)=max(diftabd(isurf,ik-1),maxval(dd))
           endif
        enddo

        if(max_diff0.gt.tol_ref0) then
           write(iout,*) ' ?ps_momtest: unexpected difference in ps_RZ_eval results... '
           ierr=1
           return
        endif

     enddo
  enddo
     
  val_norm = (RRmax+RRmin)/2
  val_norm = max(1.0e-4_R8,val_norm)

  deriv_norm = max((RRmax-RRmin),(ZZmax-ZZmin))/2
  deriv_norm = max(1.0e-4_R8,deriv_norm)

  diftabv = diftabv/val_norm
  diftabd = diftabd/deriv_norm

  call diftab_print(diftabv,'Normalized maximum deviation in {R,Z}')
  call diftab_print(diftabd,'Normalized maximum deviation in {dR/drho,dZ/drho,dR/dth,dZ/dth}')

  !--------------------------------------------------
  call cpu_time(cpt2)

  write(iout,'(/1x,a,t34,1pe10.3)') 'Time for ps_momtest: ', cpt2-cpt1

CONTAINS

  subroutine diff(imom)

    integer, intent(in) :: imom

    !--------------------
    ! value comparison

    if(imom.eq.0) then

       test1 = 0.0d0
       test2 = 0.0d0

       call xpmom_get1(s,rhobuf,1,imom,ierr, &
            rcmom=test1(:,1), zcmom=test1(:,2), ideriv=0)

       if(ierr.ne.0) then
          call xplasma_error(s,ierr,iout)
          write(iout,*) '  moment #: ',imom
          return
       endif

       call ps_intrp_1d(rhobuf,ids(1:2),test2(:,1:2),ierr, state=ss, ideriv=0)

       if(ierr.ne.0) then
          write(iout,*) ' ?ps_momtest: ps_intrp_1d error code: ',ierr
          write(iout,*) '  moment #: ',imom
          return
       endif
            
    else

       call xpmom_get1(s,rhobuf,1,imom,ierr, &
            rcmom=test1(:,1), zcmom=test1(:,2), &
            rsmom=test1(:,3), zsmom=test1(:,4), &
            ideriv=0)

       if(ierr.ne.0) then
          call xplasma_error(s,ierr,iout)
          write(iout,*) '  moment #: ',imom
          return
       endif

       call ps_intrp_1d(rhobuf,ids,test2,ierr, state=ss, ideriv=0)

       if(ierr.ne.0) then
          write(iout,*) ' ?ps_momtest: ps_intrp_1d error code: ',ierr
          write(iout,*) '  moment #: ',imom
          return
       endif
            
    endif

    call compare(imom,0,max_diff0,imax0)

    !--------------------
    ! derivative comparison

    if(imom.eq.0) then

       test1 = 0.0d0
       test2 = 0.0d0

       call xpmom_get1(s,rhobuf,1,imom,ierr, &
            rcmom=test1(:,1), zcmom=test1(:,2), ideriv=1)

       if(ierr.ne.0) then
          call xplasma_error(s,ierr,iout)
          write(iout,*) '  moment #: ',imom
          return
       endif

       call ps_intrp_1d(rhobuf,ids(1:2),test2(:,1:2),ierr, state=ss, ideriv=1)

       if(ierr.ne.0) then
          write(iout,*) ' ?ps_momtest(deriv): ps_intrp_1d error code: ',ierr
          write(iout,*) '  moment #: ',imom
          return
       endif
            
    else

       call xpmom_get1(s,rhobuf,1,imom,ierr, &
            rcmom=test1(:,1), zcmom=test1(:,2), &
            rsmom=test1(:,3), zsmom=test1(:,4), &
            ideriv=1)

       if(ierr.ne.0) then
          call xplasma_error(s,ierr,iout)
          write(iout,*) '  moment #: ',imom
          return
       endif

       call ps_intrp_1d(rhobuf,ids,test2,ierr, state=ss, ideriv=1)

       if(ierr.ne.0) then
          write(iout,*) ' ?ps_momtest(deriv): ps_intrp_1d error code: ',ierr
          write(iout,*) '  moment #: ',imom
          return
       endif
            
    endif

    call compare(imom,1,max_diff1,imax1)

  end subroutine diff

  subroutine compare(imom,ideriv,zmax,imax)

    integer, intent(in) :: imom  ! moment index of comparison
    integer, intent(in) :: ideriv  ! 0 for value, 1 for derivative comparison

    real*8, intent(inout) :: zmax  ! max difference seen
    integer, intent(inout) :: imax ! moment index of max difference seen

    real*8 :: zref
    character*40 :: label,dlabel
    character*3 :: zint

    if(ideriv.eq.0) then
       zref = tol_ref0
       dlabel = '(value)'
    else
       zref = tol_ref1
       dlabel = '(derivative)'
    endif

    zint=' '
    if(imom.le.9) then
       write(zint(1:1),'(I1)') imom
    else if(imom.le.99) then
       write(zint(1:2),'(I2)') imom
    else
       write(zint(1:3),'(I3)') imom
    endif

    label = 'R'//trim(zint)//' cos moment '//trim(dlabel)
    call compare1(label,test1(:,1),test2(:,1),imom,zref,zmax,imax)

    label = 'Z'//trim(zint)//' cos moment '//trim(dlabel)
    call compare1(label,test1(:,2),test2(:,2),imom,zref,zmax,imax)

    if(imom.gt.0) then
       label = 'R'//trim(zint)//' sin moment '//trim(dlabel)
       call compare1(label,test1(:,3),test2(:,3),imom,zref,zmax,imax)

       label = 'Z'//trim(zint)//' sin moment '//trim(dlabel)
       call compare1(label,test1(:,4),test2(:,4),imom,zref,zmax,imax)
    endif

  end subroutine compare

  subroutine compare1(label,atest1,atest2,imom,zref,zmax,imax)

    character*(*), intent(in) :: label  ! message label
    real*8, dimension(:), intent(in) :: atest1,atest2  ! data to compare
    integer, intent(in) :: imom  ! moment index of comparison
    real*8, intent(in) :: zref   ! reference maximum relative difference

    real*8, intent(inout) :: zmax  ! max difference seen
    integer, intent(inout) :: imax ! moment index of max difference seen

    !----------------------
    real*8 :: maxa1,maxa2,maxaa,maxdiff
    integer :: iprint
    !----------------------

    maxa1=maxval(abs(atest1))
    maxa2=maxval(abs(atest2))
    maxaa = max(maxa1,maxa2)

    if(maxaa.eq.0.0d0) then
       maxdiff=0.0d0
    else

       maxdiff = maxval(abs(atest1-atest2))/maxaa

    endif

    iprint=0
    if(maxdiff.gt.zref) iprint=1

#ifdef __DEBUG
    iprint=1
#endif

    if(maxdiff.gt.zmax) then
       zmax=maxdiff
       imax=imom
    endif

    if(iprint.eq.1) then
       write(iout,*) ' *** '//trim(label)//' maxdiff: ',maxdiff
    endif

  end subroutine compare1

  subroutine diftab_print(diftab,label)

    real*8, dimension(:,:) :: diftab
    character*(*) :: label

    !-----------------------------
    integer :: iki
    !-----------------------------

    write(iout,*) ' '
    write(iout,*) ' Fourier Spline (FS) // Bicubic Spline evaluation comparison: '
    write(iout,*) ' '//trim(label)
    write(iout,*) ' vs. flux coordinate (rho) and number of FS moments: '
    write(iout,*) ' '
    write(iout,'(6x,a,3(6x,i2,6x))') 'rho',ikm_vals(2),ikm_vals(3),ikm_vals(4)

    do isurf=1,nsurfs
       write(iout,'(1x,f10.3,3(2x,1pe12.5))') rhosurfs(isurf), &
            diftab(isurf,1),diftab(isurf,2),diftab(isurf,3)
    enddo

  end subroutine diftab_print

end subroutine ps_momtest
