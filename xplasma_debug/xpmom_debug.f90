subroutine xpmom_debug

  use xplasma_definitions
  use xpl_dbg_module

  implicit NONE
 
  real*8, parameter :: z0 = 0.0d0
  real*8, parameter :: zsmall = 1.0d-6

  !------------------------------------------------
  !  xmoments debug plot...
  !------------------------------------------------

  integer :: kmom,ksurf,ier,iertmp,ikmax
  real*8, dimension(:,:), allocatable :: zx,zrmcx,zrmsx,zzmcx,zzmsx
  real*8, dimension(:), allocatable :: zthg,zthwk,zxwk,zwk,zRwk,zZwk
  real*8, dimension(:,:), allocatable :: zRdiff,zZdiff,zsina,zcosa
  real*8 zf,zxa(2),zra(2),zza(2),zdrx,zdzx,zdrsum,zdzsum
  real*8, dimension(:), allocatable :: xrho ! (ksurf)
  real*8, dimension(:), allocatable :: zz ! (0:kmom)
  integer, dimension(:), allocatable :: iself ! (0:kmom)
  character*11, dimension(:), allocatable :: lbsel ! (0:kmom)
  character*17 kylab(5)
  character*60 mnlab(3)
  integer ins(1),inx,ix,ixk,ict(3),id_R,id_Z,id_th,inthg,inthwk
  integer :: ithi,ithg,ii,ith
  logical lxout,lchar
  integer lnout,iscale,im,imm,im1,im2,imc,idum,luntty
  integer, parameter :: inpth = 5

  !------------------------------------------------
  !  exit now if xmoments not fully initialized...

  luntty=lnout(0)

  !  theta grid for testing

  call xplasma_common_ids(sp,ier, id_R=id_R,id_Z=id_Z)
  if(ier.ne.0) then
     write(luntty,*) ' ?xpmom_debug: unexpected xplasma error:'
     call xplasma_error(sp,ier,luntty)
     return
  endif

  call xplasma_prof_info(sp,id_R,ier, gridId1=id_th)

  call xplasma_grid_size(sp,id_th,inthg,ier)
  inthwk=inpth*(inthg-1)+1

  allocate(zthg(inthg),zthwk(inthwk),zxwk(inthwk),zRwk(inthwk),zZwk(inthwk))
  call xplasma_grid(sp,id_th,zthg,ier)
  if(ier.ne.0) then
     write(luntty,*) ' ?xpmom_debug: unexpected xplasma error:'
     call xplasma_error(sp,ier,luntty)
     return
  endif

  zthwk(1)=zthg(1)
  ithi=1
  do ithg=2,inthg
     do ii=1,inpth
        ithi=ithi+1
        zthwk(ithi)=zthg(ithg-1) + ii*(zthg(ithg)-zthg(ithg-1))/inpth
     enddo
  enddo

  !---------------------------
  !  moments rho grid size
  call xpmom_info(sp,ier, kmom=kmom, ksurf=ksurf, ikmax=ikmax)
  if((ier.ne.0).or.(kmom.eq.0).or.(ksurf.eq.0)) then
     write(luntty,*) ' ?xpmom_debug: no Fourier Spline data.'
     return
  endif

  write(luntty,*) kmom,' moments detected.'

  !---------------------------
  !  info on axis derivative
  zdrsum=z0
  zdzsum=z0
  zxa(1)=z0
  zxa(2)=zsmall
  do im=0,kmom,2
     if(im.eq.0) then
        call xpmom_get1(sp,zxa,0,im,ier, rcmom=zra, zcmom=zza)
     else
        call xpmom_get1(sp,zxa,0,im,ier, rcmom=zra, zsmom=zza)
     endif
     if(ier.ne.0) then
        write(luntty,*) ' ?xpmom_debug: unexpected xplasma error:'
        call xplasma_error(sp,ier,luntty)
        exit
     endif
     zdrx=(zra(2)-zra(1))/(zxa(2)-zxa(1))
     zdzx=(zza(2)-zza(1))/(zxa(2)-zxa(1))
     zdrsum=zdrsum+zdrx
     zdzsum=zdzsum+zdzx
     write(luntty,'(" axis: dR[",i2,"]/drho = ",1pe13.6," dZ[",i2,"]/drho = ",1pe13.6)') &
          im,zdrx,im,zdzx
  enddo
  write(luntty,'(1x,a,1pe13.6,1x,a,1pe13.6)') &
       ' axis dR/drho sum: ',zdrsum,' dZ/drho sum: ',zdzsum

  if(ier.ne.0) return

  !----------------------------------------------
  ! test interpolation of scaled moments & plot...
 
  inx=10*(ksurf-1)+1
  ins(1)=inx
  allocate(zx(inx,0:kmom))  ! replicated kmom+1 times
  allocate(zrmcx(inx,0:kmom),zrmsx(inx,0:kmom))
  allocate(zzmcx(inx,0:kmom),zzmsx(inx,0:kmom))
  allocate(zz(0:kmom),iself(0:kmom),lbsel(0:kmom))
  allocate(xrho(ksurf))

  call xpmom_info(sp,ier, xrho=xrho)

  ! fine scale x axis to test interpolation
  ! separate copy of x axis for each moment (for plotting routine)
 
  zx(inx,0:kmom)=xrho(ksurf)
  do ix=1,inx-1
     ixk=1+(ix-1)/10
     zf=(ix-(ixk-1)*10-1)/10.0d0
     !  this is duplicated across moments because of needs of plotting routine
     zx(ix,0:kmom)=(1.0-zf)*xrho(ixk)+zf*xrho(ixk+1)
  enddo

  !---------------------------------
  !  compute R and Z differences btw bicubic spline and Fourier
  !  representation -- on fine x and th grids

  allocate(zRdiff(inthwk,inx),zZdiff(inthwk,inx))

  do
     zrmcx=0; zrmsx=0; zzmcx=0; zzmsx=0
 
     ! labeling stuff
 
     lbsel=' '
     kylab=' '
     mnlab=' '

     if(lxout(0)) write(luntty,1001)
1001 format(/ &
          ' D -- examine difference btw bicubic spline and Fourier representation'/ &
          ' M -- plot all moments'/ &
          ' X -- plot all moments scaled by 1/x'/ &
          ' N -- plot subset of moments'/ &
          ' Y -- plot subset of moments scaled by 1/x'/ &
          ' Q -- quit'/)
     
     call uread('xmoments_debug: plot option:$')
     if(lchar('Q')) then
        exit
     else if(lchar('D')) then
        iscale=-1
        im1=0
        im2=kmom
     else if(lchar('M')) then
        iscale=0
        im1=0
        im2=min(14,kmom)
     else if(lchar('X')) then
        iscale=1
        im1=0
        im2=min(14,kmom)
     else if(lchar('N')) then
        iscale=0
        im1=0
        call ureadi('first moment to plot:',im1)
        im1=min(kmom,max(0,im1))
        im2=im1
        call ureadi('last moment to plot:',im2)
        im2=min(kmom,max(im1,im2))
     else if(lchar('Y')) then
        iscale=1
        im1=0
        call ureadi('first moment to plot:',im1)
        im1=min(kmom,max(0,im1))
        im2=im1
        call ureadi('last moment to plot:',im2)
        im2=min(kmom,max(im1,im2))
     else
        call uermsg(' ?xmoments_debug: unrecognized option.',' ',0)
        cycle
     endif
 
     if(iscale.eq.-1) then
        ! compute difference-- evaluate bicubic spline first...

        do ix=1,inx
           zxwk=zx(ix,0)
           ier=0
           call xplasma_eval_prof(sp,id_R, &
                xplasma_theta_coord,zthwk, xplasma_rho_coord,zxwk, &
                zRdiff(1:inthwk,ix),iertmp)
           ier=max(ier,iertmp)
           call xplasma_eval_prof(sp,id_Z, &
                xplasma_theta_coord,zthwk, xplasma_rho_coord,zxwk, &
                zZdiff(1:inthwk,ix),ier)
           ier=max(ier,iertmp)
           if(ier.ne.0) then
              write(luntty,*) ' ?xpmom_debug: unexpected xplasma error:'
              call xplasma_error(sp,ier,luntty)
              zRdiff=0
              zZdiff=0
              exit
           endif
        enddo
     endif
     ! evaluate scaled moment splines
 
     ict(1)=1; ict(2)=0; ict(3)=0
     if(im1.eq.0) lbsel(0)='0th moment'
     imc=-1
     do im=im1,im2
        imc=imc+1
        call xpmom_get1(sp,zx(1:inx,0),max(0,iscale),im,idum, rcmom=zrmcx(1:inx,imc))
        call xpmom_get1(sp,zx(1:inx,0),max(0,iscale),im,idum, rsmom=zrmsx(1:inx,imc))
        call xpmom_get1(sp,zx(1:inx,0),max(0,iscale),im,idum, zcmom=zzmcx(1:inx,imc))
        call xpmom_get1(sp,zx(1:inx,0),max(0,iscale),im,idum, zsmom=zzmsx(1:inx,imc))
        if(iscale.ge.0) then
           zz(imc)=im
           if(im.gt.0) then
              iself(imc)=imc+1
              if(iscale.eq.1) then
                 if((im.eq.1).or.(ikmax.eq.1)) then
                    write(lbsel(imc),'("moment",i2.2,"/x")') im
                 else
                    write(lbsel(imc),'("mom#",i2.2,"/x^2")') im
                 endif
              else
                 write(lbsel(imc),'("moment ",i2.2)') im
              endif
           endif
        endif
     enddo

     if(iscale.ge.0) then
        iself(0)=-1
 
        ! make plots of moments themselves...
 
        if(iscale.eq.1) then
           kylab(1)='scaled moments'
        else
           kylab(1)='Fourier moments'
        endif
        mnlab(1)='debug plot -- xmoments_spline_setup'
        mnlab(2)='xplasma (xmoments_debug.f90)'
 
        mnlab(3)='R cos moments'
        call r8_grfmm1(zx,zrmcx,inx,ins,1,zz,imc+1,iself,imc+1, &
             ' ','m',mnlab,kylab,lbsel)
 
        mnlab(3)='R sin moments'
        call r8_grfmm1(zx,zrmsx,inx,ins,1,zz,imc+1,iself,imc+1, &
             ' ','m',mnlab,kylab,lbsel)
 
        mnlab(3)='Z cos moments'
        call r8_grfmm1(zx,zzmcx,inx,ins,1,zz,imc+1,iself,imc+1, &
             ' ','m',mnlab,kylab,lbsel)
 
        mnlab(3)='Z sin moments'
        call r8_grfmm1(zx,zzmsx,inx,ins,1,zz,imc+1,iself,imc+1, &
             ' ','m',mnlab,kylab,lbsel)

     else
        ! compute and plot FS and bc spline differences

        allocate(zsina(kmom,inthwk),zcosa(kmom,inthwk))
        do ith=1,inthwk
           call r8sincos(zthwk(ith),kmom,zsina(1:kmom,ith),zcosa(1:kmom,ith))
        enddo

        do ix=1,inx
           zRwk=zrmcx(ix,0)
           zZwk=zzmcx(ix,0)
           do ith=1,inthwk
              do im=1,kmom
                 zRwk(ith)=zRwk(ith) + zrmcx(ix,im)*zcosa(im,ith) + &
                      zrmsx(ix,im)*zsina(im,ith)
                 zZwk(ith)=zZwk(ith) + zzmcx(ix,im)*zcosa(im,ith) + &
                      zzmsx(ix,im)*zsina(im,ith)
              enddo
           enddo

           zRdiff(1:inthwk,ix)=zRdiff(1:inthwk,ix)-zRwk
           zZdiff(1:inthwk,ix)=zZdiff(1:inthwk,ix)-zZwk
        enddo

        allocate(zwk(max(inthwk,inx))); zwk=0

        call r8_grf3f1(zthwk,zRdiff,zx(1:inx,0),zwk, &
             inthwk,inx,inthwk, &
             'theta','delta(R)','rho', 'rad','m',' ', &
             'xpmom_debug plot','compare R Fourier spline & bicubic spline',0)

        call r8_grf3f1(zthwk,zZdiff,zx(1:inx,0),zwk, &
             inthwk,inx,inthwk, &
             'theta','delta(Z)','rho', 'rad','m',' ', &
             'xpmom_debug plot','compare Z Fourier spline & bicubic spline',0)

        deallocate(zwk,zsina,zcosa)

     endif

  enddo
 
  deallocate(zx,zrmcx,zrmsx,zzmcx,zzmsx,zz,iself,lbsel,xrho)
  deallocate(zRdiff,zZdiff,zthwk,zthg,zRwk,zZwk,zxwk)

end subroutine xpmom_debug
