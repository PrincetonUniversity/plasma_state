subroutine geqxpl_rawplot(gfile)

  use geqdsk_mod
  implicit NONE

  !-------------------------------------------------------------------
  character*(*), intent(in) :: gfile  ! g-eqdsk filename or MDS+ path
  !-------------------------------------------------------------------

  !  Plot G-file data "raw" without processing.  This should help explain
  !  certain failures that can occur with EFIT data...

  integer :: iok,ipsi,npsi,ipass,nR,nZ,ising,inbmax

  integer :: luntty,lnout
  logical :: lchar,lxout

  type(geqdsk) :: geq

  real(geq_r8), dimension(:), allocatable :: psi_grid,Rgrid,Zgrid,zwk
  real(geq_r8), dimension(:,:), allocatable :: rbdc,zbdc

  real(geq_r8) :: zc(2)
  integer :: iself(2),nc(2)

  real(geq_r8) :: Rmin,Rmax,Zmin,Zmax,psimin,psimax,psimin0,psimax0,ztolpsi

  !----------------------------------
  character*75 mnlab(3)
  character*17 kylab(5)
  character*10 bdclab(2)

  character*48 :: efit_label
  !----------------------------------

  luntty = lnout(0)

  call geq_init(geq, gfile, iok)
  call geq_error(iok)
  if(iok/=0) return

  efit_label(1:8) = geq%CASE_(1)
  efit_label(9:16) = geq%CASE_(2)
  efit_label(17:24) = geq%CASE_(3)
  efit_label(25:32) = geq%CASE_(4)
  efit_label(33:40) = geq%CASE_(5)
  efit_label(41:48) = geq%CASE_(6)

  mnlab(1) = gfile
  mnlab(2) = 'EFIT_LABEL: '//efit_label
  mnlab(3) = 'geqxpl_rawplot.f90 "raw" EFIT data plot'

  kylab = ' '
  kylab(1) = ' G-file contours '

  bdclab(1) = 'boundary'
  bdclab(2) = 'limiter'

  iself(1)=1
  iself(2)=2

  zc(1) = 1.0d0
  zc(2) = 2.0d0

  ! G file read OK (but it might be rubbish...)

  nR = geq%nw
  nZ = geq%nh

  Rmin = geq%Rleft_m
  Rmax = Rmin + geq%Rdim_m

  Zmin = geq%Zmid_m - geq%Zdim_m/2
  Zmax = geq%Zmid_m + geq%Zdim_m/2

  npsi = geq%nw
  if(npsi.lt.2) then
     write(luntty,*) ' ?? geqxpl_rawplot: geq%nw < 2: ',geq%nw,'; must exit.'
     return
  endif

  allocate(psi_grid(npsi),Rgrid(nR),Zgrid(nZ),zwk(nZ)); zwk=0.0d0

  call mkgrid('Psi',psi_grid,geq%SIMAG_WB__Rad,geq%SIBRY_WB__Rad)
  call mkgrid('R',Rgrid,geq%Rleft_m,geq%Rleft_m + geq%Rdim_m)
  call mkgrid('Z',Zgrid,geq%Zmid_m-geq%Zdim_m/2,geq%Zmid_m+geq%Zdim_m/2)

  call psirz_minmax(nR,nZ, Rgrid, Zgrid, geq%psiRZ_Wb__Rad, &
       geq%NBBBS,geq%RBBBS_M,geq%ZBBBS_M, psimin,psimax, ising)

  inbmax = max(geq%NBBBS,geq%LIMITR)

  allocate(rbdc(inbmax,2),zbdc(inbmax,2))
  rbdc = 0.0
  zbdc = 0.0

  nc(1)=geq%NBBBS
  nc(2)=geq%LIMITR

  rbdc(1:geq%NBBBS,1) = geq%RBBBS_M(1:geq%NBBBS)
  rbdc(1:geq%LIMITR,2) = geq%RLIM_M(1:geq%LIMITR)

  zbdc(1:geq%NBBBS,1) = geq%ZBBBS_M(1:geq%NBBBS)
  zbdc(1:geq%LIMITR,2) = geq%ZLIM_M(1:geq%LIMITR)

  ipass=0
  do
     ipass = ipass + 1

     if(ipass.eq.1) then
        write(luntty,*) ' EFIT path: ',trim(gfile)
        write(luntty,*) ' EFIT label: ',trim(efit_label)
        write(luntty,*) ' '
        write(luntty,*) ' EFIT G-file scalars values: '
        write(luntty,'(3x,2(1x,a,1pe11.4))') 'Rdim (m): ',geq%rdim_m, &
             'Zdim (m): ',geq%zdim_m
        write(luntty,'(3x,2(1x,a,1pe11.4))') 'Rleft (m): ',geq%rleft_m, &
             'Zmid (m): ',geq%zmid_m
        write(luntty,'(4x,a,1pe11.4)') 'vacuum R*BPhi (T*m): ', &
             geq%rcentr_m*geq%bcentr_t
        write(luntty,'(3x,2(1x,a,1pe11.4))') 'Raxis (m): ',geq%rmaxis_m, &
             'Zaxis (m):',geq%zmaxis_m
        write(luntty,'(4x,a,1pe11.4)') 'Toroidal Current (A): ', geq%current_a
        write(luntty,'(/3x,2(1x,a,1pe11.4)/)') &
             'Psi(0) (Wb/rad): ',geq%SIMAG_Wb__Rad, &
             'Psi(a) (Wb/rad): ',geq%SIBRY_Wb__Rad

        write(luntty,'(4x,a,1pe11.4)') ' min(Psi) w/in bdy: ',psimin
        write(luntty,'(4x,a,1pe11.4)') ' max(Psi) w/in bdy: ',psimax

        psimin0=min(geq%SIMAG_Wb__Rad,geq%SIBRY_Wb__Rad)
        psimax0=max(geq%SIMAG_Wb__Rad,geq%SIBRY_Wb__Rad)

        ztolpsi = (psimax0-psimin0)/20

        if((psimin.le.psimin0-ztolpsi).or.(psimax.ge.psimax0+ztolpsi)) then
           write(luntty,*) ' *** Boundary enclosed Psi(R,Z) range warning *** '
        endif

        if(ising.gt.1) then
           write(luntty,*) ' *** Singularity warning, #extrema.gt.1: ',ising
        endif

        write(luntty,*) ' '

     endif

     if(lxout(0)) then
        write(luntty,*) ' '
        write(luntty,*) ' Geqxpl_RAWplot options: '
        write(luntty,*) '  P -- plot Psi(R,Z) '
        write(luntty,*) '  B -- boundary & limiter plot '
        write(luntty,*) '  S -- print out scalar data '
        write(luntty,*) '  1 -- f(Psi) plot (R*B_phi) '
        write(luntty,*) '  2 -- P(psi) plot '
        write(luntty,*) '  3 -- f*fprime plot '
        write(luntty,*) '  4 -- Pprime plot '
        write(luntty,*) '  5 -- q(Psi) plot '
        write(luntty,*) '  Q -- quit '
     endif

     call uread('Geqxpl_RAWplot option select:$')

     if(lchar('S')) then
        ipass=0
 
     else if(lchar('Q')) then
        exit

     else if(lchar('B')) then

        call r8_grfmm1(rbdc,zbdc,inbmax,nc,2,zc,2,iself,2, &
             'm','m',mnlab,kylab,bdclab)

     else if(lchar('P')) then

        call r8_grf3f1(Rgrid, geq%psirz_Wb__Rad, Zgrid, zwk, nR, nZ, nR, &
             'R','EFIT Psi_poloidal','Z', &
             'm','Wb/rad','m', &
             gfile,efit_label,0)

     else if(lchar('1')) then

        call r8_grafx1(Psi_grid,geq%FPOL_TM,npsi, &
             'Wb/rad','T*m',' Fpol (R*B_phi) vs. Psi',mnlab(1),mnlab(2))

     else if(lchar('2')) then

        call r8_grafx1(Psi_grid,geq%PRES_Nt__M2,npsi, &
             'Wb/rad','Nt/m^2',' Pressure vs. Psi',mnlab(1),mnlab(2))

     else if(lchar('3')) then

        call r8_grafx1(Psi_grid,geq%FFPRIM_T2M2Rad__Wb,npsi, &
             'Wb/rad','(T*m)^2/(Wb/Rad)',' F*Fprime vs. Psi',mnlab(1),mnlab(2))

     else if(lchar('4')) then

        call r8_grafx1(Psi_grid,geq%pprime_NtRad__M2Wb,npsi, &
             'Wb/rad','Nt/m^2/(Wb/rad)',' Pprime vs. Psi',mnlab(1),mnlab(2))

     else if(lchar('5')) then

        call r8_grafx1(Psi_grid,geq%QPSI,npsi, &
             'Wb/rad',' ',' q profile vs. Psi',mnlab(1),mnlab(2))

     else
        call uermsg(' %geqxpl_rawplot: unrecognized option code.',' ',0)
     endif

     cycle

  enddo

  deallocate(psi_grid,Rgrid,Zgrid,zwk)
  deallocate(rbdc,zbdc)

CONTAINS
  subroutine mkgrid(lbl,grid,gmin,gmax)

    !  create equispaced grid

    character*(*), intent(in) :: lbl
    real*8, dimension(:), intent(out) :: grid
    real*8, intent(in) :: gmin,gmax

    integer :: ii,isize

    isize = size(grid)

    if(isize.lt.2) then
       grid=gmin
       write(luntty,*) ' ?geqxpl_rawplot(mkgrid) abnormal '//trim(lbl)// &
            ' grid size: ',isize
       return
    endif

    ! extrema must be different.
    !  gmin -> grid(1); gmax -> grid(isize)
    !   not checked if gmax < gmin

    if(gmin.eq.gmax) then
       write(luntty,*) ' %geqxpl_rawplot(mkgrid) zero range in '//trim(lbl)// &
            ' grid range: '
       write(luntty,*) '  min = max = ',gmin
       grid=gmin
       return
    endif

    grid(1)=gmin
    grid(isize)=gmax

    do ii=2,isize-1
       grid(ii) = ((isize-ii)*gmin + (ii-1)*gmax)/(isize-1)
    enddo

  end subroutine mkgrid

end subroutine geqxpl_rawplot
