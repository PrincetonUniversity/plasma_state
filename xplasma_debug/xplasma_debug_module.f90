module xplasma_debug_module

  !  collection of tools for debugging XPLASMA...

  use xplasma_definitions
  use xpl_dbg_module
  implicit NONE

  private

  public :: xplasma_debug_special
  public :: xplasma_debug_coords, xplasma_debug_grids
  public :: xplasma_debug_profs, xplasma_debug_lists
  public :: xplasma_debug_blkbxs

  !----------------------------------
  ! shared private declarations...
  ! CL fortran interface
  
  integer :: luntty,lnout,idcod
  logical :: lxout,lchar,lyesc

  external :: idcod
  external :: lnout,lxout,lchar,lyesc
  !----------------------------------

  contains

    !=========================================================================
    subroutine xplasma_debug_special(s)

      ! make debug plots specialized to test various specific aspects of
      ! xplasma capabilities

      type(xplasma), pointer :: s

      !----------------------------------
      external xplasma_debug_r4momry
      real*8 ztime,zarea,zvolume
      character*128 labelx,labelt,zgfile
      integer :: ier,inr,inz,inb,idrg,idzg
      !----------------------------------

      luntty = lnout(0)

      sp => s  ! for xplasma_debug_r4momry, and others...

      call xplasma_global_info(s,ier, initLabel=labelx, time=ztime)
      labelt=' '
      write(labelt,'("time=",1pe13.6)') ztime

      if(ier.ne.0) then
         write(luntty,*) ' ?xplasma_debug_special: unexpected error.'
         call xplasma_error(s,ier,luntty)
         return
      endif

      do

         if(lxout(0)) then
            write(luntty,*) ' '
            write(luntty,*) &
                 ' xplasma_debug_special: specialty plots, choose one:'
            write(luntty,*) '  "A" for calculation of area and volume.'
            write(luntty,*) '  "D" for distance calculation tests.'
            write(luntty,*) '  "F" for Fourier Spline moments of equilibrium.'
            write(luntty,*) '  "G" to compute data for and write G-eqdsk file.'
            write(luntty,*) '  "I" for plots of numerical integrals and averages.'
            write(luntty,*) '  "M" for (R,Z) -> (rho,theta) inverse map tests.'
            write(luntty,*) '  "P" for surface parameters: triangularity etc.'
            write(luntty,*) '  "S" for spider plot.'
            write(luntty,*) '  "T" for test of interpolation of noisy sources.'
            write(luntty,*) '  "X" for extrema profiles: Rmin&max, Zmin&max, Bmin&max vs. flux surface.'
            write(luntty,*) '  "Y" for Psi -> rho mapping test.'
            write(luntty,*) '  "Z" for miscellaneous plots.'
            write(luntty,*) ' '
            write(luntty,*) '  "Q" to quit this menu.'
         endif

         call uread('xplasma_debug_special: plot option:')

         if(lchar('Q')) then

            exit

         else if(lchar('A')) then

            call xplasma_volume(s,zvolume,ier)
            if(ier.ne.0) then
               call xplasma_error(s,ier,luntty) 
            else
               write(luntty,*) ' '
               write(luntty,*) ' Plasma volume (m^3): ',zvolume
               write(luntty,*) '   Profile function XPLASMA_VOLUME created.'
            endif

            call xplasma_area(s,zarea,ier)
            if(ier.ne.0) then
               call xplasma_error(s,ier,luntty) 
            else
               write(luntty,*) ' '
               write(luntty,*) ' Plasma poloidal plane area (m^2): ',zarea
               write(luntty,*) '   Profile function XPLASMA_AREA created.'
            endif

         else if(lchar('D')) then

            call xplasma_dmap_test(s)

         else if(lchar('F')) then

            call xpmom_debug
            
         else if(lchar('G')) then

            if(lxout(0)) then
               write(luntty,*) &
                    ' -> enter "N" here to change (R,Z) grid sizes in output file'
               write(luntty,*) &
                    ' -> enter "Q" here to skip writing the output file.'
            endif

            call ureadlw( &
               ' enter filename or file path of g-eqdsk file to be written:', &
               zgfile)
            
            if((zgfile.eq.'q').OR.(zgfile.eq.'Q')) cycle

            if((zgfile.ne.'n').AND.(zgfile.ne.'N')) then

               call xplasma_wr_geqdsk(s,ier, filename=zgfile)
               if(ier.ne.0) then
                  call xplasma_error(s,ier,luntty) 
               endif

            else

               call xplasma_gridId(s,'__Rgrid',idrg)
               call xplasma_gridId(s,'__Zgrid',idzg)

               if(idrg.gt.0) then
                  call xplasma_grid_size(s,idrg,inr,ier)
               else
                  inr=129
               endif

               if(idzg.gt.0) then
                  call xplasma_grid_size(s,idzg,inz,ier)
               else
                  inz=129
               endif

               call ureadi('enter R grid size',inr)
               call ureadi('enter Z grid size',inz)

               inb=max(inr,inz,201)
               call ureadi( &
                    'enter #pts to use for boundary countours in EQDSK',inb)

               call ureadlw( &
                    ' enter filename or file path of g-eqdsk file to be written:', &
                    zgfile)

               call xplasma_wr_geqdsk(s,ier, filename=zgfile, &
                    nh = inr, nv = inz, nbdy = inb)
               if(ier.ne.0) then
                  call xplasma_error(s,ier,luntty) 
               endif

            endif

         else if(lchar('I')) then

            call xplasma_flxint_test(s)

         else if(lchar('M')) then

            call xplasma_imap_test(s)

         else if(lchar('P')) then

            call xplasma_surfgeo_test(s)

         else if(lchar('S')) then

            call grsflux(xplasma_debug_r4momry,'m','m',labelx, &
                 'axisymmetrix flux surfaces',labelt)

         else if(lchar('T')) then

            call xplasma_irhofun_test(s)

         else if(lchar('X')) then

            call xplasma_minmax_test(s)

         else if(lchar('Y')) then

            call xplasma_rhopsi_test(s)

         else if (lchar('Z')) then

            call xplasma_misc_plot(s)

         else
            call uermsg('?xplasma_debug_special: unrecognized plot option.', &
                 ' ',0)
         endif

      enddo

    end subroutine xplasma_debug_special

    !=========================================================================
    subroutine xplasma_surfgeo_test(s)

      !  xplasma_surfgeo call & plot of results

      type (xplasma), pointer :: s

      !-----------------------------------

      real*8, dimension(:), allocatable :: rho,elong,triang,indent
      real*8, dimension(:), allocatable :: zmidp,rin_midp,rout_midp
      real*8, dimension(:,:), allocatable :: auxdata,plot_xdata,plot_ydata
      real*8, dimension(:,:), allocatable :: rcon1,zcon1,rcon2,zcon2

      integer, parameter :: maxplo=15,maxcon=10
      integer, parameter :: isize_aux=19

      integer :: nrho,ntheta
      logical :: reset

      real*8 :: zdummy(maxplo),rdat1,zdat1,rdat2,zdat2
      integer :: nc(maxplo),ncurves,ii,jj,ieven,ierr,ios
      integer :: inth,insurf,ii_odd,ii_even,i48,isurf,indx
      integer :: iself(maxplo)
      integer :: ncth(maxplo)
      character*10 :: xun,yun
      character*80 mnlab(3)
      character*17 kylab(5)
      character*11 lbsel(maxplo),lbsel1(maxplo),lbsel2(maxplo)

      logical :: lxout,lchar

      character*128 labelx,labelt,cbuff
      real*8 ztime

      !---------------------------------------------
      !  some plot labeling setup
      call xplasma_global_info(s,ierr, initLabel=labelx, time=ztime)
      labelt=' '
      write(labelt,'("time=",1pe13.6)') ztime

      mnlab(1)='xplasma_debug_module(xplasma_surfgeo_test) plot'
      mnlab(2)=trim(labelx)//' '//trim(labelt)

      do ii=1,maxplo
         iself(ii) = ii
         zdummy(ii) = ii
      enddo
      iself(1) = -iself(1)  ! prevent squared scale in plots

      kylab = ' '
      kylab(1)='  Surface  '
      kylab(2)='  Parameters  '

      !---------------------------------------------
      reset = .TRUE.

      do
         if(reset) then
            reset = .FALSE.

            if(lxout(0)) then
               write(luntty,*) ' xplasma_surfgeo_test '
               write(luntty,*) ' allowable gridsize ranges: '
               write(luntty,*) '   11  <=  nrho    <= 1001'
               write(luntty,*) '   21  <=  ntheta  <= 1001'
            endif

            nrho=101
            call ureadi('Number of surfaces for xplasma_surfgeo (nrho):',nrho)
            nrho=min(1001,max(11,nrho))

            nc(:) = nrho

            ntheta=400
            call ureadi( &
                 'Number of theta points for xplasma_surfgeo (ntheta):', &
                 ntheta)
            ntheta =min(1001,max(21,ntheta))

            ncth(:) = ntheta

            if(allocated(rho)) then
               deallocate(rho,elong,triang,indent)
               deallocate(zmidp,rin_midp,rout_midp)
               deallocate(auxdata,plot_xdata,plot_ydata)
               deallocate(rcon1,zcon1,rcon2,zcon2)
            endif

            allocate(rho(nrho),elong(nrho),triang(nrho),indent(nrho))
            allocate(zmidp(nrho),rin_midp(nrho),rout_midp(nrho))
            allocate(auxdata(isize_aux,nrho))
            allocate(plot_xdata(nrho,maxplo),plot_ydata(nrho,maxplo))
            allocate(rcon1(ntheta,maxcon),zcon1(ntheta,maxcon))
            allocate(rcon2(ntheta,maxcon),zcon2(ntheta,maxcon))

            rho(1)=0.0d0
            rho(nrho)=1.0d0
            do ii=2,nrho-1
               rho(ii) = (ii-1)*1.0d0/(nrho-1)
            enddo

            do ii=1,maxplo
               plot_xdata(:,ii) = rho
            enddo

            write(luntty,*) ' %NOTE: debug switch set, files created: '// &
                 'eq_bdy.dat, eq_mom.dat, eq_pts.dat!'

            call xplasma_surfgeo(s, rho, &
                 elong, triang, indent, &
                 zmidp, rin_midp, rout_midp, &
                 ierr, &
                 ntheta=ntheta, auxdata=auxdata, &
                 idebug=.TRUE.)

            if(ierr.ne.0) then
               write(luntty,*) ' ?xplasma_surfgeo_test: call failure.'
               call xplasma_error(s,ierr,luntty)
               exit
            endif

            write(luntty,*) ' '
            write(luntty,*) ' calculation OK with ntheta = ',ntheta
            write(luntty,*) '   boundary surface results: '
            write(luntty,'(6x,1pe12.5," -- ",a)') elong(nrho),'elongation'
            write(luntty,'(6x,1pe12.5," -- ",a)') triang(nrho),'triangularity'
            write(luntty,'(6x,1pe12.5," -- ",a)') indent(nrho),'indentation'
            write(luntty,'(6x,1pe12.5," -- ",a)') zmidp(nrho),'Z_midplane (Z centroid)'
            write(luntty,'(6x,1pe12.5," -- ",a)') Rin_midp(nrho),'Rin_midplane'
            write(luntty,'(6x,1pe12.5," -- ",a)') Rout_midp(nrho),'Rout_midplane'
            write(luntty,'(6x,1pe12.5," -- ",a)') auxdata(1,nrho),'Rmin'
            write(luntty,'(6x,1pe12.5," -- ",a)') auxdata(2,nrho),'Rmax'
            write(luntty,'(6x,1pe12.5," -- ",a)') auxdata(3,nrho),'Rtop'
            write(luntty,'(6x,1pe12.5," -- ",a)') auxdata(4,nrho),'Ztop'
            write(luntty,'(6x,1pe12.5," -- ",a)') auxdata(5,nrho),'Rbottom'
            write(luntty,'(6x,1pe12.5," -- ",a)') auxdata(6,nrho),'Zbottom'
            write(luntty,'(6x,1pe12.5," -- ",a)') auxdata(11,nrho),'Lower Holcomb squareness'
            write(luntty,'(6x,1pe12.5," -- ",a)') auxdata(12,nrho),'Upper Holcomb squareness'
            write(luntty,'(6x,1pe12.5," -- ",a)') auxdata(16,nrho),'Lower GYRO squareness'
            write(luntty,'(6x,1pe12.5," -- ",a)') auxdata(19,nrho),'Upper GYRO squareness'

            !  read the debug output file (surface contour comparison)

            rcon1 = 0.0; zcon1 = 0.0; rcon2 = 0.0; zcon2 = 0.0
            lbsel1 = ' '
            lbsel2 = ' '

            call find_io_unit(i48)
            open(unit=i48,file='eq_RZout.dat',status='old',iostat=ios)
            if(ios.ne.0) then
               write(luntty,*) ' ?open failure: eq_RZout.dat; no (R,Z) contours.'
            else
               ! check sizes

               read(i48,'(A)') cbuff
               read(i48,'(1x,2i6)') inth,insurf
               if(inth.ne.ntheta) then
                  write(luntty,*) &
                       ' ?read error: eq_RZout.dat array size (inth)'
                  ios=1
               endif
               if(insurf.ne.maxcon) then
                  write(luntty,*) &
                       ' ?read error: eq_RZout.dat array size (insurf)'
                  ios=1
               endif
            endif

            if(ios.eq.0) then
               !  read labeling information

               read(i48,'(A)') cbuff
               ieven = 1
               ii_odd = 0
               ii_even= 0

               do jj=1,insurf
                  ieven = 1-ieven
                  read(i48,'(1x,2i6,1x,A)') isurf,indx,cbuff
                  if(ieven.eq.1) then
                     ii_even = ii_even + 1
                     lbsel2(ii_even) = trim(cbuff)//'_GYRO'
                     ii_even = ii_even + 1
                     lbsel2(ii_even) = trim(cbuff)//'_orig'
                  else
                     ii_odd = ii_odd + 1
                     lbsel1(ii_odd) = trim(cbuff)//'_GYRO'
                     ii_odd = ii_odd + 1
                     lbsel1(ii_odd) = trim(cbuff)//'_orig'
                  endif
               enddo
               read(i48,'(A)') cbuff
            endif

            if(ios.eq.0) then
               !  read the data

               ieven = 1
               ii_odd = 1
               ii_even= 1

               do jj=1,insurf
                  ieven = 1-ieven
                  do ii=1,ntheta
                     read(i48,'(1x,2i6,4(1x,1pe14.7))') isurf,indx, &
                          rdat1,zdat1,rdat2,zdat2
                     if(ieven.eq.1) then
                        rcon2(ii,ii_even)=rdat1
                        zcon2(ii,ii_even)=zdat1
                        rcon2(ii,ii_even+1)=rdat2
                        zcon2(ii,ii_even+1)=zdat2
                     else
                        rcon1(ii,ii_odd)=rdat1
                        zcon1(ii,ii_odd)=zdat1
                        rcon1(ii,ii_odd+1)=rdat2
                        zcon1(ii,ii_odd+1)=zdat2
                     endif
                  enddo
                  if(ieven.eq.1) then
                     ii_even=ii_even+2
                  else
                     ii_odd=ii_odd+2
                  endif
               enddo
            endif
                  
            if(ios.ne.0) then
            endif

            close(unit=i48,iostat=ios)

         endif

         if(lxout(0)) then
            write(luntty,*) ' xplasma_surfgeo_test: '
            write(luntty,*) ' current grid settings: '
            write(luntty,*) '    nrho = ',nrho
            write(luntty,*) '  ntheta = ',ntheta
            write(luntty,*) ' '
            write(luntty,*) ' Plot options:'
            write(luntty,*) '  "N" -- recompute with different nrho and/or ntheta'
            write(luntty,*) '  "C" -- Z_centroid and corresponding Rin, Rout'
            write(luntty,*) '  "E" -- elongation, triangularity, indentation'
            write(luntty,*) '  "R" -- Rin, Rout, Rmin, Rmax, Rtop, Rbottom'
            write(luntty,*) '  "Z" -- Z_centroid (2 vsns), Ztop, Zbottom'
            write(luntty,*) '  "S" -- Squareness (all definitions)'
            write(luntty,*) '  "1" -- compare contour set #1'
            write(luntty,*) '  "2" -- compare contour set #2'
            write(luntty,*) '  "Q" -- quit'
         endif

         lbsel = ' '

         call uread('xplasma_surfgeo_test plot option:$')
         if(lchar('Q')) then
            exit

         else if(lchar('N')) then
            reset = .TRUE.
            cycle

         else if(lchar('C')) then
            ncurves=3
            mnlab(3) = 'Zmidp (centroid) and Rin & Rout intercepts at each Zmidp'
            
            plot_ydata(:,1) = Zmidp
            plot_ydata(:,2) = Rin_midp
            plot_ydata(:,3) = Rout_midp

            lbsel(1) = 'Zmidp'
            lbsel(2) = 'Rin(Zmidp)'
            lbsel(3) = 'Rout(Zmidp)'

            call r8_grfmm1(plot_xdata,plot_ydata,nrho,nc,ncurves, &
                 zdummy,ncurves,iself,ncurves, &
                 '(rho)','m',mnlab,kylab,lbsel)

         else if(lchar('E')) then
            ncurves=3
            mnlab(3) = 'Elongation, Triangularity, and Indentation'
            
            plot_ydata(:,1) = elong
            plot_ydata(:,2) = triang
            plot_ydata(:,3) = indent

            lbsel(1) = 'elong'
            lbsel(2) = 'triang'
            lbsel(3) = 'indent'

            call r8_grfmm1(plot_xdata,plot_ydata,nrho,nc,ncurves, &
                 zdummy,ncurves,iself,ncurves, &
                 '(rho)',' ',mnlab,kylab,lbsel)

         else if(lchar('R')) then
            ncurves=6
            mnlab(3) = 'Various major radius (R) locations'

            plot_ydata(:,1) = Rin_midp
            plot_ydata(:,2) = Rout_midp
            plot_ydata(:,3) = auxdata(1,:)
            plot_ydata(:,4) = auxdata(2,:)
            plot_ydata(:,5) = auxdata(3,:)
            plot_ydata(:,6) = auxdata(5,:)

            lbsel(1) = 'Rin(Zmidp)'
            lbsel(2) = 'Rout(Zmidp)'
            lbsel(3) = 'Rmin'
            lbsel(4) = 'Rmax'
            lbsel(5) = 'Rtop'
            lbsel(6) = 'Rbottom'

            call r8_grfmm1(plot_xdata,plot_ydata,nrho,nc,ncurves, &
                 zdummy,ncurves,iself,ncurves, &
                 '(rho)','m',mnlab,kylab,lbsel)

         else if(lchar('Z')) then
            ncurves=4
            mnlab(3) = 'Various vertical (Z) locations'

            plot_ydata(:,1) = Zmidp
            plot_ydata(:,2) = auxdata(10,:)
            plot_ydata(:,3) = auxdata(4,:)
            plot_ydata(:,4) = auxdata(6,:)

            lbsel(1) = 'Zmidp'
            lbsel(2) = 'Zmidp#2'
            lbsel(3) = 'Ztop'
            lbsel(4) = 'Zbottom'

            call r8_grfmm1(plot_xdata,plot_ydata,nrho,nc,ncurves, &
                 zdummy,ncurves,iself,ncurves, &
                 '(rho)','m',mnlab,kylab,lbsel)

         else if(lchar('S')) then
            ncurves=4
            mnlab(3) = 'Upper & Lower Holcomb & GYRO squarenesses'
            
            plot_ydata(:,1) = auxdata(11,:)
            plot_ydata(:,2) = auxdata(12,:)
            plot_ydata(:,3) = auxdata(16,:)
            plot_ydata(:,4) = auxdata(19,:)

            lbsel(1) = 'SQHolcomb_L'
            lbsel(2) = 'SQHolcomb_U'
            lbsel(3) = 'SQGYRO_L'
            lbsel(4) = 'SQGYRO_U'

            call r8_grfmm1(plot_xdata,plot_ydata,nrho,nc,ncurves, &
                 zdummy,ncurves,iself,ncurves, &
                 '(rho)',' ',mnlab,kylab,lbsel)

         else if(lchar('1')) then
            ncurves=maxcon
            iself(1) = -iself(1)  ! set square scale

            mnlab(3) = 'Original & GYRO (R,Z) contours comparison #1'

            call r8_grfmm1(rcon1,zcon1,ntheta,ncth,ncurves, &
                 zdummy,ncurves,iself,ncurves, &
                 'm','m',mnlab,kylab,lbsel1)

            iself(1) = -iself(1)  ! restore autoscale

         else if(lchar('2')) then
            ncurves=maxcon
            iself(1) = -iself(1)  ! set square scale

            mnlab(3) = 'Original & GYRO (R,Z) contours comparison #2'

            call r8_grfmm1(rcon2,zcon2,ntheta,ncth,ncurves, &
                 zdummy,ncurves,iself,ncurves, &
                 'm','m',mnlab,kylab,lbsel2)

            iself(1) = -iself(1)  ! restore autoscale

         else
            call uermsg('?xplasma_surfgeo_test: unrecognized plot option.', &
                 ' ',0)
         endif

      enddo

    end subroutine xplasma_surfgeo_test

    !=========================================================================
    subroutine xplasma_irhofun_test(s)

      !  generate noisy "binned" test data; plot original data and 
      !  interpolation results.

      type (xplasma), pointer :: s

      !-----------------------------------

      integer :: i,iplot

      integer, parameter :: nbins=40
      integer, parameter :: nbinb=nbins+1

      integer, parameter :: nbxi=20*nbins
      integer, parameter :: nbxb=nbxi+1

      real*8 :: xbinb(nbinb),volar(nbinb),xbi(nbxi),xbb(nbxb),volarx(nbxb)
      real*8 :: bindata(nbins),binsum(nbins),bintg(nbxb),bindens(nbxi)
      real*8 :: fintg(nbxb),findens(nbxi),ztime,zdvol,zdens

      character*80 labelx,labelt
      integer :: ier,iertmp,imode,ibins,ibin,id_xuse,id_use,ibinc

      character*20 sumlbl(2),denlbl(2),namlbl(2)

      real*8, parameter :: ZERO=0.0d0
      real*8, parameter :: ONE=1.0d0
      !----------------------------------------------------

      do i=1,nbinb
         xbinb(i)=(i-1)*ONE/(nbinb-1)
         if(i.lt.nbinb) bindata(i)=ZERO
      enddo

      call xplasma_author_set(s,'xplasma_debug',iertmp)

      call xplasma_create_grid(s,'RHO_IRHOFUN',xplasma_rho_coord, &
           xbinb,id_xuse,ier, &
           label='Rho grid for xplasma_debug eqm_irhofun test')

      call xplasma_author_clear(s,'xplasma_debug',iertmp)

      if(ier.ne.0) then
         write(luntty,*) ' ?xplasma_irhofun_test: grid creation failure.'
         call xplasma_error(s,ier,luntty)
         return
      endif

      do i=1,nbxb
         xbb(i)=(i-1)*ONE/(nbxb-1)
         if(i.gt.1) xbi(i-1)=(xbb(i-1)+xbb(i))/2
      enddo

      labelx=' '
      call xplasma_global_info(s,ier, initLabel=labelx, time=ztime)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         return
      endif

      labelt=' '
      write(labelt,'("time=",1pe13.6)') ztime
      
      imode=1
      sumlbl(1)='Power (W)'
      sumlbl(2)='Current (A)'
      denlbl(1)='Power W/m^3'
      denlbl(2)='Current A/m^2'
      namlbl(1)='P_DEBUG'
      namlbl(2)='I_DEBUG'

      call xplasma_volume(s,xbinb,volar,ier)
      if(ier.ne.0) then
         write(luntty,*) ' ?xplasma_irhofun_test: failed to get plasma volume.'
         call xplasma_error(s,ier,luntty)
         return
      endif
      call xplasma_volume(s,xbb,volarx,iertmp)
         
      ibin=nbinb/2

      do

         ibins=0
         do i=1,nbins
            if(bindata(i).ne.ZERO) ibins=ibins+1
         enddo

         if(lxout(0)) then
            write(luntty,*) ' There are ',nbins,' bins, of which ',ibins, &
                 ' have non-zero ',trim(sumlbl(imode)),'.'
            write(luntty,*) ' '
            write(luntty,*) ' xplasma_irhofun_test options:'
            write(luntty,*) '  "A" -- add ',trim(sumlbl(imode)),' to a bin'
            write(luntty,*) '  "I" -- plot integrated result before and ', &
                 'after interpolation.'
            write(luntty,*) '  "M" -- reset bins & toggle data type.'
            write(luntty,*) '  "P" -- plot ',trim(sumlbl(imode)),' density ', &
                 'before and after interpolation.'
            write(luntty,*) '  "Q" -- quit.'
         endif

         iplot=0
         call uread('xplasma_irhofun_test option code:')

         if(lchar('Q')) then
            exit

         else if(lchar('A')) then
            
            if(lxout(0)) write(luntty,*) ' Bin number should be between 1 and ',nbins
            call ureadi(' Enter number of bin to modify:',ibin)
            if((ibin.lt.1).or.(ibin.gt.nbins)) then
               call uermsi('bin number input not in range 1 to ',nbins,0)
               cycle
            endif

            call ureadr8(' Enter new '//trim(sumlbl(imode))//' for this bin:',&
                 bindata(ibin))

         else if(lchar('M')) then

            imode=3-imode
            bindata = ZERO

            if(imode.eq.1) then
               call xplasma_volume(s,xbinb,volar,iertmp)
               call xplasma_volume(s,xbb,volarx,iertmp)
            else
               call xplasma_area(s,xbinb,volar,iertmp)
               call xplasma_area(s,xbb,volarx,iertmp)
            endif

         else if(lchar('I')) then
            iplot=1

         else if(lchar('P')) then
            iplot=2

         else
            call uermsg(' ?xplasma_irhofun_test: unrecognized option.',' ',0)
         endif

         if(iplot.eq.0) cycle

         binsum(1)=bindata(1)
         do i=2,nbins
            binsum(i)=binsum(i-1)+bindata(i)
         enddo

         call xplasma_author_set(s,'xplasma_debug',iertmp)

         call xplasma_irhofun(s,id_xuse,namlbl(imode),size(binsum),binsum, &
              imode,id_use,ier)

         call xplasma_author_clear(s,'xplasma_debug',iertmp)

         if(ier.ne.0) then
            write(luntty,*) &
                 ' ?xplasma_irhofun_test: integrated profile creation failed.'
            call xplasma_error(s,ier,luntty)
            cycle
         endif

         call xplasma_eval_prof(s,id_use,xbb,fintg,ier)
         if(ier.ne.0) then
            write(luntty,*) &
                 ' ?xplasma_irhofun_test: integrated profile evaluation failed.'
            call xplasma_error(s,ier,luntty)
            cycle
         endif

         ibin=0
         bintg(1)=0
         do i=1,nbxi
            zdvol=volarx(i+1)-volarx(i)
            if(xbi(i).gt.xbinb(ibin+1)) then
               ibin=ibin+1
               zdens=bindata(ibin)/(volar(ibin+1)-volar(ibin))
            endif
            bindens(i)=zdens
            bintg(i+1)=bintg(i)+zdens*zdvol
            findens(i)=(fintg(i+1)-fintg(i))/zdvol
         enddo

         if(iplot.eq.1) then
            call r8_grafx2(xbb,fintg,bintg,nbxb,'rho',sumlbl(imode), &
                    labelx,labelt, &
                    'interpolated & orginal '//sumlbl(imode)//' data.')
         else
            call r8_grafx2(xbi,findens,bindens,nbxi,'rho',denlbl(imode), &
                    labelx,labelt, &
                    'interpolated & orginal '//denlbl(imode)//' data.')
         endif
      enddo

    end subroutine xplasma_irhofun_test

    !=========================================================================
    subroutine xplasma_minmax_test(s)

      !  profile plots of min & max R & Z & B

      type (xplasma), pointer :: s

      !----------------------------------------------------
      integer :: i,npts,imode,ier
      real*8 :: ztime
      real*8, dimension(:), allocatable :: x,fmin,fmax

      character*2 item(3),uns(3)

      character*80 labelx,labelt
      !----------------------------------------------------

      item(1)='R'; uns(1)='m'
      item(2)='Z'; uns(2)='m'
      item(3)='B'; uns(3)='T'

      imode=1

      npts = 51

      labelx=' '
      call xplasma_global_info(s,ier, initLabel=labelx, time=ztime)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         return
      endif

      labelt=' '
      write(labelt,'("time=",1pe13.6)') ztime

      do
         if(lxout(0)) then
            write(luntty,*) ' ...currently Npts=',npts,' mode= ',item(imode), &
                 ' min & max plots.'
            write(luntty,*) ' options:'
            write(luntty,*) '  "N" to reset Npts'
            write(luntty,*) '  "R" to switch to R min & max mode'
            write(luntty,*) '  "Z" to switch to Z min & max mode'
            write(luntty,*) '  "B" to switch to B min & max mode'
            write(luntty,*) ' -->"P" to produce plot'
            write(luntty,*) '  "Q" to quit.'
         endif
         call uread('xplasma_minmax_test option:')

         if(lchar('Q')) then
            exit

         else if(lchar('N')) then
            call ureadi('Npts for min & max plot (btw 5 and 10000):',npts)
            npts=max(5,min(10000,npts))

         else if(lchar('R')) then
            imode=1

         else if(lchar('Z')) then
            imode=2

         else if(lchar('B')) then
            imode=3

         else if(lchar('P')) then
            !------------------------------
            !  produce the plot...

            allocate(x(npts),fmin(npts),fmax(npts))

            do i=1,npts
               x(i)=(i-1)*1.0d0/(npts-1)

               if(imode.eq.1) then

                  call xplasma_RZminmax(s,x(i),ier, &
                       rmin=fmin(i), rmax=fmax(i))

               else if(imode.eq.2) then

                  call xplasma_RZminmax(s,x(i),ier, &
                       zmin=fmin(i), zmax=fmax(i))

               else if(imode.eq.3) then

                  call xplasma_Bminmax(s,x(i),ier, &
                       bmin=fmin(i), bmax=fmax(i))

               endif
               if(ier.ne.0) exit

            enddo

            if(ier.ne.0) then
               call xplasma_error(s,ier,luntty)

            else
               call r8_grafx2(x,fmin,fmax,npts,'x',uns(imode), &
                    labelx,labelt, &
                    item(imode)//' min & max vs. radial flux coordinate.')
            endif

            deallocate(x,fmin,fmax)
         else
            call uermsg(' ?xplasma_minmax_test: unrecognized option',' ',0)
         endif

      enddo

    end subroutine xplasma_minmax_test

    !=========================================================================
    subroutine xplasma_misc_plot(s)

      !  plot various quantities...
      !  (dlp/dtheta) vs. (rho,theta)
      !  (Btheta) vs. (rho,theta)
      !  q, (dlp/dtheta)*g/(R**2*Btheta)  vs. (rho,theta)

      type (xplasma), pointer :: s

      !----------------------------------------------------
      integer :: id_g,id_q,id_R,id_Z,id_BR,id_BZ,ierr,iertmp
      integer :: id_rho,id_th,inrho,inth,irho,ith
      integer :: ids(7),iderivs(7)
      real*8, dimension(:), allocatable :: rhoi,thi,wk,g,q
      real*8, dimension(:,:), allocatable :: dldth,Bth,R,q2d,fun,wkbuf
      real*8, parameter :: ZERO= 0.0d0
      real*8, parameter :: ONE = 1.0d0
      real*8, parameter :: C2PI=6.2831853071795862D+00

      real*8 :: ztime
      character*80 labelx,labelt
      !----------------------------------------------------

      labelx=' '
      call xplasma_global_info(s,ierr, initLabel=labelx, time=ztime)
      if(ierr.ne.0) then
         call xplasma_error(s,ierr,luntty)
         return
      endif

      labelt=' '
      write(labelt,'("time=",1pe13.6)') ztime

      call xplasma_common_ids(s,ierr, id_g=id_g, &
           id_R=id_R, id_Z=id_Z, id_BR=id_BR, id_BZ=id_BZ)
      if(ierr.ne.0) then
         call xplasma_error(s,ierr,luntty)
         return
      endif

      call ck0('g',id_g)
      call ck0('R',id_R)
      call ck0('Z',id_Z)
      call ck0('BR',id_BR)
      call ck0('BZ',id_BZ)
      if(ierr.ne.0) return

      call xplasma_gen_q(s,'Q_DEBUG',2,id_q,ierr)
      if(ierr.ne.0) then
         call xplasma_error(s,ierr,luntty)
         return
      endif

      ids(1)=id_q; iderivs(1)=0
      ids(2)=id_g; iderivs(2)=0
      ids(3)=id_R; iderivs(3)=0
      ids(4)=id_R; iderivs(4)=1
      ids(5)=id_Z; iderivs(5)=1
      ids(6)=id_BR; iderivs(6)=0
      ids(7)=id_BZ; iderivs(7)=0

      call xplasma_prof_info(s,id_R,ierr,gridId1=id_th,gridId2=id_rho)
      if(ierr.ne.0) then
         call xplasma_error(s,ierr,luntty)
         return
      endif

      call xplasma_grid_info(s,id_th,iertmp, size=inth)
      call xplasma_grid_info(s,id_rho,iertmp, size=inrho)

      !  finer grids for plotting

      inth = 10*(inth-1) + 1
      inrho = 10*(inrho-1) + 1

      allocate(rhoi(inrho),thi(inth),wk(2*max(inth,inrho)),g(inrho),q(inrho))
      allocate(dldth(inth,inrho),Bth(inth,inrho),R(inth,inrho))
      allocate(q2d(inth,inrho),fun(inth,inrho),wkbuf(inth,7))

      wk=ZERO
      do irho=1,inrho
         rhoi(irho)=(irho-1)*ONE/(inrho-1)
      enddo

      do ith=1,inth
         thi(ith)=(ith-1)*C2PI/(inth-1)
      enddo

      do irho=1,inrho
         wk(1:inth)=rhoi(irho)
         call xplasma_eval_prof(s,ids,id_th,thi,id_rho,wk(1:inth),wkbuf,ierr, &
              ideriv1s=iderivs)

         q(irho)=wkbuf(1,1)
         q2d(1:inth,irho)=wkbuf(1:inth,1)
         g(irho)=wkbuf(1,2)
         dldth(1:inth,irho)=sqrt(wkbuf(1:inth,4)**2+wkbuf(1:inth,5)**2)
         Bth(1:inth,irho)=sqrt(wkbuf(1:inth,6)**2+wkbuf(1:inth,7)**2)
         R(1:inth,irho)=wkbuf(1:inth,3)
         if(irho.eq.1) then
            fun(1:inth,irho)=q(irho)
         else
            fun(1:inth,irho)=dldth(1:inth,irho)*g(irho)/ &
                 (Bth(1:inth,irho)*R(1:inth,irho)*R(1:inth,irho))
         endif
      enddo

      do
         if(lxout(0)) then
            write(luntty,1001)
1001        format(/' misc plot options:'/ &
                 ' L -- (dLp/dtheta)'/ &
                 ' B -- |Btheta| (poloidal field)'/ &
                 ' V -- q & (dLp/dtheta)*g/(R**2*Btheta)'/ &
                 ' Q -- quit'/)
         endif
         call uread('xplasma_misc_plot option:')

         if(lchar('L')) then

            call r8_grf3f1(thi,dldth,rhoi,wk,inth,inrho,inth, &
                 'theta','d(Lp)/dtheta','rho','rad','m/rad',' ', &
                 trim(labelx),trim(labelt),0)

         else if(lchar('B')) then

            call r8_grf3f1(thi,Bth,rhoi,wk,inth,inrho,inth, &
                 'theta','mod(Bpol)','rho','rad','T',' ', &
                 trim(labelx),trim(labelt),0)

         else if(lchar('V')) then

            call r8_grf3f2(thi,q2d,fun,rhoi,wk,inth,inrho,inth, &
                 'theta','q','(dLp/dth)*g/(R**2*Bth)','rho','rad',' ',' ', &
                 trim(labelx),trim(labelt),0)

         else if(lchar('Q')) then
            exit
         else
            call uermsg('?unrecognized xplasma_misc_plot option.',' ',0)
         endif
      enddo

      deallocate(rhoi,thi,wk,g,q,dldth,Bth,R,q2d,fun,wkbuf)

      contains
         subroutine ck0(lbl,id)
           character*(*) :: lbl
           integer :: id

           if(id.eq.0) then
              ierr=ierr+1
              write(luntty,*) ' ?not found: ',trim(lbl)
           endif
         end subroutine ck0

    end subroutine xplasma_misc_plot

    !=========================================================================
    subroutine xplasma_rhopsi_test(s)

      !  map Psi --> rho test; i.e. find rho values such that Psi(rho) match
      !  desired target values.

      type (xplasma), pointer :: s

      !----------------------------------------------------
      real*8 :: psimin,psimax,psimin_u,psimax_u,zmin,zmax
      integer, parameter :: npsi=101
      integer :: id_psi,ipsi,iwarn,iertmp,ilen
      real*8 :: psitarg(npsi),xrho(npsi),psichk(npsi),ztime

      character*80 labelx,labelt,lbl1,lbl2
      !----------------------------------------------------

      call xplasma_psi_range(s,psimin,psimax)

      if(psimax.le.psimin) then
         write(luntty,*) ' ?Psi(rho) not ready: Psimin, Psimax = ', &
              psimin,psimax
         return
      endif
      
      call xplasma_common_ids(s,iertmp, id_psi=id_psi)

      labelx=' '
      call xplasma_global_info(s,iertmp, initLabel=labelx, time=ztime)
      if(iertmp.ne.0) then
         call xplasma_error(s,iertmp,luntty)
         return
      endif

      labelt=' '
      write(labelt,'("time=",1pe13.6)') ztime

      ilen=len(trim(labelx))
      if(ilen.lt.48) then
         lbl1 = trim(labelx)//' '//labelt
      else
         lbl1 = labelx
      endif

      lbl2 = 'Psi(rho)-Psi_target = 0 root finder test.'

      if(lxout(0)) then
         write(luntty,*) ' Psi(min) (at mag. axis):  ',psimin
         write(luntty,*) ' Psi(max) (at plasma bdy): ',psimax
         write(luntty,*) ' '
         write(luntty,*) ' User supplied min & max should normally conform'
         write(luntty,*) '   to this range but out of range values should be'
         write(luntty,*) '   handled gracefully.'
         write(luntty,*) ' '
      endif

      call ureadr8('User Psi(min): ',zmin)
      call ureadr8('User Psi(max): ',zmax)

      psimin_u=min(zmin,zmax)
      psimax_u=max(zmin,zmax)

      psitarg(1)=psimin_u
      psitarg(npsi)=psimax_u

      do ipsi=2,npsi-1
         psitarg(ipsi)=(psimin_u*(npsi-ipsi) + psimax_u*(ipsi-1))/(npsi-1)
      enddo

      !----------------------
      !  call the mapper

      call xplasma_rhopsi_find(s,psitarg,xrho,iertmp, iwarn=iwarn)

      write(luntty,*) ' xplasma_rhopsi_find done: ier=',iertmp
      write(luntty,*) ' out of range points: ',iwarn,' out of ',npsi,'.'

      if(iertmp.ne.0) then
         call xplasma_error(s,iertmp,luntty)
      else

         call xplasma_eval_prof(s,id_psi,xrho,psichk,iertmp)


         call r8_grafx2(xrho,psitarg,psichk,npsi,'rho','Wb', &
              lbl1,lbl2, &
              ' Original target Psi compared to Psi(rho_found).')

      endif

    end subroutine xplasma_rhopsi_test

    !=========================================================================
    subroutine xplasma_imap_test(s)

      !  inverse map (R,Z) -> (rho,theta) timing test routine

      type (xplasma), pointer :: s

      !----------------------------------------------------
      integer :: nrays, nsteps
      real*8 :: ztol,ztolf,ztolu,rmin,rmax,zmin,zmax
      real*8 :: rminp,rmaxp,zminp,zmaxp
      real*8 :: rcen,zcen,plen
      logical :: iexec,sol
      integer :: maptype

      real*8, dimension(:), allocatable :: rv,zv,phiv,rhov,thv
      real*8, dimension(:), allocatable :: rp,zp,wkp
      real*8, dimension(:,:), allocatable :: pprho,ppth
      integer, dimension(:), allocatable :: iregion
      integer :: ier,ireg(4),irays,isteps,irg,id_map,i,j,nR,nZ

      real*8 :: zctim1,zctim2,ztheta,zcos,zsin,zlen,zrho1,zth1,ztime
      real*8, parameter :: c2pi = 6.2831853071795862D+00
      character*20 maplabel(3)
      character*80 labelx,labelt
      !----------------------------------------------------

      maplabel(1)='Newton'
      maplabel(2)='Polar'
      maplabel(3)='Bilinear'

      call xplasma_rzminmax_extended(s,rmin,rmax,zmin,zmax,ier)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         return
      endif

      labelx=' '
      call xplasma_global_info(s,ier, initLabel=labelx, time=ztime, &
           scrapeoff=sol)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         return
      endif

      labelt=' '
      write(labelt,'("time=",1pe13.6)') ztime

      nrays = 100
      nsteps = 100
      nr = 100
      nz = 100

      rminp = rmin
      rmaxp = rmax
      zminp = zmin
      zmaxp = zmax

      ztol = 1.0d-10
      ztolf = 1.0d0

      rcen = (rmin+rmax)/2
      zcen = (zmin+zmax)/2

      plen = (rmax-rmin)

      maptype=1

      !  check for fast map -- compute if not already present...

      call xplasma_find_item(s,'__FASTMAP',id_map,ier, nf_noerr=.TRUE.)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         return
      endif

      if(sol.and.(id_map.eq.0)) then
         write(luntty,*) &
              ' ...setting up __FASTMAP (bilinear inverse map) data...:'
         call cpu_time(zctim1)

         call xplasma_ctrans(s,ier, r_in=rcen,z_in=zcen, &
              tol=ztol, maptype=xp_imap_rzlinear, rho_out=zrho1,theta_out=zth1)

         call cpu_time(zctim2)
         write(luntty,*) ' Inverse map complete, cpu time (s): ',zctim2-zctim1
      endif

      do
         write(luntty,1001) nrays,nsteps,nr,nz,rcen,zcen,plen,ztol, &
              trim(maplabel(maptype))
1001     format(/ &
              ' Inverse map timing test:  ',i5,' rays of ',i5,' points each;'/&
              ' Or, plot map on nR x nZ = ',i5,' x ',i5,' rectangle.'// &
              ' Rays start at (',1pd13.6,',',1pd13.6,') and have', &
                  ' length ',1pd13.6,' (m).'/ &
              ' Accuracy tolerance parameter (relative accuracy): ',1pd13.6/ &
              ' Type of inverse map: ',a/ &
              '   (accuracy tolerance applies to Newton map only).')         

         if(lxout(0)) write(luntty,1002)
1002     format( &
              ' P -- compute and plot map on (R,Z) grid'/ &
              ' N -- reset number of rays and/or #pts per ray'/ &
              ' R -- reset start point and/or length of rays'/ &
              ' T -- reset accuracy tolerance'/ &
              ' I -- reset map type option'/ &
              ' X -- EXECUTE test'/ &
              ' Q -- exit this menu without executing test.')

         call uread('xplasma_imap_test option:')

         iexec = .FALSE.

         if(lchar('Q')) then
            exit

         else if(lchar('N')) then
            call ureadi(' # of rays (between 10 and 10000):',nrays)
            nrays=max(10,min(10000,nrays))
            call ureadi(' # of points per ray (between 10 and 10000):',nsteps)
            nsteps=max(10,min(10000,nsteps))

         else if(lchar('P')) then
            do
               if(lxout(0)) then
                  write(luntty,*) ' Map type: ',trim(maplabel(maptype))
                  write(luntty,*) ' Enter "N" to reset nR, nZ: ',nR,nZ
                  write(luntty,*) ' Enter "M" to reset R and/or Z min/max:'
                  write(luntty,*) '    R range: ',rminp,rmaxp
                  write(luntty,*) '    Z range: ',zminp,zmaxp
                  write(luntty,*) ' Enter "P" to procede with plot.'
                  write(luntty,*) ' Enter "Q" to quit.'
               endif
               call uread('xplasma_imap_test plot option:')

               if(lchar('Q')) then
                  exit

               else if(lchar('N')) then
                  if(lxout(0)) write(luntty,*) ' grid sizes btw 10 and 10000:'
                  call ureadi(' new nR:',nR)
                  nR=max(10,min(10000,nR))
                  call ureadi(' new nZ:',nZ)
                  nZ=max(10,min(10000,nZ))

               else if(lchar('M')) then
                  call ureadr8(' new Rmin: ',Rminp)
                  call ureadr8(' new Rmax: ',Rmaxp)
                  if(Rmaxp.le.Rminp) then
                     write(luntty,*) ' ? Rmax > Rmin required; reset.'
                     Rminp=Rmin
                     Rmaxp=Rmax
                     cycle
                  endif
                  call ureadr8(' new Zmin: ',Zminp)
                  call ureadr8(' new Zmax: ',Zmaxp)
                  if(Zmaxp.le.Zminp) then
                     write(luntty,*) ' ? Zmax > Zmin required; reset.'
                     Zminp=Zmin
                     Zmaxp=Zmax
                     cycle
                  endif


               else if(lchar('P')) then
                  allocate(rp(nR),zp(nZ),wkp(max(nR,nZ)))
                  allocate(pprho(nR,nZ),ppth(nr,nZ))
                  allocate(rv(nR),zv(nR),phiv(nR),iregion(nR))

                  wkp = 0

                  do i=1,nR
                     rp(i)=Rminp+(i-1)*(Rmaxp-Rminp)/(nR-1)
                     rv(i)=rp(i)
                     phiv(i)=0
                  enddo
                  do i=1,nZ
                     zp(i)=Zminp+(i-1)*(Zmaxp-Zminp)/(nZ-1)
                  enddo

                  call cpu_time(zctim1)

                  do j=1,nZ
                     zv = zp(j)

                     call xplasma_ctrans(s,.TRUE.,ier, &
                          r_in=rv, phi_in=phiv, z_in=zv, &
                          tol=ztolu, maptype=maptype, &
                          rho_out=pprho(1:nR,j), theta_out=ppth(1:nR,j), &
                          nregion=iregion)
                     if(ier.ne.0) exit

                  enddo

                  if(ier.ne.0) then
                     write(luntty,*) ' ?error in xplasma_ctrans:'
                     call xplasma_error(s,ier,luntty)
                     cycle
                  endif

                  call cpu_time(zctim2)
                  write(luntty,*) ' computation completed over R,Z grids.'
                  write(luntty,*) '   cpu time (s): ',zctim2-zctim1

                  call r8_grf3f1(rp,pprho,zp,wkp,nR,nZ,nR, &
                       'R','rho','Z','m',' ','m', &
                       trim(labelx)//' map type: '//trim(maplabel(maptype)), &
                       labelt,0)

                  call r8_grf3f1(rp,ppth,zp,wkp,nR,nZ,nR, &
                       'R','theta','Z','m','rad','m', &
                       trim(labelx)//' map type: '//trim(maplabel(maptype)), &
                       labelt,0)
                  
                  deallocate(rp,zp,wkp,pprho,ppth)
                  deallocate(rv,zv,phiv,iregion)
               else
                  call uermsg(' ?xplasma_imap_test: unknown plot option.', &
                       ' ',0)
               endif
            enddo

         else if(lchar('R')) then
            if(lxout(0)) then
               write(luntty,*) ' plasma flux surface dimension:'
               write(luntty,'("  Rmin and Rmax: ",2(1x,1pd13.6))') rmin,rmax
               write(luntty,'("  Zmin and Zmax: ",2(1x,1pd13.6))') zmin,zmax
            endif
            call ureadr8(' R start point:',rcen)
            call ureadr8(' Z start point:',zcen)
            call ureadr8(' Length of each ray:',plen)

         else if(lchar('T')) then

            call ureadr8(' Error Tolerance (btw 1.0d-4 and 1.0d-15):',ztol)
            if(ztol.gt.1.0d-4) ztol=1.0d-4
            if(ztol.lt.1.0d-15) ztol=1.0d-15

         else if(lchar('I')) then

            if(lxout(0)) then
               write(luntty,*) ' enter "1" for Newton map;'
               write(luntty,*) ' enter "2" for Polar map;'
               if(.not.sol) then
                  write(luntty,*) ' (Bilinear map unavailable: no scrapeoff).'
               else
                  write(luntty,*) ' enter "3" for Bilinear map.'
               endif
            endif

            call ureadi(' New map type: ',maptype)
            if(sol) then
               maptype=max(1,min(3,maptype))
            else
               maptype=max(1,min(2,maptype))
            endif

         else if(lchar('X')) then

            ztolu = ztol
            iexec = .TRUE.

         else
            call uermsg('?xplasma_imap_test: unrecognized user option.',' ',0)

         endif

         if(iexec) then

            !  *** execute test ***

            allocate(rv(nsteps),zv(nsteps),phiv(nsteps))
            allocate(rhov(nsteps),thv(nsteps),iregion(nsteps))

            ireg = 0

            call cpu_time(zctim1)

            do irays=1,nrays
               if(20*(irays/20).eq.irays) write(luntty,*) ' ...ray #: ',irays
               ztheta = (irays-1)*c2pi/nrays
               zcos = cos(ztheta)
               zsin = sin(ztheta)
               do isteps=1,nsteps
                  zlen=(isteps-1)*plen/(nsteps-1)
                  rv(isteps)=rcen+zlen*zcos
                  zv(isteps)=zcen+zlen*zsin
                  phiv(isteps)=0
               enddo

               call xplasma_ctrans(s,.TRUE.,ier, &
                    r_in=rv, phi_in=phiv, z_in=zv, &
                    tol=ztolu, maptype=maptype, &
                    rho_out=rhov, theta_out=thv, nregion=iregion)
               if(ier.ne.0) then
                  write(luntty,*) ' ?? xplasma_ctran error: '
                  call xplasma_error(s,ier,luntty)
                  exit
               endif

               do isteps=1,nsteps
                  irg=iregion(isteps)
                  if(irg.lt.1) then
                     ireg(4)=ireg(4)+1
                  else if(irg.gt.3) then
                     ireg(4)=ireg(4)+1
                  else
                     ireg(irg)=ireg(irg)+1
                  endif
               enddo
            enddo

            call cpu_time(zctim2)

            write(luntty,2001) trim(maplabel(maptype)), &
                 zctim2-zctim1,ireg(1),ireg(2),ireg(3),ireg(4)
2001        format(/ &
                 ' "',a,'" map test complete:  cpu time = ',1pd13.6,' seconds.'/ &
                 '   in plasma: ',i6,' in mapped region: ',i6, &
                 ' beyond mapped region: ',i6/ &
                 ' unclassified: ',i6/ &
                 ' ------------------------------------------- ')

            deallocate(rv,zv,phiv,rhov,thv,iregion)

         endif

      enddo

    end subroutine xplasma_imap_test


    !=========================================================================
    subroutine xplasma_dmap_test(s)

      !  timing test routine -- calculation of distance from points to
      !  selected flux surface

      type (xplasma), pointer :: s

      !----------------------------------------------------
      real*8 :: rmin,rmax,zmin,zmax
      real*8 :: rminp,rmaxp,zminp,zmaxp
      integer :: maptype

      real*8, dimension(:), allocatable :: rv,zv,phiv
      real*8, dimension(:), allocatable :: rp,zp,wkp
      real*8, dimension(:,:), allocatable :: ppdist,ppth,pprlim,ppzlim
      integer :: ier,id,id_map,i,j,nR,nZ,igot

      real*8 :: zctim1,zctim2,rho_targ,rho_min,rho_bdy,ztime,dist1

      real*8, parameter :: c2pi = 6.2831853071795862D+00
      real*8, parameter :: ZERO = 0.0d0

      character*20 maplabel(3),rholabel,target_str
      character*80 labelx,labelt
      logical :: sol,limtarg,limrz
      integer :: imode,idlim

      character*80 mnlab(3)
      character*17 kylab(5)
      character*10 lbsel(2)
      integer :: in(2),ii(2),inum
      real*8 :: zz(2)
      real*8, dimension(:), allocatable :: rbdy,zbdy,rlim,zlim
      real*8, dimension(:,:), allocatable :: rplo,zplo
      !----------------------------------------------------

      maplabel(1)='Exact'
      maplabel(2)='Circle-fit'
      maplabel(3)='Bilinear'

      call xplasma_RZminmax_extended(s,rmin,rmax,zmin,zmax,ier)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         return
      endif

      labelx=' '
      call xplasma_global_info(s,ier, initLabel=labelx, time=ztime, &
           scrapeoff = sol)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         return
      endif

      call xplasma_find_item(s,'__LIMITER',idlim,ier)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         return
      endif

      limtarg = .FALSE.
      limrz = .FALSE.

      rho_targ = 1
      rho_min = 0.05d0

      if(sol) then

         call xplasma_find_item(sp,"__RHOX",id,ier)
         if(ier.ne.0) then
            call xplasma_error(s,ier,luntty)
            return
         endif

         call xplasma_grid_info(sp,id,ier, xmax=rho_bdy)
         if(ier.ne.0) then
            call xplasma_error(s,ier,luntty)
            return
         endif

      else

         rho_bdy = rho_targ

      endif

      labelt=' '
      write(labelt,'("time=",1pe13.6)') ztime

      nr = 100
      nz = 100

      rminp = rmin
      rmaxp = rmax
      zminp = zmin
      zmaxp = zmax

      maptype=1

      !  check for fast map -- compute if not already present...

      if(sol) then
         write(luntty,*) &
              ' ...checking __DISTMAP (bilinear distance map) data...:'
         call cpu_time(zctim1)

         call xplasma_bdfind(s, rmaxp, (zminp+zmaxp)/2, ier, &
              maptype=3, dist=dist1)

         call cpu_time(zctim2)
         write(luntty,*) ' __DISTMAP complete, cpu time (s): ',zctim2-zctim1
      endif

      do
         rholabel=' '
         write(rholabel,'("rho = ",1pd12.5)') rho_targ
         if(limtarg) then
            target_str="limiter"
         else
            target_str=rholabel(7:)
         endif
         write(luntty,1001) nr,nz,trim(target_str),trim(maplabel(maptype))
1001     format(/ &
              ' Distance map [(R,Z) to target surface] test:'/ &
              ' Plot map result on nR x nZ = ',i5,' x ',i5,' rectangle.'// &
              ' Target surface is: ',a/ &
              ' Type of distance map: ',a/)

         if(lxout(0)) write(luntty,1002) rminp,rmaxp,zminp,zmaxp
1002     format( &
              ' P -- compute and plot map on user defined (R,Z) grids'/ &
              ' N -- reset number (nR,nZ) of user grids'/ &
              ' M -- reset min/max limits of (R,Z) grids'/ &
              '      Rmin/max: ',2(1x,1pd11.4),'; Zmin/max: ',2(1x,1pd11.4)/ &
              ' I -- reset map type option'/ &
              ' L -- plot plasma boundary and limiter'/ &
              ' T -- reset target surface'/ &
              ' Q -- exit this menu without executing test.')

         call uread('xplasma_dmap_test option:')

         if(lchar('Q')) then
            exit

         else if(lchar('N')) then
            if(lxout(0)) write(luntty,*) ' grid sizes btw 10 and 10000:'
            call ureadi(' new nR:',nR)
            nR=max(10,min(10000,nR))
            call ureadi(' new nZ:',nZ)
            nZ=max(10,min(10000,nZ))

         else if(lchar('M')) then
            call ureadr8(' new Rmin: ',Rminp)
            call ureadr8(' new Rmax: ',Rmaxp)
            if(Rmaxp.le.Rminp) then
               write(luntty,*) ' ? Rmax > Rmin required; reset.'
               Rminp=Rmin
               Rmaxp=Rmax
               cycle
            endif
            call ureadr8(' new Zmin: ',Zminp)
            call ureadr8(' new Zmax: ',Zmaxp)
            if(Zmaxp.le.Zminp) then
               write(luntty,*) ' ? Zmax > Zmin required; reset.'
               Zminp=Zmin
               Zmaxp=Zmax
               cycle
            endif

         else if(lchar('P')) then

            allocate(rp(nR),zp(nZ),wkp(max(nR,nZ)))
            allocate(ppdist(nR,nZ),ppth(nr,nZ))
            allocate(pprlim(nR,nZ),ppzlim(nr,nZ))
            allocate(rv(nR),zv(nR),phiv(nR))

            wkp = 0

            do i=1,nR
               rp(i)=Rminp+(i-1)*(Rmaxp-Rminp)/(nR-1)
               rv(i)=rp(i)
               phiv(i)=0
            enddo
            do i=1,nZ
               zp(i)=Zminp+(i-1)*(Zmaxp-Zminp)/(nZ-1)
            enddo

            call cpu_time(zctim1)

            do j=1,nZ
               zv = zp(j)

               if(limtarg) then
                  if(limrz) then
                     call xplasma_lim_distance(s,rv,zv,ppdist(1:nR,j),ier, &
                          maptype=maptype, &
                          rlim=pprlim(1:nR,j), zlim=ppzlim(1:nR,j))
                  else
                     call xplasma_lim_distance(s,rv,zv,ppdist(1:nR,j),ier, &
                          maptype=maptype)
                  endif
               else
                  call xplasma_bdfind(s,nR,rv,zv,ier, &
                       maptype=maptype, &
                       phi_in=phiv, rho_in=rho_targ, &
                       theta_out=ppth(1:nR,j), dist=ppdist(1:nR,j))
               endif
               if(ier.ne.0) exit

            enddo

            if(ier.ne.0) then
               write(luntty,*) ' ?error in xplasma distance finder:'
               call xplasma_error(s,ier,luntty)
               ppth(1:nR,j)=0
               ppdist(1:nR,j)=0
               pprlim(1:nR,j)=0
               ppzlim(1:nR,j)=0
               cycle
            endif

            call cpu_time(zctim2)
            write(luntty,*) ' computation completed over R,Z grids.'
            write(luntty,*) '   cpu time (s): ',zctim2-zctim1

            call r8_grf3f1(rp,ppdist,zp,wkp,nR,nZ,nR, &
                 'R','distance to '//trim(target_str),'Z','m','m','m', &
                 trim(labelx)//' map type: '//trim(maplabel(maptype)), &
                 labelt,0)

            if(.not.limtarg) then

               call r8_grf3f1(rp,ppth,zp,wkp,nR,nZ,nR, &
                    'R','nearest theta at '//trim(rholabel),'Z','m','rad','m',&
                    trim(labelx)//' map type: '//trim(maplabel(maptype)), &
                    labelt,0)

            else if(limrz) then

               call r8_grf3f1(rp,pprlim,zp,wkp,nR,nZ,nR, &
                    'R','R of nearest limiter point','Z','m','m','m',&
                    trim(labelx)//' map type: '//trim(maplabel(maptype)), &
                    labelt,0)

               call r8_grf3f1(rp,ppzlim,zp,wkp,nR,nZ,nR, &
                    'R','Z of nearest limiter point','Z','m','m','m',&
                    trim(labelx)//' map type: '//trim(maplabel(maptype)), &
                    labelt,0)

            endif

            deallocate(rp,zp,wkp,ppdist,ppth,pprlim,ppzlim)
            deallocate(rv,zv,phiv)

         else if(lchar('I')) then

            call set_maptype

         else if(lchar('T')) then

            if(idlim.gt.0) then
               if(lxout(0)) then
                  write(luntty,*) ' available modes:'
                  write(luntty,*) &
                       '  "1" -- distance to plasma surface'
                  write(luntty,*) &
                       '  "2" -- distance to limiter (only)'
                  write(luntty,*) &
                       '  "3" -- distance to limiter, plot nearest point coordinates'
                  write(luntty,*) &
                       '         (i.e. closest point on limiter)'
               endif

               if(.not.limtarg) then
                  imode = 1
               else
                  if(limrz) then
                     imode=3
                  else
                     imode=2
                  endif
               endif

               call ureadi(' select distance mode 1/2/3: ',imode)
               imode=max(1,min(3,imode))

               if(imode.eq.1) then
                  limtarg=.FALSE.
                  limrz=.FALSE.
               else
                  limtarg=.TRUE.
                  limrz=(imode.eq.3)
               endif
            endif

            if(.not.limtarg) then
               if(lxout(0)) then
                  write(luntty,*) &
                       ' range of available target plasma surface rho values: '
                  write(luntty,*) &
                       '      ',rho_min,' to ',rho_bdy
                  write(luntty,*) &
                       '   (bilinear map available for rho=1 only)'
               endif
               call ureadr8(' new target surface rho value (1=bdy): ',rho_targ)
               rho_targ=max(rho_min,min(rho_bdy,rho_targ))
               if(maptype.eq.3) then
                  if(abs(rho_targ-1.0d0).gt.1.0d-8) then
                     maptype=2
                  else
                     rho_targ=1.0d0
                  endif
               endif
            endif

         else if(lchar('L')) then

            if(idlim.eq.0) then
               call uermsg('option not available: no limiter defined.',' ',0)
               cycle
            endif

            inum=nR+nZ
            call ureadi('enter #pts in bdy & limiter contours:',inum)
            inum=max(30,min(10000,inum))

            allocate(rbdy(inum),zbdy(inum),rlim(inum),zlim(inum))
            allocate(rplo(inum,2),zplo(inum,2))

            do 
               call xplasma_bdycon(s,rbdy,zbdy,ier)
               
               if(ier.ne.0) then
                  write(luntty,*) '?xplasma_bdycon error in xplasma_dmap_test:'
                  call xplasma_error(s,ier,luntty)
                  exit
               endif

               call xplasma_limcon(s,rlim,zlim,igot,ier, &
                    tol=ZERO,maptype=maptype)

               if(ier.ne.0) then
                  write(luntty,*) '?xplasma_limcon error in xplasma_dmap_test:'
                  call xplasma_error(s,ier,luntty)
                  exit
               endif

               rplo(1:inum,1)=rbdy
               rplo(1:inum,2)=rlim
               zplo(1:inum,1)=zbdy
               zplo(1:inum,2)=zlim

               zz(1)=1
               zz(2)=2

               ii(1)=1
               ii(2)=2

               in(1)=inum
               in(2)=inum

               mnlab(1)='xplasma_debug_module plot'
               mnlab(2)=trim(labelx)//' '//trim(labelt)
               mnlab(3)='plasma boundary and limiter'

               kylab=' '
            
               lbsel(1)='boundary'
               lbsel(2)='limiter'

               call r8_grfmm1(rplo,zplo,inum,in,2,zz,2,ii,2, &
                    'm','m',mnlab,kylab,lbsel)
               exit
            enddo

            deallocate(rbdy,zbdy,rlim,zlim,rplo,zplo)

         else
            call uermsg('?xplasma_dmap_test: unrecognized user option.',' ',0)
         endif

      enddo

      contains

        subroutine set_maptype

          if(lxout(0)) then
             write(luntty,*) ' enter "1" for Exact map;'
             write(luntty,*) ' enter "2" for Circle Fit approximation;'
             if(sol) then
                write(luntty,*) ' enter "3" for Bilinear map.'
                write(luntty,*) '   (option 3 distance from plasma bdy rho=1 only)'
             else
                write(luntty,*) ' (Bilinear map unavailable: no scrapeoff).'
             endif
          endif

          call ureadi(' New map type: ',maptype)
          if(sol) then
             maptype=max(1,min(3,maptype))
             if(maptype.eq.3) rho_targ=1.0d0
          else
             maptype=max(1,min(2,maptype))
          endif

        end subroutine set_maptype

    end subroutine xplasma_dmap_test

    !=========================================================================
    subroutine xplasma_flxint_test(s)

      !  test numeric integrations ***

      type (xplasma), pointer :: s

      !----------------------------------------------------

      integer, parameter :: ioptions=30
      character*20 options(ioptions),optuns(ioptions),optcur,zxnam,uns,userdef
      character*20 usernew

      data options/'Vol','Lpol','Surf','Itor','Area','dVdrho', &
           '<1/R>','<1/R^2>','<1/R^3>','<R>','<R^2>','<grad(rho)>', &
           '<grad(rho)^2>','<grad(rho)^2/R^2>','<grad(rho)^2/R^3>', &
           '<R^2*grad(rho)^2>','<1/(R*grad(rho))>','<grad(rho)^2/B^2>', &
           '<1/B>','<1/B^2>','<B>','<B^2>','<BZ^2>','<B*R^2>', &
           '<F(H)NCLASS>S','<F(H)NC_TSC>S', '<SQRT(1-B/BMAX)>S', &
           '<user-defined>', &
           '<ONE>','I(C4pi2I)'/

      data optuns/ 'm**3','m',   'm**2','A','m**2','m**3','1/m', &
           'm**-2','m**-3','m','m**2','1/m', &
           'm**-2','m**-4','m**-5', &
           ' ',' ','m**-2*T**-2', &
           '1/T','T**-2','T','T**2','T**2', 'T*m**2', &
           ' ','T**-2',' ','?',' ',' '/
      
      character*80 :: lbl1,lbl2,lbl3,labelx,labelt
      character*24 ttwk(3)
      integer :: iwk,inum_profs,itype,idx1,idx2,idx3,icoord,jcoord,i,ier

      integer :: ix,ixhalf,inzons,inperth,insurf,idi,iopt,ilen,id_u,id_g,id_R
      integer :: id_one,id_c4pi2i,inthzons,inthbdys,ith,idi2d,inx2d,iflagc,id_n
      integer :: id_q,id_Z,id_qnew,id_nubooz,id_iibooz,inthwk,id_curt
      integer :: iert,iertmp,idtmp1,idtmp2,irho,id_phitor
      real*8, dimension(:), allocatable :: xgridi,xgridc,resultc
      real*8, dimension(:), allocatable :: thgridi,thgridc,xwk,thwk,drwk,dzwk
      real*8, dimension(:), allocatable :: gf,flx1,flx2,zwk,q1,q2,ifun,wki,wkj

      integer :: inrhoi,inthi,id_BR,id_BZ,id_Bmod,id_psi
      integer :: ids2(2),ids5(5),iderivs(5),ids6(6),ideriv6(6)
      integer :: ideriv2a(2),ideriv2b(2),ideriv6a(6),ideriv6b(6)
      integer :: ideriv2x(2),ideriv6x(6)
      real*8, dimension(:), allocatable :: rho_intrp,th_intrp,wk_intrp
      real*8, dimension(:,:), allocatable :: mbpol1,mbpol2,wkbuf2,wkbuf5,wkbuf6

      real*8, dimension(:,:), allocatable :: sresult ! surface oriented results
      real*8, dimension(:,:), allocatable :: result2d ! 2d integration results
      real*8 :: ztime,zr2i,zdvol,zdldth
      integer, parameter :: indx_r2i=8
      integer, parameter :: indx_vol=1

      logical :: iinit

      integer, parameter :: iopt_itor = 4  ! points to "Itor" integral option

      real*8, parameter :: ZERO=0.0d0
      real*8, parameter :: ONE=1.0d0
      real*8, parameter :: C2PI=6.2831853071795862D+00
      real*8, parameter :: CMU0=C2PI*2.0d-7

      !---------------------------------------------
      !  initialization...

      call xplasma_common_ids(s,ier,id_R=id_R,id_Z=id_Z,id_g=id_G, &
           id_BR=id_BR,id_BZ=id_BZ,id_Bmod=id_Bmod,id_psi=id_psi)
      if(ier.ne.0) call xplasma_error(s,ier,luntty)
      if((id_R.eq.0).or.(id_G.eq.0).or.(id_BZ.eq.0)) then
         write(luntty,*) ' ?xplasma_flxint_test -- equilibrium not complete.'
         write(luntty,*) '  insufficient data for testing numeric integration.'
         return
      endif

      !  grid sizes for mod(Bpol) (Boozer) comparison

      call xplasma_prof_info(s,id_R,ier, gridId1=idtmp1,gridId2=idtmp2)

      call xplasma_grid_size(s,idtmp1,inthi,ier)
      call xplasma_grid_size(s,idtmp2,inrhoi,ier)

      inthi = 5*(inthi-1) + 1
      inrhoi= 5*(inrhoi-1) + 1

      !  make some test functions -- constants vs. (rho,theta)

      call make1('ONE',ONE,id_one)
      call make1('C4PI2I',ONE/(C2PI*C2PI),id_c4pi2i)

      userdef='BMOD'
      call xplasma_find_item(s,userdef,id_u,ier,nf_noerr=.TRUE.)
      if(id_u.eq.0) then
         userdef='Z'
      endif
         
      inzons=20
      inperth=1
      inthzons=30
      iinit = .FALSE.
      zxnam='x'

      labelx=' '
      call xplasma_global_info(s,ier, initLabel=labelx, time=ztime)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         return
      endif

      labelt=' '
      write(labelt,'("time=",1pe13.6)') ztime

      ilen=len(trim(labelx))
      if(ilen.lt.48) then
         lbl1 = trim(labelx)//' '//labelt
      else
         lbl1 = labelx
      endif

      do
         if(.not.iinit) then

            write(lbl2,'(" No. of x zones: ",i4,"; poloidal resolution multiplier: ",i4)') &
                 inzons,inperth

            if(allocated(sresult)) then
               deallocate(xgridi,xgridc,sresult,resultc)
               deallocate(thgridi,thgridc)
            endif
            insurf = inzons+1
            inthbdys = inthzons+1
         
            allocate(xgridi(insurf),xgridc(inzons),sresult(insurf,ioptions))
            allocate(resultc(inzons))
            allocate(thgridi(inthbdys),thgridc(inthzons))

            iinit=.TRUE.

            do ix=1,insurf
               xgridi(ix)=(ix-1)*ONE/inzons
               if(ix.gt.1) then
                  xgridc(ix-1)=(xgridi(ix)+xgridi(ix-1))/2
               endif
            enddo

            do ith=1,inthbdys
               thgridi(ith)=(ith-1)*C2PI/inthzons
               if(ith.gt.1) then
                  thgridc(ith-1)=(thgridi(ith)+thgridi(ith-1))/2
               endif
            enddo

            call xplasma_author_set(s,'XPLASMA_DEBUG',ier)
            if(ier.ne.0) then
               write(luntty,*) ' ?xplasma_author_set error in xplasma_debug.'
               exit
            endif

            call xplasma_create_integ(s,'DEBUG_INTEGRATOR',xgridi,idi,ier, &
                 nth_expand=inperth, cache_enable=.TRUE.)

            if(ier.ne.0) then
               write(luntty,*) ' ?xplasma_debug: integrator setup failed.'
               call xplasma_error(s,ier,luntty)
               exit
            endif

            call xplasma_augment_integ(s,'DEBUG_INTEG2D',idi,thgridi,idi2d,ier)

            if(ier.ne.0) then
               write(luntty,*) ' ?xplasma_debug: 2d integrator setup failed.'
               call xplasma_error(s,ier,luntty)
               exit
            endif

            call xplasma_author_clear(s,'XPLASMA_DEBUG',ier)
            if(ier.ne.0) then
               write(luntty,*) ' ?xplasma_author_clear error in xplasma_debug.'
               exit
            endif

1001        format(1x,a21,1x,3(1x,1pe17.10))
            ixhalf=(insurf+1)/2
            write(luntty,1001) trim(zxnam)//':', &
                 xgridi(1),xgridi(ixhalf),xgridi(insurf)

            do iopt=1,ioptions

               optcur=options(iopt)
               if(optcur.eq.'<user-defined>') then
                  optcur='<'//trim(userdef)//'>'
                  call xplasma_get_item_info(s,id_u,ier, units=optuns(iopt))
               endif
               ilen=len(trim(optcur))

               if((optcur(ilen:ilen).eq.'>').or.(optcur(ilen:ilen).eq.')')) then
                  call xplasma_rho_zonint(s,idi,trim(optcur)//'S', &
                       sresult(1:insurf,iopt),ier)

               else
                  call xplasma_rho_zonint(s,idi,optcur, &
                       sresult(1:insurf,iopt),ier)

               endif

               if(ier.ne.0) then
                  write(luntty,*) ' ?xplasma_debug: integration failure: ', &
                       optcur
                  call xplasma_error(s,ier,luntty)
                  exit
               endif
               write(luntty,1001) trim(options(iopt))//':', &
                    sresult(1,iopt),sresult(ixhalf,iopt),sresult(insurf,iopt)
            enddo

            if(ier.ne.0) exit
         endif

         ttwk=' '
         if(lxout(0)) then

            write(luntty,*) ' '

            iwk=0
            do iopt=1,ioptions
               iwk=iwk+1
               write(ttwk(iwk),'(i2,". ",a)') iopt,options(iopt)
               if(iwk.eq.3) then
                  write(luntty,'(3(1x,a))') ttwk(1),ttwk(2),ttwk(3)
                  ttwk=' '
                  iwk=0
               endif
            enddo
            if(iwk.gt.0) write(luntty,'(3(1x,a))') ttwk(1),ttwk(2),ttwk(3)

            write(luntty,*) ' '
            write(luntty,*) ' (user-defined="'//trim(userdef)//'")'
            write(luntty,*) ' (N=',inzons,'; M=',inperth,'; P=',inthzons,')'
            write(luntty,*) ' xplasma_flxint_test options:'
            write(luntty,*) '  "F": flux test.'
            write(luntty,*) '  "G": xplasma_gen_q, q profile gen test.'
            write(luntty,*) '  "H": plot integrated toroidal current profile.'
            write(luntty,*) '  "I": (I+g*q) and Boozer normalization tests.'
            write(luntty,*) '  "L": plot [(dL/dtheta)/(Lpol/2pi)] vs. (theta,x).'
            write(luntty,*) '  "N": change number of radial zones of integration.'
            write(luntty,*) '  "M": multiplier on number of poloidal integration steps.'
            write(luntty,*) '  "P": change number of poloidal zones for 2d integrations.'
            write(luntty,*) '  "U": update user-defined integrand definition.'
            write(luntty,*) '  "Q": quit'
            write(luntty,*) ' '
            write(luntty,*) ' enter integer value (as shown above) to plot specific result.'
            write(luntty,*) ' enter -1*[integer value] for result over 2d grid'
            write(luntty,*) ' '
         endif

         call uread('xplasma_flxint_test option:')

         if(lchar('L')) then
            inthwk=201
            call ureadi( &
                 'xplasma_flxint_test: #theta points (btw 100,10000): ', &
                 inthwk)
            inthwk=max(100,min(10000,inthwk))
            allocate(thwk(inthwk),drwk(inthwk),dzwk(inthwk),xwk(inthwk))
            allocate(zwk(max(inthwk,insurf)))
            allocate(result2d(inthwk,insurf))
            iopt=2
            do i=1,inthwk
               thwk(i)=(i-1)*C2PI/(inthwk-1)
            enddo

            do i=1,insurf
               if(xgridi(i).lt.1.0d-7) then
                  result2d(1:inthwk,i)=ONE
               else
                  xwk=xgridi(i)
                  ier=0
                  call xplasma_eval_prof(s,id_R, &
                       xplasma_theta_coord,thwk, xplasma_rho_coord,xwk, &
                       drwk, iertmp, ideriv1=1)
                  ier=max(ier,iertmp)
                  call xplasma_eval_prof(s,id_Z, &
                       xplasma_theta_coord,thwk, xplasma_rho_coord,xwk, &
                       dzwk, iertmp, ideriv1=1)
                  ier=max(ier,iertmp)
                  if(ier.ne.0) then
                     write(luntty,*) ' ?xplasma_flxint_test ier=',ier
                     call xplasma_error(s,ier,luntty)
                     result2d=0
                     exit
                  endif

                  result2d(1:inthwk,i) = &
                       sqrt(drwk*drwk+dzwk*dzwk)/(sresult(i,iopt)/C2PI)
               endif
            enddo

            call r8_grf3f1(thwk,result2d,xgridi,zwk, &
                 inthwk,insurf,inthwk, &
                 'theta','[(dL/dtheta)/(Lpol(x)/2pi)]','x', &
                 'rad',' ',' ', &
                 labelx,labelt,0)

            deallocate(zwk,xwk,thwk,drwk,dzwk,result2d)

         else if(lchar('N')) then
            call ureadi('xplasma_flxint_test: #zones (btw 2,1000): ',inzons)
            inzons=max(2,min(1000,inzons))
            iinit=.FALSE.

         else if(lchar('P')) then
            call ureadi('xplasma_flxint_test: #theta zones (btw 2,100): ', &
                 inthzons)
            inthzons=max(2,min(100,inthzons))
            iinit=.FALSE.

         else if(lchar('M')) then
            call ureadi('xplasma_flxint_test: theta integration step multiplier (btw 1,20): ',inperth)
            inperth=max(1,min(20,inperth))
            iinit=.FALSE.

         else if(lchar('U')) then
            do
               if(lxout(0)) write(luntty,*) ' choose profile vs. (rho,theta).'
               if(lxout(0)) write(luntty,*) ' enter "." to exit.'
               call ureadw( &
                   'xplasma_flxint_test: give profile name or "?" for help:', &
                   usernew)
               if(usernew.eq.'?') then
                  call xplasma_num_items(s,ier,num_profs=inum_profs)
                  call xplasma_debug_profs(s,inum_profs)
                  cycle
               endif
               call xplasma_integ_eligible(s,usernew,id_n,ier)
               if(id_n.eq.0) then
                  call xplasma_error(s,ier,luntty)
                  call uermsg(' item name not eligible',usernew,0)
                  cycle
               endif

               ! OK

               id_u=id_n
               userdef=usernew
               iinit=.FALSE.
               exit
            enddo

         else if(lchar('F')) then

            if(id_g.eq.0) then
               call uermsg(' ?flux test unavailable: no "G" function.',' ',0)
               cycle
            endif

            allocate(gf(inzons),flx1(insurf),flx2(insurf),zwk(insurf))
            call xplasma_eval_prof(s,id_g,xgridc,gf,ier)
            if(ier.ne.0) then
               call xplasma_error(s,ier,luntty)
               cycle
            endif

            flx1(1)=0
            do i=1,inzons
               zdvol=(sresult(i+1,indx_vol)-sresult(i,indx_vol))
               zr2i=(sresult(i,indx_r2i)+sresult(i+1,indx_r2i))/2
               flx1(i+1)=flx1(i)+zdVol*zr2i*gf(i)/C2PI
            enddo

            call xplasma_profId(s,'Phitor',id_phitor)
            if(id_phitor.eq.0) then
               flx2(insurf)=flx1(insurf)
            else
               call xplasma_eval_prof(s,id_phitor,xgridi(insurf),flx2(insurf),&
                    ier)
            endif

            zwk(insurf)=flx1(insurf)-flx2(insurf)
            do i=1,inzons
               flx2(i)=xgridi(i)*xgridi(i)*flx2(insurf)
               zwk(i)=flx1(i)-flx2(i)
            enddo

            write(luntty,*) ' residual fractions: '
            do i=2,insurf
               write(luntty,'(3x,i4,". x=",1pe11.4," f=",1pe11.4)') &
                    i-1,xgridi(i),abs(zwk(i))/flx2(i)
            enddo

            call r8_grafx3(xgridi,flx1,flx2,zwk,insurf,'x','Wb', &
                 lbl1,lbl2, &
                 'Tor. flux: int[dVol*<1/R^2>*g]/2pi; x^2*[Phi_bdy] dashed.')

            deallocate(gf,flx1,flx2,zwk)

         else if(lchar('G')) then

            call xplasma_find_item(s,'Q',id_q,ier,nf_noerr=.TRUE.)
            if(ier.ne.0) then
               call xplasma_error(s,ier,luntty)
               cycle
            endif

            call xplasma_gen_q(s,'Qtest',2,id_qnew,ier)
            if(ier.ne.0) then
               call xplasma_error(s,ier,luntty)
               cycle
            endif

            allocate(q1(insurf),q2(insurf))

            if(id_q.gt.0) call xplasma_eval_prof(s,id_q,xgridi,q1,ier)
            call xplasma_eval_prof(s,id_qnew,xgridi,q2,ier)

            if(id_q.eq.0) then
               call r8_grafx1(xgridi,q2,insurf,'x',' ',lbl1,lbl2, &
                    'q profile from xplasma_gen_q')
            else
               write(luntty,*) ' fractional deviation:'
               do i=1,insurf
                  write(luntty,'(3x,i4,". x=",1pe11.4," f=",1pe11.4)') &
                       i,xgridi(i),abs(q1(i)-q2(i))/max(q1(i),q2(i))
               enddo
               call r8_grafx2(xgridi,q1,q2,insurf,'x',' ',lbl1,lbl2, &
                    'q profile from xplasma_gen_q (dashed) & prior q profile')
            endif

            deallocate(q1,q2)

         else if(lchar('H')) then
            allocate(wki(insurf))
            call xplasma_profId(s,'ICURT',id_curt)
            if(id_curt.gt.0) then
               call xplasma_eval_prof(s,id_curt,xgridi,wki,ier)
               if(ier.ne.0) then
                  write(luntty,*) ' %"ICURT" interpolation failure.'
                  call xplasma_error(s,ier,luntty)
                  id_curt=0
               endif
            endif

            if(id_curt.eq.0) then
               write(luntty,*) ' %no "ICURT" profile found.'
               call r8_grafx1(xgridi,sresult(1:insurf,iopt_itor),insurf, &
                    'x','A',lbl1,lbl2,'integrated toroidal current "Itor"')
            else
               call r8_grafx2(xgridi, &
                    sresult(1:insurf,iopt_itor),wki,insurf, &
                    'x','A',lbl1,lbl2, &
                    'integrated toroidal current "Itor", "ICurT"')
            endif
            deallocate(wki)

         else if(lchar('I')) then

            if(id_g.eq.0) then
               call uermsg(' ? "I" test unavailable: no "G" function.',' ',0)
               cycle
            endif

            call xplasma_gen_q(s,'Qtest',2,id_qnew,ier)
            if(ier.ne.0) then
               call xplasma_error(s,ier,luntty)
               cycle
            endif

            allocate(q1(insurf),q2(insurf),gf(insurf),ifun(insurf))
            allocate(wki(insurf),wkj(insurf))

            call xplasma_eval_prof(s,id_qnew,xgridi,q1,ier)
            call xplasma_eval_prof(s,id_g,xgridi,gf,ier)
            ifun = sresult(1:insurf,iopt_itor)*CMU0/C2PI

            do
               if(lxout(0)) then
                  write(luntty,*) 'options:'
                  write(luntty,*) '  A: plot I*mu0/2pi & g*q separately.'
                  write(luntty,*) '  B: compare q/g with Boozer integral.'
                  write(luntty,*) '  C: compare mod(Bphi): Boozer vs. (g/R).'
                  write(luntty,*) '  D: compare mod(Bpol): Boozer vs. standard splines.'
                  write(luntty,*) '     (this test uses an expression involving the difference'
                  write(luntty,*) '     of two similar numbers and can be expected to give noisy'
                  write(luntty,*) '     results for some input data.  This is only a test; the'
                  write(luntty,*) '     tested Bpol expression would not normally be used).'
                  write(luntty,*) '  E: compare I+gq vs. B**2*R*det(J2d)/(dPsi/dx)'
                  write(luntty,*) '  Q: quit.'
               endif

               call uread('xplasma_flxint_test("I"): option selection:')
               if(lchar('Q')) then
                  exit
               else if(lchar('A')) then
                  q2=q1*gf
                  call r8_grafx2(xgridi,ifun,q2,insurf, &
                       'x','T*m',lbl1,lbl2, &
                       'I*mu0/2pi and g*q profiles')

               else if(lchar('B')) then
                  call xplasma_rho_zonint(s,idi,'Boozer_qovg',wki,ier)
                  if(ier.ne.0) then
                     call xplasma_error(s,ier,luntty)
                     cycle
                  endif
                  q2=q1/gf
                  call r8_grafx2(xgridi,q2,wki,insurf, &
                       'x','1/(T*m)',lbl1,lbl2, &
                       'q/g (solid) and Boozer q/g "zeta" integral')
                  
               else if(lchar('C')) then
                  call xplasma_gen_boozer(s,id_iibooz,id_nubooz,ier)
                  if(ier.ne.0) then
                     call xplasma_error(s,ier,luntty)
                     cycle
                  endif

                  allocate(rho_intrp(inrhoi),th_intrp(inthi))
                  allocate(wk_intrp(2*max(inrhoi,inthi))); wk_intrp=ZERO

                  !  mbpol1,mbpol2 actually used for Bphi comparisons in
                  !  this branch...

                  allocate(mbpol1(inthi,inrhoi),mbpol2(inthi,inrhoi))
                  allocate(wkbuf2(inthi,2),wkbuf6(inthi,6))

                  do irho=1,inrhoi
                     rho_intrp(irho)=(irho-1)*ONE/(inrhoi-1)
                  enddo
                  
                  do ith=1,inthi
                     th_intrp(ith)=(ith-1)*C2PI/(inthi-1)
                  enddo

                  mbpol1(1:inthi,1)=ZERO
                  mbpol2(1:inthi,1)=ZERO

                  ideriv2b = 0
                  ideriv6b = 0

                  ids2(1)=id_BR; ideriv2a(1) = 1  ! d/drho on axis
                  ids2(2)=id_BZ; ideriv2a(2) = 1  ! d/drho on axis

                  ids6(1)=id_R; ideriv6(1)=0; ideriv6a(1)=0
                  ids6(2)=id_R; ideriv6(2)=1; ideriv6a(2)=1
                  ids6(3)=id_Z; ideriv6(3)=1; ideriv6a(3)=1
                  ids6(4)=id_nubooz; ideriv6(4)=1; ideriv6a(4)=0
                  ids6(5)=id_g; ideriv6(5)=0; ideriv6a(5)=0
                  ids6(6)=id_qnew; ideriv6(6)=0; ideriv6a(6)=0

                  !  DMC: include axis; additional d/drho evals there
                  !  for L'Hopital rule 0/0 limit expression

                  do irho=1,inrhoi
                     if(irho.eq.1) then
                        ideriv2x=ideriv2a
                        ideriv6x=ideriv6a
                     else
                        ideriv2x=ideriv2b
                        ideriv6x=ideriv6b
                     endif

                     wk_intrp(1:inthi)=rho_intrp(irho)

                     call xplasma_eval_prof(s,ids2, &
                          xplasma_theta_coord,th_intrp, &
                          xplasma_rho_coord,wk_intrp(1:inthi), &
                          wkbuf2,ier, ideriv2s=ideriv2x)

                     if(ier.ne.0) then
                        write(luntty,*) ' mbpol1 eval failure.'
                        call xplasma_error(s,ier,luntty)
                        exit
                     endif

                     mbpol1(1:inthi,irho) = &
                          sqrt(wkbuf2(1:inthi,1)**2+wkbuf2(1:inthi,2)**2)

                     call xplasma_eval_prof(s,ids6, &
                          xplasma_theta_coord,th_intrp, &
                          xplasma_rho_coord,wk_intrp(1:inthi), &
                          wkbuf6,ier, ideriv1s=ideriv6, ideriv2s=ideriv6x)

                     do ith=1,inthi
                        zdldth=sqrt(wkbuf6(ith,2)**2+wkbuf6(ith,3)**2)
                        mbpol1(ith,irho) = wkbuf6(ith,1)*mbpol1(ith,irho)* &
                             (wkbuf6(ith,6)-wkbuf6(ith,4))/zdldth
                        mbpol2(ith,irho) = wkbuf6(ith,5)/wkbuf6(ith,1)
                     enddo

                     if(ier.ne.0) then
                        write(luntty,*) ' Bphi comparison eval failure.'
                        call xplasma_error(s,ier,luntty)
                        exit
                     endif
                  enddo

                  if(ier.eq.0) then

                     call r8_grf3f2(th_intrp,mbpol1,mbpol2,rho_intrp,wk_intrp,&
                          inthi,inrhoi,inthi, &
                          'theta','Bphi=R*Bpol*[q-dnu/dth]/(dl/dth)', &
                          'Bphi=g/R','x', &
                          'rad','T',' ', &
                          labelx,labelt,0)
                  endif

                  deallocate(rho_intrp,th_intrp,wk_intrp)
                  deallocate(mbpol1,mbpol2,wkbuf2,wkbuf6)
                  
               else if(lchar('D')) then
                  call xplasma_gen_boozer(s,id_iibooz,id_nubooz,ier)
                  if(ier.ne.0) then
                     call xplasma_error(s,ier,luntty)
                     cycle
                  endif

                  allocate(rho_intrp(inrhoi),th_intrp(inthi))
                  allocate(wk_intrp(2*max(inrhoi,inthi))); wk_intrp=ZERO

                  allocate(mbpol1(inthi,inrhoi),mbpol2(inthi,inrhoi))
                  allocate(wkbuf2(inthi,2),wkbuf5(inthi,5))

                  do irho=1,inrhoi
                     rho_intrp(irho)=(irho-1)*ONE/(inrhoi-1)
                  enddo
                  
                  do ith=1,inthi
                     th_intrp(ith)=(ith-1)*C2PI/(inthi-1)
                  enddo

                  mbpol1(1:inthi,1)=ZERO
                  mbpol2(1:inthi,1)=ZERO

                  ids2(1)=id_BR
                  ids2(2)=id_BZ

                  ids5(1)=id_R; iderivs(1)=1
                  ids5(2)=id_Z; iderivs(2)=1
                  ids5(3)=id_iibooz; iderivs(3)=0
                  ids5(4)=id_nubooz; iderivs(4)=1
                  ids5(5)=id_g; iderivs(5)=0

                  do irho=2,inrhoi
                     wk_intrp(1:inthi)=rho_intrp(irho)

                     call xplasma_eval_prof(s,ids2, &
                          xplasma_theta_coord,th_intrp, &
                          xplasma_rho_coord,wk_intrp(1:inthi), &
                          wkbuf2,ier)

                     if(ier.ne.0) then
                        write(luntty,*) ' mbpol1 eval failure.'
                        call xplasma_error(s,ier,luntty)
                        exit
                     endif

                     mbpol1(1:inthi,irho) = &
                          sqrt(wkbuf2(1:inthi,1)**2+wkbuf2(1:inthi,2)**2)

                     call xplasma_eval_prof(s,ids5, &
                          xplasma_theta_coord,th_intrp, &
                          xplasma_rho_coord,wk_intrp(1:inthi), &
                          wkbuf5,ier, ideriv1s=iderivs)

                     do ith=1,inthi
                        zdldth=sqrt(wkbuf5(ith,1)**2+wkbuf5(ith,2)**2)
                        mbpol2(ith,irho) = &
                             (wkbuf5(ith,5)*wkbuf5(ith,4)+wkbuf5(ith,3))/zdldth
                     enddo

                     if(ier.ne.0) then
                        write(luntty,*) ' mbpol2 eval failure.'
                        call xplasma_error(s,ier,luntty)
                        exit
                     endif

                  enddo

                  if(ier.eq.0) then

                     call r8_grf3f2(th_intrp,mbpol1,mbpol2,rho_intrp,wk_intrp,&
                          inthi,inrhoi,inthi, &
                          'theta','Bpol=sqrt(BR**2+BZ**2)', &
                          'mod(Bpol(Boozer formula))','x', &
                          'rad','T',' ', &
                          labelx,labelt,0)
                  endif

                  deallocate(rho_intrp,th_intrp,wk_intrp)
                  deallocate(mbpol1,mbpol2,wkbuf2,wkbuf5)
                  
               else if(lchar('E')) then
                  call xplasma_gen_boozer(s,id_iibooz,id_nubooz,ier)
                  if(ier.ne.0) then
                     call xplasma_error(s,ier,luntty)
                     cycle
                  endif

                  allocate(rho_intrp(inrhoi),th_intrp(inthi))
                  allocate(wk_intrp(2*max(inrhoi,inthi))); wk_intrp=ZERO

                  !  mbpol1,mbpol2 actually used for B**2*det(J) vs. I+gq
                  !  this branch...

                  allocate(mbpol1(inthi,inrhoi),mbpol2(inthi,inrhoi))
                  allocate(wkbuf6(inthi,6))

                  do irho=1,inrhoi
                     rho_intrp(irho)=(irho-1)*ONE/(inrhoi-1)
                  enddo
                  
                  do ith=1,inthi
                     th_intrp(ith)=(ith-1)*C2PI/(inthi-1)
                  enddo

                  mbpol1(1:inthi,1)=ZERO
                  mbpol2(1:inthi,1)=ZERO

                  ids6(1)=id_R; ideriv6(1)=0
                  ids6(2)=id_Bmod; ideriv6(2)=0
                  ids6(3)=id_psi; ideriv6(3)=1   ! d/drho
                  ids6(4)=id_iibooz; ideriv6(4)=0
                  ids6(5)=id_g; ideriv6(5)=0
                  ids6(6)=id_qnew; ideriv6(6)=0

                  do irho=2,inrhoi
                     wk_intrp(1:inthi)=rho_intrp(irho)

                     call xplasma_rzjac(s,wk_intrp(1:inthi),th_intrp,ier, &
                          rzdetj=mbpol1(1:inthi,irho))

                     if(ier.ne.0) then
                        write(luntty,*) ' mbpol1 det(J) eval failure.'
                        call xplasma_error(s,ier,luntty)
                        exit
                     endif

                     call xplasma_eval_prof(s,ids6, &
                          xplasma_theta_coord,th_intrp, &
                          xplasma_rho_coord,wk_intrp(1:inthi), &
                          wkbuf6,ier, ideriv2s=ideriv6)

                     do ith=1,inthi
                        !  B*B*det(J2d)*R/(dPsi/drho)
                        mbpol1(ith,irho) = &
                             abs(mbpol1(ith,irho))*wkbuf6(ith,1)* &
                             wkbuf6(ith,2)*wkbuf6(ith,2)/wkbuf6(ith,3)
                        !  I+g*q
                        mbpol2(ith,irho) = &
                             wkbuf6(ith,4) + wkbuf6(ith,5)*wkbuf6(ith,6)
                     enddo

                     if(ier.ne.0) then
                        write(luntty,*) ' B**2*det(J) comparison eval failure.'
                        call xplasma_error(s,ier,luntty)
                        exit
                     endif
                  enddo
                  write(luntty,*) ' ...at plasma bdy:'
                  write(luntty, &
                       '("   theta=",1pe12.5,"; I,g,q=",3(1pe12.5,1x))') &
                       th_intrp(1),wkbuf6(1,4),gf(insurf),q1(insurf)
                  if(ier.eq.0) then

                     call r8_grf3f2(th_intrp,mbpol1,mbpol2,rho_intrp,wk_intrp,&
                          inthi,inrhoi,inthi, &
                          'theta','B**2*det(J2d)*R/(dPsi/drho)', &
                          'I+g*q','x', &
                          'rad','T*m',' ', &
                          labelx,labelt,0)
                  endif

                  deallocate(rho_intrp,th_intrp,wk_intrp)
                  deallocate(mbpol1,mbpol2,wkbuf6)

               else
                  call uermsg('unrecognized option selection',' ',0)
               endif
            enddo

            deallocate(q1,q2,gf,ifun,wki,wkj)

         else if(lchar('Q')) then
            exit

         else
            iwk = idcod(iert)
            if((iwk.eq.0).or.(abs(iwk).gt.ioptions)) then
               call uermsg(' ?unrecognized xplasma_flxint_test option:',' ',0)
            else if(iwk.gt.0) then
               !  1d plot -- flux surface average or integral
               optcur=options(iwk)
               if(trim(optcur).eq.'<user-defined>') then
                  optcur='<'//trim(userdef)//'>'
               endif

               ilen = len(trim(optcur))
               if((optcur(ilen:ilen).eq.'>').or.(optcur(ilen:ilen).eq.')')) then

                  call xplasma_rho_zonint(s,idi,optcur,resultc,ier)

                  if(optcur.eq.'I(C4pi2I)') then
                     write(luntty,*) &
                          ' multiply zone integral of (1/(4*pi**2)) by iNzons'
                     resultc = resultc*inzons
                  endif

                  if(ier.eq.0) then
                     do i=1,inzons
                        write(luntty,*) i,xgridc(i),resultc(i)
                     enddo
                     if(optcur(ilen:ilen).eq.')') then
                        lbl3 = &
                            trim(optcur)//' vs. x (zone and surface integrals)'
                     else
                        lbl3 = &
                             trim(optcur)//' vs. x (zone and surface averages)'
                     endif
                     call r8_grafxf(xgridc,resultc,inzons, &
                          xgridi,sresult(1:insurf,iwk),insurf, &
                          'x',optuns(iwk),lbl1,lbl2,lbl3)
                  else
                     write(luntty,*) ' ?xplasma_rho_zonint failure:'
                     call xplasma_error(s,ier,luntty)
                  endif

               else
                  lbl3 = trim(optcur)//' vs. x'
                  call r8_grafx1(xgridi,sresult(1:insurf,iwk),insurf, &
                       'x',optuns(iwk),lbl1,lbl2,lbl3)

               endif
            else
               !  2d plot -- contributions or averages vs. (rho,theta)
               iwk = -iwk

               optcur=options(iwk)
               if(trim(optcur).eq.'<user-defined>') then
                  optcur='<'//trim(userdef)//'>'
               endif

               inx2d=inzons

               if(optcur.eq.'Vol') optcur='dVol'
               if(optcur.eq.'Area') optcur='dArea'

               if(optcur.eq.'Lpol') inx2d=insurf
               if(optcur.eq.'Surf') inx2d=insurf
               if(optcur.eq.'Itor') inx2d=insurf
               if(optcur.eq.'dVdrho') inx2d=insurf

               ilen = max(2,len(trim(optcur)))
               if((optcur(ilen-1:ilen).eq.'>S').or. &
                    (optcur(ilen-1:ilen).eq.')S')) then
                  inx2d=insurf
               endif

               iflagc=0
               if(optcur.eq.'I(C4pi2I)') then
                  iflagc=1
                  call uread('enter "S" for segmented surface integral:')
                  if(lyesc('S')) then
                     optcur=trim(options(iwk))//'S'
                     inx2d=insurf
                  endif
               endif

               allocate(result2d(inthzons,inx2d),zwk(max(insurf,inthzons)))
               zwk=0

               call xplasma_2d_zonint(s,idi2d,optcur, &
                    result2d,ier)

               if(iflagc.eq.1) then
                  if(inx2d.eq.insurf) then
                     write(luntty,*)  &
                          ' multiply surface integral of (1/(4*pi**2)) by iNthzons'
                     result2d = result2d * inthzons
                  else
                     write(luntty,*)  &
                          ' multiply zone integral of (1/(4*pi**2)) by iNthzons*iNxzons'
                     result2d = result2d * inthzons*inx2d

                  endif
               endif

               if(inx2d.eq.insurf) then
                  call r8_grf3f1(thgridc,result2d,xgridi,zwk, &
                       inthzons,insurf,inthzons, &
                       'theta',optcur,'x', &
                       'rad',optuns(iwk),' ', &
                       labelx,labelt,0)

               else
                  call r8_grf3f1(thgridc,result2d,xgridc,zwk, &
                       inthzons,inzons,inthzons, &
                       'theta',optcur,'x', &
                       'rad',optuns(iwk),' ', &
                       labelx,labelt,0)

               endif

               deallocate(result2d,zwk)

            endif
         endif

      enddo
            
      if(allocated(sresult)) deallocate(xgridi,xgridc,sresult,resultc)

      !  remove debug integrator

      call xplasma_author_set(s,'XPLASMA_DEBUG',ier)
      call xplasma_remove_item(s,idi,ier)
      call xplasma_remove_item(s,idi2d,ier)
      call xplasma_remove_item(s,id_one,ier)
      call xplasma_remove_item(s,id_c4pi2i,ier)
      call xplasma_author_clear(s,'XPLASMA_DEBUG',ier)

      contains

        subroutine make1(name,val,id)

          !  make a profile f(rho,theta) (matching grids of "R") 
          !  that has constant value...

          character*(*), intent(in) :: name
          real*8, intent(in) :: val
          integer, intent(out) :: id

          !-----------------------------------
          integer :: idx1,idx2,idx3,inx1,inx2,iertmp
          real*8, dimension(:,:), allocatable :: ff
          !-----------------------------------

          call xplasma_prof_info(s,id_R,iertmp, &
               gridId1=idx1,gridId2=idx2,gridId3=idx3)
          if(idx2.eq.0) then
             write(luntty,*) &
                  ' ?make1: expected 2-dimensional "R" profile; found 0 or 1.'
             return
          endif

          if(idx3.ne.0) then
             write(luntty,*) &
                  ' ?make1: expected 2-dimensional "R" profile; found 3.'
             return
          endif

          call xplasma_grid_size(s,idx1,inx1,iertmp)
          call xplasma_grid_size(s,idx2,inx2,iertmp)

          allocate(ff(inx1,inx2))

          ff = val

          call xplasma_author_set(s,'XPLASMA_DEBUG',ier)

          !  piecewise linear OK...
          call xplasma_create_2dprof(s,name, &
               idx1,idx2,ff,id,ier)

          call xplasma_author_clear(s,'XPLASMA_DEBUG',ier)

        end subroutine make1

    end subroutine xplasma_flxint_test

    !=========================================================================
    subroutine xplasma_debug_coords(s,inum)

      ! debugging tool for xplasma -- display information on coordinates.

      type (xplasma), pointer :: s
      integer, intent(in) :: inum   ! number of coordinates

      !----------------------------------
      integer, dimension(:), allocatable :: ids
      integer :: ier
      !----------------------------------

      luntty = lnout(0)

      if(lxout(0)) write(luntty,*) ' %number of known coordinates: ',inum
      if(inum.eq.0) return

      allocate(ids(inum))
      call xplasma_contents(s,ier, id_coords=ids)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         deallocate(ids)
         return
      endif

      do

         if(lxout(0)) then
            write(luntty,*) ' xplasma_debug_coords options:'
            write(luntty,*) '  "T": table of contents.'
            write(luntty,*) '  "S": examine a single coordinate.'
            write(luntty,*) '  "Q": quit.'
         endif

         call uread('xplasma_debug_coords command option:')

         if(lchar('T')) then
            call xplasma_contents_display(s,inum,ids,xplasma_coordType)

         else if(lchar('S')) then
            call xplasma_select1(s,'xplasma coordinate',xplasma_coordType)

         else if(lchar('Q')) then
            exit

         else
            call uermsg('?xplasma_debug_coords: unrecognized command option.',&
                 ' ',0)
         endif
     
      enddo

      deallocate(ids)

    end subroutine xplasma_debug_coords

    !=========================================================================
    subroutine xplasma_debug_grids(s,inum)

      ! debugging tool for xplasma -- display information on grids.

      type (xplasma), pointer :: s
      integer, intent(in) :: inum   ! number of grids

      !----------------------------------
      integer, dimension(:), allocatable :: ids
      integer :: ier
      !----------------------------------

      luntty = lnout(0)

      if(lxout(0)) write(luntty,*) ' %number of known grids: ',inum
      if(inum.eq.0) return

      allocate(ids(inum))
      call xplasma_contents(s,ier, id_grids=ids)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         deallocate(ids)
         return
      endif

      do

         if(lxout(0)) then
            write(luntty,*) ' xplasma_debug_grids options:'
            write(luntty,*) '  "T": table of contents.'
            write(luntty,*) '  "S": examine a single grid.'
            write(luntty,*) '  "Q": quit.'
         endif

         call uread('xplasma_debug_grids command option:')

         if(lchar('T')) then
            call xplasma_contents_display(s,inum,ids,xplasma_gridType)

         else if(lchar('S')) then
            call xplasma_select1(s,'xplasma grid',xplasma_gridType)

         else if(lchar('Q')) then
            exit

         else
            call uermsg('?xplasma_debug_grids: unrecognized command option.', &
                 ' ',0)
         endif

      enddo
      deallocate(ids)

    end subroutine xplasma_debug_grids

    !=========================================================================
    subroutine xplasma_debug_profs(s,inum)

      ! debugging tool for xplasma -- display information on profiles.

      type (xplasma), pointer :: s
      integer, intent(in) :: inum   ! number of profiles

      !----------------------------------
      integer, dimension(:), allocatable :: ids
      integer :: ier
      !----------------------------------

      luntty = lnout(0)

      if(lxout(0)) write(luntty,*) ' %number of known profiles: ',inum
      if(inum.eq.0) return

      allocate(ids(inum))
      call xplasma_contents(s,ier, id_profs=ids)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         deallocate(ids)
         return
      endif

      do

         if(lxout(0)) then
            write(luntty,*) ' xplasma_debug_profs options:'
            write(luntty,*) '  "M": multigraphs.'
            write(luntty,*) '  "T": table of contents (all profiles).'
            write(luntty,*) '  "S": examine a single profile.'
            write(luntty,*) '  "Q": quit.'
         endif

         call uread('xplasma_debug_profs command option:')

         if(lchar('M')) then
            call xplasma_mg_display(s,ids,inum)

         else if(lchar('T')) then
            call xplasma_contents_display(s,inum,ids,xplasma_profType)

         else if(lchar('S')) then
            call xplasma_select1(s,'xplasma profile',xplasma_profType)

         else if(lchar('Q')) then
            exit

         else
            call uermsg('?xplasma_debug_profs: unrecognized command option.', &
                 ' ',0)
         endif

      enddo

      deallocate(ids)

    end subroutine xplasma_debug_profs

    !=========================================================================
    subroutine xplasma_debug_lists(s,inum)

      ! debugging tool for xplasma -- display information on lists.

      type (xplasma), pointer :: s
      integer, intent(in) :: inum   ! number of lists

      !----------------------------------
      integer, dimension(:), allocatable :: ids
      integer :: ier
      !----------------------------------

      luntty = lnout(0)

      if(lxout(0)) write(luntty,*) ' %number of known lists: ',inum
      if(inum.eq.0) return

      allocate(ids(inum))
      call xplasma_contents(s,ier, id_lists=ids)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         deallocate(ids)
         return
      endif

      do

         if(lxout(0)) then
            write(luntty,*) ' xplasma_debug_lists options:'
            write(luntty,*) '  "T": table of contents.'
            write(luntty,*) '  "S": examine a single list.'
            write(luntty,*) '  "Q": quit.'
         endif

         call uread('xplasma_debug_lists command option:')

         if(lchar('T')) then
            call xplasma_contents_display(s,inum,ids,xplasma_listType)

         else if(lchar('S')) then
            call xplasma_select1(s,'xplasma list',xplasma_listType)

         else if(lchar('Q')) then
            exit

         else
            call uermsg('?xplasma_debug_lists: unrecognized command option.', &
                 ' ',0)
         endif

      enddo

      deallocate(ids)

    end subroutine xplasma_debug_lists

    !=========================================================================
    subroutine xplasma_debug_blkbxs(s,inum)

      ! debugging tool for xplasma -- display information on black boxes.

      type (xplasma), pointer :: s
      integer, intent(in) :: inum   ! number of black boxes

      !----------------------------------
      integer, dimension(:), allocatable :: ids,ids_mcgrid
      integer :: ier,inum_mcgrid,id_mcgrid1,ii
      character*32 zname
      !----------------------------------

      luntty = lnout(0)

      if(lxout(0)) write(luntty,*) ' %number of known black boxes: ',inum
      if(inum.eq.0) return

      allocate(ids(inum))
      call xplasma_contents(s,ier, id_blkbxs=ids)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         deallocate(ids)
         return
      endif

      do

         if(lxout(0)) then
            write(luntty,*) ' xplasma_debug_blkbxs options:'
            write(luntty,*) '  "M": examine MCgrid objects only.'
            write(luntty,*) '  "T": table of contents (all black boxes).'
            write(luntty,*) '  "S": examine available info on a single black box.'
            write(luntty,*) '  "Q": quit.'
         endif

         call uread('xplasma_debug_blkbxs command option:')

         if(lchar('M')) then
            call xplasma_mcgrid_find(s,ier,num_mcgrids=inum_mcgrid)
            if(ier.ne.0) then
               call xplasma_error(s,ier,luntty)
               cycle
            else if(inum_mcgrid.eq.0) then
               call uermsg('no MCgrid objects found.',' ',0)
               cycle
            endif

            if(allocated(ids_mcgrid)) deallocate(ids_mcgrid)
            allocate(ids_mcgrid(inum_mcgrid))
            call xplasma_mcgrid_find(s,ier,id_mcgrids=ids_mcgrid)

            if(lxout(0)) then
               do ii=1,inum_mcgrid
                  id_mcgrid1=ids_mcgrid(ii)
                  call xplasma_get_item_info(s,id_mcgrid1,ier, name=zname)
                  write(luntty,*) ii,".  "//trim(zname)
               enddo
               write(luntty,*) &
                    ' enter MCgrid index number or 0 or "Q" to skip:'
            endif
            id_mcgrid1=0
            call ureadi('MCgrid index: ',id_mcgrid1)
            if(id_mcgrid1.eq.0) cycle
            if((id_mcgrid1.lt.0).or.(id_mcgrid1.gt.inum_mcgrid)) then
               write(luntty,*) ' available index range: 1 to ',inum_mcgrid
               write(luntty,*) ' so: ',id_mcgrid1,' is out of range.'
               call uclear
               cycle
            endif

            id_mcgrid1=ids_mcgrid(id_mcgrid1)
            call xplasma_debug_mcgrid(s,id_mcgrid1)

         else if(lchar('T')) then
            call xplasma_contents_display(s,inum,ids,xplasma_blkbxType)

         else if(lchar('S')) then
            call xplasma_select1(s,'xplasma "black box"',xplasma_blkbxType)

         else if(lchar('Q')) then
            exit

         else
            call uermsg('?xplasma_debug_blkbxs: unrecognized command option.',&
                 ' ',0)
         endif

      enddo
      deallocate(ids)

    end subroutine xplasma_debug_blkbxs

    !=========================================================================
    subroutine xplasma_debug_mcgrid(s,id_mcgrid)

      !  list/plot objects associated with id_mcgrid

      type(xplasma), pointer :: s
      integer, intent(in) :: id_mcgrid

      !-----------------------------
      integer :: ierr
      integer :: inum,icount,nzons
      integer, dimension(:), allocatable :: ids
      real*8, dimension(:), allocatable :: zvols
      !-----------------------------

      icount=0
      call xplasma_display1(s,id_mcgrid,'*','*', &
           .TRUE.,.TRUE.,.TRUE.,.FALSE.,luntty,icount)

      call xplasma_mcgrid_info(s,id_mcgrid,ierr, nzons=nzons)
      if(ierr.ne.0) then
         call xplasma_error(s,ierr,luntty)
         return
      endif

      !  make sure volumes are defined...
      allocate(zvols(nzons))

      call xplasma_mcgrid_volumes(s,id_mcgrid,zvols,ierr)
      if(ierr.ne.0) then
         call xplasma_error(s,ierr,luntty)
         return
      endif

      ! inum >= 1 assured, as the volumes count as a profile...

      call xplasma_mcgrid_findProfs(s,id_mcgrid,ierr, num_profs=inum)
      if(ierr.ne.0) then
         call xplasma_error(s,ierr,luntty)
         return
      endif

      allocate(ids(inum))
      call xplasma_mcgrid_findProfs(s,id_mcgrid,ierr, id_profs=ids)

      call xplasma_contents_display(s,inum,ids,xplasma_blkbxType)

    end subroutine xplasma_debug_mcgrid

    !=========================================================================

    !=========================================================================

    !=========================================================================

    !=========================================================================
    !  **private**

    subroutine xplasma_select1(s,zlbl,itype)

      !  select a single item for more detailed display

      type (xplasma), pointer :: s
      character*(*), intent(in) :: zlbl  ! short label for type of item sought.
      integer, intent(in) :: itype       ! type of item sought: grid, profile, ...

      !---------------------------
      character*32 name
      integer :: id,ier,idum,jtype
      logical :: more_options
      !---------------------------

      luntty = lnout(0)

      call ureadw('name of '//trim(zlbl)//':',name)
      call xplasma_find_item(s,name,id,ier)
      if(id.eq.0) then
         write(luntty,*) ' '
         write(luntty,*) ' not found: ',trim(name)//'.'
         call uclear
      else

         call xplasma_get_item_info(s,id,ier, itype=jtype)
         if(itype.ne.jtype) then
            write(luntty,*) ' '
            write(luntty,*) ' ',trim(name),' is not a ',trim(zlbl)//'.'
            call uclear
         else
            write(luntty,*) &
                 ' -----------------------------------------------------------'
            more_options=.TRUE.
            idum=0
            call xplasma_display1(s,id,'*','*', &
                 .TRUE.,.TRUE.,.TRUE.,more_options,luntty,idum)

            write(luntty,*) &
                 ' -----------------------------------------------------------'
         endif

      endif

    end subroutine xplasma_select1

    !=========================================================================
    !  **private**

    subroutine xplasma_contents_display(s,inum,ids,itype)

      ! print table of contents -- items in the list ids(1:inum) which are of
      ! type (itype)

      type (xplasma), pointer :: s
      integer, intent(in) :: inum   ! number of items
      integer, intent(in) :: ids(inum)  ! the item ids
      integer, intent(in) :: itype  ! type of items

      !----------------------------------
      logical :: iunits,ilabel,idetails,more_options
      character*78 units,label
      character*78 hdr,buf
      character*32 name_ss,auth_ss,name,lbl01
      integer :: icount,i,ier,id,idum
      !----------------------------------

      luntty = lnout(0)

      iunits=.FALSE.
      ilabel=.FALSE.
      idetails=.FALSE.

      name_ss='*'
      auth_ss='*'

      hdr=' '
      if(itype.eq.xplasma_coordType) then
         lbl01 = 'xplasma coordinate'
         write(hdr,*) ' Table of Contents: xplasma coordinates.'
      else if(itype.eq.xplasma_gridType) then
         lbl01 = 'xplasma grid'
         write(hdr,*) ' Table of Contents: xplasma grids.'
      else if(itype.eq.xplasma_profType) then
         lbl01 = 'xplasma profile'
         write(hdr,*) ' Table of Contents: xplasma profiles.'
      else if(itype.eq.xplasma_listType) then
         lbl01 = 'xplasma list'
         write(hdr,*) ' Table of Contents: xplasma lists.'
      else if(itype.eq.xplasma_blkbxType) then
         lbl01 = 'xplasma "black box"'
         write(hdr,*) ' Table of Contents: xplasma "black boxes".'
      endif

      do
         if(lxout(0)) then
            write(luntty,*) ' '
            write(luntty,*) ' -------------------------'
            write(luntty,*) ' Contents Display Options:'
            write(luntty,*) '    display label: ',ilabel
            write(luntty,*) '    display units: ',iunits
            write(luntty,*) '    display details: ',idetails
            write(luntty,*) '    name substring:   ',trim(name_ss)
            write(luntty,*) '    author substring: ',trim(auth_ss)
            write(luntty,*) '      (case insensitive; "*" means wildcard)'
            write(luntty,*) ' '
            write(luntty,*) ' xplasma_contents_display user options:'
            write(luntty,*) '  "L": toggle display label switch.'
            write(luntty,*) '  "U": toggle display units switch.'
            write(luntty,*) '  "D": toggle details display switch.'
            write(luntty,*) '  "N": set name substring.'
            write(luntty,*) '  "A": set author substring.'
            write(luntty,*) ' '
            write(luntty,*) '  "P": print table of contents.'
            write(luntty,*) '  "S": examine a single item.'
            write(luntty,*) '  "Q": quit.'

            write(luntty,*) ' '
         endif

         call uread('xplasma_contents_display user option:')

         if(lchar('L')) then
            ilabel = .not.ilabel

         else if(lchar('U')) then
            iunits = .not.iunits

         else if(lchar('D')) then
            idetails = .not.idetails

         else if(lchar('N')) then
            if(lxout(0)) write(luntty,*) ' --> enter search substring, or,'
            if(lxout(0)) write(luntty,*) '     prepend "^" for prefix;'
            if(lxout(0)) write(luntty,*) '     append "$" for suffix;'
            if(lxout(0)) write(luntty,*) ' --> or enter "*" for all names.'
            call uread_cmd_disable
            call ureadw('search substring for names:',name_ss)
            call uread_cmd_enable
            call uupper(name_ss)

         else if(lchar('A')) then
            if(lxout(0)) write(luntty,*) ' --> enter search substring, or,'
            if(lxout(0)) write(luntty,*) '     prepend "^" for prefix;'
            if(lxout(0)) write(luntty,*) '     append "$" for suffix;'
            if(lxout(0)) write(luntty,*) ' --> enter "*" for all authors.'
            call uread_cmd_disable
            call ureadw('search substring for authors:',auth_ss)
            call uread_cmd_enable
            call uupper(auth_ss)

         else if(lchar('P')) then

            !---------------------------------------------------------------
            !  generate the printout...

            write(luntty,*) ' '
            write(luntty,*) trim(hdr)
            write(luntty,*) ' '
            write(luntty,*) &
                 ' Name                              Author'
            write(luntty,*) &
                 ' -----------------------------------------------------------'

            icount=0
            more_options = .FALSE.
            do i=1,inum
               call xplasma_display1(s,ids(i),name_ss,auth_ss, &
                    ilabel,iunits,idetails,more_options,luntty,icount)
            enddo

            if(icount.eq.0) write(luntty,*) '   (nothing found)'

            !---------------------------------------------------------------

         else if(lchar('S')) then

            call xplasma_select1(s,lbl01,itype)

         else if(lchar('Q')) then
            exit

         else
            call uermsg(' ?xplasma_contents_display: unknown user option.', &
                 ' ',0)
         endif
      enddo

    end subroutine xplasma_contents_display

    !=========================================================================
    !  **private**

    subroutine xpdebug_list_filter(zzname,zzauth,name_ss,auth_ss, &
              name_ok,auth_ok)

      !  routine to determine if item is eligible for display or not...

      character*(*), intent(in) :: zzname,zzauth    ! name & author of item
      character*(*), intent(in) :: name_ss,auth_ss  ! substring controls
      logical, intent(out) :: name_ok,auth_ok       ! T/F for each test

      integer :: j,ilen,jlen

      name_ok = (name_ss.eq.'*')
      ilen=len(trim(zzname))
      jlen=len(trim(name_ss))
      if((name_ss(1:1).eq.'^').and.(name_ss(jlen:jlen).eq.'$')) then
         !  whole name
         if(jlen.gt.2) then
            name_ok = zzname(1:ilen).eq.name_ss(2:jlen-1)
         else
            name_ok = .FALSE.
         endif
      else if(name_ss(1:1).eq.'^') then
         !  prefix
         if(jlen.gt.1) then
            name_ok = zzname(1:jlen-1).eq.name_ss(2:jlen)
         else
            name_ok = .FALSE.
         endif
      else if(name_ss(jlen:jlen).eq.'$') then
         !  suffix
         if(jlen.gt.1) then
            name_ok = zzname(ilen-jlen+2:ilen).eq.name_ss(1:jlen-1)
         else
            name_ok = .FALSE.
         endif
      else
         !  substring
         do j=1,ilen-jlen+1
            if(name_ok) exit
            name_ok = zzname(j:j+jlen-1).eq.name_ss(1:jlen)
         enddo
      endif

      auth_ok = (auth_ss.eq.'*')
      ilen=len(trim(zzauth))
      jlen=len(trim(auth_ss))
      if((auth_ss(1:1).eq.'^').and.(auth_ss(jlen:jlen).eq.'$')) then
         !  whole auth
         if(jlen.gt.2) then
            auth_ok = zzauth(1:ilen).eq.auth_ss(2:jlen-1)
         else
            auth_ok = .FALSE.
         endif
      else if(auth_ss(1:1).eq.'^') then
         !  prefix
         if(jlen.gt.1) then
            auth_ok = zzauth(1:jlen-1).eq.auth_ss(2:jlen)
         else
            auth_ok = .FALSE.
         endif
      else if(auth_ss(jlen:jlen).eq.'$') then
         !  suffix
         if(jlen.gt.1) then
            auth_ok = zzauth(ilen-jlen+2:ilen).eq.auth_ss(1:jlen-1)
         else
            auth_ok = .FALSE.
         endif
      else
         !  substring
         do j=1,ilen-jlen+1
            if(auth_ok) exit
            auth_ok = zzauth(j:j+jlen-1).eq.auth_ss(1:jlen)
         enddo
      endif

    end subroutine xpdebug_list_filter

    subroutine xplasma_display1(s,id,name_ss,auth_ss,ilabel,iunits,idetails, &
         more_options,luntty,icount)

      use xplasma_definitions
      implicit NONE

      !  display one item with controls

      type (xplasma), pointer :: s

      integer, intent(in) :: id   ! item id

      character*(*), intent(in) :: name_ss  ! name substring display control
      ! "*" means wildcard; otherwise display iff name_ss is substring of name

      character*(*), intent(in) :: auth_ss  ! author substring display control
      ! "*" means wildcard; otherwise display iff auth_ss is substring of auth.

      logical, intent(in) :: ilabel    ! flag to display item label
      logical, intent(in) :: iunits    ! flag to display item units label
      logical, intent(in) :: idetails  ! flag to display details of item
      !  what this means depends on the type of item.

      logical, intent(in) :: more_options  ! flag whether to offer more options
      !  what this means depends on the type of item.
      !  ***only available if idetails is also set ***

      integer, intent(in) :: luntty    ! LUN where to write ascii information
      logical :: lchar,lxout,lyesc
      external :: lchar,lxout,lyesc

      integer, intent(inout) :: icount ! Increment if item is displayed.

      !----------------------------------------------------------------
      integer :: ier
      character*32 name,zzname,auth,zzauth,units1,units2,units3
      character*78 label,units
      character*128 labelx,labelt,label0,label1,label2
      character*80 resolution
      character*80 grids
      character*80 coords
      character*128 buf
      logical :: name_ok,auth_ok
      integer :: ilen,jlen,i,j,itype
      integer :: ngrids, isize
      real*8, dimension(:), allocatable :: x1buf,x2buf,x3buf
      integer :: inx1,inx2,inx3
      real*8 :: xmin,xmax,ztime
      !-------------------------------
      !  for profile display...
      integer :: irank,ispline,idx1,idx2,idx3,jcount,idmax,id_assoc
      !----------------------------------------------------------------

      call xplasma_global_info(s,ier, initLabel=labelx, time=ztime); call ckerr
      labelt=' '
      write(labelt,'("time=",1pe13.6)') ztime
      ilen=len(trim(labelx))
      if(ilen.lt.48) then
         label0 = trim(labelx)//' '//labelt
      else
         label0 = labelx
      endif

      call xplasma_get_item_info(s,id,ier, &
           itype=itype, &
           name=name, &
           author=auth, &
           label=label, &
           units=units)
      if(ier.ne.0) then
         write(luntty,*) ' *** error on item, id=',id
         call ckerr
      else
         zzname=name; call uupper(zzname)
         zzauth=auth; call uupper(zzauth)

         call xpdebug_list_filter(zzname,zzauth,name_ss,auth_ss, &
              name_ok,auth_ok)

         if(name_ok.and.auth_ok) then
            buf = ' '//name//'  by: '//auth
            write(luntty,*) trim(buf)
            if(ilabel) write(luntty,*) '    label: ',trim(label)
            if(iunits) write(luntty,*) '    units: ',trim(units)
            
            if(idetails) then
               if(itype.eq.xplasma_coordType) then
                  call coord_display
               else if(itype.eq.xplasma_gridType) then
                  call grid_display
               else if(itype.eq.xplasma_profType) then
                  call prof_display
               else if(itype.eq.xplasma_listType) then
                  call list_display
               else if(itype.eq.xplasma_blkbxType) then
                  call blkbx_display
               endif
            endif

            icount=icount+1

         endif
      endif

    contains

      subroutine ckerr

        if(ier.ne.0) then
           write(luntty,*) ' ?unexpected error in xplasma_display1:'
           call xplasma_error(s,ier,luntty)
        endif

      end subroutine ckerr

      !------------------------------------------------
      subroutine blkbx_display

        !  details on xplasma "black box"; label & units shown separately

        !------------------------------------------
        integer :: iitype,isize,r8size
        character*32 zname

        integer :: nzrow,nzrow_ext,nth0,nzons,nzons_ext,ierr
        integer, dimension(:), allocatable :: nths,nthsum
        logical :: udsym

        integer :: nzons_prof,id_mcgrid,irank,idim1,idim2
        integer :: inrho,inth,inr,inz,ii,jj,kk,kkp,inwk,iertmp,iwarn
        integer :: nrow,ifix1,ifix2,ie,ix
        integer :: gridId1,gridId2,normcode
        integer :: integ,indx,irow,ithz

        real*8 :: zrmin,zrmax,zzmin,zzmax,rhomax,ztheta
        real*8, dimension(:), allocatable :: wk,rr,zz,rho,th,dat0,zvols
        real*8, dimension(:), allocatable :: zgrid1,zgrid2,zgrid1c,zgrid2c
        integer :: ing1,ing2
        real*8, dimension(:,:), allocatable :: dat1
        real*8, dimension(:,:,:), allocatable :: dat2
        real*8, dimension(:), allocatable :: rhomap,thmap
        real*8, dimension(:,:), allocatable :: plotrz,plotfl
        logical :: orig_data
        character*32 :: zgname1,zgname2,zgunits1,zgunits2,plunits
        character*80 :: zglabel1,zglabel2,fflabel

        real*8, parameter :: CPI = 3.1415926535897931D+00
        real*8, parameter :: ZERO = 0.0d0
        real*8, parameter :: HALF = 0.5d0
        real*8, parameter ::  ONE = 1.0d0

        integer, parameter :: inper = 5
        !------------------------------------------

        call xplasma_blackbox_info(s,id,ier, &
             type=iitype, isize=isize, r8size=r8size)
        write(luntty,*) '    black box type code: ',iitype

        if(iitype.eq.xplasma_bbgtype) then
           write(luntty,*) ' --> item appears to be a MCgrid'

           call xplasma_mcgrid_info(s,id,ierr,  &
                nzrow=nzrow, nzrow_ext=nzrow_ext, &
                nzons=nzons, nzons_ext=nzons_ext,&
                nth0=nth0, udsym=udsym)

           if(ierr.ne.0) then
              call xplasma_error(s,ierr,luntty)
              return
           endif

           write(luntty,*) ' '
           write(luntty,*) '      updown symmetry: ',udsym
           write(luntty,*) '  #rows inside plasma: ',nzrow
           write(luntty,*) '          total #rows: ',nzrow_ext
           write(luntty,*) '    #zones, first row: ',nth0,' (upper half plane)'
           write(luntty,*) ' #zones inside plasma: ',nzons
           write(luntty,*) '         total #zones: ',nzons_ext

        else if(iitype.eq.xplasma_bbftype) then
           write(luntty,*) ' --> item appears to be a profile over a MCgrid'

           call xplasma_mcgrid_profInfo(s,id,ierr, &
                id_mcgrid=id_mcgrid, nzons=nzons_prof, &
                irank=irank, idim1=idim1, idim2=idim2, &
                gridId1=gridId1, gridId2=gridId2, normcode=normcode)
           if(ierr.ne.0) then
              call xplasma_error(s,ierr,luntty)
              return
           endif

           call xplasma_mcgrid_info(s,id_mcgrid,ierr, &
                nzrow=nzrow, nzrow_ext=nzrow_ext, &
                nzons=nzons, nzons_ext=nzons_ext,&
                nth0=nth0, udsym=udsym, name=zname)
           if(ierr.ne.0) then
              call xplasma_error(s,ierr,luntty)
              return
           endif

           write(luntty,*) '  MCgrid name: ',trim(zname)
           write(luntty,*) '      updown symmetry: ',udsym
           write(luntty,*) '       #spatial zones: ',nzons_prof
           write(luntty,*) ' #zones inside plasma: ',nzons
           write(luntty,*) ' #non-spatial dimensions: ',irank
           if(irank.ge.1) then
              write(luntty,*) ' 1st non-spatial dimension: ',idim1
              if(gridId1.gt.0) then
                 call xplasma_get_item_info(s,gridId1,ierr, &
                      name=zgname1,label=zglabel1,units=zgunits1)
                 write(luntty,*) '     grid: ',trim(zgname1),' units: ', &
                      trim(zgunits1)
                 write(luntty,*) '     grid label: ',trim(zglabel1)
              endif
           endif
           if(irank.ge.2) then
              write(luntty,*) ' 2nd non-spatial dimension: ',idim2
              if(gridId2.gt.0) then
                 call xplasma_get_item_info(s,gridId2,ierr, &
                      name=zgname2,label=zglabel2,units=zgunits2)
                 write(luntty,*) '     grid: ',trim(zgname2),' units: ', &
                      trim(zgunits2)
                 write(luntty,*) '     grid label: ',trim(zglabel2)
              endif
           endif
           if(normcode.eq.-1) then
              write(luntty,*) ' normalization: divide by dVol to get density.'
           else if(normcode.eq.1) then
              write(luntty,*) ' normalization: multiply by dVol to get total.'
           else if(normcode.eq.2) then
              write(luntty,*) ' normalization: distribution function.'
           else
              write(luntty,*) ' (no normalization information available).'
           endif

           !  ** plotting options **

           if(more_options) then

              if(irank.eq.0) then
                 allocate(dat0(nzons_prof))
                 call xplasma_mcgrid_getobj(s,id,ierr, &
                      lstart0_th=.FALSE., data_1d=dat0)
              else if(irank.eq.2) then
                 if(min(gridId1,gridId2).gt.0) then
                    allocate(dat2(idim1,idim2,nzons_prof))
                    call xplasma_mcgrid_getobj(s,id,ierr, &
                         lstart0_th=.FALSE., data_3d=dat2)
                 else
                    call uermsg( &
                         ' ?xplasma_debug: non-spatial grids for 3d object'// &
                         ' are unavailable.',' ',0)
                    return
                 endif
              else
                 call uermsg( &
                      ' ?xplasma_debug: rank 1 obj plotting not available.',&
                      ' ',0)
                 return
              endif

              if(ierr.ne.0) then
                 call xplasma_error(s,ierr,luntty)
                 return
              endif

              if(normcode.ne.0) then
                 allocate(zvols(nzons_prof))
                 call xplasma_mcgrid_volumes(s,id_mcgrid,zvols,ierr)
                 if(ierr.ne.0) then
                    call xplasma_error(s,ierr,luntty)
                    return
                 endif
              endif

              ing1=0
              ing2=0

              if(gridId1.gt.0) then
                 call xplasma_grid_size(s,gridId1,ing1,ierr)
                 if(ierr.ne.0) then
                    call xplasma_error(s,ierr,luntty)
                    return
                 endif
                 allocate(zgrid1(ing1),zgrid1c(ing1-1))
                 call xplasma_grid(s,gridId1,zgrid1,ierr)
                 zgrid1c = (zgrid1(1:ing1-1)+zgrid1(2:ing1))/2
              endif

              if(gridId2.gt.0) then
                 call xplasma_grid_size(s,gridId2,ing2,ierr)
                 if(ierr.ne.0) then
                    call xplasma_error(s,ierr,luntty)
                    return
                 endif
                 allocate(zgrid2(ing2),zgrid2c(ing2-1))
                 call xplasma_grid(s,gridId2,zgrid2,ierr)
                 zgrid2c = (zgrid2(1:ing2-1)+zgrid2(2:ing2))/2
              endif

              if(nzons_prof.eq.nzons) then
                 rhomax=1.0d0
                 nrow=nzrow
                 call xplasma_rzminmax_plasma(s,zrmin,zrmax,zzmin,zzmax,iertmp)

              else
                 rhomax=(1.0d0*nzrow_ext)/nzrow
                 nrow=nzrow_ext
                 call xplasma_rzminmax_extended(s,zrmin,zrmax,zzmin,zzmax, &
                      iertmp)
              endif

              inrho=inper*nrow

              allocate(nths(nrow),nthsum(0:nrow))
              call xplasma_mcgrid_info(s,id_mcgrid,iertmp, nths=nths)
              if(udsym) then
                 inth=2*inper*nths(nrow)
              else
                 inth=inper*nths(nrow)
              endif
              
              nthsum(0)=0
              do ii=1,nrow
                 nthsum(ii)=nthsum(ii-1)+nths(ii)
              enddo

              inr=2*inrho
              inz=inr*(zzmax-zzmin)/(zrmax-zrmin)

              inwk=max(inr,inz,inrho,inth)
              if(irank.eq.1) inwk=max(inwk,idim1)
              if(irank.eq.2) inwk=max(inwk,idim2)

              allocate(wk(inwk),rr(inr),zz(inz),rho(inrho),th(inth))
              allocate(plotrz(inr,inz))    ! for plots vs. R,Z
              allocate(plotfl(inth,inrho)) ! for plots vs. flux coords

              wk=ZERO

              do ii=1,inrho
                 rho(ii)=(ii-HALF)*rhomax/inrho
              enddo

              do ii=1,inth
                 th(ii) = -CPI + (ii-HALF)*(2*CPI)/inth
              enddo

              do ii=1,inr
                 rr(ii) = zrmin + (ii-HALF)*(zrmax-zrmin)/inr
              enddo

              do ii=1,inz
                 zz(ii) = zzmin + (ii-HALF)*(zzmax-zzmin)/inz
              enddo

              orig_data = .TRUE.
              plunits = units

              do
                 iwarn=0
                 if(lxout(0)) then
                    write(luntty,*) ' '
                    write(luntty,*) ' plotting options:'
                    write(luntty,*) '  "F": plot vs. flux coordinates.'
                    write(luntty,*) '  "P": plot vs. (R,Z).'
                    if(abs(normcode).eq.1) write(luntty,*) &
                                    '  "S": select data normalization.'
                    if(irank.gt.0) write(luntty,*) &
                                    '  "V": plot vs. non-spatial coordinates.'
                    write(luntty,*) '  "Q": quit (no more plots).'
                 endif
                 call uread('MCgrid profile plotting option:')

                 if(lchar('F')) then

                    integ=0
                    if((irank.gt.0).and.(normcode.eq.2)) then
                       call uread('plot v-space integral =>ion density? (Y/N)')
                       if(lyesc('Y')) then
                          integ=1
                          ifix1=0
                          ifix2=0
                          plunits='#/m^3'
                       else
                          plunits=units
                       endif
                    endif

                    fflabel=labelx
                    if(integ.eq.0) then
                       if(irank.ge.1) then
                          call xplasma_debug_sfix(s,'1',gridId1,idim1, &
                               ifix1,fflabel)
                          if(irank.ge.2) then
                             call xplasma_debug_sfix(s,'2',gridId2,idim2, &
                                  ifix2,fflabel)
                          endif
                       endif
                    endif

                    kk=0
                    do jj=1,inrho
                       do ii=1,inth

                          kkp=kk
                          call mcindx(rho(jj),th(ii),udsym,nrow,rhomax, &
                               nths,nthsum,kk)

                          if(irank.eq.0) then
                             if(orig_data) then
                                plotfl(ii,jj) = dat0(kk)
                             else if(normcode.eq.-1) then
                                plotfl(ii,jj) = dat0(kk)/zvols(kk)
                             else if(normcode.eq.1) then
                                plotfl(ii,jj) = dat0(kk)*zvols(kk)
                             else
                                iwarn=iwarn+1
                                if(iwarn.le.3) then
                                   write(luntty,*) &
                                        ' ?xplasma_debug: normalization error.'
                                   plotfl(ii,jj) = 0.0d0
                                endif
                             endif
                          else
                             if(min(ifix1,ifix2).gt.0) then
                                plotfl(ii,jj) = dat2(ifix1,ifix2,kk)
                             else if((ii.gt.1).and.(kk.eq.kkp)) then
                                ! to save time...
                                plotfl(ii,jj) = plotfl(ii-1,jj)
                             else
                                plotfl(ii,jj) = 0.0d0
                                if(normcode.eq.2) then
                                   do ix=1,ing2-1
                                      do ie=1,ing1-1
                                         plotfl(ii,jj)=plotfl(ii,jj) + 0.5d0*&
                                              (zgrid1(ie+1)-zgrid1(ie))* &
                                              (zgrid2(ix+1)-zgrid2(ix))* &
                                              dat2(ie,ix,kk)
                                      enddo
                                   enddo
                                else
                                   iwarn=iwarn+1
                                   if(iwarn.le.3) write(luntty,*) &
                                        ' ?xplasma_debug: normalization error.'
                                endif
                             endif
                          endif

                       enddo
                    enddo

                    call r8_grf3f1(th,plotfl,rho,wk, &
                         inth,inrho,inth, &
                         'theta',label,'x', &
                         'rad',plunits,' ', &
                         fflabel,labelt,0)

                 else if(lchar('P')) then

                    integ=0
                    if((irank.gt.0).and.(normcode.eq.2)) then
                       call uread('plot v-space integral =>ion density? (Y/N)')
                       if(lyesc('Y')) then
                          integ=1
                          ifix1=0
                          ifix2=0
                          plunits='#/m^3'
                       else
                          plunits=units
                       endif
                    endif

                    fflabel=labelx
                    if(integ.eq.0) then
                       if(irank.ge.1) then
                          call xplasma_debug_sfix(s,'1',gridId1,idim1, &
                               ifix1,fflabel)
                          if(irank.ge.2) then
                             call xplasma_debug_sfix(s,'2',gridId2,idim2, &
                                  ifix2,fflabel)
                          endif
                       endif
                    endif

                    allocate(rhomap(inr),thmap(inr))
                    do jj=1,inz
                       wk(1:inr)=zz(jj)
                       ! use the medium speed coord mapping method...
                       call xplasma_ctrans(s,xp_use_coord_vecsize,ierr, &
                            r_in=rr, z_in=wk(1:inr), &
                            rho_out=rhomap, theta_out=thmap, &
                            i2pi=2, maptype=xp_imap_polar)

                       kk=0
                       do ii=1,inr
                          kkp=kk
                          call mcindx(rhomap(ii),thmap(ii), &
                               udsym,nrow,rhomax, &
                               nths,nthsum,kk)
                          if(kk.eq.0) then
                             plotrz(ii,jj)=ZERO
                          else
                             if(irank.eq.0) then
                                if(orig_data) then
                                   plotrz(ii,jj) = dat0(kk)
                                else if(normcode.eq.-1) then
                                   plotrz(ii,jj) = dat0(kk)/zvols(kk)
                                else if(normcode.eq.1) then
                                   plotrz(ii,jj) = dat0(kk)*zvols(kk)
                                else
                                   iwarn=iwarn+1
                                   if(iwarn.le.3) then
                                      write(luntty,*) &
                                           ' ?xplasma_debug: normalization error.'
                                      plotrz(ii,jj) = 0.0d0
                                   endif
                                endif
                             else
                                if(min(ifix1,ifix2).gt.0) then
                                   plotrz(ii,jj) = dat2(ifix1,ifix2,kk)
                                else if(kk.eq.kkp) then
                                   ! to save time...
                                   plotrz(ii,jj)=plotrz(ii-1,jj)
                                else
                                   plotrz(ii,jj) = 0.0d0
                                   if(normcode.eq.2) then
                                      do ix=1,ing2-1
                                         do ie=1,ing1-1
                                            plotfl(ii,jj)=plotfl(ii,jj)+0.5d0*&
                                                 (zgrid1(ie+1)-zgrid1(ie))* &
                                                 (zgrid2(ix+1)-zgrid2(ix))* &
                                                 dat2(ie,ix,kk)
                                         enddo
                                      enddo
                                   else
                                      iwarn=iwarn+1
                                      if(iwarn.le.3) write(luntty,*) &
                                        ' ?xplasma_debug: normalization error.'
                                   endif
                                endif

                             endif
                          endif
                       enddo
                    enddo
                    deallocate(rhomap,thmap)

                    call r8_grf3f1(rr,plotrz,zz,wk, &
                         inr,inz,inr, &
                         'R',label,'Z', &
                         'm',plunits,'m', &
                         fflabel,labelt,0)

                 else if(lchar('S')) then
                    if(normcode.eq.-1) then
                       if(lxout(0)) write(luntty,*) &
                            ' enter "N" to plot density (divide by dVol)'

                    else if(normcode.eq.1) then
                       if(lxout(0)) write(luntty,*) &
                            ' enter "N" to plot total (multiply by dVol)'

                    else if(normcode.eq.2) then
                       if(lxout(0)) write(luntty,*) &
                            ' enter "N" to plot dE*dA integral -> ion density'
                    else
                       call uermsg('"S" (normalization) option unavailable', &
                            ' ',0)
                       cycle
                    endif

                    call uread(' plot original data? (Y/N)')
                    if(lyesc('Y')) then
                       orig_data=.TRUE.
                       plunits = units
                    else
                       orig_data=.FALSE.
                       if(normcode.eq.-1) then
                          plunits = trim(units)//'/m^3'
                       else if(normcode.eq.1) then
                          indx=index(units,'/m^3')
                          if(indx.gt.1) then
                             plunits=units(1:indx-1)
                          else
                             write(luntty,*) ' %could not convert units: ', &
                                  units
                             plunits=units
                          endif
                       else if(normcode.eq.2) then
                          plunits='#/m^3'
                       endif
                    endif

                 else if(lchar('V')) then
                    if((irank.ne.2).or.(idim1.ne.(ing1-1)).or. &
                         (idim2.ne.(ing2-1))) then
                       call uermsg('xplasma_debug: plot not available.',' ',0)
                    else
                       if(lxout(0)) then
                          write(luntty,*) ' There are ',nrow, &
                               ' zone rows evenly spaced from 0 to ',rhomax
                          write(luntty,*) ' (enter "0" or "Q" to skip plot).'
                       endif
                       irow=0
                       call ureadi('select zone row index:',irow)
                       if(irow.eq.0) cycle
                       irow=max(1,min(nrow,irow))
                       if(lxout(0)) then
                          if(udsym) then
                             write(luntty,*) ' For row #',irow, &
                                  ' there are ',nths(irow), &
                                  ' evenly spaced theta zones from -pi to 0.'
                          else
                             write(luntty,*) ' For row #',irow, &
                                  ' there are ',nths(irow), &
                                  ' evenly spaced theta zones from -pi to pi.'
                          endif
                          write(luntty,*) ' (enter "0" or "Q" to skip plot).'
                       endif
                       ithz=0
                       call ureadi('select theta zone index:',ithz)
                       if(ithz.eq.0) cycle
                       ithz=max(1,min(nths(irow),ithz))
                       if(udsym) then
                          ztheta=-CPI + (ithz-HALF)*CPI/nths(irow)
                       else
                          ztheta=-CPI + (ithz-HALF)*2*CPI/nths(irow)
                       endif

                       kk = nthsum(irow-1)+ithz

                       fflabel=' '
                       write(fflabel,'(A,", rho=",1pe11.4,", theta=",1pe11.4)') &
                            trim(labelx), (irow-HALF)*rhomax/nrow, ztheta
                       call r8_grf3f1(zgrid1c,dat2(1,1,kk),zgrid2c,wk, &
                            idim1,idim2,idim1, &
                            zglabel1,label,zglabel2, &
                            zgunits1,units,zgunits2, &
                            fflabel,labelt,0)
                    endif
                 else if(lchar('Q')) then
                    exit

                 else
                    call uermsg('unrecogized plot option.',' ',0)
                 endif
              enddo

              deallocate(nths,wk,rr,zz,rho,th,plotrz,plotfl)
              if(allocated(zvols)) deallocate(zvols)
              if(allocated(zgrid1)) deallocate(zgrid1,zgrid1c)
              if(allocated(zgrid2)) deallocate(zgrid2,zgrid2c)

           endif

        else
           write(luntty,*) '    number of integers:  ',isize
           write(luntty,*) '    number of real no.s: ',r8size

        endif
        write(luntty,*) ' '

      end subroutine blkbx_display

      subroutine mcindx(rho,th,udsym,nrow,rhomax,nths,nthsum,indx)

        ! private

        ! this routine assumes that the theta indexing is oriented
        ! counterclockwise and that it starts at -pi for each row,
        ! *even* for updown symmetric data.

        real*8, intent(in) :: rho,th  ! flux coords in
        logical, intent(in) :: udsym  ! updown symmetry flag
        integer, intent(in) :: nrow   ! #rows
        real*8, intent(in) :: rhomax  ! max rho in range
        integer, intent(in) :: nths(nrow) ! # theta zones / row
        integer, intent(in) :: nthsum(0:nrow) ! # in prior rows

        integer, intent(out) :: indx

        !----------------------------------------
        integer :: irho,ith
        real*8 :: zth,zthlim

        real*8, parameter :: CPI = 3.1415926535897931D+00
        real*8, parameter :: ZERO = 0.0d0
        !----------------------------------------

        indx = 0

        if(udsym) then
           zth = -abs(th)
           zthlim = ZERO
        else
           zth = th
           zthlim = CPI
        endif

        irho = 1 + rho*nrow/rhomax
        if((irho.ge.1).and.(irho.le.nrow)) then

           ith = 1 + (zth+CPI)*nths(irho)/(zthlim+CPI)
           if((ith.ge.1).and.(ith.le.nths(irho))) then
              indx = nthsum(irho-1) + ith
           endif
        endif

      end subroutine mcindx

      !------------------------------------------------
      subroutine xplasma_debug_sfix(s,slbl,gridId,idim,indx,mlabel)

        ! **private**

        ! prompt user for grid value, return nearest index
        ! if idim=size(gridId) this is the nearest bdy value
        ! if idim=size(gridId)-1 this is the nearest zone centered value

        type (xplasma), pointer :: s
        character*(*), intent(in) :: slbl
        integer, intent(in) :: gridId
        integer, intent(in) :: idim
        integer, intent(out) :: indx
        character*(*), intent(inout) :: mlabel

        !------------------------------
        character*32 name,units
        character*80 label,tmplabel

        integer :: isize,id,ierr,imindiff
        real*8 :: gmin,gmax,zval,zvalu,zvalsave,zmindiff,zdiff
        real*8, dimension(:), allocatable :: gvals,gvals_zc
        !------------------------------

        id=gridId
        if(id.gt.0) then
           call xplasma_grid_info(s,id,ierr, &
                size=isize,xmin=gmin,xmax=gmax, &
                name=name,label=label,units=units)

           if(ierr.ne.0) then
              write(luntty,*) ' error with grid:'
              call xplasma_error(s,ierr,luntty)
              write(luntty,*) ' fixup taken...'
              id=0

           else if((idim.lt.(isize-1)).or.(idim.gt.isize)) then
              write(luntty,*) ' grid size does not match dimension: ', &
                   isize,idim
              write(luntty,*) ' fixup taken...'
              id=0

           else
              allocate(gvals(isize),gvals_zc(isize-1))
              call xplasma_grid(s,id,gvals,ierr)
              gvals_zc = (gvals(1:isize-1)+gvals(2:isize))/2
           endif
        endif

        if(id.eq.0) then
           if(lxout(0)) then
              write(luntty,*) ' Dimension: ',trim(slbl)
              write(luntty,*) ' available index range: 1 to ',idim
           endif

           indx=1
           call ureadi('Dimension '//trim(slbl)//' index:',indx)
           indx=max(1,min(idim,indx))

           tmplabel=mlabel
           mlabel=' '
           write(mlabel,'(A,"; index ",a,": ",i4)') &
                trim(tmplabel),trim(slbl),indx

        else

           ! labeled dimension appears to be available

           if(lxout(0)) then
              write(luntty,*) ' grid name: ',trim(name)
              write(luntty,*) ' grid label: ',trim(label)
              write(luntty,*) ' grid units: ',trim(units)
              write(luntty,*) ' grid size: ',idim
              write(luntty,*) ' range of grid values: ',gmin,' to ',gmax
              write(luntty,*) ' select grid value; nearest point taken.'
           endif

           call ureadr8('select grid value: ',zval)
           zval=max(gmin,min(gmax,zval))

           imindiff=0
           zmindiff=gmax-gmin
           do i=1,idim
              if(idim.eq.isize) then
                 zdiff=abs(zval-gvals(i))
                 zvalu=gvals(i)
              else
                 zdiff=abs(zval-gvals_zc(i))
                 zvalu=gvals_zc(i)
              endif
              if(zdiff.lt.zmindiff) then
                 zmindiff=zdiff
                 imindiff=i
                 zvalsave=zvalu
              endif
           enddo

           indx=imindiff
           tmplabel=mlabel
           write(mlabel,'(A,1x,A,"=",1pe11.4)') &
                trim(tmplabel),trim(label),zvalsave
        endif

      end subroutine xplasma_debug_sfix

      !------------------------------------------------
      subroutine coord_display

        !  details on xplasma coordinates; label & units shown separately

        logical :: periodic
        integer :: ii,idgrid
        character*32 xname

        call xplasma_coord_isperiodic(s,id,periodic,ier)
        if(ier.eq.0) then
           write(luntty,*) '    periodicity: ',periodic
        else
           write(luntty,*) ' ?xplasma_coord_isperiodic -- call failed.'
           return
        endif

        call xplasma_coord_info(s,id,ier, &
             ngrids=ngrids, xmin=xmin, xmax=xmax)

        if((.not.periodic).and.(ngrids.gt.0)) then
           write(buf,'("    xmin,xmax:",2(1x,1pe13.6))') xmin,xmax
           write(luntty,*) trim(buf)
        endif

        write(luntty,*) '    # of grids: ',ngrids

        do ii=1,ngrids
           call xplasma_coord_gridId(s,id,ii,idgrid,ier); call ckerr
           call xplasma_grid_size(s,idgrid,inx1,ier); call ckerr
           call xplasma_get_item_info(s,idgrid,ier, name=xname); call ckerr
           if(periodic) then
              allocate(x1buf(inx1))
              call xplasma_grid(s,idgrid,x1buf,ier); call ckerr
              xmin=x1buf(1); xmax=x1buf(inx1)
              deallocate(x1buf)
              write(buf,'("      size=",i4,"; xmin,xmax:",2(1x,1pe13.6),'// &
                   '"; name: ",a)') inx1,xmin,xmax,trim(xname)
           else
              write(buf,'("      size=",i4,"; ...name: ",a)') inx1,trim(xname)
           endif
           write(luntty,*) trim(buf)
        enddo
        write(luntty,*) ' '

      end subroutine coord_display

      !------------------------------------------------
      subroutine grid_display

        !  details on xplasma grids; label & units shown separately

        logical :: periodic
        integer :: ii,ict,icoord,irefs,inprofs,idg1,idg2,idg3
        integer, dimension(:), allocatable :: ids
        character*32 cname,pname,pauth

        call xplasma_grid_size(s,id,inx1,ier); call ckerr
        allocate(x1buf(inx1))

        call xplasma_grid(s,id,x1buf,ier); call ckerr

        call xplasma_grid_info(s,id,ier, &
             nrefs=irefs,coord=icoord); call ckerr

        call xplasma_get_item_info(s,icoord,ier, name=cname); call ckerr

        call xplasma_coord_isPeriodic(s,icoord,periodic,ier); call ckerr

        write(luntty,*) '    discretizes coordinate: ',trim(cname)
        write(luntty,*) '    used by this number of profiles: ',irefs
        write(luntty,*) '    periodicity: ',periodic
        write(buf,'("    xmin,xmax:",2(1x,1pe13.6))') x1buf(1),x1buf(inx1)
        write(luntty,*) trim(buf)
        write(luntty,*) '    number of grid points: ',inx1
        write(luntty,*) ' '

        if(more_options) then
           call uread(' list profiles that use this grid? (Y/N):')
           if(lyesc('Y')) then
              call xplasma_num_items(s,ier,num_profs=inprofs)
              allocate(ids(inprofs))
              call xplasma_contents(s,ier,id_profs=ids)
              ict=0
              do ii = 1,inprofs
                 call xplasma_prof_info(s,ids(ii),ier, &
                      name=pname, author=pauth, &
                      gridId1=idg1, gridId2=idg2, gridId3=idg3)
                 if((idg1.eq.id).or.(idg2.eq.id).or.(idg3.eq.id)) then
                    ict=ict+1
                    write(luntty,*) ict,'. ',pname,trim(pauth)
                 endif
              enddo
           endif
           call uread(' plot grid values vs. index? (Y/N):')
           if(lyesc('Y')) then
              call xplasma_grid_info(s,id,ier, label=label1, units=units1)
              call ckerr
              units2=' '
              label2=trim(name)//' vs. grid index.'
              allocate(x2buf(inx1))
              do ii=1,inx1
                 x2buf(ii)=ii
              enddo

              call r8_grafx1(x2buf,x1buf,inx1,units2,units1, &
                   label0,label1,label2)

              deallocate(x2buf)

           endif
        endif
        deallocate(x1buf)
      end subroutine grid_display

      !------------------------------------------------
      subroutine prof_display

        !  details on xplasma profiles; label & units shown separately

        !-------------------------------
        character*32 :: assoc_name

        call xplasma_prof_info(s,id,ier, &
             rank=irank, splineType=ispline, &
             gridId1=idx1, gridId2=idx2, gridId3=idx3, &
             profId1=id_assoc, &
             counter=jcount)

        write(luntty,*) '    profile rank: ',irank

        if(ispline.eq.-1) then
           idmax=0
           write(luntty,*) '    interpolation: zonal step function.'
        else if(ispline.eq.0) then
           idmax=1
           write(luntty,*) '    interpolation: piecewise linear.'
        else if(ispline.eq.1) then
           idmax=1
           write(luntty,*) '    interpolation: C1 Akima Hermite.'
        else if(ispline.eq.2) then
           idmax=3
           write(luntty,*) '    interpolation: cubic spline.'
        endif
      
        grids = '    grids:'
        coords = '    coordinates:'
        resolution = '    dimensions:'

        call app_grid(irank,1,idx1)
        call app_grid(irank,2,idx2)
        call app_grid(irank,3,idx3)

        write(luntty,*) trim(grids)
        write(luntty,*) trim(coords)
        write(luntty,*) trim(resolution)

        if(id_assoc.eq.0) then
           write(luntty,*) '    (no associated profile)'
        else
           call xplasma_prof_info(s,id_assoc,ier, name=assoc_name)
           write(luntty,*) '    Associated profile: ',trim(assoc_name)
        endif

        write(luntty,*) '    profile definition counter: ',jcount
        write(luntty,*) '    ------------------'
        write(luntty,*) ' '

        if(more_options) then
           if(irank.eq.1) call prof_plot1d
           if(irank.eq.2) call prof_plot2d
           if(irank.eq.3) call prof_plot3d
        endif
      end subroutine prof_display

      subroutine prof_plot1d

        ! PLOT 1D PROFILE

        !---------------------------------------------
        character*32 fname,funits
        character*78 flabel,funitsd
        character*32 xname,xlabel,xunits
        integer :: ideriv,inper,inx1,lu,inxi,ix,ixi,ii
        real*8, dimension(:), allocatable :: xx,xx0,ff
        !---------------------------------------------

        lu=luntty

        call xplasma_get_item_info(s,idx1,ier, &
             name=xname,label=xlabel,units=xunits); call ckerr

        call xplasma_get_item_info(s,id,ier, &
             name=fname,label=flabel,units=funits); call ckerr

        call xplasma_grid_size(s,idx1,inx1,ier); call ckerr

        ideriv=0
        if(len(trim(flabel)).gt.50) then
           label1 = flabel
        else
           label1 = trim(flabel)//' vs. '//trim(xlabel)
        endif
        label2 = trim(fname)//' vs. '//trim(xname)
        funitsd = funits

        inper=5

        do
           if(lxout(0)) then
              write(lu,*) 'Plotting: ',trim(label2)
              write(lu,*) 'derivative option: ',ideriv,' for ',trim(xname),'.'
              write(lu,*) 'interpolation option: ',inper,' pts per grid zone.'
              write(lu,*) ' '
              write(lu,*) 'options for plotting 1d profile:'
              if(ispline.ge.0) &
                   write(lu,*) ' "A": plot the original data.'
              write(lu,*) ' "I": plot an interpolation of data or derivative.'
              if(ispline.ge.0) &
                   write(lu,*) ' "D": set derivative options.'
              write(lu,*) ' "N": change interpolation resolution.'
              write(lu,*) ' "Q": quit.'
           endif

           call uread('prof_plot1d option:')

           if(lchar('A')) then

              if(ispline.lt.0) then
                 call uermsg( &
                      'use "I" option to see original data (step function)', &
                      ' ',0)
                 cycle
              endif

              allocate(xx(inx1),ff(inx1))

              call xplasma_grid(s,idx1,xx,ier); call ckerr
              call xplasma_eval_prof(s,id,ff,ier); call ckerr

              call r8_grafx1(xx,ff,inx1,xunits,funits, &
                   label0,label1,label2)
            
              deallocate(xx,ff)

           else if(lchar('I')) then

              inxi = (inx1-1)*inper + 1

              allocate(xx0(inx1),xx(inxi),ff(inxi))

              call xplasma_grid(s,idx1,xx0,ier); call ckerr

              xx(1)=xx0(1)
              do ix=2,inx1
                 do ixi=1,inper
                    ii=1+(ix-2)*inper+ixi
                    xx(ii)=(xx0(ix-1)*(inper-ixi)+xx0(ix)*ixi)/inper
                 enddo
              enddo

              call xplasma_eval_prof(s,id,xx,ff,ier, ideriv1=ideriv)

              call r8_grafx1(xx,ff,inxi,xunits,funitsd, &
                   label0,label1,label2)
            
              deallocate(xx0,xx,ff)

           else if(lchar('D')) then

              !  derivative control

              if(idmax.eq.0) then
                 call uermsg(' no derivative information (step function).', &
                      ' ',0)
                 cycle
              endif

              if(lxout(0)) then
                 write(lu,*) ' '
                 write(lu,*) ' min:0, max: ',idmax
                 write(lu,*) ' 0 means: no derivative; 1 means 1st derivative.'
                 if(idmax.gt.1) write(lu,*) ' >1 means higher derivative.'
              endif

              call ureadi('select derivative w.r.t. '//trim(xname)//':',ideriv)
              ideriv=max(0,min(idmax,ideriv))

              if(ideriv.eq.0) then
                 if(len(trim(flabel)).gt.50) then
                    label1 = flabel
                 else
                    label1 = trim(flabel)//' vs. '//trim(xlabel)
                 endif
                 label2 = trim(fname)//' vs. '//trim(xname)
                 funitsd = funits

              else if(ideriv.eq.1) then
                 if(len(trim(flabel)).gt.50) then
                    label1 = 'derivative of '//flabel
                 else
                    label1 = 'd('//trim(flabel)//')/d('//trim(xlabel)// &
                         ') vs. '//trim(xlabel)
                 endif
                 label2 = 'd('//trim(fname)//')/d('//trim(xname)// &
                      ') vs. '//trim(xname)
                 funitsd = trim(funits)//'/('//trim(xunits)//')'

              else if(ideriv.gt.1) then
                 write(funitsd,'(a,"/(",a,")^",i1)') &
                      trim(funits),trim(xunits),ideriv
                 if(ideriv.eq.2) then
                    label1 = '2nd derivative of '//flabel
                 else 
                    label1 = '3rd derivative of '//flabel
                 endif

                 write(label2,'("d^",i1,"(",a,")/d(",a,")^",i1," vs. ",a)') &
                      ideriv,trim(fname),trim(xname),ideriv,trim(xname)

              endif

           else if(lchar('N')) then

              !  interpolation evaluation resolution control

              if(lxout(0)) write(lu,*) ' min:2, max: 1000'
              call ureadi( &
                   'number of interpolation evaluations per grid zone:', &
                   inper)
              inper=max(2,min(1000,inper))

           else if(lchar('Q')) then

              !  all done

              exit

           else
              call uermsg('?prof_plot1d: unrecognized option.',' ',0)
           endif

        enddo

      end subroutine prof_plot1d

      subroutine prof_plot2d

        ! PLOT 2D PROFILE
        ! dmc -- upgraded for speed: using xpeval lookup objects...

        !---------------------------------------------
        character*32 fname,funits
        character*78 flabel,funitsd,fname_d,fname_w,funits_wk
        character*32 x1name,x1label,x1units
        character*32 x2name,x2label,x2units
        integer :: ideriv1,ideriv2,inper,inx1,inx2,lu,inx1i,inx2i,ix,ixi,ii
        integer :: idsum,intrp
        character*3 :: cdsum
        real*8, dimension(:), allocatable :: x1,x1i,x2,x2i,wk
        real*8, dimension(:,:), allocatable :: ff
        real*8 :: ztim1,ztim2

        type (xpeval) :: x1_intrp,x2_intrp
        !---------------------------------------------

        lu=luntty

        call xplasma_get_item_info(s,idx1,ier, &
             name=x1name,label=x1label,units=x1units); call ckerr

        call xplasma_get_item_info(s,idx2,ier, &
             name=x2name,label=x2label,units=x2units); call ckerr

        call xplasma_get_item_info(s,id,ier, &
             name=fname,label=flabel,units=funits); call ckerr

        call xplasma_grid_size(s,idx1,inx1,ier); call ckerr
        call xplasma_grid_size(s,idx2,inx2,ier); call ckerr

        ideriv1=0
        ideriv2=0
        fname_d = fname
        label1 = labelt
        label2 = label1
        funitsd = funits

        inper=5

        do
           if(lxout(0)) then
              write(lu,*) 'Plotting: ',trim(fname)
              write(lu,*) 'derivative options: '
              write(lu,*) '   ',ideriv1,' for ',trim(x1name),';'
              write(lu,*) '   ',ideriv2,' for ',trim(x2name),';'
              write(lu,*) 'interpolation option: ',inper,' pts per grid zone.'
              write(lu,*) ' '
              write(lu,*) 'options for plotting 2d profile:'
              if(ispline.ge.0) &
                   write(lu,*) ' "A": plot the original data.'
              write(lu,*) ' "I": plot an interpolation of data or derivative.'
              write(lu,*) ' "J": as "I" with axes reversed.'
              if(ispline.ge.0) &
                   write(lu,*) ' "D": set derivative options.'
              write(lu,*) ' "N": change interpolation resolution.'
              write(lu,*) ' "Q": quit.'
           endif

           call uread('prof_plot1d option:')

           intrp=0

           if(lchar('A')) then

              if(ispline.lt.0) then
                 call uermsg( &
                      'use "I" option to see original data (step function)', &
                      ' ',0)
                 cycle
              endif

              !  plot the original data -- not interpolation.

              allocate(x1(inx1),x2(inx2),ff(inx1,inx2),wk(max(inx1,inx2)))

              wk = 0
              call xplasma_grid(s,idx1,x1,ier); call ckerr
              call xplasma_grid(s,idx2,x2,ier); call ckerr
              call xplasma_eval_prof(s,id,ff,ier); call ckerr

              call r8_grf3f1(x1,ff,x2,wk,inx1,inx2,inx1, &
                   x1name,fname,x2name,x1units,funits,x2units, &
                   label1,labelx,0)
            
              deallocate(x1,x2,ff,wk)

           else if(lchar('I')) then

              intrp = 1

           else if(lchar('J')) then

              intrp = 2

           else if(lchar('D')) then

              if(idmax.eq.0) then
                 call uermsg(' no derivative information (step function).', &
                      ' ',0)
                 cycle
              endif

              !  derivative control

              if(lxout(0)) then
                 write(lu,*) ' '
                 write(lu,*) ' min:0, max: ',idmax
                 write(lu,*) ' 0 means: no derivative; 1 means 1st derivative.'
                 if(idmax.gt.1) write(lu,*) ' >1 means higher derivative.'
              endif

              call ureadi('select derivative w.r.t. '//trim(x1name)//':', &
                   ideriv1)
              ideriv1=max(0,min(idmax,ideriv1))

              call ureadi('select derivative w.r.t. '//trim(x2name)//':', &
                   ideriv2)
              ideriv2=max(0,min(idmax,ideriv2))

              idsum=ideriv1+ideriv2

              if(idsum.eq.0) then
                 fname_d = fname
                 label2 = label1
                 funitsd = funits

              else
                 if(idsum.eq.1) then
                    cdsum='1st'
                 else if(idsum.eq.2) then
                    cdsum='2nd'
                 else if(idsum.eq.3) then
                    cdsum='3rd'
                 else
                    write(cdsum,'(i1,"th")') idsum
                 endif

                 label2 = cdsum//' derivative of '//trim(fname)

                 funitsd = trim(funits)//"/"
                 if(idsum.eq.1) then
                    fname_d = 'd('//trim(fname)//')/'
                 else
                    write(fname_d,'("d^",i1,"(",a,")/")') idsum,trim(fname)
                 endif

                 if(ideriv1.eq.1) then
                    funits_wk = trim(funitsd) // '('//trim(x1units)//')'
                    fname_w = trim(fname_d)//'d('//trim(x1name)//')'
                 else if(ideriv1.gt.1) then
                    funits_wk = ' '
                    write(funits_wk,'(a,"(",a,")^",i1)') trim(funitsd), &
                         trim(x1units),ideriv1
                    fname_w = ' '
                    write(fname_w,'(a,"d^",i1,"(",a,")")') trim(fname_d), &
                         ideriv1,trim(x1name)
                 endif
                 if(ideriv1.gt.0) then
                    funitsd = funits_wk
                    fname_d = fname_w
                 endif

                 if(ideriv2.eq.1) then
                    funits_wk = trim(funitsd) // '('//trim(x2units)//')'
                    fname_w = trim(fname_d)//'d('//trim(x2name)//')'
                 else if(ideriv2.gt.1) then
                    funits_wk = ' '
                    write(funits_wk,'(a,"(",a,")^",i1)') trim(funitsd), &
                         trim(x2units),ideriv2
                    fname_w = ' '
                    write(fname_w,'(a,"d^",i1,"(",a,")")') trim(fname_d), &
                         ideriv2,trim(x2name)
                 endif
                 if(ideriv2.gt.0) then
                    funitsd = funits_wk
                    fname_d = fname_w
                 endif

              endif

           else if(lchar('N')) then

              !  interpolation evaluation resolution control

              if(lxout(0)) write(lu,*) ' min:2, max: 100'
              call ureadi('number of interpolation evaluations per grid zone:', &
                   inper)
              inper=max(2,min(100,inper))

           else if(lchar('Q')) then

              !  all done

              exit

           else
              call uermsg('?prof_plot2d: unrecognized option.',' ',0)
           endif

           if(intrp.gt.0) then

              inx1i = (inx1-1)*inper + 1
              inx2i = (inx2-1)*inper + 1

              allocate(x1i(inx1i),x2i(inx2i),wk(max(inx1i,inx2i))); wk=0

              if(intrp.eq.1) then
                 allocate(ff(inx1i,inx2i))
              else if(intrp.eq.2) then
                 allocate(ff(inx2i,inx1i))
              endif

              allocate(x1(inx1),x2(inx2))

              call xplasma_grid(s,idx1,x1,ier); call ckerr
              call xplasma_grid(s,idx2,x2,ier); call ckerr

              x1i(1)=x1(1)
              do ix=2,inx1
                 do ixi=1,inper
                    ii=1+(ix-2)*inper+ixi
                    x1i(ii)=(x1(ix-1)*(inper-ixi)+x1(ix)*ixi)/inper
                 enddo
              enddo

              x2i(1)=x2(1)
              do ix=2,inx2
                 do ixi=1,inper
                    ii=1+(ix-2)*inper+ixi
                    x2i(ii)=(x2(ix-1)*(inper-ixi)+x2(ix)*ixi)/inper
                 enddo
              enddo

              call cpu_time(ztim1)

              call xplasma_x_lookup(s, idx1, x1i, x1_intrp, ier); call ckerr
              do ix=1,inx2i
                 call xplasma_x_lookup(s, idx2, x2i(ix), x2_intrp, ier); call ckerr
                 if(intrp.eq.1) then
                    call xplasma_eval_prof(s,id,x1_intrp,x2_intrp, &
                         ff(1:inx1i,ix),ier, &
                         ideriv1=ideriv1,ideriv2=ideriv2); call ckerr
                 else
                    call xplasma_eval_prof(s,id,x1_intrp,x2_intrp, &
                         ff(ix,1:inx1i),ier, &
                         ideriv1=ideriv1,ideriv2=ideriv2); call ckerr
                 endif
              enddo

              call xpeval_free(x1_intrp)
              call xpeval_free(x2_intrp)

              call cpu_time(ztim2)
              write(luntty,*) ' cpu time for interpolation: ',ztim2-ztim1

              if(intrp.eq.1) then
                 call r8_grf3f1(x1i,ff,x2i,wk,inx1i,inx2i,inx1i, &
                      x1name,fname_d,x2name,x1units,funitsd,x2units, &
                      label2,labelx,0)
              else
                 call r8_grf3f1(x2i,ff,x1i,wk,inx2i,inx1i,inx2i, &
                      x2name,fname_d,x1name,x2units,funitsd,x1units, &
                      label2,labelx,0)
              endif
               
              deallocate(x1i,x1,x2i,x2,ff,wk)

           endif

        enddo

      end subroutine prof_plot2d

      subroutine prof_plot3d

        ! PLOT 3D PROFILE
        ! dmc NOT YET DONE-- upgrade for speed: using xpeval lookup objects...

        !---------------------------------------------
        character*32 fname,funits
        character*78 flabel,funitsd,fname_d,fname_w,funits_wk
        character*32 x1name,x1label,x1units
        character*32 x2name,x2label,x2units
        character*32 x3name,x3label,x3units
        integer :: ideriv1,ideriv2,ideriv3
        integer :: inper,inx1,inx2,inx3,lu,inx1i,inx2i,inx3i,ix,ixi,ii,ix2
        integer :: idsum
        character*3 :: cdsum
        real*8, dimension(:), allocatable :: x1,x1i,x2,x2i,x3,x3i,wk1,wk2
        real*8, dimension(:,:,:), allocatable :: ff
        !---------------------------------------------

        lu=luntty

        call xplasma_get_item_info(s,idx1,ier, &
             name=x1name,label=x1label,units=x1units); call ckerr

        call xplasma_get_item_info(s,idx2,ier, &
             name=x2name,label=x2label,units=x2units); call ckerr

        call xplasma_get_item_info(s,idx3,ier, &
             name=x3name,label=x3label,units=x3units); call ckerr

        call xplasma_get_item_info(s,id,ier, &
             name=fname,label=flabel,units=funits); call ckerr

        call xplasma_grid_size(s,idx1,inx1,ier); call ckerr
        call xplasma_grid_size(s,idx2,inx2,ier); call ckerr
        call xplasma_grid_size(s,idx3,inx3,ier); call ckerr

        ideriv1=0
        ideriv2=0
        ideriv3=0
        fname_d = fname
        funitsd = funits

        inper=5

        do
           if(lxout(0)) then
              write(lu,*) 'WARNING -- this display code not tested!'
              write(lu,*) '   bug reports to dmccune@pppl.gov...'
              write(lu,*) ' '
              write(lu,*) 'Plotting: ',trim(fname)
              write(lu,*) 'derivative options: '
              write(lu,*) '   ',ideriv1,' for ',trim(x1name),';'
              write(lu,*) '   ',ideriv2,' for ',trim(x2name),';'
              write(lu,*) '   ',ideriv3,' for ',trim(x3name),';'
              write(lu,*) 'interpolation option: ',inper,' pts per grid zone.'
              write(lu,*) ' '
              write(lu,*) 'options for plotting 3d profile:'
              if(ispline.ge.0) &
                   write(lu,*) ' "A": plot the original data.'
              write(lu,*) ' "I": plot an interpolation of data or derivative.'
              if(ispline.ge.0) &
                   write(lu,*) ' "D": set derivative options.'
              write(lu,*) ' "N": change interpolation resolution.'
              write(lu,*) ' "Q": quit.'
           endif

           call uread('prof_plot1d option:')

           if(lchar('A')) then

              if(ispline.lt.0) then
                 call uermsg( &
                      'use "I" option to see original data (step function)', &
                      ' ',0)
                 cycle
              endif

              !  plot the original data -- not interpolation.

              allocate(x1(inx1),x2(inx2),x3(inx3),ff(inx1,inx2,inx3))

              call xplasma_grid(s,idx1,x1,ier); call ckerr
              call xplasma_grid(s,idx2,x2,ier); call ckerr
              call xplasma_grid(s,idx3,x3,ier); call ckerr
              call xplasma_eval_prof(s,id,ff,ier); call ckerr

              call r8_grf4f1(ff,inx1,inx2,x1,inx1,x2,inx2,x3,inx3, &
                   trim(fname)//' @'//labelt,x1name,x2name,x3name, &
                   funits,x1units,x2units,x3units, &
                   labelx,0)
            
              deallocate(x1,x2,x3,ff)

           else if(lchar('I')) then

              inx1i = (inx1-1)*inper + 1
              inx2i = (inx2-1)*inper + 1
              inx3i = (inx3-1)*inper + 1

              allocate(x1i(inx1i),x2i(inx2i),x3i(inx3i), &
                   ff(inx1i,inx2i,inx3i),wk1(inx1i),wk2(inx1i))

              allocate(x1(inx1),x2(inx2),x3(inx3))

              call xplasma_grid(s,idx1,x1,ier); call ckerr
              call xplasma_grid(s,idx2,x2,ier); call ckerr
              call xplasma_grid(s,idx3,x3,ier); call ckerr

              x1i(1)=x1(1)
              do ix=2,inx1
                 do ixi=1,inper
                    ii=1+(ix-2)*inper+ixi
                    x1i(ii)=(x1(ix-1)*(inper-ixi)+x1(ix)*ixi)/inper
                 enddo
              enddo

              x2i(1)=x2(1)
              do ix=2,inx2
                 do ixi=1,inper
                    ii=1+(ix-2)*inper+ixi
                    x2i(ii)=(x2(ix-1)*(inper-ixi)+x2(ix)*ixi)/inper
                 enddo
              enddo

              x3i(1)=x3(1)
              do ix=3,inx3
                 do ixi=1,inper
                    ii=1+(ix-3)*inper+ixi
                    x3i(ii)=(x3(ix-1)*(inper-ixi)+x3(ix)*ixi)/inper
                 enddo
              enddo

              do ix=1,inx3i
                 do ix2=1,inx2i
                    wk1(1:inx1i)=x2i(ix2)
                    wk2(1:inx1i)=x3i(ix)

                    call xplasma_eval_prof(s,id,idx1,x1i,idx2,wk1,idx3,wk2, &
                         ff(1:inx1i,ix2,ix),ier, &
                         ideriv1=ideriv1,ideriv2=ideriv2,ideriv3=ideriv3)
                    call ckerr

                 enddo
              enddo

              call r8_grf4f1(ff,inx1i,inx2i,x1i,inx1i,x2i,inx2i,x3i,inx3i, &
                   trim(fname_d)//' @'//labelt,x1name,x2name,x3name, &
                   funitsd,x1units,x2units,x3units, &
                   labelx,0)
               
              deallocate(x1i,x1,x2i,x2,x3i,x3,ff,wk1,wk2)

           else if(lchar('D')) then

              if(idmax.eq.0) then
                 call uermsg(' no derivative information (step function).', &
                      ' ',0)
                 cycle
              endif

              !  derivative control

              if(lxout(0)) then
                 write(lu,*) ' '
                 write(lu,*) ' min:0, max: ',idmax
                 write(lu,*) ' 0 means: no derivative; 1 means 1st derivative.'
                 if(idmax.gt.1) write(lu,*) ' >1 means higher derivative.'
              endif

              call ureadi('select derivative w.r.t. '//trim(x1name)//':', &
                   ideriv1)
              ideriv1=max(0,min(idmax,ideriv1))

              call ureadi('select derivative w.r.t. '//trim(x2name)//':', &
                   ideriv2)
              ideriv2=max(0,min(idmax,ideriv2))

              call ureadi('select derivative w.r.t. '//trim(x3name)//':', &
                   ideriv3)
              ideriv3=max(0,min(idmax,ideriv3))

              idsum=ideriv1+ideriv2+ideriv3

              if(idsum.eq.0) then
                 fname_d = fname
                 funitsd = funits

              else
                 if(idsum.eq.1) then
                    cdsum='1st'
                 else if(idsum.eq.2) then
                    cdsum='2nd'
                 else if(idsum.eq.3) then
                    cdsum='3rd'
                 else
                    write(cdsum,'(i1,"th")') idsum
                 endif

                 funitsd = trim(funits)//"/"
                 if(idsum.eq.1) then
                    fname_d = 'd('//trim(fname)//')/'
                 else
                    write(fname_d,'("d^",i1,"(",a,")/")') idsum,trim(fname)
                 endif

                 if(ideriv1.eq.1) then
                    funits_wk = trim(funitsd) // '('//trim(x1units)//')'
                    fname_w = trim(fname_d)//'d('//trim(x1name)//')'
                 else if(ideriv1.gt.1) then
                    funits_wk = ' '
                    write(funits_wk,'(a,"(",a,")^",i1)') trim(funitsd), &
                         trim(x1units),ideriv1
                    fname_w = ' '
                    write(fname_w,'(a,"d^",i1,"(",a,")")') trim(fname_d), &
                         ideriv1,trim(x1name)
                 endif
                 if(ideriv1.gt.0) then
                    funitsd = funits_wk
                    fname_d = fname_w
                 endif

                 if(ideriv2.eq.1) then
                    funits_wk = trim(funitsd) // '('//trim(x2units)//')'
                    fname_w = trim(fname_d)//'d('//trim(x2name)//')'
                 else if(ideriv2.gt.1) then
                    funits_wk = ' '
                    write(funits_wk,'(a,"(",a,")^",i1)') trim(funitsd), &
                         trim(x2units),ideriv2
                    fname_w = ' '
                    write(fname_w,'(a,"d^",i1,"(",a,")")') trim(fname_d), &
                         ideriv2,trim(x2name)
                 endif
                 if(ideriv2.gt.0) then
                    funitsd = funits_wk
                    fname_d = fname_w
                 endif

                 if(ideriv3.eq.1) then
                    funits_wk = trim(funitsd) // '('//trim(x3units)//')'
                    fname_w = trim(fname_d)//'d('//trim(x3name)//')'
                 else if(ideriv3.gt.1) then
                    funits_wk = ' '
                    write(funits_wk,'(a,"(",a,")^",i1)') trim(funitsd), &
                         trim(x3units),ideriv3
                    fname_w = ' '
                    write(fname_w,'(a,"d^",i1,"(",a,")")') trim(fname_d), &
                         ideriv3,trim(x3name)
                 endif
                 if(ideriv3.gt.0) then
                    funitsd = funits_wk
                    fname_d = fname_w
                 endif

              endif

           else if(lchar('N')) then

              !  interpolation evaluation resolution control

              if(lxout(0)) write(lu,*) ' min:2, max: 20'
              call ureadi( &
                   'number of interpolation evaluations per grid zone:', &
                   inper)
              inper=max(2,min(20,inper))

           else if(lchar('Q')) then

              !  all done

              exit

           else
              call uermsg('?prof_plot3d: unrecognized option.',' ',0)
           endif

        enddo

      end subroutine prof_plot3d

      subroutine app_grid(irank,inum,idx)

        !  append grid related information...

        integer,intent(in) :: irank   ! rank of profile
        integer,intent(in) :: inum    ! which x axis this is
        integer,intent(in) :: idx     ! x axis id

        integer :: inx,icoord
        character*32 zname,cname
        character*6 intcc
        integer :: ic,ic1

        if(inum.gt.irank) return
        if(idx.eq.0) return

        call xplasma_get_item_info(s,idx,ier, name=zname); call ckerr
        call xplasma_grid_info(s,idx,ier, coord=icoord, size=inx); call ckerr
        call xplasma_get_item_info(s,icoord,ier, name=cname); call ckerr

        intcc=' '
        write(intcc,'(I6)') inx
        do ic=1,len(intcc)
           if(intcc(ic:ic).ne.' ') then
              ic1=ic
              exit
           endif
        enddo

        if(inum.eq.1) then
           ic=len(trim(grids))
           grids(ic+2:)=trim(zname)

           ic=len(trim(coords))
           coords(ic+2:)=trim(cname)

           ic=len(trim(resolution))
           resolution(ic+2:)=intcc(ic1:len(intcc))

        else
           ic=len(trim(grids))
           grids(ic+1:)=', '//trim(zname)

           ic=len(trim(coords))
           coords(ic+1:)=', '//trim(cname)

           ic=len(trim(resolution))
           resolution(ic+1:)=' x '//intcc(ic1:len(intcc))

        endif

      end subroutine app_grid

      !------------------------------------------------
      subroutine list_display

        integer :: isize,ii

        character*32, dimension(:), allocatable :: lnames
        integer, dimension(:), allocatable :: ivals
        real*8, dimension(:), allocatable :: r8vals
        character*70, dimension(:), allocatable :: chvals

        call xplasma_getList(s,id,isize,ier)

        if(isize.eq.0) then
           write(luntty,*) '   (list is empty)'
        else
           write(luntty,*) '   list contains ',isize,' elements:'

           allocate(lnames(isize),ivals(isize),r8vals(isize),chvals(isize))

           call xplasma_getList_names(s,id,lnames,ier, &
                ivals=ivals,r8vals=r8vals,chvals=chvals)

           do ii=1,isize
              write(luntty,1001) &
                   lnames(ii),ivals(ii),r8vals(ii),trim(chvals(ii))
           enddo
1001       format(6x,a,' int=',i8,4x,' float=',1pe14.7/8x,'"',a,'"')
        endif

      end subroutine list_display

    end subroutine xplasma_display1

    !=========================================================================
    !  **private**

    subroutine xplasma_mg_display(s,idplist,inum)

      ! debugging tool for xplasma -- display multigraphs.  A multigraph
      ! is a set of 1 or more profiles defined over common coordinates.

      type (xplasma), pointer :: s
      integer, intent(in) :: inum   ! number of profiles
      integer, intent(in) :: idplist(inum)  ! all the profile IDs

      !----------------------------------
      integer, dimension(:), allocatable :: ids, idlists
      integer :: ier,ii,ityp,ityp_want,jj,icount,id,itype,ifound,idum,ilen

      integer :: inum_mgtot,inum_mgmax,inum_mg
      integer :: incr = 100

      character*32, dimension(:), allocatable :: mg_names
      character*32, dimension(:), allocatable :: mg_units
      character*32, dimension(:), allocatable :: mg_author
      character*80, dimension(:), allocatable :: mg_labels
      integer, dimension(:), allocatable :: mg_types
      integer, dimension(:,:), allocatable :: mg_profs

      integer, parameter :: inum_mggmx=100
      integer :: inum_mgrps,inum_profs,inum_orig,icur,inew
      integer :: grp_sizes(inum_mggmx),grp_types(inum_mggmx)
      integer :: grp_ranks(inum_mggmx)
      character*80 gclbl(inum_mggmx),clabel

      character*32 :: name_ss,auth_ss,upname,units,pname,aname

      character*15 :: ddesc(3) = &
           (/ "minimal     ", "intermediate", "maximal     " /)
      integer :: idetail
      integer :: ig1,ig2,ig3,ic1,ic2,ic3,ictmp,icnum,irank,icode
      logical :: name_ok,auth_ok,aok
      !----------------------------------

      luntty = lnout(0)

      if(lxout(0)) write(luntty,*) ' %number of known profiles: ',inum
      if(inum.eq.0) return

      name_ss='*'
      auth_ss='*'
      idetail=2

      allocate(ids(inum))
      call xplasma_contents(s,ier, id_profs=ids)
      if(ier.ne.0) then
         call xplasma_error(s,ier,luntty)
         deallocate(ids)
         return
      endif

      call xplasma_num_items(s,ier, num_plists=inum_mgtot)
      inum_mgmax = inum_mgtot
      call alloc

      inum_mgrps = 0
      grp_sizes = 0
      grp_types = 0
      grp_ranks = 0
      gclbl = ' '

      allocate(idlists(inum_mgtot))
      call xplasma_contents(s,ier, id_plists=idlists)

      inum_mg = 0
      do ii=1,inum_mgtot
         call mg_analyze(idlists(ii))
      enddo
      inum_orig = inum_mg

      do

         if(lxout(0)) then
            write(luntty,*) ' '
            write(luntty,*) ' Number of known Multigraphs: ',inum_mg
            write(luntty,*) ' Number of known Multigraph types: ',inum_mgrps
            do ii=1,inum_mgrps
               write(luntty,*) '        type ',ii,'--  (',grp_sizes(ii),') vs. ',trim(gclbl(ii))
            enddo
            write(luntty,*) '    name substring:   ',trim(name_ss)
            write(luntty,*) '    author substring: ',trim(auth_ss)
            write(luntty,*) '      (case insensitive; "*" means wildcard)'
            write(luntty,*) ' '
            write(luntty,*) ' xplasma_mg_display options:'
            write(luntty,*) '  "N": set name substring.'
            write(luntty,*) '  "A": set author substring.'
            write(luntty,*) '  "D": set level of detail for Multigraph contents listings:'
            write(luntty,*) '       currently: ',trim(ddesc(idetail))//'.'
            write(luntty,*) '  "L": display list of Multigraphs.'
            write(luntty,*) '  "U": to add user defined Multigraph (author = "USER")'
            write(luntty,*) '  "P": *** PLOT a multigraph now.'
            write(luntty,*) ' '
            write(luntty,*) ' (following for individual profile):'
            write(luntty,*) '  "T": profiles table of contents (all profiles).'
            write(luntty,*) '  "S": examine a single profile.'
            write(luntty,*) '  "Q": quit.'
         endif

         call uread('xplasma_mg_display command option:')

         if(lchar('D')) then
            if(lxout(0)) then
               write(luntty,*) &
                    ' enter "1" for minimal detail: name & label only.'
               write(luntty,*) &
                    ' enter "2" for intermediate detail: ', &
                    'name, label, units, author, contents.'
               write(luntty,*) &
                    ' enter "3" for maximal detail: everything.'
            endif

            call ureadi('xplasma_mg_display level of detail:',idetail)
            idetail = min(3,max(1,idetail))

         else if(lchar('N')) then
            if(lxout(0)) write(luntty,*) ' --> enter search substring, or,'
            if(lxout(0)) write(luntty,*) '     prepend "^" for prefix;'
            if(lxout(0)) write(luntty,*) '     append "$" for suffix;'
            if(lxout(0)) write(luntty,*) ' --> or enter "*" for all names.'
            call uread_cmd_disable
            call ureadw('search substring for names:',name_ss)
            call uread_cmd_enable
            call uupper(name_ss)

         else if(lchar('A')) then
            call set_auth_ss

         else if(lchar('L')) then
            
            ityp_want=0
            if(lxout(0)) then
               write(lnout(0),*) &
                    ' enter number between 1 and ',inum_mgrps,' to select'
               write(lnout(0),*) &
                    'type of Multigraph, or "0" or "," for all types.'
            endif

            call ureadi('Multigraph type select for listing: ',ityp_want)
            ityp_want = max(0,min(inum_mgrps,ityp_want))

            write(luntty,*) ' '
            write(luntty,*) ' -------------------------------------- '
            write(luntty,*) ' '

            icount=0
            do ii=1,inum_mg
               ityp=mg_types(ii)
               do jj=1,inum_mgrps
                  if(ityp.eq.grp_types(jj)) then
                     ityp=jj
                     exit
                  endif
               enddo

               if((ityp_want.ne.0).and.(ityp.ne.ityp_want)) cycle
               
               call xplasma_mg_list1(s,mg_names(ii), &
                    mg_labels(ii),mg_units(ii),mg_author(ii), &
                    name_ss,auth_ss, &
                    mg_profs(1:15,ii),idetail,icount)

            enddo

            write(luntty,*) ' '
            write(luntty,*) ' -------------------------------------- '
            write(luntty,*) ' '

         else if(lchar('U')) then
            if(inum_mg.eq.inum_mgmax) call alloc

            inum_mg = inum_mg + 1
            mg_labels(inum_mg)=' '

            call ureadw('name of multigraph (alphanumeric word): ', &
                 mg_names(inum_mg))
            call xplasma_checkName_sub(mg_names(inum_mg),ier)
            if(ier.ne.0) then
               call uermsg('invalid name (syntax)',mg_names(inum_mg),0)
               inum_mg=inum_mg-1
               cycle
            endif

            icur=inum_mg
            inew=1
            do ii=1,inum_mg-1
               if(mg_names(ii).eq.mg_names(inum_mg)) then
                  if(ii.le.inum_orig) then
                     call uermsg('invalid name (already defined in xplasma)', &
                          mg_names(inum_mg),0)
                     inum_mg=inum_mg-1
                     icur=0
                  else
                     icur=ii
                     write(luntty,*) ' redefining: ',mg_names(icur)
                     inum_mg=inum_mg-1
                     inew=0
                  endif
                  exit
               endif
            enddo
            if(icur.eq.0) cycle

            mg_labels(icur)=' '
            mg_units(icur)=' '
            if(inew.eq.0) then
               itype=mg_types(icur)
               grp_sizes(itype)=grp_sizes(itype)-1  ! replacing definition...
            endif

            if(lxout(0)) write(luntty,*) ' max length of label: ', &
                 len(mg_labels(icur)),' characters.'
            call ureadll('label for multigraph: ',mg_labels(icur))

            mg_author(icur) = 'USER'

            mg_profs(1:15,icur)=0
            inum_profs=0
            do
               if(lxout(0)) then
                  write(luntty,*) ' '
                  write(luntty,*) &
                       ' Note: wildcard expressions OK but no more than 15'
                  write(luntty,*) &
                       '       profiles per multiplot are allowed.'
                  write(luntty,*) &
                       '       examples of supported wildcard expressions:'
                  write(luntty,*) &
                       '          BDENS* -- profiles with names starting with "BDENS"'
                  write(luntty,*) &
                       '          *_XDB -- profiles with names ending in "_XDB".'
                  write(luntty,*) ' Author substring restricts profile set;'
                  write(luntty,*) ' enter ".A" to modify author substring: ', &
                       trim(auth_ss)
                  write(luntty,*) ' enter NAME of profile to add, or:'
                  write(luntty,*) ' "0" or "," or "." or ".Q" to exit loop & complete definition'
                  write(luntty,*) ' ".T" for profile table of contents.'
               endif

               write(luntty,*) ' '
               write(luntty,*) ' Number of profiles in Multigraph: ',inum_profs
               upname='.'
               call uread_cmd_disable
               call ureadw(' name of profile to add: ',upname)
               call uread_cmd_enable

               if(upname.eq.'0') exit
               if(upname.eq.'.') exit
               if(upname.eq.'.Q') exit
               if(upname.eq.'.T') then
                  call xplasma_contents_display(s,inum,ids,xplasma_profType)
                  cycle
               endif
               if(upname.eq.'.A') then
                  call set_auth_ss
                  cycle
               endif

               if(upname.eq.'*') then
                  call uermsg('bare "*": invalid wildcard expression',' ',0)
                  cycle
               endif

               ilen=len(trim(upname))

               if(upname(1:1).eq.'*') then
                  upname = trim(upname(2:ilen))//'$'
               else if(upname(ilen:ilen).eq.'*') then
                  upname = '^'//upname(1:ilen-1)
               else
                  upname = '^'//upname(1:ilen)//'$'
               endif

               idum=0
               do ii=1,inum
                  id=idplist(ii)
                  call xplasma_prof_info(s,id,ier, name=pname, author=aname)
                  if(ier.ne.0) then
                     call xplasma_error(s,ier,luntty)
                  else
                     call xpdebug_list_filter(pname,aname,upname,auth_ss, &
                          name_ok,auth_ok)
                     aok = name_ok.AND.auth_ok

                     if(aok) then

                        call xplasma_prof_info(s,id,ier, &
                             gridId1=ig1,gridId2=ig2,gridId3=ig3,units=units)
                        call mg_ccode

                        if(inum_profs.eq.0) then
                           write(luntty,*) ' added to Multigraph: ',trim(pname)
                           idum=idum+1
                           inum_profs=inum_profs+1
                           mg_units(icur)=units
                           mg_profs(inum_profs,icur)=id
                           mg_types(icur)=icode

                        else
                           if(mg_types(icur).ne.icode) then
                              call uermsg( &
                                   'profile coordinates do not match',upname,0)
                              cycle
                           endif
                           if(mg_units(icur).ne.units) then
                              write(luntty,*) ' note: units mismatch (ignored):'
                              write(luntty,*) '   1st profile: ',trim(mg_units(icur))
                              write(luntty,*) '   this profile: ',trim(units)
                           endif
                           write(luntty,*) ' added to Multigraph: ',trim(pname)
                           idum=idum+1
                           inum_profs=inum_profs+1
                           mg_profs(inum_profs,icur)=id
                           if(inum_profs.eq.15) then
                              write(luntty,*) ' note: multigraph at capacity (15 profiles)'
                              call uclear
                              exit
                           endif
                        endif
                     endif
                  endif
               enddo
               if(idum.eq.0) then
                  call uermsg('nothing added under name/expression',upname,0)
               endif
               if(inum_profs.eq.15) exit
            enddo
            if(inum_profs.eq.0) then
               if(icur.eq.inum_mg) then
                  call uermsg('empty multigraph deleted',mg_names(inum_mg),0)
                  inum_mg=inum_mg-1
               else
                  call uermsg('caution: empty multigraph',mg_names(icur),0)
               endif
            else
               ifound=0
               do ityp=1,inum_mgrps
                  if(icode.eq.grp_types(ityp)) then
                     ifound=ityp
                     grp_sizes(ityp)=grp_sizes(ityp)+1
                     exit
                  endif
               enddo
               if(ifound.eq.0) then
                  inum_mgrps=inum_mgrps+1
                  grp_types(inum_mgrps)=icode
                  grp_sizes(inum_mgrps)=1
                  grp_ranks(inum_mgrps)=irank  ! cf mg_ccode call above...
               endif
            endif

         else if(lchar('P')) then
            call ureadw('name of multigraph to plot:',upname)
            ifound=0
            do ii=1,inum_mg
               if(mg_names(ii).eq.upname) then
                  ifound=ii
                  exit
               endif
            enddo
            if(ifound.eq.0) then
               call uermsg('not found',upname,0)
               cycle
            endif

            !  echo information on the selected profile...

            ii=ifound
            if(mg_profs(1,ii).eq.0) then
               call uermsg('empty multigraph',upname,0)
               cycle
            endif
            idum=0
            call xplasma_mg_list1(s,mg_names(ii), &
                 mg_labels(ii),mg_units(ii),mg_author(ii), &
                 mg_names(ii),'*', &
                 mg_profs(1:15,ii),idetail,idum)

            irank=grp_ranks(mg_types(ii))
            if(irank.gt.1) then
               call uermsg(' ** display not implemented: rank > 1 multigraphs.',upname,0)
            else

               ! refetch dependent coordinate codes
               call xplasma_prof_info(s,mg_profs(1,ii),ier, &
                    gridId1=ig1,gridId2=ig2,gridId3=ig3)
               call mg_ccode

               call xplasma_mg_plot1(s,mg_names(ii), &
                    mg_labels(ii),mg_units(ii),mg_author(ii), &
                    mg_profs(1:15,ii),ic1)

            endif

         else if(lchar('T')) then
            call xplasma_contents_display(s,inum,ids,xplasma_profType)

         else if(lchar('S')) then
            call xplasma_select1(s,'xplasma profile',xplasma_profType)

         else if(lchar('Q')) then
            exit

         else
            call uermsg('?xplasma_mg_display: unrecognized command option.', &
                 ' ',0)
         endif

      enddo

      deallocate(ids)

      call free

      contains
        subroutine set_auth_ss

          if(lxout(0)) write(luntty,*) ' --> enter search substring, or,'
          if(lxout(0)) write(luntty,*) '     prepend "^" for prefix;'
          if(lxout(0)) write(luntty,*) '     append "$" for suffix;'
          if(lxout(0)) write(luntty,*) ' --> enter "*" for all authors.'
          call uread_cmd_disable
          call ureadw('search substring for authors:',auth_ss)
          call uread_cmd_enable
          call uupper(auth_ss)

        end subroutine set_auth_ss

        subroutine mg_analyze(idlist)
          integer, intent(in) :: idlist   ! list ID

          ! list contains 1 or more profile IDs

          !--------------------
          integer :: isize,ier,jj,itype,imem,isave,ifind
          character*32 lname,pname,units,author
          character*32, dimension(:), allocatable :: enames
          character*80 :: label
          integer, dimension(:), allocatable :: ivals
          integer :: idprof,idps(15)
          !--------------------

          call xplasma_list_info(s,idlist,ier, nelems=isize, &
               name=lname, label=label, author=author)
          if(label.eq.' ') label=lname

          if(ier.ne.0) then
             write(luntty,*) ' skipped list "',trim(lname),'":'
             call xplasma_error(s,ier,luntty)
             return
          endif

          allocate(enames(isize),ivals(isize))
          call xplasma_getList_names(s,idlist,enames,ier)
          call xplasma_getList_ivals(s,idlist,ivals,ier)

          imem = 0  ! count profiles referenced in list...
          idps = 0

          do jj=1,isize
             call xplasma_get_item_info(s,ivals(jj),ier, nf_noerr=.TRUE., &
                  name=pname, itype=itype)

             if(itype.ne.xplasma_profType) cycle

             call uupper(enames(jj))
             call uupper(pname)
             if(pname.ne.enames(jj)) cycle

             ! have a valid profile id

             idprof = ivals(jj)

             if(imem.eq.0) then
                call xplasma_prof_info(s,idprof,ier, &
                     units=units)
             endif

             call xplasma_prof_info(s,idprof,ier, &
                  gridId1=ig1,gridId2=ig2,gridId3=ig3)

             !  map to coordinates

             call mg_ccode

             if(imem.eq.0) then
                isave=icode
                clabel=' '
                call clabel_append(ic1)
                if(ic2.gt.0) call clabel_append(ic2)
                if(ic3.gt.0) call clabel_append(ic3)
             else
                if(isave.ne.icode) then
                   write(luntty,*) &
                        ' %mg_analyze: profile elements in list: "',&
                        trim(lname),'".'
                   write(luntty,*) '  inconsistent (different coordinate sets)'
                   imem=0
                   exit
                endif
             endif

             imem=imem+1
             idps(imem) = idprof
             if(imem.eq.size(idps)) exit
          enddo

          if(imem.gt.0) then
             inum_mg = inum_mg + 1
             mg_names(inum_mg)=lname
             mg_units(inum_mg)=units
             mg_labels(inum_mg)=label
             mg_author(inum_mg)=author
             mg_types(inum_mg)=icode
             mg_profs(1:15,inum_mg)=idps

             ifind=0
             do jj = 1,inum_mgrps
                if(icode.eq.grp_types(jj)) then
                   ifind=jj
                   exit
                endif
             enddo

             if(ifind.eq.0) then
                inum_mgrps = inum_mgrps + 1
                ifind=inum_mgrps
                grp_types(ifind)=icode
                grp_ranks(ifind)=irank
                gclbl(ifind)=clabel
             endif

             grp_sizes(ifind)=grp_sizes(ifind) + 1
          endif

        end subroutine mg_analyze

        subroutine clabel_append(ic)

          integer, intent(in) :: ic
          character*32 zstr
          integer :: ilen

          zstr = ' '
          if(ic.eq.xplasma_rho_coord) then
             zstr='rho'

          else if(ic.eq.xplasma_theta_coord) then
             zstr='theta'

          else if(ic.eq.xplasma_phi_coord) then
             zstr='phi'

          else if(ic.eq.xplasma_R_coord) then
             zstr='R'

          else if(ic.eq.xplasma_Z_coord) then
             zstr='Z'

          else
             call xplasma_coord_info(s,ic,ier, name=zstr)
          endif

          if(clabel(1:1).eq.' ') then
             clabel=zstr

          else
             ilen=len(trim(clabel))
             clabel(ilen+1:) = ','//trim(zstr)
          endif

        end subroutine clabel_append

        subroutine mg_ccode

          ! convert grid codes to coordinate codes; form single code for all

          call xplasma_grid_info(s,ig1,ier, coord=ic1)
          if(ig2.eq.0) then
             ic2=0
          else
             call xplasma_grid_info(s,ig2,ier, coord=ic2)
          endif
          if(ig3.eq.0) then
             ic3=0
          else
             call xplasma_grid_info(s,ig3,ier, coord=ic3)
          endif

          icnum = 0
          if(ic1.gt.0) icnum = 1
          if(ic2.gt.0) icnum = 2
          if(ic3.gt.0) icnum = 3
          irank = icnum
          
          if(icnum.ge.2) then
             if(ic2.lt.ic1) then
                ictmp=ic2
                ic2=ic1
                ic1=ictmp
             endif
          endif

          if(icnum.eq.3) then
             if(ic3.lt.ic2) then
                ictmp=ic3
                ic3=ic2
                ic2=ictmp
                if(ic2.lt.ic1) then
                   ic2=ic1
                   ic1=ictmp
                endif
             endif
          endif

          !  ic1 < ic2 < ic3 now
          icode = 2**ic1 + 2**ic2 + 2**ic3

        end subroutine mg_ccode

        subroutine free

          deallocate(mg_names,mg_units,mg_labels,mg_author)
          deallocate(mg_types,mg_profs)

        end subroutine free

        subroutine alloc

          integer :: inum_prev

          character*32, dimension(:), allocatable :: buf32
          character*80, dimension(:), allocatable :: buf80
          integer, dimension(:), allocatable :: ibuf
          integer, dimension(:,:), allocatable :: i2buf

          inum_prev =inum_mgmax
          inum_mgmax=inum_mgmax + incr

          if(allocated(mg_names)) then
             allocate(buf32(inum_prev),buf80(inum_prev))
             allocate(ibuf(inum_prev),i2buf(15,inum_prev))

             buf32 = mg_names
             deallocate(mg_names)
             allocate(mg_names(inum_mgmax))
             mg_names(1:inum_prev)=buf32
             mg_names(inum_prev+1:)=' '

             buf32 = mg_units
             deallocate(mg_units)
             allocate(mg_units(inum_mgmax))
             mg_units(1:inum_prev)=buf32
             mg_units(inum_prev+1:)=' '

             buf32 = mg_author
             deallocate(mg_author)
             allocate(mg_author(inum_mgmax))
             mg_author(1:inum_prev)=buf32
             mg_author(inum_prev+1:)=' '

             buf80 = mg_labels
             deallocate(mg_labels)
             allocate(mg_labels(inum_mgmax))
             mg_labels(1:inum_prev)=buf80
             mg_labels(inum_prev+1:)=' '

             ibuf = mg_types
             deallocate(mg_types)
             allocate(mg_types(inum_mgmax))
             mg_types(1:inum_prev)=ibuf
             mg_types(inum_prev+1:)=0

             i2buf = mg_profs
             deallocate(mg_profs)
             allocate(mg_profs(15,inum_mgmax))
             mg_profs(1:15,1:inum_prev)=i2buf
             mg_profs(1:15,inum_prev+1:)=0

             deallocate(buf32,buf80,ibuf,i2buf)

          else

             allocate(mg_names(inum_mgmax))
             mg_names = ' '

             allocate(mg_units(inum_mgmax))
             mg_units = ' '

             allocate(mg_author(inum_mgmax))
             mg_author = ' '

             allocate(mg_labels(inum_mgmax))
             mg_labels = ' '

             allocate(mg_types(inum_mgmax))
             mg_types = 0

             allocate(mg_profs(15,inum_mgmax))
             mg_profs = 0

          endif
        end subroutine alloc
          
    end subroutine xplasma_mg_display

    subroutine xplasma_mg_plot1(s,name,label,units,author, &
         proflist,icoord)

      !  plot rank 1 multigraph (all profiles vs. same coordinate).

      type (xplasma), pointer :: s
      character*(*), intent(in) :: name   ! name of MG
      character*(*), intent(in) :: label  ! MG label
      character*(*), intent(in) :: units  ! MG units
      character*(*), intent(in) :: author ! MG author label

      integer, intent(in), dimension(:) :: proflist
      integer, intent(in) :: icoord

      !------------------------
      integer :: ii,ix,inum,igs(15),inxs(15),inxi,iertmp,ideriv,ixtype
      integer :: mintype,ispline,ixmul,idum,jderiv
      character*128 labelxp,label_all
      character*30 labelt,d_option,x_option
      character*32 pnames(15),x_units,x_name,f_units
      real*8 ztime,xmin,xmax
      real*8, dimension(:), allocatable :: xx,xxc,xwk,da,dv
      real*8, dimension(:,:), allocatable :: ff,ffd
      !------------------------

      inum=size(proflist)
      igs=0
      inxs=0
      iertmp=0

      mintype=2
      do ii=1,size(proflist)
         if(proflist(ii).eq.0) then
            inum=ii-1
            exit
         else
            call xplasma_prof_info(s,proflist(ii),iertmp, &
                 splineType=ispline, gridId1=igs(ii), name=pnames(ii))
            if(iertmp.ne.0) then
               inum=ii-1
               exit
            endif

            mintype=min(mintype,ispline)

            if(ii.eq.1) then
               call xplasma_grid_info(s,igs(ii),iertmp, &
                    size=inxs(ii), xmin=xmin, xmax=xmax) ! xmin/max shared
            else
               call xplasma_grid_info(s,igs(ii),iertmp, &
                    size=inxs(ii))
            endif
            if(iertmp.ne.0) then
               inum=ii-1
               exit
            endif
         endif
      enddo

      if(inum.eq.0) then
         write(luntty,*) ' ?xplasma_mg_plot1 error: no profiles in Multigraph.'
      endif

      if(iertmp.ne.0) then
         write(luntty,*) ' ?xplasma_mg_plot1: unexpected xplasma error:'
         call xplasma_error(s,iertmp,luntty)
      endif

      if((inum.eq.0).or.(iertmp.ne.0)) return

      call xplasma_global_info(s,iertmp, initLabel=labelxp, time=ztime)
      labelt=' '
      write(labelt,'("time=",1pe13.6)') ztime
      labelxp=trim(labelxp)//' '//trim(labelt)//' ('//trim(author)//')'

      !---------------------------
      !  default settings

      ideriv=0
      jderiv=0
      d_option = ' '

      ixtype=0
      x_option = 'fine mesh'
      ixmul=10

      do

         call xgen

         call ulabel

         if(lxout(0)) then
            write(luntty,*) ' '
            write(luntty,*) ' current x axis option: ',trim(x_option)
            if(ideriv.eq.0) then
               write(luntty,*) ' current derivative option: NONE'
            else
               write(luntty,*) ' current derivative option: ',trim(d_option)
            endif
            write(luntty,*) ' '
            write(luntty,*) ' xplasma_mg_plot1 options:'
            write(luntty,*) '  "D" -- set derivative option'
            write(luntty,*) '  "X" -- set x axis option'
            write(luntty,*) '  "P" -- plot the data'
            write(luntty,*) '  "Q" -- quit'
            write(luntty,*) ' '
         endif

         call uread('xplasma_mg_plot1 option:')

         if(lchar('Q')) then
            exit

         else if(lchar('X')) then
            if(lxout(0)) then
               write(luntty,*) ' 0 -- fine mesh (multiplier = ',ixmul,')'
               do ii=1,inum
                  write(luntty,'(1x,i2," -- x grid from: ",a)') &
                       ii,trim(pnames(ii))
               enddo
               write(luntty,*) ' '
               write(luntty,*) ' >> enter x axis type code btw 0 and ',inum
               write(luntty,*) '    enter "Q" to skip...'
            endif

            idum=-1
            call ureadi(' integer code to identify x axis source:',idum)
            if(idum.eq.-1) cycle

            ixtype=max(0,min(inum,ixtype))

            if(ixtype.eq.0) then
               call ureadi(' fine mesh multiplier (btw 2 and 100):',ixmul)
               ixmul=max(2,min(100,ixmul))
               x_option='fine mesh'

            else
               x_option=trim(pnames(ixtype))//' grid'
            endif

         else if(lchar('D')) then

            if(lxout(0)) then
               write(luntty,*) ' available derivative options:'
               write(luntty,*) '  0 -- no derivative'
               if(mintype.ge.1) then
                  write(luntty,*) '  1 -- 1st derivative'
               endif
               if(mintype.ge.2) then
                  write(luntty,*) '  2 -- 2nd derivative'
               endif
               if(icoord.eq.xplasma_rho_coord) then
                  write(luntty,*) '  A -- df/dArea (finite diff)'
                  write(luntty,*) '  V -- df/dVol (finite diff)'
               endif
            endif

            call uread('derivative option:')

            if(lchar('Q')) then
               continue

            else if(lchar('0')) then
               ideriv=0
               jderiv=0
               d_option=' '

            else if(lchar('1')) then
               if(mintype.ge.1) then
                  ideriv=1
                  jderiv=1
                  d_option='d/d('//trim(x_name)//')'
               else
                  write(luntty,*) ' not a spline-- no derivative option.'
                  call uclear
               endif

            else if(lchar('2')) then
               if(mintype.ge.2) then
                  ideriv=2
                  jderiv=2
                  d_option='d2/d('//trim(x_name)//')2'
               else
                  write(luntty,*) ' not a spline-- no derivative option.'
                  call uclear
               endif

            else if(lchar('A')) then
               if(icoord.eq.xplasma_rho_coord) then
                  ideriv=-2
                  jderiv=0
                  d_option='d/dArea'
               else
                  write(luntty,*) ' d/dArea not available: not f(rho)'
                  call uclear
               endif

            else if(lchar('V')) then
               if(icoord.eq.xplasma_rho_coord) then
                  ideriv=-3
                  jderiv=0
                  d_option='d/dVol'
               else
                  write(luntty,*) ' d/dVol not available: not f(rho)'
                  call uclear
               endif

            else
               call uermsg('invalid derivative option; no change.',' ',0)
            endif

         else if(lchar('P')) then

            label_all = trim(d_option)//' '//trim(label)//' vs. '//trim(x_name)

            allocate(ff(inxi,inum),ffd(inxi-1,inum))

            call xplasma_eval_prof(s,proflist(1:inum), &
                 xx,ff,iertmp,ideriv1=jderiv)

            if(iertmp.ne.0) then
               write(luntty,*) ' ?xplasma_mg_plot1 evaluation error:'
               call xplasma_error(s,iertmp,luntty)
            else

               if(ideriv.lt.0) then

                  do ii=1,inum
                     if(ideriv.eq.-2) then
                        ffd(1:inxi-1,ii) = &
                             (ff(2:inxi,ii)-ff(1:inxi-1,ii))/dA
                     else if(ideriv.eq.-3) then
                        ffd(1:inxi-1,ii) = &
                             (ff(2:inxi,ii)-ff(1:inxi-1,ii))/dV
                     endif
                  enddo

                  call r8_mulgraf(labelxp,inxi-1,inum,xxc,ffd, &
                       x_units,f_units,label_all,pnames(1:inum))

               else

                  call r8_mulgraf(labelxp,inxi,inum,xx,ff, &
                       x_units,f_units,label_all,pnames(1:inum))

               endif
            endif

            deallocate(ff,ffd)

         else
            call uermsg('invalid plot menu option.',' ',0)
         endif

      enddo
      deallocate(xx,xxc,xwk,da,dv)

      contains
        subroutine xgen

          ! regen x axis for plotting

          if(allocated(xx)) deallocate(xx,xxc,xwk)
          if(allocated(dv)) deallocate(dv,da)

          if(ixtype.eq.0) then

             inxi = ixmul*maxval(inxs(1:inum)-1) + 1
             allocate(xx(inxi))
             do ix=1,inxi
                xx(ix)=((ix-1)*xmax + (inxi-ix)*xmin)/(inxi-1)
             enddo

          else

             inxi = inxs(ixtype)
             allocate(xx(inxi))

             call xplasma_grid(s,igs(ixtype),xx,iertmp)
             if(iertmp.ne.0) then
                write(luntty,*) ' ?(xplasma_mg_plot1) unexpected xplasma error: '
                call xplasma_error(s,iertmp,luntty)
                do ix=1,inxi
                   xx(ix)=((ix-1)*xmax + (inxi-ix)*xmin)/(inxi-1)
                enddo
             endif
      
          endif

          allocate(xwk(inxi),xxc(inxi-1))
          xwk=0
          do ix=1,inxi-1
             xxc(ix)=(xx(ix)+xx(ix+1))/2
          enddo

          if(icoord.eq.xplasma_rho_coord) then
             allocate(da(inxi-1),dv(inxi-1))

             call xplasma_volume(s,xx,xwk,iertmp)
             if(iertmp.ne.0) then
                write(luntty,*) ' ?(xplasma_mg_plot1) unexpected xplasma error: '
                call xplasma_error(s,iertmp,luntty)
                dv=1
             else
                dv=xwk(2:inxi)-xwk(1:inxi-1)
             endif

             call xplasma_area(s,xx,xwk,iertmp)
             if(iertmp.ne.0) then
                write(luntty,*) ' ?(xplasma_mg_plot1) unexpected xplasma error: '
                call xplasma_error(s,iertmp,luntty)
                da=1
             else
                da=xwk(2:inxi)-xwk(1:inxi-1)
             endif
          endif

        end subroutine xgen

        subroutine ulabel

          ! update units labels

          if(icoord.eq.xplasma_rho_coord) then
             x_units=' '
             x_name='rho'
          else if(icoord.eq.xplasma_theta_coord) then
             x_units=' '
             x_name='theta'
          else if(icoord.eq.xplasma_R_coord) then
             x_units='m'
             x_name='R'
          else if(icoord.eq.xplasma_Z_coord) then
             x_units='m'
             x_name='Z'
          else
             call xplasma_coord_info(s,icoord,iertmp, &
                  name=x_name, units=x_units)
             if(iertmp.ne.0) then
                write(luntty,*) ' ?xplasma_mg_plot1 xplasma error:'
                call xplasma_error(s,iertmp,luntty)
                x_units='?'
                x_name='?'
             endif
          endif

          f_units = units

          if(ideriv.eq.1) then
             if(x_units.ne.' ') then
                f_units = trim(f_units)//'/'//trim(x_units)
             endif

          else if(ideriv.eq.2) then
             if(x_units.ne.' ') then
                f_units = trim(f_units)//'/'//trim(x_units)//'**2'
             endif

          else if(ideriv.eq.-2) then
             f_units = trim(f_units)//'/m**2'

          else if(ideriv.eq.-3) then
             f_units = trim(f_units)//'/m**3'

          endif

        end subroutine ulabel
    end subroutine xplasma_mg_plot1


    subroutine xplasma_mg_list1(s,name,label,units,author, &
         name_ss,auth_ss, &
         id_contents,idetail,icount)

      !  print out listing information on Multigraph (MG)

      type (xplasma), pointer :: s
      character*(*), intent(in) :: name   ! name of MG
      character*(*), intent(in) :: label  ! MG label
      character*(*), intent(in) :: units  ! MG units
      character*(*), intent(in) :: author ! MG author label

      !  skip listing unless following criteria are met

      character*(*), intent(in) :: name_ss  ! name substring criterion
      character*(*), intent(in) :: auth_ss  ! author substring criterion

      !  MG contents
      integer, dimension(:), intent(in) :: id_contents  ! MG member element ids

      !  detail level
      integer, intent(in) :: idetail

      !  counter: incremented if item is listed
      integer, intent(inout) :: icount

      !-----------------------------------
      character*32 zzname,zzauth,pname
      character*50 :: conbuf
      integer :: iclen,ii,id,iertmp,iplen,idum

      logical :: name_ok,auth_ok
      !-----------------------------------

      zzname=name
      zzauth=author

      call uupper(zzname)
      call uupper(zzauth)

      call xpdebug_list_filter(zzname,zzauth,name_ss,auth_ss, &
           name_ok,auth_ok)

      if(.not.(name_ok.and.auth_ok)) return

      icount = icount + 1

      idum = 0

      if(idetail.eq.1) then
         if(icount.eq.1) then
            write(luntty,*) '  name                  label'
            write(luntty,*) '  ====================  ======================================'
         endif
         if(len(trim(name)).le.20) then
            write(luntty,'(3x,a,t26,a)') trim(name),trim(label)
         else
            write(luntty,'(3x,a)') trim(name)
            write(luntty,'(t26,a)') trim(label)
         endif

      else if(idetail.eq.2) then
         if(icount.eq.1) then
            write(luntty,*) '  name                  label'
            write(luntty,*) '  ====================  ======================================'
         endif
         if(len(trim(name)).le.20) then
            write(luntty,'(3x,a,t26,a)') trim(name),trim(label)
         else
            write(luntty,'(3x,a)') trim(name)
            write(luntty,'(t26,a)') trim(label)
         endif

         call mglist

         write(luntty,'(t26,"units: ",a,"  author: ",a)') &
              trim(units),trim(author)

      else if(idetail.eq.3) then
         if(icount.eq.1) then
            write(luntty,*) '  name                  label'
            write(luntty,*) '  ====================  ======================================'
         endif
         if(len(trim(name)).le.20) then
            write(luntty,'(3x,a,t26,a)') trim(name),trim(label)
         else
            write(luntty,'(3x,a)') trim(name)
            write(luntty,'(t26,a)') trim(label)
         endif

         write(luntty,'(t26,"units: ",a,"  author: ",a)') &
              trim(units),trim(author)

         call mglist

         write(luntty,*) ' '
         do ii=1,size(id_contents)
            id=id_contents(ii)
            if(id.eq.0) cycle
            call xplasma_display1(s,id,'*','*',.TRUE.,.FALSE.,.TRUE., &
                 .FALSE.,luntty,idum)
         enddo
         write(luntty,*) ' '

      else
         write(luntty,*) ' ?xplasma_mg_list1 ("',trim(name), &
              '") detail level not supported: ',idetail
      endif

      contains

        subroutine mglist

          !  print contents in compact form

          iclen=0
          conbuf=' '
          do ii=1,size(id_contents)
             id=id_contents(ii)
             if(id.eq.0) cycle

             call xplasma_get_item_info(s,id,iertmp,name=pname)
             if(iertmp.ne.0) then
                call xplasma_error(s,iertmp,luntty)
             else
                iplen=len(trim(pname))
                if(iclen+2+iplen.gt.len(conbuf)) then
                   write(luntty,'(t26,a)') trim(conbuf)
                   iclen=0
                   conbuf=' '
                endif
                conbuf(iclen+1:)=trim(pname)
                iclen=iclen+iplen+2
             endif
          enddo
          if(iclen.gt.0) write(luntty,'(t26,a)') trim(conbuf)

        end subroutine mglist

    end subroutine xplasma_mg_list1

    !=========================================================================
    !  **private**

    !=========================================================================

end module xplasma_debug_module
