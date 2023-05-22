      program geqxpl
c
c  create an xplasma from a geqdsk file
c  ...dmc 10 Jan 2001
c
      use eqm_mds_fromgeqdsk_mod
      implicit NONE
c
      INTEGER, PARAMETER :: R8=SELECTED_REAL_KIND(12,100)
      character*150 zfile,zfili,zfilsave
      integer ns,it1,nt1,ier,ilbl,i,ist,ifin,ihsig
      character*50 label
c
      integer lnout
      logical lxout,lyes,lchar,ireduce,ilmds
c
      real*8 fbdy,fbmin,fbmax,ztest,crat,zdum,zxmin,zdtfl,zmaxerr,xbrki
c
      character*50 mds_server
      character*20 mds_tree,mds_time,mds_shot,ctest
      integer ilsrv,iltr,iltm,ilsh,ilc,itest,irz,ixtra
      integer igot,ierr,is,imom,jmom,it,inumt,it_index,icur_psi
      integer iopt_pp
      real*8 ztime
c
      real*8 :: maxvar = 1.0d-4
c
      integer :: id_rho=0,id_phi=0,id_psi=0,id_q=0,id_g=0
      integer :: id_p=0,idum=0
      integer isnccwb,isnccwi
c
      real*8,dimension(:),allocatable :: zx,zdphi,zdpsi,zqprof,zq
      real*8,dimension(:),allocatable :: zdv,zr2i,zg,ztflux,ztfint
c
      integer :: idebug = 0  ! activate (set to =1) for Q/tflux test
      real*8,parameter :: z2pi = 6.2831853071795862D+00
c
      integer :: lunio = 77  ! LUN for fortran ascii file write
c
c---------------------------------------------------------------------
c
      call uinitg("GEQXPL")
c
      ireduce=.TRUE.
      mds_server='LOCAL'
      mds_tree='EFIT01'
      mds_time='1.0'
      mds_shot='123456'
c
      irz=1
      fbdy=1.00_r8
      crat=0.080_r8
      ns=51
      ihsig=1
      nt1=101
      imom=8
      jmom=8
c
      icur_psi=0
      xbrki=0.9_r8
c
      iopt_pp=0
c
 10   continue
      ilmds=.FALSE.
      if(lxout(0)) then
         write(lnout(0),*) ' '
         write(lnout(0),*) ' geqxpl:  compute inverse equilibrium from'
         write(lnout(0),*)
     >      '          EFIT G-EQDISK or MDSplus tree data'
         write(lnout(0),*) 
     >      ' --> enter "i" or "I" for G-EQDISK access via "index file"'
         write(lnout(0),*) ' --> enter "m" or "M" for MDSplus access.'
         write(lnout(0),*) ' --> enter "q" or "Q" to quit.'
         write(lnout(0),*) ' '
      endif
 
      it_index=0
      zfile=' '
      call unixfile('geqxpl',zfile)
 
      if((zfile(1:2).eq.'q ').or.(zfile(1:2).eq.'Q ')) then
         call uexita(0)
         stop
      endif
 
      if((zfile(1:2).eq.'i ').or.(zfile(1:2).eq.'I ')) then
         call unixfile('geqxpl (G-EQDSK index file)',zfili)
         call geqdsk_index1_ntimes(99,'EFIT_INDEX:'//trim(zfili),inumt)
         if(inumt.eq.0) then
            call uermsg(' ?geqxpl: EFIT_INDEX file access failed:',
     >         trim(zfili),0)
            go to 10
         endif
         if(lxout(0)) then
            write(lnout(0),*) ' G-Eqdsk indices, times, and files:'
            do it=1,inumt
               call geqdsk_index1_gtime(99,'EFIT_INDEX:'//trim(zfili),
     >            it,ztime,ierr)
               call geqdsk_index1_gfile(99,'EFIT_INDEX:'//trim(zfili),
     >            it,zfile,ierr)
               write(lnout(0),1998) it,ztime,trim(zfile)
            enddo
 1998       format(2x,i5,'.  time=',1pe12.5,'  G-file=',a)
         endif
         it=1
         call ureadi('geqxpl: choose index to select G-file:$',it)
         it_index=max(1,min(inumt,it))
         call geqdsk_index1_gfile(99,'EFIT_INDEX:'//trim(zfili),
     >      it_index,zfilsave,ierr)
c
         write(zfile,1999) trim(zfili),it
 1999    format('EFIT_INDEX:',a,'(',i5,')')
c
      else if((zfile(1:2).eq.'m ').or.(zfile(1:2).eq.'M ')) then
 20      continue
         ilsrv=len_trim(mds_server)
         iltr=len_trim(mds_tree)
         iltm=len_trim(mds_time)
         ilsh=len_trim(mds_shot)
         if(lxout(0)) write(lnout(0),2001) mds_server(1:ilsrv),
     >      mds_tree(1:iltr),mds_time(1:iltm),mds_shot(1:ilsh),ireduce
 2001    format(
     >      ' geqxpl MDSplus options:'/
     >      ' "S" -- set MDSplus server name (currently:  "',a,'")'/
     >      ' "T" -- set MDSplus tree name   (currently:  "',a,'")'/
     >      ' "M" -- set time of interest    (currently:  "',a,'")'/
     >      ' "N" -- set shot number         (currently:  "',a,'")'/
     >      ' "1" -- to fetch nearest time   (currently:  "',L1,'")'/
     >      ' "A" -- to accept current setting, read tree...'/
     >      ' "Q" -- to quit menu without reading.')
         call uread('geqxpl:  enter MDSplus option:$')
         if(lchar('Q')) then
            go to 10
         else if(lchar('A')) then
            if(ireduce) then
               write(zfile,3001) mds_server(1:ilsrv),
     >            mds_tree(1:iltr),mds_shot(1:ilsh),mds_time(1:iltm)
            else
               write(zfile,3002) mds_server(1:ilsrv),
     >            mds_tree(1:iltr),mds_shot(1:ilsh),mds_time(1:iltm)
            endif
 3001       format('MDS+/REDUCE:',a,':',a,'(',a,';t=',a,')')
 3002       format('MDS+/NOREDUCE:',a,':',a,'(',a,';t=',a,')')
            write(lnout(0),*) '...MDSplus access string: ',zfile
            go to 50
         else if(lchar('S')) then
            call ureadw('geqxpl:  MDSplus server name or "LOCAL":$',
     >         mds_server)
         else if(lchar('T')) then
            call ureadw('geqxpl:  MDSplus tree name:$',mds_tree)
         else if(lchar('M')) then
            call ureadw('geqxpl:  EFIT data time of interest:$',
     >         mds_time)
            iltm=len_trim(mds_time)
            ctest=' '
            ilc=len(ctest)
            ctest(max(1,ilc-iltm+1):ilc)=mds_time
            read(ctest,'(G20.0)',iostat=ier) ztest
            if(ier.ne.0) then
               call uermsg(' ?geqxpl:  G20.0 decode test failed:  ',
     >            mds_time,0)
               mds_time='1.0'
            endif
         else if(lchar('N')) then
            call ureadw('geqxpl:  shot number of MDSplus EFIT data:$',
     >         mds_shot)
            iltm=len_trim(mds_shot)
            ctest=' '
            ilc=len(ctest)
            ctest(max(1,ilc-iltm+1):ilc)=mds_shot
            read(ctest,'(I20)',iostat=ier) itest
            if(ier.ne.0) then
               call uermsg(' ?geqxpl:  I20 decode test failed:  ',
     >            mds_shot,0)
               mds_shot='123456'
            endif
         else if(lchar('1')) then
            if(lxout(0)) write(lnout(0),*) 
     >           '  enter "N" to read all times & interpolate EFIT data'
            if(lxout(0)) write(lnout(0),*) '  to selected time.'
            call uread('geqxpl: enter "Y" to just read one timeslice:$')
            if(lyes(0)) then
               ireduce=.TRUE.
            else
               ireduce=.FALSE.
            endif
         else
            call uermsg(' ?geqxpl: option code not understood.',' ',0)
         endif
         go to 20
      endif
 
 50   continue
 
      if(lxout(0)) then
         write(lnout(0),*) ' '
         write(lnout(0),*) ' settings now in effect:'
         write(lnout(0),*) ' '
         write(lnout(0),*) ' (R,Z) box computation option:'
         if(irz.eq.1) then
            write(lnout(0),*) '   ->> yes, compute B(R,Z) components'
            write(lnout(0),*) '   ->> and R(rho,theta),Z(rho,theta)'
         else
            write(lnout(0),*)
     >         '   --> no, just compute R(rho,theta),Z(rho,theta)'
         endif
         write(lnout(0),*) ' '
         write(lnout(0),*) ' Poloidal angle (theta) definition option:'
         if(nt1.lt.0) then
            write(lnout(0),*) '   ->> Equal Arc.'
         endif

         write(lnout(0),*) ' '
         write(lnout(0),
     >        '(2x,"Jacobian variation min/max limit: ",1pe13.6)') 
     >        maxvar
         write(lnout(0),*) ' '

         write(lnout(0),*) ' Toroidal current & q profile options:'
         if(icur_psi.eq.1) then
            write(lnout(0),*) "   ->> use Psi(R,Z) to rederive "//
     >           "q(rho) & toroidal current;"
            write(lnout(0),*) "       Psi(R,Z) must be based on a "//
     >           "full free boundary GS solution."
            write(lnout(0),*) "       EFIT file data for q(psi) and"//
     >           " total current are discarded."
         else
            write(lnout(0),*) '   ->> EFIT q(psi) data used "as is".'
            write(lnout(0),*) '       EFIT total enclosed current '//
     >           'value from file is used.'
         endif
         write(lnout(0),*)    "       Current profile from Psi(R,Z)"//
     >        " is modified with parabolic"
         write(lnout(0),*)    "       interpolation to match "//
     >        "boundary value;"
            write(lnout(0),'(A,1pe12.5)')
     >           "        parabolic interpolation starts at rho=",xbrki

         write(lnout(0),*) ' '
         if(iopt_pp.eq.1) then
            write(lnout(0),*)
     >           ' Pressure recomputed from P(a) & dP/dPsi.'
         else
            write(lnout(0),*) ' Pressure is not recomputed.'
         endif

         write(lnout(0),*) ' '
         write(lnout(0),*) ' no. of flux surfaces:  ',ns
         write(lnout(0),*) ' no. of theta pts / surface:  ', abs(nt1)
         write(lnout(0),*) ' no. of Fourier moments (regularization):',
     >      '  ',jmom
         write(lnout(0),*) ' '
         write(lnout(0),*) ' delta_psi(bdy) reduction parameter:', fbdy
         write(lnout(0),*) ' minimum curvature parameter:',crat
         write(lnout(0),*) ' '
         if(ihsig.lt.0) then
            write(lnout(0),*) ' Internal representation: Hermite'
         else
            write(lnout(0),*) ' Internal representation: Spline'
         endif
         write(lnout(0),*) ' '
         write(lnout(0),2002)
 2002    format(' options:'/
     >      '  "Q" - quit, get different data.'/
     >      '  "R" - include psi(R,Z), B(R,Z) in xplasma calculation.'/
     >      '  "X" - just do R,Z vs. mag. coordinates in xplasma.'/
     >      '  "I" - enclosed toroidal current profile option.'/
     >      '  "P" - option to recompute P(rho) from dP/dpsi.'/
     >      '  "N" - select number of flux surfaces.'/
     >      '  "M" - select number of theta pts / flux surface.'/
     >      '  "H" - (new) select Hermite internal representation ',
     >               'for R,Z surfaces.'/
     >      '  "K" - select no. of Fourier moments ',
     >               '(surface theta regularization).'/
     >      '  "F" - set delta_psi(bdy) reduction parameter.'/
     >      '  "C" - set minimum curvature parameter.'/
     >      '  "E" - to select equal arc theta definition'/
     >      '  "V" - reset Jacobian min/max limit'/
     >      '  "B" - plot raw EFIT data, no "xplasma" processing'/
     >      '  "A" - accept parameters, go ahead...')
      endif
      call uread('geqxpl:  data processing control option:$')
 
      if(lchar('Q')) then
         go to 10

      else if(lchar('B')) then
         call geqxpl_rawplot(zfile)
 
      else if(lchar('R')) then
         irz=1
 
      else if(lchar('X')) then
         irz=0
 
      else if(lchar('E')) then
         nt1=-abs(nt1)
         jmom=max(16,imom)

      else if(lchar('T')) then
         nt1=abs(nt1)
         jmom=imom

      else if(lchar('A')) then
         go to 100
 
      else if(lchar('H')) then
         call uread(' enter "Y" for Hermite internal representation:')
         if(lyes(0)) then
            ihsig=-1
         else
            ihsig=1
         endif

      else if(lchar('I')) then
         call uread(
     >        ' enter "Y" to rederive q and I profiles from Psi(R,Z):')
         if(lyes(0)) then
            icur_psi=1
         else
            icur_psi=0
         endif

         if(lxout(0)) then
            write(lnout(0),*) ' for the encloded toroidal current I:'
            write(lnout(0),*) 
     >           ' parabolic interpolation to match edge value can'
            write(lnout(0),*) 
     >           ' start in range 0.75 <= rhobrk <= 0.95'
         endif
         call ureadr8(
     >        ' enter "rhobrk" value at which to break profile'//
     >        ' for interpolation:',xbrki)
         xbrki=max(0.75_r8,min(0.95_r8,xbrki))

      else if(lchar('P')) then
         call uread(' enter "Y" to recompute P(rho) using dP/dPsi:')
         if(lyes(0)) then
            iopt_pp=1
         else
            iopt_pp=0
         endif

      else if(lchar('N')) then
         call ureadi(
     >      'geqxpl:  enter no. of flux surfaces, incl mag. axis:$',ns)
         ns=max(4,min(1000,ns))
 
      else if(lchar('M')) then
         it1=abs(nt1)
         call ureadi('geqxpl:  enter no. of theta points:$',it1)
         it1=max(50,min(2000,it1))

         if(nt1.lt.0) then
            nt1 = -it1
         else
            nt1 = it1
         endif
 
      else if(lchar('K')) then
         if(lxout(0)) write(lnout(0),*)
     >        '#moments>=16 forced for equal arc theta parametrization.'
         call ureadi('geqxpl:  enter no. of moments (btw 5 and 64):$',
     >      imom)
         imom=min(64,max(5,imom))
         if(nt1.gt.0) then
            jmom=imom
         else
            jmom=max(16,imom)
         endif

      else if(lchar('F')) then
 
         if(lxout(0)) then
            write(lnout(0),*) ' '
            write(lnout(0),*) ' ...setting this parameter less than one'
            write(lnout(0),*) ' moves the boundary in from the EFIT'
            write(lnout(0),*) ' nominal bdy, reducing the enclosed '
            write(lnout(0),*) ' poloidal flux by the indicated amount.'
            write(lnout(0),*) ' -->recommended value: 1.00'
         endif
         call ureadr8(
     >      'geqxpl:  delta_psi(bdy) parameter (btw 0.95 and 1.0):$',
     >      fbdy)
 
         fbmin=0.95_r8
         fbmax=1.00_r8
         fbdy=max(fbmin,min(fbmax,fbdy))
 
      else if(lchar('C')) then
 
         if(lxout(0)) then
            write(lnout(0),*) ' '
            write(lnout(0),*) ' ...setting this parameter greater than'
            write(lnout(0),*) ' zero establishes a minimum allowed'
            write(lnout(0),*) ' contour curvature, relative to the'
            write(lnout(0),*) ' midplane half width of the flux'
            write(lnout(0),*) ' surface.  This is a way of avoiding a'
            write(lnout(0),*) ' "kinked" boundary contour at a'
            write(lnout(0),*) ' separatrix.  If the condition is'
            write(lnout(0),*) ' violated the software reduces the'
            write(lnout(0),*) ' enclosed poloidal flux slightly.'
            write(lnout(0),*) ' -->recommended value: 0.05'
         endif
         call ureadr8(
     >      'geqxpl:  minimum curvature parameter (btw 0.0 and 0.15):$',
     >      crat)
 
         crat=max(0.0_r8,min(0.15_r8,crat))
 
      else if(lchar('V')) then
         if(lxout(0)) then
            write(lnout(0),*) 
     >           ' reduce this parameter to allow greater variation'
            write(lnout(0),*) 
     >           ' of det(J) on each flux surface (ajac_maxvar).'
            write(lnout(0),*) 
     >           ' allowed range: 0.0000001 to 0.1; default 0.0001.'
         endif
         call ureadr8('geqxpl: ajac_maxvar: ',maxvar)
         if(maxvar.gt.0.1d0) maxvar = 0.1d0
         if(maxvar.lt.0.0000001d0) maxvar = 0.0000001d0

      else
         call uermsg('?geqxpl: invalid control option.',' ',0)
      endif
      go to 50
 
c  OK...
 
 100  continue
 
      if(it_index.eq.0) then
         zfili=zfile
      else
         zfili=zfilsave
      endif

      ilbl=len(label)
      ifin=len_trim(zfili)
      do i=ifin,1,-1
         ist=i
         if((zfili(i:i).eq.'/').or.(zfili(i:i).eq.':').or.
     >      (zfili(i:i).eq.']')) then
            ist=i+1
            exit
         endif
      enddo
      label='GeqXpl:'//zfili(ist:min(ifin,ist+(ilbl-8)))
 
c-------------------------------------
 
      call xmoments_kmom_set(jmom)
 
      call eqi_jacheck_maxvar_Set(maxvar)

      if(zfile(1:4).eq.'MDS+') ilmds=.TRUE.
      call eqm_mds_fromgeqdsk_iadj(label,zfile,ns*ihsig,nt1,
     >     fbdy,crat,icur_psi, xbrki, irz,ier)
 
      if(iopt_pp.eq.1) call eqm_geqdsk_ppadj(ier)

c-------------------------------------
      if((ier.eq.0).and.(idebug.eq.1)) then
c
c  some tests -- consistency of q and toroidal flux representation
c
         allocate(zx(ns),zdphi(ns),zdpsi(ns),zqprof(ns),zq(ns))
c
         call eq_ganum("__RHO",id_rho)
         call eq_grid(id_rho,zx,ns,igot,ierr)
         if(ierr.ne.0) then
            write(6,*) ' eq_grid ("__RHO") failure.'
         else
            call eq_gfnum("PSI",id_psi)
            call eq_gfnum("PHITOR",id_phi)
         endif
c
c  get dpsi/drho and dphi/drho
c
         if(min(id_psi,id_phi).gt.0) then
            call eq_rgetf(ns,zx,id_psi,1,zdpsi,ierr)
            call eq_rgetf(ns,zx,id_phi,1,zdphi,ierr)
c  q = d(phi)/d(psi)
            zqprof(2:ns)=zdphi(2:ns)/(z2pi*zdpsi(2:ns))
            zqprof(1)=zqprof(2)-(zqprof(3)-zqprof(2))/3
c
            call eqm_rhofun(2,id_rho,'Q_DPSIDPHI',zqprof,
     >         0,zdum,0,zdum,idum,ierr)
c
            call eq_gfnum("Q",id_q)
            if(id_q.gt.0) then
               call eq_rgetf(ns,zx,id_q,0,zq,ierr)
c  compare
               call r8_grafx2(zx,zq,zqprof,ns,'rho','q',
     >            'data: '//zfile(1:ifin),
     >            'EFIT q (solid)',
     >            'numerical d(tor.flux)/d(pol.flux) (dashed)')
               zxmin=1.0d-7*zx(ns)
c
c  do integral B.dA to compare flux functions
c
               call eq_flxint_init(1,ns,zx,zxmin,ierr)
               allocate(zdv(ns-1),zr2i(ns-1),zg(ns),ztflux(ns),
     >            ztfint(ns))
c
               call eq_flxint('dVol',1,zdv,1,ns-1,ierr) ! vol. elements
               call eq_flxint('<1/R^2>',1,zr2i,1,ns-1,ierr) ! <1/R**2>
c
c   dphi = g.dVol.<1/R**2>/2pi
c
               call eq_gfnum("G",id_g)
               call eq_rgetf(ns,zx,id_g,0,zg,ierr)
               call eq_rgetf(ns,zx,id_phi,0,ztflux,ierr) !EFIT/xplasma flux fcn
               ztfint(1)=0.0
               do is=2,ns
                  i=is-1
                  zdtfl=zdv(i)*zr2i(i)*(zg(i)+zg(i))/(2*z2pi)
                  ztfint(is)=ztfint(i)+zdtfl
               enddo
c
               call eqm_rhofun(2,id_rho,'TFLUX_INT',ztfint,
     >            0,zdum,0,zdum,idum,ierr)
c
               call r8_grafx2(zx,ztflux,ztfint,ns,'rho','phitor',
     >            'data: '//zfile(1:ifin),
     >            'EFIT/xplasma Tor.flux (solid)',
     >            'numerically integrated Tor.flux (dashed)')
c
               deallocate(zdv,zr2i,zg,ztflux,ztfint)
            endif
         endif
         deallocate(zx,zdphi,zdpsi,zqprof,zq)
      endif
c
c-------------------------------------
 
      if(ier.ne.0) then
         write(lnout(0),*) ' ?eqm_fromgeqdsk error, ier=',ier
#ifdef __DEBUG
         call eqdbg_plot
#endif
         go to 10
      else
         call eqm_geq_maxGSerr(zmaxerr)
         write(6,*) ' '
         write(6,*) ' G-EQdsk data accepted, maximum rel. GS error: ',
     >      zmaxerr
         write(6,*) ' '
         call eq_bchk_sign(isnccwb,isnccwi)
         if(isnccwb.eq.1) then
            write(6,*) ' Bphi is counter-clockwise viewed from above ',
     >         '(nsnccwb = 1)'
         else
            write(6,*) ' Bphi is clockwise viewed from above ',
     >         '(nsnccwb = -1)'
         endif
         write(6,*) ' '
         if(isnccwi.eq.1) then
            write(6,*) ' Jphi is counter-clockwise viewed from above ',
     >         '(nsnccwi = 1)'
         else
            write(6,*) ' Jphi is clockwise viewed from above ',
     >         '(nsnccwi = -1)'
         endif
         write(6,*) ' '
         call eqdbg_plot
      endif
 
      if(ilmds) then
         call uread(
     >      'write G-EQDSK disk file of MDSplus timeslice? Y/N:$')
         if(lyes(0)) then
            call gfilnam('geqxpl(output)')
            call eqm_wr_geqdsk(zfile,ier)
            write(lnout(0),*)
     >         'eqm_wr_geqdsk completion code (0=normal):',ier
         endif
      endif
 
      if(lxout(0)) write(lnout(0),*) ' enter "E" for TORIC EQUIGS file.'
      if(lxout(0)) write(lnout(0),*) ' enter "X" for xplasma save file.'

      call uread('geqxpl:  write psipqgRZ NetCDF file? (Y/N):$')

      if(lchar('X')) then
         call gfilnam('geqxpl(xplasma_netcdf_file)')
         call eq_save(zfile,ier)
         write(lnout(0),*) ' eq_save exit status (0=OK): ',ier

      else if(lchar('E')) then
         call gfilnam('TORIC EQUIGS filename')
         call eq_gfnum('q',id_q)

         !  call equigs_write with idi_xpsi = 0; leave x_psi generation
         !  to equigs_write.f90

         call equigs_write(0,id_q,lunio,zfile,ier)
         write(lnout(0),*) 
     >        'equigs_write completion code (0=normal):',ier

      else if(lyes(0)) then
         call gfilnam('psipqgRZ filename')
         call eq_gfnum('p',id_p)
         call eq_gfnum('q',id_q)
         ixtra=0
         call eq_psipqgrz(zfile,ixtra,id_p,id_q,ier)
         write(lnout(0),*)
     >      'eq_psipqgrz completion code (0=normal):',ier
      endif
 
      go to 10
 
      contains
 
        subroutine gfilnam(unixhint)
 
        character*(*), intent(in) :: unixhint
 
        zfile=' '

        call unixfile(unixhint,zfile)
        end subroutine gfilnam
 
      end
