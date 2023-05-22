program test_xplasma
!
!  read ascii form TRANSP equilibrium file & populate xplasma module
!  via xplasma api; then call plot test program
!
  use xplasma_obj_instance
  implicit NONE
!
  character*10 runid
!
! TRANSP data scalars
!
  real :: pcur,tflux,time,tavg
!
! grid sizes
!
  integer :: ntheta,nsurf
  integer, parameter :: nRZ = 100
!
! REAL arrays -- from TRANSP data (single precision)
!
  real, dimension(:), allocatable :: theta,rho,psi,pmhd,qmhd,gmhd
  real, dimension(:,:), allocatable :: R,Z
!
! REAL*8 arrays -- for xplasma
!
  real*8, dimension(:), allocatable :: theta_r8,rho_r8,psi_r8, &
       pmhd_r8,qmhd_r8,gmhd_r8
  real*8, dimension(:,:), allocatable :: R_r8,Z_r8
!
! plasma extrema
!
  real*8 zrhomax,zRmin,zRmax,zZmin,zZmax,zincr
  real*8 :: zRbdy_test(5),zZbdy_test(5),thbdy_test(4)
!
! even spacing tolerance for xplasma grids
!
  real*8 ztol,ztime
!
! grid ids
!
  integer id_rho,id_chi,id_Rgrid,id_Zgrid
!
! fcn ids
!
  integer id_psi,id_pmhd,id_qmhd,id_gmhd,id_R,id_Z
!
  integer, parameter :: ibc0 = 0  ! "not a knot" bdy condition code
  integer, parameter :: ibc1 = 1  ! "specify 1st derivative" bdy cond. code
!
  integer, parameter :: ispline = 2 ! code for "spline" interpolation
  integer, parameter :: iakherm = 1 ! code for "Akima Hermite" interpolation
!
  integer idir,isnccw               ! Ip direction, 1 = CCW
  integer bdir,bsnccw               ! Bt direction, 1 = CCW
  real*8 distlim                    ! distance, plasma edge -> limiter
  real*8 scrape_len                 ! scrapeoff length = max(0.01,0.5*distlim)
!
! misc...
!
  real*8 zr,zz,zphi,zrho,zth
  integer iregion
!
!  CCW/CW label
!
  character(3), dimension(2), parameter :: ccw = (/"CCW", " CW"/)
!
  logical :: lxout

!------------------------------
  external eqm_brz_adhoc     ! BR,BZ extrapolator (used in absence of data)
!------------------------------
  integer ilr
  integer lun
  integer ierr,iermax,istat,itest
  integer iordrz
  real*8 zdum,zsm_edge
  real ztime1,ztime2
!
  integer :: ido_test = 1
!---------------------------------
! show runs in current working directory
!
  call execute_command_line('ls *.treq_data',exitstat=istat)
!
! prompt for runid
!
  call uinitg("TEST_XPLASMA")
!
  idir=1
  bdir=1
  distlim=0.01d0
  scrape_len=0.01d0
  zsm_edge=0.0d0
!
  call xoi_init(ierr)
  if(ierr.ne.0) then
     write(6,*) ' ? xplasma_obj_instance initialization failure.'
     call ps_bad_exit
  endif
  
  do
     if(lxout(0)) then
        write(6,*) '%test_xplasma program.  enter TRANSP runid, or...'
        write(6,*) ' "Q" to quit'
        write(6,*) ' "I" to change direction of plasma current (',ccw(idir),')'
        write(6,*) ' "B" to change direction of toroidal field (',ccw(bdir),')'
        write(6,*) ' "X" to toggle "extra debugging" switch (',(ido_test.eq.2),')'
        write(6,*) ' "D" to set distance (m), plasma edge to limiter (',distlim,')'
        write(6,*) ' "E" to set edge smoothing width (m) for f(R,Z) (',zsm_edge,')'
        write(6,*) '     (P(R,Z) and field components B(R,Z) will be computed).'
     endif
     call ureadw('enter TRANSP runid nnnnnAmm (.treq_data file must exist):$',&
          runid)
     if(runid.eq.'Q ') then
        call uexita(0)
        stop
     else if(runid.eq.'I ') then
        idir=3-idir
        cycle
     else if(runid.eq.'B ') then
        bdir=3-bdir
        cycle
     else if(runid.eq.'X ') then
        ido_test=3-ido_test
        cycle
     else if(runid.eq.'D ') then
        call ureadr8('enter distance (m), plasma edge to limiter:$',distlim)
        distlim=max(0.0d0,distlim)
        scrape_len=max(0.01d0,0.5d0*distlim)
        cycle
     else if(runid.eq.'E ') then
        call ureadr8('enter edge smoothing width (m), btw 0.0 and 0.2:$', &
             zsm_edge)
        zsm_edge=max(0.0d0,min(0.2d0,zsm_edge))
        cycle
     endif
!
     call ulower(runid)
     ilr=len_trim(runid)
!
     lun=99
     open(lun,file=runid(1:ilr)//'.treq_data',status='old',iostat=istat)
     if(istat.ne.0) then
        write(6,*) ' %test_xplasma:  file open error:  ',runid(1:ilr), &
             '.treq_data'
        cycle
     endif
!
!  read header info
!
     call rdtreq_hdr(lun,runid,time,tavg,pcur,tflux,ntheta,nsurf)
     write(6,*) ' rdtreq_hdr call completed OK.'
     write(6,*) '   runid = ',runid
     write(6,*) '    time = ',time
     write(6,*) '    tavg = ',tavg
     write(6,*) '    pcur = ',pcur
     write(6,*) '   tflux = ',tflux
     write(6,*) '  ntheta = ',ntheta
     write(6,*) '   nsurf = ',nsurf
!
!  as per header info, allocate arrays
!
     allocate(theta(ntheta))
     allocate(rho(nsurf))
     allocate(psi(nsurf))
     allocate(pmhd(nsurf))
     allocate(qmhd(nsurf))
     allocate(gmhd(nsurf))
     allocate(R(ntheta,nsurf))
     allocate(Z(ntheta,nsurf))
!
     allocate(theta_r8(ntheta))
     allocate(rho_r8(nsurf))
     allocate(psi_r8(nsurf))
     allocate(pmhd_r8(nsurf))
     allocate(qmhd_r8(nsurf))
     allocate(gmhd_r8(nsurf))
     allocate(R_r8(ntheta,nsurf))
     allocate(Z_r8(ntheta,nsurf))
!
!  read equilibrium data
!
     call rdtreq_data(lun,ntheta,nsurf,theta,rho,psi,pmhd,qmhd,gmhd,R,Z)
     write(6,*) ' rdtreq_data call completed OK.'
!
!-------------------> time xplasma operations...
!
     call cpu_time(ztime1)
!
     theta_r8 = theta
     rho_r8   = rho
     psi_r8   = psi
     pmhd_r8  = pmhd
     qmhd_r8  = qmhd
     gmhd_r8  = gmhd
     R_r8     = R
     Z_r8     = Z
!
     zrhomax=rho(nsurf)
!
!  OK, now we load xplasma...
!
     call eqm_select('test_xplasma '//trim(runid),1)

     ztime=time
     call eqm_time(ztime)
!
     ztol=1.0d-4
!
!  rho grid
     call eqm_rho(rho_r8,nsurf,ztol,id_rho,ierr)
     if(ierr.ne.0) then
        write(6,*) ' ?eqm_rho:  ierr=',ierr
        go to 100
     endif
!
!  theta (or chi) grid
     call eqm_chi(theta_r8,0,ntheta,ztol,id_chi,ierr)
     if(ierr.ne.0) then
        write(6,*) ' ?eqm_chi:  ierr=',ierr
        go to 100
     endif
!
!  1d profiles
!
     iermax=0
!
     do itest = 1,ido_test
        ! pass through twice; delete & replace everything; test
        ! xplasma garbage collection..

        !  spline, 1st deriv = 0 on axis, not-a-knot at edge

        call eqm_rhofun(ispline,id_rho,'PSI',psi_r8,ibc1,0.0d0,ibc0,zdum, &
             id_psi,ierr)

        iermax=max(iermax,ierr)
        if(iermax.eq.0) then
           call eqm_flabel(id_psi,'Poloidal Flux','Wb/rad',ierr)
           iermax=max(iermax,ierr)
        endif

        !  Akima Hermite, 1st deriv = 0 on axis, fit at edge

        call eqm_rhofun(iakherm,id_rho,'P',pmhd_r8,ibc1,0.0d0,ibc0,zdum, &
             id_pmhd,ierr)
        iermax=max(iermax,ierr)
        if(iermax.eq.0) then
           call eqm_flabel(id_pmhd,'Equilibrium Pressure','Pascals',ierr)
           iermax=max(iermax,ierr)
        endif

        !  spline, 1st deriv = 0 on axis, 1st deriv = 0 at edge

        call eqm_rhofun(ispline,id_rho,'G',gmhd_r8,ibc1,0.0d0,ibc1,0.0d0, &
             id_gmhd,ierr)
        iermax=max(iermax,ierr)
        if(iermax.eq.0) then
           call eqm_flabel(id_gmhd,'B_phi*R','T*m',ierr)
           iermax=max(iermax,ierr)
        endif

        !  spline, not-a-knot at axis and at edge

        call eqm_rhofun(ispline,id_rho,'Q',qmhd_r8,ibc0,zdum,ibc0,zdum, &
             id_qmhd,ierr)
        iermax=max(iermax,ierr)
        if(iermax.eq.0) then
           call eqm_flabel(id_qmhd,'Q profile','-',ierr)
           iermax=max(iermax,ierr)
        endif

        if(iermax.gt.0) then
           write(6,*) ' ?eqm_rhofun -- 1d interpolation setup errors!'
           go to 100
        endif

        !  R(theta,rho), Z(theta,rho)
        !  bicubic splines, periodic in theta, not-a-knot bdy conds in rho

        iermax=0

        call eqm_rzmag(R_R8,Z_R8,ntheta,nsurf,2,id_R,id_Z,ierr)

        call eqm_flabel(id_R,'R of flux surfaces','m',ierr)
        call eqm_flabel(id_Z,'Z of flux surfaces','m',ierr)

!  compute B field... note signs (clockwise / counter-clockwise)

        isnccw=3-2*idir          ! +1 -- CCW looking down on machine
        bsnccw=3-2*bdir          ! -1 -- CW looking down on machine

        if(itest.eq.1) call eqm_bset(bsnccw,isnccw)

        if(itest.lt.ido_test) then
           write(6,*) ' -> basic equilibrium set.'
           write(6,*) '    test: delete and reset equilibrium...'
           call xplasma_eqClear(s,.FALSE.,ierr)  ! delete & reset equilibrium
        endif

     enddo

     call cpu_time(ztime2)
     write(6,*) ' %test_xplasma:  R(theta,rho) & Z(theta,rho) splines done.'
     write(6,*) '  BR(theta,rho), BZ(theta,rho), mod(B)(theta,rho) done.'
     write(6,*) '  xplasma cpu time so far:  ',ztime2-ztime1

     if(ido_test.eq.2) then
        write(6,*) ' ... extra testing ... save, delete, restore from file...'

        call execute_command_line('rm -f foo.cdf',exitstat=ierr)
        call eq_save('foo.cdf',ierr)
        write(6,*) ' eq_save status: ierr = ',ierr

        call eq_save('foo.cdf',ierr)
        write(6,*) ' eq_restore status: ierr = ',ierr
        call execute_command_line('rm -f foo.cdf',exitstat=ierr)

        write(6,*) ' '
        write(6,*) ' test f77 interface to numeric integration... '
        call test_eq_flxint(ntheta,nsurf)

        write(6,*) ' '
        write(6,*) ' test coordinate transformation routines... '
        call test_eq_ctrans('BEFORE scrape off layer setup')
     endif

     !  set up limiter; first find plasma bdy min/max R,Z

     call eq_glimrz(zrhomax,zRmin,zRmax,zZmin,zZmax,ierr)
     if(ierr.ne.0) then
        write(6,*) ' ?eq_glimrz:  ierr = ',ierr
        go to 100
     else
        write(6,*) ' '
        write(6,*) ' ---------------------------------------------- '
        write(6,'(" Rmin,max: ",2(1x,1pe13.6)," Zmin,max: ",2(1x,1pe13.6))') &
             zRmin,zRmax,zZmin,zZmax
     endif

!  here I define the limiter as being a fixed distance from the
!  plasma, except at the inner wall, which is vertical & touches
!  the plasma boundary ... many other limiter configurations are
!  possible...
!
     zRbdy_test(1)=zRmin; zZbdy_test(1)=zZmin; thbdy_test(1)=0.0
     zRbdy_test(2)=zRmax; zZbdy_test(2)=zZmin; thbdy_test(2)=90.0
     zRbdy_test(3)=zRmax; zZbdy_test(3)=zZmax; thbdy_test(3)=0.0
     zRbdy_test(4)=zRmin; zZbdy_test(4)=zZmax; thbdy_test(4)=90.0
     zRbdy_test(5)=zRmin; zZbdy_test(5)=zZmin

!  test f77 limiter definition calls

     call eqm_tbdy(4,zRbdy_test(1:4),zZbdy_test(1:4),thbdy_test, &
          0,zRbdy_test,zZbdy_test,thbdy_test,ierr)  ! dummy args for circ. lims
     if(ierr.ne.0) then
        write(6,*) ' ?eqm_tbdy: ierr = ',ierr
        call xplasma_error(s,ierr,6)
        go to 100
     endif

     call eqm_cbdy(5,zRbdy_test,zZbdy_test,ierr)
     if(ierr.ne.0) then
        write(6,*) ' ?eqm_cbdy: ierr = ',ierr
        call xplasma_error(s,ierr,6)
        go to 100
     endif

     call eqm_dbdy(distlim,zRmin,zRmax+distlim,zZmin-distlim,zZmax+distlim, &
          ierr)
     if(ierr.ne.0) then
        write(6,*) ' ?eqm_dbdy: ierr = ',ierr
        call xplasma_error(s,ierr,6)
        go to 100
     endif

!  set up a limiter, and at the same time...
!  a 100x100 evenly spaced (R,Z) grid is set
!  up to cover both the plasma and the SOL between the plasma
!  and limiter

     call eqm_dbdy_grid(distlim, &
          zRmin,zRmax+distlim,zZmin-distlim,zZmax+distlim, &
          nRZ,nRZ,id_Rgrid,id_Zgrid,  &
          ierr)
     if(ierr.ne.0) then
        write(6,*) ' ?eqm_dbdy_grid:  ierr = ',ierr
        go to 100
     endif

     if(ido_test.eq.2) then
        call test_eq_ctrans('AFTER scrape off layer setup')
     endif

     if(ido_test.eq.2) then
        call test_misc
     endif

     call cpu_time(ztime2)
     write(6,*) ' %test_xplasma:  limiter & (R,Z) grid setup complete.'
     write(6,*) '  xplasma cpu time so far:  ',ztime2-ztime1

!  extrapolate pressure using a scrape-off length of 0.5*distlim
!  ...to the (R,Z) grid
!
     iordrz=1   ! Hermite
     call eqm_rzfun(id_pmhd,scrape_len,iordrz,zsm_edge,ierr)
     if(ierr.ne.0) then
        write(6,*) ' ?eqm_rzfun:  ierr = ',ierr
        go to 100
     endif
!
!  and extrapolate the field to R,Z grid
!  here, a stabilized vacuum field extrapolator is used.  If actual field
!  data were available it would be better to use it.
!
     call eqm_brz(eqm_brz_adhoc,zsm_edge,ierr)
     if(ierr.ne.0) then
        write(6,*) ' ?eqm_brz:  ierr = ',ierr
        go to 100
     endif

     if(ido_test.eq.2) then
        call test_eq_get
        call test_mcgrid
        call test_vtaub
     endif

     call cpu_time(ztime2)
     write(6,*) ' %test_xplasma:  B field extrapolation done.'
     write(6,*) '  xplasma cpu time so far:  ',ztime2-ztime1

!  all done -- call plotting routine
!
     call eqdbg_plot
!
100  continue
     deallocate(theta)
     deallocate(rho)
     deallocate(psi)
     deallocate(pmhd)
     deallocate(qmhd)
     deallocate(gmhd)
     deallocate(R)
     deallocate(Z)
!
     deallocate(theta_r8)
     deallocate(rho_r8)
     deallocate(psi_r8)
     deallocate(pmhd_r8)
     deallocate(qmhd_r8)
     deallocate(gmhd_r8)
     deallocate(R_r8)
     deallocate(Z_r8)
!
     close(unit=lun)
  end do
end program test_xplasma

subroutine test_eq_flxint(ntheta,nsurf)

  !  test legacy f77 numerical flux surface averaging & integration
  !  interfaces...

  use xplasma_obj_instance
  implicit NONE
  
  integer, intent(in) :: ntheta,nsurf   ! equilibrium grid sizes

  !-------------------------------------------
  integer :: ierr,i,j,idx11,id_B
  integer :: inth,inxi
  real*8 :: z1 = 1.0d0
  real*8 :: zarea,zvol,bavg,zrhomin
  real*8 :: x11(11),x4(4),t4(4),wk11(11),wk4(4),dvs4(4)
  real*8 :: bav10(10),dvol10(10),darea10(10)
  real*8 :: bav3(3),dvol3(3),darea3(3)
  real*8 :: bav2d(3,3),dvol2d(3,3),darea2d(3,3)
  real*8 :: dvd2d(3,4),bvs2d(3,4)
  real*8, parameter :: ONE = 1.0d0, C2PI = 6.2831853071795862D+00
  real*8 :: bmodf(ntheta,nsurf)
  !-------------------------------------------

  do i=1,size(x11)
     x11(i)=(i-1)*ONE/(size(x11)-1)
  enddo

  do i=1,size(x4)
     x4(i)=(i-1)*ONE/(size(x4)-1)
  enddo

  do i=1,size(t4)
     t4(i)=(i-1)*C2PI/(size(t4)-1)
  enddo
  
1001 format(a,1pe17.10)

  call eq_volume(1,z1,0,zvol,ierr); call ckerr('eq_volume')
  write(6,1001) ' eq_area: plasma enclosed volume (m^3): ',zvol

  call eq_area(1,z1,0,zarea,ierr); call ckerr('eq_area')
  write(6,1001) ' eq_area: plasma cross sectional area (m^2): ',zarea

  !---------------------------------------------
  ! some xplasma f90 based reference data

  call xplasma_author_set(s,'test_xplasma',ierr); call ckerr('author_set')

  call xplasma_create_integ(s,'test_integ11',x11,idx11,ierr)
  call ckerr('xplasma_create_integ')

  call xplasma_rho_zonint(s,idx11,'<B>',bav10,ierr,dvol_out=dvol10)
  call ckerr('xplasma_rho_zonint')

  call xplasma_rho_zonint(s,idx11,'dArea',darea10,ierr)
  call ckerr('xplasma_rho_zonint')

  zvol = 0
  zarea = 0
  bavg = 0
  do i=1,size(bav10)
     bavg = bavg + dvol10(i)*bav10(i)
     zvol = zvol + dvol10(i)
     zarea = zarea + darea10(i)
  enddo
  bavg = bavg/zvol

  write(6,1001) ' '
  write(6,1001) '   -> fortran-90 interface:'
  write(6,1001) ' integ11: <<B>> = ',bavg
  write(6,1001) ' integ11: summed volume = ',zvol
  write(6,1001) ' integ11: summed area   = ',zarea

  call xplasma_rho_zonint(s,idx11,'dVdrho',wk11,ierr)
  call ckerr('xplasma_rho_zonint')
  write(6,1001) ' integ11: dV/drho @bdy  = ',wk11(11)

  call xplasma_rho_zonint(s,idx11,'<B>S',wk11,ierr)
  call ckerr('xplasma_rho_zonint')
  write(6,1001) ' integ11: <B> @bdy      = ',wk11(11)

  !---------------------------------------------
  ! some f77 tests

  zrhomin = 1.0d-7

  call eq_flxint_init(0,4,x4,zrhomin,ierr); call ckerr('eq_flxint_init')
  call fluxav_nsurfs_get(inth,inxi)
  write(6,*) ' after eq_flxint_init: #th_zones = ',inth,'; #x_surfs = ',inxi

  call eq_flxint('dVol',1,dvol3,1,3,ierr); call ckerr('eq_flxint(dVol)')
  call eq_flxint('dArea',1,dArea3,1,3,ierr); call ckerr('eq_flxint(dArea)')
  call eq_flxint('<B>',1,bav3,1,3,ierr); call ckerr('eq_flxint(<B>)')

  zvol = 0
  zarea = 0
  bavg = 0
  do i=1,size(bav3)
     bavg = bavg + dvol3(i)*bav3(i)
     zvol = zvol + dvol3(i)
     zarea = zarea + darea3(i)
  enddo
  bavg = bavg/zvol

  write(6,1001) ' '
  write(6,1001) ' <<>> denotes volume average over entire plasma.'
  write(6,1001) ' '
  write(6,1001) '   -> fortran-77 legacy interface:'
  write(6,1001) ' integ77: <<B>> = ',bavg
  write(6,1001) ' integ77: summed volume = ',zvol
  write(6,1001) ' integ77: summed area   = ',zarea

  call eq_flxint('dVdrho',1,dvs4,1,4,ierr); call ckerr('eq_flxint(dVdrho)')
  write(6,1001) ' integ77: dV/drho @bdy  = ',dvs4(4)

  call eq_flxint('<B>S',1,wk4,1,4,ierr); call ckerr('eq_flxint(<B>S)')
  write(6,1001) ' integ77: <B> @bdy      = ',wk4(4)

  call eq_flxint_chinit(0,4,t4,ierr); call ckerr('eq_flxint_chinit')
  call fluxav_nsurfs_get(inth,inxi)
  write(6,*) ' after eq_flxint_chinit: #th_zones = ',inth,'; #x_surfs = ',inxi

  call eq_flxint('dVol',1,dvol2d,3,3,ierr); call ckerr('eq_flxint(dVol)')
  call eq_flxint('dArea',1,dArea2d,3,3,ierr); call ckerr('eq_flxint(dArea)')
  call eq_flxint('<B>',1,bav2d,3,3,ierr); call ckerr('eq_flxint(<B>)')

  write(6,1001)
  write(6,1001) '   -> fortran-77 legacy: 2d tests:'
  write(6,1001) ' integ2d: <B> zone(1,3): ',bav2d(1,3)
  write(6,1001) ' integ2d: <B> zone(2,3): ',bav2d(2,3)
  write(6,1001) ' integ2d: <B> zone(3,3): ',bav2d(3,3)

  zvol = 0
  zarea = 0
  bavg = 0
  do j=1,size(bav2d,2)
     do i=1,size(bav2d,1)
        bavg = bavg + dvol2d(i,j)*bav2d(i,j)
        zvol = zvol + dvol2d(i,j)
        zarea = zarea + darea2d(i,j)
     enddo
  enddo
  bavg = bavg/zvol

  write(6,1001) ' '
  write(6,1001) ' integ2d: <<B>> = ',bavg
  write(6,1001) ' integ2d: summed volume = ',zvol
  write(6,1001) ' integ2d: summed area   = ',zarea

  call eq_flxint('dVdrho',1,dvd2d,3,4,ierr); call ckerr('eq_flxint(dVdrho)')
  call eq_flxint('<B>S',1,bvs2d,3,4,ierr); call ckerr('eq_flxint(<B>S)')

  zvol=0
  bavg=0
  do i=1,3
     bavg=bavg+bvs2d(i,4)*dvd2d(i,4)
     zvol=zvol+dvd2d(i,4)
  enddo
  bavg=bavg/zvol

  write(6,1001) ' integ2d: dV/drho @bdy  = ',zvol
  write(6,1001) ' integ2d: <B> @bdy      = ',bavg

  call xplasma_find_item(s,'BMOD',id_B,ierr); call ckerr('find(BMOD)')
  call xplasma_eval_prof(s,id_B,bmodf,ierr); call ckerr('retrieve BMOD')

  call eq_flxint('dVol',1,dvol3,1,3,ierr); call ckerr('eq_flxint(dVol)')
  call eq_flxint_arr2(bmodf,4,bav3,1,3,ierr); call ckerr('eq_flxint_arr2 #1')

  zvol = 0
  bavg = 0
  do i=1,size(bav3)
     bavg = bavg + bav3(i)*dvol3(i)
     zvol = zvol + dvol3(i)
  enddo
  bavg = bavg/zvol
  write(6,1001) ' '
  write(6,1001) '   -> fortran-77 interface, "user" provided copy of BMOD:'
  write(6,1001) ' intarr2: <<BMOD>> = ',bavg
  write(6,1001) ' intarr2: summed volume = ',zvol

  call eq_flxint_arr2(bmodf,6,wk4,1,4,ierr); call ckerr('eq_flxint_arr2 #2')
  write(6,1001) ' intarr2: <BMOD> @bdy   = ',wk4(4)

  call eq_flxint_arr2(bmodf,4,bav2d,3,3,ierr); call ckerr('eq_flxint_arr2 #3')

  zvol = 0
  bavg = 0
  do j=1,size(bav2d,2)
     do i=1,size(bav2d,1)
        bavg = bavg + dvol2d(i,j)*bav2d(i,j)
        zvol = zvol + dvol2d(i,j)
     enddo
  enddo
  bavg = bavg/zvol

  write(6,*) '   -> from calculation of <BMOD> on 2d grid:'
  write(6,1001) ' intarr2: <<BMOD>> = ',bavg

  call xplasma_remove_item(s,idx11,ierr); call ckerr('remove integrator idx11')

  call xplasma_author_clear(s,'test_xplasma',ierr); call ckerr('author_clear')

  !---------------------------------------------

  contains
    subroutine ckerr(lbl)

      character*(*), intent(in) :: lbl

      if(ierr.ne.0) then
         write(6,*) '  ** test_eq_flxint.ckerr ** ierr = ',ierr,' in ',lbl
         call xplasma_error(s,ierr,6)
      endif
    end subroutine ckerr

end subroutine test_eq_flxint

subroutine test_eq_ctrans(str)

  !  test legacy f77 xplasma coordinate transform interfaces...

  use xplasma_obj_instance
  implicit NONE

  character*(*), intent(in) :: str

  !---------------------------------------------
  integer, parameter :: nvec=17
  real*8 :: r_in(nvec),phi_in(nvec),zcosphi,zsinphi
  real*8 :: x_out(nvec),y_out(nvec),r_out(nvec),phi_out(nvec)
  real*8 :: xi_in(nvec),th_in(nvec),z_out(nvec)
  real*8 :: xi_out(nvec),th_out(nvec)
  real*8 :: th_out2(nvec),d1(nvec),d2(nvec),wk(nvec)
  real*8 :: rwk(nvec),zwk(nvec),drz(2)
  real*8 :: zrzbuf(nvec,4)
  real*8 :: zr,zphi

  integer :: nregion(nvec),ierr

  real*8, parameter :: ONE = 1.0d0, C2PI = 6.2831853071795862D+00
  real*8, parameter :: ZERO = 0.0d0
  real*8, parameter :: ztol_rz = 1.0d-10

  integer :: i,iflist(4),idchis(4),id_R,id_Z
  !---------------------------------------------

  call xplasma_common_ids(s,ierr,id_R=id_R,id_Z=id_Z)
  if(ierr.ne.0) then
     write(6,*) ' ?xplasma_common_ids error in test_eq_ctrans.'
     return
  endif
  
  iflist(1)=id_R; iflist(2)=id_Z; idchis(1:2)=0  ! R & Z
  iflist(3)=id_R; iflist(4)=id_Z; idchis(3:4)=1  ! dR/dtheta & dZ/dtheta

  write(6,*) ' ============================== '
  write(6,*) '  ',trim(str)
  write(6,*) ' '

  do i=1,nvec
     r_in(i)=ONE
     wk(i)=ONE
     xi_in(i)=0.975d0
     phi_in(i)=(i-1)*C2PI/(nvec-1)
     th_in(i)=(i-1)*C2PI/(nvec-1)
  enddo

  call eq_rcylcs(1,ZERO,ZERO,zr,zphi,zcosphi,zsinphi)
  write(6,1001) zr,zphi,zcosphi,zsinphi
1001 format(" [0,0] -> R=",1pe14.7," phi=",1pe14.7/ &
          " cos & sin(phi) are:",2(1x,1pe14.7))

  write(6,*) ' .......... '
  write(6,*) ' in following, R_in=1.000:'
  write(6,*) ' #  phi_in         x_out          y_out          R_back         phi_back'

  call eq_xcyl(nvec,r_in,phi_in,x_out,y_out)
  call eq_rcyl(nvec,x_out,y_out,r_out,phi_out)

  do i=1,nvec
     write(6,1002) i,phi_in(i),x_out(i),y_out(i),r_out(i),phi_out(i)
  enddo

  write(6,*) ' .......... '
  write(6,*) ' distance test -- expect distance = 1.00; th match or +/- 2pi'
  write(6,*) ' #  th_in          d(rho=0.975)   th_out(0.975)  d(rho=1)      th_out(1)'

  call dtest(xi_in,th_out,d1)
  if(ierr.ne.0) return

  call dtest(wk,th_out2,d2)
  if(ierr.ne.0) return

  do i=1,nvec
     write(6,1002) i,th_in(i),d1(i),th_out(i),d2(i),th_out2(i)
  enddo

  write(6,*) ' .......... '
  write(6,*) ' inverse map test: in following, xi_in=0.975:'
  write(6,*) ' #  th_in          R_out          Z_out          xi_back        th_back'

  nregion = 0

  call eq_rzget(nvec,xi_in,th_in,phi_in,r_out,z_out,nregion,ierr)
  if(ierr.ne.0) then
     write(6,*) ' ?? error in rzget: ',ierr
  else
     call eq_inv(nvec,r_out,z_out,phi_in,ztol_rz,xi_out,th_out,nregion,ierr)
     if(ierr.ne.0) then
        write(6,*) ' ?? error in eq_inv: ',ierr
     else
        do i=1,nvec
           write(6,1002) i,th_in(i),R_out(i),Z_out(i),xi_out(i),th_out(i)
        enddo
        write(6,*) ' (nregion range: ',minval(nregion),maxval(nregion),')'
        write(6,*) ' '
     endif
  endif

1002 format(1x,i2,5(1x,1pe14.7))

  contains

    subroutine dtest(rhovec,th_out,d_out)

      real*8, dimension(:), intent(in) :: rhovec   ! surface vector
      ! (all elements equal)

      real*8, dimension(:), intent(out) :: th_out,d_out
      ! theta locations and distances -- nearest points on surface

      call xplasma_RZeval_2d(s,iflist, &
           xplasma_rho_coord,rhovec, xplasma_theta_coord,th_in, &
           zrzbuf,ierr, ideriv2s=idchis)
      if(ierr.ne.0) then
         write(6,*) ' ?error in xplasma_eval_prof: '
         call xplasma_error(s,ierr,6)
         return
      endif

      do i=1,nvec
         drz(1)=zrzbuf(i,4)
         drz(2)=-zrzbuf(i,3)
         drz = drz/sqrt(drz(1)**2+drz(2)**2)
         rwk(i)=zrzbuf(i,1)+drz(1)
         zwk(i)=zrzbuf(i,2)+drz(2)
      enddo
  
      call eq_bdfind(nvec,rwk,zwk,phi_in,rhovec(1),th_out,phi_out,d_out,ierr)
      if(ierr.ne.0) then
         write(6,*) ' ?error in eq_bdfind: '
         call xplasma_error(s,ierr,6)
         return
      endif

    end subroutine dtest

end subroutine test_eq_ctrans

subroutine test_misc

  use xplasma_obj_instance
  implicit NONE

  !-----------------
  real*8 :: ztime,zrho0,zrho1
  real*8 :: zRmaxlim,zRminlim,zZmaxlim,zZminlim,zRmid,zZmid,zdum,zdist,zdist2
  real*8 :: ra,za,phia,da,rb,zb,phib,db,tol,rx,zx,rx2,zx2,phix
  integer :: id,ierr,itype,istat,id_Bg
  character*32 zname
  real*8 :: zbdum(41),ztol
  !-----------------

  write(6,*) ' ---------------------- '
  write(6,*) ' test miscellaneous f77 xplasma legacy calls... '

  write(6,*) ' '
  call eq_time(ztime)
  write(6,*) '    eq_time yields time = ',ztime
  
  write(6,*) ' '
  call eq_rholim(zrho0,zrho1)
  write(6,*) '    eq_rholim gives: ',zrho0,zrho1
  call eq_rhoxlim(zrho0,zrho1)
  write(6,*) '    eq_rhoxlim gives: ',zrho0,zrho1
  call eq_rholima(zrho0,zrho1)
  write(6,*) '    eq_rholima gives: ',zrho0,zrho1

  write(6,*) ' '
  call eq_ganum('__RHO',id)
  write(6,*) '    "__RHO" id from eq_ganum: ',id
  call eq_get_aname(id,zname)
  write(6,*) '    eq_get_aname(id,...)=> "',trim(zname),'".'
  call eq_gfnum('R',id)
  write(6,*) '    "R" id from eq_ganum: ',id
  call eq_get_fname(id,zname)
  write(6,*) '    eq_get_fname(id,...)=> "',trim(zname),'".'
  
  write(6,*) ' '
  call eq_bdlims(itype,zRminlim,zRmaxlim,zZminlim,zZmaxlim,ierr)
  if(ierr.ne.0) then
     write(6,*) ' ?? eq_bdlims returned ierr=',ierr
     return
  else
     write(6,*) '    eq_bdlims: limiter type: ',itype
     write(6,*) '    limiter extrema: Rmin&max, Zmin&max: '
     write(6,'(10x,4(1pe12.5,1x))') zRminlim,zRmaxlim,zZminlim,zZmaxlim
  endif

  write(6,*) ' '
  zdum=0
  call eq_dfast(1,zRmaxlim,zZmaxlim,zdum,zdist,ierr)
  if(ierr.ne.0) then
     write(6,*) ' ?? eq_dfast returned ierr=',ierr
     return
  else
     write(6,*) '    distance (m) from plasma bdy to (Rmax,Zmax): ',zdist
  endif

  zRmid=(zRmaxlim+zRminlim)/2
  zZmid=(zZmaxlim+zZminlim)/2
  call eq_dfast(1,zRmid,zZmid,zdum,zdist2,ierr)

  zdum=sqrt((zRmaxlim-zRmid)**2+(zZmaxlim-zZmid)**2)

  ra=zRmid-(zdist2/2)*(zRmaxlim-zRmid)/zdum   ! move towards bdy from inside
  za=zZmid-(zdist2/2)*(zZmaxlim-zZmid)/zdum
  phia=0
  da=0

  rb=zRmaxlim-(zdist/2)*(zRmaxlim-zRmid)/zdum
  zb=zZmaxlim-(zdist/2)*(zZmaxlim-zZmid)/zdum
  phib=0
  db=0

  tol=1.0d-5
  call eq_fxcep(1,1,0, &
       ra,za,phia,da, rb,zb,phib,db, tol, &
       rx,zx,phix,istat, ierr)

  write(6,*) ' eq_fxcep istat, ierr: ',istat,ierr
  write(6,*) '   R,Z at bdy: ',rx,zx

  call eq_fxcep(1,2,0, &
       ra,za,phia,da, rb,zb,phib,db, tol, &
       rx2,zx2,phix,istat, ierr)

  write(6,*) ' eq_fxcep istat, ierr: ',istat,ierr
  write(6,*) '   R,Z at limiter: ',rx2,zx2
  write(6,*) '   (diff: ',sqrt((rx2-rx)**2+(zx2-zx)**2),')'
  write(6,*) ' '
  write(6,*) ' ---------------------- '
  write(6,*) ' '

  ztol=1.0d-5
  call eqm_bbin(zbdum,4,size(zbdum),ztol,id_Bg,ierr)

  write(6,*) ' eqm_bbin ierr, id_Bgrid = ',ierr,id_Bg
  if(ierr.eq.0) then
     call xplasma_grid(s,id_Bg,zbdum,ierr)
     write(6,*) '   Bgrid range: min=',zbdum(1),'; max=',zbdum(size(zbdum)), &
          ' Tesla.'
  endif
  write(6,*) ' '
  write(6,*) ' ---------------------- '
  write(6,*) ' '

end subroutine test_misc

subroutine test_eq_get

  implicit NONE

  integer, parameter :: ivec=8
  integer, parameter :: nlist=6

  real*8 :: zrho(ivec),zchi(ivec),zphi(ivec),zR(ivec),zZ(ivec)

  real*8 :: bvec1(3,ivec),gbtens1(3,3,ivec),bpol1(ivec,3)
  real*8 :: fvals1(ivec,nlist),fgrads1(3,ivec,nlist)

  real*8 :: bvec2(3,ivec),gbtens2(3,3,ivec),bpol2(ivec,3)
  real*8 :: fvals2(ivec,nlist),fgrads2(3,ivec,nlist)

  real*8 :: bvec3(3,ivec),gbtens3(3,3,ivec)
  real*8 :: fvals3(ivec,nlist),fgrads3(3,ivec,nlist)

  real*8 :: bvec4(3,ivec),gbtens4(3,3,ivec),bpol4(ivec,3)
  real*8 :: fvals4(ivec,nlist),fgrads4(3,ivec,nlist)

  real*8 :: bvec5(3,ivec),gbtens5(3,3,ivec)
  real*8 :: fvals5(ivec,nlist),fgrads5(3,ivec,nlist)

  integer :: nregion(ivec),ifcns(nlist)

  integer :: ii,ierr

  real*8, parameter :: ONE = 1.0d0, C2PI = 6.2831853071795862D+00
  real*8, parameter :: ZERO= 0.0d0
  real*8, parameter :: ztol= 1.0d-10
  real*8 :: zwk3a,zwk3b,zwk5a
  real*8 :: zbpol,zsum1,zsum2,zwkp(5)
  !------------------------

  zrho = 0.98d0
  do ii=1,ivec
     zchi(ii)=(ii-1)*C2PI/(ivec-1)
     zphi(ii)=ZERO
  enddo

  call eq_rzget(ivec,zrho,zchi,zphi,zR,zZ,nregion,ierr)
  if(ierr.ne.0) then
     write(6,*) ' ?? test_eq_get: preliminary eq_rzget call failed!'
     return
  endif

1002 format(1x,i2,5(1x,1pe14.7))

  write(6,*) ' .......... '
  write(6,*) ' eq*_get* test: coordinate points used:'
  write(6,*) ' #  x              theta          phi            R              Z'

  do ii=1,ivec
     write(6,1002) ii,zrho(ii),zchi(ii),zphi(ii),zR(ii),zZ(ii)
  enddo

  write(6,*) ' '
  write(6,*) ' eq*_get* tests follow.  B values should match approximately'
  write(6,*) ' (or precisely but only if the same underlying representation'
  write(6,*) '  is used).'
  write(6,*) ' '

  !----------------------

  call eq_gfnum('BR',ifcns(1))
  call eq_gfnum('BR_RZ',ifcns(2))

  call eq_gfnum('BZ',ifcns(3))
  call eq_gfnum('BZ_RZ',ifcns(4))

  call eq_gfnum('Bphi',ifcns(5))
  call eq_gfnum('Bphi_RZ',ifcns(6))

  !----------------------

  call eqrz_bffgrad(ivec,zR,zZ,zphi,ztol, &
       bvec1,gbtens1, &
       nlist,ifcns,ivec,fvals1,fgrads1,nregion,ierr)
  if(ierr.ne.0) then
     write(6,*) 'eqrz_bffgrad call error'
     return
  endif
  call bpolgen(bvec1,fvals1,bpol1)

  call eq_bffgrad(ivec,zrho,zchi,zphi,1, &
       bvec2,gbtens2, &
       nlist,ifcns,ivec,fvals2,fgrads2,nregion,ierr)
  if(ierr.ne.0) then
     write(6,*) 'eq_bffgrad call error (1)'
     return
  endif
  call bpolgen(bvec2,fvals2,bpol2)

  call eq_bffgrad(ivec,zrho,zchi,zphi,0, &
       bvec3,gbtens3, &
       nlist,ifcns,ivec,fvals3,fgrads3,nregion,ierr)
  if(ierr.ne.0) then
     write(6,*) 'eq_bffgrad call error (2)'
     return
  endif

  call eq_bffgrad(-ivec,zrho,zchi,zphi,1, &
       bvec4,gbtens4, &
       nlist,ifcns,ivec,fvals4,fgrads4,nregion,ierr)
  if(ierr.ne.0) then
     write(6,*) 'eq_bffgrad call error (3)'
     return
  endif
  call bpolgen(bvec4,fvals4,bpol4)

  call eq_bffgrad(-ivec,zrho,zchi,zphi,0, &
       bvec5,gbtens5, &
       nlist,ifcns,ivec,fvals5,fgrads5,nregion,ierr)
  if(ierr.ne.0) then
     write(6,*) 'eq_bffgrad call error (4)'
     return
  endif

  write(6,*) ' '
  write(6,*) ' #  BR(eqrz)       BR(eqrz:f)     BR(eqrz:frz)   BR(eq)         BR(eq:f)'

  do ii=1,ivec
     write(6,1002) ii,bvec1(1,ii),fvals1(ii,1),fvals1(ii,2),bvec2(1,ii),fvals2(ii,1)
  enddo

  write(6,*) ' '
  write(6,*) ' #  BR(eq:frz)     BR(-eq)        BR(-eq:f)      BR(-eq:frz)    dummy'

  do ii=1,ivec
     write(6,1002) ii,fvals2(ii,2),bvec4(1,ii),fvals4(ii,1),fvals4(ii,2),ZERO
  enddo

  write(6,*) ' '
  write(6,*) ' #  BZ(eqrz)       BZ(eqrz:f)     BZ(eqrz:frz)   BZ(eq)         BZ(eq:f)'

  do ii=1,ivec
     write(6,1002) ii,bvec1(2,ii),fvals1(ii,3),fvals1(ii,4),bvec2(2,ii),fvals2(ii,3)
  enddo

  write(6,*) ' '
  write(6,*) ' #  BZ(eq:frz)     BZ(-eq)        BZ(-eq:f)      BZ(-eq:frz)    dummy'

  do ii=1,ivec
     write(6,1002) ii,fvals2(ii,4),bvec4(2,ii),fvals4(ii,3),fvals4(ii,4),ZERO
  enddo

  write(6,*) ' '
  write(6,*) ' #  Bphi(eqrz)     Bphi(eqrz:f)   Bphi(eqrz:frz) Bphi(eq)       Bphi(eq:f)'

  do ii=1,ivec
     write(6,1002) ii,bvec1(3,ii),fvals1(ii,5),fvals1(ii,6),bvec2(3,ii),fvals2(ii,5)
  enddo

  write(6,*) ' '
  write(6,*) ' #  Bphi(eq:frz)   Bphi(-eq)      Bphi(-eq:f)    Bphi(-eq:frz)  dummy'

  do ii=1,ivec
     write(6,1002) ii,fvals2(ii,6),bvec4(3,ii),fvals4(ii,5),fvals4(ii,6),ZERO
  enddo

  write(6,*) ' '
  write(6,*) ' #  Bpol(eqrz)     Bpol(eqrz:f)   Bpol(eqrz:frz) Bpol(eq)       Bpol(eq:f)'

  do ii=1,ivec
     write(6,1002) ii,bpol1(ii,1),bpol1(ii,2),bpol1(ii,3),bpol2(ii,1),bpol2(ii,2)
  enddo

  write(6,*) ' '
  write(6,*) ' #  Bpol(eq:frz)   Bpol(-eq)      Bpol(-eq:f)    Bpol(-eq:frz)  dummy'

  do ii=1,ivec
     write(6,1002) ii,bpol2(ii,3),bpol4(ii,1),bpol4(ii,2),bpol4(ii,3),ZERO
  enddo

  write(6,*) ' '
  write(6,*) ' #  Bpol(eq0)      Bperp(eq0)     |Bpol(eq0:f)|  |Bpol(eq0:frz)|  dummy'

  do ii=1,ivec
     write(6,1002) ii,bvec3(2,ii),bvec3(1,ii), &
          sqrt(fvals3(ii,1)**2+fvals3(ii,3)**2), &
          sqrt(fvals3(ii,2)**2+fvals3(ii,4)**2), &
          ZERO
  enddo

  write(6,*) ' '
  write(6,*) ' #  Bpol(-eq0)     Bperp(-eq0)    |Bpol(-eq0:f)| |Bpol(-eq0:frz)| dummy'

  do ii=1,ivec
     write(6,1002) ii,bvec5(2,ii),bvec5(1,ii), &
          sqrt(fvals5(ii,1)**2+fvals5(ii,3)**2), &
          sqrt(fvals5(ii,2)**2+fvals5(ii,4)**2), &
          ZERO
  enddo

  write(6,*) ' '
  write(6,*) ' d(Bphi)/dR'
  write(6,*) ' #  eqrz           eqrz:f         eqrz:frz       eq             eq:f'

  do ii=1,ivec
     write(6,1002) ii,gbtens1(1,3,ii),fgrads1(1,ii,5),fgrads1(1,ii,6), &
          gbtens2(1,3,ii),fgrads2(1,ii,5)
  enddo

  write(6,*) ' '
  write(6,*) ' #  eq:frz         -eq            -eq:f          -eq:frz        dummy'

  do ii=1,ivec
     write(6,1002) ii,fgrads2(1,ii,6), &
          gbtens4(1,3,ii),fgrads4(1,ii,5),fgrads4(1,ii,6),ZERO
  enddo

  write(6,*) ' '
  write(6,*) ' d(Bphi)/dZ'
  write(6,*) ' #  eqrz           eqrz:f         eqrz:frz       eq             eq:f'

  do ii=1,ivec
     write(6,1002) ii,gbtens1(2,3,ii),fgrads1(2,ii,5),fgrads1(2,ii,6), &
          gbtens2(2,3,ii),fgrads2(2,ii,5)
  enddo

  write(6,*) ' '
  write(6,*) ' #  eq:frz         -eq            -eq:f          -eq:frz        dummy'

  do ii=1,ivec
     write(6,1002) ii,fgrads2(2,ii,6), &
          gbtens4(2,3,ii),fgrads4(2,ii,5),fgrads4(2,ii,6),ZERO
  enddo

  write(6,*) ' '
  write(6,*) ' d(Bphi)/dsperp'
  write(6,*) ' #  eq0            eq0:f          eq0:frz        -eq0           -eq0:f'

  do ii=1,ivec
     write(6,1002) ii,gbtens3(1,3,ii),fgrads3(1,ii,5),fgrads3(1,ii,6), &
          gbtens5(1,3,ii),fgrads5(1,ii,5)
  enddo

  write(6,*) ' '
  write(6,*) ' |grad(Bphi)|'
  write(6,*) ' #  eqrz           eqrz:f         eqrz:frz       eq             eq:f'

  do ii=1,ivec
     write(6,1002) ii,sqrt(gbtens1(1,3,ii)**2+gbtens1(2,3,ii)**2), &
          sqrt(fgrads1(1,ii,5)**2+fgrads1(2,ii,5)**2), &
          sqrt(fgrads1(1,ii,6)**2+fgrads1(2,ii,6)**2), &
          sqrt(gbtens2(1,3,ii)**2+gbtens2(2,3,ii)**2), &
          sqrt(fgrads2(1,ii,5)**2+fgrads2(2,ii,5)**2)
  enddo

  write(6,*) ' '
  write(6,*) ' #  eq:frz         -eq            -eq:f          -eq:frz        eq0'

  do ii=1,ivec
     write(6,1002) ii,sqrt(fgrads2(1,ii,6)**2+fgrads2(2,ii,6)**2), &
          sqrt(gbtens4(1,3,ii)**2+gbtens4(2,3,ii)**2), &
          sqrt(fgrads4(1,ii,5)**2+fgrads4(2,ii,5)**2), &
          sqrt(fgrads4(1,ii,6)**2+fgrads4(2,ii,6)**2), &
          sqrt(gbtens3(1,3,ii)**2+gbtens3(2,3,ii)**2)
  enddo

  write(6,*) ' '
  write(6,*) ' #  eq0:f          eq0:frz        -eq0           -eq0:f         -eq0:frz'

  do ii=1,ivec
     write(6,1002) ii,sqrt(fgrads3(1,ii,5)**2+fgrads3(2,ii,5)**2), &
          sqrt(fgrads3(1,ii,6)**2+fgrads3(2,ii,6)**2), &
          sqrt(gbtens5(1,3,ii)**2+gbtens5(2,3,ii)**2), &
          sqrt(fgrads5(1,ii,5)**2+fgrads5(2,ii,5)**2), &
          sqrt(fgrads5(1,ii,6)**2+fgrads5(2,ii,6)**2)
  enddo

  write(6,*) ' '
  write(6,*) ' d(BR)/dR'
  write(6,*) ' #  eqrz           eqrz:f         eqrz:frz       eq             eq:f'

  do ii=1,ivec
     write(6,1002) ii,gbtens1(1,1,ii),fgrads1(1,ii,1),fgrads1(1,ii,2), &
          gbtens2(1,1,ii),fgrads2(1,ii,1)
  enddo

  write(6,*) ' '
  write(6,*) ' #  eq:frz         -eq            -eq:f          -eq:frz        dummy'

  do ii=1,ivec
     write(6,1002) ii,fgrads2(1,ii,2), &
          gbtens4(1,1,ii),fgrads4(1,ii,1),fgrads4(1,ii,2),ZERO
  enddo

  write(6,*) ' '
  write(6,*) ' d(BR)/dZ'
  write(6,*) ' #  eqrz           eqrz:f         eqrz:frz       eq             eq:f'

  do ii=1,ivec
     write(6,1002) ii,gbtens1(2,1,ii),fgrads1(2,ii,1),fgrads1(2,ii,2), &
          gbtens2(2,1,ii),fgrads2(2,ii,1)
  enddo

  write(6,*) ' '
  write(6,*) ' #  eq:frz         -eq            -eq:f          -eq:frz        dummy'

  do ii=1,ivec
     write(6,1002) ii,fgrads2(2,ii,2), &
          gbtens4(2,1,ii),fgrads4(2,ii,1),fgrads4(2,ii,2),ZERO
  enddo

  write(6,*) ' '
  write(6,*) ' d(BZ)/dR'
  write(6,*) ' #  eqrz           eqrz:f         eqrz:frz       eq             eq:f'

  do ii=1,ivec
     write(6,1002) ii,gbtens1(1,2,ii),fgrads1(1,ii,3),fgrads1(1,ii,4), &
          gbtens2(1,2,ii),fgrads2(1,ii,3)
  enddo

  write(6,*) ' '
  write(6,*) ' #  eq:frz         -eq            -eq:f          -eq:frz        dummy'

  do ii=1,ivec
     write(6,1002) ii,fgrads2(1,ii,4), &
          gbtens4(1,2,ii),fgrads4(1,ii,3),fgrads4(1,ii,4),ZERO
  enddo

  write(6,*) ' '
  write(6,*) ' d(BZ)/dZ'
  write(6,*) ' #  eqrz           eqrz:f         eqrz:frz       eq             eq:f'

  do ii=1,ivec
     write(6,1002) ii,gbtens1(2,2,ii),fgrads1(2,ii,3),fgrads1(2,ii,4), &
          gbtens2(2,2,ii),fgrads2(2,ii,3)
  enddo

  write(6,*) ' '
  write(6,*) ' #  eq:frz         -eq            -eq:f          -eq:frz        dummy'

  do ii=1,ivec
     write(6,1002) ii,fgrads2(2,ii,4), &
          gbtens4(2,2,ii),fgrads4(2,ii,3),fgrads4(2,ii,4),ZERO
  enddo

  write(6,*) ' '
  write(6,*) ' d(Bpol)/dsperp'
  write(6,*) ' #  eq0            eq0:f          eq0:frz        -eq0           -eq0:f'

  do ii=1,ivec
     zwk3a=(fvals3(ii,1)*fgrads3(1,ii,1)+fvals3(ii,3)*fgrads3(1,ii,3))/ &
          bvec3(2,ii)
     zwk3b=(fvals3(ii,2)*fgrads3(1,ii,2)+fvals3(ii,4)*fgrads3(1,ii,4))/ &
          bvec3(2,ii)
     zwk5a=(fvals5(ii,1)*fgrads5(1,ii,1)+fvals5(ii,3)*fgrads5(1,ii,3))/ &
          bvec5(2,ii)
     write(6,1002) ii,gbtens3(1,2,ii),zwk3a,zwk3b,gbtens5(1,2,ii),zwk5a
  enddo

  write(6,*) ' '
  write(6,*) ' d(Bpol)/dspol'
  write(6,*) ' #  eq0            eq0:f          eq0:frz        -eq0           -eq0:f'

  do ii=1,ivec
     zwk3a=(fvals3(ii,1)*fgrads3(2,ii,1)+fvals3(ii,3)*fgrads3(2,ii,3))/ &
          bvec3(2,ii)
     zwk3b=(fvals3(ii,2)*fgrads3(2,ii,2)+fvals3(ii,4)*fgrads3(2,ii,4))/ &
          bvec3(2,ii)
     zwk5a=(fvals5(ii,1)*fgrads5(2,ii,1)+fvals5(ii,3)*fgrads5(2,ii,3))/ &
          bvec5(2,ii)
     write(6,1002) ii,gbtens3(2,2,ii),zwk3a,zwk3b,gbtens5(2,2,ii),zwk5a
  enddo

  write(6,*) ' '
  write(6,*) ' |grad(Bpol)|'
  write(6,*) ' #  eqrz           eqrz:f         eqrz:frz       eq             eq:f'

  do ii=1,ivec
     zbpol=sqrt(bvec1(1,ii)**2+bvec1(2,ii)**2)
     zwk3a=(bvec1(1,ii)*gbtens1(1,1,ii)+bvec1(2,ii)*gbtens1(1,2,ii))/zbpol
     zwk3b=(bvec1(1,ii)*gbtens1(2,1,ii)+bvec1(2,ii)*gbtens1(2,2,ii))/zbpol
     zwkp(1)=sqrt(zwk3a**2+zwk3b**2)

     zbpol=sqrt(fvals1(ii,1)**2+fvals1(ii,3)**2)
     zwk3a=(fvals1(ii,1)*fgrads1(1,ii,1)+fvals1(ii,3)*fgrads1(1,ii,3))/zbpol
     zwk3b=(fvals1(ii,1)*fgrads1(2,ii,1)+fvals1(ii,3)*fgrads1(2,ii,3))/zbpol
     zwkp(2)=sqrt(zwk3a**2+zwk3b**2)

     zbpol=sqrt(fvals1(ii,2)**2+fvals1(ii,4)**2)
     zwk3a=(fvals1(ii,2)*fgrads1(1,ii,2)+fvals1(ii,4)*fgrads1(1,ii,4))/zbpol
     zwk3b=(fvals1(ii,2)*fgrads1(2,ii,2)+fvals1(ii,4)*fgrads1(2,ii,4))/zbpol
     zwkp(3)=sqrt(zwk3a**2+zwk3b**2)

     zbpol=sqrt(bvec2(1,ii)**2+bvec2(2,ii)**2)
     zwk3a=(bvec2(1,ii)*gbtens2(1,1,ii)+bvec2(2,ii)*gbtens2(1,2,ii))/zbpol
     zwk3b=(bvec2(1,ii)*gbtens2(2,1,ii)+bvec2(2,ii)*gbtens2(2,2,ii))/zbpol
     zwkp(4)=sqrt(zwk3a**2+zwk3b**2)

     zbpol=sqrt(fvals2(ii,1)**2+fvals2(ii,3)**2)
     zwk3a=(fvals2(ii,1)*fgrads2(1,ii,1)+fvals2(ii,3)*fgrads2(1,ii,3))/zbpol
     zwk3b=(fvals2(ii,1)*fgrads2(2,ii,1)+fvals2(ii,3)*fgrads2(2,ii,3))/zbpol
     zwkp(5)=sqrt(zwk3a**2+zwk3b**2)

     write(6,1002) ii,zwkp(1:5)
  enddo

  write(6,*) ' '
  write(6,*) ' #  eq:frz         -eq            -eq:f          -eq:frz        eq0'

  do ii=1,ivec
     zbpol=sqrt(fvals2(ii,2)**2+fvals2(ii,4)**2)
     zwk3a=(fvals2(ii,2)*fgrads2(1,ii,2)+fvals2(ii,4)*fgrads2(1,ii,4))/zbpol
     zwk3b=(fvals2(ii,2)*fgrads2(2,ii,2)+fvals2(ii,4)*fgrads2(2,ii,4))/zbpol
     zwkp(1)=sqrt(zwk3a**2+zwk3b**2)

     zbpol=sqrt(bvec4(1,ii)**2+bvec4(2,ii)**2)
     zwk3a=(bvec4(1,ii)*gbtens4(1,1,ii)+bvec4(2,ii)*gbtens4(1,2,ii))/zbpol
     zwk3b=(bvec4(1,ii)*gbtens4(2,1,ii)+bvec4(2,ii)*gbtens4(2,2,ii))/zbpol
     zwkp(2)=sqrt(zwk3a**2+zwk3b**2)

     zbpol=sqrt(fvals4(ii,1)**2+fvals4(ii,3)**2)
     zwk3a=(fvals4(ii,1)*fgrads4(1,ii,1)+fvals4(ii,3)*fgrads4(1,ii,3))/zbpol
     zwk3b=(fvals4(ii,1)*fgrads4(2,ii,1)+fvals4(ii,3)*fgrads4(2,ii,3))/zbpol
     zwkp(3)=sqrt(zwk3a**2+zwk3b**2)

     zbpol=sqrt(fvals4(ii,2)**2+fvals4(ii,4)**2)
     zwk3a=(fvals4(ii,2)*fgrads4(1,ii,2)+fvals4(ii,4)*fgrads4(1,ii,4))/zbpol
     zwk3b=(fvals4(ii,2)*fgrads4(2,ii,2)+fvals4(ii,4)*fgrads4(2,ii,4))/zbpol
     zwkp(4)=sqrt(zwk3a**2+zwk3b**2)

     zwkp(5)=sqrt(gbtens3(1,2,ii)**2+gbtens3(2,2,ii)**2)

     write(6,1002) ii,zwkp(1:5)
  enddo

  write(6,*) ' '
  write(6,*) ' #  eq0:f          eq0:frz        -eq0           -eq0:f         -eq0:frz'

  do ii=1,ivec

     zbpol=sqrt(fvals3(ii,1)**2+fvals3(ii,3)**2)
     zwk3a=(fvals3(ii,1)*fgrads3(1,ii,1)+fvals3(ii,3)*fgrads3(1,ii,3))/ &
          zbpol
     zwk3b=(fvals3(ii,1)*fgrads3(2,ii,1)+fvals3(ii,3)*fgrads3(2,ii,3))/ &
          zbpol
     zwkp(1)=sqrt(zwk3a**2+zwk3b**2)

     zbpol=sqrt(fvals3(ii,2)**2+fvals3(ii,4)**2)
     zwk3a=(fvals3(ii,2)*fgrads3(1,ii,2)+fvals3(ii,4)*fgrads3(1,ii,4))/ &
          zbpol
     zwk3b=(fvals3(ii,2)*fgrads3(2,ii,2)+fvals3(ii,4)*fgrads3(2,ii,4))/ &
          zbpol
     zwkp(2)=sqrt(zwk3a**2+zwk3b**2)

     zwkp(3)=sqrt(gbtens5(1,2,ii)**2+gbtens5(2,2,ii)**2)

     zbpol=sqrt(fvals5(ii,1)**2+fvals5(ii,3)**2)
     zwk3a=(fvals5(ii,1)*fgrads5(1,ii,1)+fvals5(ii,3)*fgrads5(1,ii,3))/ &
          zbpol
     zwk3b=(fvals5(ii,1)*fgrads5(2,ii,1)+fvals5(ii,3)*fgrads5(2,ii,3))/ &
          zbpol
     zwkp(4)=sqrt(zwk3a**2+zwk3b**2)

     zbpol=sqrt(fvals5(ii,2)**2+fvals5(ii,4)**2)
     zwk3a=(fvals5(ii,2)*fgrads5(1,ii,2)+fvals5(ii,4)*fgrads5(1,ii,4))/ &
          zbpol
     zwk3b=(fvals5(ii,2)*fgrads5(2,ii,2)+fvals5(ii,4)*fgrads5(2,ii,4))/ &
          zbpol
     zwkp(5)=sqrt(zwk3a**2+zwk3b**2)

     write(6,1002) ii,zwkp(1:5)
  enddo

  contains
    subroutine bpolgen(bvec,fvals,bpol)
      real*8, dimension(:,:) :: bvec,fvals,bpol
      integer :: ii

      do ii=1,ivec
         bpol(ii,1)=sqrt(bvec(1,ii)**2+bvec(2,ii)**2)
         bpol(ii,2)=sqrt(fvals(ii,1)**2+fvals(ii,3)**2)
         bpol(ii,3)=sqrt(fvals(ii,2)**2+fvals(ii,4)**2)
      enddo
    end subroutine bpolgen
      
end subroutine test_eq_get

subroutine test_mcgrid
  !  test f77 xplasma "mcgrid" grid & profile create/retrieve subroutines

  use xplasma_obj_instance
  implicit NONE

  real*8, dimension(:), allocatable :: mcprof

  integer :: iudsym = 2   ! assume no updown symmetry
  integer :: inth0 = 2    ! poloidal zones in [0:pi] of first row around axis
  integer :: inznbmri = 10   ! radial zone rows in plasma
  integer :: inphi = 1    ! no toroidal variation

  integer :: id_mcgrid

  integer :: judsym,inrow,inrow_ext,inzon,inzon_ext,jnth0,jnphi,idum,ierr
  integer :: inth,ith,ir,ii,jj,itot,itmp,iftest(12),in1,in2,inum
  integer, dimension(:), allocatable :: inthzns,ids

  real*8, dimension(:), allocatable :: zvols
  real*8, dimension(:,:,:), allocatable :: zftest

  real*8, parameter :: ONE = 1.0d0
  character*32 zname
  !----------------------------------------------------

  call mcgrid_define(inphi,iudsym,inth0,inznbmri,idum,id_mcgrid)
  write(6,*) ' after mcgrid_define call id_mcgrid = ',id_mcgrid
  if(id_mcgrid.eq.0) then
     write(6,*) ' ==> an ERROR occurred.'
     return
  endif

  call mcgrid_getsym(id_mcgrid,judsym)
  write(6,*) ' iudsym (1 means updown symmetric): ',judsym

  call mcgrid_getnumr(id_mcgrid,inrow_ext,inrow)
  write(6,*) ' #zone rows (internal, total): ',inrow,inrow_ext

  call mcgrid_getnumz(id_mcgrid,inzon_ext,inzon)
  write(6,*) ' #zones (internal, total): ',inzon,inzon_ext

  call mcgrid_getnuma(id_mcgrid,jnth0,jnphi)
  write(6,*) ' #theta zones in 1st row at axis: ',jnth0
  write(6,*) ' #toroidal zones (1 for axisymmetric): ',jnphi

  allocate(inthzns(inrow_ext))

  call mcgrid_getnumzns(id_mcgrid,inthzns)
  write(6,*) ' #theta zones in all zone rows: '
  write(6,*) inthzns

  allocate(zvols(inzon))
  call mcgrid_getobj(id_mcgrid,'Zone_Volumes',-1,0,ONE,zvols,inzon,ierr)
  if(ierr.ne.0) then
     write(6,*) ' ?mcgrid_getobj failure:'
     call xplasma_error(s,ierr,6)
     return
  endif

  write(6,*) ' '
  write(6,*) ' zvols (1st row): ',zvols(1:4)
  write(6,*) ' '
  write(6,*) ' zvols (2nd row): ',zvols(5:12)
  write(6,*) ' '

  deallocate(zvols)

  !  test function

  allocate(zftest(2,2,inzon))
  itot=0
  do ir=1,inrow
     inth=inthzns(ir)
     do ith=1,inth
        zftest(1,1,itot+ith)=ith
        zftest(2,1,itot+ith)=inth+1-ith
        itmp=ith+inth/2
        if(itmp.gt.inth) itmp=itmp-inth
        zftest(1,2,itot+ith)=itmp
        zftest(2,2,itot+ith)=inth+1-itmp
     enddo
     itot=itot+inth
  enddo

  call mcgrid_putarr2(id_mcgrid,'ftest_mcgrid', &
       -1,0,0,ONE,zftest,2,2,2,2,inzon,ierr)
  if(ierr.ne.0) then
     write(6,*) ' ?mcgrid_putarr2 error.'
  else
     in1=0
     in2=0
     zftest=0
     call mcgrid_getarr2(id_mcgrid,'ftest_mcgrid', &
          -1,0,ONE,zftest,2,2,inzon,in1,in2,ierr)
     call out3('normal')

     zftest=0
     call mcgrid_getarr2(id_mcgrid,'ftest_mcgrid', &
          -1,0,-ONE,zftest,2,2,inzon,in1,in2,ierr)
     call out3('th_ reverse')

     zftest=0
     call mcgrid_getarr2(id_mcgrid,'ftest_mcgrid', &
          0,0,ONE,zftest,2,2,inzon,in1,in2,ierr)
     call out3('shift')

     zftest=0
     call mcgrid_getarr2(id_mcgrid,'ftest_mcgrid', &
          0,0,-ONE,zftest,2,2,inzon,in1,in2,ierr)
     call out3('shift+th_reverse')
  endif

  deallocate(inthzns,zftest)
  write(6,*) ' '
  write(6,*) ' (test_mcgrid done).'
  write(6,*) ' '

  !------------------------------------------------

  call xplasma_find_grids(s,ierr, &
       icoord=xplasma_rho_coord, num_grids=inum)
  allocate(ids(inum))
  call xplasma_find_grids(s,ierr, &
       icoord=xplasma_rho_coord, id_grids=ids)

  write(6,*) ' grids discretizing "rho":'
  do ii=1,inum
     call xplasma_get_item_info(s,ids(ii),ierr,name=zname)
     write(6,*) ii,trim(zname)
  enddo

  deallocate(ids)

  !------------------------------------------------

  call xplasma_find_profs(s,ierr, &
       icoord1=xplasma_rho_coord,num_profs=inum)
  allocate(ids(inum))
  call xplasma_find_profs(s,ierr, &
       icoord1=xplasma_rho_coord, id_profs=ids)

  write(6,*) ' '
  write(6,*) ' profiles vs. "rho" only:'
  do ii=1,inum
     call xplasma_get_item_info(s,ids(ii),ierr,name=zname)
     write(6,*) ii,trim(zname)
  enddo

  deallocate(ids)
  !------------------------------------------------

  call xplasma_find_blkbxs(s,ierr, &
       itype=17,num_blkbxs=inum)
  allocate(ids(inum))
  call xplasma_find_blkbxs(s,ierr, &
       itype=17, id_blkbxs=ids)

  write(6,*) ' '
  write(6,*) ' black boxes of type # 17:'
  do ii=1,inum
     call xplasma_get_item_info(s,ids(ii),ierr,name=zname)
     write(6,*) ii,trim(zname)
  enddo

  deallocate(ids)
  !------------------------------------------------
  write(6,*) ' '

  contains
    subroutine out3(zlbl)
      character*(*), intent(in) :: zlbl

      integer :: itot,inum,irow(100)

      if(ierr.ne.0) then
         write(6,*) ' mcgrid_getarr2 "',zlbl,'" call had an error.'
      else

         inum=inthzns(3)
         itot=inthzns(1)+inthzns(2)

         write(6,*) ' '
         write(6,*) ' "',zlbl,'" (1,1) row 3:'
         irow(1:inum)=zftest(1,1,itot+1:itot+inum)+0.1
         write(6,*) irow(1:inum)

         write(6,*) ' '
         write(6,*) ' "',zlbl,'" (2,1) row 3:'
         irow(1:inum)=zftest(2,1,itot+1:itot+inum)+0.1
         write(6,*) irow(1:inum)

         write(6,*) ' '
         write(6,*) ' "',zlbl,'" (1,2) row 3:'
         irow(1:inum)=zftest(1,2,itot+1:itot+inum)+0.1
         write(6,*) irow(1:inum)

         write(6,*) ' '
         write(6,*) ' "',zlbl,'" (2,2) row 3:'
         irow(1:inum)=zftest(2,2,itot+1:itot+inum)+0.1
         write(6,*) irow(1:inum)

      endif

    end subroutine out3

end subroutine test_mcgrid

subroutine test_vtaub

  implicit NONE

  integer, parameter :: itsize = 6
  real*8 :: xb(itsize) = (/ 0.1d0, 0.3d0, 0.5d0, 0.7d0, 0.9d0, 1.1d0 /)
  real*8 :: vtaub(itsize)

  real*8 :: tol=1.0d-4
  real*8 :: cut=1.0d-8

  real*8 :: zrho=0.75d0

  real*8 :: zbmin,zbmax

  integer :: i,ierr
  !-------------------------

  call eq_tau_bounce(zrho,itsize,xb,tol,cut,zbmin,zbmax,vtaub,ierr)
  if(ierr.ne.0) then
     write(6,*) ' ?test_vtaub: ierr= ',ierr,' after eq_tau_bounce call.'
     return
  endif

  write(6,*) ' eq_tau_bounce OK: rho=',zrho
  write(6,*) ' zbmin,zbmax = ',zbmin,zbmax

  write(6,*) ' v*tau_bounce vs. x = (Brefl-Bmin)/(Bmax-Bmin)'
  write(6,*) '     trapped/passing boundary at x=1.'
  write(6,*) ' '
  write(6,*) '   #   x              v*taub'
  
  do i=1,itsize
     write(6,'(3x,i2,2(1x,1pe13.6))') i,xb(i),vtaub(i)
  enddo

  write(6,*) ' '

end subroutine test_vtaub
