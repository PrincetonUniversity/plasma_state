program genxpl
  !
  !  this program allows an xplasma representation of a user specified
  !  equilibrium and profile set to be created.  Then outputs (e.g. as
  !  required for reflectometry calculations) can be generated as well.
  !
  !----------------------------------

  implicit NONE

  !----------------------------------
  real*8, parameter :: ZERO = 0.0d0
  real*8, parameter :: HALF = 0.5d0
  real*8, parameter :: ONE  = 1.0d0
  real*8, parameter :: C2PI = 6.2831853071795862D+00
  real*8, parameter :: EPS6 = 1.0d-6

  !  labeling info
  character*30, parameter :: glabel = "GENXPL:analytic_profiles"
  character*30, parameter :: gtmplab = "GENXPL:temporary"
  character*30 cur_label

  real*8, parameter :: ztime = ZERO
  real*8, parameter :: ztavg = ZERO

  !  -------shape-------
  real*8 :: R0_mp   ! major radius (geometric center of plasma midplane) (m)
  real*8 :: a_mp    ! minor radius (plasma midplane half width) (m)
  real*8 :: ellip   ! ellipticity (b/a) (dimensionless)
  real*8 :: shift   ! relative axis shift (profile: 1-x**2) (dimensionless)

  !  -------field-------
  real*8 :: B0_vac  ! vacuum B field at R0_mp (T)
  real*8 :: q0,qa   ! q profile (dimensionless)
  real*8 :: g0,ga   ! para/diamagnetism (dimensionless)
  !                          i.e. deviation from vacuum field;
  !                          B(x,theta) = R0_mp*B0_vac*g(x)/R(x,theta)
  integer :: nsnccwi,nsnccwb   ! ccw flags for Itor, Btor...
  logical :: pol_field         ! .TRUE. to include poloidal field

  !  -------numerics-------
  real*8 :: extra   ! (extra-1)*a_mp = amount of (R,Z) enclosing rectangle
  !                   beyond plasma boundary (dimensionless)

  integer :: nx     ! no. of x grid pts to use
  integer :: nth    ! no. of theta grid pts to use

  !----------------------------------
  !  working quantities...

  real*8, dimension(:), allocatable :: g,q,polflx,torflx,x,xfs,xflux,th
  real*8, dimension(:), allocatable :: dx,dvol,xfs_new,zr2i,wk1,wk2
  integer, dimension(:), allocatable :: iwk
  real*8, dimension(:,:), allocatable :: Rarr,Zarr

  real*8 r,zth,zmax,tflux
  integer ix,ith,ierr,ixn,niter,nok,inx
  integer :: id_rho=0,id_chi=0,id_R=0,id_Z=0,id=0

  real*8 rdum(1),zdum(1),rlin(4),zlin(4),thlin(4),extrap

  !----------------------------------
  !  xplasma field extrapolator

  external eqm_brz_adhoc

  !----------------------------------
  !  uread; tty i/o

  logical lxout,lchar
  integer lnout,luntty
  real*8 ztmp

  !----------------------------------
  !  UREAD initialization

  call uinitg("GENXPL")
  luntty = lnout(0)

  !----------------------------------
  !  defaults...

  !  ...geometry:
  R0_mp = 2.5d0
  a_mp = 0.9d0
  ellip = ONE
  shift = ZERO

  !  ...field:
  B0_vac = 5.0d0
  nsnccwi = 1
  nsnccwb = 1

  q0 = ONE
  qa = 5.0d0
  pol_field = .TRUE.

  g0 = ONE
  ga = ONE

  !  ...numerical options:
  nx = 101
  nth = 101
  extra = 1.05d0

  !----------------------------------
  !  interactive user modifications to parameters...

1000 continue

  do
     if(lxout(0)) then
        write(luntty,1001)
1001    format(/ &
             ' genxpl main options:'/ &
             '  "A" -- set plasma shape (geometry)'/ &
             '  "B" -- set magnetic field'/ &
             '  "N" -- set numerical controls'/ &
             '  "P" -- print current settings'/ &
             '  "Q" -- QUIT, i.e. EXIT program.'/ &
             '  "X" -- execute (xplasma setup; plotting and ouput options).'/ &
             '         Te and ne will be specified after xplasma setup.'/)
     endif
     call uread('genxpl: please specify desired menu option:$')

     if(lchar('A')) then
        call set_shape
     else if(lchar('B')) then
        call set_field
     else if(lchar('N')) then
        call set_numerics
     else if(lchar('P')) then
        call printout
     else if(lchar('Q')) then
        call uexita(0)
        stop
     else if(lchar('X')) then
        exit
     else
        call uermsg(' ?GENXPL: unrecognized menu option:',' ',0)
     endif
  enddo

  !----------------------------------
  !  set up initial xplasma geometry; iterate to find x ~ sqrt(tor.flux)

  allocate(x(nx),xfs(nx),xflux(nx),g(nx),q(nx),polflx(nx),torflx(nx),th(nth))
  allocate(dx(nx-1),dvol(nx-1),zr2i(nx))
  allocate(Rarr(nth,nx),Zarr(nth,nx))

  x(1)=ZERO; x(nx)=ONE
  g(1)=g0;   g(nx)=ga
  q(1)=q0;   q(nx)=qa
  Rarr(1:nth,1)=R0_mp + a_mp*shift; Zarr(1:nth,1)=ZERO

  do ix=2,nx-1
     x(ix)=(ix-1)*ONE/(nx-1)
     g(ix)=(g0-ga)*(ONE-x(ix)**2)**2 + ga
     q(ix)=(q0-qa)*(ONE-x(ix)**2) + qa
  enddo

  do ix=2,nx
     dx(ix-1)=x(ix)-x(ix-1)
     r=x(ix)*a_mp
     do ith=1,nth
        zth=(ith-1)*C2PI/(nth-1)
        th(ith)=zth
        Rarr(ith,ix)=R0_mp + a_mp*shift*(ONE-x(ix)**2) + r*cos(zth)
        Zarr(ith,ix)=ellip*r*sin(zth)
     enddo
  enddo

  if(pol_field) then
     cur_label=gtmplab
  else
     cur_label=glabel
  endif
  call eqm_select(cur_label,1)
  call eqm_time(ZERO)

  call eqm_rho(x,nx,EPS6,id_rho,ierr)
  if(ierr.ne.0) call errmsg_exit('?genxpl: unexpected eqm_rho failure.')

  call eqm_chi(th,0,nth,EPS6,id_chi,ierr)
  if(ierr.ne.0) call errmsg_exit('?genxpl: unexpected eqm_chi failure.')

  call eqm_rzmag(Rarr,Zarr,nth,nx,2,id_R,id_Z,ierr)
  if(ierr.ne.0) call errmsg_exit('?genxpl: unexpected eqm_rzmag failure.')

  if(pol_field) then
     !  find the location of flux surfaces that give equal steps in 
     !  sqrt toroidal flux (skipped if no poloidal field is present)

     xfs=x
     niter=0
     nok=0
     do
        call eq_flxint_init(0,nx,xfs,EPS6,ierr)
        if(ierr.ne.0) call errmsg_exit( &
             '?genxpl: unexpected eq_flxint_init failure.')

        call eq_flxint('DVOL',1,dvol,1,nx-1,ierr)
        if(ierr.ne.0) call errmsg_exit( &
             '?genxpl: unexpected eq_flxint_init(dvol) failure.')

        call eq_flxint('<1/R^2>S',1,zr2i,1,nx,ierr)
        if(ierr.ne.0) call errmsg_exit( &
             '?genxpl: unexpected eq_flxint_init(zr2i) failure.')

        xflux(1)=ZERO
        do ix=2,nx
           xflux(ix)=xflux(ix-1)+R0_mp*B0_vac*HALF*(g(ix-1)+g(ix))* &
                HALF*(zr2i(ix-1)+zr2i(ix))*dvol(ix-1)/c2pi
        enddo
        tflux = xflux(nx)
        xflux = xflux/xflux(nx)
        xflux = sqrt(xflux)

        zmax=maxval(abs(xflux-x))
        write(luntty,*) ' iteration = ',niter,' rho deviation = ',zmax
        if(zmax.le.EPS6) then
           nok=nok+1
           if(nok.ge.3) exit
        endif

        niter=niter+1
        if(niter.gt.100) then
           call errmsg_exit( &
                '?genxpl: too many iterations seeking equispaced sqrt(phi/philim) grid')
        endif

        allocate(xfs_new(nx))
        xfs_new(1)=ZERO
        xfs_new(nx)=ONE
        inx=1
        do ix=2,nx-1
           do
              if(xflux(inx+1).lt.x(ix)) then
                 inx=inx+1
                 cycle
              else
                 exit
              endif
           enddo
           xfs_new(ix)=(xfs(inx)*(xflux(inx+1)-x(ix)) + &
                xfs(inx+1)*(x(ix)-xflux(inx)))/(xflux(inx+1)-xflux(inx))
        enddo
        xfs=xfs_new
        deallocate(xfs_new)
     enddo
     !
     !  OK -- redefine geometry to have surfaces in equal steps of 
     !  sqrt(tor.flux)
     !
     allocate(wk1(nth),wk2(nth),iwk(nth))
     wk2=ZERO  ! phi coord.
     do ix=2,nx-1
        wk1=xfs(ix)  ! old x...
        call eq_rzget(nth,wk1,th,wk2,Rarr(1:nth,ix),Zarr(1:nth,ix),iwk,ierr)
        if(ierr.ne.0) call errmsg_exit( &
             '?genxpl: unexpected eq_RZget failure.')
     enddo
     deallocate(wk1,wk2,iwk)
        
     cur_label=glabel

     call eqm_select(cur_label,1)
     call eqm_time(ZERO)

     call eqm_rho(x,nx,EPS6,id_rho,ierr)
     if(ierr.ne.0) call errmsg_exit('?genxpl: unexpected eqm_rho failure.')

     call eqm_chi(th,0,nth,EPS6,id_chi,ierr)
     if(ierr.ne.0) call errmsg_exit('?genxpl: unexpected eqm_chi failure.')

     call eqm_rzmag(Rarr,Zarr,nth,nx,2,id_R,id_Z,ierr)
     if(ierr.ne.0) call errmsg_exit('?genxpl: unexpected eqm_rzmag failure.')

  else
     !  no poloidal field; still want toroidal flux
     call eq_flxint_init(0,nx,x,EPS6,ierr)
     if(ierr.ne.0) call errmsg_exit( &
          '?genxpl: unexpected eq_flxint_init failure.')

     call eq_flxint('DVOL',1,dvol,1,nx-1,ierr)
     if(ierr.ne.0) call errmsg_exit( &
          '?genxpl: unexpected eq_flxint_init(dvol) failure.')

     call eq_flxint('<1/R^2>S',1,zr2i,1,nx,ierr)
     if(ierr.ne.0) call errmsg_exit( &
          '?genxpl: unexpected eq_flxint_init(zr2i) failure.')

     xflux(1)=ZERO
     do ix=2,nx
        xflux(ix)=xflux(ix-1)+R0_mp*B0_vac*HALF*(g(ix-1)+g(ix))* &
             HALF*(zr2i(ix-1)+zr2i(ix))*dvol(ix-1)/c2pi
     enddo
     tflux = xflux(nx)

  endif

  !----------------------------------------------------------
  !  OK geometry is defined; add the field information...

  call eq_ganum('__RHO',id_rho)

  call eqm_rhofun(2,id_rho,'g',R0_mp*B0_vac*g,1,ZERO,1,ZERO,id,ierr)
  if(ierr.ne.0) call errmsg_exit('?genxpl: unexpected eqm_rhofun(g) failure.')

  call eqm_rhofun(2,id_rho,'q',q,1,ZERO,0,ZERO,id,ierr)
  if(ierr.ne.0) call errmsg_exit('?genxpl: unexpected eqm_rhofun(q) failure.')

  call eqm_rhofun(2,id_rho,'torflx',x*x*tflux,1,ZERO,0,ZERO,id,ierr)
  if(ierr.ne.0) call errmsg_exit( &
       '?genxpl: unexpected eqm_rhofun(torflx) failure.')

  polflx(1)=ZERO
  do ix=2,nx
     polflx(ix)=polflx(ix-1)+dx(ix-1)*(x(ix)+x(ix-1))*tflux/ &
          ((q(ix)+q(ix-1))*(C2PI*HALF))
  enddo

  call eqm_rhofun(2,id_rho,'psi',polflx,1,ZERO,0,ZERO,id,ierr)
  if(ierr.ne.0) call errmsg_exit( &
       '?genxpl: unexpected eqm_rhofun(polflx) failure.')

  !----------------------------------------------------------
  !  define the limiter (rectangular box limiter)
  extrap=(extra-1)*a_mp

  Rlin(1:2)=R0_mp-a_mp-extrap
  Zlin(1:2)=ellip*a_mp+extrap
  Rlin(3:4)=R0_mp+a_mp+extrap
  Zlin(3:4)=-ellip*a_mp-extrap
  thlin(1)=ZERO
  thlin(3)=ZERO
  thlin(2)=90
  thlin(4)=90

  call eqm_tbdy_grid(4,Rlin,Zlin,thlin, &
       0,rdum,zdum,rdum, &
       2*nx,2*nx,id_R,id_Z,ierr)
  if(ierr.ne.0) call errmsg_exit('?genxpl: unexpected eqm_tbdy_grid failure.')

  !----------------------------------------------------------
  !  compute interior field

  call eqm_bset(nsnccwb,nsnccwi)

  !----------------------------------------------------------
  !  compute field extrapolation

  call eqm_brz(eqm_brz_adhoc, ZERO, ierr)
  if(ierr.ne.0) call errmsg_exit('?genxpl: unexpected eqm_brz failure.')

  !----------------------------------------------------------
  !  xplasma setup done; get Te and ne & write Reflectometry data

  do
     if(lxout(0)) then
        write(luntty,1005)
1005    format(/ &
             ' xplasma equilibrium & field complete.'/ &
             ' genxpl post-xplasma options:'/ &
             '  "P" -- plot xplasma contents'/ &
             '  "R" -- specify Te & ne; write reflectometry data.'/ &
             '  "Q" -- QUIT, i.e. go back to start of program.'/ &
             )
     endif
     call uread('genxpl: please specify post-xplasma option:$')

     if(lchar('P')) then
        call eqdbg_plot

     else if(lchar('R')) then
        call trxpl_fwr2d_cdf(glabel,ztime,ztavg)

     else if(lchar('Q')) then
        go to 1000

     else
        call uermsg(' ?GENXPL: unrecognized menu option:',' ',0)
     endif
  enddo

  deallocate(x,xfs,xflux,g,q,polflx,torflx,th,dx,dvol,zr2i,Rarr,Zarr)
  go to 1000  ! back to top...


  contains

    !-------------------------------------------------

    subroutine printout
      call print_shape
      call print_field(0)
      call print_numeric
    end subroutine printout

    subroutine print_shape
      write(luntty,1101) 'Midplane Major Radius','R0_mp',R0_mp,'(m)'
      write(luntty,1101) 'Midplane Minor Radius','a_mp',a_mp,'(m)'
      write(luntty,1101) 'Ellipticity','ellip',ellip,'(-)'
      write(luntty,1101) 'Relative axial shift','shift',shift,'(-)'
1101 format(1x,a,t30,a8,' = ',1pe11.4,1x,a)
      write(luntty,*) ' '
    end subroutine print_shape

    subroutine print_field(iarg)
      integer, intent(in) :: iarg   ! =1 to print major radius
      if(iarg.eq.1) then
         write(luntty,1101) 'Midplane Major Radius','R0_mp',R0_mp,'(m)'
      endif
      write(luntty,1101) 'Vacuum Field at R0_mp','B0_vac',B0_vac,'(T)'
      write(luntty,1101) 'B_tor/B_vac on axis','g0',g0,'(-)'
      write(luntty,1101) 'B_tor/B_vac at edge','ga',ga,'(-)'
      write(luntty,*) '  g(x) = (g0-ga)*(1-x**2)**2 + ga'
      write(luntty,1101) 'q on axis','q0',q0,'(-)'
      write(luntty,1101) 'q at edge','qa',qa,'(-)'
      write(luntty,*) '  q(x) = (q0-qa)*(1-x**2) + qa'
      write(luntty,*) &
           '  nsnCCWx = 1 means "counter-clockwise viewed from above".'
      write(luntty,1102) 'toroidal field direction','nsnCCWb',nsnccwb
      write(luntty,1102) 'toroidal current direction','nsnCCWi',nsnccwi
1101 format(1x,a,t30,a8,' = ',1pe11.4,1x,a)
1102 format(1x,a,t30,a8,' = ',i2)
      write(luntty,*) ' '
    end subroutine print_field

    subroutine print_numeric
      write(luntty,1191) nx,nth
1191  format(' Number of x points in radial profiles:  ',i4/ &
           ' Number of poloidal grid (theta) points: ',i4)
      write(luntty,1192) extra
      write(luntty,1193) (extra-ONE)*a_mp
1192  format(' Minor radius expansion factor to define (R,Z) rectangle:', &
           1pe11.4)
1193  format(' expansion of (R,Z) rectangle beyond plasma boundary:', &
           1pe11.4,' (m)')
      write(luntty,*) ' '
    end subroutine print_numeric

    !-------------------------------------------------
      
    subroutine set_shape
      do
         if(lxout(0)) then
            call print_shape
            write(luntty,1201)
1201        format(/' (the code imposes "sanity limits" on all values)'// &
                 ' "R" -- set R0_mp'/ &
                 ' "A" -- set a_mp'/ &
                 ' "E" -- set ellipticity'/ &
                 ' "S" -- set shift'/ &
                 ' "Q" -- quit this menu (accept current values)'/)
         endif
         call uread('genxpl: set_shape option:$')
         if(lchar('R')) then
            call ugr8rng(1.0d-2,1.0d2,'genxpl: enter R0_mp value:$',R0_mp)
            a_mp=max(1.0d-2*r0_mp,a_mp)
            a_mp=min(0.95d0*r0_mp,a_mp)
         else if(lchar('A')) then
            call ugr8rng(1.0d-2*r0_mp,0.95d0*r0_mp, &
                 'genxpl: enter a_mp value:$',a_mp)
         else if(lchar('E')) then
            call ugr8rng(0.1d0,10.0d0,'genxpl: enter ellip value:$',ellip)
         else if(lchar('S')) then
            call ugr8rng(ZERO,0.7d0,'genxpl: enter shift value:$',shift)
         else if(lchar('Q')) then
            exit
         else
            call uermsg('?genxpl: set_shape menu: unrecognized option.',' ',0)
         endif
      enddo
    end subroutine set_shape

    !-------------------------------------------------

    subroutine set_field
      logical lyes
      do
         if(lxout(0)) then
            call print_field(1)
            write(luntty,1301)
1301        format(/' (the code imposes "sanity limits" on all values)'// &
                 ' "B" -- set B0_vac'/ &
                 ' "G" -- set g profile'/ &
                 ' "H" -- set q profile'/ &
                 ' "K" -- kill the poloidal field (make q very large)'/ &
                 ' "N" -- set directions of Btor and Itor'/ &
                 ' "Q" -- quit this menu (accept current values)'/)
         endif
         call uread('genxpl: set_field option:$')
         if(lchar('B')) then
            call ugr8rng(1.0d-3,1.0d3,'genxpl: vacuum Btor at R0_mp:$',B0_vac)
         else if(lchar('G')) then
            ga=ONE
            call ugr8rng(0.1d0,2.0d0,'genxpl: B/Bvac at plasm axis:$',g0)
         else if(lchar('H')) then
            call ugr8rng(0.5d0,10.0d0,'genxpl: q on axis:$',q0)
            call ugr8rng(0.1d0,200.0d0,'genxpl: q at edge:$',qa)
            pol_field=.TRUE.
         else if(lchar('K')) then
            q0=1.0d20
            qa=1.0d20
            pol_field=.FALSE.
         else if(lchar('N')) then
            call uread( &
                 'genxpl: enter "Y" to make Btor CCW viewed from above:$')
            if(lyes(0)) then
               nsnccwb=1
            else
               nsnccwb=-1
            endif
            call uread( &
                 'genxpl: enter "Y" to make Itor CCW viewed from above:$')
            if(lyes(0)) then
               nsnccwi=1
            else
               nsnccwi=-1
            endif
         else if(lchar('Q')) then
            exit
         else
            call uermsg('?genxpl: set_numerics menu: unrecognized option.', &
                 ' ',0)
         endif
      enddo
    end subroutine set_field

    !-------------------------------------------------

    subroutine set_numerics
      do
         if(lxout(0)) then
            call print_numeric
            write(luntty,1401)
1401        format(/' (the code imposes "sanity limits" on all values)'// &
                 ' "E" -- set (R,Z) rectangle expansion factor'/ &
                 ' "N" -- set size of radial working grids'/ &
                 ' "Q" -- quit this menu (accept current values)'/)
         endif
         call uread('genxpl: set_numerics option:$')
         if(lchar('E')) then
            call ugr8rng(1.01d0,1.5d0,'genxpl: expansion factor:$',extra)
         else if(lchar('N')) then
            call ureadi('genxpl: nx (btw 21 and 1001):$',nx)
            nx=max(21,min(1001,nx))
            call ureadi('genxpl: nth (btw 21 and 1001):$',nth)
            nth=max(21,min(1001,nth))
         else if(lchar('Q')) then
            exit
         else
            call uermsg('?genxpl: set_numerics menu: unrecognized option.', &
                 ' ',0)
         endif
      enddo
    end subroutine set_numerics

    !-------------------------------------------------

end program genxpl

!  ** dummy subroutine ** (for trxpl_fwr2d_cdf)

subroutine trxpl_add_profile(zname)
  character*(*), intent(in) :: zname

  call errmsg_exit( &
       ' ?genxpl: unexpected call to trxpl_add_profile("'//zname//'")')

end subroutine trxpl_add_profile
