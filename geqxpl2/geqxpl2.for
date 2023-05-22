      program geqxpl2
c
c  create an xplasma from a (hard coded) geqdsk file
c  ...dmc May 2001
c
c  this code is meant as an example illustrating use of xplasma to
c  construct an equilibrium representation from an EFIT G-EQDSK file.
c
c  all floats are REAL*8 (or f90 SELECTED_REAL_KIND(12,100))
c
c..#   sample compile-load script -- Linux alpha workstation --
c..set f90opt = "fort -fast -assume no2underscore"
c..set libdir = /usr/ntcc/lib
c..$f90opt geqxpl2.for -L $libdir
c..                   -lxplasma -lfluxav -lmclib -lsmlib -ltrgraf \
c..                   -lureadsub -lsg -ljc -lnscrunch -lr8bloat -lfftsub \
c..                   -lcomput -lvaxonly -lpspline -lezcdf -llsode  \
c..                   -llsode_linpack -lportlib \
c..                -L/usr/local/lib
c..                   -lnetcdf -llapack -lblas \
c..                   -lreadline -lhistory -ltermcap
c
c  on your system:  the location of the installed NTCC libraries might
c                   not be /usr/ntcc/lib
c  and... your compiler switches might be different.
c  and... netcdf, lapack, blas might not be in the same place
c
c  gnu libraries (readline, history, termcap) are "optional" but recommended.
c  these enable command line editing & filename completion when answering
c  prompts inside interactive fortran programs.
c
      use eqm_mds_fromgeqdsk_mod
      implicit NONE
c
      INTEGER, PARAMETER :: R8=SELECTED_REAL_KIND(12,100)
c
      character*150 zfile
      integer ns,nt1,irz,ier
      character*20 label
c
      real*8 fbdy,crat
      real*8 zadjust
c
c---------------------------------------------------------------------
c
      call uinitg('GEQXPL2')
c
      label = 'geqxpl2_test'      ! arbitrary label string for xplasma
 
      zfile = 'g103875.nstx'      ! *** name or path of G-EQDSK file ***
                                  !   MDS+ path also OK on systems with MDS+
 
      fbdy=1.00_r8                ! =1.00 to start with EFIT boundary
                                  ! =0.99 to start with a boundary
                                  !  enclosing 1% less flux than EFIT's
                                  ! boundary
 
      crat=0.080_r8               ! minimum curvature ratio; local curvature
                                  ! of boundary surface .gt. crat*a, where
                                  ! a= midplane half width of plasma; code
                                  ! will reduce enclosed flux if necessary
                                  ! to find a boundary satisfying this
                                  ! condition
 
      ns= 41                      ! number of "rho" surfaces including axis
                                  ! surfaces equally spaced in rho which is
                                  ! defined as sqrt(normalized toroidal flux).
                                  ! abs(ns) is the actual no. of surfaces; if
                                  ! ns.lt.0 use Hermite instead of Spline for
                                  ! surface fits in xplasma.
 
      nt1=51                      ! number of (evenly spaced) "theta" points
                                  ! in (rho,theta) bicubic spline grids.
                                  ! theta chosen by eqm_mds_fromgeqdsk for
                                  ! numerical reasons -- no special
                                  ! properties of physical significance
                                  ! this is a "periodic" grid with
                                  ! theta(nt1)=theta(1)+2pi.  Data splined
                                  ! over this grid will have periodic
                                  ! boundary conditions in the theta
                                  ! direction.
 
      irz=1                       ! =1 to form B(R,Z), psi(R,Z)
c
c---------------
c
c  the curvature-based boundary selection seems to do well in terms of
c  avoiding future problems due to choosing a boundary which is kinked
c  at a separatrix.  It also has the advantage that for non-diverted
c  discharges the actual, unreduced boundary can be used.  In our older
c  codes a constant psi-reduction parameter (to move the boundary in a
c  little) was used, which had the disadvantages (a) sometimes we didn't
c  move it in enough; and (b) other times we moved it in more than was
c  necessary so we lost some of the plasma edge.
c
c-------------------------------------
c
c  OK this is the xplasma call which reads the G-EQDSK file and sets
c  up the equilibrium
c
      write(6,1002) crat,zfile(1:len_trim(zfile))
 1002 format(' (eqm_mds_fromgeqdsk call)'/
     >   ' adjust boundary so that minimum local curvature is at least'/
     >   1x,f12.6,' * [the midplane half-width of the plasma]'//
     >   ' --> eqm_mds_fromgeqdsk sets up xplasma g-eqdsk data: ',a/
     >   '     (note eqm_mds_fromgeqdsk can also acquire EFIT data'/
     >   '     over the network from standard EFIT MDSplus trees).'/)
c
      write(6,*) ' '
      write(6,*) ' ...reading & converting G-eqdsk... '
c
      call eqm_mds_fromgeqdsk(label,zfile,ns,nt1,fbdy,crat,irz,ier)
c
      write(6,*) ' '
c
c-------------------------------------
c
      if(ier.ne.0) then
         write(6,*) ' ?eqm_mds_fromgeqdsk error, ier=',ier
         call ps_bad_exit("eqm_mds_fromgeqdsk error")
      endif
c
c  make some analytic profiles... illustrate how to define some functions
c
      call geqxpl2_pro
c
c  print out some stuff... illustrate xplasma data lookup calls...
c
      call geqxpl2_print
c
c  interactive SG-plotting of the geometry, fields and their derivatives...
c  this also shows the full xplasma contents and the names of individual
c  items...
c
      call eqdbg_plot
c
      write(6,*) ' --------------- all done --------------- '
c
      stop
      end
c-----------------------------------------------------------------
      subroutine geqxpl2_pro
c
c  the following header file should be used!
c  it allows the compiler to check that xplasma calls have the right
c  calling arguments.  However, there is a possible performance issue:
c  contact dmccune@pppl.gov for details.
c
c  make some profiles
c
c  electron density... #/m**3
c
      implicit NONE
c
      INTEGER, PARAMETER :: R8=SELECTED_REAL_KIND(12,100)
c
      real*8, parameter :: ONE = 1.0_R8, ZERO = 0.0_R8
c
      real*8, parameter :: ne0 = 4.0e19_r8   ! central density
      real*8, parameter :: nea = 1.0e19_r8   ! edge density
      real*8, parameter :: neexp = 1.0_r8    ! exponent
c
c  ne(rho) = (ne0-nea)*(ONE-rho**2)**neexp + nea
c
c  and a similar formula for TE and TI (keV)
c
      real*8, parameter :: Ti0 = 5.0_r8, Tia=0.1_r8, tiexp=1.5_r8
      real*8, parameter :: Te0 = 5.0_r8, Tea=0.1_r8, teexp=1.5_r8
c
c  ratios for various ion densities n_i/n_e
c  I am enforcing quasineutrality by hand...
c
      real*8, parameter :: nc_rat = 0.05_r8   ! carbon (Z=6)
      real*8, parameter :: nh_rat = 0.01_r8   ! H "minority"
      real*8, parameter :: nd_rat = 0.69_r8   ! D "majority"
c
c  for all profiles...
c
      integer, parameter :: ibc0 = 1    ! central boundary condition:
      real*8, parameter :: zbc0 = ZERO  ! df/d(rho) = 0
      integer, parameter :: ibc1 = 0    ! default boundary condition at edge
                                        ! spline "not a knot" boundary cond.
      real*8 zdum                       ! edge BC data (not used)
c
      integer, parameter :: iorder = 2  ! spline, C2
c
c         iorder=1  --> Hermite cubics, C1 but less likely to "ring"
c                       in case of noisy data
c
c         iorder=0  --> piecewise linear, C0 and really safe.
c
c  working variables...
c
      real*8, dimension(:), allocatable :: rho,fun,fne
c
      real*8 :: ztol = 1.0e-8_r8        ! a small number...
c
      integer ngrid    ! size of grid currently in use
c
      integer :: id_rho=0   ! xplasma id for "rho" grid (equilibrium version)
      integer :: id_myrho=0 ! xplasma id for another "rho" grid (created here)
c
      integer :: id_te=0,id_ti=0,id_ne=0,id_nc=0,id_nd=0,id_nh=0  ! profile xplasma id's
c
c  the xplasma id's could be saved in a COMMON or module for another part
c  of the program to use, but this is not essential since the id's can
c  also retrieved by translating from the profile names...
c
      integer igot,ierr
      integer i
c
c-----------------------------------------------------------
c
      write(6,*) 'geqxpl2_pro...'
c
c  one can define profiles on the same grid as the equilibrium is defined
c  on, or... on a different grid.
c
c  first I show code using the same grid, and use this to set up TE and TI
c
      call eq_ganum('__RHO',id_rho)   ! this gets the equilibrium rho grid
c                                     ! name is always "__RHO".
c
      call eq_ngrid(id_rho,ngrid)     ! get size of the grid
c
      allocate(rho(ngrid),fun(ngrid)) ! allocate arrays
c
      call eq_grid(id_rho,rho,ngrid,igot,ierr)  ! get the grid
      if(ierr.ne.0) call ps_bad_exit(' ?? eq_grid error.')
c
c  igot is guaranteed to equal ngrid, because I just allocated the array
c  of the right size.  But in an f77 code I might have been using a fixed
c  size static array; ierr would be set if this array size were not sufficient
c  to hold the requested grid.
c
c  ps_bad_exit is a `portlib' routine that writes an error message, sets
c  an exit status code (testable by shell scripts) and exits the program.
c
c  OK... compute the functions, f90 style...
c
      write(6,*) '...define TE and TI'
c
      fun = (te0-tea)*(ONE-rho*rho)**teexp + tea ! this is an array expression
c
c  now define a 1d profile for "TE"
      call eqm_rhofun(iorder,                 ! fit order (e.g. spline)
     >                id_rho,                 ! "x axis" grid
     >                'TE',                   ! name of item
     >                fun(1:ngrid),           ! the data itself
     >                ibc0,zbc0,              ! d(TE)/drho = 0 on axis
     >                ibc1,zdum,              ! default BC for edge
     >                id_te,                  !  **TE xplasma id returned**
     >                ierr)                   ! completion code, 0=OK
      if(ierr.ne.0) call ps_bad_exit(' ?? "TE" eqm_rhofun error.')
c
c  similarly for "TI"
c
      fun = (ti0-tia)*(ONE-rho*rho)**tiexp + tia ! this is an array expression
c
      call eqm_rhofun(iorder,id_rho,'TI',fun,ibc0,zbc0,ibc1,zdum,
     >   id_ti,ierr)
      if(ierr.ne.0) call ps_bad_exit(' ?? "TI" eqm_rhofun error.')
c
c----------------
c
      deallocate(rho,fun)
c
c----------------
c
c  now suppose I want to use my own grid to define my own function f(rho).
c  the grid covers the same range [0,1] and has the same physical association
c  (sqrt(normalized toroidal flux)) but the number of points and spacing
c  can be different; grids are never required to be evenly spaced... but
c  for simplicity I make a 26 point evenly spaced grid here...
c
      ngrid=26
c
      allocate(rho(ngrid),fun(ngrid),fne(ngrid))
c
c  there is a more compact f90 way of doing this...
c
      do i = 1,ngrid
         rho(i)=(i-1)*ONE/(ngrid-1)  ! 0.00, 0.04, 0.08, ..., 1.00
      enddo
c
      write(6,*) '...setting up alternate rho grid'
c
c  define this grid to xplasma and give it the name "my_rho".
c  note that the grid is associated with the original grid:  it covers
c  the same space and has the same physical association; just a different
c  partitioning...
c
      call eqm_uaxis('my_rho',            ! the name
     >                id_rho,             ! "associated" grid ("__RHO").
     >                0,                  ! 0="not periodic" 1="periodic"
     >                rho(1:ngrid),       ! the grid points...
     >                ngrid,              ! number of points in this grid...
     >                ztol,               ! even spacing tolerance...
     >                id_myrho,           ! *** xplasma id of new grid ***
     >                ierr)
      if(ierr.ne.0) call ps_bad_exit(' ?? eqm_uaxis "my_rho" error.')
c
c  once the grid id_myrho exists, profiles f(rho) can be defined using
c  this grid; these splines can be used interchangeably with splines
c  defined against the original (MHD equilibrium) rho grid.
c
c  the 3rd argument of eqm_uaxis is "periodicity"; xplasma also grids
c  which represent poloidal angle-- these are "periodic" grids; splines
c  on periodic grids satisfy e.g.
c     f'(theta(0))=f'(theta(ntheta))
c     f''(theta(0))=f''(theta(ntheta))
c  i.e. a periodic boundary condition.
c
c  but this is a digression, because here we deal with f(rho) profiles
c  only.
c
c  the electron density:
c
      write(6,*) '...define NE and ion density profiles.'
c
      fne = (ne0-nea)*(ONE-rho*rho)**neexp + nea ! this is an array expression
c
c  note use of "id_myrho" tells eqm_rhofun that item is defined on the
c  26 point rho grid just defined...
c
      call eqm_rhofun(iorder,id_myrho,'NE',fne,ibc0,zbc0,ibc1,zdum,
     >   id_ti,ierr)
      if(ierr.ne.0) call ps_bad_exit(' ?? "NE" eqm_rhofun error.')
c
c  ion densities...
c
      fun = nd_rat*fne   ! array expression
      call eqm_rhofun(iorder,id_myrho,'ND',fun,ibc0,zbc0,ibc1,zdum,
     >   id_ti,ierr)
      if(ierr.ne.0) call ps_bad_exit(' ?? "ND" eqm_rhofun error.')
c
      fun = nh_rat*fne   ! array expression
      call eqm_rhofun(iorder,id_myrho,'NH',fun,ibc0,zbc0,ibc1,zdum,
     >   id_ti,ierr)
      if(ierr.ne.0) call ps_bad_exit(' ?? "NH" eqm_rhofun error.')
c
      fun = nc_rat*fne   ! array expression
      call eqm_rhofun(iorder,id_myrho,'NC',fun,ibc0,zbc0,ibc1,zdum,
     >   id_ti,ierr)
      if(ierr.ne.0) call ps_bad_exit(' ?? "NC" eqm_rhofun error.')
c
      deallocate(rho,fun,fne)
c
c  OK thats enough...
c
      return
      end
c------------------------------------------------------------------------
      subroutine geqxpl2_print
c
      implicit NONE
c
      INTEGER, PARAMETER :: R8=SELECTED_REAL_KIND(12,100)
c
c  demo the xplasma data access routines; print results
c
      integer i,j,id,id_rho,idgrid,ngrid,igot
c
      integer, parameter :: nlist=20
      integer idlist(nlist),n1,n,nf
      character*20 namlist(nlist),zname
      integer inumrho,inumchi,inumphi,inumR,inumZ
c
      integer nregion(nlist)
      real*8 xlist(nlist),ylist(nlist),zlist(nlist)
      real*8 rlist(nlist),philist(nlist),rholist(nlist),chilist(nlist)
      real*8 glist(nlist),datbuf(nlist,nlist),dist(nlist)
      real*8 tol
      real*8 b_axis,r_axis,z_axis
      real*8 rho_axis,rho_bdy
      real*8 Rmin,Zmin,Bmin,Rmax,Zmax,Bmax
      real*8 Rming,Zming,Rmaxg,Zmaxg
c
      real*8 Rc(1),Rs(1),Zc(1),Zs(1),x(1)
      integer kmom,im
c
      real*8, dimension(:), allocatable :: xbuf,xbuf_zc
      integer, dimension(:), allocatable :: kindx
c
      integer il1,il2,ierr,iwarn,iwant,ictwant(6)
      integer magdim,magfit,cyldim,cylfit
c------------------
c  name translation
c
      datbuf=0
c
      write(6,1001)
 1001 format(/' xplasma data access routines are described at:'/
     >   '  http://w3.pppl.gov/~pshare/help/xplasma_hlp.html'/)
c
      write(6,1002)
 1002 format(' ========>> axis & function name translation.'/
     >   '  (a) given name, fetch xplasma id.'/
     >   '      eq_ganum:  axis;  eq_gfnum:  function.'/
     >   '      eq_gflist:  list of funcionts.'/)
c
      call eq_ganum("__RHO",id_rho)
      write(6,999) ' call eq_ganum("__RHO",id_rho) --> id_rho=',id_rho
 999  format(a,1x,i4)
      write(6,*) ' '
c
      call eq_gfnum("Te",id)
      write(6,999) ' call eq_gfnum("Te",id) -> id=',id
      call eq_gfnum("TE",id)
      write(6,999) ' call eq_gfnum("TE",id) -> id=',id
      write(6,*) ' ... the name matching is case-insensitive.'
      write(6,*) ' '
c
      n=2
      namlist(1)='TE'
      namlist(2)='NE'
      call eq_gflist(n,namlist(1:n),idlist(1:n),ierr)
      write(6,1003) ierr,n
 1003 format(' call eq_gflist(n,namlist(1:n),idlist(1:n),ierr)'/
     >   '   ierr=',i4,' (set if any name is unknown)'/
     >   '   a list of length n=',i4,' was processed:')
      do i=1,n
         il1=len_trim(namlist(i))
         write(6,1005) i,namlist(i)(1:il1),i,idlist(i)
 1005    format('     namlist(',i1,')="',a,
     >      '" -> idlist(',i1,')=',i4)
      enddo
      write(6,*) ' '
c
      write(6,1006) id_rho,id
 1006 format('  (b) given id, fetch name (reverse translation).'/
     >   '      id_rho=',i4,'  id=',i4/)
c
      call eq_get_aname(id_rho,zname)
      write(6,998) ' call eq_get_aname(id_rho,zname) -> zname = "',
     >   zname(1:len_trim(zname)),'"'
c
      call eq_get_fname(id,zname)
      write(6,998) ' call eq_get_fname(id,zname) -> zname = "',
     >   zname(1:len_trim(zname)),'"'
 998  format(a,a,a)
c
      write(6,*) ' '
      call eq_fun(id,magdim,magfit,cyldim,cylfit,iwarn,ierr)
      write(6,1007) zname(1:len_trim(zname)),
     >   magdim,magfit,cyldim,cylfit,iwarn,ierr
 1007 format('  (c) given id, fetch characteristics:'/
     >   '      magdim=1 => f(rho), magdim=2 => f(rho,chi) ',
     >                     'magdim=0 => (none)'/
     >   '      magfit=[Cn of fit, e.g 2 for C2 spline]'/
     >   '      cyldim=2 => f(R,Z), magdim=0 => (none)'/
     >   '      cylfit=[Cn of fit, e.g 2 for C2 spline]'/
     >   '      iwarn set if an xplasma "private" function;'/
     >   '      ierr set if no such function for given id.'//
     >   '      for "',a,'":'/
     >   ' call eq_fun(id,magdim,magfit,cyldim,cylfit,iwarn,ierr)'/
     >   '      magdim=',i1,' magfit=',i1/
     >   '      cyldim=',i1,' cylfit=',i1/
     >   '       iwarn=',i1,'   ierr=',i1//
     >   ' a named object can exist in both f(rho) and f(R,Z) form,'/
     >   ' and sometimes this is advantageous for performance reasons.')
c
      write(6,*) ' '
      write(6,*) ' ========>> axis (grid) information.'
      write(6,*) ' '
c
      call eq_ganum("my_rho",idgrid)
      write(6,999) ' call eq_ganum("my_rho",idgrid) --> idgrid=',idgrid
      write(6,*) ' '
c
      call eq_ngrid(idgrid,ngrid)
      write(6,999) ' eq_ngrid(idgrid,ngrid) --> ngrid=',ngrid
      write(6,*) ' '
c
      allocate(xbuf(ngrid),xbuf_zc(ngrid-1))
      write(6,*) ' allocate(xbuf(ngrid),xbuf_zc(ngrid-1))'
      write(6,*) ' '
      call eq_grid(idgrid,xbuf(1:ngrid),ngrid,igot,ierr)
      write(6,997)
     >   ' call eq_grid(idgrid,xbuf(1:ngrid),ngrid,igot,ierr)  -->',
     >   '      igot=',igot,' ierr=',ierr
 997  format(a/a,i4,a,10(1x,i4))
      write(6,2001) ' xbuf=',(xbuf(i),i=1,ngrid)
      write(6,*) ' '
      call eq_grid_zc(idgrid,xbuf_zc(1:ngrid-1),ngrid-1,igot,ierr)
      write(6,997)
     >   ' call eq_grid_zc(idgrid,xbuf_zc(1:ngrid-1),'//
     >   'ngrid-1,igot,ierr) -->','      igot=',igot,' ierr=',ierr
      write(6,2001) ' xbuf_zc=',(xbuf_zc(i),i=1,ngrid-1)
      write(6,*) ' '
 2001 format(1x,a,' = '/6(1x,f10.6))
c
      call eq_ngrids(inumrho,inumchi,inumphi,inumR,inumZ)
      write(6,*)
     >   ' call eq_ngrds(inumrho,inumchi,inumphi,inumR,inumZ) ->'
      write(6,*) ' xplasma "common grid" sizes:'
      write(6,1008) inumrho,inumchi,inumphi,inumr,inumz
 1008 format(
     >   '   MHD equilibrium rho grid:    ',i4/
     >   '   MHD equilibrium theta grid:  ',i4/
     >   '     poloidal angle is sometimes called "chi" in xplasma.'/
     >   '   MHD equilibrium phi grid:    ',i4/
     >   '     (inumphi=1 means axisymmetry).'/
     >   '     (xplasma requires axisymmetry at present).'/
     >   '   (R,Z) grids:  inumR, inumZ:  ',i4,i4//
     >   ' although xplasma grid information is available, it'/
     >   ' is not necessary to use it, in order to perform'/
     >   ' interpolations...'/)
c
      n1=ngrid-1
      allocate(kindx(n1))
      write(6,*) ' n1=ngrid-1'
      write(6,*) ' allocate(kindx(n1))'
      write(6,*) ' '
      call eq_nindx(id_rho,n1,xbuf_zc(1:n1),kindx(1:n1),ierr)
      write(6,997) ' call eq_nindx('//
     >   'id_rho,n1,xbuf_zc(1:n1),kindx(1:n1),ierr) -->',
     >   '      ierr=',ierr,' kindx=',kindx
      write(6,*) ' ..these indices show which zones in the MHD rho grid'
      write(6,*) ' ..contain the zone center points from "my_rho" grid.'
      write(6,*) ' '
c
      write(6,*) ' ========>> coordinate transformation.'
      write(6,*) '  coordinates:'
      write(6,*) '    cartesian:  (x,y,z)'
      write(6,*) '    cylindric:  (R,Z,phi)'
      write(6,*) '    magnetic:   (rho,chi,phi)'
      write(6,*) '  all coordinate transformations are vectorized,'
      write(6,*) '  i.e. transform lists of coordinate tuples.'
      write(6,*) '   ***>> vectorization helps performance'
      write(6,*) '         on workstations as well as vector machines.'
      write(6,*) ' '
      write(6,*) '  ---not all routines demonstrated---'
      write(6,*) '  ---see the documentation---'
      write(6,*) ' '
      write(6,*) '  (rho,chi,phi) -> (R,Z,[phi])'
c
      n=3
      rholist(1)=0.5_r8
      chilist(1)=0.0_r8
      philist(1)=0.0_r8
      rholist(n)=0.6_r8
      chilist(n)=1.0_r8
      philist(n)=1.0_r8
      do i=2,n-1
         rholist(i)=rholist(1)+(i-1)*(rholist(n)-rholist(1))/(n-1)
         chilist(i)=chilist(1)+(i-1)*(chilist(n)-chilist(1))/(n-1)
         philist(i)=philist(1)+(i-1)*(philist(n)-philist(1))/(n-1)
      enddo
c
      call eq_rzget(n,rholist,chilist,philist,rlist,zlist,nregion,ierr)
      write(6,*) ' call eq_rzget(n,rholist,chilist,philist,'//
     >   'rlist,zlist,nregion,ierr)'
      do i=1,n
         write(6,2002) i,rholist(i),chilist(i),philist(i),
     >      rlist(i),zlist(i),philist(i)
      enddo
 2002 format(2x,i1,'.  (',3(f9.5,1x),') --> (',3(f9.5,1x),')')
      write(6,*) ' '
c
      write(6,*) '  (R,[Z],phi) -> (x,y,[z])'
      call eq_xcyl(n,rlist,philist,xlist,ylist)
      write(6,*) ' call eq_xcyl(n,rlist,philist,xlist,ylist)'
      do i=1,n
         write(6,2002) i,rlist(i),zlist(i),philist(i),
     >      xlist(i),ylist(i),zlist(i)
      enddo
      write(6,*) ' '
c
      write(6,*) '  (x,y,[z]) -> (R,[Z],phi)'
      call eq_rcyl(n,xlist,ylist,rlist,philist)
      write(6,*) ' call eq_rcyl(n,xlist,ylist,rlist,philist)'
      do i=1,n
         write(6,2002) i,xlist(i),ylist(i),zlist(i),
     >      rlist(i),zlist(i),philist(i)
      enddo
c
      tol=1.0e-10_r8
      write(6,*) '  (R,Z,phi) -> (rho,chi,[phi])'
      write(6,1009) tol
 1009 format('    [ tol = ',1pe11.4,' ]')
      call eq_inv(n,rlist,zlist,philist,tol,
     >   rholist,chilist,nregion,ierr)
      write(6,*) ' call eq_inv(n,rlist,zlist,philist,tol, &'
      write(6,*) '             rholist,chilist,nregion,ierr)'
      do i=1,n
         write(6,2002) i,rlist(i),zlist(i),philist(i),
     >      rholist(i),chilist(i),philist(i)
      enddo
      write(6,*) ' "tol" controls the accuracy of the Newton iteration'
      write(6,*) ' used to compute the "inverse map".'
      write(6,*) '   (caution: performance issue).'
      write(6,*) ' '
c
      write(6,*) ' also available:'
      write(6,*) '   eqx_inv:  (x,y,z) -> (rho,chi,phi)'
      write(6,*) '   eq_xget:  (rho,chi,phi) -> (x,y,z)'
      write(6,*) ' '
c
      write(6,*) ' '
      write(6,*) ' ========>> min/max information.'
      write(6,*) ' '
      write(6,*) ' ...axis and boundary "rho" coordinate:'
      call eq_rholim(rho_axis,rho_bdy)
      write(6,1011) rho_axis,rho_bdy
 1011 format(' call eq_rholim(rho_axis,rho_bdy)'/
     >   '  -->  rho_axis,rho_bdy = ',2(f10.4))
      write(6,*) ' '
c
      write(6,*) ' ...min and max R and Z on a flux surface:'
      call eq_glimrz(rho_bdy,Rmin,Rmax,Zmin,Zmax,ierr)
      write(6,1013) ierr,Rmin,Rmax,Zmin,Zmax
 1013 format('     [rho = rho_bdy]'/
     >   ' call eq_glimrz(rho,Rmin,Rmax,Zmin,Zmax,ierr)'/
     >   '  --> ierr=',i3/
     >   '      Rmin,Rmax,Zmin,Zmax =',4(1x,f10.4)/)
      write(6,*) ' ...min and max mod(B) on a flux surface:'
      call eq_glimb(rho_bdy,Bmin,Bmax,ierr)
      write(6,1015) ierr,Bmin,Bmax
 1015 format('     [rho = rho_bdy]'/
     >   ' call eq_glimb(rho,Bmin,Bmax,ierr)'/
     >   '  --> ierr=',i3/
     >   '      Bmin,Bmax =',2(1x,f10.4)/)
c
      write(6,*)
     >   ' ...extrema of (R,Z) rectangular grid covering plasma:'
      write(6,*) ' call eq_rzlims(Rming,Rmaxg,Zming,Zmaxg,ierr)'
      call eq_rzlims(Rming,Rmaxg,Zming,Zmaxg,ierr)
      write(6,1017) ' ...min and max R of grid: ',Rming,Rmaxg
 1017 format(a,2(1x,f10.4))
      write(6,1017) ' ...min and max Z of grid: ',Zming,Zmaxg
      write(6,*) ' '
c
      write(6,*) ' ...magnetic axis information:'
      write(6,*) ' ...assuming axisymmetry...'
      write(6,*) ' call eq_sgaxis(B_axis,R_axis,Z_axis)'
      call eq_sgaxis(B_axis,R_axis,Z_axis)
      write(6,1018) b_axis,r_axis,z_axis
 1018 format(' ...at the magnetic axis:'/
     >   '   mod(B)=',f10.4,' (R,Z) = (',f10.4,',',f10.4,')'/
     >   ' ...note this is the actual mod(B), not the vacuum field.')
      write(6,*) ' '
      write(6,*) ' ========>> f(rho) interpolation.'
      write(6,*) ' '
c
      call eq_gfnum("G",id)
      iwant=0
      call eq_rgetf(n,rholist,id,iwant,glist,ierr)
      write(6,1019) ierr
 1019 format(
     >   '   to interpolate g(rho) = R*Bphi at n rho values:'/
     >   '      use iwant=0 for g, iwant=1 for dg/drho, '/
     >   '      use iwant=2 for d2g/drho2 (splines only).'//
     >   ' iwant = 0'/
     >   ' call eq_gfnum("G",id)'/
     >   ' call eq_rgetf(n,rholist,id,iwant,glist,ierr)'/
     >   '   ---> ierr=',i4)
      do i=1,n
         write(6,1020) i,rholist(i),i,glist(i)
 1020    format('        rholist(',i1,')=',f10.4,' -> ',
     >      'glist(',i1,')=',f10.4)
      enddo
      write(6,1021)
 1021 format(/
     >'   to interpolate multiple functions {f(rho)} pass a list of'/
     >'   function ids and receive a buffer datbuf(1:n,1:nfcns);'/
     >'   see xplasma documentation of eq_rgetff (1d interpolation).'/)
c
      write(6,*) ' '
      write(6,*) ' ========>> f(rho,chi) interpolation.'
      write(6,*) ' '
c
      write(6,1025)
 1025 format(
     >   ' to interpolate sets of fucntions {f(rho,chi)}:'/
     >   '   (a) determine list of desired functions;'/
     >   '   (b) determine if {f}, {df/drho,df/dchi}, or'/
     >   '       some other combination of values and derivatives'/
     >   '       is wanted.'/
     >   '   (c) set up vectors rho(1:n) and chi(1:n), and call:'/
     >   '          eq_frhochi -- get {f} values'/
     >   '          eq_grhochi -- get {df/drho,df/dchi} values'/
     >   '          eq_hrhochi -- get combination up to 2nd order.'/
     >   '               (2nd order derivatives for splines only).'/)
c
      call eq_gfnum("R",idlist(1))
      call eq_gfnum("BR",idlist(2))
      call eq_gfnum("BZ",idlist(3))
      nf=3
      call eq_frhochi(n,rholist,chilist,nf,idlist,nlist,datbuf,ierr)
c
      write(6,1026) n,nlist,nf,ierr
 1026 format(
     >' When using these functions one must be careful with the '/
     >' dimensioning of the data buffer or array which is to contain'/
     >' the results of the interpolations.'//
     >' In this example, we are using a list of 3 functions, a'/
     >' rho,chi vector length of ',i1,', and an output buffer which'/
     >' is dimensioned   datbuf(nlist,nlist)  where nlist=',i3,'.'//
     >'    ** first example **'/
     >' call eq_gfnum("R",idlist(1))'/
     >' call eq_gfnum("BR",idlist(2))'/
     >' call eq_gfnum("BZ",idlist(3))'/
     >' nf=',i1/
     >' call eq_frhochi(n,rholist,chilist,nf,idlist,  &'/
     >'                 nlist,datbuf,ierr)'//
     >' (note that the 1st dimension of datbuf, "nlist", must be'/
     >' passed).  The result is ierr=',i4,', and:')
      do i=1,n
         write(6,1028) i,rholist(i),i,chilist(i),
     >      i,datbuf(i,1),i,datbuf(i,2),i,datbuf(i,3),
     >      i,glist(i)/datbuf(i,1)
      enddo
 1028 format(
     >   ' at rholist(',i1,')=',f10.4,', chilist(',i1,')=',f10.4,','/
     >   '     R  = datbuf(',i1,',1)=',f10.4/
     >   '     BR = datbuf(',i1,',2)=',f10.4/
     >   '     BZ = datbuf(',i1,',3)=',f10.4/
     >   '     and, glist(',i1,')/R = Bphi =',f10.4/)
c
      ictwant = (/ 0, 1, 1, 1, 1, 1 /)
      call eq_gfnum("R",idlist(1))
      call eq_gfnum("Z",idlist(2))
      nf=2
      call eq_hrhochi(n,rholist,chilist,nf,idlist,ictwant,
     >   nlist,datbuf,ierr)
      write(6,1030) ierr
 1030 format('    ** second example **'/
     >   ' call eq_gfnum("R",idlist(1))'/
     >   ' call eq_gfnum("Z",idlist(2))'/
     >   ' nf=2'/
     >   ' ictwant = (/ 0, 1, 1, 1, 1, 1 /)'/
     >   ' call eq_hrhochi(n,rholist,chilist,nf,idlist,ictwant,  &'/
     >   '                 nlist,datbuf,ierr)'//
     >   ' yields ierr=',i4,'.  "ictwant" is interpreted as follows:'/
     >   '    ictwant(1)=0 --> do NOT return R,Z values.'/
     >   '    ictwant(2)=1 --> do return d{R,Z}/drho.'/
     >   '    ictwant(3)=1 --> do return d{R,Z}/dchi.'/
     >   '    ictwant(4)=1 --> do return d2{R,Z}/drho2.'/
     >   '    ictwant(5)=1 --> do return d2{R,Z}/dchi2.'/
     >   '    ictwant(6)=1 --> do return d2{R,Z}/drhodchi.'/
     >   ' so we get:')
      do i=1,n
         write(6,1032) i,rholist(i),i,chilist(i),
     >      (i,datbuf(i,j),j=1,10)
      enddo
 1032 format(
     >   ' at rholist(',i1,')=',f10.4,', chilist(',i1,')=',f10.4,','/
     >   '    dR/drho = datbuf(',i1,',1)=',f10.4/
     >   '    dR/dchi = datbuf(',i1,',2)=',f10.4/
     >   '    d2R/drho2 =    datbuf(',i1,',3)=',f10.4/
     >   '    d2R/dchi2 =    datbuf(',i1,',4)=',f10.4/
     >   '    d2R/drhodchi = datbuf(',i1,',5)=',f10.4//
     >   '    dZ/drho = datbuf(',i1,',6)=',f10.4/
     >   '    dZ/dchi = datbuf(',i1,',7)=',f10.4/
     >   '    d2Z/drho2 =    datbuf(',i1,',8)=',f10.4/
     >   '    d2Z/dchi2 =    datbuf(',i1,',9)=',f10.4/
     >   '    d2Z/drhodchi = datbuf(',i1,',10)=',f10.4/)
c
      write(6,*) ' Note:  "eq_gfnum" is used in these examples for'
      write(6,*) '  the sake of clarity, but, name translation calls'
      write(6,*) '  should be kept out of inner loops for performance'
      write(6,*) '  reasons...'
      write(6,*) ' '
c
      write(6,*) ' ========>> demo of routines to compute distance from'
      write(6,*) ' plasma boundary.  Input: lists of (R,Z,phi) values:'
      do i=1,nlist
         rlist(i)=r_axis+(i-1)*(rmax-r_axis)/(nlist-1)
         zlist(i)=z_axis+(i-1)*(zmax-z_axis)/(nlist-1)
         philist(i)=0.0_r8
      enddo
c
      write(6,*) ' '
      write(6,*) ' call eq_dfast(nlist,rlist,zlist,philist,dist,ierr)'
      write(6,*) ' '
c
      call eq_dfast(nlist,rlist,zlist,philist,dist,ierr)
c
      write(6,1034) nlist,ierr
 1034 format(' nlist=',i4,' on input; on output ierr=',i4/
     >   '  #    R          Z          phi        distance (output)')
      do i=1,nlist
         write(6,1035) i,rlist(i),zlist(i),philist(i),dist(i)
      enddo
 1035 format(1x,i2,4(1x,f10.4))
c
      write(6,*) ' positive distance gives how far outside the plasma;'
      write(6,*) ' negative distance gives how rar inside.  The numbers'
      write(6,*) ' from eq_dfast are approximate; a slower but more'
      write(6,*) ' accurate calculation is also available.'
c
      deallocate(kindx)
c
      deallocate(xbuf,xbuf_zc)
c
c  data on Fourier representation of equilibrium...
c
      write(6,*) ' '
      write(6,*) ' Fourier moments representation of flux geometry'
      write(6,*) ' can be retrieved via calls to eqmom_rcos, etc...'
c
      call xmoments_kmom_get(kmom)  ! get no. of moments
c
      do i=1,3
         x(1)=(i-1)/2.0d0
         write(6,*) ' '
         write(6,*) ' ===> x=',x
         write(6,*)
     >      ' Moment#    Rcos         Rsin         Zcos         Zsin'
         do im=0,kmom
            !  usually would call these routines with a longer x vector...
            call eqmom_rcos(x,1,0,im,Rc,iwarn)
            call eqmom_rsin(x,1,0,im,Rs,iwarn)
            call eqmom_zcos(x,1,0,im,Zc,iwarn)
            call eqmom_zsin(x,1,0,im,Zs,iwarn)
            write(6,1100) im,Rc(1),Rs(1),Zc(1),Zs(1)
         enddo
      enddo
 1100 format(3x,i2,7x,4(1x,1pe12.5))
c
      write(6,*)
      write(6,*) ' end of geqxpl2_print demo.'
      write(6,*) ' *** not all xplasma routines shown ***'
      write(6,*) ' *** see NTCC xplasma documentation for full list ***'
      write(6,*)
c
      return
      end
