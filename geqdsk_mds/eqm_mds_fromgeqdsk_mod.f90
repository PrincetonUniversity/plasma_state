module eqm_mds_fromgeqdsk_mod
  use xplasma2_calls
contains
subroutine eqm_mds_fromgeqdsk(label,zfile,ns,nt1,fbdy,cbdy,irz, &
     ier)

  !  make an xplasma equilibrium representation from contents of
  !  a G-EQDSK file or MDSplus EFIT data.  This is done by finding an
  !  appropriately spaced set of fieldline contours psi=constant from
  !  EFIT's psi(R,Z) data.

  !  dmc Feb. 2001

  use xplasma2_calls
  implicit NONE

  character*(*), intent(in) :: label     ! xplasma eq. label
  character*(*), intent(in) :: zfile     ! path to G-EQDSK file / MDSplus
 
  integer, intent(in) :: ns              ! no. of surfaces incl. axis
         ! abs(ns)= no. of surfaces; if ns.lt.0 use Hermite fit for surfaces
 
  integer, intent(in) :: nt1             ! abs(nt1) = no. of theta pts /surface
  !  sign(nt1): +1 for traditional moments; -1 for moments based on equal arc
  !                                            theta definition
 
  integer, intent(in) :: irz             ! =1 for B(R,Z) calculation
 
  real*8, intent(in) :: fbdy             ! bdy back-off parameter (to start)
  real*8, intent(in) :: cbdy             ! min. curvature parameter
 
  integer, intent(out) :: ier            ! completion code, 0=OK

  !----------------------------------------------

  integer :: icur_psi = 0
  real*8, parameter :: xbrki = 0.9d0

  !----------------------------------------------

  call eqm_mds_fromgeqdsk_iadj(label,zfile,ns,nt1,fbdy,cbdy,icur_psi,xbrki,irz, &
     ier)

end subroutine eqm_mds_fromgeqdsk

subroutine eqm_mds_fromgeqdsk_iadj(label,zfile,ns,nt1,fbdy,cbdy, &
     icur_psi,xbrki,irz, &
     ier)

  !  make an xplasma equilibrium representation from contents of
  !  a G-EQDSK file or MDSplus EFIT data.  This is done by finding an
  !  appropriately spaced set of fieldline contours psi=constant from
  !  EFIT's psi(R,Z) data.

  !  dmc Feb. 2001
  use xplasma2_calls
  use xplasma_obj
  use xplasma_obj_instance, only: s
  use xplasma_obj_instance_mod
  use eq_module
  implicit NONE
  character*(*), intent(in) :: label     ! xplasma eq. label
  character*(*), intent(in) :: zfile     ! path to G-EQDSK file / MDSplus
 
  integer, intent(in) :: ns              ! no. of surfaces incl. axis
         ! abs(ns)= no. of surfaces; if ns.lt.0 use Hermite fit for surfaces
 
  integer, intent(in) :: nt1             ! abs(nt1) = no. of theta pts /surface
  !  sign(nt1): +1 for traditional moments; -1 for moments based on equal arc
  !                                            theta definition
 
  integer, intent(in) :: icur_psi        ! =0: use EFIT file data QPSI for
  ! q(psi) and EFIT file data CURRENT_A for total enclosed toroidal current;
  ! =1:  discard file values and re-derive q(psi) and enclosed current from
  ! analysis of the Psi(R,Z) data.

  ! Advice: if the Psi(R,Z) data in the EFIT input is from a high quality
  ! free boundary GS solution, icur_psi=1 is better; if not, icur_psi=0 is
  ! better.  EFIT datasets extracted from TRANSP time slices are not based
  ! on a free boundary solution, and, icur_psi=0 should be used.

  !  Most experimental EFIT data can be safely processed either way.

  real*8, intent(in) :: xbrki            ! break location for current profile;
  !  enclosed toroidal current is a smooth parabolic interpolation to the 
  !  edge value, matching point and slope with the current profile computed
  !  from Psi(R,Z), at normalized toroidal flux (xbrki).  Recommended value
  !  is: xbrki=0.9; constraint: 0.75 <= xbrki <= 0.95.

  integer, intent(in) :: irz             ! =1 for B(R,Z) calculation
 
  real*8, intent(in) :: fbdy             ! bdy back-off parameter (to start)
  real*8, intent(in) :: cbdy             ! min. curvature parameter
 
  integer, intent(out) :: ier            ! completion code, 0=OK

  !----------------------------------------------

  !  input arguments
  !  ---------------

  !  label ~ 30 characters -- a label for xplasma

  !  zfile - a file path, e.g. /u/efit/nstx/g123456_6800.eqdsk
  !         i.e. a path to a single EFIT G-EQDSK file...

  !               --or--

  !     EFIT_INDEX:<file-path>(<index>)

  !       where <file-path> is [<directory-path>/]<filename>
  !       gives an "EFIT INDEX ascii file" that associates a set of time
  !       points with a set of separate G-EQDSK files; the file format
  !       is illustrated in this example; "|" denotes start of each text
  !       lines:

  !         |ntimes=4
  !         |path='/pub/outgoing/pshare'
  !         |time=0.15   filename=g008500.00150
  !         |time=0.20   filename=g008500.00200
  !         |time=0.25   filename=g008500.00250
  !         |time=0.294  filename=g008500.00294

  !      and the listed files are G-EQDSK files associated with each time.
  !      The times *must* be in ascending order.

  !      The full path to each G-EQDSK file will be <directory-path>/<filename>
  !      where <directory-path> is what leads to the index file itself (i.e.
  !      the index file and the G-EQDSK files are expected all to be in the
  !      same directory; the path record in the index file is *ignored*).

  !      The field <index> should decode as integer and refers to the
  !      time index; 1 => 1st time, 2 => 2nd time, etc.

  !               --or--

  !          an MDSplus path:

  !     MDS+[/reduce|/noreduce]:<server[:port]|LOCAL>:<tree>(<shot>;t=<time>)

  !    where the initial MDS+ specifies MDSplus not a file;

  !    /reduce means just read the specified timeslice (this is the default)
  !    /noreduce means read and cache data from all available EFIT times

  !    <server...> specifies the MDSplus server, can be "LOCAL"
  !                or "CONNECTED", which means the server connection has
  !                already been made

  !    <tree> specifies the MDSplus tree, e.g. "EFIT01" or "EFIT02"
  !           value can be "OPENED", which means, the tree is already open.
  !           if value "OPENED" is provided, the correct shot number should
  !           still be provided, for information/identification purposes.

  !    <shot> specifies the MDSplus (or experiment) shot number
  !    <time> is an ascii encoding of the currently desired time of interest

  !       examples of valid MDSplus paths:

  !       1.  MDS+:LOCAL:EFIT01(104370;t=0.15)
  !       2.  MDS+:CONNECTED:OPENED(104370;t=0.15)
  !       3.  MDS+/reduce:europa.pppl.gov:8501:EFIT01(104370;t=1.0e-1)
  !       4.  MDS+/noreduce:cmoda.psfc.mit.edu:EFIT09(123456789;t=2.3)

  !     notes:  => in cases 3 & 4, the code connects to the server, opens
  !             the tree, reads the data, closes the tree, and disconnects
  !             from the server.
  !             => in case 1, the code opens the tree, reads the data, and
  !             closes the tree.
  !             => in case 2, the code just reads the data; the caller is
  !             expected to have taken care of any MDS+ connect or open 
  !             operations.

  !  ns -- abs(ns)=number of surfaces, spaced in equal steps of sqrt(tor.flux),
  !       to be generated in the xplasma representation, from the EFIT data
  !       if ns.lt.0, use Hermite instead of Spline fits for R(rho,chi),
  !       Z(rho,chi), and B(rho,chi) components computed in xplasma
  !       NOTE: spline is still calculated; it is replaced with a Hermite
  !       representation.

  !  nt1 -- abs(nt1) = number of theta points per surface in the xplasma 
  !         representation; sign(nt1) = -1 for moments based on equal arc
  !         definition of theta; +1 for traditional VMEC/descur power
  !         spectrum minimization definition
  !         

  !  fbdy -- delta_psi(use)/delta_psi(EFIT-bdy), btw 0.95 and 1.00
  !         use this to specify a plasma boundary "slightly in from" the
  !         plasma boundary given by EFIT.

  !    note that eqi_fromgeqdsk will also back off from the nominal EFIT
  !    boundary, if when calculating its fieldline contour it is found not
  !    to close.  So:  fbdy=1.0d0 as input is safe and usually desirable.

  !  cbdy -- minimum allowed boundary contour local curvature, normalized
  !    to boundary midplane half radius.  Setting this .gt.0 is another way
  !    to bind the boundary surface away from the separatrix-- the software
  !    will automatically reduce delta_psi(use) as needed to satisfy this
  !    condition.

  !  irz -- =1: extend the xplasma representation and coordinate scheme to
  !    cover EFIT's R,Z box; define psi(R,Z), B(R,Z) as xplasma objects
  !         =0: just form R(theta,rho),Z(theta,rho), psi(rho), g(rho), q(rho)
  !    P(rho) but do not save the (R,Z) information.
  !    ...irz=1 requires more work

  !----------------

  !  output argument:  ier=0 denotes success

  !    if successful, xplasma interpolation routines are available for
  !    evaluation of metrics, field, g, q, etc.,

  !----------------
  integer :: iertmp,iwarn,ikmom,ibrki
  real*8, parameter :: mom_rtol = 1.0d-6
  real*8, parameter :: xbrk_ax0 = 0.15d0
  !----------------
  !  access the EFIT data; build xplasma

  call xoi_init(ier)
  if(ier.ne.0) write(lunerr,*)&
       ' ?eqm_select: xplasma_init returned ier = ',ier
  eq_module_init = .TRUE.
  call xoi_kmom_get(ikmom)
  call xplasma_kmom_set(s,ikmom,ier)  ! error not checked...
 
  call xplasma_init(s,label,ier)
  if(ier.ne.0) write(lunerr,*)&
       ' ?eqm_select: xplasma_init returned ier = ',ier
  
  call xoi_author_set(iertmp)

  call xplasma_mds_fromgeqdsk(s,zfile,ier, &
       new_device=.TRUE., nrho=abs(ns), nth=abs(nt1), &
       bdy0=fbdy, cratio_min=cbdy, rzspace = (irz.eq.1), &
       kpsi_cur=icur_psi, rho_curbreak=xbrki, &
       kmom = ikmom, mom_rtol=mom_rtol, rhobrk_adj_axis = xbrk_ax0, &
       equal_arc = (nt1.lt.0), lhermite = (ns.lt.0) )

  call xoi_author_clear(iertmp)

  call eqi_mds_fromg_warn(iwarn)
  if((iwarn.gt.0).or.(ier.ne.0)) call xplasma_error(s,ier,lunerr)

#ifdef __DEBUG
  call eqdbg_plot
#endif

end subroutine eqm_mds_fromgeqdsk_iadj
subroutine eqm_geqdsk_ppadj(ier)

  !  Replace presure profile (based on P(psi) from G-eqdsk file originally)
  !  Use P(bdy) and integrate inward using dP/dPsi with a finite difference
  !  approximation.  The profile "DPDPSI" must be defined.  The original
  !  "P" profile is renamed "P_ORIG".

  use xplasma2_calls
  use xplasma_obj_instance
  use xplasma_obj_instance_mod
  use xplasma_fromgeq
  use eq_module
  implicit NONE

  integer, intent(out) :: ier
  !----------------------------------
  integer :: iertmp
  !----------------------------------

  call xoi_author_set(iertmp)

  call xplasma_geqdsk_ppadj(s,ier)
  if(ier.ne.0) then
     write(lunerr,*) ' ?eqm_geqdsk_ppadj: xplasma routine reports error:'
     call xplasma_error(s,ier,lunerr)
  endif

  call xoi_author_clear(iertmp)

#ifdef __DEBUG
  call eqdbg_plot
#endif

end subroutine eqm_geqdsk_ppadj
  ! module with routines that help to create various types of xplasma profiles
  subroutine xplasma_mds_fromgeqdsk(s,filename,ier, &
       new_device, nrho, nth, i2pi, lhermite, &
       bdy0, cratio_min, rzspace, &
       kpsi_cur,rho_curbreak, &
       kmom, rhobrk_adj_axis, mom_rtol, &
       equal_arc, &
       gs_errmax)
    
  use xplasma2_calls
  use xplasma_obj
  use eqi_rzbox_module

  implicit NONE
  
    
    ! (re)build equilibrium inverse represenation from EFIT g-eqdsk data
    !    R(rho,theta), Z(rho,theta), g(rho),q(rho),P(rho).
    
    type (xplasma), pointer :: s
    
    character*(*), intent(in) :: filename  ! Geqdsk filename or MDS+ path
    !                  ...or...EFIT_INDEX:filename(t=<time>) specification
    !                  see comments in source/xplasma/eqm_fromgeqdsk.f90
    
    integer, intent(out) :: ier  ! integer status code (returned; 0=OK)
    
    !---------------
    !  optional...
    
    logical, intent(in), optional :: new_device ! flag, .TRUE. if reading
    !  equilibrium for a new device.  Default: .TRUE. when reading the
    !  first equilibrium in the history of the xplasma object; .FALSE.
    !  subsequently.  When this is not .TRUE., the limiter data is presumed
    !  already known to xplasma.
    
    integer, intent(in), optional :: nrho  ! no. of flux surfaces 
    !                                        covering rho = [0:1]
    !  rho = normalized(sqrt(Psi-tor/Psi-tor-at-bdy))
    !        for 20 zones specify nrho=21 -- count includes axis and plasma
    !        boundary-- evenly spaced in rho
    !  DEFAULT: nrho=51 if new_device is .TRUE.; otherwise the value from
    !  the old equilibrium is taken.
    
    integer, intent(in), optional :: nth   ! no. of theta points -- no. of 
    !        evenly spaced points spanning 0:2pi inclusive.
    !  DEFAULT: nth=101 if new_device is .TRUE.; otherwise the value from
    !  the old equilibrium is taken.
    
    integer, intent(in), optional :: i2pi  ! set =1 for theta range to
    !  be -pi:pi instead of 0:2pi (the latter is the default).
    
    logical, intent(in), optional :: lhermite ! flag, .TRUE. for Hermite
    !  interpolation of {R,Z}(rho,theta) -- by default, bicubic spline
    !  interpolation is used.
    
    real*8, intent(in), optional :: bdy0   ! initial Psi_bdy/Psi_sep
    !  (default 1.0d0) fraction of Psi to enclose in mapped region
    !  The issue here is that a separatrix surface generally cannot be
    !  used as a boundary in an inverse representation, due to q=infinity
    !  and the kink in the surface shape.  So instead one backs off from
    !  the Psi_sep flux, defining a boundary just inside the separatrix.
    !    Recommendation: the default.  "cratio_min" causes the code to
    !  iterate to find a suitable boundary surface.
    !  Accepted range: 0.95d0 to 1.00d0
    
    real*8, intent(in), optional :: cratio_min  ! minimum curvature ratio
    !  (default 0.080d0) minimum acceptable local curvature radius, 
    !  normalized to midplane half width.  A separatrix kink has a curvature
    !  radius of zero.  This is tested by fitting a circle through successive
    !  sequences of 3 points on the boundary grid [1:nth] and looking at this
    !  circle's radius compared to an estimate of the plasma midplane half-
    !  width.
    !    Recommendation: the default, based on experience.
    !  Accepted range: 0.01d0 to 0.15d0
    
    logical, intent(in), optional :: rzspace ! .TRUE. to save (R,Z)
    !  grids using the EFIT grids, and compute B(R,Z).  Default = .TRUE.
    !  Setting this .FALSE. may speed some applications.
    
    integer, intent(in), optional :: kpsi_cur    ! =0 (default) to set
    !  total toroidal current from the EFIT file data (scalar CURRENT_A),
    !  and use EFIT q(psi) data (QPSI) for the q profile in the equilibrium
    !  which affects flux surface spacing.
    
    !  If instead kpsi_cur = 1 is set, the EFIT total toroidal current 
    !  magnitude and QPSI data are discarded, and the q(psi) and enclosed
    !  current profiles are derived from contour integrals over the 
    !  Psi(R,Z) data.
    
    !  Advice: if Psi(R,Z) is derived from a high quality free boundary
    !  solution, use kpsi_cur = 1; if unsure, use the defaults.
    
    real*8, intent(in), optional :: rho_curbreak    ! This number,
    !  constrained to be between 0.75 and 0.95, specifies where to
    !  break the enclosed toroidal current profile and impose a smooth
    !  parabolic interpolation to the edge value (see kpsi_cur for how
    !  this is set).  This is necessary to get smooth dI/drho and <J.B>
    !  profiles out.  Default: 0.90.
    
    integer, intent(in), optional :: kmom    ! number of Fourier moments
    !  to use in Fourier Spline representation of equilibrium 
    !  (default: not set here; xplasma default is 16).
    
    real*8, intent(in), optional :: rhobrk_adj_axis ! delta(rho) for
    !  moments trimming operations, and, the maximum rho where flux
    !  surfaces can be touched, in order to assure that d/drho[R0,Z0] --> 0
    !  as rho--> 0, an attributed needed by codes (e.g. RF solvers) that
    !  use the inverted equilibrium.
    !    Default value: 0.15d0.  To disable this feature, set 
    !  rhobrk_adj_axis = -1.0d0 or any other negative number
    
    real*8, intent(in), optional :: mom_rtol ! moments trimming ratio
    !  (default 1.0d-6).  The EFIT Psi(R,Z) inversion involves a Fourier
    !  representation; this control specifies a method for trimming away
    !  higher order moments that are very small over wide regions of space.
    !  Accepted range: 0.0d0 to 1.0d-4
    
    logical, intent(in), optional :: equal_arc  ! .TRUE. to use an equal
    !  arc definition for the poloidal angle coordinate; .FALSE. (default)
    !  for VMEC/descur moments power spectrum minimization definition, the
    !  traditional definition for past usage.
    
    real*8, intent(out), optional :: gs_errmax  ! normalized measure of
    !  Grad Shafranof (GS) error in the EFIT equilibrium.
    
    !-----------------------------
    real*8 :: zbdy0,zcratio,zmom_rtol,zrhobrk_axis,zerrmax
    integer :: irz,inrho,inth,id,idx,ieq,iertmp,jopt,j2pi
    real*8 :: zdum,xbrki
    
    logical :: ihermite,inew_device,ienable
    integer :: ipsi_cur
    
    character*32 :: glist,gelems(7),zauth
    real*8 :: zelems(7)
    character*140 :: ztext(7),zlabel
    
    real*8 :: rhobrk_min=0.75d0
    real*8 :: rhobrk_max=0.95d0
    
    !-----------------------------
    
    ier=0
    
    call xplasma_global_info(s,ier, initLabel=zlabel, eq_counter=ieq)
    if(ier.ne.0) return
    
    call xplasma_author_query(s,zauth,ienable,ier)
    if(ier.ne.0) return
    
    !-----------------------------
    !  check if this an equilibrium for a new device;
    !  if so xplasma re-initialization may be required...
    
    if(ieq.eq.0) then
       inew_device=.TRUE.
    else 
       inew_device=.FALSE.
       if(present(new_device)) inew_device=new_device
    endif
    
    if(inew_device.and.(ieq.gt.0)) then
       ! re-initialize xplasma: changing devices!
       call xplasma_init(s,zlabel,iertmp)
       ier=max(ier,iertmp)
       call xplasma_author_set(s,zauth,iertmp)
       ier=max(ier,iertmp)
    endif
    if(ier.ne.0) then
       call xplasma_errmsg_append(s,'error in xplasma_mds_fromgeqdsk.')
       return
    endif
    
    j2pi=2
    if(present(i2pi)) j2pi=i2pi
    
    if(present(kmom)) then
       call xplasma_kmom_set(s,kmom,ier)
       if(ier.ne.0) then
          call xplasma_errmsg_append(s,'error in xplasma_mds_fromgeqdsk.')
          return
       endif
    endif
    
    ipsi_cur=0
    xbrki=0.9d0
    
    if(present(kpsi_cur)) then
       ipsi_cur = max(0,min(1,kpsi_cur))
    endif
    
    if(present(rho_curbreak)) then
       xbrki=min(rhobrk_max,max(rhobrk_min, rho_curbreak))
    endif
    
    !-----------------------------
    
    if(inew_device) then
       !  use arguments
       if(.not.present(nrho)) then
          inrho=51
       else
          inrho=max(5,nrho)
       endif
       
       if(.not.present(nth)) then
          inth=101
       else
          inth=max(21,nth)
       endif
       
    else
       !  use previous equilibrium grid sizes
       call xplasma_gridId(s,'__RHO',idx)
       call xplasma_grid_size(s,idx,inrho,iertmp)
       ier=max(ier,iertmp)
       
       call xplasma_gridId(s,'__CHI',idx)
       call xplasma_grid_size(s,idx,inth,iertmp)
       ier=max(ier,iertmp)
    endif
    if(ier.ne.0) then
       call xplasma_errmsg_append(s,'error in xplasma_mds_fromgeqdsk.')
       return
    endif
    
    ihermite=.FALSE.
    if(present(lhermite)) ihermite=lhermite
    
    irz=1
    if(inew_device.and.present(rzspace)) then
       if(rzspace) then
          irz=1
       else
          irz=0
       endif
    endif
    
    glist = '__GFILE_INFO'
    
    gelems(1) = 'FILENAME'
    zelems(1) = 0
    ztext(1) = filename
    
    zbdy0=1.0d0
    if(present(bdy0)) then
       zdum=0.95d0
       zbdy0=max(zdum,min(zbdy0,bdy0))  ! enforce range 0.95d0 to 1.00d0
    endif
    gelems(2)='BDY0'
    zelems(2)=zbdy0
    ztext(2)='Relative start point of boundary search'
    
    zcratio=0.080d0  ! **DEFAULT** curvature ratio limit
    
    if(present(cratio_min)) then
       zdum=0.01d0
       zcratio=max(zdum,cratio_min)
       zdum=0.15d0
       zcratio=min(zdum,zcratio)        ! enforce range 0.01d0 to 0.15d0
    endif
    gelems(3)='CRATIO'
    zelems(3)=zcratio
    ztext(3)='minimum accepted boundary curvature ratio'
  
    zrhobrk_axis=0.15d0
    if(present(rhobrk_adj_axis)) then
       zdum=0.30d0
       zrhobrk_axis=min(zdum,rhobrk_adj_axis)
    endif
    gelems(4)='RHOBRK_ADJ_AXIS'
    zelems(4)=zrhobrk_axis
    ztext(4)='width of interior region affected by axis adjustment'
    
    zmom_rtol=1.0d-6
    if(present(mom_rtol)) then
       zdum=0.0d0
       zmom_rtol=max(zdum,mom_rtol)
       zdum=1.0d-4
       zmom_rtol=min(zdum,zmom_rtol)    ! enforce range 0.0 to 1.0d-4
    endif
    gelems(5)='MOM_RTOL'
    zelems(5)=zmom_rtol
    ztext(5)='trimming parameter for Fourier moments representation'
    
    gelems(6)='QPSI_OPTION'
    zelems(6)=ipsi_cur
    ztext(6)='value of 1.0 indicates Q(psi) & total current from Psi(R,Z)'
    
    gelems(7)='RHO_CURBREAK'
    zelems(7)=rho_curbreak
    ztext(7)='break location for matching current profile to edge value.'
    
    zerrmax=0.0d0
    if(present(gs_errmax)) gs_errmax=zerrmax
    
    !---------------------------------------------
    ! record geqdsk controls in a list... id not saved...
    
    call xplasma_create_list(s,glist,gelems,id,ier, &
         label='xplasma_mds_fromgeqdsk inputs list', &
         r8vals=zelems, chvals=ztext)
    if(ier.ne.0) then
       call xplasma_errmsg_append(s, &
            'xplasma_mds_fromgeqdsk: '// &
            'error occurred while attempting to define list: '//trim(glist))
       return
    endif
    
    !---------------------------------------------
    ! OK, build the xplasma equilibrium...
    
    sp => s
    
    jopt = 0
    if(present(equal_arc)) then
       if(equal_arc) then
          jopt = 1
          if(.not.present(kmom)) then
             call xplasma_kmom_set(s,64,ier)  ! set FFT moments up to 64
             ! ...for equal arc theta
          endif
       endif
    endif
    
    call eqi_mds_fromgeqdsk(filename, ihermite, inew_device, &
         inrho, inth, j2pi, zbdy0, zcratio, irz, jopt, &
         ipsi_cur,xbrkI, &
         zrhobrk_axis, zmom_rtol, zerrmax, ier)
    
    if(ier.ne.0) ier=150
    
    if(present(gs_errmax)) gs_errmax=zerrmax
    
  end subroutine xplasma_mds_fromgeqdsk

end module eqm_mds_fromgeqdsk_mod
