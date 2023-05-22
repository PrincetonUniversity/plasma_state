program geq2ps

  !  DMC Apr 2008: geq2ps, G-eqdsk to plasma state

  !  this program creates or modifies a plasma state with MHD equilibrium 
  !  representation from a file or MDS+ G-eqdsk dataset.  This includes a 
  !  mapping of the g-eqdsk Psi(R,Z) direct representation of the tokamak 
  !  MHD equilibrium to an "inverse representation" [R,Z](rho,theta), 
  !  g(rho), q(rho), P(rho).

  !  this program takes command line arguments.  Syntax:

  !     geq2ps <geq-path> <ps-path> \
  !         [-nrho:<# of rho surfaces>] [-nth:<# poloidal angle grid pts>]
  !         [-c_ratio:<curvature ratio>]
  !         [-rho_curbrk:<rho-value>] [-qcur:<EFIT|PSI|AUTO>] ...

  !            <geq-path> can be a file path or an MDS+ path
  !            if the argument starts with "MDS+:" it is the latter.

  !            MDS path examples:
  !                MDS+:skylark.pppl.gov:8501:EFIT01(104370;t=1.0e-1)
  !                MDS+:cmoda.psfc.mit.edu:EFIT09(123456789;t=2.3)

  !                generally: MDS+:<server>:<tree>(shot;t=<time-of-interest>)

  !            <ps-path> is a file path for the output plasma state file 
  !            written by this program; a writable path must be provided.

  !  -nrho defaults to 51 radial surfaces (counting axis and boundary)
  !        equally spaced in rho = normalized sqrt(enclosed toroidal flux)
  !        covering [0:1] (minimum of 11 radial grid points enforced).
  !        example:  "-nrho:101" -- no imbedded blanks, please.

  !  -nth  defaults to 101 poloidal grid points -- equal arc poloidal angle
  !        coordinate is used covering [-pi:pi] (minimum of 41 poloidal 
  !        grid points enforced).
  !        example:  "-nth:151" -- no imbedded blanks, please.

  !  -c_ratio  dimensionless quantity specifying the "minimum curvature ratio"
  !        allowed along the flux surface selected as the boundary of the
  !        equilibrium inverse representation.  The local curvature ratio is
  !        the numerically estimated radius of curvature of the boundary,
  !        divided by the plasma midplane half-width.  If a boundary surface
  !        comes near a separatrix, its curvature ratio near the separatrix
  !        has a local minimum-- indeed is zero if the surface passes directly
  !        through the separatrix.  Setting a limit on how small this minimum
  !        value can be, determines what surface inside the separatrix can
  !        be used as a boundary surface.
  !        ...various down stream codes which use the equilibrium can fail
  !        due to numerical metric singularities, if the chosen boundary 
  !        surface is too close.  Based on experience, the DEFAULT VALUE
  !        of c_ratio, 0.08, is recommended.  Use this argument to override
  !        the default.

  !  -rho_curbrk:<rho-value>  The values gives the flux location at which
  !        the enclosed toroidal current function I(rho) is transitioned
  !        from data based on Ampere's Law contour integrations over Psi(R,Z)
  !        to a parabolic interpolation to reach the edge value of total
  !        enclosed toroidal current.  This in effect smooths the edge
  !        current dI/drho and <J.B>.  <rho-value>, in normalized sqrt
  !        toroidal flux, is constrained to lie between 0.75 and 0.95.
  !        DEFAULT: 0.90.
  !
  !  -qcur ... select data source for q(psi) and the total enclosed
  !        toroidal current I(rho=1).  "EFIT" means: use q(psi) profile
  !        and the total toroidal current data provided with the EFIT
  !        data set; "PSI" means recompute q(psi) and I(1) from contour
  !        integrals over Psi(R,Z); "AUTO" (the default) means use the
  !        "EFIT" option if data label indicates that the source is a
  !        TRXPL (i.e. a TRANSP data time slice) otherwise use "PSI".
  !
  !  -pprime ... recompute Pressure profile by integrating PPRIME in
  !        from the pressure value at the boundary:  this reduces noise
  !        in the radial derivatives of P in the resulting state, but
  !        it requires the PPRIME profile in the EFIT data to be accurate.
  !
  !  -psmoo:<delta_rho> ... smoothing parameter for pressure profile
  !        Pressure data at rho is smoothed by a triangular weighted 
  !        average from rho - delta_rho to rho + delta_rho.  Default:
  !        no smoothing.  Allowed range of values: [0.0:0.2]
  !
  !  -psmoo_bdy:<delta_rho> ... smoothing parameter for pressure profile
  !        at the outer boundary (default: -psmoo <delta_rho> value).
  !        If neither -psmoo nor -psmoo_bdy are set, there is no smoothing;
  !        If only -psmoo is set, the pressure is smoothed with <delta_rho>
  !        constant over the entire profile; if only -psmoo_bdy is set,
  !        <delta_rho> varies linearly in rho from 0 to the -psmoo_bdy
  !        value; if both -psmoo and -psmoo_bdy are set, <delta_rho> varies
  !        linearly in rho from the -psmoo value at rho=0 to the -psmoo_bdy
  !        value at rho=1.  Allowed range of values: [0.0:0.2]
  !
  !  -new_state  ... forces creation of a new state containing ONLY the
  !        equilibrium data; if this is not specified, the program will
  !        try to read an existing state at <ps-path> and insert the
  !        equilibrium into it, while leaving all other data items
  !        unchanged.

  !  additional optional arguments for labeling/identification:
  !  (useful with -new_state option):

  !  -shot:<shot-number>
  !  -time:<time-in-seconds>
  !  -tok:<tokamak-ID>
 
  !--------------------------------------------------------------------------
  USE plasma_state_mod
  implicit NONE

  !-------------------------------------------

  integer :: iarg,nargs,ilen,icolon,idash,ifail,ierr,ilen_keywd
  integer :: nrho,nth,nshot,ihave_g,ihave_ps
  integer :: ilun,istat,init,iqcur
  logical :: imds,new_state,pprime_opt
  real*8 :: ztime_s,c_ratio,zt_default,rho_curbrk
  real*8 :: psmoo_axis,psmoo_bdy,rho_sm(2),delrho_sm(2)
  real*8, parameter :: ZERO = 0.0_rspec

  integer, parameter :: imax_args = 10
  character*250 gpath,pspath,args(imax_args)
  character*20 ztest,zvali,ztok,qcur_opt
  character*20 zvalr8

  !-------------------------------------------
  !  Part 1: parse command line arguments

  nrho=51
  nth=101

  nshot = -1
  zt_default = -1.2345e6_rspec
  ztime_s = zt_default
  c_ratio = 0.08_rspec
  ztok = '????'

  new_state=.FALSE.

  qcur_opt = "AUTO"
  rho_curbrk = 0.90_rspec

  pprime_opt = .FALSE.
  psmoo_axis = -1.0_rspec
  psmoo_bdy  = -1.0_rspec

  ihave_g = 0
  ihave_ps = 0
  gpath = ' '
  pspath = ' '
  imds = .FALSE.

  ierr = 0

  nargs = command_argument_count()
  if(nargs.gt.imax_args) then
     ierr=1
     write(0,*) 'geq2ps: too many arguments (max: ',imax_args, &
          '; got: ',nargs,')'
     nargs=imax_args
  endif

  do iarg=1,nargs
     call get_command_argument(iarg,args(iarg))

     ilen = len(trim(args(iarg)))

     ifail=0
     do
        ! parse arguments

        idash = 0
        if(args(iarg)(1:1).eq.'-') idash=1

        if(idash.eq.1) then
           !  command qualifer -nth or -nrho

           icolon=index(args(iarg)(1:ilen),':')
           if(icolon.le.0) then
              ilen_keywd=ilen
           else
              ilen_keywd=icolon-1
           endif
           if((ilen_keywd.le.1).or.(ilen_keywd.gt.15)) then
              ifail=1
              exit
           else
              ztest=args(iarg)(1:ilen_keywd)
              call uupper(ztest)
              if(ztest.eq.'-NEW_STATE') then
                 new_state = .TRUE.

              else if(ztest.eq.'-NRHO') then
                 call getival(iarg,nrho)
                 ! (contained subroutine; ifail=3 means value parse fails)

              else if(ztest.eq.'-NTH') then
                 call getival(iarg,nth)
                 ! (contained subroutine; ifail=3 means value parse fails)

              else if(ztest.eq.'-SHOT') then
                 call getival(iarg,nshot)
                 ! (contained subroutine; ifail=3 means value parse fails)

              else if(ztest.eq.'-TOK') then
                 call getcval(iarg,ztok)

              else if(ztest.eq.'-TIME') then
                 call getr8val(iarg,ztime_s)

              else if(ztest.eq.'-C_RATIO') then
                 call getr8val(iarg,c_ratio)
                 if((c_ratio.lt.0.02_rspec).or.(c_ratio.gt.0.15_rspec)) then
                    ifail=3
                    write(0,*) '?geq2ps: c_ratio = ',c_ratio
                    write(0,*) ' exceeds range limits: 0.02 to 0.15'
                 endif

              else if(ztest.eq.'-RHO_CURBRK') then
                 call getr8val(iarg,rho_curbrk)
                 if(rho_curbrk.lt.0.75_rspec) then
                    write(6,*) '%geq2ps: rho_curbrk set to minimum value: 0.75'
                    rho_curbrk = 0.75_rspec
                 endif
                 if(rho_curbrk.gt.0.95_rspec) then
                    write(6,*) '%geq2ps: rho_curbrk set to minimum value: 0.95'
                    rho_curbrk = 0.95_rspec
                 endif

              else if(ztest.eq.'-QCUR') then
                 call getcval(iarg,qcur_opt)
                 call uupper(qcur_opt)
                 if((qcur_opt.ne.'AUTO').and. &
                      (qcur_opt.ne.'EFIT').and.(qcur_opt.ne.'PSI')) then
                    write(0,*) '?geq2ps: qcur value: "'//trim(qcur_opt)//'".'
                    write(0,*) ' not one of {"AUTO","EFIT","PSI"}.'
                    ifail=3
                 endif

              else if(ztest.eq.'-PPRIME') then
                 pprime_opt = .TRUE.

              else if(ztest.eq.'-PSMOO') then
                 call getr8val(iarg,psmoo_axis)
                 if((psmoo_axis.lt.0.0_rspec).or. &
                      (psmoo_axis.gt.0.201_rspec)) then
                    write(0,*) '?geq2ps: -PSMOO value not in range 0.0 to 0.2.'
                    ifail=3
                 else
                    psmoo_axis = max(0.0_rspec,min(0.2_rspec,psmoo_axis))
                 endif

              else if(ztest.eq.'-PSMOO_BDY') then
                 call getr8val(iarg,psmoo_bdy)
                 if((psmoo_bdy.lt.0.0_rspec).or. &
                      (psmoo_bdy.gt.0.201_rspec)) then
                    write(0,*) '?geq2ps: -PSMOO value not in range 0.0 to 0.2.'
                    ifail=3
                 else
                    psmoo_bdy = max(0.0_rspec,min(0.2_rspec,psmoo_bdy))
                 endif

              else
                 ifail=2  ! unrecognized qualifier
              endif
           endif
        else
           ! path argument

           if(ihave_g.eq.0) then
              gpath = args(iarg)
              ihave_g = iarg
              ztest = gpath(1:5)
              call uupper(ztest)
              if(ztest.eq.'MDS+:') imds = .TRUE.

           else if(ihave_ps.eq.0) then
              pspath = args(iarg)
              ihave_ps = iarg

           else
              ifail=4  ! too many path args
           endif

        endif

        exit
     enddo

     if(ifail.eq.1) then
        ierr = ierr + 1
        write(0,*) 'geq2ps: -qualifier argument syntax error: '// &
             trim(args(iarg))
     else if(ifail.eq.2) then
        ierr = ierr + 1
        write(0,*) 'geq2ps: -qualifier argument keyword unrecognized: '// &
             trim(args(iarg))
     else if(ifail.eq.3) then
        ierr = ierr + 1
        write(0,*) 'geq2ps: -qualifier argument value parse failed: '// &
             trim(args(iarg))
     else if(ifail.eq.4) then
        ierr = ierr + 1
        write(0,*) 'geq2ps:  too many file path arguments.'
     endif
  enddo

  if(ihave_g.eq.0) then
     ierr=ierr+1
     write(0,*) 'geq2ps:  missing G-eqdsk path argument.'
  endif

  if(ihave_ps.eq.0) then
     ierr=ierr+1
     write(0,*) 'geq2ps:  missing plasma state file path argument.'
  endif

  if(ierr.gt.0) then
     write(0,*) 'geq2ps syntax:'
     write(0,*) '  geq2ps  <g-file-path OR g-MDS+-path>  <plasma-state-path>\ '
     write(0,*) '          [-nrho:<# of radial surfaces>(51)] \ '
     write(0,*) '          [-nth:<# of poloidal grid pts>(101)] \ '
     write(0,*) '          [-c_ratio:<min. bdy curvature ratio>(0.08)] \ '
     write(0,*) ' '
     write(0,*) '          [-rho_curbrk:<rho-where-I(rho)-interpolation-starts>(0.90)] \ '
     write(0,*) '          [-qcur:<AUTO | EFIT | PSI>(default keyword: AUTO)] \ '
     write(0,*) ' '
     write(0,*) '          [-pprime] \ '
     write(0,*) '          [-psmoo:<delta_rho_on_axis>(0.0)] \ '
     write(0,*) '          [-psmoo_bdy:<delta_rho_at_bdy>(0.0)] \ '
     write(0,*) ' '
     write(0,*) '          [-new_state] \ '
     write(0,*) '          [-tok:<tokamak-ID-label>] \ '
     write(0,*) '          [-shot:<shot-number>] \ '
     write(0,*) '          [-time:<time-of-g-eqdsk-equilibrium>]'
     write(0,*) ' '
     write(0,*) '  NOTES:'
     write(0,*) '    Use "-c_ratio" to control boundary offset from separatrix.'
     write(0,*) ' '
     write(0,*) '    Plasma current & q profile treatment: -qcur option:'
     write(0,*) '      "EFIT" means, use EFIT profile q(psi) and scalar current at boundary);'
     write(0,*) '      "PSI" means, rederive q(psi) and total current from Psi(R,Z) data;'
     write(0,*) '      "AUTO" means, choose "EFIT" or "PSI" based on g-eqdsk header label.'
     write(0,*) '    -rho_curbrk specifies where to stop using Psi(R,Z) contour integral'
     write(0,*) '      and commence interpolation to smoothly match enclosed current at bdy.'
     write(0,*) ' '
     write(0,*) '    Pressure profile treatment:'
     write(0,*) '    Use -pprime to reduce dP/drho noise, when the g-eqdsk'
     write(0,*) '      file is known to have good PPRIME(Psi) data.'
     write(0,*) '    Use -psmoo and -psmoo_bdy to request explicit smoothing'
     write(0,*) '      of Pressure profile.  The smoothing at location (rho)'
     write(0,*) '      is a triangular weighted average from rho-<delta_rho>'
     write(0,*) '      to rho+<delta_rho>, where <delta_rho> is linearly interpolated in rho'
     write(0,*) '      between <delta_rho_on_axis> at rho=0 and <delta_rho_at_bdy> at rho=1;'
     write(0,*) '      Defaults: <delta_rho_on_axis> is zero (no smoothing);'
     write(0,*) '                <delta_rho_at_bdy> = <delta_rho_on_axis>.'
     write(0,*) ' '
     write(0,*) '    the 2 path arguments are required; all qualifiers are'
     write(0,*) '    optional; argument default values shown in parentheses;'
     write(0,*) ' '
     write(0,*) '    If there is no file at <plasma-state-path>, -new_state is put into effect.'
     write(0,*) '    -tok, -shot & -time are used to label the -new_state output'
     write(0,*) '    plasma state file; defaults are "????", -1, and -1.2345e6;'
     write(0,*) '    so it is preferable to provide labeling when -new_state is in effect.'
     call exit(ierr)
  else
     if(imds) then
        call gshot(gpath,nshot,ztime_s)
     endif

     if(.not.new_state) then
        call find_io_unit(ilun)
        open(unit=ilun,file=trim(pspath),status='old',action='read', &
             iostat=istat)
        if(istat.ne.0) then
           write(6,*) ' '
           write(6,*) ' file not found: '//trim(pspath)
           write(6,*) ' OK -- new state will be created.'
           write(6,*) ' '
           new_state=.TRUE.
        else
           close(unit=ilun)
        endif
     endif

     psmoo_axis=max(ZERO,psmoo_axis)
     if(psmoo_bdy.lt.ZERO) psmoo_bdy=psmoo_axis

     write(6,*) ' geq2ps syntax OK:'
     write(6,*) '  g-path:  ',trim(gpath)
     write(6,*) ' ps-path:  ',trim(pspath)
     write(6,*) ' new_state: ',new_state
     write(6,*) ' '
     write(6,*) '    nrho: ',nrho
     write(6,*) '     nth: ',nth
     write(6,'(a,1pe12.5)') '  c-ratio: ',c_ratio
     write(6,*) ' '
     write(6,*) '    qcur:  ',trim(qcur_opt)
     write(6,'(a,1pe12.5)') '  rho_curbrk: ',rho_curbrk
     write(6,*) ' '
     write(6,*) ' Pprime option: ',pprime_opt
     write(6,'(a,1pe12.5)') '  Psmoo <delta_rho_on_axis>: ',psmoo_axis
     write(6,'(a,1pe12.5)') '  Psmoo <delta_rho_at_bdy>:  ',psmoo_bdy
     write(6,*) ' '
     if(ztok.ne.'????') write(6,*) '     tok:  ',trim(ztok)
     if(nshot.ne.-1) write(6,*) '    shot: ',nshot
     if(ztime_s.ne.zt_default) write(6,'(a,1pe12.5)') '  time(s): ',ztime_s

     write(6,*) ' '
  endif

  !-------------------------------------------
  !  Part 1A: read old state if necessary

  if(.not.new_state) then
     write(6,*) '...reading: '//trim(pspath)
     call ps_get_plasma_state(ierr, filename=trim(pspath), state=psp)
     if(ierr.ne.0) then
        write(6,*) '?geq2ps: failed to read pre-existing plasma state.'
        call exit(ierr)
     endif

     !  copy dimensions

     init=1
     call ps_copy_dims(psp, ps, init, ierr)
     if(ierr.ne.0) then
        write(6,*) '?geq2ps: failed to copy dimensions of pre-existing plasma state.'
        call exit(ierr)
     endif

     !  zero some dimensions -- values may be changed when G-eqdsk data
     !  is processed, below

     ps%nR = 0
     ps%nZ = 0

     ps%nrho_eq = 0
     ps%nth_eq = 0
     ps%nrho_eq_geo = 0

     ps%npsmom = 0
     ps%num_rzlim = 0

     call ps_alloc_plasma_state(ierr)  ! alloc ps accordingly
     if(ierr.ne.0) then
        write(6,*) ' ?geq2ps: failed to allocate new state.'
        call exit(ierr)
     endif

     !  dimensions are set, now copy the data (not the dimensions):
     call ps_copy_plasma_state(psp, ps, ierr, inodims=1)
     if(ierr.ne.0) then
        write(6,*) ' ?geq2ps: failed to copy data into new state.'
        call exit(ierr)
     endif
  endif

  !-------------------------------------------
  !  Part 2: execute

  !  set grid resolutions
  ps%nrho_eq = nrho
  ps%nth_eq = nth

  !  these grids will be set from the G-eqdsk data itself
  ps%nR = 0
  ps%nZ = 0

  !  provide available labeling and identifying information

  if(new_state) then
     ps%Global_Label = 'Plasma state derived from G-eqdsk: '//trim(gpath)
     ps%RunID = 'geq2ps'
  endif

  if(new_state.or.(ztok.ne.'????')) then
     ps%Tokamak_ID = trim(ztok)
  endif

  if(new_state.or.(nshot.ne.-1)) then
     ps%Shot_Number = nshot
  endif

  if(new_state.or.(ztime_s.ne.-1.2345e6_rspec)) then
     ps%t0 = ztime_s
     ps%t1 = ztime_s
  endif

  !--------------------
  !  get the equilibrium from the indicated path; use requested or defaulted
  !  curvature ratio parameter to control the boundary selection

  if(qcur_opt.eq.'AUTO') iqcur = -1
  if(qcur_opt.eq.'EFIT') iqcur = 0
  if(qcur_opt.eq.'PSI')  iqcur = 1

  rho_sm(1)=0
  rho_sm(2)=1
  delrho_sm(1)=psmoo_axis
  delrho_sm(2)=psmoo_bdy

  CALL ps_update_equilibrium(ierr, g_filepath=gpath, bdy_crat=c_ratio, &
       kcur_option=iqcur, rho_curbrk=rho_curbrk, &
       pprime_option = pprime_opt, rho_psmoo=rho_sm, delrho_psmoo=delrho_sm)
  call ckerr('ps_update_equilibrium')

  !--------------------
  !  supplement equilibrium with computation of flux surface averages and
  !  NCLASS moments

  ps%nrho_eq_geo = ps%nrho_eq
  CALL ps_alloc_plasma_state(ierr)
  call ckerr('ps_alloc_plasma_state')

  ps%rho_eq_geo = ps%rho_eq  ! use same grid
  call ps_mhdeq_derive('Everything',ierr)
  call ckerr('ps_mhdeq_derive')

  !--------------------
  !  store the result to the indicated path

  call ps_store_plasma_state(ierr, filename=trim(pspath))
  call ckerr('ps_store_plasma_state')

  !--------------------
  !  all done.
  !--------------------

  write(6,*) ' '
  write(6,*) ' %geq2ps: normal exit.'
  write(6,*) ' '

  !--------------------------------------------
CONTAINS
  !--------------------------------------------
  !  error handling routine

  subroutine ckerr(sbrtn)

    character*(*), intent(in) :: sbrtn

    if(ierr.ne.0) then
       write(0,*) ' ?geq2ps: unexpected error in subroutine '//trim(sbrtn)//'.'
       call exit(ierr)
    endif

  end subroutine ckerr
  !--------------------------------------------
  !  parse subroutines

  subroutine getival(iarg,ival)

    !  get integer value
    !    args(iarg); icolon specifies colon location

    integer, intent(in) :: iarg
    integer, intent(inout) :: ival  ! unchanged if argument value is omitted

    integer :: ilenv,istat

    if((icolon.le.0).or.(icolon.eq.ilen)) then
       ! accept current value
       return
    endif

    ilenv = ilen-icolon
    if(ilenv.gt.10) then
       ifail=3  ! value field too long
       return
    endif

    zvali=' '
    zvali(10-ilenv+1:10)=args(iarg)(icolon+1:ilen)
    read(zvali,'(I10)',iostat=istat) ival

    if(istat.ne.0) then
       ifail=3
       ival=0
    endif
  end subroutine getival

  subroutine getcval(iarg,cval)

    !  get character string value
    !    args(iarg); icolon specifies colon location

    integer, intent(in) :: iarg
    character*(*), intent(inout) :: cval

    integer :: ilenv,ilen_cval,istat

    ilen_cval=len(cval)

    if((icolon.le.0).or.(icolon.eq.ilen)) then
       ! accept current value
       return
    endif

    ilenv = ilen-icolon
    if(ilenv.gt.ilen_cval) then
       ifail=3  ! value field too long
       return
    endif

    cval=args(iarg)(icolon+1:ilen)

  end subroutine getcval

  subroutine getr8val(iarg,r8val)

    !  get integer value
    !    args(iarg); icolon specifies colon location

    integer, intent(in) :: iarg
    real*8, intent(inout) :: r8val  ! unchanged if argument value is omitted

    integer :: ilenv,istat

    if((icolon.le.0).or.(icolon.eq.ilen)) then
       ! accept current value
       return
    endif

    ilenv = ilen-icolon
    if(ilenv.gt.20) then
       ifail=3  ! value field too long
       return
    endif

    zvalr8=' '
    zvalr8(20-ilenv+1:20)=args(iarg)(icolon+1:ilen)
    read(zvalr8,'(G20.0)',iostat=istat) r8val

    if(istat.ne.0) then
       ifail=3
       r8val=0
    endif
  end subroutine getr8val

  subroutine gshot(gpath,nshot,ztime)

    ! Attempt to parse shot number and time-of-interest from MDS+ path
    ! specification.  If error occurs, silently leave nshot and ztime
    ! unchanged.

    character*(*), intent(in) :: gpath
    integer, intent(inout) :: nshot    ! may be left unchanged
    real*8, intent(inout) :: ztime     ! may be left unchanged

    !---------------------------------

    integer :: ishot
    real*8 :: ztimi

    integer :: iparen1,iparen2,ilwk,ils,ilt,isemi,istat
    character*80 zwk

    !---------------------------------
    !  find parentheses
    !    gpath of form MDS+:<server>:<tree>(<shot-number>;t=<time-of-interest>)
    !    expected.

    iparen1=index(gpath,'(')
    if(iparen1.le.0) return

    iparen2=index(gpath,')')
    if(iparen2.le.0) return

    ilwk = iparen2-iparen1-1  ! #characters btw parentheses

    if(ilwk.le.5) return      ! if too few

    zwk = gpath(iparen1+1:iparen2-1)

    !---------------------------------
    !  zwk now expected to contain <shot-number>;t=<time-of-interest>

    isemi=index(zwk,';')

    ils=isemi-1               ! length of shot number field
    ilt=ilwk-isemi-1          ! length of time field

    if(ils.le.0) return
    if(ils.gt.len(zvali)) return

    !---------------------------------
    !  shot number field has been found & length is OK

    zvali=' '
    zvali(10-ils+1:10)=zwk(1:ils)
    read(zvali,'(I10)',iostat=istat) ishot
    if(istat.ne.0) return

    ! OK -- successfully parsed the shot number

    nshot = ishot
    write(6,*) ' %geq2ps: shot number from MDS+ path: ',nshot

    !---------------------------------
    !  parse out time of interest

    if(ilt.le.0) return
    if(ilt.gt.len(zvalr8)) return

    zvalr8=' '
    zvalr8(20-ilt+1:20)=zwk(isemi+1:ilwk)
    read(zvalr8,'(G20.0)',iostat=istat) ztimi
    if(istat.ne.0) return

    ! OK -- successfully parsed the shot number

    ztime = ztimi
    write(6,'(a,1pe11.5)') ' %geq2ps: time of interest from MDS+ path: ',ztime

  end subroutine gshot

end program geq2ps
