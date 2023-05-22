program ps2jso

  !  ps2jso, plasma state to Jsolver

  !  this program reads a plasma state and writes data needed by Jsolver
  !  in a format understood by Jsolver, in a file named:  eqdska

  !  this program takes command line arguments.  Syntax:

  !     ps2jso  <ps-path>  -npsi:<# of desired psi surfaces>
  !                        -nbdy:<# of desired bdy points>

  !--------------------------------------------------------------------------
  USE plasma_state_mod
  implicit NONE
      INCLUDE 'netcdf.inc'

  !-------------------------------------------

  integer :: ii,iarg,nargs,ilen,idash,ifail,ierr,ilun,istat,nrho
  integer :: icolon,ilen_keywd
  integer :: ihave_ps

  integer, parameter :: imax_args = 5
  character*250 :: pspath,args(imax_args)
  character*10 :: ztest,zvali

  !-------------------------------------------
  !  specific to Jsolver

  REAL(KIND=rspec) :: ZERO= 0.0_rspec
  REAL(KIND=rspec) :: ONE = 1.0_rspec
  INTEGER, parameter :: iskip=10   ! thin the printed output by this factor
  REAL(KIND=rspec), parameter :: pi = 3.1415926535897931_rspec

  integer, parameter :: npsi_default = 91
  integer, parameter :: nbdy_default = 271

  integer :: i, npsi, ipsi, nbdy,ibdy
  REAL(KIND=rspec) :: mu0
  REAL(KIND=rspec) :: zintrp, Psimin, Psimax
  REAL(KIND=rspec) :: torfluxa

  REAL(KIND=rspec), dimension(:), allocatable :: psi,rhopsi,rhophi,dpsidrho, wk
  REAL(KIND=rspec), dimension(:), allocatable :: pprof,qprof,pprime,tmp
  REAL(KIND=rspec), dimension(:), allocatable :: r2i,gpsi2r2i
  REAL(KIND=rspec), dimension(:), allocatable :: Vol,Vprime, xmja2, gxmja2, satcurr 
  REAL(KIND=rspec), dimension(:), allocatable :: thbdy,rbdy,zbdy 

  REAL(KIND=rspec), dimension(:), allocatable :: jdotb, g_eq, satcurr_new

      !  added netcdf for diagnosing profiles for Jsolver
      !  may-21-2008
      integer :: NCID, RCODE, XID, TXTID
      integer :: VARXID, VARPID, VARPPID, VARCID, VARQID, VARTID, VARRID
      integer :: LABELXID, LABELPID, LABELPPID, LABELCID, LABELQID, LABELTID, LABELRID
      INTEGER  TXTLEN
      PARAMETER (TXTLEN=15)
      character*15 :: txtval

  !-------------------------------------------
  !  Part 1: parse command line arguments

  ihave_ps = 0
  pspath = ' '

  ierr = 0

  !  default values
  npsi = npsi_default
  nbdy = nbdy_default

  nargs = command_argument_count() 
  if(nargs.gt.imax_args) then
     ierr=1
     write(6,*) 'ps2jso: too many arguments (max: ',imax_args, &
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
           !  command qualifer e.g. -npsi:nnn  -nbdy:mmm

           icolon=index(args(iarg)(1:ilen),':')
           if(icolon.le.0) then
              ilen_keywd=ilen
           else
              ilen_keywd=icolon-1
           endif
           if((ilen_keywd.le.1).or.(ilen_keywd.gt.10)) then
              ifail=1
              exit
           else
              ztest=args(iarg)(1:ilen_keywd)
              call uupper(ztest)

              if(ztest.eq.'-NPSI') then
                 call getival(iarg,npsi)

              else if(ztest.eq.'-NBDY') then
                 call getival(iarg,nbdy)

              else
                 ifail=2  ! unrecognized qualifier
              endif
           endif

        else
           ! path argument

           if(ihave_ps.eq.0) then
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
        write(6,*) 'ps2jso: -qualifier argument not valid: '// &
             trim(args(iarg))
     else if(ifail.eq.2) then
        ierr = ierr + 1
        write(6,*) 'ps2jso: -qualifier argument keyword unrecognized: '// &
             trim(args(iarg))
     else if(ifail.eq.3) then
        ierr = ierr + 1
        write(6,*) 'ps2jso: -qualifier argument value parse failed: '// &
             trim(args(iarg))
     else if(ifail.eq.4) then
        ierr = ierr + 1
        write(6,*) 'ps2jso:  too many file path arguments.'
     endif
  enddo

  if(ihave_ps.eq.0) then
     ierr=ierr+1
     write(6,*) 'ps2jso:  missing plasma state file path argument.'
  endif

  if(ierr.gt.0) then
     write(6,*) 'ps2jso syntax:'
     write(6,*) '  ps2jso  <plasma-state-path> \ '
     write(6,*) '           [-npsi:<#-of-psi-surfaces>] \ '
     write(6,*) '           [-nbdy:<#-of-bdy-points>] \ '
     write(6,*) '  the plasma state path argument is required.'
     write(6,*) '  default values:  npsi = ',npsi_default, &
          '  nbdy = ',nbdy_default
     call exit(ierr)
  else
     write(6,*) ' ps2jso syntax OK:'
     write(6,*) ' ps-path:  ',trim(pspath)
     write(6,*) '    npsi = ',npsi
     write(6,*) '    nbdy = ',nbdy
  endif

  !---------------------
  !  check existence of input file

  call find_io_unit(ilun)
  open(unit=ilun,file=trim(pspath),status='old',action='read',iostat=istat)
  if(istat.ne.0) then
     write(6,*) ' '
     write(6,*) ' file not found: '//trim(pspath)
     write(6,*) ' '
     call exit(4)
  endif
  close(unit=ilun)

  !-------------------------------------------
  !  Part 2: execute

  write(6,*) ' '
  write(6,*) ' %ps2jso: reading the state...'

  call ps_get_plasma_state(ierr, filename=trim(pspath))
  if(ierr.ne.0) then
     write(6,*) '?ps2jso: failed to read input plasma state.'
     call exit(ierr)
  endif

  !  check whether the state contains an equilibrium...

  if((ps%nrho_eq.eq.0).or.(ps%nth_eq.eq.0)) then
     write(6,*) '?ps2jso: no equilibrium data in Plasma State.'
     call exit(9)
  endif

  nrho = ps%nrho_eq

  if(ps%nrho_eq_geo.eq.0) then
     !  create grid for 1d flux surface averaged metrics, if it does
     !  not already exist

     write(6,*) ' %ps2jso: creating plasma state rho_eq_geo grid...'

     ps%nrho_eq_geo = nrho
     call ps_alloc_plasma_state(ierr)
     call ckerr('ps_alloc_plasma_state for ps%nrho_eq_geo')

     ps%rho_eq_geo(1)=0.0_rspec
     ps%rho_eq_geo(nrho)=1.0_rspec
     do ii=2,nrho-1
        ! evenly spaced
        ps%rho_eq_geo(ii) = (ii-1)*1.0_rspec/(nrho-1)
     enddo
  endif

  ! update the 1d flux surface averaged metrics

  write(6,*) ' '
  write(6,*) &
       ' %ps2jso: check/update volumes and 1d flux surface averaged metrics...'

  call ps_mhdeq_derive('Everything', ierr)
  call ckerr('ps_mhdeq_derive')


  !--------------------
  !  Part 3: Jsolver specific code starts here 

  !==========================================================================
  ! (f3-jinchen) form Psi grid (from equa-distanced toroidal flux)
  !     here I set it to 3 x [the rho grid size]

  write(6,*) ' '
  write(6,*) ' -------------------------------------- '
  write(6,*) ' '
  write(6,*) ' %ps2jso: JSolver code starts ...'
  write(6,*) ' '
  write(6,*) ' form rho(Psi) grid... '

  !  Psi in plasma state: Wb/rad -- Wb for Webers, Wb/rad for Webers per Radian
  !  |Bpol| = |grad(Psi)|/R
  
  torfluxa = ps%phit(ps%nrho_eq)  ! enclosed toroidal flux at bdy, Wb.
  write(6,'(a,1pe12.5)') '  Tor. Flux (Wb) at edge: ',torfluxa
  write(6,*) ' '
  
  Psimin = ps%psipol(1) ! (usually zero)
  Psimax = ps%psipol(ps%nrho_eq)

  allocate(psi(npsi),rhopsi(npsi))

  ! form evenly spaced psi grid
  do ii = 1, npsi
     psi(ii) = ( (ii-1)*Psimax + (npsi-ii)*Psimin )/(npsi-1)  ! Wb/rad
  enddo

  !  compute rhopsi:
  call ps_psimap(psi,rhopsi,ierr)  ! -> Psi_spline(rhopsi(ii)) = psi(ii)
  
  write(6,'(a,1pe12.5)') '  Psi (Wb/rad) on axis: ',psi(1)
  write(6,'(a,1pe12.5)') '  Psi (Wb/rad) at edge: ',psi(npsi)
  
  !==========================================================================
  ! (f4-jinchen) retrieve profiles vs. Psi
  !  rhopsi can now be used to interpolate profiles to the Psi grid...
  mu0 = 4. * pi * 1.e-7_rspec

  allocate(pprof(npsi),qprof(npsi))

  call ps_intrp_1d(rhopsi, ps%id_P_eq, pprof, ierr); call ckerr('P_eq')   ! P
  call ps_intrp_1d(rhopsi, ps%id_q_eq, qprof, ierr); call ckerr('q_eq')   ! q
  pprof = mu0 * pprof
  
  ! form d(Psi)/drho; use to convert rho derivatives into Psi derivatives...
  
  allocate(dpsidrho(npsi))
  dpsidrho = rhopsi*torfluxa/(qprof*pi)  ! d(Psi)/d(rho)
  
  ! the above from torflux(rho) = torfluxa*rho**2;
  !    d(torflux)/drho = torfluxa*2*rho
  ! and q = d(torflux)/d(polflux); factor 1/2pi due to Psi in Wb/rad,
  ! and torfluxa in Wb.

  ! *** CAUTION:  dpsidrho = ZERO on axis ***

  !  <1/R**2> and <|grad(Psi)**2|/R**2> profiles:
  !  state profile interpolations available...
  allocate(r2i(npsi),gpsi2r2i(npsi))
  call ps_intrp_1d(rhopsi, ps%id_gr2i, r2i, ierr); call ckerr('<1/R**2>')
  call ps_intrp_1d(rhopsi, ps%id_grho2r2i, gpsi2r2i, ierr); call ckerr('<|grad(rho)**2|/R**2>')
  gpsi2r2i = gpsi2r2i * dpsidrho**2  ! grad(rho)**2/R**2 --> grad(Psi)**2/R**2
  gpsi2r2i(1) = 2.*gpsi2r2i(2) - gpsi2r2i(3)

  !  Pprime  --  interpolation on 1st derivative gives dP/drho
  allocate(pprime(npsi))
  allocate(tmp(npsi))
  call ps_intrp_1d(rhopsi, ps%id_P_eq, pprime, ierr,  ideriv=1)
  call ckerr('d(P_eq)/drho')

  tmp = pprime

  !  dP/dpsi = dP/drho * (dPsi/drho)**-1 BUT dPsi/drho --> 0 as rho --> 0
  !  at rho=0 one could evaluate the ratio d2P/drho2 / d2Psi/drho2
  !  but here I just set pprime(1) = 0; the 2nd derivatives are likely
  !  to be dependent on interpolation details

  pprime(2:npsi) = pprime(2:npsi)/dpsidrho(2:npsi)
  pprime(1) = 2.*pprime(2) - pprime(3)
  pprime(npsi) = ZERO
  pprime = mu0 * pprime

  write(6,*) " "
  write(6,*) "Psi         q(Psi)      P(psi)      P'(psi)     <1/R**2>    <grad(Psi)**2/R**2>"
  write(6,*) " Wb/rad                  Pa          Pa/(Wb/rad) m**-2       (Wb/rad)**2/m**2"
  do ipsi = 1,npsi,iskip
     write(6,'(6(1x,1pe11.4))') psi(ipsi),qprof(ipsi),pprof(ipsi), &
          pprime(ipsi),r2i(ipsi),gpsi2r2i(ipsi)
  enddo

  !==========================================================================
  ! (f5-jinchen) retrieve plasma boundary
  !  retrieve the plasma boundary from the state
  !  this is placed in somewhat from the separatrix (if any)

  allocate(wk(nbdy),thbdy(nbdy),rbdy(nbdy),zbdy(nbdy))

  wk = ONE   ! nbdy rho=ONE values -- the boundary rho coordinate
  do ibdy=1,nbdy
  !  thbdy(ibdy) = (ibdy-1)*(2*pi)/(nbdy-1)  ! 0:2pi equispaced
  !  thbdy(ibdy) = -PI + (ibdy-1)*2*PI/(nbdy-1)  ! -pi:pi
     thbdy(ibdy) =  PI - (ibdy-1)*2*PI/(nbdy-1)  ! pi:-pi
  enddo

  !  the equilibrium geometry in the state uses equal arc theta
  !  as theta goes from 0 to 2pi the boundary is drawn in a counter
  !  clockwise direction (when viewing the plasma cross section drawn
  !  to the right of the tokamak axis of symmetry).

  call ps_intrp_2d(wk, thbdy, ps%id_R_geo, rbdy, ierr); call ckerr('R_geo')
  call ps_intrp_2d(wk, thbdy, ps%id_Z_geo, zbdy, ierr); call ckerr('Z_geo')

  write(6,*) " "
  write(6,*) ' Sampling of plasma boundary; R & Z in meters:'
  write(6,*) ' index       R            Z'
  do ibdy=1,nbdy,iskip
     write(6,'(2x,i3,5x,2(1x,1pe12.5))') ibdy,rbdy(ibdy),zbdy(ibdy)
  enddo

  !==========================================================================
  ! (f6-jinchen) get volume and its derivative

  !  Vol  --  interpolation V(rho)

  allocate(Vol(npsi))
  call ps_intrp_1d(rhopsi, ps%id_vol, Vol, ierr); call ckerr('Vol')

  !  Vprime  --  interpolation on 1st derivative gives dV(rho)/drho

  allocate(Vprime(npsi))
  call ps_intrp_1d(rhopsi, ps%id_vol, Vprime, ierr,  ideriv=1); call ckerr('Vprime') 

  !==========================================================================
  ! (f7-jinchen) dump tsc-type data eqdsk for Jsolver
  allocate(satcurr(npsi), xmja2(npsi), gxmja2(npsi))
  satcurr = 0.
  xmja2 = 0.
  gxmja2 = 0.

  xmja2(2:npsi) = 2*pi * qprof(2:npsi) * Vprime(2:npsi)*r2i(2:npsi) / (2*rhopsi(2:npsi)*torfluxa)
  gxmja2(2:npsi) = 2*pi * qprof(2:npsi) * Vprime(2:npsi)*gpsi2r2i(2:npsi) / (2*rhopsi(2:npsi)*torfluxa)

  ! zintrp is normalized toroidal flux increment
  do ipsi=2,npsi-1
     zintrp = ONE/(npsi-1)
     satcurr(ipsi) = 2*pi*qprof(ipsi)**2 / (xmja2(ipsi)**2) *  &
      ( xmja2(ipsi+1)*gxmja2(ipsi+1)/qprof(ipsi+1) - xmja2(ipsi-1)*gxmja2(ipsi-1)/qprof(ipsi-1) ) / ( 2.*zintrp*torfluxa)
  enddo

  !==========================================================================
  ! (f71-jinchen) new method to calculate averaged current
  allocate(jdotb(npsi), satcurr_new(npsi), g_eq(npsi))

  !  B=grad phi x grad PSI + g_eq grad phi
  call ps_intrp_1d(rhopsi, ps%id_g_eq, g_eq, ierr); call ckerr('g_eq')
  call ps_intrp_1d(rhopsi, ps%id_jdotb, jdotb, ierr); call ckerr('jdotb')
  satcurr_new(1:npsi) = mu0 * jdotb(1:npsi) / g_eq(1:npsi) / r2i(1:npsi) 

  !==========================================================================
  ! (f8-jinchen) output

 ! for f7
 !call output_for_jsolver(npsi, nbdy, pprof, pprime, satcurr,     psi, rbdy, zbdy, ierr)
 ! for f71
  call output_for_jsolver(npsi, nbdy, pprof, pprime, satcurr_new, psi, rbdy, zbdy, ierr)
  call ckerr('output_for_jsolver')

  write(6,*) " "
  write(6,*) "tsc-type data eqdsk for Jsolver "
  write(6,*) "Psi         q(Psi)      xmja2(psi)      gxmja2(psi)"
  do ipsi = 1,npsi
     write(6,'(4(1x,1pe11.4))') psi(ipsi),qprof(ipsi),xmja2(ipsi), gxmja2(ipsi)
  enddo

  !==========================================================================
  ! (f81-jinchen) netCDF output

      !  added netcdf for diagnosing profiles for Jsolver
      !  may-21-2008
  ! 07/19/2013 CLF: nccre, ncvdef, etc. are netcdf 2 - totally obsolete
      !NCID = NCCRE('ps2jso.cdf', NCCLOB, RCODE)
      RCODE = nf_create('ps2jso.cdf', NF_CLOBBER, NCID)

      print *, 'NCID = ', NCID

      !XID = NCDDEF(NCID, 'npsi', npsi, RCODE)
      rcode = nf_def_dim(NCID, 'npsi', npsi, XID)
      print *, 'XID = ', XID

      !TXTID = NCDDEF(NCID, 'len', TXTLEN, RCODE)
      rcode = nf_def_dim(NCID, 'len', TXTLEN, TXTID)
      print *, 'TXTID = ', TXTID


      !VARXID = nf_def_var(NCID, 'psi', NF_DOUBLE, 1, XID, RCODE)
      rcode = nf_def_var(NCID, 'psi', NF_DOUBLE, 1, XID, VARXID)
      print *, 'VARXID = ', VARXID
      rcode = nf_def_var(NCID, 'p', NF_DOUBLE, 1, XID, VARPID)
      print *, 'VARPID = ', VARPID
      rcode = nf_def_var(NCID, 'pp', NF_DOUBLE, 1, XID, VARPPID)
      print *, 'VARPPID = ', VARPPID
      rcode = nf_def_var(NCID, 'c', NF_DOUBLE, 1, XID, VARCID )
      print *, 'VARCID = ', VARCID
      rcode = nf_def_var(NCID, 'q', NF_DOUBLE, 1, XID, VARQID)
      print *, 'VARQID = ', VARQID
      !VARTID = NCVDEF(NCID, 'tmp', NF_DOUBLE, 1, XID, RCODE)
      !print *, 'VARTID = ', VARTID
      !VARRID = NCVDEF(NCID, 'dpsidrho', NF_DOUBLE, 1, XID, RCODE)
      !print *, 'VARRID = ', VARRID

      rcode = nf_def_var(NCID, 'psi_label', NF_CHAR, 1, TXTID, LABELXID)
      print *, 'LABELXID = ', LABELXID
      rcode = nf_def_var(NCID, 'p_label', NF_CHAR, 1, TXTID, LABELPID)
      print *, 'LABELPID = ', LABELPID
      rcode = nf_def_var(NCID, 'pp_label', NF_CHAR, 1, TXTID, LABELPPID)
      print *, 'LABELPPID = ', LABELPPID
      rcode = nf_def_var(NCID, 'c_label', NF_CHAR, 1, TXTID,LABELCID )
      print *, 'LABELCID = ', LABELCID
      rcode = nf_def_var(NCID, 'q_label', NF_CHAR, 1, TXTID, LABELQID)
      print *, 'LABELQID = ', LABELQID
      !LABELTID = NCVDEF(NCID, 't_label', NF_CHAR, 1, TXTID, RCODE)
      !print *, 'LABELTID = ', LABELTID
      !LABELRID = NCVDEF(NCID, 'rho_label', NF_CHAR, 1, TXTID, RCODE)
      !print *, 'LABELRID = ', LABELRID

      CALL NCENDF(NCID, RCODE)

      CALL NCVPT(NCID, VARXID, 1, npsi, psi, RCODE)
      CALL NCVPT(NCID, VARPID, 1, npsi, pprof, RCODE)
      CALL NCVPT(NCID, VARPPID, 1, npsi, pprime, RCODE)
      CALL NCVPT(NCID, VARCID, 1, npsi, satcurr_new, RCODE)
      CALL NCVPT(NCID, VARQID, 1, npsi, qprof, RCODE)
      !CALL NCVPT(NCID, VARTID, 1, npsi, tmp, RCODE)
      !CALL NCVPT(NCID, VARRID, 1, npsi, dpsidrho, RCODE)

      txtval = "psi"
      CALL NCVPTC(NCID, LABELXID, 1, TXTLEN, txtval, TXTLEN, RCODE)
      txtval = "p"
      CALL NCVPTC(NCID, LABELPID, 1, TXTLEN, txtval, TXTLEN, RCODE)
      txtval = "pp"
      CALL NCVPTC(NCID, LABELPPID, 1, TXTLEN, txtval, TXTLEN, RCODE)
      txtval = "||c"
      CALL NCVPTC(NCID, LABELCID, 1, TXTLEN, txtval, TXTLEN, RCODE)
      txtval = "q"
      CALL NCVPTC(NCID, LABELQID, 1, TXTLEN, txtval, TXTLEN, RCODE)

      !CALL NCVPTC(NCID, LABELTID, 1, TXTLEN, "tmp", TXTLEN, RCODE)
      !CALL NCVPTC(NCID, LABELRID, 1, TXTLEN, "dpsidrho", TXTLEN, RCODE)

      CALL NCCLOS(NCID, RCODE)


  !--------------------
  !  all done.
  !--------------------

  deallocate(psi)
  write(6,*) ' '
  write(6,*) ' %ps2jso: normal exit.'
  write(6,*) ' '

  !--------------------------------------------
CONTAINS
  !--------------------------------------------
  !  error handling routine

  subroutine ckerr(sbrtn)

    character*(*), intent(in) :: sbrtn

    if(ierr.ne.0) then
       write(6,*) ' ?ps2jso: unexpected error in subroutine '//trim(sbrtn)//'.'
       call exit(ierr)
    endif

  end subroutine ckerr

  !  parse subroutines

  !  DMC: integers only; see geq2ps for additional contained parse routines
  !  if needed

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

    subroutine output_for_jsolver(npsit, nbdy, prpest0, pppest0, ajpest2, xsv2, xpass, zpass, ierr)

      integer :: neqdsk, j, k, ncycle, isyms, ipest, npsit, nbdy, kmax, ierr
      REAL(KIND=rspec) :: prpest0(npsit), pppest0(npsit), ajpest2(npsit), xsv2(npsit)
      REAL(KIND=rspec) :: xpass(nbdy), zpass(nbdy)
      REAL(KIND=rspec) :: times,xaxes,zmags,gzeros,apls,betas,betaps,ali2s,qsaws,psimins,psilims


      write(6,*) " "
      write(6,*) ' Output eq data for jsolver -- May 15, 2007'

      ncycle=  1
      isyms=  0
      ipest=  1
      kmax=nbdy-1

      times=  0.1140E-01
      xaxes=  ps%R_axis
      zmags=  ps%Z_axis
      apls=   0.3656E+05
      betas=  0.5184E-02
      betaps= 0.1472E+00
      ali2s=  0.5458E+00
      qsaws=  0.5000E+00
      psimins=psi(1)
      psilims=psi(npsi)

      !  gzeros= R * B_T in m-T   (at vacuum)
      call ps_intrp_1d(ONE, ps%id_g_eq, gzeros, ierr); call ckerr('gzeros')

      neqdsk = 31
      open(neqdsk,file='eqdska',status='unknown',form='unformatted')

      write (neqdsk) ncycle,isyms,ipest,npsit,kmax
      write(6,1000) ncycle,isyms,ipest,npsit,kmax

      write(neqdsk) times,xaxes,zmags,gzeros,apls,betas,betaps, &
                   ali2s,qsaws,psimins,psilims
      write(6,3000) times,xaxes,zmags,gzeros,apls,betas,betaps, &
                   ali2s,qsaws,psimins,psilims

      write(neqdsk) (prpest0(j),j=1,npsit)
      write(neqdsk) (pppest0(j),j=1,npsit)
      write(neqdsk) (ajpest2(j),j=1,npsit)
      write(neqdsk) (xsv2(j),j=1,npsit)
      write(neqdsk) (xpass(k),k=1,kmax+1)
      write(neqdsk) (zpass(k),k=1,kmax+1)

 1000 format(" *** data file write...ncycle=",i6,", isyms=",i3, &
            ", ipest=",i3,", npsit=",i4,", kmax=",i5,"  ***")
 3000 format("    times=",e12.4,"    xaxes=",e12.4,"    zmags=",e12.4, &
         /, "   gzeros=",e12.4,"     apls=",e12.4,"    betas=",e12.4, &
         /, "   betaps=",e12.4,"    ali2s=",e12.4,"    qsaws=",e12.4, &
         /, "  psimins=",e12.4,"  psilims=",e12.4)


      write(6,*) " "
      write(6,*) ' Output eq data for jsolver -- end'
    end subroutine output_for_jsolver

end program ps2jso
