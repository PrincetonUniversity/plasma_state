program ps_adas_test

  !  DMC Jun 2009: ps_adas_test program:
  !    This program reads a Plasma State file, to define a target plasma
  !    for testing of ADAS beam deposition data.  The Plasma State defines
  !    the target plasma species, densities and temperatures.

  !    Note: only the Plasma State temperature and density data profiles
  !    are used-- not the beam geometry description.

  !    The program also takes the beam Zb, Ab, and injection energy Eb (KeV).
  !    The atomic number and mass (AMU) are integers; Zb=Ab=1 are defaults;
  !    Plasma State file name and Eb are required inputs.

  !    Example invocations:

  !       ps_adas_test -ps:data.ps -Eb:50.0
  !          ...read state data from file "data.ps"; evaluate rates for
  !             an H-isotope beam, 50 KeV/nucleon.

  !       ps_adas_test -ps:data.ps -Eb:100.0 -Zb:1 -Ab:2
  !          ...this is equivalent to the first example (i.e. 50 KeV/nucleon,
  !             specified as a 100KeV D0 beam).

  !    At present only Zb=1 is supported.

  !--------------------------------------------------------------------------
  USE plasma_state_mod
  implicit NONE
  !-------------------------------------------

  integer :: iarg,nargs,ilen,idash,icolon,ilun,ierr,iverr

  integer :: Ab,Zb        ! beam species
  real*8  :: Eb           ! beam energy, KeV, converted to KeV/nucleon
  integer :: ihave_ps     ! =1: -ps argument has been seen.

  integer, parameter :: imax_args = 4
  character*250 :: pspath              ! plasma state file
  character*250 :: args(imax_args)     ! command arguments (text)
  character*10 :: arg_name             ! argument name
  character*10 :: zival                ! encoded integer value
  character*20 :: zfval                ! encoded floating value

  real*8, parameter :: ZERO = 0.0d0

  !-------------------------------------------
  !  Part 1: parse command line arguments

  ihave_ps = 0
  pspath = ' '

  Ab=1
  Zb=1
  Eb=ZERO

  iverr=0
  ierr = 0

  nargs = command_argument_count() 
  if(nargs.gt.imax_args) then
     ierr=1
     write(0,*) 'ps_adas_test: too many arguments (max: ',imax_args, &
          '; got: ',nargs,')'
     nargs=imax_args
  endif

  do iarg=1,nargs
     !  loop over each argument

     call get_command_argument(iarg,args(iarg))

     icolon = index(args(iarg),':')
     if((args(iarg)(1:1).ne.'-').or.(icolon.le.1)) then
        ierr = 1
        write(0,*) 'ps_adas_test: argument syntax -<name>:<value>; found: ', &
             args(iarg)
        cycle
     endif

     arg_name = args(iarg)(2:icolon-1)
     ilen = len(trim(args(iarg)))

     if(arg_name.eq.'ps') then
        pspath = args(iarg)(icolon+1:)
        ihave_ps = 1

     else if(arg_name.eq.'Ab') then
        Ab = iarg_decode('Ab')
        if((Ab.lt.1).or.(Ab.gt.4)) then
           write(0,*) ' ?ps_adas_test: Ab value not in range 1 to 4: ', &
                trim(args(iarg))
           iverr=1
        endif

     else if(arg_name.eq.'Zb') then
        Zb = iarg_decode('Zb')
        if((Zb.lt.1).or.(Zb.gt.1)) then
           write(0,*) ' ?ps_adas_test: Zb value not equal to one: ', &
                trim(args(iarg))
           iverr=1
        endif

     else if(arg_name.eq.'Eb') then
        Eb = r8arg_decode('Eb')
        if(Eb.le.ZERO) then
           write(0,*) ' ?ps_adas_test: Eb must be positive: ',trim(args(iarg))
           iverr=1
        endif

     else
        ierr = 1
        write(0,*) '?ps_adas_test: argument name "'//trim(arg_name)// &
             '" unrecognized, in: ',trim(args(iarg))
     endif

  enddo

  if(ihave_ps.eq.0) then
     ierr=ierr+1
     write(0,*) 'ps_adas_test:  missing plasma state file path argument.'
  endif

  if((ierr.ne.0).or.(iverr.ne.0)) then
     if(ierr.ne.0) then
        write(0,*) 'ps_adas_test syntax:'
        write(0,*) '  ps_adas_test -ps:<plasma-state-path> -Eb:<beam-energy, KeV> '// &
             ' -Ab:<beam-mass, AMU> -Zb:<beam-atomic-no.>'
        write(0,*) '    example: ps_adas_test -ps:foo.ps -Zb:1 -Ab:2 -Eb:100.0'
     endif
     if(iverr.ne.0) then
        write(0,*) ' supported range for Ab (AMU): 1 to 4'
        write(0,*) ' supported range for Zb (atomic number): 1 to 1'
        write(0,*) ' supported range for Eb: greater than zero.'
     endif
     if(iverr.ne.0) ierr=2
     call exit(ierr)
  else
     write(6,*) ' ps_adas_test syntax OK:'
     write(6,'(3x,a,a)')  ' ps-path:  ',trim(pspath)
     write(6,'(3x,a,i2)') '      Ab:  ',Ab
     write(6,'(3x,a,i2)') '      Zb:  ',Zb
     write(6,'(3x,a,1pe13.6)') ' Eb(KeV):  ',Eb
  endif

  !---------------------
  !  check existence of input file

  call find_io_unit(ilun)
  open(unit=ilun,file=trim(pspath),status='old',action='read',iostat=ierr)
  if(ierr.ne.0) then
     write(0,*) ' '
     write(0,*) ' file not found: '//trim(pspath)
     write(0,*) ' '
     call exit(4)
  endif
  close(unit=ilun)

  !-------------------------------------------
  !  Part 2: execute

  call ps_get_plasma_state(ierr, filename=trim(pspath))
  if(ierr.ne.0) then
     write(0,*) '?ps_adas_test: failed to read input plasma state.'
     call exit(ierr)
  endif

  write(6,*) ' '
  write(6,*) ' Plasma state file read successfully.'
  write(6,*) ' '
  write(6,'(a,i3)') ' Number of thermal ion species: ',ps%nspec_tha
  write(6,*)        '   (list may not include all charge states).'
  write(6,'(a,i3)') ' Number of beam ion species: ',ps%nspec_beam
  write(6,'(a,i3)') ' Number of fusion product species: ',ps%nspec_fusion
  write(6,'(a,i3)') ' Number of RF minority  species: ',ps%nspec_rfmin

  write(6,*) ' '
  write(6,'(a,i3)') ' Number of flux zones, thermal ions: ',ps%nrho-1
  write(6,'(a,i3)') ' Number of flux zones, beam ion: ',ps%nrho_nbi-1
  write(6,'(a,i3)') ' Number of flux zones, fusion products: ',ps%nrho_fus-1
  write(6,'(a,i3)') ' Number of flux zones, RF minorities: ',ps%nrho_icrf-1

  !--------------------
  !  all done.
  !--------------------

  write(6,*) ' '
  write(6,*) ' %ps_adas_test: normal exit.'
  write(6,*) ' '

  !--------------------------------------------
CONTAINS
  !--------------------------------------------
  !  parsing routines

  integer function iarg_decode(zname)
    character*(*), intent(in) :: zname

    integer :: ilv,ival,istat

    ilv = ilen-icolon
    if(ilv.gt.len(zival)) then
       ierr=ierr+1
       write(0,*) ' ?ps_adas_test: '//trim(zname)//' value field too long: '//&
            trim(args(iarg))
       iarg_decode=-1
       return
    endif

    zival=' '
    zival(10-ilv+1:10)=args(iarg)(icolon+1:ilen)
    read(zival,'(I10)',iostat=istat) ival

    if(istat.ne.0) then
       write(0,*) ' ?ps_adas_test: integer value decode failure: '// &
            trim(args(iarg))
       iarg_decode=-1
       ierr=1
       return
    endif

    iarg_decode = ival
  end function iarg_decode

  real*8 function r8arg_decode(zname)
    character*(*), intent(in) :: zname

    integer :: ilv,istat
    real*8 :: r8val

    ilv = ilen-icolon
    if(ilv.gt.len(zfval)) then
       ierr=ierr+1
       write(0,*) ' ?ps_adas_test: '//trim(zname)//' value field too long: '//&
            trim(args(iarg))
       r8arg_decode=-1.0d0
       return
    endif

    zfval=' '
    zfval(20-ilv+1:20)=args(iarg)(icolon+1:ilen)
    read(zfval,'(G20.0)',iostat=istat) r8val

    if(istat.ne.0) then
       write(0,*) ' ?ps_adas_test: real*8 value decode failure: '// &
            trim(args(iarg))
       r8arg_decode=-1.0d0
       ierr=1
       return
    endif

    r8arg_decode = r8val
  end function r8arg_decode

end program ps_adas_test
