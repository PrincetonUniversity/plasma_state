program ps2geq

  !  DMC Apr 2008: ps2geq, plasma state to G-eqdsk

  !  this program writes a geqdsk file from the data contained in a plasma
  !  state.  The plasma state must contain an equilibrium description and
  !  related data including:

  !      Psi(R,Z)          
  !      [R,Z](rho,theta)
  !      P(rho)
  !      q(rho)
  !      limiter [R,Z] closed contour

  !  these are all normal elements of the plasma state definition, but, not
  !  every plasma state instance has all of them defined.  If this program
  !  receives a state with required elements missing, it will report an
  !  error.

  !  this program takes command line arguments.  Syntax:

  !     ps2geq  <ps-path>  <geq-path>

  !  if <geq-path> ends in "/", the state variable ps%eqdsk_file sets the
  !  name of the file written; if not, <geq-path> is assumed to be the full
  !  filename for the file to be written. 
  !--------------------------------------------------------------------------
  USE plasma_state_mod
  implicit NONE

  !-------------------------------------------

  integer :: iarg,nargs,ilen,idash,ifail,ierr,ilun,istat
  integer :: ihave_g,ihave_ps

  integer, parameter :: imax_args = 3
  character*250 gpath,pspath,args(imax_args)

  !-------------------------------------------
  !  Part 1: parse command line arguments

  ihave_g = 0
  ihave_ps = 0
  gpath = ' '
  pspath = ' '

  ierr = 0

  nargs = command_argument_count()
  if(nargs.gt.imax_args) then
     ierr=1
     write(0,*) 'ps2geq: too many arguments (max: ',imax_args, &
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
           ifail=1
           exit

        else
           ! path argument

           if(ihave_ps.eq.0) then
              pspath = args(iarg)
              ihave_ps = iarg
           else if(ihave_g.eq.0) then
              gpath = args(iarg)
              ihave_g = iarg
           else
              ifail=4  ! too many path args
           endif

        endif

        exit
     enddo

     if(ifail.eq.1) then
        ierr = ierr + 1
        write(0,*) 'ps2geq: -qualifier argument not valid: '// &
             trim(args(iarg))
     else if(ifail.eq.4) then
        ierr = ierr + 1
        write(0,*) 'ps2geq:  too many file path arguments.'
     endif
  enddo

  if(ihave_g.eq.0) then
     ierr=ierr+1
     write(0,*) 'ps2geq:  missing G-eqdsk path argument.'
  endif

  if(ihave_ps.eq.0) then
     ierr=ierr+1
     write(0,*) 'ps2geq:  missing plasma state file path argument.'
  endif

  if(ierr.gt.0) then
     write(0,*) 'ps2geq syntax:'
     write(0,*) '  ps2geq  <plasma-state-path>  <g-file-path>'
     write(0,*) '  the 2 path arguments are required.'
     call exit(ierr)
  else
     write(6,*) ' ps2geq syntax OK:'
     write(6,*) ' ps-path:  ',trim(pspath)
     write(6,*) '  g-path:  ',trim(gpath)
  endif

  !---------------------
  !  check existence of input file

  call find_io_unit(ilun)
  open(unit=ilun,file=trim(pspath),status='old',action='read',iostat=istat)
  if(istat.ne.0) then
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
     write(0,*) '?ps2geq: failed to read input plasma state.'
     call exit(ierr)
  endif

  ilen = len(trim(gpath))
  if(gpath(ilen:ilen).eq.'/') then
     if(ps%eqdsk_file.eq.' ') then
        write(6,*) ' ps2geq: ps%eqdsk_file is blank; default name used: geqdsk.dat'
        ps%eqdsk_file = 'geqdsk.dat'
     else
        write(6,*) ' ps2geq: ps%eqdsk_file contents: ',trim(ps%eqdsk_file)
     endif
     gpath(ilen+1:) = trim(ps%eqdsk_file)
     write(6,*) ' Full g-eqdsk file path:  ',trim(gpath)
  endif

  call ps_wr_geqdsk(ierr, g_filepath=trim(gpath))
  if(ierr.ne.0) then
     write(0,*) '?ps2geq: failed to read input plasma state.'
     call exit(ierr)
  endif

  !--------------------
  !  all done.
  !--------------------

  write(6,*) ' '
  write(6,*) ' %ps2geq: normal exit.'
  write(6,*) ' '

  !--------------------------------------------
CONTAINS
  !--------------------------------------------
  !  error handling routine

  subroutine ckerr(sbrtn)

    character*(*), intent(in) :: sbrtn

    if(ierr.ne.0) then
       write(0,*) ' ?ps2geq: unexpected error in subroutine '//trim(sbrtn)//'.'
       call exit(ierr)
    endif

  end subroutine ckerr

end program ps2geq
