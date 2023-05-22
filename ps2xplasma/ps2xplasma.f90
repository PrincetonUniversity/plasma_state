program ps2xplasma

  !  DMC Apr 2008: ps2xplasma, plasma state file to xplasma file

  !  this program writes an xplasma NetCDF file from the data contained in a 
  !  plasma state NetCDF file.

  !  this program takes two command line arguments.  Syntax:

  !     ps2xplasma  <ps-path>  <xplasma-path>

  !--------------------------------------------------------------------------
  USE plasma_state_mod
  implicit NONE

  !-------------------------------------------

  integer :: iarg,nargs,ilen,idash,ifail,ierr,ilun,istat
  integer :: ihave_xp,ihave_ps

  integer, parameter :: imax_args = 3
  character*250 xpath,pspath,args(imax_args)

  !-------------------------------------------
  !  Part 1: parse command line arguments

  ihave_xp = 0
  ihave_ps = 0
  xpath = ' '
  pspath = ' '

  ierr = 0

  nargs = command_argument_count()
  if(nargs.gt.imax_args) then
     ierr=1
     write(0,*) 'ps2xplasma: too many arguments (max: ',imax_args, &
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
           else if(ihave_xp.eq.0) then
              xpath = args(iarg)
              ihave_xp = iarg
           else
              ifail=4  ! too many path args
           endif

        endif

        exit
     enddo

     if(ifail.eq.1) then
        ierr = ierr + 1
        write(0,*) 'ps2xplasma: -qualifier argument not valid: '// &
             trim(args(iarg))
     else if(ifail.eq.4) then
        ierr = ierr + 1
        write(0,*) 'ps2xplasma:  too many file path arguments.'
     endif
  enddo

  if(ihave_xp.eq.0) then
     ierr=ierr+1
     write(0,*) 'ps2xplasma:  missing xplasma file path argument.'
  endif

  if(ihave_ps.eq.0) then
     ierr=ierr+1
     write(0,*) 'ps2xplasma:  missing plasma state file path argument.'
  endif

  if(ierr.gt.0) then
     write(0,*) 'ps2xplasma syntax:'
     write(0,*) '  ps2xplasma  <plasma-state-path>  <xplasma-file-path>'
     write(0,*) '  the 2 path arguments are required.'
     call exit(ierr)
  else
     write(6,*) ' ps2xplasma syntax OK:'
     write(6,*) ' ps-path:  ',trim(pspath)
     write(6,*) ' xplasma-path:  ',trim(xpath)
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
     write(0,*) '?ps2xplasma: failed to read input plasma state.'
     call exit(ierr)
  endif

  call ps_xplasma_write
  call ckerr('ps_xplasma_write')

  !--------------------
  !  all done.
  !--------------------

  write(6,*) ' '
  write(6,*) ' %ps2xplasma: normal exit.'
  write(6,*) ' '

  !--------------------------------------------
CONTAINS
  !--------------------------------------------
  !  error handling routine

  subroutine ckerr(sbrtn)

    character*(*), intent(in) :: sbrtn

    if(ierr.ne.0) then
       write(0,*) ' ?ps2xplasma: unexpected error in subroutine '//trim(sbrtn)//'.'
       call exit(ierr)
    endif

  end subroutine ckerr

  subroutine ps_xplasma_write
    !  extract xplasma pointer; write xplasma file
    !  (option provided at request of grad student Jong-kyu Park).

    use xplasma_definitions

    type :: interp_object
       type (xplasma), pointer :: s_xpobj => NULL()
    end type interp_object

    type (interp_object) :: xobj
    type (xplasma), pointer :: s

    !------------------------------

    xobj = transfer(ps%iobj,xobj)
    s => xobj%s_xpobj

    call xplasma_write(s,trim(xpath),ierr)

    write(6,*) ' '
    if(ierr.eq.0) then
       write(6,*) ' %jsocdf2ps:  wrote xplasma file: ',trim(xpath)
    else
       write(6,*) ' %jsocdf2ps:  error writing xplasma file: ',trim(xpath)
       call xplasma_error(s,ierr,6)
    endif

  end subroutine ps_xplasma_write

end program ps2xplasma
