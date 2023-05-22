program update_state

  !  DMC Oct 2008: update state (using files)
  !    a base state is read from a file; state updates are then applied
  !    from update files in sequence.  When done, the state is written
  !    back out (optionally to a different file)

  !  syntax:
  !    update_state -input <state_filename> [-output <output_filename>]
  !                     [-updates <file1> [file2 file3 ...]]
  !                     [-stdin]

  !    -input  specifies the input state file -- this information required;
  !            subsequent argument must be a filename.

  !    -output specifies the output file; if provided subsequent argument
  !            must be a filename.  If omitted the file specified under
  !            -input will be rewritten with any updates received.
  
  !    -updates specifies a list of one or more state update files; the
  !            arguments that follow are taken as filenames, until the
  !            next argument starting with "-", or end of arguments is
  !            encountered.

  !    -stdin  specifies to read stdin for update filenames.

  !    messages are written to stdout.  If an error occurs, 
  !    the program exits with a non-zero status code.

  !--------------------------------------------------------------------------
  USE plasma_state_mod
  implicit NONE

  !-------------------------------------------

  integer :: iarg,nargs,ilen,idash,ifail,ierr,ilen_keywd,ii,ieof,icomp,idiff

  integer, parameter :: imax_args = 100
  character*250 :: args(imax_args),update_files(imax_args)
  character*250 :: infile,outfile,afile
  character*20 mode_str

  logical :: read_stdin

  integer :: imode  ! =0, seek "-" option; 
                    ! =1, seek infile
                    ! =2, seek outfile
                    ! =3, seek update_files

  integer :: num_updates

  !-------------------------------------------
  !  Part 1: parse command line arguments

  read_stdin = .FALSE.

  num_updates = 0
  imode = 0

  infile=' '
  outfile=' '

  nargs = command_argument_count() 
  if(nargs.gt.imax_args) then
     ierr=1
     write(6,*) '?update_state: too many arguments (max: ',imax_args, &
          '; got: ',nargs,')'
     call syntax_error
  endif

  do iarg=1,nargs
     call get_command_argument(iarg,args(iarg))

     ilen = len(trim(args(iarg)))

     ifail=0

     ! parse arguments

     idash = 0
     if(args(iarg)(1:1).eq.'-') idash=1

     if(idash.eq.1) then
        !  command qualifer -nth or -nrho

        if(imode.eq.1) then
           write(6,*) '?update_state: -input filename argument missing.'
           call syntax_error
        endif

        if(imode.eq.2) then
           write(6,*) '?update_state: -output filename argument missing.'
           call syntax_error
        endif

        mode_str = args(iarg)(2:)
        call uupper(mode_str)

        if(mode_str.eq.'INPUT') then
           if(infile.ne.' ') then
              write(6,*) '?update_state: duplicate -input argument.'
              call syntax_error
           endif
           imode = 1

        else if(mode_str.eq.'OUTPUT') then
           if(outfile.ne.' ') then
              write(6,*) '?update_state: duplicate -output argument.'
              call syntax_error
           endif
           imode = 2

        else if(mode_str.eq.'UPDATES') then
           imode = 3

        else if(mode_str.eq.'STDIN') then
           read_stdin = .TRUE.

        else
           write(6,*) '?update_state: unrecognized command argument: '// &
                trim(args(iarg))
           call syntax_error
        endif

     else

        if(imode.eq.0) then
           write(6,*) '?update_state: argument "'//trim(args(iarg))// &
                '" misplaced.'
           call syntax_error
        endif

        if(imode.eq.1) then
           infile = args(iarg)
           imode = 0

        else if(imode.eq.2) then
           outfile = args(iarg)
           imode = 0

        else
           num_updates = num_updates + 1
           update_files(num_updates) = args(iarg)
              
        endif

     endif

  enddo

  if(infile.eq.' ') then
     write(6,*) '?update_state: -input file argument not found.'
     call syntax_error
  endif

  if(outfile.eq.' ') then
     outfile = infile
  endif

  if(.not.read_stdin) then

     if(num_updates.eq.0) then
        write(6,*) '?update_state: no updates specified.'
        call syntax_error
     endif

  endif

  write(6,*) ' update_state: arguments OK.'
  write(6,*) '   -input:  ',trim(infile)
  write(6,*) '   -output: ',trim(outfile)
  write(6,*) '   -updates on command line: ',num_updates,' files.'
  write(6,*) '   -stdin (T/F): ',read_stdin
  write(6,*) ' '

  !------------------------------
  !  execute...

  call ps_get_plasma_state(ierr,filename=trim(infile))
  if(ierr.ne.0) call io_error('ps_get_plasma_state',infile)

  write(6,*) ' ps_get_plasma_state OK: ',trim(infile)

  !  command line directed updates:

  do ii=1,num_updates

     call ps_read_update_file(update_files(ii),ierr, update_complete=.FALSE.)
     if(ierr.ne.0) call io_error('ps_read_update_file',update_files(ii))

     write(6,*) ' update applied: ',trim(update_files(ii))
  enddo

  !  update filenames received via stdin:

  if(read_stdin) then

     write(6,*) ' '
     write(6,*) ' reading update filenames on stdin:'

     do
        read(5,'(A)',iostat=ieof) afile
        if(ieof.ne.0) exit

        call ps_read_update_file(afile,ierr)
        if(ierr.ne.0) call io_error('ps_read_update_file',afile)

        write(6,*) ' update applied: ',trim(afile)
     enddo

  endif

  call ps_store_plasma_state(ierr,filename=trim(outfile))
  if(ierr.ne.0) call io_error('ps_store_plasma_state',outfile)

  call exit(0)

CONTAINS
  
  subroutine syntax_error

    write(6,*) ' '
    write(6,*) ' command argument syntax of "update_state" program: '
    write(6,*) ' '

    write(6,*) '    update_state  -input  <input_state_filename> '
    write(6,*) '                 [-output <output_state_filename>]'
    write(6,*) '                 [-updates <list_of_update_filenames>]'
    write(6,*) '                 [-stdin]'
    write(6,*) ' '
    write(6,*) ' if no -output file is specified, the -input file is replaced'
    write(6,*) ' with an updated version.  If -stdin is specified, additional'
    write(6,*) ' update filenames are read from -stdin, after all updates'
    write(6,*) ' from files named under the -updates argument are applied.'
    write(6,*) ' '

    call exit(2)
  end subroutine syntax_error

  subroutine io_error(subr,filenam)

    character*(*), intent(in) :: subr,filenam

    write(6,*) ' '
    write(6,*) ' ?update_state: I/O operation failed:'
    write(6,*) '  failing call: '//trim(subr)
    write(6,*) '  named file:   '//trim(filenam)
    write(6,*) ' '

    call exit(1)
  end subroutine io_error

  subroutine unknown_error(subr,filenam)

    character*(*), intent(in) :: subr,filenam

    write(6,*) ' '
    write(6,*) ' ?update_state: unexpected error:'
    write(6,*) '  failing call: '//trim(subr)
    write(6,*) '  recently named file: '//trim(filenam)
    write(6,*) ' '

    call exit(1)
  end subroutine unknown_error

end program update_state
