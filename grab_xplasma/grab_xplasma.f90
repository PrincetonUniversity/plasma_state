program grab_xplasma

  !  read an xplasma file -- filename as command line argument

  use xplasma_definitions
  type (xplasma), pointer :: s

  integer :: nargs,ierr,id
  character*120 :: filename

  !----------------------------------
  !  get the filename argument

  nargs = command_argument_count() 
  if(nargs.ne.1) then
     write(6,*) ' ?grab_xplasma: nargs = ',nargs, &
          '; exactly one command line argument expected.'
     write(6,*) ' syntax:  grab_xplasma <filename>'
  else
     call get_command_argument(1,filename)
  endif

  !----------------------------------
  !  create an xplasma object and load it from the file

  allocate(s)

  call xplasma_read(s,filename,ierr)

  if(ierr.ne.0) then
     call xplasma_error(s,ierr,6)
     write(6,*) &
          ' ?grab_xplasma: xplasma_read failed, filename: ',trim(filename)

  else
     write(6,*) ' %grab_xplasma: read successful: ',trim(filename)
  endif

  !----------------------------------
  ! "s" is now loaded: do whatever...

  call xplasma_profId(s,'R',id)
  write(6,*) ' ID of R profile: ',id

  call xplasma_profId(s,'Z',id)
  write(6,*) ' ID of Z profile: ',id

  call xplasma_profId(s,'g',id)
  write(6,*) ' ID of g profile: ',id

end program grab_xplasma
