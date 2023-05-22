program convert_state

  !  read and write a plasma state -- this may also convert an
  !  old version plasma state file to a current version plasma state file.

  use plasma_state_mod
  implicit NONE

  character*256 arg1,arg2,arg3,from_state,to_state

  integer :: cclist(ps_ccount)    ! ps_ccount = #of components in state
  integer :: nargs
  integer :: ierr

  !--------------------------------

  ierr = 0

  arg1 = ' '
  arg2 = ' '
  arg3 = ' '

  cclist = ps_cc_all  ! default: copy all components...

  nargs = command_argument_count() 
  if((nargs.lt.2).or.(nargs.gt.3)) then
     write(0,*) '    incorrect number of arguments: ',nargs
     ierr=1
  else
     call get_command_argument(1,arg1)
     call get_command_argument(2,arg2)
     if(nargs.eq.3) then
        call get_command_argument(3,arg3)

        if(arg1(1:6).eq.'-omit:') then
           call get_cclist(arg1)
           from_state = arg2
           to_state = arg3

        else if(arg2(1:6).eq.'-omit:') then
           call get_cclist(arg2)
           from_state = arg1
           to_state = arg3

        else if(arg3(1:6).eq.'-omit:') then
           call get_cclist(arg3)
           from_state = arg1
           to_state = arg2

        else
           ierr=1
        endif
     else
        if((arg1(1:6).eq.'-omit:').or.(arg2(1:6).eq.'-omit:')) then
           ierr=1
           write(0,*) ' ?? both <from-state> <to-state> must be specified.'
        else
           from_state = arg1
           to_state = arg2
        endif
     endif
  endif

  if(ierr.ne.0) then
     write(0,*) ' ?syntax:  convert_state  <from-state>  <to-state>'
     write(0,*) '  -OR-     convert_state  <from-state>  <to-state> -omit:<cc1>,<cc2>,...'
     write(0,*) '  where "<cc1>,<cc2>,.." is a comma delimitted list of 1 or more component'
     write(0,*) '  names with no imbedded blanks in the entire list specification.'
     write(0,*) '  The <to-state> will be written under the current version of the software;'
     write(0,*) '  the data is copied from <from-state> with grids and profiles from the'
     write(0,*) '  indicated component(s) omitted.  Arguments were:'
     write(0,*) '    '//trim(arg1)//'  '//trim(arg2)//'  '//trim(arg3)
     call exit(1)
  endif

  if(from_state.eq.to_state) then
     write(0,*) ' ?syntax:  convert_state  <from-state>  <to-state> [-omit:...]'
     write(0,*) '           (filename arguments must be distinct):'
     write(0,*) '           ',trim(from_state),' ',trim(to_state)
     call exit(1)
  endif

  write(6,*) ' '
  write(6,*) ' reading: ',trim(from_state)
  call ps_get_plasma_state(ierr, filename=from_state, state=psp)
  if(ierr.ne.0) then
     write(0,*) ' ?convert_state:  read failure.'
     call exit(1)
  endif

  call ps_copy_plasma_state(psp, ps, ierr, cclist=cclist)
  if(ierr.ne.0) then
     write(0,*) ' ?convert_state:  state copy failure.'
     call exit(1)
  endif

  write(6,*) ' '
  write(6,*) ' writing: ',trim(to_state)
  call ps_store_plasma_state(ierr, filename=to_state)
  if(ierr.ne.0) then
     write(0,*) ' ?convert_state:  write failure.'
     call exit(1)
  endif

CONTAINS

  subroutine get_cclist(arg)

    character*(*), intent(in) :: arg

    ! parse component names from comma delimitted list
    ! if any name is not a recognized component, it is an error.

    character*32 cname
    integer :: ic,ico,ilen

    cname=' '

    ilen = len(trim(arg))

    ic=6
    ico=0

    do
       ic = ic + 1
       if(ic.gt.ilen) then
          call chk_compo(cname)
          exit
       endif
       if(arg(ic:ic).eq.',') then
          call chk_compo(cname)
          cname=' '
          ico=0
       else
          ico=min(len(cname),ico+1)
          cname(ico:ico) = arg(ic:ic)
       endif
    enddo

  end subroutine get_cclist

  subroutine chk_compo(cname)

    character*(*), intent(in) :: cname

    character*32 :: cnamu

    integer :: indx,imatch
    cnamu = cname
    call uupper(cnamu)

    imatch = 0
    do indx=1,ps_ccount
       if(cnamu.eq.ps_cnames(indx)) then
          imatch=indx
          exit
       endif
    enddo

    if(imatch.eq.0) then
       write(0,*) ' ?convert_state: unrecognized component name: '//trim(cnamu)
       ierr = ierr + 1
    else
       write(6,*) ' convert_state will OMIT component: '//trim(cnamu)
       cclist(imatch) = 0   ! OMIT this component.
    endif
  end subroutine chk_compo

end program convert_state
