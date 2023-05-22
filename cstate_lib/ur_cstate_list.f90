subroutine ur_cstate_list(iexit)

  !  acquire list of states to read for comparitive analysis / visualization
  !  UREAD command line interface library calls are used.

  use cstate_lib_mod
  implicit NONE

  integer, intent(out) :: iexit   ! =1 to request skipping the analysis
  !  this is set if there are no states to compare

  !---------------------------------
  !  local:

  integer :: i,ii,luntty,lnout,iertmp,ic,ilen,i1,i2,ierr
  integer :: ihash1(nsss_max),ihash2(nsss_max)
  logical :: lxout,lchar
  character*7 ztest
  !---------------------------------

  ierr = 0

  luntty = lnout(0)

  if(nsss_cur.eq.0) then
     user_label = .FALSE.
  endif

  write(luntty,*) ' ------------------------------------------------------ '
  write(luntty,*) ' (re)entering state list spec routine ur_cstate_list... '
  do
     !  information for the user...

     if(lxout(0)) then
        write(luntty,*) ' '
        write(luntty,'(1x,i1," states are in the list:")') nsss_cur
        do i=1,nsss_cur
           write(luntty,*) ' '
           write(luntty,'(" state #",i1,"  label: ",a)') i,trim(sss_label(i))
           write(luntty,'("    path: ",a)') trim(sss_path(i))
        enddo
        write(luntty,*) ' '
        write(luntty,*) ' options (NOTE menu has changed 3/2007): '
        if(nsss_cur.eq.nsss_max) then
           write(luntty,*) '   (maximum number of states already specified).'
        else
           write(luntty,*) '  A -- add a state to the list.'
        endif
        write(luntty,*) '  C -- clear list, remove all states.'
        if(nsss_cur.gt.0) then
           write(luntty,*) '  L -- modify a state label.'
        endif
        write(luntty,*) '  P -- proceed, using list as currently specified.'
        write(luntty,*) '  Q -- quit: exit program.'
     endif

     call uread('ur_cstate_list: command option:')

     if(lchar('Q')) then
        call erase_list
        iexit = 1
        exit             ! user requested exit (exit program)

     else if(lchar('P')) then
        if(nsss_cur.eq.0) then
           call uermsg('%ur_cstate_list: list is empty.',' ',0)
        else
           iexit = 0
           exit          ! user request: exit loop, plot selected states...
        endif

     else if(lchar('C')) then
        ! erase the list & associated states
        call erase_list

     else if(lchar('A')) then
        ! add a state
        if(nsss_cur.eq.nsss_max) then
           call uermsi('?ur_cstate_list: cannot add state, list is full',nsss_max,0)
           cycle
        endif

        nsss_cur = nsss_cur + 1
        i = nsss_cur

        user_label(i:nsss_max)=.FALSE.
        sss_label(i:nsss_max)=' '
        sss_path(i:nsss_max)=' '

        if(lxout(0)) write(luntty,*) ' an HTTP:// path may be specified.'

        sss_path(i)='NONE'
        call unixfile('state path',sss_path(i))

        if(sss_path(i).eq.'NONE') then
           nsss_cur = nsss_cur - 1
           sss_path(i)=' '
           cycle
        endif

        !  OK a path was given; assign temporary label

        sss_label(i)=' '
        write(sss_label(i),'("State_",i1)') i

        write(luntty,*) ' reading: State #',i

        call ps_get_plasma_state(iertmp, &
             filename = sss_path(i), &
             state = sss(i))

        if(iertmp.ne.0) then
           write(luntty,*) ' error reading state: ',trim(sss_path(i))
           ztest = sss_path(i)(1:7)
           call uupper(ztest)
           if(ztest.eq.'HTTP://') then
              write(luntty,*) ' '
              write(luntty,*) ' *** ur_cstate_list NOTE:'
              write(luntty,*) '  "Not a netCDF file" could mean: no such URL.'
              write(luntty,*) ' '
           endif
           sss_path(i)=' '
           sss_label(i)=' '
           call plasma_state_erase(sss(i),iertmp)
           nsss_cur = nsss_cur - 1
        endif

        !  update state labels -- try for distinguishing, but short...
        call relabel

     else if(lchar('L')) then
        !  modify label for state
        if(nsss_cur.eq.0) then
           call uermsg('%ur_cstate_list: no states have been read.',' ',0)
           cycle
        else
           call ur_cstate_setLabel
        endif

     else
        call uermsg('?ur_cstate_list: unrecognized command option.',' ',0)
     endif

  enddo

  if(nsss_cur.gt.0) then
     call cstate_empty_dims(luntty,ierr)
     if(ierr.eq.0) then
        call cstate_ldim_stats(luntty,ierr)
     endif
     call hcheck('machine descriptions',ps_shash_machine_description)
     call hcheck('shot configurations',ps_shash_shot_configuration)
     call hcheck('simulation initialization',ps_shash_simulation_init)
     call hcheck('(scalar) state data',ps_shash_state_data)
     call cstate_chk_locks(luntty)
  endif

  if(ierr.ne.0) then
     write(luntty,*) ' ?internal error detected in ur_cstate_list.f90'
     nsss_cur = 0
  endif

contains

  subroutine erase_list

    do i=1,nsss_cur
       call plasma_state_erase(sss(i),iertmp)
       if(iertmp.ne.0) then
          write(luntty,*) '  %ur_cstate_list: plasma_state_erase status: ', &
               iertmp
       endif
    enddo

    nsss_cur = 0

    sss_label = ' '
    sss_path = ' '

  end subroutine erase_list

  subroutine hcheck(label,iaddr)
    character*(*), intent(in) :: label
    integer, intent(in) :: iaddr

    ihash1 = 0
    ihash2 = 0

    do ii=1,nsss_cur
       ihash1(ii) = sss(ii)%hash_codes(iaddr)
       ihash2(ii) = sss(ii)%hash_codes(iaddr+1)
    enddo

    call cstate_hash_compare(label,ihash1(1:nsss_cur),ihash2(1:nsss_cur))
  end subroutine hcheck

  subroutine relabel

    ! collective relabeling of states not labeled by user...
    ! the idea here is to keep labels short but distinct

    integer :: inum,ivar,ilen,ildot,last_dot,ilook,ii,jj,iu,iuu
    integer :: ilstart(nsss_max),ilend(nsss_max),nmatch(nsss_max)
    character*256 :: suffix,suffix_ref
    character*256 :: prefix,prefix_ref

    ilstart=0
    ilend=0
    nmatch=0

    ! first check if common filename suffix is present:

    inum=0
    ivar=0

    do ii=1,nsss_cur
       if(.not.user_label(ii)) then
          inum = inum+1
          ilen = len(trim(sss_path(ii)))
          ilend(ii)=last_dot(sss_path(ii),ilen,'.')

          if(ilend(ii).eq.ilen) then
             suffix=' '
          else
             suffix = sss_path(ii)(ilend(ii)+1:ilen)
          endif

          if(inum.eq.1) then
             suffix_ref = suffix
          else
             if(suffix.ne.suffix_ref) then
                ivar = ivar + 1
             endif
          endif
       endif
    enddo

    if(inum.eq.0) return
    if(ivar.gt.0) then
       ! no common suffix; retain filename suffix as possible part of label
       do ii=1,nsss_cur
          if(.not.user_label(ii)) then
             ilend(ii) = len(trim(sss_path(ii)))
          endif
       enddo
    endif

    ! now look for common prefix, terminated by "_"

    iu = 0
    do 
       iu = iu + 1

       inum=0
       ivar=0

       do ii = 1,nsss_cur
          if(.not.user_label(ii)) then
             inum=inum+1

             ilook=ilend(ii)
             do iuu=1,iu
                ilook = last_dot(sss_path(ii),ilook,'_')
             enddo

             if(ilook.eq.0) then
                ilstart(ii) = 1
             else
                ilstart(ii) = ilook + 2
             endif

             if(ilook.eq.0) then
                prefix=' '
             else
                prefix=sss_path(ii)(1:ilook)
             endif

             if(inum.eq.1) then
                prefix_ref = prefix
             else
                if(prefix.ne.prefix_ref) then
                   ivar = ivar + 1
                   exit
                endif
             endif
          endif
       enddo

       if(ivar.eq.0) exit
    enddo

    do ii = 1,nsss_cur
       if(.not.user_label(ii)) then
          iuu=ilend(ii)
          if(iuu-ilstart(ii)+1.gt.20) then
             iu=ilend(ii)-16
             sss_label(ii)='...'//sss_path(ii)(iu:iuu)
          else
             iu=ilstart(ii)
             sss_label(ii)=sss_path(ii)(iu:iuu)
          endif
       endif
    enddo

    !  check against duplicate labeling
    do ii = 1,nsss_cur
       do jj=ii+1,nsss_cur
          if(sss_label(ii).eq.sss_label(jj)) then
             write(luntty,*) ' %ur_cstate_list: duplicate label suppressed.'
             nmatch(ii)=nmatch(ii)+1
             nmatch(jj)=nmatch(jj)+1
          endif
       enddo
    enddo

    do ii = 1,nsss_cur
       if(.not.user_label(ii)) then
          if(nmatch(ii).ne.0) then
             write(sss_label(ii),'("State_",i1)') ii
          endif
       endif
    enddo

  end subroutine relabel

end subroutine ur_cstate_list

integer function last_dot(str,ii,schar)

  character*(*), intent(in) :: str
  integer, intent(in) :: ii
  character*1, intent(in) :: schar

  !  search back in string for matching character; return location - 1

  integer :: jj,jmatch,kmatch

  !-------------------

  if(ii.eq.0) then
     last_dot=0
     return
  endif

  jmatch=0
  do jj=ii,1,-1
     if(str(jj:jj).eq.schar) then
        jmatch=jj
        exit
     endif
  enddo

  if(jmatch.eq.0) then
     last_dot=0
     return
  endif

  kmatch=0
  do jj=jmatch,1,-1
     if(str(jj:jj).ne.schar) then
        kmatch=jj
        exit
     endif
  enddo

  last_dot = kmatch
  return

end function last_dot

subroutine ur_cstate_setLabel

  !  modify state labels (for labeling of tables and displays)

  use cstate_lib_mod
  implicit NONE

  !---------------------------------
  !  local:

  integer :: luntty,ic,idcod,lnout,i,iert,imatch
  logical :: lxout,lchar
  character*7 ztest
  character*32 :: zlabel
  !---------------------------------

  luntty = lnout(0)

  do
     if(lxout(0)) then
        write(luntty,*) ' '
        write(luntty,'(1x,i1," states are in the list:")') nsss_cur
        do i=1,nsss_cur
           write(luntty,*) ' '
           write(luntty,'(" state #",i1,"  label: ",a)') i,trim(sss_label(i))
           write(luntty,'("    path: ",a)') trim(sss_path(i))
        enddo
        write(luntty,*) ' '
        write(luntty,*) ' options: '
        write(luntty,*) '  <number> -- indicate number of state for which to change the label.'
        write(luntty,*) '  Q -- quit (without changing any more labels).'
     endif

     call uread('ur_cstate_setLabel: enter state number or "Q" to quit:')
     if(lchar('Q')) exit

     ic=idcod(iert)
     if(iert.ne.0) cycle

     if((ic.le.0).or.(ic.gt.nsss_cur)) then
        write(luntty,*) ' Range of available state numbers: 1 to ',nsss_cur
        call uermsi('ur_cstate_setLabel: state number out of range',ic,0)
        cycle
     endif

     !  one word, uppercase not forced:
     call ureadlw('New state label:',zlabel)

     !  check/enforce uniqueness

     imatch=0
     do i=1,nsss_cur
        if(i.ne.ic) then
           if(zlabel.eq.sss_label(i)) then
              imatch=i
              exit
           endif
        else
           if(zlabel(1:6).eq.'State_') then
              imatch=-1
              exit
           endif
        endif
     enddo

     if(imatch.gt.0) then
        call uermsg('ur_cstate_setLabel: label already in use: ',zlabel,0)
        cycle
     else if(imatch.lt.0) then
        call uermsg('ur_cstate_setLabel: "State_" label not allowed: ', &
             zlabel,0)
        cycle
     endif

     sss_label(ic)=zlabel
     user_label(ic)=.TRUE.

  enddo

end subroutine ur_cstate_setLabel
