subroutine ur_cstate_contents

  !  generate tables of contents of various categories of items stored
  !  in plasma states

  use cstate_lib_mod
  implicit NONE

  !-------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar,lyes
  character*32 :: subsect

  !-------------------------

  luntty = lnout(0)

  do
     if(lxout(0)) then
        write(luntty,1001) omit_empty,trim(cur_components)
1001    format(' tables of contents (select 1):'/ &
             ' A -- option to omit empty elements from display (',L1,')'/ &
             ' C -- reselect component(s) (current setting: ',a,')'// &
             ' G -- grids'/ &
             ' S -- species lists'/ &
             ' I -- item lists'// &
             ' D -- state data, machine description, etc. (show units & dimensions)'/ &
             ' E -- state data, machine description, etc. (show descriptions)'// &
             ' N -- state profiles (show units & dimensions)'/ &
             ' O -- state profiles (show units & interpolation method)'/ &
             ' P -- state profiles (show descriptions)'// &
             ' Q -- quit this menu'/)
     endif

     call uread('ur_cstate_contents option:')

     if(lchar('Q')) then
        exit
        
     else if(lchar('A')) then
        if(lxout(0)) write(luntty,*) &
             ' option to omit display of elements that are empty in all states.'

        call uread('omit empty elements ? (Y/N):')
        omit_empty = lyes(0)

     else if(lchar('C')) then
        call ur_cstate_component

     else if(lchar('G')) then
        call ur_cstate_grid_contents

     else if(lchar('S')) then
        call ur_cstate_slist_contents(1)

     else if(lchar('I')) then
        call ur_cstate_ilist_contents(1)

     else if(lchar('D')) then
        call subselect
        if(subsect.eq.'Q') cycle
        call ur_cstate_sdata_contents(1,subsect)

     else if(lchar('E')) then
        call subselect
        if(subsect.eq.'Q') cycle
        call ur_cstate_sdata_contents(2,subsect)

     else if(lchar('N')) then
        call ur_cstate_sprof_contents(1)

     else if(lchar('O')) then
        call ur_cstate_sprof_contents(2)

     else if(lchar('P')) then
        call ur_cstate_sprof_contents(3)

     else 
        call uermsg(' ?ur_cstate_contents: unknown option.',' ',0)
     endif
  enddo

  contains
    subroutine subselect

      do
         if(lxout(0)) then
            write(luntty,1002)

1002        format(/ &
                 '  M -- view/compare MACHINE DESCRIPTION other than item lists'/ &
                 '  F -- view/compare SHOT CONFIGURATION other than species lists'/ &
                 '  N -- view/compare SIMULATION INITIALIZATION data other than grids'/ &
                 '  D -- view/compare STATE DATA'/ &
                 '  Q -- quit (skip display)')
         endif
         
         call uread('ur_cstate_contents_subselect: option selection:')

         if(lchar('Q')) then
            subsect='Q'

         else if(lchar('M')) then
            subsect='MACHINE_DESCRIPTION'

         else if(lchar('F')) then
            subsect='SHOT_CONFIGURATION'

         else if(lchar('N')) then
            subsect='SIMULATION_INIT'

         else if(lchar('D')) then
            subsect='STATE_DATA'

         else
            call uermsg('?ur_cstate_contents_subselect: unrecognized option', &
                 ' ',0)
            cycle
         endif

         exit
      enddo
    end subroutine subselect

end subroutine ur_cstate_contents

subroutine ur_cstate_grid_contents

  ! grids contained in the state(s)

  use cstate_lib_mod
  implicit NONE

  !-------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar
  integer :: i,ierr,ilen_names,ilen_vals,ilen_256,ilen_units,ilen_descr
  integer :: ilen_compo,inum_empty,ic
  character*32 :: zelem
  !-------------------------

  luntty = lnout(0)
  call psw_contents('G',ncur_components,wgrid,ierr, state=sss(1))
  call cstate_wlen(wgrid,ilen_names,ilen_compo, &
       ilen_vals,ilen_256,ilen_units,ilen_descr)

  ilen_vals = max(9,ilen_vals)
  ilen_names = ilen_names + 1   ! create an extra vertical space

2000 format(/ &
          ' no grids in selected component(s).'/)

2001 format(a,1x,a,1x,a,1x,a)

  if(wgrid%size.eq.0) then
     write(luntty,2000)
  else
     c2lab='Component'
     c3lab='Dimension'
     write(luntty,*) ' '
     write(luntty,2001) wgrid%list_name(1:ilen_names), &
          c2lab(1:ilen_compo),c3lab(1:ilen_vals),'Description'
     write(luntty,2001) rline(1:ilen_names), &
          rline(1:ilen_compo),rline(1:ilen_vals),rline(1:min(50,ilen_descr))
     do i=1,wgrid%size
        call cstate_ck_empty(wgrid%val_field(i),is_empty)
        zelem=wgrid%list_elems(i)
        if(is_empty) then
           if(omit_empty) cycle
           inum_empty=inum_empty+1
           ic=min(len(zelem),len(trim(zelem))+1)
           zelem(ic:ic)='*'
        endif
        write(luntty,2001) zelem(1:ilen_names), &
             wgrid%component(i)(1:ilen_compo), &
             wgrid%val_field(i)(1:ilen_vals), &
             trim(wgrid%elem_descr(i))
     enddo
     write(luntty,*) ' '
  endif
  if(inum_empty.gt.0) then
     write(luntty,*) '   [*]: empty in all states.'
     write(luntty,*) ' '
  endif

end subroutine ur_cstate_grid_contents

subroutine ur_cstate_slist_contents(icall)

  ! species lists contained in the state(s)

  use cstate_lib_mod
  implicit NONE

  integer, intent(in) :: icall  ! =1 for dimension & description;
  !          =2 for component & description; other values: put contents
  !          data in w1 but do not print anything

  !-------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar
  integer :: i,ierr,ilen_names,ilen_vals,ilen_256,ilen_units,ilen_descr
  integer :: ilen_compo,inum_empty,ic
  character*32 :: zelem
  !-------------------------

  luntty = lnout(0)
  call psw_contents('S',ncur_components,w1,ierr, state=sss(1))
  call cstate_wlen(w1,ilen_names,ilen_compo, &
       ilen_vals,ilen_256,ilen_units,ilen_descr)

  ilen_vals = max(9,ilen_vals)
  ilen_names = ilen_names + 1   ! create an extra vertical space

2000 format(/ &
          ' no species lists in selected component(s).'/)

2001 format(a,1x,a,1x,a)

  inum_empty = 0
  if(w1%size.eq.0) then
     write(luntty,2000)

  else if(icall.eq.1) then
     c2lab='Dimension'
     write(luntty,*) ' '
     write(luntty,2001) w1%list_name(1:ilen_names),c2lab(1:ilen_vals),'Description'
     write(luntty,2001) rline(1:ilen_names),rline(1:ilen_vals),rline(1:min(50,ilen_descr))
     do i=1,w1%size
        call cstate_ck_empty(w1%val_field(i),is_empty)
        zelem=w1%list_elems(i)
        if(is_empty) then
           if(omit_empty) cycle
           inum_empty=inum_empty+1
           ic=min(len(zelem),len(trim(zelem))+1)
           zelem(ic:ic)='*'
        endif
        write(luntty,2001) zelem(1:ilen_names), &
             w1%val_field(i)(1:ilen_vals),trim(w1%elem_descr(i))
     enddo
     write(luntty,*) ' '

  else if(icall.eq.2) then
     c2lab='Component'
     write(luntty,*) ' '
     write(luntty,2001) w1%list_name(1:ilen_names),c2lab(1:ilen_compo),'Description'
     write(luntty,2001) rline(1:ilen_names),rline(1:ilen_compo),rline(1:min(50,ilen_descr))
     do i=1,w1%size
        call cstate_ck_empty(w1%val_field(i),is_empty)
        zelem=w1%list_elems(i)
        if(is_empty) then
           if(omit_empty) cycle
           inum_empty=inum_empty+1
           ic=min(len(zelem),len(trim(zelem))+1)
           zelem(ic:ic)='*'
        endif
        write(luntty,2001) zelem(1:ilen_names), &
             w1%component(i)(1:ilen_compo),trim(w1%elem_descr(i))
     enddo
     write(luntty,*) ' '

  endif
  if(inum_empty.gt.0) then
     write(luntty,*) '   [*]: empty in all states.'
     write(luntty,*) ' '
  endif

end subroutine ur_cstate_slist_contents

subroutine ur_cstate_ilist_contents(icall)

  ! item lists contained in the state(s)

  use cstate_lib_mod
  implicit NONE

  integer, intent(in) :: icall  ! =1 for dimension & description;
  !          =2 for component & description; other values: put contents
  !          data in w1 but do not print anything
  !-------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar
  integer :: i,ierr,ilen_names,ilen_vals,ilen_256,ilen_units,ilen_descr
  integer :: ilen_compo,inum_empty,ic
  character*32 :: zelem
  !-------------------------

  luntty = lnout(0)
  call psw_contents('L',ncur_components,w1,ierr, state=sss(1))
  call cstate_wlen(w1,ilen_names,ilen_compo, &
       ilen_vals,ilen_256,ilen_units,ilen_descr)

  ilen_vals = max(9,ilen_vals)
  ilen_names = ilen_names + 1   ! create an extra vertical space

2000 format(/ &
          ' no item lists in selected component(s).'/)

2001 format(a,1x,a,1x,a)

  inum_empty = 0
  if(w1%size.eq.0) then
     write(luntty,2000)

  else if(icall.eq.1) then
     c2lab='Dimension'
     write(luntty,*) ' '
     write(luntty,2001) w1%list_name(1:ilen_names),c2lab(1:ilen_vals),'Description'
     write(luntty,2001) rline(1:ilen_names),rline(1:ilen_vals),rline(1:min(50,ilen_descr))
     do i=1,w1%size
        call cstate_ck_empty(w1%val_field(i),is_empty)
        zelem=w1%list_elems(i)
        if(is_empty) then
           if(omit_empty) cycle
           inum_empty=inum_empty+1
           ic=min(len(zelem),len(trim(zelem))+1)
           zelem(ic:ic)='*'
        endif
        write(luntty,2001) zelem(1:ilen_names), &
             w1%val_field(i)(1:ilen_vals),trim(w1%elem_descr(i))
     enddo
     write(luntty,*) ' '

  else if(icall.eq.2) then
     c2lab='Component'
     write(luntty,*) ' '
     write(luntty,2001) w1%list_name(1:ilen_names),c2lab(1:ilen_compo),'Description'
     write(luntty,2001) rline(1:ilen_names),rline(1:ilen_compo),rline(1:min(50,ilen_descr))
     do i=1,w1%size
        call cstate_ck_empty(w1%val_field(i),is_empty)
        zelem=w1%list_elems(i)
        if(is_empty) then
           if(omit_empty) cycle
           inum_empty=inum_empty+1
           ic=min(len(zelem),len(trim(zelem))+1)
           zelem(ic:ic)='*'
        endif
        write(luntty,2001) zelem(1:ilen_names), &
             w1%component(i)(1:ilen_compo),trim(w1%elem_descr(i))
     enddo
     write(luntty,*) ' '

  endif
  if(inum_empty.gt.0) then
     write(luntty,*) '   [*]: empty in all states.'
     write(luntty,*) ' '
  endif

end subroutine ur_cstate_ilist_contents

subroutine ur_cstate_sdata_contents(icall,subsect)

  ! non-profile data contained in the state(s)

  use cstate_lib_mod
  implicit NONE

  integer, intent(in) :: icall  ! =1 for units & dimensions; =2 for description
  !  other values: put contents in w1 but do not print anything.

  character*(*), intent(in) :: subsect

  !-------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar

  integer :: ierr,ilen_names,ilen_vals,ilen_256,ilen_units,ilen_descr,i,isize
  integer :: ilen_compo,inum_empty,ic
  character*32 :: zelem
  character*1 :: dcod
  logical, dimension(:), allocatable :: is_blank
  integer :: bcount
  !-------------------------

  luntty = lnout(0)

  if(subsect.eq.'MACHINE_DESCRIPTION') then
     dcod='M'
  else if(subsect.eq.'SHOT_CONFIGURATION') then
     dcod='C'
  else if(subsect.eq.'SIMULATION_INIT') then
     dcod='N'
  else
     dcod='D'
  endif

  call psw_contents(dcod,ncur_components,w1,ierr, state=sss(1))
  call cstate_wlen(w1,ilen_names,ilen_compo, &
       ilen_vals,ilen_256,ilen_units,ilen_descr)
  ilen_names = ilen_names + 2   ! create two extra vertical spaces

  ilen_256=max(10,ilen_256)
  ilen_units=max(5,ilen_units)
  ilen_descr=max(23,ilen_descr)

2000 format(/ &
          ' no state data items in selected component(s).'/)

2002 format(a,1x,a)
2003 format(a,1x,a,1x,a)
2004 format(a,1x,a,1x,a,1x,a)

  bcount = 0
  inum_empty = 0

  isize=w1%size
  if(isize.eq.0) then
     write(luntty,2000)
  else
     allocate(is_blank(isize)); is_blank = .FALSE.
     call ur_cstate_blanks(dcod,isize,is_blank)
     if(icall.eq.1) then
        c2lab='Component'
        c3lab='Units'
        write(luntty,*) ' '
        write(luntty,2004) w1%list_name(1:ilen_names), &
             c2lab(1:ilen_compo),c3lab(1:ilen_units),'Dimensions'
        write(luntty,2004) rline(1:ilen_names),&
             rline(1:ilen_compo),rline(1:ilen_units),rline(1:min(35,ilen_256))
        do i=1,isize
           call cstate_ck_empty(w1%c256val(i),is_empty)
           zelem=w1%list_elems(i)
           if(is_empty) then
              if(omit_empty) cycle
              inum_empty=inum_empty+1
              ic=len(zelem)-1
              zelem = '*'//zelem(1:ic)
           else if(is_blank(i)) then
              ic=len(zelem)-2
              zelem = '**'//zelem(1:ic)
              bcount = bcount + 1
           endif
           write(luntty,2004) zelem(1:ilen_names), &
                w1%component(i)(1:ilen_compo),w1%elem_units(i)(1:ilen_units), &
                trim(w1%c256val(i))
        enddo
        write(luntty,*) ' '
     else if(icall.eq.2) then
        write(luntty,*) ' '
        write(luntty,2002) w1%list_name(1:ilen_names),'Description or Contents'
        write(luntty,2002) rline(1:ilen_names),rline(1:min(50,ilen_descr))
        do i=1,isize
           call cstate_ck_empty(w1%c256val(i),is_empty)
           zelem=w1%list_elems(i)
           if(is_empty) then
              if(omit_empty) cycle
              inum_empty=inum_empty+1
              ic=len(zelem)-1
              zelem = '*'//zelem(1:ic)
           else if(is_blank(i)) then
              ic=len(zelem)-2
              zelem = '**'//zelem(1:ic)
              bcount = bcount + 1
           endif
           write(luntty,2002) zelem(1:ilen_names), &
                trim(w1%elem_descr(i))
        enddo
        write(luntty,*) ' '
     endif
     deallocate(is_blank)
  endif
  if(inum_empty.gt.0) write(luntty,*) '   [*]: empty in all states.'
  if(bcount.gt.0) write(luntty,*) '   [**]: zero, blank, uninitialized or empty in all states.'
  if(max(bcount,inum_empty).gt.0) write(luntty,*) ' '

end subroutine ur_cstate_sdata_contents

subroutine ur_cstate_sprof_contents(icall)

  ! profile data contained in the state(s)

  use cstate_lib_mod
  implicit NONE

  integer, intent(in) :: icall  ! =1 for units & dimensions;
  !                       =2 for units & interpolation method;
  !                       =3 for descriptions
  !  other values: put contents in w1 but do not print anything.

  !-------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar

  integer :: ierr,ilen_names,ilen_vals,ilen_256,ilen_units,ilen_descr,i,isize
  integer :: ilen_compo,inum_empty,ic
  character*32 :: zelem
  logical, dimension(:), allocatable :: is_blank
  integer :: bcount
  !-------------------------

  luntty = lnout(0)
  call psw_contents('P',ncur_components,w1,ierr, state=sss(1))
  call cstate_wlen(w1,ilen_names,ilen_compo, &
       ilen_vals,ilen_256,ilen_units,ilen_descr)

  ilen_256=max(10,ilen_256)
  ilen_units=max(5,ilen_units)
  ilen_vals=max(20,ilen_vals)
  ilen_descr=max(23,ilen_descr)
  ilen_names = ilen_names + 2   ! create two extra vertical spaces

2000 format(/ &
          ' no profiles in selected component(s).'/)

2002 format(a,1x,a)
2003 format(a,1x,a,1x,a)
2004 format(a,1x,a,1x,a,1x,a)

  inum_empty = 0
  bcount = 0

  isize = w1%size
  if(isize.eq.0) then
     write(luntty,2000)
  else
     allocate(is_blank(isize)); is_blank = .FALSE.
     call ur_cstate_blanks('P',isize,is_blank)
     if(icall.eq.1) then
        c2lab='Units'
        write(luntty,*) ' '
        write(luntty,2003) w1%list_name(1:ilen_names),c2lab(1:ilen_units),'Dimensions'
        write(luntty,2003) rline(1:ilen_names),rline(1:ilen_units),rline(1:min(40,ilen_256))
        do i=1,isize
           call cstate_ck_empty(w1%c256val(i),is_empty)
           zelem=w1%list_elems(i)
           if(is_empty) then
              if(omit_empty) cycle
              inum_empty=inum_empty+1
              ic=len(zelem)-1
              zelem = '*'//zelem(1:ic)
           else if(is_blank(i)) then
              ic=len(zelem)-2
              zelem = '**'//zelem(1:ic)
              bcount = bcount + 1
           endif
           write(luntty,2003) zelem(1:ilen_names), &
                w1%elem_units(i)(1:ilen_units),trim(w1%c256val(i))
        enddo
        write(luntty,*) ' '

     else if(icall.eq.2) then
        c2lab='Component'
        c3lab='Units'
        write(luntty,*) ' '
        write(luntty,2004) w1%list_name(1:ilen_names), &
             c2lab(1:ilen_compo),c3lab(1:ilen_units),'Interpolation method'
        write(luntty,2004) rline(1:ilen_names), &
             rline(1:ilen_compo),rline(1:ilen_units),rline(1:min(35,ilen_vals))
        do i=1,isize
           call cstate_ck_empty(w1%c256val(i),is_empty)
           zelem=w1%list_elems(i)
           if(is_empty) then
              if(omit_empty) cycle
              inum_empty=inum_empty+1
              ic=len(zelem)-1
              zelem = '*'//zelem(1:ic)
           else if(is_blank(i)) then
              ic=len(zelem)-2
              zelem = '**'//zelem(1:ic)
              bcount = bcount + 1
           endif
           write(luntty,2004) zelem(1:ilen_names), &
                w1%component(i)(1:ilen_compo),w1%elem_units(i)(1:ilen_units), &
                trim(w1%val_field(i))
        enddo
        write(luntty,*) ' '

     else if(icall.eq.3) then
        write(luntty,*) ' '
        write(luntty,2002) w1%list_name(1:ilen_names),'Description or Contents'
        write(luntty,2002) rline(1:ilen_names),rline(1:min(50,ilen_descr))
        do i=1,isize
           call cstate_ck_empty(w1%c256val(i),is_empty)
           zelem=w1%list_elems(i)
           if(is_empty) then
              if(omit_empty) cycle
              inum_empty=inum_empty+1
              ic=len(zelem)-1
              zelem = '*'//zelem(1:ic)
           else if(is_blank(i)) then
              ic=len(zelem)-2
              zelem = '**'//zelem(1:ic)
              bcount = bcount + 1
           endif
           write(luntty,2002) zelem(1:ilen_names), &
                trim(w1%elem_descr(i))
        enddo
        write(luntty,*) ' '
     endif
     deallocate(is_blank)
  endif
  if(inum_empty.gt.0) write(luntty,*) '   [*]: empty in all states.'
  if(bcount.gt.0) write(luntty,*) '   [**]: zero, blank, uninitialized or empty in all states.'
  if(max(bcount,inum_empty).gt.0) write(luntty,*) ' '

end subroutine ur_cstate_sprof_contents

subroutine ur_cstate_blanks(dcod,isize,is_blank)

  ! identify data elements that are blank, zero, or empty/unallocated for
  ! all states on the list;  the 1st state contents information is already
  ! loaded in w1.

  use cstate_lib_mod
  implicit NONE

  !--------------------------------------------
  ! passed:
  character*1 :: dcod    ! data type code
  integer :: isize       ! size of logical array
  logical :: is_blank(isize)

  !--------------------------------------------
  ! local:

  integer :: i,ii,ierr
  !--------------------------------------------

  do i=1,isize
     is_blank(i) = w1%ival(i).le.0
  enddo

  do ii=2,nsss_cur
     call psw_contents(dcod,ncur_components,wtmp,ierr, state=sss(ii))
     do i=1,isize
        if(is_blank(i)) then
           is_blank(i) = wtmp%ival(i).le.0
        endif
     enddo
  enddo

end subroutine ur_cstate_blanks
