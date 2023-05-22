subroutine ur_cstate_ilists

  ! examine item lists contained in the state(s)

  use cstate_lib_mod
  implicit NONE

  !-------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar,lyes

  integer :: ii,jj,ia,imatch,ierr,isize,isize_tot,ivd2
  integer :: ilen_coln(0:nsss_cur)
  character*32 iname
 
  integer, dimension(:,:), allocatable :: addr_table
  character*10, dimension(:,:), allocatable :: val_field
  !-------------------------

  luntty = lnout(0)

  call psw_contents('L',ncur_components,w1,ierr, state=sss(1))
  isize = w1%size
  
  do
     if(lxout(0)) then
        write(luntty,1001) omit_empty,trim(cur_components)

1001    format('ur_cstate_ilists options:'/ &
             '  A -- option to omit empty elements from display (',L1,')'/ &
             '  C -- select & restrict view to component(s) (current selection: ',a,')'/ &
             '  T -- table of contents (show dimensions and description)'/ &
             '  U -- table of contents (show component and description)'/ &
             '  D -- summary comparitive display of item lists'/ &
             '  S -- select a single item list & examine in detail'/ &
             '  L -- modify state label(s)'/ &
             '  Q -- quit this menu'/)
     endif
     call uread('ur_cstate_ilists option:')

     if(lchar('Q')) then
        exit

     else if(lchar('A')) then
        if(lxout(0)) write(luntty,*) &
             ' option to omit display of elements that are empty in all states.'

        call uread('omit empty elements ? (Y/N):')
        omit_empty = lyes(0)

     else if(lchar('C')) then
        call ur_cstate_component

     else if(lchar('D')) then
        call ur_dcomp_display('L', 'List_sizes', 'lists')

     else if(lchar('L')) then
        call ur_cstate_setLabel

     else if(lchar('T')) then
        call ur_cstate_ilist_contents(1)

     else if(lchar('U')) then
        call ur_cstate_ilist_contents(2)

     else if(lchar('S')) then
        call psw_contents('L',ncur_components,w1,ierr, state=sss(1))
        if(w1%size.eq.0) then
           write(luntty,*) ' %ur_cstate_ilists: no item lists found in currently selected components.'
           cycle
        endif

        if(lxout(0)) then
           write(luntty,*) '(unambiguous prefix of name may be entered).'
        endif
        call ureadw('name of item list to examine:',iname)
        if(iname.eq.'Q') cycle
        if(iname.eq.'.') cycle

        call cstate_findm(iname,luntty,imatch)
        if(imatch.eq.0) then
           call uclear
           cycle
        endif

        isize_tot = 0
        ilen_coln = 0

        do ii=1,nsss_cur
           call psw_ilist(iname,ww(ii),ierr, state=sss(ii))
           isize=ww(ii)%size

           if(ii.eq.1) then
              ilen_coln(0)=len(trim(ww(ii)%list_name))
           endif

           ilen_coln(ii)=max(7,len(trim(sss_label(ii))))

           if(isize.eq.0) then
              write(luntty,*) ' empty list for state: '//trim(sss_label(ii))
           else
              isize_tot=isize_tot + ww(ii)%size
           endif

           !  the following is special for item list element names only...
           !  the actual item names are in the C*256 value field
           !  alphabetic ordering is preserved in ww
           !  because initially list_elems(jj) = <prefix>//trim(c256val(jj))
           !  where <prefix> depends on the particular list selected.

           do jj=1,isize
              ww(ii)%list_elems(jj) = trim(ww(ii)%c256val(jj))
              ilen_coln(0)=max(ilen_coln(0),len(trim(ww(ii)%list_elems(jj))))
           enddo
        enddo

        if(isize_tot.eq.0) then
           write(luntty,*) ' empty list.'
           cycle
        endif

        allocate(addr_table(nsss_cur,isize_tot))
        call psw_merge(ww,wmerge,ierr, addr_table=addr_table)
        isize_tot = wmerge%size

        if(ierr.eq.0) then
           !  show which species are in which state...

           allocate(val_field(nsss_cur,isize_tot))
           ivd2 = isize_tot

           do jj=1,isize_tot
              do ii=1,nsss_cur
                 if(addr_table(ii,jj).gt.0) then
                    val_field(ii,jj)='PRESENT'
                 else
                    val_field(ii,jj)='absent'
                 endif
              enddo
           enddo

           call cstate_table_out(ww(1)%list_name, '   ', '   ', &
                ilen_coln,2,val_field,ivd2, &
                0,isize_tot)

           deallocate(val_field)
        endif

        deallocate(addr_table)

     else
        call uermsg(' ?ur_cstate_ilists: unrecognized option.',' ',0)
     endif
  enddo

end subroutine ur_cstate_ilists
