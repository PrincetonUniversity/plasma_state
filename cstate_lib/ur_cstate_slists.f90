subroutine ur_cstate_slists

  ! examine species lists contained in the state(s)

  use cstate_lib_mod
  implicit NONE

  !-------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar,lyes

  integer :: ii,jj,ia,ierr,imatch,isize_tot,izc,iza,ivd2
  integer :: idetail,ilen_coln(0:nsss_cur),ilen0d
  character*32 :: sname

  character*1 :: dd(2) = (/ 'Y', 'N' /)
  character*256 :: hdr,rln,buf

  integer, dimension(:,:), allocatable :: addr_table
  character*10, dimension(:,:), allocatable :: val_field
  !-------------------------

  luntty = lnout(0)
  idetail = 2

  call psw_contents('S',ncur_components,w1,ierr, state=sss(1))
  
  do
     if(lxout(0)) then
        write(luntty,1001) omit_empty,trim(cur_components),dd(idetail)

1001    format('ur_cstate_slists options:'/ &
             '  A -- option to omit empty elements from display (',L1,')'/ &
             '  C -- select & restrict view to component(s) (current selection: ',a,')'/ &
             '  T -- table of contents (show dimensions and description)'/ &
             '  U -- table of contents (show component and description)'/ &
             '  D -- summary comparitive display of species lists'/ &
             '  S -- select a single species list & compare; details=',a1/ &
             '  X -- set details flag for species list comparison.'/ &
             '  L -- modify state label(s)'/ &
             '  Q -- quit this menu'/)
     endif
     call uread('ur_cstate_slists option:')

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
        call ur_dcomp_display('S', 'Species_count', 'lists')

     else if(lchar('X')) then
        if(lxout(0)) then
           write(luntty,*) 'answer "Y" to for display of atomic charge and weight of species.'
        endif
        call uread('show details on species? (Y/N):')
        if(lyes(0)) then
           idetail=1
        else
           idetail=2
        endif

     else if(lchar('L')) then
        call ur_cstate_setLabel

     else if(lchar('T')) then
        call ur_cstate_slist_contents(1)

     else if(lchar('U')) then
        call ur_cstate_slist_contents(2)

     else if(lchar('S')) then
        call psw_contents('S',ncur_components,w1,ierr, state=sss(1))
        if(w1%size.eq.0) then
           write(luntty,*) ' %ur_cstate_slists: no species lists found in currently selected components.'
           cycle
        endif

        if(lxout(0)) then
           write(luntty,*) '(unambiguous prefix of name may be entered).'
        endif
        call ureadw('name of species list to examine:',sname)
        if(sname.eq.'Q') cycle
        if(sname.eq.'.') cycle

        call cstate_findm(sname,luntty,imatch)
        if(imatch.eq.0) then
           call uclear
           cycle
        endif

        isize_tot = 0
        do ii=1,nsss_cur
           call psw_slist(sname,ww(ii),ierr, state=sss(ii))

           if(ww(ii)%size.eq.0) then
              write(luntty,*) ' empty list for state: '//trim(sss_label(ii))
           else
              isize_tot=isize_tot + ww(ii)%size
           endif
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

           ilen_coln(0)=len(trim(ww(1)%list_name))
           do ii=1,nsss_cur
              ilen_coln(ii)=max(7,len(trim(sss_label(ii))))
           enddo

           allocate(val_field(nsss_cur,isize_tot))
           ivd2 = isize_tot

           do jj=1,isize_tot
              if(wmerge%list_elems(jj).eq.'E') then
                 wmerge%list_elems(jj) = 'e'
              endif
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

           ilen0d = len('Species_Label')
           do jj=1,isize_tot
              ilen0d = max(ilen0d,len(trim(wmerge%list_elems(jj))))
           enddo
           write(luntty,*) ' '

           if(idetail.eq.1) then

              !  show information on individual species

              hdr = 'Species_Label'
              rln = rline(1:ilen0d)
              hdr(ilen0d+1:) = '  Type_ID  Z_charge  Z_atom  A_wght'
              rln(ilen0d+1:) = '  -------  --------  ------  ----------'
              write(luntty,*) trim(hdr)
              write(luntty,*) trim(rln)

              do jj=1,isize_tot
                 if(wmerge%qval(jj).lt.0) then
                    buf = 'e'
                 else
                    buf = wmerge%list_elems(jj)
                 endif
                 izc = wmerge%qval(jj)
                 iza = wmerge%qatom(jj)
                 write(buf(ilen0d+1:),'(4x,i2,8x,i2,7x,i2,3x,1pe11.4)') &
                      wmerge%spectype(jj),izc,iza,wmerge%awght(jj)/ps_mp
                 write(luntty,*) trim(buf)
              enddo
              write(luntty,*) ' '
           endif

           deallocate(val_field)
        endif

        deallocate(addr_table)
     else
        call uermsg(' ?ur_cstate_slists: unrecognized option.',' ',0)
     endif
  enddo

end subroutine ur_cstate_slists
