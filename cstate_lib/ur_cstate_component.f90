subroutine ur_cstate_component

  !  compare contents of one or more states

  use cstate_lib_mod
  implicit NONE

  !-------------------------
  ! local

  character*32 ctmp,ztest

  integer :: i,j,iwarn,ilen_names,ilen_vals,ilen_units,ilen_descr,ilen_256
  integer :: ilen_compo
  integer :: luntty,lnout
  logical :: lxout,lchar
  integer :: iremove,isum,icount,iflag,ilen,ilen_new
  !-------------------------

  luntty = lnout(0)

5 continue

  call psw_components(w1)
  call cstate_wlen(w1,ilen_names, &
       ilen_compo,ilen_vals,ilen_256,ilen_units,ilen_descr)

  write(luntty,*) ' '
  write(luntty,2001) w1%list_name(1:ilen_names),'Description'
  write(luntty,2001) rline(1:ilen_names),rline(1:min(50,ilen_descr))
  do i=1,w1%size
     write(luntty,2001) w1%list_elems(i)(1:ilen_names), &
          trim(w1%elem_descr(i))
  enddo

2001 format(a,1x,a)

  do

     isum = sum(ncur_components)

     if(lxout(0)) then
        write(luntty,*) ' '
        write(luntty,*) ' cur_components: ',trim(cur_components)
        write(luntty,*) ' '
        write(luntty,*) ' enter "A" to select ALL components.'
        write(luntty,*) ' enter "0" to UNselect all components.'
        write(luntty,*) ' enter <component_name> to add component.'
        write(luntty,*) ' enter -<component_name> to remove component.'
        write(luntty,*) ' enter "R" to review components.'
        write(luntty,*) ' enter "Q" to quit this loop.'
     endif

     call ureadw('component selection: ',ctmp)

     if(ctmp.eq.'Q') then
        if(isum.eq.0) then
           call uermsg(' ?ur_cstate_component: at least one component must be selected.',' ',0)
           cycle
        else
           exit
        endif

     else if(ctmp.eq.'R') then
        go to 5

     else if(ctmp.eq.'A') then
        cur_components = '*'
        ncur_components = 1

     else if(ctmp.eq.'0') then
        cur_components = ' '
        ncur_components = 0

     else 

        if(ctmp(1:1).eq.'-') then
           iremove = 1
           ctmp = ctmp(2:)
           call ps_cclist_remove(ctmp,ncur_components,iwarn)
        else
           iremove = 0
           call ps_cclist_add(ctmp,ncur_components,iwarn)
        endif

        if(iwarn.ne.0) then
           call uermsg('component name unrecognized',ctmp,0)
        else
           !  build ordered list of selected components
           if((iremove.eq.0).and.(cur_components.eq.'*')) cycle

           icount = 0
           iflag = 0

           do i=1,w1%size
              ztest = w1%list_elems(i)
              call uupper(ztest)
              do j=1,ps_ccount
                 if(ztest.eq.ps_cnames(j)) then
                    if(ncur_components(j).eq.1) then
                       icount = icount + 1
                       if(icount.eq.1) then
                          cur_components = ztest
                       else
                          ilen_new = 1 + len(trim(ztest))
                          if((ilen+ilen_new).gt.(len(cur_components)-5)) then
                             cur_components(ilen+1:ilen+4)=',...'
                             iflag = 1
                          else
                             cur_components(ilen+1:ilen+ilen_new) = ','//ztest
                          endif
                       endif
                       ilen = len(trim(cur_components))
                    endif
                 endif
              enddo
              if(iflag.eq.1) exit
           enddo

        endif
     endif

  enddo

end subroutine ur_cstate_component
