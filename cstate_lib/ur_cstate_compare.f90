subroutine ur_cstate_compare

  !  compare contents of one or more states

  use cstate_lib_mod
  implicit NONE

  !-------------------------
  ! local

  character*32 ctmp,ztest

  integer :: i,iac,icomp,idiff
  integer :: luntty,lnout
  logical :: lxout,lchar

  !-------------------------

  mnlab(1) = 'cstate: PS_vsn '//trim(ps_version_id)
  iac = len_trim(mnlab(1)) + 2
  call fdate(mnlab(1)(iac:len(mnlab(1))))

  luntty = lnout(0)

  if(nsss_cur.eq.0) then
     write(luntty,*) ' ?ur_cstate_compare: no states found.'
     call uclear
     return
  endif

  !  allocate module variable here
  !  must free & deallocate before exiting this subroutine!

  allocate(ww(nsss_cur),wdisp(nsss_cur))
  do i=1,nsss_cur
     call free_window(ww(i))
     call free_window(wdisp(i))
  enddo

  do
     if(lxout(0)) then
        write(luntty,1001) trim(cur_components)

1001    format(/ &
             ' ur_cstate_compare options:'/ &
             '  C -- select & restrict view to components (current selection: ',a,')'/ &
             '  A -- category lookup'/ &
             '  T -- tables of contents'// &
             '  G -- view/compare GRIDS'/ &
             '  S -- view/compare SPECIES LISTS'/ &
             '  I -- view/compare ITEM LISTS'// &
             '  M -- view/compare MACHINE DESCRIPTION other than item lists'/ &
             '  F -- view/compare SHOT CONFIGURATION other than species lists'/ &
             '  N -- view/compare SIMULATION INITIALIZATION data other than grids'// &
             '  D -- view/compare STATE DATA'/ &
             '  P -- view/compare STATE PROFILES'// &
             '  X -- "xplasma" plots of individual state'/ &
             '  Y -- additional comparitive display options'/ &
             '       (limiter plots; quasineutrality, Zeff checks...)'/ &
             '  Q -- quit')
        if(nsss_cur.eq.2) then
           write(luntty,1002)
1002       format( &
                '  2 -- view summary of differences between the two selected states.')
        endif
     endif

     call uread('ur_cstate_compare: option select:')

     if(lchar('Q')) then
        exit

     else if(lchar('2')) then
        if(nsss_cur.ne.2) then
           call uermsg('option "2" only available when exactly 2 states are selected.',' ',0)
           cycle
        else
           call plasma_state_hash_diff( &
                sss(1),sss_label(1),sss(2),sss_label(2),'*',icomp,idiff)
        endif

     else if(lchar('X')) then
        call ur_cstate_xplasma_plots

     else if(lchar('Y')) then
        call ur_cstate_misc_plots

     else if(lchar('C')) then
        call ur_cstate_component

     else if(lchar('A')) then
        call ureadw('enter state element name or substring, ".Q" to skip:', &
             ctmp)
        if(ctmp.eq.'.Q') cycle

        call ps_print_category(luntty,ctmp)

     else if(lchar('T')) then
        call ur_cstate_contents

     else if(lchar('G')) then
        call ur_cstate_grids

     else if(lchar('S')) then
        call ur_cstate_slists

     else if(lchar('I')) then
        call ur_cstate_ilists

     else if(lchar('M')) then
        call ur_cstate_sdata('MACHINE_DESCRIPTION')

     else if(lchar('F')) then
        call ur_cstate_sdata('SHOT_CONFIGURATION')

     else if(lchar('N')) then
        call ur_cstate_sdata('SIMULATION_INIT')

     else if(lchar('D')) then
        call ur_cstate_sdata('STATE_DATA')

     else if(lchar('P')) then
        call ur_cstate_sprof

     else
        call uermsg(' ?ur_cstate_compare: unrecognized option',' ',0)
     endif

  enddo

  do i=1,nsss_cur
     call free_window(ww(i))
     call free_window(wdisp(i))
  enddo
  deallocate(ww,wdisp)

end subroutine ur_cstate_compare
