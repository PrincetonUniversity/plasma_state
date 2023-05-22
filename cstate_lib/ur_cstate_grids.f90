subroutine ur_cstate_grids

  ! examine grids contained in the state(s)

  use cstate_lib_mod
  implicit NONE

  !-------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar,lyes
  integer :: isize,ierr,i,ii,inum,imatch_cur,inmax,insel
  integer :: inums(nsss_cur),iself(nsss_cur)

  character*32 :: slbls(nsss_cur)
  character*32 :: ztest,gname

  real*8, dimension(:,:), allocatable :: xxindx,yygrid
  real*8 :: zz(nsss_cur)

  !-------------------------

  luntty = lnout(0)

  call psw_contents('G',ncur_components,wgrid,ierr, state=sss(1))
  isize = wgrid%size

  call clone_window(wgrid,w1)
  
  do
     if(lxout(0)) then
        write(luntty,1001) omit_empty,trim(cur_components)

1001    format(/'ur_cstate_grids options:'/ &
             '  A -- option to omit empty elements from display (',L1,')'/ &
             '  C -- select & restrict view to component(s) (current selection: ',a,')'/ &
             '  T -- grid table of contents & descriptions'/ &
             '  D -- summary comparitive display of grids'/ &
             '  S -- select a single grid for comparitive plot'/ &
             '  L -- modify state label(s)'/ &
             '  Q -- quit this menu'/)
     endif
     call uread('ur_cstate_grids option:')

     if(lchar('Q')) then
        exit

     else if(lchar('A')) then
        if(lxout(0)) write(luntty,*) &
             ' option to omit display of elements that are empty in all states.'

        call uread('omit empty elements ? (Y/N):')
        omit_empty = lyes(0)

     else if(lchar('C')) then
        call ur_cstate_component

     else if(lchar('L')) then
        call ur_cstate_setLabel

     else if(lchar('T')) then
        call ur_cstate_grid_contents

     else if(lchar('D')) then
        call ur_dcomp_display('G', 'Grid_sizes', 'grids')

     else if(lchar('S')) then
        if(lxout(0)) then
           write(luntty,*) '(unambiguous prefix of name may be entered).'
        endif
        call ureadw('enter grid name or "Q" to skip:',gname)
        if(gname.eq.'Q') cycle
        if(gname.eq.'.') cycle

        call cstate_findm(gname,luntty,imatch_cur)
        if(imatch_cur.eq.0) then
           call uclear
           cycle
        endif

        ! find max grid size in selection
        inmax=0
        insel=0
        do i=1,nsss_cur
           call psw_contents('G',ncur_components,ww(i),ierr, state=sss(i))
           zz(i) = i
           inum = ww(i)%ival(imatch_cur)
           if(inum.gt.0) then
              insel=insel+1
              iself(insel) = i
              inums(insel) = inum
              inmax=max(inmax,inum)
              slbls(insel) = sss_label(i)
           else
              write(luntty,*) ' %grid not present for state: ', &
                   trim(sss_label(i))
           endif
        enddo
        if(insel.eq.0) then
           write(luntty,*) ' %grid not present.'
           cycle
        endif
        iself(1) = -iself(1)  ! signals not an (R,Z) contour plot...

        allocate(xxindx(inmax,nsss_cur),yygrid(inmax,nsss_cur))

        do i=1,nsss_cur
           do ii=1,inmax
              xxindx(ii,i)=ii
           enddo

           inum = ww(i)%ival(imatch_cur)
           if(inum.gt.0) then
              call ps_get_grid_by_name(gname, ierr, &
                   state = sss(i), gdata=yygrid(1:inmax,i))
           else
              yygrid(1:inmax,i) = 0.0d0
           endif
        enddo

        !  plot the grids...

        kylab = ' '
        kylab(1) = '  state'
        kylab(2) = 'identification'

        mnlab(2:3) = ' '
        mnlab(2) = 'plot of '//trim(gname)//' grid, vs. grid point index.'
        call ur_cstate_mnlab3(insel,nsss_cur-insel)

        call r8_grfmm1(xxindx,yygrid,inmax,inums,nsss_cur,zz,nsss_cur, &
             iself,insel,'index',gname,mnlab,kylab,slbls)
        
        deallocate(xxindx,yygrid)

     else
        call uermsg(' ?ur_cstate_grids: unrecognized option.',' ',0)
     endif
  enddo

end subroutine ur_cstate_grids
