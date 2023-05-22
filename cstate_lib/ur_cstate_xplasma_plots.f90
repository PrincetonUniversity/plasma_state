subroutine ur_cstate_xplasma_plots

  !  XPLASMA level plots of a single individual state

  use plasma_state_mod
  implicit NONE

  !---------------------------------
  !  local:

  integer :: luntty,lnout
  integer :: i,istate,ierr
  logical :: lxout
  !---------------------------------

  luntty = lnout(0)

  if(nsss_cur.eq.0) then

     write(luntty,*) ' %ur_cstate_xplasma_plots: no states are loaded.'
     return

  endif

  do
     if(lxout(0)) then
        write(luntty,*) ' '
        write(luntty,'(1x,i1," states are loaded:")') nsss_cur
        do i=1,nsss_cur
           write(luntty,*) ' '
           write(luntty,'(" state #",i1,"  label: ",a)') i,trim(sss_label(i))
           write(luntty,'("    path: ",a)') trim(sss_path(i))
        enddo

        write(luntty,*) ' '
        write(luntty,*) ' state data to be plotted with original labels.'
        write(luntty,*) ' enter state # or "0" or "Q" to skip.'
        write(luntty,*) ' '
     endif

     istate = 0
     call ureadi('ur_cstate_xplasma_plots: state# or "0" to exit:',istate)

     if(istate.eq.0) exit

     if((istate.lt.0).or.(istate.gt.nsss_cur)) then
        call uermsi('state number not in available range',istate,0)
        cycle
     endif

     call ps_dbg_plot(sss(istate)%iobj, luntty, ierr)
  enddo

end subroutine ur_cstate_xplasma_plots
