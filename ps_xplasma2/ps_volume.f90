subroutine ps_eq_volume(iobj,isize,rhovals,vols,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 10 Apr 2007
  !
  ! Fetch XPLASMA2 enclosed volumes, from equilibrium Vol(rho)
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer :: iobj(ps_container_size)     ! xplasma opaque container

  integer, intent(in) :: isize     ! size of array to receive grid
  real(kind=rspec), intent(in) :: rhovals(isize) ! ascending grid of rho values
  real(kind=rspec) :: vols(isize)  ! array to receive volumes(rhovals)

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  !-----------------------
  ! local:
  integer :: ii,iflag,iwarn
  real(kind=rspec) zrhos(isize)
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  vols = 0.0_rspec

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_eq_volume: invalid or unitialized xplasma object.'
     return
  endif

  iflag = 0
  iwarn = 0

  do ii=1,isize
     if(rhovals(ii).lt.(-1.0e-10_rspec)) then
        write(iout,*) &
             ' ?ps_volume: negative rhovals(',ii,'):',rhovals(ii)
        ierr = 999
     else if(rhovals(ii).gt.(1.0000000001_rspec)) then
        write(iout,*) &
             ' ?ps_volume: rhovals(',ii,') exceeds 1.000:',rhovals(ii)
        ierr = 999
     else if((rhovals(ii).lt.0.0_rspec).or.(rhovals(ii).gt.1.0_rspec)) then
        zrhos(ii)=max(0.0_rspec,min(1.0_rspec,rhovals(ii)))
        iflag = iflag + 1
     else
        zrhos(ii) = rhovals(ii)
     endif
  enddo
  if(ierr.ne.0) return

  do ii=1,isize-1
     if(zrhos(ii+1).le.zrhos(ii)) then
        write(iout,*) &
             ' ?ps_volume: rho grid input values not strict ascending:'
        write(iout,'("  rhovals(",i4,")=",1pe11.4)') ii,zrhos(ii)
        write(iout,'("  rhovals(",i4,")=",1pe11.4)') ii+1,zrhos(ii+1)
        if(iflag.gt.0) then
           if((zrhos(ii).eq.0.0_rspec).or.(zrhos(ii).eq.1.0_rspec)) then
              write(iout,*) '  NOTE: rhovals forced to be between 0.0 and 1.0'
           endif
        endif
        ierr=999
        iwarn = iwarn + 1
        if(iwarn.gt.3) exit
     endif
  enddo
  if(ierr.ne.0) return

  call xplasma_volume(s,zrhos,vols,ierr)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_volume: xplasma_volume error:'
     call xplasma_error(s,ierr,iout)
     ierr=999
     return
  endif

end subroutine ps_eq_volume

subroutine ps_eq_area(iobj,isize,rhovals,areas,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 10 Apr 2007
  !
  ! Fetch XPLASMA2 enclosed areas, from equilibrium Vol(rho)
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer :: iobj(ps_container_size)     ! xplasma opaque container

  integer, intent(in) :: isize     ! size of array to receive grid
  real(kind=rspec), intent(in) :: rhovals(isize) ! ascending grid of rho values
  real(kind=rspec) :: areas(isize)  ! array to receive areas(rhovals)

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  !-----------------------
  ! local:
  integer :: ii,iflag,iwarn
  real(kind=rspec) zrhos(isize)
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  areas = 0.0_rspec

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_eq_area: invalid or unitialized xplasma object.'
     return
  endif

  iflag = 0
  iwarn = 0

  do ii=1,isize
     if(rhovals(ii).lt.(-1.0e-10_rspec)) then
        write(iout,*) &
             ' ?ps_area: negative rhovals(',ii,'):',rhovals(ii)
        ierr = 999
     else if(rhovals(ii).gt.(1.0000000001_rspec)) then
        write(iout,*) &
             ' ?ps_area: rhovals(',ii,') exceeds 1.000:',rhovals(ii)
        ierr = 999
     else if((rhovals(ii).lt.0.0_rspec).or.(rhovals(ii).gt.1.0_rspec)) then
        zrhos(ii)=max(0.0_rspec,min(1.0_rspec,rhovals(ii)))
        iflag = iflag + 1
     else
        zrhos(ii) = rhovals(ii)
     endif
  enddo
  if(ierr.ne.0) return

  do ii=1,isize-1
     if(zrhos(ii+1).le.zrhos(ii)) then
        write(iout,*) &
             ' ?ps_area: rho grid input values not strict ascending:'
        write(iout,'("  rhovals(",i4,")=",1pe11.4)') ii,zrhos(ii)
        write(iout,'("  rhovals(",i4,")=",1pe11.4)') ii+1,zrhos(ii+1)
        if(iflag.gt.0) then
           if((zrhos(ii).eq.0.0_rspec).or.(zrhos(ii).eq.1.0_rspec)) then
              write(iout,*) '  NOTE: rhovals forced to be between 0.0 and 1.0'
           endif
        endif
        ierr=999
        iwarn = iwarn + 1
        if(iwarn.gt.3) exit
     endif
  enddo
  if(ierr.ne.0) return

  call xplasma_area(s,zrhos,areas,ierr)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_area: xplasma_area error:'
     call xplasma_error(s,ierr,iout)
     ierr=999
     return
  endif

end subroutine ps_eq_area
