subroutine xplasma_example(filename)

  implicit NONE

  character*(*), intent(in) :: filename

  !
  !  read, access and print TR_PBI data from XPLASMA file "filename".
  !
  !  this is meant as a simple example for prospective XPLASMA uses.
  !

  !--------------------------------------------------
  !  set up a grid -- any grid will do...
  
  integer, parameter :: nx = 21     ! #bdys; #zones = nx-1
  real*8, parameter :: ONE = 1.0d0  ! bdy value of grid (axis is 0.0)

  real*8 x(nx)       ! grid
  real*8 pint(nx)    ! integrated power from TR_PBI profile
  real*8 vol(nx)     ! integrated volumes
  real*8 pdens(nx-1) ! power density "at zone centers".

  integer :: ix,iwant,id,ierr
  real*8 :: xzc

  !-------------

  do ix=1,nx
     x(ix) = (ix-1)*ONE/(nx-1)  ! for nx=20: 0.0,0.05,0.10, ... ,1.00
  enddo
  
  !-------------
  !  read the XPLASMA state file (NetCDF format)

  call eq_restore(filename,ierr)
  if(ierr.ne.0) then
     write(6,*) ' xplasma_example:  failed to read: ',trim(filename)
     return
  endif

  !-------------
  !  get enclosed volumes vs. x

  iwant = 0  ! this means get enclosed volumes vs. x
  call eq_volume(nx,x,iwant,vol,ierr)
  if(ierr.ne.0) then
     write(6,*) ' xplasma_example:  eq_volume call failed: no equilibrium?'
     return
  endif

  !-------------
  !  get ID of TR_PBI profile

  call eq_gfnum('TR_PBI',id)
  if(id.eq.0) then
     write(6,*) ' xplasma_example:  TR_PBI: profile not found.'
     return
  endif

  !-------------
  !  get the data itself (integrated powers vs. x)
  !  by using eq_rgetf we expect the profile to be a function of x only.
  !  here iwant=0 means get the data itself not a derivative

  call eq_rgetf(nx,x,id,iwant,pint,ierr)
  if(ierr.ne.0) then
     write(6,*) ' xplasma_example:  error fetching TR_PBI:  eq_rgetf call.'
     return
  endif

  !-------------
  !  compute power density profile -- presumed appropriate for TR_PBI.

  write(6,*) '  #  x       power density    volume integrated power'
  write(6,*) ' ----------------------------------------------------'

  do ix=1,nx-1
     pdens(ix)=(pint(ix+1)-pint(ix))/(vol(ix+1)-vol(ix))
     xzc = (x(ix)+x(ix+1))/2
     write(6,'(1x,i3,3(1x,1pe12.5))') ix,xzc,pdens(ix),pint(ix+1)
  enddo

end subroutine xplasma_example
