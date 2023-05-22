subroutine ps_1dprof_grid(iobj,profId,profName,gridName,iout,ierr)

  !------------------------------------------------------------
  ! given ID and/or name of a 1d f(rho) profile, return the name 
  ! of the grid over which the profile is defined.
  !
  ! DMC May 2008
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer :: iobj(ps_container_size)  ! opaque container for xplasma object

  !  if ID.gt.0 is provided, profName is fetched for output;
  !  if ID.le.0 is provided, profName is used as input, to look up ID.

  integer, intent(inout) :: profId    ! ID of 1d profile
  character*(*), intent(inout) :: profName ! name of 1d profile

  character*(*), intent(out) :: gridName   ! name of profile's grid
  
  integer, intent(in) :: iout         ! Fortran LUN of file for messages

  integer, intent(out) :: ierr        ! exit status, 0=OK

  ! non-zero value means: profile ID information invalid or points to
  ! something other than a 1d profile.

  !-----------------------
  ! local:
  integer :: id,idzero,irank
  character*32 zname
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------
 
  ierr = 0
  gridName = ' '

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  if(profId.le.0) then
     idzero = 1
     call xplasma_profId(s,profName,profId)
     if(profId.le.0) then
        write(iout,*) ' ?ps_1dprof_grid: invalid profile ID and/or name: ', &
             profId,' "'//trim(profName)//'".'
        ierr=1
        return
     endif

  else
     idzero = 0   ! non-zero ID was provided.
  endif

  call xplasma_prof_info(s,profId,ierr, rank=irank, name=zname, gridId1=id)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_1dprof_grid: could not find profile:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  if(irank.ne.1) then
     write(iout,*) ' ?ps_1dprof_grid: profile not 1d: ',profId,' "'// &
          trim(zname)//'".'
     ierr=1
     return
  endif

  if(idzero.eq.0) profName=zname  ! name is output; non-zero ID was provided.

  call xplasma_grid_info(s,id,ierr, name=gridName)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_1dprof_grid: grid look up failed:'
     call xplasma_error(s,ierr,iout)
  endif

end subroutine ps_1dprof_grid

subroutine ps_grid_exist(iobj,gridname,iout,iexist)

  !------------------------------------------------------------
  ! return iexist=.TRUE. if grid exists; .FALSE. if not or if an error occrs.
  ! only print a message if the xplasma object initialization check fails.
  !
  ! DMC Feb 2008
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer :: iobj(ps_container_size)  ! opaque container for xplasma object

  character*(*), intent(in) :: gridname  ! name of grid

  integer, intent(in) :: iout     ! Fortran LUN of file for messages

  logical, intent(out) :: iexist  ! .TRUE. if grid exists

  !-----------------------
  ! local:
  integer :: id,ierr
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------
 
  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  iexist = .FALSE.

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_grid_size: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_gridId(s,gridname,id)

  iexist = (id.NE.0)

end subroutine ps_grid_exist

subroutine ps_grid_size(iobj,gridname,isize,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 10 Apr 2007
  !
  ! Fetch size of XPLASMA2 grid, supplying the ASCII grid name
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer :: iobj(ps_container_size)  ! opaque container for xplasma object

  character*(*), intent(in) :: gridname  ! name of grid

  integer, intent(out) :: isize   ! size of grid

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  !-----------------------
  ! local:
  integer :: id
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  isize = 0

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_grid_size: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_gridId(s,gridname,id)
  if(id.eq.0) then
     ierr=999
     write(iout,*) ' ?ps_grid_size: grid name unrecognized: ',trim(gridname)
     return
  endif

  call xplasma_grid_info(s,id,ierr, size=isize)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_grid_size: xplasma_grid_info error:'
     call xplasma_error(s,ierr,iout)
     ierr=999
  endif

end subroutine ps_grid_size
  
subroutine ps_grid(iobj,gridname,isize,gridvals,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 10 Apr 2007
  !
  ! Fetch XPLASMA2 grid, supplying the ASCII grid name & sized array
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  !  passed arguments:

  integer :: iobj(ps_container_size)  ! opaque container for xplasma object

  character*(*), intent(in) :: gridname  ! name of grid

  integer, intent(in) :: isize    ! size of array to receive grid
  real(kind=rspec) :: gridvals(isize)  ! array to receive grid

  integer, intent(in) :: iout     ! Fortran LUN of file for messages
  integer, intent(out) :: ierr    ! completion status code (0=OK)

  !-----------------------
  ! local:
  integer :: id,jsize
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  gridvals = 0.0_rspec

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_grid: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_gridId(s,gridname,id)
  if(id.eq.0) then
     ierr=999
     write(iout,*) ' ?ps_grid: grid name unrecognized: ',trim(gridname)
     return
  endif

  call xplasma_grid_info(s,id,ierr, size=jsize)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_grid: xplasma_grid_info error:'
     call xplasma_error(s,ierr,iout)
     ierr=999
     return
  endif

  if(jsize.gt.isize) then
     write(iout,*) ' ?ps_grid: passed array size too small for grid ', &
          trim(gridname)
     write(iout,*) '  size needed: ',jsize
     write(iout,*) '  size passed: ',isize
     ierr=999
     return
  endif

  call xplasma_grid(s,id,gridvals(1:jsize),ierr)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_grid: xplasma_grid error:'
     call xplasma_error(s,ierr,iout)
     ierr=999
     return
  endif

end subroutine ps_grid
