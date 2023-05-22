subroutine ps_get_integ(iobj, rhoname, idi, iout, ierr)

  !------------------------------------------------------------
  ! D. McCune 4 Apr 2007

  ! XPLASMA2 implementation of plasma_state subroutine
  ! get or create a metric integrator for the indicated RHO grid

  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)     ! xplasma opaque container

  character*(*), intent(in) :: rhoname     ! NAME of RHO grid 

  integer, intent(out) :: idi              ! handle for integrator (0 if fail)

  integer, intent(in) :: iout              ! LUN for error messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: id,icoord,isize,iertmp
  character*32 pauth,integ_name
  real(kind=rspec), dimension(:), allocatable :: rhogrid

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------
  idi = 0

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_get_integ: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  find grid ID

  call xplasma_gridId(s,rhoname,id)
  if(id.eq.0) then
     if(id.eq.0) then
        ierr=705
        write(iout,*) ' ?ps_get_integ: RHO grid not found: ',trim(rhoname)
        return
     endif
  endif

  !--------------------------------------------------
  !  fetch and check grid info

  call xplasma_grid_info(s,id,ierr, &
       coord=icoord, author=pauth, size=isize)

  if(icoord.ne.xplasma_rho_coord) then
     write(iout,*) ' ?ps_get_integ: not a RHO grid: ',trim(rhoname)
     if(ierr.ne.0) call xplasma_error(s,ierr,iout)
     ierr=705
     return
  endif

  !  get the integrator ID; if not found create...

  integ_name = '__'//trim(rhoname)//'_INTEG1D'
  call xplasma_blkbxId(s,integ_name,idi)

  if(idi.eq.0) then
     ! OK; need to create integrator; fetch the grid data

     allocate(rhogrid(isize))
     call xplasma_grid(s,id,rhogrid,ierr)

     if(ierr.ne.0) then
        write(iout,*) &
             ' ?ps_get_integ: XPLASMA2 error attempting rho retrieval: ', &
             trim(rhoname)
        call xplasma_error(s,ierr,iout)
        ierr=707

     else

        !  use xplasma_xmhd as author -- this arranges for cache data to be
        !  cleared if the equilibrium changes.

        call xplasma_author_set(s,xplasma_xmhd,iertmp)

        call xplasma_create_integ(s,integ_name,rhogrid,idi,ierr)

        call xplasma_author_clear(s,xplasma_xmhd,iertmp)

        if(ierr.ne.0) then
           write(iout,*) &
                ' ?ps_get_integ: XPLASMA2 error creating rho integrator: ', &
                trim(rhoname)
           call xplasma_error(s,ierr,iout)
        endif
     endif

     deallocate(rhogrid)
  endif

end subroutine ps_get_integ

subroutine ps_get_userInteg(iobj,rhoname,nrho,rho,idi,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 4 Apr 2007

  ! XPLASMA2 implementation of plasma_state subroutine
  ! get or create a metric integrator for the user supplied RHO grid or subset

  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)     ! xplasma opaque container

  character*(*), intent(in) :: rhoname     ! NAME of user provided RHO grid 

  integer, intent(in) :: nrho              ! size of user provided RHO grid 
  real(kind=rspec) :: rho(nrho)            ! the user provided RHO grid

  integer, intent(out) :: idi              ! handle for integrator (0 if fail)

  integer, intent(in) :: iout              ! LUN for error messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: id,i,incr,idiff,iertmp
  character*32 integ_name

  integer, pointer, dimension(:) :: ibuf
  real*8, pointer, dimension(:) :: rbuf

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------
  idi = 0

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_get_userInteg: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------

  integ_name = '__'//trim(rhoname)//'_INTEG1D'

  call xplasma_blkbxId(s,integ_name,id)
  if(id.gt.0) then

     call xplasma_blackBox_retrieve(s,id,ierr, ia_ptr=ibuf, r8a_ptr=rbuf)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_get_userInteg: unexpected xplasma2 error:'
        call xplasma_error(s,ierr,iout)
        return
     endif

     incr=1
     idiff=0
     do i=1,nrho
        if(rho(i).ne.rbuf(incr+i)) then
           idiff=1
           exit
        endif
     enddo

     if(idiff.gt.0) then
        id = 0   ! user RHO grid changed...
     endif
  endif

  if(id.gt.0) then
     idi = id

  else
     ! create integrator
     !  use xplasma_xmhd as author -- this arranges for cache data to be
     !  cleared if the equilibrium changes.

     call xplasma_author_set(s,xplasma_xmhd,iertmp)
        
     call xplasma_create_integ(s,integ_name,rho,idi,ierr)

     call xplasma_author_clear(s,xplasma_xmhd,iertmp)

     if(ierr.ne.0) then
        write(iout,*) &
             ' ?ps_get_userInteg: XPLASMA2 error creating rho integrator: ', &
             trim(rhoname)
        call xplasma_error(s,ierr,iout)
     endif
  endif

end subroutine ps_get_userInteg
