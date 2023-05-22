subroutine ps_grid_get(iobj, coord_tag, gridname, iauthor, label, &
     max_altnames, alt_author, alt_gridname, &
     n, k_ccw, grid_data, iout, ierr)

  !------------------------------------------------------------
  ! D. McCune 8 Oct 2006

  ! XPLASMA2 implementation of plasma_state subroutine
  ! fetch a grid, perform error checking

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.

  ! Added: alternate grid names.  This is a mechanism to allow
  ! earlier versions of state file data to be read with the current
  ! software.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)  ! opaque container for xplasma object

  character*(*), intent(in) :: coord_tag   ! "RHO", "TH", "R", or "Z", ...
  character*(*), intent(in) :: gridname    ! NAME of grid 
  integer, intent(in) :: iauthor           ! author ID (checked)

  character*(*), intent(out) :: label      ! grid label

  integer, intent(in) :: max_altnames      ! array dimension: alternate names
  character*(*), intent(in) :: alt_author(max_altnames)  ! alternate author IDs
  character*(*), intent(in) :: alt_gridname(max_altnames)  ! alt. grid names

  integer, intent(in) :: n                 ! expected grid data size
  integer, intent(in) :: k_ccw             ! CCW flag ("TH" only)

  REAL(KIND=rspec) :: grid_data(n)          ! grid data returned

  integer, intent(in) :: iout              ! LUN for error messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: id,icoord,ialt,imatch
  character*32 pauth,zauth
  logical :: ccwflag

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_grid_get: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  find grid ID

  call xplasma_gridId(s,gridname,id)
  if(id.eq.0) then
     do ialt = 1,max_altnames
        if(alt_gridname(ialt).eq.' ') exit
        call xplasma_gridId(s,alt_gridname(ialt),id)
        if(id.ne.0) exit
     enddo
     if(id.eq.0) then
        ierr=705
        write(iout,*) ' ?ps_grid_get: grid not found: ',trim(gridname)
        return
     endif
  endif

  !--------------------------------------------------
  !  fetch and check grid info

  call xplasma_grid_info(s,id,ierr, &
       coord=icoord, label=label, author=pauth)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_grid_get: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=706

  else
     ! general checks...

     ccwflag = .TRUE.
     if(icoord.eq.xplasma_theta_coord) then
        ccwflag = (k_ccw.ne.ps_ccw_reverse)
     endif

     if(ps_cnames(iauthor).ne.pauth) then
        imatch = 0
        do ialt = 1,max_altnames
           zauth = alt_author(ialt)
           call uupper(zauth)
           if(zauth.eq.pauth) then
              imatch=ialt
              exit
           endif
        enddo

        if(imatch.eq.0) then
           write(iout,*) ' ?ps_grid_get: author label mismatch:'
           write(iout,*) '  grid name: "',trim(gridname),'".'
           write(iout,*) '  grid has: "',trim(pauth), &
                '"; passed author reference: "',trim(ps_cnames(iauthor)),'".'
           ierr=706
        else
           call xplasma_reset_author(s,id,pauth,ps_cnames(iauthor),ierr)
        endif
     endif

  endif

  if(ierr.ne.0) return

  !--------------------------------------------------
  ! OK fetch the data

  call xplasma_grid(s,id,grid_data,ierr, ccwflag=ccwflag)

  if(ierr.ne.0) then
     write(iout,*) ' ?XPLASMA2 error while attempting grid data retrieval:'
     call xplasma_error(s,ierr,iout)
     ierr=707
  endif

end subroutine ps_grid_get

subroutine ps_gridId(iobj,iout,gridname,id,inum)
  !  get ID of grid item

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)  ! opaque container for xplasma object
  integer, intent(in) :: iout   ! LUN for messages
  character*(*), intent(in) :: gridname  ! name of grid item
  integer, intent(out) :: id    ! id of item returned (0 if not found).
  integer, intent(out) :: inum  ! size of grid (0 if none was found).

  !-----------------------
  integer :: ierr

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  id = 0
  inum = 0

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_gridId: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_gridId(s,gridname,id)
  if(id.gt.0) then
     call xplasma_grid_size(s,id,inum,ierr)
  endif

end subroutine ps_gridId

subroutine ps_gridCoord(iobj,iout,id,cname,cunits)

  !  get coordinate tag of grid (id given, fetched by prior call to ps_gridId).

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)  ! opaque container for xplasma object
  integer, intent(in) :: iout   ! LUN for messages
  integer, intent(in) :: id     ! id of grid

  character*(*), intent(out) :: cname  ! coordinate tag: "RHO", "R", "Z", ...
  character*(*), intent(out) :: cunits ! coordinate units

  !-----------------------
  integer :: ierr,idc

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  cname=' '

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_gridCoord: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_grid_info(s,id,ierr, coord=idc, units=cunits)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_gridCoord: xplasma error on xplasma_grid_info call:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  call ps_coordTag(xobj,idc,cname)
  if(cname.eq.'??') then
     call xplasma_grid_info(s,id,ierr, name=cname)
     write(iout,*) &
          ' %ps_gridCoord: no abbreviation found, full grid name used: ', &
          trim(cname)
  endif

end subroutine ps_gridCoord

subroutine ps_grid_get_data(iobj,iout,id,imax,data,inum,icoord,iwarn,ierr)
  !  get grid data, given state tag and grid id.

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)  ! opaque container for xplasma object
  integer, intent(in) :: iout   ! LUN for messages
  integer, intent(in) :: id     ! id of grid

  integer, intent(in) :: imax   ! size of array to receive grid data
  
  REAL(KIND=rspec) :: data(imax)     ! grid data returned (if inum.le.imax)
  integer, intent(out) :: inum       ! actual number of points in grid
  integer, intent(out) :: icoord     ! coordinate tag (indicates "RHO", etc.)

  integer, intent(out) :: iwarn ! =1 if grid data too large for data(1:imax)
  integer, intent(out) :: ierr  ! =0 if OK; >0 if error is detected.

  ! setting iwarn=1 does not cause ierr to be set >1.

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  inum=0
  icoord=0
  iwarn=0
  ierr=0
  data = 0.0_rspec

  if(id.eq.0) return

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_grid_get_data: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_grid_size(s,id,inum,ierr)
  if(ierr.ne.0) return

  if(inum.gt.imax) then
     iwarn=1
     return
  endif

  call xplasma_grid_info(s,id,ierr, coord=icoord)
  if(ierr.ne.0) return

  call xplasma_grid(s,id,data(1:inum),ierr)

end subroutine ps_grid_get_data
