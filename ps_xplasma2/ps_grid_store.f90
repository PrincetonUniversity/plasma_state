subroutine ps_grid_store(iobj, coord_tag, gridname, iauthor, label, &
     n, k_ccw, grid_data, iout, ierr)

  !------------------------------------------------------------
  ! D. McCune 8 Oct 2006

  ! XPLASMA2 implementation of plasma_state subroutine
  ! store a grid, perform error checking.  NOTE-- if the
  ! grid already exists, its size and values are compared with
  ! those passed; these must match within a high level of tolerance
  ! or an error is reported.

  ! Special code is needed to change a grid, because, generally, profiles
  ! defined over the grids have to be changed when their underlying grids
  ! are changed.  This is *not* handled here.

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)  ! opaque container for xplasma object

  character*(*), intent(in) :: coord_tag   ! "RHO", "TH", "R", or "Z", ...
  character*(*), intent(in) :: gridname    ! NAME of grid 
  integer, intent(in) :: iauthor           ! author ID

  character*(*), intent(in) :: label       ! grid label

  integer, intent(in) :: n                 ! grid data size
  integer, intent(in) :: k_ccw             ! CCW flag ("TH" only)

  REAL(KIND=rspec) :: grid_data(n)         ! grid data to be stored

  integer, intent(in) :: iout              ! LUN for error messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK
  !--------------------------------------------------
  ! local...

  integer :: id,icoord,jcoord,i,inx,iertmp
  REAL(KIND=rspec) :: bdytol,ztol
  REAL(KIND=rspec), dimension(:), allocatable :: grid
  logical :: ccwflag
  character*32 pauth

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_grid_store: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  find grid ID -- if grid is already defined, compare; if not, create

  call xplasma_gridId(s,gridname,id)

  call xplasma_author_set(s,ps_cnames(iauthor),ierr)
  if(ierr.ne.0) then
     ! (this is the only time an xplasma_author call is checked)
     write(iout,*) ' ?ps_grid_store: unexpected xplasma_author_set error:'
     call xplasma_error(s,ierr,iout)
     ierr=201
     return
  endif

  call ps_coordchk(xobj,coord_tag,gridname,id,iauthor, &
       icoord,n,grid_data,iout,ierr)

  call xplasma_author_clear(s,ps_cnames(iauthor),iertmp)

  if(ierr.ne.0) then
     call gen_errmsg
     return
  endif

  ccwflag = .TRUE.
  if(icoord.eq.xplasma_theta_coord) then
     ccwflag = (k_ccw.ne.ps_ccw_reverse)
  endif

  if(id.eq.0) then

     !---------------------------
     ! OK: create the grid

     call xplasma_author_set(s,ps_cnames(iauthor),iertmp)

     call xplasma_create_grid(s,gridname,icoord,grid_data,id,ierr, &
          ccwflag=ccwflag, label=label)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_grid_store: unexpected xplasma_create_grid error:'
        call xplasma_error(s,ierr,iout)
        ierr=201
     endif

     call xplasma_author_clear(s,ps_cnames(iauthor),iertmp)

  else

     !--------------------------------------------------
     !  fetch and check grid info

     call xplasma_grid_info(s,id,ierr, &
          size=inx, coord=jcoord, author=pauth)

     if(ierr.ne.0) then
        write(iout,*) ' ?ps_grid_store: unexpected xplasma_grid_info error:'
        call xplasma_error(s,ierr,iout)
        ierr=206
     else
        ! general checks...

        if(ps_cnames(iauthor).ne.pauth) then
           write(iout,*) ' ?ps_grid_store: existing grid author mismatch:'
           write(iout,*) '  existing grid has: "',trim(pauth), &
                '"; passed author reference: "',trim(ps_cnames(iauthor)),'".'
           ierr=206
        endif

        if(icoord.ne.jcoord) then
           write(iout,*) &
                ' ?ps_grid_store: existing grid coordinate ID mismatch:', &
                icoord, jcoord
           write(iout,*) '  this error is unexpected.'
           ierr=206
        endif

        if(inx.ne.n) then
           write(iout,*) ' ?ps_grid_store: existing grid size mismatch:'
           write(iout,*) '  existing grid size: ',inx, &
                '; passed grid size: ',n
           ierr=206
        endif
     endif

     if(ierr.eq.0) then
        allocate(grid(n))
        call xplasma_grid(s,id,grid,iertmp, ccwflag)

        call xplasma_global_info(s,iertmp, bdytol=bdytol)
        ztol=bdytol*max(abs(grid(1)),abs(grid(n)))

        do i=1,n
           if(abs(grid(i)-grid_data(i)).gt.ztol) then
              if(ierr.eq.0) then
                 ierr=209
                 write(iout,*) ' ?ps_grid_store:  grid value does not match'
                 write(iout,*) '  value in copy of grid already stored:'
              endif
              write(iout,*) '  index# ',i,' stored: ',grid(i), &
                   ' passed: ',grid_data(i)
           endif
        enddo
     endif
  endif

  if(ierr.ne.0) call gen_errmsg

  contains
    subroutine gen_errmsg
      write(iout,*) ' *** error in ps_grid_store, gridname is "'// &
           trim(gridname)//'".'
    end subroutine gen_errmsg
end subroutine ps_grid_store
