subroutine ps_limcon(iobj,npts,rpts,zpts,igot,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 19 Dec 2006

  ! XPLASMA2 implementation of plasma_state subroutine
  !   fetch the (R,Z) axisymmetric limiter contour (1:igot)
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------

  integer :: iobj(ps_container_size)     ! xplasma opaque container
  integer, intent(in) :: npts              ! #pts in limiter arrays

  REAL(kind=rspec), intent(out) :: Rpts(npts),Zpts(npts) ! limiter contour
  integer, intent(out) :: igot             ! #pts in limiter data returned

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !-----------------------
  integer :: itype,ipts
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  !  initialize output; check state tag

  igot=0
  Rpts=0
  Zpts=0

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_limcon: invalid or unitialized xplasma object.'
     return
  endif

  !-----------------------

  call xplasma_lim_info(s,ierr, itype=itype, npts=ipts)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_limcon: xplasma_lim_info error:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  if(itype.ne.100) then
     write(iout,*) ' ?ps_limcon: no contour limiter available; itype=',itype
     ierr=999
     return
  endif

  if(ipts.gt.npts) then
     write(iout,*) ' ?ps_limcon: passed array size "npts" too small.'
     write(iout,*) '  received npts=',npts,'; need at least ',ipts
     ierr=999
     return
  endif

  igot=ipts

  call xplasma_lim_info(s,ierr, rpts=Rpts(1:igot), zpts=Zpts(1:igot))

end subroutine ps_limcon
