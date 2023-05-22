subroutine ps_limsize(iobj,ipts,iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 19 Dec 2006

  ! XPLASMA2 implementation of plasma_state subroutine
  !   fetch the length of the (R,Z) axisymmetric limiter contour (#pts).
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------

  integer :: iobj(ps_container_size)     ! xplasma opaque container
  integer, intent(out) :: ipts             ! #pts in limiter

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !-----------------------
  integer :: itype
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_limsize: invalid or unitialized xplasma object.'
     return
  endif

  !-----------------------

  call xplasma_lim_info(s,ierr, itype=itype, npts=ipts)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_limsize: xplasma_lim_info error:'
     call xplasma_error(s,ierr,iout)
     ipts=0
     return
  endif

  if(itype.ne.100) then
     write(iout,*) ' ?ps_limsize: no contour limiter available; itype=',itype
     ipts=0
     ierr=999
     return
  endif

end subroutine ps_limsize
