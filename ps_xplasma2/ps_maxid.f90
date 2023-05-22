subroutine ps_maxid(iobj,idmax,iout,ierr)

  !------------------------------------------------------------
  ! return the maximum ID in the xplasma dictionary
  ! D. McCune 13 Oct 2006
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)     ! xplasma opaque container

  integer, intent(out) :: idmax      ! the max ID returned

  integer, intent(in) :: iout              ! LUN for error messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE., nitems=idmax)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_maxid: invalid or unitialized xplasma object.'
     return
  endif

end subroutine ps_maxid
