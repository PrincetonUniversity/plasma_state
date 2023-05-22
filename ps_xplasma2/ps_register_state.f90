subroutine ps_register_state(state,sname,iout,ierr)

  ! register a plasma state object:
  !   1) mark as initialized
  !   2) copy tagname into state memory
  !   3) create and initialize xplasma interpolation object

  use plasma_state_definition_mod  ! plasma state object definitions
  use plasma_state_xpobj           ! XPLASMA2 state object definitions
  implicit NONE
  
  type (plasma_state) :: state
  character*(*), intent(in) :: sname  ! name for this state object
  !  short alphanumeric name, 1st character not a digit.

  integer, intent(in) :: iout         ! LUN for messages
  integer, intent(out) :: ierr        ! status code, 0=OK

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-------------------------------

  state%itag = 1

  state%chtag = sname

  allocate(xobj%s_xpobj)

  s => xobj%s_xpobj

  call xplasma_init(s, sname, ierr)

  ! transfer back
  call ps_reg_backtrans(xobj, state)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_register_state: xplasma interpolation object initialization error.'
     write(iout,*) '  state tag name: "',trim(sname),'".'
     call xplasma_error(s,ierr,iout)
  endif

end subroutine ps_register_state

subroutine ps_deregister_state(state,iout,ierr)

  ! deregister a plasma state object:
  !   1) mark as UNinitialized
  !   2) clear tagname
  !   3) clean up and remove xplasma interpolation object

  use plasma_state_definition_mod  ! plasma state object definitions
  use plasma_state_xpobj           ! XPLASMA2 state object definitions
  implicit NONE
  
  type (plasma_state) :: state
  integer, intent(in) :: iout         ! LUN for messages
  integer, intent(out) :: ierr        ! status code, 0=OK

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-------------------------------

  state%itag = ps_unInit
  state%chtag = ' '

  ! transfer opaque representation imbedded in plasma state object
  xobj = transfer(state%iobj, xobj)
  s => xobj%s_xpobj

  call xplasma_free(s, ierr, total=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_deregister_state: xplasma_free error; continuing...'
     call xplasma_error(s,ierr,iout)
  endif

  nullify(s)

  deallocate(xobj%s_xpobj)
  nullify(xobj%s_xpobj)

  state%iobj = 0

end subroutine ps_deregister_state

subroutine ps_reg_backtrans(xobj, state)

  use plasma_state_definition_mod  ! plasma state object definitions
  use plasma_state_xpobj           ! XPLASMA2 state object definitions
  implicit NONE
  
  type (plasma_state) :: state
  type (interp_object) :: xobj

  state%iobj = transfer(xobj, state%iobj)

end subroutine ps_reg_backtrans
