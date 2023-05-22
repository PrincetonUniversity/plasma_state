subroutine ps_getlabel(ps,id,label,units,author,iout,ierr)

  !  retrieve labeling information on a profile
  !  DMC 3 Jan 2007

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions

  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps       ! plasma state object
  integer, intent(in) :: id       ! ID of profile for which label info needed

  character*(*), intent(out) :: label
  character*(*), intent(out) :: units
  character*(*), intent(out) :: author

  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-----------------------
  !  local:
  type (interp_object) :: xobj
  type (xplasma), pointer :: s

  !-----------------------
  ! executable code...

  call ps_tagchk('ps_getLabel',ps%itag,iout,ierr)
  if(ierr.ne.0) then
     label=' '
     units=' '
     author=' '
     return
  endif

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_prof_info(s,id,ierr, &
       label=label,units=units,author=author)

  if(ierr.ne.0) then
     label=' '
     units=' '
     author=' '
     write(iout,*) ' ?xplasma2 error in ps_getlabel:'
     call xplasma_error(s,ierr,iout)
  endif

end subroutine ps_getlabel
