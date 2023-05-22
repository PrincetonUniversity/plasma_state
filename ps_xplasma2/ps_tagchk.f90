subroutine ps_tagchk(caller_str,itag,iout,ierr)

  use plasma_state_constants_mod
  implicit NONE

  !  quick error check

  character*(*), intent(in) :: caller_str
  integer, intent(in) :: itag   ! = ps_unInit means not initialized...
  integer, intent(in) :: iout   ! unit for error message
  integer, intent(out) :: ierr  ! status code, 0=OK

  !-------------------------

  if(itag.eq.ps_unInit) then
     write(iout,*) ' ?'//trim(caller_str)// &
          ': passed plasma state was never initialized.'
     ierr=999
  else
     ierr=0
  endif

end subroutine ps_tagchk
