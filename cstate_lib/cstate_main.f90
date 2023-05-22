subroutine cstate_main(imod)

  implicit NONE

  ! construct or extend list of states to be compared; call code to compare
  ! states

  ! routines which use the UREAD cmd line interface have prefix "ur_" in
  ! their names.  Exception: cstate_main.f90, this routine, which calls
  ! ur_ cstate_lib routines.

  ! DMC 25 June 2007

  !----------------------------------------------
  !  argument:

  integer, intent(in) :: imod   ! =1 to allow user modification of state list

  !----------------------------------------------
  !  local:

  integer :: iexit

  !----------------------------------------------

  iexit=0
  do

     if(imod.eq.1) then
        call ur_cstate_list(iexit)
     endif

     if(iexit.eq.1) exit

     call ur_cstate_compare

     iexit=1
  enddo

end subroutine cstate_main
