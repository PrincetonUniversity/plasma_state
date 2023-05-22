subroutine ur_cstate_mnlab3(ipresent,iabsent)
  ! generate mnlab(3) label "Present in # states; absent in # states."
  ! get the pluralization right

  use cstate_lib_mod
  implicit NONE

  integer, intent(in) :: ipresent   ! #of states present
  integer, intent(in) :: iabsent   ! #of states absent

  !---------------------------------

  mnlab(3) = ' '

  if((ipresent.eq.1).and.(iabsent.eq.1)) then
     mnlab(3)='Present in 1 state; absent in 1 state.'

  else if((ipresent.eq.1).and.(iabsent.ne.1)) then
     write(mnlab(3),'("Present in 1 state; absent in ",i1," states.")') &
          iabsent

  else if((ipresent.ne.1).and.(iabsent.eq.1)) then
     write(mnlab(3),'("Present in ",i1," states; absent in 1 state.")') &
          ipresent

  else
     write(mnlab(3),'("Present in ",i1," states; absent in ",i1," states")') &
          ipresent,iabsent
  endif

end subroutine ur_cstate_mnlab3
