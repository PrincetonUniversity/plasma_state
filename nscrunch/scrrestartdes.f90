SUBROUTINE scrrestartdes(irst,ier)
  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: zero, two
  use scrunch_inc1
  implicit none
  integer, intent(inout) :: irst
  integer, intent(inout) :: ier
  integer :: l
  !**********************************************************************
  ! This routine either stores an accepted value of the local solution
  ! (irst=1) or reset the present solution to a previous value (irst=2)
  !**********************************************************************
  go to (15,25)irst

15 continue
  xstore(1:n2) = xvec(1:n2)
  return

25 continue
  do l = 1,n2
    xdot(l) = zero
    xvec(l) = xstore(l)
  end do

  delt = .95_fp*delt
  irst = 1
  nresets = nresets + 1
  if(nresets.ge.100) then
    write(lunmsg,*)' Time step reduced 100 times without convergence'
    ier=13
  end if

end SUBROUTINE scrrestartdes
