SUBROUTINE scrclear
  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: zero
  USE mintrp
  USE scrunch_inc1
  USE scrunch_inc2

  !...begin set dynamic arrays to zero
  implicit none
  rin = zero
  zin = zero
  xvec = zero
  gvec = zero
  xdot = zero
  xstore = zero
  m1 = 0
  n1 = 0
  rmomb = zero
  zmomb = zero
  mm = zero
  dm1 = zero
  faccon = zero
  xmpq = zero
  result = zero
  !...end set dynamic arrays to zero
  RETURN
END SUBROUTINE scrclear
