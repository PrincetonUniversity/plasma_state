subroutine eqm_geq_curtime(ztime1)

  ! return the currently selected G-eqdsk time from MDS+ data
  ! if a file was read (i.e. no MDS+, no time dependence), 
  !   ztime1=0.00 will be returned.

  use eqi_geq_mod
  implicit NONE

  real*8, intent(out) :: ztime1

  call geq_curtime(geq,ztime1)

end subroutine eqm_geq_curtime
