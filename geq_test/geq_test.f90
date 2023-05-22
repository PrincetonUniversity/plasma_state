program geq_test

  ! minimal test driver for geqdsk_mds (file only, no MDS+).
  !   (actually trying to replicate a Pathscale/debugger interface problem
  !   in a context simpler than the Plasma State test driver).

  use geqdsk_mod
  implicit NONE

  type(geqdsk) :: geq          ! GEQDISK data
  character*20 :: filename = 'g104403.nstx'
  integer :: iok

  !----------------------------

  call geq_init(geq, filename, iok)
  call geq_error(iok)

  if(iok.eq.0) then

     write(6,*) ' fpol_tm dimension: ',size(geq%fpol_tm)
     write(6,*) ' PsiRZ 1st dimension: ',size(geq%psirz_wb__rad,1)
     write(6,*) ' PsiRZ 2nd dimension: ',size(geq%psirz_wb__rad,2)

  endif

end program geq_test

! appended to resolve an MDS-related external (not expected to be used)


!subroutine mdsCacheUnConnect

!  write(6,*) ' ?mdsCacheUnConnect: dummy call '
!  return

!end subroutine mdsCacheUnConnect
