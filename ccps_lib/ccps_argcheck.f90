subroutine ccps_argcheck(srname,iobj,ierr)

  ! ccps_lib internal routine
  ! check that object argument is not empty (not all zero)

  use plasma_state_mod
  implicit NONE

  character*(*), intent(in) :: srname  ! name of caller
  integer :: iobj(*)
  integer, intent(out) :: ierr         ! status code 0=OK

  !-------------------------
  integer :: ii, ichk, iout
  !-------------------------

  ierr=0

  ichk=0
  do ii=1,ps_container_size
     if(iobj(ii).ne.0) then
        ichk=1
        exit
     endif
  enddo

  if(ichk.eq.0) then
     iout = Swim_err_out_file
     write(iout,*) ' ?'//trim(srname)//': state container "iobj" empty (all zeroes).'
     ierr=1
     return
  endif

end subroutine ccps_argcheck
