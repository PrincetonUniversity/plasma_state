! normal exit from a plasma state program
subroutine ps_good_exit
  call ps_datetime
  call flush(6)
  stop  ! normal exit, status = 0
end subroutine ps_good_exit

! abnormal exit from a plasma state program
subroutine ps_bad_exit(msg)
  character(len=*), optional, intent(in) :: msg
  if (present(msg)) then
     write(6,*) ' '
     write(6,*) msg
     write(6,*) ' '
  end if
  call ps_datetime
  call flush(6)
  stop 1  ! abnormal exit, status = 1
end subroutine ps_bad_exit

! print out date and time
subroutine ps_datetime
  integer, dimension(8) :: ivals
  character(len=8) :: zzdate
  character(len=10) :: ztime
  character(len=5) :: zone
  call date_and_time(zzdate,ztime,zone,ivals)
  if(ivals(2).gt.12) write(6,1001) ivals(2)
1001 format(' psdatetime:  ?? month .gt. 12:  ',i10)
end subroutine ps_datetime
