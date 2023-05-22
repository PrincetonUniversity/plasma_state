SUBROUTINE scraborter(iu,message)
  implicit none
  integer, intent(in) :: iu
  character(len=*), intent(in) :: message

  write(iu,*) '  nscrunch(scraborter) abnormal exit message:'
  write(iu,*) trim(message)

  return
END SUBROUTINE scraborter
 
SUBROUTINE scrabwarn(iu,message)
  implicit none
  integer, intent(in) :: iu
  character(len=*), intent(in) :: message

  write(iu,*) '  nscrunch(scrabwarn) warning message:'
  write(iu,*) trim(message)

  return
END SUBROUTINE scrabwarn
 
