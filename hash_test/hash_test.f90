program hash_test

  use plasma_hash_code
  implicit NONE

  !  simple test of the hash code

  real*8 :: zinput,r8dcod
  integer :: ier,ihash,ihash2,i
  logical :: lchar

  real*8 :: qktest(10)
  character*10 ctest(2)
  !----------------------

  do i=1,10
     qktest(i)=i
  enddo

  ctest(1)= 'hello'
  ctest(2)= 'dolly'

  ihash=17
  ihash2=23

  call hashCode(qktest,ihash,ihash2)
  write(6,*) ' qktest hash pair: ',ihash,ihash2

  ihash=17

  call hashCode(qktest,ihash)
  write(6,*) ' qktest hash single: ',ihash

  ihash=17
  ihash2=23

  call hashCode(ctest(1),ihash,ihash2)
  write(6,*) ' ctest(1) hash pair: ',ihash,ihash2

  ihash=17
  ihash2=23

  call hashCode(ctest,ihash,ihash2)
  write(6,*) ' ctest hash pair: ',ihash,ihash2

  ihash=17

  call hashCode(ctest(1),ihash)
  write(6,*) ' ctest(1) hash single: ',ihash

  ihash=17

  call hashCode(ctest,ihash)
  write(6,*) ' ctest hash single: ',ihash

  call uinita(0)

  do 
     call uread('new real*8 number or "Q" to quit:')
     if(lchar('Q')) exit

     zinput=r8dcod(ier)
     if(ier.ne.0) then
        write(6,*) ' input skipped (not real*8)'
        call uclear

     else
        ihash=17
        call hashCode(zinput,ihash)
        write(6,*) ' input=',zinput,' ... hash(from 17)=',ihash

     endif

  enddo

  call uexita(0)

end program hash_test
