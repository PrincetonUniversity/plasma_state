subroutine cstate_gen_indmap(indmap,isize)

  !  create index map:  members of groups are mapped to group that contains
  !  them...

  use cstate_lib_mod
  implicit NONE

  !-------------------------
  !  passed:

  integer, intent(in) :: isize
  integer, intent(out) :: indmap(isize)

  !-------------------------
  !  local:

  integer :: itest,ii,indx,ic,ic1,ic2,icprev,ilen
  logical :: ilast
  character*32 zname
  character*200 zbuf
  !-------------------------
 
  do ii=1,isize
     indmap(ii)=ii
  enddo

  if(isize.ne.w1%size) then
     call ps_bad_exit(' ?cstate_gen_indmap: internal error, w1 & indmap sizes not consistent.')
  endif

  do ii=1,isize

     itest = w1%ival(ii)
     if((itest.lt.-1).or.(itest.gt.1)) then
        ! group name detected, parse out contents

        zbuf = w1%elem_descr(ii)
        ilen = len(trim(zbuf))

        icprev=1
        ilast = .FALSE.

        do 
           ic1=icprev+1
           do ic=ic1,ilen
              if(zbuf(ic:ic).eq.',') then
                 ilast=.FALSE.
                 icprev=ic
                 ic2=ic-1
                 exit
              else if(zbuf(ic:ic).eq.'}') then
                 ilast=.TRUE.
                 ic2=ic-1
                 exit
              endif
           enddo

           zname = zbuf(ic1:ic2)
           call uupper(zname)

           call cstate_find1(zname,indx)

           indmap(indx)=ii
           if(ilast) exit
        enddo
     endif
  enddo

end subroutine cstate_gen_indmap
