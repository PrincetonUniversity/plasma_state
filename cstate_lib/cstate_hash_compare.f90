subroutine cstate_hash_compare(hlabel,hash1,hash2)

  !  display matched sets according to hash codes

  use cstate_lib_mod
  use plasma_hash_code
  implicit NONE

  !------------------------
  ! passed arguments
  !   if entities have 2 positive matching hash codes they are assumed
  !   to be identical

  character*(*), intent(in) :: hlabel    ! label of entity
  integer, intent(in) :: hash1(nsss_cur) ! 1st set of hash codes
  integer, intent(in) :: hash2(nsss_cur) ! 2nd set of hash codes
  !------------------------
  ! local

  integer :: ii,jj,maxmatch,imatch(nsss_cur),ihashm(nsss_cur)
  integer :: luntty,lnout
  character*40 :: describ
  !------------------------

  luntty = lnout(0)

  imatch = 0
  maxmatch = 0

  do ii=2,nsss_cur
     if(hash1(ii).lt.-1) cycle
     if(hash2(ii).lt.-1) cycle

     do jj=1,ii-1
        if((hash1(ii).eq.hash1(jj)).AND.(hash2(ii).eq.hash2(jj))) then
           if(imatch(jj).eq.0) then
              maxmatch = maxmatch + 1
              ihashm(maxmatch)=hash1(ii)
              imatch(jj)=maxmatch
              imatch(ii)=maxmatch
           else
              imatch(ii)=imatch(jj)
           endif
           exit
        endif
     enddo
  enddo

  do ii=1,maxmatch
     if(ihashm(ii).lt.0) then
        describ='identically ZERO'
     else
        describ='identical'
     endif
     if(maxmatch.gt.1) then
        write(luntty,'(/" {set ",i1,"} ",a,1x,a" in states:")') &
             ii,trim(hlabel),trim(describ)
     else
        write(luntty,'(/1x,a,1x,a" in states:")') trim(hlabel),trim(describ)
     endif
     do jj=1,nsss_cur
        if(imatch(jj).eq.ii) then
           write(luntty,*) '     '//trim(sss_label(jj))
        endif
     enddo
  enddo

end subroutine cstate_hash_compare
