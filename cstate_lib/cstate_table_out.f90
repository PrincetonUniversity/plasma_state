subroutine cstate_table_out(disp0,xlabel,cmt, &
     ilen_coln,incr,val_field,ivd2, &
     ienum,totsize)

  ! write ASCII table output

  ! output loop modified to support display of enumerations
  !     iper=max(1,ienum)
  !     (iper > 1 indicates an enumeration is present; iper=1 usually).

  use cstate_lib_mod
  implicit NONE

  character*(*), intent(in) :: disp0  ! header of column of data names
  character*(*), intent(in) :: xlabel ! field displayed above table if not ' '
  character*(*), intent(in) :: cmt    ! field displayed below table if not ' '

  integer, intent(in) :: ilen_coln(0:nsss_cur)  ! column widths
  integer, intent(in) :: incr  ! spacing btw columns

  integer :: ivd2      ! 2nd dimension of val_field
  character*(*) :: val_field(nsss_cur,ivd2)

  integer, intent(in) :: ienum   ! enumeration display flag
  integer, intent(in) :: totsize

  !-----------------------
  integer :: ia_coln(0:nsss_cur)    ! ASCII column buffer addresses
  integer :: lnout,luntty
  integer :: ii,jj,kk,ia,iper,ierr

  character*256 :: hdr,rln,buf

  !-----------------------

  luntty = lnout(0)

  ia_coln(0)=1
  do ii=1,nsss_cur
     if(ii.eq.1) then
        hdr = disp0
        rln = rline(1:ilen_coln(0))
     endif

     ia_coln(ii)=ia_coln(ii-1) + ilen_coln(ii-1) + incr

     ia=ia_coln(ii)
     hdr(ia:) = trim(sss_label(ii))
     rln(ia:) = rline(1:ilen_coln(ii))
  enddo

  write(luntty,*) ' '
  write(luntty,*) trim(xlabel)
  write(luntty,*) ' '
  write(luntty,*) trim(hdr)
  write(luntty,*) trim(rln)

  do jj=1,totsize
     do kk=1,iper
        if(kk.eq.1) then
           buf = wmerge%list_elems(jj)
        else
           buf = ' '
        endif

        do ii=1,nsss_cur
           ia=ia_coln(ii)
           buf(ia:) = val_field(ii,(jj-1)*iper + kk)
        enddo
        write(luntty,*) trim(buf)
     enddo
  enddo
  write(luntty,*) ' '
  if(cmt.ne.' ') then
     write(luntty,*) trim(cmt)
     write(luntty,*) ' '
  endif

end subroutine cstate_table_out
