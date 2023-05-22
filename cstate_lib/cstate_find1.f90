subroutine cstate_find1(dname,indx)

  !  find index matching "dname" in alphabetized list w1%list_elems(:)
  !  return as indx; return indx=0 if no match is found.

  use cstate_lib_mod
  implicit NONE

  !-------------------------
  !  passed:

  character*(*), intent(in) :: dname
  integer, intent(out) :: indx

  !-------------------------
  !  local:

  character*32 zname
  integer :: ii,i1,i2,isize
  !-------------------------

  zname = dname
  call uupper(zname)  ! force uppercase

  indx = 0

  isize=w1%size
  if(isize.eq.0) return

  if(zname.lt.w1%list_elems(1)) return
  if(zname.gt.w1%list_elems(isize)) return

  if(zname.eq.w1%list_elems(1)) then
     indx = 1
     return
  endif

  if(zname.eq.w1%list_elems(isize)) then
     indx = isize
     return
  endif

  i1=1
  i2=isize

  do
     ii = (i1+i2)/2
     if(ii.eq.i1) ii=i1+1

     if(zname.gt.w1%list_elems(ii)) then
        i1=ii

     else if(zname.eq.w1%list_elems(ii)) then
        indx=ii
        exit

     else
        i2=ii
        if(i2.eq.i1+1) exit  ! inbetwee i1 and i2; no match

     endif
  enddo

end subroutine cstate_find1

subroutine cstate_findm(dname,luntty,indx)

  !  find index matching "dname" in alphabetized list w1%list_elems(:)
  !  return as indx; return indx=0 if no match is found.

  !  "dname" can be a prefix of a name; if its length is N and it matches
  !  character positions (1:N) of one name only, that counts as a match.

  use cstate_lib_mod
  implicit NONE

  !-------------------------
  !  passed:

  character*(*), intent(inout) :: dname   ! name or prefix item
  !  full name, in uppercase, returned (if match is found).

  integer, intent(in) :: luntty        ! LUN (for warning message)

  integer, intent(out) :: indx         ! match index, or 0 if none.

  !-------------------------
  !  local:

  character*32 zname
  integer :: ii,i1,i2,isize,inmatch,inb
  !-------------------------

  zname = dname
  call uupper(zname)     ! force uppercase
  inb = len(trim(zname)) ! non-blank length

  indx = 0
  inmatch = 0

  isize=w1%size
  do
     if(isize.eq.0) exit

     i1=1
     i2=isize

     do ii=i1,i2
        if(zname.eq.w1%list_elems(ii)) then
           inmatch = 1
           indx = ii
           exit
        endif
        if(zname(1:inb).eq.w1%list_elems(ii)(1:inb)) then
           inmatch = inmatch + 1
           indx = ii
           write(luntty,*) '   ...match: '//trim(w1%list_elems(indx))
        endif
     enddo

     exit
  enddo

  if(inmatch.eq.0) then
     write(luntty,*) ' %cstate_findm: "'//trim(zname)//'" does not match any available name.'
     write(luntty,*) ' '
     indx=0
  else if(inmatch.gt.1) then
     write(luntty,*) ' %cstate_findm: "'//trim(zname)//'" ambiguous: matches more than one name.'
     write(luntty,*) ' '
     indx=0
  else
     dname = w1%list_elems(indx)
  endif

end subroutine cstate_findm
