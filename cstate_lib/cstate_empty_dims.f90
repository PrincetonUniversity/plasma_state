subroutine cstate_empty_dims(luntty,ierr)

  ! find all empty dimensions (could be none...)
  ! MOD DMC: also set grid_dims

  use cstate_lib_mod
  implicit NONE

  integer, intent(in) :: luntty  ! LUN for messages

  !-------------------------
  ! local

  integer :: itype,ii,jj,isize,ilen,ierr,inum_empty,isize_dim,ici,iersum
  integer :: ing
  logical :: iok(nsss_cur)
  character*32 dim0,dim
  character*1 :: dtype(3) = (/ "G", "S", "L" /)
  !-------------------------

  write(luntty,*) ' '
  write(luntty,*) ' %cstate_empty_dims: dimensions empty in ALL states:'

  if(allocated(empty_dims)) deallocate(empty_dims)
  if(allocated(grid_dims)) deallocate(grid_dims)

  if(allocated(ww)) deallocate(ww)
  allocate(ww(nsss_cur))

  !  get dimension info on each state...

  inum_empty = 0
  nldims = 0
  iersum = 0

  do itype=1,3

     do ii=1,nsss_cur
        call psw_contents(dtype(itype),ps_cc_all,ww(ii),ierr, state=sss(ii))
        iok(ii) = (ierr.eq.0)
        if(.not.iok(ii)) iersum = iersum + 1
     enddo

     !  #dimensions the same in all states...
     isize = ww(1)%size
     if(itype.gt.1) then
        nldims = nldims + isize
     else
        ing=isize
     endif

     !  count the number that are empty in all of the states

     do jj=1,isize
        isize_dim = 0
        do ii=1,nsss_cur
           isize_dim = max(isize_dim,ww(ii)%ival(jj))
        enddo

        if(isize_dim.eq.0) then
           inum_empty = inum_empty + 1
        endif
     enddo
  enddo

  if(iersum.ne.0) then
     write(luntty,*) ' ?unexpected internal error: cstate_empty_dims.f90'
     ierr=1
     return
  endif

  !  allocate the list of empties...

  allocate(empty_dims(inum_empty))
  allocate(grid_dims(ing))

  !  fill the list of empties...

  inum_empty = 0
  ing = 0

  do itype = 1,3

     do ii=1,nsss_cur
        call psw_contents(dtype(itype),ps_cc_all,ww(ii),ierr, state=sss(ii))
        iok(ii) = (ierr.eq.0)
     enddo

     isize = ww(1)%size

     do jj=1,isize
        dim0=ww(1)%val_field(jj)
        ilen=len(trim(dim0))
        ici=1
        ici=max(ici,index(dim0,':'))
        dim = dim0(ici+1:ilen-1)   ! strip parens

        isize_dim = 0
        do ii=1,nsss_cur
           isize_dim = max(isize_dim,ww(ii)%ival(jj))
        enddo

        if(isize_dim.eq.0) then
           inum_empty = inum_empty + 1
           empty_dims(inum_empty) = dim
           call ulower(empty_dims(inum_empty))
           write(luntty,'(3x,i4,".  ",a)') inum_empty,empty_dims(inum_empty)
        endif

        if(itype.eq.1) then
           ing = ing + 1
           grid_dims(ing) = dim
           call ulower(grid_dims(ing))
        endif

     enddo
  enddo

  if(inum_empty.eq.0) write(luntty,*) '   NONE.'

  do ii=1,nsss_cur
     call free_window(ww(ii))
  enddo
  deallocate(ww)

end subroutine cstate_empty_dims
