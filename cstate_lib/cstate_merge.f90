subroutine cstate_merge_size(ndims,idims,ilodiffs,jsize)

  ! given list of dimensions produce size of merged list of members
  ! across all states being compared

  ! use the data from cstate_ldim_stats.f90

  use cstate_lib_mod
  implicit NONE

  integer, intent(in) :: ndims   ! # of dimensions (.ge.1)
  integer, intent(in) :: idims(ndims)  ! the dimension indices
  integer, intent(in) :: ilodiffs(ndims)  ! indexing modification at low end

  integer, intent(out) :: jsize

  !------------------------------------------------
  ! local:
  integer :: iscan(ndims),isizes(ndims),izero(nsss_cur)
  integer :: idim,i,ii,kdims,izsum,imrg
  !------------------------------------------------

  iscan(1)=0
  if(ndims.gt.1) iscan(2:ndims)=1

  !  iscan will span the potential N1 x N2 x ... X N[ndims] index space
  !  of the merged dimensions

  !  the space to be used is reduced, removing [i1,i2,...i[ndims]] points
  !  for which there is no corresponding data

  do i=1,ndims
     idim = idims(i)
     isizes(i) = mrgsizes(idim)  ! Nj
  enddo

  jsize = 0

  do
     !  find next index point in merged space
     kdims=1
     do
        ! scan order is like fortran indexing: 1st index varies most
        ! rapidly...

        iscan(kdims)=iscan(kdims)+1
        if(iscan(kdims).le.isizes(kdims)) exit

        iscan(kdims)=1
        kdims=kdims+1
        if(kdims.gt.ndims) exit
     enddo
     if(kdims.gt.ndims) exit

     !  find if any of the compared states provide data for this index point
     !  a non-zero state-specific index must exist along all dimensions for
     !  this to be the case

     izero(1:nsss_cur) = 1
     do i=1,ndims
        idim = idims(i)
        imrg = iscan(i)
        do ii=1,nsss_cur
           if(mrg_addr_map(ii,imrg,idim).le.ilodiffs(i)) izero(ii)=0
           if(ww(ii)%size.eq.0) izero(ii)=0
        enddo
     enddo

     izsum=sum(izero)

     if(izsum.gt.0) jsize = jsize + 1
  enddo

end subroutine cstate_merge_size


subroutine cstate_merge_addr(ndims,idims,ilodiffs,jsize,dname,dunits,ienum, &
     sum_table,sum_key,merge_names,totsize)

  ! given list of dimensions produce merged list of members
  ! across all states being compared; incorporate reduction sums as needed

  ! use the data from cstate_ldim_stats.f90

  use cstate_lib_mod
  implicit NONE

  integer, intent(in) :: ndims   ! # of dimensions (.ge.1)
  integer, intent(in) :: idims(ndims)  ! the dimension indices
  integer, intent(in) :: ilodiffs(ndims)  ! indexing modification at low end
  integer, intent(in) :: jsize   ! size of merged list
  !            ---> "jsize" from prior call to cstate_merge_size
  character*(*), intent(in) :: dname   ! base name of data
  character*(*), intent(in) :: dunits  ! data units
  !            ---> if "integer" or "character", suppress sums
  integer, intent(in) :: ienum         ! .gt.0: enumerated output
  !            ---> if enumerated output, suppress sums

  integer, intent(out) :: sum_table(0:jsize,nsss_cur,jsize)
  !   sum_table(0,ii,jj) = N = #of terms in sum for merged list element (jj)
  !                            from state (ii).  0=> absent data in state (ii)
  !
  !   sum_table(1:N,ii,jj) = elements from state (ii) to be summed to form
  !                            merged element (jj)
  !    

  integer, intent(out) :: sum_key(ndims,jsize)
  !   sum_key(i,jj) is summation number for dimension (idims(i)) for merged 
  !             element (jj)

  character*32, intent(out) :: merge_names(jsize)

  integer, intent(out) :: totsize
  !   final number of merged elements, totsize < jsize if there are reductions;
  !   if there are no reduction sums, totsize = jsize on output

  !  ONLY elements sum_table(:,1:nsss_cur,1:totsize) are populated!
  !  ONLY elements sum_key(1:ndims,1:totsize) are populated!

  !------------------------------------------------
  ! local:
  integer :: iscan(ndims),isizes(ndims),izero(nsss_cur)
  integer :: dsizes(0:ndims,nsss_cur),iadr(ndims)
  integer :: idim,i,ii,kdims,izsum,imrg,iord,ictrl

  logical :: iactive(nreduce_opt)  ! flag active reduction sum options
  integer :: loc_sumkey(ndims),locsum,iorig_addr,indx,imatch,iterm,isum
  character*32 :: zname1,zname2
  integer, dimension(:,:), allocatable :: nosum_key
  character*32, dimension(:), allocatable :: orig_names
  !------------------------------------------------

  allocate(nosum_key(ndims,jsize)); nosum_key=0
  allocate(orig_names(jsize))

  if((dunits.eq.'character').or.(dunits.eq.'integer')) then
     iactive = .FALSE.  ! no sums for non-floating point data

  else if(ienum.gt.0) then
     iactive = .FALSE.  ! no sums for enumerated displays

  else
     iactive = reduce_active
  endif

  iscan(1)=0
  if(ndims.gt.1) iscan(2:ndims)=1

  !  iscan will span the potential N1 x N2 x ... X N[ndims] index space
  !  of the merged dimensions

  !  the space to be used is reduced, removing [i1,i2,...i[ndims]] points
  !  for which there is no corresponding data

  !  the size (jsize) of the reduced space is already known by means of the 
  !  prior call to cstate_merge_size.

  !  Each individual state has its own subset index space; the sizes of
  !  linear address space corresponding to each index increment are stored
  !  in dsizes(...)

  dsizes(0,1:nsss_cur)=1
  do i=1,ndims
     idim = idims(i)
     isizes(i) = mrgsizes(idim)  ! Nj
     do ii=1,nsss_cur
        dsizes(i,ii)=dsizes(i-1,ii)*(dimsizes(ii,idim)-ilodiffs(i))
     enddo
  enddo

  totsize = 0
  iadr = 0

  do
     !  find next index point in merged space
     kdims=1
     do
        ! scan order is like fortran indexing: 1st index varies most
        ! rapidly...

        iscan(kdims)=iscan(kdims)+1
        if(iscan(kdims).le.isizes(kdims)) exit

        iscan(kdims)=1
        kdims=kdims+1
        if(kdims.gt.ndims) exit
     enddo
     if(kdims.gt.ndims) exit

     !  find if any of the compared states provide data for this index point
     !  a non-zero state-specific index must exist along all dimensions for
     !  this to be the case

     izero(1:nsss_cur) = 1
     do i=1,ndims
        idim = idims(i)
        imrg = iscan(i)
        do ii=1,nsss_cur
           if(mrg_addr_map(ii,imrg,idim).le.ilodiffs(i)) then
              izero(ii)=0
           else if(ww(ii)%size.eq.0) then
              izero(ii)=0
           else
              iord=mrg_addr_map(ii,imrg,idim)
              ictrl=sumctrl(iord,ii,idim)
              if(ictrl.gt.0) then
                 if(iactive(ictrl)) then
                    loc_sumkey(i)=sumindx(iord,ii,idim)  ! done repeatedly
                 else
                    loc_sumkey(i)=0  ! sum not active
                 endif
              else
                 loc_sumkey(i)=0  ! no sum
              endif
           endif
        enddo
     enddo

     izsum=sum(izero)

     if(izsum.eq.0) cycle

     imatch=0
     locsum = sum(loc_sumkey)  ! (0,0,0) -> zero -> no sum.

     if(locsum.gt.0) then
        do ii=1,totsize
           do i=1,ndims
              if(loc_sumkey(i).eq.sum_key(i,ii)) imatch = imatch + 1
              if(loc_sumkey(i).eq.0) then
                 if(iscan(i).ne.nosum_key(i,ii)) imatch = imatch - 1
              endif
           enddo
           if(imatch.eq.ndims) then
              indx=ii
              exit
           else
              imatch=0  ! & keep looking for match on all dimensions
           endif
        enddo
     endif

     if(imatch.eq.0) then
        totsize=totsize + 1
        sum_key(1:ndims,totsize) = loc_sumkey
        do i=1,ndims
           if(sum_key(i,totsize).eq.0) nosum_key(i,totsize)=iscan(i)
        enddo
        sum_table(0:jsize,1:nsss_cur,totsize) = 0
        indx = totsize
        merge_names(totsize)=' '
     endif

     !  OK there is data from at least one state

     do ii=1,nsss_cur
        iorig_addr = 0
        if(izero(ii).eq.1) then

           ! state (ii) has data for this index point

           do i=1,ndims
              idim=idims(i)
              imrg=iscan(i)
              iord=mrg_addr_map(ii,imrg,idim)
              iadr(i)=orig_order(iord,ii,idim)-ilodiffs(i)
           enddo

           ! compute linear address

           iorig_addr = 1
           do i=1,ndims
              iorig_addr = iorig_addr + dsizes(i-1,ii)*(iadr(i)-1)
           enddo
        endif
        if(iorig_addr.gt.0) then
           iterm=sum_table(0,ii,indx)+1
           sum_table(0,ii,indx)=iterm
           sum_table(iterm,ii,indx)=iorig_addr
        endif
        if((merge_names(totsize).eq.' ').and.(iorig_addr.gt.0)) then
           if(locsum.eq.0) then
              merge_names(totsize)=ww(ii)%list_elems(iorig_addr)
              orig_names(totsize)=ww(ii)%list_elems(iorig_addr)
           else
              orig_names(totsize)=ww(ii)%list_elems(iorig_addr)
              zname1=dname
              do i=1,ndims
                 idim=idims(i)
                 imrg=iscan(i)
                 iord=mrg_addr_map(ii,imrg,idim)
                 isum=sumindx(iord,ii,idim)
                 if(isum.eq.0) then
                    zname2=trim(zname1)//'_'//mrg_name(imrg,idim)
                 else
                    zname2=trim(zname1)//'_'//sumlbl(isum,idim)
                 endif
                 zname1=zname2
              enddo
              merge_names(totsize)=zname1
           endif
        else if(iorig_addr.gt.0) then
           if(orig_names(totsize).ne.ww(ii)%list_elems(iorig_addr)) then
              orig_names(totsize)=' '
           endif
        endif
     enddo

  enddo

  do ii=1,totsize
     if(orig_names(ii).ne.' ') merge_names(ii)=orig_names(ii)
  enddo

  deallocate(nosum_key,orig_names)
end subroutine cstate_merge_addr
