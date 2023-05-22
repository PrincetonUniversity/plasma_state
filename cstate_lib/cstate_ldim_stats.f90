subroutine cstate_ldim_stats(luntty,ierr)

  ! get exhaustive statistics and data on "list dimensions" -- dimensions
  ! not representing physical coordinates: species lists, etc.

  ! **call cstate_empty_dims FIRST**
  !   cstate_empty_dims computs "nldims" which is used here.

  use cstate_lib_mod
  implicit NONE

  integer, intent(in) :: luntty  ! LUN for messages

  !-------------------------
  ! local
  integer :: itype,ii,jj,kk,iii,jjj,inum,idim,isize,ierr,iersum,isize_mrg,itot
  integer :: idum,isum,ilparen,irparen,imatch
  logical :: iok(nsss_cur)
  character*1 :: dtype(2) = (/ "S", "L" /)
  character*32 :: lname,mname,tmpname(1),str

  integer, dimension(:,:), allocatable :: addr_table

  integer :: ired(nldims)
  !-------------------------

  write(luntty,*) ' '
  write(luntty,*) ' %cstate_ldim_stats: starting...'

  !-----------------
  if(allocated(dimsizes)) deallocate(dimsizes,mrgsizes,list_dims,list_lolim)

  allocate(dimsizes(nsss_cur,nldims),mrgsizes(nldims))
  allocate(list_dims(nldims),list_lolim(nldims))
  dimsizes = 0
  mrgsizes = 0
  !-----------------

  maxnl=0
  maxml=0

  if(allocated(ww)) deallocate(ww)
  allocate(ww(nsss_cur))
  allocate(wdisp(nsss_cur))

  iersum = 0
  idim = 0

  ired = 0

  !-------------------------------------------------------------------------
  !  On first pass get basic information:  name & sizes of each list
  !  dimension before & after merge across states; maximum sizes as well.

  do itype = 1,2

     do ii=1,nsss_cur
        call psw_contents(dtype(itype),ps_cc_all,ww(ii),ierr, state=sss(ii))
        iok(ii) = (ierr.eq.0)
        if(.not.iok(ii)) iersum = iersum + 1
     enddo
     if(iersum.gt.0) exit

     !  #dimensions of type (itype): attribute of state definition...
     isize = ww(1)%size

     do jj=1,isize

        idim = idim + 1
        itot = 0

        ! extract bare dimension, e.g. "(0:nspec_th)" -> "nspec_th"

        call cstate_extract_dims(ww(1)%val_field(jj),1,idum, &
             tmpname,list_lolim(idim:idim))
        list_dims(idim) = tmpname(1)

        ! check for possibility of reduction operation
        do ii=1,nreduce_opt
           if(list_dims(idim).eq.reduce_dims(ii)) then
              ired(idim)=ii
              exit
           endif
        enddo

        do ii=1,nsss_cur
           dimsizes(ii,idim) = ww(ii)%ival(jj)
           itot = itot + dimsizes(ii,idim)
           maxnl = max(maxnl,dimsizes(ii,idim))
        enddo

        if(itot.gt.0) then
           lname = ww(1)%list_elems(jj)
           do ii=1,nsss_cur
              if(dtype(itype).eq.'S') then
                 call psw_slist(lname,wdisp(ii),ierr, state=sss(ii))
              else
                 call psw_ilist(lname,wdisp(ii),ierr, state=sss(ii))
              endif
              if(ierr.ne.0) then
                 iersum = iersum + 1
                 exit
              endif
           enddo
           if(iersum.gt.0) exit

           call psw_merge(wdisp,wmerge,ierr)
           if(ierr.gt.0) then
              iersum=iersum+1
              exit
           endif

           isize_mrg = wmerge%size
           mrgsizes(idim) = isize_mrg
           maxml = max(maxml,isize_mrg)

        endif
     enddo
     if(iersum.gt.0) exit
  enddo

  if(iersum.ne.0) then
     write(luntty,*) ' ?unexpected internal error: cstate_ldim_stats.f90'
     ierr=1
     return
  endif

  if(allocated(mrg_addr_map)) deallocate(mrg_addr_map,orig_order, &
       sumlbl,sumindx,sumctrl,sumterms,mrg_name)
  allocate(mrg_addr_map(nsss_cur,maxml,nldims)); mrg_addr_map = 0
  allocate(mrg_name(maxml,nldims)); mrg_name=' '
  allocate(orig_order(maxnl,nsss_cur,nldims)); orig_order = 0
  allocate(sumlbl(nmax_rsum,nldims)); sumlbl=' '
  allocate(sumterms(nmax_rsum,nldims)); sumterms = 0
  allocate(sumindx(maxnl,nsss_cur,nldims)); sumindx = 0
  allocate(sumctrl(maxnl,nsss_cur,nldims)); sumctrl = 0

  !-------------------------------------------------------------------------
  !  On 2nd pass compute address and order mappings
  !
  !  for k'th list dimension:
  !    mrg_addr_map(istate,jmerge,k) = 0
  !      means merged list element (jmerge) does not exist in state (istate)
  !                                  = j
  !      means merged list element (jmerge) exists in state (istate) at
  !      ordered position (j)
  !         --> original position:  L = orig_order(j,istate,k)
  !
  !  for a multiple dimension lookup in dimensions (k1,k2)-> (j1,j2)-> (L1,L2)
  !  item does not exist if either j1 or j2 is zero.  If item does exist, the
  !  linear address is L1 + (dimsizes(k1))*(L2-1)
  !
  !  for 3d lookup:  L1 + dimsizes(k1)*((L2-1) + dimsizes(k2)*(L3-1))
  !    ..etc..
  !
  !  this allows computation of merge of data objects in manner consistent
  !  with the merging of species and item lists.

  idim = 0

  do itype = 1,2

     ! only need 1st one this time... call is repeat so ierr check is skipped

     call psw_contents(dtype(itype),ps_cc_all,ww(1),ierr, state=sss(1))

     isize = ww(1)%size

     do jj=1,isize

        idim = idim + 1
        if(mrgsizes(idim).eq.0) cycle

        lname = ww(1)%list_elems(jj)
        do ii=1,nsss_cur
           if(dtype(itype).eq.'S') then
              call psw_slist(lname,wdisp(ii),ierr, state=sss(ii))
              call psw_slist(lname,wtmp,ierr, state=sss(ii), iorder=.FALSE.)
              inum=wtmp%size  ! = wdisp(ii)%size
              if((lname.eq.'ALL').or.(lname.eq.'ALLA')) then
                 call ired_chk('ALL')
                 sumlbl(1,idim)='IMP'
                 sumlbl(2,idim)='BEAM'
                 sumlbl(3,idim)='FUSN'
                 sumlbl(4,idim)='MINO'
                 do iii=1,inum
                    if(wdisp(ii)%spectype(iii).eq.ps_beam_ion) then
                       isum=2
                       sumindx(iii,ii,idim)=isum
                       sumctrl(iii,ii,idim)=ired(idim)
                       sumterms(isum,idim) = sumterms(isum,idim) + 1
                    else if(wdisp(ii)%spectype(iii).eq.ps_fusion_ion) then
                       isum=3
                       sumindx(iii,ii,idim)=isum
                       sumctrl(iii,ii,idim)=ired(idim)
                       sumterms(isum,idim) = sumterms(isum,idim) + 1
                    else if(wdisp(ii)%spectype(iii).eq.ps_rf_minority) then
                       isum=4
                       sumindx(iii,ii,idim)=isum
                       sumctrl(iii,ii,idim)=ired(idim)
                       sumterms(isum,idim) = sumterms(isum,idim) + 1
                    else
                       if(wdisp(ii)%spectype(iii).eq.ps_impurity) then
                          isum=1
                          sumindx(iii,ii,idim)=isum
                          sumctrl(iii,ii,idim)=ired(idim)
                          sumterms(isum,idim) = sumterms(isum,idim) + 1
                       endif
                    endif
                 enddo
              else if((lname.eq.'S').or.(lname.eq.'SA')) then
                 call ired_chk('S')
                 sumlbl(1,idim)='IMP'
                 do iii=1,inum
                    if(wdisp(ii)%spectype(iii).eq.ps_impurity) then
                       isum=1
                       sumindx(iii,ii,idim)=isum
                       sumctrl(iii,ii,idim)=ired(idim)
                       sumterms(isum,idim) = sumterms(isum,idim) + 1
                    endif
                 enddo
              else if(lname.eq.'SGAS') then
                 call ired_chk('SGAS')
                 sumlbl(1,idim)='H_iso'
                 sumlbl(2,idim)='He_iso'
                 do iii=1,inum
                    if(wdisp(ii)%qatom(iii).eq.1) then
                       isum=1
                       sumindx(iii,ii,idim)=isum
                       sumctrl(iii,ii,idim)=ired(idim)
                       sumterms(isum,idim) = sumterms(isum,idim) + 1
                    else if(wdisp(ii)%qatom(iii).eq.2) then
                       isum=2
                       sumindx(iii,ii,idim)=isum
                       sumctrl(iii,ii,idim)=ired(idim)
                       sumterms(isum,idim) = sumterms(isum,idim) + 1
                    endif
                 enddo
              else if(lname.eq.'SNBI') then
                 call ired_chk('SNBI')
                 sumlbl(1,idim)='BEAM'
                 do iii=1,inum
                    isum=1
                    sumindx(iii,ii,idim)=isum
                    sumctrl(iii,ii,idim)=ired(idim)
                    sumterms(isum,idim) = sumterms(isum,idim) + 1
                 enddo
              else if(lname.eq.'SFUS') then
                 call ired_chk('SFUS')
                 sumlbl(1,idim)='FUSN'
                 do iii=1,inum
                    isum=1
                    sumindx(iii,ii,idim)=isum
                    sumctrl(iii,ii,idim)=ired(idim)
                    sumterms(isum,idim) = sumterms(isum,idim) + 1
                 enddo
              else if(lname.eq.'RFMIN') then
                 call ired_chk('RFMIN')
                 sumlbl(1,idim)='MINO'
                 do iii=1,inum
                    isum=1
                    sumindx(iii,ii,idim)=isum
                    sumctrl(iii,ii,idim)=ired(idim)
                    sumterms(isum,idim) = sumterms(isum,idim) + 1
                 enddo
              endif
           else
              call psw_ilist(lname,wdisp(ii),ierr, state=sss(ii))
              call psw_ilist(lname,wtmp,ierr, state=sss(ii), iorder=.FALSE.)

              !  put short names in place...
              do iii=1,wtmp%size
                 wtmp%list_elems(iii)=trim(wtmp%c256val(iii))
                 wdisp(ii)%list_elems(iii)=trim(wdisp(ii)%c256val(iii))
              enddo

              inum=wtmp%size  ! = wdisp(ii)%size
              if(lname.eq.'ECRF_SRC_NAME') then
                 call ired_chk('ECRF_SRC_NAME')
                 sumlbl(1,idim)='ECTOT'
                 if(inum.gt.0) then
                    isum=1
                    sumindx(1:inum,ii,idim)=isum
                    sumctrl(1:inum,ii,idim)=ired(idim)
                    sumterms(isum,idim) = sumterms(isum,idim) + inum
                 endif
              else if(lname.eq.'LHRF_SRC_NAME') then
                 call ired_chk('LHRF_SRC_NAME')
                 sumlbl(1,idim)='LHTOT'
                 if(inum.gt.0) then
                    isum=1
                    sumindx(1:inum,ii,idim)=isum
                    sumctrl(1:inum,ii,idim)=ired(idim)
                    sumterms(isum,idim) = sumterms(isum,idim) + inum
                 endif
              else if(lname.eq.'ICRF_SRC_NAME') then
                 call ired_chk('ICRF_SRC_NAME')
                 sumlbl(1,idim)='ICTOT'
                 if(inum.gt.0) then
                    isum=1
                    sumindx(1:inum,ii,idim)=isum
                    sumctrl(1:inum,ii,idim)=ired(idim)
                    sumterms(isum,idim) = sumterms(isum,idim) + inum
                 endif
              else if(lname.eq.'NBI_SRC_NAME') then
                 call ired_chk('NBI_SRC_NAME')
                 sumlbl(1,idim)='BTOT'
                 if(inum.gt.0) then
                    isum=1
                    sumindx(1:inum,ii,idim)=isum
                    sumctrl(1:inum,ii,idim)=ired(idim)
                    sumterms(isum,idim) = sumterms(isum,idim) + inum
                 endif
              endif

           endif

           ! find orig_order

           do iii=1,inum
              mname=wdisp(ii)%list_elems(iii)
              do jjj=1,inum
                 if(mname.eq.wtmp%list_elems(jjj)) then
                    orig_order(iii,ii,idim) = jjj
                    exit
                 endif
              enddo
           enddo
        enddo

        ! find addr_table
        inum = mrgsizes(idim)
        allocate(addr_table(nsss_cur,inum))

        call psw_merge(wdisp,wmerge,ierr, addr_table=addr_table)

        mrg_addr_map(1:nsss_cur,1:inum,idim) = addr_table
        mrg_name(1:inum,idim)=wmerge%list_elems(1:inum)

        deallocate(addr_table)

        ! disallow reduction sums with only a single contributor
        ! (original labeling will be retained).

        do iii=1,nmax_rsum
           if(sumterms(iii,idim).eq.1) then
              do ii=1,nsss_cur
                 do jjj=1,maxnl
                    if(sumindx(jjj,ii,idim).eq.iii) then
                       sumindx(jjj,ii,idim)=0
                       sumctrl(jjj,ii,idim)=0
                    endif
                 enddo
              enddo
              sumterms(iii,idim)=0
           endif
        enddo

     enddo
  enddo

  !-------------------------------------------------------------------------
  !  get enumeration dimension data

  iersum = 0
  do ii=1,nsss_cur
     call psw_contents('E',ps_cc_all,ww(ii),ierr, state=sss(ii))
     if(ierr.ne.0) iersum = iersum + 1
  enddo

  !  #dimensions of type (itype): attribute of state definition...

  if(allocated(enum_dims)) &
       deallocate(enum_dims,enum_max,enum_decl,enum_ldim_ref)

  isize = ww(1)%size
  nedims= isize
  allocate(enum_dims(isize))
  allocate(enum_max(isize)); enum_max = 0
  allocate(enum_decl(isize))
  allocate(enum_ldim_ref(isize)); enum_ldim_ref = 0

  do jj=1,isize
     enum_dims(jj) = trim(ww(1)%list_elems(jj))
     call ulower(enum_dims(jj)) ! -> lowercase
     enum_decl(jj) = trim(ww(1)%val_field(jj))
     ilparen = index(enum_decl(jj),'(')
     irparen = max((ilparen+2),index(enum_decl(jj),')'))
     if(ilparen.gt.0) then
        str = enum_decl(jj)(ilparen+1:irparen-1)
        imatch = 0
        do kk=1,nldims
           if(str.eq.list_dims(kk)) then
              enum_ldim_ref(jj)=kk
              imatch = kk
              exit
           endif
        enddo
        if(imatch.eq.0) then
           iersum=iersum + 1
           write(luntty,*) '?cstate_ldim_stats: no list dimension found to '//&
                'match: ',trim(enum_decl(jj))
        endif
     endif
  enddo

  do ii=1,nsss_cur
     do jj=1,isize
        enum_max(jj)=max(enum_max(jj),ww(ii)%ival(jj))
     enddo
  enddo

  !-------------------------------------------------------------------------
  !  clean up...

  do ii=1,nsss_cur
     call free_window(ww(ii))
     call free_window(wdisp(ii))
  enddo
  deallocate(ww,wdisp)
     
  if(iersum.ne.0) then
     write(luntty,*) ' ?unexpected internal error: cstate_ldim_stats.f90'// &
          ' (enumeration scan).'
     ierr=1
  else
     write(luntty,*) ' %cstate_ldim_stats: completed.'
     ierr=0
  endif

  contains
    subroutine ired_chk(lisnam)

      character*(*), intent(in) :: lisnam
      if(ired(idim).eq.0) then
         write(luntty,*) ' ?cstate_ldim_stats: internal error.'
         write(luntty,*) '  inconsistency in code data on reducibility:'
         write(luntty,*) '    list name: ',trim(lisnam)
         write(luntty,*) '    dimension name: ',trim(list_dims(idim))
         write(luntty,*) '  code for list reduction requires dimension reducibility.'
         call ps_bad_exit
      endif
    end subroutine ired_chk

end subroutine cstate_ldim_stats
