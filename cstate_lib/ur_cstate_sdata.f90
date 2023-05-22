subroutine ur_cstate_sdata(subsect)

  ! examine non-profile data contained in the state(s)

  use cstate_lib_mod
  implicit NONE

  character*(*), intent(in) :: subsect

  !-------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar,lyes
  integer :: icontents,iimap,indx,ind2,isize,icount
  integer, dimension(:), allocatable :: indmap
  integer :: imin,imax,ilen_coln(0:nsss_cur),ilen0d
  integer :: isize_tot,ii,ierr,jsize,jj,kk,ia,ifloat,inorm
  integer :: totsize
  integer :: iterms,iii,jjj,ienum,imaxe,iper

  integer :: ndims,idims(20),ilodims(20),indx_enum

  character*9 zdate
  character*32 :: dname,dunits,pname
  character*70 :: disp0,xlabel,xunits,plabel,punits,cmt
  real*8 :: pnorm,psum,pwght
  logical :: avg_flag

  integer :: ivd2

  character*32, dimension(:,:), allocatable :: val_field
  integer, dimension(:,:), allocatable :: sum_key
  integer, dimension(:,:,:), allocatable :: sum_table
  character*32, dimension(:), allocatable :: merge_names

  !-------------------------
  ! for enumeration plot option:
  integer :: jself(nsss_cur),inumc(nsss_cur)
  integer :: isel,ivecsiz
  real*8 :: zdum(nsss_cur)
  character*32 fnlab(nsss_cur)
  real*8, dimension(:,:), allocatable :: xxplot,yyplot
  !-------------------------
  real*8, parameter :: ZERO=0.0d0
  !-------------------------

  luntty = lnout(0)

  kk=0

  icontents = 0
  iimap = 0

  do
     if(lxout(0)) then
        write(luntty,1001) omit_empty,trim(cur_components),nprec,ntail,nenum

1001    format('ur_cstate_sdata options:'/ &
             '  A -- option to omit empty elements from display (',L1,')'/ &
             '  C -- select & restrict view to component(s) (current selection: ',a,')'/ &
             '  T -- table of contents (show descriptions)'/ &
             '  U -- table of contents (show component, units & dimensions)'/ &
             '  R -- set list dimension reduction (summation) options'/ &
             '  S -- select a single data item & examine in detail'/ &
             '  P -- reset precision for display of floating point data (', &
             i2,')'/ &
             '  H -- adjust display subset for long character strings (', &
             i3,')'/ &
             '  L -- modify state label(s)'/ &
             '  M -- reset "soft" maximum enumeration data length (',i2,')'/ &
             '  Q -- quit this menu'/)
     endif
     call uread('ur_cstate_sdata option:')

     if(lchar('Q')) then
        exit

     else if(lchar('A')) then
        if(lxout(0)) write(luntty,*) &
             ' option to omit display of elements that are empty in all states.'

        call uread('omit empty elements ? (Y/N):')
        omit_empty = lyes(0)

     else if(lchar('C')) then
        icontents = 0
        iimap = 0
        call ur_cstate_component

     else if(lchar('L')) then
        call ur_cstate_setLabel

     else if(lchar('T')) then
        icontents = 1
        call ur_cstate_sdata_contents(2,subsect)

     else if(lchar('U')) then
        icontents = 1
        call ur_cstate_sdata_contents(1,subsect)

     else if(lchar('R')) then
        call ur_cstate_rcon

     else if(lchar('P')) then
        if(lxout(0)) then
           write(luntty,*) ' Floating point format: <sign>n.mm<esign>Exx'
           write(luntty,*) ' precision controls number of digits mm after the'
           write(luntty,*) ' decimal point; values btw 2 and 16 are accepted.'
        endif

        call ureadi('ur_cstate_sdata: floating point display precision:',nprec)
        nprec=max(2,min(16,nprec))

     else if(lchar('M')) then
        if(lxout(0)) then
           write(luntty,*) ' Provide a hint for maximum length of enumeration'
           write(luntty,*) ' data displays.  If the actual length is within'
           write(luntty,*) ' 50% of the limit, the actual length will be used.'
           write(luntty,*) ' Values between 6 and 50 are accepted.'
        endif

        call ureadi('ur_cstate_sdata: enumeration display lengh limit:',nenum)
        nenum=max(6,min(50,nenum))

     else if(lchar('H')) then
        if(lxout(0)) then
           write(luntty,*) ' In some table outputs, displays of character'
           write(luntty,*) ' strings of length greater than (32) are truncated.'
           write(luntty,*) ' The truncation variable controls how this is done:'
           write(luntty,*) ' 0 means display leading part of string;'
           write(luntty,*) ' -1 means display trailing part of string;'
           write(luntty,*) ' a positive integer value means, display starting'
           write(luntty,*) ' at the indicated character index value; this is'
           write(luntty,*) ' adjusted so that as much of the character string'
           write(luntty,*) ' as possible is displayed.  Truncation will be'
           write(luntty,*) ' indicated by ellipses "...".'
        endif

        call ureadi( &
             'ur_cstate_sdata: string truncation control (btw -1 and 999):', &
             ntail)
        ntail=max(-1,min(999,ntail))

     else if(lchar('S')) then
        if(icontents.eq.0) then
           icontents = 1
           call ur_cstate_sdata_contents(0,subsect)
        endif

        ! check contents...
        isize = w1%size
        if(isize.eq.0) then
           write(luntty,*) &
                ' %ur_cstate_sdatea: selected components contain no items.'
           call uclear
           cycle
        endif

        ! map some names to container group name...
        if(iimap.eq.0) then
           if(allocated(indmap)) deallocate(indmap)
           allocate(indmap(isize))
           call cstate_gen_indmap(indmap,isize)
           iimap=1
        endif

        ! get data item name
        if(lxout(0)) then
           write(luntty,*) '(unambiguous prefix of name may be entered).'
        endif
        call ureadw('ur_cstate_sdata: enter data item name or "." to skip:', &
             dname)
        if(dname.eq.'.') cycle

        call cstate_findm(dname,luntty,indx)
        if(indx.eq.0) then
           call uclear
           cycle
        endif

        ! map name to self or to container...
        ind2 = indmap(indx)

        dname = w1%list_elems(ind2)
        dunits = w1%elem_units(ind2)

        pname = ' '
        avg_flag=.FALSE.
        do inorm=1,nreduce_avg
           if(dname.eq.avg_var(inorm)) then
              pname=avg_wgt(inorm)
              call free_wwnorm
              allocate(wwnorm(nsss_cur))  ! weighted average possible
              avg_flag=.TRUE.
              exit
           endif
        enddo

        ! minimum, maximum display lengths
        if(dunits.eq.'character') then
           imin=6
           ifloat=0
        else if(dunits.eq.'integer') then
           imin=10
           ifloat=0
        else
           imin=nprec + 8  ! floating point...
           ifloat=1
        endif
        imax = 32

        isize_tot = 0
        icount = 0
        imaxe = 0   ! max enumeration size (if enumerations are encountered)

        ! set minimum display column widths...
        do ii=1,nsss_cur
           ilen_coln(ii)=max(imin,len(trim(sss_label(ii))))

           call psw_dlist(dname,ww(ii),xlabel,xunits,ierr, state=sss(ii))
           if(associated(ww(ii)%vecsizes)) then
              imaxe=max(1,imaxe)
              imaxe=max(imaxe,maxval(ww(ii)%vecsizes))
           endif

           if(ww(ii)%size.gt.0) then
              icount=icount+1
              if(pname.ne.' ') then
                 call psw_dlist(pname,wwnorm(ii),plabel,punits,ierr, &
                      state=sss(ii))
              endif
           endif

           if(icount.eq.1) then
              if(ifloat.eq.0) then
                 disp0 = trim(ww(ii)%list_name)
              else
                 disp0 = trim(ww(ii)%list_name)//' ('//trim(dunits)//')'
              endif
              ilen_coln(0)=len(trim(disp0))
           endif

           if(ww(ii)%size.eq.0) then
              write(luntty,*) ' empty list for state: '//trim(sss_label(ii))
           else
              isize_tot=isize_tot + ww(ii)%size
           endif
        enddo
        if(isize_tot.eq.0) then
           write(luntty,*) ' empty list.'
           cycle
        endif

        ienum=0
        if(imaxe.gt.0) then
           if(avg_flag) then
              write(luntty,*) ' ?ur_cstate_sdata: internal error, avg_flag=T'
              write(luntty,*) '  flag not applicable to enumeration data.'
              cycle
           endif
           ienum=imaxe
           if(ienum.gt.((3*nenum)/2)) ienum=nenum
        endif

        ! form merged list...

        cmt = ' '

        call cstate_ck_listdims(w1%c256val(ind2),size(idims),ndims, &
             idims,ilodims,indx_enum)
        if(ndims.eq.0) then

           ! scalar or scalar group; contents same for all states

           call clone_window(ww(1),wmerge)
           jsize = wmerge%size
           totsize = jsize
           if(imaxe.eq.0) then
              allocate(val_field(nsss_cur,jsize))
           else
              allocate(val_field(nsss_cur,ienum*jsize))
           endif
           val_field = ' '

           do jj=1,jsize
              ilen_coln(0)=max(ilen_coln(0),len(trim(ww(1)%list_elems(jj))))
              do ii=1,nsss_cur
                 if(imaxe.eq.0) then
                    call cstate_encode(dunits,ww(ii)%ival(jj),ww(ii)%fval(jj),&
                         ww(ii)%c256val(jj), val_field(ii,jj), ilen_coln(ii))
                 else
                    if(associated(ww(ii)%fvec)) then
                       call cstate_enumco( &
                            ww(ii)%vecsizes(jj),ww(ii)%fvec(:,jj), &
                            val_field,nsss_cur,ienum*jsize,ii,jj,ienum, &
                            ilen_coln(ii))
                    else
                       call cstate_inumco( &
                            ww(ii)%vecsizes(jj),ww(ii)%ivec(:,jj), &
                            val_field,nsss_cur,ienum*jsize,ii,jj,ienum, &
                            ilen_coln(ii))
                    endif
                 endif
              enddo
           enddo
        else

           ! separate dimensioning for each state

           call free_window(wmerge)
           call cstate_merge_size(ndims,idims(1:ndims),ilodims(1:ndims),jsize)
           allocate(sum_table(0:jsize,nsss_cur,jsize))
           allocate(sum_key(ndims,jsize))
           allocate(merge_names(jsize))

           call cstate_merge_addr( &
                ndims,idims(1:ndims),ilodims(1:ndims),jsize, &
                dname,dunits,imaxe, &
                sum_table,sum_key,merge_names,totsize)

           if(imaxe.eq.0) then
              allocate(val_field(nsss_cur,totsize))
           else
              allocate(val_field(nsss_cur,ienum*totsize))
           endif
           val_field = ' '

           wmerge%size = totsize
           allocate(wmerge%list_elems(totsize))
           wmerge%list_elems(1:totsize) = merge_names(1:totsize)

           do ii=1,nsss_cur
              do kk=1,totsize
                 if(sum(sum_key(1:ndims,kk)).eq.0) then
                    ! no summation
                    jj=sum_table(1,ii,kk)
                    if(jj.eq.0) then
                       if(imaxe.eq.0) then
                          val_field(ii,kk)=' (absent)'
                       else
                          val_field(ii,1+(kk-1)*ienum)=' (absent)'
                       endif
                    else
                       if(imaxe.eq.0) then
                          call cstate_encode(dunits, &
                               ww(ii)%ival(jj),ww(ii)%fval(jj), &
                               ww(ii)%c256val(jj), val_field(ii,kk), &
                               ilen_coln(ii))
                       else
                          if(associated(ww(ii)%fvec)) then
                             call cstate_enumco( &
                                  ww(ii)%vecsizes(jj),ww(ii)%fvec(:,jj), &
                                  val_field,nsss_cur,ienum*totsize, &
                                  ii,kk,ienum,ilen_coln(ii))
                          else
                             call cstate_inumco( &
                                  ww(ii)%vecsizes(jj),ww(ii)%ivec(:,jj), &
                                  val_field,nsss_cur,ienum*totsize, &
                                  ii,kk,ienum,ilen_coln(ii))
                          endif
                       endif

                       ilen_coln(0)=max(ilen_coln(0), &
                            len(trim(wmerge%list_elems(kk))))
                    endif
                 else
                    psum=ZERO
                    pnorm=1.0d0
                    if(avg_flag) then
                       cmt='   ...power weighted average(s).'
                       pnorm=ZERO
                    else
                       cmt='   ...summation(s) applied.'
                    endif
                    iterms=sum_table(0,ii,kk)

                    if(iterms.eq.0) then
                       val_field(ii,kk)=' (absent)'

                    else
                       do iii=1,iterms
                          jjj=sum_table(iii,ii,kk)
                          if(avg_flag) then
                             pwght=max(ZERO,wwnorm(ii)%fval(jjj))
                             pnorm=pnorm+pwght
                             psum=psum+pwght*ww(ii)%fval(jjj)
                          else
                             psum=psum+ww(ii)%fval(jjj)
                          endif
                       enddo

                       if(pnorm.gt.ZERO) psum = psum/pnorm

                       call cstate_encode(dunits, &
                            0,psum,' ', val_field(ii,kk), ilen_coln(ii))
                       ilen_coln(0)=max(ilen_coln(0), &
                            len(trim(wmerge%list_elems(kk))))
                    endif
                 endif
              enddo
           enddo

        endif

        !----------------------------------
        !  write the output table...

        ivd2 = size(val_field,2)

        call cstate_table_out(disp0,xlabel,cmt, &
             ilen_coln,1,val_field,ivd2, &
             ienum,totsize)

        !----------------------------------

        if(ienum.gt.0) then
           allocate(xxplot(imaxe,nsss_cur),yyplot(imaxe,nsss_cur))
           do kk=1,nsss_cur
              do jj=1,imaxe
                 xxplot(jj,kk)=jj
                 yyplot(jj,kk)=ZERO
              enddo
           enddo

           call c9date(zdate)

           kylab = ' '
           kylab(1)='State Label'

           mnlab(2)=' '

           do 
              if(lxout(0)) then
                 write(luntty,*) &
                      ' enter "Q" to quit (skip plot), or indicated number to select:'
                 do jj=1,totsize
                    write(luntty,'(3x,i3,".  ",a)') jj,wmerge%list_elems(jj)
                 enddo
              endif
              isel=0
              call ureadi( &
                   'ur_cstat_sdata plot option: enumeration # or "Q":',isel)
              if(isel.eq.0) exit

              if((isel.lt.0).or.(isel.gt.totsize)) then
                 write(luntty,*) ' %ur_cstate_sdata: enumeration selection not in range:'
                 write(luntty,*) '  min=1, max=',totsize,'  received: ',isel
                 call uclear
                 cycle
              endif

              kk=0
              do ii=1,nsss_cur
                 if(allocated(sum_table)) then
                    jj=sum_table(1,ii,isel)
                 else
                    jj=isel
                 endif
                 if(jj.ne.0) then
                    ivecsiz=ww(ii)%vecsizes(jj)
                    if(ivecsiz.gt.0) then
                       kk=kk+1
                       if(kk.eq.1) then
                          jself(kk)=-kk
                          mnlab(3)=trim(wmerge%list_elems(isel))// &
                               ' vs. array index'
                       else
                          jself(kk)=kk
                       endif
                       fnlab(kk)=trim(sss_label(ii))
                       inumc(kk)=ivecsiz
                       zdum(kk)=kk

                       if(associated(ww(ii)%fvec)) then
                          yyplot(1:ivecsiz,kk)=ww(ii)%fvec(1:ivecsiz,jj)
                       else
                          yyplot(1:ivecsiz,kk)=ww(ii)%ivec(1:ivecsiz,jj)
                       endif
                    endif
                 endif
              enddo

              if(kk.gt.0) then
                 call r8_grfmm1(xxplot,yyplot,imaxe,inumc,kk, &
                      zdum,kk,jself,kk, &
                      ' ',dunits,mnlab,kylab,fnlab)
              else
                 write(luntty,*) ' '
                 write(luntty,*) ' %ur_cstate_sdata: no data available.'
                 write(luntty,*) ' '
              endif

           enddo
           deallocate(xxplot,yyplot)
        endif

        deallocate(val_field)
        if(allocated(sum_table)) deallocate(sum_table)
        if(allocated(sum_key)) deallocate(sum_key)
        if(allocated(merge_names)) deallocate(merge_names)
        
        call free_wwnorm
     else
        call uermsg(' ?ur_cstate_sdata: unrecognized option.',' ',0)
     endif
  enddo

contains

  subroutine free_wwnorm
    if(allocated(wwnorm)) then
       do ii=1,nsss_cur
          call free_window(wwnorm(ii))
       enddo
       deallocate(wwnorm)
    endif
  end subroutine free_wwnorm

end subroutine ur_cstate_sdata
