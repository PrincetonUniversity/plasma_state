subroutine ur_dcomp_display(dtype, tlabel, ilabel)

  !  generate summary comparitive display of all grids or species lists or
  !  item lists

  use cstate_lib_mod
  implicit NONE

  !-------------------------
  ! passed:

  character*1, intent(in) :: dtype      ! "G" for grids;
  !                           "S" for species lists; "L" for item lists

  character*(*), intent(in) :: tlabel   ! label for table cells
  !                              e.g. "Grid_sizes"

  character*(*), intent(in) :: ilabel   ! short label for objects compared
  !                              e.g. "grids"

  !-------------------------
  ! local

  integer :: isize

  integer :: luntty,lnout
 
  integer :: ival_lens(nsss_cur)
  integer, dimension(:,:), allocatable :: imatch_sets,igsizes
  integer :: ierr,i,ii,j,jlim,jj,kk,ilen_gname,imatch_cur,insel
  integer :: inum,inums(nsss_cur),inmax,iself(nsss_cur)
  logical :: match

  character*32 iname,gname1,gname2,chdr,cvals(nsss_cur),sname1,sname2

  type (ps_list_window), dimension(:), allocatable :: wwloc

  !-------------------------
  luntty = lnout(0)

  isize = w1%size

  if((dtype.ne.'G').and.(dtype.ne.'S').and.(dtype.ne.'L')) then
     write(luntty,*) ' ?ur_dcomp_display: cstate_lib internal error: dtype=', &
          dtype
     return
  endif

  ! set up display objects
  ! all have the same members as the data objects
  ! because the list of grids or lists is an attribute of the state definition

  do i=1,nsss_cur
     call psw_contents(dtype,ncur_components,ww(i),ierr, state=sss(i))
     if(ierr.ne.0) exit

     call free_window(wdisp(i))
     wdisp(i)%size = isize
     wdisp(i)%list_name = tlabel
     allocate(wdisp(i)%list_elems(isize))
     wdisp(i)%list_elems = w1%list_elems
     allocate(wdisp(i)%val_field(isize)); wdisp(i)%val_field = ' '
  enddo
  if(ierr.ne.0) return

  imatch_cur = 0
  allocate(igsizes(nsss_cur,isize))
  allocate(imatch_sets(nsss_cur,isize))
  imatch_sets = 0

  ival_lens = 0
  ilen_gname = len(trim(wdisp(1)%list_name))

  !  loop over grids or lists
  do ii = 1,isize
     do i=1,nsss_cur
        igsizes(i,ii) = ww(i)%ival(ii)
     enddo
     do i=1,nsss_cur
        call iencode(igsizes(i,ii),wdisp(i)%val_field(ii),ival_lens(i))
     enddo
     ilen_gname=max(ilen_gname,len(trim(wdisp(1)%list_elems(ii))))
  enddo

  ilen_gname = ilen_gname + 1  ! extra space added

  !  look for and mark matching grids or lists
  do ii = 1,isize
     if(dtype.eq.'G') then
        ! special code for grids: grids of different names can be considered
        ! identical...

        do i=1,nsss_cur

           match = .FALSE.
           gname1 = ww(i)%list_elems(ii)
           do jj=1,ii
              if(jj.lt.ii) then
                 jlim=nsss_cur
              else
                 jlim=i-1
              endif

              do j = 1,jlim
                 gname2 = ww(j)%list_elems(jj)
                 call pscomp_grids(sss(i),sss(j),gname1,gname2,match,ierr)

                 if(match) then
                    if(imatch_sets(j,jj).gt.0) then
                       imatch_sets(i,ii) = imatch_sets(j,jj)
                       call augment(wdisp(i)%val_field(ii), &
                            imatch_sets(i,ii),ival_lens(i))
                    else
                       imatch_cur = imatch_cur + 1
                       imatch_sets(i,ii) = imatch_cur
                       call augment(wdisp(i)%val_field(ii), &
                            imatch_sets(i,ii),ival_lens(i))
                       imatch_sets(j,jj) = imatch_cur
                       call augment(wdisp(j)%val_field(jj), &
                            imatch_sets(j,jj),ival_lens(j))
                    endif
                    exit
                 endif
              enddo
              if(match) exit
           enddo

        enddo

     else if(dtype.eq.'S') then
        ! special code for species lists: lists of different names can 
        ! be considered identical...

        allocate(wwloc(2))

        do i=1,nsss_cur

           match = .FALSE.
           gname1 = ww(i)%list_elems(ii)
           do jj=1,ii
              if(jj.lt.ii) then
                 jlim=nsss_cur
              else
                 jlim=i-1
              endif

              do j=1,jlim
                 gname2 = ww(j)%list_elems(jj)

                 if((igsizes(i,ii).gt.0).and. &
                      (igsizes(i,ii).eq.igsizes(j,jj))) then

                    match = .TRUE.  ! size match only at present

                    call psw_slist(gname1,wwloc(1),ierr, state=sss(i))
                    call psw_slist(gname2,wwloc(2),ierr, state=sss(j))

                    do kk=1,igsizes(i,ii)
                       sname1=wwloc(1)%list_elems(kk)
                       sname2=wwloc(2)%list_elems(kk)
                       if(sname1.ne.sname2) then
                          match=.FALSE.
                          exit
                       endif
                    enddo
                 endif

                 if(match) then
                    if(imatch_sets(j,jj).gt.0) then
                       imatch_sets(i,ii) = imatch_sets(j,jj)
                       call augment(wdisp(i)%val_field(ii), &
                            imatch_sets(i,ii),ival_lens(i))
                    else
                       imatch_cur = imatch_cur + 1
                       imatch_sets(i,ii) = imatch_cur
                       call augment(wdisp(i)%val_field(ii), &
                            imatch_sets(i,ii),ival_lens(i))
                       imatch_sets(j,jj) = imatch_cur
                       call augment(wdisp(j)%val_field(jj), &
                            imatch_sets(j,jj),ival_lens(j))
                    endif
                    exit
                 endif
              enddo
              if(match) exit
           enddo

        enddo

        do i=1,2
           call free_window(wwloc(i))
        enddo

        deallocate(wwloc)

     else
        ! for non-grid, non-species-list dimensions, only lists of the 
        ! same names are compared between states

        allocate(wwloc(nsss_cur))

        iname = ww(1)%list_elems(ii)
        do i=1,nsss_cur
           call psw_ilist(iname,wwloc(i),ierr, state=sss(i))
        enddo

        do i=2,nsss_cur
           do j=1,i-1
              match = .FALSE.
              if(igsizes(i,ii).eq.igsizes(j,ii)) then
                 ! sizes match; compare element names
                 ! Note: empty lists not considered to match.

                 if(igsizes(i,ii).gt.0) match=.TRUE.

                 do jj=1,igsizes(i,ii)
                    gname1=wwloc(i)%list_elems(jj)
                    gname2=wwloc(j)%list_elems(jj)
                    if(gname1.ne.gname2) then
                       match=.FALSE.
                       exit
                    endif
                 enddo
              endif

              if(match) then
                 if(imatch_sets(j,ii).gt.0) then
                    imatch_sets(i,ii) = imatch_sets(j,ii)
                    call augment(wdisp(i)%val_field(ii), &
                         imatch_sets(i,ii),ival_lens(i))
                 else
                    imatch_cur = imatch_cur + 1
                    imatch_sets(i,ii) = imatch_cur
                    call augment(wdisp(i)%val_field(ii), &
                         imatch_sets(i,ii),ival_lens(i))
                    imatch_sets(j,ii) = imatch_cur
                    call augment(wdisp(j)%val_field(ii), &
                         imatch_sets(j,ii),ival_lens(j))
                 endif
              endif
              exit
           enddo
        enddo

        do i=1,nsss_cur
           call free_window(wwloc(i))
        enddo

        deallocate(wwloc)
     endif
  enddo

  do i=1,nsss_cur
     ival_lens(i)=max(ival_lens(i),len(trim(sss_label(i))))
  enddo

  write(luntty,*) ' '
  call disp1(ilen_gname,ival_lens, &
       wdisp(1)%list_name,sss_label(1:nsss_cur))

  chdr = rline(1:len(chdr))
  cvals = chdr
  call disp1(ilen_gname,ival_lens,chdr,cvals)

  do ii = 1,isize
     do jj=1,nsss_cur
        cvals(jj) = wdisp(jj)%val_field(ii)
     enddo
     call disp1(ilen_gname,ival_lens, &
          wdisp(1)%list_elems(ii),cvals)
  enddo

  deallocate(igsizes,imatch_sets)

  contains
    subroutine disp1(len1,lenn,col1,coln)
      !  display 1 line
      integer, intent(in) :: len1
      integer, intent(in), dimension(:) :: lenn
      character*(*) :: col1
      character*32, dimension(:) :: coln

      !---------------------
      integer :: ic,ii,isum
      !---------------------

      isum = len1 + sum(lenn) + size(lenn)
      if(isum.gt.len(cbuf)) then
         write(luntty,*) ' ?ur_dcomp_display:disp1: char buf overflow.'

      else
         cbuf=' '
         cbuf(1:len1) = col1(1:len1)
         ic = len1
         do ii=1,size(lenn)
            ic = ic + 1
            cbuf(ic+1:ic+lenn(ii)) = coln(ii)(1:lenn(ii))
            ic = ic + lenn(ii)
         enddo
         write(luntty,'(A)') trim(cbuf)
      endif

    end subroutine disp1

    subroutine iencode(ival,val_field,ilen_max)
      ! encode a non-negative integer value as character, left-justified
      ! record maximum length

      integer :: ival   ! integer value (in)
      character*(*) :: val_field  ! encoded string (out)
      integer :: ilen_max  ! max length (inout)

      !---------------------
      integer :: iuse,ilen
      !---------------------

      iuse = max(0,ival)
      val_field = ' '

      if(iuse.eq.0) then
         ilen=7
         val_field(1:7)='(empty)'
      else if(iuse.le.9) then
         ilen=1
         write(val_field(1:1),'(I1)') iuse
      else if(iuse.le.99) then
         ilen=2
         write(val_field(1:2),'(I2)') iuse
      else if(iuse.le.999) then
         ilen=3
         write(val_field(1:3),'(I3)') iuse
      else if(iuse.le.9999) then
         ilen=4
         write(val_field(1:4),'(I4)') iuse
      else if(iuse.le.99999) then
         ilen=5
         write(val_field(1:5),'(I5)') iuse
      else if(iuse.le.999999) then
         ilen=6
         write(val_field(1:6),'(I6)') iuse
      else if(iuse.le.9999999) then
         ilen=7
         write(val_field(1:7),'(I7)') iuse
      else
         ilen=7
         val_field='*******'
      endif

      ilen_max = max(ilen_max,ilen)
    end subroutine iencode

    subroutine augment(val_field,imatch_set,ilen_max)

      !  extend label to mark presence of matched set

      character*(*) val_field ! display value (inout)
      integer :: imatch_set   ! number of set of matching grids (in)
      integer :: ilen_max     ! max val_field non-blank length (out)

      !----------------------
      integer :: ic,ib,ilen
      character*4 zmatch
      !----------------------

      write(zmatch,'(i4)') imatch_set

      ilen = len(trim(val_field))

      ic = ilen + 2
      val_field(ic:ic)='['; ic = ic + 1
      do ib = 1,len(zmatch)
         if(zmatch(ib:ib).ne.' ') then
            val_field(ic:ic)=zmatch(ib:ib)
            ic = ic + 1
         endif
      enddo
      val_field(ic:ic)=']'

      ilen = ic
      ilen_max = max(ilen_max,ilen)

    end subroutine augment

end subroutine ur_dcomp_display
