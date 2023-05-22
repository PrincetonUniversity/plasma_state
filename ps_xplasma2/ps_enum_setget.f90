subroutine ps_enum_1d_store(iobj,enum_name,iauthor, &
     isize,enum_1d, imod, iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 9 Jun 2008

  ! XPLASMA2 implementation of plasma_state subroutine
  ! store a 1d enumeration of floating point values

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)       ! xplasma opaque container

  character*(*), intent(in) :: enum_name   ! NAME of list to store

  integer, intent(in) :: iauthor           ! author ID

  integer, intent(in) :: isize             ! size of enumeration (1d array)
  REAL(kind=rspec), intent(in) :: enum_1d(isize)  ! the data

  integer, intent(out) :: imod             ! =1 if data is new or modified
  !  =0 on output if data was not changed.

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: id,ii,iertmp,idhdr(2)

  type (interp_object) :: xobj
  type (xplasma), pointer :: s

  real*8, dimension(:), pointer :: r8data
  integer, dimension(:), pointer :: idata

  integer, parameter :: bbtype = 177       ! arbitrary blackbox "type" id

  integer, parameter :: maxsize = 5000
  !--------------------------------------------------

  imod = 0
  nullify(r8data)
  nullify(idata)

  ierr=0
  if(isize.lt.0) then
     ierr=999
     write(iout,*) ' ?ps_enum_1d_store: negative enumeration size: ',isize
  else if(isize.gt.maxsize) then
     ierr=999
     write(iout,*) ' ?ps_enum_1d_store: enumeration size: ',isize, &
          ' exceeds limit: ',maxsize
  endif
  if(ierr.ne.0) return
     
  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_enum_1d_store: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  set author

  call xplasma_author_set(s,ps_cnames(iauthor),ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_enum_1d_store: unexpected xplasma_author_set error:'
     call xplasma_error(s,ierr,iout)
     ierr=101
     return
  endif

  !--------------------------------------------------
  !  look to see if enumeration has already been stored

  call xplasma_blkbxId(s,enum_name,id)

  if(id.ne.0) then
     call xplasma_blackBox_retrieve(s,id,ierr, ia_ptr=idata, r8a_ptr=r8data)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_enum_1d_store: unexpected XPLASMA2 error:'
        call xplasma_error(s,ierr,iout)
        ierr=101
        go to 999
     endif

     if(size(idata).ne.2) then
        imod=1   ! integer descriptor size does not match
     else
        if((idata(1).ne.0).or.(idata(2).ne.isize)) then
           imod=1  ! enumeration rank and/or size do not match
        else
           do ii=1,isize
              if(enum_1d(ii).ne.r8data(ii)) then
                 imod=1
                 exit        ! change in data value
              endif
           enddo
        endif
     endif
     nullify(idata)
     nullify(r8data)

  else
     imod=1    ! storing for first time...
  endif

  if(imod.eq.1) then

     idhdr(1) = 0             ! indicates 1d enumeration array
     idhdr(2) = isize

     call xplasma_create_blackBox(s,enum_name,bbtype,id,ierr, &
          iarray=idhdr, r8array=enum_1d)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_enum_1d_store: unexpected XPLASMA2 error:'
        call xplasma_error(s,ierr,iout)
        ierr=101
        go to 999
     endif
  endif
  !--------------------------------------------------
  ! clear author

999 continue

  call xplasma_author_clear(s,ps_cnames(iauthor),iertmp)
  if(iertmp.ne.0) then
     write(iout,*) ' ?ps_enum_1d_store: unexpected xplasma_author_clear error:'
     call xplasma_error(s,ierr,iout)
     ierr=101
  endif

end subroutine ps_enum_1d_store

subroutine ps_enum_2d_store(iobj,enum_name,iauthor, &
     inume,isize_vec,idim1,enum_2d, imod, iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 9 Jun 2008

  ! XPLASMA2 implementation of plasma_state subroutine
  ! store a list of 1d enumerations of floating point values
  ! rank of resulting array is 2d

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)       ! xplasma opaque container

  character*(*), intent(in) :: enum_name   ! NAME of list to store

  integer, intent(in) :: iauthor           ! author ID

  integer, intent(in) :: inume             ! number of enumerations
  integer, intent(in) :: isize_vec(inume)  ! size of each enumeration (0 OK)
  integer, intent(in) :: idim1             ! 1st dim of data array

  REAL(kind=rspec), intent(in) :: enum_2d(idim1,inume)  ! the data

  integer, intent(out) :: imod             ! =1 if data is new or modified
  !  =0 on output if data was not changed.

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: id,ii,ie,ia,iertmp,idhdr(inume+1),isize_tot

  type (interp_object) :: xobj
  type (xplasma), pointer :: s

  real*8, dimension(:), pointer :: r8data
  integer, dimension(:), pointer :: idata

  integer, parameter :: bbtype = 177       ! arbitrary blackbox "type" id

  integer, parameter :: maxsize = 5000
  !--------------------------------------------------

  imod = 0
  nullify(r8data)
  nullify(idata)

  ierr=0
  if(minval(isize_vec).lt.0) then
     ierr=999
     write(iout,*) ' ?ps_enum_1d_store: negative enumeration size: ',isize_vec
  else if(maxval(isize_vec).gt.maxsize) then
     ierr=999
     write(iout,*) ' ?ps_enum_1d_store: enumeration size: ',isize_vec, &
          ' exceeds limit: ',maxsize
  else if(maxval(isize_vec).gt.idim1) then
     ierr=999
     write(iout,*) ' ?ps_enum_1d_store: enumeration size: ',isize_vec, &
          ' exceeds array dimension: ',idim1
  endif
  if(ierr.ne.0) return

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_enum_2d_store: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  set author

  call xplasma_author_set(s,ps_cnames(iauthor),ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_enum_2d_store: unexpected xplasma_author_set error:'
     call xplasma_error(s,ierr,iout)
     ierr=101
     return
  endif

  !--------------------------------------------------
  !  look to see if enumeration has already been stored

  idhdr(1)=inume
  idhdr(2:inume+1) = isize_vec
  isize_tot = sum(isize_vec)

  call xplasma_blkbxId(s,enum_name,id)

  if(id.ne.0) then
     call xplasma_blackBox_retrieve(s,id,ierr, ia_ptr=idata, r8a_ptr=r8data)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_enum_2d_store: unexpected XPLASMA2 error:'
        call xplasma_error(s,ierr,iout)
        ierr=101
        go to 999
     endif

     if(size(idata).ne.(inume+1)) then
        imod=1   ! integer header size does not match
     else
        do ii=1,inume+1
           if(idata(ii).ne.idhdr(ii)) then
              imod=1
              exit   ! headers do not match
           endif
        enddo
        if(imod.eq.0) then
           ! check the data
           ia=0
           do ie=1,inume
              do ii=1,isize_vec(ie)
                 ia=ia+1
                 if(r8data(ia).ne.enum_2d(ii,ie)) then
                    imod=1
                    exit     ! change in data value
                 endif
              enddo
              if(imod.eq.1) exit
           enddo
        endif
     endif
     nullify(idata)
     nullify(r8data)

  else
     imod=1    ! storing for first time...
  endif

  if(imod.eq.1) then

     allocate(r8data(max(1,isize_tot))); r8data(1)=0.0_rspec

     ia=0
     do ie=1,inume
        do ii=1,isize_vec(ie)
           ia=ia+1
           r8data(ia) = enum_2d(ii,ie)
        enddo
     enddo

     call xplasma_create_blackBox(s,enum_name,bbtype,id,ierr, &
          iarray=idhdr, r8array=r8data)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_enum_2d_store: unexpected XPLASMA2 error:'
        call xplasma_error(s,ierr,iout)
        ierr=101
        go to 999
     endif
  endif
  !--------------------------------------------------
  ! clear author

999 continue
  if(associated(r8data)) deallocate(r8data)

  call xplasma_author_clear(s,ps_cnames(iauthor),iertmp)
  if(iertmp.ne.0) then
     write(iout,*) ' ?ps_enum_2d_store: unexpected xplasma_author_clear error:'
     call xplasma_error(s,ierr,iout)
     ierr=101
  endif

end subroutine ps_enum_2d_store

subroutine ps_inum_1d_store(iobj,inum_name,iauthor, &
     isize,inum_1d, imod, iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 9 Jun 2008

  ! XPLASMA2 implementation of plasma_state subroutine
  ! store a 1d enumeration of INTEGER values

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)       ! xplasma opaque container

  character*(*), intent(in) :: inum_name   ! NAME of list to store

  integer, intent(in) :: iauthor           ! author ID

  integer, intent(in) :: isize             ! size of enumeration (1d array)
  integer, intent(in) :: inum_1d(isize)    ! the data

  integer, intent(out) :: imod             ! =1 if data is new or modified
  !  =0 on output if data was not changed.

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: id,ii,iertmp,idhdr(2)

  type (interp_object) :: xobj
  type (xplasma), pointer :: s

  real*8, dimension(:), pointer :: r8data
  integer, dimension(:), pointer :: idata

  real*8 :: r8dum(1)

  integer, parameter :: bbtype = 178       ! arbitrary blackbox "type" id

  integer, parameter :: maxsize = 5000
  !--------------------------------------------------

  imod = 0
  nullify(r8data)
  nullify(idata)

  ierr=0
  if(isize.lt.0) then
     ierr=999
     write(iout,*) ' ?ps_inum_1d_store: negative enumeration size: ',isize
  else if(isize.gt.maxsize) then
     ierr=999
     write(iout,*) ' ?ps_inum_1d_store: enumeration size: ',isize, &
          ' exceeds limit: ',maxsize
  endif
  if(ierr.ne.0) return
     
  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_inum_1d_store: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  set author

  call xplasma_author_set(s,ps_cnames(iauthor),ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_inum_1d_store: unexpected xplasma_author_set error:'
     call xplasma_error(s,ierr,iout)
     ierr=101
     return
  endif

  !--------------------------------------------------
  !  look to see if enumeration has already been stored

  call xplasma_blkbxId(s,inum_name,id)

  if(id.ne.0) then
     call xplasma_blackBox_retrieve(s,id,ierr, ia_ptr=idata, r8a_ptr=r8data)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_inum_1d_store: unexpected XPLASMA2 error:'
        call xplasma_error(s,ierr,iout)
        ierr=101
        go to 999
     endif

     if(size(idata).ne.2+isize) then
        imod=1   ! integer descriptor size does not match
     else
        if((idata(1).ne.0).or.(idata(2).ne.isize)) then
           imod=1  ! enumeration rank and/or size do not match
        else
           do ii=1,isize
              if(inum_1d(ii).ne.idata(ii+2)) then
                 imod=1
                 exit        ! change in data value
              endif
           enddo
        endif
     endif
     nullify(idata)
     nullify(r8data)

  else
     imod=1    ! storing for first time...
  endif

  if(imod.eq.1) then

     r8dum = 0.0_rspec

     allocate(idata(isize+2))

     idata(1) = 0             ! indicates 1d enumeration array
     idata(2) = isize
     idata(3:isize+2) = inum_1d(1:isize)

     call xplasma_create_blackBox(s,inum_name,bbtype,id,ierr, &
          iarray=idata, r8array=r8dum)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_inum_1d_store: unexpected XPLASMA2 error:'
        call xplasma_error(s,ierr,iout)
        ierr=101
        go to 999
     endif
  endif
  !--------------------------------------------------
  ! clear author

999 continue
  if(associated(idata)) deallocate(idata)

  call xplasma_author_clear(s,ps_cnames(iauthor),iertmp)
  if(iertmp.ne.0) then
     write(iout,*) ' ?ps_inum_1d_store: unexpected xplasma_author_clear error:'
     call xplasma_error(s,ierr,iout)
     ierr=101
  endif

end subroutine ps_inum_1d_store

subroutine ps_inum_2d_store(iobj,inum_name,iauthor, &
     inume,isize_vec,idim1,inum_2d, imod, iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 9 Jun 2008

  ! XPLASMA2 implementation of plasma_state subroutine
  ! store a list of 1d enumerations of floating point values
  ! rank of resulting array is 2d

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)       ! xplasma opaque container

  character*(*), intent(in) :: inum_name   ! NAME of list to store

  integer, intent(in) :: iauthor           ! author ID

  integer, intent(in) :: inume             ! number of enumerations
  integer, intent(in) :: isize_vec(inume)  ! size of each enumeration (0 OK)
  integer, intent(in) :: idim1             ! 1st dim of data array

  integer, intent(in) :: inum_2d(idim1,inume)  ! the data

  integer, intent(out) :: imod             ! =1 if data is new or modified
  !  =0 on output if data was not changed.

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: id,ii,ie,ia,iertmp,isize_tot

  type (interp_object) :: xobj
  type (xplasma), pointer :: s

  real*8, dimension(:), pointer :: r8data
  integer, dimension(:), pointer :: idata

  real*8 :: r8dum(1)

  integer, parameter :: bbtype = 178       ! arbitrary blackbox "type" id

  integer, parameter :: maxsize = 5000
  !--------------------------------------------------

  imod = 0
  nullify(r8data)
  nullify(idata)

  ierr=0
  if(minval(isize_vec).lt.0) then
     ierr=999
     write(iout,*) ' ?ps_inum_2d_store: negative enumeration size: ',isize_vec
  else if(maxval(isize_vec).gt.maxsize) then
     ierr=999
     write(iout,*) ' ?ps_inum_2d_store: enumeration size: ',isize_vec, &
          ' exceeds limit: ',maxsize
  else if(maxval(isize_vec).gt.idim1) then
     ierr=999
     write(iout,*) ' ?ps_inum_2d_store: enumeration size: ',isize_vec, &
          ' exceeds array dimension: ',idim1
  endif
  if(ierr.ne.0) return

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_inum_2d_store: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  set author

  call xplasma_author_set(s,ps_cnames(iauthor),ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_inum_2d_store: unexpected xplasma_author_set error:'
     call xplasma_error(s,ierr,iout)
     ierr=101
     return
  endif

  !--------------------------------------------------
  !  look to see if enumeration has already been stored

  isize_tot = sum(isize_vec)

  call xplasma_blkbxId(s,inum_name,id)

  if(id.ne.0) then
     call xplasma_blackBox_retrieve(s,id,ierr, ia_ptr=idata, r8a_ptr=r8data)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_inum_2d_store: unexpected XPLASMA2 error:'
        call xplasma_error(s,ierr,iout)
        ierr=101
        go to 999
     endif

     if(size(idata).ne.(inume+1+isize_tot)) then
        imod=1   ! integer header size does not match
     else
        if(idata(1).ne.inume) then
           imod=1
        endif
        if(imod.eq.0) then
           do ii=1,inume
              if(idata(ii+1).ne.isize_vec(ii)) then
                 imod=1
                 exit   ! headers do not match
              endif
           enddo
        endif
        if(imod.eq.0) then
           ! check the data
           ia=inume+1
           do ie=1,inume
              do ii=1,isize_vec(ie)
                 ia=ia+1
                 if(idata(ia).ne.inum_2d(ii,ie)) then
                    imod=1
                    exit     ! change in data value
                 endif
              enddo
              if(imod.eq.1) exit
           enddo
        endif
     endif
     nullify(idata)
     nullify(r8data)

  else
     imod=1    ! storing for first time...
  endif

  if(imod.eq.1) then

     r8dum=0.0_rspec
     allocate(idata(1+inume+isize_tot))

     idata(1)=inume
     idata(2:inume+1)=isize_vec

     ia=inume+1
     do ie=1,inume
        do ii=1,isize_vec(ie)
           ia=ia+1
           idata(ia) = inum_2d(ii,ie)
        enddo
     enddo

     call xplasma_create_blackBox(s,inum_name,bbtype,id,ierr, &
          iarray=idata, r8array=r8dum)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_inum_2d_store: unexpected XPLASMA2 error:'
        call xplasma_error(s,ierr,iout)
        ierr=101
        go to 999
     endif
  endif
  !--------------------------------------------------
  ! clear author

999 continue
  if(associated(idata)) deallocate(idata)

  call xplasma_author_clear(s,ps_cnames(iauthor),iertmp)
  if(iertmp.ne.0) then
     write(iout,*) ' ?ps_inum_2d_store: unexpected xplasma_author_clear error:'
     call xplasma_error(s,ierr,iout)
     ierr=101
  endif

end subroutine ps_inum_2d_store

subroutine ps_enum_1d_get(iobj,enum_name,iauthor, &
     isize_dim, isize,enum_1d, iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 9 Jun 2008

  ! XPLASMA2 implementation of plasma_state subroutine
  ! retrieve a 1d enumeration of floating point values

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)       ! xplasma opaque container

  character*(*), intent(in) :: enum_name   ! NAME of list to store

  integer, intent(in) :: iauthor           ! author ID

  integer, intent(in) :: isize_dim         ! max size of enumeration (1d array)
  integer, intent(out) :: isize            ! actual size
  REAL(kind=rspec), intent(out) :: enum_1d(isize_dim)  ! the data

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: id,ii,iertmp,idhdr(2),iasize,r8asize

  character*32 pauth

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  ierr=0

  isize=0
  enum_1d=0.0_rspec

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_enum_1d_get: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  find list ID

  call xplasma_blkbxId(s,enum_name,id)
  if(id.eq.0) then
     write(iout,*) ' ?ps_enum_1d_get: enumeration data not found: '// &
          trim(enum_name)
     ierr=801
     return
  endif

  !--------------------------------------------------
  !  fetch and check info

  call xplasma_blackbox_info(s,id,ierr, isize=iasize, r8size=r8asize, &
       author=pauth)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_enum_1d_get: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=801
  else
     if(iauthor.gt.0) then
        if(trim(ps_cnames(iauthor)).ne.trim(pauth)) then
           write(iout,*) ' ?ps_enum_1d_get: author label mismatch:'
           write(iout,*) '  data ',trim(enum_name),' has: "',trim(pauth), &
                '"; passed author reference: "',trim(ps_cnames(iauthor)),'".'
           ierr=801
        endif
     endif
  endif

  if(ierr.ne.0) return

  if((iasize.ne.2).or.(r8asize.gt.isize_dim)) then 
     write(iout,*) ' ?ps_enum_1d_get: "'//trim(enum_name)//'" data object sizes not consistent:'
     write(iout,*) '  expected integer size=2, got: ',iasize
     write(iout,*) '  expected floating pt size <= ',isize_dim,', got:',r8asize
     ierr=801
     return
  endif

  !--------------------------------------------------
  ! OK fetch the data

  call xplasma_blackbox_retrieve(s,id,ierr, &
       iarray=idhdr,r8array=enum_1d(1:r8asize))
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_enum_1d_get: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=801
     return
  endif

  if((idhdr(1).ne.0).and.(idhdr(1).ne.1)) then
     ierr=801
     write(iout,*) ' ?ps_enum_1d_get: 1st integer header value: ',idhdr(1), &
          '; should be zero or one.'
  endif

  if(idhdr(2).ne.r8asize) then
     ierr=801
     write(iout,*) ' ?ps_enum_1d_get: 2nd integer header value: ',idhdr(2), &
          ' does not match enum_1d size: ',r8asize
  else
     isize=idhdr(2)
  endif

end subroutine ps_enum_1d_get

subroutine ps_enum_2d_get(iobj,enum_name,iauthor, &
     inume,isize_vec, idim1,enum_2d, iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 9 Jun 2008

  ! XPLASMA2 implementation of plasma_state subroutine
  ! retrieve a 2d enumeration of floating point values
  !    isize_vec(1:inume)
  !    for j=1:inume
  !       enum_2d(1:isize_vec(j),j)

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)       ! xplasma opaque container

  character*(*), intent(in) :: enum_name   ! NAME of list to store

  integer, intent(in) :: iauthor           ! author ID

  integer, intent(in) :: inume             ! number of enumerations expected
  ! (data object must match this)

  integer, intent(out) :: isize_vec(inume) ! sizes of enumerations
  integer, intent(in) :: idim1             ! enum_2d array dimension

  REAL(kind=rspec), intent(out) :: enum_2d(idim1,inume)  ! the data

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: id,ii,ie,ia,iertmp,iasize,r8asize,isize_max

  integer, dimension(:), pointer :: idata
  real(kind=rspec), dimension(:), pointer :: r8data

  character*32 pauth

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  ierr=0

  isize_vec=0
  enum_2d=0.0_rspec

  nullify(r8data)
  nullify(idata)

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_enum_2d_get: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  find list ID

  call xplasma_blkbxId(s,enum_name,id)
  if(id.eq.0) then
     write(iout,*) ' ?ps_enum_2d_get: enumeration data not found: '// &
          trim(enum_name)
     ierr=801
     return
  endif

  !--------------------------------------------------
  !  fetch and check info

  call xplasma_blackbox_info(s,id,ierr, isize=iasize, r8size=r8asize, &
       author=pauth)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_enum_2d_get: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=801
  else
     if(iauthor.gt.0) then
        if(trim(ps_cnames(iauthor)).ne.trim(pauth)) then
           write(iout,*) ' ?ps_enum_2d_get: author label mismatch:'
           write(iout,*) '  data ',trim(enum_name),' has: "',trim(pauth), &
                '"; passed author reference: "',trim(ps_cnames(iauthor)),'".'
           ierr=801
        endif
     endif
  endif

  if(ierr.ne.0) return

  if((iasize.ne.1+inume)) then 
     write(iout,*) ' ?ps_enum_2d_get: "'//trim(enum_name)//'" data object sizes not consistent:'
     write(iout,*) '  expected integer size=',inume+1,', got: ',iasize
     ierr=801
     return
  endif

  !--------------------------------------------------
  ! OK fetch the data

  call xplasma_blackbox_retrieve(s,id,ierr, &
       ia_ptr=idata,r8a_ptr=r8data)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_enum_2d_get: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=801
     return
  endif

  if(idata(1).ne.inume) then
     ierr=801
     write(iout,*) ' ?ps_enum_2d_get: 1st integer header value: ',idata(1), &
          '; should match #enumerations:',inume
  endif

  isize_max=maxval(idata(2:inume+1))
  if(isize_max.gt.idim1) then
     ierr=801
     write(iout,*) ' ?ps_enum_2d_get: max enumeration size: ',isize_max, &
          ' exceeds array dimension: ',idim1
     write(iout,*) '  error retrieving enumeration object: '// &
          trim(enum_name)
  endif
  if(ierr.ne.0) return

  isize_vec=idata(2:inume+1)

  ia=0
  do ie=1,inume
     do ii=1,isize_vec(ie)
        ia=ia+1
        if(ia.gt.r8asize) then
           ierr=801
        else
           enum_2d(ii,ie)=r8data(ia)
        endif
     enddo
  enddo

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_enum_2d_get: enumeration data size: ',r8asize
     write(iout,*) '  not consistent with header information.'
     write(iout,*) '  error retrieving enumeration object: '// &
          trim(enum_name)
  endif

  nullify(r8data)
  nullify(idata)

end subroutine ps_enum_2d_get

subroutine ps_inum_1d_get(iobj,inum_name,iauthor, &
     isize_dim, isize,inum_1d, iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 9 Jun 2008

  ! XPLASMA2 implementation of plasma_state subroutine
  ! retrieve a 1d enumeration of integer values

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)       ! xplasma opaque container

  character*(*), intent(in) :: inum_name   ! NAME of list to store

  integer, intent(in) :: iauthor           ! author ID

  integer, intent(in) :: isize_dim         ! max size of enumeration (1d array)
  integer, intent(out) :: isize            ! actual size
  integer, intent(out) :: inum_1d(isize_dim)  ! the data

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: id,ii,iertmp,idhdr(2),iasize,r8asize
  
  integer, dimension(:), pointer :: idata
  real*8, dimension(:), pointer :: r8data

  character*32 pauth

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  ierr=0

  isize=0
  inum_1d=0

  nullify(idata)
  nullify(r8data)

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_inum_1d_get: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  find list ID

  call xplasma_blkbxId(s,inum_name,id)
  if(id.eq.0) then
     write(iout,*) ' ?ps_inum_1d_get: enumeration data not found: '// &
          trim(inum_name)
     ierr=801
     return
  endif

  !--------------------------------------------------
  !  fetch and check info

  call xplasma_blackbox_info(s,id,ierr, isize=iasize, r8size=r8asize, &
       author=pauth)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_inum_1d_get: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=801
  else
     if(iauthor.gt.0) then
        if(trim(ps_cnames(iauthor)).ne.trim(pauth)) then
           write(iout,*) ' ?ps_inum_1d_get: author label mismatch:'
           write(iout,*) '  data ',trim(inum_name),' has: "',trim(pauth), &
                '"; passed author reference: "',trim(ps_cnames(iauthor)),'".'
           ierr=801
        endif
     endif
  endif

  if(ierr.ne.0) return

  !--------------------------------------------------
  ! OK fetch the data

  call xplasma_blackbox_retrieve(s,id,ierr, &
       ia_ptr=idata, r8a_ptr=r8data)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_inum_1d_get: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=801
     return
  endif

  if((idata(1).ne.0).and.(idata(1).ne.1)) then
     ierr=801
     write(iout,*) ' ?ps_inum_1d_get: 1st integer header value: ',idata(1), &
          '; should be zero or one.'
  endif

  isize=idata(2)
  if(isize.gt.isize_dim) then
     ierr=801
     write(iout,*) ' ?ps_inum_1d_get: size of integer enumeration: ',idata(2),&
          ' in '//trim(inum_name)
     write(iout,*) '  exceeds size of array provided: ',isize_dim
  else
     if(size(idata).ne.isize+2) then
        write(iout,*) ' ?ps_inum_1d_get: in '//trim(inum_name)// &
             ' size of integer does not match'
        write(iout,*) '  header size of ',isize,': ',size(idata)-2
     else
        inum_1d(1:isize) = idata(3:isize+2)
     endif
  endif

end subroutine ps_inum_1d_get

subroutine ps_inum_2d_get(iobj,inum_name,iauthor, &
     inume,isize_vec, idim1,inum_2d, iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 9 Jun 2008

  ! XPLASMA2 implementation of plasma_state subroutine
  ! retrieve a 2d enumeration of integer values
  !    isize_vec(1:inume)
  !    for j=1:inume
  !       inum_2d(1:isize_vec(j),j)

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)       ! xplasma opaque container

  character*(*), intent(in) :: inum_name   ! NAME of list to store

  integer, intent(in) :: iauthor           ! author ID

  integer, intent(in) :: inume             ! number of enumerations expected
  ! (data object must match this)

  integer, intent(out) :: isize_vec(inume) ! sizes of enumerations
  integer, intent(in) :: idim1             ! inum_2d array dimension

  integer, intent(out) :: inum_2d(idim1,inume)  ! the data

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: id,ii,ie,ia,iertmp,iasize,r8asize,isize_max

  integer, dimension(:), pointer :: idata
  real(kind=rspec), dimension(:), pointer :: r8data

  character*32 pauth

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  ierr=0

  isize_vec=0
  inum_2d=0.0_rspec

  nullify(r8data)
  nullify(idata)

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_inum_2d_get: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  find list ID

  call xplasma_blkbxId(s,inum_name,id)
  if(id.eq.0) then
     write(iout,*) ' ?ps_inum_2d_get: enumeration data not found: '// &
          trim(inum_name)
     ierr=801
     return
  endif

  !--------------------------------------------------
  !  fetch and check info

  call xplasma_blackbox_info(s,id,ierr, isize=iasize, r8size=r8asize, &
       author=pauth)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_inum_2d_get: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=801
  else
     if(iauthor.gt.0) then
        if(trim(ps_cnames(iauthor)).ne.trim(pauth)) then
           write(iout,*) ' ?ps_inum_2d_get: author label mismatch:'
           write(iout,*) '  data ',trim(inum_name),' has: "',trim(pauth), &
                '"; passed author reference: "',trim(ps_cnames(iauthor)),'".'
           ierr=801
        endif
     endif
  endif

  if(ierr.ne.0) return

  !--------------------------------------------------
  ! OK fetch the data

  call xplasma_blackbox_retrieve(s,id,ierr, &
       ia_ptr=idata,r8a_ptr=r8data)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_inum_2d_get: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=801
     return
  endif

  if(idata(1).ne.inume) then
     ierr=801
     write(iout,*) ' ?ps_inum_2d_get: 1st integer header value: ',idata(1), &
          '; should match #enumerations:',inume
     write(iout,*) '  error retrieving integer enumeration object: '// &
          trim(inum_name)
  endif

  isize_max=maxval(idata(2:inume+1))
  if(isize_max.gt.idim1) then
     ierr=801
     write(iout,*) ' ?ps_inum_2d_get: max enumeration size: ',isize_max, &
          ' exceeds array dimension: ',idim1
     write(iout,*) '  error retrieving integer enumeration object: '// &
          trim(inum_name)
  endif
  if(ierr.ne.0) return

  isize_vec=idata(2:inume+1)

  ia=inume+1
  do ie=1,inume
     do ii=1,isize_vec(ie)
        ia=ia+1
        if(ia.le.iasize) then
           inum_2d(ii,ie)=idata(ia)
        else
           ierr=801
        endif
     enddo
  enddo

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_inum_2d_get: enumeration data size: ',iasize
     write(iout,*) '  not consistent with header information.'
     write(iout,*) '  error retrieving integer enumeration object: '// &
          trim(inum_name)
  endif

  nullify(r8data)
  nullify(idata)

end subroutine ps_inum_2d_get
