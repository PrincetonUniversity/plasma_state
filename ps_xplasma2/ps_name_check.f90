subroutine ps_name_check(listname,dim1,dim2,names,iout,ierr)

  !---------------------------------------------------------------------
  ! D. McCune Oct. 8 2006
  !   Part of plasma state implementation
  !   No actual xplasma2 dependency in this code; the only NTCC
  !   dependency is use of subroutine "uupper" to convert a character
  !   string to uppercase.
  !---------------------------------------------------------------------

  !---------------------------------------------------------------------
  ! what this subroutine does:
  !
  ! check names-- 
  !   (a) all must be non-blank;
  !   (b) all must consist of alphanumeric characters and "_"
  !   (c) no duplications within list, after uppercase conversion
  !---------------------------------------------------------------------

  implicit NONE

  character*(*), intent(in) :: listname  ! name of list of names...
  integer, intent(in) :: dim1,dim2       ! start & stop dimensions

  character*(*), intent(inout) :: names(dim1:dim2)  ! the list of names

  integer, intent(in) :: iout            ! Fortran LUN for output messages

  integer, intent(out) :: ierr           ! status code on exit, 0=OK

  !--------------------------
  ! local:

  integer :: i,j,ic,idups

  character*37, parameter :: legal = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"

  character*32 zname1,zname2
  !--------------------------

  ierr=0
  if(dim1.gt.dim2) return

  !  check for blanks...

  do i=dim1,dim2
     if(names(i).eq.' ') then
        if(ierr.eq.0) then 
           ierr=601
           write(iout,*) ' ?ps_name_check: blank names detected in: ', &
                trim(listname)
           write(iout,*) '  list is dimensioned (',dim1,':',dim2,').'
        endif
        write(iout,*) '  ...name #',i,' is blank.'
     endif
  enddo

  if(ierr.ne.0) then
     do i=dim1,dim2
        write(names(i),'(a3,i2.2)') 'var', i
        write(iout,*) '  ...reset name #',i,' to ', names(i)
     end do
  end if

  if(ierr.ne.0) return

  !  check for illegal characters...

  do i=dim1,dim2
     zname1=names(i)
     call uupper(zname1)
     do ic=1,len(trim(zname1))
        if(index(legal,zname1(ic:ic)).eq.0) then
           if(ierr.eq.0) then
              ierr=602
              write(iout,*) ' ?ps_name_check: illegal character(s) detected.'
              write(iout,*) '  in list "',trim(listname),'" -- non-alphanumeric, not "_":'
                  
              write(iout,*) '  list is dimensioned (',dim1,':',dim2,').'
           endif
           write(iout,*) '  ...name #',i,' is: "',trim(names(i)),'"'
           exit
        endif
     enddo
  enddo

  if(ierr.ne.0) return

  !  check for duplication

  idups=0

  do j=dim1,dim2-1
     zname2=names(j)
     call uupper(zname2)
     do i=j+1,dim2
        zname1=names(i)
        call uupper(zname1)
        if(zname1.eq.zname2) idups = idups+1
     enddo
  enddo

  if(idups.gt.0) then
     ierr=603
     write(iout,*) ' ?ps_name_check: duplicates in list of names "', &
          trim(listname),'".'
     write(iout,*) '  list of names:'
     do i=dim1,dim2
        write(iout,*) '    name #',i,':  "',trim(names(i)),'".'
     enddo
  endif

end subroutine ps_name_check
