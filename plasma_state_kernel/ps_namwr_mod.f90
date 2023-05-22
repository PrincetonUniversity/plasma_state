module ps_namwr_mod

  ! hand maintained
  ! module routines to assist production of commented namelist output
  ! from plasma state

  use plasma_state_constants_mod
  implicit NONE

  PRIVATE

  PUBLIC :: ps_namwr
  PUBLIC :: ps_namwr_c0,ps_namwr_c1,ps_namwr_i0,ps_namwr_i1,ps_namwr_i2
  PUBLIC :: ps_namwr_d0,ps_namwr_d1,ps_namwr_d2
  PUBLIC :: ps_namwr_hd

  INTERFACE ps_namwr
     module procedure ps_namwr_c0,ps_namwr_c1
     module procedure ps_namwr_i0,ps_namwr_i1,ps_namwr_i2
     module procedure ps_namwr_d0,ps_namwr_d1,ps_namwr_d2
     module procedure ps_namwr_slist
     module procedure ps_namwr_hd
  END INTERFACE

!
! gfortran can not handle a namelist entry like
!   rlim(1) =  0.1851,  0.1851,  0.2794,  0.2794,  0.2979,  0.5712,  0.5712
! my guess is that it would prefer this to be
!   rlim(1:7) =  0.1851,  0.1851,  0.2794,  0.2794,  0.2979,  0.5712,  0.5712
! so for simplicity in code changes, just write out a single line when using gfortran
!
#ifdef __GFORTRAN
  logical, parameter :: namwr_single_line=.true.  ! write one line per namelist array element 
#else
  logical, parameter :: namwr_single_line=.false.
#endif

  !--------------------------------------------------
  ! local data private to the module (not SAVEd) 
  character*20 :: cval
  integer :: ilnb

  !--------------------------------------------------
  ! local private constants
  character*1, parameter :: echar = 'd'
  character*1, parameter :: dquot = '"'

  !--------------------------------------------------
  ! public routines...

CONTAINS

  subroutine ps_namwr_hd(ilun,descr,units,name)

    integer, intent(in) :: ilun         ! unit to write
    character*(*), intent(in) :: descr  ! description
    character*(*), intent(in) :: units  ! "units" label
    character*(*), intent(in) :: name   ! name of variable

    write(ilun,'(" ")')
    write(ilun,'("   ! name: ",a,"  units: ",a)') trim(name), trim(units)
    write(ilun,'("   ! description: ",a)') trim(descr)

  end subroutine ps_namwr_hd

  subroutine ps_namwr_c0(ilun,descr,units,name,val)

    integer, intent(in) :: ilun         ! unit to write
    character*(*), intent(in) :: descr  ! description
    character*(*), intent(in) :: units  ! "units" label
    character*(*), intent(in) :: name   ! name of variable
    character*(*), intent(in) :: val    ! its value

    call ps_namwr_hd(ilun,descr,units,name)
    write(ilun,'(" ")')

    ilnb = max(1,len(trim(val)))
    write(ilun,'("   ",a," = ",a,a,a)') trim(name),dquot,val(1:ilnb),dquot

  end subroutine ps_namwr_c0

  subroutine ps_namwr_c1(ilun,descr,units,name,inum,val)

    integer, intent(in) :: ilun         ! unit to write
    character*(*), intent(in) :: descr  ! description
    character*(*), intent(in) :: units  ! "units" label
    character*(*), intent(in) :: name   ! name of array
    integer, intent(in) :: inum         ! number of values in array
    character*(*), dimension(inum), intent(in) :: val    ! the values

    integer :: ii

    call ps_namwr_hd(ilun,descr,units,name)
    write(ilun,'(" ")')

    do ii=1,inum
       ilnb = max(1,len(trim(val(ii))))
       if(ii.le.9) then
          write(ilun,'("   ",a,"(",i1,") = ",a,a,a)') trim(name),ii, &
               dquot,val(ii)(1:ilnb),dquot
       else if(ii.le.99) then
          write(ilun,'("   ",a,"(",i2,") = ",a,a,a)') trim(name),ii, &
               dquot,val(ii)(1:ilnb),dquot
       else if(ii.le.999) then
          write(ilun,'("   ",a,"(",i3,") = ",a,a,a)') trim(name),ii, &
               dquot,val(ii)(1:ilnb),dquot
       endif
    enddo
  end subroutine ps_namwr_c1


  subroutine ps_namwr_i0(ilun,descr,units,name,val)

    integer, intent(in) :: ilun         ! unit to write
    character*(*), intent(in) :: descr  ! description
    character*(*), intent(in) :: units  ! "units" label
    character*(*), intent(in) :: name   ! name of variable
    integer, intent(in) :: val    ! its value

    call ps_namwr_hd(ilun,descr,units,name)
    write(ilun,'(" ")')

    if(val.ne.ps_unInit) then
       call ival_clean(val)
       write(ilun,'("   ",a," = ",a)') trim(name),cval(1:ilnb)
    endif

  end subroutine ps_namwr_i0

  subroutine ps_namwr_i1(ilun,descr,units,name,inum,val)

    integer, intent(in) :: ilun         ! unit to write
    character*(*), intent(in) :: descr  ! description
    character*(*), intent(in) :: units  ! "units" label
    character*(*), intent(in) :: name   ! name of array
    integer, intent(in) :: inum         ! number of values in array
    integer, dimension(inum), intent(in) :: val    ! the values

    call ps_namwr_hd(ilun,descr,units,name)
    write(ilun,'(" ")')

    if(inum.gt.0) then
       call namwr_i1_write(ilun,name,0,val)
    endif

  end subroutine ps_namwr_i1

  subroutine ps_namwr_i2(ilun,descr,units,name,isize_vec,vals)

    integer, intent(in) :: ilun         ! unit to write
    character*(*), intent(in) :: descr  ! description
    character*(*), intent(in) :: units  ! "units" label
    character*(*), intent(in) :: name   ! name of array

    integer, intent(in), dimension(:) :: isize_vec  ! row sizes
    integer, intent(in), dimension(:,:) :: vals     ! 2d array of values

    integer :: irows,inum,ii

    call ps_namwr_hd(ilun,descr,units,name)
    write(ilun,'(" ")')

    irows = size(isize_vec)
    do ii=1,irows
       inum = isize_vec(ii)
       if(inum.gt.0) then
          call namwr_i1_write(ilun,name,ii,vals(1:inum,ii))
       endif
    enddo

  end subroutine ps_namwr_i2

  subroutine ps_namwr_d0(ilun,descr,units,name,val)

    integer, intent(in) :: ilun         ! unit to write
    character*(*), intent(in) :: descr  ! description
    character*(*), intent(in) :: units  ! "units" label
    character*(*), intent(in) :: name   ! name of variable
    real*8, intent(in) :: val    ! its value

    call ps_namwr_hd(ilun,descr,units,name)
    write(ilun,'(" ")')

    call dval_clean(val)
    write(ilun,'("   ",a," = ",a)') trim(name),cval(1:ilnb)

  end subroutine ps_namwr_d0

  subroutine ps_namwr_d1(ilun,descr,units,name,inum,val)

    integer, intent(in) :: ilun         ! unit to write
    character*(*), intent(in) :: descr  ! description
    character*(*), intent(in) :: units  ! "units" label
    character*(*), intent(in) :: name   ! name of array
    integer, intent(in) :: inum         ! number of values in array
    real*8, dimension(inum), intent(in) :: val    ! the values

    call ps_namwr_hd(ilun,descr,units,name)
    write(ilun,'(" ")')

    if(inum.gt.0) then
       call namwr_d1_write(ilun,name,0,val)
    endif

  end subroutine ps_namwr_d1

  subroutine ps_namwr_d2(ilun,descr,units,name,isize_vec,vals)

    integer, intent(in) :: ilun         ! unit to write
    character*(*), intent(in) :: descr  ! description
    character*(*), intent(in) :: units  ! "units" label
    character*(*), intent(in) :: name   ! name of array

    integer, intent(in), dimension(:) :: isize_vec  ! row sizes
    real*8, intent(in), dimension(:,:) :: vals      ! 2d array of values

    integer :: irows,inum,ii

    call ps_namwr_hd(ilun,descr,units,name)
    write(ilun,'(" ")')

    irows = size(isize_vec)
    do ii=1,irows
       inum = isize_vec(ii)
       if(inum.gt.0) then
          call namwr_d1_write(ilun,name,ii,vals(1:inum,ii))
       endif
    enddo

  end subroutine ps_namwr_d2

  subroutine ps_namwr_slist(ilun,descr,units,name,inum, &
       snames,sqatom,sq,sm)

    integer, intent(in) :: ilun         ! unit to write
    character*(*), intent(in) :: descr  ! description
    character*(*), intent(in) :: units  ! "units" label
    character*(*), intent(in) :: name   ! name of array
    integer, intent(in) :: inum         ! number of values in array

    character*(*), dimension(inum), intent(in) :: snames  ! specie names
    real*8, dimension(inum), intent(in) :: sqatom  ! specie atomic charges
    real*8, dimension(inum), intent(in) :: sq      ! specie ionic charges
    real*8, dimension(inum), intent(in) :: sm      ! specie masses

    integer :: ibuf(inum)

    call ps_namwr_hd(ilun,descr,units,name)
    write(ilun,'(" ")')

    if(inum.gt.0) then
       ibuf = sm(1:inum)/ps_mp + 0.1
       call namwr_i1_write(ilun,'iAMU_'//trim(name),0,ibuf)

       ibuf = sqatom(1:inum)/ps_xe + 0.1
       call namwr_i1_write(ilun,'iZatom_'//trim(name),0,ibuf)

       ibuf = sq(1:inum)/ps_xe + 0.1
       call namwr_i1_write(ilun,'iZion_'//trim(name),0,ibuf)
    endif

  end subroutine ps_namwr_slist


  !--------------------------------------------------
  ! private routines...

  subroutine namwr_i1_write(ilun,name,ind2,iarray)

    integer, intent(in) :: ilun
    character*(*), intent(in) :: name
    integer, intent(in) :: ind2
    integer, dimension(:), intent(in) :: iarray

    character*120 :: buf
    integer :: ilnam
    integer :: ind0,ind,isize,ic
    character*10 indc
    logical :: toggle

    ilnam = len(trim(name))
    isize = size(iarray)
    ind0=1

    call indc_encode(ind0,ind2,indc)

    do 
       buf = '   '//name(1:ilnam)//'('//trim(indc)//') = '
       ic = len(trim(buf)) + 1

       ind=ind0-1
       toggle = .false.
       do
          ind=ind+1
          if(ind.gt.isize) exit

          call ival_clean(iarray(ind))

          if((ic + ilnb + 1).ge.80 .or. toggle) then
             ind0=ind
             exit
          endif

          if(ind.lt.isize) then
             buf(ic+1:ic+ilnb+1) = cval(1:ilnb)//','
          else
             buf(ic+1:ic+ilnb) = cval(1:ilnb)
          endif
          ic = len(trim(buf)) + 1
          toggle = namwr_single_line
       enddo

       if(ind.le.isize) then
          ic=len(trim(buf))
          buf(ic:ic)=' '  ! remove trailing comma
       endif

       write(ilun,'(A)') trim(buf)
       if(ind.gt.isize) exit

       call indc_encode(ind0,ind2,indc)
    enddo

  end subroutine namwr_i1_write

  subroutine namwr_d1_write(ilun,name,ind2,darray)

    integer, intent(in) :: ilun
    character*(*), intent(in) :: name
    integer, intent(in) :: ind2
    real*8, dimension(:), intent(in) :: darray

    character*120 :: buf
    integer :: ilnam
    integer :: ind0,ind,isize,ic
    character*10 indc
    logical :: toggle

    ilnam = len(trim(name))
    isize = size(darray)
    ind0=1

    call indc_encode(ind0,ind2,indc)

    do 
       buf = '   '//name(1:ilnam)//'('//trim(indc)//') = '
       ic = len(trim(buf)) + 1

       ind=ind0-1
       toggle = .false.
       do
          ind=ind+1
          if(ind.gt.isize) exit

          call dval_clean(darray(ind))

          if((ic + ilnb + 1).ge.80 .or. toggle) then
             ind0=ind
             exit
          endif

          if(ind.lt.isize) then
             buf(ic+1:ic+ilnb+1) = cval(1:ilnb)//','
          else
             buf(ic+1:ic+ilnb) = cval(1:ilnb)
          endif
          ic = len(trim(buf)) + 1
          toggle = namwr_single_line
       enddo

       if(ind.le.isize) then
          ic=len(trim(buf))
          buf(ic:ic)=' '  ! remove trailing comma
       endif

       write(ilun,'(A)') trim(buf)
       if(ind.gt.isize) exit

       call indc_encode(ind0,ind2,indc)
    enddo

  end subroutine namwr_d1_write

  subroutine indc_encode(ind0,ind2,indc)

    integer, intent(in) :: ind0 ! 1st dimension value
    integer, intent(in) :: ind2 ! IF .GT. 0: 2nd dimension value
    character*(*), intent(out) :: indc

    character*4 :: indc2

    indc=' '
    if(ind0.le.9) then
       write(indc(1:1),'(I1)') ind0
    else if(ind0.le.99) then
       write(indc(1:2),'(I2)') ind0
    else if(ind0.le.999) then
       write(indc(1:3),'(I3)') ind0
    else
       write(indc(1:4),'(I4)') ind0
    endif
       
    if(ind2.gt.0) then
       indc2=' '
       if(ind2.le.9) then
          write(indc2(1:1),'(I1)') ind2
       else if(ind2.le.99) then
          write(indc2(1:2),'(I2)') ind2
       else if(ind2.le.999) then
          write(indc2(1:3),'(I3)') ind2
       else
          write(indc2(1:4),'(I4)') ind2
       endif

       indc = trim(indc)//','//trim(indc2)
    endif

  end subroutine indc_encode

  subroutine ival_clean(ival)
    ! (private)
    ! encode an integer (ival) left justified

    integer, intent(in) :: ival

    integer :: ii,ic

    cval=' '
    write(cval,'(I18)') ival

    ic=0
    do ii=1,len(cval)
       if(cval(ii:ii).ne.' ') then
          ic=ic+1
          cval(ic:ic) = cval(ii:ii)
       endif
    enddo

    cval(ic+1:len(cval)) = ' '

    ilnb = len(trim(cval))

  end subroutine ival_clean

  subroutine dval_clean(dval)
    ! (private)
    ! encode a floating point number (dval) and then shorten the
    ! encoding where possible...

    !  examples:  1.200000E+01 -> 12.0
    !             7.000000E-01 ->  0.7
    !             6.300000E+23 ->  6.3e23

    real*8, intent(in) :: dval

    !--------------------
    character*4 cexp
    integer iexp,ic,ilen,ie1,ie2,iechar,ine,istop,imove
    !--------------------

    !------------------------------------------------

    cval=' '
    write(cval,'(1pe18.11)') dval

    ! first find and decode the exponent; substitute "echar"

    ilen=len(cval)
    ie1=0
    ie2=0
    do ic=ilen,1,-1
       if(ie2.eq.0) then
          if(cval(ic:ic).ne.' ') then
             ie2=ic
          endif
       else
          if((cval(ic:ic).eq.'E').or.(cval(ic:ic).eq.'e').or. &
               (cval(ic:ic).eq.'D').or.(cval(ic:ic).eq.'d')) then
             iechar=ic
             ie1=ic+1
             exit
          endif
       endif
    enddo

    cexp=' '
    ine=ie2-ie1+1
    cexp(4-ine+1:4)=cval(ie1:ie2)
    read(cexp,'(I4)') iexp  ! encode exponent
    cval(iechar:)=' '       ! delete exponent from string
    !debug    write(6,*) ' iexp = ',iexp,' cval = ',cval

    istop=5
    if(iexp.eq.1) istop=6
    if(iexp.eq.2) istop=7
    if(iexp.eq.-1) istop=4

    !  now strip off any trailing zeroes

    imove=iechar
    do ic=iechar-1,istop,-1
       if(cval(ic:ic).eq.'0') then
          imove=ic
       else
          exit
       endif
    enddo

    cval(imove:iechar-1)=' '
    if(iexp.eq.-1) then
       do ic=imove-1,4,-1
          cval(ic+1:ic+1)=cval(ic:ic)
       enddo
       cval(4:4)=cval(2:2)
       cval(2:2)='0'
    else if(iexp.eq.0) then
       continue
    else if(iexp.eq.1) then
       cval(3:3)=cval(4:4)
       cval(4:4)='.'
    else if(iexp.eq.2) then
       cval(3:3)=cval(4:4)
       cval(4:4)=cval(5:5)
       cval(5:5)='.'
    else
       cval(imove:imove)=echar
       ic=imove+1
       if(iexp.gt.0) then
          if(iexp.lt.10) then
             write(cval(ic:ic),'(I1)') iexp
          else if(iexp.lt.100) then
             write(cval(ic:ic+1),'(I2)') iexp
          else
             write(cval(ic:ic+2),'(I3)') iexp
          endif
       else
          if(iexp.gt.-10) then
             write(cval(ic:ic+1),'(I2)') iexp
          else if(iexp.gt.-100) then
             write(cval(ic:ic+2),'(I3)') iexp
          else
             write(cval(ic:ic+3),'(I4)') iexp
          endif
       endif
    endif

    ilnb = len(trim(cval))

  end subroutine dval_clean

end module ps_namwr_mod
