subroutine cstate_encode(units,ival,r8val,c256val,disp,ilen_disp)

  ! encode data for display

  use cstate_lib_mod
  implicit NONE

  character*(*), intent(in) :: units  ! units of data
  !  if "integer": use ival
  !  if "character": use c256val
  !  otherwise use r8val

  integer, intent(in) :: ival
  real*8, intent(in) :: r8val
  character*(*), intent(in) :: c256val

  character*(*), intent(out) :: disp  ! display form of data
  integer, intent(inout) :: ilen_disp ! output max(ilen_disp,len(trim(disp))) 

  !--------------------------------------
  integer :: ilend,ilenc,itest
  !--------------------------------------

  disp = ' '
  if(units.eq.'integer') then
     write(disp(1:10),'(I10)') ival

  else if(units.eq.'character') then
     ilend=len(disp)
     ilenc=len(trim(c256val))
     itest=ilenc-ilend+4
     if(ilenc.lt.ilend) then
        disp=c256val(1:ilenc)
     else if(ntail.eq.0) then
        disp=c256val(1:ilend-3)//'...'
     else if(ntail.le.itest) then
        disp='...'//c256val(ilenc-ilend+4:ilenc)
     else
        disp='...'//c256val(ntail:ntail+ilend-5)//'...'
     endif

  else
     call cstate_r8_encode(r8val,disp)
  endif

  ilen_disp=max(ilen_disp,len(trim(disp)))

end subroutine cstate_encode

subroutine cstate_r8_encode(r8val,disp)

  !  encode a single floating point number

  use cstate_lib_mod
  implicit NONE

  real*8, intent(in) :: r8val
  character*(*), intent(out) :: disp

  !-----------------------------------
  integer :: iprec,idot,iexp,isign,itmp,ic,id,ie,ict
  integer :: iesign,jexp,jsave
  character*24 :: z24
  character*25 :: zdisp
  character*1 :: zsave

  real*8, parameter :: ZERO = 0.0d0
  logical :: all9
  !-----------------------------------

  if(r8val.eq.ZERO) then
     disp=' 0.0'
  else
     all9=.TRUE.
     iprec=min(16,nprec)
     zdisp=' '
     z24=' '
     write(z24,'(1pe24.16)') r8val
     ic = 0
     id = 1 
     idot = 0
     ict = 0
     iexp = 0
     isign = 1
     do
        ic=ic+1
        if(ic.gt.24) exit
        if(z24(ic:ic).eq.' ') cycle
        if(id.eq.1) then
           if(z24(ic:ic).eq.'+') then
              z24(ic:ic)=' '
              cycle
           else if(z24(ic:ic).eq.'-') then
              isign=-1
              zdisp(1:1)='-'
              cycle
           endif
        endif
        if(ict.lt.iprec) then
           id=id+1
           zdisp(id:id)=z24(ic:ic)
           if(idot.gt.0) then
              if(z24(ic:ic).ne.'9') all9=.FALSE.
              ict=ict+1
              if(ict.eq.iprec) then
                 zsave=z24(ic+1:ic+1)
                 if(zsave.eq.'+') zsave='0'
                 if(zsave.eq.'-') zsave='0'
                 if(zsave.eq.'e') zsave='0'
                 if(zsave.eq.'E') zsave='0'
                 if(zsave.eq.'d') zsave='0'
                 if(zsave.eq.'D') zsave='0'
                 read(zsave,'(I1)') jsave
              endif
           else
              if(z24(ic:ic).eq.'.') then
                 idot=ic
              else if((z24(ic:ic).ne.'+').and.(z24(ic:ic).ne.'-')) then
                 if(z24(ic:ic).ne.'9') all9=.FALSE.
              endif
           endif
        else if(iexp.eq.0) then
           if((z24(ic:ic).eq.'+').or.(z24(ic:ic).eq.'-')) then
              if(z24(ic:ic).eq.'-') then
                 iesign=-1
              else
                 iesign=1
              endif
              iexp=1
              ie=0
           endif
        else
           read(z24(ic:ic),'(i1)') jexp
           ie=10*ie+jexp
        endif
     enddo

     ! apply increment if necessary
     if(jsave.ge.5) then
        if(all9) then
           ie=ie+iesign
           zdisp(2:2)='1'
           do ic=4,4+iprec-1
              zdisp(ic:ic)='0'
           enddo
        else
           ic=id
           do
              if(zdisp(ic:ic).eq.'9') then
                 zdisp(ic:ic)='0'
                 ic=ic-1
                 if(zdisp(ic:ic).eq.'.') ic=ic-1
              else
                 read(zdisp(ic:ic),'(I1)') itmp
                 itmp=itmp+1
                 write(zdisp(ic:ic),'(I1)') itmp
                 exit
              endif
           enddo
        endif
     endif
     
     if((ie*iesign.lt.-1).or.(ie*iesign.gt.min(3,iprec))) then
        ! encode exponent
        id=id+1
        zdisp(id:id)='e'
        id=id+1
        if(iesign.eq.1) zdisp(id:id)='+'
        if(iesign.eq.-1) zdisp(id:id)='-'
        if(ie.le.99) then
           write(zdisp(id+1:id+2),'(i2.2)') ie
        else
           write(zdisp(id+1:id+3),'(i3)') ie
        endif
        
        disp = zdisp
        
     else if(ie.eq.0) then
        disp = zdisp
        
     else if(ie*iesign.eq.-1) then
        disp = zdisp(1:1)//'0.'//zdisp(2:2)//zdisp(4:)
        
     else
        do ic=1,ie
           id=ic+2
           zdisp(id:id)=zdisp(id+1:id+1)
           zdisp(id+1:id+1)='.'
        enddo
        disp = zdisp
     endif
  endif

end subroutine cstate_r8_encode

subroutine cstate_enumco(ivec,fvec,dispa,id1,id2,icol,iblk,iblk_size,ilen)

  ! encode floating point enumeration data for display

  use cstate_lib_mod
  implicit NONE

  !--------------------------------------------------
  integer, intent(in) :: ivec       ! size of data
  real*8, intent(in) :: fvec(ivec)  ! the data

  integer, intent(in) :: id1,id2    ! display array dimensions
  character*(*) :: dispa(id1,id2)   ! display array (modified on output)

  integer, intent(in) :: icol       ! display column now being worked on
  integer, intent(in) :: iblk       ! # of contiguous block within column
  integer, intent(in) :: iblk_size  ! block size -- can be < ivec!

  integer :: ilen                   ! max display length (modified on output)

  !--------------------------------------------------
  !  local:
  integer :: irow1,ii
  !--------------------------------------------------

  irow1 = (iblk-1)*iblk_size + 1

  if(ivec.eq.0) then
     dispa(icol,irow1) = '  (empty)'

  else if(ivec.le.iblk_size) then
     do ii=1,ivec
        call cstate_r8_encode(fvec(ii),dispa(icol,irow1+ii-1))
     enddo

  else
     call cstate_dim_encode(ivec,dispa(icol,irow1))
     do ii=1,iblk_size-4
        call cstate_r8_encode(fvec(ii),dispa(icol,irow1+ii))
     enddo
     dispa(icol,irow1+iblk_size-3) = '  ... '
     do ii=1,2
        call cstate_r8_encode(fvec(ivec-2+ii),dispa(icol,irow1+iblk_size-3+ii))
     enddo
  endif

end subroutine cstate_enumco

subroutine cstate_inumco(ivec,iivec,dispa,id1,id2,icol,iblk,iblk_size,ilen)

  ! encode floating point enumeration data for display

  use cstate_lib_mod
  implicit NONE

  !--------------------------------------------------
  integer, intent(in) :: ivec       ! size of data
  integer, intent(in) :: iivec(ivec)  ! the data

  integer, intent(in) :: id1,id2    ! display array dimensions
  character*(*) :: dispa(id1,id2)   ! display array (modified on output)

  integer, intent(in) :: icol       ! display column now being worked on
  integer, intent(in) :: iblk       ! # of contiguous block within column
  integer, intent(in) :: iblk_size  ! block size -- can be < ivec!

  integer :: ilen                   ! max display length (modified on output)

  !--------------------------------------------------
  !  local:
  integer :: irow1,ii
  !--------------------------------------------------

  irow1 = (iblk-1)*iblk_size + 1

  if(ivec.eq.0) then
     dispa(icol,irow1) = '  (empty)'

  else if(ivec.le.iblk_size) then
     do ii=1,ivec
        write(dispa(icol,irow1+ii-1)(1:10),'(I10)') iivec(ii)
     enddo

  else
     call cstate_dim_encode(ivec,dispa(icol,irow1))
     do ii=1,iblk_size-4
        write(dispa(icol,irow1+ii)(1:10),'(I10)') iivec(ii)
     enddo
     dispa(icol,irow1+iblk_size-3) = '  ... '
     do ii=1,2
        write(dispa(icol,irow1+iblk_size-3+ii)(1:10),'(I10)') iivec(ivec-2+ii)
     enddo
  endif

end subroutine cstate_inumco

subroutine cstate_dim_encode(idim,disp)

  !  encode "(1:<idim>)"

  implicit NONE

  integer, intent(in) :: idim
  character*(*), intent(out) :: disp

  !------------------------------
  character*10 zdisp
  integer :: ii,jj
  !------------------------------

  disp='(1:'
  ii=3

  write(zdisp,'(I10)') idim

  do jj=1,len(zdisp)
     if(zdisp(jj:jj).ne.' ') then
        ii=ii+1
        disp(ii:ii)=zdisp(jj:jj)
     endif
  enddo

  ii=ii+1
  disp(ii:ii)=')'

end subroutine cstate_dim_encode
