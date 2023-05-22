subroutine cstate_extract_dims(dims,nmax,iloc,locdims,lowdims)

  !  from original dimension string (e.g. "0:nspec_th") extract the
  !  root dimensions (e.g. nspec_th).

  use cstate_lib_mod
  implicit NONE

  character*(*), intent(in) :: dims
  integer, intent(in) :: nmax      ! max no. of extracted dimensions

  integer, intent(out) :: iloc
  character*(*), intent(out), dimension(nmax) :: locdims  ! dim names
  integer, intent(out), dimension(nmax) :: lowdims        ! dim lower limit

  ! examples:
  ! "(0:nspec_th)" -> iloc=1, locdims(1)="nspec_th",lowdims(1)=0
  ! "(nbeam,0:nspec_nonMax)" -> iloc=2, locdims(1:2) = "nbeam", "nspec_nonmax"
  !                                     lowdims(1:2) = 1, 0

  !---------------------------------
  integer :: ic,ic1,ic2,ilow,il1,il2,inclow
  character*6 :: zlow
  !---------------------------------

  locdims(1:nmax)=' '
  lowdims(1:nmax)=0
  iloc = 0

  ic = 0
  ic2 = len(dims) + 1

  ! extract dims from dims string
  !   typical dims string value: "(1:nrho_fp-1,1:nspec_nonmax)"

  ilow=1
  il1=2
  il2=1

  do
     ic = ic + 1
     if((dims(ic:ic).eq.',').or.(dims(ic:ic).eq.')')) then
        ic2=min(ic2,ic-1)
        iloc = iloc + 1
        if(iloc.le.nmax) then
           locdims(iloc) = dims(ic1:ic2)
           call ulower(locdims(iloc))

           if(il2.ge.il1) then
              zlow=' '
              inclow=il2-il1+1
              zlow(6-inclow+1:6)=dims(il1:il2)
              read(zlow,'(I6)') ilow
           endif
           lowdims(iloc)=ilow

           if(dims(ic:ic).eq.')') exit
        endif

        ic1 = ic + 1
        ic2 = len(dims) + 1

        ilow = 1
        il1 = ic + 1

     endif

     if(dims(ic:ic).eq.'(') then
        ic1 = ic + 1
        il1 = ic + 1
     endif

     if(dims(ic:ic).eq.':') then
        il2 = ic - 1
        ic1 = ic + 1
     endif

     if(dims(ic:ic).eq.'-') ic2 = ic - 1
  enddo

end subroutine cstate_extract_dims
