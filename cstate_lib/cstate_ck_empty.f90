subroutine cstate_ck_empty(dims,result)

  !  check if item is empty -- it is empty if any of its dimensions are zero.

  use cstate_lib_mod
  implicit NONE

  character*(*), intent(in) :: dims
  logical, intent(out) :: result

  !---------------------------------
  integer :: ii,inum,jj,iloc
  character*32 :: locdims(20)  ! 20 a more than reasonable upper bound
  integer :: ilowlims(20)
  !---------------------------------

  result = .FALSE.
  if(dims.eq.'scalar') return  ! scalars are never empty...

  inum = 0
  if(allocated(empty_dims)) inum = size(empty_dims)
  if(inum.eq.0) return

  iloc = 0

  ! extract dims from dims string
  !   typical dims string value: "(1:nrho_fp-1,1:nspec_nonmax)"

  call cstate_extract_dims(dims,size(locdims),iloc,locdims,ilowlims)

  !  see if any dims are on the empty list
  do jj=1,inum
     do ii=1,iloc
        if(locdims(ii).eq.empty_dims(jj)) then
           result = .TRUE.
           exit
        endif
     enddo
     if(result) exit
  enddo

end subroutine cstate_ck_empty
