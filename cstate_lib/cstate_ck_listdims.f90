subroutine cstate_ck_listdims(dims,nmax,ndims,idims,ilolims,indx_enum)

  !  check if item has list dimensions

  use cstate_lib_mod
  implicit NONE

  character*(*), intent(in) :: dims
  integer, intent(in) :: nmax      ! max no. of list dimensions

  integer, intent(out) :: ndims    ! actual no. of list dimensions (0 possible)
  !  CAUTION: if ndims=nmax, output list may be incomplete...

  integer, intent(out) :: idims(nmax)   ! the dimension numbers...
  integer, intent(out) :: ilolims(nmax) ! difference btw declared lower limits
  !  for each dimension in "dims" vs. the original list dimension declaration.
  integer, intent(out) :: indx_enum     ! "enumeration" dimension index, or 
  !  zero if there is none.

  !---------------------------------
  integer :: luntty,lnout
  integer :: ii,inum,jj,iloc,jjmatch
  character*32 :: locdims(20)  ! 20 a more than reasonable upper bound
  character*32 :: enum_dim
  integer :: loc_lodims(20)
  !---------------------------------

  ndims = 0
  idims = 0
  ilolims = 0
  indx_enum = 0

  enum_dim = ' '

  inum = 0
  if(allocated(list_dims)) inum = size(list_dims)
  if(inum.eq.0) return
  if(dims.eq.'scalar') return

  ! extract dims from dims string
  !   typical dims string value: "(1:nrho_fp-1,1:nspec_nonmax)"

  call cstate_extract_dims(dims,size(locdims),iloc,locdims,loc_lodims)

  !  see if any dims are list dims

  do ii=1,iloc
     jjmatch=0
     do jj=1,inum
        if(locdims(ii).eq.list_dims(jj)) then
           jjmatch=jj
           if(nmax.gt.ndims) then
              ndims = ndims + 1
              idims(ndims) = jj
              ilolims(ndims) = loc_lodims(ii)-list_lolim(jj)
              exit
           endif
        endif
     enddo
     if(jjmatch.eq.0) then
        ! not a list dim: grid dim?
        do jj=1,size(grid_dims)
           if(locdims(ii).eq.grid_dims(jj)) then
              jjmatch=jj
              exit
           endif
        enddo
     endif
     if(jjmatch.eq.0) then
        ! not a list dim, not a grid dim: must be an enumeration dimension
        if(enum_dim.ne.' ') then
           luntty=lnout(0)
           write(luntty,*) ' ?cstate_ck_listdims: >1 enumeration dimensions:'
           write(luntty,*) enum_dim,locdims(ii)
        else
           enum_dim = locdims(ii)
           call ulower(enum_dim)
           do jj=1,nedims
              if(enum_dim.eq.enum_dims(jj)) then
                 jjmatch=jj
                 indx_enum=jj
                 exit
              endif
           enddo
        endif
     endif

     if(jjmatch.eq.0) then
        write(luntty,*) &
             ' ?cstate_ck_listdims: could not classify dimension: ', &
             locdims(ii)
     endif
  enddo

end subroutine cstate_ck_listdims
