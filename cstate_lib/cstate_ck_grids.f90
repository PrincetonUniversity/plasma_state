subroutine cstate_ck_grids(dims,nmax,ngrids,igrids)

  !  check item (profile or group of profiles) for grid dimensions
  !  it is assumed that "wgrid" contains the list of grid dimensions

  use cstate_lib_mod
  implicit NONE

  character*(*), intent(in) :: dims
  integer, intent(in) :: nmax      ! max no. of grids

  integer, intent(out) :: ngrids   ! actual no. of grids (0 possible)
  !  CAUTION: if ndims=ngrids, output list may be incomplete...

  integer, intent(out) :: igrids(nmax)   ! the dimension numbers (ref wgrid).

  !---------------------------------
  integer :: ii,inum,jj,iloc,imatch,igot
  character*32 :: locdims(20)  ! 20 a more than reasonable upper bound
  character*32 ztest2(1)
  integer :: loc_lodims(20),ilow2(1)
  !---------------------------------

  ngrids = 0
  igrids = 0

  inum = 0
  if(allocated(list_dims)) inum = size(list_dims)
  if(dims.eq.'scalar') return

  ! extract dims from dims string
  !   typical dims string value: "(1:nrho_fp-1,1:nspec_nonmax)"

  call cstate_extract_dims(dims,size(locdims),iloc,locdims,loc_lodims)

  !  see if any dims are list dims

  do ii=1,iloc
     imatch=0
     do jj=1,inum
        if(locdims(ii).eq.list_dims(jj)) then
           imatch=jj
           exit
        endif
     enddo
     if(imatch.ne.0) cycle

     ! not a list dim, so, must be a grid dim

     if(ngrids.eq.nmax) cycle

     ngrids = ngrids + 1

     imatch=0
     do jj=1,wgrid%size
        call cstate_extract_dims(wgrid%val_field(jj),1,igot,ztest2,ilow2)
        if(ztest2(1).eq.locdims(ii)) then
           imatch=jj
           exit
        endif
     enddo

     if(imatch.eq.0) then
        ngrids=ngrids-1   ! this should not happen

     else
        igrids(ngrids)=imatch
     endif

  enddo

end subroutine cstate_ck_grids

subroutine cstate_grid_info(luntty,ngrids,igrids, &
     gnames,ids,igsizes,cnames,xunits)

  !  check item (profile or group of profiles) for grid dimensions
  !  it is assumed that "wgrid" contains the list of grid dimensions

  use cstate_lib_mod
  implicit NONE

  !----------------------

  integer, intent(in) :: luntty  ! LUN for messages...

  integer, intent(in) :: ngrids  ! number of grids
  integer, intent(in) :: igrids(ngrids)          ! grid numbers in contents...

  character*(*), intent(out) :: gnames(ngrids)   ! grid names
  integer, intent(out) :: ids(ngrids,nsss_cur)   ! the grid IDs
  integer, intent(out) :: igsizes(ngrids,nsss_cur) ! the grid sizes
  character*(*), intent(out) :: cnames(ngrids)   ! coord tag: "rho", "R", "Z", ...
  character*(*), intent(out) :: xunits(ngrids)   ! coord units

  !----------------------
  integer :: ig,igrid,is,ierr,idgot
  character*32 :: gname
  logical :: ifirst(ngrids)
  !----------------------

  ids=0
  igsizes=0
  cnames=' '

  ifirst = .TRUE.

  do is=1,nsss_cur
     call psw_contents('G',ncur_components,wtmp,ierr, state=sss(is))
     if(ierr.ne.0) exit

     do ig=1,ngrids
        igrid=igrids(ig)
        gname = wtmp%list_elems(igrid)
        call ps_gridId(sss(is)%iobj,luntty,gname,idgot,igsizes(ig,is))
        ids(ig,is)=idgot
        if(idgot.gt.0) then
           if(ifirst(ig)) then
              ifirst(ig)=.FALSE.
              gnames(ig)=gname
              call ps_gridCoord(sss(is)%iobj,luntty,ids(ig,is),cnames(ig), &
                   xunits(ig))
              if(cnames(ig).eq.'RHO') cnames(ig)='rho'
              if(cnames(ig).eq.'TH') cnames(ig)='theta'
              if(cnames(ig).eq.'PHI') cnames(ig)='phi'
           endif
        endif
     enddo
  enddo

  if(ierr.ne.0) then
     write(luntty,*) ' ?cstate_grid_info: unexpected error.'
     return
  endif

end subroutine cstate_grid_info
