subroutine ps_coordchk(xobj,coord_tag,gridname,gridId,iauthor, &
     coord_id,n,grid_data,iout,ierr)

  !----------------------------------------
  ! D. McCune 9 Oct 2006
  ! internal routine for xplasma2 implementation of plasma state
  ! check coord_tag string & translate into an xplasma2 coord ID;
  ! check that range of grid data provided is appropriate.

  ! This routine called indirectly from generated code -- not expected
  ! to be called directly from user components
  !
  ! NB DMC 10 Aug 2007 -- add ps_coordTag internal routine -- given
  !                       coord ID return coord_tag
  !----------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  type (interp_object) :: xobj
  character*(*), intent(in) :: coord_tag   ! "RHO", "TH", "R", or "Z", ...
  character*(*), intent(in) :: gridname    ! NAME of grid 
  integer, intent(in) :: gridId            ! ID of grid
  integer, intent(in) :: iauthor           ! author (component) index

  integer, intent(out) :: coord_id         ! xplasma coord ID

  integer, intent(in) :: n                 ! expected grid data size
  REAL(KIND=rspec) :: grid_data(n)         ! grid data (input; may be checked).

  integer, intent(in) :: iout              ! LUN for error messages
  integer, intent(out) :: ierr              ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...
  
  logical :: periodic

  REAL(KIND=rspec) :: bdytol,ztol,xmin,xmax
  real(KIND=rspec), parameter :: pi = 3.1415926535897931_rspec

  type (xplasma), pointer :: s
  integer :: inum,idc,ilenc
  character*3 :: suffix
  character*32 coord_name

  logical :: not_periodic = .FALSE.

  !--------------------------------------------------

  ierr = 0

  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, bdytol=bdytol)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_coordchk: unexpected error in xplasma_global_info:'
     call xplasma_error(s,ierr,iout)
     ierr=99
     return
  endif

  !--------------------------------------------------
  ! *** if this if-then-else series changes, please
  !     make matching changes in ps_coordTag, below...

  if(coord_tag.eq.'TH') then
     coord_id = xplasma_theta_coord
     periodic = .TRUE.

  else if(coord_tag.eq.'PHI') then
     coord_id = xplasma_phi_coord
     periodic = .TRUE.

  else if(coord_tag.eq.'R') then
     periodic = .FALSE.
     if(ps_cnames(iauthor).eq.'EQ') then
        coord_id = xplasma_R_coord
     else
        inum=0
        coord_name='__R_'//trim(ps_cnames(iauthor))
        ilenc=len(trim(coord_name))+3
        do 
           inum=inum+1
           write(suffix,'(I3.3)') inum
           coord_name(ilenc-2:ilenc)=suffix
           call xplasma_coordId(s,coord_name,idc)
           if(idc.eq.0) exit
           call xplasma_coord_info(s,idc,ierr,xmin=xmin,xmax=xmax)
           ztol=1.0e-10_rspec*max(abs(xmin),abs(xmax))
           if(max(abs(xmin-grid_data(1)),abs(xmax-grid_data(n))).le.ztol) then
              exit  ! coord idc is OK
           endif
        enddo
        !  if suitable coordinate was not found, create one.
        if(idc.eq.0) then
           call xplasma_create_coord(s,coord_name,not_periodic,idc,ierr, &
                label='R ('//trim(ps_cnames(iauthor))//')',units='m')
        endif
        coord_id = idc
     endif

  else if(coord_tag.eq.'Z') then
     periodic = .FALSE.
     if(ps_cnames(iauthor).eq.'EQ') then
        coord_id = xplasma_Z_coord
     else
        inum=0
        coord_name='__Z_'//trim(ps_cnames(iauthor))
        ilenc=len(trim(coord_name))+3
        do 
           inum=inum+1
           write(suffix,'(I3.3)') inum
           coord_name(ilenc-2:ilenc)=suffix
           call xplasma_coordId(s,coord_name,idc)
           if(idc.eq.0) exit
           call xplasma_coord_info(s,idc,ierr,xmin=xmin,xmax=xmax)
           ztol=1.0e-10_rspec*max(abs(xmin),abs(xmax))
           if(max(abs(xmin-grid_data(1)),abs(xmax-grid_data(n))).le.ztol) then
              exit  ! coord idc is OK
           endif
        enddo
        !  if suitable coordinate was not found, create one.
        if(idc.eq.0) then
           call xplasma_create_coord(s,coord_name,not_periodic,idc,ierr, &
                label='Z ('//trim(ps_cnames(iauthor))//')',units='m')
        endif
        coord_id = idc
     endif

  else if(coord_tag.eq.'E') then
     ! energy
     periodic = .FALSE.
     inum=0
     coord_name='__E_'//trim(ps_cnames(iauthor))
     ilenc=len(trim(coord_name))+3
     do 
        inum=inum+1
        write(suffix,'(I3.3)') inum
        coord_name(ilenc-2:ilenc)=suffix
        call xplasma_coordId(s,coord_name,idc)
        if(idc.eq.0) exit
        call xplasma_coord_info(s,idc,ierr,xmin=xmin,xmax=xmax)
        ztol=1.0e-10_rspec*max(abs(xmin),abs(xmax))
        if(max(abs(xmin-grid_data(1)),abs(xmax-grid_data(n))).le.ztol) then
           exit  ! coord idc is OK
        endif
     enddo
        !  if suitable coordinate was not found, create one.
     if(idc.eq.0) then
        call xplasma_create_coord(s,coord_name,not_periodic,idc,ierr, &
             label='E ('//trim(ps_cnames(iauthor))//')',units='keV')
     endif
     coord_id = idc

  else if(coord_tag.eq.'KY') then
     ! energy
     periodic = .FALSE.
     inum=0
     coord_name='__KY'//trim(ps_cnames(iauthor))
     ilenc=len(trim(coord_name))+3
     do
        inum=inum+1
        write(suffix,'(I3.3)') inum
        coord_name(ilenc-2:ilenc)=suffix
        call xplasma_coordId(s,coord_name,idc)
        if(idc.eq.0) exit
        call xplasma_coord_info(s,idc,ierr,xmin=xmin,xmax=xmax)
        ztol=1.0e-10_rspec*max(abs(xmin),abs(xmax))
        if(max(abs(xmin-grid_data(1)),abs(xmax-grid_data(n))).le.ztol) then
           exit  ! coord idc is OK
        endif
     enddo
        !  if suitable coordinate was not found, create one.
     if(idc.eq.0) then
        call xplasma_create_coord(s,coord_name,not_periodic,idc,ierr, &
             label='KY ('//trim(ps_cnames(iauthor))//')',units='a.u.')
     endif
     coord_id = idc

  else if(coord_tag.eq.'RHO') then
     coord_id = xplasma_rho_coord
     periodic = .FALSE.

     ztol = bdytol
     if((abs(grid_data(1)).gt.bdytol).OR. &
          (abs(grid_data(n)-1.0_rspec).gt.bdytol)) then

        ierr=99
        write(iout,*) ' ?ps_coordchk: range error in grid "',trim(gridname),'".'
        write(iout,*) '  "RHO" grid range not [0:1]'
        write(iout,*) '  1st value:  ',grid_data(1)
        write(iout,*) '  last value: ',grid_data(n)

     endif

  else

     write(iout,*) ' ?ps_coordchk: for grid "',trim(gridname),'",' 
     write(iout,*) '  coordinate tag "',trim(coord_tag),'" not recognized.'
     ierr=99
  endif

  !  end of if-then-else block for coord IDs and tags
  !--------------------------------------------------

  if(ierr.ne.0) return

  if(periodic) then

     ztol = 2*pi*bdytol

     ! grid data must either have range [-pi:pi] or [0:2pi]

     if((abs(grid_data(1)+pi).gt.ztol).OR. &
          (abs(grid_data(n)-pi).gt.ztol)) then
        if((abs(grid_data(1)).gt.ztol).OR. &
             (abs(grid_data(n)-2*pi).gt.ztol)) then

           ierr=99
           write(iout,*) ' ?ps_coordchk: range error in grid "',trim(gridname),'".'
           write(iout,*) '  periodic grid "',trim(coord_tag),'" range'
           write(iout,*) '     should be either [-pi:pi] or [0:2*pi].'
           write(iout,*) '  1st value:  ',grid_data(1)
           write(iout,*) '  last value: ',grid_data(n)
        endif
     endif

  endif

end subroutine ps_coordchk
           
        
subroutine ps_coordTag(xobj,idc,tag)

  ! internal routine for ps_xplasma

  ! given a coordinate tag, return a tag coordinate string, consistent
  ! with the string -> tag mapping of subroutine ps_coordchk


  use plasma_state_xpobj  ! XPLASMA2 state object definitions

  use plasma_state_constants_mod  ! PARAMETERs ps_* defined here
  use swim_global_data_mod, only: rspec
  implicit NONE

  !-----------------------
  type (interp_object) :: xobj
  integer, intent(in) :: idc      ! coordinate tag
  character*(*), intent(out) :: tag
  integer :: ierr,itype
  character*32 :: cname

  type (xplasma), pointer :: s
  !-----------------------

  tag = '??'

  if(idc.eq.xplasma_rho_coord) then
     tag = 'RHO'

  else if(idc.eq.xplasma_theta_coord) then
     tag = 'TH'

  else if(idc.eq.xplasma_phi_coord) then
     tag = 'PHI'

  else if(idc.eq.xplasma_R_coord) then
     tag = 'R'

  else if(idc.eq.xplasma_Z_coord) then
     tag = 'Z'

  else

     s => xobj%s_xpobj
     call xplasma_get_item_info(s,idc,ierr,name=cname,itype=itype)
     write(*,*) 'cname=',cname
     if(itype.eq.xplasma_coordType) then
        if(cname(1:3).eq.'__R') tag='R'
        if(cname(1:3).eq.'__Z') tag='Z'
        if(cname(1:3).eq.'__E') tag='E'
     endif

  endif

end subroutine ps_coordTag
