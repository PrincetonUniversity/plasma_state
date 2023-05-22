subroutine ps_list_get(section, iobj, listname, &
     iauthor, units, label, &
     max_prior, alt_listname, alt_author, &
     nelems, elem_names, alt_elem_names, &
     elem_ivals, elem_r8vals, elem_labels, &
     iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 8 Oct 2006

  ! XPLASMA2 implementation of plasma_state subroutine
  ! fetch a list of values, perform error checking

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  character*(*), intent(in) :: section     ! plasma state section name

  integer :: iobj(ps_container_size)     ! xplasma opaque container

  character*(*), intent(in) :: listname    ! NAME of list to fetch

  integer, intent(in) :: iauthor           ! author ID (checked if non-zero)
  character*(*), intent(inout) :: units    ! units label (checked for match)
  ! if the passed units label is "*" the check is skipped; the actual units
  ! are substituted as output.

  character*(*), intent(out) :: label      ! label for whole list

  integer, intent(in) :: max_prior         ! max prior names dimension
  character*(*), intent(in) :: alt_listname(max_prior)
           ! alternate listnames (compatibility with earlier vsn state files)
  character*(*), intent(in) :: alt_author(max_prior)
           ! alternate authornames (compatibility with earlier vsn state files)

  integer, intent(in) :: nelems            ! expected list size (checked)

  character*(*), intent(inout) :: elem_names(nelems) ! element names
                                           ! if "*", read from list
                                           ! otherwise the names must match

  character*(*), intent(in) :: alt_elem_names(max_prior,nelems)
           ! alternalte element names (compatibility with earlier versions).

  integer, intent(out) :: elem_ivals(nelems)   ! element integer values
  REAL(kind=rspec), intent(out) :: elem_r8vals(nelems)  ! element values
  character*(*), intent(out) :: elem_labels(nelems)  ! element labels or
                                           ! character data

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: ialt,ifound,id,ielems,ielem,jelem
  logical :: imatch,jmatch
  character*32 punits,pauth,ztest1,ztest2
  character*32, dimension(:), allocatable :: enames
  character*80, dimension(:), allocatable :: chvals
  integer, dimension(:), allocatable :: ivals
  REAL(kind=rspec), dimension(:), allocatable :: r8vals

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_list_get: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  find list ID

  call xplasma_listId(s,listname,id)
  if(id.eq.0) then
     do ialt = 1,max_prior
        if(alt_listname(ialt).eq.' ') exit
        call xplasma_listId(s,alt_listname(ialt),id)
        if(id.ne.0) exit
     enddo
     if(id.eq.0) then
        if(elem_names(1).eq.'*') then
           write(iout,*) ' ?ps_list_get: list not found: ',trim(listname), &
                '   (*-element, cannot recover).'
           ierr=801
           return
        else
           write(iout,*) ' %ps_list_get: list not found: ',trim(listname), &
                '   (cleared).'
           label = '(undefined).'
           elem_ivals = 0
           elem_r8vals = 0
           elem_labels = ' '
           return
        endif
     endif
  endif

  !--------------------------------------------------
  !  fetch and check profile info

  call xplasma_list_info(s,id,ierr, &
       nelems=ielems, label=label, &
       units=punits, author=pauth)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_list_get: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=801

  else
     ! general checks...

     if(units.ne.'*') then
        if(max(len(trim(units)),len(trim(punits))).gt.0) then
           if(trim(units).ne.trim(punits)) then
              write(iout,*) ' %ps_list_get: units label mismatch:'
              write(iout,*) '  list ',trim(listname),' has: "',trim(punits), &
                   '"; passed units are: "',trim(units),'" (fixed).'
              units = punits
           endif
        endif
     else
        units = punits
     endif

     if(iauthor.gt.0) then
        if(trim(ps_cnames(iauthor)).ne.trim(pauth)) then
           ifound=0
           do ialt = 1,max_prior
              if(alt_author(ialt).eq.' ') exit
              if(alt_author(ialt).eq.trim(pauth)) then
                 ifound=ialt
                 exit
              endif
           enddo
           if(ifound.eq.0) then
              write(iout,*) ' ?ps_list_get: author label mismatch:'
              write(iout,*) '  list ',trim(listname),' has: "',trim(pauth), &
                   '"; passed author reference: "',trim(ps_cnames(iauthor)),'".'
              ierr=801
           else
              call xplasma_reset_author(s,id,pauth,ps_cnames(iauthor),ierr)
           endif
        endif
     endif
  endif

  if(ierr.ne.0) return

  !--------------------------------------------------
  ! OK fetch the data

  allocate(enames(ielems),chvals(ielems),ivals(ielems),r8vals(ielems))

  call xplasma_getList_names(s,id,enames,ierr, &
       ivals=ivals, r8vals=r8vals, chvals=chvals)

  if(ierr.ne.0) then
     write(iout,*) ' ?XPLASMA2 error while attempting list data retrieval:'
     call xplasma_error(s,ierr,iout)
     ierr=802
  endif
  if(ierr.ne.0) return

  if(elem_names(1).eq.'*') then
     ! names are output
     if(ielems.ne.nelems) then
        write(iout,*) ' ?ps_list_get: size of list: ',trim(listname),' = ',nelems
        write(iout,*) '  does not match value found in file: ',ielems
        write(iout,*) '  As element names are not provided: cannot recover.'
        ierr=803

     else
        elem_names = enames
        elem_ivals = ivals
        elem_r8vals = r8vals
        elem_labels = chvals

     endif

  else

     imatch=.FALSE.
     if(ielems.eq.nelems) then
        imatch=.TRUE.
        do ielem=1,nelems
           ztest1=enames(ielem)
           ztest2=elem_names(ielem)
           call uupper(ztest1)
           call uupper(ztest2)
           if(ztest1.ne.ztest2) then
              imatch=.FALSE.
              exit
           endif
        enddo
     endif

     if(imatch) then
        elem_ivals(1:nelems)=ivals(1:nelems)
        elem_r8vals(1:nelems)=r8vals(1:nelems)
        elem_labels(1:nelems)=chvals(1:nelems)

     else
     
        ! look for match on name by name basis
        do ielem=1,nelems
           ztest1=elem_names(ielem)
           call uupper(ztest1)
           call search_list
           if(.not.jmatch) then
              do ialt=1,max_prior
                 ztest1=alt_elem_names(ialt,ielem)
                 if(ztest1.eq.' ') exit
                 call uupper(ztest1)
                 call search_list
              enddo
           endif

           if(.not.jmatch) then
              write(iout,*) ' %ps_list_get:  in ',trim(listname)
              write(iout,*) '  failed to locate list element: ', &
                   elem_names(ielem), ' (cleared).'
              elem_ivals(ielem) = 0
              elem_r8vals(ielem) = 0.0_rspec
              elem_labels(ielem) = ' '
           endif
        enddo
     endif
  endif

  contains
    subroutine search_list

      jmatch=.FALSE.
      do jelem=1,ielems
         ztest2=enames(jelem)
         call uupper(ztest2)
         if(ztest1.eq.ztest2) then
            jmatch=.TRUE.
            elem_ivals(ielem)=ivals(jelem)
            elem_r8vals(ielem)=r8vals(jelem)
            elem_labels(ielem)=chvals(jelem)
            exit
         endif
      enddo

    end subroutine search_list

end subroutine ps_list_get

subroutine ps_listId(iobj,iout,listname,id,inum)
  !  get ID of list item

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)     ! xplasma opaque container
  integer, intent(in) :: iout   ! LUN for messages
  character*(*), intent(in) :: listname  ! name of list item
  integer, intent(out) :: id    ! id of item returned (0 if not found).
  integer, intent(out) :: inum  ! size of list item (0 if not found).

  !-----------------------
  integer :: ierr
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  id = 0
  inum = 0

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_listId: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_listId(s,listname,id)
  if(id.gt.0) then
     call xplasma_getList_size(s,id,inum,ierr)
  endif

end subroutine ps_listId
