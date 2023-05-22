subroutine ps_list_store(section, iobj, listname, &
     iauthor, units, label, &
     nelems, elem_names, elem_ivals, elem_r8vals, elem_labels, &
     imod, iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 8 Oct 2006

  ! XPLASMA2 implementation of plasma_state subroutine
  ! store a list of values, perform error checking

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  character*(*), intent(in) :: section     ! plasma state section name

  integer :: iobj(ps_container_size)     ! xplasma opaque container

  character*(*), intent(in) :: listname    ! NAME of list to store

  integer, intent(in) :: iauthor           ! author ID
  character*(*), intent(in) :: units       ! units label

  character*(*), intent(in) :: label       ! label for whole list

  integer, intent(in) :: nelems            ! list size

  character*(*), intent(in) :: elem_names(nelems) ! element names

  integer, intent(in) :: elem_ivals(nelems)   ! element integer values
  REAL(kind=rspec), intent(in) :: elem_r8vals(nelems)  ! element values
  character*(*), intent(in) :: elem_labels(nelems)  ! element labels or
                                           ! character data

  integer, intent(out) :: imod             ! =1 if profile is new or modified
  !  =0 on output if profile was not changed.

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------
  ! local...

  integer :: id,istat,iertmp

  !--------------------------------------------------

  imod = 0

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_list_store: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  set author

  call xplasma_author_set(s,ps_cnames(iauthor),ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_list_store: unexpected xplasma_author_set error:'
     call xplasma_error(s,ierr,iout)
     ierr=101
  endif
  if(ierr.ne.0) return

  !--------------------------------------------------
  !  write the list

  call xplasma_create_list(s,listname,elem_names,id,ierr, &
       label=label, units=units, &
       ivals=elem_ivals, r8vals=elem_r8vals, chvals=elem_labels, &
       change_status = istat)

  imod = min(1,istat)
       
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_list_store: unexpected XPLASMA2 error:'
     call xplasma_error(s,ierr,iout)
     ierr=101
  endif

  !--------------------------------------------------
  ! clear author

  call xplasma_author_clear(s,ps_cnames(iauthor),iertmp)
  if(iertmp.ne.0) then
     write(iout,*) ' ?ps_list_store: unexpected xplasma_author_clear error:'
     call xplasma_error(s,ierr,iout)
     ierr=101
  endif

end subroutine ps_list_store

subroutine ps_list_update_r8(iobj, listname, iauthor, &
     nelems, elem_r8vals, iout, ierr)

  !------------------------------------------------------------
  ! D. McCune 8 Oct 2006

  ! XPLASMA2 implementation of plasma_state subroutine
  ! insert list of (modified) floating point values in existing list
  !    this is used in computing scalar reduced values from profiles
  !    modification of values is not checked, because, modifications
  !    to the profile itself was already checked (it is assumed).

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)     ! xplasma opaque container

  character*(*), intent(in) :: listname    ! NAME of list to store

  integer, intent(in) :: iauthor           ! author ID

  integer, intent(in) :: nelems            ! list size

  REAL(kind=rspec), intent(in) :: elem_r8vals(nelems)  ! element values

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: id,iertmp

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_list_update: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  !  set author

  call xplasma_author_set(s,ps_cnames(iauthor),ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_list_update_r8: unexpected xplasma_author_set error:'
     call xplasma_error(s,ierr,iout)
     ierr=101
  endif
  if(ierr.ne.0) return

  !--------------------------------------------------
  
  call xplasma_listId(s,listname,id)
  if(id.eq.0) then
     write(iout,*) ' ?ps_list_update_r8: expected list not found: ', &
          trim(listname)
     ierr=103
     goto 999
  endif

  call xplasma_setList_r8vals(s,id,elem_r8vals,ierr)
  if(ierr.ne.0) then
     write(iout,*) &
          ' ?ps_list_update_r8: unexpected xplasma_setList_r8vals error:'
     call xplasma_error(s,ierr,iout)
     ierr=105
     goto 999
  endif

  !--------------------------------------------------
  ! clear author

999 continue
  call xplasma_author_clear(s,ps_cnames(iauthor),iertmp)
  if(iertmp.ne.0) then
     write(iout,*) ' ?ps_list_store: unexpected xplasma_author_clear error:'
     call xplasma_error(s,ierr,iout)
     ierr=101
  endif

end subroutine ps_list_update_r8
