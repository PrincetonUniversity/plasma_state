subroutine ccps_get_prof_label(iobj,id,label,units,component,ierr)

  ! retrieve label information for specified profile
  !    C caller should allow 120 bytes for label, and 32 bytes for units
  !    and components -- all returned as null terminated byte strings

  use iso_c_binding, only: c_char
  use plasma_state_mod
  implicit NONE


  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  integer :: id             ! profile ID

  character(kind=c_char), intent(out) :: label(*)     ! item label (up to 120 characters)
  character(kind=c_char), intent(out) :: units(*)     ! item physical units (up to 32 characters)
  character(kind=c_char), intent(out) :: component(*) ! component owning the item (up to 32 characters).

  integer,intent(out) :: ierr   ! exit status 0=OK

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  character*32 :: zunits,zcomponent
  character*120 :: zlabel
  !-------------------------

  call cstring(" ",label,'2C')
  call cstring(" ",units,'2C')
  call cstring(" ",component,'2C')

  call ccps_argcheck('ccps_get_prof_label',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_get_prof_label(id,ierr, state=ss, &
       label=zlabel, units=zunits, author=zcomponent)

  call cstring(trim(zlabel),label,'2C')
  call cstring(trim(zunits),units,'2C')
  call cstring(trim(zcomponent),component,'2C')

end subroutine ccps_get_prof_label
