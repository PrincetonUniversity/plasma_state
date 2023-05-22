SUBROUTINE ccps_hash_diff(iobj1, lbl1, iobj2, lbl2, sstr, icomp, idiff)

  ! summarize difference between two states, based on hash code comparison.

  use iso_c_binding, only: c_char
  use plasma_state_mod
  implicit NONE


  integer :: iobj1(*)        ! 1st state
  character(kind=c_char), intent(in) :: lbl1 ! label for 1st state

  integer :: iobj2(*)        ! 2nd state
  character(kind=c_char), intent(in) :: lbl2 ! label for 2nd state

  character(kind=c_char), intent(in) :: sstr ! search string
  !  if section, dimension, or profile name contains sstr as a substring
  !  check the hash difference for that item.

  !  ==> if sstr = "*", check the hash difference for ALL items.

  INTEGER, intent(out) :: icomp     ! number of hash comparisons done
  INTEGER, intent(out) :: idiff     ! number of DIFFERENCES found

  !-------------------------
  character*256 lbl1_ftn, lbl2_ftn, sstr_ftn
  type (plasma_state_container) :: ptr1
  type (plasma_state), pointer :: ss1
  type (plasma_state_container) :: ptr2
  type (plasma_state), pointer :: ss2
  integer :: ierr
  !-------------------------
  
  call ccps_argcheck('ccps_hash_diff',iobj1,ierr)
  if(ierr.ne.0) return
  call ccps_argcheck('ccps_hash_diff',iobj2,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj1, ptr1)
  ss1 => ptr1%ps_ptr
  call load_ps_container(iobj2, ptr2)
  ss2 => ptr2%ps_ptr

  ! convert null terminated string to fortran string
  call cstring(lbl1_ftn, lbl1,'2F')
  call cstring(lbl2_ftn, lbl2,'2F')
  call cstring(sstr_ftn, sstr,'2F')
  
  call plasma_state_hash_diff(ss1,lbl1,ss2,lbl2,sstr,icomp,idiff)

end SUBROUTINE ccps_hash_diff

