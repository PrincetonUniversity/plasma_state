! $Id: ccps_species.f90,v 1.2 2009-01-20 02:13:58 xshare Exp $
! F77/C access routines to plasma_state methods

subroutine ccps_species_convert(iZatom, iZcharge, iAMU, qatom, qcharge, mass, &
     ierr)
  use plasma_state_mod
  implicit none

  integer, intent(in) :: iZatom    ! atomic number: 1 for H, 2 for He, ...
  !  special: -1 for electrons

  integer, intent(in) :: iZcharge  ! ionic charge btw 1 and iZatom
  !  ignored for electrons

  integer, intent(in) :: iAMU      ! atomic weight in AMU, nearest integer
  !  ignored unless 1 <= iZatom <= 2

  REAL(KIND=rspec), intent(out) :: qatom    ! charge of fully stripped ion
  REAL(KIND=rspec), intent(out) :: qcharge  ! actual charge
  REAL(KIND=rspec), intent(out) :: mass     ! mass in kg

  integer,intent(out) :: ierr      ! exit status 0=OK

  call ps_species_convert(iZatom, iZcharge, iAMU, qatom, qcharge, mass, ierr)
end subroutine ccps_species_convert

subroutine ccps_label_species(iobj, ierr)
  use plasma_state_mod
  implicit none

  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  integer,intent(out) :: ierr      ! exit status 0=OK

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_label_species',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_label_species(ierr, state=ss)
end subroutine ccps_label_species

subroutine ccps_merge_species_lists(iobj, ierr)
  use plasma_state_mod
  implicit none

  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  integer,intent(out) :: ierr      ! exit status 0=OK

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_merge_species_lists',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_merge_species_lists(ierr, state=ss)
end subroutine ccps_merge_species_lists

subroutine ccps_neutral_species(iobj, ierr)
  use plasma_state_mod
  implicit none

  integer :: iobj(*)        ! (ps_container_size) words are used: state object
  integer,intent(out) :: ierr      ! exit status 0=OK

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_neutral_species',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_neutral_species(ierr, state=ss)
end subroutine ccps_neutral_species
