subroutine cstate_wlen(w, &
     ilen_names,ilen_compo,ilen_vals,ilen_256,ilen_units,ilen_descr)

  !  return maximum non-blank length of various categories of ps_list_window
  !  CHARACTER*(*) data

  use plasma_state_mod
  implicit NONE

  type (ps_list_window) :: w

  integer, intent(out) :: ilen_names  ! max length: list name & element names
  integer, intent(out) :: ilen_compo  ! max length: list name & element names
  integer, intent(out) :: ilen_vals   ! max length: value fields
  integer, intent(out) :: ilen_256    ! max length: long value fields
  integer, intent(out) :: ilen_units  ! max length: units fields
  integer, intent(out) :: ilen_descr  ! max length: description fields

  !-------------------------------------
  integer :: ii
  !-------------------------------------

  ilen_names = len(trim(w%list_name))
  ilen_compo = 0
  ilen_vals = 0
  ilen_256 = 0
  ilen_units = 0
  ilen_descr = 0

  if(associated(w%list_elems)) then
     do ii=1,w%size
        ilen_names = max(ilen_names, len(trim(w%list_elems(ii))) )
     enddo
  endif

  if(associated(w%val_field)) then
     do ii=1,w%size
        ilen_vals = max(ilen_vals, len(trim(w%val_field(ii))) )
     enddo
  endif

  if(associated(w%c256val)) then
     do ii=1,w%size
        ilen_256 = max(ilen_256, len(trim(w%c256val(ii))) )
     enddo
  endif

  if(associated(w%component)) then
     ilen_compo = 9  ! allow for "Component" column header
     do ii=1,w%size
        ilen_compo = max(ilen_compo, len(trim(w%component(ii))) )
     enddo
  endif

  if(associated(w%elem_units)) then
     ilen_units = 5  ! allow for "Units" column header
     do ii=1,w%size
        ilen_units = max(ilen_units, len(trim(w%elem_units(ii))) )
     enddo
  endif

  if(associated(w%elem_descr)) then
     ilen_descr = 11 ! allow for "Description" column header
     do ii=1,w%size
        ilen_descr = max(ilen_descr, len(trim(w%elem_descr(ii))) )
     enddo
  endif

end subroutine cstate_wlen
