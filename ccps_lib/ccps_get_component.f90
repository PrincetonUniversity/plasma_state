subroutine ccps_get_num_components(num)
  ! Number of components
  use plasma_state_util_mod, only : ps_ccount
  implicit none
  integer, intent(out) :: num
  num = ps_ccount
end subroutine ccps_get_num_components

subroutine ccps_get_component_number(name, number)
  ! Hand maintained routine returning the index number of 
  ! a plasma sub-component. This would typically be used 
  ! in conjunction with the ps_copy_plasma_state routine.
  ! $Id: ccps_get_component.f90,v 1.1 2009-12-07 23:57:49 Alex_Pletzer Exp $
  use plasma_state_util_mod
  implicit none
  character(*), intent(in) :: name
  integer, intent(out) :: number
  
  number = -1 ! <0 is an error

  if (name == 'ANOM') then
     number = ps_cnum_ANOM
  else if (name == 'EC') then
     number = ps_cnum_EC
  else if (name == 'EQ') then
     number = ps_cnum_EQ
  else if (name == 'FUS') then
     number = ps_cnum_FUS
  else if (name == 'GAS') then
     number = ps_cnum_GAS
  else if (name == 'IC') then
     number = ps_cnum_IC
  else if (name == 'LH') then
     number = ps_cnum_LH
  else if (name == 'LMHD') then
     number = ps_cnum_LMHD
  else if (name == 'NBI') then
     number = ps_cnum_NBI
  else if (name == 'PLASMA') then
     number = ps_cnum_PLASMA
  else if (name == 'RAD') then
     number = ps_cnum_RAD
  else if (name == 'RIPPLE') then
     number = ps_cnum_RIPPLE
  else if (name == 'RUNAWAY') then
     number = ps_cnum_RUNAWAY
  endif

end subroutine ccps_get_component_number

subroutine ccps_get_component_descriptor(name, descr)
  ! Hand maintained routine returning the descriptor of 
  ! a plasma sub-component. This would typically be used 
  ! in conjunction with the ps_copy_plasma_state routine.
  ! $Id: ccps_get_component.f90,v 1.1 2009-12-07 23:57:49 Alex_Pletzer Exp $
  use plasma_state_util_mod
  implicit none
  character(*), intent(in) :: name
  character(*), intent(out) :: descr
  
  descr = '' ! empty string is an error

  if (name == 'ANOM') then
     descr = ps_cdescr_ANOM
  else if (name == 'EC') then
     descr = ps_cdescr_EC
  else if (name == 'EQ') then
     descr = ps_cdescr_EQ
  else if (name == 'FUS') then
     descr = ps_cdescr_FUS
  else if (name == 'GAS') then
     descr = ps_cdescr_GAS
  else if (name == 'IC') then
     descr = ps_cdescr_IC
  else if (name == 'LH') then
     descr = ps_cdescr_LH
  else if (name == 'LMHD') then
     descr = ps_cdescr_LMHD
  else if (name == 'NBI') then
     descr = ps_cdescr_NBI
  else if (name == 'PLASMA') then
     descr = ps_cdescr_PLASMA
  else if (name == 'RAD') then
     descr = ps_cdescr_RAD
  else if (name == 'RIPPLE') then
     descr = ps_cdescr_RIPPLE
  else if (name == 'RUNAWAY') then
     descr = ps_cdescr_RUNAWAY
  endif

end subroutine ccps_get_component_descriptor
