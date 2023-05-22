module eq_module
  use iso_c_binding, only: fp => c_double
  ! private module used in implementation of original f77 style XPLASMA
  ! interface... i.e. private to the xplasma library, not meant for use
  ! by subroutines outside xplasma.

  ! NOTE: this module is meant to be used in the xplasma library along with
  ! the public module "xplasma_obj_instance" which makes a shared xplasma
  ! object available.

  implicit NONE
  save

  logical :: eq_module_init = .FALSE.

  integer :: lunerr = 6

  character*132 :: eq_premsg_text = ' '

  !--------------------------
  !  constants...

  real*8, parameter :: ceps4 = 1.0d-4

  real(fp), parameter :: czero = 0.0
  real(fp), parameter :: chalf = 0.5_fp
  real(fp), parameter :: cone = 1.0_fp
  real(fp), parameter :: ctwo = 2.0_fp
  real(fp), parameter :: cpi  = atan2(0.0_fp,-1.0_fp)
  real(fp), parameter :: c2pi = 2.0_fp*atan2(0.0_fp,-1.0_fp)

end module eq_module
