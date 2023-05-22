module xpl_dbg_module

  !  mechanism for getting xplasma object pointer to xplasma_debug_r4momry...

  use xplasma_obj
  implicit NONE

  private
  public :: sp

  type (xplasma), pointer :: sp

end module xpl_dbg_module
