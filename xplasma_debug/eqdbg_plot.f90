subroutine eqdbg_plot

  ! this was the old f77 plotting routine; now calls f90 module routine

  use xplasma_obj_instance
  use xplasma_debug_link

  implicit NONE

  call xplasma_debug_access(s)

end subroutine eqdbg_plot
