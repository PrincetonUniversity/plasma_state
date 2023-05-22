module xplasma_debug_link

  ! provide method to load and call xplasma debug routines
  ! if a program just "use"s this module, it should link xplasma_debug
  ! even if the xplasma_debug_access call is not made.

  use xplasma_definitions
  use eqi_intg_module

  private

  public :: xplasma_debug_access

  contains
    subroutine xplasma_debug_access(s)

      type (xplasma), pointer :: s

      sp_debug => s
      call xplasma_debug

    end subroutine xplasma_debug_access

end module xplasma_debug_link
