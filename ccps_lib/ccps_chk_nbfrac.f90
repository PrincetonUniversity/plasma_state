! $Id: ccps_chk_nbfrac.f90,v 1.1 2009-02-09 20:38:28 Alex_Pletzer Exp $

subroutine ccps_chk_nbfrac(iobj, ichk, ierr)
    !-------------------------------------------------------------------
    !
    ! PS_CHK_NBFRAC -- utility routine for beam energy fractions
    !
    ! For He or negative_ion beams (ib): set ss%frac_full(ib)=1 and
    !     ss%frac_half(ib)=0:  these beams are at 100% full energy
    !
    ! For H/D/T beams with ss%beam_type(ib).eq."standard":
    !   if ss%nbion(ib).eq."H" check H energy fraction tables
    !   if ss%nbion(ib).eq."D" check H energy fraction tables
    !   if ss%nbion(ib).eq."T" check H energy fraction tables
    !   ...and interpolate to get full, 1/2, and 1/3 energy fractions of
    !   beam current for each beam
    !
    !   if ichk=1 set the error code if a needed energy table has not been
    !   initialized; if ichk=0, do nothing in this case.
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------
  use plasma_state_mod
  implicit NONE

   integer :: iobj(*)        ! (ps_container_size) words are used: state object
   integer, intent(in) :: ichk              ! =1 to set error code if
    !  energy fraction table(s) are not ready.

   integer, intent(out) :: ierr             ! exit status code (0=OK)

  !-------------------------
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss

  call ccps_argcheck('ccps_mhd_verify',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  call ps_chk_nbfrac(ichk=ichk, ierr=ierr, state=ss)
   
end subroutine ccps_chk_nbfrac
