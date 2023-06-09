subroutine ccps_frantic(iobj_plasma, iobj_srces, ilun_msgs, &
  iwall, ihalo, ireco, ierr)

  ! use FRANTIC to evaluate neutral transport using input 
  ! data from two plasma states. 
  !

  use plasma_state_mod
  implicit NONE

  integer :: iobj_plasma(*) ! (ps_container_size) contains equilibrium data
  integer :: iobj_srces(*)  ! (ps_container_size) contains sources
  integer, intent(in) :: ilun_msgs
  integer, intent(in) :: iwall ! /= 0 for wall source neutral transport turned on
  integer, intent(in) :: ihalo ! /= 0 for halo source neutral transp. enabled
  integer, intent(in) :: ireco ! /= 0 to turn recombination on
  
  integer,intent(out) :: ierr  ! exit status 0=OK

  !-------------------------
  type (plasma_state_container) :: ptr_plasma, ptr_srces
  type (plasma_state), pointer :: ss_plasma, ss_srces
  !-------------------------

  call ps_bad_exit("Frantic not available as part of this version of plasma state")

  call ccps_argcheck('ccps_frantic plasma',iobj_plasma,ierr)
  if(ierr.ne.0) return
  call ccps_argcheck('ccps_frantic srces',iobj_srces,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj_plasma, ptr_plasma)
  call load_ps_container(iobj_srces, ptr_srces)
  ss_plasma => ptr_plasma%ps_ptr
  ss_srces => ptr_srces%ps_ptr

  !call ps_frantic(ss_plasma, ss_srces, ilun_msgs, iwall, ihalo, ireco, ierr)

end subroutine ccps_frantic
