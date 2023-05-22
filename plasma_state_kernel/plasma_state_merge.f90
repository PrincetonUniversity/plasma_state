! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_merge(cclist,w1,ss1,ss2,ss3,icheck,ierr)

  ! Merge floating point data
  !   ss3 <-- w1*ss1 + (1-w1)*ss2

  ! cclist(...) control specifies the components for which data are merged.
  !   If cclist(j)=1 all data are merged for component j.
  !   If cclist(j)=0, data merger suppressed for this component.
  !   NOTE: species list data if present is checked, regardless of cclist(...).

  ! Non-floating-point data is copied from ss1
  ! with warnings if ss1 & ss2 values do not match & icheck = ps_warn
  ! with error messages and ierr set if values do not match & icheck = ps_error

  ! if an array is not allocated in (ss3):
  !    no action taken, regardless of allocation status in (ss1) or (ss2).

  ! if an array is allocated in (ss3) but in neither (ss1) nor (ss2):
  !    no action taken.

  ! if an array element is allocated in all of {(ss1),(ss2),(ss3)}:
  !    all must be conformal in shape; for grids, (ss1) & (ss2) values
  !    must match as well.
  !       for floating point in state_data or state_profile sections:
  !           ss3 <-- w1*ss1 + (1-w1)*ss2 formula ... is applied.
  !       for everything else: ss1 values copied to ss3.

  ! for quantities associated with species lists, values must match...

  ! if an array element is allocated in {(ss1),(ss3)}, not (ss2):
  !    (ss1) values are copied to (ss3); all arrays must be conformal in shape.

  ! if an array element is allocated in {(ss2),(ss3)}, not (ss1):
  !    (ss2) values are copied to (ss3); all arrays must be conformal in shape.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components to merge
                                ! use ps_cc_all to merge all components

  REAL(KIND=rspec) :: w1        ! weight on ss1
  TYPE (plasma_state) :: ss1    ! state data to copy *from* with weight (w1)
  TYPE (plasma_state) :: ss2    ! state data to copy *from* with weight (1-w1)
  TYPE (plasma_state) :: ss3    ! state to which data is copied.
  INTEGER, INTENT(IN) :: icheck ! control for checking match of non-floating-
  !  point data: ps_ignore, ps_warn, or ps_error on mismatch.

  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------
  INTEGER :: iout,isize
  INTEGER :: ilock_mdescr,ilock_sconfig,ilock_siminit,iertmp
  !-------------------

  iout = SWIM_err_out_file
  ierr = 0

  call plasma_state_merge_0(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)
  call plasma_state_merge_1(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)
  call plasma_state_merge_2(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)
  call plasma_state_merge_3(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)
  call plasma_state_merge_4(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)
  call plasma_state_merge_5(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)
  call plasma_state_merge_6(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)
  call plasma_state_merge_7(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)
  call plasma_state_merge_8(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)
  call plasma_state_merge_9(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)

  ! set hash codes of new copy of state

  ilock_mdescr = ss3%lock_machine_descr
  ilock_sconfig = ss3%lock_shot_config
  ilock_siminit = ss3%lock_sim_init

  ss3%lock_machine_descr = ps_unlocked
  ss3%lock_shot_config = ps_unlocked
  ss3%lock_sim_init = ps_unlocked

  CALL plasma_state_hash_code(ss3,ps_cc_all,iertmp)
  ss3%hash_codes = ss3%new_hash_codes

  ss3%lock_machine_descr = ilock_mdescr
  ss3%lock_shot_config = ilock_sconfig
  ss3%lock_sim_init = ilock_siminit

END SUBROUTINE plasma_state_merge
