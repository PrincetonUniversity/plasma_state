! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_empty_alloc(ss,ierr)

  ! incremental allocation
  ! allocate arrays not previously allocated -- only done at initialization.
  !   initialization steps:
  !      0.  declaration of state gives array dimensions values of zero.
  !      1.  set some array dimensions.
  !      2.  call this routine via PS_alloc_state(ierr); arrays with non-
  !          zero dimensioning are allocated.
  !          --ERROR-- if arrays were previously allocated with different
  !          dimension sizes.
  !      3.  based on further calculations or data access, set some more
  !          array dimensions; call this routine again to allocate all
  !          arrays associated with these additional, newly specified
  !          dimensions.  Do not change dimensions already set.  Repeat
  !          until entire structure is allocated and filled in.
  !
  ! It is an error to try to change the size of an already-allocated array.
  !

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss    ! state copy to receive empty allocation
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------
  INTEGER :: iout,isize,jsize
  !-------------------

  iout = SWIM_err_out_file

  ierr = 0
  !==============================
  ! update enumeration dimension maxima

  IF(ALLOCATED(ss%nrz_antgeo)) THEN
     ss%max_nrz_antgeo = max(ss%max_nrz_antgeo,maxval(ss%nrz_antgeo))
  ENDIF

  IF(ALLOCATED(ss%nspectrumlh_src)) THEN
     ss%max_nspectrumlh_src = max(ss%max_nspectrumlh_src,maxval(ss%nspectrumlh_src))
  ENDIF

  IF(ALLOCATED(ss%num_nphi)) THEN
     ss%max_num_nphi = max(ss%max_num_nphi,maxval(ss%num_nphi))
  ENDIF

  IF(ALLOCATED(ss%num_nphi_vac)) THEN
     ss%max_num_nphi_vac = max(ss%max_num_nphi_vac,maxval(ss%num_nphi_vac))
  ENDIF

  IF(ALLOCATED(ss%n_straps)) THEN
     ss%max_n_straps = max(ss%max_n_straps,maxval(ss%n_straps))
  ENDIF

  !==============================
  ! count dimensions with non-zero values (by component)

  ss%active_dims = 0

  ! count the number of active array dimensions...

  ! component: ANOM:
  IF(ss%nefi_anom.GT.0) ss%active_dims(1) = ss%active_dims(1) + 1
  IF(ss%nefi_anom2.GT.0) ss%active_dims(1) = ss%active_dims(1) + 1
  IF(ss%nky_anomth.GT.0) ss%active_dims(1) = ss%active_dims(1) + 1
  IF(ss%nky_mmm.GT.0) ss%active_dims(1) = ss%active_dims(1) + 1
  IF(ss%nrho_anom.GT.0) ss%active_dims(1) = ss%active_dims(1) + 1
  IF(ss%nrho_anom2.GT.0) ss%active_dims(1) = ss%active_dims(1) + 1
  IF(ss%nrho_anomth.GT.0) ss%active_dims(1) = ss%active_dims(1) + 1

  ! component: EC:
  IF(ss%necrf_src.GT.0) ss%active_dims(2) = ss%active_dims(2) + 1
  IF(ss%nrho_ecrf.GT.0) ss%active_dims(2) = ss%active_dims(2) + 1

  ! component: EQ:
  IF(ss%ncircuits.GT.0) ss%active_dims(3) = ss%active_dims(3) + 1
  IF(ss%ncoils.GT.0) ss%active_dims(3) = ss%active_dims(3) + 1
  IF(ss%neqmom.GT.0) ss%active_dims(3) = ss%active_dims(3) + 1
  IF(ss%npsmom.GT.0) ss%active_dims(3) = ss%active_dims(3) + 1
  IF(ss%nr.GT.0) ss%active_dims(3) = ss%active_dims(3) + 1
  IF(ss%nrho_eq.GT.0) ss%active_dims(3) = ss%active_dims(3) + 1
  IF(ss%nrho_eq_geo.GT.0) ss%active_dims(3) = ss%active_dims(3) + 1
  IF(ss%nth_eq.GT.0) ss%active_dims(3) = ss%active_dims(3) + 1
  IF(ss%num_rzlim.GT.0) ss%active_dims(3) = ss%active_dims(3) + 1
  IF(ss%nz.GT.0) ss%active_dims(3) = ss%active_dims(3) + 1

  ! component: FUS:
  IF(ss%nrho_fus.GT.0) ss%active_dims(4) = ss%active_dims(4) + 1
  IF(ss%nspec_fusion.GT.0) ss%active_dims(4) = ss%active_dims(4) + 1

  ! component: GAS:
  IF(ss%ngsc0.GT.0) ss%active_dims(5) = ss%active_dims(5) + 1
  IF(ss%nrho_gas.GT.0) ss%active_dims(5) = ss%active_dims(5) + 1
  IF(ss%nspec_gas.GT.0) ss%active_dims(5) = ss%active_dims(5) + 1
  IF(ss%nspec_imp0.GT.0) ss%active_dims(5) = ss%active_dims(5) + 1

  ! component: IC:
  IF(ss%max_nrz_antgeo.GT.0) ss%active_dims(6) = ss%active_dims(6) + 1
  IF(ss%max_num_nphi.GT.0) ss%active_dims(6) = ss%active_dims(6) + 1
  IF(ss%max_num_nphi_vac.GT.0) ss%active_dims(6) = ss%active_dims(6) + 1
  IF(ss%max_n_straps.GT.0) ss%active_dims(6) = ss%active_dims(6) + 1
  IF(ss%nicrf_src.GT.0) ss%active_dims(6) = ss%active_dims(6) + 1
  IF(ss%nrho_icrf.GT.0) ss%active_dims(6) = ss%active_dims(6) + 1
  IF(ss%nspec_rfmin.GT.0) ss%active_dims(6) = ss%active_dims(6) + 1

  ! component: LH:
  IF(ss%max_nspectrumlh_src.GT.0) ss%active_dims(7) = ss%active_dims(7) + 1
  IF(ss%nlhrf_src.GT.0) ss%active_dims(7) = ss%active_dims(7) + 1
  IF(ss%nrho_lhrf.GT.0) ss%active_dims(7) = ss%active_dims(7) + 1

  ! component: LMHD:
  IF(ss%mmodes.GT.0) ss%active_dims(8) = ss%active_dims(8) + 1
  IF(ss%nballoon.GT.0) ss%active_dims(8) = ss%active_dims(8) + 1
  IF(ss%ndelta_w.GT.0) ss%active_dims(8) = ss%active_dims(8) + 1
  IF(ss%nnewcomb.GT.0) ss%active_dims(8) = ss%active_dims(8) + 1
  IF(ss%nrho_lmhd.GT.0) ss%active_dims(8) = ss%active_dims(8) + 1

  ! component: NBI:
  IF(ss%nbeam.GT.0) ss%active_dims(9) = ss%active_dims(9) + 1
  IF(ss%nrho_nbi.GT.0) ss%active_dims(9) = ss%active_dims(9) + 1
  IF(ss%nspec_beam.GT.0) ss%active_dims(9) = ss%active_dims(9) + 1
  IF(ss%n_D_einj_standard.GT.0) ss%active_dims(9) = ss%active_dims(9) + 1
  IF(ss%n_H_einj_standard.GT.0) ss%active_dims(9) = ss%active_dims(9) + 1
  IF(ss%n_T_einj_standard.GT.0) ss%active_dims(9) = ss%active_dims(9) + 1

  ! component: PEL:
  IF(ss%nrho_pel.GT.0) ss%active_dims(10) = ss%active_dims(10) + 1

  ! component: PLASMA:
  IF(ss%nrho.GT.0) ss%active_dims(11) = ss%active_dims(11) + 1
  IF(ss%nspec_all.GT.0) ss%active_dims(11) = ss%active_dims(11) + 1
  IF(ss%nspec_alla.GT.0) ss%active_dims(11) = ss%active_dims(11) + 1
  IF(ss%nspec_impi.GT.0) ss%active_dims(11) = ss%active_dims(11) + 1
  IF(ss%nspec_th.GT.0) ss%active_dims(11) = ss%active_dims(11) + 1
  IF(ss%nspec_tha.GT.0) ss%active_dims(11) = ss%active_dims(11) + 1
  IF(ss%ntrcat.GT.0) ss%active_dims(11) = ss%active_dims(11) + 1

  ! component: RAD:
  IF(ss%nrho_mist.GT.0) ss%active_dims(12) = ss%active_dims(12) + 1
  IF(ss%nrho_rad.GT.0) ss%active_dims(12) = ss%active_dims(12) + 1
  IF(ss%nxs.GT.0) ss%active_dims(12) = ss%active_dims(12) + 1

  ! component: RIPPLE:
  IF(ss%nr_rip.GT.0) ss%active_dims(13) = ss%active_dims(13) + 1
  IF(ss%nz_rip.GT.0) ss%active_dims(13) = ss%active_dims(13) + 1

  ! component: RUNAWAY:
  IF(ss%nrho_rw.GT.0) ss%active_dims(14) = ss%active_dims(14) + 1

  !==============================
  ! call routines to allocate arrays with non-zero dimensions
  call plasma_state_empty_alloc_0(ss,iout,ierr)
  call plasma_state_empty_alloc_1(ss,iout,ierr)
  call plasma_state_empty_alloc_2(ss,iout,ierr)
  call plasma_state_empty_alloc_3(ss,iout,ierr)
  call plasma_state_empty_alloc_4(ss,iout,ierr)
  call plasma_state_empty_alloc_5(ss,iout,ierr)
  call plasma_state_empty_alloc_6(ss,iout,ierr)
  call plasma_state_empty_alloc_7(ss,iout,ierr)
  call plasma_state_empty_alloc_8(ss,iout,ierr)
  call plasma_state_empty_alloc_9(ss,iout,ierr)


END SUBROUTINE plasma_state_empty_alloc
