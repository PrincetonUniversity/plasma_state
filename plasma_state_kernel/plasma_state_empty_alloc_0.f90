! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_empty_alloc_0(ss,iout,ierr)


  ! Allocate array data for which all dimensions are provided

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss  ! state containing array elements to be allocated
  INTEGER, INTENT(IN) :: iout
  INTEGER, INTENT(INOUT) :: ierr  ! completion code (not cleared,set on error)

  !-------------------
  INTEGER :: isize,jsize
  !-------------------


  !-----------------------------------------------
  ! allocate arrays...

  isize = (ss%nspec_alla+1)
  IF(ss%nspec_alla.LE.0) isize = 0
  IF(ALLOCATED(ss%alla_index).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%alla_index)
  ENDIF
  IF(ALLOCATED(ss%alla_index)) then
     jsize = SIZE(ss%alla_index)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: alla_index already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%alla_index(0:ss%nspec_alla))
     ss%alla_index = 0
  ENDIF

  isize = (ss%nspec_alla+1)
  IF(ss%nspec_alla.LE.0) isize = 0
  IF(ALLOCATED(ss%ALLA_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ALLA_name)
  ENDIF
  IF(ALLOCATED(ss%ALLA_name)) then
     jsize = SIZE(ss%ALLA_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ALLA_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ALLA_name(0:ss%nspec_alla))
     ss%ALLA_name = " "
  ENDIF

  isize = (ss%nspec_alla+1)
  IF(ss%nspec_alla.LE.0) isize = 0
  IF(ALLOCATED(ss%ALLA_type).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ALLA_type)
  ENDIF
  IF(ALLOCATED(ss%ALLA_type)) then
     jsize = SIZE(ss%ALLA_type)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ALLA_type already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ALLA_type(0:ss%nspec_alla))
     ss%ALLA_type = 0
  ENDIF

  isize = (ss%nspec_all+1)
  IF(ss%nspec_all.LE.0) isize = 0
  IF(ALLOCATED(ss%all_index).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%all_index)
  ENDIF
  IF(ALLOCATED(ss%all_index)) then
     jsize = SIZE(ss%all_index)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: all_index already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%all_index(0:ss%nspec_all))
     ss%all_index = 0
  ENDIF

  isize = (ss%nspec_all+1)
  IF(ss%nspec_all.LE.0) isize = 0
  IF(ALLOCATED(ss%ALL_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ALL_name)
  ENDIF
  IF(ALLOCATED(ss%ALL_name)) then
     jsize = SIZE(ss%ALL_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ALL_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ALL_name(0:ss%nspec_all))
     ss%ALL_name = " "
  ENDIF

  isize = (ss%nspec_all+1)
  IF(ss%nspec_all.LE.0) isize = 0
  IF(ALLOCATED(ss%ALL_type).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ALL_type)
  ENDIF
  IF(ALLOCATED(ss%ALL_type)) then
     jsize = SIZE(ss%ALL_type)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ALL_type already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ALL_type(0:ss%nspec_all))
     ss%ALL_type = 0
  ENDIF

  isize = (ss%max_nspectrumlh_src)*(ss%nlhrf_src)
  IF(ALLOCATED(ss%amaxlh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%amaxlh)
  ENDIF
  IF(ALLOCATED(ss%amaxlh)) then
     jsize = SIZE(ss%amaxlh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: amaxlh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%amaxlh(ss%max_nspectrumlh_src,ss%nlhrf_src))
     ss%amaxlh = 0.0_rspec
  ENDIF

  isize = (ss%max_nspectrumlh_src)*(ss%nlhrf_src)
  IF(ALLOCATED(ss%aminlh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%aminlh)
  ENDIF
  IF(ALLOCATED(ss%aminlh)) then
     jsize = SIZE(ss%aminlh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: aminlh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%aminlh(ss%max_nspectrumlh_src,ss%nlhrf_src))
     ss%aminlh = 0.0_rspec
  ENDIF

  isize = (ss%ncoils)
  IF(ALLOCATED(ss%Ang_Href_coil).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Ang_Href_coil)
  ENDIF
  IF(ALLOCATED(ss%Ang_Href_coil)) then
     jsize = SIZE(ss%Ang_Href_coil)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Ang_Href_coil already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Ang_Href_coil(ss%ncoils))
     ss%Ang_Href_coil = 0.0_rspec
  ENDIF

  isize = (ss%ncoils)
  IF(ALLOCATED(ss%Ang_Vref_coil).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Ang_Vref_coil)
  ENDIF
  IF(ALLOCATED(ss%Ang_Vref_coil)) then
     jsize = SIZE(ss%Ang_Vref_coil)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Ang_Vref_coil already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Ang_Vref_coil(ss%ncoils))
     ss%Ang_Vref_coil = 0.0_rspec
  ENDIF

  isize = (ss%nefi_anom)
  IF(ALLOCATED(ss%anom_evar).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%anom_evar)
  ENDIF
  IF(ALLOCATED(ss%anom_evar)) then
     jsize = SIZE(ss%anom_evar)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: anom_evar already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%anom_evar(ss%nefi_anom))
     ss%anom_evar = 0.0_rspec
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%ant_model).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ant_model)
  ENDIF
  IF(ALLOCATED(ss%ant_model)) then
     jsize = SIZE(ss%ant_model)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ant_model already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ant_model(ss%nicrf_src))
     ss%ant_model = " "
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%ap2_halfheight).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ap2_halfheight)
  ENDIF
  IF(ALLOCATED(ss%ap2_halfheight)) then
     jsize = SIZE(ss%ap2_halfheight)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ap2_halfheight already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ap2_halfheight(ss%nbeam))
     ss%ap2_halfheight = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%ap2_halfwidth).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ap2_halfwidth)
  ENDIF
  IF(ALLOCATED(ss%ap2_halfwidth)) then
     jsize = SIZE(ss%ap2_halfwidth)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ap2_halfwidth already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ap2_halfwidth(ss%nbeam))
     ss%ap2_halfwidth = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%ap2_horiz_offset).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ap2_horiz_offset)
  ENDIF
  IF(ALLOCATED(ss%ap2_horiz_offset)) then
     jsize = SIZE(ss%ap2_horiz_offset)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ap2_horiz_offset already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ap2_horiz_offset(ss%nbeam))
     ss%ap2_horiz_offset = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%ap2_vert_offset).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ap2_vert_offset)
  ENDIF
  IF(ALLOCATED(ss%ap2_vert_offset)) then
     jsize = SIZE(ss%ap2_vert_offset)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ap2_vert_offset already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ap2_vert_offset(ss%nbeam))
     ss%ap2_vert_offset = 0.0_rspec
  ENDIF

  isize = (ss%max_nspectrumlh_src)*(ss%nlhrf_src)
  IF(ALLOCATED(ss%apolmaxlh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%apolmaxlh)
  ENDIF
  IF(ALLOCATED(ss%apolmaxlh)) then
     jsize = SIZE(ss%apolmaxlh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: apolmaxlh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%apolmaxlh(ss%max_nspectrumlh_src,ss%nlhrf_src))
     ss%apolmaxlh = 0.0_rspec
  ENDIF

  isize = (ss%max_nspectrumlh_src)*(ss%nlhrf_src)
  IF(ALLOCATED(ss%apolminlh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%apolminlh)
  ENDIF
  IF(ALLOCATED(ss%apolminlh)) then
     jsize = SIZE(ss%apolminlh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: apolminlh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%apolminlh(ss%max_nspectrumlh_src,ss%nlhrf_src))
     ss%apolminlh = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%ap_halfheight).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ap_halfheight)
  ENDIF
  IF(ALLOCATED(ss%ap_halfheight)) then
     jsize = SIZE(ss%ap_halfheight)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ap_halfheight already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ap_halfheight(ss%nbeam))
     ss%ap_halfheight = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%ap_halfwidth).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ap_halfwidth)
  ENDIF
  IF(ALLOCATED(ss%ap_halfwidth)) then
     jsize = SIZE(ss%ap_halfwidth)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ap_halfwidth already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ap_halfwidth(ss%nbeam))
     ss%ap_halfwidth = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%ap_horiz_offset).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ap_horiz_offset)
  ENDIF
  IF(ALLOCATED(ss%ap_horiz_offset)) then
     jsize = SIZE(ss%ap_horiz_offset)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ap_horiz_offset already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ap_horiz_offset(ss%nbeam))
     ss%ap_horiz_offset = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%ap_vert_offset).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ap_vert_offset)
  ENDIF
  IF(ALLOCATED(ss%ap_vert_offset)) then
     jsize = SIZE(ss%ap_vert_offset)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ap_vert_offset already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ap_vert_offset(ss%nbeam))
     ss%ap_vert_offset = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq)
  IF(ALLOCATED(ss%area).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%area)
  ENDIF
  IF(ALLOCATED(ss%area)) then
     jsize = SIZE(ss%area)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: area already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%area(ss%nrho_eq))
     ss%area = 0.0_rspec
  ENDIF

  isize = (ss%max_nspectrumlh_src)*(ss%nlhrf_src)
  IF(ALLOCATED(ss%atormaxlh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%atormaxlh)
  ENDIF
  IF(ALLOCATED(ss%atormaxlh)) then
     jsize = SIZE(ss%atormaxlh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: atormaxlh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%atormaxlh(ss%max_nspectrumlh_src,ss%nlhrf_src))
     ss%atormaxlh = 0.0_rspec
  ENDIF

  isize = (ss%max_nspectrumlh_src)*(ss%nlhrf_src)
  IF(ALLOCATED(ss%atorminlh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%atorminlh)
  ENDIF
  IF(ALLOCATED(ss%atorminlh)) then
     jsize = SIZE(ss%atorminlh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: atorminlh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%atorminlh(ss%max_nspectrumlh_src,ss%nlhrf_src))
     ss%atorminlh = 0.0_rspec
  ENDIF

  isize = (ss%nballoon)
  IF(ALLOCATED(ss%Balloon_code).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Balloon_code)
  ENDIF
  IF(ALLOCATED(ss%Balloon_code)) then
     jsize = SIZE(ss%Balloon_code)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Balloon_code already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Balloon_code(ss%nballoon))
     ss%Balloon_code = " "
  ENDIF

  isize = (ss%nballoon)
  IF(ALLOCATED(ss%Balloon_mapper).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Balloon_mapper)
  ENDIF
  IF(ALLOCATED(ss%Balloon_mapper)) then
     jsize = SIZE(ss%Balloon_mapper)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Balloon_mapper already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Balloon_mapper(ss%nballoon))
     ss%Balloon_mapper = " "
  ENDIF

  isize = (ss%nballoon)
  IF(ALLOCATED(ss%Balloon_stable).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Balloon_stable)
  ENDIF
  IF(ALLOCATED(ss%Balloon_stable)) then
     jsize = SIZE(ss%Balloon_stable)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Balloon_stable already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Balloon_stable(ss%nballoon))
     ss%Balloon_stable = 0
  ENDIF

  isize = (ss%nballoon)
  IF(ALLOCATED(ss%Balloon_status).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Balloon_status)
  ENDIF
  IF(ALLOCATED(ss%Balloon_status)) then
     jsize = SIZE(ss%Balloon_status)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Balloon_status already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Balloon_status(ss%nballoon))
     ss%Balloon_status = 0
  ENDIF

  isize = (ss%nrho_lmhd)
  IF(ALLOCATED(ss%balstab).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%balstab)
  ENDIF
  IF(ALLOCATED(ss%balstab)) then
     jsize = SIZE(ss%balstab)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: balstab already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%balstab(ss%nrho_lmhd))
     ss%balstab = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%beam_type).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%beam_type)
  ENDIF
  IF(ALLOCATED(ss%beam_type)) then
     jsize = SIZE(ss%beam_type)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: beam_type already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%beam_type(ss%nbeam))
     ss%beam_type = " "
  ENDIF

  isize = (ss%nefi_anom2)*(ss%nrho_anom2)*(ss%nspec_beam)
  IF(ALLOCATED(ss%bidiff_bpass_co).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%bidiff_bpass_co)
  ENDIF
  IF(ALLOCATED(ss%bidiff_bpass_co)) then
     jsize = SIZE(ss%bidiff_bpass_co)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: bidiff_bpass_co already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%bidiff_bpass_co(ss%nefi_anom2,ss%nrho_anom2,ss%nspec_beam))
     ss%bidiff_bpass_co = 0.0_rspec
     ALLOCATE(ss%id_bidiff_bpass_co(ss%nspec_beam))
     ss%id_bidiff_bpass_co = 0
  ENDIF

  isize = (ss%nefi_anom2)*(ss%nrho_anom2)*(ss%nspec_beam)
  IF(ALLOCATED(ss%bidiff_bpass_ctr).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%bidiff_bpass_ctr)
  ENDIF
  IF(ALLOCATED(ss%bidiff_bpass_ctr)) then
     jsize = SIZE(ss%bidiff_bpass_ctr)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: bidiff_bpass_ctr already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%bidiff_bpass_ctr(ss%nefi_anom2,ss%nrho_anom2,ss%nspec_beam))
     ss%bidiff_bpass_ctr = 0.0_rspec
     ALLOCATE(ss%id_bidiff_bpass_ctr(ss%nspec_beam))
     ss%id_bidiff_bpass_ctr = 0
  ENDIF

  isize = (ss%nefi_anom2)*(ss%nrho_anom2)*(ss%nspec_beam)
  IF(ALLOCATED(ss%bidiff_btrap).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%bidiff_btrap)
  ENDIF
  IF(ALLOCATED(ss%bidiff_btrap)) then
     jsize = SIZE(ss%bidiff_btrap)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: bidiff_btrap already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%bidiff_btrap(ss%nefi_anom2,ss%nrho_anom2,ss%nspec_beam))
     ss%bidiff_btrap = 0.0_rspec
     ALLOCATE(ss%id_bidiff_btrap(ss%nspec_beam))
     ss%id_bidiff_btrap = 0
  ENDIF

  isize = (ss%nefi_anom2)*(ss%nrho_anom2)*(ss%nspec_beam)
  IF(ALLOCATED(ss%bidiff_dpass_co).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%bidiff_dpass_co)
  ENDIF
  IF(ALLOCATED(ss%bidiff_dpass_co)) then
     jsize = SIZE(ss%bidiff_dpass_co)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: bidiff_dpass_co already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%bidiff_dpass_co(ss%nefi_anom2,ss%nrho_anom2,ss%nspec_beam))
     ss%bidiff_dpass_co = 0.0_rspec
     ALLOCATE(ss%id_bidiff_dpass_co(ss%nspec_beam))
     ss%id_bidiff_dpass_co = 0
  ENDIF

  isize = (ss%nefi_anom2)*(ss%nrho_anom2)*(ss%nspec_beam)
  IF(ALLOCATED(ss%bidiff_dpass_ctr).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%bidiff_dpass_ctr)
  ENDIF
  IF(ALLOCATED(ss%bidiff_dpass_ctr)) then
     jsize = SIZE(ss%bidiff_dpass_ctr)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: bidiff_dpass_ctr already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%bidiff_dpass_ctr(ss%nefi_anom2,ss%nrho_anom2,ss%nspec_beam))
     ss%bidiff_dpass_ctr = 0.0_rspec
     ALLOCATE(ss%id_bidiff_dpass_ctr(ss%nspec_beam))
     ss%id_bidiff_dpass_ctr = 0
  ENDIF

  isize = (ss%nefi_anom2)*(ss%nrho_anom2)*(ss%nspec_beam)
  IF(ALLOCATED(ss%bidiff_dtrap).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%bidiff_dtrap)
  ENDIF
  IF(ALLOCATED(ss%bidiff_dtrap)) then
     jsize = SIZE(ss%bidiff_dtrap)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: bidiff_dtrap already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%bidiff_dtrap(ss%nefi_anom2,ss%nrho_anom2,ss%nspec_beam))
     ss%bidiff_dtrap = 0.0_rspec
     ALLOCATE(ss%id_bidiff_dtrap(ss%nspec_beam))
     ss%id_bidiff_dtrap = 0
  ENDIF

  isize = (ss%nr)*(ss%nz)
  IF(ALLOCATED(ss%BphiRZ).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%BphiRZ)
  ENDIF
  IF(ALLOCATED(ss%BphiRZ)) then
     jsize = SIZE(ss%BphiRZ)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: BphiRZ already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%BphiRZ(ss%nr,ss%nz))
     ss%BphiRZ = 0.0_rspec
  ENDIF

  isize = (ss%nr)*(ss%nz)
  IF(ALLOCATED(ss%BRRZ).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%BRRZ)
  ENDIF
  IF(ALLOCATED(ss%BRRZ)) then
     jsize = SIZE(ss%BRRZ)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: BRRZ already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%BRRZ(ss%nr,ss%nz))
     ss%BRRZ = 0.0_rspec
  ENDIF

  isize = (ss%nr)*(ss%nz)
  IF(ALLOCATED(ss%BZRZ).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%BZRZ)
  ENDIF
  IF(ALLOCATED(ss%BZRZ)) then
     jsize = SIZE(ss%BZRZ)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: BZRZ already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%BZRZ(ss%nr,ss%nz))
     ss%BZRZ = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%b_halfHeight).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%b_halfHeight)
  ENDIF
  IF(ALLOCATED(ss%b_halfHeight)) then
     jsize = SIZE(ss%b_halfHeight)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: b_halfHeight already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%b_halfHeight(ss%nbeam))
     ss%b_halfHeight = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%b_halfwidth).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%b_halfwidth)
  ENDIF
  IF(ALLOCATED(ss%b_halfwidth)) then
     jsize = SIZE(ss%b_halfwidth)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: b_halfwidth already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%b_halfwidth(ss%nbeam))
     ss%b_halfwidth = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%b_Hdivergence).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%b_Hdivergence)
  ENDIF
  IF(ALLOCATED(ss%b_Hdivergence)) then
     jsize = SIZE(ss%b_Hdivergence)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: b_Hdivergence already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%b_Hdivergence(ss%nbeam))
     ss%b_Hdivergence = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%b_Hfocal_length).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%b_Hfocal_length)
  ENDIF
  IF(ALLOCATED(ss%b_Hfocal_length)) then
     jsize = SIZE(ss%b_Hfocal_length)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: b_Hfocal_length already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%b_Hfocal_length(ss%nbeam))
     ss%b_Hfocal_length = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%B_surfMax).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%B_surfMax)
  ENDIF
  IF(ALLOCATED(ss%B_surfMax)) then
     jsize = SIZE(ss%B_surfMax)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: B_surfMax already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%B_surfMax(ss%nrho_eq_geo))
     ss%B_surfMax = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%B_surfMin).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%B_surfMin)
  ENDIF
  IF(ALLOCATED(ss%B_surfMin)) then
     jsize = SIZE(ss%B_surfMin)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: B_surfMin already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%B_surfMin(ss%nrho_eq_geo))
     ss%B_surfMin = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%b_Vdivergence).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%b_Vdivergence)
  ENDIF
  IF(ALLOCATED(ss%b_Vdivergence)) then
     jsize = SIZE(ss%b_Vdivergence)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: b_Vdivergence already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%b_Vdivergence(ss%nbeam))
     ss%b_Vdivergence = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%b_Vfocal_length).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%b_Vfocal_length)
  ENDIF
  IF(ALLOCATED(ss%b_Vfocal_length)) then
     jsize = SIZE(ss%b_Vfocal_length)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: b_Vfocal_length already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%b_Vfocal_length(ss%nbeam))
     ss%b_Vfocal_length = 0.0_rspec
  ENDIF

  isize = (ss%nrho_icrf-1)*(ss%nicrf_src)
  IF(ALLOCATED(ss%cdicrf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%cdicrf)
  ENDIF
  IF(ALLOCATED(ss%cdicrf)) then
     jsize = SIZE(ss%cdicrf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: cdicrf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%cdicrf(ss%nrho_icrf-1,ss%nicrf_src))
     ss%cdicrf = 0.0_rspec
     ALLOCATE(ss%id_cdicrf(ss%nicrf_src))
     ss%id_cdicrf = 0
  ENDIF

  isize = (ss%nrho_icrf-1)*(ss%max_num_nphi)*(ss%nicrf_src)
  IF(ALLOCATED(ss%cdicrf_nphi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%cdicrf_nphi)
  ENDIF
  IF(ALLOCATED(ss%cdicrf_nphi)) then
     jsize = SIZE(ss%cdicrf_nphi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: cdicrf_nphi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%cdicrf_nphi(ss%nrho_icrf-1,ss%max_num_nphi,ss%nicrf_src))
     ss%cdicrf_nphi = 0.0_rspec
     ALLOCATE(ss%id_cdicrf_nphi(ss%max_num_nphi,ss%nicrf_src))
     ss%id_cdicrf_nphi = 0
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chie_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_anom)
  ENDIF
  IF(ALLOCATED(ss%chie_anom)) then
     jsize = SIZE(ss%chie_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_anom(ss%nrho_anomth))
     ss%chie_anom = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chie_drbm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_drbm)
  ENDIF
  IF(ALLOCATED(ss%chie_drbm)) then
     jsize = SIZE(ss%chie_drbm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_drbm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_drbm(ss%nrho_anomth))
     ss%chie_drbm = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chie_etg).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_etg)
  ENDIF
  IF(ALLOCATED(ss%chie_etg)) then
     jsize = SIZE(ss%chie_etg)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_etg already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_etg(ss%nrho_anomth))
     ss%chie_etg = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chie_glf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_glf)
  ENDIF
  IF(ALLOCATED(ss%chie_glf)) then
     jsize = SIZE(ss%chie_glf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_glf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_glf(ss%nrho_anomth))
     ss%chie_glf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chie_gtcneo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_gtcneo)
  ENDIF
  IF(ALLOCATED(ss%chie_gtcneo)) then
     jsize = SIZE(ss%chie_gtcneo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_gtcneo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_gtcneo(ss%nrho_anomth))
     ss%chie_gtcneo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chie_mmm71).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_mmm71)
  ENDIF
  IF(ALLOCATED(ss%chie_mmm71)) then
     jsize = SIZE(ss%chie_mmm71)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_mmm71 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_mmm71(ss%nrho_anomth))
     ss%chie_mmm71 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chie_mtm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_mtm)
  ENDIF
  IF(ALLOCATED(ss%chie_mtm)) then
     jsize = SIZE(ss%chie_mtm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_mtm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_mtm(ss%nrho_anomth))
     ss%chie_mtm = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chie_nclass).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_nclass)
  ENDIF
  IF(ALLOCATED(ss%chie_nclass)) then
     jsize = SIZE(ss%chie_nclass)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_nclass already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_nclass(ss%nrho_anomth))
     ss%chie_nclass = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chie_neo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_neo)
  ENDIF
  IF(ALLOCATED(ss%chie_neo)) then
     jsize = SIZE(ss%chie_neo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_neo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_neo(ss%nrho_anomth))
     ss%chie_neo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chie_ntm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_ntm)
  ENDIF
  IF(ALLOCATED(ss%chie_ntm)) then
     jsize = SIZE(ss%chie_ntm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_ntm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_ntm(ss%nrho_anomth))
     ss%chie_ntm = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chie_paleo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_paleo)
  ENDIF
  IF(ALLOCATED(ss%chie_paleo)) then
     jsize = SIZE(ss%chie_paleo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_paleo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_paleo(ss%nrho_anomth))
     ss%chie_paleo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chie_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_tglf)
  ENDIF
  IF(ALLOCATED(ss%chie_tglf)) then
     jsize = SIZE(ss%chie_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_tglf(ss%nrho_anomth))
     ss%chie_tglf = 0.0_rspec
  ENDIF

  isize = (ss%nrho)
  IF(ALLOCATED(ss%chie_trans).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_trans)
  ENDIF
  IF(ALLOCATED(ss%chie_trans)) then
     jsize = SIZE(ss%chie_trans)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_trans already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_trans(ss%nrho))
     ss%chie_trans = 0.0_rspec
  ENDIF

  isize = (ss%nrho)*(ss%ntrcat)
  IF(ALLOCATED(ss%chie_transcat).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_transcat)
  ENDIF
  IF(ALLOCATED(ss%chie_transcat)) then
     jsize = SIZE(ss%chie_transcat)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_transcat already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_transcat(ss%nrho,ss%ntrcat))
     ss%chie_transcat = 0.0_rspec
     ALLOCATE(ss%id_chie_transcat(ss%ntrcat))
     ss%id_chie_transcat = 0
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chie_w19).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chie_w19)
  ENDIF
  IF(ALLOCATED(ss%chie_w19)) then
     jsize = SIZE(ss%chie_w19)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chie_w19 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chie_w19(ss%nrho_anomth))
     ss%chie_w19 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chii_anom).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chii_anom)
  ENDIF
  IF(ALLOCATED(ss%chii_anom)) then
     jsize = SIZE(ss%chii_anom)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chii_anom already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chii_anom(ss%nrho_anomth))
     ss%chii_anom = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chii_drbm).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chii_drbm)
  ENDIF
  IF(ALLOCATED(ss%chii_drbm)) then
     jsize = SIZE(ss%chii_drbm)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chii_drbm already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chii_drbm(ss%nrho_anomth))
     ss%chii_drbm = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chii_glf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chii_glf)
  ENDIF
  IF(ALLOCATED(ss%chii_glf)) then
     jsize = SIZE(ss%chii_glf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chii_glf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chii_glf(ss%nrho_anomth))
     ss%chii_glf = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chii_gtcneo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chii_gtcneo)
  ENDIF
  IF(ALLOCATED(ss%chii_gtcneo)) then
     jsize = SIZE(ss%chii_gtcneo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chii_gtcneo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chii_gtcneo(ss%nrho_anomth))
     ss%chii_gtcneo = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chii_mmm71).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chii_mmm71)
  ENDIF
  IF(ALLOCATED(ss%chii_mmm71)) then
     jsize = SIZE(ss%chii_mmm71)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chii_mmm71 already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chii_mmm71(ss%nrho_anomth))
     ss%chii_mmm71 = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chii_nclass).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chii_nclass)
  ENDIF
  IF(ALLOCATED(ss%chii_nclass)) then
     jsize = SIZE(ss%chii_nclass)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chii_nclass already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chii_nclass(ss%nrho_anomth))
     ss%chii_nclass = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)
  IF(ALLOCATED(ss%chii_neo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%chii_neo)
  ENDIF
  IF(ALLOCATED(ss%chii_neo)) then
     jsize = SIZE(ss%chii_neo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: chii_neo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%chii_neo(ss%nrho_anomth))
     ss%chii_neo = 0.0_rspec
  ENDIF

END SUBROUTINE plasma_state_empty_alloc_0
