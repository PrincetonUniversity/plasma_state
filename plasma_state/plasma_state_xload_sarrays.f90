
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_sarrays(ss,xhash,iout,ierr)

  ! called from plasma_state_xload: check grids; store in xplasma structure

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:

  INTEGER, PARAMETER :: max_itemlist = ps_max_static

  INTEGER :: icomponent,imod,nitems,isize
  CHARACTER*32 :: listname,units,enum_name
  CHARACTER*80 :: label
  INTEGER :: ahash(ps_max_static)

  CHARACTER*32 :: item_names(max_itemlist)
  INTEGER :: item_ivals(max_itemlist)
  REAL(KIND=rspec) :: item_r8vals(max_itemlist)
  CHARACTER*256 :: item_c256(max_itemlist)

  INTEGER :: i1,i2,i3,i4
  CHARACTER*32 :: nam1,nam2,nam3,nam4
  CHARACTER*80 :: ilab1,ilab2,ilab3,ilab4

  !-----------------------

  ahash(1) = ps_shash_alla_index
  !------------------------
  IF(ALLOCATED(ss%alla_index).AND.hcheck(1,1)) THEN
     ! I alla_index(0:nspec_alla)
     listname = "ALLA_INDEX"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "species index in original lists"
     nitems = 0
     DO I1 = 0,ss%nspec_alla
        NAM1 = TRIM(ss%ALLA_name(I1))
        ILAB1 = 'specie: '//TRIM(ss%ALLA_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%alla_index(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_all_index
  !------------------------
  IF(ALLOCATED(ss%all_index).AND.hcheck(1,1)) THEN
     ! I all_index(0:nspec_all)
     listname = "ALL_INDEX"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "species index in original lists"
     nitems = 0
     DO I1 = 0,ss%nspec_all
        NAM1 = TRIM(ss%ALL_name(I1))
        ILAB1 = 'specie: '//TRIM(ss%ALL_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%all_index(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_amaxlh
  !------------------------
  IF(ALLOCATED(ss%amaxlh).AND.hcheck(1,1)) THEN
     ! R|units=- amaxlh(nspectrumlh_src(),nlhrf_src)
     listname = "AMAXLH"
     enum_name = "AMAXLH__RVEC"
     icomponent = ps_cnum_LH
     units = "-"
     label = "width of the spectrum peak"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nspectrumlh_src(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%amaxlh,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%amaxlh, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_aminlh
  !------------------------
  IF(ALLOCATED(ss%aminlh).AND.hcheck(1,1)) THEN
     ! R|units=- aminlh(nspectrumlh_src(),nlhrf_src)
     listname = "AMINLH"
     enum_name = "AMINLH__RVEC"
     icomponent = ps_cnum_LH
     units = "-"
     label = "parallel mode number, center of the spectrum peak"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nspectrumlh_src(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%aminlh,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%aminlh, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Ang_Href_coil
  !------------------------
  IF(ALLOCATED(ss%Ang_Href_coil).AND.hcheck(1,1)) THEN
     ! R|units=degrees Ang_Href_coil(ncoils)
     listname = "ANG_HREF_COIL"
     icomponent = ps_cnum_EQ
     units = "degrees"
     label = "angle w.r.t. horizontal of coil base"
     nitems = 0
     DO I1 = 1,ss%ncoils
        NAM1 = TRIM(ss%coil_name(I1))
        ILAB1 = 'pf_coils: '//TRIM(ss%coil_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Ang_Href_coil(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Ang_Vref_coil
  !------------------------
  IF(ALLOCATED(ss%Ang_Vref_coil).AND.hcheck(1,1)) THEN
     ! R|units=degrees Ang_Vref_coil(ncoils)
     listname = "ANG_VREF_COIL"
     icomponent = ps_cnum_EQ
     units = "degrees"
     label = "angle w.r.t. vertical of coil sides"
     nitems = 0
     DO I1 = 1,ss%ncoils
        NAM1 = TRIM(ss%coil_name(I1))
        ILAB1 = 'pf_coils: '//TRIM(ss%coil_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Ang_Vref_coil(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ant_model
  !------------------------
  IF(ALLOCATED(ss%ant_model).AND.hcheck(1,1)) THEN
     ! F ant_model(nicrf_src)
     listname = "ANT_MODEL"
     icomponent = ps_cnum_IC
     units = " "
     label = "antenna model filenames (1 per antenna source)"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%ant_model(I1)
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ap2_halfheight
  !------------------------
  IF(ALLOCATED(ss%ap2_halfheight).AND.hcheck(1,1)) THEN
     ! R|units=m         ap2_halfheight(nbeam)
     listname = "AP2_HALFHEIGHT"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "aperture half-height"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%ap2_halfheight(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ap2_halfwidth
  !------------------------
  IF(ALLOCATED(ss%ap2_halfwidth).AND.hcheck(1,1)) THEN
     ! R|units=m         ap2_halfwidth(nbeam)
     listname = "AP2_HALFWIDTH"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "2nd aperture half-width"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%ap2_halfwidth(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ap2_horiz_offset
  !------------------------
  IF(ALLOCATED(ss%ap2_horiz_offset).AND.hcheck(1,1)) THEN
     ! R|units=m         ap2_horiz_offset(nbeam)
     listname = "AP2_HORIZ_OFFSET"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "horizontal offset, 2nd aperture"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%ap2_horiz_offset(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ap2_vert_offset
  !------------------------
  IF(ALLOCATED(ss%ap2_vert_offset).AND.hcheck(1,1)) THEN
     ! R|units=m         ap2_vert_offset(nbeam)
     listname = "AP2_VERT_OFFSET"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "vertical offset, 2nd aperture"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%ap2_vert_offset(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_apolmaxlh
  !------------------------
  IF(ALLOCATED(ss%apolmaxlh).AND.hcheck(1,1)) THEN
     ! R|units=- apolmaxlh(nspectrumlh_src(),nlhrf_src)
     listname = "APOLMAXLH"
     enum_name = "APOLMAXLH__RVEC"
     icomponent = ps_cnum_LH
     units = "-"
     label = "poloidal mode number right of lumps"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nspectrumlh_src(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%apolmaxlh,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%apolmaxlh, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_apolminlh
  !------------------------
  IF(ALLOCATED(ss%apolminlh).AND.hcheck(1,1)) THEN
     ! R|units=- apolminlh(nspectrumlh_src(),nlhrf_src)
     listname = "APOLMINLH"
     enum_name = "APOLMINLH__RVEC"
     icomponent = ps_cnum_LH
     units = "-"
     label = "poloidal mode number left of lumps"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nspectrumlh_src(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%apolminlh,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%apolminlh, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ap_halfheight
  !------------------------
  IF(ALLOCATED(ss%ap_halfheight).AND.hcheck(1,1)) THEN
     ! R|units=m         ap_halfheight(nbeam)
     listname = "AP_HALFHEIGHT"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "aperture half-height"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%ap_halfheight(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ap_halfwidth
  !------------------------
  IF(ALLOCATED(ss%ap_halfwidth).AND.hcheck(1,1)) THEN
     ! R|units=m         ap_halfwidth(nbeam)
     listname = "AP_HALFWIDTH"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "aperture half-width"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%ap_halfwidth(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ap_horiz_offset
  !------------------------
  IF(ALLOCATED(ss%ap_horiz_offset).AND.hcheck(1,1)) THEN
     ! R|units=m         ap_horiz_offset(nbeam)
     listname = "AP_HORIZ_OFFSET"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "horizontal offset of aperture"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%ap_horiz_offset(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ap_vert_offset
  !------------------------
  IF(ALLOCATED(ss%ap_vert_offset).AND.hcheck(1,1)) THEN
     ! R|units=m         ap_vert_offset(nbeam)
     listname = "AP_VERT_OFFSET"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "vertical offset of aperture"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%ap_vert_offset(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_atormaxlh
  !------------------------
  IF(ALLOCATED(ss%atormaxlh).AND.hcheck(1,1)) THEN
     ! R|units=- atormaxlh(nspectrumlh_src(),nlhrf_src)
     listname = "ATORMAXLH"
     enum_name = "ATORMAXLH__RVEC"
     icomponent = ps_cnum_LH
     units = "-"
     label = "toroidal mode number right of lumps"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nspectrumlh_src(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%atormaxlh,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%atormaxlh, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_atorminlh
  !------------------------
  IF(ALLOCATED(ss%atorminlh).AND.hcheck(1,1)) THEN
     ! R|units=- atorminlh(nspectrumlh_src(),nlhrf_src)
     listname = "ATORMINLH"
     enum_name = "ATORMINLH__RVEC"
     icomponent = ps_cnum_LH
     units = "-"
     label = "toroidal mode number left of lumps"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nspectrumlh_src(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%atorminlh,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%atorminlh, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NBALLOON
  !------------------------
  IF(ALLOCATED(ss%Balloon_code).AND.hcheck(1,1)) THEN
     ! L|Balloon_codes  Balloon_code(nballoon)
     listname = "BALLOON_CODE"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "name(s) of Ballooning code(s) used"
     nitems = 0
     DO I1 = 1,ss%nballoon
        NAM1 = TRIM(ss%Balloon_code(I1))
        ILAB1 = 'Balloon_codes: '//TRIM(ss%Balloon_code(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%Balloon_code(I1)
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Balloon_mapper
  !------------------------
  IF(ALLOCATED(ss%Balloon_mapper).AND.hcheck(1,1)) THEN
     ! N  Balloon_mapper(nballoon)
     listname = "BALLOON_MAPPER"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "mapper code used for each ballooning code"
     nitems = 0
     DO I1 = 1,ss%nballoon
        NAM1 = TRIM(ss%Balloon_code(I1))
        ILAB1 = 'Balloon_codes: '//TRIM(ss%Balloon_code(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%Balloon_mapper(I1)
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Balloon_stable
  !------------------------
  IF(ALLOCATED(ss%Balloon_stable).AND.hcheck(1,1)) THEN
     ! I   Balloon_stable(nballoon)
     listname = "BALLOON_STABLE"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "0: stable everywhere; =1: unstable somewhere"
     nitems = 0
     DO I1 = 1,ss%nballoon
        NAM1 = TRIM(ss%Balloon_code(I1))
        ILAB1 = 'Balloon_codes: '//TRIM(ss%Balloon_code(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%Balloon_stable(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Balloon_status
  !------------------------
  IF(ALLOCATED(ss%Balloon_status).AND.hcheck(1,1)) THEN
     ! I   Balloon_status(nballoon)
     listname = "BALLOON_STATUS"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "completion status: Ballooning codes"
     nitems = 0
     DO I1 = 1,ss%nballoon
        NAM1 = TRIM(ss%Balloon_code(I1))
        ILAB1 = 'Balloon_codes: '//TRIM(ss%Balloon_code(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%Balloon_status(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_beam_type
  !------------------------
  IF(ALLOCATED(ss%beam_type).AND.hcheck(1,1)) THEN
     ! N     beam_type(nbeam)
     listname = "BEAM_TYPE"
     icomponent = ps_cnum_NBI
     units = " "
     label = "beam type:"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%beam_type(I1)
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_b_halfHeight
  !------------------------
  IF(ALLOCATED(ss%b_halfHeight).AND.hcheck(1,1)) THEN
     ! R|units=m       b_halfHeight(nbeam)
     listname = "B_HALFHEIGHT"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "beam half-height at source"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%b_halfHeight(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_b_halfwidth
  !------------------------
  IF(ALLOCATED(ss%b_halfwidth).AND.hcheck(1,1)) THEN
     ! R|units=m       b_halfwidth(nbeam)
     listname = "B_HALFWIDTH"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "beam half-width at source"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%b_halfwidth(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_b_Hdivergence
  !------------------------
  IF(ALLOCATED(ss%b_Hdivergence).AND.hcheck(1,1)) THEN
     ! R|units=degrees   b_Hdivergence(nbeam)
     listname = "B_HDIVERGENCE"
     icomponent = ps_cnum_NBI
     units = "degrees"
     label = "horizontal divergence (D) of beam"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%b_Hdivergence(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_b_Hfocal_length
  !------------------------
  IF(ALLOCATED(ss%b_Hfocal_length).AND.hcheck(1,1)) THEN
     ! R|units=m       b_Hfocal_length(nbeam)
     listname = "B_HFOCAL_LENGTH"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "horizontal focal length"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%b_Hfocal_length(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_b_Vdivergence
  !------------------------
  IF(ALLOCATED(ss%b_Vdivergence).AND.hcheck(1,1)) THEN
     ! R|units=degrees   b_Vdivergence(nbeam)
     listname = "B_VDIVERGENCE"
     icomponent = ps_cnum_NBI
     units = "degrees"
     label = "vertical divergence (D) of beam"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%b_Vdivergence(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_b_Vfocal_length
  !------------------------
  IF(ALLOCATED(ss%b_Vfocal_length).AND.hcheck(1,1)) THEN
     ! R|units=m       b_Vfocal_length(nbeam)
     listname = "B_VFOCAL_LENGTH"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "vertical focal length"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%b_Vfocal_length(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NCIRCUITS
  !------------------------
  IF(ALLOCATED(ss%circuit_name).AND.hcheck(1,1)) THEN
     ! L|pf_circuits  circuit_name(ncircuits)
     listname = "CIRCUIT_NAME"
     icomponent = ps_cnum_EQ
     units = " "
     label = "Number & names of circuits"
     nitems = 0
     DO I1 = 1,ss%ncircuits
        NAM1 = TRIM(ss%circuit_name(I1))
        ILAB1 = 'pf_circuits: '//TRIM(ss%circuit_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%circuit_name(I1)
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_coil_apt
  !------------------------
  IF(ALLOCATED(ss%coil_apt).AND.hcheck(1,1)) THEN
     ! R|units=amps/turn coil_apt(ncircuits)
     listname = "COIL_APT"
     icomponent = ps_cnum_EQ
     units = "amps/turn"
     label = "SIGNED circuit current"
     nitems = 0
     DO I1 = 1,ss%ncircuits
        NAM1 = TRIM(ss%circuit_name(I1))
        ILAB1 = 'pf_circuits: '//TRIM(ss%circuit_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%coil_apt(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_coil_in_circuit
  !------------------------
  IF(ALLOCATED(ss%coil_in_circuit).AND.hcheck(1,1)) THEN
     ! N  coil_in_circuit(ncoils)
     listname = "COIL_IN_CIRCUIT"
     icomponent = ps_cnum_EQ
     units = " "
     label = "circuit to which each coil belongs"
     nitems = 0
     DO I1 = 1,ss%ncoils
        NAM1 = TRIM(ss%coil_name(I1))
        ILAB1 = 'pf_coils: '//TRIM(ss%coil_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%coil_in_circuit(I1)
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NCOILS
  !------------------------
  IF(ALLOCATED(ss%coil_name).AND.hcheck(1,1)) THEN
     ! L|pf_coils     coil_name(ncoils)
     listname = "COIL_NAME"
     icomponent = ps_cnum_EQ
     units = " "
     label = "Number & name of axisymmetric coils"
     nitems = 0
     DO I1 = 1,ss%ncoils
        NAM1 = TRIM(ss%coil_name(I1))
        ILAB1 = 'pf_coils: '//TRIM(ss%coil_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%coil_name(I1)
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_coil_resispt
  !------------------------
  IF(ALLOCATED(ss%coil_resispt).AND.hcheck(1,1)) THEN
     ! R|units=ohms/turn  coil_resispt(ncoils)
     listname = "COIL_RESISPT"
     icomponent = ps_cnum_EQ
     units = "ohms/turn"
     label = "coil resistance per turn"
     nitems = 0
     DO I1 = 1,ss%ncoils
        NAM1 = TRIM(ss%coil_name(I1))
        ILAB1 = 'pf_coils: '//TRIM(ss%coil_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%coil_resispt(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NDELTA_W
  !------------------------
  IF(ALLOCATED(ss%DeltaW_code).AND.hcheck(1,1)) THEN
     ! L|Delta_W_codes  DeltaW_code(ndelta_w)
     listname = "DELTAW_CODE"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "name(s) of delta(W) code(s) used"
     nitems = 0
     DO I1 = 1,ss%ndelta_w
        NAM1 = TRIM(ss%DeltaW_code(I1))
        ILAB1 = 'Delta_W_codes: '//TRIM(ss%DeltaW_code(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%DeltaW_code(I1)
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_DeltaW_mapper
  !------------------------
  IF(ALLOCATED(ss%DeltaW_mapper).AND.hcheck(1,1)) THEN
     ! N  DeltaW_mapper(ndelta_W)
     listname = "DELTAW_MAPPER"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "mapper code used for each delta(W) code"
     nitems = 0
     DO I1 = 1,ss%ndelta_w
        NAM1 = TRIM(ss%DeltaW_code(I1))
        ILAB1 = 'Delta_W_codes: '//TRIM(ss%DeltaW_code(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%DeltaW_mapper(I1)
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_DeltaW_stable
  !------------------------
  IF(ALLOCATED(ss%DeltaW_stable).AND.hcheck(1,1)) THEN
     ! I   DeltaW_stable(mmodes,ndelta_W)
     listname = "DELTAW_STABLE"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "0: stable; 1: unstable;"
     nitems = 0
     DO I2 = 1,ss%ndelta_w
        NAM2 = TRIM(ss%DeltaW_code(I2))
        ILAB2 = 'Delta_W_codes: '//TRIM(ss%DeltaW_code(I2))
        DO I1 = 1,ss%mmodes
           NAM1 = TRIM(ss%Tor_mode_label(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'toroidal_modes: '//TRIM(ss%Tor_mode_label(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%DeltaW_stable(I1,I2)
           item_c256(nitems) = ILAB1
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_DeltaW_status
  !------------------------
  IF(ALLOCATED(ss%DeltaW_status).AND.hcheck(1,1)) THEN
     ! I   DeltaW_status(ndelta_W)
     listname = "DELTAW_STATUS"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "completion status: Delta(W) codes"
     nitems = 0
     DO I1 = 1,ss%ndelta_w
        NAM1 = TRIM(ss%DeltaW_code(I1))
        ILAB1 = 'Delta_W_codes: '//TRIM(ss%DeltaW_code(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%DeltaW_status(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_dist_fun
  !------------------------
  IF(ALLOCATED(ss%dist_fun).AND.hcheck(1,1)) THEN
     ! F  dist_fun(0:nspec_alla)
     listname = "DIST_FUN"
     icomponent = ps_cnum_IC
     units = " "
     label = "distribution function filenames"
     nitems = 0
     DO I1 = 0,ss%nspec_alla
        NAM1 = TRIM(ss%ALLA_name(I1))
        ILAB1 = 'specie: '//TRIM(ss%ALLA_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%dist_fun(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_dx_fshield
  !------------------------
  IF(ALLOCATED(ss%dx_fshield).AND.hcheck(1,1)) THEN
     ! R|units=m dx_fshield(nicrf_src)
     listname = "DX_FSHIELD"
     icomponent = ps_cnum_IC
     units = "m"
     label = "distance, antenna to Faraday shield"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%dx_fshield(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_e0_av
  !------------------------
  IF(ALLOCATED(ss%e0_av).AND.hcheck(1,1)) THEN
     ! R|units=KeV   e0_av(ngsc0)
     listname = "E0_AV"
     icomponent = ps_cnum_GAS
     units = "KeV"
     label = 'average energy of neutral sources, "(3/2)*T0"'
     nitems = 0
     DO I1 = 1,ss%ngsc0
        NAM1 = TRIM(ss%gs_name(I1))
        ILAB1 = 'gas_source: '//TRIM(ss%gs_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%e0_av(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NECRF_SRC
  !------------------------
  IF(ALLOCATED(ss%ecrf_src_name).AND.hcheck(1,1)) THEN
     ! L|ecrf_source   ecrf_src_name(necrf_src)
     listname = "ECRF_SRC_NAME"
     icomponent = ps_cnum_EC
     units = " "
     label = "number & name of ECRF sources"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%ecrf_src_name(I1)
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_EC_Beam_Elongation
  !------------------------
  IF(ALLOCATED(ss%EC_Beam_Elongation).AND.hcheck(1,1)) THEN
     ! R|units=-   EC_Beam_Elongation(necrf_src)
     listname = "EC_BEAM_ELONGATION"
     icomponent = ps_cnum_EC
     units = "-"
     label = "EC beam ellipticity"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%EC_Beam_Elongation(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_EC_Half_Power_Angle
  !------------------------
  IF(ALLOCATED(ss%EC_Half_Power_Angle).AND.hcheck(1,1)) THEN
     ! R|units=degrees  EC_Half_Power_Angle(necrf_src)
     listname = "EC_HALF_POWER_ANGLE"
     icomponent = ps_cnum_EC
     units = "degrees"
     label = "Divergence of EC beam"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%EC_Half_Power_Angle(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_EC_Omode_Fraction
  !------------------------
  IF(ALLOCATED(ss%EC_Omode_Fraction).AND.hcheck(1,1)) THEN
     ! R|units=-   EC_Omode_Fraction(necrf_src)
     listname = "EC_OMODE_FRACTION"
     icomponent = ps_cnum_EC
     units = "-"
     label = "O-mode fraction of EC source"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%EC_Omode_Fraction(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_EC_phi_aim
  !------------------------
  IF(ALLOCATED(ss%EC_phi_aim).AND.hcheck(1,1)) THEN
     ! R|units=degrees  EC_phi_aim(necrf_src)
     listname = "EC_PHI_AIM"
     icomponent = ps_cnum_EC
     units = "degrees"
     label = "toroidal aiming angle"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%EC_phi_aim(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_EC_PRINC_CURV_HOR
  !------------------------
  IF(ALLOCATED(ss%EC_PRINC_CURV_HOR).AND.hcheck(1,1)) THEN
     ! R|units=m  EC_PRINC_CURV_HOR(necrf_src)
     listname = "EC_PRINC_CURV_HOR"
     icomponent = ps_cnum_EC
     units = "m"
     label = "principal curvature in horizontal direction"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%EC_PRINC_CURV_HOR(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_EC_PRINC_CURV_VERT
  !------------------------
  IF(ALLOCATED(ss%EC_PRINC_CURV_VERT).AND.hcheck(1,1)) THEN
     ! R|units=m  EC_PRINC_CURV_VERT(necrf_src)
     listname = "EC_PRINC_CURV_VERT"
     icomponent = ps_cnum_EC
     units = "m"
     label = "principal curvature in vertical direction"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%EC_PRINC_CURV_VERT(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_EC_PRINC_WIDTH_HOR
  !------------------------
  IF(ALLOCATED(ss%EC_PRINC_WIDTH_HOR).AND.hcheck(1,1)) THEN
     ! R|units=m  EC_PRINC_WIDTH_HOR(necrf_src)
     listname = "EC_PRINC_WIDTH_HOR"
     icomponent = ps_cnum_EC
     units = "m"
     label = "principal beam width in horizontal direction"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%EC_PRINC_WIDTH_HOR(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_EC_PRINC_WIDTH_VERT
  !------------------------
  IF(ALLOCATED(ss%EC_PRINC_WIDTH_VERT).AND.hcheck(1,1)) THEN
     ! R|units=m  EC_PRINC_WIDTH_VERT(necrf_src)
     listname = "EC_PRINC_WIDTH_VERT"
     icomponent = ps_cnum_EC
     units = "m"
     label = "principal beam width in vertical direction"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%EC_PRINC_WIDTH_VERT(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_EC_theta_aim
  !------------------------
  IF(ALLOCATED(ss%EC_theta_aim).AND.hcheck(1,1)) THEN
     ! R|units=degrees  EC_theta_aim(necrf_src)
     listname = "EC_THETA_AIM"
     icomponent = ps_cnum_EC
     units = "degrees"
     label = "poloidal aiming angle"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%EC_theta_aim(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Einj_max
  !------------------------
  IF(ALLOCATED(ss%Einj_max).AND.hcheck(1,1)) THEN
     ! R|units=keV       Einj_max(nbeam)
     listname = "EINJ_MAX"
     icomponent = ps_cnum_NBI
     units = "keV"
     label = "maximum injection energy"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Einj_max(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Einj_min
  !------------------------
  IF(ALLOCATED(ss%Einj_min).AND.hcheck(1,1)) THEN
     ! R|units=keV       Einj_min(nbeam)
     listname = "EINJ_MIN"
     icomponent = ps_cnum_NBI
     units = "keV"
     label = "minimum injection energy"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Einj_min(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NEQMOM
  !------------------------
  IF(ALLOCATED(ss%eqmom_num).AND.hcheck(1,1)) THEN
     ! L|EQ_moments  eqmom_num(neqmom)
     listname = "EQMOM_NUM"
     icomponent = ps_cnum_EQ
     units = " "
     label = "Equilibrium moments"
     nitems = 0
     DO I1 = 1,ss%neqmom
        NAM1 = TRIM(ss%eqmom_num(I1))
        ILAB1 = 'EQ_moments: '//TRIM(ss%eqmom_num(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%eqmom_num(I1)
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_fracmin
  !------------------------
  IF(ALLOCATED(ss%fracmin).AND.hcheck(1,1)) THEN
     ! R|units=- fracmin(nspec_rfmin)
     listname = "FRACMIN"
     icomponent = ps_cnum_IC
     units = "-"
     label = 'minority density fraction if kdens_rfmin="fraction"'
     nitems = 0
     DO I1 = 1,ss%nspec_rfmin
        NAM1 = TRIM(ss%RFMIN_name(I1))
        ILAB1 = 'RF_minority: '//TRIM(ss%RFMIN_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%fracmin(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_fracpslh
  !------------------------
  IF(ALLOCATED(ss%fracpslh).AND.hcheck(1,1)) THEN
     ! R|units=- fracpslh(nspectrumlh_src(),nlhrf_src)
     listname = "FRACPSLH"
     enum_name = "FRACPSLH__RVEC"
     icomponent = ps_cnum_LH
     units = "-"
     label = "fraction of power on each peak (the sum over all peaks must be 1)"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nspectrumlh_src(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%fracpslh,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%fracpslh, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_frac_full
  !------------------------
  IF(ALLOCATED(ss%frac_full).AND.hcheck(1,1)) THEN
     ! R|units=-  frac_full(nbeam)
     listname = "FRAC_FULL"
     icomponent = ps_cnum_NBI
     units = "-"
     label = "fraction of beam current at full voltage"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%frac_full(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_frac_half
  !------------------------
  IF(ALLOCATED(ss%frac_half).AND.hcheck(1,1)) THEN
     ! R|units=-  frac_half(nbeam)
     listname = "FRAC_HALF"
     icomponent = ps_cnum_NBI
     units = "-"
     label = "fraction of beam current at half voltage"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%frac_half(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_freq_ec
  !------------------------
  IF(ALLOCATED(ss%freq_ec).AND.hcheck(1,1)) THEN
     ! R|units=Hz freq_ec(necrf_src)
     listname = "FREQ_EC"
     icomponent = ps_cnum_EC
     units = "Hz"
     label = "frequency of EC source"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%freq_ec(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_freq_ic
  !------------------------
  IF(ALLOCATED(ss%freq_ic).AND.hcheck(1,1)) THEN
     ! R|units=Hz freq_ic(nicrf_src)
     listname = "FREQ_IC"
     icomponent = ps_cnum_IC
     units = "Hz"
     label = "frequency on each ICRF source"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%freq_ic(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_freq_lh
  !------------------------
  IF(ALLOCATED(ss%freq_lh).AND.hcheck(1,1)) THEN
     ! R|units=Hz freq_lh(nlhrf_src)
     listname = "FREQ_LH"
     icomponent = ps_cnum_LH
     units = "Hz"
     label = "frequency on each LHRF source"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%freq_lh(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_gamma_lmhd
  !------------------------
  IF(ALLOCATED(ss%gamma_lmhd).AND.hcheck(1,1)) THEN
     ! R|units=-  gamma_lmhd(mmodes,ndelta_W)
     listname = "GAMMA_LMHD"
     icomponent = ps_cnum_LMHD
     units = "-"
     label = "ideal growth rate"
     nitems = 0
     DO I2 = 1,ss%ndelta_w
        NAM2 = TRIM(ss%DeltaW_code(I2))
        ILAB2 = 'Delta_W_codes: '//TRIM(ss%DeltaW_code(I2))
        DO I1 = 1,ss%mmodes
           NAM1 = TRIM(ss%Tor_mode_label(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'toroidal_modes: '//TRIM(ss%Tor_mode_label(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
           item_r8vals(nitems) = ss%gamma_lmhd(I1,I2)
           item_ivals(nitems) = 0
           item_c256(nitems) = ILAB1
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_gas_atom
  !------------------------
  IF(ALLOCATED(ss%gas_atom).AND.hcheck(1,1)) THEN
     ! N  gas_atom(ngsc0)
     listname = "GAS_ATOM"
     icomponent = ps_cnum_GAS
     units = " "
     label = "name of species introduced by each gas source"
     nitems = 0
     DO I1 = 1,ss%ngsc0
        NAM1 = TRIM(ss%gs_name(I1))
        ILAB1 = 'gas_source: '//TRIM(ss%gs_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%gas_atom(I1)
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NGSC0
  !------------------------
  IF(ALLOCATED(ss%gs_name).AND.hcheck(1,1)) THEN
     ! L|gas_source    gs_name(ngsc0)
     listname = "GS_NAME"
     icomponent = ps_cnum_GAS
     units = " "
     label = "number & name of edge neutral gas sources"
     nitems = 0
     DO I1 = 1,ss%ngsc0
        NAM1 = TRIM(ss%gs_name(I1))
        ILAB1 = 'gas_source: '//TRIM(ss%gs_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%gs_name(I1)
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_hegrilh
  !------------------------
  IF(ALLOCATED(ss%hegrilh).AND.hcheck(1,1)) THEN
     ! R|units=- hegrilh(nlhrf_src)
     listname = "HEGRILH"
     icomponent = ps_cnum_LH
     units = "-"
     label = "need description"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%hegrilh(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Hsize_coil
  !------------------------
  IF(ALLOCATED(ss%Hsize_coil).AND.hcheck(1,1)) THEN
     ! R|units=m  Hsize_coil(ncoils)
     listname = "HSIZE_COIL"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "(Rotatable) Horizontal coil base size"
     nitems = 0
     DO I1 = 1,ss%ncoils
        NAM1 = TRIM(ss%coil_name(I1))
        ILAB1 = 'pf_coils: '//TRIM(ss%coil_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Hsize_coil(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_icircuit_coil
  !------------------------
  IF(ALLOCATED(ss%icircuit_coil).AND.hcheck(1,1)) THEN
     ! I  icircuit_coil(ncoils)
     listname = "ICIRCUIT_COIL"
     icomponent = ps_cnum_EQ
     units = " "
     label = "index of circuit to which each coil belongs"
     nitems = 0
     DO I1 = 1,ss%ncoils
        NAM1 = TRIM(ss%coil_name(I1))
        ILAB1 = 'pf_coils: '//TRIM(ss%coil_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%icircuit_coil(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NICRF_SRC
  !------------------------
  IF(ALLOCATED(ss%icrf_src_name).AND.hcheck(1,1)) THEN
     ! L|icrf_source   icrf_src_name(nicrf_src)
     listname = "ICRF_SRC_NAME"
     icomponent = ps_cnum_IC
     units = " "
     label = "number & name of ICRF sources"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%icrf_src_name(I1)
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Imag_ant_coef
  !------------------------
  IF(ALLOCATED(ss%Imag_ant_coef).AND.hcheck(1,1)) THEN
     ! R|units=Amps/m Imag_ant_coef(num_nphi_vac(),nicrf_src)
     listname = "IMAG_ANT_COEF"
     enum_name = "IMAG_ANT_COEF__RVEC"
     icomponent = ps_cnum_IC
     units = "Amps/m"
     label = "imag part of Fourier Coef"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%num_nphi_vac(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%Imag_ant_coef,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%Imag_ant_coef, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_isThermal
  !------------------------
  IF(ALLOCATED(ss%isThermal).AND.hcheck(1,1)) THEN
     ! I  isThermal(nspec_rfmin)
     listname = "ISTHERMAL"
     icomponent = ps_cnum_IC
     units = " "
     label = "=1 if minority specie is thermalized"
     nitems = 0
     DO I1 = 1,ss%nspec_rfmin
        NAM1 = TRIM(ss%RFMIN_name(I1))
        ILAB1 = 'RF_minority: '//TRIM(ss%RFMIN_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%isThermal(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_is_recycling
  !------------------------
  IF(ALLOCATED(ss%is_recycling).AND.hcheck(1,1)) THEN
     ! I  is_recycling(ngsc0)
     listname = "IS_RECYCLING"
     icomponent = ps_cnum_GAS
     units = " "
     label = "0/1; =1 to for recycling sources"
     nitems = 0
     DO I1 = 1,ss%ngsc0
        NAM1 = TRIM(ss%gs_name(I1))
        ILAB1 = 'gas_source: '//TRIM(ss%gs_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%is_recycling(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_kvolt_nbi
  !------------------------
  IF(ALLOCATED(ss%kvolt_nbi).AND.hcheck(1,1)) THEN
     ! R|units=keV kvolt_nbi(nbeam)
     listname = "KVOLT_NBI"
     icomponent = ps_cnum_NBI
     units = "keV"
     label = "energy of each beam source **keV**"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%kvolt_nbi(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Lbscap
  !------------------------
  IF(ALLOCATED(ss%Lbscap).AND.hcheck(1,1)) THEN
     ! R|units=m       Lbscap(nbeam)
     listname = "LBSCAP"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "distance, source to aperture"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Lbscap(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Lbscap2
  !------------------------
  IF(ALLOCATED(ss%Lbscap2).AND.hcheck(1,1)) THEN
     ! R|units=m       Lbscap2(nbeam)
     listname = "LBSCAP2"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "distance, source to 2nd aperture"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Lbscap2(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Lbsctan
  !------------------------
  IF(ALLOCATED(ss%Lbsctan).AND.hcheck(1,1)) THEN
     ! R|units=m       Lbsctan(nbeam)
     listname = "LBSCTAN"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "distance, source to tangency point"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Lbsctan(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NLHRF_SRC
  !------------------------
  IF(ALLOCATED(ss%lhrf_src_name).AND.hcheck(1,1)) THEN
     ! L|lhrf_source   lhrf_src_name(nlhrf_src)
     listname = "LHRF_SRC_NAME"
     icomponent = ps_cnum_LH
     units = " "
     label = "number & name of LHRF sources"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%lhrf_src_name(I1)
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Mercier_stable
  !------------------------
  IF(ALLOCATED(ss%Mercier_stable).AND.hcheck(1,1)) THEN
     ! I   Mercier_stable(nballoon)
     listname = "MERCIER_STABLE"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "0: stable everywhere; =1: unstable somewhere"
     nitems = 0
     DO I1 = 1,ss%nballoon
        NAM1 = TRIM(ss%Balloon_code(I1))
        ILAB1 = 'Balloon_codes: '//TRIM(ss%Balloon_code(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%Mercier_stable(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nbap2_shape
  !------------------------
  IF(ALLOCATED(ss%nbap2_shape).AND.hcheck(1,1)) THEN
     ! N     nbap2_shape(nbeam)
     listname = "NBAP2_SHAPE"
     icomponent = ps_cnum_NBI
     units = " "
     label = "keyword for optional 2nd aperture"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%nbap2_shape(I1)
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nbap_shape
  !------------------------
  IF(ALLOCATED(ss%nbap_shape).AND.hcheck(1,1)) THEN
     ! N     nbap_shape(nbeam)
     listname = "NBAP_SHAPE"
     icomponent = ps_cnum_NBI
     units = " "
     label = "shape keyword for beam aperture"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%nbap_shape(I1)
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nbeami_bdy
  !------------------------
  IF(ALLOCATED(ss%nbeami_bdy).AND.hcheck(1,1)) THEN
     ! R|units=m^-3  nbeami_bdy(nspec_beam)
     listname = "NBEAMI_BDY"
     icomponent = ps_cnum_NBI
     units = "m^-3"
     label = "beam ion densities at/beyond boundary"
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        NAM1 = TRIM(ss%SNBI_name(I1))
        ILAB1 = 'beam_ion: '//TRIM(ss%SNBI_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%nbeami_bdy(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nbion
  !------------------------
  IF(ALLOCATED(ss%nbion).AND.hcheck(1,1)) THEN
     ! N  nbion(nbeam)
     listname = "NBION"
     icomponent = ps_cnum_NBI
     units = " "
     label = "name of species injected by each beam."
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%nbion(I1)
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nbion_trace
  !------------------------
  IF(ALLOCATED(ss%nbion_trace).AND.hcheck(1,1)) THEN
     ! N  nbion_trace(nbeam)
     listname = "NBION_TRACE"
     icomponent = ps_cnum_NBI
     units = " "
     label = "(if non-blank) beam trace element"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%nbion_trace(I1)
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NBEAM
  !------------------------
  IF(ALLOCATED(ss%nbi_src_name).AND.hcheck(1,1)) THEN
     ! L|neutral_beams nbi_src_name(nbeam)
     listname = "NBI_SRC_NAME"
     icomponent = ps_cnum_NBI
     units = " "
     label = "number & name of neutral beams"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%nbi_src_name(I1)
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nbshape
  !------------------------
  IF(ALLOCATED(ss%nbshape).AND.hcheck(1,1)) THEN
     ! N     nbshape(nbeam)
     listname = "NBSHAPE"
     icomponent = ps_cnum_NBI
     units = " "
     label = "shape keyword for beam source grid"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%nbshape(I1)
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NNEWCOMB
  !------------------------
  IF(ALLOCATED(ss%Newcomb_code).AND.hcheck(1,1)) THEN
     ! L|Newcomb_codes  Newcomb_code(nnewcomb)
     listname = "NEWCOMB_CODE"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "name(s) of Newcomb code(s) used"
     nitems = 0
     DO I1 = 1,ss%nnewcomb
        NAM1 = TRIM(ss%Newcomb_code(I1))
        ILAB1 = 'Newcomb_codes: '//TRIM(ss%Newcomb_code(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%Newcomb_code(I1)
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Newcomb_mapper
  !------------------------
  IF(ALLOCATED(ss%Newcomb_mapper).AND.hcheck(1,1)) THEN
     ! N  Newcomb_mapper(nnewcomb)
     listname = "NEWCOMB_MAPPER"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "mapper code used for each Newcomb code"
     nitems = 0
     DO I1 = 1,ss%nnewcomb
        NAM1 = TRIM(ss%Newcomb_code(I1))
        ILAB1 = 'Newcomb_codes: '//TRIM(ss%Newcomb_code(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%Newcomb_mapper(I1)
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Newcomb_stable
  !------------------------
  IF(ALLOCATED(ss%Newcomb_stable).AND.hcheck(1,1)) THEN
     ! I   Newcomb_stable(mmodes,nnewcomb)
     listname = "NEWCOMB_STABLE"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "0: stable; 1: unstable;"
     nitems = 0
     DO I2 = 1,ss%nnewcomb
        NAM2 = TRIM(ss%Newcomb_code(I2))
        ILAB2 = 'Newcomb_codes: '//TRIM(ss%Newcomb_code(I2))
        DO I1 = 1,ss%mmodes
           NAM1 = TRIM(ss%Tor_mode_label(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'toroidal_modes: '//TRIM(ss%Tor_mode_label(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%Newcomb_stable(I1,I2)
           item_c256(nitems) = ILAB1
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Newcomb_status
  !------------------------
  IF(ALLOCATED(ss%Newcomb_status).AND.hcheck(1,1)) THEN
     ! I   Newcomb_status(nnewcomb)
     listname = "NEWCOMB_STATUS"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "completion status: Newcomb codes"
     nitems = 0
     DO I1 = 1,ss%nnewcomb
        NAM1 = TRIM(ss%Newcomb_code(I1))
        ILAB1 = 'Newcomb_codes: '//TRIM(ss%Newcomb_code(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%Newcomb_status(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nfusi_bdy
  !------------------------
  IF(ALLOCATED(ss%nfusi_bdy).AND.hcheck(1,1)) THEN
     ! R|units=m^-3  nfusi_bdy(nspec_fusion)
     listname = "NFUSI_BDY"
     icomponent = ps_cnum_IC
     units = "m^-3"
     label = "fusion ion densities at/beyond boundary"
     nitems = 0
     DO I1 = 1,ss%nspec_fusion
        NAM1 = TRIM(ss%SFUS_name(I1))
        ILAB1 = 'fusion_ion: '//TRIM(ss%SFUS_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%nfusi_bdy(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nmini_bdy
  !------------------------
  IF(ALLOCATED(ss%nmini_bdy).AND.hcheck(1,1)) THEN
     ! R|units=m^-3  nmini_bdy(nspec_rfmin)
     listname = "NMINI_BDY"
     icomponent = ps_cnum_FUS
     units = "m^-3"
     label = "RF minority ion densities at/beyond boundary"
     nitems = 0
     DO I1 = 1,ss%nspec_rfmin
        NAM1 = TRIM(ss%RFMIN_name(I1))
        ILAB1 = 'RF_minority: '//TRIM(ss%RFMIN_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%nmini_bdy(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nnkparlh
  !------------------------
  IF(ALLOCATED(ss%nnkparlh).AND.hcheck(1,1)) THEN
     ! I nnkparlh(nspectrumlh_src(),nlhrf_src)
     listname = "NNKPARLH"
     enum_name = "NNKPARLH__IVEC"
     icomponent = ps_cnum_LH
     units = " "
     label = "need description"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nspectrumlh_src(I1)
        item_c256(nitems) = "__inumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%nnkparlh,1)
     CALL ps_inum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%nnkparlh, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nnkpollh
  !------------------------
  IF(ALLOCATED(ss%nnkpollh).AND.hcheck(1,1)) THEN
     ! I nnkpollh(nspectrumlh_src(),nlhrf_src)
     listname = "NNKPOLLH"
     enum_name = "NNKPOLLH__IVEC"
     icomponent = ps_cnum_LH
     units = " "
     label = "need description"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nspectrumlh_src(I1)
        item_c256(nitems) = "__inumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%nnkpollh,1)
     CALL ps_inum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%nnkpollh, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nnktorlh
  !------------------------
  IF(ALLOCATED(ss%nnktorlh).AND.hcheck(1,1)) THEN
     ! I nnktorlh(nspectrumlh_src(),nlhrf_src)
     listname = "NNKTORLH"
     enum_name = "NNKTORLH__IVEC"
     icomponent = ps_cnum_LH
     units = " "
     label = "need description"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nspectrumlh_src(I1)
        item_c256(nitems) = "__inumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%nnktorlh,1)
     CALL ps_inum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%nnktorlh, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nphi
  !------------------------
  IF(ALLOCATED(ss%nphi).AND.hcheck(1,1)) THEN
     ! I nphi(num_nphi(),nicrf_src)
     listname = "NPHI"
     enum_name = "NPHI__IVEC"
     icomponent = ps_cnum_IC
     units = " "
     label = "n_phi wave spectrum from antenna"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%num_nphi(I1)
        item_c256(nitems) = "__inumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%nphi,1)
     CALL ps_inum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%nphi, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nrz_antgeo
  !------------------------
  IF(ALLOCATED(ss%nrz_antgeo).AND.hcheck(1,1)) THEN
     ! I|ENUM nrz_antgeo(nicrf_src)
     listname = "NRZ_ANTGEO"
     icomponent = ps_cnum_IC
     units = " "
     label = "number of (R,Z) points, antenna geometries"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nrz_antgeo(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nspectrumlh_src
  !------------------------
  IF(ALLOCATED(ss%nspectrumlh_src).AND.hcheck(1,1)) THEN
     ! I|ENUM nspectrumlh_src(nlhrf_src)
     listname = "NSPECTRUMLH_SRC"
     icomponent = ps_cnum_LH
     units = " "
     label = "number of spectrum peaks"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nspectrumlh_src(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ns_bdy
  !------------------------
  IF(ALLOCATED(ss%ns_bdy).AND.hcheck(1,1)) THEN
     ! R|units=m^-3 ns_bdy(0:nspec_th)
     listname = "NS_BDY"
     icomponent = ps_cnum_PLASMA
     units = "m^-3"
     label = "specie densities at/beyond boundary"
     nitems = 0
     DO I1 = 0,ss%nspec_th
        NAM1 = TRIM(ss%S_name(I1))
        ILAB1 = 'thermal_specie: '//TRIM(ss%S_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%ns_bdy(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ns_is_input
  !------------------------
  IF(ALLOCATED(ss%ns_is_input).AND.hcheck(1,1)) THEN
     ! I ns_is_input(0:nspec_th)
     listname = "NS_IS_INPUT"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "specie density measurement flag"
     nitems = 0
     DO I1 = 0,ss%nspec_th
        NAM1 = TRIM(ss%S_name(I1))
        ILAB1 = 'thermal_specie: '//TRIM(ss%S_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%ns_is_input(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_nthinlh
  !------------------------
  IF(ALLOCATED(ss%nthinlh).AND.hcheck(1,1)) THEN
     ! I nthinlh(nlhrf_src)
     listname = "NTHINLH"
     icomponent = ps_cnum_LH
     units = " "
     label = "need description"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nthinlh(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Nturns
  !------------------------
  IF(ALLOCATED(ss%Nturns).AND.hcheck(1,1)) THEN
     ! I  Nturns(ncoils)
     listname = "NTURNS"
     icomponent = ps_cnum_EQ
     units = " "
     label = "SIGNED number of conductor turns wrapped in each coil"
     nitems = 0
     DO I1 = 1,ss%ncoils
        NAM1 = TRIM(ss%coil_name(I1))
        ILAB1 = 'pf_coils: '//TRIM(ss%coil_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%Nturns(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_num_nphi
  !------------------------
  IF(ALLOCATED(ss%num_nphi).AND.hcheck(1,1)) THEN
     ! I|ENUM:nphi  num_nphi(nicrf_src)
     listname = "NUM_NPHI"
     icomponent = ps_cnum_IC
     units = " "
     label = "number of non-zero n_phi values"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%num_nphi(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_num_nphi_vac
  !------------------------
  IF(ALLOCATED(ss%num_nphi_vac).AND.hcheck(1,1)) THEN
     ! I|ENUM num_nphi_vac(nicrf_src)
     listname = "NUM_NPHI_VAC"
     icomponent = ps_cnum_IC
     units = " "
     label = "number of non-zero n_phi values"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%num_nphi_vac(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_n_straps
  !------------------------
  IF(ALLOCATED(ss%n_straps).AND.hcheck(1,1)) THEN
     ! I|ENUM n_straps(nicrf_src)
     listname = "N_STRAPS"
     icomponent = ps_cnum_IC
     units = " "
     label = "number of straps in the antenna"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%n_straps(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Phibsc
  !------------------------
  IF(ALLOCATED(ss%Phibsc).AND.hcheck(1,1)) THEN
     ! R|units=degrees       Phibsc(nbeam)
     listname = "PHIBSC"
     icomponent = ps_cnum_NBI
     units = "degrees"
     label = "toroidal location of beam source"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Phibsc(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Phi_EC_launch
  !------------------------
  IF(ALLOCATED(ss%Phi_EC_launch).AND.hcheck(1,1)) THEN
     ! R|units=degrees  Phi_EC_launch(necrf_src)
     listname = "PHI_EC_LAUNCH"
     icomponent = ps_cnum_EC
     units = "degrees"
     label = "Phi of center of launcher"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Phi_EC_launch(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_picrf_abs
  !------------------------
  IF(ALLOCATED(ss%picrf_abs).AND.hcheck(1,1)) THEN
     ! R|units=W picrf_abs(nicrf_src)
     listname = "PICRF_ABS"
     icomponent = ps_cnum_IC
     units = "W"
     label = "RF power absorbed inside plasma"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%picrf_abs(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_picrf_ext
  !------------------------
  IF(ALLOCATED(ss%picrf_ext).AND.hcheck(1,1)) THEN
     ! R|units=W picrf_ext(nicrf_src)
     listname = "PICRF_EXT"
     icomponent = ps_cnum_IC
     units = "W"
     label = "RF power deposited outside plasma"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%picrf_ext(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_power_ec
  !------------------------
  IF(ALLOCATED(ss%power_ec).AND.hcheck(1,1)) THEN
     ! R|units=W  power_ec(necrf_src)
     listname = "POWER_EC"
     icomponent = ps_cnum_EC
     units = "W"
     label = "power on each ECRF source"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%power_ec(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_power_ic
  !------------------------
  IF(ALLOCATED(ss%power_ic).AND.hcheck(1,1)) THEN
     ! R|units=W  power_ic(nicrf_src)
     listname = "POWER_IC"
     icomponent = ps_cnum_IC
     units = "W"
     label = "power on each ICRF source"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%power_ic(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_power_lh
  !------------------------
  IF(ALLOCATED(ss%power_lh).AND.hcheck(1,1)) THEN
     ! R|units=W  power_lh(nlhrf_src)
     listname = "POWER_LH"
     icomponent = ps_cnum_LH
     units = "W"
     label = "power on each LHRF source"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%power_lh(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_power_nbi
  !------------------------
  IF(ALLOCATED(ss%power_nbi).AND.hcheck(1,1)) THEN
     ! R|units=W  power_nbi(nbeam)
     listname = "POWER_NBI"
     icomponent = ps_cnum_NBI
     units = "W"
     label = "power on each beam source"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%power_nbi(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_power_nbi_trace
  !------------------------
  IF(ALLOCATED(ss%power_nbi_trace).AND.hcheck(1,1)) THEN
     ! R|units=W  power_nbi_trace(nbeam)
     listname = "POWER_NBI_TRACE"
     icomponent = ps_cnum_NBI
     units = "W"
     label = "power injected as trace element (if any)"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%power_nbi_trace(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NPSMOM
  !------------------------
  IF(ALLOCATED(ss%psmom_num).AND.hcheck(1,1)) THEN
     ! L|PS_moments  psmom_num(npsmom)
     listname = "PSMOM_NUM"
     icomponent = ps_cnum_EQ
     units = " "
     label = "Pfirsch-Schlutter moments"
     nitems = 0
     DO I1 = 1,ss%npsmom
        NAM1 = TRIM(ss%psmom_num(I1))
        ILAB1 = 'PS_moments: '//TRIM(ss%psmom_num(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%psmom_num(I1)
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_ql_operator
  !------------------------
  IF(ALLOCATED(ss%ql_operator).AND.hcheck(1,1)) THEN
     ! F  ql_operator(0:nspec_alla)
     listname = "QL_OPERATOR"
     icomponent = ps_cnum_IC
     units = " "
     label = "quasilinear operator filenames"
     nitems = 0
     DO I1 = 0,ss%nspec_alla
        NAM1 = TRIM(ss%ALLA_name(I1))
        ILAB1 = 'specie: '//TRIM(ss%ALLA_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%ql_operator(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Real_ant_coef
  !------------------------
  IF(ALLOCATED(ss%Real_ant_coef).AND.hcheck(1,1)) THEN
     ! R|units=Amps/m Real_ant_coef(num_nphi_vac(),nicrf_src)
     listname = "REAL_ANT_COEF"
     enum_name = "REAL_ANT_COEF__RVEC"
     icomponent = ps_cnum_IC
     units = "Amps/m"
     label = "real part of Fourier Coef"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%num_nphi_vac(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%Real_ant_coef,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%Real_ant_coef, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_rfmin_to_all
  !------------------------
  IF(ALLOCATED(ss%rfmin_to_all).AND.hcheck(1,1)) THEN
     ! I  rfmin_to_all(nspec_rfmin)
     listname = "RFMIN_TO_ALL"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = 'map rfmin index to "all" list'
     nitems = 0
     DO I1 = 1,ss%nspec_rfmin
        NAM1 = TRIM(ss%RFMIN_name(I1))
        ILAB1 = 'RF_minority: '//TRIM(ss%RFMIN_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%rfmin_to_all(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_rfmin_to_alla
  !------------------------
  IF(ALLOCATED(ss%rfmin_to_alla).AND.hcheck(1,1)) THEN
     ! I  rfmin_to_alla(nspec_rfmin)
     listname = "RFMIN_TO_ALLA"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = 'map rfmin index to "alla" list'
     nitems = 0
     DO I1 = 1,ss%nspec_rfmin
        NAM1 = TRIM(ss%RFMIN_name(I1))
        ILAB1 = 'RF_minority: '//TRIM(ss%RFMIN_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%rfmin_to_alla(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_rhopsi0lh
  !------------------------
  IF(ALLOCATED(ss%rhopsi0lh).AND.hcheck(1,1)) THEN
     ! R|units=- rhopsi0lh(nlhrf_src)
     listname = "RHOPSI0LH"
     icomponent = ps_cnum_LH
     units = "-"
     label = "need description"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%rhopsi0lh(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_rho_bdy_ns
  !------------------------
  IF(ALLOCATED(ss%rho_bdy_ns).AND.hcheck(1,1)) THEN
     ! R|units=- rho_bdy_ns(0:nspec_th)
     listname = "RHO_BDY_NS"
     icomponent = ps_cnum_PLASMA
     units = "-"
     label = "locations beyond which densities are known"
     nitems = 0
     DO I1 = 0,ss%nspec_th
        NAM1 = TRIM(ss%S_name(I1))
        ILAB1 = 'thermal_specie: '//TRIM(ss%S_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%rho_bdy_ns(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Rloc_coil
  !------------------------
  IF(ALLOCATED(ss%Rloc_coil).AND.hcheck(1,1)) THEN
     ! R|units=m  Rloc_coil(ncoils)
     listname = "RLOC_COIL"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "R location of coil, lower left corner"
     nitems = 0
     DO I1 = 1,ss%ncoils
        NAM1 = TRIM(ss%coil_name(I1))
        ILAB1 = 'pf_coils: '//TRIM(ss%coil_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Rloc_coil(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_R_ant
  !------------------------
  IF(ALLOCATED(ss%R_ant).AND.hcheck(1,1)) THEN
     ! R|units=m R_ant(nicrf_src)
     listname = "R_ANT"
     icomponent = ps_cnum_IC
     units = "m"
     label = "major radius of antenna"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%R_ant(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_R_antgeo
  !------------------------
  IF(ALLOCATED(ss%R_antgeo).AND.hcheck(1,1)) THEN
     ! R|units=m R_antgeo(nrz_antgeo(),nicrf_src)
     listname = "R_ANTGEO"
     enum_name = "R_ANTGEO__RVEC"
     icomponent = ps_cnum_IC
     units = "m"
     label = "antenna geo: R pts"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nrz_antgeo(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%R_antgeo,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%R_antgeo, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_R_EC_launch
  !------------------------
  IF(ALLOCATED(ss%R_EC_launch).AND.hcheck(1,1)) THEN
     ! R|units=m   R_EC_launch(necrf_src)
     listname = "R_EC_LAUNCH"
     icomponent = ps_cnum_EC
     units = "m"
     label = "R of center of launcher"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%R_EC_launch(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_sa_index
  !------------------------
  IF(ALLOCATED(ss%sa_index).AND.hcheck(1,1)) THEN
     ! I sa_index(0:nspec_all)
     listname = "SA_INDEX"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "species index in original lists"
     nitems = 0
     DO I1 = 0,ss%nspec_all
        NAM1 = TRIM(ss%ALL_name(I1))
        ILAB1 = 'specie: '//TRIM(ss%ALL_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%sa_index(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_sc0
  !------------------------
  IF(ALLOCATED(ss%sc0).AND.hcheck(1,1)) THEN
     ! R|units=#/sec  sc0(ngsc0)
     listname = "SC0"
     icomponent = ps_cnum_GAS
     units = "#/sec"
     label = "edge neutral sources (atoms/sec)"
     nitems = 0
     DO I1 = 1,ss%ngsc0
        NAM1 = TRIM(ss%gs_name(I1))
        ILAB1 = 'gas_source: '//TRIM(ss%gs_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%sc0(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_sc0_to_sgas
  !------------------------
  IF(ALLOCATED(ss%sc0_to_sgas).AND.hcheck(1,1)) THEN
     ! I  sc0_to_sgas(ngsc0)
     listname = "SC0_TO_SGAS"
     icomponent = ps_cnum_GAS
     units = " "
     label = "index map from neutral source to neutral species"
     nitems = 0
     DO I1 = 1,ss%ngsc0
        NAM1 = TRIM(ss%gs_name(I1))
        ILAB1 = 'gas_source: '//TRIM(ss%gs_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%sc0_to_sgas(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_sfus_to_all
  !------------------------
  IF(ALLOCATED(ss%sfus_to_all).AND.hcheck(1,1)) THEN
     ! I  sfus_to_all(nspec_fusion)
     listname = "SFUS_TO_ALL"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = 'map sfus index to "all" list'
     nitems = 0
     DO I1 = 1,ss%nspec_fusion
        NAM1 = TRIM(ss%SFUS_name(I1))
        ILAB1 = 'fusion_ion: '//TRIM(ss%SFUS_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%sfus_to_all(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_sfus_to_alla
  !------------------------
  IF(ALLOCATED(ss%sfus_to_alla).AND.hcheck(1,1)) THEN
     ! I  sfus_to_alla(nspec_fusion)
     listname = "SFUS_TO_ALLA"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = 'map sfus index to "alla" list'
     nitems = 0
     DO I1 = 1,ss%nspec_fusion
        NAM1 = TRIM(ss%SFUS_name(I1))
        ILAB1 = 'fusion_ion: '//TRIM(ss%SFUS_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%sfus_to_alla(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_sgas_to_s
  !------------------------
  IF(ALLOCATED(ss%sgas_to_s).AND.hcheck(1,1)) THEN
     ! I  sgas_to_s(nspec_gas)
     listname = "SGAS_TO_S"
     icomponent = ps_cnum_GAS
     units = " "
     label = 'map sgas index to "s" list'
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        NAM1 = TRIM(ss%SGAS_name(I1))
        ILAB1 = 'neutral_gas: '//TRIM(ss%SGAS_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%sgas_to_s(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_simp0_to_s
  !------------------------
  IF(ALLOCATED(ss%simp0_to_s).AND.hcheck(1,1)) THEN
     ! I  simp0_to_s(nspec_imp0)
     listname = "SIMP0_TO_S"
     icomponent = ps_cnum_GAS
     units = " "
     label = 'map simp0 index to "s" list'
     nitems = 0
     DO I1 = 1,ss%nspec_imp0
        NAM1 = TRIM(ss%SIMP0_name(I1))
        ILAB1 = 'impurity_atoms: '//TRIM(ss%SIMP0_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%simp0_to_s(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_simpi_to_s
  !------------------------
  IF(ALLOCATED(ss%simpi_to_s).AND.hcheck(1,1)) THEN
     ! I  simpi_to_s(nspec_impi)
     listname = "SIMPI_TO_S"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = 'map simpi index to "s" list'
     nitems = 0
     DO I1 = 1,ss%nspec_impi
        NAM1 = TRIM(ss%SIMPI_name(I1))
        ILAB1 = 'impurity_atoms: '//TRIM(ss%SIMPI_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%simpi_to_s(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_snbi_to_all
  !------------------------
  IF(ALLOCATED(ss%snbi_to_all).AND.hcheck(1,1)) THEN
     ! I  snbi_to_all(nspec_beam)
     listname = "SNBI_TO_ALL"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = 'map snbi index to "all" list'
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        NAM1 = TRIM(ss%SNBI_name(I1))
        ILAB1 = 'beam_ion: '//TRIM(ss%SNBI_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%snbi_to_all(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_snbi_to_alla
  !------------------------
  IF(ALLOCATED(ss%snbi_to_alla).AND.hcheck(1,1)) THEN
     ! I  snbi_to_alla(nspec_beam)
     listname = "SNBI_TO_ALLA"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = 'map snbi index to "alla" list'
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        NAM1 = TRIM(ss%SNBI_name(I1))
        ILAB1 = 'beam_ion: '//TRIM(ss%SNBI_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%snbi_to_alla(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_sRtcen
  !------------------------
  IF(ALLOCATED(ss%sRtcen).AND.hcheck(1,1)) THEN
     ! R|units=m       sRtcen(nbeam)
     listname = "SRTCEN"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "signed tangency radius"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%sRtcen(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_s_to_sa
  !------------------------
  IF(ALLOCATED(ss%s_to_sa).AND.hcheck(1,1)) THEN
     ! I s_to_sa(0:nspec_th)
     listname = "S_TO_SA"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "address in sa species list"
     nitems = 0
     DO I1 = 0,ss%nspec_th
        NAM1 = TRIM(ss%S_name(I1))
        ILAB1 = 'thermal_specie: '//TRIM(ss%S_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%s_to_sa(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_thegrilh
  !------------------------
  IF(ALLOCATED(ss%thegrilh).AND.hcheck(1,1)) THEN
     ! R|units=- thegrilh(nlhrf_src)
     listname = "THEGRILH"
     icomponent = ps_cnum_LH
     units = "-"
     label = "need description"
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        NAM1 = TRIM(ss%lhrf_src_name(I1))
        ILAB1 = 'lhrf_source: '//TRIM(ss%lhrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%thegrilh(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Tornum_balcrit
  !------------------------
  IF(ALLOCATED(ss%Tornum_balcrit).AND.hcheck(1,1)) THEN
     ! I   Tornum_balcrit(nballoon)
     listname = "TORNUM_BALCRIT"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "critical toroidal mode number for ballooning"
     nitems = 0
     DO I1 = 1,ss%nballoon
        NAM1 = TRIM(ss%Balloon_code(I1))
        ILAB1 = 'Balloon_codes: '//TRIM(ss%Balloon_code(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%Tornum_balcrit(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_MMODES
  !------------------------
  IF(ALLOCATED(ss%Tor_mode_label).AND.hcheck(1,1)) THEN
     ! L|toroidal_modes Tor_mode_label(mmodes)
     listname = "TOR_MODE_LABEL"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "Toroidal modes to monitor"
     nitems = 0
     DO I1 = 1,ss%mmodes
        NAM1 = TRIM(ss%Tor_mode_label(I1))
        ILAB1 = 'toroidal_modes: '//TRIM(ss%Tor_mode_label(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%Tor_mode_label(I1)
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Tor_mode_No
  !------------------------
  IF(ALLOCATED(ss%Tor_mode_No).AND.hcheck(1,1)) THEN
     ! I   Tor_mode_No(mmodes)
     listname = "TOR_MODE_NO"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "(from input) Toroidal modes monitored"
     nitems = 0
     DO I1 = 1,ss%mmodes
        NAM1 = TRIM(ss%Tor_mode_label(I1))
        ILAB1 = 'toroidal_modes: '//TRIM(ss%Tor_mode_label(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%Tor_mode_No(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_trace_flag
  !------------------------
  IF(ALLOCATED(ss%trace_flag).AND.hcheck(1,1)) THEN
     ! I  trace_flag(nbeam)
     listname = "TRACE_FLAG"
     icomponent = ps_cnum_NBI
     units = " "
     label = "trace element flag"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%trace_flag(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NTRCAT
  !------------------------
  IF(ALLOCATED(ss%trcat).AND.hcheck(1,1)) THEN
     ! L|TR_categories  trcat(ntrcat)
     listname = "TRCAT"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "Application defined list of"
     nitems = 0
     DO I1 = 1,ss%ntrcat
        NAM1 = TRIM(ss%trcat(I1))
        ILAB1 = 'TR_categories: '//TRIM(ss%trcat(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%trcat(I1)
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_trmodels
  !------------------------
  IF(ALLOCATED(ss%trmodels).AND.hcheck(1,1)) THEN
     ! C*256 trmodels(ntrcat)
     listname = "TRMODELS"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "short description or ID of transport models"
     nitems = 0
     DO I1 = 1,ss%ntrcat
        NAM1 = TRIM(ss%trcat(I1))
        ILAB1 = 'TR_categories: '//TRIM(ss%trcat(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%trmodels(I1)
     ENDDO   ! I1

     CALL ps_list_store("SIMULATION_INIT",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Ts_is_input
  !------------------------
  IF(ALLOCATED(ss%Ts_is_input).AND.hcheck(1,1)) THEN
     ! I Ts_is_input(0:nspec_th)
     listname = "TS_IS_INPUT"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "specie temperature measurement flag"
     nitems = 0
     DO I1 = 0,ss%nspec_th
        NAM1 = TRIM(ss%S_name(I1))
        ILAB1 = 'thermal_specie: '//TRIM(ss%S_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%Ts_is_input(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_vphi0_av
  !------------------------
  IF(ALLOCATED(ss%vphi0_av).AND.hcheck(1,1)) THEN
     ! R|units=m/sec vphi0_av(ngsc0)
     listname = "VPHI0_AV"
     icomponent = ps_cnum_GAS
     units = "m/sec"
     label = "average toroidal velocity of neutral source"
     nitems = 0
     DO I1 = 1,ss%ngsc0
        NAM1 = TRIM(ss%gs_name(I1))
        ILAB1 = 'gas_source: '//TRIM(ss%gs_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%vphi0_av(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_vpol_is_input
  !------------------------
  IF(ALLOCATED(ss%vpol_is_input).AND.hcheck(1,1)) THEN
     ! I vpol_is_input(0:nspec_th)
     listname = "VPOL_IS_INPUT"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "specie outer midplane poloidal velocit flag"
     nitems = 0
     DO I1 = 0,ss%nspec_th
        NAM1 = TRIM(ss%S_name(I1))
        ILAB1 = 'thermal_specie: '//TRIM(ss%S_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%vpol_is_input(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_VRep_coil_count
  !------------------------
  IF(ALLOCATED(ss%VRep_coil_count).AND.hcheck(1,1)) THEN
     ! I  VRep_coil_count(ncoils)
     listname = "VREP_COIL_COUNT"
     icomponent = ps_cnum_EQ
     units = " "
     label = "replication count, vertical coil stack"
     nitems = 0
     DO I1 = 1,ss%ncoils
        NAM1 = TRIM(ss%coil_name(I1))
        ILAB1 = 'pf_coils: '//TRIM(ss%coil_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%VRep_coil_count(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Vrep_coil_spacing
  !------------------------
  IF(ALLOCATED(ss%Vrep_coil_spacing).AND.hcheck(1,1)) THEN
     ! R|units=m Vrep_coil_spacing(ncoils)
     listname = "VREP_COIL_SPACING"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "replicated coil separation"
     nitems = 0
     DO I1 = 1,ss%ncoils
        NAM1 = TRIM(ss%coil_name(I1))
        ILAB1 = 'pf_coils: '//TRIM(ss%coil_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Vrep_coil_spacing(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Vsize_coil
  !------------------------
  IF(ALLOCATED(ss%Vsize_coil).AND.hcheck(1,1)) THEN
     ! R|units=m  Vsize_coil(ncoils)
     listname = "VSIZE_COIL"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "(Rotatable) Vertical coil size"
     nitems = 0
     DO I1 = 1,ss%ncoils
        NAM1 = TRIM(ss%coil_name(I1))
        ILAB1 = 'pf_coils: '//TRIM(ss%coil_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Vsize_coil(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_vtor_is_input
  !------------------------
  IF(ALLOCATED(ss%vtor_is_input).AND.hcheck(1,1)) THEN
     ! I vtor_is_input(0:nspec_th)
     listname = "VTOR_IS_INPUT"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "specie outer midplane toroidal velocit flag"
     nitems = 0
     DO I1 = 0,ss%nspec_th
        NAM1 = TRIM(ss%S_name(I1))
        ILAB1 = 'thermal_specie: '//TRIM(ss%S_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%vtor_is_input(I1)
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_wt_nphi
  !------------------------
  IF(ALLOCATED(ss%wt_nphi).AND.hcheck(1,1)) THEN
     ! R|units=- wt_nphi(num_nphi(),nicrf_src)
     listname = "WT_NPHI"
     enum_name = "WT_NPHI__RVEC"
     icomponent = ps_cnum_IC
     units = "-"
     label = "vacuum spectrum n_phi weight"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%num_nphi(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%wt_nphi,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%wt_nphi, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_wt_nphi_abs
  !------------------------
  IF(ALLOCATED(ss%wt_nphi_abs).AND.hcheck(1,1)) THEN
     ! R|units=- wt_nphi_abs(num_nphi(),nicrf_src)
     listname = "WT_NPHI_ABS"
     enum_name = "WT_NPHI_ABS__RVEC"
     icomponent = ps_cnum_IC
     units = "-"
     label = "absorbed spectrum n_phi weight"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%num_nphi(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%wt_nphi_abs,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%wt_nphi_abs, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_wt_nphi_ext
  !------------------------
  IF(ALLOCATED(ss%wt_nphi_ext).AND.hcheck(1,1)) THEN
     ! R|units=- wt_nphi_ext(num_nphi(),nicrf_src)
     listname = "WT_NPHI_EXT"
     enum_name = "WT_NPHI_EXT__RVEC"
     icomponent = ps_cnum_IC
     units = "-"
     label = "fraction of power deposited outside"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%num_nphi(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%wt_nphi_ext,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%wt_nphi_ext, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_xs
  !------------------------
  IF(ALLOCATED(ss%xs).AND.hcheck(1,1)) THEN
     ! R|units=#/sec    xs(nxs)
     listname = "XS"
     icomponent = ps_cnum_RAD
     units = "#/sec"
     label = "impurity source magnitude (atoms/sec)"
     nitems = 0
     DO I1 = 1,ss%nxs
        NAM1 = TRIM(ss%xs_name(I1))
        ILAB1 = 'impurity_source: '//TRIM(ss%xs_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%xs(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("STATE_DATA",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NXS
  !------------------------
  IF(ALLOCATED(ss%xs_name).AND.hcheck(1,1)) THEN
     ! L|impurity_source  xs_name(nxs)
     listname = "XS_NAME"
     icomponent = ps_cnum_RAD
     units = " "
     label = "number & name of impurity sources"
     nitems = 0
     DO I1 = 1,ss%nxs
        NAM1 = TRIM(ss%xs_name(I1))
        ILAB1 = 'impurity_source: '//TRIM(ss%xs_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = 0
        item_c256(nitems) = ss%xs_name(I1)
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Zbap
  !------------------------
  IF(ALLOCATED(ss%Zbap).AND.hcheck(1,1)) THEN
     ! R|units=m       Zbap(nbeam)
     listname = "ZBAP"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "elevation of beam centerline at aperture"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Zbap(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Zbsc
  !------------------------
  IF(ALLOCATED(ss%Zbsc).AND.hcheck(1,1)) THEN
     ! R|units=m       Zbsc(nbeam)
     listname = "ZBSC"
     icomponent = ps_cnum_NBI
     units = "m"
     label = "elevation of beam source"
     nitems = 0
     DO I1 = 1,ss%nbeam
        NAM1 = TRIM(ss%nbi_src_name(I1))
        ILAB1 = 'neutral_beams: '//TRIM(ss%nbi_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Zbsc(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Zloc_coil
  !------------------------
  IF(ALLOCATED(ss%Zloc_coil).AND.hcheck(1,1)) THEN
     ! R|units=m  Zloc_coil(ncoils)
     listname = "ZLOC_COIL"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "Z location of coil, lower left corner"
     nitems = 0
     DO I1 = 1,ss%ncoils
        NAM1 = TRIM(ss%coil_name(I1))
        ILAB1 = 'pf_coils: '//TRIM(ss%coil_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Zloc_coil(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Z_ant
  !------------------------
  IF(ALLOCATED(ss%Z_ant).AND.hcheck(1,1)) THEN
     ! R|units=m Z_ant(nicrf_src)
     listname = "Z_ANT"
     icomponent = ps_cnum_IC
     units = "m"
     label = "height of antenna"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Z_ant(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Z_antgeo
  !------------------------
  IF(ALLOCATED(ss%Z_antgeo).AND.hcheck(1,1)) THEN
     ! R|units=m Z_antgeo(nrz_antgeo(),nicrf_src)
     listname = "Z_ANTGEO"
     enum_name = "Z_ANTGEO__RVEC"
     icomponent = ps_cnum_IC
     units = "m"
     label = "antenna geo: Z pts"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%nrz_antgeo(I1)
        item_c256(nitems) = "__enumeration__ "//ilab1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN

     isize = size(ss%Z_antgeo,1)
     CALL ps_enum_2d_store(ss%iobj,enum_name,icomponent, &
          nitems,item_ivals(1:nitems),isize, &
          ss%Z_antgeo, imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Z_EC_launch
  !------------------------
  IF(ALLOCATED(ss%Z_EC_launch).AND.hcheck(1,1)) THEN
     ! R|units=m   Z_EC_launch(necrf_src)
     listname = "Z_EC_LAUNCH"
     icomponent = ps_cnum_EC
     units = "m"
     label = "Z of center of launcher"
     nitems = 0
     DO I1 = 1,ss%necrf_src
        NAM1 = TRIM(ss%ecrf_src_name(I1))
        ILAB1 = 'ecrf_source: '//TRIM(ss%ecrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Z_EC_launch(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("MACHINE_DESCRIPTION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_Z_mid_ant
  !------------------------
  IF(ALLOCATED(ss%Z_mid_ant).AND.hcheck(1,1)) THEN
     ! R|units=m Z_mid_ant(nicrf_src)
     listname = "Z_MID_ANT"
     icomponent = ps_cnum_IC
     units = "m"
     label = "center of antenna relative to TF coil midplane"
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        NAM1 = TRIM(ss%icrf_src_name(I1))
        ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = TRIM(listname)//"_"//TRIM(NAM1)
        item_r8vals(nitems) = ss%Z_mid_ant(I1)
        item_ivals(nitems) = 0
        item_c256(nitems) = ILAB1
     ENDDO   ! I1

     CALL ps_list_store("SHOT_CONFIGURATION",ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

CONTAINS

  LOGICAL FUNCTION hcheck(inum,idchk)
     INTEGER, intent(in) :: inum
     INTEGER, intent(in) :: idchk

     !  check all hash codes: set function value TRUE if any show changes.
     !  also set TRUE if idchk = 0

     INTEGER :: ia1,ia2,ii

     if(idchk.eq.0) then
        hcheck=.TRUE.
        return
     endif

     hcheck = .FALSE.
     DO ii=1,inum
        hcheck = .TRUE.
        ia1 = ahash(ii)
        ia2 = ia1 + 1
        IF(ss%hash_codes(ia1).NE.ss%new_hash_codes(ia1)) EXIT
        IF(ss%hash_codes(ia2).NE.ss%new_hash_codes(ia2)) EXIT
        IF(xhash(ia1).NE.ss%new_hash_codes(ia1)) EXIT
        IF(xhash(ia2).NE.ss%new_hash_codes(ia2)) EXIT
        hcheck = .FALSE.
     ENDDO
  END FUNCTION hcheck

  SUBROUTINE lefpack(str)
     CHARACTER*(*), intent(inout) :: str
     !  left pack non-blank characters in str

     INTEGER :: ii,jj
     ii=0
     DO jj=1,len(str)
        IF(str(jj:jj).NE.' ') THEN
           ii = ii + 1
           str(ii:ii) = str(jj:jj)
        ENDIF
     ENDDO
     str(ii+1:) = ' '
  END SUBROUTINE lefpack

END SUBROUTINE plasma_state_xload_sarrays
