
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_sgrps(ss,xhash,iout,ierr)

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

  INTEGER :: icomponent,imod,nitems
  CHARACTER*32 :: listname,units
  CHARACTER*80 :: label
  INTEGER :: ahash(ps_max_static)

  CHARACTER*32 :: item_names(max_itemlist)
  INTEGER :: item_ivals(max_itemlist)
  REAL(KIND=rspec) :: item_r8vals(max_itemlist)
  CHARACTER*256 :: item_c256(max_itemlist)
  !-----------------------

  ahash(1) = ps_shash_STATE_DATA
  IF(hcheck(1,1)) THEN
     listname = "EQ_BAXIS"
     icomponent = ps_cnum_EQ
     units = "T"
     label = "group: EQ_BAXIS; component: EQ"
     nitems = 2
     item_names(1) = "B_axis"
     item_r8vals(1) = ss%B_axis
     item_ivals(1) = 0
     item_c256(1) = " "
     item_names(2) = "B_axis_vac"
     item_r8vals(2) = ss%B_axis_vac
     item_ivals(2) = 0
     item_c256(2) = " "

     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_STATE_DATA
  IF(hcheck(1,1)) THEN
     listname = "EQ_BLCFS"
     icomponent = ps_cnum_EQ
     units = "T"
     label = "group: EQ_BLCFS; component: EQ"
     nitems = 2
     item_names(1) = "B_min_lcfs"
     item_r8vals(1) = ss%B_min_lcfs
     item_ivals(1) = 0
     item_c256(1) = " "
     item_names(2) = "B_max_lcfs"
     item_r8vals(2) = ss%B_max_lcfs
     item_ivals(2) = 0
     item_c256(2) = " "

     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_SHOT_CONFIGURATION
  IF(hcheck(1,1)) THEN
     listname = "EQ_BSIGNS"
     icomponent = ps_cnum_EQ
     units = " "
     label = "group: EQ_BSIGNS; component: EQ"
     nitems = 2
     item_names(1) = "kccw_Bphi"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%kccw_Bphi
     item_c256(1) = " "
     item_names(2) = "kccw_Jphi"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%kccw_Jphi
     item_c256(2) = " "

     CALL ps_list_store("SHOT_CONFIGURATION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_STATE_DATA
  IF(hcheck(1,1)) THEN
     listname = "EQ_FILES"
     icomponent = ps_cnum_EQ
     units = " "
     label = "group: EQ_FILES; component: EQ"
     nitems = 1
     item_names(1) = "eqdsk_file"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%eqdsk_file

     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_STATE_DATA
  IF(hcheck(1,1)) THEN
     listname = "EQ_RZAXIS"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "group: EQ_RZAXIS; component: EQ"
     nitems = 2
     item_names(1) = "R_axis"
     item_r8vals(1) = ss%R_axis
     item_ivals(1) = 0
     item_c256(1) = " "
     item_names(2) = "Z_axis"
     item_r8vals(2) = ss%Z_axis
     item_ivals(2) = 0
     item_c256(2) = " "

     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_MACHINE_DESCRIPTION
  IF(hcheck(1,1)) THEN
     listname = "EQ_RZBOX"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "group: EQ_RZBOX; component: EQ"
     nitems = 4
     item_names(1) = "R_min_box"
     item_r8vals(1) = ss%R_min_box
     item_ivals(1) = 0
     item_c256(1) = " "
     item_names(2) = "R_max_box"
     item_r8vals(2) = ss%R_max_box
     item_ivals(2) = 0
     item_c256(2) = " "
     item_names(3) = "Z_min_box"
     item_r8vals(3) = ss%Z_min_box
     item_ivals(3) = 0
     item_c256(3) = " "
     item_names(4) = "Z_max_box"
     item_r8vals(4) = ss%Z_max_box
     item_ivals(4) = 0
     item_c256(4) = " "

     CALL ps_list_store("MACHINE_DESCRIPTION", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_STATE_DATA
  IF(hcheck(1,1)) THEN
     listname = "EQ_RZLCFS"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "group: EQ_RZLCFS; component: EQ"
     nitems = 4
     item_names(1) = "R_min_lcfs"
     item_r8vals(1) = ss%R_min_lcfs
     item_ivals(1) = 0
     item_c256(1) = " "
     item_names(2) = "R_max_lcfs"
     item_r8vals(2) = ss%R_max_lcfs
     item_ivals(2) = 0
     item_c256(2) = " "
     item_names(3) = "Z_min_lcfs"
     item_r8vals(3) = ss%Z_min_lcfs
     item_ivals(3) = 0
     item_c256(3) = " "
     item_names(4) = "Z_max_lcfs"
     item_r8vals(4) = ss%Z_max_lcfs
     item_ivals(4) = 0
     item_c256(4) = " "

     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_SIMULATION_INIT
  IF(hcheck(1,1)) THEN
     listname = "ICRF_CONTROLS"
     icomponent = ps_cnum_IC
     units = " "
     label = "group: ICRF_CONTROLS; component: IC"
     nitems = 1
     item_names(1) = "kdens_rfmin"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = 0
     item_c256(1) = ss%kdens_rfmin

     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_STATE_DATA
  IF(hcheck(1,1)) THEN
     listname = "LMHD_FLAGS"
     icomponent = ps_cnum_LMHD
     units = " "
     label = "group: LMHD_FLAGS; component: LMHD"
     nitems = 2
     item_names(1) = "MHD_eq_status"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%MHD_eq_status
     item_c256(1) = " "
     item_names(2) = "LMHD_status"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%LMHD_status
     item_c256(2) = " "

     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_STATE_DATA
  IF(hcheck(1,1)) THEN
     listname = "LOCK_VARIABLES"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "group: LOCK_VARIABLES; component: PLASMA"
     nitems = 3
     item_names(1) = "lock_machine_descr"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%lock_machine_descr
     item_c256(1) = " "
     item_names(2) = "lock_shot_config"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%lock_shot_config
     item_c256(2) = " "
     item_names(3) = "lock_sim_init"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%lock_sim_init
     item_c256(3) = " "

     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_STATE_DATA
  IF(hcheck(1,1)) THEN
     listname = "RHO_BDY_SOLVE"
     icomponent = ps_cnum_PLASMA
     units = "-"
     label = "group: RHO_BDY_SOLVE; component: PLASMA"
     nitems = 3
     item_names(1) = "rho_bdy_Te"
     item_r8vals(1) = ss%rho_bdy_Te
     item_ivals(1) = 0
     item_c256(1) = " "
     item_names(2) = "rho_bdy_Ti"
     item_r8vals(2) = ss%rho_bdy_Ti
     item_ivals(2) = 0
     item_c256(2) = " "
     item_names(3) = "rho_bdy_omegat"
     item_r8vals(3) = ss%rho_bdy_omegat
     item_ivals(3) = 0
     item_c256(3) = " "

     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_STATE_DATA
  IF(hcheck(1,1)) THEN
     listname = "SURFACE_VOLTAGE"
     icomponent = ps_cnum_PLASMA
     units = "Volts"
     label = "group: SURFACE_VOLTAGE; component: PLASMA"
     nitems = 1
     item_names(1) = "vsur"
     item_r8vals(1) = ss%vsur
     item_ivals(1) = 0
     item_c256(1) = " "

     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_STATE_DATA
  IF(hcheck(1,1)) THEN
     listname = "TEMPERATURE_BCS"
     icomponent = ps_cnum_PLASMA
     units = "KeV"
     label = "group: TEMPERATURE_BCS; component: PLASMA"
     nitems = 2
     item_names(1) = "Te_bdy"
     item_r8vals(1) = ss%Te_bdy
     item_ivals(1) = 0
     item_c256(1) = " "
     item_names(2) = "Ti_bdy"
     item_r8vals(2) = ss%Ti_bdy
     item_ivals(2) = 0
     item_c256(2) = " "

     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_SIMULATION_INIT
  IF(hcheck(1,1)) THEN
     listname = "TIME_RANGE"
     icomponent = ps_cnum_PLASMA
     units = "sec"
     label = "group: TIME_RANGE; component: PLASMA"
     nitems = 2
     item_names(1) = "tinit"
     item_r8vals(1) = ss%tinit
     item_ivals(1) = 0
     item_c256(1) = " "
     item_names(2) = "tfinal"
     item_r8vals(2) = ss%tfinal
     item_ivals(2) = 0
     item_c256(2) = " "

     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_STATE_DATA
  IF(hcheck(1,1)) THEN
     listname = "TIME_STEP"
     icomponent = ps_cnum_PLASMA
     units = "sec"
     label = "group: TIME_STEP; component: PLASMA"
     nitems = 2
     item_names(1) = "t0"
     item_r8vals(1) = ss%t0
     item_ivals(1) = 0
     item_c256(1) = " "
     item_names(2) = "t1"
     item_r8vals(2) = ss%t1
     item_ivals(2) = 0
     item_c256(2) = " "

     CALL ps_list_store("STATE_DATA", ss%iobj, &
          listname,icomponent,units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_shash_SIMULATION_INIT
  IF(hcheck(1,1)) THEN
     listname = "ZIMP_SA"
     icomponent = ps_cnum_PLASMA
     units = " "
     label = "group: ZIMP_SA; component: PLASMA"
     nitems = 2
     item_names(1) = "Zimp1"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%Zimp1
     item_c256(1) = " "
     item_names(2) = "Zimp2"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%Zimp2
     item_c256(2) = " "

     CALL ps_list_store("SIMULATION_INIT", ss%iobj, &
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

END SUBROUTINE plasma_state_xload_sgrps
