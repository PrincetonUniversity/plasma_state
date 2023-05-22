
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_parrays(ss,xhash,iout,ierr)

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:

  INTEGER, PARAMETER :: max_itemlist = ps_max_static

  INTEGER :: icomponent,imod,jmod,nitems,idchk
  CHARACTER*32 :: listname,units,list_units,profname,iiencod
  CHARACTER*80 :: label,plabel,list_label
  INTEGER :: ahash(ps_max_static)

  CHARACTER*32 :: item_names(max_itemlist)
  INTEGER :: item_ivals(max_itemlist)
  REAL(KIND=rspec) :: item_r8vals(max_itemlist)
  CHARACTER*256 :: item_c256(max_itemlist)

  INTEGER :: i1,i2,i3,i4,isize
  CHARACTER*32 :: nam1,nam2,nam3,nam4
  CHARACTER*80 :: ilab1,ilab2,ilab3,ilab4

  !-----------------------
  ! for enumeration labeling...
  CHARACTER*32, DIMENSION(:), ALLOCATABLE :: enum_lbl_1d
  CHARACTER*32, DIMENSION(:,:), ALLOCATABLE :: enum_lbl_NPHI
   
  !----------------------------------------------------
  ! generated integer enumeration labels
  isize = 1
  isize = max(isize,ss%num_rzlim)
  isize = max(isize,ss%n_h_einj_standard)
  isize = max(isize,ss%n_d_einj_standard)
  isize = max(isize,ss%n_t_einj_standard)
  isize = max(isize,ss%max_nrz_antgeo)
  isize = max(isize,ss%max_n_straps)
  isize = max(isize,ss%max_num_nphi_vac)
  isize = max(isize,ss%max_nspectrumlh_src)
  isize = max(isize,ss%nky_anomth)
  isize = max(isize,ss%nky_mmm)

  ALLOCATE(enum_lbl_1d(isize))
  DO i1 = 1,isize
     enum_lbl_1d(i1) = " "
     WRITE(enum_lbl_1d(i1),"(I8)") i1
     CALL lefpack(enum_lbl_1d(i1))
  ENDDO

  IF(ALLOCATED(ss%nphi)) THEN
     ALLOCATE(enum_lbl_nphi(ss%max_num_nphi,ss%nicrf_src))
     enum_lbl_nphi = " "
     CALL load_lbl_2d(ss%num_nphi,ss%nphi,enum_lbl_nphi)
  ENDIF


  ahash(1) = ps_phash_BIDIFF_BPASS_CO
  idchk = 0
  IF(ALLOCATED(ss%id_bidiff_bpass_co)) idchk = ss%id_bidiff_bpass_co(1)
  IF(ALLOCATED(ss%bidiff_bpass_co).AND.hcheck(1,idchk)) THEN
     listname = "BIDIFF_BPASS_CO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "beam ion diffusivity: co barely passing orbits"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: bidiff_bpass_co"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "bidiff_bpass_co_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "bidiff_bpass_co_"//TRIM(nam1)
        CALL ps_2d_prof_store(ss%iobj,profname,"E_ANOM2","RHO_ANOM2", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nefi_anom2,ss%nrho_anom2,0,0, &
             ss%bidiff_bpass_co(:,:,I1),ss%id_bidiff_bpass_co(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_bidiff_bpass_co(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BIDIFF_BPASS_CTR
  idchk = 0
  IF(ALLOCATED(ss%id_bidiff_bpass_ctr)) idchk = ss%id_bidiff_bpass_ctr(1)
  IF(ALLOCATED(ss%bidiff_bpass_ctr).AND.hcheck(1,idchk)) THEN
     listname = "BIDIFF_BPASS_CTR_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "beam ion diffusivity: ctr barely passing orbits"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: bidiff_bpass_ctr"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "bidiff_bpass_ctr_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "bidiff_bpass_ctr_"//TRIM(nam1)
        CALL ps_2d_prof_store(ss%iobj,profname,"E_ANOM2","RHO_ANOM2", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nefi_anom2,ss%nrho_anom2,0,0, &
             ss%bidiff_bpass_ctr(:,:,I1),ss%id_bidiff_bpass_ctr(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_bidiff_bpass_ctr(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BIDIFF_BTRAP
  idchk = 0
  IF(ALLOCATED(ss%id_bidiff_btrap)) idchk = ss%id_bidiff_btrap(1)
  IF(ALLOCATED(ss%bidiff_btrap).AND.hcheck(1,idchk)) THEN
     listname = "BIDIFF_BTRAP_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "beam ion diffusivity: barely trapped orbits"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: bidiff_btrap"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "bidiff_btrap_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "bidiff_btrap_"//TRIM(nam1)
        CALL ps_2d_prof_store(ss%iobj,profname,"E_ANOM2","RHO_ANOM2", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nefi_anom2,ss%nrho_anom2,0,0, &
             ss%bidiff_btrap(:,:,I1),ss%id_bidiff_btrap(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_bidiff_btrap(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BIDIFF_DPASS_CO
  idchk = 0
  IF(ALLOCATED(ss%id_bidiff_dpass_co)) idchk = ss%id_bidiff_dpass_co(1)
  IF(ALLOCATED(ss%bidiff_dpass_co).AND.hcheck(1,idchk)) THEN
     listname = "BIDIFF_DPASS_CO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "beam ion diffusivity: co passing |vpll/v| -> 1"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: bidiff_dpass_co"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "bidiff_dpass_co_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "bidiff_dpass_co_"//TRIM(nam1)
        CALL ps_2d_prof_store(ss%iobj,profname,"E_ANOM2","RHO_ANOM2", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nefi_anom2,ss%nrho_anom2,0,0, &
             ss%bidiff_dpass_co(:,:,I1),ss%id_bidiff_dpass_co(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_bidiff_dpass_co(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BIDIFF_DPASS_CTR
  idchk = 0
  IF(ALLOCATED(ss%id_bidiff_dpass_ctr)) idchk = ss%id_bidiff_dpass_ctr(1)
  IF(ALLOCATED(ss%bidiff_dpass_ctr).AND.hcheck(1,idchk)) THEN
     listname = "BIDIFF_DPASS_CTR_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "beam ion diffusivity: ctr passing |vpll/v| -> 1"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: bidiff_dpass_ctr"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "bidiff_dpass_ctr_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "bidiff_dpass_ctr_"//TRIM(nam1)
        CALL ps_2d_prof_store(ss%iobj,profname,"E_ANOM2","RHO_ANOM2", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nefi_anom2,ss%nrho_anom2,0,0, &
             ss%bidiff_dpass_ctr(:,:,I1),ss%id_bidiff_dpass_ctr(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_bidiff_dpass_ctr(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BIDIFF_DTRAP
  idchk = 0
  IF(ALLOCATED(ss%id_bidiff_dtrap)) idchk = ss%id_bidiff_dtrap(1)
  IF(ALLOCATED(ss%bidiff_dtrap).AND.hcheck(1,idchk)) THEN
     listname = "BIDIFF_DTRAP_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "beam ion diffusivity: deeply trapped orbits"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: bidiff_dtrap"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "bidiff_dtrap_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "bidiff_dtrap_"//TRIM(nam1)
        CALL ps_2d_prof_store(ss%iobj,profname,"E_ANOM2","RHO_ANOM2", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nefi_anom2,ss%nrho_anom2,0,0, &
             ss%bidiff_dtrap(:,:,I1),ss%id_bidiff_dtrap(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_bidiff_dtrap(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CDICRF
  idchk = 0
  IF(ALLOCATED(ss%id_cdicrf)) idchk = ss%id_cdicrf(1)
  IF(ALLOCATED(ss%cdicrf).AND.hcheck(1,idchk)) THEN
     listname = "CDICRF_LIST"
     icomponent = ps_cnum_IC
     units = "A"
     label = "ICRF current drive"
     ! for assoc. scalar list:
     list_units = "A"
     list_label = "area int: cdicrf"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "cdicrf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "cdicrf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ICRF", &
             units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
             ss%nrho_icrf-1,0, &
             ss%cdicrf(:,I1),ss%id_cdicrf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_cdicrf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_TRANSCAT
  idchk = 0
  IF(ALLOCATED(ss%id_chie_transcat)) idchk = ss%id_chie_transcat(1)
  IF(ALLOCATED(ss%chie_transcat).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m^2/sec"
     label = "Electron energy diffusivity"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: chie_transcat"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "chie_transcat_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "chie_transcat_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho,0, &
             ss%chie_transcat(:,I1),ss%id_chie_transcat(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_chie_transcat(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_TRANSCAT
  idchk = 0
  IF(ALLOCATED(ss%id_chii_transcat)) idchk = ss%id_chii_transcat(1)
  IF(ALLOCATED(ss%chii_transcat).AND.hcheck(1,idchk)) THEN
     listname = "CHII_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m^2/sec"
     label = "Ion energy diffusivity"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: chii_transcat"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "chii_transcat_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "chii_transcat_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho,0, &
             ss%chii_transcat(:,I1),ss%id_chii_transcat(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_chii_transcat(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIMO_TRANSCAT
  idchk = 0
  IF(ALLOCATED(ss%id_chimo_transcat)) idchk = ss%id_chimo_transcat(1)
  IF(ALLOCATED(ss%chimo_transcat).AND.hcheck(1,idchk)) THEN
     listname = "CHIMO_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m^2/sec"
     label = "Angular momentum diffusivity"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: chimo_transcat"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "chimo_transcat_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "chimo_transcat_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho,0, &
             ss%chimo_transcat(:,I1),ss%id_chimo_transcat(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_chimo_transcat(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURECH_SRC
  idchk = 0
  IF(ALLOCATED(ss%id_curech_src)) idchk = ss%id_curech_src(1)
  IF(ALLOCATED(ss%curech_src).AND.hcheck(1,idchk)) THEN
     listname = "CURECH_SRC_LIST"
     icomponent = ps_cnum_EC
     units = "A"
     label = "ECH current drive (by antenna)"
     ! for assoc. scalar list:
     list_units = "A"
     list_label = "area int: curech_src"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "curech_src_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "curech_src_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ECRF", &
             units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
             ss%nrho_ecrf-1,0, &
             ss%curech_src(:,I1),ss%id_curech_src(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_curech_src(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURLH_SRC
  idchk = 0
  IF(ALLOCATED(ss%id_curlh_src)) idchk = ss%id_curlh_src(1)
  IF(ALLOCATED(ss%curlh_src).AND.hcheck(1,idchk)) THEN
     listname = "CURLH_SRC_LIST"
     icomponent = ps_cnum_LH
     units = "A"
     label = "LH current drive (by antenna)"
     ! for assoc. scalar list:
     list_units = "A"
     list_label = "area int: curlh_src"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "curlh_src_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "curlh_src_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_LHRF", &
             units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
             ss%nrho_lhrf-1,0, &
             ss%curlh_src(:,I1),ss%id_curlh_src(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_curlh_src(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFF_TRANS
  idchk = 0
  IF(ALLOCATED(ss%id_diff_trans)) idchk = ss%id_diff_trans(1)
  IF(ALLOCATED(ss%diff_trans).AND.hcheck(1,idchk)) THEN
     listname = "DIFF_TRANS_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m^2/sec"
     label = "specie ptcl diffusivity"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: diff_trans"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "diff_trans_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "diff_trans_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho,0, &
             ss%diff_trans(:,I1),ss%id_diff_trans(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_diff_trans(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFF_TRANSCAT
  idchk = 0
  IF(ALLOCATED(ss%id_diff_transcat)) idchk = ss%id_diff_transcat(1,1)
  IF(ALLOCATED(ss%diff_transcat).AND.hcheck(1,idchk)) THEN
     listname = "DIFF_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m^2/sec"
     label = "specie diffusivity"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: diff_transcat"

     nitems = 0
     jmod = 0
     DO I2 = 1,ss%ntrcat
        NAM2 = TRIM(ss%trcat(I2))
        ILAB2 = 'TR_categories: '//TRIM(ss%trcat(I2))
        DO I1 = 0,ss%nspec_th
           NAM1 = TRIM(ss%S_name(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'thermal_specie: '//TRIM(ss%S_name(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = "diff_transcat_"//TRIM(NAM1)
           item_c256(nitems) = ILAB1
           plabel = TRIM(label)//": "//TRIM(nam1)
           profname = "diff_transcat_"//TRIM(nam1)
           CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
                units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
                ss%nrho,0, &
                ss%diff_transcat(:,I1,I2),ss%id_diff_transcat(I1,I2), &
                imod,iout,ierr)
           IF(ierr.NE.0) RETURN
           IF(imod.gt.0) jmod = 1
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%id_diff_transcat(I1,I2)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_ANOM
  idchk = 0
  IF(ALLOCATED(ss%id_difns_anom)) idchk = ss%id_difns_anom(1)
  IF(ALLOCATED(ss%difns_anom).AND.hcheck(1,idchk)) THEN
     listname = "DIFNS_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "density-equations, diffusivity term"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: difns_anom"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "difns_anom_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "difns_anom_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%difns_anom(:,I1),ss%id_difns_anom(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_difns_anom(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_DRBM
  idchk = 0
  IF(ALLOCATED(ss%id_difns_drbm)) idchk = ss%id_difns_drbm(1)
  IF(ALLOCATED(ss%difns_drbm).AND.hcheck(1,idchk)) THEN
     listname = "DIFNS_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "density-equations, diffusivity term"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: difns_drbm"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "difns_drbm_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "difns_drbm_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%difns_drbm(:,I1),ss%id_difns_drbm(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_difns_drbm(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_GLF
  idchk = 0
  IF(ALLOCATED(ss%id_difns_glf)) idchk = ss%id_difns_glf(1)
  IF(ALLOCATED(ss%difns_glf).AND.hcheck(1,idchk)) THEN
     listname = "DIFNS_GLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     label = "density-equations, diffusivity from GLF23 model"
     ! for assoc. scalar list:
     list_units = "m^2/s"
     list_label = "simple avg: difns_glf"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "difns_glf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "difns_glf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%difns_glf(:,I1),ss%id_difns_glf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_difns_glf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_GTCNEO
  idchk = 0
  IF(ALLOCATED(ss%id_difns_gtcneo)) idchk = ss%id_difns_gtcneo(1)
  IF(ALLOCATED(ss%difns_gtcneo).AND.hcheck(1,idchk)) THEN
     listname = "DIFNS_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     label = "density-equations, neoclassical diffusivity from GTC_NEO model"
     ! for assoc. scalar list:
     list_units = "m^2/s"
     list_label = "simple avg: difns_gtcneo"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "difns_gtcneo_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "difns_gtcneo_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%difns_gtcneo(:,I1),ss%id_difns_gtcneo(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_difns_gtcneo(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_MMM71
  idchk = 0
  IF(ALLOCATED(ss%id_difns_mmm71)) idchk = ss%id_difns_mmm71(1)
  IF(ALLOCATED(ss%difns_mmm71).AND.hcheck(1,idchk)) THEN
     listname = "DIFNS_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "density-equations, diffusivity term"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: difns_mmm71"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "difns_mmm71_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "difns_mmm71_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%difns_mmm71(:,I1),ss%id_difns_mmm71(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_difns_mmm71(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_NCLASS
  idchk = 0
  IF(ALLOCATED(ss%id_difns_nclass)) idchk = ss%id_difns_nclass(1)
  IF(ALLOCATED(ss%difns_nclass).AND.hcheck(1,idchk)) THEN
     listname = "DIFNS_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     label = "density-equations, neoclassical diffusivity from NCLASS model"
     ! for assoc. scalar list:
     list_units = "m^2/s"
     list_label = "simple avg: difns_nclass"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "difns_nclass_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "difns_nclass_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%difns_nclass(:,I1),ss%id_difns_nclass(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_difns_nclass(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_NEO
  idchk = 0
  IF(ALLOCATED(ss%id_difns_neo)) idchk = ss%id_difns_neo(1)
  IF(ALLOCATED(ss%difns_neo).AND.hcheck(1,idchk)) THEN
     listname = "DIFNS_NEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     label = "density-equations, neoclassical diffusivity from NEO model"
     ! for assoc. scalar list:
     list_units = "m^2/s"
     list_label = "simple avg: difns_neo"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "difns_neo_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "difns_neo_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%difns_neo(:,I1),ss%id_difns_neo(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_difns_neo(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_difns_tglf)) idchk = ss%id_difns_tglf(1)
  IF(ALLOCATED(ss%difns_tglf).AND.hcheck(1,idchk)) THEN
     listname = "DIFNS_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     label = "density-equations, diffusivity from TGLF model"
     ! for assoc. scalar list:
     list_units = "m^2/s"
     list_label = "simple avg: difns_tglf"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "difns_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "difns_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%difns_tglf(:,I1),ss%id_difns_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_difns_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_W19
  idchk = 0
  IF(ALLOCATED(ss%id_difns_w19)) idchk = ss%id_difns_w19(1)
  IF(ALLOCATED(ss%difns_w19).AND.hcheck(1,idchk)) THEN
     listname = "DIFNS_W19_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "density-equations, diffusivity term"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: difns_w19"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "difns_w19_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "difns_w19_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%difns_w19(:,I1),ss%id_difns_w19(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_difns_w19(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPERP_BEAMI
  idchk = 0
  IF(ALLOCATED(ss%id_eperp_beami)) idchk = ss%id_eperp_beami(1)
  IF(ALLOCATED(ss%eperp_beami).AND.hcheck(1,idchk)) THEN
     listname = "EPERP_BEAMI_LIST"
     icomponent = ps_cnum_NBI
     units = "keV"
     label = "beam species <Eperp>, lab frame"
     ! for assoc. scalar list:
     list_units = "keV"
     list_label = "wght avg (density): EPERP"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "EPERP_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "EPERP_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_nbi-1,0, &
             ss%eperp_beami(:,I1),ss%id_eperp_beami(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_eperp_beami(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPERP_FUSI
  idchk = 0
  IF(ALLOCATED(ss%id_eperp_fusi)) idchk = ss%id_eperp_fusi(1)
  IF(ALLOCATED(ss%eperp_fusi).AND.hcheck(1,idchk)) THEN
     listname = "EPERP_FUSI_LIST"
     icomponent = ps_cnum_FUS
     units = "keV"
     label = "fusion ion <Eperp>, lab frame"
     ! for assoc. scalar list:
     list_units = "keV"
     list_label = "wght avg (density): EPERP"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "EPERP_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "EPERP_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_FUS", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_fus-1,0, &
             ss%eperp_fusi(:,I1),ss%id_eperp_fusi(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_eperp_fusi(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPERP_MINI
  idchk = 0
  IF(ALLOCATED(ss%id_eperp_mini)) idchk = ss%id_eperp_mini(1)
  IF(ALLOCATED(ss%eperp_mini).AND.hcheck(1,idchk)) THEN
     listname = "EPERP_MINI_LIST"
     icomponent = ps_cnum_IC
     units = "keV"
     label = "minority species <Eperp>, lab frame"
     ! for assoc. scalar list:
     list_units = "keV"
     list_label = "wght avg (density): EPERP"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "EPERP_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "EPERP_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ICRF", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_icrf-1,0, &
             ss%eperp_mini(:,I1),ss%id_eperp_mini(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_eperp_mini(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPLL_BEAMI
  idchk = 0
  IF(ALLOCATED(ss%id_epll_beami)) idchk = ss%id_epll_beami(1)
  IF(ALLOCATED(ss%epll_beami).AND.hcheck(1,idchk)) THEN
     listname = "EPLL_BEAMI_LIST"
     icomponent = ps_cnum_NBI
     units = "keV"
     label = "beam species <Epll>, lab frame"
     ! for assoc. scalar list:
     list_units = "keV"
     list_label = "wght avg (density): EPLL"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "EPLL_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "EPLL_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_nbi-1,0, &
             ss%epll_beami(:,I1),ss%id_epll_beami(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_epll_beami(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPLL_FUSI
  idchk = 0
  IF(ALLOCATED(ss%id_epll_fusi)) idchk = ss%id_epll_fusi(1)
  IF(ALLOCATED(ss%epll_fusi).AND.hcheck(1,idchk)) THEN
     listname = "EPLL_FUSI_LIST"
     icomponent = ps_cnum_FUS
     units = "keV"
     label = "fusion ion <Epll>, lab frame"
     ! for assoc. scalar list:
     list_units = "keV"
     list_label = "wght avg (density): EPLL"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "EPLL_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "EPLL_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_FUS", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_fus-1,0, &
             ss%epll_fusi(:,I1),ss%id_epll_fusi(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_epll_fusi(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPLL_MINI
  idchk = 0
  IF(ALLOCATED(ss%id_epll_mini)) idchk = ss%id_epll_mini(1)
  IF(ALLOCATED(ss%epll_mini).AND.hcheck(1,idchk)) THEN
     listname = "EPLL_MINI_LIST"
     icomponent = ps_cnum_IC
     units = "keV"
     label = "minority species <Epll>, lab frame"
     ! for assoc. scalar list:
     list_units = "keV"
     list_label = "wght avg (density): EPLL"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "EPLL_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "EPLL_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ICRF", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_icrf-1,0, &
             ss%epll_mini(:,I1),ss%id_epll_mini(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_epll_mini(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FIDIFF_BPASS_CO
  idchk = 0
  IF(ALLOCATED(ss%id_fidiff_bpass_co)) idchk = ss%id_fidiff_bpass_co(1)
  IF(ALLOCATED(ss%fidiff_bpass_co).AND.hcheck(1,idchk)) THEN
     listname = "FIDIFF_BPASS_CO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "fusion ion diffusivity: co barely passing orbits"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: fidiff_bpass_co"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "fidiff_bpass_co_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "fidiff_bpass_co_"//TRIM(nam1)
        CALL ps_2d_prof_store(ss%iobj,profname,"E_ANOM2","RHO_ANOM2", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nefi_anom2,ss%nrho_anom2,0,0, &
             ss%fidiff_bpass_co(:,:,I1),ss%id_fidiff_bpass_co(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_fidiff_bpass_co(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FIDIFF_BPASS_CTR
  idchk = 0
  IF(ALLOCATED(ss%id_fidiff_bpass_ctr)) idchk = ss%id_fidiff_bpass_ctr(1)
  IF(ALLOCATED(ss%fidiff_bpass_ctr).AND.hcheck(1,idchk)) THEN
     listname = "FIDIFF_BPASS_CTR_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "fusion ion diffusivity: ctr barely passing orbits"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: fidiff_bpass_ctr"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "fidiff_bpass_ctr_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "fidiff_bpass_ctr_"//TRIM(nam1)
        CALL ps_2d_prof_store(ss%iobj,profname,"E_ANOM2","RHO_ANOM2", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nefi_anom2,ss%nrho_anom2,0,0, &
             ss%fidiff_bpass_ctr(:,:,I1),ss%id_fidiff_bpass_ctr(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_fidiff_bpass_ctr(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FIDIFF_BTRAP
  idchk = 0
  IF(ALLOCATED(ss%id_fidiff_btrap)) idchk = ss%id_fidiff_btrap(1)
  IF(ALLOCATED(ss%fidiff_btrap).AND.hcheck(1,idchk)) THEN
     listname = "FIDIFF_BTRAP_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "fusion ion diffusivity: barely trapped orbits"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: fidiff_btrap"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "fidiff_btrap_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "fidiff_btrap_"//TRIM(nam1)
        CALL ps_2d_prof_store(ss%iobj,profname,"E_ANOM2","RHO_ANOM2", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nefi_anom2,ss%nrho_anom2,0,0, &
             ss%fidiff_btrap(:,:,I1),ss%id_fidiff_btrap(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_fidiff_btrap(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FIDIFF_DPASS_CO
  idchk = 0
  IF(ALLOCATED(ss%id_fidiff_dpass_co)) idchk = ss%id_fidiff_dpass_co(1)
  IF(ALLOCATED(ss%fidiff_dpass_co).AND.hcheck(1,idchk)) THEN
     listname = "FIDIFF_DPASS_CO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "fusion ion diffusivity: co passing |vpll/v| -> 1"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: fidiff_dpass_co"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "fidiff_dpass_co_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "fidiff_dpass_co_"//TRIM(nam1)
        CALL ps_2d_prof_store(ss%iobj,profname,"E_ANOM2","RHO_ANOM2", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nefi_anom2,ss%nrho_anom2,0,0, &
             ss%fidiff_dpass_co(:,:,I1),ss%id_fidiff_dpass_co(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_fidiff_dpass_co(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FIDIFF_DPASS_CTR
  idchk = 0
  IF(ALLOCATED(ss%id_fidiff_dpass_ctr)) idchk = ss%id_fidiff_dpass_ctr(1)
  IF(ALLOCATED(ss%fidiff_dpass_ctr).AND.hcheck(1,idchk)) THEN
     listname = "FIDIFF_DPASS_CTR_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "fusion ion diffusivity: ctr passing |vpll/v| -> 1"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: fidiff_dpass_ctr"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "fidiff_dpass_ctr_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "fidiff_dpass_ctr_"//TRIM(nam1)
        CALL ps_2d_prof_store(ss%iobj,profname,"E_ANOM2","RHO_ANOM2", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nefi_anom2,ss%nrho_anom2,0,0, &
             ss%fidiff_dpass_ctr(:,:,I1),ss%id_fidiff_dpass_ctr(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_fidiff_dpass_ctr(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FIDIFF_DTRAP
  idchk = 0
  IF(ALLOCATED(ss%id_fidiff_dtrap)) idchk = ss%id_fidiff_dtrap(1)
  IF(ALLOCATED(ss%fidiff_dtrap).AND.hcheck(1,idchk)) THEN
     listname = "FIDIFF_DTRAP_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     label = "fusion ion diffusivity: deeply trapped orbits"
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     list_label = "simple avg: fidiff_dtrap"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "fidiff_dtrap_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "fidiff_dtrap_"//TRIM(nam1)
        CALL ps_2d_prof_store(ss%iobj,profname,"E_ANOM2","RHO_ANOM2", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nefi_anom2,ss%nrho_anom2,0,0, &
             ss%fidiff_dtrap(:,:,I1),ss%id_fidiff_dtrap(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_fidiff_dtrap(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_MMM71
  idchk = 0
  IF(ALLOCATED(ss%id_freq_mmm71)) idchk = ss%id_freq_mmm71(1)
  IF(ALLOCATED(ss%freq_mmm71).AND.hcheck(1,idchk)) THEN
     listname = "FREQ_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     units = "1/sec"
     label = "frequency from mmm71 model"
     ! for assoc. scalar list:
     list_units = "1/sec"
     list_label = "simple avg: freq_mmm71"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_mmm
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "freq_mmm71_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "freq_mmm71_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%freq_mmm71(:,I1),ss%id_freq_mmm71(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_freq_mmm71(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_freq_tglf)) idchk = ss%id_freq_tglf(1)
  IF(ALLOCATED(ss%freq_tglf).AND.hcheck(1,idchk)) THEN
     listname = "FREQ_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "frequency from TGLF model"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: freq_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "freq_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "freq_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%freq_tglf(:,I1),ss%id_freq_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_freq_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_MMM71
  idchk = 0
  IF(ALLOCATED(ss%id_growthrate_mmm71)) idchk = ss%id_growthrate_mmm71(1)
  IF(ALLOCATED(ss%growthrate_mmm71).AND.hcheck(1,idchk)) THEN
     listname = "GROWTHRATE_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     units = "1/sec"
     label = "growthrate from mmm71 model"
     ! for assoc. scalar list:
     list_units = "1/sec"
     list_label = "simple avg: growthrate_mmm71"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_mmm
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "growthrate_mmm71_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "growthrate_mmm71_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%growthrate_mmm71(:,I1),ss%id_growthrate_mmm71(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_growthrate_mmm71(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_growthrate_tglf)) idchk = ss%id_growthrate_tglf(1)
  IF(ALLOCATED(ss%growthrate_tglf).AND.hcheck(1,idchk)) THEN
     listname = "GROWTHRATE_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "growthrate from TGLF model"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: growthrate_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "growthrate_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "growthrate_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%growthrate_tglf(:,I1),ss%id_growthrate_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_growthrate_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_VPAR
  idchk = 0
  IF(ALLOCATED(ss%id_growthrate_vpar)) idchk = ss%id_growthrate_vpar(1)
  IF(ALLOCATED(ss%growthrate_vpar).AND.hcheck(1,idchk)) THEN
     listname = "GROWTHRATE_VPAR_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "growthrate from parallel velocity shear"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: growthrate_vpar"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "growthrate_vpar_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "growthrate_vpar_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%growthrate_vpar(:,I1),ss%id_growthrate_vpar(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_growthrate_vpar(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_KYSPECTRUM_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_kyspectrum_tglf)) idchk = ss%id_kyspectrum_tglf(1)
  IF(ALLOCATED(ss%kyspectrum_tglf).AND.hcheck(1,idchk)) THEN
     listname = "KYSPECTRUM_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "-"
     label = "ky-spectrum from TGLF model"
     ! for assoc. scalar list:
     list_units = "-"
     list_label = "simple avg: kyspectrum_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "kyspectrum_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "kyspectrum_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%kyspectrum_tglf(:,I1),ss%id_kyspectrum_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_kyspectrum_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_N0NORM
  idchk = 0
  IF(ALLOCATED(ss%id_n0norm)) idchk = ss%id_n0norm(1,1)
  IF(ALLOCATED(ss%n0norm).AND.hcheck(1,idchk)) THEN
     listname = "N0NORM_LIST"
     icomponent = ps_cnum_GAS
     units = "(m^-3)/(#/sec)"
     label = "neutral density per unit gas influx (sc0)"
     ! for assoc. scalar list:
     list_units = "(m^-3)/(#/sec)"
     list_label = "simple avg: n0norm"

     nitems = 0
     jmod = 0
     DO I2 = 1,ss%ngsc0
        NAM2 = TRIM(ss%gs_name(I2))
        ILAB2 = 'gas_source: '//TRIM(ss%gs_name(I2))
        DO I1 = 1,ss%nspec_gas
           NAM1 = TRIM(ss%SGAS_name(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'neutral_gas: '//TRIM(ss%SGAS_name(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = "n0norm_"//TRIM(NAM1)
           item_c256(nitems) = ILAB1
           plabel = TRIM(label)//": "//TRIM(nam1)
           profname = "n0norm_"//TRIM(nam1)
           CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
                units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
                ss%nrho_gas-1,0, &
                ss%n0norm(:,I1,I2),ss%id_n0norm(I1,I2), &
                imod,iout,ierr)
           IF(ierr.NE.0) RETURN
           IF(imod.gt.0) jmod = 1
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%id_n0norm(I1,I2)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_N0_HALO
  idchk = 0
  IF(ALLOCATED(ss%id_n0_halo)) idchk = ss%id_n0_halo(1)
  IF(ALLOCATED(ss%n0_halo).AND.hcheck(1,idchk)) THEN
     listname = "N0_HALO_LIST"
     icomponent = ps_cnum_NBI
     units = "m^-3"
     label = "halo thermal neutral density"
     ! for assoc. scalar list:
     list_units = "#"
     list_label = "vol int: n0_halo"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "n0_halo_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "n0_halo_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_nbi-1,0, &
             ss%n0_halo(:,I1),ss%id_n0_halo(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_n0_halo(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_N0_RECO
  idchk = 0
  IF(ALLOCATED(ss%id_n0_reco)) idchk = ss%id_n0_reco(1)
  IF(ALLOCATED(ss%n0_reco).AND.hcheck(1,idchk)) THEN
     listname = "N0_RECO_LIST"
     icomponent = ps_cnum_GAS
     units = "m^-3"
     label = "reco thermal neutral density"
     ! for assoc. scalar list:
     list_units = "#"
     list_label = "vol int: n0_reco"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "n0_reco_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "n0_reco_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_gas-1,0, &
             ss%n0_reco(:,I1),ss%id_n0_reco(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_n0_reco(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NBEAMI
  idchk = 0
  IF(ALLOCATED(ss%id_nbeami)) idchk = ss%id_nbeami(1)
  IF(ALLOCATED(ss%nbeami).AND.hcheck(1,idchk)) THEN
     listname = "NBEAMI_LIST"
     icomponent = ps_cnum_NBI
     units = "m^-3"
     label = "beam species density"
     ! for assoc. scalar list:
     list_units = "#"
     list_label = "vol int: N"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "N"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "N"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_nbi-1,0, &
             ss%nbeami(:,I1),ss%id_nbeami(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_nbeami(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NEFLUX_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_neflux_ky_tglf)) idchk = ss%id_neflux_ky_tglf(1)
  IF(ALLOCATED(ss%neflux_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "NEFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "electron density flux spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: neflux_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "neflux_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "neflux_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%neflux_ky_tglf(:,I1),ss%id_neflux_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_neflux_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NE_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_ne_ky_tglf)) idchk = ss%id_ne_ky_tglf(1)
  IF(ALLOCATED(ss%ne_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "NE_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "electron density fluctuation spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: ne_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "ne_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "ne_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%ne_ky_tglf(:,I1),ss%id_ne_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_ne_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NFUSI
  idchk = 0
  IF(ALLOCATED(ss%id_nfusi)) idchk = ss%id_nfusi(1)
  IF(ALLOCATED(ss%nfusi).AND.hcheck(1,idchk)) THEN
     listname = "NFUSI_LIST"
     icomponent = ps_cnum_FUS
     units = "m^-3"
     label = "fusion ion density"
     ! for assoc. scalar list:
     list_units = "#"
     list_label = "vol int: N"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "N"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "N"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_FUS", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_fus-1,0, &
             ss%nfusi(:,I1),ss%id_nfusi(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_nfusi(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NIFLUX_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_niflux_ky_tglf)) idchk = ss%id_niflux_ky_tglf(1)
  IF(ALLOCATED(ss%niflux_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "NIFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "ion density flux spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: niflux_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "niflux_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "niflux_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%niflux_ky_tglf(:,I1),ss%id_niflux_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_niflux_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NI_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_ni_ky_tglf)) idchk = ss%id_ni_ky_tglf(1)
  IF(ALLOCATED(ss%ni_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "NI_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "ion density fluctuation spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: ni_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "ni_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "ni_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%ni_ky_tglf(:,I1),ss%id_ni_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_ni_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NMINI
  idchk = 0
  IF(ALLOCATED(ss%id_nmini)) idchk = ss%id_nmini(1)
  IF(ALLOCATED(ss%nmini).AND.hcheck(1,idchk)) THEN
     listname = "NMINI_LIST"
     icomponent = ps_cnum_IC
     units = "m^-3"
     label = "minority species density"
     ! for assoc. scalar list:
     list_units = "#"
     list_label = "vol int: N"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "N"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "N"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ICRF", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_icrf-1,0, &
             ss%nmini(:,I1),ss%id_nmini(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_nmini(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NS
  idchk = 0
  IF(ALLOCATED(ss%id_ns)) idchk = ss%id_ns(1)
  IF(ALLOCATED(ss%ns).AND.hcheck(1,idchk)) THEN
     listname = "NS_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m^-3"
     label = "thermal specie density"
     ! for assoc. scalar list:
     list_units = "#"
     list_label = "vol int: N"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "N"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "N"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho-1,0, &
             ss%ns(:,I1),ss%id_ns(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_ns(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NZFLUX_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_nzflux_ky_tglf)) idchk = ss%id_nzflux_ky_tglf(1)
  IF(ALLOCATED(ss%nzflux_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "NZFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "impurity density flux spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: nzflux_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "nzflux_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "nzflux_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%nzflux_ky_tglf(:,I1),ss%id_nzflux_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_nzflux_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NZ_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_nz_ky_tglf)) idchk = ss%id_nz_ky_tglf(1)
  IF(ALLOCATED(ss%nz_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "NZ_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "impurity density fluctuation spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: nz_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "nz_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "nz_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%nz_ky_tglf(:,I1),ss%id_nz_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_nz_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_OMEG0CX
  idchk = 0
  IF(ALLOCATED(ss%id_omeg0cx)) idchk = ss%id_omeg0cx(1)
  IF(ALLOCATED(ss%omeg0cx).AND.hcheck(1,idchk)) THEN
     listname = "OMEG0CX_LIST"
     icomponent = ps_cnum_GAS
     units = "rad/sec"
     label = "effective neutral angular velocity for CX torque"
     ! for assoc. scalar list:
     list_units = "rad/sec"
     list_label = "wght avg (TQQCX): omeg0cx"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "omeg0cx_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "omeg0cx_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_gas-1,0, &
             ss%omeg0cx(:,I1),ss%id_omeg0cx(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_omeg0cx(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_OMEG0SC0
  idchk = 0
  IF(ALLOCATED(ss%id_omeg0sc0)) idchk = ss%id_omeg0sc0(1,1)
  IF(ALLOCATED(ss%omeg0sc0).AND.hcheck(1,idchk)) THEN
     listname = "OMEG0SC0_LIST"
     icomponent = ps_cnum_GAS
     units = "(rad/sec)"
     label = "toroidal angular velocity"
     ! for assoc. scalar list:
     list_units = "(rad/sec)"
     list_label = "wght avg (N0NORM): omeg0sc0"

     nitems = 0
     jmod = 0
     DO I2 = 1,ss%ngsc0
        NAM2 = TRIM(ss%gs_name(I2))
        ILAB2 = 'gas_source: '//TRIM(ss%gs_name(I2))
        DO I1 = 1,ss%nspec_gas
           NAM1 = TRIM(ss%SGAS_name(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'neutral_gas: '//TRIM(ss%SGAS_name(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = "omeg0sc0_"//TRIM(NAM1)
           item_c256(nitems) = ILAB1
           plabel = TRIM(label)//": "//TRIM(nam1)
           profname = "omeg0sc0_"//TRIM(nam1)
           CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
                units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
                ss%nrho_gas-1,0, &
                ss%omeg0sc0(:,I1,I2),ss%id_omeg0sc0(I1,I2), &
                imod,iout,ierr)
           IF(ierr.NE.0) RETURN
           IF(imod.gt.0) jmod = 1
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%id_omeg0sc0(I1,I2)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_OMEG0_HALO
  idchk = 0
  IF(ALLOCATED(ss%id_omeg0_halo)) idchk = ss%id_omeg0_halo(1)
  IF(ALLOCATED(ss%omeg0_halo).AND.hcheck(1,idchk)) THEN
     listname = "OMEG0_HALO_LIST"
     icomponent = ps_cnum_NBI
     units = "rad/sec"
     label = "halo neutral toroidal angular velocity"
     ! for assoc. scalar list:
     list_units = "rad/sec"
     list_label = "wght avg (N0_HALO): omeg0_halo"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "omeg0_halo_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "omeg0_halo_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_nbi-1,0, &
             ss%omeg0_halo(:,I1),ss%id_omeg0_halo(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_omeg0_halo(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_OMEG0_RECO
  idchk = 0
  IF(ALLOCATED(ss%id_omeg0_reco)) idchk = ss%id_omeg0_reco(1)
  IF(ALLOCATED(ss%omeg0_reco).AND.hcheck(1,idchk)) THEN
     listname = "OMEG0_RECO_LIST"
     icomponent = ps_cnum_GAS
     units = "rad/sec"
     label = "reco neutral toroidal angular velocity"
     ! for assoc. scalar list:
     list_units = "rad/sec"
     list_label = "wght avg (N0_RECO): omeg0_reco"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "omeg0_reco_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "omeg0_reco_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_gas-1,0, &
             ss%omeg0_reco(:,I1),ss%id_omeg0_reco(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_omeg0_reco(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PEECH_SRC
  idchk = 0
  IF(ALLOCATED(ss%id_peech_src)) idchk = ss%id_peech_src(1)
  IF(ALLOCATED(ss%peech_src).AND.hcheck(1,idchk)) THEN
     listname = "PEECH_SRC_LIST"
     icomponent = ps_cnum_EC
     units = "W"
     label = "ECH heating (by antenna)"
     ! for assoc. scalar list:
     list_units = "W"
     list_label = "vol int: peech_src"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "peech_src_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "peech_src_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ECRF", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_ecrf-1,0, &
             ss%peech_src(:,I1),ss%id_peech_src(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_peech_src(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PELH_SRC
  idchk = 0
  IF(ALLOCATED(ss%id_pelh_src)) idchk = ss%id_pelh_src(1)
  IF(ALLOCATED(ss%pelh_src).AND.hcheck(1,idchk)) THEN
     listname = "PELH_SRC_LIST"
     icomponent = ps_cnum_LH
     units = "W"
     label = "LH electron heating by antenna"
     ! for assoc. scalar list:
     list_units = "W"
     list_label = "vol int: pelh_src"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "pelh_src_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "pelh_src_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_LHRF", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_lhrf-1,0, &
             ss%pelh_src(:,I1),ss%id_pelh_src(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_pelh_src(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PE_TRANSCAT
  idchk = 0
  IF(ALLOCATED(ss%id_pe_transcat)) idchk = ss%id_pe_transcat(1)
  IF(ALLOCATED(ss%pe_transcat).AND.hcheck(1,idchk)) THEN
     listname = "PE_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "W"
     label = "electron transport"
     ! for assoc. scalar list:
     list_units = "W"
     list_label = "vol int: pe_transcat"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "pe_transcat_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "pe_transcat_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho-1,0, &
             ss%pe_transcat(:,I1),ss%id_pe_transcat(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_pe_transcat(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PHIFLUX_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_phiflux_ky_tglf)) idchk = ss%id_phiflux_ky_tglf(1)
  IF(ALLOCATED(ss%phiflux_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "PHIFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "momentum fluctuation spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: phiflux_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "phiflux_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "phiflux_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%phiflux_ky_tglf(:,I1),ss%id_phiflux_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_phiflux_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PICRF_SRCS
  idchk = 0
  IF(ALLOCATED(ss%id_picrf_srcs)) idchk = ss%id_picrf_srcs(1,1)
  IF(ALLOCATED(ss%picrf_srcs).AND.hcheck(1,idchk)) THEN
     listname = "PICRF_SRCS_LIST"
     icomponent = ps_cnum_IC
     units = "W"
     label = "direct ICRF power deposition"
     ! for assoc. scalar list:
     list_units = "W"
     list_label = "vol int: PICRFS"

     nitems = 0
     jmod = 0
     DO I2 = 0,ss%nspec_alla
        NAM2 = TRIM(ss%ALLA_name(I2))
        ILAB2 = 'specie: '//TRIM(ss%ALLA_name(I2))
        DO I1 = 1,ss%nicrf_src
           NAM1 = TRIM(ss%icrf_src_name(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'icrf_source: '//TRIM(ss%icrf_src_name(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = "PICRFS_"//TRIM(NAM1)
           item_c256(nitems) = ILAB1
           plabel = TRIM(label)//": "//TRIM(nam1)
           profname = "PICRFS_"//TRIM(nam1)
           CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ICRF", &
                units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
                ss%nrho_icrf-1,0, &
                ss%picrf_srcs(:,I1,I2),ss%id_picrf_srcs(I1,I2), &
                imod,iout,ierr)
           IF(ierr.NE.0) RETURN
           IF(imod.gt.0) jmod = 1
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%id_picrf_srcs(I1,I2)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PICRF_TOTALS
  idchk = 0
  IF(ALLOCATED(ss%id_picrf_totals)) idchk = ss%id_picrf_totals(1)
  IF(ALLOCATED(ss%picrf_totals).AND.hcheck(1,idchk)) THEN
     listname = "PICRF_TOTALS_LIST"
     icomponent = ps_cnum_IC
     units = "W"
     label = "direct ICRF power deposition"
     ! for assoc. scalar list:
     list_units = "W"
     list_label = "vol int: PICRF"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "PICRF_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "PICRF_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ICRF", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_icrf-1,0, &
             ss%picrf_totals(:,I1),ss%id_picrf_totals(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_picrf_totals(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PILH_SRC
  idchk = 0
  IF(ALLOCATED(ss%id_pilh_src)) idchk = ss%id_pilh_src(1)
  IF(ALLOCATED(ss%pilh_src).AND.hcheck(1,idchk)) THEN
     listname = "PILH_SRC_LIST"
     icomponent = ps_cnum_LH
     units = "W"
     label = "LH ion heating by antenna"
     ! for assoc. scalar list:
     list_units = "W"
     list_label = "vol int: pilh_src"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "pilh_src_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "pilh_src_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_LHRF", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_lhrf-1,0, &
             ss%pilh_src(:,I1),ss%id_pilh_src(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_pilh_src(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PI_TRANSCAT
  idchk = 0
  IF(ALLOCATED(ss%id_pi_transcat)) idchk = ss%id_pi_transcat(1)
  IF(ALLOCATED(ss%pi_transcat).AND.hcheck(1,idchk)) THEN
     listname = "PI_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "W"
     label = "ion transport"
     ! for assoc. scalar list:
     list_units = "W"
     list_label = "vol int: pi_transcat"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "pi_transcat_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "pi_transcat_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho-1,0, &
             ss%pi_transcat(:,I1),ss%id_pi_transcat(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_pi_transcat(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_POT_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_pot_ky_tglf)) idchk = ss%id_pot_ky_tglf(1)
  IF(ALLOCATED(ss%pot_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "POT_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "potential fluctuation spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: pot_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "pot_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "pot_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%pot_ky_tglf(:,I1),ss%id_pot_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_pot_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PSMOM_NC
  idchk = 0
  IF(ALLOCATED(ss%id_psmom_nc)) idchk = ss%id_psmom_nc(1)
  IF(ALLOCATED(ss%psmom_nc).AND.hcheck(1,idchk)) THEN
     listname = "PSMOM_NC_LIST"
     icomponent = ps_cnum_EQ
     units = "m^-2"
     label = "PS moments"
     ! for assoc. scalar list:
     list_units = "m^-2"
     list_label = "vol avg: psmom_nc"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "psmom_nc_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "psmom_nc_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_EQ_GEO", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_eq_geo,0, &
             ss%psmom_nc(:,I1),ss%id_psmom_nc(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_psmom_nc(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_QBEAMI
  idchk = 0
  IF(ALLOCATED(ss%id_qbeami)) idchk = ss%id_qbeami(1)
  IF(ALLOCATED(ss%qbeami).AND.hcheck(1,idchk)) THEN
     listname = "QBEAMI_LIST"
     icomponent = ps_cnum_NBI
     units = "C"
     label = "average beam ion charge"
     ! for assoc. scalar list:
     list_units = "C"
     list_label = "wght avg (density): qbeami"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "qbeami_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "qbeami_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_nbi-1,0, &
             ss%qbeami(:,I1),ss%id_qbeami(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_qbeami(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_QIONIZ
  idchk = 0
  IF(ALLOCATED(ss%id_qioniz)) idchk = ss%id_qioniz(1)
  IF(ALLOCATED(ss%qioniz).AND.hcheck(1,idchk)) THEN
     listname = "QIONIZ_LIST"
     icomponent = ps_cnum_GAS
     units = "W/(#/sec)"
     label = "ionization power per unit influx (sc0)"
     ! for assoc. scalar list:
     list_units = "W/(#/sec)"
     list_label = "vol int: qioniz"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "qioniz_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "qioniz_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_gas-1,0, &
             ss%qioniz(:,I1),ss%id_qioniz(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_qioniz(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_QQCX
  idchk = 0
  IF(ALLOCATED(ss%id_qqcx)) idchk = ss%id_qqcx(1)
  IF(ALLOCATED(ss%qqcx).AND.hcheck(1,idchk)) THEN
     listname = "QQCX_LIST"
     icomponent = ps_cnum_GAS
     units = "W/(#/sec)/keV"
     label = "cx power per unit influx per (T0-Ti)"
     ! for assoc. scalar list:
     list_units = "W/(#/sec)/keV"
     list_label = "vol int: qqcx"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "qqcx_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "qqcx_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_gas-1,0, &
             ss%qqcx(:,I1),ss%id_qqcx(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_qqcx(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RATE_SINB0I
  idchk = 0
  IF(ALLOCATED(ss%id_rate_sinb0i)) idchk = ss%id_rate_sinb0i(1)
  IF(ALLOCATED(ss%rate_sinb0i).AND.hcheck(1,idchk)) THEN
     listname = "RATE_SINB0I_LIST"
     icomponent = ps_cnum_NBI
     units = "1/sec"
     label = "sink rate due to ionization by beam ions"
     ! for assoc. scalar list:
     list_units = "1/sec"
     list_label = "simple avg: rate_sinb0i"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "rate_sinb0i_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "rate_sinb0i_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_nbi-1,0, &
             ss%rate_sinb0i(:,I1),ss%id_rate_sinb0i(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_rate_sinb0i(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RATE_SINB0X
  idchk = 0
  IF(ALLOCATED(ss%id_rate_sinb0x)) idchk = ss%id_rate_sinb0x(1)
  IF(ALLOCATED(ss%rate_sinb0x).AND.hcheck(1,idchk)) THEN
     listname = "RATE_SINB0X_LIST"
     icomponent = ps_cnum_NBI
     units = "1/sec"
     label = "sink rate due to CX with beam ions (total)"
     ! for assoc. scalar list:
     list_units = "1/sec"
     list_label = "simple avg: rate_sinb0x"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "rate_sinb0x_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "rate_sinb0x_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_nbi-1,0, &
             ss%rate_sinb0x(:,I1),ss%id_rate_sinb0x(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_rate_sinb0x(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RATE_SINB0XS
  idchk = 0
  IF(ALLOCATED(ss%id_rate_sinb0xs)) idchk = ss%id_rate_sinb0xs(1,1)
  IF(ALLOCATED(ss%rate_sinb0xs).AND.hcheck(1,idchk)) THEN
     listname = "RATE_SINB0XS_LIST"
     icomponent = ps_cnum_NBI
     units = "1/sec"
     label = "sink rate due to CX with beam ions"
     ! for assoc. scalar list:
     list_units = "1/sec"
     list_label = "simple avg: rate_sinb0xs"

     nitems = 0
     jmod = 0
     DO I2 = 1,ss%nspec_beam
        NAM2 = TRIM(ss%SNBI_name(I2))
        ILAB2 = 'beam_ion: '//TRIM(ss%SNBI_name(I2))
        DO I1 = 1,ss%nspec_gas
           NAM1 = TRIM(ss%SGAS_name(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'neutral_gas: '//TRIM(ss%SGAS_name(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = "rate_sinb0xs_"//TRIM(NAM1)
           item_c256(nitems) = ILAB1
           plabel = TRIM(label)//": "//TRIM(nam1)
           profname = "rate_sinb0xs_"//TRIM(nam1)
           CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
                units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
                ss%nrho_nbi-1,0, &
                ss%rate_sinb0xs(:,I1,I2),ss%id_rate_sinb0xs(I1,I2), &
                imod,iout,ierr)
           IF(ierr.NE.0) RETURN
           IF(imod.gt.0) jmod = 1
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%id_rate_sinb0xs(I1,I2)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RATE_SINF0I
  idchk = 0
  IF(ALLOCATED(ss%id_rate_sinf0i)) idchk = ss%id_rate_sinf0i(1)
  IF(ALLOCATED(ss%rate_sinf0i).AND.hcheck(1,idchk)) THEN
     listname = "RATE_SINF0I_LIST"
     icomponent = ps_cnum_FUS
     units = "1/sec"
     label = "sink rate due to ionization by fusion ions"
     ! for assoc. scalar list:
     list_units = "1/sec"
     list_label = "simple avg: rate_sinf0i"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "rate_sinf0i_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "rate_sinf0i_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_FUS", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_fus-1,0, &
             ss%rate_sinf0i(:,I1),ss%id_rate_sinf0i(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_rate_sinf0i(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RATE_SINF0X
  idchk = 0
  IF(ALLOCATED(ss%id_rate_sinf0x)) idchk = ss%id_rate_sinf0x(1)
  IF(ALLOCATED(ss%rate_sinf0x).AND.hcheck(1,idchk)) THEN
     listname = "RATE_SINF0X_LIST"
     icomponent = ps_cnum_FUS
     units = "1/sec"
     label = "sink rate due to CX with fusion ions (total)"
     ! for assoc. scalar list:
     list_units = "1/sec"
     list_label = "simple avg: rate_sinf0x"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "rate_sinf0x_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "rate_sinf0x_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_FUS", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_fus-1,0, &
             ss%rate_sinf0x(:,I1),ss%id_rate_sinf0x(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_rate_sinf0x(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RATE_SINF0XS
  idchk = 0
  IF(ALLOCATED(ss%id_rate_sinf0xs)) idchk = ss%id_rate_sinf0xs(1,1)
  IF(ALLOCATED(ss%rate_sinf0xs).AND.hcheck(1,idchk)) THEN
     listname = "RATE_SINF0XS_LIST"
     icomponent = ps_cnum_FUS
     units = "1/sec"
     label = "sink rate due to CX with fusion ions"
     ! for assoc. scalar list:
     list_units = "1/sec"
     list_label = "simple avg: rate_sinf0xs"

     nitems = 0
     jmod = 0
     DO I2 = 1,ss%nspec_fusion
        NAM2 = TRIM(ss%SFUS_name(I2))
        ILAB2 = 'fusion_ion: '//TRIM(ss%SFUS_name(I2))
        DO I1 = 1,ss%nspec_gas
           NAM1 = TRIM(ss%SGAS_name(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'neutral_gas: '//TRIM(ss%SGAS_name(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = "rate_sinf0xs_"//TRIM(NAM1)
           item_c256(nitems) = ILAB1
           plabel = TRIM(label)//": "//TRIM(nam1)
           profname = "rate_sinf0xs_"//TRIM(nam1)
           CALL ps_1d_prof_store(ss%iobj,profname,"RHO_FUS", &
                units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
                ss%nrho_fus-1,0, &
                ss%rate_sinf0xs(:,I1,I2),ss%id_rate_sinf0xs(I1,I2), &
                imod,iout,ierr)
           IF(ierr.NE.0) RETURN
           IF(imod.gt.0) jmod = 1
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%id_rate_sinf0xs(I1,I2)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RES_SN
  idchk = 0
  IF(ALLOCATED(ss%id_res_sn)) idchk = ss%id_res_sn(1)
  IF(ALLOCATED(ss%res_sn).AND.hcheck(1,idchk)) THEN
     listname = "RES_SN_LIST"
     icomponent = ps_cnum_PLASMA
     units = "Norm"
     label = "residuals for particle transport eqn"
     ! for assoc. scalar list:
     list_units = "Norm"
     list_label = "vol int: res_sn"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "res_sn_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "res_sn_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho-1,0, &
             ss%res_sn(:,I1),ss%id_res_sn(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_res_sn(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_S0RECO
  idchk = 0
  IF(ALLOCATED(ss%id_s0reco)) idchk = ss%id_s0reco(1)
  IF(ALLOCATED(ss%s0reco).AND.hcheck(1,idchk)) THEN
     listname = "S0RECO_LIST"
     icomponent = ps_cnum_GAS
     units = "#/sec"
     label = "recombination neutral sce"
     ! for assoc. scalar list:
     list_units = "#/sec"
     list_label = "vol int: s0reco"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "s0reco_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "s0reco_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_gas-1,0, &
             ss%s0reco(:,I1),ss%id_s0reco(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_s0reco(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_S0RECO_RECAP
  idchk = 0
  IF(ALLOCATED(ss%id_s0reco_recap)) idchk = ss%id_s0reco_recap(1)
  IF(ALLOCATED(ss%s0reco_recap).AND.hcheck(1,idchk)) THEN
     listname = "S0RECO_RECAP_LIST"
     icomponent = ps_cnum_GAS
     units = "#/sec"
     label = "recombination neutrals recapture"
     ! for assoc. scalar list:
     list_units = "#/sec"
     list_label = "vol int: s0reco_recap"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "s0reco_recap_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "s0reco_recap_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_gas-1,0, &
             ss%s0reco_recap(:,I1),ss%id_s0reco_recap(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_s0reco_recap(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SB0HALO
  idchk = 0
  IF(ALLOCATED(ss%id_sb0halo)) idchk = ss%id_sb0halo(1)
  IF(ALLOCATED(ss%sb0halo).AND.hcheck(1,idchk)) THEN
     listname = "SB0HALO_LIST"
     icomponent = ps_cnum_NBI
     units = "#/sec"
     label = "beam dep cx sink"
     ! for assoc. scalar list:
     list_units = "#/sec"
     list_label = "vol int: sb0halo"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "sb0halo_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "sb0halo_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_nbi-1,0, &
             ss%sb0halo(:,I1),ss%id_sb0halo(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_sb0halo(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SB0HALO_RECAP
  idchk = 0
  IF(ALLOCATED(ss%id_sb0halo_recap)) idchk = ss%id_sb0halo_recap(1)
  IF(ALLOCATED(ss%sb0halo_recap).AND.hcheck(1,idchk)) THEN
     listname = "SB0HALO_RECAP_LIST"
     icomponent = ps_cnum_NBI
     units = "#/sec"
     label = "cx neutral recapture"
     ! for assoc. scalar list:
     list_units = "#/sec"
     list_label = "vol int: sb0halo_recap"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "sb0halo_recap_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "sb0halo_recap_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_nbi-1,0, &
             ss%sb0halo_recap(:,I1),ss%id_sb0halo_recap(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_sb0halo_recap(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SBSCE
  idchk = 0
  IF(ALLOCATED(ss%id_sbsce)) idchk = ss%id_sbsce(1)
  IF(ALLOCATED(ss%sbsce).AND.hcheck(1,idchk)) THEN
     listname = "SBSCE_LIST"
     icomponent = ps_cnum_NBI
     units = "#/sec"
     label = "net source, thermal ions & electrons"
     ! for assoc. scalar list:
     list_units = "#/sec"
     list_label = "vol int: sbsce"

     nitems = 0
     jmod = 0
     DO I1 = 0,ss%nspec_tha
        NAM1 = TRIM(ss%SA_name(I1))
        ILAB1 = 'thermal_specie: '//TRIM(ss%SA_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "sbsce_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "sbsce_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_nbi-1,0, &
             ss%sbsce(:,I1),ss%id_sbsce(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_sbsce(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SBTHERM
  idchk = 0
  IF(ALLOCATED(ss%id_sbtherm)) idchk = ss%id_sbtherm(1)
  IF(ALLOCATED(ss%sbtherm).AND.hcheck(1,idchk)) THEN
     listname = "SBTHERM_LIST"
     icomponent = ps_cnum_NBI
     units = "#/sec"
     label = "beam ion thermalization"
     ! for assoc. scalar list:
     list_units = "#/sec"
     list_label = "vol int: sbtherm"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "sbtherm_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "sbtherm_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_nbi-1,0, &
             ss%sbtherm(:,I1),ss%id_sbtherm(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_sbtherm(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SC_ANOM
  idchk = 0
  IF(ALLOCATED(ss%id_sc_anom)) idchk = ss%id_sc_anom(1)
  IF(ALLOCATED(ss%sc_anom).AND.hcheck(1,idchk)) THEN
     listname = "SC_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     units = "#/sec"
     label = "anomalous sources"
     ! for assoc. scalar list:
     list_units = "#/sec"
     list_label = "vol int: sc_anom"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "sc_anom_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "sc_anom_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_anomth-1,0, &
             ss%sc_anom(:,I1),ss%id_sc_anom(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_sc_anom(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SFSCE
  idchk = 0
  IF(ALLOCATED(ss%id_sfsce)) idchk = ss%id_sfsce(1)
  IF(ALLOCATED(ss%sfsce).AND.hcheck(1,idchk)) THEN
     listname = "SFSCE_LIST"
     icomponent = ps_cnum_FUS
     units = "#/sec"
     label = "net thermalization source of fusion ions"
     ! for assoc. scalar list:
     list_units = "#/sec"
     list_label = "vol int: sfsce"

     nitems = 0
     jmod = 0
     DO I1 = 0,ss%nspec_tha
        NAM1 = TRIM(ss%SA_name(I1))
        ILAB1 = 'thermal_specie: '//TRIM(ss%SA_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "sfsce_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "sfsce_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_FUS", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_fus-1,0, &
             ss%sfsce(:,I1),ss%id_sfsce(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_sfsce(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SFTHERM
  idchk = 0
  IF(ALLOCATED(ss%id_sftherm)) idchk = ss%id_sftherm(1)
  IF(ALLOCATED(ss%sftherm).AND.hcheck(1,idchk)) THEN
     listname = "SFTHERM_LIST"
     icomponent = ps_cnum_FUS
     units = "#/sec"
     label = "fusion ion thermalization"
     ! for assoc. scalar list:
     list_units = "#/sec"
     list_label = "vol int: sftherm"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "sftherm_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "sftherm_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_FUS", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_fus-1,0, &
             ss%sftherm(:,I1),ss%id_sftherm(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_sftherm(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SII_XS
  idchk = 0
  IF(ALLOCATED(ss%id_sii_xs)) idchk = ss%id_sii_xs(1,1)
  IF(ALLOCATED(ss%sii_xs).AND.hcheck(1,idchk)) THEN
     listname = "SII_XS_LIST"
     icomponent = ps_cnum_RAD
     units = "(#/sec)/(#/sec)"
     label = "species source/sinks driven by xs(1:nxs)"
     ! for assoc. scalar list:
     list_units = "(#/sec)/(#/sec)"
     list_label = "vol int: sii_xs"

     nitems = 0
     jmod = 0
     DO I2 = 1,ss%nxs
        NAM2 = TRIM(ss%xs_name(I2))
        ILAB2 = 'impurity_source: '//TRIM(ss%xs_name(I2))
        DO I1 = 0,ss%nspec_th
           NAM1 = TRIM(ss%S_name(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'thermal_specie: '//TRIM(ss%S_name(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = "sii_xs_"//TRIM(NAM1)
           item_c256(nitems) = ILAB1
           plabel = TRIM(label)//": "//TRIM(nam1)
           profname = "sii_xs_"//TRIM(nam1)
           CALL ps_1d_prof_store(ss%iobj,profname,"RHO_MIST", &
                units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
                ss%nrho_mist-1,0, &
                ss%sii_xs(:,I1,I2),ss%id_sii_xs(I1,I2), &
                imod,iout,ierr)
           IF(ierr.NE.0) RETURN
           IF(imod.gt.0) jmod = 1
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%id_sii_xs(I1,I2)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SN_TRANS
  idchk = 0
  IF(ALLOCATED(ss%id_sn_trans)) idchk = ss%id_sn_trans(1)
  IF(ALLOCATED(ss%sn_trans).AND.hcheck(1,idchk)) THEN
     listname = "SN_TRANS_LIST"
     icomponent = ps_cnum_PLASMA
     units = "#/sec"
     label = "particle transport (loss)"
     ! for assoc. scalar list:
     list_units = "#/sec"
     list_label = "vol int: sn_trans"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "sn_trans_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "sn_trans_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho-1,0, &
             ss%sn_trans(:,I1),ss%id_sn_trans(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_sn_trans(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SN_TRANSCAT
  idchk = 0
  IF(ALLOCATED(ss%id_sn_transcat)) idchk = ss%id_sn_transcat(1,1)
  IF(ALLOCATED(ss%sn_transcat).AND.hcheck(1,idchk)) THEN
     listname = "SN_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "#/sec"
     label = "species transport"
     ! for assoc. scalar list:
     list_units = "#/sec"
     list_label = "vol int: sn_transcat"

     nitems = 0
     jmod = 0
     DO I2 = 1,ss%ntrcat
        NAM2 = TRIM(ss%trcat(I2))
        ILAB2 = 'TR_categories: '//TRIM(ss%trcat(I2))
        DO I1 = 0,ss%nspec_th
           NAM1 = TRIM(ss%S_name(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'thermal_specie: '//TRIM(ss%S_name(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = "sn_transcat_"//TRIM(NAM1)
           item_c256(nitems) = ILAB1
           plabel = TRIM(label)//": "//TRIM(nam1)
           profname = "sn_transcat_"//TRIM(nam1)
           CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
                units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
                ss%nrho-1,0, &
                ss%sn_transcat(:,I1,I2),ss%id_sn_transcat(I1,I2), &
                imod,iout,ierr)
           IF(ierr.NE.0) RETURN
           IF(imod.gt.0) jmod = 1
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%id_sn_transcat(I1,I2)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SPROF0
  idchk = 0
  IF(ALLOCATED(ss%id_sprof0)) idchk = ss%id_sprof0(1,1)
  IF(ALLOCATED(ss%sprof0).AND.hcheck(1,idchk)) THEN
     listname = "SPROF0_LIST"
     icomponent = ps_cnum_GAS
     units = "(#/sec)/(#/sec)"
     label = "gas species source/sinks per unit species influx"
     ! for assoc. scalar list:
     list_units = "(#/sec)/(#/sec)"
     list_label = "vol int: sprof0"

     nitems = 0
     jmod = 0
     DO I2 = 1,ss%ngsc0
        NAM2 = TRIM(ss%gs_name(I2))
        ILAB2 = 'gas_source: '//TRIM(ss%gs_name(I2))
        DO I1 = 1,ss%nspec_gas
           NAM1 = TRIM(ss%SGAS_name(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'neutral_gas: '//TRIM(ss%SGAS_name(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = "sprof0_"//TRIM(NAM1)
           item_c256(nitems) = ILAB1
           plabel = TRIM(label)//": "//TRIM(nam1)
           profname = "sprof0_"//TRIM(nam1)
           CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
                units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
                ss%nrho_gas-1,0, &
                ss%sprof0(:,I1,I2),ss%id_sprof0(I1,I2), &
                imod,iout,ierr)
           IF(ierr.NE.0) RETURN
           IF(imod.gt.0) jmod = 1
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%id_sprof0(I1,I2)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SPROF0E
  idchk = 0
  IF(ALLOCATED(ss%id_sprof0e)) idchk = ss%id_sprof0e(1)
  IF(ALLOCATED(ss%sprof0e).AND.hcheck(1,idchk)) THEN
     listname = "SPROF0E_LIST"
     icomponent = ps_cnum_GAS
     units = "(#/sec)/(#/sec)"
     label = "electron source per unit neutral species influx"
     ! for assoc. scalar list:
     list_units = "(#/sec)/(#/sec)"
     list_label = "vol int: sprof0e"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "sprof0e_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "sprof0e_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_gas-1,0, &
             ss%sprof0e(:,I1),ss%id_sprof0e(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_sprof0e(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SPSCE
  idchk = 0
  IF(ALLOCATED(ss%id_spsce)) idchk = ss%id_spsce(1)
  IF(ALLOCATED(ss%spsce).AND.hcheck(1,idchk)) THEN
     listname = "SPSCE_LIST"
     icomponent = ps_cnum_PEL
     units = "#/sec"
     label = "net source, thermal ions & electrons"
     ! for assoc. scalar list:
     list_units = "#/sec"
     list_label = "vol int: spsce"

     nitems = 0
     jmod = 0
     DO I1 = 0,ss%nspec_tha
        NAM1 = TRIM(ss%SA_name(I1))
        ILAB1 = 'thermal_specie: '//TRIM(ss%SA_name(I1))
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "spsce_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "spsce_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_PEL", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_pel-1,0, &
             ss%spsce(:,I1),ss%id_spsce(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_spsce(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_T0CX
  idchk = 0
  IF(ALLOCATED(ss%id_T0cx)) idchk = ss%id_T0cx(1)
  IF(ALLOCATED(ss%T0cx).AND.hcheck(1,idchk)) THEN
     listname = "T0CX_LIST"
     icomponent = ps_cnum_GAS
     units = "keV"
     label = "effective neutral temperature for CX power"
     ! for assoc. scalar list:
     list_units = "keV"
     list_label = "wght avg (QQCX): T0cx"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "T0cx_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "T0cx_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_gas-1,0, &
             ss%T0cx(:,I1),ss%id_T0cx(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_T0cx(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_T0SC0
  idchk = 0
  IF(ALLOCATED(ss%id_T0sc0)) idchk = ss%id_T0sc0(1,1)
  IF(ALLOCATED(ss%T0sc0).AND.hcheck(1,idchk)) THEN
     listname = "T0SC0_LIST"
     icomponent = ps_cnum_GAS
     units = "keV"
     label = "neutral temperature (2/3)<E0>"
     ! for assoc. scalar list:
     list_units = "keV"
     list_label = "wght avg (N0NORM): T0sc0"

     nitems = 0
     jmod = 0
     DO I2 = 1,ss%ngsc0
        NAM2 = TRIM(ss%gs_name(I2))
        ILAB2 = 'gas_source: '//TRIM(ss%gs_name(I2))
        DO I1 = 1,ss%nspec_gas
           NAM1 = TRIM(ss%SGAS_name(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'neutral_gas: '//TRIM(ss%SGAS_name(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = "T0sc0_"//TRIM(NAM1)
           item_c256(nitems) = ILAB1
           plabel = TRIM(label)//": "//TRIM(nam1)
           profname = "T0sc0_"//TRIM(nam1)
           CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
                units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
                ss%nrho_gas-1,0, &
                ss%T0sc0(:,I1,I2),ss%id_T0sc0(I1,I2), &
                imod,iout,ierr)
           IF(ierr.NE.0) RETURN
           IF(imod.gt.0) jmod = 1
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%id_T0sc0(I1,I2)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_T0_HALO
  idchk = 0
  IF(ALLOCATED(ss%id_T0_halo)) idchk = ss%id_T0_halo(1)
  IF(ALLOCATED(ss%T0_halo).AND.hcheck(1,idchk)) THEN
     listname = "T0_HALO_LIST"
     icomponent = ps_cnum_NBI
     units = "keV"
     label = "halo neutral temperature (2/3)<E0>"
     ! for assoc. scalar list:
     list_units = "keV"
     list_label = "wght avg (N0_HALO): T0_halo"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "T0_halo_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "T0_halo_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_NBI", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_nbi-1,0, &
             ss%T0_halo(:,I1),ss%id_T0_halo(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_T0_halo(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_T0_RECO
  idchk = 0
  IF(ALLOCATED(ss%id_T0_reco)) idchk = ss%id_T0_reco(1)
  IF(ALLOCATED(ss%T0_reco).AND.hcheck(1,idchk)) THEN
     listname = "T0_RECO_LIST"
     icomponent = ps_cnum_GAS
     units = "keV"
     label = "reco neutral temperature (2/3)<E0>"
     ! for assoc. scalar list:
     list_units = "keV"
     list_label = "wght avg (N0_RECO): T0_reco"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "T0_reco_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "T0_reco_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho_gas-1,0, &
             ss%T0_reco(:,I1),ss%id_T0_reco(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_T0_reco(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TEFLUX_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_teflux_ky_tglf)) idchk = ss%id_teflux_ky_tglf(1)
  IF(ALLOCATED(ss%teflux_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "TEFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "electron density flux spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: teflux_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "teflux_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "teflux_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%teflux_ky_tglf(:,I1),ss%id_teflux_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_teflux_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TE_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_te_ky_tglf)) idchk = ss%id_te_ky_tglf(1)
  IF(ALLOCATED(ss%te_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "TE_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "electron density fluctuation spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: te_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "te_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "te_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%te_ky_tglf(:,I1),ss%id_te_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_te_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TIFLUX_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_tiflux_ky_tglf)) idchk = ss%id_tiflux_ky_tglf(1)
  IF(ALLOCATED(ss%tiflux_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "TIFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "ion density flux spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: tiflux_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "tiflux_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "tiflux_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%tiflux_ky_tglf(:,I1),ss%id_tiflux_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_tiflux_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TI_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_ti_ky_tglf)) idchk = ss%id_ti_ky_tglf(1)
  IF(ALLOCATED(ss%ti_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "TI_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "ion density fluctuation spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: ti_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "ti_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "ti_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%ti_ky_tglf(:,I1),ss%id_ti_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_ti_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQIONIZ
  idchk = 0
  IF(ALLOCATED(ss%id_tqioniz)) idchk = ss%id_tqioniz(1)
  IF(ALLOCATED(ss%tqioniz).AND.hcheck(1,idchk)) THEN
     listname = "TQIONIZ_LIST"
     icomponent = ps_cnum_GAS
     units = "Nt*m/(#/sec)"
     label = "ionization torque per unit influx (sc0)"
     ! for assoc. scalar list:
     list_units = "Nt*m/(#/sec)"
     list_label = "vol int: tqioniz"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "tqioniz_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "tqioniz_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_gas-1,0, &
             ss%tqioniz(:,I1),ss%id_tqioniz(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_tqioniz(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQQCX
  idchk = 0
  IF(ALLOCATED(ss%id_tqqcx)) idchk = ss%id_tqqcx(1)
  IF(ALLOCATED(ss%tqqcx).AND.hcheck(1,idchk)) THEN
     listname = "TQQCX_LIST"
     icomponent = ps_cnum_GAS
     units = "Nt*m/(#/sec)/(rad/sec)"
     label = "cx torque per unit influx per (omeg0-omegi)"
     ! for assoc. scalar list:
     list_units = "Nt*m/(#/sec)/(rad/sec)"
     list_label = "vol int: tqqcx"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "tqqcx_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "tqqcx_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_GAS", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho_gas-1,0, &
             ss%tqqcx(:,I1),ss%id_tqqcx(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_tqqcx(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQ_TRANSCAT
  idchk = 0
  IF(ALLOCATED(ss%id_tq_transcat)) idchk = ss%id_tq_transcat(1)
  IF(ALLOCATED(ss%tq_transcat).AND.hcheck(1,idchk)) THEN
     listname = "TQ_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "Nt*m"
     label = "toroidal angular momentum transport"
     ! for assoc. scalar list:
     list_units = "Nt*m"
     list_label = "vol int: tq_transcat"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "tq_transcat_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "tq_transcat_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
             ss%nrho-1,0, &
             ss%tq_transcat(:,I1),ss%id_tq_transcat(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_tq_transcat(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TS
  idchk = 0
  IF(ALLOCATED(ss%id_Ts)) idchk = ss%id_Ts(1)
  IF(ALLOCATED(ss%Ts).AND.hcheck(1,idchk)) THEN
     listname = "TS_LIST"
     icomponent = ps_cnum_PLASMA
     units = "keV"
     label = "thermal specie temperature"
     ! for assoc. scalar list:
     list_units = "keV"
     list_label = "wght avg (density): T"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "T"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "T"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho-1,0, &
             ss%Ts(:,I1),ss%id_Ts(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_Ts(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TZFLUX_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_tzflux_ky_tglf)) idchk = ss%id_tzflux_ky_tglf(1)
  IF(ALLOCATED(ss%tzflux_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "TZFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "impurity density flux spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: tzflux_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "tzflux_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "tzflux_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%tzflux_ky_tglf(:,I1),ss%id_tzflux_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_tzflux_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TZ_KY_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_tz_ky_tglf)) idchk = ss%id_tz_ky_tglf(1)
  IF(ALLOCATED(ss%tz_ky_tglf).AND.hcheck(1,idchk)) THEN
     listname = "TZ_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     label = "impurity density fluctuation spectrum"
     ! for assoc. scalar list:
     list_units = "GB"
     list_label = "simple avg: tz_ky_tglf"

     nitems = 0
     jmod = 0
     DO I1 = 1,ss%nky_anomth
        NAM1 = TRIM(enum_lbl_1d(I1))
        ILAB1 = '('//TRIM(enum_lbl_1d(I1))//')'
        nitems = nitems + 1
        IF(nitems.GT.max_itemlist) THEN
           WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
           WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
           ierr = 9999
           RETURN
        ENDIF
        item_names(nitems) = "tz_ky_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "tz_ky_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%tz_ky_tglf(:,I1),ss%id_tz_ky_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_tz_ky_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_UPWIND_PFRAC_NS
  idchk = 0
  IF(ALLOCATED(ss%id_upwind_pfrac_ns)) idchk = ss%id_upwind_pfrac_ns(1)
  IF(ALLOCATED(ss%upwind_pfrac_ns).AND.hcheck(1,idchk)) THEN
     listname = "UPWIND_PFRAC_NS_LIST"
     icomponent = ps_cnum_PLASMA
     units = "-"
     label = "specie density forward upwind fractions"
     ! for assoc. scalar list:
     list_units = "-"
     list_label = "simple avg: upwind_pfrac_ns"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "upwind_pfrac_ns_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "upwind_pfrac_ns_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho,0, &
             ss%upwind_pfrac_ns(:,I1),ss%id_upwind_pfrac_ns(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_upwind_pfrac_ns(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VEE_TRANSCAT
  idchk = 0
  IF(ALLOCATED(ss%id_vee_transcat)) idchk = ss%id_vee_transcat(1)
  IF(ALLOCATED(ss%vee_transcat).AND.hcheck(1,idchk)) THEN
     listname = "VEE_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m/sec"
     label = "Electron energy radial velocity"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "simple avg: vee_transcat"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "vee_transcat_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "vee_transcat_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho,0, &
             ss%vee_transcat(:,I1),ss%id_vee_transcat(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_vee_transcat(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_ANOM
  idchk = 0
  IF(ALLOCATED(ss%id_velns_anom)) idchk = ss%id_velns_anom(1)
  IF(ALLOCATED(ss%velns_anom).AND.hcheck(1,idchk)) THEN
     listname = "VELNS_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     label = "density-equations, advective velocity term"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "simple avg: velns_anom"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "velns_anom_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "velns_anom_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%velns_anom(:,I1),ss%id_velns_anom(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_velns_anom(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_GLF
  idchk = 0
  IF(ALLOCATED(ss%id_velns_glf)) idchk = ss%id_velns_glf(1)
  IF(ALLOCATED(ss%velns_glf).AND.hcheck(1,idchk)) THEN
     listname = "VELNS_GLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     label = "density-equations, advective velocity term from GLF23 model"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "simple avg: velns_glf"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "velns_glf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "velns_glf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%velns_glf(:,I1),ss%id_velns_glf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_velns_glf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_GTCNEO
  idchk = 0
  IF(ALLOCATED(ss%id_velns_gtcneo)) idchk = ss%id_velns_gtcneo(1)
  IF(ALLOCATED(ss%velns_gtcneo).AND.hcheck(1,idchk)) THEN
     listname = "VELNS_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     label = "density-equations, advective velocity term from GTC_NEO model"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "simple avg: velns_gtcneo"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "velns_gtcneo_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "velns_gtcneo_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%velns_gtcneo(:,I1),ss%id_velns_gtcneo(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_velns_gtcneo(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_MMM71
  idchk = 0
  IF(ALLOCATED(ss%id_velns_mmm71)) idchk = ss%id_velns_mmm71(1)
  IF(ALLOCATED(ss%velns_mmm71).AND.hcheck(1,idchk)) THEN
     listname = "VELNS_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     label = "density-equations, advective velocity term from MMM71 model"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "simple avg: velns_mmm71"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "velns_mmm71_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "velns_mmm71_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%velns_mmm71(:,I1),ss%id_velns_mmm71(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_velns_mmm71(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_NCLASS
  idchk = 0
  IF(ALLOCATED(ss%id_velns_nclass)) idchk = ss%id_velns_nclass(1)
  IF(ALLOCATED(ss%velns_nclass).AND.hcheck(1,idchk)) THEN
     listname = "VELNS_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     label = "density-equations, advective velocity term from NCLASS model"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "simple avg: velns_nclass"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "velns_nclass_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "velns_nclass_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%velns_nclass(:,I1),ss%id_velns_nclass(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_velns_nclass(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_NEO
  idchk = 0
  IF(ALLOCATED(ss%id_velns_neo)) idchk = ss%id_velns_neo(1)
  IF(ALLOCATED(ss%velns_neo).AND.hcheck(1,idchk)) THEN
     listname = "VELNS_NEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     label = "density-equations, advective velocity term from NEO model"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "simple avg: velns_neo"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "velns_neo_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "velns_neo_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%velns_neo(:,I1),ss%id_velns_neo(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_velns_neo(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_TGLF
  idchk = 0
  IF(ALLOCATED(ss%id_velns_tglf)) idchk = ss%id_velns_tglf(1)
  IF(ALLOCATED(ss%velns_tglf).AND.hcheck(1,idchk)) THEN
     listname = "VELNS_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     label = "density-equations, advective velocity term from TGLF model"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "simple avg: velns_tglf"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "velns_tglf_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "velns_tglf_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ANOMTH", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho_anomth,0, &
             ss%velns_tglf(:,I1),ss%id_velns_tglf(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_velns_tglf(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VIE_TRANSCAT
  idchk = 0
  IF(ALLOCATED(ss%id_vie_transcat)) idchk = ss%id_vie_transcat(1)
  IF(ALLOCATED(ss%vie_transcat).AND.hcheck(1,idchk)) THEN
     listname = "VIE_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m/sec"
     label = "Ion energy radial velocity"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "simple avg: vie_transcat"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "vie_transcat_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "vie_transcat_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho,0, &
             ss%vie_transcat(:,I1),ss%id_vie_transcat(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_vie_transcat(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VMO_TRANSCAT
  idchk = 0
  IF(ALLOCATED(ss%id_vmo_transcat)) idchk = ss%id_vmo_transcat(1)
  IF(ALLOCATED(ss%vmo_transcat).AND.hcheck(1,idchk)) THEN
     listname = "VMO_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m/sec"
     label = "Angular momentum radial velocity"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "simple avg: vmo_transcat"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "vmo_transcat_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "vmo_transcat_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho,0, &
             ss%vmo_transcat(:,I1),ss%id_vmo_transcat(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_vmo_transcat(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VN_TRANS
  idchk = 0
  IF(ALLOCATED(ss%id_vn_trans)) idchk = ss%id_vn_trans(1)
  IF(ALLOCATED(ss%vn_trans).AND.hcheck(1,idchk)) THEN
     listname = "VN_TRANS_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m/sec"
     label = "specie radial velocity"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "simple avg: vn_trans"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "vn_trans_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "vn_trans_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
             ss%nrho,0, &
             ss%vn_trans(:,I1),ss%id_vn_trans(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_vn_trans(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VN_TRANSCAT
  idchk = 0
  IF(ALLOCATED(ss%id_vn_transcat)) idchk = ss%id_vn_transcat(1,1)
  IF(ALLOCATED(ss%vn_transcat).AND.hcheck(1,idchk)) THEN
     listname = "VN_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m/sec"
     label = "specie radial velocity"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "simple avg: vn_transcat"

     nitems = 0
     jmod = 0
     DO I2 = 1,ss%ntrcat
        NAM2 = TRIM(ss%trcat(I2))
        ILAB2 = 'TR_categories: '//TRIM(ss%trcat(I2))
        DO I1 = 0,ss%nspec_th
           NAM1 = TRIM(ss%S_name(I1))//'_'//TRIM(NAM2)
           ILAB1 = 'thermal_specie: '//TRIM(ss%S_name(I1))//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = "vn_transcat_"//TRIM(NAM1)
           item_c256(nitems) = ILAB1
           plabel = TRIM(label)//": "//TRIM(nam1)
           profname = "vn_transcat_"//TRIM(nam1)
           CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
                units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
                ss%nrho,0, &
                ss%vn_transcat(:,I1,I2),ss%id_vn_transcat(I1,I2), &
                imod,iout,ierr)
           IF(ierr.NE.0) RETURN
           IF(imod.gt.0) jmod = 1
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%id_vn_transcat(I1,I2)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VPOL_INMP
  idchk = 0
  IF(ALLOCATED(ss%id_vpol_inmp)) idchk = ss%id_vpol_inmp(1)
  IF(ALLOCATED(ss%vpol_inmp).AND.hcheck(1,idchk)) THEN
     listname = "VPOL_INMP_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m/sec"
     label = "poloidal velocity, inner half midplane"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "wght avg (density): vpol_inmp"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "vpol_inmp_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "vpol_inmp_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho-1,0, &
             ss%vpol_inmp(:,I1),ss%id_vpol_inmp(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_vpol_inmp(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VPOL_OMP
  idchk = 0
  IF(ALLOCATED(ss%id_vpol_omp)) idchk = ss%id_vpol_omp(1)
  IF(ALLOCATED(ss%vpol_omp).AND.hcheck(1,idchk)) THEN
     listname = "VPOL_OMP_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m/sec"
     label = "poloidal velocity, outer half midplane"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "wght avg (density): vpol_omp"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "vpol_omp_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "vpol_omp_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho-1,0, &
             ss%vpol_omp(:,I1),ss%id_vpol_omp(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_vpol_omp(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VTOR_INMP
  idchk = 0
  IF(ALLOCATED(ss%id_vtor_inmp)) idchk = ss%id_vtor_inmp(1)
  IF(ALLOCATED(ss%vtor_inmp).AND.hcheck(1,idchk)) THEN
     listname = "VTOR_INMP_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m/sec"
     label = "toroidal velocity, inner half midplane"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "wght avg (density): vtor_inmp"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "vtor_inmp_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "vtor_inmp_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho-1,0, &
             ss%vtor_inmp(:,I1),ss%id_vtor_inmp(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_vtor_inmp(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VTOR_OMP
  idchk = 0
  IF(ALLOCATED(ss%id_vtor_omp)) idchk = ss%id_vtor_omp(1)
  IF(ALLOCATED(ss%vtor_omp).AND.hcheck(1,idchk)) THEN
     listname = "VTOR_OMP_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m/sec"
     label = "toroidal velocity, outer half midplane"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "wght avg (density): vtor_omp"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "vtor_omp_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "vtor_omp_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho-1,0, &
             ss%vtor_omp(:,I1),ss%id_vtor_omp(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_vtor_omp(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_V_PARS
  idchk = 0
  IF(ALLOCATED(ss%id_v_pars)) idchk = ss%id_v_pars(1)
  IF(ALLOCATED(ss%v_pars).AND.hcheck(1,idchk)) THEN
     listname = "V_PARS_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m/sec"
     label = "thermal specie <vpll>"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "wght avg (density): VPLL"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "VPLL_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "VPLL_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho-1,0, &
             ss%v_pars(:,I1),ss%id_v_pars(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_v_pars(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_V_PERS
  idchk = 0
  IF(ALLOCATED(ss%id_v_pers)) idchk = ss%id_v_pers(1)
  IF(ALLOCATED(ss%v_pers).AND.hcheck(1,idchk)) THEN
     listname = "V_PERS_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m/sec"
     label = "thermal specie <vper>"
     ! for assoc. scalar list:
     list_units = "m/sec"
     list_label = "wght avg (density): VPER"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "VPER_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "VPER_"//TRIM(nam1)
        CALL ps_1d_prof_store(ss%iobj,profname,"RHO", &
             units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
             ss%nrho-1,0, &
             ss%v_pers(:,I1),ss%id_v_pers(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_v_pers(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_XRJCOS_MOMEQ
  ahash(2) = ps_phash_DXRJCOS_MOMEQ_DRHO
  idchk = 0
  IF(ALLOCATED(ss%id_xRjcos_momeq)) idchk = ss%id_xRjcos_momeq(1)
  IF(ALLOCATED(ss%xRjcos_momeq).AND.hcheck(2,idchk)) THEN
     listname = "XRJCOS_MOMEQ_LIST"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "scaled R cos moments"
     ! for assoc. scalar list:
     list_units = "m"
     list_label = "vol avg: xRjcos_momeq"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "xRjcos_momeq_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "xRjcos_momeq_"//TRIM(nam1)
        CALL ps_1dx_prof_store(ss%iobj,profname,"RHO_EQ", &
             units,icomponent,plabel, &
             ss%nrho_eq,0, &
             ss%xRjcos_momeq(:,I1),ss%dxRjcos_momeq_dRHO(:,I1),ss%id_xRjcos_momeq(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_xRjcos_momeq(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_XRJSIN_MOMEQ
  ahash(2) = ps_phash_DXRJSIN_MOMEQ_DRHO
  idchk = 0
  IF(ALLOCATED(ss%id_xRjsin_momeq)) idchk = ss%id_xRjsin_momeq(1)
  IF(ALLOCATED(ss%xRjsin_momeq).AND.hcheck(2,idchk)) THEN
     listname = "XRJSIN_MOMEQ_LIST"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "scaled R sin moments"
     ! for assoc. scalar list:
     list_units = "m"
     list_label = "vol avg: xRjsin_momeq"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "xRjsin_momeq_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "xRjsin_momeq_"//TRIM(nam1)
        CALL ps_1dx_prof_store(ss%iobj,profname,"RHO_EQ", &
             units,icomponent,plabel, &
             ss%nrho_eq,0, &
             ss%xRjsin_momeq(:,I1),ss%dxRjsin_momeq_dRHO(:,I1),ss%id_xRjsin_momeq(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_xRjsin_momeq(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_XZJCOS_MOMEQ
  ahash(2) = ps_phash_DXZJCOS_MOMEQ_DRHO
  idchk = 0
  IF(ALLOCATED(ss%id_xZjcos_momeq)) idchk = ss%id_xZjcos_momeq(1)
  IF(ALLOCATED(ss%xZjcos_momeq).AND.hcheck(2,idchk)) THEN
     listname = "XZJCOS_MOMEQ_LIST"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "scaled Z cos moments"
     ! for assoc. scalar list:
     list_units = "m"
     list_label = "vol avg: xZjcos_momeq"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "xZjcos_momeq_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "xZjcos_momeq_"//TRIM(nam1)
        CALL ps_1dx_prof_store(ss%iobj,profname,"RHO_EQ", &
             units,icomponent,plabel, &
             ss%nrho_eq,0, &
             ss%xZjcos_momeq(:,I1),ss%dxZjcos_momeq_dRHO(:,I1),ss%id_xZjcos_momeq(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_xZjcos_momeq(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_XZJSIN_MOMEQ
  ahash(2) = ps_phash_DXZJSIN_MOMEQ_DRHO
  idchk = 0
  IF(ALLOCATED(ss%id_xZjsin_momeq)) idchk = ss%id_xZjsin_momeq(1)
  IF(ALLOCATED(ss%xZjsin_momeq).AND.hcheck(2,idchk)) THEN
     listname = "XZJSIN_MOMEQ_LIST"
     icomponent = ps_cnum_EQ
     units = "m"
     label = "scaled Z sin moments"
     ! for assoc. scalar list:
     list_units = "m"
     list_label = "vol avg: xZjsin_momeq"

     nitems = 0
     jmod = 0
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
        item_names(nitems) = "xZjsin_momeq_"//TRIM(NAM1)
        item_c256(nitems) = ILAB1
        plabel = TRIM(label)//": "//TRIM(nam1)
        profname = "xZjsin_momeq_"//TRIM(nam1)
        CALL ps_1dx_prof_store(ss%iobj,profname,"RHO_EQ", &
             units,icomponent,plabel, &
             ss%nrho_eq,0, &
             ss%xZjsin_momeq(:,I1),ss%dxZjsin_momeq_dRHO(:,I1),ss%id_xZjsin_momeq(I1), &
             imod,iout,ierr)
        IF(ierr.NE.0) RETURN
        IF(imod.gt.0) jmod = 1
        item_r8vals(nitems) = 0.0_rspec
        item_ivals(nitems) = ss%id_xZjsin_momeq(I1)
     ENDDO   ! I1

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
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

  SUBROUTINE load_lbl_1d(indx_1d,elabel)
     integer, dimension(:), intent(in) :: indx_1d
     character*32, dimension(:), intent(inout) :: elabel

     integer :: iwarn,idup,ii,jj

     iwarn=0
     do ii=1,size(indx_1d)
        if(indx_1d(ii).lt.0) then
           write(elabel(ii),'("m",i8)') -indx_1d(ii)
        else
           write(elabel(ii),'("p",i8)') indx_1d(ii)
        endif

        idup=0
        do jj=1,ii-1
           if(indx_1d(ii).eq.indx_1d(jj)) idup=idup+1
        enddo

        if(idup.gt.0) then
           if(iwarn.eq.0) then
              iwarn=1
              write(iout,*) ' %plasma_state_xload: enumeration index values not unique (patch applied).'
           endif
           write(elabel(ii)(10:18),'("x",i8)') idup+1
        endif

        call lefpack(elabel(ii))
     enddo
  END SUBROUTINE load_lbl_1d

  SUBROUTINE load_lbl_2d(indx_num,indx_2d,elabel)
     integer, dimension(:), intent(in) :: indx_num
     integer, dimension(:,:), intent(in) :: indx_2d
     character*32, dimension(:,:), intent(inout) :: elabel

     integer :: kk,inum

     do kk=1,size(indx_num)
        inum = indx_num(kk)
        if(inum.gt.0) then
           call load_lbl_1d(indx_2d(1:inum,kk),elabel(1:inum,kk))
        endif
     enddo
  END SUBROUTINE load_lbl_2d

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

END SUBROUTINE plasma_state_xload_parrays
