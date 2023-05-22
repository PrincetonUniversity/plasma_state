! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_xload(ss,ierr)

  ! store plasma state data in xplasma
  ! enable interpolation services

  ! scalar data is written if section hashes indicate change, or if tests
  ! indicate t

  ! grid data items are written if hashes indicate change, or if grid IDs
  ! cannot be found in xplasma

  ! profile data is written if hashes indicate change of id ss%id_<profile>
  ! variables are zero.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-----------------------
  ! local:

  INTEGER, PARAMETER :: max_itemlist = ps_max_static

  CHARACTER*32 :: listname,gridname,profname,enum_name,units,list_units
  CHARACTER*80 :: label,plabel,list_label
  INTEGER :: icomponent

  INTEGER :: nitems
  CHARACTER*32 :: item_names(max_itemlist)
  INTEGER :: item_ivals(max_itemlist)
  REAL(KIND=rspec) :: item_r8vals(max_itemlist)
  CHARACTER*256 :: item_c256(max_itemlist)

  INTEGER :: i1,i2,i3,i4,indx,itype,isize
  CHARACTER*32 :: nam1,nam2,nam3,nam4,iiencod,tmpnam,vid_found
  CHARACTER*80 :: ilab1,ilab2,ilab3,ilab4
  INTEGER :: iout,imod,jmod,iconvert,iertmp,iwarn,idchk

  INTEGER :: xhash(ps_hash_total)
  INTEGER :: ahash(ps_max_static)

  !-----------------------
  ! for enumeration labeling...
  CHARACTER*32, DIMENSION(:), ALLOCATABLE :: enum_lbl_1d
  CHARACTER*32, DIMENSION(:,:), ALLOCATABLE :: enum_lbl_NPHI

  !-----------------------
  ! for ps_rezone_rho(...)

  REAL(KIND=rspec), dimension(2) :: x01 = (/ 0.0_rspec, 1.0_rspec /)
  INTEGER :: idvec(1),idwvec(1)
  REAL(KIND=rspec) :: ansvec(1,1),dvvec(1)
  INTEGER :: id_intg

  !-----------------------

  iout = SWIM_err_out_file

  CALL ps_xhashes_get(ss%iobj,xhash,iout,ierr)
  IF(ierr.NE.0) RETURN

  !----------------------------------------------------
  ! check for valid sets of names, non-coordinate dimensions:
  ierr = 0

  ahash(1) = ps_dhash_MMODES
  IF(ALLOCATED(ss%Tor_mode_label).AND.hcheck(1,1)) THEN
     CALL ps_name_check("Tor_mode_label",1,ss%mmodes,ss%Tor_mode_label,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NBALLOON
  IF(ALLOCATED(ss%Balloon_code).AND.hcheck(1,1)) THEN
     CALL ps_name_check("Balloon_code",1,ss%nballoon,ss%Balloon_code,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NBEAM
  IF(ALLOCATED(ss%nbi_src_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("nbi_src_name",1,ss%nbeam,ss%nbi_src_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NCIRCUITS
  IF(ALLOCATED(ss%circuit_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("circuit_name",1,ss%ncircuits,ss%circuit_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NCOILS
  IF(ALLOCATED(ss%coil_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("coil_name",1,ss%ncoils,ss%coil_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NDELTA_W
  IF(ALLOCATED(ss%DeltaW_code).AND.hcheck(1,1)) THEN
     CALL ps_name_check("DeltaW_code",1,ss%ndelta_w,ss%DeltaW_code,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NECRF_SRC
  IF(ALLOCATED(ss%ecrf_src_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("ecrf_src_name",1,ss%necrf_src,ss%ecrf_src_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NEQMOM
  IF(ALLOCATED(ss%eqmom_num).AND.hcheck(1,1)) THEN
     CALL ps_name_check("eqmom_num",1,ss%neqmom,ss%eqmom_num,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NGSC0
  IF(ALLOCATED(ss%gs_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("gs_name",1,ss%ngsc0,ss%gs_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NICRF_SRC
  IF(ALLOCATED(ss%icrf_src_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("icrf_src_name",1,ss%nicrf_src,ss%icrf_src_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NLHRF_SRC
  IF(ALLOCATED(ss%lhrf_src_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("lhrf_src_name",1,ss%nlhrf_src,ss%lhrf_src_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NNEWCOMB
  IF(ALLOCATED(ss%Newcomb_code).AND.hcheck(1,1)) THEN
     CALL ps_name_check("Newcomb_code",1,ss%nnewcomb,ss%Newcomb_code,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NPSMOM
  IF(ALLOCATED(ss%psmom_num).AND.hcheck(1,1)) THEN
     CALL ps_name_check("psmom_num",1,ss%npsmom,ss%psmom_num,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NSPEC_ALL
  IF(ALLOCATED(ss%ALL_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("ALL_name",0,ss%nspec_all,ss%ALL_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NSPEC_ALLA
  IF(ALLOCATED(ss%ALLA_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("ALLA_name",0,ss%nspec_alla,ss%ALLA_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NSPEC_BEAM
  IF(ALLOCATED(ss%SNBI_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("SNBI_name",1,ss%nspec_beam,ss%SNBI_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NSPEC_FUSION
  IF(ALLOCATED(ss%SFUS_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("SFUS_name",1,ss%nspec_fusion,ss%SFUS_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NSPEC_GAS
  IF(ALLOCATED(ss%SGAS_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("SGAS_name",1,ss%nspec_gas,ss%SGAS_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NSPEC_IMP0
  IF(ALLOCATED(ss%SIMP0_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("SIMP0_name",1,ss%nspec_imp0,ss%SIMP0_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NSPEC_IMPI
  IF(ALLOCATED(ss%SIMPI_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("SIMPI_name",1,ss%nspec_impi,ss%SIMPI_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NSPEC_RFMIN
  IF(ALLOCATED(ss%RFMIN_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("RFMIN_name",1,ss%nspec_rfmin,ss%RFMIN_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NSPEC_TH
  IF(ALLOCATED(ss%S_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("S_name",0,ss%nspec_th,ss%S_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NSPEC_THA
  IF(ALLOCATED(ss%SA_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("SA_name",0,ss%nspec_tha,ss%SA_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NTRCAT
  IF(ALLOCATED(ss%trcat).AND.hcheck(1,1)) THEN
     CALL ps_name_check("trcat",1,ss%ntrcat,ss%trcat,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

  ahash(1) = ps_dhash_NXS
  IF(ALLOCATED(ss%xs_name).AND.hcheck(1,1)) THEN
     CALL ps_name_check("xs_name",1,ss%nxs,ss%xs_name,iout,iertmp)
     ierr = max(ierr,iertmp)
  ENDIF

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

  IF(ierr.GT.0) RETURN

  !----------------------------------------------------
  ! store and perform error check on coordinate grids

  CALL plasma_state_xload_grids(ss,xhash,iout,ierr)

  IF(ierr.GT.0) RETURN

  !----------------------------------------------------
  ! store species lists

  CALL plasma_state_xload_slists(ss,xhash,iout,ierr)

  IF(ierr.GT.0) RETURN

  !----------------------------------------------------
  ! store the time (t1)

  call ps_time_set(ss%iobj,ss%t1,iout,ierr)

  IF(ierr.GT.0) RETURN

  !----------------------------------------------------
  ! store scalar groups as lists

  CALL plasma_state_xload_sgrps(ss,xhash,iout,ierr)

  IF(ierr.GT.0) RETURN

  !----------------------------------------------------
  ! store standalone scalars as single valued lists
  ! store 1d enumerated arrays: list + blackbox

  CALL plasma_state_xload_singl(ss,xhash,iout,ierr)

  IF(ierr.GT.0) RETURN

  !----------------------------------------------------
  ! store allocatable scalar arrays (non-profile data)
  ! this includes 2d enumerations.

  CALL plasma_state_xload_sarrays(ss,xhash,iout,ierr)

  IF(ierr.GT.0) RETURN

  !----------------------------------------------------
  ! store singleton profiles with single valued lists

  CALL plasma_state_xload_psingl(ss,xhash,iout,ierr)

  IF(ierr.GT.0) RETURN

  !----------------------------------------------------
  ! store profiles in groups; groups themselves as lists

  CALL plasma_state_xload_pgrps(ss,xhash,iout,ierr)

  IF(ierr.GT.0) RETURN

  !----------------------------------------------------
  ! store indexed arrays of profiles
  ! "parrays" subroutine handles non-enumerated subset.

  CALL plasma_state_xload_parrays(ss,xhash,iout,ierr)

  ahash(1) = ps_phash_CDICRF_NPHI
  idchk = 0
  IF(ALLOCATED(ss%id_cdicrf_nphi)) idchk = ss%id_cdicrf_nphi(1,1)
  IF(ALLOCATED(ss%cdicrf_nphi).AND.hcheck(1,idchk)) THEN
     listname = "CDICRF_NPHI_LIST"
     icomponent = ps_cnum_IC
     units = "A"
     label = "ICRF current drive"
     ! for assoc. scalar list:
     list_units = "A"
     list_label = "area int: cdicrf_nphi"

     nitems = 0
     jmod = 0
     DO I2 = 1,ss%nicrf_src
        NAM2 = TRIM(ss%icrf_src_name(I2))
        ILAB2 = 'icrf_source: '//TRIM(ss%icrf_src_name(I2))
        DO I1 = 1,ss%max_num_nphi
           IF(enum_lbl_nphi(I1,I2)(1:1).eq.' ') CYCLE
           NAM1 = TRIM(enum_lbl_nphi(I1,I2))//'_'//TRIM(NAM2)
           iiencod = ' '
           WRITE(iiencod,'(I8)') ss%nphi(I1,I2)
           CALL lefpack(iiencod)
           ILAB1 = 'nphi='//TRIM(iiencod)//' '//TRIM(ILAB2)
           nitems = nitems + 1
           IF(nitems.GT.max_itemlist) THEN
              WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
              WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
              ierr = 9999
              RETURN
           ENDIF
           item_names(nitems) = "cdicrf_nphi_"//TRIM(NAM1)
           item_c256(nitems) = ILAB1
           plabel = TRIM(label)//": "//TRIM(nam1)
           profname = "cdicrf_nphi_"//TRIM(nam1)
           CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ICRF", &
                units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
                ss%nrho_icrf-1,0, &
                ss%cdicrf_nphi(:,I1,I2),ss%id_cdicrf_nphi(I1,I2), &
                imod,iout,ierr)
           IF(ierr.NE.0) RETURN
           IF(imod.gt.0) jmod = 1
           item_r8vals(nitems) = 0.0_rspec
           item_ivals(nitems) = ss%id_cdicrf_nphi(I1,I2)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PICRF_NPHI_SRCS
  idchk = 0
  IF(ALLOCATED(ss%id_picrf_nphi_srcs)) idchk = ss%id_picrf_nphi_srcs(1,1,1)
  IF(ALLOCATED(ss%picrf_nphi_srcs).AND.hcheck(1,idchk)) THEN
     listname = "PICRF_NPHI_SRCS_LIST"
     icomponent = ps_cnum_IC
     units = "W"
     label = "direct ICRF power deposition"
     ! for assoc. scalar list:
     list_units = "W"
     list_label = "vol int: PICRF_NPHI"

     nitems = 0
     jmod = 0
     DO I3 = 0,ss%nspec_alla
        NAM3 = TRIM(ss%ALLA_name(I3))
        ILAB3 = 'specie: '//TRIM(ss%ALLA_name(I3))
        DO I2 = 1,ss%nicrf_src
           NAM2 = TRIM(ss%icrf_src_name(I2))//'_'//TRIM(NAM3)
           ILAB2 = 'icrf_source: '//TRIM(ss%icrf_src_name(I2))//' '//TRIM(ILAB3)
           DO I1 = 1,ss%max_num_nphi
              IF(enum_lbl_nphi(I1,I2)(1:1).eq.' ') CYCLE
              NAM1 = TRIM(enum_lbl_nphi(I1,I2))//'_'//TRIM(NAM2)
              iiencod = ' '
              WRITE(iiencod,'(I8)') ss%nphi(I1,I2)
              CALL lefpack(iiencod)
              ILAB1 = 'nphi='//TRIM(iiencod)//' '//TRIM(ILAB2)
              nitems = nitems + 1
              IF(nitems.GT.max_itemlist) THEN
                 WRITE(iout,*) " ?plasma_state_xload:  max list size (",max_itemlist,") exceeded."
                 WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"
                 ierr = 9999
                 RETURN
              ENDIF
              item_names(nitems) = "PICRF_NPHI_"//TRIM(NAM1)
              item_c256(nitems) = ILAB1
              plabel = TRIM(label)//": "//TRIM(nam1)
              profname = "PICRF_NPHI_"//TRIM(nam1)
              CALL ps_1d_prof_store(ss%iobj,profname,"RHO_ICRF", &
                   units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
                   ss%nrho_icrf-1,0, &
                   ss%picrf_nphi_srcs(:,I1,I2,I3),ss%id_picrf_nphi_srcs(I1,I2,I3), &
                   imod,iout,ierr)
              IF(ierr.NE.0) RETURN
              IF(imod.gt.0) jmod = 1
              item_r8vals(nitems) = 0.0_rspec
              item_ivals(nitems) = ss%id_picrf_nphi_srcs(I1,I2,I3)
           ENDDO   ! I1
        ENDDO   ! I2
     ENDDO   ! I3

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,list_label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  IF(ierr.GT.0) RETURN

  !----------------------------------------------------
  ! ALL PROFILES STORED -- update weight IDs

  CALL plasma_state_zonwt_update(ss,ierr)
  IF(ierr.NE.0) return

  !  check if rezone integrals are available (this depends on an equilibrium
  !  being included in the state).

  CALL ps_rezone_chk(ss,id_intg,iout,ierr)
  IF(ierr.NE.0) return

  !  id_intg=0 indicates no rezone available.

  !----------------------------------------------------
  ! profile group list values updated here...

  ahash(1) = ps_phash_DIFB_NBI
  ahash(2) = ps_phash_DIFB_FUSI
  ahash(3) = ps_phash_DIFB_RFMI
  IF(ALLOCATED(ss%difb_nbi).AND.hcheck(3,1)) THEN
     listname = "ANOM_DIFFUS"
     icomponent = ps_cnum_ANOM
     nitems = 3
     ansvec(1,1) = sum(ss%difb_nbi)/size(ss%difb_nbi)
     item_r8vals(1) = ansvec(1,1)
     ansvec(1,1) = sum(ss%difb_fusi)/size(ss%difb_fusi)
     item_r8vals(2) = ansvec(1,1)
     ansvec(1,1) = sum(ss%difb_rfmi)/size(ss%difb_rfmi)
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELB_NBI
  ahash(2) = ps_phash_VELB_FUSI
  ahash(3) = ps_phash_VELB_RFMI
  IF(ALLOCATED(ss%velb_nbi).AND.hcheck(3,1)) THEN
     listname = "ANOM_VELOC"
     icomponent = ps_cnum_ANOM
     nitems = 3
     ansvec(1,1) = sum(ss%velb_nbi)/size(ss%velb_nbi)
     item_r8vals(1) = ansvec(1,1)
     ansvec(1,1) = sum(ss%velb_fusi)/size(ss%velb_fusi)
     item_r8vals(2) = ansvec(1,1)
     ansvec(1,1) = sum(ss%velb_rfmi)/size(ss%velb_rfmi)
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PB0_HALO
  ahash(2) = ps_phash_PSC_HALO
  ahash(3) = ps_phash_PCX_HALO
  IF(ALLOCATED(ss%pb0_halo).AND.(id_intg.GT.0).AND.hcheck(3,1)) THEN
     listname = "BEAM_HALO_POWER"
     icomponent = ps_cnum_NBI
     nitems = 3
     idvec(1) = ss%id_pb0_halo
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_psc_halo
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)
     idvec(1) = ss%id_pcx_halo
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQB0_HALO
  ahash(2) = ps_phash_TQSC_HALO
  ahash(3) = ps_phash_TQCX_HALO
  IF(ALLOCATED(ss%tqb0_halo).AND.(id_intg.GT.0).AND.hcheck(3,1)) THEN
     listname = "BEAM_HALO_TORQUE"
     icomponent = ps_cnum_NBI
     nitems = 3
     idvec(1) = ss%id_tqb0_halo
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_tqsc_halo
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)
     idvec(1) = ss%id_tqcx_halo
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PBE
  ahash(2) = ps_phash_PBI
  ahash(3) = ps_phash_PBTH
  IF(ALLOCATED(ss%pbe).AND.(id_intg.GT.0).AND.hcheck(3,1)) THEN
     listname = "BEAM_HEATING"
     icomponent = ps_cnum_NBI
     nitems = 3
     idvec(1) = ss%id_pbe
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_pbi
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)
     idvec(1) = ss%id_pbth
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQBE
  ahash(2) = ps_phash_TQBI
  ahash(3) = ps_phash_TQBJXB
  ahash(4) = ps_phash_TQBTH
  IF(ALLOCATED(ss%tqbe).AND.(id_intg.GT.0).AND.hcheck(4,1)) THEN
     listname = "BEAM_TORQUE"
     icomponent = ps_cnum_NBI
     nitems = 4
     idvec(1) = ss%id_tqbe
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_tqbi
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)
     idvec(1) = ss%id_tqbjxb
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(3) = ansvec(1,1)
     idvec(1) = ss%id_tqbth
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(4) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_B_SURFMAX
  ahash(2) = ps_phash_B_SURFMIN
  IF(ALLOCATED(ss%B_surfMax).AND.hcheck(2,1)) THEN
     listname = "BMINMAX"
     icomponent = ps_cnum_EQ
     nitems = 2
     ansvec(1,1) = ss%B_surfMax(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)
     ansvec(1,1) = ss%B_surfMin(ss%nrho_eq_geo)
     item_r8vals(2) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_QCOMP_E
  ahash(2) = ps_phash_QCOMP_I
  IF(ALLOCATED(ss%qcomp_e).AND.(id_intg.GT.0).AND.hcheck(2,1)) THEN
     listname = "COMPRESSION"
     icomponent = ps_cnum_PLASMA
     nitems = 2
     idvec(1) = ss%id_qcomp_e
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_qcomp_i
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_TRANS
  ahash(2) = ps_phash_CHII_TRANS
  ahash(3) = ps_phash_CHIMO_TRANS
  IF(ALLOCATED(ss%chie_trans).AND.hcheck(3,1)) THEN
     listname = "DIFFUSIVITY"
     icomponent = ps_cnum_PLASMA
     nitems = 3
     ansvec(1,1) = sum(ss%chie_trans)/size(ss%chie_trans)
     item_r8vals(1) = ansvec(1,1)
     ansvec(1,1) = sum(ss%chii_trans)/size(ss%chii_trans)
     item_r8vals(2) = ansvec(1,1)
     ansvec(1,1) = sum(ss%chimo_trans)/size(ss%chimo_trans)
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PFUSE
  ahash(2) = ps_phash_PFUSI
  ahash(3) = ps_phash_PFUSTH
  IF(ALLOCATED(ss%pfuse).AND.(id_intg.GT.0).AND.hcheck(3,1)) THEN
     listname = "FUSION_HEATING"
     icomponent = ps_cnum_FUS
     nitems = 3
     idvec(1) = ss%id_pfuse
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_pfusi
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)
     idvec(1) = ss%id_pfusth
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PELH
  ahash(2) = ps_phash_PILH
  IF(ALLOCATED(ss%pelh).AND.(id_intg.GT.0).AND.hcheck(2,1)) THEN
     listname = "LOWER_HYBRID_HEATING"
     icomponent = ps_cnum_LH
     nitems = 2
     idvec(1) = ss%id_pelh
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_pilh
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_Z_MIDP
  ahash(2) = ps_phash_R_MIDP_IN
  ahash(3) = ps_phash_R_MIDP_OUT
  IF(ALLOCATED(ss%Z_midp).AND.hcheck(3,1)) THEN
     listname = "MIDPLANE"
     icomponent = ps_cnum_EQ
     nitems = 3
     ansvec(1,1) = ss%Z_midp(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)
     ansvec(1,1) = ss%R_midp_in(ss%nrho_eq_geo)
     item_r8vals(2) = ansvec(1,1)
     ansvec(1,1) = ss%R_midp_out(ss%nrho_eq_geo)
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PMINE
  ahash(2) = ps_phash_PMINI
  ahash(3) = ps_phash_PMINTH
  IF(ALLOCATED(ss%pmine).AND.(id_intg.GT.0).AND.hcheck(3,1)) THEN
     listname = "MINORITY_HEATING"
     icomponent = ps_cnum_IC
     nitems = 3
     idvec(1) = ss%id_pmine
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_pmini
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)
     idvec(1) = ss%id_pminth
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VEE_TRANS
  ahash(2) = ps_phash_VIE_TRANS
  ahash(3) = ps_phash_VMO_TRANS
  IF(ALLOCATED(ss%vee_trans).AND.hcheck(3,1)) THEN
     listname = "RADIAL_VELOCITY"
     icomponent = ps_cnum_PLASMA
     nitems = 3
     ansvec(1,1) = sum(ss%vee_trans)/size(ss%vee_trans)
     item_r8vals(1) = ansvec(1,1)
     ansvec(1,1) = sum(ss%vie_trans)/size(ss%vie_trans)
     item_r8vals(2) = ansvec(1,1)
     ansvec(1,1) = sum(ss%vmo_trans)/size(ss%vmo_trans)
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PRAD
  ahash(2) = ps_phash_PRAD_CY
  ahash(3) = ps_phash_PRAD_BR
  ahash(4) = ps_phash_PRAD_LI
  IF(ALLOCATED(ss%prad).AND.(id_intg.GT.0).AND.hcheck(4,1)) THEN
     listname = "RADIATED_POWERS"
     icomponent = ps_cnum_RAD
     nitems = 4
     idvec(1) = ss%id_prad
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_prad_cy
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)
     idvec(1) = ss%id_prad_br
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(3) = ansvec(1,1)
     idvec(1) = ss%id_prad_li
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(4) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_P0_RECO
  ahash(2) = ps_phash_PSC_RECO
  ahash(3) = ps_phash_PCX_RECO
  IF(ALLOCATED(ss%p0_reco).AND.(id_intg.GT.0).AND.hcheck(3,1)) THEN
     listname = "RECO_POWER"
     icomponent = ps_cnum_GAS
     nitems = 3
     idvec(1) = ss%id_p0_reco
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_psc_reco
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)
     idvec(1) = ss%id_pcx_reco
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQ0_RECO
  ahash(2) = ps_phash_TQSC_RECO
  ahash(3) = ps_phash_TQCX_RECO
  IF(ALLOCATED(ss%tq0_reco).AND.(id_intg.GT.0).AND.hcheck(3,1)) THEN
     listname = "RECO_TORQUE"
     icomponent = ps_cnum_GAS
     nitems = 3
     idvec(1) = ss%id_tq0_reco
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_tqsc_reco
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)
     idvec(1) = ss%id_tqcx_reco
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RES_TE
  ahash(2) = ps_phash_RES_TI
  IF(ALLOCATED(ss%res_te).AND.(id_intg.GT.0).AND.hcheck(2,1)) THEN
     listname = "RESIDUAL"
     icomponent = ps_cnum_PLASMA
     nitems = 2
     idvec(1) = ss%id_res_te
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_res_ti
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TFRIP_LOG
  ahash(2) = ps_phash_TFRIP2_LOG
  IF(ALLOCATED(ss%tfrip_log).AND.hcheck(2,1)) THEN
     listname = "RIPPLE_FIELDS"
     icomponent = ps_cnum_RIPPLE
     nitems = 2
     ansvec(1,1) = sum(ss%tfrip_log)/size(ss%tfrip_log)
     item_r8vals(1) = ansvec(1,1)
     ansvec(1,1) = sum(ss%tfrip2_log)/size(ss%tfrip2_log)
     item_r8vals(2) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RMAJOR_MEAN
  ahash(2) = ps_phash_RMINOR_MEAN
  IF(ALLOCATED(ss%Rmajor_mean).AND.hcheck(2,1)) THEN
     listname = "RMAJMIN"
     icomponent = ps_cnum_EQ
     nitems = 2
     ansvec(1,1) = ss%Rmajor_mean(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)
     ansvec(1,1) = ss%rMinor_mean(ss%nrho_eq_geo)
     item_r8vals(2) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_QROT_CONV
  ahash(2) = ps_phash_QROT_DIFF
  IF(ALLOCATED(ss%qrot_conv).AND.(id_intg.GT.0).AND.hcheck(2,1)) THEN
     listname = "ROTATION_DISSIPATION"
     icomponent = ps_cnum_PLASMA
     nitems = 2
     idvec(1) = ss%id_qrot_conv
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_qrot_diff
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_R_SURFMAX
  ahash(2) = ps_phash_R_SURFMIN
  ahash(3) = ps_phash_Z_SURFMAX
  ahash(4) = ps_phash_Z_SURFMIN
  IF(ALLOCATED(ss%R_surfMax).AND.hcheck(4,1)) THEN
     listname = "RZMINMAX"
     icomponent = ps_cnum_EQ
     nitems = 4
     ansvec(1,1) = ss%R_surfMax(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)
     ansvec(1,1) = ss%R_surfMin(ss%nrho_eq_geo)
     item_r8vals(2) = ansvec(1,1)
     ansvec(1,1) = ss%Z_surfMax(ss%nrho_eq_geo)
     item_r8vals(3) = ansvec(1,1)
     ansvec(1,1) = ss%Z_surfMin(ss%nrho_eq_geo)
     item_r8vals(4) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_ELONG
  ahash(2) = ps_phash_TRIANG
  ahash(3) = ps_phash_TRIANGU
  ahash(4) = ps_phash_TRIANGL
  ahash(5) = ps_phash_SQUAREUO
  ahash(6) = ps_phash_SQUARELO
  IF(ALLOCATED(ss%elong).AND.hcheck(6,1)) THEN
     listname = "SURFSHAPE"
     icomponent = ps_cnum_EQ
     nitems = 6
     ansvec(1,1) = ss%elong(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)
     ansvec(1,1) = ss%triang(ss%nrho_eq_geo)
     item_r8vals(2) = ansvec(1,1)
     ansvec(1,1) = ss%triangU(ss%nrho_eq_geo)
     item_r8vals(3) = ansvec(1,1)
     ansvec(1,1) = ss%triangL(ss%nrho_eq_geo)
     item_r8vals(4) = ansvec(1,1)
     ansvec(1,1) = ss%squareUO(ss%nrho_eq_geo)
     item_r8vals(5) = ansvec(1,1)
     ansvec(1,1) = ss%squareLO(ss%nrho_eq_geo)
     item_r8vals(6) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PE_TRANS
  ahash(2) = ps_phash_PI_TRANS
  IF(ALLOCATED(ss%pe_trans).AND.(id_intg.GT.0).AND.hcheck(2,1)) THEN
     listname = "TRANSPORT"
     icomponent = ps_cnum_PLASMA
     nitems = 2
     idvec(1) = ss%id_pe_trans
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_pi_trans
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TRIANG_MILLER_U
  ahash(2) = ps_phash_TRIANG_MILLER_L
  IF(ALLOCATED(ss%triang_miller_U).AND.hcheck(2,1)) THEN
     listname = "TRIANG_MILLER"
     icomponent = ps_cnum_EQ
     nitems = 2
     ansvec(1,1) = ss%triang_miller_U(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)
     ansvec(1,1) = ss%triang_miller_L(ss%nrho_eq_geo)
     item_r8vals(2) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_UPWIND_PFRAC_TE
  ahash(2) = ps_phash_UPWIND_PFRAC_TI
  ahash(3) = ps_phash_UPWIND_PFRAC_OMEGA
  IF(ALLOCATED(ss%upwind_pfrac_Te).AND.hcheck(3,1)) THEN
     listname = "UPWIND_FORWARD_FRACTIONS"
     icomponent = ps_cnum_PLASMA
     nitems = 3
     ansvec(1,1) = sum(ss%upwind_pfrac_Te)/size(ss%upwind_pfrac_Te)
     item_r8vals(1) = ansvec(1,1)
     ansvec(1,1) = sum(ss%upwind_pfrac_Ti)/size(ss%upwind_pfrac_Ti)
     item_r8vals(2) = ansvec(1,1)
     ansvec(1,1) = sum(ss%upwind_pfrac_omega)/size(ss%upwind_pfrac_omega)
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_ZEFF
  ahash(2) = ps_phash_ZEFF_TH
  ahash(3) = ps_phash_ZEFF_FI
  IF(ALLOCATED(ss%Zeff).AND.(id_intg.GT.0).AND.hcheck(3,1)) THEN
     listname = "ZEFF_GROUP"
     icomponent = ps_cnum_PLASMA
     nitems = 3
     idvec(1) = ss%id_Zeff
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)
     idvec(1) = ss%id_Zeff_th
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(2) = ansvec(1,1)
     idvec(1) = ss%id_Zeff_fi
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(3) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  !----------------------------------------------------
  ! list values for singleton profiles

  ahash(1) = ps_phash_VOL
  IF(ALLOCATED(ss%vol).AND.hcheck(1,1)) THEN
     listname = "VOL_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%vol(ss%nrho_eq)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_AREA
  IF(ALLOCATED(ss%area).AND.hcheck(1,1)) THEN
     listname = "AREA_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%area(ss%nrho_eq)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_ANOM_EVAR
  IF(ALLOCATED(ss%anom_evar).AND.hcheck(1,1)) THEN
     listname = "ANOM_EVAR_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%anom_evar)/size(ss%anom_evar)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BALSTAB
  IF(ALLOCATED(ss%balstab).AND.hcheck(1,1)) THEN
     listname = "BALSTAB_LIST"
     icomponent = ps_cnum_LMHD
     nitems = 1
     ansvec(1,1) = sum(ss%balstab)/size(ss%balstab)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BPHIRZ
  IF(ALLOCATED(ss%BphiRZ).AND.hcheck(1,1)) THEN
     listname = "BPHIRZ_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = sum(ss%BphiRZ)/size(ss%BphiRZ)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BRRZ
  IF(ALLOCATED(ss%BRRZ).AND.hcheck(1,1)) THEN
     listname = "BRRZ_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = sum(ss%BRRZ)/size(ss%BRRZ)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BZRZ
  IF(ALLOCATED(ss%BZRZ).AND.hcheck(1,1)) THEN
     listname = "BZRZ_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = sum(ss%BZRZ)/size(ss%BZRZ)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_ANOM
  IF(ALLOCATED(ss%chie_anom).AND.hcheck(1,1)) THEN
     listname = "CHIE_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chie_anom)/size(ss%chie_anom)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_DRBM
  IF(ALLOCATED(ss%chie_drbm).AND.hcheck(1,1)) THEN
     listname = "CHIE_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chie_drbm)/size(ss%chie_drbm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_ETG
  IF(ALLOCATED(ss%chie_etg).AND.hcheck(1,1)) THEN
     listname = "CHIE_ETG_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chie_etg)/size(ss%chie_etg)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_GLF
  IF(ALLOCATED(ss%chie_glf).AND.hcheck(1,1)) THEN
     listname = "CHIE_GLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chie_glf)/size(ss%chie_glf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_GTCNEO
  IF(ALLOCATED(ss%chie_gtcneo).AND.hcheck(1,1)) THEN
     listname = "CHIE_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chie_gtcneo)/size(ss%chie_gtcneo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_MMM71
  IF(ALLOCATED(ss%chie_mmm71).AND.hcheck(1,1)) THEN
     listname = "CHIE_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chie_mmm71)/size(ss%chie_mmm71)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_MTM
  IF(ALLOCATED(ss%chie_mtm).AND.hcheck(1,1)) THEN
     listname = "CHIE_MTM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chie_mtm)/size(ss%chie_mtm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_NCLASS
  IF(ALLOCATED(ss%chie_nclass).AND.hcheck(1,1)) THEN
     listname = "CHIE_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chie_nclass)/size(ss%chie_nclass)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_NEO
  IF(ALLOCATED(ss%chie_neo).AND.hcheck(1,1)) THEN
     listname = "CHIE_NEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chie_neo)/size(ss%chie_neo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_NTM
  IF(ALLOCATED(ss%chie_ntm).AND.hcheck(1,1)) THEN
     listname = "CHIE_NTM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chie_ntm)/size(ss%chie_ntm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_PALEO
  IF(ALLOCATED(ss%chie_paleo).AND.hcheck(1,1)) THEN
     listname = "CHIE_PALEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chie_paleo)/size(ss%chie_paleo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_TGLF
  IF(ALLOCATED(ss%chie_tglf).AND.hcheck(1,1)) THEN
     listname = "CHIE_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chie_tglf)/size(ss%chie_tglf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_W19
  IF(ALLOCATED(ss%chie_w19).AND.hcheck(1,1)) THEN
     listname = "CHIE_W19_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chie_w19)/size(ss%chie_w19)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_ANOM
  IF(ALLOCATED(ss%chii_anom).AND.hcheck(1,1)) THEN
     listname = "CHII_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chii_anom)/size(ss%chii_anom)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_DRBM
  IF(ALLOCATED(ss%chii_drbm).AND.hcheck(1,1)) THEN
     listname = "CHII_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chii_drbm)/size(ss%chii_drbm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_GLF
  IF(ALLOCATED(ss%chii_glf).AND.hcheck(1,1)) THEN
     listname = "CHII_GLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chii_glf)/size(ss%chii_glf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_GTCNEO
  IF(ALLOCATED(ss%chii_gtcneo).AND.hcheck(1,1)) THEN
     listname = "CHII_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chii_gtcneo)/size(ss%chii_gtcneo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_MMM71
  IF(ALLOCATED(ss%chii_mmm71).AND.hcheck(1,1)) THEN
     listname = "CHII_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chii_mmm71)/size(ss%chii_mmm71)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_NCLASS
  IF(ALLOCATED(ss%chii_nclass).AND.hcheck(1,1)) THEN
     listname = "CHII_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chii_nclass)/size(ss%chii_nclass)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_NEO
  IF(ALLOCATED(ss%chii_neo).AND.hcheck(1,1)) THEN
     listname = "CHII_NEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chii_neo)/size(ss%chii_neo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_NTM
  IF(ALLOCATED(ss%chii_ntm).AND.hcheck(1,1)) THEN
     listname = "CHII_NTM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chii_ntm)/size(ss%chii_ntm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_TGLF
  IF(ALLOCATED(ss%chii_tglf).AND.hcheck(1,1)) THEN
     listname = "CHII_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chii_tglf)/size(ss%chii_tglf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_W19
  IF(ALLOCATED(ss%chii_w19).AND.hcheck(1,1)) THEN
     listname = "CHII_W19_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chii_w19)/size(ss%chii_w19)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_ANOM
  IF(ALLOCATED(ss%chiphi_anom).AND.hcheck(1,1)) THEN
     listname = "CHIPHI_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chiphi_anom)/size(ss%chiphi_anom)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_DRBM
  IF(ALLOCATED(ss%chiphi_drbm).AND.hcheck(1,1)) THEN
     listname = "CHIPHI_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chiphi_drbm)/size(ss%chiphi_drbm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_GLF
  IF(ALLOCATED(ss%chiphi_glf).AND.hcheck(1,1)) THEN
     listname = "CHIPHI_GLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chiphi_glf)/size(ss%chiphi_glf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_GTCNEO
  IF(ALLOCATED(ss%chiphi_gtcneo).AND.hcheck(1,1)) THEN
     listname = "CHIPHI_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chiphi_gtcneo)/size(ss%chiphi_gtcneo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_MMM71
  IF(ALLOCATED(ss%chiphi_mmm71).AND.hcheck(1,1)) THEN
     listname = "CHIPHI_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chiphi_mmm71)/size(ss%chiphi_mmm71)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_NEO
  IF(ALLOCATED(ss%chiphi_neo).AND.hcheck(1,1)) THEN
     listname = "CHIPHI_NEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chiphi_neo)/size(ss%chiphi_neo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_TGLF
  IF(ALLOCATED(ss%chiphi_tglf).AND.hcheck(1,1)) THEN
     listname = "CHIPHI_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chiphi_tglf)/size(ss%chiphi_tglf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_W19
  IF(ALLOCATED(ss%chiphi_w19).AND.hcheck(1,1)) THEN
     listname = "CHIPHI_W19_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chiphi_w19)/size(ss%chiphi_w19)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHITHETA_MMM71
  IF(ALLOCATED(ss%chitheta_mmm71).AND.hcheck(1,1)) THEN
     listname = "CHITHETA_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%chitheta_mmm71)/size(ss%chitheta_mmm71)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURBEAM
  IF(ALLOCATED(ss%curbeam).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "CURBEAM_LIST"
     icomponent = ps_cnum_NBI
     nitems = 1
     idvec(1) = ss%id_curbeam
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURECH
  IF(ALLOCATED(ss%curech).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "CURECH_LIST"
     icomponent = ps_cnum_EC
     nitems = 1
     idvec(1) = ss%id_curech
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURFUSN
  IF(ALLOCATED(ss%curfusn).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "CURFUSN_LIST"
     icomponent = ps_cnum_FUS
     nitems = 1
     idvec(1) = ss%id_curfusn
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURICH
  IF(ALLOCATED(ss%curich).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "CURICH_LIST"
     icomponent = ps_cnum_IC
     nitems = 1
     idvec(1) = ss%id_curich
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURLH
  IF(ALLOCATED(ss%curlh).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "CURLH_LIST"
     icomponent = ps_cnum_LH
     nitems = 1
     idvec(1) = ss%id_curlh
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURMINO
  IF(ALLOCATED(ss%curmino).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "CURMINO_LIST"
     icomponent = ps_cnum_IC
     nitems = 1
     idvec(1) = ss%id_curmino
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURR_BOOTSTRAP
  IF(ALLOCATED(ss%curr_bootstrap).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "CURR_BOOTSTRAP_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     idvec(1) = ss%id_curr_bootstrap
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURR_OHMIC
  IF(ALLOCATED(ss%curr_ohmic).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "CURR_OHMIC_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     idvec(1) = ss%id_curr_ohmic
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURT
  IF(ALLOCATED(ss%curt).AND.hcheck(1,1)) THEN
     listname = "CURT_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%curt(ss%nrho_eq)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CUR_RW
  IF(ALLOCATED(ss%cur_rw).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "CUR_RW_LIST"
     icomponent = ps_cnum_RUNAWAY
     nitems = 1
     idvec(1) = ss%id_cur_rw
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DB_MTM
  IF(ALLOCATED(ss%db_mtm).AND.hcheck(1,1)) THEN
     listname = "DB_MTM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%db_mtm)/size(ss%db_mtm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DCUR_RW_DVLOOP
  IF(ALLOCATED(ss%dcur_rw_dVloop).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "DCUR_RW_DVLOOP_LIST"
     icomponent = ps_cnum_RUNAWAY
     nitems = 1
     idvec(1) = ss%id_dcur_rw_dVloop
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNE_NCLASS
  IF(ALLOCATED(ss%difne_nclass).AND.hcheck(1,1)) THEN
     listname = "DIFNE_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%difne_nclass)/size(ss%difne_nclass)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNH_NCLASS
  IF(ALLOCATED(ss%difnh_nclass).AND.hcheck(1,1)) THEN
     listname = "DIFNH_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%difnh_nclass)/size(ss%difnh_nclass)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNX_NCLASS
  IF(ALLOCATED(ss%difnx_nclass).AND.hcheck(1,1)) THEN
     listname = "DIFNX_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%difnx_nclass)/size(ss%difnx_nclass)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNZ_MMM71
  IF(ALLOCATED(ss%difnz_mmm71).AND.hcheck(1,1)) THEN
     listname = "DIFNZ_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%difnz_mmm71)/size(ss%difnz_mmm71)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DQEDGRDTE_TGLF
  IF(ALLOCATED(ss%dQedGrdte_tglf).AND.hcheck(1,1)) THEN
     listname = "DQEDGRDTE_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%dQedGrdte_tglf)/size(ss%dQedGrdte_tglf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DQEDGRDTI_TGLF
  IF(ALLOCATED(ss%dQedGrdti_tglf).AND.hcheck(1,1)) THEN
     listname = "DQEDGRDTI_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%dQedGrdti_tglf)/size(ss%dQedGrdti_tglf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DQIDGRDTE_TGLF
  IF(ALLOCATED(ss%dQidGrdte_tglf).AND.hcheck(1,1)) THEN
     listname = "DQIDGRDTE_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%dQidGrdte_tglf)/size(ss%dQidGrdte_tglf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DQIDGRDTI_TGLF
  IF(ALLOCATED(ss%dQidGrdti_tglf).AND.hcheck(1,1)) THEN
     listname = "DQIDGRDTI_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%dQidGrdti_tglf)/size(ss%dQidGrdti_tglf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EE_MOBILITY_FACTOR
  IF(ALLOCATED(ss%ee_mobility_factor).AND.hcheck(1,1)) THEN
     listname = "EE_MOBILITY_FACTOR_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%ee_mobility_factor)/size(ss%ee_mobility_factor)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPERP_RW
  IF(ALLOCATED(ss%eperp_rw).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "EPERP_RW_LIST"
     icomponent = ps_cnum_RUNAWAY
     nitems = 1
     idvec(1) = ss%id_eperp_rw
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPLL_RW
  IF(ALLOCATED(ss%epll_rw).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "EPLL_RW_LIST"
     icomponent = ps_cnum_RUNAWAY
     nitems = 1
     idvec(1) = ss%id_epll_rw
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPOT
  IF(ALLOCATED(ss%Epot).AND.hcheck(1,1)) THEN
     listname = "EPOT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     ansvec(1,1) = sum(ss%Epot)/size(ss%Epot)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_ETA_PARALLEL
  IF(ALLOCATED(ss%eta_parallel).AND.hcheck(1,1)) THEN
     listname = "ETA_PARALLEL_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     ansvec(1,1) = sum(ss%eta_parallel)/size(ss%eta_parallel)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FI_DEPLETION
  IF(ALLOCATED(ss%Fi_depletion).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "FI_DEPLETION_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     idvec(1) = ss%id_Fi_depletion
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_DRBM
  IF(ALLOCATED(ss%freq_drbm).AND.hcheck(1,1)) THEN
     listname = "FREQ_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%freq_drbm)/size(ss%freq_drbm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_ETG
  IF(ALLOCATED(ss%freq_etg).AND.hcheck(1,1)) THEN
     listname = "FREQ_ETG_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%freq_etg)/size(ss%freq_etg)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_GLF
  IF(ALLOCATED(ss%freq_glf).AND.hcheck(1,1)) THEN
     listname = "FREQ_GLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%freq_glf)/size(ss%freq_glf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_MAX_TGLF
  IF(ALLOCATED(ss%freq_max_tglf).AND.hcheck(1,1)) THEN
     listname = "FREQ_MAX_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%freq_max_tglf)/size(ss%freq_max_tglf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_MTM
  IF(ALLOCATED(ss%freq_mtm).AND.hcheck(1,1)) THEN
     listname = "FREQ_MTM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%freq_mtm)/size(ss%freq_mtm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GAMMA_NC
  IF(ALLOCATED(ss%gamma_nc).AND.hcheck(1,1)) THEN
     listname = "GAMMA_NC_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%gamma_nc(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GB1
  IF(ALLOCATED(ss%gb1).AND.hcheck(1,1)) THEN
     listname = "GB1_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%gb1(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GB2
  IF(ALLOCATED(ss%gb2).AND.hcheck(1,1)) THEN
     listname = "GB2_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%gb2(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GB2I
  IF(ALLOCATED(ss%gb2i).AND.hcheck(1,1)) THEN
     listname = "GB2I_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%gb2i(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GBR2
  IF(ALLOCATED(ss%gbr2).AND.hcheck(1,1)) THEN
     listname = "GBR2_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%gbr2(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GNCFB2H
  IF(ALLOCATED(ss%gncfb2h).AND.hcheck(1,1)) THEN
     listname = "GNCFB2H_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%gncfb2h(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GNCFH
  IF(ALLOCATED(ss%gncfh).AND.hcheck(1,1)) THEN
     listname = "GNCFH_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%gncfh(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GR1
  IF(ALLOCATED(ss%gr1).AND.hcheck(1,1)) THEN
     listname = "GR1_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%gr1(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GR2
  IF(ALLOCATED(ss%gr2).AND.hcheck(1,1)) THEN
     listname = "GR2_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%gr2(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GR2I
  IF(ALLOCATED(ss%gr2i).AND.hcheck(1,1)) THEN
     listname = "GR2I_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%gr2i(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GR2RHO2
  IF(ALLOCATED(ss%gr2rho2).AND.hcheck(1,1)) THEN
     listname = "GR2RHO2_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%gr2rho2(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GR3I
  IF(ALLOCATED(ss%gr3i).AND.hcheck(1,1)) THEN
     listname = "GR3I_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%gr3i(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRHO1
  IF(ALLOCATED(ss%grho1).AND.hcheck(1,1)) THEN
     listname = "GRHO1_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%grho1(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRHO2
  IF(ALLOCATED(ss%grho2).AND.hcheck(1,1)) THEN
     listname = "GRHO2_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%grho2(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRHO2B2I
  IF(ALLOCATED(ss%grho2b2i).AND.hcheck(1,1)) THEN
     listname = "GRHO2B2I_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%grho2b2i(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRHO2R2I
  IF(ALLOCATED(ss%grho2r2i).AND.hcheck(1,1)) THEN
     listname = "GRHO2R2I_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%grho2r2i(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRHO2R3I
  IF(ALLOCATED(ss%grho2r3i).AND.hcheck(1,1)) THEN
     listname = "GRHO2R3I_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%grho2r3i(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRI
  IF(ALLOCATED(ss%gri).AND.hcheck(1,1)) THEN
     listname = "GRI_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%gri(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRIRHOI
  IF(ALLOCATED(ss%grirhoi).AND.hcheck(1,1)) THEN
     listname = "GRIRHOI_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%grirhoi(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_DRBM
  IF(ALLOCATED(ss%growthrate_drbm).AND.hcheck(1,1)) THEN
     listname = "GROWTHRATE_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%growthrate_drbm)/size(ss%growthrate_drbm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_ETG
  IF(ALLOCATED(ss%growthrate_etg).AND.hcheck(1,1)) THEN
     listname = "GROWTHRATE_ETG_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%growthrate_etg)/size(ss%growthrate_etg)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_GLF
  IF(ALLOCATED(ss%growthrate_glf).AND.hcheck(1,1)) THEN
     listname = "GROWTHRATE_GLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%growthrate_glf)/size(ss%growthrate_glf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_MAX_TGLF
  IF(ALLOCATED(ss%growthrate_max_tglf).AND.hcheck(1,1)) THEN
     listname = "GROWTHRATE_MAX_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%growthrate_max_tglf)/size(ss%growthrate_max_tglf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_MTM
  IF(ALLOCATED(ss%growthrate_mtm).AND.hcheck(1,1)) THEN
     listname = "GROWTHRATE_MTM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%growthrate_mtm)/size(ss%growthrate_mtm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_G_EQ
  IF(ALLOCATED(ss%g_eq).AND.hcheck(1,1)) THEN
     listname = "G_EQ_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%g_eq(ss%nrho_eq)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_IOTA
  IF(ALLOCATED(ss%iota).AND.hcheck(1,1)) THEN
     listname = "IOTA_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     ansvec(1,1) = sum(ss%iota)/size(ss%iota)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_JDOTB
  IF(ALLOCATED(ss%jdotb).AND.hcheck(1,1)) THEN
     listname = "JDOTB_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%jdotb(ss%nrho_eq)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_KYRHOS_DRBM
  IF(ALLOCATED(ss%kyrhos_drbm).AND.hcheck(1,1)) THEN
     listname = "KYRHOS_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%kyrhos_drbm)/size(ss%kyrhos_drbm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_KYRHOS_ETG
  IF(ALLOCATED(ss%kyrhos_etg).AND.hcheck(1,1)) THEN
     listname = "KYRHOS_ETG_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%kyrhos_etg)/size(ss%kyrhos_etg)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_KYRHOS_MTM
  IF(ALLOCATED(ss%kyrhos_mtm).AND.hcheck(1,1)) THEN
     listname = "KYRHOS_MTM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%kyrhos_mtm)/size(ss%kyrhos_mtm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_LPOL
  IF(ALLOCATED(ss%Lpol).AND.hcheck(1,1)) THEN
     listname = "LPOL_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%Lpol(ss%nrho_eq)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NGRADB2_AV
  IF(ALLOCATED(ss%ngradb2_av).AND.hcheck(1,1)) THEN
     listname = "NGRADB2_AV_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%ngradb2_av(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NI
  IF(ALLOCATED(ss%ni).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "NI_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     idvec(1) = ss%id_ni
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NMODEL
  IF(ALLOCATED(ss%nmodel).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "NMODEL_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     idvec(1) = ss%id_nmodel
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NRW
  IF(ALLOCATED(ss%nrw).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "NRW_LIST"
     icomponent = ps_cnum_RUNAWAY
     nitems = 1
     idvec(1) = ss%id_nrw
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_OMEGAT
  IF(ALLOCATED(ss%omegat).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "OMEGAT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     idvec(1) = ss%id_omegat
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_OMEGA_EXB
  IF(ALLOCATED(ss%omega_exb).AND.hcheck(1,1)) THEN
     listname = "OMEGA_EXB_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%omega_exb)/size(ss%omega_exb)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PEECH
  IF(ALLOCATED(ss%peech).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "PEECH_LIST"
     icomponent = ps_cnum_EC
     nitems = 1
     idvec(1) = ss%id_peech
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PE_ANOM
  IF(ALLOCATED(ss%pe_anom).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "PE_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     idvec(1) = ss%id_pe_anom
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PHIT
  IF(ALLOCATED(ss%phit).AND.hcheck(1,1)) THEN
     listname = "PHIT_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%phit(ss%nrho_eq)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PICTH
  IF(ALLOCATED(ss%picth).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "PICTH_LIST"
     icomponent = ps_cnum_IC
     nitems = 1
     idvec(1) = ss%id_picth
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PI_ANOM
  IF(ALLOCATED(ss%pi_anom).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "PI_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     idvec(1) = ss%id_pi_anom
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_POHME
  IF(ALLOCATED(ss%pohme).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "POHME_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     idvec(1) = ss%id_pohme
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PSIPOL
  IF(ALLOCATED(ss%psipol).AND.hcheck(1,1)) THEN
     listname = "PSIPOL_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%psipol(ss%nrho_eq)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PSIRZ
  IF(ALLOCATED(ss%PsiRZ).AND.hcheck(1,1)) THEN
     listname = "PSIRZ_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = sum(ss%PsiRZ)/size(ss%PsiRZ)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PSMOM_ERRCK
  IF(ALLOCATED(ss%psmom_errck).AND.hcheck(1,1)) THEN
     listname = "PSMOM_ERRCK_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%psmom_errck(ss%nrho_eq_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_P_EQ
  IF(ALLOCATED(ss%P_eq).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "P_EQ_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     idvec(1) = ss%id_P_eq
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_QIE
  IF(ALLOCATED(ss%qie).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "QIE_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     idvec(1) = ss%id_qie
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_Q_EQ
  IF(ALLOCATED(ss%q_eq).AND.hcheck(1,1)) THEN
     listname = "Q_EQ_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%q_eq(ss%nrho_eq)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_R0_MOMEQ
  ahash(2) = ps_phash_DR0_MOMEQ_DRHO
  IF(ALLOCATED(ss%R0_momeq).AND.hcheck(2,1)) THEN
     listname = "R0_MOMEQ_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%R0_momeq(ss%nrho_eq)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RES_TQ
  IF(ALLOCATED(ss%res_tq).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "RES_TQ_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     idvec(1) = ss%id_res_tq
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_R_GEO
  ahash(2) = ps_phash_DR_GEO_DRHO
  ahash(3) = ps_phash_DR_GEO_DTH
  ahash(4) = ps_phash_D2R_GEO_DRHODTH
  IF(ALLOCATED(ss%R_geo).AND.hcheck(4,1)) THEN
     listname = "R_GEO_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = sum(ss%R_geo)/size(ss%R_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_S0RECO_E
  IF(ALLOCATED(ss%s0reco_e).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "S0RECO_E_LIST"
     icomponent = ps_cnum_GAS
     nitems = 1
     idvec(1) = ss%id_s0reco_e
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SBEDEP
  IF(ALLOCATED(ss%sbedep).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SBEDEP_LIST"
     icomponent = ps_cnum_NBI
     nitems = 1
     idvec(1) = ss%id_sbedep
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SBEHALO
  IF(ALLOCATED(ss%sbehalo).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SBEHALO_LIST"
     icomponent = ps_cnum_NBI
     nitems = 1
     idvec(1) = ss%id_sbehalo
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SURF
  IF(ALLOCATED(ss%surf).AND.hcheck(1,1)) THEN
     listname = "SURF_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%surf(ss%nrho_eq)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TI
  IF(ALLOCATED(ss%Ti).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "TI_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     idvec(1) = ss%id_Ti
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TMODEL
  IF(ALLOCATED(ss%tmodel).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "TMODEL_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     idvec(1) = ss%id_tmodel
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQ_ANOM
  IF(ALLOCATED(ss%tq_anom).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "TQ_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     idvec(1) = ss%id_tq_anom
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQ_TRANS
  IF(ALLOCATED(ss%tq_trans).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "TQ_TRANS_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     idvec(1) = ss%id_tq_trans
     idwvec(1) = ss%id_zonwt(idvec(1))
     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
          .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNE_NCLASS
  IF(ALLOCATED(ss%velne_nclass).AND.hcheck(1,1)) THEN
     listname = "VELNE_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velne_nclass)/size(ss%velne_nclass)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNH_NCLASS
  IF(ALLOCATED(ss%velnh_nclass).AND.hcheck(1,1)) THEN
     listname = "VELNH_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velnh_nclass)/size(ss%velnh_nclass)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNI_DRBM
  IF(ALLOCATED(ss%velni_drbm).AND.hcheck(1,1)) THEN
     listname = "VELNI_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velni_drbm)/size(ss%velni_drbm)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNX_NCLASS
  IF(ALLOCATED(ss%velnx_nclass).AND.hcheck(1,1)) THEN
     listname = "VELNX_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velnx_nclass)/size(ss%velnx_nclass)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELPPHI_ANOM
  IF(ALLOCATED(ss%velpphi_anom).AND.hcheck(1,1)) THEN
     listname = "VELPPHI_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velpphi_anom)/size(ss%velpphi_anom)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELPPHI_GLF
  IF(ALLOCATED(ss%velpphi_glf).AND.hcheck(1,1)) THEN
     listname = "VELPPHI_GLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velpphi_glf)/size(ss%velpphi_glf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELPPHI_GTCNEO
  IF(ALLOCATED(ss%velpphi_gtcneo).AND.hcheck(1,1)) THEN
     listname = "VELPPHI_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velpphi_gtcneo)/size(ss%velpphi_gtcneo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELPPHI_MMM71
  IF(ALLOCATED(ss%velpphi_mmm71).AND.hcheck(1,1)) THEN
     listname = "VELPPHI_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velpphi_mmm71)/size(ss%velpphi_mmm71)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELPPHI_NEO
  IF(ALLOCATED(ss%velpphi_neo).AND.hcheck(1,1)) THEN
     listname = "VELPPHI_NEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velpphi_neo)/size(ss%velpphi_neo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELPPHI_TGLF
  IF(ALLOCATED(ss%velpphi_tglf).AND.hcheck(1,1)) THEN
     listname = "VELPPHI_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velpphi_tglf)/size(ss%velpphi_tglf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_ANOM
  IF(ALLOCATED(ss%velte_anom).AND.hcheck(1,1)) THEN
     listname = "VELTE_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velte_anom)/size(ss%velte_anom)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_GLF
  IF(ALLOCATED(ss%velte_glf).AND.hcheck(1,1)) THEN
     listname = "VELTE_GLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velte_glf)/size(ss%velte_glf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_GTCNEO
  IF(ALLOCATED(ss%velte_gtcneo).AND.hcheck(1,1)) THEN
     listname = "VELTE_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velte_gtcneo)/size(ss%velte_gtcneo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_MMM71
  IF(ALLOCATED(ss%velte_mmm71).AND.hcheck(1,1)) THEN
     listname = "VELTE_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velte_mmm71)/size(ss%velte_mmm71)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_NCLASS
  IF(ALLOCATED(ss%velte_nclass).AND.hcheck(1,1)) THEN
     listname = "VELTE_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velte_nclass)/size(ss%velte_nclass)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_NEO
  IF(ALLOCATED(ss%velte_neo).AND.hcheck(1,1)) THEN
     listname = "VELTE_NEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velte_neo)/size(ss%velte_neo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_PALEO
  IF(ALLOCATED(ss%velte_paleo).AND.hcheck(1,1)) THEN
     listname = "VELTE_PALEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velte_paleo)/size(ss%velte_paleo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_TGLF
  IF(ALLOCATED(ss%velte_tglf).AND.hcheck(1,1)) THEN
     listname = "VELTE_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velte_tglf)/size(ss%velte_tglf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_ANOM
  IF(ALLOCATED(ss%velti_anom).AND.hcheck(1,1)) THEN
     listname = "VELTI_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velti_anom)/size(ss%velti_anom)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_GLF
  IF(ALLOCATED(ss%velti_glf).AND.hcheck(1,1)) THEN
     listname = "VELTI_GLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velti_glf)/size(ss%velti_glf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_GTCNEO
  IF(ALLOCATED(ss%velti_gtcneo).AND.hcheck(1,1)) THEN
     listname = "VELTI_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velti_gtcneo)/size(ss%velti_gtcneo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_MMM71
  IF(ALLOCATED(ss%velti_mmm71).AND.hcheck(1,1)) THEN
     listname = "VELTI_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velti_mmm71)/size(ss%velti_mmm71)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_NCLASS
  IF(ALLOCATED(ss%velti_nclass).AND.hcheck(1,1)) THEN
     listname = "VELTI_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velti_nclass)/size(ss%velti_nclass)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_NEO
  IF(ALLOCATED(ss%velti_neo).AND.hcheck(1,1)) THEN
     listname = "VELTI_NEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velti_neo)/size(ss%velti_neo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_TGLF
  IF(ALLOCATED(ss%velti_tglf).AND.hcheck(1,1)) THEN
     listname = "VELTI_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 1
     ansvec(1,1) = sum(ss%velti_tglf)/size(ss%velti_tglf)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_V_LOOP
  IF(ALLOCATED(ss%V_loop).AND.hcheck(1,1)) THEN
     listname = "V_LOOP_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 1
     ansvec(1,1) = sum(ss%V_loop)/size(ss%V_loop)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_Z0_MOMEQ
  ahash(2) = ps_phash_DZ0_MOMEQ_DRHO
  IF(ALLOCATED(ss%Z0_momeq).AND.hcheck(2,1)) THEN
     listname = "Z0_MOMEQ_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = ss%Z0_momeq(ss%nrho_eq)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_Z_GEO
  ahash(2) = ps_phash_DZ_GEO_DRHO
  ahash(3) = ps_phash_DZ_GEO_DTH
  ahash(4) = ps_phash_D2Z_GEO_DRHODTH
  IF(ALLOCATED(ss%Z_geo).AND.hcheck(4,1)) THEN
     listname = "Z_GEO_LIST"
     icomponent = ps_cnum_EQ
     nitems = 1
     ansvec(1,1) = sum(ss%Z_geo)/size(ss%Z_geo)
     item_r8vals(1) = ansvec(1,1)

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  !----------------------------------------------------
  ! update list data on indexed arrays of profiles

  ahash(1) = ps_phash_BIDIFF_BPASS_CO
  IF(ALLOCATED(ss%bidiff_bpass_co).AND.hcheck(1,1)) THEN
     listname = "BIDIFF_BPASS_CO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%bidiff_bpass_co(:,:,I1))/size(ss%bidiff_bpass_co(:,:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BIDIFF_BPASS_CTR
  IF(ALLOCATED(ss%bidiff_bpass_ctr).AND.hcheck(1,1)) THEN
     listname = "BIDIFF_BPASS_CTR_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%bidiff_bpass_ctr(:,:,I1))/size(ss%bidiff_bpass_ctr(:,:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BIDIFF_BTRAP
  IF(ALLOCATED(ss%bidiff_btrap).AND.hcheck(1,1)) THEN
     listname = "BIDIFF_BTRAP_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%bidiff_btrap(:,:,I1))/size(ss%bidiff_btrap(:,:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BIDIFF_DPASS_CO
  IF(ALLOCATED(ss%bidiff_dpass_co).AND.hcheck(1,1)) THEN
     listname = "BIDIFF_DPASS_CO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%bidiff_dpass_co(:,:,I1))/size(ss%bidiff_dpass_co(:,:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BIDIFF_DPASS_CTR
  IF(ALLOCATED(ss%bidiff_dpass_ctr).AND.hcheck(1,1)) THEN
     listname = "BIDIFF_DPASS_CTR_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%bidiff_dpass_ctr(:,:,I1))/size(ss%bidiff_dpass_ctr(:,:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BIDIFF_DTRAP
  IF(ALLOCATED(ss%bidiff_dtrap).AND.hcheck(1,1)) THEN
     listname = "BIDIFF_DTRAP_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%bidiff_dtrap(:,:,I1))/size(ss%bidiff_dtrap(:,:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CDICRF
  IF(ALLOCATED(ss%cdicrf).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "CDICRF_LIST"
     icomponent = ps_cnum_IC
     nitems = 0
     DO I1 = 1,ss%nicrf_src
        nitems = nitems + 1
        idvec(1) = ss%id_cdicrf(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CDICRF_NPHI
  IF(ALLOCATED(ss%cdicrf_nphi).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "CDICRF_NPHI_LIST"
     icomponent = ps_cnum_IC
     nitems = 0
     DO I2 = 1,ss%nicrf_src
        DO I1 = 1,ss%max_num_nphi
           idvec(1) = ss%id_cdicrf_nphi(I1,I2)
           IF(idvec(1).EQ.0) CYCLE
           nitems = nitems + 1
           idwvec(1) = ss%id_zonwt(idvec(1))
           call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
                .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
           IF(ierr.NE.0) RETURN
           item_r8vals(nitems) = ansvec(1,1)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_TRANSCAT
  IF(ALLOCATED(ss%chie_transcat).AND.hcheck(1,1)) THEN
     listname = "CHIE_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 1,ss%ntrcat
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%chie_transcat(:,I1))/size(ss%chie_transcat(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_TRANSCAT
  IF(ALLOCATED(ss%chii_transcat).AND.hcheck(1,1)) THEN
     listname = "CHII_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 1,ss%ntrcat
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%chii_transcat(:,I1))/size(ss%chii_transcat(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIMO_TRANSCAT
  IF(ALLOCATED(ss%chimo_transcat).AND.hcheck(1,1)) THEN
     listname = "CHIMO_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 1,ss%ntrcat
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%chimo_transcat(:,I1))/size(ss%chimo_transcat(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURECH_SRC
  IF(ALLOCATED(ss%curech_src).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "CURECH_SRC_LIST"
     icomponent = ps_cnum_EC
     nitems = 0
     DO I1 = 1,ss%necrf_src
        nitems = nitems + 1
        idvec(1) = ss%id_curech_src(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURLH_SRC
  IF(ALLOCATED(ss%curlh_src).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "CURLH_SRC_LIST"
     icomponent = ps_cnum_LH
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        nitems = nitems + 1
        idvec(1) = ss%id_curlh_src(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .TRUE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFF_TRANS
  IF(ALLOCATED(ss%diff_trans).AND.hcheck(1,1)) THEN
     listname = "DIFF_TRANS_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%diff_trans(:,I1))/size(ss%diff_trans(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFF_TRANSCAT
  IF(ALLOCATED(ss%diff_transcat).AND.hcheck(1,1)) THEN
     listname = "DIFF_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I2 = 1,ss%ntrcat
        DO I1 = 0,ss%nspec_th
           nitems = nitems + 1
           ansvec(1,1) = sum(ss%diff_transcat(:,I1,I2))/size(ss%diff_transcat(:,I1,I2))
           item_r8vals(nitems) = ansvec(1,1)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_ANOM
  IF(ALLOCATED(ss%difns_anom).AND.hcheck(1,1)) THEN
     listname = "DIFNS_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%difns_anom(:,I1))/size(ss%difns_anom(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_DRBM
  IF(ALLOCATED(ss%difns_drbm).AND.hcheck(1,1)) THEN
     listname = "DIFNS_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%difns_drbm(:,I1))/size(ss%difns_drbm(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_GLF
  IF(ALLOCATED(ss%difns_glf).AND.hcheck(1,1)) THEN
     listname = "DIFNS_GLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%difns_glf(:,I1))/size(ss%difns_glf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_GTCNEO
  IF(ALLOCATED(ss%difns_gtcneo).AND.hcheck(1,1)) THEN
     listname = "DIFNS_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%difns_gtcneo(:,I1))/size(ss%difns_gtcneo(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_MMM71
  IF(ALLOCATED(ss%difns_mmm71).AND.hcheck(1,1)) THEN
     listname = "DIFNS_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%difns_mmm71(:,I1))/size(ss%difns_mmm71(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_NCLASS
  IF(ALLOCATED(ss%difns_nclass).AND.hcheck(1,1)) THEN
     listname = "DIFNS_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%difns_nclass(:,I1))/size(ss%difns_nclass(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_NEO
  IF(ALLOCATED(ss%difns_neo).AND.hcheck(1,1)) THEN
     listname = "DIFNS_NEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%difns_neo(:,I1))/size(ss%difns_neo(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_TGLF
  IF(ALLOCATED(ss%difns_tglf).AND.hcheck(1,1)) THEN
     listname = "DIFNS_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%difns_tglf(:,I1))/size(ss%difns_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNS_W19
  IF(ALLOCATED(ss%difns_w19).AND.hcheck(1,1)) THEN
     listname = "DIFNS_W19_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%difns_w19(:,I1))/size(ss%difns_w19(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPERP_BEAMI
  IF(ALLOCATED(ss%eperp_beami).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "EPERP_BEAMI_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        nitems = nitems + 1
        idvec(1) = ss%id_eperp_beami(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPERP_FUSI
  IF(ALLOCATED(ss%eperp_fusi).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "EPERP_FUSI_LIST"
     icomponent = ps_cnum_FUS
     nitems = 0
     DO I1 = 1,ss%nspec_fusion
        nitems = nitems + 1
        idvec(1) = ss%id_eperp_fusi(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPERP_MINI
  IF(ALLOCATED(ss%eperp_mini).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "EPERP_MINI_LIST"
     icomponent = ps_cnum_IC
     nitems = 0
     DO I1 = 1,ss%nspec_rfmin
        nitems = nitems + 1
        idvec(1) = ss%id_eperp_mini(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPLL_BEAMI
  IF(ALLOCATED(ss%epll_beami).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "EPLL_BEAMI_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        nitems = nitems + 1
        idvec(1) = ss%id_epll_beami(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPLL_FUSI
  IF(ALLOCATED(ss%epll_fusi).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "EPLL_FUSI_LIST"
     icomponent = ps_cnum_FUS
     nitems = 0
     DO I1 = 1,ss%nspec_fusion
        nitems = nitems + 1
        idvec(1) = ss%id_epll_fusi(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPLL_MINI
  IF(ALLOCATED(ss%epll_mini).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "EPLL_MINI_LIST"
     icomponent = ps_cnum_IC
     nitems = 0
     DO I1 = 1,ss%nspec_rfmin
        nitems = nitems + 1
        idvec(1) = ss%id_epll_mini(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FIDIFF_BPASS_CO
  IF(ALLOCATED(ss%fidiff_bpass_co).AND.hcheck(1,1)) THEN
     listname = "FIDIFF_BPASS_CO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nspec_fusion
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%fidiff_bpass_co(:,:,I1))/size(ss%fidiff_bpass_co(:,:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FIDIFF_BPASS_CTR
  IF(ALLOCATED(ss%fidiff_bpass_ctr).AND.hcheck(1,1)) THEN
     listname = "FIDIFF_BPASS_CTR_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nspec_fusion
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%fidiff_bpass_ctr(:,:,I1))/size(ss%fidiff_bpass_ctr(:,:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FIDIFF_BTRAP
  IF(ALLOCATED(ss%fidiff_btrap).AND.hcheck(1,1)) THEN
     listname = "FIDIFF_BTRAP_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nspec_fusion
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%fidiff_btrap(:,:,I1))/size(ss%fidiff_btrap(:,:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FIDIFF_DPASS_CO
  IF(ALLOCATED(ss%fidiff_dpass_co).AND.hcheck(1,1)) THEN
     listname = "FIDIFF_DPASS_CO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nspec_fusion
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%fidiff_dpass_co(:,:,I1))/size(ss%fidiff_dpass_co(:,:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FIDIFF_DPASS_CTR
  IF(ALLOCATED(ss%fidiff_dpass_ctr).AND.hcheck(1,1)) THEN
     listname = "FIDIFF_DPASS_CTR_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nspec_fusion
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%fidiff_dpass_ctr(:,:,I1))/size(ss%fidiff_dpass_ctr(:,:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FIDIFF_DTRAP
  IF(ALLOCATED(ss%fidiff_dtrap).AND.hcheck(1,1)) THEN
     listname = "FIDIFF_DTRAP_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nspec_fusion
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%fidiff_dtrap(:,:,I1))/size(ss%fidiff_dtrap(:,:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_MMM71
  IF(ALLOCATED(ss%freq_mmm71).AND.hcheck(1,1)) THEN
     listname = "FREQ_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_mmm
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%freq_mmm71(:,I1))/size(ss%freq_mmm71(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_TGLF
  IF(ALLOCATED(ss%freq_tglf).AND.hcheck(1,1)) THEN
     listname = "FREQ_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%freq_tglf(:,I1))/size(ss%freq_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_MMM71
  IF(ALLOCATED(ss%growthrate_mmm71).AND.hcheck(1,1)) THEN
     listname = "GROWTHRATE_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_mmm
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%growthrate_mmm71(:,I1))/size(ss%growthrate_mmm71(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_TGLF
  IF(ALLOCATED(ss%growthrate_tglf).AND.hcheck(1,1)) THEN
     listname = "GROWTHRATE_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%growthrate_tglf(:,I1))/size(ss%growthrate_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_VPAR
  IF(ALLOCATED(ss%growthrate_vpar).AND.hcheck(1,1)) THEN
     listname = "GROWTHRATE_VPAR_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%growthrate_vpar(:,I1))/size(ss%growthrate_vpar(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_KYSPECTRUM_TGLF
  IF(ALLOCATED(ss%kyspectrum_tglf).AND.hcheck(1,1)) THEN
     listname = "KYSPECTRUM_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%kyspectrum_tglf(:,I1))/size(ss%kyspectrum_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_N0NORM
  IF(ALLOCATED(ss%n0norm).AND.hcheck(1,1)) THEN
     listname = "N0NORM_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I2 = 1,ss%ngsc0
        DO I1 = 1,ss%nspec_gas
           nitems = nitems + 1
           ansvec(1,1) = sum(ss%n0norm(:,I1,I2))/size(ss%n0norm(:,I1,I2))
           item_r8vals(nitems) = ansvec(1,1)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_N0_HALO
  IF(ALLOCATED(ss%n0_halo).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "N0_HALO_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        idvec(1) = ss%id_n0_halo(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_N0_RECO
  IF(ALLOCATED(ss%n0_reco).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "N0_RECO_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        idvec(1) = ss%id_n0_reco(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NBEAMI
  IF(ALLOCATED(ss%nbeami).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "NBEAMI_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        nitems = nitems + 1
        idvec(1) = ss%id_nbeami(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NEFLUX_KY_TGLF
  IF(ALLOCATED(ss%neflux_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "NEFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%neflux_ky_tglf(:,I1))/size(ss%neflux_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NE_KY_TGLF
  IF(ALLOCATED(ss%ne_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "NE_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%ne_ky_tglf(:,I1))/size(ss%ne_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NFUSI
  IF(ALLOCATED(ss%nfusi).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "NFUSI_LIST"
     icomponent = ps_cnum_FUS
     nitems = 0
     DO I1 = 1,ss%nspec_fusion
        nitems = nitems + 1
        idvec(1) = ss%id_nfusi(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NIFLUX_KY_TGLF
  IF(ALLOCATED(ss%niflux_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "NIFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%niflux_ky_tglf(:,I1))/size(ss%niflux_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NI_KY_TGLF
  IF(ALLOCATED(ss%ni_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "NI_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%ni_ky_tglf(:,I1))/size(ss%ni_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NMINI
  IF(ALLOCATED(ss%nmini).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "NMINI_LIST"
     icomponent = ps_cnum_IC
     nitems = 0
     DO I1 = 1,ss%nspec_rfmin
        nitems = nitems + 1
        idvec(1) = ss%id_nmini(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NS
  IF(ALLOCATED(ss%ns).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "NS_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        idvec(1) = ss%id_ns(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NZFLUX_KY_TGLF
  IF(ALLOCATED(ss%nzflux_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "NZFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%nzflux_ky_tglf(:,I1))/size(ss%nzflux_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NZ_KY_TGLF
  IF(ALLOCATED(ss%nz_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "NZ_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%nz_ky_tglf(:,I1))/size(ss%nz_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_OMEG0CX
  IF(ALLOCATED(ss%omeg0cx).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "OMEG0CX_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I1 = 1,ss%ngsc0
        nitems = nitems + 1
        idvec(1) = ss%id_omeg0cx(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_OMEG0SC0
  IF(ALLOCATED(ss%omeg0sc0).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "OMEG0SC0_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I2 = 1,ss%ngsc0
        DO I1 = 1,ss%nspec_gas
           nitems = nitems + 1
           idvec(1) = ss%id_omeg0sc0(I1,I2)
           idwvec(1) = ss%id_zonwt(idvec(1))
           call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
                .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
           IF(ierr.NE.0) RETURN
           item_r8vals(nitems) = ansvec(1,1)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_OMEG0_HALO
  IF(ALLOCATED(ss%omeg0_halo).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "OMEG0_HALO_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        idvec(1) = ss%id_omeg0_halo(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_OMEG0_RECO
  IF(ALLOCATED(ss%omeg0_reco).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "OMEG0_RECO_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        idvec(1) = ss%id_omeg0_reco(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PEECH_SRC
  IF(ALLOCATED(ss%peech_src).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "PEECH_SRC_LIST"
     icomponent = ps_cnum_EC
     nitems = 0
     DO I1 = 1,ss%necrf_src
        nitems = nitems + 1
        idvec(1) = ss%id_peech_src(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PELH_SRC
  IF(ALLOCATED(ss%pelh_src).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "PELH_SRC_LIST"
     icomponent = ps_cnum_LH
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        nitems = nitems + 1
        idvec(1) = ss%id_pelh_src(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PE_TRANSCAT
  IF(ALLOCATED(ss%pe_transcat).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "PE_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 1,ss%ntrcat
        nitems = nitems + 1
        idvec(1) = ss%id_pe_transcat(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PHIFLUX_KY_TGLF
  IF(ALLOCATED(ss%phiflux_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "PHIFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%phiflux_ky_tglf(:,I1))/size(ss%phiflux_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PICRF_NPHI_SRCS
  IF(ALLOCATED(ss%picrf_nphi_srcs).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "PICRF_NPHI_SRCS_LIST"
     icomponent = ps_cnum_IC
     nitems = 0
     DO I3 = 0,ss%nspec_alla
        DO I2 = 1,ss%nicrf_src
           DO I1 = 1,ss%max_num_nphi
              idvec(1) = ss%id_picrf_nphi_srcs(I1,I2,I3)
              IF(idvec(1).EQ.0) CYCLE
              nitems = nitems + 1
              idwvec(1) = ss%id_zonwt(idvec(1))
              call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
                   .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
              IF(ierr.NE.0) RETURN
              item_r8vals(nitems) = ansvec(1,1)
           ENDDO   ! I1
        ENDDO   ! I2
     ENDDO   ! I3

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PICRF_SRCS
  IF(ALLOCATED(ss%picrf_srcs).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "PICRF_SRCS_LIST"
     icomponent = ps_cnum_IC
     nitems = 0
     DO I2 = 0,ss%nspec_alla
        DO I1 = 1,ss%nicrf_src
           nitems = nitems + 1
           idvec(1) = ss%id_picrf_srcs(I1,I2)
           idwvec(1) = ss%id_zonwt(idvec(1))
           call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
                .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
           IF(ierr.NE.0) RETURN
           item_r8vals(nitems) = ansvec(1,1)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PICRF_TOTALS
  IF(ALLOCATED(ss%picrf_totals).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "PICRF_TOTALS_LIST"
     icomponent = ps_cnum_IC
     nitems = 0
     DO I1 = 0,ss%nspec_alla
        nitems = nitems + 1
        idvec(1) = ss%id_picrf_totals(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PILH_SRC
  IF(ALLOCATED(ss%pilh_src).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "PILH_SRC_LIST"
     icomponent = ps_cnum_LH
     nitems = 0
     DO I1 = 1,ss%nlhrf_src
        nitems = nitems + 1
        idvec(1) = ss%id_pilh_src(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PI_TRANSCAT
  IF(ALLOCATED(ss%pi_transcat).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "PI_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 1,ss%ntrcat
        nitems = nitems + 1
        idvec(1) = ss%id_pi_transcat(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_POT_KY_TGLF
  IF(ALLOCATED(ss%pot_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "POT_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%pot_ky_tglf(:,I1))/size(ss%pot_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PSMOM_NC
  IF(ALLOCATED(ss%psmom_nc).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "PSMOM_NC_LIST"
     icomponent = ps_cnum_EQ
     nitems = 0
     DO I1 = 1,ss%npsmom
        nitems = nitems + 1
        idvec(1) = ss%id_psmom_nc(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_QBEAMI
  IF(ALLOCATED(ss%qbeami).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "QBEAMI_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        nitems = nitems + 1
        idvec(1) = ss%id_qbeami(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_QIONIZ
  IF(ALLOCATED(ss%qioniz).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "QIONIZ_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I1 = 1,ss%ngsc0
        nitems = nitems + 1
        idvec(1) = ss%id_qioniz(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_QQCX
  IF(ALLOCATED(ss%qqcx).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "QQCX_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I1 = 1,ss%ngsc0
        nitems = nitems + 1
        idvec(1) = ss%id_qqcx(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RATE_SINB0I
  IF(ALLOCATED(ss%rate_sinb0i).AND.hcheck(1,1)) THEN
     listname = "RATE_SINB0I_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%rate_sinb0i(:,I1))/size(ss%rate_sinb0i(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RATE_SINB0X
  IF(ALLOCATED(ss%rate_sinb0x).AND.hcheck(1,1)) THEN
     listname = "RATE_SINB0X_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%rate_sinb0x(:,I1))/size(ss%rate_sinb0x(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RATE_SINB0XS
  IF(ALLOCATED(ss%rate_sinb0xs).AND.hcheck(1,1)) THEN
     listname = "RATE_SINB0XS_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I2 = 1,ss%nspec_beam
        DO I1 = 1,ss%nspec_gas
           nitems = nitems + 1
           ansvec(1,1) = sum(ss%rate_sinb0xs(:,I1,I2))/size(ss%rate_sinb0xs(:,I1,I2))
           item_r8vals(nitems) = ansvec(1,1)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RATE_SINF0I
  IF(ALLOCATED(ss%rate_sinf0i).AND.hcheck(1,1)) THEN
     listname = "RATE_SINF0I_LIST"
     icomponent = ps_cnum_FUS
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%rate_sinf0i(:,I1))/size(ss%rate_sinf0i(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RATE_SINF0X
  IF(ALLOCATED(ss%rate_sinf0x).AND.hcheck(1,1)) THEN
     listname = "RATE_SINF0X_LIST"
     icomponent = ps_cnum_FUS
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%rate_sinf0x(:,I1))/size(ss%rate_sinf0x(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RATE_SINF0XS
  IF(ALLOCATED(ss%rate_sinf0xs).AND.hcheck(1,1)) THEN
     listname = "RATE_SINF0XS_LIST"
     icomponent = ps_cnum_FUS
     nitems = 0
     DO I2 = 1,ss%nspec_fusion
        DO I1 = 1,ss%nspec_gas
           nitems = nitems + 1
           ansvec(1,1) = sum(ss%rate_sinf0xs(:,I1,I2))/size(ss%rate_sinf0xs(:,I1,I2))
           item_r8vals(nitems) = ansvec(1,1)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RES_SN
  IF(ALLOCATED(ss%res_sn).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "RES_SN_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        idvec(1) = ss%id_res_sn(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_S0RECO
  IF(ALLOCATED(ss%s0reco).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "S0RECO_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        idvec(1) = ss%id_s0reco(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_S0RECO_RECAP
  IF(ALLOCATED(ss%s0reco_recap).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "S0RECO_RECAP_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        idvec(1) = ss%id_s0reco_recap(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SB0HALO
  IF(ALLOCATED(ss%sb0halo).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SB0HALO_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        idvec(1) = ss%id_sb0halo(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SB0HALO_RECAP
  IF(ALLOCATED(ss%sb0halo_recap).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SB0HALO_RECAP_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        idvec(1) = ss%id_sb0halo_recap(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SBSCE
  IF(ALLOCATED(ss%sbsce).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SBSCE_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I1 = 0,ss%nspec_tha
        nitems = nitems + 1
        idvec(1) = ss%id_sbsce(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SBTHERM
  IF(ALLOCATED(ss%sbtherm).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SBTHERM_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I1 = 1,ss%nspec_beam
        nitems = nitems + 1
        idvec(1) = ss%id_sbtherm(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SC_ANOM
  IF(ALLOCATED(ss%sc_anom).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SC_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        idvec(1) = ss%id_sc_anom(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SFSCE
  IF(ALLOCATED(ss%sfsce).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SFSCE_LIST"
     icomponent = ps_cnum_FUS
     nitems = 0
     DO I1 = 0,ss%nspec_tha
        nitems = nitems + 1
        idvec(1) = ss%id_sfsce(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SFTHERM
  IF(ALLOCATED(ss%sftherm).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SFTHERM_LIST"
     icomponent = ps_cnum_FUS
     nitems = 0
     DO I1 = 1,ss%nspec_fusion
        nitems = nitems + 1
        idvec(1) = ss%id_sftherm(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SII_XS
  IF(ALLOCATED(ss%sii_xs).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SII_XS_LIST"
     icomponent = ps_cnum_RAD
     nitems = 0
     DO I2 = 1,ss%nxs
        DO I1 = 0,ss%nspec_th
           nitems = nitems + 1
           idvec(1) = ss%id_sii_xs(I1,I2)
           idwvec(1) = ss%id_zonwt(idvec(1))
           call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
                .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
           IF(ierr.NE.0) RETURN
           item_r8vals(nitems) = ansvec(1,1)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SN_TRANS
  IF(ALLOCATED(ss%sn_trans).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SN_TRANS_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        idvec(1) = ss%id_sn_trans(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SN_TRANSCAT
  IF(ALLOCATED(ss%sn_transcat).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SN_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I2 = 1,ss%ntrcat
        DO I1 = 0,ss%nspec_th
           nitems = nitems + 1
           idvec(1) = ss%id_sn_transcat(I1,I2)
           idwvec(1) = ss%id_zonwt(idvec(1))
           call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
                .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
           IF(ierr.NE.0) RETURN
           item_r8vals(nitems) = ansvec(1,1)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SPROF0
  IF(ALLOCATED(ss%sprof0).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SPROF0_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I2 = 1,ss%ngsc0
        DO I1 = 1,ss%nspec_gas
           nitems = nitems + 1
           idvec(1) = ss%id_sprof0(I1,I2)
           idwvec(1) = ss%id_zonwt(idvec(1))
           call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
                .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
           IF(ierr.NE.0) RETURN
           item_r8vals(nitems) = ansvec(1,1)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SPROF0E
  IF(ALLOCATED(ss%sprof0e).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SPROF0E_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I1 = 1,ss%ngsc0
        nitems = nitems + 1
        idvec(1) = ss%id_sprof0e(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SPSCE
  IF(ALLOCATED(ss%spsce).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "SPSCE_LIST"
     icomponent = ps_cnum_PEL
     nitems = 0
     DO I1 = 0,ss%nspec_tha
        nitems = nitems + 1
        idvec(1) = ss%id_spsce(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_T0CX
  IF(ALLOCATED(ss%T0cx).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "T0CX_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I1 = 1,ss%ngsc0
        nitems = nitems + 1
        idvec(1) = ss%id_T0cx(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_T0SC0
  IF(ALLOCATED(ss%T0sc0).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "T0SC0_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I2 = 1,ss%ngsc0
        DO I1 = 1,ss%nspec_gas
           nitems = nitems + 1
           idvec(1) = ss%id_T0sc0(I1,I2)
           idwvec(1) = ss%id_zonwt(idvec(1))
           call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
                .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
           IF(ierr.NE.0) RETURN
           item_r8vals(nitems) = ansvec(1,1)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_T0_HALO
  IF(ALLOCATED(ss%T0_halo).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "T0_HALO_LIST"
     icomponent = ps_cnum_NBI
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        idvec(1) = ss%id_T0_halo(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_T0_RECO
  IF(ALLOCATED(ss%T0_reco).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "T0_RECO_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I1 = 1,ss%nspec_gas
        nitems = nitems + 1
        idvec(1) = ss%id_T0_reco(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TEFLUX_KY_TGLF
  IF(ALLOCATED(ss%teflux_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "TEFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%teflux_ky_tglf(:,I1))/size(ss%teflux_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TE_KY_TGLF
  IF(ALLOCATED(ss%te_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "TE_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%te_ky_tglf(:,I1))/size(ss%te_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TIFLUX_KY_TGLF
  IF(ALLOCATED(ss%tiflux_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "TIFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%tiflux_ky_tglf(:,I1))/size(ss%tiflux_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TI_KY_TGLF
  IF(ALLOCATED(ss%ti_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "TI_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%ti_ky_tglf(:,I1))/size(ss%ti_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQIONIZ
  IF(ALLOCATED(ss%tqioniz).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "TQIONIZ_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I1 = 1,ss%ngsc0
        nitems = nitems + 1
        idvec(1) = ss%id_tqioniz(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQQCX
  IF(ALLOCATED(ss%tqqcx).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "TQQCX_LIST"
     icomponent = ps_cnum_GAS
     nitems = 0
     DO I1 = 1,ss%ngsc0
        nitems = nitems + 1
        idvec(1) = ss%id_tqqcx(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQ_TRANSCAT
  IF(ALLOCATED(ss%tq_transcat).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "TQ_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 1,ss%ntrcat
        nitems = nitems + 1
        idvec(1) = ss%id_tq_transcat(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.TRUE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TS
  IF(ALLOCATED(ss%Ts).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "TS_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        idvec(1) = ss%id_Ts(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TZFLUX_KY_TGLF
  IF(ALLOCATED(ss%tzflux_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "TZFLUX_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%tzflux_ky_tglf(:,I1))/size(ss%tzflux_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TZ_KY_TGLF
  IF(ALLOCATED(ss%tz_ky_tglf).AND.hcheck(1,1)) THEN
     listname = "TZ_KY_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 1,ss%nky_anomth
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%tz_ky_tglf(:,I1))/size(ss%tz_ky_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_UPWIND_PFRAC_NS
  IF(ALLOCATED(ss%upwind_pfrac_ns).AND.hcheck(1,1)) THEN
     listname = "UPWIND_PFRAC_NS_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%upwind_pfrac_ns(:,I1))/size(ss%upwind_pfrac_ns(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VEE_TRANSCAT
  IF(ALLOCATED(ss%vee_transcat).AND.hcheck(1,1)) THEN
     listname = "VEE_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 1,ss%ntrcat
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%vee_transcat(:,I1))/size(ss%vee_transcat(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_ANOM
  IF(ALLOCATED(ss%velns_anom).AND.hcheck(1,1)) THEN
     listname = "VELNS_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%velns_anom(:,I1))/size(ss%velns_anom(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_GLF
  IF(ALLOCATED(ss%velns_glf).AND.hcheck(1,1)) THEN
     listname = "VELNS_GLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%velns_glf(:,I1))/size(ss%velns_glf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_GTCNEO
  IF(ALLOCATED(ss%velns_gtcneo).AND.hcheck(1,1)) THEN
     listname = "VELNS_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%velns_gtcneo(:,I1))/size(ss%velns_gtcneo(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_MMM71
  IF(ALLOCATED(ss%velns_mmm71).AND.hcheck(1,1)) THEN
     listname = "VELNS_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%velns_mmm71(:,I1))/size(ss%velns_mmm71(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_NCLASS
  IF(ALLOCATED(ss%velns_nclass).AND.hcheck(1,1)) THEN
     listname = "VELNS_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%velns_nclass(:,I1))/size(ss%velns_nclass(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_NEO
  IF(ALLOCATED(ss%velns_neo).AND.hcheck(1,1)) THEN
     listname = "VELNS_NEO_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%velns_neo(:,I1))/size(ss%velns_neo(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNS_TGLF
  IF(ALLOCATED(ss%velns_tglf).AND.hcheck(1,1)) THEN
     listname = "VELNS_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%velns_tglf(:,I1))/size(ss%velns_tglf(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VIE_TRANSCAT
  IF(ALLOCATED(ss%vie_transcat).AND.hcheck(1,1)) THEN
     listname = "VIE_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 1,ss%ntrcat
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%vie_transcat(:,I1))/size(ss%vie_transcat(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VMO_TRANSCAT
  IF(ALLOCATED(ss%vmo_transcat).AND.hcheck(1,1)) THEN
     listname = "VMO_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 1,ss%ntrcat
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%vmo_transcat(:,I1))/size(ss%vmo_transcat(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VN_TRANS
  IF(ALLOCATED(ss%vn_trans).AND.hcheck(1,1)) THEN
     listname = "VN_TRANS_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        ansvec(1,1) = sum(ss%vn_trans(:,I1))/size(ss%vn_trans(:,I1))
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VN_TRANSCAT
  IF(ALLOCATED(ss%vn_transcat).AND.hcheck(1,1)) THEN
     listname = "VN_TRANSCAT_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I2 = 1,ss%ntrcat
        DO I1 = 0,ss%nspec_th
           nitems = nitems + 1
           ansvec(1,1) = sum(ss%vn_transcat(:,I1,I2))/size(ss%vn_transcat(:,I1,I2))
           item_r8vals(nitems) = ansvec(1,1)
        ENDDO   ! I1
     ENDDO   ! I2

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VPOL_INMP
  IF(ALLOCATED(ss%vpol_inmp).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "VPOL_INMP_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        idvec(1) = ss%id_vpol_inmp(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VPOL_OMP
  IF(ALLOCATED(ss%vpol_omp).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "VPOL_OMP_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        idvec(1) = ss%id_vpol_omp(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VTOR_INMP
  IF(ALLOCATED(ss%vtor_inmp).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "VTOR_INMP_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        idvec(1) = ss%id_vtor_inmp(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VTOR_OMP
  IF(ALLOCATED(ss%vtor_omp).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "VTOR_OMP_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        idvec(1) = ss%id_vtor_omp(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_V_PARS
  IF(ALLOCATED(ss%v_pars).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "V_PARS_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        idvec(1) = ss%id_v_pars(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_V_PERS
  IF(ALLOCATED(ss%v_pers).AND.(id_intg.GT.0).AND.hcheck(1,1)) THEN
     listname = "V_PERS_LIST"
     icomponent = ps_cnum_PLASMA
     nitems = 0
     DO I1 = 0,ss%nspec_th
        nitems = nitems + 1
        idvec(1) = ss%id_v_pers(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_XRJCOS_MOMEQ
  ahash(2) = ps_phash_DXRJCOS_MOMEQ_DRHO
  IF(ALLOCATED(ss%xRjcos_momeq).AND.(id_intg.GT.0).AND.hcheck(2,1)) THEN
     listname = "XRJCOS_MOMEQ_LIST"
     icomponent = ps_cnum_EQ
     nitems = 0
     DO I1 = 1,ss%neqmom
        nitems = nitems + 1
        idvec(1) = ss%id_xRjcos_momeq(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_XRJSIN_MOMEQ
  ahash(2) = ps_phash_DXRJSIN_MOMEQ_DRHO
  IF(ALLOCATED(ss%xRjsin_momeq).AND.(id_intg.GT.0).AND.hcheck(2,1)) THEN
     listname = "XRJSIN_MOMEQ_LIST"
     icomponent = ps_cnum_EQ
     nitems = 0
     DO I1 = 1,ss%neqmom
        nitems = nitems + 1
        idvec(1) = ss%id_xRjsin_momeq(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_XZJCOS_MOMEQ
  ahash(2) = ps_phash_DXZJCOS_MOMEQ_DRHO
  IF(ALLOCATED(ss%xZjcos_momeq).AND.(id_intg.GT.0).AND.hcheck(2,1)) THEN
     listname = "XZJCOS_MOMEQ_LIST"
     icomponent = ps_cnum_EQ
     nitems = 0
     DO I1 = 1,ss%neqmom
        nitems = nitems + 1
        idvec(1) = ss%id_xZjcos_momeq(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_XZJSIN_MOMEQ
  ahash(2) = ps_phash_DXZJSIN_MOMEQ_DRHO
  IF(ALLOCATED(ss%xZjsin_momeq).AND.(id_intg.GT.0).AND.hcheck(2,1)) THEN
     listname = "XZJSIN_MOMEQ_LIST"
     icomponent = ps_cnum_EQ
     nitems = 0
     DO I1 = 1,ss%neqmom
        nitems = nitems + 1
        idvec(1) = ss%id_xZjsin_momeq(I1)
        idwvec(1) = ss%id_zonwt(idvec(1))
        call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &
             .FALSE.,.FALSE.,.FALSE.,ansvec,dvvec,iout,ierr)
        IF(ierr.NE.0) RETURN
        item_r8vals(nitems) = ansvec(1,1)
     ENDDO   ! I1

     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &
          nitems,item_r8vals,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  IF(ALLOCATED(ss%nphi)) THEN
     DEALLOCATE(enum_lbl_nphi)
  ENDIF

  ! OK ALL DONE: update hash codes

  ss%hash_codes = ss%new_hash_codes
  xhash = ss%new_hash_codes

  call ps_xhashes_put(ss%iobj,xhash,iout,ierr)

  deallocate(enum_lbl_1d)

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

END SUBROUTINE plasma_state_xload
