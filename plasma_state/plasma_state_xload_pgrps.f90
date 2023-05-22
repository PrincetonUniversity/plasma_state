
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_pgrps(ss,xhash,iout,ierr)

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

  INTEGER :: icomponent,imod,nitems,idchk
  CHARACTER*32 :: listname,units,list_units
  CHARACTER*80 :: label,plabel
  INTEGER :: ahash(ps_max_static)

  CHARACTER*32 :: item_names(max_itemlist)
  INTEGER :: item_ivals(max_itemlist)
  REAL(KIND=rspec) :: item_r8vals(max_itemlist)
  CHARACTER*256 :: item_c256(max_itemlist)

  !-----------------------

  ahash(1) = ps_phash_DIFB_NBI
  idchk = ss%id_DIFB_NBI
  ahash(2) = ps_phash_DIFB_FUSI
  idchk = min(idchk,ss%id_DIFB_FUSI)
  ahash(3) = ps_phash_DIFB_RFMI
  idchk = min(idchk,ss%id_DIFB_RFMI)
  IF(ALLOCATED(ss%difb_nbi).AND.hcheck(3,idchk)) THEN
     listname = "ANOM_DIFFUS"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: difb_nbi"

     plabel = "beam ion anomalous diffusivity"
     CALL ps_1d_prof_store(ss%iobj,"difb_nbi","RHO_ANOM", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anom,0, &
          ss%difb_nbi,ss%id_difb_nbi, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "difb_nbi"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_difb_nbi
     item_c256(1) = plabel

     plabel = "fusion ion anomalous diffusivity"
     CALL ps_1d_prof_store(ss%iobj,"difb_fusi","RHO_ANOM", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anom,0, &
          ss%difb_fusi,ss%id_difb_fusi, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "difb_fusi"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_difb_fusi
     item_c256(2) = plabel

     plabel = "RF minority ion anomalous diffusivity"
     CALL ps_1d_prof_store(ss%iobj,"difb_rfmi","RHO_ANOM", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anom,0, &
          ss%difb_rfmi,ss%id_difb_rfmi, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "difb_rfmi"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_difb_rfmi
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELB_NBI
  idchk = ss%id_VELB_NBI
  ahash(2) = ps_phash_VELB_FUSI
  idchk = min(idchk,ss%id_VELB_FUSI)
  ahash(3) = ps_phash_VELB_RFMI
  idchk = min(idchk,ss%id_VELB_RFMI)
  IF(ALLOCATED(ss%velb_nbi).AND.hcheck(3,idchk)) THEN
     listname = "ANOM_VELOC"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velb_nbi"

     plabel = "beam ion anom. radial velocity"
     CALL ps_1d_prof_store(ss%iobj,"velb_nbi","RHO_ANOM", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anom,0, &
          ss%velb_nbi,ss%id_velb_nbi, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velb_nbi"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velb_nbi
     item_c256(1) = plabel

     plabel = "fusion ion anom. radial velocity"
     CALL ps_1d_prof_store(ss%iobj,"velb_fusi","RHO_ANOM", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anom,0, &
          ss%velb_fusi,ss%id_velb_fusi, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "velb_fusi"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_velb_fusi
     item_c256(2) = plabel

     plabel = "RF minority ion anom. radial velocity"
     CALL ps_1d_prof_store(ss%iobj,"velb_rfmi","RHO_ANOM", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anom,0, &
          ss%velb_rfmi,ss%id_velb_rfmi, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "velb_rfmi"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_velb_rfmi
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PB0_HALO
  idchk = ss%id_PB0_HALO
  ahash(2) = ps_phash_PSC_HALO
  idchk = min(idchk,ss%id_PSC_HALO)
  ahash(3) = ps_phash_PCX_HALO
  idchk = min(idchk,ss%id_PCX_HALO)
  IF(ALLOCATED(ss%pb0_halo).AND.hcheck(3,idchk)) THEN
     listname = "BEAM_HALO_POWER"
     icomponent = ps_cnum_NBI
     units = "W"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: pb0_halo"

     plabel = "power in halo neutral source"
     CALL ps_1d_prof_store(ss%iobj,"pb0_halo","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%pb0_halo,ss%id_pb0_halo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "pb0_halo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_pb0_halo
     item_c256(1) = plabel

     plabel = "beam halo power: recapture - pb0_halo"
     CALL ps_1d_prof_store(ss%iobj,"psc_halo","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%psc_halo,ss%id_psc_halo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "psc_halo"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_psc_halo
     item_c256(2) = plabel

     plabel = "beam halo driven CX power"
     CALL ps_1d_prof_store(ss%iobj,"pcx_halo","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%pcx_halo,ss%id_pcx_halo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "pcx_halo"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_pcx_halo
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQB0_HALO
  idchk = ss%id_TQB0_HALO
  ahash(2) = ps_phash_TQSC_HALO
  idchk = min(idchk,ss%id_TQSC_HALO)
  ahash(3) = ps_phash_TQCX_HALO
  idchk = min(idchk,ss%id_TQCX_HALO)
  IF(ALLOCATED(ss%tqb0_halo).AND.hcheck(3,idchk)) THEN
     listname = "BEAM_HALO_TORQUE"
     icomponent = ps_cnum_NBI
     units = "Nt*m"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "Nt*m"
     label = "vol int: tqb0_halo"

     plabel = "torque of halo neutral source"
     CALL ps_1d_prof_store(ss%iobj,"tqb0_halo","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%tqb0_halo,ss%id_tqb0_halo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "tqb0_halo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_tqb0_halo
     item_c256(1) = plabel

     plabel = "beam halo torque: recapture - tqb0_halo"
     CALL ps_1d_prof_store(ss%iobj,"tqsc_halo","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%tqsc_halo,ss%id_tqsc_halo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "tqsc_halo"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_tqsc_halo
     item_c256(2) = plabel

     plabel = "beam halo driven CX momentum torque"
     CALL ps_1d_prof_store(ss%iobj,"tqcx_halo","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%tqcx_halo,ss%id_tqcx_halo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "tqcx_halo"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_tqcx_halo
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PBE
  idchk = ss%id_PBE
  ahash(2) = ps_phash_PBI
  idchk = min(idchk,ss%id_PBI)
  ahash(3) = ps_phash_PBTH
  idchk = min(idchk,ss%id_PBTH)
  IF(ALLOCATED(ss%pbe).AND.hcheck(3,idchk)) THEN
     listname = "BEAM_HEATING"
     icomponent = ps_cnum_NBI
     units = "W"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: pbe"

     plabel = "electron heating by all beam ions"
     CALL ps_1d_prof_store(ss%iobj,"pbe","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%pbe,ss%id_pbe, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "pbe"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_pbe
     item_c256(1) = plabel

     plabel = "thermal ion heating by all beam ions"
     CALL ps_1d_prof_store(ss%iobj,"pbi","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%pbi,ss%id_pbi, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "pbi"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_pbi
     item_c256(2) = plabel

     plabel = "thermalization of beam ions"
     CALL ps_1d_prof_store(ss%iobj,"pbth","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%pbth,ss%id_pbth, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "pbth"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_pbth
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQBE
  idchk = ss%id_TQBE
  ahash(2) = ps_phash_TQBI
  idchk = min(idchk,ss%id_TQBI)
  ahash(3) = ps_phash_TQBJXB
  idchk = min(idchk,ss%id_TQBJXB)
  ahash(4) = ps_phash_TQBTH
  idchk = min(idchk,ss%id_TQBTH)
  IF(ALLOCATED(ss%tqbe).AND.hcheck(4,idchk)) THEN
     listname = "BEAM_TORQUE"
     icomponent = ps_cnum_NBI
     units = "Nt*m"
     nitems = 4
     ! for assoc. scalar list:
     list_units = "Nt*m"
     label = "vol int: tqbe"

     plabel = "collisional beam torque to electrons"
     CALL ps_1d_prof_store(ss%iobj,"tqbe","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%tqbe,ss%id_tqbe, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "tqbe"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_tqbe
     item_c256(1) = plabel

     plabel = "collisional beam torque to thermal ions"
     CALL ps_1d_prof_store(ss%iobj,"tqbi","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%tqbi,ss%id_tqbi, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "tqbi"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_tqbi
     item_c256(2) = plabel

     plabel = "JxB beam torque"
     CALL ps_1d_prof_store(ss%iobj,"tqbjxb","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%tqbjxb,ss%id_tqbjxb, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "tqbjxb"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_tqbjxb
     item_c256(3) = plabel

     plabel = "momentum in thermalization of beam ions"
     CALL ps_1d_prof_store(ss%iobj,"tqbth","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%tqbth,ss%id_tqbth, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(4) = "tqbth"
     item_r8vals(4) = 0.0_rspec
     item_ivals(4) = ss%id_tqbth
     item_c256(4) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_B_SURFMAX
  idchk = ss%id_B_SURFMAX
  ahash(2) = ps_phash_B_SURFMIN
  idchk = min(idchk,ss%id_B_SURFMIN)
  IF(ALLOCATED(ss%B_surfMax).AND.hcheck(2,idchk)) THEN
     listname = "BMINMAX"
     icomponent = ps_cnum_EQ
     units = "T"
     nitems = 2
     ! for assoc. scalar list:
     list_units = "T"
     label = "edge value: B_surfMax"

     plabel = "max mod(B) on flux surface"
     CALL ps_1d_prof_store(ss%iobj,"B_surfMax","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%B_surfMax,ss%id_B_surfMax, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "B_surfMax"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_B_surfMax
     item_c256(1) = plabel

     plabel = "min mod(B) on flux surface"
     CALL ps_1d_prof_store(ss%iobj,"B_surfMin","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%B_surfMin,ss%id_B_surfMin, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "B_surfMin"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_B_surfMin
     item_c256(2) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_QCOMP_E
  idchk = ss%id_QCOMP_E
  ahash(2) = ps_phash_QCOMP_I
  idchk = min(idchk,ss%id_QCOMP_I)
  IF(ALLOCATED(ss%qcomp_e).AND.hcheck(2,idchk)) THEN
     listname = "COMPRESSION"
     icomponent = ps_cnum_PLASMA
     units = "W"
     nitems = 2
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: qcomp_e"

     plabel = "Compressional electron heating"
     CALL ps_1d_prof_store(ss%iobj,"qcomp_e","RHO", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%qcomp_e,ss%id_qcomp_e, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "qcomp_e"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_qcomp_e
     item_c256(1) = plabel

     plabel = "Compressional ion heating"
     CALL ps_1d_prof_store(ss%iobj,"qcomp_i","RHO", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%qcomp_i,ss%id_qcomp_i, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "qcomp_i"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_qcomp_i
     item_c256(2) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_TRANS
  idchk = ss%id_CHIE_TRANS
  ahash(2) = ps_phash_CHII_TRANS
  idchk = min(idchk,ss%id_CHII_TRANS)
  ahash(3) = ps_phash_CHIMO_TRANS
  idchk = min(idchk,ss%id_CHIMO_TRANS)
  IF(ALLOCATED(ss%chie_trans).AND.hcheck(3,idchk)) THEN
     listname = "DIFFUSIVITY"
     icomponent = ps_cnum_PLASMA
     units = "m^2/sec"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chie_trans"

     plabel = "electron thermal diffusivity"
     CALL ps_1d_prof_store(ss%iobj,"chie_trans","RHO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho,0, &
          ss%chie_trans,ss%id_chie_trans, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_trans"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_trans
     item_c256(1) = plabel

     plabel = "ion thermal diffusivity"
     CALL ps_1d_prof_store(ss%iobj,"chii_trans","RHO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho,0, &
          ss%chii_trans,ss%id_chii_trans, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "chii_trans"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_chii_trans
     item_c256(2) = plabel

     plabel = "angular momentum diffusivity"
     CALL ps_1d_prof_store(ss%iobj,"chimo_trans","RHO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho,0, &
          ss%chimo_trans,ss%id_chimo_trans, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "chimo_trans"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_chimo_trans
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PFUSE
  idchk = ss%id_PFUSE
  ahash(2) = ps_phash_PFUSI
  idchk = min(idchk,ss%id_PFUSI)
  ahash(3) = ps_phash_PFUSTH
  idchk = min(idchk,ss%id_PFUSTH)
  IF(ALLOCATED(ss%pfuse).AND.hcheck(3,idchk)) THEN
     listname = "FUSION_HEATING"
     icomponent = ps_cnum_FUS
     units = "W"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: pfuse"

     plabel = "electron heating by all fusion ions"
     CALL ps_1d_prof_store(ss%iobj,"pfuse","RHO_FUS", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_fus-1,0, &
          ss%pfuse,ss%id_pfuse, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "pfuse"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_pfuse
     item_c256(1) = plabel

     plabel = "thermal ion heating by all fusion ions"
     CALL ps_1d_prof_store(ss%iobj,"pfusi","RHO_FUS", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_fus-1,0, &
          ss%pfusi,ss%id_pfusi, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "pfusi"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_pfusi
     item_c256(2) = plabel

     plabel = "thermalization of fusion ions"
     CALL ps_1d_prof_store(ss%iobj,"pfusth","RHO_FUS", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_fus-1,0, &
          ss%pfusth,ss%id_pfusth, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "pfusth"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_pfusth
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PELH
  idchk = ss%id_PELH
  ahash(2) = ps_phash_PILH
  idchk = min(idchk,ss%id_PILH)
  IF(ALLOCATED(ss%pelh).AND.hcheck(2,idchk)) THEN
     listname = "LOWER_HYBRID_HEATING"
     icomponent = ps_cnum_LH
     units = "W"
     nitems = 2
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: pelh"

     plabel = "electron heating by LH"
     CALL ps_1d_prof_store(ss%iobj,"pelh","RHO_LHRF", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_lhrf-1,0, &
          ss%pelh,ss%id_pelh, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "pelh"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_pelh
     item_c256(1) = plabel

     plabel = "ion heating by LH"
     CALL ps_1d_prof_store(ss%iobj,"pilh","RHO_LHRF", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_lhrf-1,0, &
          ss%pilh,ss%id_pilh, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "pilh"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_pilh
     item_c256(2) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_Z_MIDP
  idchk = ss%id_Z_MIDP
  ahash(2) = ps_phash_R_MIDP_IN
  idchk = min(idchk,ss%id_R_MIDP_IN)
  ahash(3) = ps_phash_R_MIDP_OUT
  idchk = min(idchk,ss%id_R_MIDP_OUT)
  IF(ALLOCATED(ss%Z_midp).AND.hcheck(3,idchk)) THEN
     listname = "MIDPLANE"
     icomponent = ps_cnum_EQ
     units = "m"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "m"
     label = "edge value: Z_midp"

     plabel = "flux surface midplane elevation"
     CALL ps_1d_prof_store(ss%iobj,"Z_midp","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%Z_midp,ss%id_Z_midp, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "Z_midp"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_Z_midp
     item_c256(1) = plabel

     plabel = "R (high field side) midplane intercept"
     CALL ps_1d_prof_store(ss%iobj,"R_midp_in","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%R_midp_in,ss%id_R_midp_in, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "R_midp_in"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_R_midp_in
     item_c256(2) = plabel

     plabel = "R (low field side) midplane intercept"
     CALL ps_1d_prof_store(ss%iobj,"R_midp_out","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%R_midp_out,ss%id_R_midp_out, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "R_midp_out"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_R_midp_out
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PMINE
  idchk = ss%id_PMINE
  ahash(2) = ps_phash_PMINI
  idchk = min(idchk,ss%id_PMINI)
  ahash(3) = ps_phash_PMINTH
  idchk = min(idchk,ss%id_PMINTH)
  IF(ALLOCATED(ss%pmine).AND.hcheck(3,idchk)) THEN
     listname = "MINORITY_HEATING"
     icomponent = ps_cnum_IC
     units = "W"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: pmine"

     plabel = "electron heating by minority ions"
     CALL ps_1d_prof_store(ss%iobj,"pmine","RHO_ICRF", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_icrf-1,0, &
          ss%pmine,ss%id_pmine, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "pmine"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_pmine
     item_c256(1) = plabel

     plabel = "thermal ion heating by minority ions"
     CALL ps_1d_prof_store(ss%iobj,"pmini","RHO_ICRF", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_icrf-1,0, &
          ss%pmini,ss%id_pmini, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "pmini"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_pmini
     item_c256(2) = plabel

     plabel = "(de)thermalization of minority ions"
     CALL ps_1d_prof_store(ss%iobj,"pminth","RHO_ICRF", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_icrf-1,0, &
          ss%pminth,ss%id_pminth, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "pminth"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_pminth
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VEE_TRANS
  idchk = ss%id_VEE_TRANS
  ahash(2) = ps_phash_VIE_TRANS
  idchk = min(idchk,ss%id_VIE_TRANS)
  ahash(3) = ps_phash_VMO_TRANS
  idchk = min(idchk,ss%id_VMO_TRANS)
  IF(ALLOCATED(ss%vee_trans).AND.hcheck(3,idchk)) THEN
     listname = "RADIAL_VELOCITY"
     icomponent = ps_cnum_PLASMA
     units = "m/sec"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: vee_trans"

     plabel = "electron energy radial velocity"
     CALL ps_1d_prof_store(ss%iobj,"vee_trans","RHO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho,0, &
          ss%vee_trans,ss%id_vee_trans, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "vee_trans"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_vee_trans
     item_c256(1) = plabel

     plabel = "ion energy radial velocity"
     CALL ps_1d_prof_store(ss%iobj,"vie_trans","RHO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho,0, &
          ss%vie_trans,ss%id_vie_trans, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "vie_trans"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_vie_trans
     item_c256(2) = plabel

     plabel = "angular momentum radial velocity"
     CALL ps_1d_prof_store(ss%iobj,"vmo_trans","RHO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho,0, &
          ss%vmo_trans,ss%id_vmo_trans, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "vmo_trans"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_vmo_trans
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PRAD
  idchk = ss%id_PRAD
  ahash(2) = ps_phash_PRAD_CY
  idchk = min(idchk,ss%id_PRAD_CY)
  ahash(3) = ps_phash_PRAD_BR
  idchk = min(idchk,ss%id_PRAD_BR)
  ahash(4) = ps_phash_PRAD_LI
  idchk = min(idchk,ss%id_PRAD_LI)
  IF(ALLOCATED(ss%prad).AND.hcheck(4,idchk)) THEN
     listname = "RADIATED_POWERS"
     icomponent = ps_cnum_RAD
     units = "W"
     nitems = 4
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: prad"

     plabel = "total radiated power"
     CALL ps_1d_prof_store(ss%iobj,"prad","RHO_RAD", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_rad-1,0, &
          ss%prad,ss%id_prad, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "prad"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_prad
     item_c256(1) = plabel

     plabel = "cyclotron radiation contribution"
     CALL ps_1d_prof_store(ss%iobj,"prad_cy","RHO_RAD", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_rad-1,0, &
          ss%prad_cy,ss%id_prad_cy, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "prad_cy"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_prad_cy
     item_c256(2) = plabel

     plabel = "bremsstrahlung contribution"
     CALL ps_1d_prof_store(ss%iobj,"prad_br","RHO_RAD", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_rad-1,0, &
          ss%prad_br,ss%id_prad_br, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "prad_br"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_prad_br
     item_c256(3) = plabel

     plabel = "line radiation contribution"
     CALL ps_1d_prof_store(ss%iobj,"prad_li","RHO_RAD", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_rad-1,0, &
          ss%prad_li,ss%id_prad_li, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(4) = "prad_li"
     item_r8vals(4) = 0.0_rspec
     item_ivals(4) = ss%id_prad_li
     item_c256(4) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_P0_RECO
  idchk = ss%id_P0_RECO
  ahash(2) = ps_phash_PSC_RECO
  idchk = min(idchk,ss%id_PSC_RECO)
  ahash(3) = ps_phash_PCX_RECO
  idchk = min(idchk,ss%id_PCX_RECO)
  IF(ALLOCATED(ss%p0_reco).AND.hcheck(3,idchk)) THEN
     listname = "RECO_POWER"
     icomponent = ps_cnum_GAS
     units = "W"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: p0_reco"

     plabel = "power in recombination neutral source"
     CALL ps_1d_prof_store(ss%iobj,"p0_reco","RHO_GAS", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_gas-1,0, &
          ss%p0_reco,ss%id_p0_reco, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "p0_reco"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_p0_reco
     item_c256(1) = plabel

     plabel = "reco power: recapture - p0_reco"
     CALL ps_1d_prof_store(ss%iobj,"psc_reco","RHO_GAS", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_gas-1,0, &
          ss%psc_reco,ss%id_psc_reco, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "psc_reco"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_psc_reco
     item_c256(2) = plabel

     plabel = "reco driven CX power"
     CALL ps_1d_prof_store(ss%iobj,"pcx_reco","RHO_GAS", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_gas-1,0, &
          ss%pcx_reco,ss%id_pcx_reco, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "pcx_reco"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_pcx_reco
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQ0_RECO
  idchk = ss%id_TQ0_RECO
  ahash(2) = ps_phash_TQSC_RECO
  idchk = min(idchk,ss%id_TQSC_RECO)
  ahash(3) = ps_phash_TQCX_RECO
  idchk = min(idchk,ss%id_TQCX_RECO)
  IF(ALLOCATED(ss%tq0_reco).AND.hcheck(3,idchk)) THEN
     listname = "RECO_TORQUE"
     icomponent = ps_cnum_GAS
     units = "Nt*m"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "Nt*m"
     label = "vol int: tq0_reco"

     plabel = "torque of recombination neutral source"
     CALL ps_1d_prof_store(ss%iobj,"tq0_reco","RHO_GAS", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_gas-1,0, &
          ss%tq0_reco,ss%id_tq0_reco, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "tq0_reco"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_tq0_reco
     item_c256(1) = plabel

     plabel = "reco torque: recapture - tq0_reco"
     CALL ps_1d_prof_store(ss%iobj,"tqsc_reco","RHO_GAS", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_gas-1,0, &
          ss%tqsc_reco,ss%id_tqsc_reco, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "tqsc_reco"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_tqsc_reco
     item_c256(2) = plabel

     plabel = "reco driven CX momentum torque"
     CALL ps_1d_prof_store(ss%iobj,"tqcx_reco","RHO_GAS", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_gas-1,0, &
          ss%tqcx_reco,ss%id_tqcx_reco, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "tqcx_reco"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_tqcx_reco
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RES_TE
  idchk = ss%id_RES_TE
  ahash(2) = ps_phash_RES_TI
  idchk = min(idchk,ss%id_RES_TI)
  IF(ALLOCATED(ss%res_te).AND.hcheck(2,idchk)) THEN
     listname = "RESIDUAL"
     icomponent = ps_cnum_PLASMA
     units = "Norm"
     nitems = 2
     ! for assoc. scalar list:
     list_units = "Norm"
     label = "vol int: res_te"

     plabel = "residuals for electron transport eqn"
     CALL ps_1d_prof_store(ss%iobj,"res_te","RHO", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%res_te,ss%id_res_te, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "res_te"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_res_te
     item_c256(1) = plabel

     plabel = "residuals for ion transport eqn"
     CALL ps_1d_prof_store(ss%iobj,"res_ti","RHO", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%res_ti,ss%id_res_ti, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "res_ti"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_res_ti
     item_c256(2) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TFRIP_LOG
  idchk = ss%id_TFRIP_LOG
  ahash(2) = ps_phash_TFRIP2_LOG
  idchk = min(idchk,ss%id_TFRIP2_LOG)
  IF(ALLOCATED(ss%tfrip_log).AND.hcheck(2,idchk)) THEN
     listname = "RIPPLE_FIELDS"
     icomponent = ps_cnum_RIPPLE
     units = "-"
     nitems = 2
     ! for assoc. scalar list:
     list_units = "-"
     label = "simple avg: tfrip_log"

     plabel = "TF ripple: log((Bmax-Bmin)/(Bmax+Bmin))"
     CALL ps_2d_prof_store(ss%iobj,"tfrip_log","R_RIPPLE","Z_RIPPLE", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nr_rip,ss%nz_rip,0,0, &
          ss%tfrip_log,ss%id_tfrip_log, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "tfrip_log"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_tfrip_log
     item_c256(1) = plabel

     plabel = "2nd component of TF ripple"
     CALL ps_2d_prof_store(ss%iobj,"tfrip2_log","R_RIPPLE","Z_RIPPLE", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nr_rip,ss%nz_rip,0,0, &
          ss%tfrip2_log,ss%id_tfrip2_log, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "tfrip2_log"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_tfrip2_log
     item_c256(2) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RMAJOR_MEAN
  idchk = ss%id_RMAJOR_MEAN
  ahash(2) = ps_phash_RMINOR_MEAN
  idchk = min(idchk,ss%id_RMINOR_MEAN)
  IF(ALLOCATED(ss%Rmajor_mean).AND.hcheck(2,idchk)) THEN
     listname = "RMAJMIN"
     icomponent = ps_cnum_EQ
     units = "m"
     nitems = 2
     ! for assoc. scalar list:
     list_units = "m"
     label = "edge value: Rmajor_mean"

     plabel = "Rmajor_mean = (R_Surfmax+R_surfmin)/2"
     CALL ps_1d_prof_store(ss%iobj,"Rmajor_mean","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%Rmajor_mean,ss%id_Rmajor_mean, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "Rmajor_mean"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_Rmajor_mean
     item_c256(1) = plabel

     plabel = "rMinor_mean = (R_Surfmax-R_surfmin)/2"
     CALL ps_1d_prof_store(ss%iobj,"rMinor_mean","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%rMinor_mean,ss%id_rMinor_mean, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "rMinor_mean"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_rMinor_mean
     item_c256(2) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_QROT_CONV
  idchk = ss%id_QROT_CONV
  ahash(2) = ps_phash_QROT_DIFF
  idchk = min(idchk,ss%id_QROT_DIFF)
  IF(ALLOCATED(ss%qrot_conv).AND.hcheck(2,idchk)) THEN
     listname = "ROTATION_DISSIPATION"
     icomponent = ps_cnum_PLASMA
     units = "W"
     nitems = 2
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: qrot_conv"

     plabel = "rotational energy dissipation (convective)"
     CALL ps_1d_prof_store(ss%iobj,"qrot_conv","RHO", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%qrot_conv,ss%id_qrot_conv, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "qrot_conv"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_qrot_conv
     item_c256(1) = plabel

     plabel = "rotational energy dissipation (diffusive)"
     CALL ps_1d_prof_store(ss%iobj,"qrot_diff","RHO", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%qrot_diff,ss%id_qrot_diff, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "qrot_diff"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_qrot_diff
     item_c256(2) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_R_SURFMAX
  idchk = ss%id_R_SURFMAX
  ahash(2) = ps_phash_R_SURFMIN
  idchk = min(idchk,ss%id_R_SURFMIN)
  ahash(3) = ps_phash_Z_SURFMAX
  idchk = min(idchk,ss%id_Z_SURFMAX)
  ahash(4) = ps_phash_Z_SURFMIN
  idchk = min(idchk,ss%id_Z_SURFMIN)
  IF(ALLOCATED(ss%R_surfMax).AND.hcheck(4,idchk)) THEN
     listname = "RZMINMAX"
     icomponent = ps_cnum_EQ
     units = "m"
     nitems = 4
     ! for assoc. scalar list:
     list_units = "m"
     label = "edge value: R_surfMax"

     plabel = "max R on flux surface"
     CALL ps_1d_prof_store(ss%iobj,"R_surfMax","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%R_surfMax,ss%id_R_surfMax, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "R_surfMax"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_R_surfMax
     item_c256(1) = plabel

     plabel = "min R on flux surface"
     CALL ps_1d_prof_store(ss%iobj,"R_surfMin","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%R_surfMin,ss%id_R_surfMin, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "R_surfMin"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_R_surfMin
     item_c256(2) = plabel

     plabel = "max Z on flux surface"
     CALL ps_1d_prof_store(ss%iobj,"Z_surfMax","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%Z_surfMax,ss%id_Z_surfMax, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "Z_surfMax"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_Z_surfMax
     item_c256(3) = plabel

     plabel = "min Z on flux surface"
     CALL ps_1d_prof_store(ss%iobj,"Z_surfMin","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%Z_surfMin,ss%id_Z_surfMin, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(4) = "Z_surfMin"
     item_r8vals(4) = 0.0_rspec
     item_ivals(4) = ss%id_Z_surfMin
     item_c256(4) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_ELONG
  idchk = ss%id_ELONG
  ahash(2) = ps_phash_TRIANG
  idchk = min(idchk,ss%id_TRIANG)
  ahash(3) = ps_phash_TRIANGU
  idchk = min(idchk,ss%id_TRIANGU)
  ahash(4) = ps_phash_TRIANGL
  idchk = min(idchk,ss%id_TRIANGL)
  ahash(5) = ps_phash_SQUAREUO
  idchk = min(idchk,ss%id_SQUAREUO)
  ahash(6) = ps_phash_SQUARELO
  idchk = min(idchk,ss%id_SQUARELO)
  IF(ALLOCATED(ss%elong).AND.hcheck(6,idchk)) THEN
     listname = "SURFSHAPE"
     icomponent = ps_cnum_EQ
     units = "-"
     nitems = 6
     ! for assoc. scalar list:
     list_units = "-"
     label = "edge value: elong"

     plabel = "elongation (b/a)"
     CALL ps_1d_prof_store(ss%iobj,"elong","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%elong,ss%id_elong, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "elong"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_elong
     item_c256(1) = plabel

     plabel = "triangularity (symmetrized)"
     CALL ps_1d_prof_store(ss%iobj,"triang","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%triang,ss%id_triang, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "triang"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_triang
     item_c256(2) = plabel

     plabel = "upper triangularity"
     CALL ps_1d_prof_store(ss%iobj,"triangU","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%triangU,ss%id_triangU, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "triangU"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_triangU
     item_c256(3) = plabel

     plabel = "lower triangularity"
     CALL ps_1d_prof_store(ss%iobj,"triangL","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%triangL,ss%id_triangL, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(4) = "triangL"
     item_r8vals(4) = 0.0_rspec
     item_ivals(4) = ss%id_triangL
     item_c256(4) = plabel

     plabel = "upper outer squareness"
     CALL ps_1d_prof_store(ss%iobj,"squareUO","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%squareUO,ss%id_squareUO, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(5) = "squareUO"
     item_r8vals(5) = 0.0_rspec
     item_ivals(5) = ss%id_squareUO
     item_c256(5) = plabel

     plabel = "lower outer squareness"
     CALL ps_1d_prof_store(ss%iobj,"squareLO","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%squareLO,ss%id_squareLO, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(6) = "squareLO"
     item_r8vals(6) = 0.0_rspec
     item_ivals(6) = ss%id_squareLO
     item_c256(6) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PE_TRANS
  idchk = ss%id_PE_TRANS
  ahash(2) = ps_phash_PI_TRANS
  idchk = min(idchk,ss%id_PI_TRANS)
  IF(ALLOCATED(ss%pe_trans).AND.hcheck(2,idchk)) THEN
     listname = "TRANSPORT"
     icomponent = ps_cnum_PLASMA
     units = "W"
     nitems = 2
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: pe_trans"

     plabel = "electron transport (loss) power"
     CALL ps_1d_prof_store(ss%iobj,"pe_trans","RHO", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%pe_trans,ss%id_pe_trans, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "pe_trans"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_pe_trans
     item_c256(1) = plabel

     plabel = "ion transport (loss) power"
     CALL ps_1d_prof_store(ss%iobj,"pi_trans","RHO", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%pi_trans,ss%id_pi_trans, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "pi_trans"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_pi_trans
     item_c256(2) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TRIANG_MILLER_U
  idchk = ss%id_TRIANG_MILLER_U
  ahash(2) = ps_phash_TRIANG_MILLER_L
  idchk = min(idchk,ss%id_TRIANG_MILLER_L)
  IF(ALLOCATED(ss%triang_miller_U).AND.hcheck(2,idchk)) THEN
     listname = "TRIANG_MILLER"
     icomponent = ps_cnum_EQ
     units = "-"
     nitems = 2
     ! for assoc. scalar list:
     list_units = "-"
     label = "edge value: triang_miller_U"

     plabel = "Miller upper triangularity"
     CALL ps_1d_prof_store(ss%iobj,"triang_miller_U","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%triang_miller_U,ss%id_triang_miller_U, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "triang_miller_U"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_triang_miller_U
     item_c256(1) = plabel

     plabel = "Miller lower triangularity"
     CALL ps_1d_prof_store(ss%iobj,"triang_miller_L","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%triang_miller_L,ss%id_triang_miller_L, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "triang_miller_L"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_triang_miller_L
     item_c256(2) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_UPWIND_PFRAC_TE
  idchk = ss%id_UPWIND_PFRAC_TE
  ahash(2) = ps_phash_UPWIND_PFRAC_TI
  idchk = min(idchk,ss%id_UPWIND_PFRAC_TI)
  ahash(3) = ps_phash_UPWIND_PFRAC_OMEGA
  idchk = min(idchk,ss%id_UPWIND_PFRAC_OMEGA)
  IF(ALLOCATED(ss%upwind_pfrac_Te).AND.hcheck(3,idchk)) THEN
     listname = "UPWIND_FORWARD_FRACTIONS"
     icomponent = ps_cnum_PLASMA
     units = "-"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "-"
     label = "simple avg: upwind_pfrac_Te"

     plabel = "Te (electron energy advection) forward upwind fraction"
     CALL ps_1d_prof_store(ss%iobj,"upwind_pfrac_Te","RHO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho,0, &
          ss%upwind_pfrac_Te,ss%id_upwind_pfrac_Te, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "upwind_pfrac_Te"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_upwind_pfrac_Te
     item_c256(1) = plabel

     plabel = "Ti (ion energy advection) forward upwind fraction"
     CALL ps_1d_prof_store(ss%iobj,"upwind_pfrac_Ti","RHO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho,0, &
          ss%upwind_pfrac_Ti,ss%id_upwind_pfrac_Ti, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "upwind_pfrac_Ti"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_upwind_pfrac_Ti
     item_c256(2) = plabel

     plabel = "omega (ang. velocity) forward upwind fraction"
     CALL ps_1d_prof_store(ss%iobj,"upwind_pfrac_omega","RHO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho,0, &
          ss%upwind_pfrac_omega,ss%id_upwind_pfrac_omega, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "upwind_pfrac_omega"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_upwind_pfrac_omega
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_ZEFF
  idchk = ss%id_ZEFF
  ahash(2) = ps_phash_ZEFF_TH
  idchk = min(idchk,ss%id_ZEFF_TH)
  ahash(3) = ps_phash_ZEFF_FI
  idchk = min(idchk,ss%id_ZEFF_FI)
  IF(ALLOCATED(ss%Zeff).AND.hcheck(3,idchk)) THEN
     listname = "ZEFF_GROUP"
     icomponent = ps_cnum_PLASMA
     units = "-"
     nitems = 3
     ! for assoc. scalar list:
     list_units = "-"
     label = "wght avg (density): Zeff"

     plabel = "Zeff (sum[nj*Zj**2]/ne)"
     CALL ps_1d_prof_store(ss%iobj,"Zeff","RHO", &
          units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%Zeff,ss%id_Zeff, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "Zeff"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_Zeff
     item_c256(1) = plabel

     plabel = "Thermal ion contribution to Zeff"
     CALL ps_1d_prof_store(ss%iobj,"Zeff_th","RHO", &
          units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%Zeff_th,ss%id_Zeff_th, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(2) = "Zeff_th"
     item_r8vals(2) = 0.0_rspec
     item_ivals(2) = ss%id_Zeff_th
     item_c256(2) = plabel

     plabel = "Fast ion contribution to Zeff"
     CALL ps_1d_prof_store(ss%iobj,"Zeff_fi","RHO", &
          units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%Zeff_fi,ss%id_Zeff_fi, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(3) = "Zeff_fi"
     item_r8vals(3) = 0.0_rspec
     item_ivals(3) = ss%id_Zeff_fi
     item_c256(3) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
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

END SUBROUTINE plasma_state_xload_pgrps
