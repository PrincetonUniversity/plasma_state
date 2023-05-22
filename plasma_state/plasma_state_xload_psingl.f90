
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_psingl(ss,xhash,iout,ierr)

  ! called from plasma_state_xload: check grids; store in xplasma structure

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:

  INTEGER, PARAMETER :: max_local = 4

  INTEGER :: icomponent,imod,nitems,idchk
  CHARACTER*32 :: listname,units,list_units
  CHARACTER*80 :: label,plabel
  INTEGER :: ahash(ps_max_static)

  CHARACTER*32 :: item_names(max_local)
  INTEGER :: item_ivals(max_local)
  REAL(KIND=rspec) :: item_r8vals(max_local)
  CHARACTER*256 :: item_c256(max_local)
  !-----------------------

  ahash(1) = ps_phash_VOL
  idchk = ss%id_vol
  IF(ALLOCATED(ss%vol).AND.hcheck(1,idchk)) THEN
     listname = "VOL_LIST"
     icomponent = ps_cnum_EQ
     units = "m^3"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^3"
     label = "edge value: vol"

     plabel = "enclosed volume"
     CALL ps_1d_prof_store(ss%iobj,"vol","RHO_EQ", &
          units,icomponent,plabel,ps_spline,ps_bc_grad00, &
          ss%nrho_eq,0, &
          ss%vol,ss%id_vol, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "vol"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_vol
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_AREA
  idchk = ss%id_area
  IF(ALLOCATED(ss%area).AND.hcheck(1,idchk)) THEN
     listname = "AREA_LIST"
     icomponent = ps_cnum_EQ
     units = "m^2"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2"
     label = "edge value: area"

     plabel = "enclosed area"
     CALL ps_1d_prof_store(ss%iobj,"area","RHO_EQ", &
          units,icomponent,plabel,ps_spline,ps_bc_grad00, &
          ss%nrho_eq,0, &
          ss%area,ss%id_area, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "area"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_area
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_ANOM_EVAR
  idchk = ss%id_anom_evar
  IF(ALLOCATED(ss%anom_evar).AND.hcheck(1,idchk)) THEN
     listname = "ANOM_EVAR_LIST"
     icomponent = ps_cnum_ANOM
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "simple avg: anom_evar"

     plabel = "anomalous transport energy variation"
     CALL ps_1d_prof_store(ss%iobj,"anom_evar","E_ANOM", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nefi_anom,0, &
          ss%anom_evar,ss%id_anom_evar, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "anom_evar"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_anom_evar
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BALSTAB
  idchk = ss%id_balstab
  IF(ALLOCATED(ss%balstab).AND.hcheck(1,idchk)) THEN
     listname = "BALSTAB_LIST"
     icomponent = ps_cnum_LMHD
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "simple avg: balstab"

     plabel = "Ballooning/Mercier stability indicator"
     CALL ps_1d_prof_store(ss%iobj,"balstab","RHO_LMHD", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_lmhd,0, &
          ss%balstab,ss%id_balstab, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "balstab"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_balstab
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BPHIRZ
  idchk = ss%id_BphiRZ
  IF(ALLOCATED(ss%BphiRZ).AND.hcheck(1,idchk)) THEN
     listname = "BPHIRZ_LIST"
     icomponent = ps_cnum_EQ
     units = "T"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "T"
     label = "simple avg: BphiRZ"

     plabel = "Toroidal field"
     CALL ps_2d_prof_store(ss%iobj,"BphiRZ","R_GRID","Z_GRID", &
          units,icomponent,plabel,ps_hermite,ps_bc_dflt, &
          ss%nr,ss%nz,0,0, &
          ss%BphiRZ,ss%id_BphiRZ, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "BphiRZ"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_BphiRZ
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BRRZ
  idchk = ss%id_BRRZ
  IF(ALLOCATED(ss%BRRZ).AND.hcheck(1,idchk)) THEN
     listname = "BRRZ_LIST"
     icomponent = ps_cnum_EQ
     units = "T"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "T"
     label = "simple avg: BRRZ"

     plabel = "R component of poloidal field"
     CALL ps_2d_prof_store(ss%iobj,"BRRZ","R_GRID","Z_GRID", &
          units,icomponent,plabel,ps_hermite,ps_bc_dflt, &
          ss%nr,ss%nz,0,0, &
          ss%BRRZ,ss%id_BRRZ, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "BRRZ"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_BRRZ
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_BZRZ
  idchk = ss%id_BZRZ
  IF(ALLOCATED(ss%BZRZ).AND.hcheck(1,idchk)) THEN
     listname = "BZRZ_LIST"
     icomponent = ps_cnum_EQ
     units = "T"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "T"
     label = "simple avg: BZRZ"

     plabel = "Z component of poloidal field"
     CALL ps_2d_prof_store(ss%iobj,"BZRZ","R_GRID","Z_GRID", &
          units,icomponent,plabel,ps_hermite,ps_bc_dflt, &
          ss%nr,ss%nz,0,0, &
          ss%BZRZ,ss%id_BZRZ, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "BZRZ"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_BZRZ
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_ANOM
  idchk = ss%id_chie_anom
  IF(ALLOCATED(ss%chie_anom).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chie_anom"

     plabel = "Te-equation, diffusivity term"
     CALL ps_1d_prof_store(ss%iobj,"chie_anom","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chie_anom,ss%id_chie_anom, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_anom"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_anom
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_DRBM
  idchk = ss%id_chie_drbm
  IF(ALLOCATED(ss%chie_drbm).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chie_drbm"

     plabel = "Te-equation, diffusivity term"
     CALL ps_1d_prof_store(ss%iobj,"chie_drbm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chie_drbm,ss%id_chie_drbm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_drbm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_drbm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_ETG
  idchk = ss%id_chie_etg
  IF(ALLOCATED(ss%chie_etg).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_ETG_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chie_etg"

     plabel = "Te-equation, diffusivity term ETG model"
     CALL ps_1d_prof_store(ss%iobj,"chie_etg","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chie_etg,ss%id_chie_etg, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_etg"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_etg
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_GLF
  idchk = ss%id_chie_glf
  IF(ALLOCATED(ss%chie_glf).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_GLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chie_glf"

     plabel = "Te-equation, diffusivity from GLF23 model"
     CALL ps_1d_prof_store(ss%iobj,"chie_glf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chie_glf,ss%id_chie_glf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_glf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_glf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_GTCNEO
  idchk = ss%id_chie_gtcneo
  IF(ALLOCATED(ss%chie_gtcneo).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chie_gtcneo"

     plabel = "Te-equation, neoclassical diffusivity from GTC_NEO model"
     CALL ps_1d_prof_store(ss%iobj,"chie_gtcneo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chie_gtcneo,ss%id_chie_gtcneo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_gtcneo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_gtcneo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_MMM71
  idchk = ss%id_chie_mmm71
  IF(ALLOCATED(ss%chie_mmm71).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chie_mmm71"

     plabel = "Te-equation, diffusivity term"
     CALL ps_1d_prof_store(ss%iobj,"chie_mmm71","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chie_mmm71,ss%id_chie_mmm71, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_mmm71"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_mmm71
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_MTM
  idchk = ss%id_chie_mtm
  IF(ALLOCATED(ss%chie_mtm).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_MTM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chie_mtm"

     plabel = "Te-equation, thermal diffusivity term MTM model"
     CALL ps_1d_prof_store(ss%iobj,"chie_mtm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chie_mtm,ss%id_chie_mtm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_mtm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_mtm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_NCLASS
  idchk = ss%id_chie_nclass
  IF(ALLOCATED(ss%chie_nclass).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chie_nclass"

     plabel = "Te-equation, neoclassical diffusivity from NCLASS model"
     CALL ps_1d_prof_store(ss%iobj,"chie_nclass","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chie_nclass,ss%id_chie_nclass, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_nclass"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_nclass
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_NEO
  idchk = ss%id_chie_neo
  IF(ALLOCATED(ss%chie_neo).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_NEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chie_neo"

     plabel = "Te-equation, neoclassical diffusivity from NEO model"
     CALL ps_1d_prof_store(ss%iobj,"chie_neo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chie_neo,ss%id_chie_neo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_neo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_neo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_NTM
  idchk = ss%id_chie_ntm
  IF(ALLOCATED(ss%chie_ntm).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_NTM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chie_ntm"

     plabel = "Te-equation, neoclassical diffusivity from NTM model"
     CALL ps_1d_prof_store(ss%iobj,"chie_ntm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chie_ntm,ss%id_chie_ntm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_ntm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_ntm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_PALEO
  idchk = ss%id_chie_paleo
  IF(ALLOCATED(ss%chie_paleo).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_PALEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chie_paleo"

     plabel = "Te-equation, diffusivity term PALEO model"
     CALL ps_1d_prof_store(ss%iobj,"chie_paleo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chie_paleo,ss%id_chie_paleo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_paleo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_paleo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_TGLF
  idchk = ss%id_chie_tglf
  IF(ALLOCATED(ss%chie_tglf).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chie_tglf"

     plabel = "Te-equation, diffusivity from TGLF model"
     CALL ps_1d_prof_store(ss%iobj,"chie_tglf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chie_tglf,ss%id_chie_tglf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_tglf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_tglf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIE_W19
  idchk = ss%id_chie_w19
  IF(ALLOCATED(ss%chie_w19).AND.hcheck(1,idchk)) THEN
     listname = "CHIE_W19_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chie_w19"

     plabel = "Te-equation, diffusivity term"
     CALL ps_1d_prof_store(ss%iobj,"chie_w19","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chie_w19,ss%id_chie_w19, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chie_w19"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chie_w19
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_ANOM
  idchk = ss%id_chii_anom
  IF(ALLOCATED(ss%chii_anom).AND.hcheck(1,idchk)) THEN
     listname = "CHII_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chii_anom"

     plabel = "Ti-equation, diffusivity term"
     CALL ps_1d_prof_store(ss%iobj,"chii_anom","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chii_anom,ss%id_chii_anom, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chii_anom"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chii_anom
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_DRBM
  idchk = ss%id_chii_drbm
  IF(ALLOCATED(ss%chii_drbm).AND.hcheck(1,idchk)) THEN
     listname = "CHII_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chii_drbm"

     plabel = "Ti-equation, diffusivity term"
     CALL ps_1d_prof_store(ss%iobj,"chii_drbm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chii_drbm,ss%id_chii_drbm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chii_drbm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chii_drbm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_GLF
  idchk = ss%id_chii_glf
  IF(ALLOCATED(ss%chii_glf).AND.hcheck(1,idchk)) THEN
     listname = "CHII_GLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chii_glf"

     plabel = "Ti-equation, diffusivity from GLF23 model"
     CALL ps_1d_prof_store(ss%iobj,"chii_glf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chii_glf,ss%id_chii_glf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chii_glf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chii_glf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_GTCNEO
  idchk = ss%id_chii_gtcneo
  IF(ALLOCATED(ss%chii_gtcneo).AND.hcheck(1,idchk)) THEN
     listname = "CHII_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chii_gtcneo"

     plabel = "Ti-equation, neoclassical diffusivity from GTC_NEO model"
     CALL ps_1d_prof_store(ss%iobj,"chii_gtcneo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chii_gtcneo,ss%id_chii_gtcneo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chii_gtcneo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chii_gtcneo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_MMM71
  idchk = ss%id_chii_mmm71
  IF(ALLOCATED(ss%chii_mmm71).AND.hcheck(1,idchk)) THEN
     listname = "CHII_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chii_mmm71"

     plabel = "Ti-equation, diffusivity term"
     CALL ps_1d_prof_store(ss%iobj,"chii_mmm71","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chii_mmm71,ss%id_chii_mmm71, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chii_mmm71"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chii_mmm71
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_NCLASS
  idchk = ss%id_chii_nclass
  IF(ALLOCATED(ss%chii_nclass).AND.hcheck(1,idchk)) THEN
     listname = "CHII_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chii_nclass"

     plabel = "Ti-equation, neoclassical diffusivity from NCALSS model"
     CALL ps_1d_prof_store(ss%iobj,"chii_nclass","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chii_nclass,ss%id_chii_nclass, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chii_nclass"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chii_nclass
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_NEO
  idchk = ss%id_chii_neo
  IF(ALLOCATED(ss%chii_neo).AND.hcheck(1,idchk)) THEN
     listname = "CHII_NEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chii_neo"

     plabel = "Ti-equation, neoclassical diffusivity from NEO model"
     CALL ps_1d_prof_store(ss%iobj,"chii_neo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chii_neo,ss%id_chii_neo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chii_neo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chii_neo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_NTM
  idchk = ss%id_chii_ntm
  IF(ALLOCATED(ss%chii_ntm).AND.hcheck(1,idchk)) THEN
     listname = "CHII_NTM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chii_ntm"

     plabel = "Ti-equation, neoclassical diffusivity from NTM model"
     CALL ps_1d_prof_store(ss%iobj,"chii_ntm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chii_ntm,ss%id_chii_ntm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chii_ntm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chii_ntm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_TGLF
  idchk = ss%id_chii_tglf
  IF(ALLOCATED(ss%chii_tglf).AND.hcheck(1,idchk)) THEN
     listname = "CHII_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chii_tglf"

     plabel = "Ti-equation, diffusivity from TGLF model"
     CALL ps_1d_prof_store(ss%iobj,"chii_tglf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chii_tglf,ss%id_chii_tglf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chii_tglf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chii_tglf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHII_W19
  idchk = ss%id_chii_w19
  IF(ALLOCATED(ss%chii_w19).AND.hcheck(1,idchk)) THEN
     listname = "CHII_W19_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chii_w19"

     plabel = "Ti-equation, diffusivity term"
     CALL ps_1d_prof_store(ss%iobj,"chii_w19","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chii_w19,ss%id_chii_w19, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chii_w19"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chii_w19
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_ANOM
  idchk = ss%id_chiphi_anom
  IF(ALLOCATED(ss%chiphi_anom).AND.hcheck(1,idchk)) THEN
     listname = "CHIPHI_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chiphi_anom"

     plabel = "momentum-equation, diffusivity term"
     CALL ps_1d_prof_store(ss%iobj,"chiphi_anom","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chiphi_anom,ss%id_chiphi_anom, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chiphi_anom"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chiphi_anom
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_DRBM
  idchk = ss%id_chiphi_drbm
  IF(ALLOCATED(ss%chiphi_drbm).AND.hcheck(1,idchk)) THEN
     listname = "CHIPHI_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chiphi_drbm"

     plabel = "momentum-equation, diffusivity term"
     CALL ps_1d_prof_store(ss%iobj,"chiphi_drbm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chiphi_drbm,ss%id_chiphi_drbm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chiphi_drbm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chiphi_drbm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_GLF
  idchk = ss%id_chiphi_glf
  IF(ALLOCATED(ss%chiphi_glf).AND.hcheck(1,idchk)) THEN
     listname = "CHIPHI_GLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chiphi_glf"

     plabel = "momentum-equation, diffusivity from GLF23 model"
     CALL ps_1d_prof_store(ss%iobj,"chiphi_glf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chiphi_glf,ss%id_chiphi_glf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chiphi_glf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chiphi_glf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_GTCNEO
  idchk = ss%id_chiphi_gtcneo
  IF(ALLOCATED(ss%chiphi_gtcneo).AND.hcheck(1,idchk)) THEN
     listname = "CHIPHI_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chiphi_gtcneo"

     plabel = "momentum-equation, neoclassical diffusivity from GTC_NEO model"
     CALL ps_1d_prof_store(ss%iobj,"chiphi_gtcneo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chiphi_gtcneo,ss%id_chiphi_gtcneo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chiphi_gtcneo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chiphi_gtcneo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_MMM71
  idchk = ss%id_chiphi_mmm71
  IF(ALLOCATED(ss%chiphi_mmm71).AND.hcheck(1,idchk)) THEN
     listname = "CHIPHI_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chiphi_mmm71"

     plabel = "momentum-equation, diffusivity term, toroidal"
     CALL ps_1d_prof_store(ss%iobj,"chiphi_mmm71","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chiphi_mmm71,ss%id_chiphi_mmm71, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chiphi_mmm71"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chiphi_mmm71
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_NEO
  idchk = ss%id_chiphi_neo
  IF(ALLOCATED(ss%chiphi_neo).AND.hcheck(1,idchk)) THEN
     listname = "CHIPHI_NEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chiphi_neo"

     plabel = "momentum-equation, neoclassical diffusivity from NEO model"
     CALL ps_1d_prof_store(ss%iobj,"chiphi_neo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chiphi_neo,ss%id_chiphi_neo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chiphi_neo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chiphi_neo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_TGLF
  idchk = ss%id_chiphi_tglf
  IF(ALLOCATED(ss%chiphi_tglf).AND.hcheck(1,idchk)) THEN
     listname = "CHIPHI_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: chiphi_tglf"

     plabel = "momentum-equation, diffusivity from TGLF model"
     CALL ps_1d_prof_store(ss%iobj,"chiphi_tglf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chiphi_tglf,ss%id_chiphi_tglf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chiphi_tglf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chiphi_tglf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHIPHI_W19
  idchk = ss%id_chiphi_w19
  IF(ALLOCATED(ss%chiphi_w19).AND.hcheck(1,idchk)) THEN
     listname = "CHIPHI_W19_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chiphi_w19"

     plabel = "momentum-equation, diffusivity term"
     CALL ps_1d_prof_store(ss%iobj,"chiphi_w19","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chiphi_w19,ss%id_chiphi_w19, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chiphi_w19"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chiphi_w19
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CHITHETA_MMM71
  idchk = ss%id_chitheta_mmm71
  IF(ALLOCATED(ss%chitheta_mmm71).AND.hcheck(1,idchk)) THEN
     listname = "CHITHETA_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: chitheta_mmm71"

     plabel = "momentum-equation, diffusivity term, poloidal"
     CALL ps_1d_prof_store(ss%iobj,"chitheta_mmm71","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%chitheta_mmm71,ss%id_chitheta_mmm71, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "chitheta_mmm71"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_chitheta_mmm71
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURBEAM
  idchk = ss%id_curbeam
  IF(ALLOCATED(ss%curbeam).AND.hcheck(1,idchk)) THEN
     listname = "CURBEAM_LIST"
     icomponent = ps_cnum_NBI
     units = "A"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "A"
     label = "area int: curbeam"

     plabel = "beam ion driven current (shielded)"
     CALL ps_1d_prof_store(ss%iobj,"curbeam","RHO_NBI", &
          units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%curbeam,ss%id_curbeam, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "curbeam"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_curbeam
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURECH
  idchk = ss%id_curech
  IF(ALLOCATED(ss%curech).AND.hcheck(1,idchk)) THEN
     listname = "CURECH_LIST"
     icomponent = ps_cnum_EC
     units = "A"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "A"
     label = "area int: curech"

     plabel = "ECH current drive"
     CALL ps_1d_prof_store(ss%iobj,"curech","RHO_ECRF", &
          units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
          ss%nrho_ecrf-1,0, &
          ss%curech,ss%id_curech, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "curech"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_curech
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURFUSN
  idchk = ss%id_curfusn
  IF(ALLOCATED(ss%curfusn).AND.hcheck(1,idchk)) THEN
     listname = "CURFUSN_LIST"
     icomponent = ps_cnum_FUS
     units = "A"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "A"
     label = "area int: curfusn"

     plabel = "fusion ion driven current (shielded)"
     CALL ps_1d_prof_store(ss%iobj,"curfusn","RHO_FUS", &
          units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
          ss%nrho_fus-1,0, &
          ss%curfusn,ss%id_curfusn, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "curfusn"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_curfusn
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURICH
  idchk = ss%id_curich
  IF(ALLOCATED(ss%curich).AND.hcheck(1,idchk)) THEN
     listname = "CURICH_LIST"
     icomponent = ps_cnum_IC
     units = "A"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "A"
     label = "area int: curich"

     plabel = "total driven current (all IC srcs)"
     CALL ps_1d_prof_store(ss%iobj,"curich","RHO_ICRF", &
          units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
          ss%nrho_icrf-1,0, &
          ss%curich,ss%id_curich, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "curich"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_curich
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURLH
  idchk = ss%id_curlh
  IF(ALLOCATED(ss%curlh).AND.hcheck(1,idchk)) THEN
     listname = "CURLH_LIST"
     icomponent = ps_cnum_LH
     units = "A"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "A"
     label = "area int: curlh"

     plabel = "LH current drive"
     CALL ps_1d_prof_store(ss%iobj,"curlh","RHO_LHRF", &
          units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
          ss%nrho_lhrf-1,0, &
          ss%curlh,ss%id_curlh, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "curlh"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_curlh
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURMINO
  idchk = ss%id_curmino
  IF(ALLOCATED(ss%curmino).AND.hcheck(1,idchk)) THEN
     listname = "CURMINO_LIST"
     icomponent = ps_cnum_IC
     units = "A"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "A"
     label = "area int: curmino"

     plabel = "minority ion driven cur (shielded)."
     CALL ps_1d_prof_store(ss%iobj,"curmino","RHO_ICRF", &
          units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
          ss%nrho_icrf-1,0, &
          ss%curmino,ss%id_curmino, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "curmino"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_curmino
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURR_BOOTSTRAP
  idchk = ss%id_curr_bootstrap
  IF(ALLOCATED(ss%curr_bootstrap).AND.hcheck(1,idchk)) THEN
     listname = "CURR_BOOTSTRAP_LIST"
     icomponent = ps_cnum_PLASMA
     units = "A"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "A"
     label = "area int: curr_bootstrap"

     plabel = "Neoclassical bootstrap current"
     CALL ps_1d_prof_store(ss%iobj,"curr_bootstrap","RHO", &
          units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%curr_bootstrap,ss%id_curr_bootstrap, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "curr_bootstrap"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_curr_bootstrap
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURR_OHMIC
  idchk = ss%id_curr_ohmic
  IF(ALLOCATED(ss%curr_ohmic).AND.hcheck(1,idchk)) THEN
     listname = "CURR_OHMIC_LIST"
     icomponent = ps_cnum_PLASMA
     units = "A"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "A"
     label = "area int: curr_ohmic"

     plabel = "Ohmic current"
     CALL ps_1d_prof_store(ss%iobj,"curr_ohmic","RHO", &
          units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%curr_ohmic,ss%id_curr_ohmic, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "curr_ohmic"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_curr_ohmic
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CURT
  idchk = ss%id_curt
  IF(ALLOCATED(ss%curt).AND.hcheck(1,idchk)) THEN
     listname = "CURT_LIST"
     icomponent = ps_cnum_EQ
     units = "A"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "A"
     label = "edge value: curt"

     plabel = "total enclosed toroidal current"
     CALL ps_1d_prof_store(ss%iobj,"curt","RHO_EQ", &
          units,icomponent,plabel,ps_spline,ps_bc_grad00, &
          ss%nrho_eq,0, &
          ss%curt,ss%id_curt, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "curt"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_curt
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_CUR_RW
  idchk = ss%id_cur_rw
  IF(ALLOCATED(ss%cur_rw).AND.hcheck(1,idchk)) THEN
     listname = "CUR_RW_LIST"
     icomponent = ps_cnum_RUNAWAY
     units = "A"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "A"
     label = "area int: cur_rw"

     plabel = "current carried by fast electrons"
     CALL ps_1d_prof_store(ss%iobj,"cur_rw","RHO_RW", &
          units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
          ss%nrho_rw-1,0, &
          ss%cur_rw,ss%id_cur_rw, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "cur_rw"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_cur_rw
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DB_MTM
  idchk = ss%id_db_mtm
  IF(ALLOCATED(ss%db_mtm).AND.hcheck(1,idchk)) THEN
     listname = "DB_MTM_LIST"
     icomponent = ps_cnum_ANOM
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "simple avg: db_mtm"

     plabel = "dB/B from MTM model"
     CALL ps_1d_prof_store(ss%iobj,"db_mtm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%db_mtm,ss%id_db_mtm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "db_mtm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_db_mtm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DCUR_RW_DVLOOP
  idchk = ss%id_dcur_rw_dVloop
  IF(ALLOCATED(ss%dcur_rw_dVloop).AND.hcheck(1,idchk)) THEN
     listname = "DCUR_RW_DVLOOP_LIST"
     icomponent = ps_cnum_RUNAWAY
     units = "A/Volt"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "A/Volt"
     label = "area int: dcur_rw_dVloop"

     plabel = "sensitivity to loop volts"
     CALL ps_1d_prof_store(ss%iobj,"dcur_rw_dVloop","RHO_RW", &
          units,icomponent,plabel,ps_stepda,ps_bc_dflt, &
          ss%nrho_rw-1,0, &
          ss%dcur_rw_dVloop,ss%id_dcur_rw_dVloop, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "dcur_rw_dVloop"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_dcur_rw_dVloop
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNE_NCLASS
  idchk = ss%id_difne_nclass
  IF(ALLOCATED(ss%difne_nclass).AND.hcheck(1,idchk)) THEN
     listname = "DIFNE_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: difne_nclass"

     plabel = "electron-density-equations, neoclassical diffusivity from NCLASS model"
     CALL ps_1d_prof_store(ss%iobj,"difne_nclass","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%difne_nclass,ss%id_difne_nclass, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "difne_nclass"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_difne_nclass
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNH_NCLASS
  idchk = ss%id_difnh_nclass
  IF(ALLOCATED(ss%difnh_nclass).AND.hcheck(1,idchk)) THEN
     listname = "DIFNH_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: difnh_nclass"

     plabel = "main-hydrogenic-density-equations, neoclassical diffusivity from NCLASS model"
     CALL ps_1d_prof_store(ss%iobj,"difnh_nclass","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%difnh_nclass,ss%id_difnh_nclass, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "difnh_nclass"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_difnh_nclass
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNX_NCLASS
  idchk = ss%id_difnx_nclass
  IF(ALLOCATED(ss%difnx_nclass).AND.hcheck(1,idchk)) THEN
     listname = "DIFNX_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/s"
     label = "simple avg: difnx_nclass"

     plabel = "main-impurity-density-equations, neoclassical diffusivity from NCLASS model"
     CALL ps_1d_prof_store(ss%iobj,"difnx_nclass","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%difnx_nclass,ss%id_difnx_nclass, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "difnx_nclass"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_difnx_nclass
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DIFNZ_MMM71
  idchk = ss%id_difnz_mmm71
  IF(ALLOCATED(ss%difnz_mmm71).AND.hcheck(1,idchk)) THEN
     listname = "DIFNZ_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^2/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2/sec"
     label = "simple avg: difnz_mmm71"

     plabel = "impurity ion diffusivity term"
     CALL ps_1d_prof_store(ss%iobj,"difnz_mmm71","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%difnz_mmm71,ss%id_difnz_mmm71, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "difnz_mmm71"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_difnz_mmm71
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DQEDGRDTE_TGLF
  idchk = ss%id_dQedGrdte_tglf
  IF(ALLOCATED(ss%dQedGrdte_tglf).AND.hcheck(1,idchk)) THEN
     listname = "DQEDGRDTE_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "1/(m*s)"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "1/(m*s)"
     label = "simple avg: dQedGrdte_tglf"

     plabel = "Te flux gradient term from TGLF model"
     CALL ps_1d_prof_store(ss%iobj,"dQedGrdte_tglf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%dQedGrdte_tglf,ss%id_dQedGrdte_tglf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "dQedGrdte_tglf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_dQedGrdte_tglf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DQEDGRDTI_TGLF
  idchk = ss%id_dQedGrdti_tglf
  IF(ALLOCATED(ss%dQedGrdti_tglf).AND.hcheck(1,idchk)) THEN
     listname = "DQEDGRDTI_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "1/(m*s)"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "1/(m*s)"
     label = "simple avg: dQedGrdti_tglf"

     plabel = "Te flux gradient term from TGLF model"
     CALL ps_1d_prof_store(ss%iobj,"dQedGrdti_tglf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%dQedGrdti_tglf,ss%id_dQedGrdti_tglf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "dQedGrdti_tglf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_dQedGrdti_tglf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DQIDGRDTE_TGLF
  idchk = ss%id_dQidGrdte_tglf
  IF(ALLOCATED(ss%dQidGrdte_tglf).AND.hcheck(1,idchk)) THEN
     listname = "DQIDGRDTE_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "1/(m*s)"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "1/(m*s)"
     label = "simple avg: dQidGrdte_tglf"

     plabel = "Ti flux gradient term from TGLF model"
     CALL ps_1d_prof_store(ss%iobj,"dQidGrdte_tglf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%dQidGrdte_tglf,ss%id_dQidGrdte_tglf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "dQidGrdte_tglf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_dQidGrdte_tglf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_DQIDGRDTI_TGLF
  idchk = ss%id_dQidGrdti_tglf
  IF(ALLOCATED(ss%dQidGrdti_tglf).AND.hcheck(1,idchk)) THEN
     listname = "DQIDGRDTI_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "1/(m*s)"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "1/(m*s)"
     label = "simple avg: dQidGrdti_tglf"

     plabel = "Ti flux gradient term from TGLF model"
     CALL ps_1d_prof_store(ss%iobj,"dQidGrdti_tglf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%dQidGrdti_tglf,ss%id_dQidGrdti_tglf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "dQidGrdti_tglf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_dQidGrdti_tglf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EE_MOBILITY_FACTOR
  idchk = ss%id_ee_mobility_factor
  IF(ALLOCATED(ss%ee_mobility_factor).AND.hcheck(1,idchk)) THEN
     listname = "EE_MOBILITY_FACTOR_LIST"
     icomponent = ps_cnum_ANOM
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "simple avg: ee_mobility_factor"

     plabel = "chi_e/chi_i"
     CALL ps_1d_prof_store(ss%iobj,"ee_mobility_factor","RHO_ANOMTH", &
          units,icomponent,plabel,ps_hermite,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%ee_mobility_factor,ss%id_ee_mobility_factor, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "ee_mobility_factor"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_ee_mobility_factor
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPERP_RW
  idchk = ss%id_eperp_rw
  IF(ALLOCATED(ss%eperp_rw).AND.hcheck(1,idchk)) THEN
     listname = "EPERP_RW_LIST"
     icomponent = ps_cnum_RUNAWAY
     units = "keV"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "keV"
     label = "wght avg (NRW): eperp_rw"

     plabel = "runaway electron <Eperp>"
     CALL ps_1d_prof_store(ss%iobj,"eperp_rw","RHO_RW", &
          units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
          ss%nrho_rw-1,0, &
          ss%eperp_rw,ss%id_eperp_rw, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "eperp_rw"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_eperp_rw
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPLL_RW
  idchk = ss%id_epll_rw
  IF(ALLOCATED(ss%epll_rw).AND.hcheck(1,idchk)) THEN
     listname = "EPLL_RW_LIST"
     icomponent = ps_cnum_RUNAWAY
     units = "keV"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "keV"
     label = "wght avg (NRW): epll_rw"

     plabel = "runaway electron <Epll>"
     CALL ps_1d_prof_store(ss%iobj,"epll_rw","RHO_RW", &
          units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
          ss%nrho_rw-1,0, &
          ss%epll_rw,ss%id_epll_rw, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "epll_rw"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_epll_rw
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_EPOT
  idchk = ss%id_Epot
  IF(ALLOCATED(ss%Epot).AND.hcheck(1,idchk)) THEN
     listname = "EPOT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "keV"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "keV"
     label = "simple avg: Epot"

     plabel = "radial electrostatic potential"
     CALL ps_1d_prof_store(ss%iobj,"Epot","RHO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho,0, &
          ss%Epot,ss%id_Epot, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "Epot"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_Epot
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_ETA_PARALLEL
  idchk = ss%id_eta_parallel
  IF(ALLOCATED(ss%eta_parallel).AND.hcheck(1,idchk)) THEN
     listname = "ETA_PARALLEL_LIST"
     icomponent = ps_cnum_PLASMA
     units = "ohm*m"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "ohm*m"
     label = "simple avg: eta_parallel"

     plabel = "parallel electrical resistivity"
     CALL ps_1d_prof_store(ss%iobj,"eta_parallel","RHO", &
          units,icomponent,plabel,ps_hermite,ps_bc_dflt, &
          ss%nrho,0, &
          ss%eta_parallel,ss%id_eta_parallel, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "eta_parallel"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_eta_parallel
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FI_DEPLETION
  idchk = ss%id_Fi_depletion
  IF(ALLOCATED(ss%Fi_depletion).AND.hcheck(1,idchk)) THEN
     listname = "FI_DEPLETION_LIST"
     icomponent = ps_cnum_PLASMA
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "wght avg (density): Fi_depletion"

     plabel = "fast ion depletion: sum[nj*Zj/ne]"
     CALL ps_1d_prof_store(ss%iobj,"Fi_depletion","RHO", &
          units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%Fi_depletion,ss%id_Fi_depletion, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "Fi_depletion"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_Fi_depletion
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_DRBM
  idchk = ss%id_freq_drbm
  IF(ALLOCATED(ss%freq_drbm).AND.hcheck(1,idchk)) THEN
     listname = "FREQ_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     units = "1/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "1/sec"
     label = "simple avg: freq_drbm"

     plabel = "frequency from DRBM model"
     CALL ps_1d_prof_store(ss%iobj,"freq_drbm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%freq_drbm,ss%id_freq_drbm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "freq_drbm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_freq_drbm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_ETG
  idchk = ss%id_freq_etg
  IF(ALLOCATED(ss%freq_etg).AND.hcheck(1,idchk)) THEN
     listname = "FREQ_ETG_LIST"
     icomponent = ps_cnum_ANOM
     units = "1/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "1/sec"
     label = "simple avg: freq_etg"

     plabel = "frequency from ETG model"
     CALL ps_1d_prof_store(ss%iobj,"freq_etg","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%freq_etg,ss%id_freq_etg, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "freq_etg"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_freq_etg
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_GLF
  idchk = ss%id_freq_glf
  IF(ALLOCATED(ss%freq_glf).AND.hcheck(1,idchk)) THEN
     listname = "FREQ_GLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "GB"
     label = "simple avg: freq_glf"

     plabel = "frequency from GLF23 model"
     CALL ps_1d_prof_store(ss%iobj,"freq_glf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%freq_glf,ss%id_freq_glf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "freq_glf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_freq_glf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_MAX_TGLF
  idchk = ss%id_freq_max_tglf
  IF(ALLOCATED(ss%freq_max_tglf).AND.hcheck(1,idchk)) THEN
     listname = "FREQ_MAX_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "GB"
     label = "simple avg: freq_max_tglf"

     plabel = "frequency corresponding maximum growthrate from TGLF model"
     CALL ps_1d_prof_store(ss%iobj,"freq_max_tglf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%freq_max_tglf,ss%id_freq_max_tglf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "freq_max_tglf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_freq_max_tglf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_FREQ_MTM
  idchk = ss%id_freq_mtm
  IF(ALLOCATED(ss%freq_mtm).AND.hcheck(1,idchk)) THEN
     listname = "FREQ_MTM_LIST"
     icomponent = ps_cnum_ANOM
     units = "1/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "1/sec"
     label = "simple avg: freq_mtm"

     plabel = "frequency from MTM model"
     CALL ps_1d_prof_store(ss%iobj,"freq_mtm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%freq_mtm,ss%id_freq_mtm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "freq_mtm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_freq_mtm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GAMMA_NC
  idchk = ss%id_gamma_nc
  IF(ALLOCATED(ss%gamma_nc).AND.hcheck(1,idchk)) THEN
     listname = "GAMMA_NC_LIST"
     icomponent = ps_cnum_EQ
     units = "m^-1"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^-1"
     label = "edge value: gamma_nc"

     plabel = "NC gamma"
     CALL ps_1d_prof_store(ss%iobj,"gamma_nc","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%gamma_nc,ss%id_gamma_nc, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "gamma_nc"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_gamma_nc
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GB1
  idchk = ss%id_gb1
  IF(ALLOCATED(ss%gb1).AND.hcheck(1,idchk)) THEN
     listname = "GB1_LIST"
     icomponent = ps_cnum_EQ
     units = "T"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "T"
     label = "edge value: gb1"

     plabel = "<|B|>"
     CALL ps_1d_prof_store(ss%iobj,"gb1","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%gb1,ss%id_gb1, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "gb1"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_gb1
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GB2
  idchk = ss%id_gb2
  IF(ALLOCATED(ss%gb2).AND.hcheck(1,idchk)) THEN
     listname = "GB2_LIST"
     icomponent = ps_cnum_EQ
     units = "T^2"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "T^2"
     label = "edge value: gb2"

     plabel = "<B^2>"
     CALL ps_1d_prof_store(ss%iobj,"gb2","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%gb2,ss%id_gb2, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "gb2"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_gb2
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GB2I
  idchk = ss%id_gb2i
  IF(ALLOCATED(ss%gb2i).AND.hcheck(1,idchk)) THEN
     listname = "GB2I_LIST"
     icomponent = ps_cnum_EQ
     units = "T^-2"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "T^-2"
     label = "edge value: gb2i"

     plabel = "<1/B^2>"
     CALL ps_1d_prof_store(ss%iobj,"gb2i","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%gb2i,ss%id_gb2i, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "gb2i"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_gb2i
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GBR2
  idchk = ss%id_gbr2
  IF(ALLOCATED(ss%gbr2).AND.hcheck(1,idchk)) THEN
     listname = "GBR2_LIST"
     icomponent = ps_cnum_EQ
     units = "T*m^2"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "T*m^2"
     label = "edge value: gbr2"

     plabel = "<|B|*R^2>"
     CALL ps_1d_prof_store(ss%iobj,"gbr2","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%gbr2,ss%id_gbr2, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "gbr2"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_gbr2
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GNCFB2H
  idchk = ss%id_gncfb2h
  IF(ALLOCATED(ss%gncfb2h).AND.hcheck(1,idchk)) THEN
     listname = "GNCFB2H_LIST"
     icomponent = ps_cnum_EQ
     units = "T^-2"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "T^-2"
     label = "edge value: gncfb2h"

     plabel = "<(B^-2)*((1-H)^0.5-(1/3)*(1-H)^1.5)>; H=B/Bmax"
     CALL ps_1d_prof_store(ss%iobj,"gncfb2h","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%gncfb2h,ss%id_gncfb2h, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "gncfb2h"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_gncfb2h
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GNCFH
  idchk = ss%id_gncfh
  IF(ALLOCATED(ss%gncfh).AND.hcheck(1,idchk)) THEN
     listname = "GNCFH_LIST"
     icomponent = ps_cnum_EQ
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "edge value: gncfh"

     plabel = "<H^-2*(1-SQRT(1-H)*(1+H/2))>; H=B/Bmax"
     CALL ps_1d_prof_store(ss%iobj,"gncfh","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%gncfh,ss%id_gncfh, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "gncfh"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_gncfh
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GR1
  idchk = ss%id_gr1
  IF(ALLOCATED(ss%gr1).AND.hcheck(1,idchk)) THEN
     listname = "GR1_LIST"
     icomponent = ps_cnum_EQ
     units = "m"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m"
     label = "edge value: gr1"

     plabel = "<R>"
     CALL ps_1d_prof_store(ss%iobj,"gr1","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%gr1,ss%id_gr1, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "gr1"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_gr1
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GR2
  idchk = ss%id_gr2
  IF(ALLOCATED(ss%gr2).AND.hcheck(1,idchk)) THEN
     listname = "GR2_LIST"
     icomponent = ps_cnum_EQ
     units = "m^2"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2"
     label = "edge value: gr2"

     plabel = "<R^2>"
     CALL ps_1d_prof_store(ss%iobj,"gr2","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%gr2,ss%id_gr2, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "gr2"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_gr2
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GR2I
  idchk = ss%id_gr2i
  IF(ALLOCATED(ss%gr2i).AND.hcheck(1,idchk)) THEN
     listname = "GR2I_LIST"
     icomponent = ps_cnum_EQ
     units = "m^-2"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^-2"
     label = "edge value: gr2i"

     plabel = "<R^-2>"
     CALL ps_1d_prof_store(ss%iobj,"gr2i","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%gr2i,ss%id_gr2i, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "gr2i"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_gr2i
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GR2RHO2
  idchk = ss%id_gr2rho2
  IF(ALLOCATED(ss%gr2rho2).AND.hcheck(1,idchk)) THEN
     listname = "GR2RHO2_LIST"
     icomponent = ps_cnum_EQ
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "edge value: gr2rho2"

     plabel = "<R^2*|grad(rho)|^2>"
     CALL ps_1d_prof_store(ss%iobj,"gr2rho2","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%gr2rho2,ss%id_gr2rho2, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "gr2rho2"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_gr2rho2
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GR3I
  idchk = ss%id_gr3i
  IF(ALLOCATED(ss%gr3i).AND.hcheck(1,idchk)) THEN
     listname = "GR3I_LIST"
     icomponent = ps_cnum_EQ
     units = "m^-3"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^-3"
     label = "edge value: gr3i"

     plabel = "<R^-3>"
     CALL ps_1d_prof_store(ss%iobj,"gr3i","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%gr3i,ss%id_gr3i, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "gr3i"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_gr3i
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRHO1
  idchk = ss%id_grho1
  IF(ALLOCATED(ss%grho1).AND.hcheck(1,idchk)) THEN
     listname = "GRHO1_LIST"
     icomponent = ps_cnum_EQ
     units = "m^-1"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^-1"
     label = "edge value: grho1"

     plabel = "<|grad(rho)|>"
     CALL ps_1d_prof_store(ss%iobj,"grho1","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%grho1,ss%id_grho1, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "grho1"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_grho1
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRHO2
  idchk = ss%id_grho2
  IF(ALLOCATED(ss%grho2).AND.hcheck(1,idchk)) THEN
     listname = "GRHO2_LIST"
     icomponent = ps_cnum_EQ
     units = "m^-2"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^-2"
     label = "edge value: grho2"

     plabel = "<|grad(rho)|^2>"
     CALL ps_1d_prof_store(ss%iobj,"grho2","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%grho2,ss%id_grho2, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "grho2"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_grho2
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRHO2B2I
  idchk = ss%id_grho2b2i
  IF(ALLOCATED(ss%grho2b2i).AND.hcheck(1,idchk)) THEN
     listname = "GRHO2B2I_LIST"
     icomponent = ps_cnum_EQ
     units = "T^-2*m^-2"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "T^-2*m^-2"
     label = "edge value: grho2b2i"

     plabel = "<|grad(rho)|^2/B^2>"
     CALL ps_1d_prof_store(ss%iobj,"grho2b2i","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%grho2b2i,ss%id_grho2b2i, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "grho2b2i"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_grho2b2i
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRHO2R2I
  idchk = ss%id_grho2r2i
  IF(ALLOCATED(ss%grho2r2i).AND.hcheck(1,idchk)) THEN
     listname = "GRHO2R2I_LIST"
     icomponent = ps_cnum_EQ
     units = "m^-4"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^-4"
     label = "edge value: grho2r2i"

     plabel = "<|grad(rho)|^2/R^2>"
     CALL ps_1d_prof_store(ss%iobj,"grho2r2i","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%grho2r2i,ss%id_grho2r2i, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "grho2r2i"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_grho2r2i
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRHO2R3I
  idchk = ss%id_grho2r3i
  IF(ALLOCATED(ss%grho2r3i).AND.hcheck(1,idchk)) THEN
     listname = "GRHO2R3I_LIST"
     icomponent = ps_cnum_EQ
     units = "m^-5"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^-5"
     label = "edge value: grho2r3i"

     plabel = "<|grad(rho)|^2/R^3>"
     CALL ps_1d_prof_store(ss%iobj,"grho2r3i","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%grho2r3i,ss%id_grho2r3i, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "grho2r3i"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_grho2r3i
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRI
  idchk = ss%id_gri
  IF(ALLOCATED(ss%gri).AND.hcheck(1,idchk)) THEN
     listname = "GRI_LIST"
     icomponent = ps_cnum_EQ
     units = "m^-2"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^-2"
     label = "edge value: gri"

     plabel = "<1/R>"
     CALL ps_1d_prof_store(ss%iobj,"gri","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%gri,ss%id_gri, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "gri"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_gri
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GRIRHOI
  idchk = ss%id_grirhoi
  IF(ALLOCATED(ss%grirhoi).AND.hcheck(1,idchk)) THEN
     listname = "GRIRHOI_LIST"
     icomponent = ps_cnum_EQ
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "edge value: grirhoi"

     plabel = "<1/(R*|grad(rho)|)>"
     CALL ps_1d_prof_store(ss%iobj,"grirhoi","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%grirhoi,ss%id_grirhoi, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "grirhoi"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_grirhoi
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_DRBM
  idchk = ss%id_growthrate_drbm
  IF(ALLOCATED(ss%growthrate_drbm).AND.hcheck(1,idchk)) THEN
     listname = "GROWTHRATE_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     units = "1/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "1/sec"
     label = "simple avg: growthrate_drbm"

     plabel = "growthrate from DRBM model"
     CALL ps_1d_prof_store(ss%iobj,"growthrate_drbm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%growthrate_drbm,ss%id_growthrate_drbm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "growthrate_drbm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_growthrate_drbm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_ETG
  idchk = ss%id_growthrate_etg
  IF(ALLOCATED(ss%growthrate_etg).AND.hcheck(1,idchk)) THEN
     listname = "GROWTHRATE_ETG_LIST"
     icomponent = ps_cnum_ANOM
     units = "1/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "1/sec"
     label = "simple avg: growthrate_etg"

     plabel = "growthrate from ETG model"
     CALL ps_1d_prof_store(ss%iobj,"growthrate_etg","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%growthrate_etg,ss%id_growthrate_etg, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "growthrate_etg"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_growthrate_etg
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_GLF
  idchk = ss%id_growthrate_glf
  IF(ALLOCATED(ss%growthrate_glf).AND.hcheck(1,idchk)) THEN
     listname = "GROWTHRATE_GLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "GB"
     label = "simple avg: growthrate_glf"

     plabel = "growthrate from GLF23 model"
     CALL ps_1d_prof_store(ss%iobj,"growthrate_glf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%growthrate_glf,ss%id_growthrate_glf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "growthrate_glf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_growthrate_glf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_MAX_TGLF
  idchk = ss%id_growthrate_max_tglf
  IF(ALLOCATED(ss%growthrate_max_tglf).AND.hcheck(1,idchk)) THEN
     listname = "GROWTHRATE_MAX_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "GB"
     label = "simple avg: growthrate_max_tglf"

     plabel = "maximum growthrate from TGLF model"
     CALL ps_1d_prof_store(ss%iobj,"growthrate_max_tglf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%growthrate_max_tglf,ss%id_growthrate_max_tglf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "growthrate_max_tglf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_growthrate_max_tglf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_GROWTHRATE_MTM
  idchk = ss%id_growthrate_mtm
  IF(ALLOCATED(ss%growthrate_mtm).AND.hcheck(1,idchk)) THEN
     listname = "GROWTHRATE_MTM_LIST"
     icomponent = ps_cnum_ANOM
     units = "1/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "1/sec"
     label = "simple avg: growthrate_mtm"

     plabel = "growthrate from MTM model"
     CALL ps_1d_prof_store(ss%iobj,"growthrate_mtm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%growthrate_mtm,ss%id_growthrate_mtm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "growthrate_mtm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_growthrate_mtm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_G_EQ
  idchk = ss%id_g_eq
  IF(ALLOCATED(ss%g_eq).AND.hcheck(1,idchk)) THEN
     listname = "G_EQ_LIST"
     icomponent = ps_cnum_EQ
     units = "T*m"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "T*m"
     label = "edge value: g_eq"

     plabel = "equilibrium R*|B_phi|"
     CALL ps_1d_prof_store(ss%iobj,"g_eq","RHO_EQ", &
          units,icomponent,plabel,ps_spline,ps_bc_grad00, &
          ss%nrho_eq,0, &
          ss%g_eq,ss%id_g_eq, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "g_eq"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_g_eq
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_IOTA
  idchk = ss%id_iota
  IF(ALLOCATED(ss%iota).AND.hcheck(1,idchk)) THEN
     listname = "IOTA_LIST"
     icomponent = ps_cnum_PLASMA
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "simple avg: iota"

     plabel = "iota profile (1/q)"
     CALL ps_1d_prof_store(ss%iobj,"iota","RHO", &
          units,icomponent,plabel,ps_spline,ps_bc_grad00, &
          ss%nrho,0, &
          ss%iota,ss%id_iota, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "iota"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_iota
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_JDOTB
  idchk = ss%id_jdotb
  IF(ALLOCATED(ss%jdotb).AND.hcheck(1,idchk)) THEN
     listname = "JDOTB_LIST"
     icomponent = ps_cnum_EQ
     units = "A*T/m^2"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "A*T/m^2"
     label = "edge value: jdotb"

     plabel = "<J.B>"
     CALL ps_1d_prof_store(ss%iobj,"jdotb","RHO_EQ", &
          units,icomponent,plabel,ps_hermite,ps_bc_grad00, &
          ss%nrho_eq,0, &
          ss%jdotb,ss%id_jdotb, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "jdotb"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_jdotb
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_KYRHOS_DRBM
  idchk = ss%id_kyrhos_drbm
  IF(ALLOCATED(ss%kyrhos_drbm).AND.hcheck(1,idchk)) THEN
     listname = "KYRHOS_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "simple avg: kyrhos_drbm"

     plabel = "ky*rhos at max growth rate for DRBM model"
     CALL ps_1d_prof_store(ss%iobj,"kyrhos_drbm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%kyrhos_drbm,ss%id_kyrhos_drbm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "kyrhos_drbm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_kyrhos_drbm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_KYRHOS_ETG
  idchk = ss%id_kyrhos_etg
  IF(ALLOCATED(ss%kyrhos_etg).AND.hcheck(1,idchk)) THEN
     listname = "KYRHOS_ETG_LIST"
     icomponent = ps_cnum_ANOM
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "simple avg: kyrhos_etg"

     plabel = "ky*rhos at max growth rate for ETG model"
     CALL ps_1d_prof_store(ss%iobj,"kyrhos_etg","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%kyrhos_etg,ss%id_kyrhos_etg, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "kyrhos_etg"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_kyrhos_etg
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_KYRHOS_MTM
  idchk = ss%id_kyrhos_mtm
  IF(ALLOCATED(ss%kyrhos_mtm).AND.hcheck(1,idchk)) THEN
     listname = "KYRHOS_MTM_LIST"
     icomponent = ps_cnum_ANOM
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "simple avg: kyrhos_mtm"

     plabel = "ky*rhos at max growth rate for MTM model"
     CALL ps_1d_prof_store(ss%iobj,"kyrhos_mtm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%kyrhos_mtm,ss%id_kyrhos_mtm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "kyrhos_mtm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_kyrhos_mtm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_LPOL
  idchk = ss%id_Lpol
  IF(ALLOCATED(ss%Lpol).AND.hcheck(1,idchk)) THEN
     listname = "LPOL_LIST"
     icomponent = ps_cnum_EQ
     units = "m"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m"
     label = "edge value: Lpol"

     plabel = "poloidal path length"
     CALL ps_1d_prof_store(ss%iobj,"Lpol","RHO_EQ", &
          units,icomponent,plabel,ps_spline,ps_bc_dflt, &
          ss%nrho_eq,0, &
          ss%Lpol,ss%id_Lpol, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "Lpol"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_Lpol
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NGRADB2_AV
  idchk = ss%id_ngradb2_av
  IF(ALLOCATED(ss%ngradb2_av).AND.hcheck(1,idchk)) THEN
     listname = "NGRADB2_AV_LIST"
     icomponent = ps_cnum_EQ
     units = "T^2*m^-2"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "T^2*m^-2"
     label = "edge value: ngradb2_av"

     plabel = "<(B.grad(B)/|B|)^2>"
     CALL ps_1d_prof_store(ss%iobj,"ngradb2_av","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%ngradb2_av,ss%id_ngradb2_av, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "ngradb2_av"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_ngradb2_av
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NI
  idchk = ss%id_ni
  IF(ALLOCATED(ss%ni).AND.hcheck(1,idchk)) THEN
     listname = "NI_LIST"
     icomponent = ps_cnum_PLASMA
     units = "m^-3"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "#"
     label = "vol int: ni"

     plabel = "total thermal ion density"
     CALL ps_1d_prof_store(ss%iobj,"ni","RHO", &
          units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%ni,ss%id_ni, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "ni"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_ni
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NMODEL
  idchk = ss%id_nmodel
  IF(ALLOCATED(ss%nmodel).AND.hcheck(1,idchk)) THEN
     listname = "NMODEL_LIST"
     icomponent = ps_cnum_ANOM
     units = "m^-3"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "#"
     label = "vol int: nmodel"

     plabel = "target density profile"
     CALL ps_1d_prof_store(ss%iobj,"nmodel","RHO_ANOMTH", &
          units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
          ss%nrho_anomth-1,0, &
          ss%nmodel,ss%id_nmodel, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "nmodel"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_nmodel
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_NRW
  idchk = ss%id_nrw
  IF(ALLOCATED(ss%nrw).AND.hcheck(1,idchk)) THEN
     listname = "NRW_LIST"
     icomponent = ps_cnum_RUNAWAY
     units = "m^-3"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "#"
     label = "vol int: nrw"

     plabel = "runaway electron density"
     CALL ps_1d_prof_store(ss%iobj,"nrw","RHO_RW", &
          units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
          ss%nrho_rw-1,0, &
          ss%nrw,ss%id_nrw, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "nrw"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_nrw
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_OMEGAT
  idchk = ss%id_omegat
  IF(ALLOCATED(ss%omegat).AND.hcheck(1,idchk)) THEN
     listname = "OMEGAT_LIST"
     icomponent = ps_cnum_PLASMA
     units = "rad/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "rad/sec"
     label = "wght avg (NI): omegat"

     plabel = "ion toroidal angular velocity"
     CALL ps_1d_prof_store(ss%iobj,"omegat","RHO", &
          units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%omegat,ss%id_omegat, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "omegat"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_omegat
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_OMEGA_EXB
  idchk = ss%id_omega_exb
  IF(ALLOCATED(ss%omega_exb).AND.hcheck(1,idchk)) THEN
     listname = "OMEGA_EXB_LIST"
     icomponent = ps_cnum_ANOM
     units = "GB"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "GB"
     label = "simple avg: omega_exb"

     plabel = "ExB flow shear rate"
     CALL ps_1d_prof_store(ss%iobj,"omega_exb","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%omega_exb,ss%id_omega_exb, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "omega_exb"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_omega_exb
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PEECH
  idchk = ss%id_peech
  IF(ALLOCATED(ss%peech).AND.hcheck(1,idchk)) THEN
     listname = "PEECH_LIST"
     icomponent = ps_cnum_EC
     units = "W"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: peech"

     plabel = "electron heating by ECH"
     CALL ps_1d_prof_store(ss%iobj,"peech","RHO_ECRF", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_ecrf-1,0, &
          ss%peech,ss%id_peech, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "peech"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_peech
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PE_ANOM
  idchk = ss%id_pe_anom
  IF(ALLOCATED(ss%pe_anom).AND.hcheck(1,idchk)) THEN
     listname = "PE_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     units = "W"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: pe_anom"

     plabel = "anomalous electron heating"
     CALL ps_1d_prof_store(ss%iobj,"pe_anom","RHO_ANOMTH", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_anomth-1,0, &
          ss%pe_anom,ss%id_pe_anom, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "pe_anom"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_pe_anom
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PHIT
  idchk = ss%id_phit
  IF(ALLOCATED(ss%phit).AND.hcheck(1,idchk)) THEN
     listname = "PHIT_LIST"
     icomponent = ps_cnum_EQ
     units = "Wb"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "Wb"
     label = "edge value: phit"

     plabel = "toroidal flux vs. rho"
     CALL ps_1d_prof_store(ss%iobj,"phit","RHO_EQ", &
          units,icomponent,plabel,ps_spline,ps_bc_grad00, &
          ss%nrho_eq,0, &
          ss%phit,ss%id_phit, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "phit"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_phit
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PICTH
  idchk = ss%id_picth
  IF(ALLOCATED(ss%picth).AND.hcheck(1,idchk)) THEN
     listname = "PICTH_LIST"
     icomponent = ps_cnum_IC
     units = "W"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: picth"

     plabel = "direct thermal ion heating by ICRF"
     CALL ps_1d_prof_store(ss%iobj,"picth","RHO_ICRF", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_icrf-1,0, &
          ss%picth,ss%id_picth, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "picth"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_picth
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PI_ANOM
  idchk = ss%id_pi_anom
  IF(ALLOCATED(ss%pi_anom).AND.hcheck(1,idchk)) THEN
     listname = "PI_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     units = "W"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: pi_anom"

     plabel = "anomalous ion heating"
     CALL ps_1d_prof_store(ss%iobj,"pi_anom","RHO_ANOMTH", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_anomth-1,0, &
          ss%pi_anom,ss%id_pi_anom, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "pi_anom"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_pi_anom
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_POHME
  idchk = ss%id_pohme
  IF(ALLOCATED(ss%pohme).AND.hcheck(1,idchk)) THEN
     listname = "POHME_LIST"
     icomponent = ps_cnum_PLASMA
     units = "W"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: pohme"

     plabel = "Ohmic heating of electrons"
     CALL ps_1d_prof_store(ss%iobj,"pohme","RHO", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%pohme,ss%id_pohme, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "pohme"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_pohme
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PSIPOL
  idchk = ss%id_psipol
  IF(ALLOCATED(ss%psipol).AND.hcheck(1,idchk)) THEN
     listname = "PSIPOL_LIST"
     icomponent = ps_cnum_EQ
     units = "Wb/rad"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "Wb/rad"
     label = "edge value: psipol"

     plabel = "poloidal flux vs. rho"
     CALL ps_1d_prof_store(ss%iobj,"psipol","RHO_EQ", &
          units,icomponent,plabel,ps_spline,ps_bc_grad00, &
          ss%nrho_eq,0, &
          ss%psipol,ss%id_psipol, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "psipol"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_psipol
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PSIRZ
  idchk = ss%id_PsiRZ
  IF(ALLOCATED(ss%PsiRZ).AND.hcheck(1,idchk)) THEN
     listname = "PSIRZ_LIST"
     icomponent = ps_cnum_EQ
     units = "Wb/rad"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "Wb/rad"
     label = "simple avg: PsiRZ"

     plabel = "Psi(R,Z)"
     CALL ps_2d_prof_store(ss%iobj,"PsiRZ","R_GRID","Z_GRID", &
          units,icomponent,plabel,ps_spline,ps_bc_dflt, &
          ss%nr,ss%nz,0,0, &
          ss%PsiRZ,ss%id_PsiRZ, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "PsiRZ"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_PsiRZ
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_PSMOM_ERRCK
  idchk = ss%id_psmom_errck
  IF(ALLOCATED(ss%psmom_errck).AND.hcheck(1,idchk)) THEN
     listname = "PSMOM_ERRCK_LIST"
     icomponent = ps_cnum_EQ
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "edge value: psmom_errck"

     plabel = "PS moments sum convergence"
     CALL ps_1d_prof_store(ss%iobj,"psmom_errck","RHO_EQ_GEO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_eq_geo,0, &
          ss%psmom_errck,ss%id_psmom_errck, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "psmom_errck"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_psmom_errck
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_P_EQ
  idchk = ss%id_P_eq
  IF(ALLOCATED(ss%P_eq).AND.hcheck(1,idchk)) THEN
     listname = "P_EQ_LIST"
     icomponent = ps_cnum_EQ
     units = "Pa"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "Pa"
     label = "vol avg: P_eq"

     plabel = "equilibrium scalar pressure"
     CALL ps_1d_prof_store(ss%iobj,"P_eq","RHO_EQ", &
          units,icomponent,plabel,ps_spline,ps_bc_grad00, &
          ss%nrho_eq,0, &
          ss%P_eq,ss%id_P_eq, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "P_eq"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_P_eq
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_QIE
  idchk = ss%id_qie
  IF(ALLOCATED(ss%qie).AND.hcheck(1,idchk)) THEN
     listname = "QIE_LIST"
     icomponent = ps_cnum_PLASMA
     units = "W"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "W"
     label = "vol int: qie"

     plabel = "Ion->Electron heat coupling"
     CALL ps_1d_prof_store(ss%iobj,"qie","RHO", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%qie,ss%id_qie, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "qie"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_qie
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_Q_EQ
  idchk = ss%id_q_eq
  IF(ALLOCATED(ss%q_eq).AND.hcheck(1,idchk)) THEN
     listname = "Q_EQ_LIST"
     icomponent = ps_cnum_EQ
     units = "-"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "-"
     label = "edge value: q_eq"

     plabel = "equilibrium q profile"
     CALL ps_1d_prof_store(ss%iobj,"q_eq","RHO_EQ", &
          units,icomponent,plabel,ps_spline,ps_bc_dflt, &
          ss%nrho_eq,0, &
          ss%q_eq,ss%id_q_eq, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "q_eq"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_q_eq
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_R0_MOMEQ
  ahash(2) = ps_phash_DR0_MOMEQ_DRHO
  idchk = ss%id_R0_momeq
  IF(ALLOCATED(ss%R0_momeq).AND.hcheck(2,idchk)) THEN
     listname = "R0_MOMEQ_LIST"
     icomponent = ps_cnum_EQ
     units = "m"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m"
     label = "edge value: R0_momeq"

     plabel = "R0 of flux surface"
     CALL ps_1dx_prof_store(ss%iobj,"R0_momeq","RHO_EQ", &
          units,icomponent,plabel, &
          ss%nrho_eq,0, &
          ss%R0_momeq,ss%dR0_momeq_dRHO,ss%id_R0_momeq, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "R0_momeq"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_R0_momeq
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_RES_TQ
  idchk = ss%id_res_tq
  IF(ALLOCATED(ss%res_tq).AND.hcheck(1,idchk)) THEN
     listname = "RES_TQ_LIST"
     icomponent = ps_cnum_PLASMA
     units = "Norm"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "Norm"
     label = "vol int: res_tq"

     plabel = "rediduals for angular momentum transport eqn"
     CALL ps_1d_prof_store(ss%iobj,"res_tq","RHO", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%res_tq,ss%id_res_tq, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "res_tq"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_res_tq
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_R_GEO
  ahash(2) = ps_phash_DR_GEO_DRHO
  ahash(3) = ps_phash_DR_GEO_DTH
  ahash(4) = ps_phash_D2R_GEO_DRHODTH
  idchk = ss%id_R_geo
  IF(ALLOCATED(ss%R_geo).AND.hcheck(4,idchk)) THEN
     listname = "R_GEO_LIST"
     icomponent = ps_cnum_EQ
     units = "m"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m"
     label = "simple avg: R_geo"

     plabel = "flux surfaces R(rho,theta)"
     CALL ps_2dx_prof_store(ss%iobj,"R_geo","RHO_EQ","TH_EQ", &
          units,icomponent,plabel, &
          ss%nrho_eq,ss%nth_eq,0,ps_ccw_select, &
          ss%R_geo,ss%dR_geo_dRHO,ss%dR_geo_dTH,ss%d2R_geo_dRHOdTH,ss%id_R_geo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "R_geo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_R_geo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_S0RECO_E
  idchk = ss%id_s0reco_e
  IF(ALLOCATED(ss%s0reco_e).AND.hcheck(1,idchk)) THEN
     listname = "S0RECO_E_LIST"
     icomponent = ps_cnum_GAS
     units = "#/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "#/sec"
     label = "vol int: s0reco_e"

     plabel = "net electron source associated with recombination:"
     CALL ps_1d_prof_store(ss%iobj,"s0reco_e","RHO_GAS", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_gas-1,0, &
          ss%s0reco_e,ss%id_s0reco_e, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "s0reco_e"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_s0reco_e
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SBEDEP
  idchk = ss%id_sbedep
  IF(ALLOCATED(ss%sbedep).AND.hcheck(1,idchk)) THEN
     listname = "SBEDEP_LIST"
     icomponent = ps_cnum_NBI
     units = "#/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "#/sec"
     label = "vol int: sbedep"

     plabel = "beam deposition cold electron sce"
     CALL ps_1d_prof_store(ss%iobj,"sbedep","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%sbedep,ss%id_sbedep, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "sbedep"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_sbedep
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SBEHALO
  idchk = ss%id_sbehalo
  IF(ALLOCATED(ss%sbehalo).AND.hcheck(1,idchk)) THEN
     listname = "SBEHALO_LIST"
     icomponent = ps_cnum_NBI
     units = "#/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "#/sec"
     label = "vol int: sbehalo"

     plabel = "cold electron sce due to"
     CALL ps_1d_prof_store(ss%iobj,"sbehalo","RHO_NBI", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_nbi-1,0, &
          ss%sbehalo,ss%id_sbehalo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "sbehalo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_sbehalo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_SURF
  idchk = ss%id_surf
  IF(ALLOCATED(ss%surf).AND.hcheck(1,idchk)) THEN
     listname = "SURF_LIST"
     icomponent = ps_cnum_EQ
     units = "m^2"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m^2"
     label = "edge value: surf"

     plabel = "area of flux surfaces"
     CALL ps_1d_prof_store(ss%iobj,"surf","RHO_EQ", &
          units,icomponent,plabel,ps_spline,ps_bc_dflt, &
          ss%nrho_eq,0, &
          ss%surf,ss%id_surf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "surf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_surf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TI
  idchk = ss%id_Ti
  IF(ALLOCATED(ss%Ti).AND.hcheck(1,idchk)) THEN
     listname = "TI_LIST"
     icomponent = ps_cnum_PLASMA
     units = "keV"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "keV"
     label = "wght avg (NI): Ti"

     plabel = "ion temperature"
     CALL ps_1d_prof_store(ss%iobj,"Ti","RHO", &
          units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%Ti,ss%id_Ti, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "Ti"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_Ti
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TMODEL
  idchk = ss%id_tmodel
  IF(ALLOCATED(ss%tmodel).AND.hcheck(1,idchk)) THEN
     listname = "TMODEL_LIST"
     icomponent = ps_cnum_ANOM
     units = "keV"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "keV"
     label = "wght avg (NMODEL): tmodel"

     plabel = "target temperature profile"
     CALL ps_1d_prof_store(ss%iobj,"tmodel","RHO_ANOMTH", &
          units,icomponent,plabel,ps_stepfun,ps_bc_dflt, &
          ss%nrho_anomth-1,0, &
          ss%tmodel,ss%id_tmodel, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "tmodel"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_tmodel
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQ_ANOM
  idchk = ss%id_tq_anom
  IF(ALLOCATED(ss%tq_anom).AND.hcheck(1,idchk)) THEN
     listname = "TQ_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     units = "Nt*m"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "Nt*m"
     label = "vol int: tq_anom"

     plabel = "anomalous torque"
     CALL ps_1d_prof_store(ss%iobj,"tq_anom","RHO_ANOMTH", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho_anomth-1,0, &
          ss%tq_anom,ss%id_tq_anom, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "tq_anom"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_tq_anom
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_TQ_TRANS
  idchk = ss%id_tq_trans
  IF(ALLOCATED(ss%tq_trans).AND.hcheck(1,idchk)) THEN
     listname = "TQ_TRANS_LIST"
     icomponent = ps_cnum_PLASMA
     units = "Nt*m"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "Nt*m"
     label = "vol int: tq_trans"

     plabel = "angular momentum transport (loss)"
     CALL ps_1d_prof_store(ss%iobj,"tq_trans","RHO", &
          units,icomponent,plabel,ps_stepdv,ps_bc_dflt, &
          ss%nrho-1,0, &
          ss%tq_trans,ss%id_tq_trans, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "tq_trans"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_tq_trans
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNE_NCLASS
  idchk = ss%id_velne_nclass
  IF(ALLOCATED(ss%velne_nclass).AND.hcheck(1,idchk)) THEN
     listname = "VELNE_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velne_nclass"

     plabel = "electron-density-equations, advective velocity term from NCLASS model"
     CALL ps_1d_prof_store(ss%iobj,"velne_nclass","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velne_nclass,ss%id_velne_nclass, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velne_nclass"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velne_nclass
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNH_NCLASS
  idchk = ss%id_velnh_nclass
  IF(ALLOCATED(ss%velnh_nclass).AND.hcheck(1,idchk)) THEN
     listname = "VELNH_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velnh_nclass"

     plabel = "main-hydrogenic-density-equations, advective velocity term from NCLASS model"
     CALL ps_1d_prof_store(ss%iobj,"velnh_nclass","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velnh_nclass,ss%id_velnh_nclass, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velnh_nclass"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velnh_nclass
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNI_DRBM
  idchk = ss%id_velni_drbm
  IF(ALLOCATED(ss%velni_drbm).AND.hcheck(1,idchk)) THEN
     listname = "VELNI_DRBM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/s"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/s"
     label = "simple avg: velni_drbm"

     plabel = "ion convective velocity"
     CALL ps_1d_prof_store(ss%iobj,"velni_drbm","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velni_drbm,ss%id_velni_drbm, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velni_drbm"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velni_drbm
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELNX_NCLASS
  idchk = ss%id_velnx_nclass
  IF(ALLOCATED(ss%velnx_nclass).AND.hcheck(1,idchk)) THEN
     listname = "VELNX_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velnx_nclass"

     plabel = "main-impurity-density-equations, advective velocity term from NCLASS model"
     CALL ps_1d_prof_store(ss%iobj,"velnx_nclass","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velnx_nclass,ss%id_velnx_nclass, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velnx_nclass"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velnx_nclass
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELPPHI_ANOM
  idchk = ss%id_velpphi_anom
  IF(ALLOCATED(ss%velpphi_anom).AND.hcheck(1,idchk)) THEN
     listname = "VELPPHI_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velpphi_anom"

     plabel = "momentum-equation, advective velocity term"
     CALL ps_1d_prof_store(ss%iobj,"velpphi_anom","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velpphi_anom,ss%id_velpphi_anom, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velpphi_anom"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velpphi_anom
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELPPHI_GLF
  idchk = ss%id_velpphi_glf
  IF(ALLOCATED(ss%velpphi_glf).AND.hcheck(1,idchk)) THEN
     listname = "VELPPHI_GLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velpphi_glf"

     plabel = "momentum-equation, advective velocity term from GLF23 model"
     CALL ps_1d_prof_store(ss%iobj,"velpphi_glf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velpphi_glf,ss%id_velpphi_glf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velpphi_glf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velpphi_glf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELPPHI_GTCNEO
  idchk = ss%id_velpphi_gtcneo
  IF(ALLOCATED(ss%velpphi_gtcneo).AND.hcheck(1,idchk)) THEN
     listname = "VELPPHI_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velpphi_gtcneo"

     plabel = "momentum-equation, advective velocity term from GTC_NEO model"
     CALL ps_1d_prof_store(ss%iobj,"velpphi_gtcneo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velpphi_gtcneo,ss%id_velpphi_gtcneo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velpphi_gtcneo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velpphi_gtcneo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELPPHI_MMM71
  idchk = ss%id_velpphi_mmm71
  IF(ALLOCATED(ss%velpphi_mmm71).AND.hcheck(1,idchk)) THEN
     listname = "VELPPHI_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velpphi_mmm71"

     plabel = "momentum-equation, advective velocity term from MMM71 model"
     CALL ps_1d_prof_store(ss%iobj,"velpphi_mmm71","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velpphi_mmm71,ss%id_velpphi_mmm71, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velpphi_mmm71"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velpphi_mmm71
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELPPHI_NEO
  idchk = ss%id_velpphi_neo
  IF(ALLOCATED(ss%velpphi_neo).AND.hcheck(1,idchk)) THEN
     listname = "VELPPHI_NEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velpphi_neo"

     plabel = "momentum-equation, advective velocity term from NEO model"
     CALL ps_1d_prof_store(ss%iobj,"velpphi_neo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velpphi_neo,ss%id_velpphi_neo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velpphi_neo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velpphi_neo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELPPHI_TGLF
  idchk = ss%id_velpphi_tglf
  IF(ALLOCATED(ss%velpphi_tglf).AND.hcheck(1,idchk)) THEN
     listname = "VELPPHI_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velpphi_tglf"

     plabel = "momentum-equation, advective velocity term from TGLF model"
     CALL ps_1d_prof_store(ss%iobj,"velpphi_tglf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velpphi_tglf,ss%id_velpphi_tglf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velpphi_tglf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velpphi_tglf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_ANOM
  idchk = ss%id_velte_anom
  IF(ALLOCATED(ss%velte_anom).AND.hcheck(1,idchk)) THEN
     listname = "VELTE_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velte_anom"

     plabel = "Te-equation, advective velocity term"
     CALL ps_1d_prof_store(ss%iobj,"velte_anom","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velte_anom,ss%id_velte_anom, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velte_anom"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velte_anom
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_GLF
  idchk = ss%id_velte_glf
  IF(ALLOCATED(ss%velte_glf).AND.hcheck(1,idchk)) THEN
     listname = "VELTE_GLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velte_glf"

     plabel = "Te-equation, advective velocity term from GLF23 model"
     CALL ps_1d_prof_store(ss%iobj,"velte_glf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velte_glf,ss%id_velte_glf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velte_glf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velte_glf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_GTCNEO
  idchk = ss%id_velte_gtcneo
  IF(ALLOCATED(ss%velte_gtcneo).AND.hcheck(1,idchk)) THEN
     listname = "VELTE_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velte_gtcneo"

     plabel = "Te-equation, advective velocity term from GTC_NEO model"
     CALL ps_1d_prof_store(ss%iobj,"velte_gtcneo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velte_gtcneo,ss%id_velte_gtcneo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velte_gtcneo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velte_gtcneo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_MMM71
  idchk = ss%id_velte_mmm71
  IF(ALLOCATED(ss%velte_mmm71).AND.hcheck(1,idchk)) THEN
     listname = "VELTE_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velte_mmm71"

     plabel = "Te-equation, advective velocity term from MMM71 model"
     CALL ps_1d_prof_store(ss%iobj,"velte_mmm71","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velte_mmm71,ss%id_velte_mmm71, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velte_mmm71"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velte_mmm71
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_NCLASS
  idchk = ss%id_velte_nclass
  IF(ALLOCATED(ss%velte_nclass).AND.hcheck(1,idchk)) THEN
     listname = "VELTE_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velte_nclass"

     plabel = "Te-equation, advective velocity term from NCLASS model"
     CALL ps_1d_prof_store(ss%iobj,"velte_nclass","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velte_nclass,ss%id_velte_nclass, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velte_nclass"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velte_nclass
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_NEO
  idchk = ss%id_velte_neo
  IF(ALLOCATED(ss%velte_neo).AND.hcheck(1,idchk)) THEN
     listname = "VELTE_NEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velte_neo"

     plabel = "Te-equation, advective velocity term from NEO model"
     CALL ps_1d_prof_store(ss%iobj,"velte_neo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velte_neo,ss%id_velte_neo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velte_neo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velte_neo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_PALEO
  idchk = ss%id_velte_paleo
  IF(ALLOCATED(ss%velte_paleo).AND.hcheck(1,idchk)) THEN
     listname = "VELTE_PALEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velte_paleo"

     plabel = "Te-equation, convection term from PALEO model"
     CALL ps_1d_prof_store(ss%iobj,"velte_paleo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velte_paleo,ss%id_velte_paleo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velte_paleo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velte_paleo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTE_TGLF
  idchk = ss%id_velte_tglf
  IF(ALLOCATED(ss%velte_tglf).AND.hcheck(1,idchk)) THEN
     listname = "VELTE_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velte_tglf"

     plabel = "Te-equation, advective velocity term from TGLF model"
     CALL ps_1d_prof_store(ss%iobj,"velte_tglf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velte_tglf,ss%id_velte_tglf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velte_tglf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velte_tglf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_ANOM
  idchk = ss%id_velti_anom
  IF(ALLOCATED(ss%velti_anom).AND.hcheck(1,idchk)) THEN
     listname = "VELTI_ANOM_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velti_anom"

     plabel = "Ti-equation, advective velocity term"
     CALL ps_1d_prof_store(ss%iobj,"velti_anom","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velti_anom,ss%id_velti_anom, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velti_anom"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velti_anom
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_GLF
  idchk = ss%id_velti_glf
  IF(ALLOCATED(ss%velti_glf).AND.hcheck(1,idchk)) THEN
     listname = "VELTI_GLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velti_glf"

     plabel = "Ti-equation, advective velocity term from GLF23 model"
     CALL ps_1d_prof_store(ss%iobj,"velti_glf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velti_glf,ss%id_velti_glf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velti_glf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velti_glf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_GTCNEO
  idchk = ss%id_velti_gtcneo
  IF(ALLOCATED(ss%velti_gtcneo).AND.hcheck(1,idchk)) THEN
     listname = "VELTI_GTCNEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velti_gtcneo"

     plabel = "Ti-equation, advective velocity term from GTC_NEO model"
     CALL ps_1d_prof_store(ss%iobj,"velti_gtcneo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velti_gtcneo,ss%id_velti_gtcneo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velti_gtcneo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velti_gtcneo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_MMM71
  idchk = ss%id_velti_mmm71
  IF(ALLOCATED(ss%velti_mmm71).AND.hcheck(1,idchk)) THEN
     listname = "VELTI_MMM71_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velti_mmm71"

     plabel = "Ti-equation, advective velocity term from MMM71 model"
     CALL ps_1d_prof_store(ss%iobj,"velti_mmm71","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velti_mmm71,ss%id_velti_mmm71, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velti_mmm71"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velti_mmm71
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_NCLASS
  idchk = ss%id_velti_nclass
  IF(ALLOCATED(ss%velti_nclass).AND.hcheck(1,idchk)) THEN
     listname = "VELTI_NCLASS_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velti_nclass"

     plabel = "Ti-equation, advective velocity term from NCLASS model"
     CALL ps_1d_prof_store(ss%iobj,"velti_nclass","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velti_nclass,ss%id_velti_nclass, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velti_nclass"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velti_nclass
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_NEO
  idchk = ss%id_velti_neo
  IF(ALLOCATED(ss%velti_neo).AND.hcheck(1,idchk)) THEN
     listname = "VELTI_NEO_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velti_neo"

     plabel = "Ti-equation, advective velocity term from NEO model"
     CALL ps_1d_prof_store(ss%iobj,"velti_neo","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velti_neo,ss%id_velti_neo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velti_neo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velti_neo
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_VELTI_TGLF
  idchk = ss%id_velti_tglf
  IF(ALLOCATED(ss%velti_tglf).AND.hcheck(1,idchk)) THEN
     listname = "VELTI_TGLF_LIST"
     icomponent = ps_cnum_ANOM
     units = "m/sec"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m/sec"
     label = "simple avg: velti_tglf"

     plabel = "Ti-equation, advective velocity term from TGLF model"
     CALL ps_1d_prof_store(ss%iobj,"velti_tglf","RHO_ANOMTH", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho_anomth,0, &
          ss%velti_tglf,ss%id_velti_tglf, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "velti_tglf"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_velti_tglf
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_V_LOOP
  idchk = ss%id_V_loop
  IF(ALLOCATED(ss%V_loop).AND.hcheck(1,idchk)) THEN
     listname = "V_LOOP_LIST"
     icomponent = ps_cnum_PLASMA
     units = "Volts"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "Volts"
     label = "simple avg: V_loop"

     plabel = "loop voltage"
     CALL ps_1d_prof_store(ss%iobj,"V_loop","RHO", &
          units,icomponent,plabel,ps_pclin,ps_bc_dflt, &
          ss%nrho,0, &
          ss%V_loop,ss%id_V_loop, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "V_loop"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_V_loop
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_Z0_MOMEQ
  ahash(2) = ps_phash_DZ0_MOMEQ_DRHO
  idchk = ss%id_Z0_momeq
  IF(ALLOCATED(ss%Z0_momeq).AND.hcheck(2,idchk)) THEN
     listname = "Z0_MOMEQ_LIST"
     icomponent = ps_cnum_EQ
     units = "m"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m"
     label = "edge value: Z0_momeq"

     plabel = "Z0 of flux surface"
     CALL ps_1dx_prof_store(ss%iobj,"Z0_momeq","RHO_EQ", &
          units,icomponent,plabel, &
          ss%nrho_eq,0, &
          ss%Z0_momeq,ss%dZ0_momeq_dRHO,ss%id_Z0_momeq, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "Z0_momeq"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_Z0_momeq
     item_c256(1) = plabel

     CALL ps_list_store("STATE_PROFILES", ss%iobj, &
          listname,icomponent,list_units,label, &
          nitems,item_names,item_ivals,item_r8vals, &
          item_c256,imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_phash_Z_GEO
  ahash(2) = ps_phash_DZ_GEO_DRHO
  ahash(3) = ps_phash_DZ_GEO_DTH
  ahash(4) = ps_phash_D2Z_GEO_DRHODTH
  idchk = ss%id_Z_geo
  IF(ALLOCATED(ss%Z_geo).AND.hcheck(4,idchk)) THEN
     listname = "Z_GEO_LIST"
     icomponent = ps_cnum_EQ
     units = "m"
     nitems = 1
     ! for assoc. scalar list:
     list_units = "m"
     label = "simple avg: Z_geo"

     plabel = "flux surfaces Z(rho,theta)"
     CALL ps_2dx_prof_store(ss%iobj,"Z_geo","RHO_EQ","TH_EQ", &
          units,icomponent,plabel, &
          ss%nrho_eq,ss%nth_eq,0,ps_ccw_select, &
          ss%Z_geo,ss%dZ_geo_dRHO,ss%dZ_geo_dTH,ss%d2Z_geo_dRHOdTH,ss%id_Z_geo, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
     item_names(1) = "Z_geo"
     item_r8vals(1) = 0.0_rspec
     item_ivals(1) = ss%id_Z_geo
     item_c256(1) = plabel

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

END SUBROUTINE plasma_state_xload_psingl
