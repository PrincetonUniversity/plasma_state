! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_empty_alloc_4(ss,iout,ierr)


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

  isize = (ss%nrho_nbi-1)*(ss%nspec_beam)
  IF(ALLOCATED(ss%nbeami).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nbeami)
  ENDIF
  IF(ALLOCATED(ss%nbeami)) then
     jsize = SIZE(ss%nbeami)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nbeami already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nbeami(ss%nrho_nbi-1,ss%nspec_beam))
     ss%nbeami = 0.0_rspec
     ALLOCATE(ss%id_nbeami(ss%nspec_beam))
     ss%id_nbeami = 0
  ENDIF

  isize = (ss%nspec_beam)
  IF(ALLOCATED(ss%nbeami_bdy).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nbeami_bdy)
  ENDIF
  IF(ALLOCATED(ss%nbeami_bdy)) then
     jsize = SIZE(ss%nbeami_bdy)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nbeami_bdy already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nbeami_bdy(ss%nspec_beam))
     ss%nbeami_bdy = 0.0_rspec
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%nbion).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nbion)
  ENDIF
  IF(ALLOCATED(ss%nbion)) then
     jsize = SIZE(ss%nbion)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nbion already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nbion(ss%nbeam))
     ss%nbion = " "
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%nbion_trace).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nbion_trace)
  ENDIF
  IF(ALLOCATED(ss%nbion_trace)) then
     jsize = SIZE(ss%nbion_trace)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nbion_trace already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nbion_trace(ss%nbeam))
     ss%nbion_trace = " "
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%nbi_src_name).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nbi_src_name)
  ENDIF
  IF(ALLOCATED(ss%nbi_src_name)) then
     jsize = SIZE(ss%nbi_src_name)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nbi_src_name already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nbi_src_name(ss%nbeam))
     ss%nbi_src_name = " "
  ENDIF

  isize = (ss%nbeam)
  IF(ALLOCATED(ss%nbshape).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nbshape)
  ENDIF
  IF(ALLOCATED(ss%nbshape)) then
     jsize = SIZE(ss%nbshape)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nbshape already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nbshape(ss%nbeam))
     ss%nbshape = " "
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%neflux_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%neflux_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%neflux_ky_tglf)) then
     jsize = SIZE(ss%neflux_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: neflux_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%neflux_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%neflux_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_neflux_ky_tglf(ss%nky_anomth))
     ss%id_neflux_ky_tglf = 0
  ENDIF

  isize = (ss%nnewcomb)
  IF(ALLOCATED(ss%Newcomb_code).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Newcomb_code)
  ENDIF
  IF(ALLOCATED(ss%Newcomb_code)) then
     jsize = SIZE(ss%Newcomb_code)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Newcomb_code already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Newcomb_code(ss%nnewcomb))
     ss%Newcomb_code = " "
  ENDIF

  isize = (ss%nnewcomb)
  IF(ALLOCATED(ss%Newcomb_mapper).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Newcomb_mapper)
  ENDIF
  IF(ALLOCATED(ss%Newcomb_mapper)) then
     jsize = SIZE(ss%Newcomb_mapper)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Newcomb_mapper already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Newcomb_mapper(ss%nnewcomb))
     ss%Newcomb_mapper = " "
  ENDIF

  isize = (ss%mmodes)*(ss%nnewcomb)
  IF(ALLOCATED(ss%Newcomb_stable).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Newcomb_stable)
  ENDIF
  IF(ALLOCATED(ss%Newcomb_stable)) then
     jsize = SIZE(ss%Newcomb_stable)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Newcomb_stable already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Newcomb_stable(ss%mmodes,ss%nnewcomb))
     ss%Newcomb_stable = 0
  ENDIF

  isize = (ss%nnewcomb)
  IF(ALLOCATED(ss%Newcomb_status).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%Newcomb_status)
  ENDIF
  IF(ALLOCATED(ss%Newcomb_status)) then
     jsize = SIZE(ss%Newcomb_status)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: Newcomb_status already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%Newcomb_status(ss%nnewcomb))
     ss%Newcomb_status = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%ne_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ne_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%ne_ky_tglf)) then
     jsize = SIZE(ss%ne_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ne_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ne_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%ne_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_ne_ky_tglf(ss%nky_anomth))
     ss%id_ne_ky_tglf = 0
  ENDIF

  isize = (ss%nrho_fus-1)*(ss%nspec_fusion)
  IF(ALLOCATED(ss%nfusi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nfusi)
  ENDIF
  IF(ALLOCATED(ss%nfusi)) then
     jsize = SIZE(ss%nfusi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nfusi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nfusi(ss%nrho_fus-1,ss%nspec_fusion))
     ss%nfusi = 0.0_rspec
     ALLOCATE(ss%id_nfusi(ss%nspec_fusion))
     ss%id_nfusi = 0
  ENDIF

  isize = (ss%nspec_fusion)
  IF(ALLOCATED(ss%nfusi_bdy).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nfusi_bdy)
  ENDIF
  IF(ALLOCATED(ss%nfusi_bdy)) then
     jsize = SIZE(ss%nfusi_bdy)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nfusi_bdy already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nfusi_bdy(ss%nspec_fusion))
     ss%nfusi_bdy = 0.0_rspec
  ENDIF

  isize = (ss%nrho_eq_geo)
  IF(ALLOCATED(ss%ngradb2_av).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ngradb2_av)
  ENDIF
  IF(ALLOCATED(ss%ngradb2_av)) then
     jsize = SIZE(ss%ngradb2_av)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ngradb2_av already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ngradb2_av(ss%nrho_eq_geo))
     ss%ngradb2_av = 0.0_rspec
  ENDIF

  isize = (ss%nrho-1)
  IF(ALLOCATED(ss%ni).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ni)
  ENDIF
  IF(ALLOCATED(ss%ni)) then
     jsize = SIZE(ss%ni)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ni already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ni(ss%nrho-1))
     ss%ni = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%niflux_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%niflux_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%niflux_ky_tglf)) then
     jsize = SIZE(ss%niflux_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: niflux_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%niflux_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%niflux_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_niflux_ky_tglf(ss%nky_anomth))
     ss%id_niflux_ky_tglf = 0
  ENDIF

  isize = (ss%nrho_anomth)*(ss%nky_anomth)
  IF(ALLOCATED(ss%ni_ky_tglf).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ni_ky_tglf)
  ENDIF
  IF(ALLOCATED(ss%ni_ky_tglf)) then
     jsize = SIZE(ss%ni_ky_tglf)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ni_ky_tglf already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ni_ky_tglf(ss%nrho_anomth,ss%nky_anomth))
     ss%ni_ky_tglf = 0.0_rspec
     ALLOCATE(ss%id_ni_ky_tglf(ss%nky_anomth))
     ss%id_ni_ky_tglf = 0
  ENDIF

  isize = (ss%nrho_icrf-1)*(ss%nspec_rfmin)
  IF(ALLOCATED(ss%nmini).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nmini)
  ENDIF
  IF(ALLOCATED(ss%nmini)) then
     jsize = SIZE(ss%nmini)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nmini already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nmini(ss%nrho_icrf-1,ss%nspec_rfmin))
     ss%nmini = 0.0_rspec
     ALLOCATE(ss%id_nmini(ss%nspec_rfmin))
     ss%id_nmini = 0
  ENDIF

  isize = (ss%nspec_rfmin)
  IF(ALLOCATED(ss%nmini_bdy).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nmini_bdy)
  ENDIF
  IF(ALLOCATED(ss%nmini_bdy)) then
     jsize = SIZE(ss%nmini_bdy)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nmini_bdy already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nmini_bdy(ss%nspec_rfmin))
     ss%nmini_bdy = 0.0_rspec
  ENDIF

  isize = (ss%nrho_anomth-1)
  IF(ALLOCATED(ss%nmodel).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nmodel)
  ENDIF
  IF(ALLOCATED(ss%nmodel)) then
     jsize = SIZE(ss%nmodel)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nmodel already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nmodel(ss%nrho_anomth-1))
     ss%nmodel = 0.0_rspec
  ENDIF

  isize = (ss%max_nspectrumlh_src)*(ss%nlhrf_src)
  IF(ALLOCATED(ss%nnkparlh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nnkparlh)
  ENDIF
  IF(ALLOCATED(ss%nnkparlh)) then
     jsize = SIZE(ss%nnkparlh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nnkparlh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nnkparlh(ss%max_nspectrumlh_src,ss%nlhrf_src))
     ss%nnkparlh = 0
  ENDIF

  isize = (ss%max_nspectrumlh_src)*(ss%nlhrf_src)
  IF(ALLOCATED(ss%nnkpollh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nnkpollh)
  ENDIF
  IF(ALLOCATED(ss%nnkpollh)) then
     jsize = SIZE(ss%nnkpollh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nnkpollh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nnkpollh(ss%max_nspectrumlh_src,ss%nlhrf_src))
     ss%nnkpollh = 0
  ENDIF

  isize = (ss%max_nspectrumlh_src)*(ss%nlhrf_src)
  IF(ALLOCATED(ss%nnktorlh).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nnktorlh)
  ENDIF
  IF(ALLOCATED(ss%nnktorlh)) then
     jsize = SIZE(ss%nnktorlh)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nnktorlh already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nnktorlh(ss%max_nspectrumlh_src,ss%nlhrf_src))
     ss%nnktorlh = 0
  ENDIF

  isize = (ss%max_num_nphi)*(ss%nicrf_src)
  IF(ALLOCATED(ss%nphi).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nphi)
  ENDIF
  IF(ALLOCATED(ss%nphi)) then
     jsize = SIZE(ss%nphi)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nphi already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nphi(ss%max_num_nphi,ss%nicrf_src))
     ss%nphi = 0
  ENDIF

  isize = (ss%nrho_rw-1)
  IF(ALLOCATED(ss%nrw).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nrw)
  ENDIF
  IF(ALLOCATED(ss%nrw)) then
     jsize = SIZE(ss%nrw)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nrw already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nrw(ss%nrho_rw-1))
     ss%nrw = 0.0_rspec
  ENDIF

  isize = (ss%nicrf_src)
  IF(ALLOCATED(ss%nrz_antgeo).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nrz_antgeo)
  ENDIF
  IF(ALLOCATED(ss%nrz_antgeo)) then
     jsize = SIZE(ss%nrz_antgeo)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nrz_antgeo already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nrz_antgeo(ss%nicrf_src))
     ss%nrz_antgeo = 0
  ENDIF

  isize = (ss%nrho-1)*(ss%nspec_th+1)
  IF(ss%nspec_th.LE.0) isize = 0
  IF(ALLOCATED(ss%ns).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%ns)
  ENDIF
  IF(ALLOCATED(ss%ns)) then
     jsize = SIZE(ss%ns)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: ns already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%ns(ss%nrho-1,0:ss%nspec_th))
     ss%ns = 0.0_rspec
     ALLOCATE(ss%id_ns(0:ss%nspec_th))
     ss%id_ns = 0
  ENDIF

  isize = (ss%nlhrf_src)
  IF(ALLOCATED(ss%nspectrumlh_src).AND.(isize.EQ.0)) then
     DEALLOCATE(ss%nspectrumlh_src)
  ENDIF
  IF(ALLOCATED(ss%nspectrumlh_src)) then
     jsize = SIZE(ss%nspectrumlh_src)
     IF(isize.NE.jsize) THEN
        WRITE(iout,*) &
             " ?plasma_state_empty_alloc: nspectrumlh_src already allocated with size:"
        WRITE(iout,*) "    old size: ",jsize
        WRITE(iout,*) "    requested new size: ",isize
        ierr=999
     ENDIF
  ELSE IF (isize.GT.0) THEN
     ALLOCATE(ss%nspectrumlh_src(ss%nlhrf_src))
     ss%nspectrumlh_src = 0
  ENDIF

END SUBROUTINE plasma_state_empty_alloc_4
