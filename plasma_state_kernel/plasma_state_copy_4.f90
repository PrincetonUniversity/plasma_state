! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_copy_4(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)


  ! Copy scalars, and, arrays that are allocated
  !   ss2 <-- ss

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components to merge
                                ! use ps_cc_all to merge all components

  TYPE (plasma_state) :: ss     ! state data to copy *from*
  TYPE (plasma_state) :: ss2    ! state data to copy *to*

  INTEGER, INTENT(IN) :: icopy_lists ! =1 to require copying of all lists

  INTEGER, INTENT(IN) :: iwant_1deq  ! =1 to request fcns of (rho_eq)
                                     !  even if cclist(ps_cnum_EQ).eq.0

  LOGICAL, INTENT(IN) :: partial_copy ! .TRUE. if full state copy not in effect

  INTEGER, INTENT(INOUT) :: ierr  ! completion flag (not cleared, set by error)

  !-------------------
  INTEGER :: iout
  !-------------------

  iout = SWIM_err_out_file


  !-----------------------------------------------
  ! copy scalars and arrays that are allocated...
  IF(ALLOCATED(ss2%id_nbeami)) THEN
     IF(partial_copy) THEN
        ss2%id_nbeami = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%nbeami).AND.ALLOCATED(ss2%nbeami)).AND. &
        ((cclist(ps_cnum_NBI).EQ.1))) THEN
     ss2%nbeami = ss%nbeami
     IF(ALLOCATED(ss2%id_nbeami)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_nbeami = ss%id_nbeami
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%nbeami_bdy)).AND.(ALLOCATED(ss%nbeami_bdy))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%nbeami_bdy = ss%nbeami_bdy
  ENDIF
  IF( ((ALLOCATED(ss2%nbion)).AND.(ALLOCATED(ss%nbion))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%nbion = ss%nbion
  ENDIF
  IF( ((ALLOCATED(ss2%nbion_trace)).AND.(ALLOCATED(ss%nbion_trace))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%nbion_trace = ss%nbion_trace
  ENDIF
  IF((cclist(ps_cnum_NBI).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%NBI_Code_Info = ss%NBI_Code_Info
  ENDIF
  IF((cclist(ps_cnum_NBI).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%NBI_Data_Info = ss%NBI_Data_Info
  ENDIF
  IF( ((ALLOCATED(ss2%nbi_src_name)).AND.(ALLOCATED(ss%nbi_src_name))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%nbi_src_name = ss%nbi_src_name
  ENDIF
  IF( ((ALLOCATED(ss2%nbshape)).AND.(ALLOCATED(ss%nbshape))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_NBI).EQ.1)) ) THEN
     ss2%nbshape = ss%nbshape
  ENDIF
  IF((cclist(ps_cnum_PLASMA).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%nebar = ss%nebar
  ENDIF
  IF(ALLOCATED(ss2%id_neflux_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_neflux_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%neflux_ky_tglf).AND.ALLOCATED(ss2%neflux_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%neflux_ky_tglf = ss%neflux_ky_tglf
     IF(ALLOCATED(ss2%id_neflux_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_neflux_ky_tglf = ss%id_neflux_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%Newcomb_code)).AND.(ALLOCATED(ss%Newcomb_code))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%Newcomb_code = ss%Newcomb_code
  ENDIF
  IF( ((ALLOCATED(ss2%Newcomb_mapper)).AND.(ALLOCATED(ss%Newcomb_mapper))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%Newcomb_mapper = ss%Newcomb_mapper
  ENDIF
  IF( ((ALLOCATED(ss2%Newcomb_stable)).AND.(ALLOCATED(ss%Newcomb_stable))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%Newcomb_stable = ss%Newcomb_stable
  ENDIF
  IF( ((ALLOCATED(ss2%Newcomb_status)).AND.(ALLOCATED(ss%Newcomb_status))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LMHD).EQ.1)) ) THEN
     ss2%Newcomb_status = ss%Newcomb_status
  ENDIF
  IF(ALLOCATED(ss2%id_ne_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_ne_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%ne_ky_tglf).AND.ALLOCATED(ss2%ne_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%ne_ky_tglf = ss%ne_ky_tglf
     IF(ALLOCATED(ss2%id_ne_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_ne_ky_tglf = ss%id_ne_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_nfusi)) THEN
     IF(partial_copy) THEN
        ss2%id_nfusi = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%nfusi).AND.ALLOCATED(ss2%nfusi)).AND. &
        ((cclist(ps_cnum_FUS).EQ.1))) THEN
     ss2%nfusi = ss%nfusi
     IF(ALLOCATED(ss2%id_nfusi)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_nfusi = ss%id_nfusi
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%nfusi_bdy)).AND.(ALLOCATED(ss%nfusi_bdy))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%nfusi_bdy = ss%nfusi_bdy
  ENDIF
  IF(partial_copy) THEN
     ss2%id_ngradb2_av = 0
  ENDIF
  IF((ALLOCATED(ss%ngradb2_av).AND.ALLOCATED(ss2%ngradb2_av)).AND. &
        ((cclist(ps_cnum_EQ).EQ.1))) THEN
     ss2%ngradb2_av = ss%ngradb2_av
     IF(.not.partial_copy) THEN
        ss2%id_ngradb2_av = ss%id_ngradb2_av
     ENDIF
  ENDIF
  IF(partial_copy) THEN
     ss2%id_ni = 0
  ENDIF
  IF((ALLOCATED(ss%ni).AND.ALLOCATED(ss2%ni)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%ni = ss%ni
     IF(.not.partial_copy) THEN
        ss2%id_ni = ss%id_ni
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_niflux_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_niflux_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%niflux_ky_tglf).AND.ALLOCATED(ss2%niflux_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%niflux_ky_tglf = ss%niflux_ky_tglf
     IF(ALLOCATED(ss2%id_niflux_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_niflux_ky_tglf = ss%id_niflux_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF(ALLOCATED(ss2%id_ni_ky_tglf)) THEN
     IF(partial_copy) THEN
        ss2%id_ni_ky_tglf = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%ni_ky_tglf).AND.ALLOCATED(ss2%ni_ky_tglf)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%ni_ky_tglf = ss%ni_ky_tglf
     IF(ALLOCATED(ss2%id_ni_ky_tglf)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_ni_ky_tglf = ss%id_ni_ky_tglf
        ENDIF
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_ANOM).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%nmdifb = ss%nmdifb
  ENDIF
  IF(ALLOCATED(ss2%id_nmini)) THEN
     IF(partial_copy) THEN
        ss2%id_nmini = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%nmini).AND.ALLOCATED(ss2%nmini)).AND. &
        ((cclist(ps_cnum_IC).EQ.1))) THEN
     ss2%nmini = ss%nmini
     IF(ALLOCATED(ss2%id_nmini)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_nmini = ss%id_nmini
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%nmini_bdy)).AND.(ALLOCATED(ss%nmini_bdy))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_FUS).EQ.1)) ) THEN
     ss2%nmini_bdy = ss%nmini_bdy
  ENDIF
  IF(partial_copy) THEN
     ss2%id_nmodel = 0
  ENDIF
  IF((ALLOCATED(ss%nmodel).AND.ALLOCATED(ss2%nmodel)).AND. &
        ((cclist(ps_cnum_ANOM).EQ.1))) THEN
     ss2%nmodel = ss%nmodel
     IF(.not.partial_copy) THEN
        ss2%id_nmodel = ss%id_nmodel
     ENDIF
  ENDIF
  IF((cclist(ps_cnum_ANOM).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%nmodel_bdy = ss%nmodel_bdy
  ENDIF
  IF((cclist(ps_cnum_EQ).EQ.1).OR.(icopy_lists.EQ.1)) THEN
     ss2%nmom = ss%nmom
  ENDIF
  IF( ((ALLOCATED(ss2%nnkparlh)).AND.(ALLOCATED(ss%nnkparlh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%nnkparlh = ss%nnkparlh
  ENDIF
  IF( ((ALLOCATED(ss2%nnkpollh)).AND.(ALLOCATED(ss%nnkpollh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%nnkpollh = ss%nnkpollh
  ENDIF
  IF( ((ALLOCATED(ss2%nnktorlh)).AND.(ALLOCATED(ss%nnktorlh))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%nnktorlh = ss%nnktorlh
  ENDIF
  IF( ((ALLOCATED(ss2%nphi)).AND.(ALLOCATED(ss%nphi))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%nphi = ss%nphi
  ENDIF
  IF(partial_copy) THEN
     ss2%id_nrw = 0
  ENDIF
  IF((ALLOCATED(ss%nrw).AND.ALLOCATED(ss2%nrw)).AND. &
        ((cclist(ps_cnum_RUNAWAY).EQ.1))) THEN
     ss2%nrw = ss%nrw
     IF(.not.partial_copy) THEN
        ss2%id_nrw = ss%id_nrw
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%nrz_antgeo)).AND.(ALLOCATED(ss%nrz_antgeo))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_IC).EQ.1)) ) THEN
     ss2%nrz_antgeo = ss%nrz_antgeo
  ENDIF
  IF(ALLOCATED(ss2%id_ns)) THEN
     IF(partial_copy) THEN
        ss2%id_ns = 0
     ENDIF
  ENDIF
  IF((ALLOCATED(ss%ns).AND.ALLOCATED(ss2%ns)).AND. &
        ((cclist(ps_cnum_PLASMA).EQ.1))) THEN
     ss2%ns = ss%ns
     IF(ALLOCATED(ss2%id_ns)) THEN
        IF(.not.partial_copy) THEN
           ss2%id_ns = ss%id_ns
        ENDIF
     ENDIF
  ENDIF
  IF( ((ALLOCATED(ss2%nspectrumlh_src)).AND.(ALLOCATED(ss%nspectrumlh_src))).AND. &
       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_LH).EQ.1)) ) THEN
     ss2%nspectrumlh_src = ss%nspectrumlh_src
  ENDIF

END SUBROUTINE plasma_state_copy_4
