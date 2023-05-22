! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_merge_4(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)


  ! Merge floating point data
  !   ss3 <-- w1*ss1 + (1-w1)*ss2

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components to merge
                                ! use ps_cc_all to merge all components

  REAL(KIND=rspec) :: w1        ! weight on ss1
  TYPE (plasma_state) :: ss1    ! state data to copy *from* with weight (w1)
  TYPE (plasma_state) :: ss2    ! state data to copy *from* with weight (1-w1)
  TYPE (plasma_state) :: ss3    ! state to which data is copied.
  INTEGER, INTENT(IN) :: icheck ! control for checking match of non-floating-
  !  point data: ps_ignore, ps_warn, or ps_error on mismatch.

  INTEGER, INTENT(IN) :: iout
  INTEGER, INTENT(INOUT) :: ierr  ! completion code (not cleared,set on error)

  !-------------------
  INTEGER :: isize
  !-------------------

  !-----------------------------------------------
  ! copy scalars and arrays that are allocated...

  IF((ALLOCATED(ss3%nbeami)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%nbeami).AND.ALLOCATED(ss2%nbeami)) THEN
        CALL ps_conformal("nbeami",ss2%nbeami,ss3%nbeami,iout,ierr)
        CALL ps_conformal("nbeami",ss1%nbeami,ss3%nbeami,iout,ierr)
        if(ierr.eq.0) ss3%nbeami = w1*ss1%nbeami + (1.0_rspec-w1)*ss2%nbeami
     ELSE IF(ALLOCATED(ss1%nbeami)) THEN
        CALL ps_conformal("nbeami",ss1%nbeami,ss3%nbeami,iout,ierr)
        ss3%nbeami = ss1%nbeami
     ELSE IF(ALLOCATED(ss2%nbeami)) THEN
        CALL ps_conformal("nbeami",ss2%nbeami,ss3%nbeami,iout,ierr)
        ss3%nbeami = ss2%nbeami
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nbeami_bdy)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%nbeami_bdy).AND.ALLOCATED(ss2%nbeami_bdy)) THEN
        CALL ps_conformal("nbeami_bdy",ss2%nbeami_bdy,ss3%nbeami_bdy,iout,ierr)
        CALL ps_conformal("nbeami_bdy",ss1%nbeami_bdy,ss3%nbeami_bdy,iout,ierr)
        if(ierr.eq.0) ss3%nbeami_bdy = w1*ss1%nbeami_bdy + (1.0_rspec-w1)*ss2%nbeami_bdy
     ELSE IF(ALLOCATED(ss1%nbeami_bdy)) THEN
        CALL ps_conformal("nbeami_bdy",ss1%nbeami_bdy,ss3%nbeami_bdy,iout,ierr)
        ss3%nbeami_bdy = ss1%nbeami_bdy
     ELSE IF(ALLOCATED(ss2%nbeami_bdy)) THEN
        CALL ps_conformal("nbeami_bdy",ss2%nbeami_bdy,ss3%nbeami_bdy,iout,ierr)
        ss3%nbeami_bdy = ss2%nbeami_bdy
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nbion)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%nbion).AND.ALLOCATED(ss2%nbion)) THEN
        CALL ps_diff("nbion",ss1%nbion,ss2%nbion,icheck,iout,ierr)
        CALL ps_conformal("nbion",ss1%nbion,ss3%nbion,iout,ierr)
        if(ierr.eq.0) ss3%nbion = ss1%nbion
     ELSE IF(ALLOCATED(ss1%nbion)) THEN
        CALL ps_conformal("nbion",ss1%nbion,ss3%nbion,iout,ierr)
        ss3%nbion = ss1%nbion
     ELSE IF(ALLOCATED(ss2%nbion)) THEN
        CALL ps_conformal("nbion",ss2%nbion,ss3%nbion,iout,ierr)
        ss3%nbion = ss2%nbion
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nbion_trace)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%nbion_trace).AND.ALLOCATED(ss2%nbion_trace)) THEN
        CALL ps_diff("nbion_trace",ss1%nbion_trace,ss2%nbion_trace,icheck,iout,ierr)
        CALL ps_conformal("nbion_trace",ss1%nbion_trace,ss3%nbion_trace,iout,ierr)
        if(ierr.eq.0) ss3%nbion_trace = ss1%nbion_trace
     ELSE IF(ALLOCATED(ss1%nbion_trace)) THEN
        CALL ps_conformal("nbion_trace",ss1%nbion_trace,ss3%nbion_trace,iout,ierr)
        ss3%nbion_trace = ss1%nbion_trace
     ELSE IF(ALLOCATED(ss2%nbion_trace)) THEN
        CALL ps_conformal("nbion_trace",ss2%nbion_trace,ss3%nbion_trace,iout,ierr)
        ss3%nbion_trace = ss2%nbion_trace
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_NBI).EQ.1) THEN

     CALL ps_diff("NBI_Code_Info",ss1%NBI_Code_Info,ss2%NBI_Code_Info,icheck,iout,ierr)
     ss3%NBI_Code_Info = ss1%NBI_Code_Info
  ENDIF

  IF(cclist(ps_cnum_NBI).EQ.1) THEN

     CALL ps_diff("NBI_Data_Info",ss1%NBI_Data_Info,ss2%NBI_Data_Info,icheck,iout,ierr)
     ss3%NBI_Data_Info = ss1%NBI_Data_Info
  ENDIF

  IF((ALLOCATED(ss3%nbi_src_name)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%nbi_src_name).AND.ALLOCATED(ss2%nbi_src_name)) THEN
        CALL ps_diff("nbi_src_name",ss1%nbi_src_name,ss2%nbi_src_name,icheck,iout,ierr)
        CALL ps_conformal("nbi_src_name",ss1%nbi_src_name,ss3%nbi_src_name,iout,ierr)
        if(ierr.eq.0) ss3%nbi_src_name = ss1%nbi_src_name
     ELSE IF(ALLOCATED(ss1%nbi_src_name)) THEN
        CALL ps_conformal("nbi_src_name",ss1%nbi_src_name,ss3%nbi_src_name,iout,ierr)
        ss3%nbi_src_name = ss1%nbi_src_name
     ELSE IF(ALLOCATED(ss2%nbi_src_name)) THEN
        CALL ps_conformal("nbi_src_name",ss2%nbi_src_name,ss3%nbi_src_name,iout,ierr)
        ss3%nbi_src_name = ss2%nbi_src_name
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nbshape)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%nbshape).AND.ALLOCATED(ss2%nbshape)) THEN
        CALL ps_diff("nbshape",ss1%nbshape,ss2%nbshape,icheck,iout,ierr)
        CALL ps_conformal("nbshape",ss1%nbshape,ss3%nbshape,iout,ierr)
        if(ierr.eq.0) ss3%nbshape = ss1%nbshape
     ELSE IF(ALLOCATED(ss1%nbshape)) THEN
        CALL ps_conformal("nbshape",ss1%nbshape,ss3%nbshape,iout,ierr)
        ss3%nbshape = ss1%nbshape
     ELSE IF(ALLOCATED(ss2%nbshape)) THEN
        CALL ps_conformal("nbshape",ss2%nbshape,ss3%nbshape,iout,ierr)
        ss3%nbshape = ss2%nbshape
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     ss3%nebar = w1*ss1%nebar + (1.0_rspec-w1)*ss2%nebar
  ENDIF

  IF((ALLOCATED(ss3%neflux_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%neflux_ky_tglf).AND.ALLOCATED(ss2%neflux_ky_tglf)) THEN
        CALL ps_conformal("neflux_ky_tglf",ss2%neflux_ky_tglf,ss3%neflux_ky_tglf,iout,ierr)
        CALL ps_conformal("neflux_ky_tglf",ss1%neflux_ky_tglf,ss3%neflux_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%neflux_ky_tglf = w1*ss1%neflux_ky_tglf + (1.0_rspec-w1)*ss2%neflux_ky_tglf
     ELSE IF(ALLOCATED(ss1%neflux_ky_tglf)) THEN
        CALL ps_conformal("neflux_ky_tglf",ss1%neflux_ky_tglf,ss3%neflux_ky_tglf,iout,ierr)
        ss3%neflux_ky_tglf = ss1%neflux_ky_tglf
     ELSE IF(ALLOCATED(ss2%neflux_ky_tglf)) THEN
        CALL ps_conformal("neflux_ky_tglf",ss2%neflux_ky_tglf,ss3%neflux_ky_tglf,iout,ierr)
        ss3%neflux_ky_tglf = ss2%neflux_ky_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Newcomb_code)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%Newcomb_code).AND.ALLOCATED(ss2%Newcomb_code)) THEN
        CALL ps_diff("Newcomb_code",ss1%Newcomb_code,ss2%Newcomb_code,icheck,iout,ierr)
        CALL ps_conformal("Newcomb_code",ss1%Newcomb_code,ss3%Newcomb_code,iout,ierr)
        if(ierr.eq.0) ss3%Newcomb_code = ss1%Newcomb_code
     ELSE IF(ALLOCATED(ss1%Newcomb_code)) THEN
        CALL ps_conformal("Newcomb_code",ss1%Newcomb_code,ss3%Newcomb_code,iout,ierr)
        ss3%Newcomb_code = ss1%Newcomb_code
     ELSE IF(ALLOCATED(ss2%Newcomb_code)) THEN
        CALL ps_conformal("Newcomb_code",ss2%Newcomb_code,ss3%Newcomb_code,iout,ierr)
        ss3%Newcomb_code = ss2%Newcomb_code
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Newcomb_mapper)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%Newcomb_mapper).AND.ALLOCATED(ss2%Newcomb_mapper)) THEN
        CALL ps_diff("Newcomb_mapper",ss1%Newcomb_mapper,ss2%Newcomb_mapper,icheck,iout,ierr)
        CALL ps_conformal("Newcomb_mapper",ss1%Newcomb_mapper,ss3%Newcomb_mapper,iout,ierr)
        if(ierr.eq.0) ss3%Newcomb_mapper = ss1%Newcomb_mapper
     ELSE IF(ALLOCATED(ss1%Newcomb_mapper)) THEN
        CALL ps_conformal("Newcomb_mapper",ss1%Newcomb_mapper,ss3%Newcomb_mapper,iout,ierr)
        ss3%Newcomb_mapper = ss1%Newcomb_mapper
     ELSE IF(ALLOCATED(ss2%Newcomb_mapper)) THEN
        CALL ps_conformal("Newcomb_mapper",ss2%Newcomb_mapper,ss3%Newcomb_mapper,iout,ierr)
        ss3%Newcomb_mapper = ss2%Newcomb_mapper
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Newcomb_stable)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%Newcomb_stable).AND.ALLOCATED(ss2%Newcomb_stable)) THEN
        CALL ps_diff("Newcomb_stable",ss1%Newcomb_stable,ss2%Newcomb_stable,icheck,iout,ierr)
        CALL ps_conformal("Newcomb_stable",ss1%Newcomb_stable,ss3%Newcomb_stable,iout,ierr)
        if(ierr.eq.0) ss3%Newcomb_stable = ss1%Newcomb_stable
     ELSE IF(ALLOCATED(ss1%Newcomb_stable)) THEN
        CALL ps_conformal("Newcomb_stable",ss1%Newcomb_stable,ss3%Newcomb_stable,iout,ierr)
        ss3%Newcomb_stable = ss1%Newcomb_stable
     ELSE IF(ALLOCATED(ss2%Newcomb_stable)) THEN
        CALL ps_conformal("Newcomb_stable",ss2%Newcomb_stable,ss3%Newcomb_stable,iout,ierr)
        ss3%Newcomb_stable = ss2%Newcomb_stable
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Newcomb_status)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%Newcomb_status).AND.ALLOCATED(ss2%Newcomb_status)) THEN
        CALL ps_diff("Newcomb_status",ss1%Newcomb_status,ss2%Newcomb_status,icheck,iout,ierr)
        CALL ps_conformal("Newcomb_status",ss1%Newcomb_status,ss3%Newcomb_status,iout,ierr)
        if(ierr.eq.0) ss3%Newcomb_status = ss1%Newcomb_status
     ELSE IF(ALLOCATED(ss1%Newcomb_status)) THEN
        CALL ps_conformal("Newcomb_status",ss1%Newcomb_status,ss3%Newcomb_status,iout,ierr)
        ss3%Newcomb_status = ss1%Newcomb_status
     ELSE IF(ALLOCATED(ss2%Newcomb_status)) THEN
        CALL ps_conformal("Newcomb_status",ss2%Newcomb_status,ss3%Newcomb_status,iout,ierr)
        ss3%Newcomb_status = ss2%Newcomb_status
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ne_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%ne_ky_tglf).AND.ALLOCATED(ss2%ne_ky_tglf)) THEN
        CALL ps_conformal("ne_ky_tglf",ss2%ne_ky_tglf,ss3%ne_ky_tglf,iout,ierr)
        CALL ps_conformal("ne_ky_tglf",ss1%ne_ky_tglf,ss3%ne_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%ne_ky_tglf = w1*ss1%ne_ky_tglf + (1.0_rspec-w1)*ss2%ne_ky_tglf
     ELSE IF(ALLOCATED(ss1%ne_ky_tglf)) THEN
        CALL ps_conformal("ne_ky_tglf",ss1%ne_ky_tglf,ss3%ne_ky_tglf,iout,ierr)
        ss3%ne_ky_tglf = ss1%ne_ky_tglf
     ELSE IF(ALLOCATED(ss2%ne_ky_tglf)) THEN
        CALL ps_conformal("ne_ky_tglf",ss2%ne_ky_tglf,ss3%ne_ky_tglf,iout,ierr)
        ss3%ne_ky_tglf = ss2%ne_ky_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nfusi)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%nfusi).AND.ALLOCATED(ss2%nfusi)) THEN
        CALL ps_conformal("nfusi",ss2%nfusi,ss3%nfusi,iout,ierr)
        CALL ps_conformal("nfusi",ss1%nfusi,ss3%nfusi,iout,ierr)
        if(ierr.eq.0) ss3%nfusi = w1*ss1%nfusi + (1.0_rspec-w1)*ss2%nfusi
     ELSE IF(ALLOCATED(ss1%nfusi)) THEN
        CALL ps_conformal("nfusi",ss1%nfusi,ss3%nfusi,iout,ierr)
        ss3%nfusi = ss1%nfusi
     ELSE IF(ALLOCATED(ss2%nfusi)) THEN
        CALL ps_conformal("nfusi",ss2%nfusi,ss3%nfusi,iout,ierr)
        ss3%nfusi = ss2%nfusi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nfusi_bdy)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%nfusi_bdy).AND.ALLOCATED(ss2%nfusi_bdy)) THEN
        CALL ps_conformal("nfusi_bdy",ss2%nfusi_bdy,ss3%nfusi_bdy,iout,ierr)
        CALL ps_conformal("nfusi_bdy",ss1%nfusi_bdy,ss3%nfusi_bdy,iout,ierr)
        if(ierr.eq.0) ss3%nfusi_bdy = w1*ss1%nfusi_bdy + (1.0_rspec-w1)*ss2%nfusi_bdy
     ELSE IF(ALLOCATED(ss1%nfusi_bdy)) THEN
        CALL ps_conformal("nfusi_bdy",ss1%nfusi_bdy,ss3%nfusi_bdy,iout,ierr)
        ss3%nfusi_bdy = ss1%nfusi_bdy
     ELSE IF(ALLOCATED(ss2%nfusi_bdy)) THEN
        CALL ps_conformal("nfusi_bdy",ss2%nfusi_bdy,ss3%nfusi_bdy,iout,ierr)
        ss3%nfusi_bdy = ss2%nfusi_bdy
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ngradb2_av)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%ngradb2_av).AND.ALLOCATED(ss2%ngradb2_av)) THEN
        CALL ps_conformal("ngradb2_av",ss2%ngradb2_av,ss3%ngradb2_av,iout,ierr)
        CALL ps_conformal("ngradb2_av",ss1%ngradb2_av,ss3%ngradb2_av,iout,ierr)
        if(ierr.eq.0) ss3%ngradb2_av = w1*ss1%ngradb2_av + (1.0_rspec-w1)*ss2%ngradb2_av
     ELSE IF(ALLOCATED(ss1%ngradb2_av)) THEN
        CALL ps_conformal("ngradb2_av",ss1%ngradb2_av,ss3%ngradb2_av,iout,ierr)
        ss3%ngradb2_av = ss1%ngradb2_av
     ELSE IF(ALLOCATED(ss2%ngradb2_av)) THEN
        CALL ps_conformal("ngradb2_av",ss2%ngradb2_av,ss3%ngradb2_av,iout,ierr)
        ss3%ngradb2_av = ss2%ngradb2_av
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ni)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%ni).AND.ALLOCATED(ss2%ni)) THEN
        CALL ps_conformal("ni",ss2%ni,ss3%ni,iout,ierr)
        CALL ps_conformal("ni",ss1%ni,ss3%ni,iout,ierr)
        if(ierr.eq.0) ss3%ni = w1*ss1%ni + (1.0_rspec-w1)*ss2%ni
     ELSE IF(ALLOCATED(ss1%ni)) THEN
        CALL ps_conformal("ni",ss1%ni,ss3%ni,iout,ierr)
        ss3%ni = ss1%ni
     ELSE IF(ALLOCATED(ss2%ni)) THEN
        CALL ps_conformal("ni",ss2%ni,ss3%ni,iout,ierr)
        ss3%ni = ss2%ni
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%niflux_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%niflux_ky_tglf).AND.ALLOCATED(ss2%niflux_ky_tglf)) THEN
        CALL ps_conformal("niflux_ky_tglf",ss2%niflux_ky_tglf,ss3%niflux_ky_tglf,iout,ierr)
        CALL ps_conformal("niflux_ky_tglf",ss1%niflux_ky_tglf,ss3%niflux_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%niflux_ky_tglf = w1*ss1%niflux_ky_tglf + (1.0_rspec-w1)*ss2%niflux_ky_tglf
     ELSE IF(ALLOCATED(ss1%niflux_ky_tglf)) THEN
        CALL ps_conformal("niflux_ky_tglf",ss1%niflux_ky_tglf,ss3%niflux_ky_tglf,iout,ierr)
        ss3%niflux_ky_tglf = ss1%niflux_ky_tglf
     ELSE IF(ALLOCATED(ss2%niflux_ky_tglf)) THEN
        CALL ps_conformal("niflux_ky_tglf",ss2%niflux_ky_tglf,ss3%niflux_ky_tglf,iout,ierr)
        ss3%niflux_ky_tglf = ss2%niflux_ky_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ni_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%ni_ky_tglf).AND.ALLOCATED(ss2%ni_ky_tglf)) THEN
        CALL ps_conformal("ni_ky_tglf",ss2%ni_ky_tglf,ss3%ni_ky_tglf,iout,ierr)
        CALL ps_conformal("ni_ky_tglf",ss1%ni_ky_tglf,ss3%ni_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%ni_ky_tglf = w1*ss1%ni_ky_tglf + (1.0_rspec-w1)*ss2%ni_ky_tglf
     ELSE IF(ALLOCATED(ss1%ni_ky_tglf)) THEN
        CALL ps_conformal("ni_ky_tglf",ss1%ni_ky_tglf,ss3%ni_ky_tglf,iout,ierr)
        ss3%ni_ky_tglf = ss1%ni_ky_tglf
     ELSE IF(ALLOCATED(ss2%ni_ky_tglf)) THEN
        CALL ps_conformal("ni_ky_tglf",ss2%ni_ky_tglf,ss3%ni_ky_tglf,iout,ierr)
        ss3%ni_ky_tglf = ss2%ni_ky_tglf
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_ANOM).EQ.1) THEN

     CALL ps_diff("nmdifb",ss1%nmdifb,ss2%nmdifb,icheck,iout,ierr)
     ss3%nmdifb = ss1%nmdifb
  ENDIF

  IF((ALLOCATED(ss3%nmini)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%nmini).AND.ALLOCATED(ss2%nmini)) THEN
        CALL ps_conformal("nmini",ss2%nmini,ss3%nmini,iout,ierr)
        CALL ps_conformal("nmini",ss1%nmini,ss3%nmini,iout,ierr)
        if(ierr.eq.0) ss3%nmini = w1*ss1%nmini + (1.0_rspec-w1)*ss2%nmini
     ELSE IF(ALLOCATED(ss1%nmini)) THEN
        CALL ps_conformal("nmini",ss1%nmini,ss3%nmini,iout,ierr)
        ss3%nmini = ss1%nmini
     ELSE IF(ALLOCATED(ss2%nmini)) THEN
        CALL ps_conformal("nmini",ss2%nmini,ss3%nmini,iout,ierr)
        ss3%nmini = ss2%nmini
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nmini_bdy)).AND.(cclist(ps_cnum_FUS).EQ.1)) THEN
     IF(ALLOCATED(ss1%nmini_bdy).AND.ALLOCATED(ss2%nmini_bdy)) THEN
        CALL ps_conformal("nmini_bdy",ss2%nmini_bdy,ss3%nmini_bdy,iout,ierr)
        CALL ps_conformal("nmini_bdy",ss1%nmini_bdy,ss3%nmini_bdy,iout,ierr)
        if(ierr.eq.0) ss3%nmini_bdy = w1*ss1%nmini_bdy + (1.0_rspec-w1)*ss2%nmini_bdy
     ELSE IF(ALLOCATED(ss1%nmini_bdy)) THEN
        CALL ps_conformal("nmini_bdy",ss1%nmini_bdy,ss3%nmini_bdy,iout,ierr)
        ss3%nmini_bdy = ss1%nmini_bdy
     ELSE IF(ALLOCATED(ss2%nmini_bdy)) THEN
        CALL ps_conformal("nmini_bdy",ss2%nmini_bdy,ss3%nmini_bdy,iout,ierr)
        ss3%nmini_bdy = ss2%nmini_bdy
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nmodel)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%nmodel).AND.ALLOCATED(ss2%nmodel)) THEN
        CALL ps_conformal("nmodel",ss2%nmodel,ss3%nmodel,iout,ierr)
        CALL ps_conformal("nmodel",ss1%nmodel,ss3%nmodel,iout,ierr)
        if(ierr.eq.0) ss3%nmodel = w1*ss1%nmodel + (1.0_rspec-w1)*ss2%nmodel
     ELSE IF(ALLOCATED(ss1%nmodel)) THEN
        CALL ps_conformal("nmodel",ss1%nmodel,ss3%nmodel,iout,ierr)
        ss3%nmodel = ss1%nmodel
     ELSE IF(ALLOCATED(ss2%nmodel)) THEN
        CALL ps_conformal("nmodel",ss2%nmodel,ss3%nmodel,iout,ierr)
        ss3%nmodel = ss2%nmodel
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     ss3%nmodel_bdy = w1*ss1%nmodel_bdy + (1.0_rspec-w1)*ss2%nmodel_bdy
  ENDIF

  IF(cclist(ps_cnum_EQ).EQ.1) THEN

     CALL ps_diff("nmom",ss1%nmom,ss2%nmom,icheck,iout,ierr)
     ss3%nmom = ss1%nmom
  ENDIF

  IF((ALLOCATED(ss3%nnkparlh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%nnkparlh).AND.ALLOCATED(ss2%nnkparlh)) THEN
        CALL ps_diff("nnkparlh",ss1%nnkparlh,ss2%nnkparlh,icheck,iout,ierr)
        CALL ps_conformal("nnkparlh",ss1%nnkparlh,ss3%nnkparlh,iout,ierr)
        if(ierr.eq.0) ss3%nnkparlh = ss1%nnkparlh
     ELSE IF(ALLOCATED(ss1%nnkparlh)) THEN
        CALL ps_conformal("nnkparlh",ss1%nnkparlh,ss3%nnkparlh,iout,ierr)
        ss3%nnkparlh = ss1%nnkparlh
     ELSE IF(ALLOCATED(ss2%nnkparlh)) THEN
        CALL ps_conformal("nnkparlh",ss2%nnkparlh,ss3%nnkparlh,iout,ierr)
        ss3%nnkparlh = ss2%nnkparlh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nnkpollh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%nnkpollh).AND.ALLOCATED(ss2%nnkpollh)) THEN
        CALL ps_diff("nnkpollh",ss1%nnkpollh,ss2%nnkpollh,icheck,iout,ierr)
        CALL ps_conformal("nnkpollh",ss1%nnkpollh,ss3%nnkpollh,iout,ierr)
        if(ierr.eq.0) ss3%nnkpollh = ss1%nnkpollh
     ELSE IF(ALLOCATED(ss1%nnkpollh)) THEN
        CALL ps_conformal("nnkpollh",ss1%nnkpollh,ss3%nnkpollh,iout,ierr)
        ss3%nnkpollh = ss1%nnkpollh
     ELSE IF(ALLOCATED(ss2%nnkpollh)) THEN
        CALL ps_conformal("nnkpollh",ss2%nnkpollh,ss3%nnkpollh,iout,ierr)
        ss3%nnkpollh = ss2%nnkpollh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nnktorlh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%nnktorlh).AND.ALLOCATED(ss2%nnktorlh)) THEN
        CALL ps_diff("nnktorlh",ss1%nnktorlh,ss2%nnktorlh,icheck,iout,ierr)
        CALL ps_conformal("nnktorlh",ss1%nnktorlh,ss3%nnktorlh,iout,ierr)
        if(ierr.eq.0) ss3%nnktorlh = ss1%nnktorlh
     ELSE IF(ALLOCATED(ss1%nnktorlh)) THEN
        CALL ps_conformal("nnktorlh",ss1%nnktorlh,ss3%nnktorlh,iout,ierr)
        ss3%nnktorlh = ss1%nnktorlh
     ELSE IF(ALLOCATED(ss2%nnktorlh)) THEN
        CALL ps_conformal("nnktorlh",ss2%nnktorlh,ss3%nnktorlh,iout,ierr)
        ss3%nnktorlh = ss2%nnktorlh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nphi)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%nphi).AND.ALLOCATED(ss2%nphi)) THEN
        CALL ps_diff("nphi",ss1%nphi,ss2%nphi,icheck,iout,ierr)
        CALL ps_conformal("nphi",ss1%nphi,ss3%nphi,iout,ierr)
        if(ierr.eq.0) ss3%nphi = ss1%nphi
     ELSE IF(ALLOCATED(ss1%nphi)) THEN
        CALL ps_conformal("nphi",ss1%nphi,ss3%nphi,iout,ierr)
        ss3%nphi = ss1%nphi
     ELSE IF(ALLOCATED(ss2%nphi)) THEN
        CALL ps_conformal("nphi",ss2%nphi,ss3%nphi,iout,ierr)
        ss3%nphi = ss2%nphi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nrw)).AND.(cclist(ps_cnum_RUNAWAY).EQ.1)) THEN
     IF(ALLOCATED(ss1%nrw).AND.ALLOCATED(ss2%nrw)) THEN
        CALL ps_conformal("nrw",ss2%nrw,ss3%nrw,iout,ierr)
        CALL ps_conformal("nrw",ss1%nrw,ss3%nrw,iout,ierr)
        if(ierr.eq.0) ss3%nrw = w1*ss1%nrw + (1.0_rspec-w1)*ss2%nrw
     ELSE IF(ALLOCATED(ss1%nrw)) THEN
        CALL ps_conformal("nrw",ss1%nrw,ss3%nrw,iout,ierr)
        ss3%nrw = ss1%nrw
     ELSE IF(ALLOCATED(ss2%nrw)) THEN
        CALL ps_conformal("nrw",ss2%nrw,ss3%nrw,iout,ierr)
        ss3%nrw = ss2%nrw
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nrz_antgeo)).AND.(cclist(ps_cnum_IC).EQ.1)) THEN
     IF(ALLOCATED(ss1%nrz_antgeo).AND.ALLOCATED(ss2%nrz_antgeo)) THEN
        CALL ps_diff("nrz_antgeo",ss1%nrz_antgeo,ss2%nrz_antgeo,icheck,iout,ierr)
        CALL ps_conformal("nrz_antgeo",ss1%nrz_antgeo,ss3%nrz_antgeo,iout,ierr)
        if(ierr.eq.0) ss3%nrz_antgeo = ss1%nrz_antgeo
     ELSE IF(ALLOCATED(ss1%nrz_antgeo)) THEN
        CALL ps_conformal("nrz_antgeo",ss1%nrz_antgeo,ss3%nrz_antgeo,iout,ierr)
        ss3%nrz_antgeo = ss1%nrz_antgeo
     ELSE IF(ALLOCATED(ss2%nrz_antgeo)) THEN
        CALL ps_conformal("nrz_antgeo",ss2%nrz_antgeo,ss3%nrz_antgeo,iout,ierr)
        ss3%nrz_antgeo = ss2%nrz_antgeo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%ns)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%ns).AND.ALLOCATED(ss2%ns)) THEN
        CALL ps_conformal("ns",ss2%ns,ss3%ns,iout,ierr)
        CALL ps_conformal("ns",ss1%ns,ss3%ns,iout,ierr)
        if(ierr.eq.0) ss3%ns = w1*ss1%ns + (1.0_rspec-w1)*ss2%ns
     ELSE IF(ALLOCATED(ss1%ns)) THEN
        CALL ps_conformal("ns",ss1%ns,ss3%ns,iout,ierr)
        ss3%ns = ss1%ns
     ELSE IF(ALLOCATED(ss2%ns)) THEN
        CALL ps_conformal("ns",ss2%ns,ss3%ns,iout,ierr)
        ss3%ns = ss2%ns
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%nspectrumlh_src)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%nspectrumlh_src).AND.ALLOCATED(ss2%nspectrumlh_src)) THEN
        CALL ps_diff("nspectrumlh_src",ss1%nspectrumlh_src,ss2%nspectrumlh_src,icheck,iout,ierr)
        CALL ps_conformal("nspectrumlh_src",ss1%nspectrumlh_src,ss3%nspectrumlh_src,iout,ierr)
        if(ierr.eq.0) ss3%nspectrumlh_src = ss1%nspectrumlh_src
     ELSE IF(ALLOCATED(ss1%nspectrumlh_src)) THEN
        CALL ps_conformal("nspectrumlh_src",ss1%nspectrumlh_src,ss3%nspectrumlh_src,iout,ierr)
        ss3%nspectrumlh_src = ss1%nspectrumlh_src
     ELSE IF(ALLOCATED(ss2%nspectrumlh_src)) THEN
        CALL ps_conformal("nspectrumlh_src",ss2%nspectrumlh_src,ss3%nspectrumlh_src,iout,ierr)
        ss3%nspectrumlh_src = ss2%nspectrumlh_src
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_merge_4
