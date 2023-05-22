! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_merge_8(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)


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

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     ss3%t1 = w1*ss1%t1 + (1.0_rspec-w1)*ss2%t1
  ENDIF

  IF((ALLOCATED(ss3%teflux_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%teflux_ky_tglf).AND.ALLOCATED(ss2%teflux_ky_tglf)) THEN
        CALL ps_conformal("teflux_ky_tglf",ss2%teflux_ky_tglf,ss3%teflux_ky_tglf,iout,ierr)
        CALL ps_conformal("teflux_ky_tglf",ss1%teflux_ky_tglf,ss3%teflux_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%teflux_ky_tglf = w1*ss1%teflux_ky_tglf + (1.0_rspec-w1)*ss2%teflux_ky_tglf
     ELSE IF(ALLOCATED(ss1%teflux_ky_tglf)) THEN
        CALL ps_conformal("teflux_ky_tglf",ss1%teflux_ky_tglf,ss3%teflux_ky_tglf,iout,ierr)
        ss3%teflux_ky_tglf = ss1%teflux_ky_tglf
     ELSE IF(ALLOCATED(ss2%teflux_ky_tglf)) THEN
        CALL ps_conformal("teflux_ky_tglf",ss2%teflux_ky_tglf,ss3%teflux_ky_tglf,iout,ierr)
        ss3%teflux_ky_tglf = ss2%teflux_ky_tglf
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     ss3%Te_bdy = w1*ss1%Te_bdy + (1.0_rspec-w1)*ss2%Te_bdy
  ENDIF

  IF((ALLOCATED(ss3%te_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%te_ky_tglf).AND.ALLOCATED(ss2%te_ky_tglf)) THEN
        CALL ps_conformal("te_ky_tglf",ss2%te_ky_tglf,ss3%te_ky_tglf,iout,ierr)
        CALL ps_conformal("te_ky_tglf",ss1%te_ky_tglf,ss3%te_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%te_ky_tglf = w1*ss1%te_ky_tglf + (1.0_rspec-w1)*ss2%te_ky_tglf
     ELSE IF(ALLOCATED(ss1%te_ky_tglf)) THEN
        CALL ps_conformal("te_ky_tglf",ss1%te_ky_tglf,ss3%te_ky_tglf,iout,ierr)
        ss3%te_ky_tglf = ss1%te_ky_tglf
     ELSE IF(ALLOCATED(ss2%te_ky_tglf)) THEN
        CALL ps_conformal("te_ky_tglf",ss2%te_ky_tglf,ss3%te_ky_tglf,iout,ierr)
        ss3%te_ky_tglf = ss2%te_ky_tglf
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("tfinal",ss1%tfinal,ss2%tfinal,icheck,iout,ierr)
     ss3%tfinal = ss1%tfinal
  ENDIF

  IF((ALLOCATED(ss3%tfrip2_log)).AND.(cclist(ps_cnum_RIPPLE).EQ.1)) THEN
     IF(ALLOCATED(ss1%tfrip2_log).AND.ALLOCATED(ss2%tfrip2_log)) THEN
        CALL ps_conformal("tfrip2_log",ss2%tfrip2_log,ss3%tfrip2_log,iout,ierr)
        CALL ps_conformal("tfrip2_log",ss1%tfrip2_log,ss3%tfrip2_log,iout,ierr)
        if(ierr.eq.0) ss3%tfrip2_log = w1*ss1%tfrip2_log + (1.0_rspec-w1)*ss2%tfrip2_log
     ELSE IF(ALLOCATED(ss1%tfrip2_log)) THEN
        CALL ps_conformal("tfrip2_log",ss1%tfrip2_log,ss3%tfrip2_log,iout,ierr)
        ss3%tfrip2_log = ss1%tfrip2_log
     ELSE IF(ALLOCATED(ss2%tfrip2_log)) THEN
        CALL ps_conformal("tfrip2_log",ss2%tfrip2_log,ss3%tfrip2_log,iout,ierr)
        ss3%tfrip2_log = ss2%tfrip2_log
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tfrip_log)).AND.(cclist(ps_cnum_RIPPLE).EQ.1)) THEN
     IF(ALLOCATED(ss1%tfrip_log).AND.ALLOCATED(ss2%tfrip_log)) THEN
        CALL ps_conformal("tfrip_log",ss2%tfrip_log,ss3%tfrip_log,iout,ierr)
        CALL ps_conformal("tfrip_log",ss1%tfrip_log,ss3%tfrip_log,iout,ierr)
        if(ierr.eq.0) ss3%tfrip_log = w1*ss1%tfrip_log + (1.0_rspec-w1)*ss2%tfrip_log
     ELSE IF(ALLOCATED(ss1%tfrip_log)) THEN
        CALL ps_conformal("tfrip_log",ss1%tfrip_log,ss3%tfrip_log,iout,ierr)
        ss3%tfrip_log = ss1%tfrip_log
     ELSE IF(ALLOCATED(ss2%tfrip_log)) THEN
        CALL ps_conformal("tfrip_log",ss2%tfrip_log,ss3%tfrip_log,iout,ierr)
        ss3%tfrip_log = ss2%tfrip_log
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("TF_Data_Info",ss1%TF_Data_Info,ss2%TF_Data_Info,icheck,iout,ierr)
     ss3%TF_Data_Info = ss1%TF_Data_Info
  ENDIF

  IF((ALLOCATED(ss3%thegrilh)).AND.(cclist(ps_cnum_LH).EQ.1)) THEN
     IF(ALLOCATED(ss1%thegrilh).AND.ALLOCATED(ss2%thegrilh)) THEN
        CALL ps_diff("thegrilh",ss1%thegrilh,ss2%thegrilh,icheck,iout,ierr)
        CALL ps_conformal("thegrilh",ss1%thegrilh,ss3%thegrilh,iout,ierr)
        if(ierr.eq.0) ss3%thegrilh = ss1%thegrilh
     ELSE IF(ALLOCATED(ss1%thegrilh)) THEN
        CALL ps_conformal("thegrilh",ss1%thegrilh,ss3%thegrilh,iout,ierr)
        ss3%thegrilh = ss1%thegrilh
     ELSE IF(ALLOCATED(ss2%thegrilh)) THEN
        CALL ps_conformal("thegrilh",ss2%thegrilh,ss3%thegrilh,iout,ierr)
        ss3%thegrilh = ss2%thegrilh
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%th_eq)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%th_eq).AND.ALLOCATED(ss2%th_eq)) THEN
        CALL ps_diff("th_eq",ss1%th_eq,ss2%th_eq,ps_error,iout,ierr)
        CALL ps_conformal("th_eq",ss1%th_eq,ss3%th_eq,iout,ierr)
        if(ierr.eq.0) ss3%th_eq = ss1%th_eq
     ELSE IF(ALLOCATED(ss1%th_eq)) THEN
        CALL ps_conformal("th_eq",ss1%th_eq,ss3%th_eq,iout,ierr)
        ss3%th_eq = ss1%th_eq
     ELSE IF(ALLOCATED(ss2%th_eq)) THEN
        CALL ps_conformal("th_eq",ss2%th_eq,ss3%th_eq,iout,ierr)
        ss3%th_eq = ss2%th_eq
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Ti)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%Ti).AND.ALLOCATED(ss2%Ti)) THEN
        CALL ps_conformal("Ti",ss2%Ti,ss3%Ti,iout,ierr)
        CALL ps_conformal("Ti",ss1%Ti,ss3%Ti,iout,ierr)
        if(ierr.eq.0) ss3%Ti = w1*ss1%Ti + (1.0_rspec-w1)*ss2%Ti
     ELSE IF(ALLOCATED(ss1%Ti)) THEN
        CALL ps_conformal("Ti",ss1%Ti,ss3%Ti,iout,ierr)
        ss3%Ti = ss1%Ti
     ELSE IF(ALLOCATED(ss2%Ti)) THEN
        CALL ps_conformal("Ti",ss2%Ti,ss3%Ti,iout,ierr)
        ss3%Ti = ss2%Ti
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tiflux_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%tiflux_ky_tglf).AND.ALLOCATED(ss2%tiflux_ky_tglf)) THEN
        CALL ps_conformal("tiflux_ky_tglf",ss2%tiflux_ky_tglf,ss3%tiflux_ky_tglf,iout,ierr)
        CALL ps_conformal("tiflux_ky_tglf",ss1%tiflux_ky_tglf,ss3%tiflux_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%tiflux_ky_tglf = w1*ss1%tiflux_ky_tglf + (1.0_rspec-w1)*ss2%tiflux_ky_tglf
     ELSE IF(ALLOCATED(ss1%tiflux_ky_tglf)) THEN
        CALL ps_conformal("tiflux_ky_tglf",ss1%tiflux_ky_tglf,ss3%tiflux_ky_tglf,iout,ierr)
        ss3%tiflux_ky_tglf = ss1%tiflux_ky_tglf
     ELSE IF(ALLOCATED(ss2%tiflux_ky_tglf)) THEN
        CALL ps_conformal("tiflux_ky_tglf",ss2%tiflux_ky_tglf,ss3%tiflux_ky_tglf,iout,ierr)
        ss3%tiflux_ky_tglf = ss2%tiflux_ky_tglf
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("tinit",ss1%tinit,ss2%tinit,icheck,iout,ierr)
     ss3%tinit = ss1%tinit
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN
     ss3%Ti_bdy = w1*ss1%Ti_bdy + (1.0_rspec-w1)*ss2%Ti_bdy
  ENDIF

  IF((ALLOCATED(ss3%ti_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%ti_ky_tglf).AND.ALLOCATED(ss2%ti_ky_tglf)) THEN
        CALL ps_conformal("ti_ky_tglf",ss2%ti_ky_tglf,ss3%ti_ky_tglf,iout,ierr)
        CALL ps_conformal("ti_ky_tglf",ss1%ti_ky_tglf,ss3%ti_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%ti_ky_tglf = w1*ss1%ti_ky_tglf + (1.0_rspec-w1)*ss2%ti_ky_tglf
     ELSE IF(ALLOCATED(ss1%ti_ky_tglf)) THEN
        CALL ps_conformal("ti_ky_tglf",ss1%ti_ky_tglf,ss3%ti_ky_tglf,iout,ierr)
        ss3%ti_ky_tglf = ss1%ti_ky_tglf
     ELSE IF(ALLOCATED(ss2%ti_ky_tglf)) THEN
        CALL ps_conformal("ti_ky_tglf",ss2%ti_ky_tglf,ss3%ti_ky_tglf,iout,ierr)
        ss3%ti_ky_tglf = ss2%ti_ky_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tmodel)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%tmodel).AND.ALLOCATED(ss2%tmodel)) THEN
        CALL ps_conformal("tmodel",ss2%tmodel,ss3%tmodel,iout,ierr)
        CALL ps_conformal("tmodel",ss1%tmodel,ss3%tmodel,iout,ierr)
        if(ierr.eq.0) ss3%tmodel = w1*ss1%tmodel + (1.0_rspec-w1)*ss2%tmodel
     ELSE IF(ALLOCATED(ss1%tmodel)) THEN
        CALL ps_conformal("tmodel",ss1%tmodel,ss3%tmodel,iout,ierr)
        ss3%tmodel = ss1%tmodel
     ELSE IF(ALLOCATED(ss2%tmodel)) THEN
        CALL ps_conformal("tmodel",ss2%tmodel,ss3%tmodel,iout,ierr)
        ss3%tmodel = ss2%tmodel
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_ANOM).EQ.1) THEN
     ss3%tmodel_bdy = w1*ss1%tmodel_bdy + (1.0_rspec-w1)*ss2%tmodel_bdy
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("tokamak_id",ss1%tokamak_id,ss2%tokamak_id,icheck,iout,ierr)
     ss3%tokamak_id = ss1%tokamak_id
  ENDIF

  IF((ALLOCATED(ss3%Tornum_balcrit)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%Tornum_balcrit).AND.ALLOCATED(ss2%Tornum_balcrit)) THEN
        CALL ps_diff("Tornum_balcrit",ss1%Tornum_balcrit,ss2%Tornum_balcrit,icheck,iout,ierr)
        CALL ps_conformal("Tornum_balcrit",ss1%Tornum_balcrit,ss3%Tornum_balcrit,iout,ierr)
        if(ierr.eq.0) ss3%Tornum_balcrit = ss1%Tornum_balcrit
     ELSE IF(ALLOCATED(ss1%Tornum_balcrit)) THEN
        CALL ps_conformal("Tornum_balcrit",ss1%Tornum_balcrit,ss3%Tornum_balcrit,iout,ierr)
        ss3%Tornum_balcrit = ss1%Tornum_balcrit
     ELSE IF(ALLOCATED(ss2%Tornum_balcrit)) THEN
        CALL ps_conformal("Tornum_balcrit",ss2%Tornum_balcrit,ss3%Tornum_balcrit,iout,ierr)
        ss3%Tornum_balcrit = ss2%Tornum_balcrit
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Tor_mode_label)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%Tor_mode_label).AND.ALLOCATED(ss2%Tor_mode_label)) THEN
        CALL ps_diff("Tor_mode_label",ss1%Tor_mode_label,ss2%Tor_mode_label,icheck,iout,ierr)
        CALL ps_conformal("Tor_mode_label",ss1%Tor_mode_label,ss3%Tor_mode_label,iout,ierr)
        if(ierr.eq.0) ss3%Tor_mode_label = ss1%Tor_mode_label
     ELSE IF(ALLOCATED(ss1%Tor_mode_label)) THEN
        CALL ps_conformal("Tor_mode_label",ss1%Tor_mode_label,ss3%Tor_mode_label,iout,ierr)
        ss3%Tor_mode_label = ss1%Tor_mode_label
     ELSE IF(ALLOCATED(ss2%Tor_mode_label)) THEN
        CALL ps_conformal("Tor_mode_label",ss2%Tor_mode_label,ss3%Tor_mode_label,iout,ierr)
        ss3%Tor_mode_label = ss2%Tor_mode_label
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Tor_mode_No)).AND.(cclist(ps_cnum_LMHD).EQ.1)) THEN
     IF(ALLOCATED(ss1%Tor_mode_No).AND.ALLOCATED(ss2%Tor_mode_No)) THEN
        CALL ps_diff("Tor_mode_No",ss1%Tor_mode_No,ss2%Tor_mode_No,icheck,iout,ierr)
        CALL ps_conformal("Tor_mode_No",ss1%Tor_mode_No,ss3%Tor_mode_No,iout,ierr)
        if(ierr.eq.0) ss3%Tor_mode_No = ss1%Tor_mode_No
     ELSE IF(ALLOCATED(ss1%Tor_mode_No)) THEN
        CALL ps_conformal("Tor_mode_No",ss1%Tor_mode_No,ss3%Tor_mode_No,iout,ierr)
        ss3%Tor_mode_No = ss1%Tor_mode_No
     ELSE IF(ALLOCATED(ss2%Tor_mode_No)) THEN
        CALL ps_conformal("Tor_mode_No",ss2%Tor_mode_No,ss3%Tor_mode_No,iout,ierr)
        ss3%Tor_mode_No = ss2%Tor_mode_No
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tq0_reco)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%tq0_reco).AND.ALLOCATED(ss2%tq0_reco)) THEN
        CALL ps_conformal("tq0_reco",ss2%tq0_reco,ss3%tq0_reco,iout,ierr)
        CALL ps_conformal("tq0_reco",ss1%tq0_reco,ss3%tq0_reco,iout,ierr)
        if(ierr.eq.0) ss3%tq0_reco = w1*ss1%tq0_reco + (1.0_rspec-w1)*ss2%tq0_reco
     ELSE IF(ALLOCATED(ss1%tq0_reco)) THEN
        CALL ps_conformal("tq0_reco",ss1%tq0_reco,ss3%tq0_reco,iout,ierr)
        ss3%tq0_reco = ss1%tq0_reco
     ELSE IF(ALLOCATED(ss2%tq0_reco)) THEN
        CALL ps_conformal("tq0_reco",ss2%tq0_reco,ss3%tq0_reco,iout,ierr)
        ss3%tq0_reco = ss2%tq0_reco
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tqb0_halo)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%tqb0_halo).AND.ALLOCATED(ss2%tqb0_halo)) THEN
        CALL ps_conformal("tqb0_halo",ss2%tqb0_halo,ss3%tqb0_halo,iout,ierr)
        CALL ps_conformal("tqb0_halo",ss1%tqb0_halo,ss3%tqb0_halo,iout,ierr)
        if(ierr.eq.0) ss3%tqb0_halo = w1*ss1%tqb0_halo + (1.0_rspec-w1)*ss2%tqb0_halo
     ELSE IF(ALLOCATED(ss1%tqb0_halo)) THEN
        CALL ps_conformal("tqb0_halo",ss1%tqb0_halo,ss3%tqb0_halo,iout,ierr)
        ss3%tqb0_halo = ss1%tqb0_halo
     ELSE IF(ALLOCATED(ss2%tqb0_halo)) THEN
        CALL ps_conformal("tqb0_halo",ss2%tqb0_halo,ss3%tqb0_halo,iout,ierr)
        ss3%tqb0_halo = ss2%tqb0_halo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tqbe)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%tqbe).AND.ALLOCATED(ss2%tqbe)) THEN
        CALL ps_conformal("tqbe",ss2%tqbe,ss3%tqbe,iout,ierr)
        CALL ps_conformal("tqbe",ss1%tqbe,ss3%tqbe,iout,ierr)
        if(ierr.eq.0) ss3%tqbe = w1*ss1%tqbe + (1.0_rspec-w1)*ss2%tqbe
     ELSE IF(ALLOCATED(ss1%tqbe)) THEN
        CALL ps_conformal("tqbe",ss1%tqbe,ss3%tqbe,iout,ierr)
        ss3%tqbe = ss1%tqbe
     ELSE IF(ALLOCATED(ss2%tqbe)) THEN
        CALL ps_conformal("tqbe",ss2%tqbe,ss3%tqbe,iout,ierr)
        ss3%tqbe = ss2%tqbe
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tqbi)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%tqbi).AND.ALLOCATED(ss2%tqbi)) THEN
        CALL ps_conformal("tqbi",ss2%tqbi,ss3%tqbi,iout,ierr)
        CALL ps_conformal("tqbi",ss1%tqbi,ss3%tqbi,iout,ierr)
        if(ierr.eq.0) ss3%tqbi = w1*ss1%tqbi + (1.0_rspec-w1)*ss2%tqbi
     ELSE IF(ALLOCATED(ss1%tqbi)) THEN
        CALL ps_conformal("tqbi",ss1%tqbi,ss3%tqbi,iout,ierr)
        ss3%tqbi = ss1%tqbi
     ELSE IF(ALLOCATED(ss2%tqbi)) THEN
        CALL ps_conformal("tqbi",ss2%tqbi,ss3%tqbi,iout,ierr)
        ss3%tqbi = ss2%tqbi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tqbjxb)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%tqbjxb).AND.ALLOCATED(ss2%tqbjxb)) THEN
        CALL ps_conformal("tqbjxb",ss2%tqbjxb,ss3%tqbjxb,iout,ierr)
        CALL ps_conformal("tqbjxb",ss1%tqbjxb,ss3%tqbjxb,iout,ierr)
        if(ierr.eq.0) ss3%tqbjxb = w1*ss1%tqbjxb + (1.0_rspec-w1)*ss2%tqbjxb
     ELSE IF(ALLOCATED(ss1%tqbjxb)) THEN
        CALL ps_conformal("tqbjxb",ss1%tqbjxb,ss3%tqbjxb,iout,ierr)
        ss3%tqbjxb = ss1%tqbjxb
     ELSE IF(ALLOCATED(ss2%tqbjxb)) THEN
        CALL ps_conformal("tqbjxb",ss2%tqbjxb,ss3%tqbjxb,iout,ierr)
        ss3%tqbjxb = ss2%tqbjxb
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tqbth)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%tqbth).AND.ALLOCATED(ss2%tqbth)) THEN
        CALL ps_conformal("tqbth",ss2%tqbth,ss3%tqbth,iout,ierr)
        CALL ps_conformal("tqbth",ss1%tqbth,ss3%tqbth,iout,ierr)
        if(ierr.eq.0) ss3%tqbth = w1*ss1%tqbth + (1.0_rspec-w1)*ss2%tqbth
     ELSE IF(ALLOCATED(ss1%tqbth)) THEN
        CALL ps_conformal("tqbth",ss1%tqbth,ss3%tqbth,iout,ierr)
        ss3%tqbth = ss1%tqbth
     ELSE IF(ALLOCATED(ss2%tqbth)) THEN
        CALL ps_conformal("tqbth",ss2%tqbth,ss3%tqbth,iout,ierr)
        ss3%tqbth = ss2%tqbth
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tqcx_halo)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%tqcx_halo).AND.ALLOCATED(ss2%tqcx_halo)) THEN
        CALL ps_conformal("tqcx_halo",ss2%tqcx_halo,ss3%tqcx_halo,iout,ierr)
        CALL ps_conformal("tqcx_halo",ss1%tqcx_halo,ss3%tqcx_halo,iout,ierr)
        if(ierr.eq.0) ss3%tqcx_halo = w1*ss1%tqcx_halo + (1.0_rspec-w1)*ss2%tqcx_halo
     ELSE IF(ALLOCATED(ss1%tqcx_halo)) THEN
        CALL ps_conformal("tqcx_halo",ss1%tqcx_halo,ss3%tqcx_halo,iout,ierr)
        ss3%tqcx_halo = ss1%tqcx_halo
     ELSE IF(ALLOCATED(ss2%tqcx_halo)) THEN
        CALL ps_conformal("tqcx_halo",ss2%tqcx_halo,ss3%tqcx_halo,iout,ierr)
        ss3%tqcx_halo = ss2%tqcx_halo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tqcx_reco)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%tqcx_reco).AND.ALLOCATED(ss2%tqcx_reco)) THEN
        CALL ps_conformal("tqcx_reco",ss2%tqcx_reco,ss3%tqcx_reco,iout,ierr)
        CALL ps_conformal("tqcx_reco",ss1%tqcx_reco,ss3%tqcx_reco,iout,ierr)
        if(ierr.eq.0) ss3%tqcx_reco = w1*ss1%tqcx_reco + (1.0_rspec-w1)*ss2%tqcx_reco
     ELSE IF(ALLOCATED(ss1%tqcx_reco)) THEN
        CALL ps_conformal("tqcx_reco",ss1%tqcx_reco,ss3%tqcx_reco,iout,ierr)
        ss3%tqcx_reco = ss1%tqcx_reco
     ELSE IF(ALLOCATED(ss2%tqcx_reco)) THEN
        CALL ps_conformal("tqcx_reco",ss2%tqcx_reco,ss3%tqcx_reco,iout,ierr)
        ss3%tqcx_reco = ss2%tqcx_reco
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tqioniz)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%tqioniz).AND.ALLOCATED(ss2%tqioniz)) THEN
        CALL ps_conformal("tqioniz",ss2%tqioniz,ss3%tqioniz,iout,ierr)
        CALL ps_conformal("tqioniz",ss1%tqioniz,ss3%tqioniz,iout,ierr)
        if(ierr.eq.0) ss3%tqioniz = w1*ss1%tqioniz + (1.0_rspec-w1)*ss2%tqioniz
     ELSE IF(ALLOCATED(ss1%tqioniz)) THEN
        CALL ps_conformal("tqioniz",ss1%tqioniz,ss3%tqioniz,iout,ierr)
        ss3%tqioniz = ss1%tqioniz
     ELSE IF(ALLOCATED(ss2%tqioniz)) THEN
        CALL ps_conformal("tqioniz",ss2%tqioniz,ss3%tqioniz,iout,ierr)
        ss3%tqioniz = ss2%tqioniz
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tqqcx)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%tqqcx).AND.ALLOCATED(ss2%tqqcx)) THEN
        CALL ps_conformal("tqqcx",ss2%tqqcx,ss3%tqqcx,iout,ierr)
        CALL ps_conformal("tqqcx",ss1%tqqcx,ss3%tqqcx,iout,ierr)
        if(ierr.eq.0) ss3%tqqcx = w1*ss1%tqqcx + (1.0_rspec-w1)*ss2%tqqcx
     ELSE IF(ALLOCATED(ss1%tqqcx)) THEN
        CALL ps_conformal("tqqcx",ss1%tqqcx,ss3%tqqcx,iout,ierr)
        ss3%tqqcx = ss1%tqqcx
     ELSE IF(ALLOCATED(ss2%tqqcx)) THEN
        CALL ps_conformal("tqqcx",ss2%tqqcx,ss3%tqqcx,iout,ierr)
        ss3%tqqcx = ss2%tqqcx
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tqsc_halo)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%tqsc_halo).AND.ALLOCATED(ss2%tqsc_halo)) THEN
        CALL ps_conformal("tqsc_halo",ss2%tqsc_halo,ss3%tqsc_halo,iout,ierr)
        CALL ps_conformal("tqsc_halo",ss1%tqsc_halo,ss3%tqsc_halo,iout,ierr)
        if(ierr.eq.0) ss3%tqsc_halo = w1*ss1%tqsc_halo + (1.0_rspec-w1)*ss2%tqsc_halo
     ELSE IF(ALLOCATED(ss1%tqsc_halo)) THEN
        CALL ps_conformal("tqsc_halo",ss1%tqsc_halo,ss3%tqsc_halo,iout,ierr)
        ss3%tqsc_halo = ss1%tqsc_halo
     ELSE IF(ALLOCATED(ss2%tqsc_halo)) THEN
        CALL ps_conformal("tqsc_halo",ss2%tqsc_halo,ss3%tqsc_halo,iout,ierr)
        ss3%tqsc_halo = ss2%tqsc_halo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tqsc_reco)).AND.(cclist(ps_cnum_GAS).EQ.1)) THEN
     IF(ALLOCATED(ss1%tqsc_reco).AND.ALLOCATED(ss2%tqsc_reco)) THEN
        CALL ps_conformal("tqsc_reco",ss2%tqsc_reco,ss3%tqsc_reco,iout,ierr)
        CALL ps_conformal("tqsc_reco",ss1%tqsc_reco,ss3%tqsc_reco,iout,ierr)
        if(ierr.eq.0) ss3%tqsc_reco = w1*ss1%tqsc_reco + (1.0_rspec-w1)*ss2%tqsc_reco
     ELSE IF(ALLOCATED(ss1%tqsc_reco)) THEN
        CALL ps_conformal("tqsc_reco",ss1%tqsc_reco,ss3%tqsc_reco,iout,ierr)
        ss3%tqsc_reco = ss1%tqsc_reco
     ELSE IF(ALLOCATED(ss2%tqsc_reco)) THEN
        CALL ps_conformal("tqsc_reco",ss2%tqsc_reco,ss3%tqsc_reco,iout,ierr)
        ss3%tqsc_reco = ss2%tqsc_reco
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tq_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%tq_anom).AND.ALLOCATED(ss2%tq_anom)) THEN
        CALL ps_conformal("tq_anom",ss2%tq_anom,ss3%tq_anom,iout,ierr)
        CALL ps_conformal("tq_anom",ss1%tq_anom,ss3%tq_anom,iout,ierr)
        if(ierr.eq.0) ss3%tq_anom = w1*ss1%tq_anom + (1.0_rspec-w1)*ss2%tq_anom
     ELSE IF(ALLOCATED(ss1%tq_anom)) THEN
        CALL ps_conformal("tq_anom",ss1%tq_anom,ss3%tq_anom,iout,ierr)
        ss3%tq_anom = ss1%tq_anom
     ELSE IF(ALLOCATED(ss2%tq_anom)) THEN
        CALL ps_conformal("tq_anom",ss2%tq_anom,ss3%tq_anom,iout,ierr)
        ss3%tq_anom = ss2%tq_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tq_trans)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%tq_trans).AND.ALLOCATED(ss2%tq_trans)) THEN
        CALL ps_conformal("tq_trans",ss2%tq_trans,ss3%tq_trans,iout,ierr)
        CALL ps_conformal("tq_trans",ss1%tq_trans,ss3%tq_trans,iout,ierr)
        if(ierr.eq.0) ss3%tq_trans = w1*ss1%tq_trans + (1.0_rspec-w1)*ss2%tq_trans
     ELSE IF(ALLOCATED(ss1%tq_trans)) THEN
        CALL ps_conformal("tq_trans",ss1%tq_trans,ss3%tq_trans,iout,ierr)
        ss3%tq_trans = ss1%tq_trans
     ELSE IF(ALLOCATED(ss2%tq_trans)) THEN
        CALL ps_conformal("tq_trans",ss2%tq_trans,ss3%tq_trans,iout,ierr)
        ss3%tq_trans = ss2%tq_trans
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tq_transcat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%tq_transcat).AND.ALLOCATED(ss2%tq_transcat)) THEN
        CALL ps_conformal("tq_transcat",ss2%tq_transcat,ss3%tq_transcat,iout,ierr)
        CALL ps_conformal("tq_transcat",ss1%tq_transcat,ss3%tq_transcat,iout,ierr)
        if(ierr.eq.0) ss3%tq_transcat = w1*ss1%tq_transcat + (1.0_rspec-w1)*ss2%tq_transcat
     ELSE IF(ALLOCATED(ss1%tq_transcat)) THEN
        CALL ps_conformal("tq_transcat",ss1%tq_transcat,ss3%tq_transcat,iout,ierr)
        ss3%tq_transcat = ss1%tq_transcat
     ELSE IF(ALLOCATED(ss2%tq_transcat)) THEN
        CALL ps_conformal("tq_transcat",ss2%tq_transcat,ss3%tq_transcat,iout,ierr)
        ss3%tq_transcat = ss2%tq_transcat
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%trace_flag)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%trace_flag).AND.ALLOCATED(ss2%trace_flag)) THEN
        CALL ps_diff("trace_flag",ss1%trace_flag,ss2%trace_flag,icheck,iout,ierr)
        CALL ps_conformal("trace_flag",ss1%trace_flag,ss3%trace_flag,iout,ierr)
        if(ierr.eq.0) ss3%trace_flag = ss1%trace_flag
     ELSE IF(ALLOCATED(ss1%trace_flag)) THEN
        CALL ps_conformal("trace_flag",ss1%trace_flag,ss3%trace_flag,iout,ierr)
        ss3%trace_flag = ss1%trace_flag
     ELSE IF(ALLOCATED(ss2%trace_flag)) THEN
        CALL ps_conformal("trace_flag",ss2%trace_flag,ss3%trace_flag,iout,ierr)
        ss3%trace_flag = ss2%trace_flag
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%trcat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%trcat).AND.ALLOCATED(ss2%trcat)) THEN
        CALL ps_diff("trcat",ss1%trcat,ss2%trcat,icheck,iout,ierr)
        CALL ps_conformal("trcat",ss1%trcat,ss3%trcat,iout,ierr)
        if(ierr.eq.0) ss3%trcat = ss1%trcat
     ELSE IF(ALLOCATED(ss1%trcat)) THEN
        CALL ps_conformal("trcat",ss1%trcat,ss3%trcat,iout,ierr)
        ss3%trcat = ss1%trcat
     ELSE IF(ALLOCATED(ss2%trcat)) THEN
        CALL ps_conformal("trcat",ss2%trcat,ss3%trcat,iout,ierr)
        ss3%trcat = ss2%trcat
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%triang)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%triang).AND.ALLOCATED(ss2%triang)) THEN
        CALL ps_conformal("triang",ss2%triang,ss3%triang,iout,ierr)
        CALL ps_conformal("triang",ss1%triang,ss3%triang,iout,ierr)
        if(ierr.eq.0) ss3%triang = w1*ss1%triang + (1.0_rspec-w1)*ss2%triang
     ELSE IF(ALLOCATED(ss1%triang)) THEN
        CALL ps_conformal("triang",ss1%triang,ss3%triang,iout,ierr)
        ss3%triang = ss1%triang
     ELSE IF(ALLOCATED(ss2%triang)) THEN
        CALL ps_conformal("triang",ss2%triang,ss3%triang,iout,ierr)
        ss3%triang = ss2%triang
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%triangL)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%triangL).AND.ALLOCATED(ss2%triangL)) THEN
        CALL ps_conformal("triangL",ss2%triangL,ss3%triangL,iout,ierr)
        CALL ps_conformal("triangL",ss1%triangL,ss3%triangL,iout,ierr)
        if(ierr.eq.0) ss3%triangL = w1*ss1%triangL + (1.0_rspec-w1)*ss2%triangL
     ELSE IF(ALLOCATED(ss1%triangL)) THEN
        CALL ps_conformal("triangL",ss1%triangL,ss3%triangL,iout,ierr)
        ss3%triangL = ss1%triangL
     ELSE IF(ALLOCATED(ss2%triangL)) THEN
        CALL ps_conformal("triangL",ss2%triangL,ss3%triangL,iout,ierr)
        ss3%triangL = ss2%triangL
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%triangU)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%triangU).AND.ALLOCATED(ss2%triangU)) THEN
        CALL ps_conformal("triangU",ss2%triangU,ss3%triangU,iout,ierr)
        CALL ps_conformal("triangU",ss1%triangU,ss3%triangU,iout,ierr)
        if(ierr.eq.0) ss3%triangU = w1*ss1%triangU + (1.0_rspec-w1)*ss2%triangU
     ELSE IF(ALLOCATED(ss1%triangU)) THEN
        CALL ps_conformal("triangU",ss1%triangU,ss3%triangU,iout,ierr)
        ss3%triangU = ss1%triangU
     ELSE IF(ALLOCATED(ss2%triangU)) THEN
        CALL ps_conformal("triangU",ss2%triangU,ss3%triangU,iout,ierr)
        ss3%triangU = ss2%triangU
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%triang_miller_L)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%triang_miller_L).AND.ALLOCATED(ss2%triang_miller_L)) THEN
        CALL ps_conformal("triang_miller_L",ss2%triang_miller_L,ss3%triang_miller_L,iout,ierr)
        CALL ps_conformal("triang_miller_L",ss1%triang_miller_L,ss3%triang_miller_L,iout,ierr)
        if(ierr.eq.0) ss3%triang_miller_L = w1*ss1%triang_miller_L + (1.0_rspec-w1)*ss2%triang_miller_L
     ELSE IF(ALLOCATED(ss1%triang_miller_L)) THEN
        CALL ps_conformal("triang_miller_L",ss1%triang_miller_L,ss3%triang_miller_L,iout,ierr)
        ss3%triang_miller_L = ss1%triang_miller_L
     ELSE IF(ALLOCATED(ss2%triang_miller_L)) THEN
        CALL ps_conformal("triang_miller_L",ss2%triang_miller_L,ss3%triang_miller_L,iout,ierr)
        ss3%triang_miller_L = ss2%triang_miller_L
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%triang_miller_U)).AND.(cclist(ps_cnum_EQ).EQ.1)) THEN
     IF(ALLOCATED(ss1%triang_miller_U).AND.ALLOCATED(ss2%triang_miller_U)) THEN
        CALL ps_conformal("triang_miller_U",ss2%triang_miller_U,ss3%triang_miller_U,iout,ierr)
        CALL ps_conformal("triang_miller_U",ss1%triang_miller_U,ss3%triang_miller_U,iout,ierr)
        if(ierr.eq.0) ss3%triang_miller_U = w1*ss1%triang_miller_U + (1.0_rspec-w1)*ss2%triang_miller_U
     ELSE IF(ALLOCATED(ss1%triang_miller_U)) THEN
        CALL ps_conformal("triang_miller_U",ss1%triang_miller_U,ss3%triang_miller_U,iout,ierr)
        ss3%triang_miller_U = ss1%triang_miller_U
     ELSE IF(ALLOCATED(ss2%triang_miller_U)) THEN
        CALL ps_conformal("triang_miller_U",ss2%triang_miller_U,ss3%triang_miller_U,iout,ierr)
        ss3%triang_miller_U = ss2%triang_miller_U
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%trmodels)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%trmodels).AND.ALLOCATED(ss2%trmodels)) THEN
        CALL ps_diff("trmodels",ss1%trmodels,ss2%trmodels,icheck,iout,ierr)
        CALL ps_conformal("trmodels",ss1%trmodels,ss3%trmodels,iout,ierr)
        if(ierr.eq.0) ss3%trmodels = ss1%trmodels
     ELSE IF(ALLOCATED(ss1%trmodels)) THEN
        CALL ps_conformal("trmodels",ss1%trmodels,ss3%trmodels,iout,ierr)
        ss3%trmodels = ss1%trmodels
     ELSE IF(ALLOCATED(ss2%trmodels)) THEN
        CALL ps_conformal("trmodels",ss2%trmodels,ss3%trmodels,iout,ierr)
        ss3%trmodels = ss2%trmodels
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%Ts)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%Ts).AND.ALLOCATED(ss2%Ts)) THEN
        CALL ps_conformal("Ts",ss2%Ts,ss3%Ts,iout,ierr)
        CALL ps_conformal("Ts",ss1%Ts,ss3%Ts,iout,ierr)
        if(ierr.eq.0) ss3%Ts = w1*ss1%Ts + (1.0_rspec-w1)*ss2%Ts
     ELSE IF(ALLOCATED(ss1%Ts)) THEN
        CALL ps_conformal("Ts",ss1%Ts,ss3%Ts,iout,ierr)
        ss3%Ts = ss1%Ts
     ELSE IF(ALLOCATED(ss2%Ts)) THEN
        CALL ps_conformal("Ts",ss2%Ts,ss3%Ts,iout,ierr)
        ss3%Ts = ss2%Ts
     ENDIF
  ENDIF

  IF(cclist(ps_cnum_PLASMA).EQ.1) THEN

     CALL ps_diff("Ts_Data_Info",ss1%Ts_Data_Info,ss2%Ts_Data_Info,icheck,iout,ierr)
     ss3%Ts_Data_Info = ss1%Ts_Data_Info
  ENDIF

  IF((ALLOCATED(ss3%Ts_is_input)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%Ts_is_input).AND.ALLOCATED(ss2%Ts_is_input)) THEN
        CALL ps_diff("Ts_is_input",ss1%Ts_is_input,ss2%Ts_is_input,icheck,iout,ierr)
        CALL ps_conformal("Ts_is_input",ss1%Ts_is_input,ss3%Ts_is_input,iout,ierr)
        if(ierr.eq.0) ss3%Ts_is_input = ss1%Ts_is_input
     ELSE IF(ALLOCATED(ss1%Ts_is_input)) THEN
        CALL ps_conformal("Ts_is_input",ss1%Ts_is_input,ss3%Ts_is_input,iout,ierr)
        ss3%Ts_is_input = ss1%Ts_is_input
     ELSE IF(ALLOCATED(ss2%Ts_is_input)) THEN
        CALL ps_conformal("Ts_is_input",ss2%Ts_is_input,ss3%Ts_is_input,iout,ierr)
        ss3%Ts_is_input = ss2%Ts_is_input
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tzflux_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%tzflux_ky_tglf).AND.ALLOCATED(ss2%tzflux_ky_tglf)) THEN
        CALL ps_conformal("tzflux_ky_tglf",ss2%tzflux_ky_tglf,ss3%tzflux_ky_tglf,iout,ierr)
        CALL ps_conformal("tzflux_ky_tglf",ss1%tzflux_ky_tglf,ss3%tzflux_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%tzflux_ky_tglf = w1*ss1%tzflux_ky_tglf + (1.0_rspec-w1)*ss2%tzflux_ky_tglf
     ELSE IF(ALLOCATED(ss1%tzflux_ky_tglf)) THEN
        CALL ps_conformal("tzflux_ky_tglf",ss1%tzflux_ky_tglf,ss3%tzflux_ky_tglf,iout,ierr)
        ss3%tzflux_ky_tglf = ss1%tzflux_ky_tglf
     ELSE IF(ALLOCATED(ss2%tzflux_ky_tglf)) THEN
        CALL ps_conformal("tzflux_ky_tglf",ss2%tzflux_ky_tglf,ss3%tzflux_ky_tglf,iout,ierr)
        ss3%tzflux_ky_tglf = ss2%tzflux_ky_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%tz_ky_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%tz_ky_tglf).AND.ALLOCATED(ss2%tz_ky_tglf)) THEN
        CALL ps_conformal("tz_ky_tglf",ss2%tz_ky_tglf,ss3%tz_ky_tglf,iout,ierr)
        CALL ps_conformal("tz_ky_tglf",ss1%tz_ky_tglf,ss3%tz_ky_tglf,iout,ierr)
        if(ierr.eq.0) ss3%tz_ky_tglf = w1*ss1%tz_ky_tglf + (1.0_rspec-w1)*ss2%tz_ky_tglf
     ELSE IF(ALLOCATED(ss1%tz_ky_tglf)) THEN
        CALL ps_conformal("tz_ky_tglf",ss1%tz_ky_tglf,ss3%tz_ky_tglf,iout,ierr)
        ss3%tz_ky_tglf = ss1%tz_ky_tglf
     ELSE IF(ALLOCATED(ss2%tz_ky_tglf)) THEN
        CALL ps_conformal("tz_ky_tglf",ss2%tz_ky_tglf,ss3%tz_ky_tglf,iout,ierr)
        ss3%tz_ky_tglf = ss2%tz_ky_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%T_einj_standard)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%T_einj_standard).AND.ALLOCATED(ss2%T_einj_standard)) THEN
        CALL ps_diff("T_einj_standard",ss1%T_einj_standard,ss2%T_einj_standard,icheck,iout,ierr)
        CALL ps_conformal("T_einj_standard",ss1%T_einj_standard,ss3%T_einj_standard,iout,ierr)
        if(ierr.eq.0) ss3%T_einj_standard = ss1%T_einj_standard
     ELSE IF(ALLOCATED(ss1%T_einj_standard)) THEN
        CALL ps_conformal("T_einj_standard",ss1%T_einj_standard,ss3%T_einj_standard,iout,ierr)
        ss3%T_einj_standard = ss1%T_einj_standard
     ELSE IF(ALLOCATED(ss2%T_einj_standard)) THEN
        CALL ps_conformal("T_einj_standard",ss2%T_einj_standard,ss3%T_einj_standard,iout,ierr)
        ss3%T_einj_standard = ss2%T_einj_standard
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%T_ffull_standard)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%T_ffull_standard).AND.ALLOCATED(ss2%T_ffull_standard)) THEN
        CALL ps_diff("T_ffull_standard",ss1%T_ffull_standard,ss2%T_ffull_standard,icheck,iout,ierr)
        CALL ps_conformal("T_ffull_standard",ss1%T_ffull_standard,ss3%T_ffull_standard,iout,ierr)
        if(ierr.eq.0) ss3%T_ffull_standard = ss1%T_ffull_standard
     ELSE IF(ALLOCATED(ss1%T_ffull_standard)) THEN
        CALL ps_conformal("T_ffull_standard",ss1%T_ffull_standard,ss3%T_ffull_standard,iout,ierr)
        ss3%T_ffull_standard = ss1%T_ffull_standard
     ELSE IF(ALLOCATED(ss2%T_ffull_standard)) THEN
        CALL ps_conformal("T_ffull_standard",ss2%T_ffull_standard,ss3%T_ffull_standard,iout,ierr)
        ss3%T_ffull_standard = ss2%T_ffull_standard
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%T_fhalf_standard)).AND.(cclist(ps_cnum_NBI).EQ.1)) THEN
     IF(ALLOCATED(ss1%T_fhalf_standard).AND.ALLOCATED(ss2%T_fhalf_standard)) THEN
        CALL ps_diff("T_fhalf_standard",ss1%T_fhalf_standard,ss2%T_fhalf_standard,icheck,iout,ierr)
        CALL ps_conformal("T_fhalf_standard",ss1%T_fhalf_standard,ss3%T_fhalf_standard,iout,ierr)
        if(ierr.eq.0) ss3%T_fhalf_standard = ss1%T_fhalf_standard
     ELSE IF(ALLOCATED(ss1%T_fhalf_standard)) THEN
        CALL ps_conformal("T_fhalf_standard",ss1%T_fhalf_standard,ss3%T_fhalf_standard,iout,ierr)
        ss3%T_fhalf_standard = ss1%T_fhalf_standard
     ELSE IF(ALLOCATED(ss2%T_fhalf_standard)) THEN
        CALL ps_conformal("T_fhalf_standard",ss2%T_fhalf_standard,ss3%T_fhalf_standard,iout,ierr)
        ss3%T_fhalf_standard = ss2%T_fhalf_standard
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%upwind_pfrac_ns)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%upwind_pfrac_ns).AND.ALLOCATED(ss2%upwind_pfrac_ns)) THEN
        CALL ps_conformal("upwind_pfrac_ns",ss2%upwind_pfrac_ns,ss3%upwind_pfrac_ns,iout,ierr)
        CALL ps_conformal("upwind_pfrac_ns",ss1%upwind_pfrac_ns,ss3%upwind_pfrac_ns,iout,ierr)
        if(ierr.eq.0) ss3%upwind_pfrac_ns = w1*ss1%upwind_pfrac_ns + (1.0_rspec-w1)*ss2%upwind_pfrac_ns
     ELSE IF(ALLOCATED(ss1%upwind_pfrac_ns)) THEN
        CALL ps_conformal("upwind_pfrac_ns",ss1%upwind_pfrac_ns,ss3%upwind_pfrac_ns,iout,ierr)
        ss3%upwind_pfrac_ns = ss1%upwind_pfrac_ns
     ELSE IF(ALLOCATED(ss2%upwind_pfrac_ns)) THEN
        CALL ps_conformal("upwind_pfrac_ns",ss2%upwind_pfrac_ns,ss3%upwind_pfrac_ns,iout,ierr)
        ss3%upwind_pfrac_ns = ss2%upwind_pfrac_ns
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%upwind_pfrac_omega)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%upwind_pfrac_omega).AND.ALLOCATED(ss2%upwind_pfrac_omega)) THEN
        CALL ps_conformal("upwind_pfrac_omega",ss2%upwind_pfrac_omega,ss3%upwind_pfrac_omega,iout,ierr)
        CALL ps_conformal("upwind_pfrac_omega",ss1%upwind_pfrac_omega,ss3%upwind_pfrac_omega,iout,ierr)
        if(ierr.eq.0) ss3%upwind_pfrac_omega = w1*ss1%upwind_pfrac_omega + (1.0_rspec-w1)*ss2%upwind_pfrac_omega
     ELSE IF(ALLOCATED(ss1%upwind_pfrac_omega)) THEN
        CALL ps_conformal("upwind_pfrac_omega",ss1%upwind_pfrac_omega,ss3%upwind_pfrac_omega,iout,ierr)
        ss3%upwind_pfrac_omega = ss1%upwind_pfrac_omega
     ELSE IF(ALLOCATED(ss2%upwind_pfrac_omega)) THEN
        CALL ps_conformal("upwind_pfrac_omega",ss2%upwind_pfrac_omega,ss3%upwind_pfrac_omega,iout,ierr)
        ss3%upwind_pfrac_omega = ss2%upwind_pfrac_omega
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%upwind_pfrac_Te)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%upwind_pfrac_Te).AND.ALLOCATED(ss2%upwind_pfrac_Te)) THEN
        CALL ps_conformal("upwind_pfrac_Te",ss2%upwind_pfrac_Te,ss3%upwind_pfrac_Te,iout,ierr)
        CALL ps_conformal("upwind_pfrac_Te",ss1%upwind_pfrac_Te,ss3%upwind_pfrac_Te,iout,ierr)
        if(ierr.eq.0) ss3%upwind_pfrac_Te = w1*ss1%upwind_pfrac_Te + (1.0_rspec-w1)*ss2%upwind_pfrac_Te
     ELSE IF(ALLOCATED(ss1%upwind_pfrac_Te)) THEN
        CALL ps_conformal("upwind_pfrac_Te",ss1%upwind_pfrac_Te,ss3%upwind_pfrac_Te,iout,ierr)
        ss3%upwind_pfrac_Te = ss1%upwind_pfrac_Te
     ELSE IF(ALLOCATED(ss2%upwind_pfrac_Te)) THEN
        CALL ps_conformal("upwind_pfrac_Te",ss2%upwind_pfrac_Te,ss3%upwind_pfrac_Te,iout,ierr)
        ss3%upwind_pfrac_Te = ss2%upwind_pfrac_Te
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%upwind_pfrac_Ti)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%upwind_pfrac_Ti).AND.ALLOCATED(ss2%upwind_pfrac_Ti)) THEN
        CALL ps_conformal("upwind_pfrac_Ti",ss2%upwind_pfrac_Ti,ss3%upwind_pfrac_Ti,iout,ierr)
        CALL ps_conformal("upwind_pfrac_Ti",ss1%upwind_pfrac_Ti,ss3%upwind_pfrac_Ti,iout,ierr)
        if(ierr.eq.0) ss3%upwind_pfrac_Ti = w1*ss1%upwind_pfrac_Ti + (1.0_rspec-w1)*ss2%upwind_pfrac_Ti
     ELSE IF(ALLOCATED(ss1%upwind_pfrac_Ti)) THEN
        CALL ps_conformal("upwind_pfrac_Ti",ss1%upwind_pfrac_Ti,ss3%upwind_pfrac_Ti,iout,ierr)
        ss3%upwind_pfrac_Ti = ss1%upwind_pfrac_Ti
     ELSE IF(ALLOCATED(ss2%upwind_pfrac_Ti)) THEN
        CALL ps_conformal("upwind_pfrac_Ti",ss2%upwind_pfrac_Ti,ss3%upwind_pfrac_Ti,iout,ierr)
        ss3%upwind_pfrac_Ti = ss2%upwind_pfrac_Ti
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vee_trans)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vee_trans).AND.ALLOCATED(ss2%vee_trans)) THEN
        CALL ps_conformal("vee_trans",ss2%vee_trans,ss3%vee_trans,iout,ierr)
        CALL ps_conformal("vee_trans",ss1%vee_trans,ss3%vee_trans,iout,ierr)
        if(ierr.eq.0) ss3%vee_trans = w1*ss1%vee_trans + (1.0_rspec-w1)*ss2%vee_trans
     ELSE IF(ALLOCATED(ss1%vee_trans)) THEN
        CALL ps_conformal("vee_trans",ss1%vee_trans,ss3%vee_trans,iout,ierr)
        ss3%vee_trans = ss1%vee_trans
     ELSE IF(ALLOCATED(ss2%vee_trans)) THEN
        CALL ps_conformal("vee_trans",ss2%vee_trans,ss3%vee_trans,iout,ierr)
        ss3%vee_trans = ss2%vee_trans
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%vee_transcat)).AND.(cclist(ps_cnum_PLASMA).EQ.1)) THEN
     IF(ALLOCATED(ss1%vee_transcat).AND.ALLOCATED(ss2%vee_transcat)) THEN
        CALL ps_conformal("vee_transcat",ss2%vee_transcat,ss3%vee_transcat,iout,ierr)
        CALL ps_conformal("vee_transcat",ss1%vee_transcat,ss3%vee_transcat,iout,ierr)
        if(ierr.eq.0) ss3%vee_transcat = w1*ss1%vee_transcat + (1.0_rspec-w1)*ss2%vee_transcat
     ELSE IF(ALLOCATED(ss1%vee_transcat)) THEN
        CALL ps_conformal("vee_transcat",ss1%vee_transcat,ss3%vee_transcat,iout,ierr)
        ss3%vee_transcat = ss1%vee_transcat
     ELSE IF(ALLOCATED(ss2%vee_transcat)) THEN
        CALL ps_conformal("vee_transcat",ss2%vee_transcat,ss3%vee_transcat,iout,ierr)
        ss3%vee_transcat = ss2%vee_transcat
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velb_fusi)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velb_fusi).AND.ALLOCATED(ss2%velb_fusi)) THEN
        CALL ps_conformal("velb_fusi",ss2%velb_fusi,ss3%velb_fusi,iout,ierr)
        CALL ps_conformal("velb_fusi",ss1%velb_fusi,ss3%velb_fusi,iout,ierr)
        if(ierr.eq.0) ss3%velb_fusi = w1*ss1%velb_fusi + (1.0_rspec-w1)*ss2%velb_fusi
     ELSE IF(ALLOCATED(ss1%velb_fusi)) THEN
        CALL ps_conformal("velb_fusi",ss1%velb_fusi,ss3%velb_fusi,iout,ierr)
        ss3%velb_fusi = ss1%velb_fusi
     ELSE IF(ALLOCATED(ss2%velb_fusi)) THEN
        CALL ps_conformal("velb_fusi",ss2%velb_fusi,ss3%velb_fusi,iout,ierr)
        ss3%velb_fusi = ss2%velb_fusi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velb_nbi)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velb_nbi).AND.ALLOCATED(ss2%velb_nbi)) THEN
        CALL ps_conformal("velb_nbi",ss2%velb_nbi,ss3%velb_nbi,iout,ierr)
        CALL ps_conformal("velb_nbi",ss1%velb_nbi,ss3%velb_nbi,iout,ierr)
        if(ierr.eq.0) ss3%velb_nbi = w1*ss1%velb_nbi + (1.0_rspec-w1)*ss2%velb_nbi
     ELSE IF(ALLOCATED(ss1%velb_nbi)) THEN
        CALL ps_conformal("velb_nbi",ss1%velb_nbi,ss3%velb_nbi,iout,ierr)
        ss3%velb_nbi = ss1%velb_nbi
     ELSE IF(ALLOCATED(ss2%velb_nbi)) THEN
        CALL ps_conformal("velb_nbi",ss2%velb_nbi,ss3%velb_nbi,iout,ierr)
        ss3%velb_nbi = ss2%velb_nbi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velb_rfmi)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velb_rfmi).AND.ALLOCATED(ss2%velb_rfmi)) THEN
        CALL ps_conformal("velb_rfmi",ss2%velb_rfmi,ss3%velb_rfmi,iout,ierr)
        CALL ps_conformal("velb_rfmi",ss1%velb_rfmi,ss3%velb_rfmi,iout,ierr)
        if(ierr.eq.0) ss3%velb_rfmi = w1*ss1%velb_rfmi + (1.0_rspec-w1)*ss2%velb_rfmi
     ELSE IF(ALLOCATED(ss1%velb_rfmi)) THEN
        CALL ps_conformal("velb_rfmi",ss1%velb_rfmi,ss3%velb_rfmi,iout,ierr)
        ss3%velb_rfmi = ss1%velb_rfmi
     ELSE IF(ALLOCATED(ss2%velb_rfmi)) THEN
        CALL ps_conformal("velb_rfmi",ss2%velb_rfmi,ss3%velb_rfmi,iout,ierr)
        ss3%velb_rfmi = ss2%velb_rfmi
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velne_nclass)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velne_nclass).AND.ALLOCATED(ss2%velne_nclass)) THEN
        CALL ps_conformal("velne_nclass",ss2%velne_nclass,ss3%velne_nclass,iout,ierr)
        CALL ps_conformal("velne_nclass",ss1%velne_nclass,ss3%velne_nclass,iout,ierr)
        if(ierr.eq.0) ss3%velne_nclass = w1*ss1%velne_nclass + (1.0_rspec-w1)*ss2%velne_nclass
     ELSE IF(ALLOCATED(ss1%velne_nclass)) THEN
        CALL ps_conformal("velne_nclass",ss1%velne_nclass,ss3%velne_nclass,iout,ierr)
        ss3%velne_nclass = ss1%velne_nclass
     ELSE IF(ALLOCATED(ss2%velne_nclass)) THEN
        CALL ps_conformal("velne_nclass",ss2%velne_nclass,ss3%velne_nclass,iout,ierr)
        ss3%velne_nclass = ss2%velne_nclass
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velnh_nclass)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velnh_nclass).AND.ALLOCATED(ss2%velnh_nclass)) THEN
        CALL ps_conformal("velnh_nclass",ss2%velnh_nclass,ss3%velnh_nclass,iout,ierr)
        CALL ps_conformal("velnh_nclass",ss1%velnh_nclass,ss3%velnh_nclass,iout,ierr)
        if(ierr.eq.0) ss3%velnh_nclass = w1*ss1%velnh_nclass + (1.0_rspec-w1)*ss2%velnh_nclass
     ELSE IF(ALLOCATED(ss1%velnh_nclass)) THEN
        CALL ps_conformal("velnh_nclass",ss1%velnh_nclass,ss3%velnh_nclass,iout,ierr)
        ss3%velnh_nclass = ss1%velnh_nclass
     ELSE IF(ALLOCATED(ss2%velnh_nclass)) THEN
        CALL ps_conformal("velnh_nclass",ss2%velnh_nclass,ss3%velnh_nclass,iout,ierr)
        ss3%velnh_nclass = ss2%velnh_nclass
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velni_drbm)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velni_drbm).AND.ALLOCATED(ss2%velni_drbm)) THEN
        CALL ps_conformal("velni_drbm",ss2%velni_drbm,ss3%velni_drbm,iout,ierr)
        CALL ps_conformal("velni_drbm",ss1%velni_drbm,ss3%velni_drbm,iout,ierr)
        if(ierr.eq.0) ss3%velni_drbm = w1*ss1%velni_drbm + (1.0_rspec-w1)*ss2%velni_drbm
     ELSE IF(ALLOCATED(ss1%velni_drbm)) THEN
        CALL ps_conformal("velni_drbm",ss1%velni_drbm,ss3%velni_drbm,iout,ierr)
        ss3%velni_drbm = ss1%velni_drbm
     ELSE IF(ALLOCATED(ss2%velni_drbm)) THEN
        CALL ps_conformal("velni_drbm",ss2%velni_drbm,ss3%velni_drbm,iout,ierr)
        ss3%velni_drbm = ss2%velni_drbm
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velns_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velns_anom).AND.ALLOCATED(ss2%velns_anom)) THEN
        CALL ps_conformal("velns_anom",ss2%velns_anom,ss3%velns_anom,iout,ierr)
        CALL ps_conformal("velns_anom",ss1%velns_anom,ss3%velns_anom,iout,ierr)
        if(ierr.eq.0) ss3%velns_anom = w1*ss1%velns_anom + (1.0_rspec-w1)*ss2%velns_anom
     ELSE IF(ALLOCATED(ss1%velns_anom)) THEN
        CALL ps_conformal("velns_anom",ss1%velns_anom,ss3%velns_anom,iout,ierr)
        ss3%velns_anom = ss1%velns_anom
     ELSE IF(ALLOCATED(ss2%velns_anom)) THEN
        CALL ps_conformal("velns_anom",ss2%velns_anom,ss3%velns_anom,iout,ierr)
        ss3%velns_anom = ss2%velns_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velns_glf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velns_glf).AND.ALLOCATED(ss2%velns_glf)) THEN
        CALL ps_conformal("velns_glf",ss2%velns_glf,ss3%velns_glf,iout,ierr)
        CALL ps_conformal("velns_glf",ss1%velns_glf,ss3%velns_glf,iout,ierr)
        if(ierr.eq.0) ss3%velns_glf = w1*ss1%velns_glf + (1.0_rspec-w1)*ss2%velns_glf
     ELSE IF(ALLOCATED(ss1%velns_glf)) THEN
        CALL ps_conformal("velns_glf",ss1%velns_glf,ss3%velns_glf,iout,ierr)
        ss3%velns_glf = ss1%velns_glf
     ELSE IF(ALLOCATED(ss2%velns_glf)) THEN
        CALL ps_conformal("velns_glf",ss2%velns_glf,ss3%velns_glf,iout,ierr)
        ss3%velns_glf = ss2%velns_glf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velns_gtcneo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velns_gtcneo).AND.ALLOCATED(ss2%velns_gtcneo)) THEN
        CALL ps_conformal("velns_gtcneo",ss2%velns_gtcneo,ss3%velns_gtcneo,iout,ierr)
        CALL ps_conformal("velns_gtcneo",ss1%velns_gtcneo,ss3%velns_gtcneo,iout,ierr)
        if(ierr.eq.0) ss3%velns_gtcneo = w1*ss1%velns_gtcneo + (1.0_rspec-w1)*ss2%velns_gtcneo
     ELSE IF(ALLOCATED(ss1%velns_gtcneo)) THEN
        CALL ps_conformal("velns_gtcneo",ss1%velns_gtcneo,ss3%velns_gtcneo,iout,ierr)
        ss3%velns_gtcneo = ss1%velns_gtcneo
     ELSE IF(ALLOCATED(ss2%velns_gtcneo)) THEN
        CALL ps_conformal("velns_gtcneo",ss2%velns_gtcneo,ss3%velns_gtcneo,iout,ierr)
        ss3%velns_gtcneo = ss2%velns_gtcneo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velns_mmm71)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velns_mmm71).AND.ALLOCATED(ss2%velns_mmm71)) THEN
        CALL ps_conformal("velns_mmm71",ss2%velns_mmm71,ss3%velns_mmm71,iout,ierr)
        CALL ps_conformal("velns_mmm71",ss1%velns_mmm71,ss3%velns_mmm71,iout,ierr)
        if(ierr.eq.0) ss3%velns_mmm71 = w1*ss1%velns_mmm71 + (1.0_rspec-w1)*ss2%velns_mmm71
     ELSE IF(ALLOCATED(ss1%velns_mmm71)) THEN
        CALL ps_conformal("velns_mmm71",ss1%velns_mmm71,ss3%velns_mmm71,iout,ierr)
        ss3%velns_mmm71 = ss1%velns_mmm71
     ELSE IF(ALLOCATED(ss2%velns_mmm71)) THEN
        CALL ps_conformal("velns_mmm71",ss2%velns_mmm71,ss3%velns_mmm71,iout,ierr)
        ss3%velns_mmm71 = ss2%velns_mmm71
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velns_nclass)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velns_nclass).AND.ALLOCATED(ss2%velns_nclass)) THEN
        CALL ps_conformal("velns_nclass",ss2%velns_nclass,ss3%velns_nclass,iout,ierr)
        CALL ps_conformal("velns_nclass",ss1%velns_nclass,ss3%velns_nclass,iout,ierr)
        if(ierr.eq.0) ss3%velns_nclass = w1*ss1%velns_nclass + (1.0_rspec-w1)*ss2%velns_nclass
     ELSE IF(ALLOCATED(ss1%velns_nclass)) THEN
        CALL ps_conformal("velns_nclass",ss1%velns_nclass,ss3%velns_nclass,iout,ierr)
        ss3%velns_nclass = ss1%velns_nclass
     ELSE IF(ALLOCATED(ss2%velns_nclass)) THEN
        CALL ps_conformal("velns_nclass",ss2%velns_nclass,ss3%velns_nclass,iout,ierr)
        ss3%velns_nclass = ss2%velns_nclass
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velns_neo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velns_neo).AND.ALLOCATED(ss2%velns_neo)) THEN
        CALL ps_conformal("velns_neo",ss2%velns_neo,ss3%velns_neo,iout,ierr)
        CALL ps_conformal("velns_neo",ss1%velns_neo,ss3%velns_neo,iout,ierr)
        if(ierr.eq.0) ss3%velns_neo = w1*ss1%velns_neo + (1.0_rspec-w1)*ss2%velns_neo
     ELSE IF(ALLOCATED(ss1%velns_neo)) THEN
        CALL ps_conformal("velns_neo",ss1%velns_neo,ss3%velns_neo,iout,ierr)
        ss3%velns_neo = ss1%velns_neo
     ELSE IF(ALLOCATED(ss2%velns_neo)) THEN
        CALL ps_conformal("velns_neo",ss2%velns_neo,ss3%velns_neo,iout,ierr)
        ss3%velns_neo = ss2%velns_neo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velns_tglf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velns_tglf).AND.ALLOCATED(ss2%velns_tglf)) THEN
        CALL ps_conformal("velns_tglf",ss2%velns_tglf,ss3%velns_tglf,iout,ierr)
        CALL ps_conformal("velns_tglf",ss1%velns_tglf,ss3%velns_tglf,iout,ierr)
        if(ierr.eq.0) ss3%velns_tglf = w1*ss1%velns_tglf + (1.0_rspec-w1)*ss2%velns_tglf
     ELSE IF(ALLOCATED(ss1%velns_tglf)) THEN
        CALL ps_conformal("velns_tglf",ss1%velns_tglf,ss3%velns_tglf,iout,ierr)
        ss3%velns_tglf = ss1%velns_tglf
     ELSE IF(ALLOCATED(ss2%velns_tglf)) THEN
        CALL ps_conformal("velns_tglf",ss2%velns_tglf,ss3%velns_tglf,iout,ierr)
        ss3%velns_tglf = ss2%velns_tglf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velnx_nclass)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velnx_nclass).AND.ALLOCATED(ss2%velnx_nclass)) THEN
        CALL ps_conformal("velnx_nclass",ss2%velnx_nclass,ss3%velnx_nclass,iout,ierr)
        CALL ps_conformal("velnx_nclass",ss1%velnx_nclass,ss3%velnx_nclass,iout,ierr)
        if(ierr.eq.0) ss3%velnx_nclass = w1*ss1%velnx_nclass + (1.0_rspec-w1)*ss2%velnx_nclass
     ELSE IF(ALLOCATED(ss1%velnx_nclass)) THEN
        CALL ps_conformal("velnx_nclass",ss1%velnx_nclass,ss3%velnx_nclass,iout,ierr)
        ss3%velnx_nclass = ss1%velnx_nclass
     ELSE IF(ALLOCATED(ss2%velnx_nclass)) THEN
        CALL ps_conformal("velnx_nclass",ss2%velnx_nclass,ss3%velnx_nclass,iout,ierr)
        ss3%velnx_nclass = ss2%velnx_nclass
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velpphi_anom)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velpphi_anom).AND.ALLOCATED(ss2%velpphi_anom)) THEN
        CALL ps_conformal("velpphi_anom",ss2%velpphi_anom,ss3%velpphi_anom,iout,ierr)
        CALL ps_conformal("velpphi_anom",ss1%velpphi_anom,ss3%velpphi_anom,iout,ierr)
        if(ierr.eq.0) ss3%velpphi_anom = w1*ss1%velpphi_anom + (1.0_rspec-w1)*ss2%velpphi_anom
     ELSE IF(ALLOCATED(ss1%velpphi_anom)) THEN
        CALL ps_conformal("velpphi_anom",ss1%velpphi_anom,ss3%velpphi_anom,iout,ierr)
        ss3%velpphi_anom = ss1%velpphi_anom
     ELSE IF(ALLOCATED(ss2%velpphi_anom)) THEN
        CALL ps_conformal("velpphi_anom",ss2%velpphi_anom,ss3%velpphi_anom,iout,ierr)
        ss3%velpphi_anom = ss2%velpphi_anom
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velpphi_glf)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velpphi_glf).AND.ALLOCATED(ss2%velpphi_glf)) THEN
        CALL ps_conformal("velpphi_glf",ss2%velpphi_glf,ss3%velpphi_glf,iout,ierr)
        CALL ps_conformal("velpphi_glf",ss1%velpphi_glf,ss3%velpphi_glf,iout,ierr)
        if(ierr.eq.0) ss3%velpphi_glf = w1*ss1%velpphi_glf + (1.0_rspec-w1)*ss2%velpphi_glf
     ELSE IF(ALLOCATED(ss1%velpphi_glf)) THEN
        CALL ps_conformal("velpphi_glf",ss1%velpphi_glf,ss3%velpphi_glf,iout,ierr)
        ss3%velpphi_glf = ss1%velpphi_glf
     ELSE IF(ALLOCATED(ss2%velpphi_glf)) THEN
        CALL ps_conformal("velpphi_glf",ss2%velpphi_glf,ss3%velpphi_glf,iout,ierr)
        ss3%velpphi_glf = ss2%velpphi_glf
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velpphi_gtcneo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velpphi_gtcneo).AND.ALLOCATED(ss2%velpphi_gtcneo)) THEN
        CALL ps_conformal("velpphi_gtcneo",ss2%velpphi_gtcneo,ss3%velpphi_gtcneo,iout,ierr)
        CALL ps_conformal("velpphi_gtcneo",ss1%velpphi_gtcneo,ss3%velpphi_gtcneo,iout,ierr)
        if(ierr.eq.0) ss3%velpphi_gtcneo = w1*ss1%velpphi_gtcneo + (1.0_rspec-w1)*ss2%velpphi_gtcneo
     ELSE IF(ALLOCATED(ss1%velpphi_gtcneo)) THEN
        CALL ps_conformal("velpphi_gtcneo",ss1%velpphi_gtcneo,ss3%velpphi_gtcneo,iout,ierr)
        ss3%velpphi_gtcneo = ss1%velpphi_gtcneo
     ELSE IF(ALLOCATED(ss2%velpphi_gtcneo)) THEN
        CALL ps_conformal("velpphi_gtcneo",ss2%velpphi_gtcneo,ss3%velpphi_gtcneo,iout,ierr)
        ss3%velpphi_gtcneo = ss2%velpphi_gtcneo
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velpphi_mmm71)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velpphi_mmm71).AND.ALLOCATED(ss2%velpphi_mmm71)) THEN
        CALL ps_conformal("velpphi_mmm71",ss2%velpphi_mmm71,ss3%velpphi_mmm71,iout,ierr)
        CALL ps_conformal("velpphi_mmm71",ss1%velpphi_mmm71,ss3%velpphi_mmm71,iout,ierr)
        if(ierr.eq.0) ss3%velpphi_mmm71 = w1*ss1%velpphi_mmm71 + (1.0_rspec-w1)*ss2%velpphi_mmm71
     ELSE IF(ALLOCATED(ss1%velpphi_mmm71)) THEN
        CALL ps_conformal("velpphi_mmm71",ss1%velpphi_mmm71,ss3%velpphi_mmm71,iout,ierr)
        ss3%velpphi_mmm71 = ss1%velpphi_mmm71
     ELSE IF(ALLOCATED(ss2%velpphi_mmm71)) THEN
        CALL ps_conformal("velpphi_mmm71",ss2%velpphi_mmm71,ss3%velpphi_mmm71,iout,ierr)
        ss3%velpphi_mmm71 = ss2%velpphi_mmm71
     ENDIF
  ENDIF

  IF((ALLOCATED(ss3%velpphi_neo)).AND.(cclist(ps_cnum_ANOM).EQ.1)) THEN
     IF(ALLOCATED(ss1%velpphi_neo).AND.ALLOCATED(ss2%velpphi_neo)) THEN
        CALL ps_conformal("velpphi_neo",ss2%velpphi_neo,ss3%velpphi_neo,iout,ierr)
        CALL ps_conformal("velpphi_neo",ss1%velpphi_neo,ss3%velpphi_neo,iout,ierr)
        if(ierr.eq.0) ss3%velpphi_neo = w1*ss1%velpphi_neo + (1.0_rspec-w1)*ss2%velpphi_neo
     ELSE IF(ALLOCATED(ss1%velpphi_neo)) THEN
        CALL ps_conformal("velpphi_neo",ss1%velpphi_neo,ss3%velpphi_neo,iout,ierr)
        ss3%velpphi_neo = ss1%velpphi_neo
     ELSE IF(ALLOCATED(ss2%velpphi_neo)) THEN
        CALL ps_conformal("velpphi_neo",ss2%velpphi_neo,ss3%velpphi_neo,iout,ierr)
        ss3%velpphi_neo = ss2%velpphi_neo
     ENDIF
  ENDIF

END SUBROUTINE plasma_state_merge_8
