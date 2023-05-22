! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_zonwt_update(ss,ierr)

  !   update zone weighting index map

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss    ! state object to be updated
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------
  INTEGER :: iout,isize,i1,i2,i3,i4,iertot,iertarg,ierval
  !-------------------

  iout = SWIM_err_out_file

  ierr = 0

  CALL ps_maxid(ss%iobj, isize, iout, ierr)
  if(ierr.NE.0) return

  ! reallocate to a safe size; initialize all elements to ZERO

  IF(ALLOCATED(ss%id_zonwt)) DEALLOCATE(ss%id_zonwt)
  ALLOCATE(ss%id_zonwt(isize))
  ss%id_zonwt = 0

  iertot = 0

  IF(ALLOCATED(ss%id_NBEAMI)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 1,ss%nspec_beam
        IF((ss%id_eperp_beami(I1).GT.0).AND.(ss%id_eperp_beami(I1).LE.isize)) THEN
           IF((ss%id_NBEAMI(I1).GT.0).AND.(ss%id_NBEAMI(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_eperp_beami(I1)) = ss%id_NBEAMI(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_eperp_beami: ",ss%id_eperp_beami
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NBEAMI: ",ss%id_NBEAMI
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NFUSI)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 1,ss%nspec_fusion
        IF((ss%id_eperp_fusi(I1).GT.0).AND.(ss%id_eperp_fusi(I1).LE.isize)) THEN
           IF((ss%id_NFUSI(I1).GT.0).AND.(ss%id_NFUSI(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_eperp_fusi(I1)) = ss%id_NFUSI(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_eperp_fusi: ",ss%id_eperp_fusi
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NFUSI: ",ss%id_NFUSI
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NMINI)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 1,ss%nspec_rfmin
        IF((ss%id_eperp_mini(I1).GT.0).AND.(ss%id_eperp_mini(I1).LE.isize)) THEN
           IF((ss%id_NMINI(I1).GT.0).AND.(ss%id_NMINI(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_eperp_mini(I1)) = ss%id_NMINI(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_eperp_mini: ",ss%id_eperp_mini
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NMINI: ",ss%id_NMINI
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF
  !singleton
  IF(ss%id_eperp_rw.GT.0) THEN
     IF(ss%id_eperp_rw.LE.isize) THEN
        IF((ss%id_NRW.GT.0).AND.(ss%id_NRW.LE.isize)) THEN
           ss%id_zonwt(ss%id_eperp_rw) = ss%id_NRW
        ELSE
           WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
           write(IOUT,*) "  id_NRW: ",ss%id_NRW
           write(IOUT,*) "  allowable range is 1 to ",isize
           iertot = iertot + 1
        ENDIF
     ELSE
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_eperp_rw: ",ss%id_eperp_rw
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NBEAMI)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 1,ss%nspec_beam
        IF((ss%id_epll_beami(I1).GT.0).AND.(ss%id_epll_beami(I1).LE.isize)) THEN
           IF((ss%id_NBEAMI(I1).GT.0).AND.(ss%id_NBEAMI(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_epll_beami(I1)) = ss%id_NBEAMI(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_epll_beami: ",ss%id_epll_beami
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NBEAMI: ",ss%id_NBEAMI
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NFUSI)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 1,ss%nspec_fusion
        IF((ss%id_epll_fusi(I1).GT.0).AND.(ss%id_epll_fusi(I1).LE.isize)) THEN
           IF((ss%id_NFUSI(I1).GT.0).AND.(ss%id_NFUSI(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_epll_fusi(I1)) = ss%id_NFUSI(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_epll_fusi: ",ss%id_epll_fusi
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NFUSI: ",ss%id_NFUSI
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NMINI)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 1,ss%nspec_rfmin
        IF((ss%id_epll_mini(I1).GT.0).AND.(ss%id_epll_mini(I1).LE.isize)) THEN
           IF((ss%id_NMINI(I1).GT.0).AND.(ss%id_NMINI(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_epll_mini(I1)) = ss%id_NMINI(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_epll_mini: ",ss%id_epll_mini
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NMINI: ",ss%id_NMINI
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF
  !singleton
  IF(ss%id_epll_rw.GT.0) THEN
     IF(ss%id_epll_rw.LE.isize) THEN
        IF((ss%id_NRW.GT.0).AND.(ss%id_NRW.LE.isize)) THEN
           ss%id_zonwt(ss%id_epll_rw) = ss%id_NRW
        ELSE
           WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
           write(IOUT,*) "  id_NRW: ",ss%id_NRW
           write(IOUT,*) "  allowable range is 1 to ",isize
           iertot = iertot + 1
        ENDIF
     ELSE
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_epll_rw: ",ss%id_epll_rw
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NS)) THEN
     IF((ss%id_Fi_depletion.GT.0).AND.(ss%id_Fi_depletion.LE.isize)) THEN
        IF((ss%id_NS(PS_ELEC_INDEX).GT.0).AND.(ss%id_NS(PS_ELEC_INDEX).LE.isize)) THEN
           ss%id_zonwt(ss%id_Fi_depletion) = ss%id_NS(PS_ELEC_INDEX)
        ELSE
           iertot=iertot+1
           WRITE(iout,*) " ?plasma_state_zonwt_update element: id_NS(PS_ELEC_INDEX) = ",ss%id_NS(PS_ELEC_INDEX)
           WRITE(iout,*) "  out of bounds 1 to ",isize
        ENDIF
     ELSE
        iertot=iertot+1
        WRITE(iout,*) " ?plasma_state_zonwt_update element: id_Fi_depletion = ",ss%id_Fi_depletion
        WRITE(iout,*) "  out of bounds 1 to ",isize
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_TQQCX)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 1,ss%ngsc0
        IF((ss%id_omeg0cx(I1).GT.0).AND.(ss%id_omeg0cx(I1).LE.isize)) THEN
           IF((ss%id_TQQCX(I1).GT.0).AND.(ss%id_TQQCX(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_omeg0cx(I1)) = ss%id_TQQCX(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_omeg0cx: ",ss%id_omeg0cx
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_TQQCX: ",ss%id_TQQCX
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_N0NORM)) THEN
     iertarg = 0; ierval = 0
     DO I2 = 1,ss%ngsc0
        DO I1 = 1,ss%nspec_gas
           IF((ss%id_omeg0sc0(I1,I2).GT.0).AND.(ss%id_omeg0sc0(I1,I2).LE.isize)) THEN
              IF((ss%id_N0NORM(I1,I2).GT.0).AND.(ss%id_N0NORM(I1,I2).LE.isize)) THEN
                 ss%id_zonwt(ss%id_omeg0sc0(I1,I2)) = ss%id_N0NORM(I1,I2)
              ELSE
                 ierval = ierval + 1
              ENDIF
           ELSE
              iertarg = iertarg + 1
           ENDIF
        ENDDO   ! I1
     ENDDO   ! I2
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_omeg0sc0: ",ss%id_omeg0sc0
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_N0NORM: ",ss%id_N0NORM
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_N0_HALO)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 1,ss%nspec_gas
        IF((ss%id_omeg0_halo(I1).GT.0).AND.(ss%id_omeg0_halo(I1).LE.isize)) THEN
           IF((ss%id_N0_HALO(I1).GT.0).AND.(ss%id_N0_HALO(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_omeg0_halo(I1)) = ss%id_N0_HALO(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_omeg0_halo: ",ss%id_omeg0_halo
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_N0_HALO: ",ss%id_N0_HALO
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_N0_RECO)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 1,ss%nspec_gas
        IF((ss%id_omeg0_reco(I1).GT.0).AND.(ss%id_omeg0_reco(I1).LE.isize)) THEN
           IF((ss%id_N0_RECO(I1).GT.0).AND.(ss%id_N0_RECO(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_omeg0_reco(I1)) = ss%id_N0_RECO(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_omeg0_reco: ",ss%id_omeg0_reco
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_N0_RECO: ",ss%id_N0_RECO
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF
  !singleton
  IF(ss%id_omegat.GT.0) THEN
     IF(ss%id_omegat.LE.isize) THEN
        IF((ss%id_NI.GT.0).AND.(ss%id_NI.LE.isize)) THEN
           ss%id_zonwt(ss%id_omegat) = ss%id_NI
        ELSE
           WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
           write(IOUT,*) "  id_NI: ",ss%id_NI
           write(IOUT,*) "  allowable range is 1 to ",isize
           iertot = iertot + 1
        ENDIF
     ELSE
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_omegat: ",ss%id_omegat
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NBEAMI)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 1,ss%nspec_beam
        IF((ss%id_qbeami(I1).GT.0).AND.(ss%id_qbeami(I1).LE.isize)) THEN
           IF((ss%id_NBEAMI(I1).GT.0).AND.(ss%id_NBEAMI(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_qbeami(I1)) = ss%id_NBEAMI(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_qbeami: ",ss%id_qbeami
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NBEAMI: ",ss%id_NBEAMI
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_QQCX)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 1,ss%ngsc0
        IF((ss%id_T0cx(I1).GT.0).AND.(ss%id_T0cx(I1).LE.isize)) THEN
           IF((ss%id_QQCX(I1).GT.0).AND.(ss%id_QQCX(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_T0cx(I1)) = ss%id_QQCX(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_T0cx: ",ss%id_T0cx
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_QQCX: ",ss%id_QQCX
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_N0NORM)) THEN
     iertarg = 0; ierval = 0
     DO I2 = 1,ss%ngsc0
        DO I1 = 1,ss%nspec_gas
           IF((ss%id_T0sc0(I1,I2).GT.0).AND.(ss%id_T0sc0(I1,I2).LE.isize)) THEN
              IF((ss%id_N0NORM(I1,I2).GT.0).AND.(ss%id_N0NORM(I1,I2).LE.isize)) THEN
                 ss%id_zonwt(ss%id_T0sc0(I1,I2)) = ss%id_N0NORM(I1,I2)
              ELSE
                 ierval = ierval + 1
              ENDIF
           ELSE
              iertarg = iertarg + 1
           ENDIF
        ENDDO   ! I1
     ENDDO   ! I2
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_T0sc0: ",ss%id_T0sc0
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_N0NORM: ",ss%id_N0NORM
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_N0_HALO)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 1,ss%nspec_gas
        IF((ss%id_T0_halo(I1).GT.0).AND.(ss%id_T0_halo(I1).LE.isize)) THEN
           IF((ss%id_N0_HALO(I1).GT.0).AND.(ss%id_N0_HALO(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_T0_halo(I1)) = ss%id_N0_HALO(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_T0_halo: ",ss%id_T0_halo
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_N0_HALO: ",ss%id_N0_HALO
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_N0_RECO)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 1,ss%nspec_gas
        IF((ss%id_T0_reco(I1).GT.0).AND.(ss%id_T0_reco(I1).LE.isize)) THEN
           IF((ss%id_N0_RECO(I1).GT.0).AND.(ss%id_N0_RECO(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_T0_reco(I1)) = ss%id_N0_RECO(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_T0_reco: ",ss%id_T0_reco
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_N0_RECO: ",ss%id_N0_RECO
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF
  !singleton
  IF(ss%id_Ti.GT.0) THEN
     IF(ss%id_Ti.LE.isize) THEN
        IF((ss%id_NI.GT.0).AND.(ss%id_NI.LE.isize)) THEN
           ss%id_zonwt(ss%id_Ti) = ss%id_NI
        ELSE
           WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
           write(IOUT,*) "  id_NI: ",ss%id_NI
           write(IOUT,*) "  allowable range is 1 to ",isize
           iertot = iertot + 1
        ENDIF
     ELSE
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_Ti: ",ss%id_Ti
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF
  !singleton
  IF(ss%id_tmodel.GT.0) THEN
     IF(ss%id_tmodel.LE.isize) THEN
        IF((ss%id_NMODEL.GT.0).AND.(ss%id_NMODEL.LE.isize)) THEN
           ss%id_zonwt(ss%id_tmodel) = ss%id_NMODEL
        ELSE
           WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
           write(IOUT,*) "  id_NMODEL: ",ss%id_NMODEL
           write(IOUT,*) "  allowable range is 1 to ",isize
           iertot = iertot + 1
        ENDIF
     ELSE
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_tmodel: ",ss%id_tmodel
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NS)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 0,ss%nspec_th
        IF((ss%id_Ts(I1).GT.0).AND.(ss%id_Ts(I1).LE.isize)) THEN
           IF((ss%id_NS(I1).GT.0).AND.(ss%id_NS(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_Ts(I1)) = ss%id_NS(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_Ts: ",ss%id_Ts
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NS: ",ss%id_NS
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NS)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 0,ss%nspec_th
        IF((ss%id_vpol_inmp(I1).GT.0).AND.(ss%id_vpol_inmp(I1).LE.isize)) THEN
           IF((ss%id_NS(I1).GT.0).AND.(ss%id_NS(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_vpol_inmp(I1)) = ss%id_NS(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_vpol_inmp: ",ss%id_vpol_inmp
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NS: ",ss%id_NS
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NS)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 0,ss%nspec_th
        IF((ss%id_vpol_omp(I1).GT.0).AND.(ss%id_vpol_omp(I1).LE.isize)) THEN
           IF((ss%id_NS(I1).GT.0).AND.(ss%id_NS(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_vpol_omp(I1)) = ss%id_NS(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_vpol_omp: ",ss%id_vpol_omp
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NS: ",ss%id_NS
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NS)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 0,ss%nspec_th
        IF((ss%id_vtor_inmp(I1).GT.0).AND.(ss%id_vtor_inmp(I1).LE.isize)) THEN
           IF((ss%id_NS(I1).GT.0).AND.(ss%id_NS(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_vtor_inmp(I1)) = ss%id_NS(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_vtor_inmp: ",ss%id_vtor_inmp
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NS: ",ss%id_NS
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NS)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 0,ss%nspec_th
        IF((ss%id_vtor_omp(I1).GT.0).AND.(ss%id_vtor_omp(I1).LE.isize)) THEN
           IF((ss%id_NS(I1).GT.0).AND.(ss%id_NS(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_vtor_omp(I1)) = ss%id_NS(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_vtor_omp: ",ss%id_vtor_omp
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NS: ",ss%id_NS
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NS)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 0,ss%nspec_th
        IF((ss%id_v_pars(I1).GT.0).AND.(ss%id_v_pars(I1).LE.isize)) THEN
           IF((ss%id_NS(I1).GT.0).AND.(ss%id_NS(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_v_pars(I1)) = ss%id_NS(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_v_pars: ",ss%id_v_pars
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NS: ",ss%id_NS
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NS)) THEN
     iertarg = 0; ierval = 0
     DO I1 = 0,ss%nspec_th
        IF((ss%id_v_pers(I1).GT.0).AND.(ss%id_v_pers(I1).LE.isize)) THEN
           IF((ss%id_NS(I1).GT.0).AND.(ss%id_NS(I1).LE.isize)) THEN
              ss%id_zonwt(ss%id_v_pers(I1)) = ss%id_NS(I1)
           ELSE
              ierval = ierval + 1
           ENDIF
        ELSE
           iertarg = iertarg + 1
        ENDIF
     ENDDO   ! I1
     IF(iertarg.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"
        write(IOUT,*) "  id_v_pers: ",ss%id_v_pers
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
     IF(ierval.GT.0) THEN
        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"
        write(IOUT,*) "  id_NS: ",ss%id_NS
        write(IOUT,*) "  allowable range is 1 to ",isize
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NS)) THEN
     IF((ss%id_Zeff.GT.0).AND.(ss%id_Zeff.LE.isize)) THEN
        IF((ss%id_NS(PS_ELEC_INDEX).GT.0).AND.(ss%id_NS(PS_ELEC_INDEX).LE.isize)) THEN
           ss%id_zonwt(ss%id_Zeff) = ss%id_NS(PS_ELEC_INDEX)
        ELSE
           iertot=iertot+1
           WRITE(iout,*) " ?plasma_state_zonwt_update element: id_NS(PS_ELEC_INDEX) = ",ss%id_NS(PS_ELEC_INDEX)
           WRITE(iout,*) "  out of bounds 1 to ",isize
        ENDIF
     ELSE
        iertot=iertot+1
        WRITE(iout,*) " ?plasma_state_zonwt_update element: id_Zeff = ",ss%id_Zeff
        WRITE(iout,*) "  out of bounds 1 to ",isize
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NS)) THEN
     IF((ss%id_Zeff_fi.GT.0).AND.(ss%id_Zeff_fi.LE.isize)) THEN
        IF((ss%id_NS(PS_ELEC_INDEX).GT.0).AND.(ss%id_NS(PS_ELEC_INDEX).LE.isize)) THEN
           ss%id_zonwt(ss%id_Zeff_fi) = ss%id_NS(PS_ELEC_INDEX)
        ELSE
           iertot=iertot+1
           WRITE(iout,*) " ?plasma_state_zonwt_update element: id_NS(PS_ELEC_INDEX) = ",ss%id_NS(PS_ELEC_INDEX)
           WRITE(iout,*) "  out of bounds 1 to ",isize
        ENDIF
     ELSE
        iertot=iertot+1
        WRITE(iout,*) " ?plasma_state_zonwt_update element: id_Zeff_fi = ",ss%id_Zeff_fi
        WRITE(iout,*) "  out of bounds 1 to ",isize
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%id_NS)) THEN
     IF((ss%id_Zeff_th.GT.0).AND.(ss%id_Zeff_th.LE.isize)) THEN
        IF((ss%id_NS(PS_ELEC_INDEX).GT.0).AND.(ss%id_NS(PS_ELEC_INDEX).LE.isize)) THEN
           ss%id_zonwt(ss%id_Zeff_th) = ss%id_NS(PS_ELEC_INDEX)
        ELSE
           iertot=iertot+1
           WRITE(iout,*) " ?plasma_state_zonwt_update element: id_NS(PS_ELEC_INDEX) = ",ss%id_NS(PS_ELEC_INDEX)
           WRITE(iout,*) "  out of bounds 1 to ",isize
        ENDIF
     ELSE
        iertot=iertot+1
        WRITE(iout,*) " ?plasma_state_zonwt_update element: id_Zeff_th = ",ss%id_Zeff_th
        WRITE(iout,*) "  out of bounds 1 to ",isize
     ENDIF
  ENDIF


  IF(iertot.GT.0) THEN
     WRITE(iout,*) ' ?plasma_state_zonwt_update: encountered invalid profile IDs!'
     ierr=9999
  ENDIF

END SUBROUTINE plasma_state_zonwt_update
