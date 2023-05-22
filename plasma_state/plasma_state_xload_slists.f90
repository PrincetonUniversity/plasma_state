
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_slists(ss,xhash,iout,ierr)

  ! called from plasma_state_xload: check grids; store in xplasma structure

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:
  INTEGER :: icomponent,imod
  CHARACTER*32 :: listname
  INTEGER :: ahash(ps_max_static)
  !-----------------------

  ahash(1) = ps_dhash_NSPEC_ALL
  IF(ALLOCATED(ss%ALL_name).AND.hcheck(1,1)) THEN
     listname = "PS_SPECIES_ALL"
     icomponent = ps_cnum_PLASMA
     CALL ps_species_list_store(ss%iobj,listname,icomponent,0,ss%nspec_all, &
          ss%ALL_name,ss%qatom_ALL,ss%q_ALL,ss%m_ALL,ss%ALL_type, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NSPEC_ALLA
  IF(ALLOCATED(ss%ALLA_name).AND.hcheck(1,1)) THEN
     listname = "PS_SPECIES_ALLA"
     icomponent = ps_cnum_PLASMA
     CALL ps_species_list_store(ss%iobj,listname,icomponent,0,ss%nspec_alla, &
          ss%ALLA_name,ss%qatom_ALLA,ss%q_ALLA,ss%m_ALLA,ss%ALLA_type, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NSPEC_BEAM
  IF(ALLOCATED(ss%SNBI_name).AND.hcheck(1,1)) THEN
     listname = "PS_SPECIES_SNBI"
     icomponent = ps_cnum_NBI
     CALL ps_species_list_store(ss%iobj,listname,icomponent,1,ss%nspec_beam, &
          ss%SNBI_name,ss%qatom_SNBI,ss%q_SNBI,ss%m_SNBI,ss%SNBI_type, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NSPEC_FUSION
  IF(ALLOCATED(ss%SFUS_name).AND.hcheck(1,1)) THEN
     listname = "PS_SPECIES_SFUS"
     icomponent = ps_cnum_FUS
     CALL ps_species_list_store(ss%iobj,listname,icomponent,1,ss%nspec_fusion, &
          ss%SFUS_name,ss%qatom_SFUS,ss%q_SFUS,ss%m_SFUS,ss%SFUS_type, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NSPEC_GAS
  IF(ALLOCATED(ss%SGAS_name).AND.hcheck(1,1)) THEN
     listname = "PS_SPECIES_SGAS"
     icomponent = ps_cnum_GAS
     CALL ps_species_list_store(ss%iobj,listname,icomponent,1,ss%nspec_gas, &
          ss%SGAS_name,ss%qatom_SGAS,ss%q_SGAS,ss%m_SGAS,ss%SGAS_type, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NSPEC_IMP0
  IF(ALLOCATED(ss%SIMP0_name).AND.hcheck(1,1)) THEN
     listname = "PS_SPECIES_SIMP0"
     icomponent = ps_cnum_GAS
     CALL ps_species_list_store(ss%iobj,listname,icomponent,1,ss%nspec_imp0, &
          ss%SIMP0_name,ss%qatom_SIMP0,ss%q_SIMP0,ss%m_SIMP0,ss%SIMP0_type, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NSPEC_IMPI
  IF(ALLOCATED(ss%SIMPI_name).AND.hcheck(1,1)) THEN
     listname = "PS_SPECIES_SIMPI"
     icomponent = ps_cnum_PLASMA
     CALL ps_species_list_store(ss%iobj,listname,icomponent,1,ss%nspec_impi, &
          ss%SIMPI_name,ss%qatom_SIMPI,ss%q_SIMPI,ss%m_SIMPI,ss%SIMPI_type, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NSPEC_RFMIN
  IF(ALLOCATED(ss%RFMIN_name).AND.hcheck(1,1)) THEN
     listname = "PS_SPECIES_RFMIN"
     icomponent = ps_cnum_IC
     CALL ps_species_list_store(ss%iobj,listname,icomponent,1,ss%nspec_rfmin, &
          ss%RFMIN_name,ss%qatom_RFMIN,ss%q_RFMIN,ss%m_RFMIN,ss%RFMIN_type, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NSPEC_TH
  IF(ALLOCATED(ss%S_name).AND.hcheck(1,1)) THEN
     listname = "PS_SPECIES_S"
     icomponent = ps_cnum_PLASMA
     CALL ps_species_list_store(ss%iobj,listname,icomponent,0,ss%nspec_th, &
          ss%S_name,ss%qatom_S,ss%q_S,ss%m_S,ss%S_type, &
          imod,iout,ierr)
     IF(ierr.NE.0) RETURN
  ENDIF

  ahash(1) = ps_dhash_NSPEC_THA
  IF(ALLOCATED(ss%SA_name).AND.hcheck(1,1)) THEN
     listname = "PS_SPECIES_SA"
     icomponent = ps_cnum_PLASMA
     CALL ps_species_list_store(ss%iobj,listname,icomponent,0,ss%nspec_tha, &
          ss%SA_name,ss%qatom_SA,ss%q_SA,ss%m_SA,ss%SA_type, &
          imod,iout,ierr)
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

END SUBROUTINE plasma_state_xload_slists
