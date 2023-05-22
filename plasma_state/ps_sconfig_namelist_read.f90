! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


subroutine ps_sconfig_namelist_read(init,filename,tmp_filename,ss,ierr)

  ! from current contents of state, write shot configuration namelist

  USE plasma_state_mod
  IMPLICIT NONE

  LOGICAL, INTENT(IN) :: init                  ! .TRUE. to pre-initialize
  ! state variables prior to namelist read (caution -- could cause problems
  ! if partial configuration dataset is already in the state).

  CHARACTER*(*), INTENT(IN) :: filename        ! file to contain namelist
  CHARACTER*(*), INTENT(IN) :: tmp_filename    ! namelist (comments removed)
  !  (leave tmp_filename blank to get internally generated filename)
  TYPE (plasma_state) :: ss     ! state containing data to read
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------------------
  INTEGER :: iout,iertmp,isize,isiz0,ii
  INTEGER :: ilun_namel
  !-------------------------------
  INTEGER :: shot_number
  INTEGER, dimension(ps_max_static) :: iAMU_S
  INTEGER, dimension(ps_max_static) :: iZatom_S
  INTEGER, dimension(ps_max_static) :: iZion_S
  CHARACTER*32, dimension(ps_max_static) :: nbion
  CHARACTER*32, dimension(ps_max_static) :: nbion_trace
  INTEGER, dimension(ps_max_static) :: iAMU_SFUS
  INTEGER, dimension(ps_max_static) :: iZatom_SFUS
  INTEGER, dimension(ps_max_static) :: iZion_SFUS
  INTEGER, dimension(ps_max_static) :: iAMU_RFMIN
  INTEGER, dimension(ps_max_static) :: iZatom_RFMIN
  INTEGER, dimension(ps_max_static) :: iZion_RFMIN
  REAL(KIND=rspec), dimension(ps_max_static_2d) :: freq_ic
  INTEGER, dimension(ps_max_static_2d) :: n_straps
  INTEGER :: max_n_straps
  REAL(KIND=rspec), dimension(ps_max_static_2d) :: R_ant
  REAL(KIND=rspec), dimension(ps_max_static_2d) :: Z_ant
  REAL(KIND=rspec), dimension(ps_max_static_2d) :: Z_mid_ant
  INTEGER, dimension(ps_max_static_2d) :: num_nphi_vac
  INTEGER :: max_num_nphi_vac
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: Real_ant_coef
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: Imag_ant_coef
  INTEGER, dimension(ps_max_static_2d) :: num_nphi
  INTEGER :: max_num_nphi
  INTEGER, dimension(ps_max_static_2d,ps_max_static_2d) :: nphi
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: wt_nphi
  INTEGER :: kccw_Bphi
  INTEGER :: kccw_Jphi
  CHARACTER*32, dimension(ps_max_static) :: gs_name
  CHARACTER*32, dimension(ps_max_static) :: gas_atom
  INTEGER, dimension(ps_max_static) :: is_recycling
  CHARACTER*32, dimension(ps_max_static) :: xs_name
  REAL(KIND=rspec), dimension(ps_max_static) :: freq_ec
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_theta_aim
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_phi_aim
  REAL(KIND=rspec), dimension(ps_max_static) :: EC_Omode_Fraction
  REAL(KIND=rspec), dimension(ps_max_static_2d) :: freq_lh
  INTEGER, dimension(ps_max_static_2d) :: nspectrumlh_src
  INTEGER :: max_nspectrumlh_src
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: aminlh
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: amaxlh
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: atorminlh
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: atormaxlh
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: apolminlh
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: apolmaxlh
  REAL(KIND=rspec), dimension(ps_max_static_2d,ps_max_static_2d) :: fracpslh
  INTEGER :: i_npoloidal
  REAL(KIND=rspec), dimension(ps_max_static_2d) :: rhopsi0lh
  REAL(KIND=rspec), dimension(ps_max_static_2d) :: thegrilh
  REAL(KIND=rspec), dimension(ps_max_static_2d) :: hegrilh
  INTEGER, dimension(ps_max_static_2d) :: nthinlh
  INTEGER, dimension(ps_max_static_2d,ps_max_static_2d) :: nnkparlh
  INTEGER, dimension(ps_max_static_2d,ps_max_static_2d) :: nnktorlh
  INTEGER, dimension(ps_max_static_2d,ps_max_static_2d) :: nnkpollh

  NAMELIST /sconfig/ &
       shot_number, &
       iAMU_S,iZatom_S,iZion_S, &
       nbion, &
       nbion_trace, &
       iAMU_SFUS,iZatom_SFUS,iZion_SFUS, &
       iAMU_RFMIN,iZatom_RFMIN,iZion_RFMIN, &
       freq_ic, &
       n_straps, &
       max_n_straps, &
       R_ant, &
       Z_ant, &
       Z_mid_ant, &
       num_nphi_vac, &
       max_num_nphi_vac, &
       Real_ant_coef, &
       Imag_ant_coef, &
       num_nphi, &
       max_num_nphi, &
       nphi, &
       wt_nphi, &
       kccw_Bphi, &
       kccw_Jphi, &
       gs_name, &
       gas_atom, &
       is_recycling, &
       xs_name, &
       freq_ec, &
       EC_theta_aim, &
       EC_phi_aim, &
       EC_Omode_Fraction, &
       freq_lh, &
       nspectrumlh_src, &
       max_nspectrumlh_src, &
       aminlh, &
       amaxlh, &
       atorminlh, &
       atormaxlh, &
       apolminlh, &
       apolmaxlh, &
       fracpslh, &
       i_npoloidal, &
       rhopsi0lh, &
       thegrilh, &
       hegrilh, &
       nthinlh, &
       nnkparlh, &
       nnktorlh, &
       nnkpollh
  !---------------------------------------

  iout = SWIM_err_out_file

  ierr = 0

  ! initialize local namelist variables to state values
  IF(init) ss%shot_number = 0
  shot_number = ss%shot_number
  iAMU_S = ps_unInit
  iZatom_S = ps_unInit
  iZion_S = ps_unInit
  nbion = " "
  IF(ALLOCATED(ss%nbion)) THEN
     isize = size(nbion)
     IF(init) THEN
        ss%nbion(1:isize) = nbion(1:isize)
     ELSE
        nbion(1:isize) = ss%nbion(1:isize)
     ENDIF
  ENDIF
  nbion_trace = " "
  IF(ALLOCATED(ss%nbion_trace)) THEN
     isize = size(nbion_trace)
     IF(init) THEN
        ss%nbion_trace(1:isize) = nbion_trace(1:isize)
     ELSE
        nbion_trace(1:isize) = ss%nbion_trace(1:isize)
     ENDIF
  ENDIF
  iAMU_SFUS = ps_unInit
  iZatom_SFUS = ps_unInit
  iZion_SFUS = ps_unInit
  iAMU_RFMIN = ps_unInit
  iZatom_RFMIN = ps_unInit
  iZion_RFMIN = ps_unInit
  freq_ic = 0.0_rspec
  IF(ALLOCATED(ss%freq_ic)) THEN
     isize = size(freq_ic)
     IF(init) THEN
        ss%freq_ic(1:isize) = freq_ic(1:isize)
     ELSE
        freq_ic(1:isize) = ss%freq_ic(1:isize)
     ENDIF
  ENDIF
  n_straps = ps_unInit
  IF(ALLOCATED(ss%n_straps)) THEN
     isize = size(n_straps)
     IF(init) THEN
        ss%n_straps(1:isize) = n_straps(1:isize)
     ELSE
        n_straps(1:isize) = ss%n_straps(1:isize)
     ENDIF
  ENDIF
  IF(init) ss%max_n_straps = 0
  max_n_straps = ss%max_n_straps
  R_ant = 0.0_rspec
  IF(ALLOCATED(ss%R_ant)) THEN
     isize = size(R_ant)
     IF(init) THEN
        ss%R_ant(1:isize) = R_ant(1:isize)
     ELSE
        R_ant(1:isize) = ss%R_ant(1:isize)
     ENDIF
  ENDIF
  Z_ant = 0.0_rspec
  IF(ALLOCATED(ss%Z_ant)) THEN
     isize = size(Z_ant)
     IF(init) THEN
        ss%Z_ant(1:isize) = Z_ant(1:isize)
     ELSE
        Z_ant(1:isize) = ss%Z_ant(1:isize)
     ENDIF
  ENDIF
  Z_mid_ant = 0.0_rspec
  IF(ALLOCATED(ss%Z_mid_ant)) THEN
     isize = size(Z_mid_ant)
     IF(init) THEN
        ss%Z_mid_ant(1:isize) = Z_mid_ant(1:isize)
     ELSE
        Z_mid_ant(1:isize) = ss%Z_mid_ant(1:isize)
     ENDIF
  ENDIF
  num_nphi_vac = ps_unInit
  IF(ALLOCATED(ss%num_nphi_vac)) THEN
     isize = size(num_nphi_vac)
     IF(init) THEN
        ss%num_nphi_vac(1:isize) = num_nphi_vac(1:isize)
     ELSE
        num_nphi_vac(1:isize) = ss%num_nphi_vac(1:isize)
     ENDIF
  ENDIF
  IF(init) ss%max_num_nphi_vac = 0
  max_num_nphi_vac = ss%max_num_nphi_vac
  Real_ant_coef = 0.0_rspec
  IF(ALLOCATED(ss%Real_ant_coef)) THEN
     isiz0 = size(Real_ant_coef,1)
     isize = size(Real_ant_coef,2)
     IF(init) THEN
        ss%Real_ant_coef(1:isiz0,1:isize) = Real_ant_coef(1:isiz0,1:isize)
     ELSE
        Real_ant_coef(1:isiz0,1:isize) = ss%Real_ant_coef(1:isiz0,1:isize)
     ENDIF
  ENDIF
  Imag_ant_coef = 0.0_rspec
  IF(ALLOCATED(ss%Imag_ant_coef)) THEN
     isiz0 = size(Imag_ant_coef,1)
     isize = size(Imag_ant_coef,2)
     IF(init) THEN
        ss%Imag_ant_coef(1:isiz0,1:isize) = Imag_ant_coef(1:isiz0,1:isize)
     ELSE
        Imag_ant_coef(1:isiz0,1:isize) = ss%Imag_ant_coef(1:isiz0,1:isize)
     ENDIF
  ENDIF
  num_nphi = ps_unInit
  IF(ALLOCATED(ss%num_nphi)) THEN
     isize = size(num_nphi)
     IF(init) THEN
        ss%num_nphi(1:isize) = num_nphi(1:isize)
     ELSE
        num_nphi(1:isize) = ss%num_nphi(1:isize)
     ENDIF
  ENDIF
  IF(init) ss%max_num_nphi = 0
  max_num_nphi = ss%max_num_nphi
  nphi = ps_unInit
  IF(ALLOCATED(ss%nphi)) THEN
     isiz0 = size(nphi,1)
     isize = size(nphi,2)
     IF(init) THEN
        ss%nphi(1:isiz0,1:isize) = nphi(1:isiz0,1:isize)
     ELSE
        nphi(1:isiz0,1:isize) = ss%nphi(1:isiz0,1:isize)
     ENDIF
  ENDIF
  wt_nphi = 0.0_rspec
  IF(ALLOCATED(ss%wt_nphi)) THEN
     isiz0 = size(wt_nphi,1)
     isize = size(wt_nphi,2)
     IF(init) THEN
        ss%wt_nphi(1:isiz0,1:isize) = wt_nphi(1:isiz0,1:isize)
     ELSE
        wt_nphi(1:isiz0,1:isize) = ss%wt_nphi(1:isiz0,1:isize)
     ENDIF
  ENDIF
  IF(init) ss%kccw_Bphi = 0
  kccw_Bphi = ss%kccw_Bphi
  IF(init) ss%kccw_Jphi = 0
  kccw_Jphi = ss%kccw_Jphi
  gs_name = " "
  IF(ALLOCATED(ss%gs_name)) THEN
     isize = size(gs_name)
     IF(init) THEN
        ss%gs_name(1:isize) = gs_name(1:isize)
     ELSE
        gs_name(1:isize) = ss%gs_name(1:isize)
     ENDIF
  ENDIF
  gas_atom = " "
  IF(ALLOCATED(ss%gas_atom)) THEN
     isize = size(gas_atom)
     IF(init) THEN
        ss%gas_atom(1:isize) = gas_atom(1:isize)
     ELSE
        gas_atom(1:isize) = ss%gas_atom(1:isize)
     ENDIF
  ENDIF
  is_recycling = ps_unInit
  IF(ALLOCATED(ss%is_recycling)) THEN
     isize = size(is_recycling)
     IF(init) THEN
        ss%is_recycling(1:isize) = is_recycling(1:isize)
     ELSE
        is_recycling(1:isize) = ss%is_recycling(1:isize)
     ENDIF
  ENDIF
  xs_name = " "
  IF(ALLOCATED(ss%xs_name)) THEN
     isize = size(xs_name)
     IF(init) THEN
        ss%xs_name(1:isize) = xs_name(1:isize)
     ELSE
        xs_name(1:isize) = ss%xs_name(1:isize)
     ENDIF
  ENDIF
  freq_ec = 0.0_rspec
  IF(ALLOCATED(ss%freq_ec)) THEN
     isize = size(freq_ec)
     IF(init) THEN
        ss%freq_ec(1:isize) = freq_ec(1:isize)
     ELSE
        freq_ec(1:isize) = ss%freq_ec(1:isize)
     ENDIF
  ENDIF
  EC_theta_aim = 0.0_rspec
  IF(ALLOCATED(ss%EC_theta_aim)) THEN
     isize = size(EC_theta_aim)
     IF(init) THEN
        ss%EC_theta_aim(1:isize) = EC_theta_aim(1:isize)
     ELSE
        EC_theta_aim(1:isize) = ss%EC_theta_aim(1:isize)
     ENDIF
  ENDIF
  EC_phi_aim = 0.0_rspec
  IF(ALLOCATED(ss%EC_phi_aim)) THEN
     isize = size(EC_phi_aim)
     IF(init) THEN
        ss%EC_phi_aim(1:isize) = EC_phi_aim(1:isize)
     ELSE
        EC_phi_aim(1:isize) = ss%EC_phi_aim(1:isize)
     ENDIF
  ENDIF
  EC_Omode_Fraction = 0.0_rspec
  IF(ALLOCATED(ss%EC_Omode_Fraction)) THEN
     isize = size(EC_Omode_Fraction)
     IF(init) THEN
        ss%EC_Omode_Fraction(1:isize) = EC_Omode_Fraction(1:isize)
     ELSE
        EC_Omode_Fraction(1:isize) = ss%EC_Omode_Fraction(1:isize)
     ENDIF
  ENDIF
  freq_lh = 0.0_rspec
  IF(ALLOCATED(ss%freq_lh)) THEN
     isize = size(freq_lh)
     IF(init) THEN
        ss%freq_lh(1:isize) = freq_lh(1:isize)
     ELSE
        freq_lh(1:isize) = ss%freq_lh(1:isize)
     ENDIF
  ENDIF
  nspectrumlh_src = ps_unInit
  IF(ALLOCATED(ss%nspectrumlh_src)) THEN
     isize = size(nspectrumlh_src)
     IF(init) THEN
        ss%nspectrumlh_src(1:isize) = nspectrumlh_src(1:isize)
     ELSE
        nspectrumlh_src(1:isize) = ss%nspectrumlh_src(1:isize)
     ENDIF
  ENDIF
  IF(init) ss%max_nspectrumlh_src = 0
  max_nspectrumlh_src = ss%max_nspectrumlh_src
  aminlh = 0.0_rspec
  IF(ALLOCATED(ss%aminlh)) THEN
     isiz0 = size(aminlh,1)
     isize = size(aminlh,2)
     IF(init) THEN
        ss%aminlh(1:isiz0,1:isize) = aminlh(1:isiz0,1:isize)
     ELSE
        aminlh(1:isiz0,1:isize) = ss%aminlh(1:isiz0,1:isize)
     ENDIF
  ENDIF
  amaxlh = 0.0_rspec
  IF(ALLOCATED(ss%amaxlh)) THEN
     isiz0 = size(amaxlh,1)
     isize = size(amaxlh,2)
     IF(init) THEN
        ss%amaxlh(1:isiz0,1:isize) = amaxlh(1:isiz0,1:isize)
     ELSE
        amaxlh(1:isiz0,1:isize) = ss%amaxlh(1:isiz0,1:isize)
     ENDIF
  ENDIF
  atorminlh = 0.0_rspec
  IF(ALLOCATED(ss%atorminlh)) THEN
     isiz0 = size(atorminlh,1)
     isize = size(atorminlh,2)
     IF(init) THEN
        ss%atorminlh(1:isiz0,1:isize) = atorminlh(1:isiz0,1:isize)
     ELSE
        atorminlh(1:isiz0,1:isize) = ss%atorminlh(1:isiz0,1:isize)
     ENDIF
  ENDIF
  atormaxlh = 0.0_rspec
  IF(ALLOCATED(ss%atormaxlh)) THEN
     isiz0 = size(atormaxlh,1)
     isize = size(atormaxlh,2)
     IF(init) THEN
        ss%atormaxlh(1:isiz0,1:isize) = atormaxlh(1:isiz0,1:isize)
     ELSE
        atormaxlh(1:isiz0,1:isize) = ss%atormaxlh(1:isiz0,1:isize)
     ENDIF
  ENDIF
  apolminlh = 0.0_rspec
  IF(ALLOCATED(ss%apolminlh)) THEN
     isiz0 = size(apolminlh,1)
     isize = size(apolminlh,2)
     IF(init) THEN
        ss%apolminlh(1:isiz0,1:isize) = apolminlh(1:isiz0,1:isize)
     ELSE
        apolminlh(1:isiz0,1:isize) = ss%apolminlh(1:isiz0,1:isize)
     ENDIF
  ENDIF
  apolmaxlh = 0.0_rspec
  IF(ALLOCATED(ss%apolmaxlh)) THEN
     isiz0 = size(apolmaxlh,1)
     isize = size(apolmaxlh,2)
     IF(init) THEN
        ss%apolmaxlh(1:isiz0,1:isize) = apolmaxlh(1:isiz0,1:isize)
     ELSE
        apolmaxlh(1:isiz0,1:isize) = ss%apolmaxlh(1:isiz0,1:isize)
     ENDIF
  ENDIF
  fracpslh = 0.0_rspec
  IF(ALLOCATED(ss%fracpslh)) THEN
     isiz0 = size(fracpslh,1)
     isize = size(fracpslh,2)
     IF(init) THEN
        ss%fracpslh(1:isiz0,1:isize) = fracpslh(1:isiz0,1:isize)
     ELSE
        fracpslh(1:isiz0,1:isize) = ss%fracpslh(1:isiz0,1:isize)
     ENDIF
  ENDIF
  IF(init) ss%i_npoloidal = 0
  i_npoloidal = ss%i_npoloidal
  rhopsi0lh = 0.0_rspec
  IF(ALLOCATED(ss%rhopsi0lh)) THEN
     isize = size(rhopsi0lh)
     IF(init) THEN
        ss%rhopsi0lh(1:isize) = rhopsi0lh(1:isize)
     ELSE
        rhopsi0lh(1:isize) = ss%rhopsi0lh(1:isize)
     ENDIF
  ENDIF
  thegrilh = 0.0_rspec
  IF(ALLOCATED(ss%thegrilh)) THEN
     isize = size(thegrilh)
     IF(init) THEN
        ss%thegrilh(1:isize) = thegrilh(1:isize)
     ELSE
        thegrilh(1:isize) = ss%thegrilh(1:isize)
     ENDIF
  ENDIF
  hegrilh = 0.0_rspec
  IF(ALLOCATED(ss%hegrilh)) THEN
     isize = size(hegrilh)
     IF(init) THEN
        ss%hegrilh(1:isize) = hegrilh(1:isize)
     ELSE
        hegrilh(1:isize) = ss%hegrilh(1:isize)
     ENDIF
  ENDIF
  nthinlh = ps_unInit
  IF(ALLOCATED(ss%nthinlh)) THEN
     isize = size(nthinlh)
     IF(init) THEN
        ss%nthinlh(1:isize) = nthinlh(1:isize)
     ELSE
        nthinlh(1:isize) = ss%nthinlh(1:isize)
     ENDIF
  ENDIF
  nnkparlh = ps_unInit
  IF(ALLOCATED(ss%nnkparlh)) THEN
     isiz0 = size(nnkparlh,1)
     isize = size(nnkparlh,2)
     IF(init) THEN
        ss%nnkparlh(1:isiz0,1:isize) = nnkparlh(1:isiz0,1:isize)
     ELSE
        nnkparlh(1:isiz0,1:isize) = ss%nnkparlh(1:isiz0,1:isize)
     ENDIF
  ENDIF
  nnktorlh = ps_unInit
  IF(ALLOCATED(ss%nnktorlh)) THEN
     isiz0 = size(nnktorlh,1)
     isize = size(nnktorlh,2)
     IF(init) THEN
        ss%nnktorlh(1:isiz0,1:isize) = nnktorlh(1:isiz0,1:isize)
     ELSE
        nnktorlh(1:isiz0,1:isize) = ss%nnktorlh(1:isiz0,1:isize)
     ENDIF
  ENDIF
  nnkpollh = ps_unInit
  IF(ALLOCATED(ss%nnkpollh)) THEN
     isiz0 = size(nnkpollh,1)
     isize = size(nnkpollh,2)
     IF(init) THEN
        ss%nnkpollh(1:isiz0,1:isize) = nnkpollh(1:isiz0,1:isize)
     ELSE
        nnkpollh(1:isiz0,1:isize) = ss%nnkpollh(1:isiz0,1:isize)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%S_type)) THEN
     ierr = ierr + 1
     write(iout,*) " ?sconfig namelist read error: state element S already allocated."
  ENDIF

  IF(ALLOCATED(ss%SFUS_type)) THEN
     ierr = ierr + 1
     write(iout,*) " ?sconfig namelist read error: state element SFUS already allocated."
  ENDIF

  IF(ALLOCATED(ss%RFMIN_type)) THEN
     ierr = ierr + 1
     write(iout,*) " ?sconfig namelist read error: state element RFMIN already allocated."
  ENDIF

  IF(ierr.GT.0) RETURN

  ! OK: open namelist file, strip comments.
  !     namelist to be read on ilun_namel after this call:

  CALL ps_namel_strip(filename,tmp_filename,ilun_namel,iout,ierr)

  IF(ierr.NE.0) THEN
     WRITE(iout,*) ' ?namelist file OPEN or READ error: '//trim(filename)
     RETURN
  ENDIF

  READ(UNIT=ilun_namel,NML=sconfig)

  CLOSE(UNIT=ilun_namel,status="DELETE")

  ! check list quantities; infer dimensions

  CALL ps_namrd_slist_chk("S", &
       ps_max_static,iAMU_S,iZatom_S,iZion_S,ss%NSPEC_TH,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  CALL ps_namrd_slist_chk("SFUS", &
       ps_max_static,iAMU_SFUS,iZatom_SFUS,iZion_SFUS,ss%NSPEC_FUSION,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  CALL ps_namrd_slist_chk("RFMIN", &
       ps_max_static,iAMU_RFMIN,iZatom_RFMIN,iZion_RFMIN,ss%NSPEC_RFMIN,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  ss%max_n_straps = max_n_straps

  ss%max_num_nphi_vac = max_num_nphi_vac

  ss%max_num_nphi = max_num_nphi

  CALL ps_namrd_ilist_chk("gs_name", &
       ps_max_static,gs_name,ss%NGSC0,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  CALL ps_namrd_ilist_chk("xs_name", &
       ps_max_static,xs_name,ss%NXS,iout,iertmp)
  IF(iertmp.NE.0) ierr = ierr + 1

  ss%max_nspectrumlh_src = max_nspectrumlh_src

  IF(ierr.NE.0) THEN
     WRITE(iout,*) " ?sconfig namelist list check errors after read."
     RETURN
  ENDIF

  ! OK, allocate arrays per dimensioning in namelist
  CALL plasma_state_empty_alloc(ss,ierr)
  IF(ierr.NE.0) RETURN

  ! Now pick up any vector dimensions

  isize = ss%NICRF_SRC
  ss%n_straps(1:isize) = n_straps(1:isize)

  isize = ss%NICRF_SRC
  ss%num_nphi_vac(1:isize) = num_nphi_vac(1:isize)

  isize = ss%NICRF_SRC
  ss%num_nphi(1:isize) = num_nphi(1:isize)

  isize = ss%NGSC0
  ss%is_recycling(1:isize) = is_recycling(1:isize)

  isize = ss%NLHRF_SRC
  ss%nspectrumlh_src(1:isize) = nspectrumlh_src(1:isize)

  isize = ss%NLHRF_SRC
  ss%nthinlh(1:isize) = nthinlh(1:isize)

  ! OK, allocate arrays per vector dimensioning in namelist
  CALL plasma_state_empty_alloc(ss,ierr)
  IF(ierr.NE.0) RETURN

  ! Now copy namelist variables into state
  ss%shot_number = shot_number

  IF(ALLOCATED(ss%qatom_S)) THEN
     CALL ps_species_convert(-1, -1, 0, &
          ss%qatom_S(0), ss%q_S(0), ss%m_S(0), ierr)
     IF(ierr.NE.0) RETURN

     DO II = 1,ss%NSPEC_TH
        CALL ps_species_convert(iZatom_S(ii),iZion_S(ii),iAMU_S(ii), &
             ss%qatom_S(ii), ss%q_S(ii), ss%m_S(ii), ierr)
        IF(ierr.NE.0) RETURN
     ENDDO
  ENDIF


  IF(ALLOCATED(ss%nbion)) THEN
     isize = ss%NBEAM
     ss%nbion(1:isize) = nbion(1:isize)
  ENDIF

  IF(ALLOCATED(ss%nbion_trace)) THEN
     isize = ss%NBEAM
     ss%nbion_trace(1:isize) = nbion_trace(1:isize)
  ENDIF

  IF(ALLOCATED(ss%qatom_SFUS)) THEN
     DO II = 1,ss%NSPEC_FUSION
        CALL ps_species_convert(iZatom_SFUS(ii),iZion_SFUS(ii),iAMU_SFUS(ii), &
             ss%qatom_SFUS(ii), ss%q_SFUS(ii), ss%m_SFUS(ii), ierr)
        IF(ierr.NE.0) RETURN
     ENDDO
  ENDIF


  IF(ALLOCATED(ss%qatom_RFMIN)) THEN
     DO II = 1,ss%NSPEC_RFMIN
        CALL ps_species_convert(iZatom_RFMIN(ii),iZion_RFMIN(ii),iAMU_RFMIN(ii), &
             ss%qatom_RFMIN(ii), ss%q_RFMIN(ii), ss%m_RFMIN(ii), ierr)
        IF(ierr.NE.0) RETURN
     ENDDO
  ENDIF


  IF(ALLOCATED(ss%freq_ic)) THEN
     isize = ss%NICRF_SRC
     ss%freq_ic(1:isize) = freq_ic(1:isize)
  ENDIF

  IF(ALLOCATED(ss%n_straps)) THEN
     isize = ss%NICRF_SRC
     ss%n_straps(1:isize) = n_straps(1:isize)
  ENDIF

  IF(ALLOCATED(ss%R_ant)) THEN
     isize = ss%NICRF_SRC
     ss%R_ant(1:isize) = R_ant(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Z_ant)) THEN
     isize = ss%NICRF_SRC
     ss%Z_ant(1:isize) = Z_ant(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Z_mid_ant)) THEN
     isize = ss%NICRF_SRC
     ss%Z_mid_ant(1:isize) = Z_mid_ant(1:isize)
  ENDIF

  IF(ALLOCATED(ss%num_nphi_vac)) THEN
     isize = ss%NICRF_SRC
     ss%num_nphi_vac(1:isize) = num_nphi_vac(1:isize)
  ENDIF

  IF(ALLOCATED(ss%Real_ant_coef)) THEN
     isiz0 = maxval(ss%NUM_NPHI_VAC)
     isize = ss%NICRF_SRC
     ss%Real_ant_coef(1:isiz0,1:isize) = Real_ant_coef(1:isiz0,1:isize)
  ENDIF

  IF(ALLOCATED(ss%Imag_ant_coef)) THEN
     isiz0 = maxval(ss%NUM_NPHI_VAC)
     isize = ss%NICRF_SRC
     ss%Imag_ant_coef(1:isiz0,1:isize) = Imag_ant_coef(1:isiz0,1:isize)
  ENDIF

  IF(ALLOCATED(ss%num_nphi)) THEN
     isize = ss%NICRF_SRC
     ss%num_nphi(1:isize) = num_nphi(1:isize)
  ENDIF

  IF(ALLOCATED(ss%nphi)) THEN
     isiz0 = maxval(ss%NUM_NPHI)
     isize = ss%NICRF_SRC
     ss%nphi(1:isiz0,1:isize) = nphi(1:isiz0,1:isize)
  ENDIF

  IF(ALLOCATED(ss%wt_nphi)) THEN
     isiz0 = maxval(ss%NUM_NPHI)
     isize = ss%NICRF_SRC
     ss%wt_nphi(1:isiz0,1:isize) = wt_nphi(1:isiz0,1:isize)
  ENDIF
  ss%kccw_Bphi = kccw_Bphi
  ss%kccw_Jphi = kccw_Jphi

  IF(ALLOCATED(ss%gs_name)) THEN
     isize = ss%NGSC0
     ss%gs_name(1:isize) = gs_name(1:isize)
  ENDIF

  IF(ALLOCATED(ss%gas_atom)) THEN
     isize = ss%NGSC0
     ss%gas_atom(1:isize) = gas_atom(1:isize)
  ENDIF

  IF(ALLOCATED(ss%is_recycling)) THEN
     isize = ss%NGSC0
     ss%is_recycling(1:isize) = is_recycling(1:isize)
  ENDIF

  IF(ALLOCATED(ss%xs_name)) THEN
     isize = ss%NXS
     ss%xs_name(1:isize) = xs_name(1:isize)
  ENDIF

  IF(ALLOCATED(ss%freq_ec)) THEN
     isize = ss%NECRF_SRC
     ss%freq_ec(1:isize) = freq_ec(1:isize)
  ENDIF

  IF(ALLOCATED(ss%EC_theta_aim)) THEN
     isize = ss%NECRF_SRC
     ss%EC_theta_aim(1:isize) = EC_theta_aim(1:isize)
  ENDIF

  IF(ALLOCATED(ss%EC_phi_aim)) THEN
     isize = ss%NECRF_SRC
     ss%EC_phi_aim(1:isize) = EC_phi_aim(1:isize)
  ENDIF

  IF(ALLOCATED(ss%EC_Omode_Fraction)) THEN
     isize = ss%NECRF_SRC
     ss%EC_Omode_Fraction(1:isize) = EC_Omode_Fraction(1:isize)
  ENDIF

  IF(ALLOCATED(ss%freq_lh)) THEN
     isize = ss%NLHRF_SRC
     ss%freq_lh(1:isize) = freq_lh(1:isize)
  ENDIF

  IF(ALLOCATED(ss%nspectrumlh_src)) THEN
     isize = ss%NLHRF_SRC
     ss%nspectrumlh_src(1:isize) = nspectrumlh_src(1:isize)
  ENDIF

  IF(ALLOCATED(ss%aminlh)) THEN
     isiz0 = maxval(ss%NSPECTRUMLH_SRC)
     isize = ss%NLHRF_SRC
     ss%aminlh(1:isiz0,1:isize) = aminlh(1:isiz0,1:isize)
  ENDIF

  IF(ALLOCATED(ss%amaxlh)) THEN
     isiz0 = maxval(ss%NSPECTRUMLH_SRC)
     isize = ss%NLHRF_SRC
     ss%amaxlh(1:isiz0,1:isize) = amaxlh(1:isiz0,1:isize)
  ENDIF

  IF(ALLOCATED(ss%atorminlh)) THEN
     isiz0 = maxval(ss%NSPECTRUMLH_SRC)
     isize = ss%NLHRF_SRC
     ss%atorminlh(1:isiz0,1:isize) = atorminlh(1:isiz0,1:isize)
  ENDIF

  IF(ALLOCATED(ss%atormaxlh)) THEN
     isiz0 = maxval(ss%NSPECTRUMLH_SRC)
     isize = ss%NLHRF_SRC
     ss%atormaxlh(1:isiz0,1:isize) = atormaxlh(1:isiz0,1:isize)
  ENDIF

  IF(ALLOCATED(ss%apolminlh)) THEN
     isiz0 = maxval(ss%NSPECTRUMLH_SRC)
     isize = ss%NLHRF_SRC
     ss%apolminlh(1:isiz0,1:isize) = apolminlh(1:isiz0,1:isize)
  ENDIF

  IF(ALLOCATED(ss%apolmaxlh)) THEN
     isiz0 = maxval(ss%NSPECTRUMLH_SRC)
     isize = ss%NLHRF_SRC
     ss%apolmaxlh(1:isiz0,1:isize) = apolmaxlh(1:isiz0,1:isize)
  ENDIF

  IF(ALLOCATED(ss%fracpslh)) THEN
     isiz0 = maxval(ss%NSPECTRUMLH_SRC)
     isize = ss%NLHRF_SRC
     ss%fracpslh(1:isiz0,1:isize) = fracpslh(1:isiz0,1:isize)
  ENDIF
  ss%i_npoloidal = i_npoloidal

  IF(ALLOCATED(ss%rhopsi0lh)) THEN
     isize = ss%NLHRF_SRC
     ss%rhopsi0lh(1:isize) = rhopsi0lh(1:isize)
  ENDIF

  IF(ALLOCATED(ss%thegrilh)) THEN
     isize = ss%NLHRF_SRC
     ss%thegrilh(1:isize) = thegrilh(1:isize)
  ENDIF

  IF(ALLOCATED(ss%hegrilh)) THEN
     isize = ss%NLHRF_SRC
     ss%hegrilh(1:isize) = hegrilh(1:isize)
  ENDIF

  IF(ALLOCATED(ss%nthinlh)) THEN
     isize = ss%NLHRF_SRC
     ss%nthinlh(1:isize) = nthinlh(1:isize)
  ENDIF

  IF(ALLOCATED(ss%nnkparlh)) THEN
     isiz0 = maxval(ss%NSPECTRUMLH_SRC)
     isize = ss%NLHRF_SRC
     ss%nnkparlh(1:isiz0,1:isize) = nnkparlh(1:isiz0,1:isize)
  ENDIF

  IF(ALLOCATED(ss%nnktorlh)) THEN
     isiz0 = maxval(ss%NSPECTRUMLH_SRC)
     isize = ss%NLHRF_SRC
     ss%nnktorlh(1:isiz0,1:isize) = nnktorlh(1:isiz0,1:isize)
  ENDIF

  IF(ALLOCATED(ss%nnkpollh)) THEN
     isiz0 = maxval(ss%NSPECTRUMLH_SRC)
     isize = ss%NLHRF_SRC
     ss%nnkpollh(1:isiz0,1:isize) = nnkpollh(1:isiz0,1:isize)
  ENDIF


END SUBROUTINE ps_sconfig_namelist_read
