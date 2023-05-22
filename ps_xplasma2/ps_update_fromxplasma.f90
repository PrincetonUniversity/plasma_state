subroutine ps_update_fromxplasma(ps,iout,ierr)

  !  Update plasma state "ps" with the equilibrium in
  !  the modified xplasma state.
  !
  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions

  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state)  :: ps      ! plasma state object to be updated
  integer, intent(in)  :: iout    ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  integer :: id_rho, id_chi, id_rgrid, id_zgrid, id_lim
  integer :: nrho, ntheta, nr, nz, nlim
  integer :: itype, id_p, ii, jj
  integer :: iflag_rho,iflag_th,iflag_RZ,iflag_lim,iertmp
 
  integer :: id_psipol,iwarn,idi,id_R,id_Z
  integer :: kccw_Bphi, kccw_Jphi

  real(rspec), dimension(:), allocatable :: xp_rho,xp_theta,xp_rgrid,xp_zgrid
  real(rspec), dimension(:), allocatable :: xp_rlim,xp_zlim

  real(KIND=rspec), dimension(:), allocatable :: pprof
  real(KIND=rspec), dimension(:,:), allocatable :: r_geo_xpl,z_geo_xpl
  real(KIND=rspec), dimension(:,:,:), allocatable :: r_dgeo_xpl,z_dgeo_xpl

  real(KIND=rspec), parameter :: ZERO  = 0.0_rspec
  real(KIND=rspec), parameter :: TWOPI = 6.2831853071795862_rspec
  real(KIND=rspec), parameter ::    PI = 3.1415926535897931_rspec

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-------------------------------------------------------------------------
  ! executable code...

  call ps_tagchk('ps_update_fromxplasma',ps%itag,iout,ierr)
  if(ierr.ne.0) return

  if(ps_debug.ge.2) then
     write(iout,*) 'ps_update_fromxplasma: set equilibrium from modified xplasma'
  endif

  call ps_reset_mhdeq_hash(ps)

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s, ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_update_fromxplasma: invalid or unitialized xplasma object.'
     return
  endif

  !----------------------

  call xplasma_author_set(s,'EQ',ierr)
  if (ierr/=0) then
     call xplasma_error(s,ierr,iout)
     return
  end if

  ! ---------------------
  ! grab xplasma grids and limiter

  call fetch_grid("__RHO",   id_rho,   nrho,   xp_rho)   ; if (ierr/=0) goto 999
  call fetch_grid("__CHI",   id_chi,   ntheta, xp_theta) ; if (ierr/=0) goto 999
  call fetch_grid("__Rgrid", id_rgrid, nr,     xp_rgrid) ; if (ierr/=0) goto 999
  call fetch_grid("__Zgrid", id_zgrid, nz,     xp_zgrid) ; if (ierr/=0) goto 999

  call xplasma_find_item(s,'__LIMITER',id_lim,iertmp,nf_noerr=.TRUE.)
  if(id_lim.eq.0) then 
     ierr=9999
     write(iout,*) ' ?ps_update_fromxplasma: no limiter in xplasma object'
     go to 999
  endif

  call xplasma_lim_info(s, ierr, npts=nlim, itype=itype)
  if(ierr.eq.0) then
     if (itype/=100) then
        write(iout,*) ' ?ps_update_fromxplasma: can not handle a lines & circle limiter'
        ierr=9999 ; goto 999
     end if

     allocate(xp_rlim(nlim), xp_zlim(nlim))

     call xplasma_lim_info(s, ierr, rpts=xp_rlim, zpts=xp_zlim)

     if(ierr.ne.0) then
        write(iout,*) ' ?ps_update_fromxplasma: error fetching limiter points'
        ierr=9999 ; goto 999
     endif
  else
     write(iout,*) ' ?ps_update_fromxplasma: xplasma_lim_info error:'
     call xplasma_error(s,ierr,iout)
     ierr=9999 ; goto 999
  endif

  ! ---------------------
  ! check xplasma grids

  if (max(abs(xp_rho(1)),abs(xp_rho(nrho)-1.0_rspec)).gt. &
       1.0e-10_rspec) then
     write(iout,*) ' ?ps_update_fromxplasma: rho grid limits are not [0,1]'
     ierr=1 ; goto 999
  else
     call chk_spacing('__RHO',xp_rho,ierr)
     if (ierr/=0) goto 999
  endif

  if(abs(xp_theta(ntheta)-xp_theta(1)-twopi).gt.1.0e-1_rspec) then   ! theta can be unevenly spaced
     !  difference not close to twopi
     write(iout,*) ' ?ps_update_fromxplasma: theta grid limits do not have 2pi range'
     ierr=1 ; goto 999
  endif

  ! ---------------------
  ! check xplasma grids against defined plasma_state grids

  iflag_rho = 0
  iflag_th  = 0
  iflag_RZ  = 0
  iflag_lim = 0

  if(ps%nrho_eq.eq.0 .or. .not. allocated(ps%rho_eq)) then
     call simcon_check('ps%nrho_eq'); if (ierr.ne.0) go to 999
     ps%nrho_eq = nrho
     iflag_rho=1
  else if (ps%nrho_eq/=nrho) then
     write(iout,*) ' ?ps_update_fromxplasma: mismatch in rho grid size between plasma_state and xplasma'
     ierr=1 ; goto 999
  else if (ps%rho_eq(nrho)<0.5_rspec) then
     call simcon_check('ps%nrho_eq'); if (ierr.ne.0) go to 999      ! might be allocated and not defined
  else if (maxval(abs(xp_rho-ps%rho_eq))>1.e-5_rspec) then
     write(iout,*) ' ?ps_update_fromxplasma: rho grid difference between plasma_state and xplasma'   ! probably an unevenly space ps grid
     ierr=1 ; goto 999
  end if

  if(ps%nth_eq.eq.0 .or. .not. allocated(ps%th_eq)) then
     call simcon_check('ps%nth_eq'); if (ierr.ne.0) go to 999
     ps%nth_eq = ntheta
     iflag_th=1
  else if (ps%nth_eq/=ntheta) then
     write(iout,*) ' ?ps_update_fromxplasma: mismatch in theta grid size between plasma_state and xplasma'
     ierr=1 ; goto 999
  else if (ps%th_eq(ntheta)<0.5_rspec) then
     call simcon_check('ps%nth_eq'); if (ierr.ne.0) go to 999      ! might be allocated and not defined
  else if (maxval(abs(xp_theta-ps%th_eq))>1.e-3_rspec) then
     write(iout,*) ' ?ps_update_fromxplasma: theta grid difference between plasma_state and xplasma'  
     ierr=1 ; goto 999
  end if

  if(ps%num_rzlim.eq.0 .or. .not. allocated(ps%rlim)) then
     call mdescr_check('ps%num_rzlim'); if (ierr.ne.0) go to 999
     ps%num_rzlim = nlim
     iflag_lim=1
  else if (ps%num_rzlim/=nlim) then
     write(iout,*) ' ?ps_update_fromxplasma: mismatch in limiter size between plasma_state and xplasma'
     ierr=1 ; goto 999
  else if (maxval(abs(xp_rlim-ps%rlim))>1.e-6_rspec .or. maxval(abs(xp_zlim-ps%zlim))>1.e-6_rspec) then
     call mdescr_check('ps%num_rzlim'); if (ierr.ne.0) go to 999
  end if

  if(ps%nR.eq.0 .or. .not. allocated(ps%R_grid)) then
     call simcon_check('ps%nR'); if (ierr.ne.0) go to 999
     ps%nR = nr
     ps%nZ = nz
     iflag_RZ=1
  else if (ps%nR/=nr .or. ps%nZ/=nz) then
     write(iout,*) ' ?ps_update_fromxplasma: mismatch in R,Z grid size between plasma_state and xplasma'
     ierr=1 ; goto 999
  else if (minval(ps%R_grid)<=0._rspec) then
     call simcon_check('ps%nR'); if (ierr.ne.0) go to 999      ! might be allocated and not defined
  else if (maxval(abs(xp_rgrid-ps%R_grid))>1.e-6_rspec .or. maxval(abs(xp_zgrid-ps%Z_grid))>1.e-6_rspec) then
     write(iout,*) ' ?ps_update_fromxplasma: R,Z grid difference between plasma_state and xplasma'  
     ierr=1 ; goto 999
  end if
 
  !  fetch kmom; store in state element ps%nmom
  call xplasma_global_info(s, ierr, kmom=ps%nmom )
  if(ierr.ne.0) then
     write(iout,*) ' %ps_update_fromxplasma: error in xplasma_global_info:'
     call xplasma_error(s,ierr,iout)
     go to 999
  endif

  ! convert P(rho) from Hermite to Spline as this is what plasma state expects

  call xplasma_profId(s,'P',id_p)
  if (id_p<=0) then
     write(iout,*) " %ps_update_fromxplasma: could not find pressure profile 'P'"
     ierr=1 ; goto 999
  end if

  call xplasma_prof_info(s,id_p,ierr, gridId1=id_rho, splineType=itype)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_update_fromxplasma: error in xplasma_prof_info:'
     call xplasma_error(s,ierr,iout)
     go to 999
  endif

  if (itype/=2) then
     allocate(pprof(nrho))
 
     call xplasma_eval_prof(s,id_p,pprof,ierr)
     if(ierr.ne.0) then
        write(iout,*) ' %ps_update_fromxplasma: error in xplasma_eval_prof:'
        call xplasma_error(s,ierr,iout)
        go to 999
     endif

     call xplasma_create_1dprof(s,'P',id_rho,pprof,id_p,ierr, &
          ispline=2,ibca=1,zbca=ZERO,ibcb=0, &
          label='MHD equilibrium pressure',units='Pa')
     if(ierr.ne.0) then
        write(iout,*) ' %ps_update_fromxplasma: error in xplasma_create_1dprof'
        call xplasma_error(s,ierr,iout)
        go to 999
     endif
  end if


  !----------------------
  !  allocate arrays associated with grids whose sizes were just defined
  !  (if necessary)

  if (iflag_rho>0) write(iout,*) ' %ps_update_fromxplasma: allocating nrho   = ',nrho
  if (iflag_th>0)  write(iout,*) ' %ps_update_fromxplasma: allocating ntheta = ',ntheta
  if (iflag_RZ>0)  write(iout,*) ' %ps_update_fromxplasma: allocating nr,nz  = ',nr,",",nz
  if (iflag_lim>0) write(iout,*) ' %ps_update_fromxplasma: allocating nlim   = ',nlim

  ierr=0

  if(max(iflag_rho,iflag_th,iflag_RZ,iflag_lim).gt.0) then
     call plasma_state_empty_alloc(ps,ierr)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_update_fromxplasma: error in plasma_state_empty_alloc.'
        go to 999
     endif
  endif

  ps%rho_eq = xp_rho
  ps%th_eq  = xp_theta
  
  ps%R_grid = xp_rgrid
  ps%Z_grid = xp_zgrid

  ps%rlim = xp_rlim
  ps%zlim = xp_zlim

  !----------------------
  !  load state copy of Psi(R,Z)

  call xplasma_profId(s,'Psi_RZ',id_psipol)
  if(id_psipol.eq.0) then
     ierr=1
     write(iout,*) ' ?ps_update_fromxplasma: "Psi_RZ" not found.'
     go to 999
  endif

  call xplasma_eval_prof(s,id_psipol,ps%psirz,ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_update_fromxplasma: xplasma error fetching PsiRZ:'
     call xplasma_error(s,ierr,iout)
     go to 999
  endif
  ps%id_psirz=id_psipol

  !---------------------------------------------------------
  !  OK -- set scalar EQ data

  !  R & Z extrema -- see subroutine, below
  !  check for change; if there is a change; check machine description lock.

  call ps_update_rzbox(ps,iout,ierr)
  if (ierr/=0) goto 999

  !  Field signs; check for shot configuration lock...

  call xplasma_global_info(s,ierr, bphi_ccw = kccw_Bphi, jphi_ccw = kccw_Jphi)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_update_fromxplasma: unexpected  xplasma_global_info call error:'
     call xplasma_error(s,ierr,iout)
     go to 999
  endif

  if((kccw_Bphi.ne.ps%kccw_Bphi).or.(kccw_Jphi.ne.ps%kccw_Jphi)) then
     if(ps%lock_shot_config.eq.ps_locked) then
        ierr=9999
        write(iout,*) &
             ' ?ps_update_fromxplasma: attempt to change field orientation when'
        write(iout,*) &
             '  the shot configuration data has already been locked.'
        write(iout,*) ' kccw_Bphi locked value: ',ps%kccw_Bphi, &
             '; new value:',kccw_Bphi
        write(iout,*) ' kccw_Jphi locked value: ',ps%kccw_Jphi, &
             '; new value:',kccw_Jphi
        go to 999
     endif
     if(ps%lock_shot_config.eq.ps_monitor) then
        write(iout,*) ' %ps_update_fromxplasma: field orientations set:'
        write(iout,*) '  kccw_Bphi = ',kccw_Bphi,'; kccw_Jphi=',kccw_Jphi
     endif
     ps%kccw_Bphi = kccw_Bphi
     ps%kccw_Jphi = kccw_Jphi
  endif

  ! --scalars derived from equilibrium and field

  call ps_mhdeq_derived_scalars(ps,iout,ierr)
  if(ierr.ne.0) go to 999

  ! profiles vs. (rho)

  call eqpro('P',ps%P_eq,ps%id_P_eq); if(ierr.ne.0) go to 999
  call eqpro('G',ps%g_eq,ps%id_g_eq); if(ierr.ne.0) go to 999
  call eqpro('Q',ps%q_eq,ps%id_q_eq); if(ierr.ne.0) go to 999
  call eqpro('PHITOR',ps%phit,ps%id_phit); if(ierr.ne.0) go to 999
  call eqpro('PSI',ps%psipol,ps%id_psipol); if(ierr.ne.0) go to 999
  call eqpro('ICURT',ps%curt,ps%id_curt); if(ierr.ne.0) go to 999

  allocate(r_geo_xpl(ps%nth_eq,ps%nrho_eq))
  allocate(z_geo_xpl(ps%nth_eq,ps%nrho_eq))
  allocate(r_dgeo_xpl(3,ps%nth_eq,ps%nrho_eq))
  allocate(z_dgeo_xpl(3,ps%nth_eq,ps%nrho_eq))

  call xplasma_profId(s,'R',id_R)
  call xplasma_profId(s,'Z',id_Z)

  call xplasma_eval_prof(s,id_R,r_geo_xpl,ierr,zcoeff=r_dgeo_xpl)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_update_fromxplasma: R_geo fetch error:'
     call xplasma_error(s,ierr,iout)
     go to 999
  endif

  call xplasma_eval_prof(s,id_Z,z_geo_xpl,ierr,zcoeff=z_dgeo_xpl)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_update_fromxplasma: Z_geo fetch error:'
     call xplasma_error(s,ierr,iout)
     go to 999
  endif

  ps%id_R_geo = id_R
  ps%id_Z_geo = id_Z

  do jj=1,ps%nth_eq
     do ii=1,ps%nrho_eq
        ps%R_geo(ii,jj) = r_geo_xpl(jj,ii)
        ps%Z_geo(ii,jj) = Z_geo_xpl(jj,ii)

        ps%dR_geo_drho(ii,jj) = r_dgeo_xpl(2,jj,ii)
        ps%dR_geo_dth(ii,jj) = r_dgeo_xpl(1,jj,ii)
        ps%d2R_geo_drhodth(ii,jj) = r_dgeo_xpl(3,jj,ii)

        ps%dZ_geo_drho(ii,jj) = z_dgeo_xpl(2,jj,ii)
        ps%dZ_geo_dth(ii,jj) = z_dgeo_xpl(1,jj,ii)
        ps%d2Z_geo_drhodth(ii,jj) = z_dgeo_xpl(3,jj,ii)
     enddo
  enddo

  call eqpro2d('BPHI_RZ',ps%BphiRZ,ps%id_BphiRZ); if(ierr.ne.0) go to 999
  call eqpro2d('BR_RZ',ps%BRRZ,ps%id_BRRZ); if(ierr.ne.0) go to 999
  call eqpro2d('BZ_RZ',ps%BZRZ,ps%id_BZRZ); if(ierr.ne.0) go to 999

  !---------------------------------------------------------
  ! set EQ derived profiles...

  call ps_mhdeq_volumes(ps,iout,ierr)
  if(ierr.ne.0) go to 999

  call ps_mhdeq_fsmoms(ps,iout,ierr)
  if(ierr.ne.0) go to 999

  call ps_update_jdotb(ps,iout,ierr)
  if(ierr.ne.0) go to 999

999 continue
  call xplasma_author_clear(s,'EQ',iertmp)
  if (iertmp/=0) then
     call xplasma_error(s,iertmp,iout)
     ierr=1
  end if

  if (allocated(xp_rho))   deallocate(xp_rho)     ! deallocate just in case
  if (allocated(xp_theta)) deallocate(xp_theta)
  if (allocated(xp_rgrid)) deallocate(xp_rgrid)
  if (allocated(xp_zgrid)) deallocate(xp_zgrid)
  if (allocated(xp_rlim))  deallocate(xp_rlim)
  if (allocated(xp_zlim))  deallocate(xp_zlim)
  if (allocated(pprof))    deallocate(pprof)

  if (allocated(r_geo_xpl))  deallocate(r_geo_xpl)
  if (allocated(z_geo_xpl))  deallocate(z_geo_xpl)
  if (allocated(r_dgeo_xpl)) deallocate(r_dgeo_xpl)
  if (allocated(z_dgeo_xpl)) deallocate(z_dgeo_xpl)

  if(ierr.ne.0) then
     ierr=999
     write(iout,*) ' ?ps_update_fromxplasma: an error was detected.'
  else
     if(ps_debug.ge.2) then
        write(iout,*) ' ps_update_fromxplasma: MHD equilibrium OK, normal exit.'
        write(iout,*) ' '
     endif
  endif

  CONTAINS

    subroutine fetch_grid(sd,id,nd,xd)
      character*(*),intent(in)  :: sd     ! name of grid
      integer,      intent(out) :: id     ! id of grid
      integer,      intent(out) :: nd     ! size of grid
      real(rspec),  allocatable :: xd(:)  ! allocated grid if no error
      
      ierr=0
      nd=0
      
      call xplasma_gridId(s, sd, id)
      if (id<=0) then
         write(iout, *) ' %ps_update_fromxplasma: unable to find '//sd//' grid'
         ierr=1 ; return
      end if
      
      call xplasma_grid_size(s, id, nd, ierr)
      if (ierr/=0) nd=0
      if (nd<=0) then
         write(iout, *) ' %ps_update_fromxplasma: error fetching size of '//sd//' grid'
         ierr=1 ; return
      end if
      
      allocate(xd(nd))
      
      call xplasma_grid(s,id,xd,ierr)
      if (ierr/=0) then
         write(iout, *) ' %ps_update_fromxplasma: error fetching data for '//sd//' grid'
         ierr=1 ; return
      end if
    end subroutine fetch_grid


    subroutine chk_spacing(gname, grid, iflag)
      character*(*), intent(in) :: gname
      real(kind=rspec), dimension(:), intent(in) :: grid
      integer, intent(out) :: iflag

      integer :: isize,ii
      real(kind=rspec) :: dxg

      iflag = 1  ! signal unevenly spaced grid or empty grid
      isize = size(grid)
      if(isize.lt.2) then
         write(iout,*) ' ?ps_update_fromxplasma: '//gname//' grid is too small'
         return
      end if

      iflag = 0  ! signal evenly spaced grid
      if(isize.eq.2) return

      dxg = (grid(isize) - grid(1))/(isize-1)
      if(dxg.le.0.0_rspec) then
         iflag = 1
         write(iout,*) ' ?ps_update_fromxplasma: '//gname//' grid has null range'
         return
      endif

      do ii=1,isize-1
         if(abs(dxg - (grid(ii+1)-grid(ii))).gt.1.0e-9_rspec*dxg) then
            iflag = 1
            write(iout,*) ' ?ps_update_fromxplasma: '//gname//' grid not evenly spaced'
            return
         endif
      enddo

    end subroutine chk_spacing

    subroutine mdescr_check(name_in)
      character*(*), intent(in) :: name_in

      if(ps%lock_machine_descr.eq.ps_locked) then
         ierr=1
         write(iout,*) ' ?ps_update_fromxplasma: machine description is locked;'
         write(iout,*) '  detected request to change: ',trim(name_in)
      endif
    end subroutine mdescr_check

    subroutine simcon_check(name_in)
      character*(*), intent(in) :: name_in

      if(ps%lock_sim_init.eq.ps_locked) then
         ierr=1
         write(iout,*) ' ?ps_update_fromxplasma: simulation initialization is locked;'
         write(iout,*) '  detected request to change: ',trim(name_in)
      endif
    end subroutine simcon_check

    subroutine eqpro(name_in,array,id)
      character*(*), intent(in) :: name_in
      REAL(KIND=rspec), dimension(:), intent(out) :: array ! result
      integer,intent(out) :: id

      !  fetch profile on original rho_eq grid

      call xplasma_profId(s,name_in,idi)
      if(idi.eq.0) then
         write(iout,*) ' ?ps_update_fromxplasma: expected profile not found after g file read: ',trim(name_in)
         ierr = 1
         array = ZERO
         id = 0
         return
      endif

      call xplasma_eval_prof(s,idi,array,ierr)
      if(ierr.ne.0) then
         write(iout,*) ' ?ps_update_fromxplasma: xplasma_eval_prof error:'
         call xplasma_error(s,ierr,iout)
         array = ZERO
         id = 0
         return
      endif

      id = idi

    end subroutine eqpro

    subroutine eqpro2d(name_in,array,id)
      character*(*), intent(in) :: name_in
      REAL(KIND=rspec), dimension(:,:), intent(out) :: array ! result
      integer, intent(out) :: id

      !  fetch profile on original 2d grid

      call xplasma_profId(s,name_in,idi)
      if(idi.eq.0) then
         write(iout,*) ' ?ps_update_fromxplasma: expected profile not found after g file read: ',trim(name_in)
         ierr = 1
         array = ZERO
         id = 0
         return
      endif

      call xplasma_eval_prof(s,idi,array,ierr)
      if(ierr.ne.0) then
         write(iout,*) ' ?ps_update_fromxplasma: xplasma_eval_prof error:'
         call xplasma_error(s,ierr,iout)
         array = ZERO
         id = 0
         return
      endif

      id = idi

    end subroutine eqpro2d

end subroutine ps_update_fromxplasma


subroutine ps_reset_mhdeq_hash(ps)

  !  Reset hash codes related to MHDEQ-derived quantities:  good to do
  !  when the geometry is being explicitly updated.

  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated

  !-----------------------
  !  local:

  integer :: ia,iaddr1,iaddr2

  !-----------------------

  iaddr1 = ps_hash_aux1
  iaddr2 = size(ps%hash_codes)

  do ia=iaddr1,iaddr2
     ps%hash_codes(ia)=ps_unInit
  enddo

end subroutine ps_reset_mhdeq_hash

subroutine ps_mhdeq_derived_scalars(ps,iout,ierr)

  !  Update plasma state "ps" with derived scalars from equilibrium
  !  XPLASMA2 implementation -- DMC Oct 10 2006

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated
  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------
  !  local:

  integer :: idg,idum
  real(KIND=rspec), parameter :: ONE  = 1.0_rspec

  !-----------------------
  !    PLASMA (LCFS = last closed flux surface-- although actually the 
  !    boundary will be shifted in slightly from the precise separatrix
  !    surface, in cases of diverted plasmas).

  call ps_tagchk('ps_mhdeq_derived_scalars',ps%itag,iout,ierr)
  if(ierr.ne.0) return

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_update_fromxplasma: invalid or unitialized xplasma object.'
     return
  endif

  call xplasma_RZminmax_plasma(s, &
       ps%R_min_lcfs, ps%R_max_lcfs, ps%Z_min_lcfs, ps%Z_max_lcfs, &
       ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_mhdeq_derived_scalars: unexpected xplasma_RZminmax_plasma call error:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  !  mod(B) extrema

  call xplasma_Bminmax(s, ONE, ierr, bmin=ps%B_min_lcfs, bmax=ps%B_max_lcfs)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_mhdeq_derived_scalars: unexpected xplasma_Bminmax call error:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  !  Axial values

  call xplasma_mag_axis(s,ierr, &
       Raxis=ps%R_axis, Zaxis=ps%Z_axis, Baxis=ps%B_axis)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_mhdeq_derived_scalars: unexpected xplasma_mag_axis call error:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  !  Vacuum field -- g(1)/R_axis

  call xplasma_profId(s,'G',idg)
  call xplasma_eval_prof(s,idg,ONE,ps%B_axis_vac,ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_mhdeq_derived_scalars: unexpected "G" xplasma_eval_prof call error:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  ps%B_axis_vac = ps%B_axis_vac/ps%R_axis  ! |B_vac @ axis| = g(1)/R_axis

  call xplasma_get_psi0(s,ps%Psi_to_machine_axis,idum,ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_mhdeq_derived_scalars: unexpected xplasma_get_Psi0 call error:'
     call xplasma_error(s,ierr,iout)
     return
  endif

end subroutine ps_mhdeq_derived_scalars

subroutine ps_update_fromxplasma_integ(ps,iout,idi,ierr)

  !  Called from ps_mhdeq_volumes

  !  Create an integrator for flux surface integrals and averages
  !  on the ps%rho_eq grid 

  !  Integrator ownership set to xplasma_xmhd --> removed when equilibrium
  !  changes.

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps       ! plasma state object to be updated
  integer, intent(in) :: iout     ! Fortran LUN for messages

  integer, intent(out) :: idi     ! ID of integrator, returned (0 if error)
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-----------------------
  !  local:
  integer :: iertmp
  character*32, parameter :: iiname = '_ps_update_fromxplasma_integ          '

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_blkbxId(s,iiname,idi)
  if(idi.ne.0) return

  !  integrator not found: create it...

  call xplasma_author_set(s,xplasma_xmhd,iertmp)

  call xplasma_create_integ(s,trim(iiname),ps%rho_eq,idi,ierr, &
       cache_enable = .TRUE.)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_update_fromxplasma_integ: unexpected xplasma error:'
     call xplasma_error(s,ierr,iout)
  endif

  call xplasma_author_clear(s,xplasma_xmhd,iertmp)

end subroutine ps_update_fromxplasma_integ

subroutine ps_mhdeq_volumes(ps,iout,ierr)

  !  Update plasma state "ps" with derived profiles from equilibrium:
  !  enclosed volumes, cross sectional areas, surface areas of flux
  !  surfaces, and poloidal path length.

  !  A hash code test is used to determine whether the profiles need
  !  updating.  If they do, then so do all other flux surface averages;
  !  the others are then zeroed out here.  Separate routines are provided
  !  for computation of these additional flux surface averages and
  !  integrations.

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated
  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-----------------------
  !  local:

  integer :: ii,jj,idi,kf1,kf2,idiff,iaddr

  real(KIND=rspec), parameter :: ZERO = 0.0_rspec

  integer :: loc_cclist(ps_ccount)
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  call ps_tagchk('ps_mhdeq_volumes',ps%itag,iout,ierr)
  if(ierr.ne.0) return

  if(ps%nrho_eq.eq.0) then
     ierr=1
     write(iout,*) ' ?ps_update_volumes: called but no rho_eq grid found.'
     return
  endif

  !-----------------------
  !  hash code test

  loc_cclist = ps_cczero
  loc_cclist(ps_cnum_EQ)=ps_chkProfs  ! update EQ profile hashcodes only...
  call plasma_state_hash_code(ps,loc_cclist,ierr)
  if(ierr.ne.0) return

  idiff=0
  call ps_rzsurf_hash(ps,kf1,kf2)

  iaddr=ps_hash_aux1

  ps%new_hash_codes(iaddr)=kf1
  if(kf1.ne.ps%hash_codes(iaddr)) then
     ps%hash_codes(iaddr)=kf1
     idiff=idiff+1
  endif

  iaddr=iaddr+1
  ps%new_hash_codes(iaddr)=kf2
  if(kf2.ne.ps%hash_codes(iaddr)) then
     ps%hash_codes(iaddr)=kf2
     idiff=idiff+1
  endif

  if(idiff.eq.0) return

  !-----------------------
  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_mhdeq_volumes: invalid or unitialized xplasma object.'
     return
  endif

  ! setup up for flux surface averaging and integration...

  call ps_update_fromxplasma_integ(ps,iout,idi,ierr)
  if(ierr.ne.0) then
     write(iout,*) ' (error in call from ps_mhdeq_volumes).'
     return
  endif

  ! compute flux surface averages and integrals -- surface oriented -- save
  ! as profiles

  call doi('Vol',ps%vol); if(ierr.ne.0) return
  call doi('Area',ps%area); if(ierr.ne.0) return
  call doi('Surf',ps%surf); if(ierr.ne.0) return
  call doi('Lpol',ps%lpol); if(ierr.ne.0) return

  ! auxilliary flux surface averages NOT recomputed here...
  ! (a time interpolation of these quantities may suffice).

#ifdef __DEBUG
  if(allocated(ps%gr2i)) then
     write(iout,*) 'ps_mhdeq_volumes: updates possible for: <1/R**2>, etc.'
  endif
#endif

  CONTAINS

    subroutine doi(ilabel,array)
      character*(*), intent(in) :: ilabel     ! integration tag
      REAL(KIND=rspec), dimension(:), intent(out) :: array ! result

      !  compute numerical integration; store results in array

      array = ZERO
      call xplasma_rho_zonint(s,idi,ilabel,array,ierr)
      if(ierr.ne.0) then
         write(iout,*) ' ?ps_mhdeq_volumes: unexpected error in xplasma_rho_zonint("',ilabel,'",...)'
         call xplasma_error(s,ierr,iout)
      endif

    end subroutine doi

end subroutine ps_mhdeq_volumes

subroutine ps_rzsurf_hash(ps,kf1,kf2)

  !------------------------------------------------------------
  ! compute hash of hash codes for: rho_eq grid, th_eq grid, R_geo, Z_geo,
  ! and derivatives.
  !
  ! this routine assumes that ps%new_hash_codes(...) have been updated,
  ! at least for the EQ component.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_hash_code
  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be checked
  integer, intent(out) :: kf1,kf2         ! hash code pair

  !-----------------------
  integer :: iaddr,ipair(2),iout
  !-----------------------

  kf1=0
  kf2=0
  if(.not.allocated(ps%rho_eq)) return
  if(.not.allocated(ps%th_eq)) return

  iaddr=ps_dhash_nrho_eq

  kf1=ps%new_hash_codes(iaddr)
  kf2=ps%new_hash_codes(iaddr+1)

  call addpair(ps_dhash_nth_eq)

  call addpair(ps_phash_R_geo)
  call addpair(ps_phash_dR_geo_drho)
  call addpair(ps_phash_dR_geo_dth)
  call addpair(ps_phash_d2R_geo_drhodth)

  call addpair(ps_phash_R_geo)
  call addpair(ps_phash_dR_geo_drho)
  call addpair(ps_phash_dR_geo_dth)
  call addpair(ps_phash_d2R_geo_drhodth)

  if(ps_debug.ge.2) then
     iout=SWIM_err_out_file
     write(iout,*) ' ........ps_rzsurf_hash:  ',kf1,kf2
  endif

  contains
    subroutine addpair(iarg)
      integer :: iarg

      iaddr=iarg
      ipair(1)=ps%new_hash_codes(iaddr)
      ipair(2)=ps%new_hash_codes(iaddr+1)

      call hashCode(ipair,kf1,kf2)
    end subroutine addpair
end subroutine ps_rzsurf_hash

subroutine ps_mhdeq_fsmoms(ps,iout,ierr)

  ! extract Fourier Spline moments (fsmoms) from xplasma representation
  ! make these available in the Plasma State object (ps)

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps       ! plasma state object to be updated
  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-----------------------
  integer :: im,inorm
  integer :: ikmom,ikmaxe
  character*3 :: zint

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  call ps_tagchk('ps_mhdeq_fsmoms',ps%itag,iout,ierr)
  if(ierr.ne.0) return

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  if(ps%neqmom.eq.0) then
     ikmom=ps%nmom
     ps%neqmom = ikmom
     call plasma_state_empty_alloc(ps,ierr)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_mhdeq_fsmoms: moments array alloc failure.'
        return
     endif
     ! plasma state neqmom dimension labels...
     do im=1,ikmom
        zint=' '
        if(im.le.9) then
           write(zint(1:1),'(I1)') im
        else if(im.le.99) then
           write(zint(1:2),'(I2)') im
        else
           write(zint(1:3),'(I3)') im
        endif
        ps%eqmom_num(im)=zint
     enddo
  else
     ikmom=ps%neqmom
     ps%nmom=ikmom
  endif

  ikmaxe=1  ! higher moments scaled by 1/x only, not 1/x**N, N>1.

  call xplasma_kmom_set(s,ikmom,ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_mhdeq_fsmoms: xplasma_kmom_set error:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  call xpmom_free(s)

  ! recompute and extract Fourier Spline (scaled moments) & derivatives

  inorm=1  ! return scaled moments xRj, xRj~, xZj, xZj~

  ! R = R0 + sum(j=1:nmom)[rho*(xRj*cos(j*theta)+xRj~*sin(j*theta))]
  ! Z = Z0 + sum(j=1:nmom)[rho*(xZj~*cos(j*theta)+xZj*sin(j*theta))]
  !     (scaled moment needs factor applied to get to sin & cos coefficients)

  call xpmom_get1(s,ps%rho_eq,inorm,0,ierr, &
       rcmom=ps%R0_momeq, zcmom=ps%Z0_momeq, ideriv=0)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_mhdeq_fsmoms: xpmom_get1 (R0,Z0) error: '
     call xplasma_error(s,ierr,iout)
     return
  endif

  call xpmom_get1(s,ps%rho_eq,inorm,0,ierr, &
       rcmom=ps%dR0_momeq_drho, zcmom=ps%dZ0_momeq_drho, ideriv=1)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_mhdeq_fsmoms: xpmom_get1 (R0,Z0 derivatives) error: '
     call xplasma_error(s,ierr,iout)
     return
  endif

  do im=1,ps%neqmom

     call xpmom_get1(s,ps%rho_eq,inorm,im,ierr, &
          rcmom=ps%xRjcos_momeq(:,im), rsmom=ps%xRjsin_momeq(:,im), &
          zcmom=ps%xZjcos_momeq(:,im), zsmom=ps%xZjsin_momeq(:,im), &
          ideriv = 0)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_mhdeq_fsmoms: xpmom_get1 (Rj,Zj) error: '
        call xplasma_error(s,ierr,iout)
        return
     endif

     call xpmom_get1(s,ps%rho_eq,inorm,im,ierr, &
          rcmom=ps%dxRjcos_momeq_drho(:,im), &
          rsmom=ps%dxRjsin_momeq_drho(:,im), &
          zcmom=ps%dxZjcos_momeq_drho(:,im), &
          zsmom=ps%dxZjsin_momeq_drho(:,im), &
          ideriv = 1)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_mhdeq_fsmoms: xpmom_get1 (Rj,Zj deriv.) error: '
        call xplasma_error(s,ierr,iout)
        return
     endif

  enddo

end subroutine ps_mhdeq_fsmoms

subroutine ps_update_brz(ps,iout,ierr)

  ! after Psi(R,Z), Psi(rho), [R,Z](rho,theta), and g(rho) have been set up
  ! and the field components computed, retrieve these for the plasma state.

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated
  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-----------------------
  !  local:
  integer :: iersum
  integer :: id_Bphi,id_BR,id_BZ
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  call ps_tagchk('ps_update_brz',ps%itag,iout,ierr)
  if(ierr.ne.0) return

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_update_fromxplasma: invalid or unitialized xplasma object.'
     return
  endif

  iersum = 0

  call xplasma_profId(s,'BPHI_RZ',id_Bphi)
  if(id_Bphi.eq.0) then
     write(iout,*) ' ?ps_update_brz:  BPHI_RZ expected, not found in xplasma.'
     iersum = iersum + 1
  endif

  call xplasma_profId(s,'BR_RZ',id_BR)
  if(id_Bphi.eq.0) then
     write(iout,*) ' ?ps_update_brz:  BR_RZ expected, not found in xplasma.'
     iersum = iersum + 1
  endif

  call xplasma_profId(s,'BZ_RZ',id_BZ)
  if(id_Bphi.eq.0) then
     write(iout,*) ' ?ps_update_brz:  BZ_RZ expected, not found in xplasma.'
     iersum = iersum + 1
  endif

  if(iersum.ne.0) then
     ierr=9999
     return
  endif

  call xplasma_eval_prof(s,id_Bphi,ps%BphiRZ,ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_update_fromxplasma: xplasma error fetching Bphi_RZ:'
     call xplasma_error(s,ierr,iout)
     iersum = iersum + 1
  endif
  ps%id_BphiRZ = id_Bphi

  call xplasma_eval_prof(s,id_BR,ps%BRRZ,ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_update_fromxplasma: xplasma error fetching BR_RZ:'
     call xplasma_error(s,ierr,iout)
     iersum = iersum + 1
  endif
  ps%id_BRRZ = id_BR

  call xplasma_eval_prof(s,id_BZ,ps%BZRZ,ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_update_fromxplasma: xplasma error fetching BZ_RZ:'
     call xplasma_error(s,ierr,iout)
     iersum = iersum + 1
  endif
  ps%id_BZRZ = id_BZ

  if(iersum.ne.0) then
     ierr=9999
     return
  endif
  
end subroutine ps_update_brz

subroutine ps_update_rzbox(ps,iout,ierr)

  !------------------------------------------------------------
  ! update (R,Z) extrema -- checking that the machine description
  ! is not locked, if there is a change.
  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated
  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-----------------------
  integer :: iflag
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------
  ! set box extrema from (R,Z) GRID

  call ps_tagchk('ps_update_rzbox',ps%itag,iout,ierr)
  if(ierr.ne.0) return

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_update_fromxplasma: invalid or unitialized xplasma object.'
     return
  endif

  if(.not.allocated(ps%R_grid)) return
  if(.not.allocated(ps%Z_grid)) return

  if((ps%R_min_box.ne.ps%R_grid(1)).OR. &
       (ps%R_max_box.ne.ps%R_grid(ps%nR)).OR. &
       (ps%Z_min_box.ne.ps%Z_grid(1)).OR. &
       (ps%Z_max_box.ne.ps%Z_grid(ps%nZ))) then

     if(ps%lock_machine_descr.eq.ps_locked) then
        iflag = 1
        ierr = 9999
        write(iout,*) ' ?ps_update_rzbox: attempt to update [R,Z] box after'
        write(iout,*) '  machine description is locked:'
     else if(ps%lock_machine_descr.eq.ps_monitor) then
        iflag = 1
        ierr = 0
        write(iout,*) ' %ps_update_rzbox: change in [R,Z] box extrema:'
     else
        iflag = 0
        ierr = 0
     endif

     if(iflag.eq.1) then
        write(iout,'(4(1x,a13))') ' ','box',' ','grid'
        write(iout,'(2(1x,a13,1x,1pe13.6))') &
             'R_min_box',ps%R_min_box,'R_grid(1)',ps%R_grid(1)
        write(iout,'(2(1x,a13,1x,1pe13.6))') &
             'R_max_box',ps%R_max_box,'R_grid(nR)',ps%R_grid(ps%nR)
        write(iout,'(2(1x,a13,1x,1pe13.6))') &
             'Z_min_box',ps%Z_min_box,'Z_grid(1)',ps%Z_grid(1)
        write(iout,'(2(1x,a13,1x,1pe13.6))') &
             'Z_max_box',ps%Z_max_box,'Z_grid(nZ)',ps%Z_grid(ps%nZ)
     endif

     if(ierr.eq.0) then
        ps%R_min_box = ps%R_grid(1)
        ps%R_max_box = ps%R_grid(ps%nR)

        ps%Z_min_box = ps%Z_grid(1)
        ps%Z_max_box = ps%Z_grid(ps%nZ)
     endif
  endif

end subroutine ps_update_rzbox
