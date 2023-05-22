subroutine ps_update_mhdeq(g_filename,crat,kpsi_cur,rho_curbrk,ps,iout,ierr)

  !  Update plasma state "ps" with equilibrium from g file.
  !  XPLASMA2 implementation -- DMC Oct 10 2006

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions

  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  character*(*), intent(in) :: g_filename ! filename
  real*8, intent(in) :: crat              ! minimum allowed bdy curvature ratio
  integer, intent(in) :: kpsi_cur         ! select method for getting I(rho)
  real*8, intent(in) :: rho_curbrk        ! interpolation break pt for I(rho)
  type (plasma_state) :: ps               ! plasma state object to be updated
  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-----------------------
  !  crat -- curvature ratio -- controls offset of bdy surface from separatrix
  !
  !  kpsi_cur -- =0: use EFIT file data for total toroidal current and for
  !                  q(psi);
  !              =1: compute q(psi) and total current from Psi(R,Z) data; EFIT
  !                  file data for these quantities is discarded.
  !
  !  rho_curbrk -- normalize sqrt tor flux location (btw 0.75 and 0.95) where
  !                transition from Psi(R,Z) computation of I(rho) to parabolic
  !                interpolation to reach boundary value occurs
  !    
  !-----------------------
  !  local:

  integer :: inrho,inth,i2pi,iwarn,iertmp
  integer :: ikmom
  real(KIND=rspec), parameter :: TWOPI = 6.2831853071795862_rspec
  real(KIND=rspec), parameter ::    PI = 3.1415926535897931_rspec

  real(KIND=rspec), parameter ::  zvar = 0.00010_rspec  ! see also: ps_xload_check.f90...

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-------------------------------------------------------------------------
  ! executable code...

  call ps_tagchk('ps_update_mhdeq',ps%itag,iout,ierr)
  if(ierr.ne.0) return

  if(ps_debug.ge.2) then
     write(iout,*) 'ps_update_mhdeq: set equilibrium from g_file: '// &
          trim(g_filename)
  endif

  call ps_reset_mhdeq_hash(ps)

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_update_mhdeq: invalid or unitialized xplasma object.'
     return
  endif

  ! allow more than the default det(J) variation...
  !   zvar is related to a limit on min(det(J))/max(det(J)) over each
  !   flux surface.

  call xplasma_ajac_maxvar_set(s,zvar,ierr)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_update_mhdeq: xplasma_ajac_maxvar_set error.'
     return
  endif

  !----------------------

  call xplasma_author_set(s,'EQ',ierr)
  if (ierr/=0) then
     call xplasma_error(s,ierr,iout)
     return
  end if

  !----------------------
  !  G-eqdsk filename ***
  ps%eqdsk_file = g_filename

  !----------------------
  !  get rho,theta dimensions to use for geqdsk read

  if(ps%nrho_eq.le.0) then
     write(iout,*) ' %ps_update_mhdeq: default "rho_eq" grid size set = 51'
     inrho = 51  ! default radial resolution
  else
     inrho = ps%nrho_eq
  endif

  i2pi = 0

  if(ps%nth_eq.le.0) then
     inth = 2*(inrho-1) + 1
     i2pi=1
  else
     inth = ps%nth_eq
     if(allocated(ps%th_eq)) then
        if(abs(ps%th_eq(inth)-ps%th_eq(1)-twopi).gt.1.0e-1_rspec) then
           !  difference not close to twopi
           i2pi = 1
        else if(abs(ps%th_eq(1)+pi).lt.abs(ps%th_eq(1))) then
           !  [-pi,pi] range
           i2pi = 1
        else
           !  [0,2pi] range
           i2pi = 2
        endif
     endif
  endif

  !----------------------
  ! read the G-eqdsk file; map the equilibrium
  ikmom=min(ps%nmom,64) 
  if(ikmom.le.0) ikmom=64
  call xplasma_kmom_set(s,ikmom,ierr)  ! set FFT moments up to 64
  if(ierr.ne.0) then
     write(iout,*) ' %ps_update_mhdeq: error in  xplasma_kmom_set:'
     call xplasma_error(s,ierr,iout)
     go to 999
  endif

  call xplasma_fromgeqdsk(s,g_filename,ierr, &
       nrho=inrho, nth=inth, i2pi=i2pi, &
       cratio_min = crat, equal_arc = .TRUE., kmom = ikmom,&
       kpsi_cur = kpsi_cur, rho_curbreak = rho_curbrk, &
       lhermite = .TRUE.)
  
  if(ierr.ne.0) then
     write(iout,*) ' %ps_update_mhdeq: error in xplasma_fromgeqdsk:'
     call xplasma_error(s,ierr,iout)
     go to 999
  else
     call eqi_fromg_warn(iwarn)
     if(iwarn.gt.0) then
        write(iout,*) ' '
        write(iout,*) ' %ps_update_mhdeq: xplasma_fromgeqdsk messages:'
        call xplasma_error(s,ierr,iout)
     endif
  endif

  ! ------------------
  ! update plasma_state from xplasma object

  call ps_update_fromxplasma(ps,iout,ierr)
  if(ierr.ne.0) go to 999
 
999 continue
  call xplasma_author_clear(s,'EQ',iertmp)
  if (iertmp/=0) then
     call xplasma_error(s,iertmp,iout)
     ierr=1
  end if

  if(ierr.ne.0) then
     ierr=999
     write(iout,*) ' ?ps_update_mhdeq: an error was detected.'
  else
     if(ps_debug.ge.2) then
        write(iout,*) ' ps_update_mhdeq: MHD equilibrium OK, normal exit.'
        write(iout,*) ' '
     endif
  endif

end subroutine ps_update_mhdeq
