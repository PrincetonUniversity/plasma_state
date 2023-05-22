subroutine ps_update_cur(ps,pcur,zrho_brk,iout,ierr)

  !  update the plasma state profiles related to current:
  !     ps%curt(1:ps%nrho_eq) -- enclosed toroidal current w/in flux surfaces
  !     ps%jdotb(1:ps%nrho_eq) -- <J.B> (positive sign convention)

  !  the total toroidal plasma current (Amps) is input: pcur
  !  in the range [zrho_brk:1.0] an interpolation is done to obtain a match
  !  with the total current specified.

  !  *** it is assumed *** that an up-to-date equilibrium has been stored
  !  in the state.  This routine checks that <|grad(rho)|**2/R**2> is up-to-date.

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions

  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated

  REAL(KIND=rspec), intent(in) :: pcur    ! the total plasma current
  REAL(KIND=rspec), intent(in) :: zrho_brk  ! point at which to start ps%curt
                                          ! interpolation to match pcur

  integer, intent(in) :: iout             ! Fortran LUN for messages
  integer, intent(out) :: ierr            ! exit status code 0=OK

  !-----------------------
  !  local:
  integer :: ii,ibrk,inrho,inlim,imod
  integer :: id3(3),ideriv3(3)
  REAL(KIND=rspec), dimension(:,:), allocatable :: zwk
  REAL(KIND=rspec) :: zdelrho,zrhobrk,zdpsidrho,zdvdrho,zrho2r2i
  REAL(KIND=rspec) :: aicur,bicur,x1,x2,x3,xxx,zdrho

  type (interp_object) :: xobj
  type (xplasma), pointer :: s

  real(kind=rspec), parameter :: z2pi   = 6.2831853071795862_rspec
  real(kind=rspec), parameter :: zmu0   = 2*z2pi*1.0e-7_rspec

  !-----------------------
  !  init check
  call ps_tagchk('ps_update_cur',ps%itag,iout,ierr)
  if(ierr.ne.0) return

  !  get xplasma object pointer
  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  id3(1) = ps%id_psipol;    ideriv3(1)=1
  id3(2) = ps%id_vol;       ideriv3(2)=1
  id3(3) = ps%id_grho2r2i;  ideriv3(3)=0

  !-------------------------------
  !  get number of pts; set bdy values
  inrho = ps%nrho_eq

  ps%curt(1) = 0.0_rspec
  ps%curt(inrho) = pcur

  !-------------------------------
  allocate(zwk(inrho,3))

  call xplasma_eval_prof(s,id3,ps%rho_eq,zwk,ierr,ideriv1s=ideriv3)
  if(ierr.ne.0) then
     deallocate(zwk)
     ps%curt=0.0_rspec
     ps%jdotb=0.0_rspec
     write(iout,*) ' ?ps_update_cur: failed to interpolate data needed for curt evaluation:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  !-------------------------------
  !  break location (constrained)
  zrhobrk = max(0.8_rspec,min(0.98_rspec,zrho_brk))

  !  offset of break from edge
  zdelrho = 1.0_rspec - zrhobrk
  inlim = 1.0_rspec/zdelrho + 0.5_rspec

  !  if inrho.le.inlim, no interpolation will be done; the edge value of curt
  !  is simply set to pcur.

  if(inrho.gt.inlim) then
     ibrk=inrho-2
  else
     ibrk=inrho-1
  endif

  zdrho = (ps%rho_eq(inrho)-ps%rho_eq(inrho-1))/100

  do ii=2,ibrk
     zdpsidrho = zwk(ii,1)
     zdvdrho = zwk(ii,2)
     zrho2r2i = zwk(ii,3)
     ps%curt(ii) = zrho2r2i*zdvdrho*zdpsidrho/(z2pi*zmu0)
     if(inrho.gt.inlim) then
        if(ps%rho_eq(ii).ge.zrhobrk-zdrho) then
           ibrk=ii
           exit
        endif
     endif
  enddo

  deallocate(zwk)

  zrhobrk = ps%rho_eq(ibrk)

  if(inrho.gt.inlim) then
     ! parabolic piece fit to reach known total toroidal current
     x1=zrhobrk-ps%rho_eq(ibrk-1)
     x2=zrhobrk-ps%rho_eq(ibrk-2)
     ! parabolic fit to get slope d(icurt)/dx at zrhobrk:
     bicur = (x2*x2*(ps%curt(ibrk-1)-ps%curt(ibrk)) - &
          x1*x1*(ps%curt(ibrk-2)-ps%curt(ibrk))) / (x1*x2*(x1-x2))
     ! parabolic fit to match point and slope at zrhobrk and reach the
     ! boundary value ps%curt(inrho):
     x3=ps%rho_eq(inrho)-zrhobrk
     aicur = (ps%curt(inrho)-ps%curt(ibrk)-bicur*x3)/(x3*x3)
     do ii=ibrk+1,inrho-1
        xxx = ps%rho_eq(ii)-zrhobrk
        ps%curt(ii)=ps%curt(ibrk) + xxx*(bicur + xxx*aicur)
     enddo
  endif

  ! copied with small changes from plasma_state/plasma_state_xload.f90:
  ! make the spline:

  CALL ps_1d_prof_store(ps%iobj,"curt","RHO_EQ", &
       "A",ps_cnum_eq,"total enclosed toroidal current",ps_spline,ps_bc_grad00, &
       ps%nrho_eq,0, &
       ps%curt,ps%id_curt, &
       imod,iout,ierr)

  call ps_update_jdotb(ps,iout,ierr)

end subroutine ps_update_cur


subroutine ps_update_jdotb(ps,iout,ierr)

  ! compute <J.B> from g(rho) and I(rho) profiles

  ! <J.B> on axis: j*g(0)/R_axis; j = d2(curt)/drho2 / d2(Area)/drho2
  ! <J.B> off axis: 2pi*[g*d(curt)/drho - curt*dg/drho]/(dVol/drho)

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions

  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated

  integer, intent(in) :: iout             ! Fortran LUN for messages
  integer, intent(out) :: ierr            ! exit status code 0=OK

  !-----------------------
  !  local:

  integer :: inrho,id,jj
  real*8, dimension(:), allocatable :: zg,zdgdrho,zi,zdidrho,zdvdrho,zrho

  type (interp_object) :: xobj
  type (xplasma), pointer :: s

  real(kind=rspec), parameter :: z2pi   = 6.2831853071795862_rspec

  !-----------------
  !  get xplasma object pointer
  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  !  set up for calculation; allocate arrays

  inrho = ps%nrho_eq
  allocate(zrho(inrho)); zrho = ps%rho_eq

  allocate(zg(inrho),zdgdrho(inrho),zi(inrho),zdidrho(inrho),zdvdrho(inrho))

  ps%jdotb = 0.0_rspec

  do 
     call xplasma_volume(s,zrho,zdvdrho,ierr,ideriv=1)  ! dVol/drho vs. rho
     if(ierr.ne.0) exit

     id = ps%id_curt
     call xplasma_eval_prof(s,id,zrho,zi,ierr, ideriv1=0)     ! I(rho)
     if(ierr.ne.0) exit
     call xplasma_eval_prof(s,id,zrho,zdidrho,ierr, ideriv1=1)  ! dI/drho
     if(ierr.ne.0) exit

     id = ps%id_g_eq
     call xplasma_eval_prof(s,id,zrho,zg,ierr, ideriv1=0)     ! g(rho)
     if(ierr.ne.0) exit
     call xplasma_eval_prof(s,id,zrho,zdgdrho,ierr, ideriv1=1)  ! dg/drho
     if(ierr.ne.0) exit

     do jj=2,inrho
        ps%jdotb(jj) = z2pi*(zg(jj)*zdidrho(jj)-zi(jj)*zdgdrho(jj)) / &
             zdvdrho(jj)
     enddo

     !  axial extrapolation (quadratic)

     ps%jdotb(1)=(zrho(3)**2*ps%jdotb(2) - zrho(2)**2*ps%jdotb(3)) / &
          (zrho(3)**2-zrho(2)**2)

     exit
  enddo

  deallocate(zrho,zi,zdidrho,zg,zdgdrho,zdvdrho)

  !-----------------------
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_update_jdotb: error during <J.B> evaluation:'
     call xplasma_error(s,iout,ierr)
  endif

end subroutine ps_update_jdotb
