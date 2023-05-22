module pspro

  ! routines to compute physics profile quantities that from Plasma State 
  ! object data are gathered here:
  !
  !    pspro_cloge -- electron Coulomb Log
  !    pspro_resis_hhb -- resistivity (Hirshman-Hawryluk-Birge Neoclassical)
  !      ... ...
  !
  ! It is recommended to compute full profiles (rho(:) covering the whole
  ! plasma column or nearly so).  If doing one surface at a time use intrp=0
  ! for direct interpolation.

  use plasma_state_mod
  implicit NONE

  PRIVATE

  public :: pspro_cloge,pspro_resis_hhb

  real*8, parameter :: ZERO = 0.0d0
  real*8, parameter :: ONE  = 1.0d0

CONTAINS

  subroutine pspro_cloge(pps,rho,cloge,ierr,intrp)

    ! Return electron Coulomb Log at indicated locations

    type (plasma_state) :: pps  ! Plasma State object
    !   (PLASMA and EQ components should be filled out)
    !   (electron temperature, density, and Zeff are used with rezoning)

    real*8, dimension(:), intent(in) :: rho ! radial flux zone locations in
    real*8, dimension(:), intent(out) :: cloge ! electron coulomb log out

    integer, intent(out) :: ierr   ! status code on exit, 0=OK

    integer, intent(in), optional :: intrp  ! interpolation options
    !                     =0 direct interpolation
    !                     =1 rezone w/o smoothing (default)
    !                     =2 rezone w/smoothing
    !                     =3 rezone, smoothing if size(rho) larger than
    !                                     (3/2)*[pps PLASMA rho grid size]

    !-----------------------------------------------
    !  local:

    integer :: intrpi,isize,ii,iwarn,jwarn,iout
    LOGICAL :: zonesmoo

    real*8, dimension(:), allocatable :: rhozb,ne,te,zeff
    real*8 :: cloge_r8_fcn
    !-----------------------------------------------

    iout = SWIM_err_out_file

    cloge = ZERO

    !  error checks

    call chk_pps('pspro_cloge',pps,1,ierr)
    if(ierr.ne.0) return

    call chk_rho('pspro_cloge',rho,ierr)
    if(ierr.ne.0) return

    !  rezone option

    intrpi=1
    if(present(intrp)) intrpi = max(0,min(3,intrp))

    isize = size(rho)
    call get_zonesmoo(pps,intrpi,isize,zonesmoo)

    !  allocate local arrays

    allocate(rhozb(isize+1),ne(isize),Te(isize),Zeff(isize))

    call gen_rhozb(rho,rhozb)

    call get_parms(pps,rho,rhozb,intrpi,zonesmoo,ne,Te,Zeff,ierr)
    if(ierr.ne.0) then
       write(iout,*) &
            ' pspro("pspro_cloge"): {ne,Te,Zeff} interpolation/rezone error.'
       return
    endif

    jwarn=0
    do ii=1,isize
       cloge(ii) = cloge_r8_fcn( ne(ii)*1.0e-6_rspec, Te(ii)*1000.0_rspec, &
            Zeff(ii), iwarn)
       jwarn = max(jwarn,iwarn)
    enddo

    if(jwarn.gt.0) then
       ierr=1
       write(iout,*) &
            ' pspro("pspro_cloge"): cloge_r8_fcn warning flag detected.'
       cloge = ZERO
       return
    endif

  end subroutine pspro_cloge

  subroutine pspro_resis_hhb(pps,rho,eta_sp,eta_nc,ierr,intrp)

    ! Return resistivity at indicated locations, using Hirshman-Hawryluk-Birge
    ! aspect ratio neoclassical formula.  Both the Spitzer and the Neoclassical
    ! resistivities are computed.

    type (plasma_state) :: pps  ! Plasma State object
    !   (PLASMA and EQ components should be filled out)
    !   (electron temperature, density, and Zeff are used with rezoning)

    real*8, dimension(:), intent(in) :: rho ! radial flux zone locations in
    real*8, dimension(:), intent(out) :: eta_sp ! Spitzer resistivity, Ohm*m
    real*8, dimension(:), intent(out) :: eta_nc ! Neoclassical resistivity, 
    !                                                also Ohm*m

    integer, intent(out) :: ierr   ! status code on exit, 0=OK

    integer, intent(in), optional :: intrp  ! interpolation options
    !                     =0 direct interpolation
    !                     =1 rezone w/o smoothing (default)
    !                     =2 rezone w/smoothing
    !                     =3 rezone, smoothing if size(rho) larger than
    !                                     (3/2)*[pps PLASMA rho grid size]

    !-----------------------------------------------
    !  local:

    integer :: intrpi,isize,ii,iwarn,jwarn,iout
    LOGICAL :: zonesmoo

    real*8, dimension(:), allocatable :: rhozb,rhozc,ne,te,zeff
    real*8, dimension(:), allocatable :: aratii,Lpol,dVdrho,xiotb,Rin,Rout
    real*8 :: tflux,bzxr,rmin_mp,rmaj_mp
    !-----------------------------------------------

    iout = SWIM_err_out_file

    eta_sp = ZERO
    eta_nc = ZERO

    !  error checks

    call chk_pps('pspro_resis_hhb',pps,2,ierr)
    if(ierr.ne.0) return

    call chk_rho('pspro_resis_hhb',rho,ierr)
    if(ierr.ne.0) return

    !  rezone option

    intrpi=1
    if(present(intrp)) intrpi = max(0,min(3,intrp))

    isize = size(rho)
    call get_zonesmoo(pps,intrpi,isize,zonesmoo)

    !  allocate local arrays

    allocate(rhozc(isize),rhozb(isize+1),ne(isize),Te(isize),Zeff(isize))
    allocate(aratii(isize),Lpol(isize),dVdrho(isize),xiotb(isize))
    allocate(Rin(isize),Rout(isize))

    call gen_rhozb(rho,rhozb)

    ! for HHB resistivity, need to stay epsilon away from magnetic axis, so,
    ! rhozc is (rho(:)) with 1st point adjusted if necessary

    rhozc = rho
    if(isize.gt.1) then
       rhozc(1) = max(rhozc(1),min(1.e-6_rspec,rhozc(2)/10.0_rspec))
    else
       rhozc(1) = max(rhozc(1),1.e-6_rspec)
    endif

    ! get ne,Te,Zeff

    call get_parms(pps,rho,rhozb,intrpi,zonesmoo,ne,Te,Zeff,ierr)
    if(ierr.ne.0) then
       write(iout,*) &
            ' pspro("pspro_resis_hhb"): {ne,Te,Zeff} interp/rezone error.'
       return
    endif

    ! get field & geometry information

    tflux = pps%phit(pps%nrho_eq)  ! enclosed toroidal flux, Wb
    bzxr = 100.0_rspec*pps%g_eq(pps%nrho_eq)  ! vacuum R*B_phi, Tesla*cm

    do
       call ps_intrp_1d(rhozc,pps%id_R_midp_in,Rin,ierr,state=pps)
       if(ierr.ne.0) exit
       call ps_intrp_1d(rhozc,pps%id_R_midp_out,Rout,ierr,state=pps)
       if(ierr.ne.0) exit

       ! inverse aspect ratio r/R

       do ii=1,isize
          Rmin_mp = (Rout(ii)-Rin(ii))*0.5_rspec
          Rmaj_mp = (Rout(ii)+Rin(ii))*0.5_rspec
          if(Rmaj_mp.le.ZERO) then
             aratii(ii)=ZERO
          else
             aratii(ii)=Rmin_mp/Rmaj_mp
          endif
       enddo

       ! poloidal path length -> cm
       call ps_intrp_1d(rhozc,pps%id_Lpol,Lpol,ierr,state=pps)
       if(ierr.ne.0) exit

       Lpol = 100.0_rspec*Lpol

       ! dVol/drho -> cm**3

       call ps_intrp_1d(rhozc,pps%id_vol,dVdrho,ierr,state=pps,ideriv=1)
       if(ierr.ne.0) exit

       dVdrho = 1.0e6_rspec*dVdrho

       !  iota(bar) = 1/q

       call ps_intrp_1d(rhozc,pps%id_iota,xiotb,ierr,state=pps)
       if(ierr.ne.0) exit

       exit
    enddo

    if(ierr.ne.0) then
       write(iout,*) &
            ' ?pspro("pspro_resis_hhb") input data interpolation error.'
       return
    endif

#ifdef __DEBUG
    write(iout,*) ' '
    write(iout,*) ' #,ne,Te,Zeff,eta_sp,eta_nc '
#endif

    jwarn=0
    do ii=1,isize
       call hhb_resis(ne(ii)*1.0e-6_rspec, Te(ii)*1000.0_rspec, Zeff(ii), &
            aratii(ii), Lpol(ii), tflux, rhozc(ii), dVdrho(ii), &
            xiotb(ii), bzxr, &
            eta_sp(ii),eta_nc(ii), iwarn)

       eta_sp(ii) = eta_sp(ii)/100.0_rspec  ! -> Ohm*m
       eta_nc(ii) = eta_nc(ii)/100.0_rspec  ! -> Ohm*m

#ifdef __DEBUG
       write(iout,'(1x,i2,5(1x,1pe11.4))') ii,ne(ii),Te(ii),Zeff(ii), &
            eta_sp(ii),eta_nc(ii)
#endif

       jwarn = max(jwarn,iwarn)
    enddo

    if(jwarn.gt.0) then
       ierr=1
       write(iout,*) &
            ' pspro("pspro_resis_hhb"): SR hhb_resis warning flag detected.'
       eta_sp = ZERO
       eta_nc = ZERO
    endif

  end subroutine pspro_resis_hhb

  !-------------------------------
  ! private internal routines...

  subroutine chk_pps(label,pps,ilev,ierr)

    ! check if Plasma State contains allocated PLASMA & EQ component data

    character*(*), intent(in) :: label
    type (plasma_state) :: pps     ! Plasma State object
    integer, intent(in) :: ilev    ! check "level"
    integer, intent(out) :: ierr   ! exit status 0=OK
   
    !--------------------------------
    integer :: iout
    !--------------------------------

    iout = SWIM_err_out_file

    call ps_state_memory_update(ierr,state=pps,eqcheck=.FALSE.)
    if(ierr.ne.0) then
       write(iout,*) ' ?pspro("'//trim(label)// &
            '") chk_pps ps_state_memory_update error.'
       return
    endif

    if(pps%nrho.le.0) then
       ierr=1
       write(iout,*) ' ?pspro("'//trim(label)// &
            '") chk_pps: PLASMA component nrho=0.'
       return
    endif

    if(pps%nrho_eq.le.0) then
       ierr=1
       write(iout,*) ' ?pspro("'//trim(label)// &
            '") chk_pps: EQ component nrho_eq=0.'
       return
    endif

    if((ilev.gt.1).AND.(pps%nrho_eq_geo.eq.0)) then
       ierr=1
       write(iout,*) ' ?pspro("'//trim(label)// &
            '") chk_pps: EQ component nrho_eq_geo=0.'
       return
    endif

  end subroutine chk_pps

  subroutine get_zonesmoo(pps,intrpi,isize,zonesmoo)

    ! rezone interpolation smoothing option (T/F) set here

    type (plasma_state) :: pps          ! Plasma State object
    integer, intent(in) :: intrpi           ! interpolation option
    integer, intent(in) :: isize            ! target grid size
    logical, intent(out) :: zonesmoo        ! rezone smoothing option (out)
   
    !--------------------------------
    zonesmoo = .FALSE.

    if(intrpi.eq.2) then
       zonesmoo=.TRUE.

    else if(intrpi.eq.3) then
       if(pps%nrho .lt. (2*isize)/3 ) then
          zonesmoo=.TRUE.  ! rezone from coarse to fine grid: smooth
       else
          zonesmoo=.FALSE.
       endif
    endif

  end subroutine get_zonesmoo

  subroutine gen_rhozb(rho,rhozb)

    ! get bdys for rezone target grid

    real*8, dimension(:) :: rho,rhozb
    integer :: isize,ii

    !-------------------------------
    isize = size(rho)

    rhozb(1) = ZERO
    rhozb(isize+1) = ONE

    do ii=2,isize
       rhozb(ii) = 0.5_rspec * (rho(ii-1) + rho(ii))
    enddo

  end subroutine gen_rhozb

  subroutine chk_rho(label,rho,ierr)

    character*(*), intent(in) :: label
    real*8, dimension(:), intent(in) :: rho
    integer, intent(out) :: ierr

    ! set status flag if: 
    !    minval(rho) < ZERO, or 
    !    maxval(rho) > ONE, or
    !    rho not strict ascending, rho(j+1).gt.rho(j)

    !---------------------------------------------
    integer :: ii,iwarn1,iwarn2,iwarn3,iout
    !---------------------------------------------

    ierr = 0
    iout = SWIM_err_out_file

    iwarn1=0
    iwarn2=0
    iwarn3=0

    if(rho(1).lt.ZERO) iwarn1=1
    if(rho(1).gt.ONE) iwarn2=1

    do ii=2,size(rho)
       if(rho(ii).lt.ZERO) iwarn1=1
       if(rho(ii).gt.ONE) iwarn2=1
       if(rho(ii).le.rho(ii-1)) iwarn3=1
    enddo

    if(iwarn1.eq.1) then
       write(iout,*) ' ?pspro("'//trim(label)// &
            '") chk_rho: minval(rho).lt.ZERO'
       ierr=1
    endif

    if(iwarn2.eq.1) then
       write(iout,*) ' ?pspro("'//trim(label)// &
            '") chk_rho: maxval(rho).gt.ONE'
       ierr=1
    endif

    if(iwarn3.eq.1) then
       write(iout,*) ' ?pspro("'//trim(label)// &
            '") chk_rho: rho(:) not strict ascending.'
       ierr=1
    endif

    if(ierr.gt.0) then
       write(iout,*) ' ...rho = ',rho
    endif

  end subroutine chk_rho

  subroutine get_parms(pps,rho,rhozb,intrpi,zonesmoo,ne,Te,Zeff,ierr)

    ! get ne,Te,Zeff profiles (m**-3, keV, -)

    type (plasma_state) :: pps          ! Plasma State object
    real*8, dimension(:), intent(in) :: rho,rhozb   ! interpolation targets
    integer, intent(in) :: intrpi       ! interpolation option
    logical, intent(in) :: zonesmoo     ! smoothing option

    real*8, dimension(:), intent(out) :: ne,Te,Zeff ! output data
    integer, intent(out) :: ierr        ! status code on exit, 0=OK

    !-------------------------

    do
       if(intrpi.eq.0) then
          call ps_intrp_1d(rho,pps%id_ns(0),ne,ierr,state=pps)
          if(ierr.ne.0) exit

          call ps_intrp_1d(rho,pps%id_Ts(0),Te,ierr,state=pps)
          if(ierr.ne.0) exit

          call ps_intrp_1d(rho,pps%id_zeff,Zeff,ierr,state=pps)
          if(ierr.ne.0) exit

       else
          call ps_rho_rezone(rhozb,pps%id_ns(0),ne,ierr, &
               state=pps, zonesmoo=zonesmoo)
          if(ierr.ne.0) exit

          call ps_rho_rezone(rhozb,pps%id_Ts(0),Te,ierr, &
               state=pps, zonesmoo=zonesmoo)
          if(ierr.ne.0) exit

          call ps_rho_rezone(rhozb,pps%id_Zeff,Zeff,ierr, &
               state=pps, zonesmoo=zonesmoo)
          if(ierr.ne.0) exit
       endif

       exit
    enddo

  end subroutine get_parms

end module pspro
