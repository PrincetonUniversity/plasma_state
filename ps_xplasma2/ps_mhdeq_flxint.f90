subroutine ps_mhdeq_grho2r2i(ps,nwk,rhowk,iout,ierr)

  !  Update plasma state "ps" with derived profiles from equilibrium:
  !     flux surface averages <1/R**2> (gr2i)
  !                           <|grad(rho)|**2/R**2> (grho2r2i)

  !  Profiles are mapped to ps%rho_eq_geo by interpolation from the 
  !  (presumably coarser) grid rhowk(1:nwk) -- computing flux surface
  !  averages on a coarser grid is a way to speed the calculation.
  !  (From TRANSP experience it is a good idea of rhowk and ps%rho_eq_geo
  !  points match near the axis and edge).  To avoid the approximation
  !  associated with interpolation, simply set rhowk = ps%rho_eq_geo.

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated
  integer, intent(in) :: nwk              ! size of working grid
  real(KIND=rspec), intent(in) :: rhowk(nwk) ! working grid for integrations
  integer, intent(in) :: iout             ! Fortran LUN for messages
  integer, intent(out) :: ierr            ! exit status code 0=OK

  !-----------------------
  !  local:

  integer :: idi,kf1,kf2,idiff,iaddr,iertot

  logical :: intrp  ! .TRUE. if rhowk(1:nwk) not identical to ps%rho_eq_geo

  real(KIND=rspec), parameter :: ZERO = 0.0_rspec
  real(KIND=rspec), parameter :: ZTOL = 5.0e-7_rspec  ! for genxpkg

  real(KIND=rspec), dimension(:,:), allocatable :: zxpkg
  real(KIND=rspec), dimension(:), allocatable :: zwk
  integer, parameter :: ispline=1
  integer, parameter :: ilinear=0

  integer :: loc_cclist(ps_ccount)
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  call ps_mhdflx_errchk('ps_mhdeq_grho2r2i',ps,nwk,rhowk,iout,ierr)
  if(ierr.ne.0) return

  call ps_rzsurf_hash(ps,kf1,kf2)

  idiff=1
  if(ps%grho2r2i(1).gt.ZERO) then

     !-----------------------
     !  hash code test

     loc_cclist = ps_cczero
     loc_cclist(ps_cnum_EQ)=ps_chkProfs  ! update EQ profile hashcodes only...
     call plasma_state_hash_code(ps,loc_cclist,ierr)
     if(ierr.ne.0) return

     idiff=0

     iaddr=ps_hash_aux1
     if((kf1.ne.ps%hash_codes(iaddr)).or.(kf2.ne.ps%hash_codes(iaddr+1))) then
        write(iout,*) ' %ps_mhdeq_grho2r2i: volumes need updating...'
        call ps_mhdeq_volumes(ps,iout,ierr)
        if(ierr.ne.0) return
        call ps_mhdeq_fsmoms(ps,iout,ierr)
        if(ierr.ne.0) return
        idiff=1
     else
        iaddr=ps_hash_aux2
        if(kf1.ne.ps%hash_codes(iaddr)) idiff=idiff+1

        iaddr=iaddr+1
        if(kf2.ne.ps%hash_codes(iaddr)) idiff=idiff+1
     endif
  endif

  if(idiff.eq.0) return

  iaddr=ps_hash_aux2
  ps%new_hash_codes(iaddr)=kf1
  ps%new_hash_codes(iaddr+1)=kf2
  ps%hash_codes(iaddr)=kf1
  ps%hash_codes(iaddr+1)=kf2

  call ps_rhowk_integ(ps,nwk,rhowk,idi,iout,ierr)
  if(ierr.ne.0) return

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  intrp=.TRUE.
  if(nwk.eq.ps%nrho_eq_geo) then
     if(maxval(abs(ps%rho_eq_geo-rhowk)).lt.1.0e-12_rspec) intrp=.FALSE.
  endif

  iertot=0
  if(intrp) then
     allocate(zxpkg(nwk,4),zwk(nwk))
     call genxpkg(nwk,rhowk,zxpkg,0,1,1,ztol,3,ierr)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_mhdeq_grho2r2i: unexpected error, genxpkg.'
        deallocate(zxpkg,zwk)
        return
     endif

     call xplasma_rho_zonint(s,idi,'<1/R^2>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%gr2i)
     endif
     call xplasma_rho_zonint(s,idi,'<grad(rho)^2/R^2>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%grho2r2i)
     endif
     deallocate(zxpkg,zwk)

  else
     call xplasma_rho_zonint(s,idi,'<1/R^2>S',ps%gr2i,ierr)
     if(ierr.ne.0) iertot=iertot+1
     call xplasma_rho_zonint(s,idi,'<grad(rho)^2/R^2>S',ps%grho2r2i,ierr)
     if(ierr.ne.0) iertot=iertot+1
  endif

  if(iertot.gt.0) then
     ierr=9999
     write(iout,*) ' ?ps_mhdeq_grho2r2i: error during flux surface integration'
     call xplasma_error(s,ierr,iout)
  endif

end subroutine ps_mhdeq_grho2r2i

subroutine ps_mhdeq_geoprof(ps,nwk,rhowk,iout,ierr)

  !  Update plasma state "ps" with derived profiles from equilibrium:
  !     flux surface averages <1/R>,<R>,<R**2>, etc.
  !                           <|grad(rho)|>,<|grad(rho)|**2> & more
  !  All derivable from {R,Z}surface only-- averages involving powers
  !  of R and grad(rho).

  !  Profiles are mapped to ps%rho_eq_geo by interpolation from the 
  !  (presumably coarser) grid rhowk(1:nwk) -- computing flux surface
  !  averages on a coarser grid is a way to speed the calculation.
  !  (From TRANSP experience it is a good idea of rhowk and ps%rho_eq_geo
  !  points match near the axis and edge).  To avoid the approximation
  !  associated with interpolation, simply set rhowk = ps%rho_eq_geo.

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated
  integer, intent(in) :: nwk              ! size of working grid
  real(KIND=rspec), intent(in) :: rhowk(nwk) ! working grid for integrations
  integer, intent(in) :: iout             ! Fortran LUN for messages
  integer, intent(out) :: ierr            ! exit status code 0=OK

  !-----------------------
  !  local:

  integer :: idi,kf1,kf2,idiff,iaddr,iertot

  logical :: intrp  ! .TRUE. if rhowk(1:nwk) not identical to ps%rho_eq_geo

  real(KIND=rspec), parameter :: ZERO = 0.0_rspec
  real(KIND=rspec), parameter :: ZTOL = 5.0e-7_rspec  ! for genxpkg

  real(KIND=rspec), dimension(:,:), allocatable :: zxpkg
  real(KIND=rspec), dimension(:), allocatable :: zwk
  integer, parameter :: ispline=1
  integer, parameter :: ilinear=0

  integer :: loc_cclist(ps_ccount)
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  call ps_mhdflx_errchk('ps_mhdeq_geoprof',ps,nwk,rhowk,iout,ierr)
  if(ierr.ne.0) return

  call ps_rzsurf_hash(ps,kf1,kf2)

  idiff=1
  if(ps%gri(1).gt.ZERO) then

     !-----------------------
     !  hash code test

     loc_cclist = ps_cczero
     loc_cclist(ps_cnum_EQ)=ps_chkProfs  ! update EQ profile hashcodes only...
     call plasma_state_hash_code(ps,loc_cclist,ierr)
     if(ierr.ne.0) return

     idiff=0

     iaddr=ps_hash_aux1
     if((kf1.ne.ps%hash_codes(iaddr)).or.(kf2.ne.ps%hash_codes(iaddr+1))) then
        write(iout,*) ' %ps_mhdeq_geoprof: volumes need updating...'
        call ps_mhdeq_volumes(ps,iout,ierr)
        if(ierr.ne.0) return
        call ps_mhdeq_fsmoms(ps,iout,ierr)
        if(ierr.ne.0) return
        idiff=1
     else
        iaddr=ps_hash_aux3
        if(kf1.ne.ps%hash_codes(iaddr)) idiff=idiff+1

        iaddr=iaddr+1
        if(kf2.ne.ps%hash_codes(iaddr)) idiff=idiff+1
     endif
  endif

  if(idiff.eq.0) return

  iaddr=ps_hash_aux3
  ps%new_hash_codes(iaddr)=kf1
  ps%new_hash_codes(iaddr+1)=kf2
  ps%hash_codes(iaddr)=kf1
  ps%hash_codes(iaddr+1)=kf2

  call ps_rhowk_integ(ps,nwk,rhowk,idi,iout,ierr)
  if(ierr.ne.0) return

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  intrp=.TRUE.
  if(nwk.eq.ps%nrho_eq_geo) then
     if(maxval(abs(ps%rho_eq_geo-rhowk)).lt.1.0e-12_rspec) intrp=.FALSE.
  endif

  iertot=0
  if(intrp) then
     allocate(zxpkg(nwk,4),zwk(nwk))
     call genxpkg(nwk,rhowk,zxpkg,0,1,1,ztol,3,ierr)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_mhdeq_geoprof: unexpected error, genxpkg.'
        deallocate(zxpkg,zwk)
        return
     endif

     call xplasma_rho_zonint(s,idi,'<1/R>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%gri)
     endif
     call xplasma_rho_zonint(s,idi,'<grad(rho)>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%grho1)
     endif
     call xplasma_rho_zonint(s,idi,'<grad(rho)^2>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%grho2)
     endif
     call xplasma_rho_zonint(s,idi,'<R>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%gr1)
     endif
     call xplasma_rho_zonint(s,idi,'<R^2>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%gr2)
     endif
     call xplasma_rho_zonint(s,idi,'<1/R^3>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%gr3i)
     endif
     call xplasma_rho_zonint(s,idi,'<grad(rho)^2/R^3>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%grho2r3i)
     endif
     call xplasma_rho_zonint(s,idi,'<R^2*grad(rho)^2>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%gr2rho2)
     endif
     call xplasma_rho_zonint(s,idi,'<1/(R*grad(rho))>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%grirhoi)
     endif

     deallocate(zxpkg,zwk)

  else
     call xplasma_rho_zonint(s,idi,'<1/R>S',ps%gri,ierr)
     if(ierr.ne.0) iertot=iertot+1
     call xplasma_rho_zonint(s,idi,'<grad(rho)>S',ps%grho1,ierr)
     if(ierr.ne.0) iertot=iertot+1
     call xplasma_rho_zonint(s,idi,'<grad(rho)^2>S',ps%grho2,ierr)
     if(ierr.ne.0) iertot=iertot+1
     call xplasma_rho_zonint(s,idi,'<R>S',ps%gr1,ierr)
     if(ierr.ne.0) iertot=iertot+1
     call xplasma_rho_zonint(s,idi,'<R^2>S',ps%gr2,ierr)
     if(ierr.ne.0) iertot=iertot+1
     call xplasma_rho_zonint(s,idi,'<1/R^3>S',ps%gr3i,ierr)
     if(ierr.ne.0) iertot=iertot+1
     call xplasma_rho_zonint(s,idi,'<grad(rho)^2/R^3>S',ps%grho2r3i,ierr)
     if(ierr.ne.0) iertot=iertot+1
     call xplasma_rho_zonint(s,idi,'<R^2*grad(rho)^2>S',ps%gr2rho2,ierr)
     if(ierr.ne.0) iertot=iertot+1
     call xplasma_rho_zonint(s,idi,'<1/(R*grad(rho))>S',ps%grirhoi,ierr)
     if(ierr.ne.0) iertot=iertot+1
  endif

  if(iertot.gt.0) then
     ierr=9999
     write(iout,*) ' ?ps_mhdeq_geoprof: error during flux surface integration'
     call xplasma_error(s,ierr,iout)
  endif

end subroutine ps_mhdeq_geoprof

subroutine ps_mhdeq_nclass(ps,nwk,rhowk,iout,ierr)

  !  Update plasma state "ps" with field and geometry
  !     flux surface averages <B**2> (gb2) and Pfirsch-Schlutter moments
  !  Also update other flux surface averages involving powers of B

  !  Profiles are mapped to ps%rho_eq_geo by interpolation from the 
  !  (presumably coarser) grid rhowk(1:nwk) -- computing flux surface
  !  averages on a coarser grid is a way to speed the calculation.
  !  (From TRANSP experience it is a good idea of rhowk and ps%rho_eq_geo
  !  points match near the axis and edge).  To avoid the approximation
  !  associated with interpolation, simply set rhowk = ps%rho_eq_geo.

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod
  use plasma_hash_code

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated
  integer, intent(in) :: nwk              ! size of working grid
  real(KIND=rspec), intent(in) :: rhowk(nwk) ! working grid for integrations
  integer, intent(in) :: iout             ! Fortran LUN for messages
  integer, intent(out) :: ierr            ! exit status code 0=OK

  !-----------------------
  !  local:

  integer :: idi,ii,kf1,kf2,idiff,iaddr,iertot,ipair(2)

  logical :: intrp  ! .TRUE. if rhowk(1:nwk) not identical to ps%rho_eq_geo

  real(KIND=rspec), parameter :: ZERO = 0.0_rspec
  real(KIND=rspec), parameter :: ZTOL = 5.0e-7_rspec  ! for genxpkg

  real(KIND=rspec), dimension(:,:), allocatable :: zxpkg,zpsmom
  real(KIND=rspec), dimension(:), allocatable :: zwk,zgamma,zgb2,zb2,zerrck
  real(KIND=rspec) :: zrhomin
  integer, parameter :: ispline=1
  integer, parameter :: ilinear=0

  integer :: loc_cclist(ps_ccount)
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  integer :: id_Bmod,id_BR,id_BZ
  !-----------------------

  call ps_mhdflx_errchk('ps_mhdeq_nclass',ps,nwk,rhowk,iout,ierr)
  if(ierr.ne.0) return

  call ps_rzsurf_hash(ps,kf1,kf2)

  idiff=1
  if(ps%gb2(1).gt.ZERO) then

     !-----------------------
     !  hash code test

     loc_cclist = ps_cczero
     loc_cclist(ps_cnum_EQ)=ps_chkProfs  ! update EQ profile hashcodes only...
     call plasma_state_hash_code(ps,loc_cclist,ierr)
     if(ierr.ne.0) return

     idiff=0

     iaddr=ps_hash_aux1
     if((kf1.ne.ps%hash_codes(iaddr)).or.(kf2.ne.ps%hash_codes(iaddr+1))) then
        write(iout,*) ' %ps_mhdeq_nclass: volumes need updating...'
        call ps_mhdeq_volumes(ps,iout,ierr)
        if(ierr.ne.0) return
        call ps_mhdeq_fsmoms(ps,iout,ierr)
        if(ierr.ne.0) return
        call addpair(ps_phash_g_eq)
        call addpair(ps_phash_psipol)
        call addpair(ps_phash_phit)
        idiff=1
     else
        call addpair(ps_phash_g_eq)
        call addpair(ps_phash_psipol)
        call addpair(ps_phash_phit)
        iaddr=ps_hash_aux4
        if(kf1.ne.ps%hash_codes(iaddr)) idiff=idiff+1

        iaddr=iaddr+1
        if(kf2.ne.ps%hash_codes(iaddr)) idiff=idiff+1
     endif
  else
     call addpair(ps_phash_g_eq)
     call addpair(ps_phash_psipol)
     call addpair(ps_phash_phit)
  endif

  if(idiff.eq.0) return

  iaddr=ps_hash_aux4
  ps%new_hash_codes(iaddr)=kf1
  ps%new_hash_codes(iaddr+1)=kf2
  ps%hash_codes(iaddr)=kf1
  ps%hash_codes(iaddr+1)=kf2

  if(ps%npsmom.eq.0) then
     write(iout,*) ' %ps_mhdeq_nclass: allocating 16 Pfirsch-Schlutter moments profiles.'
     ps%npsmom=16
     call plasma_state_empty_alloc(ps,ierr)
     if(ierr.ne.0) return
     do ii=1,ps%npsmom
        ps%psmom_num(ii)=' '
        if(ii.lt.10) then
           write(ps%psmom_num(ii)(1:1),'(I1)') ii
        else
           write(ps%psmom_num(ii)(1:2),'(I2)') ii
        endif
     enddo
  endif

  zrhomin = rhowk(2)*0.001_rspec

  call ps_rhowk_integ(ps,nwk,rhowk,idi,iout,ierr)
  if(ierr.ne.0) return

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  ! sanity check
  call xplasma_common_ids(s,ierr,id_Bmod=id_Bmod,id_BR=id_BR,id_BZ=id_BZ)
  if(min(id_Bmod,id_BR,id_BZ).eq.0) then
     write(iout,*) ' ?ps_mhdeq_nclass: data missing in xplasma; equilibrium may not be complete.'
     ierr=999
     return
  endif

  intrp=.TRUE.
  if(nwk.eq.ps%nrho_eq_geo) then
     if(maxval(abs(ps%rho_eq_geo-rhowk)).lt.1.0e-12_rspec) intrp=.FALSE.
  endif

  iertot=0
  if(intrp) then
     allocate(zxpkg(nwk,4),zwk(nwk))
     call genxpkg(nwk,rhowk,zxpkg,0,1,1,ztol,3,ierr)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_mhdeq_nclass: unexpected error, genxpkg.'
        deallocate(zxpkg,zwk)
        return
     endif

     call xplasma_rho_zonint(s,idi,'<B*R^2>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%gbr2)
     endif
     call xplasma_rho_zonint(s,idi,'<B>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%gb1)
     endif
     call xplasma_rho_zonint(s,idi,'<1/B^2>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%gb2i)
     endif
     call xplasma_rho_zonint(s,idi,'<grad(rho)^2/B^2>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%grho2b2i)
     endif

     allocate(zpsmom(ps%npsmom,nwk),zgamma(nwk),zgb2(nwk),zb2(nwk),zerrck(nwk))
     call xplasma_psmom(s,rhowk,zrhomin, &
          zpsmom,zgamma,zgb2,zb2,zerrck, &
          ierr)
     if(ierr.ne.0) iertot=iertot+1

     do ii=1,ps%npsmom
        call ps_mhdeq_intrp(nwk,zxpkg,zpsmom(ii,:), &
             ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%psmom_nc(:,ii))
     enddo
     call ps_mhdeq_intrp(nwk,zxpkg,zgamma,ps%nrho_eq_geo,ps%rho_eq_geo, &
          ispline,iout,ps%gamma_nc)
     call ps_mhdeq_intrp(nwk,zxpkg,zgb2,ps%nrho_eq_geo,ps%rho_eq_geo, &
          ispline,iout,ps%ngradb2_av)
     call ps_mhdeq_intrp(nwk,zxpkg,zb2,ps%nrho_eq_geo,ps%rho_eq_geo, &
          ispline,iout,ps%gb2)
     call ps_mhdeq_intrp(nwk,zxpkg,zerrck,ps%nrho_eq_geo,ps%rho_eq_geo, &
          ilinear,iout,ps%psmom_errck)
     deallocate(zpsmom,zgamma,zgb2,zb2,zerrck)
  else
     call xplasma_rho_zonint(s,idi,'<B*R^2>S',ps%gbr2,ierr)
     if(ierr.ne.0) iertot=iertot+1
     call xplasma_rho_zonint(s,idi,'<B>S',ps%gb1,ierr)
     if(ierr.ne.0) iertot=iertot+1
     call xplasma_rho_zonint(s,idi,'<1/B^2>S',ps%gb2i,ierr)
     if(ierr.ne.0) iertot=iertot+1
     call xplasma_rho_zonint(s,idi,'<grad(rho)^2/B^2>S',ps%grho2b2i,ierr)
     if(ierr.ne.0) iertot=iertot+1

     allocate(zpsmom(ps%npsmom,ps%nrho_eq_geo))
     call xplasma_psmom(s,ps%rho_eq_geo,zrhomin, &
          zpsmom,ps%gamma_nc,ps%ngradb2_av,ps%gb2,ps%psmom_errck, &
          ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
        ps%psmom_nc=ZERO
     else
        do ii=1,ps%npsmom
           ps%psmom_nc(1:ps%nrho_eq_geo,ii)=zpsmom(ii,1:ps%nrho_eq_geo)
        enddo
     endif
     deallocate(zpsmom)
  endif

  if(iertot.gt.0) then
     ierr=9999
     write(iout,*) ' ?ps_mhdeq_nclass: error during flux surface integration'
     call xplasma_error(s,ierr,iout)
  endif

  contains
    subroutine addpair(iarg)
      integer :: iarg

      iaddr=iarg
      ipair(1)=ps%new_hash_codes(iaddr)
      ipair(2)=ps%new_hash_codes(iaddr+1)

      call hashCode(ipair,kf1,kf2)
    end subroutine addpair

end subroutine ps_mhdeq_nclass

subroutine ps_mhdeq_geosing(ps,nwk,rhowk,iout,ierr)

  !  Update plasma state "ps" with derived profiles from equilibrium and
  !     field -- singular integrals involving expressions with sqrt(1-B/Bmax).

  !  Profiles are mapped to ps%rho_eq_geo by interpolation from the 
  !  (presumably coarser) grid rhowk(1:nwk) -- computing flux surface
  !  averages on a coarser grid is a way to speed the calculation.
  !  (From TRANSP experience it is a good idea of rhowk and ps%rho_eq_geo
  !  points match near the axis and edge).  To avoid the approximation
  !  associated with interpolation, simply set rhowk = ps%rho_eq_geo.

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod
  use plasma_hash_code

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated
  integer, intent(in) :: nwk              ! size of working grid
  real(KIND=rspec), intent(in) :: rhowk(nwk) ! working grid for integrations
  integer, intent(in) :: iout             ! Fortran LUN for messages
  integer, intent(out) :: ierr            ! exit status code 0=OK

  !-----------------------
  !  local:

  integer :: idi,kf1,kf2,idiff,iaddr,iertot,ipair(2)

  logical :: intrp  ! .TRUE. if rhowk(1:nwk) not identical to ps%rho_eq_geo

  real(KIND=rspec), parameter :: ZERO = 0.0_rspec
  real(KIND=rspec), parameter :: ZTOL = 5.0e-7_rspec  ! for genxpkg

  real(KIND=rspec), dimension(:,:), allocatable :: zxpkg
  real(KIND=rspec), dimension(:), allocatable :: zwk
  integer, parameter :: ispline=1
  integer, parameter :: ilinear=0

  integer :: loc_cclist(ps_ccount)
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  integer :: id_Bmod,id_BR,id_BZ
  !-----------------------

  call ps_mhdflx_errchk('ps_mhdeq_geosing',ps,nwk,rhowk,iout,ierr)
  if(ierr.ne.0) return

  call ps_rzsurf_hash(ps,kf1,kf2)

  idiff=1
  if(ps%gncfh(1).gt.ZERO) then

     !-----------------------
     !  hash code test

     loc_cclist = ps_cczero
     loc_cclist(ps_cnum_EQ)=ps_chkProfs  ! update EQ profile hashcodes only...
     call plasma_state_hash_code(ps,loc_cclist,ierr)
     if(ierr.ne.0) return

     idiff=0

     iaddr=ps_hash_aux1
     if((kf1.ne.ps%hash_codes(iaddr)).or.(kf2.ne.ps%hash_codes(iaddr+1))) then
        write(iout,*) ' %ps_mhdeq_geosing: volumes need updating...'
        call ps_mhdeq_volumes(ps,iout,ierr)
        if(ierr.ne.0) return
        call ps_mhdeq_fsmoms(ps,iout,ierr)
        if(ierr.ne.0) return
        call addpair(ps_phash_g_eq)
        call addpair(ps_phash_psipol)
        call addpair(ps_phash_phit)
        idiff=1
     else
        call addpair(ps_phash_g_eq)
        call addpair(ps_phash_psipol)
        call addpair(ps_phash_phit)
        iaddr=ps_hash_aux5
        if(kf1.ne.ps%hash_codes(iaddr)) idiff=idiff+1

        iaddr=iaddr+1
        if(kf2.ne.ps%hash_codes(iaddr)) idiff=idiff+1
     endif
  else
     call addpair(ps_phash_g_eq)
     call addpair(ps_phash_psipol)
     call addpair(ps_phash_phit)
  endif

  if(idiff.eq.0) return

  iaddr=ps_hash_aux5
  ps%new_hash_codes(iaddr)=kf1
  ps%new_hash_codes(iaddr+1)=kf2
  ps%hash_codes(iaddr)=kf1
  ps%hash_codes(iaddr+1)=kf2

  call ps_rhowk_integ(ps,nwk,rhowk,idi,iout,ierr)
  if(ierr.ne.0) return

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  ! sanity check
  call xplasma_common_ids(s,ierr,id_Bmod=id_Bmod,id_BR=id_BR,id_BZ=id_BZ)
  if(min(id_Bmod,id_BR,id_BZ).eq.0) then
     write(iout,*) ' ?ps_mhdeq_geosing: data missing in xplasma; equilibrium may not be complete.'
     ierr=999
     return
  endif

  intrp=.TRUE.
  if(nwk.eq.ps%nrho_eq_geo) then
     if(maxval(abs(ps%rho_eq_geo-rhowk)).lt.1.0e-12_rspec) intrp=.FALSE.
  endif

  iertot=0
  if(intrp) then
     allocate(zxpkg(nwk,4),zwk(nwk))
     call genxpkg(nwk,rhowk,zxpkg,0,1,1,ztol,3,ierr)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_mhdeq_geosing: unexpected error, genxpkg.'
        deallocate(zxpkg,zwk)
        return
     endif

     call xplasma_rho_zonint(s,idi,'<f(h)NCLASS>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ilinear,iout,ps%gncfh)
     endif
     call xplasma_rho_zonint(s,idi,'<f(h)NC_TSC>S',zwk,ierr)
     if(ierr.ne.0) then
        iertot=iertot+1
     else
        call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ilinear,iout,ps%gncfb2h)
     endif
  else
     call xplasma_rho_zonint(s,idi,'<f(h)NCLASS>S',ps%gncfh,ierr)
     if(ierr.ne.0) iertot=iertot+1
     call xplasma_rho_zonint(s,idi,'<f(h)NC_TSC>S',ps%gncfb2h,ierr)
     if(ierr.ne.0) iertot=iertot+1
  endif

  if(iertot.gt.0) then
     ierr=9999
     write(iout,*) ' ?ps_mhdeq_geosing: error during flux surface integration'
     call xplasma_error(s,ierr,iout)
  endif

  contains
    subroutine addpair(iarg)
      integer :: iarg

      iaddr=iarg
      ipair(1)=ps%new_hash_codes(iaddr)
      ipair(2)=ps%new_hash_codes(iaddr+1)

      call hashCode(ipair,kf1,kf2)
    end subroutine addpair

end subroutine ps_mhdeq_geosing

subroutine ps_rhowk_integ(ps,nwk,rhowk,idi,iout,ierr)

  !  Called from ps_mhdeq_flxint subroutines

  !  Create an integrator for flux surface integrals and averages
  !  on the passed grid rhowk(1:nwk).  Integrator may already exist
  !  but grid match must be tested.

  !  Integrator ownership set to xplasma_xmhd --> removed when equilibrium
  !  changes.

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps       ! plasma state object to be updated

  integer, intent(in) :: nwk      ! rhowk grid size
  Real(kind=rspec) :: rhowk(nwk)  ! rhowk grid

  integer, intent(out) :: idi     ! ID of integrator, returned (0 if error)

  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-----------------------
  !  local:
  integer :: iertmp
  character*32, parameter :: iiname = '_ps_mhdeq_rhowk_integ'

  integer, dimension(:), pointer :: iptr
  real(kind=rspec), dimension(:), pointer :: r8ptr
  real(kind=rspec) :: zdiffa,ztol

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------
  ztol=1.0d-10
  if((abs(rhowk(1)).gt.ztol).or.(abs(1.0_rspec-rhowk(nwk)).gt.ztol)) then
     ierr=9999
     write(iout,*) ' ?ps_rhowk_integ: integration grid must span range [0,1]:'
     write(iout,*) '  rhowk(1)=',rhowk(1),'; rhowk(nwk)=',rhowk(nwk)
     return
  endif

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_blkbxId(s,iiname,idi)
  if(idi.gt.0) then
     call xplasma_blackBox_retrieve(s,idi,ierr, ia_ptr=iptr, r8a_ptr=r8ptr)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_rhowk_integ: xplasma blackBox retrieve error:'
        call xplasma_error(s,ierr,iout)
        ierr=9999
        return
     endif
     if((size(iptr).ge.12).and.(size(r8ptr).gt.(nwk+1))) then
        if(iptr(12).eq.nwk) then
           zdiffa=maxval(abs(rhowk(1:nwk)-r8ptr(2:nwk+1)))
           if(zdiffa.lt.ztol) then
              return  !  accept existing integrator -- grid matches
           endif
        endif
     endif
  endif
  !  integrator not found: create it...

  call xplasma_author_set(s,xplasma_xmhd,iertmp)

  call xplasma_create_integ(s,trim(iiname),rhowk,idi,ierr, &
       cache_enable = .TRUE.)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_rhowk_integ: unexpected xplasma error:'
     call xplasma_error(s,ierr,iout)
     ierr=9999
  endif

  call xplasma_author_clear(s,xplasma_xmhd,iertmp)

end subroutine ps_rhowk_integ

subroutine ps_mhdeq_intrp(nwk,zxpkg,zf,nxtarg,xtarg,imethod,iout,zftarg)

  ! linear or spline interpolation for ps_mhdeq_flxint
  ! take data zf[1:nwk] defined on grid [zxpkg(1:nwk,4)] 
  ! to target data zftarg(1:nxtarg) over grid xtarg(1:nxtarg).

  ! zxpkg is to have been prepared by a prior call to genxpkg

  ! imethod specifies interpolation method (spline with not-a-knot BC, or,
  ! piecewise linear).

  implicit NONE

  integer, intent(in) :: nwk          ! # of points in source data
  real*8, intent(in) :: zxpkg(nwk,4)  ! x grid description
  !                     (built from 1d grid array by "genxpkg" call)
  real*8, intent(in) :: zf(nwk)       ! data to be interpolated

  integer, intent(in) :: nxtarg       ! target data size
  real*8, intent(in) :: xtarg(nxtarg) ! points to which to interpolate data
  integer, intent(in) :: imethod      ! interpolation method

  integer, intent(in) :: iout         ! I/O LUN for messages
  ! (none should occur -- messages indicate a code bug)

  real*8, intent(out) :: zftarg(nxtarg)  ! results of interpolation, output

  !----------------------------------
  ! local:

  integer, parameter :: ispline=1
  integer, parameter :: ilinear=0

  integer :: ibc=0
  real*8, parameter :: ZERO = 0.0d0
  real*8 :: zbc=ZERO

  integer idum,ict(3),iwarn,ierr

  real*8 :: zspl(4,nwk),zwk(nwk)

  data ict/1,0,0/

  !----------------------------------
  !  spline setup

  if(imethod.eq.ispline) then

     ! spline

     zspl(1,1:nwk)=zf(1:nwk)

     call cspline(zxpkg(:,1),nwk,zspl,ibc,zbc,ibc,zbc,zwk,nwk,idum,ierr)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_mhdeq_intrp: internal error (cspline call), results set to ZERO'
     else
        call spvec(ict,nxtarg,xtarg,nxtarg,zftarg,nwk,zxpkg,zspl,idum,ierr)
        if(ierr.ne.0) then
           write(iout,*) ' ?ps_mhdeq_intrp: internal error (spvec call), results set to ZERO'
        endif
     endif

  else
     ! piecewise linear interpolation

     call vecpc1(ict,nxtarg,xtarg,nxtarg,zftarg,nwk,zxpkg,zf,iwarn,ierr)
     if(iwarn.ne.0) ierr=iwarn
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_mhdeq_intrp: internal error (vecpc1 call), results set to ZERO'
     endif
  endif

  if(ierr.ne.0) zftarg = ZERO

end subroutine ps_mhdeq_intrp

!--------------------------------------
! remaining routines compute surface quantities other than
! numerically integrated flux surface averages -- e.g. elongation,
! triangularity...

subroutine ps_mhdeq_surfparam(ps,nwk,rhowk,iout,ierr)

  !  Update plasma state "ps" with derived profiles from equilibrium:
  !     Rmin, Rmax, Zmin, Zmax
  !
  !     elongation  =  (Zmax-Zmin)/(Rmax-Rmin)
  !        let R0=(Rmax+Rmin)/2; let a = (Rmax-Rmin)/2
  !
  !     triangularity  =  (R0 - min(R(Zmin),R(Zmax)))/a
  !     upper triangularity  =  (R0 - R(Zmax))/a
  !     lower triangularity  =  (R0 - R(Zmin))/a
  !
  !        R(Zmax) = major radius where Zmax occurs on flux surface
  !        R(Zmin) = major radius where Zmin occurs on flux surface
  !
  !     Approximate:
  !     midplane elevation Z_midp = Z location such that half the area
  !        enclose by a flux surface is above, and half below;
  !        R_midp_in and R_midp_out are the flux surface intercepts
  !        at Z=Z_midp, on high field and low field sides, respectively.

  !  Profiles are mapped to ps%rho_eq_geo by interpolation from the 
  !  (presumably coarser) grid rhowk(1:nwk) -- computing flux surface
  !  averages on a coarser grid is a way to speed the calculation.
  !  (From TRANSP experience it is a good idea of rhowk and ps%rho_eq_geo
  !  points match near the axis and edge).  To avoid the approximation
  !  associated with interpolation, simply set rhowk = ps%rho_eq_geo.

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated
  integer, intent(in) :: nwk              ! size of working grid
  real(KIND=rspec), intent(in) :: rhowk(nwk) ! working grid for integrations
  integer, intent(in) :: iout             ! Fortran LUN for messages
  integer, intent(out) :: ierr            ! exit status code 0=OK

  !-----------------------
  !  local:

  integer :: kf1,kf2,idiff,iaddr,iertot,iwk,ivec,ith

  logical :: intrp  ! .TRUE. if rhowk(1:nwk) not identical to ps%rho_eq_geo

  real(KIND=rspec), parameter :: ZERO = 0.0_rspec
  real(KIND=rspec), parameter :: ZTOL = 5.0e-7_rspec  ! for genxpkg
  real(kind=rspec), parameter :: Z2PI = 6.2831853071795862_rspec

  integer, parameter :: vecsize=401
  real(KIND=rspec) :: rhovec(vecsize),thvec(vecsize)
  real(KIND=rspec) :: rvec(vecsize),zvec(vecsize)

  real(KIND=rspec), dimension(:), allocatable :: Zmidp,Rmidp_in,Rmidp_out
  real(KIND=rspec), dimension(:), allocatable :: Rmin,Rmax,Zmin,Zmax
  real(KIND=rspec), dimension(:), allocatable :: th_Zmin,th_Zmax
  real(KIND=rspec), dimension(:), allocatable :: R_Zmin,R_Zmax,zgeo,zR0,za0
  real(KIND=rspec), dimension(:), allocatable :: th_Rmin,th_Rmax
  real(KIND=rspec), dimension(:), allocatable :: Z_Rmin,Z_Rmax,zgeo2

  real(KIND=rspec), dimension(:,:), allocatable :: zxpkg
  integer, parameter :: ispline=1
  integer, parameter :: ilinear=0

  integer :: loc_cclist(ps_ccount)
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------
  !  for squareness evaluation
  
  real(KIND=rspec) :: zlen,zlen_ab,zlen_ac,zlen_cd,zsqi2
  real(KIND=rspec) :: ra(2),za(2),phia(2)
  real(KIND=rspec) :: rb(2),zb(2),phib(2)
  real(KIND=rspec) :: rx(2),zx(2),phix(2)
  integer :: istat(2)
  integer, parameter :: ibdy_type=1
  !-----------------------

  call ps_mhdflx_errchk('ps_mhdeq_surfparam',ps,nwk,rhowk,iout,ierr)
  if(ierr.ne.0) return

  !-----------------------
  !  for squareness...

  zsqi2 = sqrt(0.5_rspec)
  phia = 0.0_rspec; phib=0.0_rspec; phix=0.0_rspec

  !-----------------------

  call ps_rzsurf_hash(ps,kf1,kf2)

  idiff=1
  if(ps%R_midp_out(1).gt.ZERO) then

     !-----------------------
     !  hash code test

     loc_cclist = ps_cczero
     loc_cclist(ps_cnum_EQ)=ps_chkProfs  ! update EQ profile hashcodes only...
     call plasma_state_hash_code(ps,loc_cclist,ierr)
     if(ierr.ne.0) return

     idiff=0

     iaddr=ps_hash_aux1
     if((kf1.ne.ps%hash_codes(iaddr)).or.(kf2.ne.ps%hash_codes(iaddr+1))) then
        write(iout,*) ' %ps_mhdeq_surfparam: volumes need updating...'
        call ps_mhdeq_volumes(ps,iout,ierr)
        if(ierr.ne.0) return
        call ps_mhdeq_fsmoms(ps,iout,ierr)
        if(ierr.ne.0) return
        idiff=1
     else
        iaddr=ps_hash_aux6
        if(kf1.ne.ps%hash_codes(iaddr)) idiff=idiff+1

        iaddr=iaddr+1
        if(kf2.ne.ps%hash_codes(iaddr)) idiff=idiff+1
     endif
  endif

  if(idiff.eq.0) return

  iaddr=ps_hash_aux6
  ps%new_hash_codes(iaddr)=kf1
  ps%new_hash_codes(iaddr+1)=kf2
  ps%hash_codes(iaddr)=kf1
  ps%hash_codes(iaddr+1)=kf2

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  intrp=.TRUE.
  if(nwk.eq.ps%nrho_eq_geo) then
     if(maxval(abs(ps%rho_eq_geo-rhowk)).lt.1.0e-12_rspec) intrp=.FALSE.
  endif

  if(intrp) then
     allocate(zxpkg(nwk,4))
     call genxpkg(nwk,rhowk,zxpkg,0,1,1,ztol,3,ierr)
  endif

  !------------------------------------------------------------
  !  compute midplane
  allocate(Zmidp(nwk),Rmidp_in(nwk),Rmidp_out(nwk))

  do
     call xplasma_mag_axis(s,ierr, Raxis=Rmidp_in(1),Zaxis=Zmidp(1))
     if(ierr.ne.0) exit

     Rmidp_out(1)=Rmidp_in(1)

     do ith=1,vecsize
        thvec(ith)=(ith-1)*Z2PI/(vecsize-1)
     enddo

     do iwk=2,nwk
        rhovec = rhowk(iwk)

        call xplasma_ctrans(s,xp_use_coord_vecsize,ierr, &
             rho_in=rhovec, theta_in=thvec, &
             r_out=rvec, z_out=zvec)
        if(ierr.ne.0) exit

        call r8_getmpa_ry(rvec,zvec,vecsize, &
             Rmidp_in(iwk),Rmidp_out(iwk),Zmidp(iwk))
     enddo

     if(intrp) then
        call ps_mhdeq_intrp(nwk,zxpkg,Rmidp_in,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%R_midp_in)
        call ps_mhdeq_intrp(nwk,zxpkg,Rmidp_out,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%R_midp_out)
        call ps_mhdeq_intrp(nwk,zxpkg,Zmidp,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%Z_midp)

     else
        ps%R_midp_in = Rmidp_in
        ps%R_midp_out= Rmidp_out
        ps%Z_midp    = Zmidp

     endif

     exit
  enddo

  deallocate(Rmidp_in,Rmidp_out,Zmidp)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_mhdeq_surfparam: xplasma error while computing midplane:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  !------------------------------------------------------------
  ! compute R & Z min/max; also get data for elongation & triangularity
  !  DMC 14 Aug 2009 -- adding upper and lower outer squareness, per request.
  !  definition: Holcomb et al., Phys. Plasmas 16, 056116 (2009) Fig. 1

  allocate(Rmin(nwk),Rmax(nwk),Zmin(nwk),Zmax(nwk))
  allocate(th_Zmin(nwk),th_Zmax(nwk))
  allocate(R_Zmin(nwk),R_Zmax(nwk),zgeo(nwk),zR0(nwk),za0(nwk))
  allocate(th_Rmin(nwk),th_Rmax(nwk))
  allocate(Z_Rmin(nwk),Z_Rmax(nwk),zgeo2(nwk))

  do
     ! evaluate at non-singular surfaces
     do iwk=2,nwk
        call xplasma_RZminmax(s,rhowk(iwk),ierr, &
             i2pi=2,rmin=Rmin(iwk),rmax=Rmax(iwk), &
             thRmin=th_Rmin(iwk),thRmax=th_Rmax(iwk), &
             zmin=Zmin(iwk),thZmin=th_Zmin(iwk), &
             zmax=Zmax(iwk),thZmax=th_Zmax(iwk))
        if(ierr.ne.0) exit
     enddo
     if(ierr.ne.0) exit

     ! convert (rho,theta) -> R_Zmin,max, etc.

     call xplasma_ctrans(s,xp_use_coord_vecsize,ierr, &
          rho_in=rhowk(2:nwk), theta_in=th_Zmin(2:nwk), &
             r_out=R_Zmin(2:nwk), z_out=zgeo(2:nwk))
     if(ierr.ne.0) exit

     call xplasma_ctrans(s,xp_use_coord_vecsize,ierr, &
          rho_in=rhowk(2:nwk), theta_in=th_Zmax(2:nwk), &
             r_out=R_Zmax(2:nwk), z_out=zgeo(2:nwk))
     if(ierr.ne.0) exit

     call xplasma_ctrans(s,xp_use_coord_vecsize,ierr, &
          rho_in=rhowk(2:nwk), theta_in=th_Rmin(2:nwk), &
             r_out=zgeo(2:nwk), z_out=Z_Rmin(2:nwk))
     if(ierr.ne.0) exit

     call xplasma_ctrans(s,xp_use_coord_vecsize,ierr, &
          rho_in=rhowk(2:nwk), theta_in=th_Rmax(2:nwk), &
             r_out=zgeo(2:nwk), z_out=Z_Rmax(2:nwk))
     if(ierr.ne.0) exit

     ! extrapolate to axis and copy or interpolate results

     Rmin(1)=ps%R_midp_in(1)
     Rmax(1)=ps%R_midp_in(1)
     call fini(Rmin,ps%R_surfmin,0)
     call fini(Rmax,ps%R_surfmax,0)

     Zmin(1)=ps%Z_midp(1)
     Zmax(1)=ps%Z_midp(1)
     call fini(Zmin,ps%Z_surfmin,0)
     call fini(Zmax,ps%Z_surfmax,0)

     do iwk=2,nwk
        zgeo(iwk)=(Zmax(iwk)-Zmin(iwk))/(Rmax(iwk)-Rmin(iwk))
        zR0(iwk) =(Rmin(iwk)+Rmax(iwk))*0.5_rspec
        za0(iwk) =(Rmax(iwk)-Rmin(iwk))*0.5_rspec
     enddo

     zR0(1)=Rmin(1)
     call fini(zR0, ps%Rmajor_mean, 0)
     za0(1)=0.0_Rspec
     call fini(za0, ps%Rminor_mean, 0)

     call fini(zgeo,ps%elong,1)  ! quad. extrap. to axis

     do iwk=2,nwk
        zgeo(iwk)=(zR0(iwk)-min(R_Zmin(iwk),R_Zmax(iwk)))/za0(iwk)
     enddo
     zgeo(1)=0.0_Rspec
     call fini(zgeo,ps%triang,0)

     do iwk=2,nwk
        zgeo(iwk)=(zR0(iwk)-R_Zmax(iwk))/za0(iwk)
     enddo
     zgeo(1)=0.0_Rspec
     call fini(zgeo,ps%triangU,0)

     do iwk=2,nwk
        zgeo(iwk)=(zR0(iwk)-R_Zmin(iwk))/za0(iwk)
     enddo
     zgeo(1)=0.0_Rspec
     call fini(zgeo,ps%triangL,0)

     do iwk=2,nwk
        zgeo(iwk)=sin(acos((R_Zmax(iwk)-zR0(iwk))/za0(iwk))-z2pi/4)
     enddo
     zgeo(1)=0.0_Rspec
     call fini(zgeo,ps%triang_miller_U,0)

     do iwk=2,nwk
        zgeo(iwk)=sin(acos((R_Zmin(iwk)-zR0(iwk))/za0(iwk))-z2pi/4)
     enddo
     zgeo(1)=0.0_Rspec
     call fini(zgeo,ps%triang_miller_L,0)

     !  squareness
     zgeo(1)=0.0_rspec
     zgeo2(1)=0.0_rspec
     do iwk=2,nwk
        ! do bdy search two at a time: upper, lower...
        ra(1)=R_Zmax(iwk)
        ra(2)=R_Zmin(iwk)
        za(1:2)=Z_Rmax(iwk)
        rb(1:2)=Rmax(iwk)
        zb(1)=Zmax(iwk)
        zb(2)=Zmin(iwk)

        call xplasma_fxcep(s,ibdy_type, &
             ra,za,phia, rb,zb,phib, rx,zx,phix, istat, ierr, &
             rhosurf=rhowk(iwk))
        if(ierr.ne.0) then
           write(iout,*) ' ?ps_mhdeq_surfparam: xplasma_fxcep error:'
           call xplasma_error(s,ierr,iout)
           exit
        endif

        !  upper:
        zlen = sqrt( (ra(1)-rb(1))**2 + (za(1)-zb(1))**2 )
        zlen_ac = zsqi2*zlen
        zlen_cd = zlen-zlen_ac
        zlen_ab = sqrt( (ra(1)-rx(1))**2 + (za(1)-zx(1))**2 )

        zgeo(iwk)=(zlen_ab-zlen_ac)/zlen_cd

        !  lower:
        zlen = sqrt( (ra(2)-rb(2))**2 + (za(2)-zb(2))**2 )
        zlen_ac = zsqi2*zlen
        zlen_cd = zlen-zlen_ac
        zlen_ab = sqrt( (ra(2)-rx(2))**2 + (za(2)-zx(2))**2 )

        zgeo2(iwk)=(zlen_ab-zlen_ac)/zlen_cd
        
     enddo

     call fini(zgeo,ps%squareUO,0)
     call fini(zgeo2,ps%squareLO,0)

     exit
  enddo

  deallocate(Rmin,Rmax,Zmin,Zmax)
  deallocate(th_Zmin,th_Zmax)
  deallocate(R_Zmin,R_Zmax,zgeo,zR0,za0)
  deallocate(th_Rmin,th_Rmax)
  deallocate(Z_Rmin,Z_Rmax,zgeo2)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_mhdeq_surfparam: xplasma error while computing [R,Z]minmax:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  contains
    subroutine fini(zwk,result,iextrap)
      
      real(kind=rspec), dimension(:) :: zwk
      real(kind=rspec), dimension(:) :: result
      integer, intent(in) :: iextrap

      !  quadratic extrapolation to axis; assume rhowk(1)=ZERO

      if(iextrap.eq.1) then
         zwk(1) = zwk(2) - &
              rhowk(2)**2/(rhowk(3)**2-rhowk(2)**2)*(zwk(3)-zwk(2))
      endif

      if(intrp) then
         call ps_mhdeq_intrp(nwk,zxpkg,zwk,ps%nrho_eq_geo,ps%rho_eq_geo, &
              ispline,iout,result)
      else
         result = zwk
      endif

    end subroutine fini

end subroutine ps_mhdeq_surfparam

subroutine ps_mhdeq_Bminmax(ps,nwk,rhowk,iout,ierr)

  !  Update plasma state "ps" with derived profiles from equilibrium:
  !     Bmin, Bmax, Bmin, Bmax on each flux surface
  !
  !  Profiles are mapped to ps%rho_eq_geo by interpolation from the 
  !  (presumably coarser) grid rhowk(1:nwk) -- computing flux surface
  !  averages on a coarser grid is a way to speed the calculation.
  !  (From TRANSP experience it is a good idea of rhowk and ps%rho_eq_geo
  !  points match near the axis and edge).  To avoid the approximation
  !  associated with interpolation, simply set rhowk = ps%rho_eq_geo.

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod
  use plasma_hash_code

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated
  integer, intent(in) :: nwk              ! size of working grid
  real(KIND=rspec), intent(in) :: rhowk(nwk) ! working grid for integrations
  integer, intent(in) :: iout             ! Fortran LUN for messages
  integer, intent(out) :: ierr            ! exit status code 0=OK

  !-----------------------
  !  local:

  integer :: kf1,kf2,idiff,iaddr,iertot,iwk

  logical :: intrp  ! .TRUE. if rhowk(1:nwk) not identical to ps%rho_eq_geo

  real(KIND=rspec), parameter :: ZERO = 0.0_rspec
  real(KIND=rspec), parameter :: ZTOL = 5.0e-7_rspec  ! for genxpkg

  real(KIND=rspec), dimension(:), allocatable :: Bmin,Bmax

  real(KIND=rspec), dimension(:,:), allocatable :: zxpkg
  integer, parameter :: ispline=1
  integer, parameter :: ilinear=0

  integer :: loc_cclist(ps_ccount),ipair(2)
  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-----------------------

  call ps_mhdflx_errchk('ps_mhdeq_Bminmax',ps,nwk,rhowk,iout,ierr)
  if(ierr.ne.0) return

  call ps_rzsurf_hash(ps,kf1,kf2)

  idiff=1
  if(ps%B_surfMax(1).gt.ZERO) then

     !-----------------------
     !  hash code test

     loc_cclist = ps_cczero
     loc_cclist(ps_cnum_EQ)=ps_chkProfs  ! update EQ profile hashcodes only...
     call plasma_state_hash_code(ps,loc_cclist,ierr)
     if(ierr.ne.0) return

     idiff=0

     iaddr=ps_hash_aux1
     if((kf1.ne.ps%hash_codes(iaddr)).or.(kf2.ne.ps%hash_codes(iaddr+1))) then
        write(iout,*) ' %ps_mhdeq_Bminmax: volumes need updating...'
        call ps_mhdeq_volumes(ps,iout,ierr)
        if(ierr.ne.0) return
        call ps_mhdeq_fsmoms(ps,iout,ierr)
        if(ierr.ne.0) return
        call addpair(ps_phash_g_eq)
        call addpair(ps_phash_psipol)
        call addpair(ps_phash_phit)
        idiff=1
     else
        call addpair(ps_phash_g_eq)
        call addpair(ps_phash_psipol)
        call addpair(ps_phash_phit)
        iaddr=ps_hash_aux7
        if(kf1.ne.ps%hash_codes(iaddr)) idiff=idiff+1

        iaddr=iaddr+1
        if(kf2.ne.ps%hash_codes(iaddr)) idiff=idiff+1
     endif
  else
     call addpair(ps_phash_g_eq)
     call addpair(ps_phash_psipol)
     call addpair(ps_phash_phit)
  endif

  if(idiff.eq.0) return

  iaddr=ps_hash_aux7
  ps%new_hash_codes(iaddr)=kf1
  ps%new_hash_codes(iaddr+1)=kf2
  ps%hash_codes(iaddr)=kf1
  ps%hash_codes(iaddr+1)=kf2

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  intrp=.TRUE.
  if(nwk.eq.ps%nrho_eq_geo) then
     if(maxval(abs(ps%rho_eq_geo-rhowk)).lt.1.0e-12_rspec) intrp=.FALSE.
  endif

  if(intrp) then
     allocate(zxpkg(nwk,4))
     call genxpkg(nwk,rhowk,zxpkg,0,1,1,ztol,3,ierr)
  endif

  !------------------------------------------------------------
  ! compute mod(B) min/max

  allocate(Bmin(nwk),Bmax(nwk))

  do
     call xplasma_mag_axis(s,ierr, baxis=Bmin(1))
     if(ierr.ne.0) exit
     Bmax(1)=Bmin(1)

     do iwk = 2,nwk
        call xplasma_Bminmax(s,rhowk(iwk),ierr, bmin=Bmin(iwk), bmax=Bmax(iwk))
        if(ierr.ne.0) exit
     enddo
     if(ierr.ne.0) exit

     if(intrp) then
        call ps_mhdeq_intrp(nwk,zxpkg,Bmin,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%B_surfmin)
        call ps_mhdeq_intrp(nwk,zxpkg,Bmax,ps%nrho_eq_geo,ps%rho_eq_geo, &
             ispline,iout,ps%B_surfmax)
     else
        ps%B_surfmin = Bmin
        ps%B_surfmax = Bmax
     endif

     exit
  enddo

  deallocate(Bmin,Bmax)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_mhdeq_Bminmax: xplasma error while computing [R,Z]minmax:'
     call xplasma_error(s,ierr,iout)
     return
  endif

  contains

    subroutine addpair(iarg)
      integer :: iarg

      iaddr=iarg
      ipair(1)=ps%new_hash_codes(iaddr)
      ipair(2)=ps%new_hash_codes(iaddr+1)

      call hashCode(ipair,kf1,kf2)
    end subroutine addpair

end subroutine ps_mhdeq_Bminmax

subroutine ps_mhdflx_errchk(subname,ps,nwk,rhowk,iout,ierr)

  ! perform some common error checks

  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  character*(*), intent(in) :: subname    ! name of calling subroutine
  type (plasma_state) :: ps               ! plasma state object to be updated
  integer, intent(in) :: nwk              ! size of working grid
  real(KIND=rspec), intent(in) :: rhowk(nwk) ! working grid for integrations
  integer, intent(in) :: iout             ! Fortran LUN for messages
  integer, intent(out) :: ierr            ! exit status code 0=OK

  real(KIND=rspec), parameter :: ZTOL = 1.0e-10_rspec  ! grid range

  !-----------------------
  !  local:

  integer :: ii
  !-----------------------

  call ps_tagchk(subname,ps%itag,iout,ierr)
  if(ierr.ne.0) return

  if(ps%nrho_eq_geo.eq.0) then
     ierr=1
     write(iout,*) ' '//trim(subname)//': called but no rho_eq_geo grid found.'
     return
  endif

  if((abs(rhowk(1)).gt.ZTOL).or.(abs(rhowk(nwk)-1.0_rspec).gt.ZTOL)) then
     write(iout,*) ' ?'//trim(subname)//': rhowk grid does not span [0,1].'
     ierr=1
     return
  endif

  do ii=2,nwk
     if(rhowk(ii).le.rhowk(ii-1)) then
        write(iout,*) ' ?'//trim(subname)//': rhowk grid not in ascending order.'
        ierr=1
        return
     endif
  enddo

end subroutine ps_mhdflx_errchk
        
