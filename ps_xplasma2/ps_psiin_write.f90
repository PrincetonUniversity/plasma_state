subroutine ps_psiin_write(ps,nonlin,zeqdsk,zpsiin,inprof,zgafsep, ierr)

  !  DMC 24 Jan 2007 -- make psiin from eqdsk
  !  for GA TORAY

  !  DMC 18 Jun 2010 -- adapt for use w/ Plasma State
  !  heatlib/psiin_write.f90 -> ps_xplasma2/ps_psiin_write.f90

  !  The psiin file is a file related to the eqdsk file; it is an ascii
  !  file with format and contents specified by GA physicists.

  use geqdsk_nomds_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod

  implicit NONE

  INTEGER, PARAMETER :: R8=SELECTED_REAL_KIND(12,100)

  !------------------------
  !  subroutine arguments:

  type (plasma_state) :: ps        ! plasma state instance
  !  (expected to contain MHD equilibrium data matching (zeqdsk) data).

  integer, intent(in) :: nonlin    ! LUN for messages

  character*(*), intent(in) :: zeqdsk   ! path to existing GEQDSK file to read
  character*(*), intent(in) :: zpsiin   ! path to psiin file to write

  integer, intent(in) :: inprof    ! size of (bdy-oriented) TORAY grid
  real(R8), intent(in) :: zgafsep  ! Separatrix gap: a datum needed for psiin

  integer, intent(out) :: ierr     ! completion status code, 0=normal

  !------------------------
  !  local data:

  type(geqdsk) :: zg
  integer :: id_psi,icentr,ii,jj,ix,idi,lunphd,ilun_save

  real*8, parameter :: ZERO = 0.0d0
  real*8, parameter :: ONE = 1.0d0
  real*8, dimension(inprof) :: zx,zpsi  ! sqrt(Phi/Philim) grid, & Psi(x)

  !  normalized flux surface averages for TORAY 1.8
  integer :: inps
  real*8 :: zb0,zr0,zdelpsi,zpsi00,zpsirz_min
  real*8, dimension(:), allocatable :: zpsif,zxpsi,zhmmx,zb2n,zb1n,zrin

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !------------------------

  call ps_tagchk('ps_psiin_write',ps%itag,nonlin,ierr)
  if(ierr.ne.0) return

  ! check contents

  call ps_wrgeq_chk(ps,'ps_psiin_write',nonlin,ierr)
  if(ierr.ne.0) return

  ! OK basics seems to be there...

  ! get xplasma pointer

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  ! find available fortran I/O unit number
  call find_io_unit(lunphd)

  call geq_getlun(ilun_save)  ! GEQ module I/O unit saved off; restored later

  do
     ! read 
     call geq_setlun(lunphd)
     call geq_init(zg, zeqdsk, ierr)
     if(ierr.ne.0) then
        write(nonlin,*) ' ?psiin_write: failed to read G-eqdsk file: ', &
             trim(zeqdsk)
        exit
     endif

     if(abs(zg%ZMID_M).gt.1.0d-3) then
        write(nonlin,*) ' ?psiin_write: G-eqdsk file Zmid non-zero: ',zg%ZMID_M
        write(nonlin,*) '  error reading file: ',trim(zeqdsk)
        ierr=1
        exit
     endif

     ! make sure Psi(0)=0; delta(Psi) unchanged

     zpsi00 = zg%simag_Wb__Rad
     zdelpsi= zg%sibry_Wb__Rad - zg%simag_Wb__Rad

     zpsirz_min = minval(zg%psirz_Wb__Rad)
     
     ! RGA, if simag and sibry are being adjusted then psirz must also
     !      be adjusted to maintain consistency (TRANSP call to toray bug fix)
     if(.true. .or. abs(zpsirz_min).gt.0.05d0*abs(zdelpsi)) then
        write(nonlin,*) ' %psiin_write: apply Psi(0) correction to Psi(R,Z)'
        zg%psirz_Wb__Rad = zg%psirz_Wb__rad - zg%simag_Wb__Rad
     endif

     zg%sibry_Wb__Rad = zdelpsi
     zg%simag_Wb__Rad = 0.0d0

     ! compute Psi(x) on equispaced x grid

     do ix=1,inprof
        zx(ix)=(ix-1)*ONE/(inprof-1)
     enddo

     call xplasma_common_ids(s,ierr, id_psi=id_psi)
     call ckerr('xplasma_common_ids'); if(ierr.ne.0) exit

     call xplasma_eval_prof(s,id_psi,zx,zpsi,ierr)
     call ckerr('xplasma_eval_prof'); if(ierr.ne.0) exit

     !-------------------------------
     !  allocate profiles for flux surface averages
  
     zb0 = zg%bcentr_t
     zr0 = zg%rcentr_m

     inps = zg%nw
     allocate(zpsif(inps))  ! equispaced Psi grid
     allocate(zxpsi(inps))  ! corresponding x values

     call xplasma_rhopsi_gen(s,inps,ierr, psivals=zpsif, rhovals=zxpsi)
     call ckerr('xplasma_rhopsi_gen'); if(ierr.ne.0) exit

     !  ** normalize the Psi vector ** to match G-eqdsk

     zpsi(1:inprof) = zpsi(1:inprof)*(zdelpsi/zpsi(inprof))

     !  the flux surface averages:
     allocate(zhmmx(inps))  ! <sqrt(1-B/Bmax)>
     allocate(zb2n(inps))   ! <B^2>/B0^2
     allocate(zb1n(inps))   ! <B>/B0
     allocate(zrin(inps))   ! R0*<1/R>

     !  set up for numerical integration on this grid

     call xplasma_author_set(s,'psiin_write',ierr)
     call ckerr('xplasma_author_set'); if(ierr.ne.0) exit

     call xplasma_create_integ(s,'psiin_write_integ',zxpsi,idi,ierr, &
          cache_enable=.TRUE.)
     call ckerr('xplasma_create_integ'); if(ierr.ne.0) exit

     call xplasma_rho_zonint(s,idi,'<sqrt(1-B/Bmax)>S',zhmmx,ierr)
     call ckerr('xplasma_rho_zonint (1)'); if(ierr.ne.0) exit

     call xplasma_rho_zonint(s,idi,'<B^2>S',zb2n,ierr)
     call ckerr('xplasma_rho_zonint (2)'); if(ierr.ne.0) exit
     zb2n = zb2n/(zb0*zb0)

     call xplasma_rho_zonint(s,idi,'<B>S',zb1n,ierr)
     call ckerr('xplasma_rho_zonint (3)'); if(ierr.ne.0) exit
     zb1n = abs(zb1n/zb0)

     call xplasma_rho_zonint(s,idi,'<1/R>S',zrin,ierr)
     call ckerr('xplasma_rho_zonint (4)'); if(ierr.ne.0) exit
     zrin = zr0*zrin

     !  all done; clean up...

     call xplasma_remove_item(s,idi,ierr)
     call ckerr('xplasma_remove_item'); if(ierr.ne.0) exit

     call xplasma_author_clear(s,'psiin_write',ierr)
     call ckerr('xplasma_author_clear'); if(ierr.ne.0) exit

     !-------------------------------
     zpsi = zpsi * 1.0d5   ! T*m**2/rad --> kGauss*cm**2/rad
     !-------------------------------

     ierr=0
     exit  ! normal exit

  enddo

  call geq_setlun(ilun_save)  ! GEQ module I/O unit restored
  if(ierr.ne.0) return

  !-------------------------------
  ! write psiin

  open(unit=lunphd, file=zpsiin, status='unknown')

200 format(5e16.9)

  icentr=2
  write(lunphd, '(4i4)') icentr,inprof,inps,zg%nh

  write(lunphd, 200) zg%rdim_m, zg%zdim_m, zg%rcentr_m, zg%rleft_m, ZERO
  write(lunphd, 200) zg%rmaxis_m, zg%zmaxis_m, &
       zg%simag_Wb__Rad, zg%sibry_Wb__Rad, zg%bcentr_t

  write(lunphd, 200) zgafsep
  write(lunphd, 200) (zg%fpol_tm(ii),ii=1,inps)
  write(lunphd, 200) ((zg%psirz_Wb__Rad(ii,jj),ii=1,inps),jj=1,zg%nh)

  write(lunphd, 200) (zpsi(ii),ii=1,inprof)  ! Psi on "transport" grid

  write(lunphd, 200) (zg%qpsi(ii),ii=1,inps)

  write(lunphd, '(2i5)') zg%nbbbs,zg%limitr
  write(lunphd, 200) (zg%rbbbs_m(ii),zg%zbbbs_m(ii),ii=1,zg%nbbbs)
  write(lunphd, 200) (zg%rlim_m(ii),zg%zlim_m(ii),ii=1,zg%limitr)

  !---------------------------------
  !  append flux surface averages...

  write(lunphd, 200) (zhmmx(ii),ii=1,inps)
  write(lunphd, 200) (zb2n(ii),ii=1,inps)
  write(lunphd, 200) (zb1n(ii),ii=1,inps)
  write(lunphd, 200) (zrin(ii),ii=1,inps)

  !---------------------------------
  close(unit=lunphd)

  deallocate(zpsif,zxpsi,zhmmx,zb2n,zb1n,zrin)

CONTAINS

  subroutine ckerr(zname)
    character*(*) zname  ! xplasma call that failed

    if(ierr.ne.0) then
       write(nonlin,*) ' ?psiin_write: error in xplasma: ',trim(zname)
       call xplasma_error(s,ierr,nonlin)
    endif

  end subroutine ckerr

end subroutine ps_psiin_write
