subroutine ps_rzsurf_sdata(ps, idim1,idim2, Rarr,Zarr, idim_rho, inth, &
     ibcR0,zbcR0, ibcR1,zbcR1, ibcZ0,zbcZ0, ibcZ1,zbcZ1, &
     iout,ierr)

  !  Update plasma state "ps" with equilibrium flux surfaces from data
  !  XPLASMA2 implementation -- DMC Feb 2008

  !  All dimensioning is explicit; error checking is minimal.
  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps               ! plasma state object to be updated

  integer, intent(in) :: idim1,idim2      ! Rarr & Zarr dimensions
  real(KIND=rspec), intent(in) :: Rarr(idim1,idim2), Zarr(idim1,idim2)

  integer, intent(in) :: idim_rho         ! indicate which dimension is "rho"
  ! 1 for 1st dimension; 2 for 2nd dimension

  integer, intent(in) :: inth             ! "th" dimensions for bdy cond data

  integer, intent(in) :: ibcR0            ! rho=0 BC type code (0="not a knot")
  real(KIND=rspec), intent(in) :: zbcR0(inth)  ! rho=0 BC data

  integer, intent(in) :: ibcR1            ! rho=1 BC type code (1="not a knot")
  real(KIND=rspec), intent(in) :: zbcR1(inth)  ! rho=1 BC data

  integer, intent(in) :: ibcZ0            ! rho=0 BC type code (0="not a knot")
  real(KIND=rspec), intent(in) :: zbcZ0(inth)  ! rho=0 BC data

  integer, intent(in) :: ibcZ1            ! rho=1 BC type code (1="not a knot")
  real(KIND=rspec), intent(in) :: zbcZ1(inth)  ! rho=1 BC data

  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-----------------------
  !  local:

  integer :: inrho,inth1,id_rho,id_th,id_R,id_Z,ii,jj,iertmp

  real(KIND=rspec), dimension(:,:), allocatable :: r_geo_xpl,z_geo_xpl
  real(KIND=rspec), dimension(:,:,:), allocatable :: r_dgeo_xpl,z_dgeo_xpl

  type (interp_object) :: xobj
  type (xplasma), pointer :: s

  !-------------------------------------------------------------------------
  ! executable code...

  call ps_tagchk('ps_rzsurf_sdata',ps%itag,iout,ierr)
  if(ierr.ne.0) return

  if(ps_debug.ge.2) then
     write(iout,*) 'ps_rzsurf_sdata: set flux surface geometry from'// &
          ' passed data.'
  endif

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_rzsurf_sdata: invalid or unitialized xplasma object.'
     return
  endif

  if(idim_rho.ge.2) then
     inrho=idim2
     inth1=idim1
  else
     inrho=idim1
     inth1=idim2
  endif

  if((inrho.ne.ps%nrho_eq).or.(inth1.ne.ps%nth_eq)) then
     write(iout,*) ' ?ps_rzsurf_sdata:  Rarr and Zarr array dimensions not'// &
          ' consistent with'
     write(iout,*) '  state object rho_eq and th_eq grid sizes: ', &
          ps%nrho_eq, ps%nth_eq
     ierr=999
     return
  endif

  if(inth.ne.inth1) then
     write(iout,*) ' ?ps_rzsurf_sdata:  theta dimension for Rarr and Zarr'
     write(iout,*) '     not consistent with bdy condition data dimension.'
     ierr=999
     return
  endif

  !----------------------
  ! OK...
  !----------------------

  call xplasma_author_set(s,'EQ',iertmp)

  !----------------------

  call xplasma_gridId(s,'RHO_EQ',id_rho)
  call xplasma_gridId(s,'TH_EQ',id_th)

  if(idim_rho.ge.2) then
     call xplasma_rzmag(s,id_th,id_rho, Rarr,Zarr, &
          id_R,id_Z,ierr, &
          ibc0=ibcR0,Rbc0=zbcR0,Zbc0=zbcZ0, &
          ibc1=ibcR1,Rbc1=zbcR1,Zbc1=zbcZ1)
  else
     call xplasma_rzmag(s,id_rho,id_th, Rarr,Zarr, &
          id_R,id_Z,ierr, &
          ibc0=ibcR0,Rbc0=zbcR0,Zbc0=zbcZ0, &
          ibc1=ibcR1,Rbc1=zbcR1,Zbc1=zbcZ1)
  endif

  if(ierr.ne.0) then
     call xplasma_error(s,ierr,iout)
     write(iout,*) ' ?error occured in ps_rzsurf_sdata: (R,Z) load operation.'
     return
  endif

  !----------------------
  !  Now, convert internal representation to equivalent bicubic Hermite;
  !----------------------

  call xplasma_hermitize(s,id_R,iertmp)
  if(iertmp.gt.0) then
     call xplasma_error(s,iertmp,iout)
     ierr=iertmp
  endif

  call xplasma_hermitize(s,id_Z,iertmp)
  if(iertmp.gt.0) then
     call xplasma_error(s,iertmp,iout)
     ierr=iertmp
  endif

  if(ierr.ne.0) then
     write(iout,*) &
          ' ?error occured in ps_rzsurf_sdata: (R,Z) Hermite conversion.'
     return
  endif
  !----------------------

  call xplasma_author_clear(s,'EQ',iertmp)

  !----------------------
  !  then retrieve data and store in state element arrays

  allocate(r_geo_xpl(inth,inrho))
  allocate(z_geo_xpl(inth,inrho))
  allocate(r_dgeo_xpl(3,inth,inrho))
  allocate(z_dgeo_xpl(3,inth,inrho))

  do
     call xplasma_eval_prof(s,id_R,r_geo_xpl,ierr,zcoeff=r_dgeo_xpl)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_update_mhdeq: R_geo fetch error:'
        call xplasma_error(s,ierr,iout)
        exit
     endif

     call xplasma_eval_prof(s,id_Z,z_geo_xpl,ierr,zcoeff=z_dgeo_xpl)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_update_mhdeq: Z_geo fetch error:'
        call xplasma_error(s,ierr,iout)
        exit
     endif

     do jj=1,inth
        do ii=1,inrho
           ps%R_geo(ii,jj) = r_geo_xpl(jj,ii)
           ps%Z_geo(ii,jj) = z_geo_xpl(jj,ii)

           ps%dR_geo_drho(ii,jj) = r_dgeo_xpl(2,jj,ii)
           ps%dR_geo_dth(ii,jj) = r_dgeo_xpl(1,jj,ii)
           ps%d2R_geo_drhodth(ii,jj) = r_dgeo_xpl(3,jj,ii)

           ps%dZ_geo_drho(ii,jj) = z_dgeo_xpl(2,jj,ii)
           ps%dZ_geo_dth(ii,jj) = z_dgeo_xpl(1,jj,ii)
           ps%d2Z_geo_drhodth(ii,jj) = z_dgeo_xpl(3,jj,ii)
        enddo
     enddo
     exit
  enddo

  deallocate(r_geo_xpl,z_geo_xpl)
  deallocate(r_dgeo_xpl,z_dgeo_xpl)

  ! and finally: derived metric flux surface averages and integrals:

  if(ierr.eq.0) then
     call ps_mhdeq_volumes(ps,iout,ierr)
     if(ierr.eq.0) then
        call ps_mhdeq_fsmoms(ps,iout,ierr)
     endif
  endif

end subroutine ps_rzsurf_sdata
