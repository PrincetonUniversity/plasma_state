subroutine ps_mhdeq_pchk(ps,ipprime,insmoo,rho_smoo,delrho_smoo,iout,ierr)

  !  Given a state "ps" which imbeds an equilibrium representation read
  !  from a g file, update the pressure profile to improve smoothness of 
  !  grad(P).

  !  Two options are provided:

  !     ipprime=1 -- recompute pressure by integrating dP/dpsi in from
  !                  the edge, starting from the existing P(bdy).

  !     explicit smoothing -- [rho_smoo(1:insmoo),delrho_smoo(1:insmoo)]
  !                  to specify radially varying smoothing using a moving
  !                  triangular weighted average

  !                  Psmoo(rho) = avg[rho-delrho_smoo:rho+delrho_smoo]{P(rho)}

  !                  rho_smoo must be monotonic increasing but need not cover
  !                  [0:1] and may contain just one point (insmoo=1 OK).

  !                  delrho_smoo=0.0 means no smoothing; 
  !                        0.0 <= delrho_smoo(j) <= 0.2 expected everywhere.

  !                  delrho_smoo is interpolated linearly (and extrapolated 
  !                  flat) to cover the rho of the pressure profile data.

  !  NOTE: this routine is related to ps_update_mhdeq -- it assumes xplasma
  !        data to be present that was read from a G-eqdsk file.  Therefore,
  !        this routine should be called only just after a state has been 
  !        loaded from G-eqdsk by ps_update_mhdeq(...).
  !        
  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions

  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ps       ! plasma state object to be updated

  integer, intent(in) :: ipprime  ! =0: no PPRIME correction; =1: use PPRIME

  integer, intent(in) :: insmoo   ! number of smoothing parameter data points
  real(KIND=rspec), intent(in) :: rho_smoo(insmoo)     ! rho values
  real(KIND=rspec), intent(in) :: delrho_smoo(insmoo)  ! values vs rho

  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-----------------------
  integer :: ii,indx,id_p,ismoo,imono,irange,inrho,idum1,idum2

  type (interp_object) :: xobj
  type (xplasma), pointer :: s

  real(KIND=rspec), dimension(:), allocatable :: pp,psm,del,eps
  real(KIND=rspec) :: zrho,zrhop,zbc0,zbc1,zdum1
  !-----------------------
  !  argument error checking

  ierr=0

  ismoo=0

  if((ipprime.lt.0).or.(ipprime.gt.1)) then
     write(iout,*) ' ?ps_mhdeq_pchk: "ipprime" argument value invalid: ', &
          ipprime
     write(iout,*) '  0 or 1 was expected.'
     ierr = ierr + 1
  endif

  imono=1
  irange=1

  zrho=0.0_rspec

  do ii=1,insmoo
     zrhop=zrho
     zrho=max(0.0_rspec,min(1.0_rspec, rho_smoo(ii)))

     if((rho_smoo(ii).lt.-0.001_rspec).or.(rho_smoo(ii).gt.1.001_rspec)) then
        if(imono.eq.1) then
           write(iout,*) ' ?ps_mhdeq_pchk: rho_smoo value not btw 0 and 1: ', &
                rho_smoo(ii)
           imono=0
        endif
        ierr = ierr + 1
     endif

     if((delrho_smoo(ii).lt.-0.001_rspec).or. &
          (delrho_smoo(ii).gt.0.201_rspec)) then
        if(irange.eq.1) then
           write(iout,*) &
                ' ?ps_mhdeq_pchk: delrho_smoo value not btw 0.0 and0.2: ', &
                delrho_smoo(ii)
           irange=0
        endif
        ierr = ierr + 1
     endif

     if(ii.gt.1) then
        if(zrho.le.zrhop) then
           if(imono.eq.1) then
              write(iout,*) &
                   ' ?ps_mhdeq_pchk: rho_smoo(...) not monotonic: ', &
                   zrho,zrhop
              imono=0
           endif
           ierr = ierr + 1
        endif
     endif

     if(delrho_smoo(ii).gt.0.0_rspec) ismoo = 1
  enddo

  if(ierr.ne.0) then
     ierr=9999
     write(iout,*) ' ?ps_mhdeq_pchk: argument input data error.'
     return
  endif

  if((ismoo.eq.0).and.(ipprime.eq.0)) then
     return  ! nothing to be done
  endif

  !------------------------------------------------------
  !  OK... process data...

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_profId(s,'P',id_p)

  if(ipprime.eq.1) then
     call xplasma_geqdsk_ppadj(s,ierr,match_auth=.TRUE.)
  endif

  call xplasma_eval_prof(s,id_p,ps%rho_eq,ps%p_eq, ierr)
  if(ierr.ne.0) then
     write(iout,*) ' ?ps_mhdeq_pchk: xplasma P profile eval error:'
     call xplasma_error(s,ierr,iout)
     ierr=9999
     return
  endif

  if(ismoo.eq.0) return

  inrho = ps%nrho_eq
  allocate(pp(inrho),psm(inrho),del(inrho),eps(inrho))

  eps = 1.0e20_rspec

  pp = ps%p_eq
  psm = 0.0_rspec

  indx = 1
  do ii=1,inrho
     if(ps%rho_eq(ii).le.rho_smoo(1)) then
        del(ii)=delrho_smoo(1)
     else if(ps%rho_eq(ii).ge.rho_smoo(insmoo)) then
        del(ii)=delrho_smoo(insmoo)

     else
        do 
           if(ps%rho_eq(ii).gt.rho_smoo(indx+1)) then
              indx=indx+1
           else
              exit
           endif
        enddo

        del(ii) = delrho_smoo(indx) + (ps%rho_eq(ii)-rho_smoo(indx))* &
             (delrho_smoo(indx+1)-delrho_smoo(indx))/ &
             (rho_smoo(indx+1)-rho_smoo(indx))
     endif
  enddo

  zbc0=1.0_rspec   ! dP/drho -> 0 @ rho=0
  zbc1=0.5_rspec   ! intermediate smoothing BC @ rho=1

  call ps_filtr6(ps%rho_eq, pp, psm, inrho, eps, inrho, eps(1:1), 0, del, &
       0,eps(1), 0,del(1), zbc0, zbc1, zdum1, idum1,idum2)

  ps%p_eq = psm

  deallocate(pp,psm,del,eps)

end subroutine ps_mhdeq_pchk
