subroutine ur_cstate_zeff_check(isel)

  !  Quasi-neutrality check on state #isel

  use cstate_lib_mod
  implicit NONE

  integer, intent(in) :: isel  ! selected state (btw 1 & nsss_cur)

  !------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar

  integer :: i,ii,jj,inrho,inrhom1,icopy,ins,ierr
  integer :: ith,inbi,ifus,irf
  real*8, dimension(:), allocatable :: my_rho,nzth,nzfast,nztot,adiff,rdiff,ne
  real*8, dimension(:), allocatable :: Zeff
  real*8, dimension(:,:), allocatable :: nall

  real*8, dimension(:), allocatable :: Zia
  logical, dimension(:), allocatable :: isfast

  real*8 :: nezef1,ne1

  real*8, parameter :: ZERO = 0.0d0
  real*8, parameter :: ONE  = 1.0d0
  !------------------------

  luntty = lnout(0)

  !  error checks...

  if(nsss_cur.eq.0) then
     write(luntty,*) ' ur_cstate_zeff_check: no states are loaded.'
     return
  endif

  if((isel.le.0).or.(isel.gt.nsss_cur)) then
     write(luntty,*) ' ur_cstate_zeff_check: state index out of range:',isel
     return
  endif

  if(.NOT.allocated(sss(isel)%ns)) then
     write(luntty,*) ' ur_cstate_zeff_check: state thermal densities not found.'
     return
  endif

  if(.NOT.allocated(sss(isel)%qatom_all)) then
     write(luntty,*) ' ur_cstate_zeff_check: state merged specie list not found.'
     return
  endif

  i = isel
  ins = sss(i)%nspec_all

  do

     !  choose rho grid

     if(lxout(0)) then
        write(luntty,*) 'ur_cstate_zeff_check rho-grid select menu:'
        write(luntty,*) '  A -- use PLASMA grid, size is: ',sss(i)%nrho
        if(allocated(sss(i)%rho_nbi)) then
           write(luntty,*) '  B -- use NBI grid, size is: ',sss(i)%nrho_nbi
        endif
        if(allocated(sss(i)%rho_fus)) then
           write(luntty,*) '  C -- use FUS grid, size is: ',sss(i)%nrho_fus
        endif
        if(allocated(sss(i)%rho_icrf)) then
           write(luntty,*) '  D -- use IC grid, size is: ',sss(i)%nrho_icrf
        endif
        write(luntty,*) '  N -- use equispaced grid, user selected size.'
        write(luntty,*) '  U -- examine un-rezoned data.'
        write(luntty,*) '  Q -- quit this menu.'
     endif

     call uread('ur_cstate_zeff_check: menu option:')

     if(lchar('Q')) then
        exit

     else if(lchar('A')) then
        inrho = sss(i)%nrho
        icopy = 1

     else if(lchar('B')) then
        inrho = sss(i)%nrho_nbi
        icopy = 2

     else if(lchar('C')) then
        inrho = sss(i)%nrho_fus
        icopy = 3

     else if(lchar('D')) then
        inrho = sss(i)%nrho_icrf
        icopy = 4

     else if(lchar('U')) then
        call ur_cstate_nchk(isel)
        cycle

     else if(lchar('N')) then
        icopy = 0

        inrho = sss(i)%nrho
        if(lxout(0)) then
           write(luntty,*) ' #zones = grid size - 1'
        endif

        call ureadi('enter grid size btw 5 and 501:',inrho)
        inrho=max(5,min(501,inrho))

     else
        call uermsg('unrecognized menu option',' ',0)
        cycle
     endif

     !  allocate and initialize arrays

     if(inrho.eq.0) then
        call uermsg('ur_cstate_zeff_check: no such grid.',0)
        cycle
     endif

     if(inrho.gt.999) then
        call uermsi('grid too large, max size = 999',inrho,0)
        cycle
     endif

     if(allocated(my_rho)) then
        deallocate(isfast,Zia,my_rho,nzth,nzfast,nztot,adiff,rdiff,nall,ne, &
             Zeff)
     endif

     allocate(isfast(ins),Zia(ins))
     allocate(my_rho(inrho),nzth(inrho-1),nzfast(inrho-1),nztot(inrho-1))
     allocate(adiff(inrho-1),rdiff(inrho-1))
     allocate(nall(inrho-1,ins))
     allocate(ne(inrho-1),Zeff(inrho-1))

     nzth = ZERO; nzfast = ZERO; nztot = ZERO
     adiff = ZERO; rdiff = ZERO
     nall = ZERO

     !  copy or generate chosen rho grid

     if(icopy.eq.1) then
        my_rho = sss(i)%rho
     else if(icopy.eq.2) then
        my_rho = sss(i)%rho_nbi
     else if(icopy.eq.3) then
        my_rho = sss(i)%rho_fus
     else if(icopy.eq.4) then
        my_rho = sss(i)%rho_icrf
     else
        my_rho(1) = ZERO
        my_rho(inrho) = ONE
        do jj = 2,inrho-1
           my_rho(jj) = (jj-1)*ONE/(inrho-1)
        enddo
     endif

     ! indexing for each ion category

     ith = 0
     inbi = 0
     ifus = 0
     irf = 0

     ! get electron density rezoned to chosen grid

     call ps_rho_rezone(my_rho,sss(i)%id_ns(0),ne,ierr, state=sss(i))
     if(ierr.ne.0) then
        call uermsg('ur_cstate_zeff_check: error during rezoning.',0)
        cycle
     endif

     call ps_rho_rezone(my_rho,sss(i)%id_Zeff,Zeff,ierr, state=sss(i))
     if(ierr.ne.0) then
        call uermsg('ur_cstate_zeff_check: error during rezoning.',0)
        cycle
     endif

     ! get all the densities rezoned to the chosen grid
     ! get all the charge numbers

     do ii=1,ins
        if(sss(i)%all_type(ii).eq.ps_beam_ion) then
           inbi = inbi + 1
           Zia(ii) = sss(i)%q_snbi(inbi)/ps_xe
           call ps_rho_rezone(my_rho,sss(i)%id_nbeami(inbi),nall(:,ii),ierr, &
                state=sss(i))
           isfast(ii)=.TRUE.
           if(ierr.ne.0) exit

        else if(sss(i)%all_type(ii).eq.ps_fusion_ion) then
           ifus = ifus + 1
           Zia(ii) = sss(i)%q_sfus(ifus)/ps_xe
           call ps_rho_rezone(my_rho,sss(i)%id_nfusi(ifus),nall(:,ii),ierr, &
                state=sss(i))
           isfast(ii)=.TRUE.
           if(ierr.ne.0) exit

        else if(sss(i)%all_type(ii).eq.ps_rf_minority) then
           irf = irf + 1
           Zia(ii) = sss(i)%q_rfmin(irf)/ps_xe
           call ps_rho_rezone(my_rho,sss(i)%id_nmini(irf),nall(:,ii),ierr, &
                state=sss(i))
           isfast(ii)=.TRUE.
           if(ierr.ne.0) exit

        else
           ith = ith + 1
           Zia(ii) = sss(i)%q_s(ith)/ps_xe
           call ps_rho_rezone(my_rho,sss(i)%id_ns(ith),nall(:,ii),ierr, &
                state=sss(i))
           isfast(ii)=.FALSE.
           if(ierr.ne.0) exit
        endif
     enddo

     if(ierr.ne.0) then
        call uermsg('ur_cstate_zeff_check: error during rezoning.',0)
        cycle
     endif

     ! compute (summation arrays were already ZEROed).

     do ii=1,ins
        if(isfast(ii)) then
           nzfast = nzfast + Zia(ii)*Zia(ii)*nall(:,ii)
        else
           nzth = nzth + Zia(ii)*Zia(ii)*nall(:,ii)
        endif
     enddo

     nztot = nzfast + nzth

     ! compute absolute and relative deviation

     do jj=1,inrho-1
        nezef1 = max(ONE,ne(jj)*Zeff(jj))
        adiff(jj) = nezef1-nztot(jj)
        rdiff(jj) = abs(adiff(jj))/nezef1
     enddo

     ! printout

     write(luntty,*) ' '
     write(luntty,*) ' zone  Zeff(th)    Zeff(fast)  Zeff(ions)  Zeff(data)  diff        rel. diff '
     write(luntty,*) ' ----  ----------  ----------  ----------  ----------  ----------  ----------'
     do jj=1,inrho-1
        ne1 = max(ONE,ne(jj))
        write(luntty,'(2x,i3,2x,6(1pe11.4,1x))') jj,nzth(jj)/ne1, &
             nzfast(jj)/ne1, &
             nztot(jj)/ne1,Zeff(jj),adiff(jj)/ne1,rdiff(jj)
     enddo
     write(luntty,*) ' '

  enddo

end subroutine ur_cstate_zeff_check
