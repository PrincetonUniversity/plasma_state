subroutine ps_sconfig_namelist_check(ss,iout,ierr)

  !------------------------------------------------------------
  ! (hand maintained) namelist input checks -- machine description namelist
  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions

  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ss       ! plasma state object
  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-----------------------

  call ps_sconfig_nphi_check(ss,iout,ierr)

end subroutine ps_sconfig_namelist_check

subroutine ps_sconfig_nphi_check(ss,iout,ierr)

  !------------------------------------------------------------
  ! (hand maintained) data checks: nphi, wt_nphi...
  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions

  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  !  passed:

  type (plasma_state) :: ss       ! plasma state object
  integer, intent(in) :: iout     ! Fortran LUN for messages
  integer, intent(out) :: ierr    ! exit status code 0=OK

  !-----------------------
  !  local:

  integer :: iant,i,inum,iwarn,iwarn2,iprev_nphi
  real*8 :: zwsum
  !-----------------------

  ierr = 0

  iwarn = 0
  iwarn2 = 0

  do iant = 1,ss%nicrf_src
     inum = ss%num_nphi(iant)

     zwsum = 0.0_rspec
     do i=1,inum
        if(ss%wt_nphi(i,iant).le.0.0_rspec) then
           if(iwarn.eq.0) then
              write(iout,*) &
                   ' ?ps_sconfig_nphi_check: invalid ICRF n_phi weight (.LE.0):'
           endif
           iwarn = iwarn + 1
           write(iout, &
                '(" antenna #",i3," n_phi #",i3,"   n_phi =",i4,"  wt = ",1pe12.5)') &
                iant, i, ss%nphi(i,iant), ss%wt_nphi(i,iant)
        else
           zwsum = zwsum + ss%wt_nphi(i,iant)
        endif
        
        if(i.eq.1) then
           iprev_nphi=ss%nphi(i,iant)-1
        else
           iprev_nphi=ss%nphi(i-1,iant)
        endif

        if(iprev_nphi.ge.ss%nphi(i,iant)) then
           if(iwarn2.eq.0) then
              write(iout,*) &
                   ' ?ps_sconfig_nphi_check: nphi values not in ascending order:'
           endif
           iwarn2 = iwarn2 + 1
           write(iout, &
                '(" antenna #",i3," n_phi values: ",2(1x,i4))') &
                iant,iprev_nphi,ss%nphi(i,iant)
        endif
     enddo
     if(zwsum.gt.0.0_rspec) then
        ss%wt_nphi(1:inum,iant) = ss%wt_nphi(1:inum,iant)/zwsum
     endif

     if(ierr.ne.0) return

  enddo

end subroutine ps_sconfig_nphi_check
