subroutine ur_cstate_misc_plots

  !  miscellaneous plotting options...

  use cstate_lib_mod
  use plasma_hash_code
  implicit NONE

  !------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar

  integer, parameter :: nbdy=501
  real*8, parameter :: z2pi = 6.2831853071795862D+00

  integer :: ith
  real*8 :: xbdy(nbdy),thbdy(nbdy)

  real*8, dimension(:,:), allocatable :: rbdy,zbdy,rlim,zlim,rcon,zcon

  integer :: iself(nsss_cur),inums(nsss_cur),insel,i,indx,idR,idZ,ierr
  integer :: isel2(nsss_cur),inum2(nsss_cur),insel2,indx2
  character*32 slbls(nsss_cur),slbl2(nsss_cur)
  real*8 :: zndx(nsss_cur)
  integer :: have_geo(nsss_cur)
  integer :: have_lim(nsss_cur)
  integer :: num_rzlim(nsss_cur)
  integer :: max_rzlim,inum

  integer :: ihash,inmax,isel,incon,ii,ind
  integer :: lim_hash(nsss_cur)
  integer :: bdy_hash(nsss_cur)
  integer :: ns_hash(nsss_cur),insgot
  integer :: inum_contours(nsss_cur)

  integer, dimension(:), allocatable :: iselc,inumc
  real*8, dimension(:), allocatable :: zndxc
  character*32, dimension(:), allocatable :: slblc
  !------------------------

  luntty = lnout(0)

  if(nsss_cur.eq.0) then
     write(luntty,*) ' ur_cstate_misc_plots: no states are loaded.'
     return
  endif

  !------------------------
  !  bdy in flux coordinates:
  do ith=1,nbdy
     xbdy(ith)=1.0d0
     thbdy(ith)=(ith-1)*z2pi/(nbdy-1)
  enddo

  !------------------------
  !  count number of states with geometry & limiter information available

  lim_hash=0
  bdy_hash=0

  have_geo = 0
  have_lim = 0
  inum_contours = 0

  inmax=nbdy

  insel = 0
  insel2 = 0
  insgot = 0
  max_rzlim = 0
  do i=1,nsss_cur
     zndx(i)=i
     idR = sss(i)%id_R_geo
     idZ = sss(i)%id_Z_geo
     if((idR.gt.0).and.(idZ.gt.0)) then
        insel = insel + 1
        iself(insel)=insel
        inums(insel)=nbdy
        slbls(insel)=sss_label(i)

        have_geo(i)=insel
        inum_contours(i) = inum_contours(i) + 1
     endif

     inum=sss(i)%num_rzlim
     num_rzlim(i)=inum
     if(inum.gt.0) then
        max_rzlim = max(max_rzlim,inum)
        inmax = max(inmax,inum)
        insel2=insel2 + 1
        isel2(insel2)=insel2
        inum2(insel2)=inum
        slbl2(insel2)=sss_label(i)

        have_lim(i)=insel2
        inum_contours(i) = inum_contours(i) + 1
     endif

     !  count contours of ICRF antennas
     inmax = max(inmax,sss(i)%max_nrz_antgeo)
     inum_contours(i) = inum_contours(i) + sss(i)%nicrf_src
  enddo

  if(insel.gt.0) then
     allocate(rbdy(nbdy,insel),zbdy(nbdy,insel))
     rbdy = 0
     zbdy = 0
  endif

  if(insel2.gt.0) then
     allocate(rlim(max_rzlim,insel2),zlim(max_rzlim,insel2))
     rlim = 0
     zlim = 0
  endif

  indx = 0
  indx2 = 0
  do i=1,nsss_cur
     idR = sss(i)%id_R_geo
     idZ = sss(i)%id_Z_geo
     if((idR.gt.0).and.(idZ.gt.0)) then
        indx = indx + 1

        call ps_intrp_2d(xbdy,thbdy,idR,rbdy(1:nbdy,indx),ierr, &
             state = sss(i))
        if(ierr.ne.0) then
           write(luntty,*) ' ?? ur_cstate_misc_plots: R(rho,theta) interpolation error.'
           insel = 0
           exit
        endif

        call ps_intrp_2d(xbdy,thbdy,idZ,zbdy(1:nbdy,indx),ierr, &
             state = sss(i))
        if(ierr.ne.0) then
           write(luntty,*) ' ?? ur_cstate_misc_plots: Z(rho,theta) interpolation error.'
           insel = 0
           exit
        endif

        ihash=17
        call hashCode(nbdy,ihash)
        call hashCode(Rbdy(1:nbdy,indx),ihash)
        call hashCode(Zbdy(1:nbdy,indx),ihash)
        bdy_hash(i)=ihash
     endif

     inum=sss(i)%num_rzlim
     if(inum.gt.0) then
        indx2 = indx2 + 1
        
        rlim(1:inum,indx2) = sss(i)%rlim(1:inum)
        zlim(1:inum,indx2) = sss(i)%zlim(1:inum)

        ihash=17
        call hashCode(inum,ihash)
        call hashCode(Rlim(1:inum,indx2),ihash)
        call hashCode(Zlim(1:inum,indx2),ihash)
        lim_hash(i)=ihash
     endif

     ns_hash(i) = -1
     if((allocated(sss(i)%ns)).AND.(allocated(sss(i)%qatom_all))) then
        insgot = insgot + 1
        ihash=17
        call hashCode(sss(i)%ns,ihash)
        if(allocated(sss(i)%nbeami)) then
           call hashCode(sss(i)%nbeami,ihash)
        endif
        if(allocated(sss(i)%nfusi)) then
           call hashCode(sss(i)%nfusi,ihash)
        endif
        if(allocated(sss(i)%nmini)) then
           call hashCode(sss(i)%nmini,ihash)
        endif
        ns_hash(i)=ihash
     endif
  enddo

  if(insel.gt.0) then
     call cstate_hash_compare('plasma boundaries',bdy_hash,bdy_hash)
  else
     write(luntty,*) ' '
     write(luntty,*) ' (none of the states contain plasma geometry w/boundary)'
  endif

  if(insel2.gt.0) then
     call cstate_hash_compare('limiters',lim_hash,lim_hash)
  else
     write(luntty,*) ' '
     write(luntty,*) ' (none of the states specify a limiter)'
  endif

  if(insgot.gt.0) then
     call cstate_hash_compare('densities',ns_hash,ns_hash)
  else
     write(luntty,*) ' '
     write(luntty,*) ' (none of the states contain thermal specie densities)'
  endif

  !------------------------
  !  interactive loop:

  do
     if(lxout(0)) then
        write(luntty,*) ' '
        write(luntty,*) 'ur_cstate_misc_plots options:'
        if(insel.gt.0) &
             write(luntty,*) ' B -- comparitive plot of boundaries.'
        if(insel2.gt.0) &
             write(luntty,*) ' L -- comparitive plot of axisymmetric limiters.'
        if(maxval(inum_contours).gt.0) &
             write(luntty,*) ' S -- plot boundary, limiter, and antennas for single state.'
        write(luntty,*) ' X -- "xplasma" plots of individual state.'
        write(luntty,*) ' N -- quasiNeutrality test, individual state.'
        write(luntty,*) ' Z -- Zeff test, individual state.'
        write(luntty,*) ' Q -- quit.'
        write(luntty,*) ' '
     endif

     call uread('ur_cstate_misc_plots option:')

     if(lchar('Q')) then
        exit

     else if(lchar('X')) then
        call ur_cstate_xplasma_plots

     else if(lchar('B')) then
        if(insel.eq.0) then
           call uermsg('ur_cstate_misc_plots: geometry data not available.', &
                ' ',0)
           cycle
        endif

        !  plot the boundaries...

        kylab = ' '
        kylab(1) = '  state'
        kylab(2) = 'identification'

        mnlab(2:3) = ' '
        mnlab(2) = 'plot of plasma boundaries'
        call ur_cstate_mnlab3(insel,nsss_cur-insel)

        call r8_grfmm1(Rbdy,Zbdy,nbdy,inums,insel,zndx,insel, &
             iself,insel,'m','m',mnlab,kylab,slbls)

     else if(lchar('L')) then
        if(insel2.eq.0) then
           call uermsg('ur_cstate_misc_plots: limiter data not available.', &
                ' ',0)
           cycle
        endif

        !  plot the boundaries...

        kylab = ' '
        kylab(1) = '  state'
        kylab(2) = 'identification'

        mnlab(2:3) = ' '
        mnlab(2) = 'plot of limiters'
        call ur_cstate_mnlab3(insel2,nsss_cur-insel2)

        call r8_grfmm1(Rlim,Zlim,max_rzlim,inum2,insel2,zndx,insel2, &
             isel2,insel2,'m','m',mnlab,kylab,slbl2)

     else if(lchar('S')) then
        if(maxval(inum_contours).eq.0) then
           call uermsg('ur_cstate_misc_plots: no single state plots available.',' ',0)
           cycle
        endif

        do
           if(lxout(0)) then
              write(luntty,*) ' '
              write(luntty,*) ' select single state: '
              do i=1,nsss_cur
                 if(inum_contours(i).eq.0) then
                    write(luntty,'(2x,i1," -- ",a," (plot not available).")') &
                         i,trim(sss_label(i))
                 else
                    write(luntty,'(2x,i1," -- ",a)') &
                         i,trim(sss_label(i))
                 endif
              enddo
              write(luntty,*) ' ... 0 or "Q" to exit this loop.'
           endif
           isel = 0
           call ureadi('enter 1 digit number to select state:',isel)
           if(isel.eq.0) exit

           incon=inum_contours(isel)
           if(incon.eq.0) then
              call uermsi('ur_cstate_misc_plots: no plot available for state',&
                   isel,0)
              cycle
           endif

           if(allocated(rcon)) deallocate(rcon,zcon)
           if(allocated(inumc)) deallocate(inumc,iselc,zndxc,slblc)

           allocate(rcon(inmax,incon),zcon(inmax,incon)); rcon=0.0; zcon=0.0
           allocate(inumc(incon),iselc(incon),zndxc(incon),slblc(incon))

           !  plot the contours

           kylab = ' '
           kylab(1) = '  contour'
           kylab(2) = 'identification'

           mnlab(2:3)=' '
           mnlab(2)='plot of boundary, limiter, and antenna locations'
           mnlab(3)='for state: '//trim(sss_label(isel))

           do i=1,incon
              iselc(i)=i
              zndxc(i)=i
           enddo

           ii=0

           if(have_geo(isel).gt.0) then
              ii=ii+1
              slblc(ii)='bdy'
              inumc(ii)=nbdy
              Rcon(1:nbdy,ii)=Rbdy(1:nbdy,have_geo(isel))
              Zcon(1:nbdy,ii)=Zbdy(1:nbdy,have_geo(isel))
           endif

           if(have_lim(isel).gt.0) then
              ii=ii+1
              ind=have_lim(isel)
              inumc(ii)=inum2(ind)
              slblc(ii)='limiter'
              Rcon(1:inumc(ii),ii)=Rlim(1:inum2(ind),ind)
              Zcon(1:inumc(ii),ii)=Zlim(1:inum2(ind),ind)
           endif

           do i=1,sss(isel)%nicrf_src
              if(sss(isel)%nrz_antgeo(i).gt.0) then
                 ii=ii+1
                 inumc(ii)=sss(isel)%nrz_antgeo(i)
                 slblc(ii)='Ant.'//trim(sss(isel)%icrf_src_name(i))
                 Rcon(1:inumc(ii),ii)=sss(isel)%R_antgeo(1:inumc(ii),i)
                 Zcon(1:inumc(ii),ii)=sss(isel)%Z_antgeo(1:inumc(ii),i)
              endif
           enddo

           call r8_grfmm1(Rcon,Zcon,inmax,inumc,ii,zndxc,ii, &
                iselc,ii,'m','m',mnlab,kylab,slblc)
        enddo

     else if(lchar('N')) then
        call choose1(isel)
        if(isel.eq.0) cycle

        call ur_cstate_quasi_check(isel)
        
     else if(lchar('Z')) then
        call choose1(isel)
        if(isel.eq.0) cycle

        call ur_cstate_Zeff_check(isel)
        
     else
        call uermsg('unrecognized option',' ',0)
     endif
  enddo

CONTAINS
  subroutine choose1(isel)

    integer, intent(out) :: isel

    !----------------
    integer :: iskip
    !----------------

    isel = 0

    if(lxout(0)) then
       write(luntty,*) ' '
       write(luntty,*) ' select single state: '
       do i=1,nsss_cur
          if(ns_hash(i).eq.-1) then
             write(luntty,'(2x,i1," -- ",a," (densities not available).")') &
                  i,trim(sss_label(i))
          else
             write(luntty,'(2x,i1," -- ",a)') &
                  i,trim(sss_label(i))
          endif
       enddo
       write(luntty,*) ' ... 0 or "Q" to exit.'
    endif

    iskip = 0

    call ureadi('enter 1 digit number to select state:',isel)
    if(ns_hash(isel).eq.-1) iskip = 1
    if(isel.lt.0) iskip = 1
    if(isel.gt.nsss_cur) iskip = 1

    if(iskip.gt.0) then
       call uermsi('%ur_cstate_misc_plots: no density data for state #',isel,0)
       isel=0
    endif

  end subroutine choose1

end subroutine ur_cstate_misc_plots
