program geq_plot

  !  uread/sglib program to look at an EFIT g-eqdisk file or MDS+ time slice
  !  directly, without using xplasma or Plasma State.  (Useful on the
  !  occasions where xplasma interpretation of g-eqdisk fails).  (This could
  !  occur due to problems with the g-eqdisk data, or perhaps with the
  !  xplasma software).

  !------------------------------
  use geqdsk_mod
  implicit NONE
  !------------------------------
  ! local:

  type (geqdsk) :: geq

  character*180 :: geq_path,lbl1,lbl2,lbl3,mnlab(3)
  character*17 :: kylab(5)
  character*10 :: lbsel(2)

  logical :: lxout,lchar
  integer :: lnout,nonlin

  integer :: ierr

  integer :: ic,ic1
  integer :: inpsi,ipsi,inbmax,nr,nz,irz
  real(geq_r8), dimension(:), allocatable :: psigeq,rgrid,zgrid,wk
  real(geq_r8), dimension(:,:), allocatable :: rplo,zplo

  real(geq_r8) :: zz(2),Zmin,Zmax
  integer :: ii(2),in(2)
  !------------------------------

  call uinitg('GEQ_PLOT')

  nonlin = lnout(0)  ! TTY LUN

  geq_path = "foo.geq"

  do

     ! ask user for path to GEQ data

     if(lxout(0)) then
        write(nonlin,*) ' '
        write(nonlin,*) ' A filename path or an MDS+ path can be entered here.'
        write(nonlin,*) '   MDS+ path syntax: '
        write(nonlin,*) &
             '     "MDS+:<server>[:port]:<tree_name>(<shot#>;t=<time>)"'
        write(nonlin,*) '   For example: '
        write(nonlin,*) &
             '     "MDS+:skylark.pppl.gov:8501:EFIT02(124901;t=0.25)"'
     endif

     !  get GEQ path

     call ureadLw('GEQ path, or "q" or "Q" to quit:',geq_path)
     if(geq_path.eq.'q') exit
     if(geq_path.eq.'Q') exit

     write(nonlin,*) 'GEQ path entered: '//trim(geq_path)

     !  read GEQ data

     call geq_init(geq, geq_path, ierr)
     if(ierr.ne.0) then
        call uermsi(' ?geq_init: GEQ data access unsuccessful, ierr',ierr,0)
        cycle
     else
        write(nonlin,*) 'GEQ data access OK.'
     endif

     !  print scalars

     call locgeq_stats

     !  construct 1d R Z and Psi grids

     inpsi = geq%nw
     if(allocated(psigeq)) deallocate(psigeq)
     allocate (psigeq(inpsi))
     do ipsi = 1,inpsi
        psigeq(ipsi) = ((inpsi-ipsi)*geq%simag_wb__rad + &
             (ipsi-1)*geq%sibry_wb__rad)/(inpsi-1)
     enddo

     nR = geq%nW
     allocate(Rgrid(nR))
     do irz=1,nR
        Rgrid(irZ) = &
             (geq%Rleft_m*(nR-irz) + (geq%Rleft_m+geq%Rdim_m)*(irz-1))/(nR-1)
     enddo

     nZ = geq%nh
     allocate(Zgrid(nZ))
     Zmin = geq%Zmid_m - 0.5d0*geq%Zdim_m
     Zmax = geq%Zmid_m + 0.5d0*geq%Zdim_m
     do irz=1,nZ
        Zgrid(irz) = (Zmin*(nZ-irz) + Zmax*(irz-1))/(nZ-1)
     enddo

     !  plotting workspace
     allocate(wk(max(nR,nZ)))

     !  some labeling
     lbl1 = 'geq_plot: G-eqdisk data display'

     ic1 = 1
     do ic=len(trim(geq_path)),1,-1
        if((geq_path(ic:ic).eq.':').OR.(geq_path(ic:ic).eq.'/')) then
           ic1=ic + 1
           exit
        endif
     enddo
     lbl3 = geq_path(ic1:)

     !  data for limiter & boundary contour plot

     inbmax = max( geq%limitr, geq%nbbbs)
     if(allocated(rplo)) deallocate(rplo,zplo)
     allocate(rplo(inbmax,2),zplo(inbmax,2)); rplo = 0.0d0; zplo = 0.0d0

     zz(1)=1
     zz(2)=2

     ii(1)=1
     ii(2)=2

     in(1)=geq%nbbbs
     in(2)=geq%limitr

     mnlab(1) = lbl1
     mnlab(2) = 'plasma boundary and limiter contours'
     mnlab(3) = lbl3

     kylab=' '

     lbsel(1)='boundary'
     lbsel(2)='limiter'

     rplo(1:geq%nbbbs,1) = geq%Rbbbs_m(1:geq%nbbbs)
     zplo(1:geq%nbbbs,1) = geq%Zbbbs_m(1:geq%nbbbs)

     rplo(1:geq%limitr,2) = geq%Rlim_m(1:geq%limitr)
     zplo(1:geq%limitr,2) = geq%Zlim_m(1:geq%limitr)

     ! interactive plotting/viewing...

     do
        if(lxout(0)) then
           write(nonlin,*) ' '
           write(nonlin,*) ' GEQ data viewing options: '
           write(nonlin,*) '  A -- print grid sizes and scalars'
           write(nonlin,*) '  B -- plot boundary contours'
           write(nonlin,*) '  Q -- quit (use I for q profile)'
           write(nonlin,*) '  F -- plot f(psi) = R*B_phi'
           write(nonlin,*) '  G -- plot f*fprime'
           write(nonlin,*) '  I -- plot q(psi)'
           write(nonlin,*) '  H -- plot P(psi)'
           write(nonlin,*) '  K -- plot Pprime'
           write(nonlin,*) '  P -- plot Psi(R,Z)'
        endif

        call uread('GEQ data viewing option:')
        if(lchar('Q')) then
           exit

        else if(lchar('A')) then
           !  print scalars
           call locgeq_stats

        else if(lchar('B')) then
           !  plot boundary and limiter contours
           call r8_grfmm1(rplo,zplo,inbmax,in,2,zz,2,ii,2, &
                'm','m',mnlab,kylab,lbsel)

        else if(lchar('F')) then
           lbl2 = 'Fpol (R*B_phi) vs. Psi'
           call r8_grafx1(psigeq,geq%fpol_tm,inpsi, &
                'Wb/rad','T*m', &
                lbl1,lbl2,lbl3)

        else if(lchar('G')) then
           lbl2 = 'FFprime (Fpol*(dFpol/dPsi)) vs. Psi'
           call r8_grafx1(psigeq,geq%ffprim_t2m2rad__wb,inpsi, &
                'Wb/rad','T^2*m^2*rad/Wb', &
                lbl1,lbl2,lbl3)

        else if(lchar('I')) then
           lbl2 = 'q vs. Psi'
           call r8_grafx1(psigeq,geq%qpsi,inpsi, &
                'Wb/rad','-', &
                lbl1,lbl2,lbl3)

        else if(lchar('H')) then
           lbl2 = 'Pressure (P) vs. Psi'
           call r8_grafx1(psigeq,geq%pres_nt__m2,inpsi, &
                'Wb/rad','Nt/m^2', &
                lbl1,lbl2,lbl3)

        else if(lchar('K')) then
           lbl2 = 'Pprime (dP/dpsi) vs. Psi'
           call r8_grafx1(psigeq,geq%pprime_ntRad__m2Wb,inpsi, &
                'Wb/rad','Nt*rad/Wb/m^2', &
                lbl1,lbl2,lbl3)

        else if(lchar('P')) then
           ! Psi(R,Z)
           call r8_grf3f1(Rgrid,geq%PsiRZ_Wb__Rad,Zgrid,wk,nR,nZ,nR, &
                'R','Psi(R,Z)','Z', &
                'm','Wb/rad','m', &
                'geq_plot',lbl3,0)

        else
           call uermsg(' ?geq_plot: unrecognized data viewing option.',' ',0)
        endif
     enddo

  enddo

CONTAINS

  subroutine locgeq_stats

    ! print scalars

    write(nonlin,*) ' '
    write(nonlin,*) 'GEQ comment field: '//GEQ%CASE_(1)//GEQ%CASE_(2)// &
         GEQ%CASE_(3)//GEQ%CASE_(4)//GEQ%CASE_(5)//GEQ%CASE_(6)
    write(nonlin,*) ' '
    write(nonlin,*) 'GEQ grid and contour sizes:'
    write(nonlin,'(3x,a,i4)') ' NW: ',geq%nw
    write(nonlin,'(3x,a,i4)') ' NH: ',geq%nh
    write(nonlin,'(3x,a,i4)') ' #pts in boundary contour: ',geq%nbbbs
    write(nonlin,'(3x,a,i4)') ' #pts in limiter contour:  ',geq%limitr
    write(nonlin,*) ' '
    write(nonlin,*) 'GEQ scalar data:'
    write(nonlin,'(3x,a,1pe12.5)') ' box width (m):   ',geq%Rdim_m
    write(nonlin,'(3x,a,1pe12.5)') ' box LHS (m):     ',geq%Rleft_m
    write(nonlin,'(3x,a,1pe12.5)') ' box height (m):  ',geq%Zdim_m
    write(nonlin,'(3x,a,1pe12.5)') ' box Z midpt (m): ',geq%Zmid_m
    write(nonlin,'(3x,a,1pe12.5)') ' R_axis (m):      ',geq%Rmaxis_m
    write(nonlin,'(3x,a,1pe12.5)') ' Z_axis (m):      ',geq%Zmaxis_m
    write(nonlin,'(3x,a,1pe12.5)') ' vacuum R*Bphi (T*m): ', &
         geq%Rcentr_m*geq%Bcentr_T
    write(nonlin,'(3x,a,1pe12.5)') ' plasma cur (A):  ',geq%current_A
    write(nonlin,'(3x,a,1pe12.5)') ' Psi(0) (Wb/rad): ',geq%SIMAG_Wb__Rad
    write(nonlin,'(3x,a,1pe12.5)') ' Psi(a) (Wb/rad): ',geq%SIBRY_Wb__Rad
    write(nonlin,*) ' '

  end subroutine locgeq_stats
     
end program geq_plot
