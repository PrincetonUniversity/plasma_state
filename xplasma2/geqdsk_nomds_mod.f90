module geqdsk_nomds_mod
!
! package geqdsk data
! pletzer@ pppl.gov Mon Jul 31 16:04:04 EDT 2000
!
! Refer to http://lithos.gat.com/efit/g_eqdsk.html for a detailed
! description of the data contained in g-eqdsk files. The module
! herein follows the terminology described in the above document; variables
! bear the same name with their units trailing after a underscore (_). A
! double underscore (__) stands for /, as in Wb__Rad (=Weber/rads).
 
  implicit NONE

  integer, parameter :: geq_r8 = selected_real_kind(12,100)
  real(geq_r8), parameter :: geq_pi = 3.141592653589793116
  real(geq_r8), parameter :: geq_2pi = 6.2831853071795862320
  real(geq_r8), parameter :: geq_mu0 = 1.2566370614359172885e-06
 
  type geqdsk
 
 
     integer  :: NW ! Number of horizontal R grid points
     integer  :: NH ! Number of vertical Z grid points
     character*8 :: CASE_(6)
     real(geq_r8) :: RDIM_M, ZDIM_M ! box dimensions
     real(geq_r8) :: RCENTR_M ! Major radius (not magnetic axis!)
     real(geq_r8) :: RLEFT_M ! left side of computational box
     real(geq_r8) :: ZMID_M ! mid elevation of computational box
     real(geq_r8) :: RMAXIS_M, ZMAXIS_M ! Magnetic axis position
     real(geq_r8) :: BCENTR_T, CURRENT_A
     ! poloidal flux/2*pi on magnetic axis and plasma boundary
     real(geq_r8) :: SIMAG_Wb__Rad, SIBRY_Wb__Rad
     real(geq_r8), dimension(:), allocatable :: FPOL_TM ! toroidal current
     real(geq_r8), dimension(:), allocatable :: PRES_Nt__M2 ! pressure
     real(geq_r8), dimension(:), allocatable :: FFPRIM_T2M2Rad__Wb ! F F'(psi)
     real(geq_r8), dimension(:), allocatable :: pprime_NtRad__M2Wb ! p'(psi)
     real(geq_r8), dimension(:,:), allocatable :: psirz_Wb__Rad
     real(geq_r8), dimension(:), allocatable :: QPSI
     integer  :: nbbbs ! Number of plasma-vacuum boundary points
     integer  :: limitr ! Number of limiter points
     real(geq_r8), dimension(:), allocatable :: RBBBS_M, ZBBBS_M ! Plasma-vacuum
     real(geq_r8), dimension(:), allocatable :: RLIM_M, ZLIM_M ! Limiter geometry
 
!  the "original" timebase contains some points for which there is no valid
!    data, and these are excluded from the "actual" timebase.  T_INDEX
!    indexes from the "actual" to the "orginal" time dimension.
 
     integer, dimension(:), allocatable :: T_NW
     integer, dimension(:), allocatable :: T_NH
 
     integer :: kt_max  ! max index for T_RDIM,T_ZDIM ....
 
     integer :: kt_sw2  ! storage ordering for profiles f(psi,t)
     integer :: kt_sw3  ! storage ordering for profile psi(R,Z,t)
 
! affected quantities:  T_NW,T_NH,T_RDIM,T_ZDIM,T_RCENTR,T_RLEFT,T_CMID
!
! kt_sw2,kt_sw3:
! (for some EFIT trees, profiles are stored psi-contiguous; and in
! others, t-contiguous; the code has to adapt)
 
     real(geq_r8), dimension(:), allocatable :: T_RDIM,T_ZDIM
     real(geq_r8), dimension(:), allocatable :: T_RCENTR
     real(geq_r8), dimension(:), allocatable :: T_RLEFT
     real(geq_r8), dimension(:), allocatable :: T_ZMID
     real(geq_r8), dimension(:), allocatable :: T_RMAXIS, T_ZMAXIS
     real(geq_r8), dimension(:), allocatable :: T_BCENTR, T_CURRENT
     real(geq_r8), dimension(:), allocatable :: T_SIMAG, T_SIBRY
 
     real(geq_r8), dimension(:,:), allocatable :: T_FPOL
     real(geq_r8), dimension(:,:), allocatable :: T_PRES
     real(geq_r8), dimension(:,:), allocatable :: T_FFPRIM
     real(geq_r8), dimension(:,:), allocatable :: T_PPRIME
     real(geq_r8), dimension(:,:), allocatable :: T_QPSI
 
     real(geq_r8), dimension(:,:,:), allocatable :: T_PSIRZ
 
     integer, dimension(:), allocatable :: T_NBBBS
     real(geq_r8), dimension(:,:), allocatable :: T_RBBBS,T_ZBBBS
 
!    the "time dependent" limiter data is not read; the limiter is assumed
!    not to move.
 
  end type geqdsk
 
! static items:
 
contains
 
  subroutine geq_xz_is_in_plasma(geq, x, z, iok, reltol)
 
    ! Return iok=0 if (x,z) is within plasma boundary
 
    ! Method: integrate over the plasma boundary,
    ! the angle between the point (x,z)
    ! and a segment of the boundary. If the result is
    ! nearly 2*pi then the point must be inside, if
    ! it's close to zero it must be outside.
 
    ! (dmc) also compute distance from (x,z) to the nearest point
    ! of approach to the boundary (treated as a sequence of line
    ! segments), normalized to the distance of the boundary point
    ! furthest from (x,z)
 
    ! iok = 0 => point inside
    ! iok = 1 => point ouside
    ! iok = 2 => can't tell for sure
 
    ! reltol
 
    implicit none
    type(geqdsk), intent(in) :: geq
    real(geq_r8), intent(in) :: x, z
    integer, intent(out) :: iok
    real(geq_r8), intent(out) :: reltol
 
    real(geq_r8) angle, dotprod, crsprod, x1, x2, z1, z2, tol
    real(geq_r8) distmin2,distmax2,dist2,dist12sq,alpha,beta
    real(geq_r8), parameter :: ZERO = 0.0d0
    real(geq_r8), parameter :: ONE  = 1.0d0
 
    integer i
 
    !------------------------------------------
 
    iok = 1
    reltol = 1000.0_geq_r8
 
    angle = 0._geq_r8
 
    x1=geq%RBBBS_M(geq%NBBBS)
    z1=geq%ZBBBS_M(geq%NBBBS)
 
    ! reference distance:  plasma bdy:  min((Rmax-Rmin),(Zmax-Zmin))
    distmax2=min( &
         maxval(geq%RBBBS_M(1:geq%NBBBS))-minval(geq%RBBBS_M(1:geq%NBBBS)), &
         maxval(geq%ZBBBS_M(1:geq%NBBBS))-minval(geq%ZBBBS_M(1:geq%NBBBS)) )
    distmax2=distmax2*distmax2  ! ref. dist. squared
 
    if(distmax2.eq.0._geq_r8) return   ! assume outside -- degenerate case.
 
    dist2=(x-x1)*(x-x1)+(z-z1)*(z-z1)
    distmin2=dist2
 
    do i = 1, geq%NBBBS - 1
 
       x1 = geq%RBBBS_M(i  ); z1 = geq%ZBBBS_M(i  )
       x2 = geq%RBBBS_M(i+1); z2 = geq%ZBBBS_M(i+1)
 
       dist2=(x-x1)*(x-x1)+(z-z1)*(z-z1)
       distmin2=min(distmin2,dist2)  ! find distance^2 to closest bdy pt
 
       dotprod = (x2-x)*(x1-x) + (z2-z)*(z1-z)
       crsProd = (x2-x)*(z1-z) - (z2-z)*(x1-x)
       dist12sq = (x2-x1)*(x2-x1)+(z2-z1)*(z2-z1)
       !  skip coincident points
       if((crsProd.eq.ZERO).or.(dist12sq.le.ZERO)) cycle
 
       angle = angle + abs(atan2(crsProd, dotprod))
 
       !  refine distance estimate by considering nearest approach to
       !  line segment itself, not just the end pts.
       !     P=(x,z)  P1=(x1,z1)  P2=(x2,z2)  DEL=(P2-P1)
       !     DELperp = vector perpendicular to DEL and of same length
       !
       !  bdy segment is represented by Pseg = P1 + alpha*DEL
       !     with alpha in [0,1]
       !
       !  line from P normal to Pseg is Plin = P + beta*DELperp
       !     and the intersection is found by setting Plin=Pseg and
       !     solving the 2x2 system for alpha & beta...
       !     if alpha in [0,1] then P is closer to the segment than
       !     to the segment end points, and the distance^2 is
       !     abs(beta)*|DEL|.
 
       alpha=-( (x2-x1)*(x1-x)+(z2-z1)*(z1-z) )/dist12sq
       beta = ( (x2-x1)*(z1-z)-(z2-z1)*(x1-x) )/dist12sq
 
       !debug  write(6,*) x1+alpha*(x2-x1), z1+alpha*(z2-z1)
       !debug  write(6,*) x -beta*(z2-z1),  z +beta*(x2-x1)
 
       if((alpha.ge.ZERO).and.(alpha.le.ONE)) then
          distmin2=min(distmin2,(dist12sq*beta*beta))
       endif
 
    enddo
 
 
    ! may need to adjust this tol
    tol = 0.01_geq_r8 * (2._geq_r8 * geq_pi)
 
    iok = 2
    if(abs(angle - geq_2pi) < tol) iok = 0
    if(abs(angle          ) < tol) iok = 1
 
    reltol=sqrt(distmin2/distmax2)
 
  end subroutine geq_xz_is_in_plasma
 
!------------------------------------------------------
! compute GS error
 
  subroutine geq_GSerror(isign, geq, nx1, nz1, x, z, bdist, gserror, ier)
 
    ! Compute rel. Grad Shafranov error *inside* plasma boundary
    ! (error is set to zero outside) (also, points "too close" to
    ! the boundary have error set to zero, see "bdist" control).
    !   ier=1 is set if no points are found (which generally indicates a
    !   very bad input dataset).
 
    use ezspline_obj
    use ezspline
    use geqdsk_nomds_aux

    implicit none
 
    integer, intent(in)      :: isign    ! +1 for EFIT G-eqdsk

    ! if Psi(a) < Psi(0), isign = -1 -- never true for EFIT but it
    !  is true for some imitators...

    type(geqdsk), intent(in) :: geq      ! G-eqdsk object
    integer, intent(in)      :: nx1, nz1 ! X,Z grids...
    real(geq_r8), intent(in) :: x(nx1), z(nz1)
 
    real(geq_r8), intent(in) :: bdist    ! min. relative distance to bdy
 
    real(geq_r8), intent(out) :: gserror(nx1, nz1)
    integer, intent(out) :: ier
 
    !  points within bdist*min((Rmax-Rmin),(Zmax-Zmin)) of the boundary
    !  have their error set to zero.  {Rmin,Rmax,Zmin,Zmax} are taken
    !  from the G-Eqdsk plasma boundary data.
 
    type(ezspline2) :: psispl
    type(ezspline1) :: ppspl, ggpspl
    real(geq_r8), dimension(:,:), allocatable :: d2xPsi, dxPsi, d2zPsi
    real(geq_r8), dimension(:), allocatable :: zwk
    real(geq_r8) :: pprime, ggprime, psi, r_jphi0
    real(geq_r8) :: dist_norm
    real(geq_r8) :: loc_psimin,loc_psimax
    integer iok, i, j, idum1, idum2, ict, ictbb
    character*30 zlabel       ! (for debugging)
 
    iok = 0
    ier = 0
 
    ! compute del^* psi
 
    call ezspline_init(psispl, geq%NW, geq%NH, (/0,0/), (/0,0/), iok)
    if(iok/=0) call ezspline_error(iok)
 
    psispl%x1 = (/ &
         & ( geq%RLEFT_M + &
         & geq%RDIM_M*real(i-1,geq_r8)/real(geq%NW-1, geq_r8), &
         & i = 1, geq%NW)/)
    psispl%x2 = (/ &
         & ( geq%ZMID_M - geq%ZDIM_M/2._geq_r8 + &
         & geq%ZDIM_M*real(i-1,geq_r8)/real(geq%NH-1, geq_r8), &
         & i = 1, geq%NH)/)
 
    call ezspline_setup(psispl, isign*geq%psirz_Wb__Rad, iok)
    if(iok/=0) call ezspline_error(iok)
 
    allocate(d2xPsi(nx1, nz1), stat=iok)
    allocate(dxPsi(nx1, nz1), stat=iok)
    allocate(d2zPsi(nx1, nz1), stat=iok)
 
    call ezspline_derivative(psispl, 1, 0, nx1, nz1, x, z, dxPsi, iok)
    if(iok/=0) call ezspline_error(iok)
 
    call ezspline_derivative(psispl, 2, 0, nx1, nz1, x, z, d2xPsi, iok)
    if(iok/=0) call ezspline_error(iok)
 
    call ezspline_derivative(psispl, 0, 2, nx1, nz1, x, z, d2zPsi, iok)
    if(iok/=0) call ezspline_error(iok)
 
    gserror = 0._geq_r8 ! d2xPsi - dxPsi/x + d2zPsi ! so far only del^* psi
 
    ! compute R^2 p' + g g'
 
    call ezspline_init(ppspl, geq%NW, (/0,0/), iok)
    if(iok/=0) call ezspline_error(iok)
 
    ppspl%x1 = (/ (  geq%SIMAG_Wb__Rad + (geq%SIBRY_Wb__Rad-geq%SIMAG_Wb__Rad) * &
         & real(i-1,geq_r8)/real(geq%NW-1), i = 1, geq%NW) /)
    ppspl%x1 = isign*ppspl%x1
 
    call ezspline_setup(ppspl, isign*geq_mu0*geq%pprime_NtRad__M2Wb, iok)
    if(iok/=0) call ezspline_error(iok)
 
    call ezspline_init(ggpspl, geq%NW, (/0,0/), iok)
    if(iok/=0) call ezspline_error(iok)
 
    ggpspl%x1 = ppspl%x1
 
    call ezspline_setup(ggpspl, isign*geq%FFPRIM_T2M2Rad__Wb, iok)
    if(iok/=0) call ezspline_error(iok)
 
    ! normalization = "nominal r_jphi"
    r_jphi0 = maxval(abs( &
         geq%RMAXIS_M**2 * geq_mu0*geq%pprime_NtRad__M2Wb(1:geq%NW) + &
         geq%FFPRIM_T2M2Rad__Wb(1:geq%NW) ))
    r_jphi0 = isign * r_jphi0
 
    ict = 0
    ictbb = 0
    do j = 1, nz1
       do i = 1, nx1
          ! get psi
          call ezspline_interp(psispl, x(i), z(j), psi, iok)

          if((i*j).eq.1) then
             loc_psimin=psi
             loc_psimax=psi
          else
             loc_psimin=min(loc_psimin,psi)
             loc_psimax=max(loc_psimax,psi)
          endif

          if( (psi>isign*geq%SIMAG_Wb__Rad .and. psi<isign*geq%SIBRY_Wb__Rad)  .or.  &
              (psi>isign*geq%SIBRY_Wb__Rad .and. psi<isign*geq%SIMAG_Wb__Rad) ) then
             ictbb = ictbb + 1

             ! most likely inside the plasma
             iok = 1 ! outside by default
             call geq_xz_is_in_plasma(geq, x(i), z(j), iok, dist_norm)
             if(iok==0 .and. dist_norm > bdist ) then
                ! definitely inside the plasma
                ict = ict + 1
                call ezspline_interp(ppspl, psi, pprime, iok)
                call ezspline_interp(ggpspl, psi, ggprime, iok)
                gserror(i,j) = (d2xPsi(i,j) - dxPsi(i,j)/x(i) + d2zPsi(i,j) &
                     & + x(i)**2 * pprime + ggprime)/r_jphi0
             endif
          endif
       enddo
    enddo
 
    call ezspline_free(psispl, iok)
    if(iok/=0) call ezspline_error(iok)
    call ezspline_free(ppspl, iok)
    if(iok/=0) call ezspline_error(iok)
    call ezspline_free(ggpspl, iok)
    if(iok/=0) call ezspline_error(iok)
 
    deallocate(d2xPsi)
    deallocate(dxPsi)
    deallocate(d2zPsi)
 
    if(ict.eq.0) then
       ier=1
       if(isign.ne.1) then
          write(lunmsg,*) ' %geq_GSerror: EQDSK format variant detected.'
       endif

       if(ictbb.eq.0) then
          write(lunmsg,*) ' ?geq_GSerror: no Psi(R,Z) values in range:'
          write(lunmsg,*) '   (Psi values expected to be in Wb/rad)'
          write(lunmsg,*) '   Psi(axis) data: ',isign*geq%SIMAG_Wb__Rad
          write(lunmsg,*) '   Psi(bdy) data:  ',isign*geq%SIBRY_Wb__Rad
          write(lunmsg,*) '   min value in Psi(R,Z): ',loc_psimin
          write(lunmsg,*) '   max value in Psi(R,Z): ',loc_psimax
       else
          write(lunmsg,*) &
               ' ?geq_GSerror: valid Psi(R,Z) values not found inside bdy'
       endif
    endif

  end subroutine geq_GSerror
 
 !------------------------------------------------------
! set LUN for G-EQdisk file i/o
 
  subroutine geq_setlun(ilun)
 
    use geqdsk_nomds_aux
 
    implicit none
    integer, intent(in) :: ilun    ! unit number
 
    neqdsk=ilun
 
    return
  end subroutine geq_setlun
 
!------------------------------------------------------
! retrieve LUN for G-EQdisk file i/o
 
  subroutine geq_getlun(ilun)
 
    use geqdsk_nomds_aux
 
    implicit none
    integer, intent(out) :: ilun    ! unit number
 
    ilun=neqdsk
 
    return
  end subroutine geq_getlun
 
!------------------------------------------------------
  subroutine geq_writ(geq, filename, ier)
!
! ...write a G-EQDSK file from a stored objec...
!
    use geqdsk_nomds_aux
 
    implicit none
    type(geqdsk), intent(in) :: geq
    character*(*), intent(in) :: filename
    integer, intent(out) :: ier
 
! local stuff
 
    integer iok
    integer :: idum = 0
    real(geq_r8) :: adum = 0.0_geq_r8
    integer i,j
 
! executable code
 
    ier = 0
 
    open(neqdsk, file=filename, status='unknown', action='write', iostat=iok)
 
    if (iok /= 0) then
       write(lunmsg,*) ' %geq_writ: open failure: ',filename
       ier = 1
       return
    endif
 
    write (neqdsk,2000) (geq%case_(i),i=1,6),idum,geq%nw,geq%nh
    write (neqdsk,2020) geq%rdim_m, geq%zdim_m, &
         & geq%rcentr_m, geq%rleft_m, geq%zmid_m
    write (neqdsk,2020) geq%rmaxis_m,geq%zmaxis_m, &
         & geq%simag_Wb__Rad,geq%sibry_Wb__Rad,geq%bcentr_t
    write (neqdsk,2020) geq%current_a,geq%simag_Wb__Rad, &
         & adum,geq%rmaxis_m,adum
    write (neqdsk,2020) geq%zmaxis_m,adum,geq%sibry_Wb__Rad,adum,adum
 
    write (neqdsk,2020) (geq%fpol_tm(i),i=1,geq%nw)
    write (neqdsk,2020) (geq%pres_Nt__M2(i),i=1,geq%nw)
    write (neqdsk,2020) (geq%ffprim_T2M2Rad__Wb(i),i=1,geq%nw)
    write (neqdsk,2020) (geq%pprime_NtRad__M2Wb(i),i=1,geq%nw)
    write (neqdsk,2020) ((geq%psirz_Wb__Rad(i,j),i=1,geq%nw),j=1,geq%nh)
    write (neqdsk,2020) (geq%qpsi(i),i=1,geq%nw)
    write (neqdsk,2022) geq%nbbbs,geq%limitr
 
    write (neqdsk,2020) (geq%rbbbs_m(i),geq%zbbbs_m(i),i=1,geq%nbbbs)
 
    write (neqdsk,2020) (geq%rlim_m(i),geq%zlim_m(i),i=1,geq%limitr)
 
    close(neqdsk)
 
2000 format (6a8,3i4)
2020 format (5e16.9)
2022 format (2i5)
 
  end subroutine geq_writ
!------------------------------------------------------
! initialize a G-EQdisk object from file or MDS+ data
 
  subroutine geq_init(geq, filenami, ier)
 
    use geqdsk_nomds_aux
 
    implicit none
    type(geqdsk), intent(inout) :: geq
    character*(*), intent(in) :: filenami
    integer, intent(out) :: ier
 
! local stuff
 
    character*200 filename,zpath,ztail
    integer iok, i, j, idum, istat, indx, ilast
    character*4 mtest
    real(geq_r8) adum,zsimag
 
! local, for MDSplus support
 
    logical ireduce
    integer icln,ict,ilen,ishot,iopen
    character*10 opt
    character*11 itest
    character*50 mds_server
    character*20 mds_tree
    character*10 mds_id1
    character*10 mds_id2
    character*20 mds_tstr,z20
    real(geq_r8) ztime
 
    character*48 tmplbl
    integer iltmp,iltmp2
 
!-------------------------------------
! executable code
 
    ier = 0

! dmc modification -- translate EFIT_INDEX file specification if given
    itest=filenami(1:11)
    call uupper(itest)
    if(itest.eq.'EFIT_INDEX:') then
       call geqdsk_findex(filenami,ilast,indx)
       call geqdsk_index1_path(neqdsk,filenami(1:ilast),zpath)
       if(zpath.eq.'?') then
          write(lunmsg,*) ' ?geqdsk_mod.geq_init: index file access failure.'
          ier=1
       else
          call geqdsk_index1_gfile(neqdsk,filenami(1:ilast),indx,ztail,ier)
          if(zpath.eq.' ') then
             filename=trim(ztail)
          else
             filename=trim(zpath)//trim(ztail)
          endif
       endif
    else
       filename=filenami
    endif
    if(ier.ne.0) return
 
    mtest=filename(1:4)
    call uupper(mtest)
 
    iopen = 0                ! MDS+ tree open flag
 
    if(mtest.ne.'MDS+') then
 
! prevent memory leaks, in case where G-EQdisk object is reused.
 
       call geq_free(geq,ier)
 
!  ********* file access *************
 
       open(neqdsk, file=filename, action='read', status='old', iostat=iok)
 
       if (iok /= 0) then
          ier = 1
          go to 1001
       endif
  
       read (neqdsk,2000) (geq%case_(i),i=1,6),idum,geq%nw,geq%nh
!dbg       write(6,*) geq%nw,geq%nh
       read (neqdsk,2020) geq%rdim_m, geq%zdim_m, &
            & geq%rcentr_m, geq%rleft_m, geq%zmid_m
!dbg       write(6,*) geq%rdim_m,geq%zdim_m
       read (neqdsk,2020) geq%rmaxis_m,geq%zmaxis_m, &
            & geq%simag_Wb__Rad,geq%sibry_Wb__Rad,geq%bcentr_t
!dbg       write(6,*) geq%rmaxis_m,geq%zmaxis_m
       read (neqdsk,2020) geq%current_a,zsimag, &
            & adum,geq%rmaxis_m,adum
!dbg       write(6,*) geq%current_a
       read (neqdsk,2020) geq%zmaxis_m,adum,geq%sibry_Wb__Rad,adum,adum
!dbg       write(6,*) geq%sibry_Wb__Rad
 
       if(zsimag.ne.geq%simag_Wb__Rad) then
          write(lunmsg,*) &
               'G-eqdsk file warning: simag inconsistency, 1st value taken.'
          write(lunmsg,*) '1st simag(3rd line, 3rd word)=',geq%simag_Wb__Rad
          write(lunmsg,*) '2nd simag(4th line, 2nd word)=',zsimag
       endif
 
       allocate(geq%fpol_tm(geq%nw), stat=iok)
       if(iok /= 0) then
          ier = 2
          go to 1000
       endif
       allocate(geq%pres_Nt__M2(geq%nw), stat=iok)
       if(iok /= 0) then
          ier = 2
          go to 1000
       endif
       allocate(geq%ffprim_T2M2Rad__Wb(geq%nw), stat=iok)
       if(iok /= 0) then
          ier = 2
          go to 1000
       endif
       allocate(geq%pprime_NtRad__M2Wb(geq%nw), stat=iok)
       if(iok /= 0) then
          ier = 2
          go to 1000
       endif
       allocate(geq%qpsi(geq%nw), stat=iok)
       if(iok /= 0) then
          ier = 2
          go to 1000
       endif
 
       allocate(geq%psirz_Wb__Rad(geq%nw, geq%nh), stat=iok)
       if(iok /= 0) then
          ier = 2
          go to 1000
       endif
 
       read (neqdsk,2020) (geq%fpol_tm(i),i=1,geq%nw)
!dbg       write(6,*) 'fpol',(geq%fpol_tm(i),i=1,geq%nw)
       read (neqdsk,2020) (geq%pres_Nt__M2(i),i=1,geq%nw)
!dbg       write(6,*) 'pres',(geq%pres_Nt__M2(i),i=1,geq%nw)
       read (neqdsk,2020) (geq%ffprim_T2M2Rad__Wb(i),i=1,geq%nw)
       read (neqdsk,2020) (geq%pprime_NtRad__M2Wb(i),i=1,geq%nw)
       read (neqdsk,2020) ((geq%psirz_Wb__Rad(i,j),i=1,geq%nw),j=1,geq%nh)
       read (neqdsk,2020) (geq%qpsi(i),i=1,geq%nw)
!dbg       write(6,*) 'qpsi',(geq%qpsi(i),i=1,geq%nw)
       read (neqdsk,2022) geq%nbbbs,geq%limitr
 
       allocate(geq%rbbbs_m(geq%nbbbs+1), geq%zbbbs_m(geq%nbbbs+1), stat=iok)
       if(iok /= 0) then
          ier = 2
          go to 1000
       endif
       allocate(geq%rlim_m(geq%limitr+1), geq%zlim_m(geq%limitr+1), stat=iok)
       if(iok /= 0) then
          ier = 2
          go to 1000
       endif
       geq%rbbbs_m=0
       geq%zbbbs_m=0
 
       read (neqdsk,2020) (geq%rbbbs_m(i),geq%zbbbs_m(i),i=1,geq%nbbbs)
!dbg       write(6,*) 'r,zbbbs', (geq%rbbbs_m(i),geq%zbbbs_m(i),i=1,geq%nbbbs)
       call geq_ckrzc(geq%rbbbs_m(1),geq%zbbbs_m(1),geq%nbbbs,geq%nbbbs+1)
!dbg       write(6,*) 'r,z(ck)', (geq%rbbbs_m(i),geq%zbbbs_m(i),i=1,geq%nbbbs)
 
       read (neqdsk,2020) (geq%rlim_m(i),geq%zlim_m(i),i=1,geq%limitr)
!dbg       write(6,*) 'r,zlim', (geq%rlim_m(i),geq%zlim_m(i),i=1,geq%limitr)
       call geq_ckrzc(geq%rlim_m(1),geq%zlim_m(1),geq%limitr,geq%limitr+1)
 
      ! rotation data not extracted....
 
       close(neqdsk)
 
2000   format (6a8,3i4)
2020   format (5e16.9)
2022   format (2i5)
 
    else
       write(lunmsg,*) ' ?MDS+ not supported'
       call ps_bad_exit(' ?geqdsk__nomds_mod -- error')
    endif
 
1000 continue
 
    ! check that bdy is entirely contained inside the grid domain--
    ! flag an error if not.

    if (ier==0) then
       call geq_bchk(geq, 'plasma_bdy(RBBBS_m,ZBBBS_m)', &
            geq%nbbbs, geq%rbbbs_m, geq%zbbbs_m, ier)
    end if

    return
 
1001 continue
 
    ! failed to open the geqdsk file
 
    write(lunmsg,*) ' ?geq_init: file ', filename,' does not exist'
    return
 
  end subroutine geq_init
  subroutine geq_mergbb(rbout,zbout, &
       zf1,inb1,rb1,zb1, zf2,inb2,rb2,zb2)

    real(geq_r8), dimension(:) :: rbout,zbout  ! output contour

    real(geq_r8), intent(in) :: zf1   ! weighting for 1st input contour
    integer, intent(in) :: inb1       ! 1st input contour array dimension
    real(geq_r8), intent(in) :: rb1(inb1),zb1(inb1)  ! 1st contour

    real(geq_r8), intent(in) :: zf2   ! weighting for 2nd input contour
    integer, intent(in) :: inb2       ! 2nd input contour array dimension
    real(geq_r8), intent(in) :: rb2(inb2),zb2(inb2)  ! 2nd contour

    !--------------------------------
    ! local:

    integer :: i,n1,n2,n1a,n2a,nbout
    real(geq_r8), dimension(:), allocatable :: r1,z1,r2,z2
    real(geq_r8), dimension(:), allocatable :: a1,d1,a2,d2
    real(geq_r8) :: rcen1,zcen1,rcen2,zcen2,rcen,zcen
    real(geq_r8) :: rmaxwk,rminwk
    real(geq_r8) :: zdamin,zalim,ztha,dfind1,dfind2,dfind
    !--------------------------------

    nbout=size(rbout)
    zdamin=geq_2pi/(10*nbout-1)

    allocate(r1(inb1+1),z1(inb1+1))
    allocate(r2(inb2+1),z2(inb2+1))

    r1(1:inb1)=rb1; r1(inb1+1)=0.0_geq_r8
    z1(1:inb1)=zb1; z1(inb1+1)=0.0_geq_r8

    r2(1:inb2)=rb2; r2(inb2+1)=0.0_geq_r8
    z2(1:inb2)=zb2; z2(inb2+1)=0.0_geq_r8

    n1=inb1
    n2=inb2

    !  make sure contours are closed with duplicate points removed...

    call geq_ckrzc(r1,z1,n1,inb1+1)
    call geq_ckrzc(r2,z2,n2,inb2+1)

    rmaxwk=maxval(r1(1:n1))
    rminwk=minval(r1(1:n1))

    rcen1=0.55_geq_r8*rmaxwk + 0.45_geq_r8*rminwk  ! shift out 10%
    zcen1=0.5_geq_r8*(maxval(z1(1:n1))+minval(z1(1:n1)))

    rmaxwk=maxval(r2(1:n2))
    rminwk=minval(r2(1:n2))

    rcen2=0.55_geq_r8*rmaxwk + 0.45_geq_r8*rminwk  ! shift out 10%
    zcen2=0.5_geq_r8*(maxval(z2(1:n2))+minval(z2(1:n2)))

    rcen = zf1*rcen1 + zf2*rcen2
    zcen = zf1*zcen1 + zf2*zcen2

    allocate(a1(n1),d1(n1),a2(n2),d2(n2))

    call geq_aseq(rcen1,zcen1,r1,z1,n1,zdamin,a1,d1,n1a)
    call geq_aseq(rcen2,zcen2,r2,z2,n2,zdamin,a2,d2,n2a)

    do i=1,nbout
       ztha=geq_2pi/2 - (i-1)*geq_2pi/(nbout-1)  ! pi ... 0 ... -pi
       call geq_afind(a1,d1,n1a,ztha,dfind1)
       call geq_afind(a2,d2,n2a,ztha,dfind2)
       dfind = zf1*dfind1 + zf2*dfind2

       rbout(i)=rcen+cos(ztha)*dfind
       zbout(i)=zcen+sin(ztha)*dfind
    enddo

  end subroutine geq_mergbb

  subroutine geq_aseq(rcen,zcen,r,z,n,damin,a,d,na)

    ! convert closed sequence to succession of angles and distances
    ! relative to indicated center; force minimum angle change per step

    real(geq_r8), intent(in) :: rcen,zcen
    integer, intent(in) :: n
    real(geq_r8), intent(in) :: r(n),z(n)
    real(geq_r8), intent(in) :: damin
    real(geq_r8) :: a(n),d(n)   ! angles and distances
    integer, intent(out) :: na  ! number spanning +/- 2pi range

    !----------------------
    integer :: isign,i,ishift
    real(geq_r8) :: alast,alim,aa,aprev
    !----------------------

    isign=0
    do i=1,n-1
       a(i)=atan2(z(i)-zcen,r(i)-rcen)
       d(i)=sqrt((z(i)-zcen)**2+(r(i)-rcen)**2)
       if(i.gt.1) then
          if(a(i).gt.a(i-1)) then
             isign=isign+1
          else
             isign=isign-1
          endif
       endif
    enddo

    if(isign.lt.0) then
       isign=-1
       a(1:n-1)=a(1:n-1)*isign
    else
       isign=1
    endif
    
    alast=a(1)+geq_2pi
    alim=alast-damin
    a(n)=alast
    d(n)=d(1)

    ishift=0
    na=1
    i=1
    aprev=a(1)
    do
       i=i+1
       if(i.eq.n) then
          na=na+1
          a(na)=alast
          d(na)=d(n)
          exit
       endif
       aa=a(i)+ishift*geq_2pi
       if(aa.lt.aprev-geq_2pi/2) then
          ishift=1
          aa=aa+geq_2pi
       endif
       if((aa.ge.aprev+damin).and.(aa.le.alim)) then
          na=na+1
          a(na)=aa
          d(na)=d(i)
       endif
    enddo

    if(na.lt.n) then
       a(na+1:n)=0
       d(na+1:n)=0
    endif

    if(isign.eq.-1) a=a*isign

  end subroutine geq_aseq

  subroutine geq_afind(a,d,n,ang,dist)
    ! interpolate distance-- linear interpolation of parabolic fits
    !  as a function of angle

    integer, intent(in) :: n
    real(geq_r8), intent(in) :: a(n),d(n)  ! dist vs. angle
    real(geq_r8), intent(in) :: ang        ! angle in
    real(geq_r8), intent(out) :: dist      ! distance out

    ! a is either monotonic increasing or monotonic decreasing
    !----------------------------
    real(geq_r8) :: zang,zfac1,zfac2,zd1,zd2
    integer :: isign,iest
    !----------------------------
    ! force angle into range

    zang=ang

    if(zang.lt.min(a(1),a(n))) then
       do
          zang = zang + geq_2pi
          if(zang.ge.min(a(1),a(n))) exit
       enddo
    else if(zang.gt.max(a(1),a(n))) then
       do
          zang = zang - geq_2pi
          if(zang.le.max(a(1),a(n))) exit
       enddo
    endif

    if(a(n).gt.a(1)) then
       isign=1
    else
       isign=-1
    endif

    iest = 1 + (zang-a(1))*(n-1)/(a(n)-a(1))
    iest = max(1,min((n-1),iest))

    if(isign*zang.lt.isign*a(iest)) then
       do
          if(iest.eq.1) exit
          iest=iest-1
          if(isign*zang.ge.isign*a(iest)) exit
       enddo
    else if(isign*zang.gt.isign*a(iest+1)) then
       do
          if((iest+1).eq.n) exit
          iest=iest+1
          if(isign*zang.le.isign*a(iest+1)) exit
       enddo
    endif

    zfac2=(zang-a(iest))/(a(iest+1)-a(iest))
    zfac1=1.0_geq_r8-zfac2

    if(iest.eq.1) then
       call dfit(a(n-1)-isign*geq_2pi,d(n-1),a(1),d(1),a(2),d(2), &
            zang,zd1)
    else
       call dfit(a(iest-1),d(iest-1),a(iest),d(iest),a(iest+1),d(iest+1), &
            zang,zd1)
    endif

    if(iest.eq.(n-1)) then
       call dfit(a(n-1),d(n-1),a(n),d(n),a(2)+isign*geq_2pi,d(2), &
            zang,zd2)
    else
       call dfit(a(iest),d(iest),a(iest+1),d(iest+1),a(iest+2),d(iest+2), &
            zang,zd2)
    endif

    dist = zfac1*zd1 + zfac2*zd2

    contains
      subroutine dfit(am,dm,a,d,ap,dp,ang,dist)
        ! parabolic fit; ang btw am and ap; am,a,ap either strict increasing
        ! or strict decreasing.
        !   d(am)=dm; d(a)=d; d(ap)=dp; want d(ang)

        real(geq_r8), intent(in) :: am,dm,a,d,ap,dp,ang
        real(geq_r8), intent(out) :: dist

        real(geq_r8) :: xm,xp,fm,fp,c1,c2,x,den,f
        real(geq_r8) :: dlin,dmin,dmax

        !  f = c1*x**2 + c2*x;  ang = x+a;  dist = f+d

        xm = am-a
        xp = ap-a

        fm = dm-d
        fp = dp-d

        x = ang-a

        den = xp*xm*(xp-xm)
        c1 = (fp*xm-fm*xp)/den
        c2 = (fm*xp*xp-fp*xm*xm)/den

        f = x*(c1*x+c2)

        dist = f + d

        ! mod DMC: limits need to be imposed, for some extreme cases...
        ! these are based on the linear interpolation

        if(x*xm.ge.0.0_geq_r8) then
           f=x/xm
           dlin = f*dm + (1.0_geq_r8-f)*d
        else
           f=x/xp
           dlin = f*dp + (1.0_geq_r8-f)*d
        endif

        dmin = dlin*0.90_geq_r8
        dmax = dlin*1.15_geq_r8

        if((dist.lt.dmin).OR.(dist.gt.dmax)) then
           ! limits...
           !!! write(6,*) 'dm,d,dp = ',dm,d,dp
           !!! write(6,*) ' --> got: ',dist
           dist=max(dmin,min(dmax,dist))
           !!! write(6,*) ' --> use: ',dist
           !!! write(6,*) ' x_ratio: ',min(abs(xm),abs(xp))/max(abs(xm),abs(xp))
           !!! write(6,*) ' '
        endif

      end subroutine dfit

  end subroutine geq_afind

!------------------------------------------------------
!  free memory associated with G-EQdisk object
!  if the object was never initialized: deallocate errors are ignored!
!
  subroutine geq_free(geq, ier)
    implicit none
    type(geqdsk), intent(inout) :: geq
    integer, intent(out) :: ier
 
    ier = 0
 
! basic G-EQdisk items
 
    call geq_free_slice(geq)
 
! items for time dependent G-EQdisk data from MDSplus
 
    call geq_free_tvar(geq)
 
  end subroutine geq_free
 
!-----------------------------------------------
! deallocate G-EQdisk time slice data
 
  subroutine geq_free_slice(geq)
    implicit none
    type(geqdsk), intent(inout) :: geq
 
    integer istat
 
    deallocate(geq%fpol_tm,stat=istat)
    deallocate(geq%pres_Nt__M2,stat=istat)
    deallocate(geq%ffprim_T2M2Rad__Wb,stat=istat)
    deallocate(geq%pprime_NtRad__M2Wb,stat=istat)
    deallocate(geq%qpsi,stat=istat)
 
    deallocate(geq%psirz_Wb__Rad,stat=istat)
 
    deallocate(geq%rbbbs_m,stat=istat)
    deallocate(geq%zbbbs_m,stat=istat)
 
  end subroutine geq_free_slice
 
!-----------------------------------------------
! deallocate (MDS+) time variation data
 
  subroutine geq_free_tvar(geq)
    implicit none
    type(geqdsk), intent(inout) :: geq
 
    integer istat
 
    deallocate(geq%t_nw,stat=istat)
    deallocate(geq%t_nh,stat=istat)
 
    deallocate(geq%t_rdim,stat=istat)
    deallocate(geq%t_zdim,stat=istat)
    deallocate(geq%t_rcentr,stat=istat)
    deallocate(geq%t_rleft,stat=istat)
    deallocate(geq%t_zmid,stat=istat)
    deallocate(geq%t_rmaxis,stat=istat)
    deallocate(geq%t_zmaxis,stat=istat)
    deallocate(geq%t_bcentr,stat=istat)
    deallocate(geq%t_current,stat=istat)
    deallocate(geq%t_simag,stat=istat)
    deallocate(geq%t_sibry,stat=istat)
 
    deallocate(geq%t_fpol,stat=istat)
    deallocate(geq%t_pres,stat=istat)
    deallocate(geq%t_ffprim,stat=istat)
    deallocate(geq%t_pprime,stat=istat)
    deallocate(geq%t_qpsi,stat=istat)
 
    deallocate(geq%t_psirz,stat=istat)
 
    deallocate(geq%t_nbbbs,stat=istat)
 
    deallocate(geq%t_rbbbs,stat=istat)
    deallocate(geq%t_zbbbs,stat=istat)
 
    deallocate(geq%rlim_m,stat=istat)
    deallocate(geq%zlim_m,stat=istat)
 
  end subroutine geq_free_tvar
 
!.............................................................................
 
  subroutine geq_error(ier)
    implicit none
    integer, intent(in) :: ier
 
    integer lunout
 
!.....................
 
    call geqdsk_lunmsg_get(lunout)
 
    select case (ier)
 
       case (1)
          write(lunout,*) &
               & 'geqdsk::geqdsk_init:: ERROR cannot open G-EQDSK file!'
       case (2)
          write(lunout,*) &
               & 'geqdsk::geqdsk_init:: ERROR occurred while allocating!'
       case (3)
          write(lunout,*) &
               & 'geqdsk::geqdsk_init:: MDS+ access string parse error!'
       case (4)
          write(lunout,*) &
               & 'geqdsk::geqdsk_init:: MDS+ slice time request not readable!'
       case (5)
          write(lunout,*) &
               & 'geqdsk::geqdsk_init:: MDS+ error (unspecified)!'
       case (6)
          write(lunout,*) &
               & 'geqdsk::geqdsk_init:: MDS+ i/o error!'
       case (7)
          write(lunout,*) &
               & 'geqdsk::geqdsk_init:: MDS+ data consistency error!'
 
       case default
 
    end select
  end subroutine geq_error
 
!-----------------------------------------------
  subroutine geq_echo(str1,str2)
    implicit none
    character*(*) str1,str2   ! message components
 
    integer lunout,ilen
 
    call geqdsk_lunmsg_get(lunout)
 
    ilen=max(1,len_trim(str1))
    write(lunout,'(a)') str1(1:ilen)
    ilen=max(1,len_trim(str2))
    write(lunout,'(2x,a)') str2(1:ilen)
 
  end subroutine geq_echo
 
  subroutine geq_nanscan(zexpr,inum,zr4buf,ilnanflag)
    character*(*), intent(in) :: zexpr  ! MDSplus expression (for message)
    integer, intent(in) :: inum         ! array dimension
    real, intent(inout) :: zr4buf(inum)       ! array to be scanned for NANs
    logical, intent(inout) :: ilnanflag(inum) ! FLAG any NaN occurrences

    ! detect NaNs by writing value into a character string; if character string
    ! using "*" format.  If this contains no decimal point assume a NaN

    ! a warning message is printed, the element flag is set, and the value
    ! is cleared to prevent FPEs occurring later...

    ! NOTE that the ilnanflag array is NOT cleared to .FALSE. This is the
    ! caller's responsibility; this allows multiple geq_nanscan calls on
    ! multiple equivalently shaped data items, s.t. the flag is a logical OR
    ! of NaNs occurring in any of the data items.

    character*30 :: nantest
    integer :: ii,lunout

    do ii=1,inum
       nantest=' '
       write(nantest,*) zr4buf(ii)
       if(index(nantest,'.').eq.0) then
          ilnanflag(ii)=.TRUE.
          zr4buf(ii)=0.0
          call geqdsk_lunmsg_get(lunout)
          write(lunout,*) ' *** at index ',ii,': ',trim(nantest), &
               ' detected in ',trim(zexpr)
       endif
    enddo

  end subroutine geq_nanscan

  subroutine geq_bchk(geq,zlbl,inum,rb,zb,ier)

    ! scan the bdy data -- report error on any points "outside the box".

    use geqdsk_nomds_aux
    implicit NONE

    type(geqdsk), intent(in) :: geq    ! G-eqdsk data object
    character*(*), intent(in) :: zlbl  ! label for contour
    integer, intent(in) :: inum        ! number of (R,Z) points
    real*8, intent(inout) :: rb(inum),zb(inum)  ! the (R,Z) point sequence

    integer, intent(out) :: ier        ! return code, 0=OK

    real*8, parameter :: threshold = 2.d-5  ! fix up out of bound points which are less than this threshold

    !--------------------------
    ! local:
    real*8 :: zrmin,zrmax,zzmin,zzmax
    integer :: ib,iwarnR,iwarnZ,iwasR,iwasZ
    !--------------------------

    zrmin = geq%Rleft_m
    zrmax = zrmin + geq%Rdim_m

    zzmin = geq%Zmid_m - geq%Zdim_m * 0.5d0
    zzmax = zzmin + geq%Zdim_m

    ier = 0
    iwarnR = 0
    iwarnZ = 0

    do ib = 1,inum
       if((rb(ib).lt.zrmin).or.(rb(ib).gt.zrmax)) then
          iwasR = iwarnR
          if(iwarnR.eq.0) then
             iwarnR=1
             write(lunmsg,*) ' ?geq_bchk: '//trim(zlbl)//' out of R bounds:'
             write(lunmsg,*) '  Rmin, Rmax = ',zRmin,zRmax
          endif
          write(lunmsg,*) '  Rb(',ib,')= ',Rb(ib)
          if (iwasR<=0) then
             if (max(zrmin-rb(ib),rb(ib)-zrmax)<threshold*(zrmax-zrmin)) then
                write(lunmsg,*) '  ---> !fixup applied'
                rb(ib) = min(zrmax,max(rb(ib),zrmin))
                iwarnR = -1
             else
                write(lunmsg,*) '  ---> ?fixup not possible'
             end if
          end if
       endif
       if((zb(ib).lt.zzmin).or.(zb(ib).gt.zzmax)) then
          iwasZ = iwarnZ
          if(iwarnZ.eq.0) then
             iwarnZ=1
             write(lunmsg,*) ' ?geq_bchk: '//trim(zlbl)//' out of Z bounds:'
             write(lunmsg,*) '  Zmin, Zmax = ',zZmin,zZmax
          endif
          write(lunmsg,*) '  Zb(',ib,')= ',Zb(ib)
          if (iwasZ<=0) then
             if (max(zzmin-zb(ib),zb(ib)-zzmax)<threshold*(zzmax-zzmin)) then
                write(lunmsg,*) '  ---> !fixup applied'
                zb(ib) = min(zzmax,max(zb(ib),zzmin))
                iwarnZ = -1
             else
                write(lunmsg,*) '  ---> ?fixup not possible'
             end if
          end if
       endif
    enddo

    if (iwarnR>0 .or. iwarnZ>0) ier=1
  end subroutine geq_bchk

  subroutine geq_curtime(geq,ztime1)

    ! fetch the current time (if MDS+ EFIT tree is in use)
    !  if data was from a G file, no time is available; ztime1=0.00 will be
    !  returned.

    type(geqdsk), intent(in) :: geq ! geqdsk object w/possible MDS+ time series
    real(geq_r8), intent(out) :: ztime1  ! time returned

!    if(.not.allocated(geq%gtime_s)) then
       ztime1=0.00
!    else
!       ztime1=geq%curtime
!    endif

  end subroutine geq_curtime

end module geqdsk_nomds_mod
