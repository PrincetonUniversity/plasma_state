SUBROUTINE scradphase(raxis,yaxis,rmcx1,ymcx1,immax,imom,zthadj,ier)
  !
  !  adjust phase of theta parametrization on "1" surface
  !    want theta=0 to give R.gt.raxis, y.eq.yaxis
  !
  !  input:  raxis,yaxis -- mag. axis position
  !          rmcx1,ymcx1 -- moments of surface (unadjusted)
  !          immax -- max no. of moments (array dimension)
  !          imom  -- no. of active moments (use 0:imom).
  !
  ! output:  rmcx1,ymcx1 -- adjusted
  !
  !  the idea here is to align the phases of two surfaces, to reduce as
  !  much as possible, on average, the non-orthogonality of the theta
  !  lines -- i.e. remove any "swastika-like" skewing of the theta
  !  parametrization.
  !
  use iso_c_binding, only: fp => c_double
  use ps_physconst_mod, only: twopi
  implicit none
  integer, intent(out) :: ier
  real(fp) :: delth
  real(fp) :: angspan
  real(fp) :: angnorm
  real(fp) :: zthadj
  integer :: imom
  integer :: im
  real(fp) :: ztheta
  integer :: ith, itp
  real(fp) :: zpi
  integer :: inpts
  integer :: immax
  !
  integer iy0   ! interval where y=yaxis falls, with r.gt.raxis
  !
  real(fp) :: raxis,yaxis
  real(fp) :: rmcx1(0:immax,2)
  real(fp) :: ymcx1(0:immax,2)
  !
  parameter (inpts=128)
  real(fp) :: r0(inpts),y0(inpts),r1(inpts),y1(inpts)
  real(fp) :: enorm(2),espan(2)
  !
  real(fp) :: zthetap,zthp,zth,zyp,zy,zero
  !
  real(fp) :: zsn(100),zcs(100)
  !
  integer :: lunmsg
  !
  !---------------------------------------------------------------------
  !
  call scrgetlun(lunmsg)
  !
  !  expand surfaces into (R,Y) sequences
  !
  ier = 1
  zero=0
  ztheta=zero
  iy0=0
  !
  do ith=1,inpts
    ztheta=(ith-1)*twopi/inpts
    CALL scrsincos(ztheta,imom,zsn,zcs)
    r1(ith)=rmcx1(0,1)
    y1(ith)=ymcx1(0,1)
    do im=1,imom
      r1(ith)=r1(ith)+rmcx1(im,1)*zcs(im)+rmcx1(im,2)*zsn(im)
      y1(ith)=y1(ith)+ymcx1(im,1)*zcs(im)+ymcx1(im,2)*zsn(im)
    enddo
  enddo
  !
  !  look for interval on large major radius side containing y=yaxis
  !
  do ith=1,inpts
    zthetap=ztheta
    ztheta=(ith-1)*twopi/inpts
    if(ith.gt.1) then
      itp=ith-1
      if((r1(ith).gt.raxis).and.(r1(itp).gt.raxis)) then
        if((y1(ith)-yaxis)*(y1(itp)-yaxis).le.zero) then
          if(iy0.gt.0) then
            if(y1(itp).ne.yaxis) then
              call scraborter(6, '?scradphase:  mult. y=yaxis intervals?')
              ier = 2
            end if
          else
            iy0=itp
            zthp=zthetap
            zth=ztheta
            zyp=y1(itp)
            zy=y1(ith)
          endif
        endif
      endif
    endif
    !
  enddo
  if(ier.ne.1) return
  !
  if(iy0.eq.0) then
    zy=y1(1)
    zyp=y1(inpts)
    zth=zero
    zthp=ztheta-twopi
  endif
  !
  if((zy-yaxis)*(zyp-yaxis).gt.zero) then
    call scraborter(6, '?scradphase: y=yaxis interval search failure?')
    ier = 3
    return
  endif
  !
  zthadj=zthp+(yaxis-zyp)*(zth-zthp)/(zy-zyp)  ! theta -> y=yaxis
  !
  CALL scrphasead(rmcx1,ymcx1,immax,imom,zthadj)
  !
  return
end SUBROUTINE scradphase
