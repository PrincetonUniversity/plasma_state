subroutine xdistrib_sub(ntarg,lmask,xtarg,atarg, &
     & idum0,ztarg,idum1,zdum,idum2)
  !
  ! evaluation subroutine for zridderx
  !
  !    looking for zeroes of F(x(k)) =
  !                    int(x0 to x(k))[f'*dx']/int(x0 to xn)[f'*dx'] - z(k)
  !
  use xdistrib_mod
  implicit NONE
 
  ! ------------------------ arguments --------------------------------
 
  integer, intent(in) :: ntarg         ! input vector dimension
  logical, intent(in) :: lmask(ntarg)  ! masking vector
  real*8, intent(in) :: xtarg(ntarg)   ! input x values to evaluate
  real*8, intent(out) :: atarg(ntarg)  ! output function eval. results.
  real*8, intent(in) :: ztarg(ntarg)   ! data for function eval.
 
  integer, intent(in) :: idum0,idum1,idum2  ! placeholders
  real*8, intent(in) :: zdum(ntarg)    ! placeholder
  ! ------------------------ local stuff ------------------------------
 
  integer nvec,jvec,i,imode,iwarn
  real*8 xvec(ntarg)
  integer iv(ntarg)
  real*8 dxn(ntarg),zdum2(ntarg),zdum3(ntarg)
 
  real*8 feqi_1int                     ! integration subfunction
 
  ! ------------------------ executable code --------------------------
 
  ! --> find the active points
 
  nvec=0
  do i=1,ntarg
     if(.not.lmask(i)) then
        nvec=nvec+1
        xvec(nvec)=xtarg(i)
     endif
  enddo
 
  ! --> table lookup on these
 
  imode=1
 
  iv=0
  call xlookup(nvec,xvec,nx_spl,xpkg,imode,iv,dxn,zdum2,zdum3,iwarn)
 
  jvec=0
  do i=1,ntarg
     if(.not.lmask(i)) then
        jvec=jvec+1
        atarg(i)=ftabl(iv(jvec)) + &
             & feqi_1int(iorder_spl,xpkg,splcoef,nx_spl,iv(jvec),dxn(jvec))
        atarg(i)=atarg(i)/ftabl(nx_spl) - ztarg(i)
     endif
  enddo
 
  return
 
end subroutine xdistrib_sub
 
