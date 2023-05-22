!------------------------------------------------------
subroutine geq_ckrzc(r,z,n,ndim)
  
  !  squeeze out duplicate points, make sure end pts connect
  
  implicit NONE
  
  integer, parameter :: r8 = selected_real_kind(12,100)
  integer, intent(in) :: ndim     ! array dims
  integer, intent(inout) :: n     ! actual number in use
  real(r8), dimension(ndim), intent(inout) :: r,z    ! the contour
  
  integer i,j
  real(r8) zmax,rmax,dl,dlmin
  
  ! --------------------------------------
  
  ! squeeze out consecutive duplicate points
  
  rmax=maxval(abs(r(1:n)))
  zmax=maxval(abs(z(1:n)))
  dlmin=5.0e-6_r8*max(rmax,zmax)
  
  i=0
  do
     i=i+1
     if(i.ge.n) exit
     dl=sqrt((r(i+1)-r(i))**2+(z(i+1)-z(i))**2)
     if(dl.lt.dlmin) then
        do j=i+1,n-1
           r(j)=r(j+1)
           z(j)=z(j+1)
        enddo
        n=n-1
        i=i-1
     endif
  enddo
  
  ! make sure end pts are connected
  
  if((r(1).ne.r(n)).or.(z(1).ne.z(n))) then
     n=n+1
     r(n)=r(1)
     z(n)=z(1)
  endif
  
  ! make sure no NaNs left at high end
  
  do i=n+1,ndim
     r(i)=r(n)
     z(i)=z(n)
  enddo
  
end subroutine geq_ckrzc
