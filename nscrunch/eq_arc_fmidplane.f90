subroutine eq_arc_fmidplane(ivec,iok,th,ansr,ivecd, &
     zinput,ninput,zoutput,noutput)

  ! subroutine for comput/zridderx root finder

  ! find midplane intercept; zinput contains midplane Z value

  use iso_c_binding, only: fp => c_double
  use eq_arc_scrunch_mod
  implicit NONE

  integer :: ivec
  logical :: iok(ivec)  ! mask
  real(fp) :: th(ivec)    ! theta
  real(fp) :: ansr(ivec)  ! Z(th)-Zmidplane

  integer :: ivecd
  integer :: ninput,noutput
  real(fp) :: zinput(ivecd,ninput)
  real(fp) :: zoutput(ivecd,noutput)  ! not used here

  !-----------------
  real(fp) :: Zmidplane
  integer, dimension(3) :: ict = (/ 1, 0, 0 /)
  integer :: idum,ier,jvec,ii

  real(fp) :: thbuf(ivec),abuf(ivec)
  !-----------------

  Zmidplane = zinput(1,1)

  jvec=0
  do ii=1,ivec
    if(.not.iok(ii)) then
      jvec=jvec+1
      thbuf(jvec)=th(ii)
    endif
  enddo

  call spvec(ict,jvec,thbuf,jvec,abuf,ntha,tha_pkg,Zspl,idum,ier)

  jvec=0
  do ii=1,ivec
    if(.not.iok(ii)) then
      jvec=jvec+1
      ansr(ii)=abuf(jvec)-Zmidplane
    endif
  enddo

  return
end subroutine eq_arc_fmidplane
