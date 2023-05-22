subroutine eq_arc_findarc(ivec,iok,th,ansr,ivecd, &
     zinput,ninput,zoutput,noutput)

  ! subroutine for comput/zridderx root finder

  ! find points at desired arclength along a contour
  ! target arclength passed in input

  use iso_c_binding, only: fp => c_double
  use eq_arc_scrunch_mod
  implicit NONE

  integer, intent(in) :: ivec
  logical, dimension(ivec), intent(in) :: iok
  real(fp), dimension(ivec), intent(in) :: th    ! theta
  real(fp), dimension(ivec), intent(inout) :: ansr  ! L(th)-Ltarget

  integer, intent(in) :: ivecd
  integer, intent(in) :: ninput,noutput
  real(fp), dimension(ivecd,ninput), intent(in) :: zinput
  real(fp), dimension(ivecd,ninput), intent(in) :: zoutput  ! not used here

  !-----------------
  integer, dimension(3) :: ict = (/ 1, 0, 0 /)
  integer :: idum,ier,jvec,ii

  real(fp), dimension(ivec) :: thbuf, abuf
  !-----------------
  jvec=0
  do ii=1,ivec
    if(.not.iok(ii)) then
      jvec=jvec+1
      thbuf(jvec)=th(ii)
    endif
  enddo

  call spvec(ict,jvec,thbuf,jvec,abuf,ntha,tha_pkg,Lspl,idum,ier)

  jvec=0
  do ii=1,ivec
    if(.not.iok(ii)) then
      jvec=jvec+1
      ansr(ii)=abuf(jvec)-Zinput(ii,1)
    endif
  enddo

  return
end subroutine eq_arc_findarc
