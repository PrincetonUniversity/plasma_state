subroutine eqi_geq_mds_nlimitr(zpath,inlim,ierr)

  !  read EFIT data; return the number of points in the [R,Z]
  !  axisymmetric
  !  limiter contour

  use eqi_geq_mds_mod
  implicit none

  character*(*), intent(in) :: zpath   ! path to G-eqdsk file or MDS+ tree
  integer, intent(out) :: inlim  ! #pts in limiter contour
  integer, intent(out) :: ierr   ! status code, 0=OK

  !----------------
  
  inlim = 0

  call geq_init(geq,zpath,ierr)
  if(ierr.ne.0) return

  inlim = geq%limitr

end subroutine eqi_geq_mds_nlimitr
