subroutine ps_mdescr_gfile_read(gfilename,ss,ierr)

  ! hand coded routine -- update machine description elements from G eqdsk
  ! file:  Box Rmin/max & Zmin/max, and axisymmetric limiter

  USE plasma_state_definition_mod
  USE geqdsk_nomds_mod
  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN) :: gfilename  ! path to G-eqdsk (MDS+ path OK).
  TYPE (plasma_state) :: ss     ! state containing data to write
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !----------------------
  integer :: ilun,iout,inum,ilun_tmp
  TYPE(geqdsk) :: geq
  !----------------------

  iout = SWIM_err_out_file

  call find_io_unit(ilun_tmp)

  call geq_getlun(ilun)            ! save this
  call geq_setlun(ilun_tmp)        ! use this unit for any fortran I/O

  do
     call geq_init(geq, gfilename, ierr)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_mdescr_gfile_read: file or MDS+ access failure:'
        write(iout,*) '  gfilename was: '//trim(gfilename)
        exit
     endif

     ! OK -- set box limits

     ss%R_min_box = geq%RLeft_m
     ss%R_max_box = geq%RLeft_m + geq%Rdim_m

     ss%Z_min_box = geq%Zmid_m - geq%Zdim_m/2
     ss%Z_max_box = geq%Zmid_m + geq%Zdim_m/2

     ! OK -- set limiter

     inum = geq%limitr
     ss%num_rzlim = inum

     if(allocated(ss%rlim)) deallocate(ss%rlim)
     if(allocated(ss%zlim)) deallocate(ss%zlim)

     call plasma_state_empty_alloc(ss,ierr)
     if(ierr.ne.0) then
        write(iout,*) ' ?ps_mdescr_gfile_read: plasma_state_empty_alloc error.'
        exit
     endif

     ss%rlim(1:inum) = geq%rlim_m(1:inum)
     ss%zlim(1:inum) = geq%zlim_m(1:inum)

     exit
  enddo

  call geq_setlun(ilun)            ! restore prior geq LUN
end subroutine ps_mdescr_gfile_read
