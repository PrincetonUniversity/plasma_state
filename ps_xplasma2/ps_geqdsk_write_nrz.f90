subroutine ps_geqdsk_write_nrz(ps,gfile,glabel,inR,inZ,inB,iout,ierr)

  !------------------------------------------------------------
  ! write G-eqdsk file from contents of plasma state; control the sizes
  ! of the R & Z grids (inR,inZ) and the number of boundary points to 
  ! be written (inB)
  ! Psi(R,Z), [R,Z](rho,theta), g(rho), q(rho), and P(rho) must be there.
  !------------------------------------------------------------

  use plasma_state_xpobj  ! XPLASMA2 state object definitions

  use plasma_state_definition_mod

  implicit NONE

  !-----------------------
  ! arguments:

  type (plasma_state) :: ps            ! plasma state instance 
  character*(*), intent(in) :: gfile   ! name of file to write
  character*(*), intent(in) :: glabel  ! label string for file

  integer, intent(in) :: inR           ! desired R grid size
  integer, intent(in) :: inZ           ! desired Z grid size
  integer, intent(in) :: inB           ! desired no. of bdy points

  integer, intent(in) :: iout          ! Fortran LUN for messages
  integer, intent(out) :: ierr         ! status code, 0=OK

  !------------------------
  ! local:

  integer :: ilun,id_p,id_q,id_Rc,id_Zc
  real*8 :: Rmin,Rmax,Zmin,Zmax,zcur
  !------------------------

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !-------------------------------------------------------------------------
  ! executable code...

  call ps_tagchk('ps_geqdsk_write_nrz',ps%itag,iout,ierr)
  if(ierr.ne.0) return

  ! check contents

  call ps_wrgeq_chk(ps,'ps_geqdsk_write_nrz',iout,ierr)
  if(ierr.ne.0) return

  ! OK basics seems to be there...

  ! get xplasma pointer

  xobj = transfer(ps%iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_profID(s,'P_EQ',id_p)
  call xplasma_profID(s,'Q_EQ',id_q)

  zcur = ps%curt(ps%nrho_eq) ! total plasma current (A)

  do
     call xplasma_find_item(s,'__R_coord',id_Rc,ierr); if(ierr.ne.0) exit
     call xplasma_find_item(s,'__Z_coord',id_Zc,ierr); if(ierr.ne.0) exit

     call xplasma_coord_info(s,id_Rc,ierr, &
          xmin=Rmin,xmax=Rmax); if(ierr.ne.0) exit

     call xplasma_coord_info(s,id_Zc,ierr, &
          xmin=Zmin,xmax=Zmax); if(ierr.ne.0) exit

     call find_io_unit(ilun)

     call xplasma_wr_geqdsk(s,ierr, &
          lun_geqdsk=ilun, filename=gfile, &
          label=glabel, &
          Rmin=Rmin, Rmax=Rmax, Zmin=Zmin, Zmax=Zmax, &
          cur=zcur, id_pprof=id_p, id_qprof=id_q, &
          nh=inR, nv=inZ, nbdy=inB)

     if(ierr.ne.0) then
        write(iout,*) ' ?ps_geqdsk_write_nRZ: xplasma_wr_geqdsk error status:'
        call xplasma_error(s,ierr,iout)
     endif

     exit
  enddo

end subroutine ps_geqdsk_write_nrz
