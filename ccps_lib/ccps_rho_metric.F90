subroutine ccps_rho_metric(iobj,metric_name,ngrid,rho_grid,nresult,result, &
     ierr)

  ! compute and return flux surface averaged or integrated metrics over
  ! a specified rho grid.

  ! specify metric_name "DVOL_EQ" to use the Volume(rho) profile employed
  ! in ps_rho_rezone and ps_user_rezone; use "DVOL" to get exact volumes.

  ! specify metric_name "AREA_EQ" to use the Area(rho) profile employed
  ! in ps_rho_rezone and ps_user_rezone; use "DAREA" to get exact cross
  ! sectional areas.

  use iso_c_binding, only: c_char
  use plasma_state_mod
  implicit NONE


  integer :: iobj(*)        ! (ps_container_size) words are used: state object

  character(kind=c_char), intent(in) :: metric_name(*) ! null terminated string specifying the desired
  ! metric... name specification not sensitive to use of upper or lower case
  ! alphabetic characters: e.g. "dVol" and "DVOL" yield the same result.

  integer :: ngrid          ! size of RHO grid provided
  REAL(kind=rspec) :: rho_grid(ngrid)  ! the RHO grid
          ! *** strict ascending order ***
          ! *** in range [0.0_rspec:1.0_rspec] ***

  integer :: nresult        ! size of result expected (=ngrid or =ngrid-1)
  REAL(kind=rspec) :: result(nresult) ! results returned

  !  there will either be (ngrid) results or (ngrid-1) results depending
  !  on the type of metric chosen-- see e.g. partial table, below

  !  ***The caller must set ngrid and nresults consistently, to avoid an error
  !  condition.

  integer,intent(out) :: ierr   ! exit status 0=OK

  !-----------------------------------------
  !  partial table of common metrics
  !
  !      sizes:  nrho = [rho_name grid size]           (possible nresult value)
  !              nrho-1  = [rho_name grid size] - 1    (possible nresult value)
  !
  !  metric_name   size      description
  ! 
  !    "dVol"      nrho-1    zone volumes, m^3
  !    "dArea"     nrho-1    zone cross sectional areas, m^2
  !    "Vol"       nrho      volume enclosed in rho surfaces, m^3
  !    "Area"      nrho      cross section area enclosed by rho surfaces, m^2
  !    "<1/R^2>S"  nrho      differential volume flux surface average of
  !                          (1/R^2) at each surface, m^-2
  !    "<1/R^2>    nrho-1    zonal volume average of (1/R^2), m^-2
  !
  !  in some cases, surface averages are faster to compute than zonal
  !  averages.
  !
  !    "dVol_eq"   nrho-1    interpolated volumes as used in ccps_rho_rezone
  !    "dArea_eq"  nrho-1    interpolated areas as used in ccps_rho_rezone
  ! 
  !-----------------------------------------
  ! there are many more; see the xplasma documentation.
  ! in TRANSP this is in source/doc/xplasma.hlp; on the web it is currently
  ! at http://w3.pppl.gov/~pshare/help/xplasma.htm
  !      (or access via the xplasma web page under the catalog at
  !       http://w3.pppl.gov/NTCC).
  !
  !-----------------------------------------

  !-------------------------
  character*32 zmetric
  type (plasma_state_container) :: ptr
  type (plasma_state), pointer :: ss
  !-------------------------

  call ccps_argcheck('ccps_rho_metric',iobj,ierr)
  if(ierr.ne.0) return

  ! proceed...

  call load_ps_container(iobj,ptr)
  ss => ptr%ps_ptr

  ! convert metric specification to fortran string
  call cstring(zmetric,metric_name,'2F')

  call ps_rho_metric('USER',zmetric,result,ierr, &
       state=ss, user_rho_grid=rho_grid)

end subroutine ccps_rho_metric
