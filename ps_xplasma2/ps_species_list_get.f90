subroutine ps_species_list_get(iobj, listname, iauthor, dim1, dim2, &
     elem_names, qatoms, qvals, mvals, itypes, max_prior, alt_component, &
     iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 8 Oct 2006

  ! XPLASMA2 implementation of plasma_state subroutine
  ! fetch a species list, perform error checking

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)     ! xplasma opaque container

  character*(*), intent(in) :: listname    ! NAME of list to fetch

  integer, intent(in) :: iauthor           ! author ID (checked if non-zero)

  integer, intent(in) :: dim1,dim2         ! array dimension limits

  character*(*), intent(out) :: elem_names(dim1:dim2) ! specie names

  REAL(kind=rspec), intent(out) :: qatoms(dim1:dim2)  ! atomic charges (C)
  REAL(kind=rspec), intent(out) :: qvals(dim1:dim2)   ! specie charges (C)
  REAL(kind=rspec), intent(out) :: mvals(dim1:dim2)   ! specie masses (kg)
  integer, intent(out) :: itypes(dim1:dim2)           ! specie type

  integer, intent(in) :: max_prior         ! max prior names
  character*(*), intent(in) :: alt_component(max_prior) ! prior component names

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: ielems,ie,iz,itype
  character*32 units,section
  character*128 tmp_label
  integer, dimension(:), allocatable :: ivals
  REAL(kind=rspec), dimension(:), allocatable :: r8vals
  character*80, dimension(:), allocatable :: chvals

  character*32 dummy_altlist(max_prior)
  character*32 dummy_altelems(max_prior,dim1:dim2)

  !--------------------------------------------------

  if(dim2.lt.dim1) then
     ierr=901
     write(iout,*) ' ?ps_species_list_get: dimensioning error:'
     write(iout,*) '  list dimension lower limit: ',dim1
     write(iout,*) '  list dimension upper limit: ',dim2
     return
  endif

  !--------------------------------------------------
  !  set up to use ps_list_get call

  dummy_altlist = ' '
  dummy_altelems = ' '

  allocate(ivals(dim1:dim2),r8vals(dim1:dim2),chvals(dim1:dim2))

  ielems = dim2 - dim1 + 1

  section = 'SPECIES_LIST'
  units = '*'
  elem_names = '*'

  call ps_list_get(section, iobj, listname, &
       iauthor, units, tmp_label, &
       max_prior, dummy_altlist, alt_component, &
       ielems, elem_names, dummy_altelems, ivals, r8vals, chvals, &
       iout, ierr)

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_species_list_get: error code = ',ierr
     write(iout,*) '  returned by ps_list_get call.'
     return
  endif

  qvals = ps_xe * ivals   ! convert charge count (Z) to charge (C).

  mvals = ps_mp * r8vals  ! convert atomic weight, AMU, to mass (kg).

  do ie=dim1,dim2
     read(chvals(ie),'(2x,i3,6x,i3)') iz,itype
     qatoms(ie) = ps_xe * iz
     itypes(ie) = itype
  enddo

end subroutine ps_species_list_get
