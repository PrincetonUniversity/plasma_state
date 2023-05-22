subroutine ps_species_list_store(iobj, listname, iauthor, dim1, dim2, &
     elem_names, qatoms, qvals, mvals, itypes, imod, iout,ierr)

  !------------------------------------------------------------
  ! D. McCune 8 Oct 2006

  ! XPLASMA2 implementation of plasma_state subroutine
  ! store a species list, perform error checking

  ! This routine is called by generated code associated with the
  ! plasma state.  Component codes will not call this directly.
  !------------------------------------------------------------

  use plasma_state_definition_mod
  use plasma_state_xpobj  ! XPLASMA2 state object definitions
  implicit NONE

  !-----------------------
  integer :: iobj(ps_container_size)     ! xplasma opaque container

  character*(*), intent(in) :: listname    ! NAME of list to store

  integer, intent(in) :: iauthor           ! author ID

  integer, intent(in) :: dim1,dim2         ! array dimension limits

  character*(*), intent(in) :: elem_names(dim1:dim2) ! specie names

  REAL(kind=rspec), intent(in) :: qatoms(dim1:dim2)  ! atomic charges (C)
  REAL(kind=rspec), intent(in) :: qvals(dim1:dim2)   ! specie charges (C)
  REAL(kind=rspec), intent(in) :: mvals(dim1:dim2)   ! specie masses (kg)
  integer, intent(in) :: itypes(dim1:dim2)           ! specie type

  integer, intent(out) :: imod             ! =1 if profile is new or modified
  !  =0 on output if profile was not changed.

  integer, intent(in) :: iout              ! Fortran LUN for messages
  integer, intent(out) :: ierr             ! error status code returned 0 = OK

  !--------------------------------------------------
  ! local...

  integer :: ielems,ie,id,iertmp,inum,idiff,iz,itype,ilen
  character*32 units,section
  character*32 tmp_label
  integer, dimension(:), allocatable :: ivals,ichek
  REAL(kind=rspec), dimension(:), allocatable :: r8vals,r8chek
  character*32, dimension(:), allocatable :: chvals
  character*32, dimension(:), allocatable :: namchek

  REAL(kind=rspec) :: zdiff,zdenom
  REAL(kind=rspec), parameter :: ztol = 1.0e-12_rspec

  type (interp_object) :: xobj
  type (xplasma), pointer :: s
  !--------------------------------------------------

  imod = 0

  xobj = transfer(iobj,xobj)
  s => xobj%s_xpobj

  call xplasma_global_info(s,ierr, init_check=.TRUE.)
  if(ierr.ne.0) then
     write(iout,*) ' %ps_species_list_store: invalid or unitialized xplasma object.'
     return
  endif

  !--------------------------------------------------
  if(dim2.lt.dim1) then
     ierr=901
     write(iout,*) ' ?ps_species_list_store: dimensioning error:'
     write(iout,*) '  list dimension lower limit: ',dim1
     write(iout,*) '  list dimension upper limit: ',dim2
     write(iout,*) '  list name: ',trim(listname)
     return
  endif

  !--------------------------------------------------
  !  set up to use ps_list_store call

  allocate(ivals(dim1:dim2),r8vals(dim1:dim2),chvals(dim1:dim2))
  allocate(ichek(dim1:dim2),r8chek(dim1:dim2),namchek(dim1:dim2))
  chvals = ' '
  ilen=len(chvals(1))

  do ie=dim1,dim2
     if(qvals(ie).lt.0.0_rspec) then
        ivals(ie)=-int(abs(qvals(ie))/ps_xe + 0.1_rspec)
     else
        ivals(ie)=qvals(ie)/ps_xe + 0.1_rspec
     endif
     r8vals(ie)=mvals(ie)/ps_mp

     !  store atomic no. and type in character buffer
     if(qatoms(ie).lt.0.0_rspec) then
        iz=-int(abs(qatoms(ie))/ps_xe + 0.1_rspec)
     else
        iz=qatoms(ie)/ps_xe + 0.1_rspec
     endif
     itype = itypes(ie)

     write(chvals(ie),'("Z:",i3," Type:",i3)') iz,itype
     if(itype.eq.ps_electron) then
        chvals(ie)(16:ilen)='(electrons)'
     else if(itype.eq.ps_therm_ion) then
        chvals(ie)(16:ilen)='(thermal ion)'
     else if(itype.eq.ps_impurity) then
        chvals(ie)(16:ilen)='(impurity)'
     else if(itype.eq.ps_tokamakium) then
        chvals(ie)(16:ilen)='(Tokamakium)'
     else if(itype.eq.ps_beam_ion) then
        chvals(ie)(16:ilen)='(beam ion)'
     else if(itype.eq.ps_rf_minority) then
        chvals(ie)(16:ilen)='(RF minority)'
     else if(itype.eq.ps_fusion_ion) then
        chvals(ie)(16:ilen)='(fusion ion)'
     else
        ierr=99
        write(iout,*) ' ?ps_species_list_store: specie #',ie
        write(iout,*) '  unrecognized type code: ',itype
        write(iout,*) '  list name: ',trim(listname)
     endif
  enddo
  if(ierr.ne.0) return

  ielems = dim2 - dim1 + 1

  section = 'SPECIES_LIST'
  units = ' '
  tmp_label = 'species list'

  !  read list first if available; check for data match with tolerance.
  !  this to avoid spurious reports of change in speciel list values.

  idiff=1
  call xplasma_listId(s,listname,id)
  if(id.gt.0) then
     call xplasma_list_info(s,id,iertmp, nelems=inum)
     if(inum.eq.ielems) then
        call xplasma_getList_names(s,id,namchek(dim1:dim2),iertmp, &
             ivals=ichek(dim1:dim2),r8vals=r8chek(dim1:dim2))
        if(iertmp.eq.0) then
           idiff=0
           do ie=dim1,dim2
              if(namchek(ie).ne.elem_names(ie)) then
                 idiff=1
                 exit
              endif
              if(ichek(ie).ne.ivals(ie)) then
                 idiff=1
                 exit
              endif
              zdenom=max(abs(r8vals(ie)),abs(r8chek(ie)))
              zdenom=max(zdenom,(ztol*ztol))
              zdiff=abs(r8vals(ie)-r8chek(ie))/zdenom
              if(zdiff.gt.ztol) then
                 idiff=1
                 exit
              endif
           enddo
        endif
     endif
  endif

  if(idiff.eq.0) then
     imod=0

  else
     call ps_list_store(section, iobj, listname, &
          iauthor, units, tmp_label, &
          ielems, elem_names, ivals, r8vals, chvals, &
          imod, iout, ierr)
  endif

  if(ierr.ne.0) then
     write(iout,*) ' ?ps_species_list_store: error code = ',ierr
     write(iout,*) '  returned by ps_list_store call.'
  endif

  deallocate(ivals,r8vals,chvals,ichek,r8chek,namchek)

end subroutine ps_species_list_store
