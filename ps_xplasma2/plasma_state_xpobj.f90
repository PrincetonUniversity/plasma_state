module plasma_state_xpobj

  !------------------------------------------------------
  ! D. McCune  ... revised Jan 2008
  !   define "interp_object" to implement interpolation
  !   capability for plasma state, using xplasma.
  !------------------------------------------------------

  use xplasma_definitions

  implicit NONE
  public

  type :: interp_object

     type (xplasma), pointer :: s_xpobj => NULL()

  end type interp_object

  character*32, parameter :: xpobj_hash_name = 'ps_xplasma_hash'
  integer, parameter :: r8hash_size = 1

  contains

    subroutine xpobj_blkbx(xobj, &
         name,isize,r8size,inew,id,ia_ptr,r8a_ptr,iout,ierr)

      ! fetch or create blackbox of indicated sizes 
      ! s pointer assumed to already be set

      type(interp_object) :: xobj

      character*(*), intent(in) :: name   ! name of black box (bb)
      integer, intent(in) :: isize,r8size ! integer and real*8 sizes of bb
      logical, intent(out) :: inew        ! .TRUE. if bb is new in xplasma
      integer, intent(out) :: id          ! ID of xplasma object
      integer, dimension(:), pointer :: ia_ptr   ! integer data pointer
      real*8, dimension(:), pointer :: r8a_ptr   ! real*8 data pointer
      integer, intent(in) :: iout         ! LUN for messages
      integer, intent(out) :: ierr        ! completion code; 0=OK

      !----------------------------
      integer :: isize_loc,r8size_loc
      integer, parameter :: loctype = 12345

      type (xplasma), pointer :: s
      !----------------------------

      ierr=0
      
      ! set "s" pointer
      if(.not.associated(xobj%s_xpobj)) then
         write(iout,*) ' ?xpobj_blkbx: xobj has not been initialized.'
         ierr=999
         return
      endif

      s => xobj%s_xpobj

      inew=.FALSE.
      nullify(ia_ptr)
      nullify(r8a_ptr)

      call xplasma_blkbxId(s,name,id)
      if(id.gt.0) then
         call xplasma_blackbox_info(s,id,ierr, &
              r8size=r8size_loc, isize=isize_loc)

         if(ierr.ne.0) then
            write(iout,*) ' ?xpobj_blkbx: xplasma_blackbox_info error:'
            call xplasma_error(s,iout,ierr)
            return
         endif

         if((r8size.eq.r8size_loc).and.(isize.eq.isize_loc)) then
            call xplasma_blackBox_retrieve(s,id,ierr, &
                 ia_ptr=ia_ptr, r8a_ptr=r8a_ptr)
            if(ierr.ne.0) then
               write(iout,*) ' ?xpobj_blkbx: xplasma_blackbox_info error:'
               call xplasma_error(s,iout,ierr)
            endif
         else
            inew = .TRUE.  ! sizes differ
         endif

      else
         inew = .TRUE.  ! did not exist
      endif

      if(inew) then
         call xplasma_author_set(s,'plasma_state_xpobj',ierr)
         if(ierr.ne.0) then
            write(iout,*) ' ?xpobj_blkbx: xplasma_author_set error:'
            call xplasma_error(s,iout,ierr)
            return
         endif

         call xplasma_create_blackBox(s,name,loctype,id,ierr, &
              iasize=isize, r8asize=r8size, ia_ptr=ia_ptr, r8a_ptr=r8a_ptr)
         if(ierr.ne.0) then
            write(iout,*) ' ?xpobj_blkbx: xplasma_create_blackbox error:'
            call xplasma_error(s,iout,ierr)
            return
         endif

         call xplasma_author_clear(s,'plasma_state_xpobj',ierr)
         if(ierr.ne.0) then
            write(iout,*) ' ?xpobj_blkbx: xplasma_author_clear error:'
            call xplasma_error(s,iout,ierr)
            return
         endif
      endif

    end subroutine xpobj_blkbx

end module plasma_state_xpobj
