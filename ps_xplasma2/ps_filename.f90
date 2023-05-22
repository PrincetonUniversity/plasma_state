subroutine ps_filename(chtag,spath,sfile,suffix)

  ! form filename for state file I/O
  ! typically:
  !   trim(spath)/<chtag>_state.cdf

  implicit NONE

  character*(*), intent(in) :: chtag  ! state object tag string
  character*(*), intent(in) :: spath  ! file path (directory)

  character*(*), intent(out) :: sfile ! full file name

  character*(*), intent(in) :: suffix ! suffix to filename (if non-blank)
  !  '_'//trim(suffix) can be added to base filename

  !  All generated filenames end in ".cdf".  The suffix, if present, is
  !  applied to the base filename.

  !-------------------------------
  integer :: ilen
  !-------------------------------

  if(spath.eq.' ') then

     if(suffix.eq.' ') then
        sfile = trim(chtag)//'_state.cdf'
     else
        sfile = trim(chtag)//'_state_'//trim(suffix)//'.cdf'
     endif

  else
     ilen = len(trim(spath))
     if(spath(ilen:ilen).eq.'/') ilen=ilen-1

     if(suffix.eq.' ') then
        sfile = spath(1:ilen)//'/'//trim(chtag)//'_state.cdf'
     else
        sfile = spath(1:ilen)//'/'//trim(chtag)// &
             '_state_'//trim(suffix)//'.cdf'
     endif
  endif

end subroutine ps_filename
