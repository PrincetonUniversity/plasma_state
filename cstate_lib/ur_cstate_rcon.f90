subroutine ur_cstate_rcon

  ! control list dimension reduction options

  use cstate_lib_mod
  implicit NONE

  !-------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar
  integer :: i,ifound
  character*32 :: dword

  !-------------------------

  luntty = lnout(0)
  
  do
     if(lxout(0)) then
        write(luntty,*) ' '
        write(luntty,*) ' Status of reduction operators:'
        write(luntty,*) ' dimension     activated? (T/F)'
        write(luntty,*) ' ------------  ----------------'
        do i=1,nreduce_opt
           write(luntty,'(2x,a,6x,L1)') reduce_dims(i),reduce_active(i)
        enddo

        write(luntty,*) ' '
        write(luntty,*) ' options:'
        write(luntty,*) '  <dimension-name> to ACTIVATE reduction operator'
        write(luntty,*) '  -<dimension-name> to DEactivate reduction operator'
        write(luntty,*) '     use a listed item for <dimension-name>, or,'
        write(luntty,*) '     "nspec" or "nsa" as prefix names.'
        write(luntty,*) ' '
        write(luntty,*) '  A  activate ALL'
        write(luntty,*) '  B  DEactivate ALL'
        write(luntty,*) '  D  for description of reduction operators'
        write(luntty,*) '  Q  quit (no further changes).'
        write(luntty,*) ' '

     endif

     call ureadw('ur_cstate_rcon: option selection:',dword)
     if(dword.eq.'Q') then
        exit

     else if(dword.eq.'A') then
        reduce_active = .TRUE.
        
     else if(dword.eq.'B') then
        reduce_active = .FALSE.

     else if(dword.eq.'D') then
        dword=reduce_dims(1)
        call ureadw( &
             'ur_cstate_rcon: select <dimension-name> or "*" for all:', &
             dword)

        ifound = 0

        if(dword.eq.'*') then
           do i=1,nreduce_opt
              dword = reduce_dims(i)
              call descr(dword)
           enddo

        else
           call ulower(dword)
           call descr(dword)

        endif

        if(ifound.gt.0) then
           write(luntty,*) ' '
           write(luntty,*) '   No name change if all states combined only have a'
           write(luntty,*) '   single species of a given category.'
           write(luntty,*) ' '
           write(luntty,*) '   If a given category is missing from all states, it'
           write(luntty,*) '   will be omitted.'
           write(luntty,*) ' '
        endif

     else if(dword(1:1).eq.'-') then
        call ulower(dword)
        call deactivate(dword(2:))

     else
        call ulower(dword)
        call activate(dword)

     endif
  enddo

  contains

    subroutine activate(dim)

      character*(*), intent(in) :: dim
      integer :: imatch

      call chk(dim,imatch)

      if(imatch.gt.0) reduce_active(imatch) = .TRUE.
      if(dim.eq.'nspec') then
         call chk('nspec_th',imatch)
         if(imatch.gt.0) reduce_active(imatch)=.TRUE.
         call chk('nspec_all',imatch)
         if(imatch.gt.0) reduce_active(imatch)=.TRUE.
      endif
      if(dim.eq.'nsa') then
         call chk('nsa_th',imatch)
         if(imatch.gt.0) reduce_active(imatch)=.TRUE.
         call chk('nsa_all',imatch)
         if(imatch.gt.0) reduce_active(imatch)=.TRUE.
      endif

    end subroutine activate

    subroutine deactivate(dim)

      character*(*), intent(in) :: dim
      integer :: imatch

      call chk(dim,imatch)

      if(imatch.gt.0) reduce_active(imatch) = .FALSE.
      if(dim.eq.'nspec') then
         call chk('nspec_th',imatch)
         if(imatch.gt.0) reduce_active(imatch)=.FALSE.
         call chk('nspec_all',imatch)
         if(imatch.gt.0) reduce_active(imatch)=.FALSE.
      endif
      if(dim.eq.'nsa') then
         call chk('nsa_th',imatch)
         if(imatch.gt.0) reduce_active(imatch)=.FALSE.
         call chk('nsa_all',imatch)
         if(imatch.gt.0) reduce_active(imatch)=.FALSE.
      endif

    end subroutine deactivate

    subroutine chk(chkdim,imatch)

      character*(*), intent(in) :: chkdim
      integer, intent(out) :: imatch

      integer :: i

      imatch = 0

      do i=1,nreduce_opt
         if(chkdim.eq.reduce_dims(i)) then
            imatch = i
            exit
         endif
      enddo

      if(imatch.eq.0) then
         call uermsg('dimension unrecognized or irreducible',chkdim,0)
      endif
    end subroutine chk

    subroutine descr(dim)

      character*(*), intent(in) :: dim
      integer :: imatch

      call chk(dim,imatch); if(imatch.eq.0) return

      write(luntty,*)
      write(luntty,*) ' ........... '//trim(dim)//':'

      if(dim.eq.'nsbeam') then
         ifound = ifound + 1
         write(luntty,*) ' combine together all beam ions -> "BEAM";'
      else if(dim.eq.'nsfusn') then
         ifound = ifound + 1
         write(luntty,*) ' combine together all fusion product ions -> "FUSN";'
      else if(dim.eq.'nrfmin') then
         ifound = ifound + 1
         write(luntty,*) ' combine together all RF minority ions; -> "MINO".'

      else if(dim.eq.'ngas') then
         ifound = ifound + 1
         write(luntty,*) ' combine all H isotopes -> "H_ISO".'
         write(luntty,*) ' combine all He isotopes -> "HE_ISO".'

      else if(dim.eq.'nspec_th') then
         ifound = ifound + 1
         write(luntty,*) ' combine impurity ions -> "IMP".'

      else if(dim.eq.'nsa_th') then
         ifound = ifound + 1
         write(luntty,*) ' combine impurity ions -> "IMP".'

      else if(dim.eq.'nspec_all') then
         ifound = ifound + 1
         write(luntty,*) ' combine impurity ions -> "IMP".'
         write(luntty,*) ' combine together all beam ions -> "BEAM";'
         write(luntty,*) ' combine together all fusion product ions -> "FUSN";'
         write(luntty,*) ' combine together all RF minority ions; -> "MINO".'

      else if(dim.eq.'nsa_all') then
         ifound = ifound + 1
         write(luntty,*) ' combine impurity ions -> "IMP".'
         write(luntty,*) ' combine together all beam ions -> "BEAM";'
         write(luntty,*) ' combine together all fusion product ions -> "FUSN";'
         write(luntty,*) ' combine together all RF minority ions; -> "MINO".'

      else if(dim.eq.'nbeam') then
         ifound = ifound + 1
         write(luntty,*) ' sum over neutral beams -> "NBI".'

      else if(dim.eq.'nicrf_src') then
         ifound = ifound + 1
         write(luntty,*) ' sum over ICRF antennas -> "ICRF".'

      else if(dim.eq.'nlhrf_src') then
         ifound = ifound + 1
         write(luntty,*) ' sum over LH antennas -> "LHRF".'

      else if(dim.eq.'necrf_src') then
         ifound = ifound + 1
         write(luntty,*) ' sum over ECRF antennas -> "ECRF".'

      else
         ifound = ifound + 1
         write(luntty,*) ' ?? unknown: ',trim(dim)
      endif

    end subroutine descr

end subroutine ur_cstate_rcon
