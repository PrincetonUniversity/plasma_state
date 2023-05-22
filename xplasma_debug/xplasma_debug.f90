subroutine xplasma_debug

  ! debugging tool for xplasma
  ! list contents, make displays, run tests

  ! ***this is not a module routine*** so the XPLASMA pointer is retrieved
  !   via eqi_intg_module

  use xplasma_definitions
  use eqi_intg_module

  use xplasma_debug_module

  implicit NONE

  type (xplasma), pointer :: s

  !----------------------------------
  ! debugging capability will be added for each new xplasma data type
  ! as appropriate
  !----------------------------------
  integer :: inum_lists,inum_coords,inum_grids,inum_profs,inum_blkbxs,ier
  !----------------------------------
  ! CL fortran interface

  integer :: luntty,lnout
  logical :: lxout,lchar

  !----------------------------------

  luntty = lnout(0)

  !----------------------------------
  s => sp_debug
  if(.not.associated(s)) then
     write(luntty,*) ' ?xplasma_debug:  received NULL xplasma object pointer.'
     return
  endif
  !----------------------------------

  do 

     call xplasma_num_items(s,ier, &
          num_lists=inum_lists, &
          num_coords=inum_coords, &
          num_grids=inum_grids, &
          num_profs=inum_profs, &
          num_blkbxs=inum_blkbxs)

     if(ier.ne.0) then
        call xplasma_error(s,ier,luntty)
        return
     endif

     if(lxout(0)) then
        write(luntty,*) ' xplasma_debug: xplasma contents summary:'
        write(luntty,*) '    ',inum_coords,' coordinates.'
        write(luntty,*) '    ',inum_grids,' grids.'
        write(luntty,*) '    ',inum_profs,' profiles.'
        write(luntty,*) '    ',inum_lists,' lists.'
        write(luntty,*) '    ',inum_blkbxs,' "black boxes".'
        write(luntty,*) ' enter:'
        write(luntty,*) '  "A": for special plots or tests.'
        write(luntty,*) '  "C": to examine coordinates.'
        write(luntty,*) '  "G": to examine grids.'
        write(luntty,*) '  "P": to examine profiles.'
        write(luntty,*) '  "L": to examine lists.'
        write(luntty,*) '  "Z": to examine black boxes.'
        write(luntty,*) ' '
        write(luntty,*) '  "S": to examine global scalar data.'
        write(luntty,*) '  "Q": to quit.'
     endif

     call uread('xplasma_debug_master: action option:')

     if(lchar('Q')) then
        exit

     else if(lchar('A')) then
        call xplasma_debug_special(s)

     else if(lchar('C')) then
        call xplasma_debug_coords(s,inum_coords)

     else if(lchar('G')) then
        call xplasma_debug_grids(s,inum_grids)

     else if(lchar('P')) then
        call xplasma_debug_profs(s,inum_profs)

     else if(lchar('L')) then
        call xplasma_debug_lists(s,inum_lists)

     else if(lchar('Z')) then
        call xplasma_debug_blkbxs(s,inum_blkbxs)

     else if(lchar('S')) then
        call examine_scalars

     else
        call uermsg('?xplasma_debug: unrecognized action option.',' ',0)
     endif

  enddo

  contains

    subroutine examine_scalars

      character*128 label0
      real*8 time
      logical :: axisymm,scrapeoff
      integer :: bccw,jccw
      real*8 bdytol
      integer :: rzOrder,iprof

      call xplasma_global_info(s,ier, &
           initLabel=label0, time=time, &
           axisymm=axisymm, scrapeoff=scrapeoff, &
           bphi_ccw=bccw, jphi_ccw=jccw, &
           bdytol=bdytol, rzOrder=rzOrder, prof_counter = iprof)

      write(luntty,*) ' '
      write(luntty,*) ' xplasma global scalar information: '
      write(luntty,*) ' ---------------------------------- '

      write(luntty,*) ' general label: ',trim(label0)
      write(luntty,*) ' time (s): ',time

      write(luntty,*) ' axisymmetry flag: ',axisymm
      write(luntty,*) ' scrapeoff region flag: ',scrapeoff

      if(bccw.eq.1) then
         write(luntty,*) &
              ' direction of toroidal field:  counter-clockwise seen from above.'
      else if(bccw.eq.-1) then
         write(luntty,*) &
              ' direction of toroidal field:  clockwise seen from above.'
      else
         write(luntty,*) &
              ' direction of toroidal field:  unknown.'
      endif

      if(jccw.eq.1) then
         write(luntty,*) &
              ' direction of toroidal current:  counter-clockwise seen from above.'
      else if(jccw.eq.-1) then
         write(luntty,*) &
              ' direction of toroidal current:  clockwise seen from above.'
      else
         write(luntty,*) &
              ' direction of toroidal current:  unknown.'
      endif

      write(luntty,*) ' out-of-bounds tolerance for interpolations: ',bdytol
      write(luntty,*) ' default spline order for {R,Z}(rho,theta): ',rzOrder

      write(luntty,*) ' profile definitions counter: ',iprof

      write(luntty,*) ' '
      write(luntty,*) ' ---------------------------------- '
      write(luntty,*) ' '

    end subroutine examine_scalars
end subroutine xplasma_debug
