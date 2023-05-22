      program plot_xplasma

!  read "xplasma" module from NetCDF file, and plot...

      character*128 filename
      integer i,ier,inum_lists

      character*32, dimension(:), allocatable :: list_names
      integer, dimension(:), allocatable :: list_ids

      logical lchar,lxout
!---------------------------------------------

      call uinitg("PLOT_XPLASMA")
      call urdext('tmi')

      do

         if(lxout(0)) then
            write(6,*) ' precede filename with "-" character to call'
            write(6,*) ' "example" subroutine which will try to read'
            write(6,*) ' and print TR_PBI data from the named file.'
         endif

         call ureadLw('plot_xplasma:  enter filename or "Q" to quit:$',
     >        filename)
!
         if((filename.eq.'q ').or.(filename.eq.'Q ')) then
            call uexita(0)
            stop
         endif

         if(filename(1:1).eq.'-') then
            call xplasma_example(filename(2:))
            cycle
         endif

         call eq_restore(filename,ier)
         if(ier.ne.0) then
            write(6,*) ' %plot_xplasma:  open failure:  ', filename

         else
            call eqdbg_plot     ! as before...

         endif

      enddo

      end
