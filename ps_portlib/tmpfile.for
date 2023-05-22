C  tmpfile -- create a temporary filename, based on the current process id
C   mod dmc 5.11.98:  use pid instead of date-time
C
      subroutine tmpfile(zprefix,zfile,ilz)
c
      character*(*) zprefix  ! filename prefix (input)
      character*(*) zfile    ! temp filename (output)
      integer ilz            ! length of temp filename (output)
C
C  zprefix can just be the first part of a filename, or a
C  complete path spefication followed (optionally) by the first part
C  of a filename.
C----------------
C  local
C
      character*12 cpid
C
C----------------
C
      zfile=' '
      ilzf=len(zfile)
C
      if(zprefix.eq.' ') then
         ilzp=0
      else
         ilzp=len_trim(zprefix)
      endif
C
      call sget_pid_str(cpid,ilcp)
C
      if(ilzp.eq.0) then
         zfile = cpid(1:ilcp)//'.tmp'
      else
         zfile = zprefix(1:ilzp)//cpid(1:ilcp)//'.tmp'
      endif
C
      ilz=len_trim(zfile)
C
      return
      end
C-----------------------------------------------------------------
      subroutine tmpfile_d(zprefix,zfile,ilz)
C
C  create a temporary file in a temporary files directory:
C
C     TMPDIR_TR if defined, or else ./tmp/
C
      character*(*) zprefix  ! filename prefix (input)
      character*(*) zfile    ! temp filename (output)
      integer ilz            ! length of temp filename (output)
C
C  local:
C
      character*512 zpath
C
      integer istat
C--------------
C
      zfile=' '
C
      call sget_env('TMPDIR_TR',zpath,iertmp)
      ilpath=len_trim(zpath)
      if(ilpath.eq.0) then
         call execute_command_line('mkdir -p ./tmp',exitstat=istat) ! istat not checked
         zpath(1:6)='./tmp/'
         ilpath=6
      else
         if(zpath(ilpath:ilpath).ne.'/') then
            ilpath=ilpath+1
            zpath(ilpath:ilpath)='/'
         endif
      endif
C
      ilzp=len_trim(zprefix)
      if(ilzp.gt.0) then
         zpath=zpath(1:ilpath)//zprefix(1:ilzp)
         ilpath=len_trim(zpath)
      endif
C
      call tmpfile(zpath(1:ilpath),zfile,ilz)
C
      return
      end
