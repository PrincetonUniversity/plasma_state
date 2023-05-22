subroutine cstate_chk_locks(luntty)

  ! print a table showing lock status of init sections of states
  ! (machine description, etc).

  use cstate_lib_mod
  implicit NONE

  integer, intent(in) :: luntty   ! LUN on which to write the table

  integer, parameter :: num_init_sects = 3
  character*32 :: init_sects(num_init_sects)
  character*8 :: lockstat(nsss_cur,num_init_sects)

  !-------------------------
  ! local

  integer :: ii,jj,ia,ilen_coln(0:nsss_cur),ia_coln(0:nsss_cur)

  character*256 :: hdr,rln,buf

  !-------------------------

  init_sects(1)='MACHINE_DESCRIPTION'
  init_sects(2)='SHOT_CONFIGURATION'
  init_sects(3)='SIMULATION_INIT'

  ia_coln(0)=1
  ilen_coln(0)=11          ! min length: enough for string "Lock Status".

  ilen_coln(1:nsss_cur)=8  ! min length: enough for string "unlocked".

  do ii=1,num_init_sects
     ilen_coln(0)=max(ilen_coln(0),len(trim(init_sects(ii))))
  enddo

  ilen_coln(0) = ilen_coln(0) + 1  ! pad 1st column by 1 space

  do ii=1,nsss_cur
     ilen_coln(ii)=max(ilen_coln(ii),len(trim(sss_label(ii))))
     ia_coln(ii)=ia_coln(ii-1) + ilen_coln(ii-1) + 1
     if(sss(ii)%lock_machine_descr.eq.ps_locked) then
        lockstat(ii,1)='LOCKED'
     else
        lockstat(ii,1)='unlocked'
     endif

     if(sss(ii)%lock_shot_config.eq.ps_locked) then
        lockstat(ii,2)='LOCKED'
     else
        lockstat(ii,2)='unlocked'
     endif

     if(sss(ii)%lock_sim_init.eq.ps_locked) then
        lockstat(ii,3)='LOCKED'
     else
        lockstat(ii,3)='unlocked'
     endif
  enddo

  hdr = 'Lock Status'
  rln = rline(1:ilen_coln(0))
  do ii=1,nsss_cur
     ia=ia_coln(ii)
     hdr(ia:)=trim(sss_label(ii))
     rln(ia:)=rline(1:ilen_coln(ii))
  enddo

  write(luntty,*) ' '
  write(luntty,*) trim(hdr)
  write(luntty,*) trim(rln)

  do jj=1,num_init_sects
     buf = init_sects(jj)
     do ii=1,nsss_cur
        ia=ia_coln(ii)
        buf(ia:)=lockstat(ii,jj)
     enddo
     write(luntty,*) trim(buf)
  enddo

  write(luntty,*) ' '

end subroutine cstate_chk_locks
