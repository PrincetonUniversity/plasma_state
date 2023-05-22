subroutine ur_cstate_nchk(isel)

  !  Print density data, state #isel

  use cstate_lib_mod
  implicit NONE

  integer, intent(in) :: isel  ! selected state (btw 1 & nsss_cur)

  !------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar
  !------------------------

  luntty = lnout(0)

  do 
     if(lxout(0)) then
        write(luntty,*) ' A -- examine thermal species data'
        write(luntty,*) ' B -- examine beam species data'
        write(luntty,*) ' C -- examine fusion product species data'
        write(luntty,*) ' D -- examine RF minority data'
        write(luntty,*) ' Q -- quit'
     endif

     call uread('ur_cstate_nchk: menu option:')

     if(lchar('Q')) then
        exit

     else if(lchar('A')) then
        if(.NOT.allocated(sss(isel)%ns)) then
           write(luntty,*) ' %ur_cshate_nchk: no thermal densities available.'
           cycle
        endif

        call display(sss(isel)%rho,sss(isel)%m_s/ps_mp,sss(isel)%q_s/ps_xe, &
             sss(isel)%qatom_s/ps_xe,sss(isel)%ns,sss(isel)%s_name)

     else if(lchar('B')) then
        if(.NOT.allocated(sss(isel)%nbeami)) then
           write(luntty,*) ' %ur_cshate_nchk: no beam ion densities available.'
           cycle
        endif

        call display(sss(isel)%rho_nbi,sss(isel)%m_snbi/ps_mp, &
             sss(isel)%q_snbi/ps_xe, &
             sss(isel)%qatom_snbi/ps_xe,sss(isel)%nbeami,sss(isel)%snbi_name)

     else if(lchar('C')) then
        if(.NOT.allocated(sss(isel)%nfusi)) then
           write(luntty,*) ' %ur_cshate_nchk: no fusion ion densities available.'
           cycle
        endif

        call display(sss(isel)%rho_fus,sss(isel)%m_sfus/ps_mp, &
             sss(isel)%q_sfus/ps_xe, &
             sss(isel)%qatom_sfus/ps_xe,sss(isel)%nfusi,sss(isel)%sfus_name)

     else if(lchar('D')) then
        if(.NOT.allocated(sss(isel)%nmini)) then
           write(luntty,*) ' %ur_cshate_nchk: no RF minority densities available.'
           cycle
        endif

        call display(sss(isel)%rho_icrf,sss(isel)%m_rfmin/ps_mp, &
             sss(isel)%q_rfmin/ps_xe, &
             sss(isel)%qatom_rfmin/ps_xe,sss(isel)%nfusi,sss(isel)%rfmin_name)

     else
        call uermsg('unrecognized menu option',' ',0)
        cycle
     endif

  enddo

CONTAINS

  subroutine display(rho,A,Z,Zatom,ns,names)
    real*8, dimension(:) :: rho
    real*8, dimension(:) :: A
    real*8, dimension(:) :: Z
    real*8, dimension(:) :: Zatom
    real*8, dimension(:,:) :: ns
    character*(*), dimension(:) :: names

    !-------------------------------
    integer :: inzon,ins,izon,is
    real*8 :: nzsum
    !-------------------------------

    inzon = size(ns,1)
    ins   = size(ns,2)

    do
       if(lxout(0)) then
          write(luntty,*) ' ...choose a grid zone index, btw 1 and ',inzon
          write(luntty,*) '    or "Q" or 0 to quit'
       endif

       izon = 0

       call ureadi('ur_cstate_nchk(display): choose zone index:',izon)
       izon= max(0,min(inzon,izon))

       if(izon.eq.0) exit

       nzsum = 0.0d0
       
       write(luntty,*) ' '
       write(luntty,'(A,i4,A,2(1x,1pe11.4))') '  Zone #',izon,' rho range: ',&
            rho(izon),rho(izon+1)
       write(luntty,*) ' '
       write(luntty,*) 'name         density     Z           Zatom       A           NZsum'
       write(luntty,*) '----------   ----------  ----------  ----------  ----------  ----------'
       do is=1,ins
          nzsum = nzsum + ns(izon,is)*Z(is)
          write(luntty,'(1x,a,1x,5(1x,1pe11.4))') names(is)(1:10),ns(izon,is),&
               Z(is),Zatom(is),A(is),NZsum
       enddo
    enddo

  end subroutine display

end subroutine ur_cstate_nchk
