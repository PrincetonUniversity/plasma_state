subroutine ur_cstate_sprof

  ! examine profile data contained in the state(s)

  ! functionality includes:
  !   -- options to display available profiles and groups of profiles
  !      with labeling and descriptive information
  !   -- options to "reduce" profile sets (e.g. combine all profiles
  !      related to "impurities" into a single profile
  !   -- select profile or profile group for plotting
  !   -- choose data to plot: several profiles from one of the states, or,
  !      a compartive plot of one profile for all states that contain the 
  !      profile
  !   -- plot the original data, or interpolation, or rezoning
  !   -- option to plot volume- or area-integral where appropriate
  !   -- option to plot results of differential operators, e.g. <grad()> or
  !      scale lengths

  use cstate_lib_mod
  implicit NONE

  !-------------------------
  ! local

  integer :: luntty,lnout
  logical :: lxout,lchar,lyes
  integer :: icontents,iimap,indx,ind2,isize
  integer, dimension(:), allocatable :: indmap
  integer :: imin,imaxx,ilen_coln(0:nsss_cur),ilen0d
  integer :: isize_tot,ii,ierr,jsize,jj,kk,ia,ifloat,ngrids,ig,is,inx
  integer :: totsize,iminx,ifc
  integer :: iterms,iii,jjj,icount,icountp,ifirst,imaxn
  integer, dimension(:,:), allocatable :: addr_table

  integer :: ndims,idims(20),ilodims(20),igrids(20),idim,igrid,indx_enum

  character*32 :: dname,dunits,funits,iunits,pname,name1,group_name
  character*70 :: disp0,xlabel,xxunits,tmplabel,tmpunits,cmt,method
  real*8 :: pnorm,psum,pwght
  logical :: avg_flag,smflag,curdens

  character*32, dimension(:,:), allocatable :: val_field
  integer, dimension(:,:), allocatable :: sum_key
  integer, dimension(:,:,:), allocatable :: sum_table
  character*32, dimension(:), allocatable :: merge_names

  character*32, dimension(:), allocatable :: ctags,xunits,gnames
  integer, dimension(:,:), allocatable :: ids,igsizes,intargs
  real*8, dimension(:,:,:), allocatable :: xgrids,xgridz
  real*8, dimension(:,:,:), allocatable :: xgridt,xgridtz
  real*8, dimension(:,:), allocatable :: xrezon,xrezonz
  real*8, dimension(:), allocatable :: vfixed
  integer :: inum_rezone

  integer :: intflag,iexit

  real*8, save :: scalemax = 1000.0d0

  real*8, parameter :: ZERO=0.0d0

  logical :: metric_flag(nsss_cur),metric_any,metric_all

  integer, save :: ntarg_factor = 5

  !-------------------------
  !  we will need grid information to look at profiles...

  call psw_contents('G',ncur_components,wgrid,ierr, state=sss(1))
  if(ierr.ne.0) return

  !-------------------------

  luntty = lnout(0)
  
  icontents = 0
  iimap = 0
  
  ! gather information on which states include "metric data" -- i.e.  
  ! include an MHD equilibrium specification

  metric_any = .FALSE.
  metric_all = .TRUE.
  do ii=1,nsss_cur
     metric_flag(ii) = (sss(ii)%id_vol.gt.0).AND. &
          (sss(ii)%id_surf.gt.0).AND.(sss(ii)%id_area.gt.0)
     metric_any = metric_any.OR.metric_flag(ii)
     metric_all = metric_all.AND.metric_flag(ii)
  enddo
     
  ! control loop

  do
     if(lxout(0)) then
        write(luntty,1001) omit_empty,trim(cur_components),nprec

1001    format('ur_cstate_sprof options:'/ &
             '  A -- option to omit empty elements from display (',L1,')'/ &
             '  C -- select & restrict view to component(s) (current selection: ',a,')'/ &
             '  T -- table of contents (show units & dimensions)'/ &
             '  U -- table of contents (show component, units & interpolation method)'/ &
             '  V -- table of contents (show descriptions)'/ &
             '  R -- set list dimension reduction (summation) options'/ &
             '  S -- select a single profile item & examine in detail'/ &
             '  P -- reset precision for display of floating point data (', &
             i2,')'/ &
             '  L -- modify state label(s)'/ &
             '  X -- "xplasma" plots of individual state'/ &
             '  Y -- additional comparitive display options'/ &
             '       (limiter plots; quasineutrality, Zeff checks...)'/ &
             '  Q -- quit this menu'/)
     endif
     call uread('ur_cstate_sprof option:')

     if(lchar('Q')) then
        exit

     else if(lchar('X')) then
        call ur_cstate_xplasma_plots

     else if(lchar('Y')) then
        call ur_cstate_misc_plots

     else if(lchar('A')) then
        ! contents display option: by default, omit empty elements...

        if(lxout(0)) write(luntty,*) &
             ' option to omit display of elements that are empty in all states.'

        call uread('omit empty elements ? (Y/N):')
        omit_empty = lyes(0)

     else if(lchar('C')) then
        ! restrict view to subset of components, or, expand view to all 
        ! components...

        icontents = 0
        iimap = 0
        call ur_cstate_component

     else if(lchar('L')) then
        ! modify state labels
        call ur_cstate_setLabel

     else if(lchar('T')) then
        ! table of contents variant
        icontents = 1
        call ur_cstate_sprof_contents(1)

     else if(lchar('U')) then
        ! table of contents variant
        icontents = 1
        call ur_cstate_sprof_contents(2)

     else if(lchar('V')) then
        ! table of contents variant
        icontents = 1
        call ur_cstate_sprof_contents(3)

     else if(lchar('R')) then
        ! profile reduction options (e.g. sum over subgroups of species or
        ! across other dimensions such as beam lines)

        call ur_cstate_rcon

     else if(lchar('P')) then
        ! floating point precision control for scalar displays

        if(lxout(0)) then
           write(luntty,*) ' Floating point format: <sign>n.mm<esign>Exx'
           write(luntty,*) ' precision controls number of digits mm after the'
           write(luntty,*) ' decimal point; values btw 2 and 16 are accepted.'
        endif

        call ureadi('ur_cstate_sdata: floating point display precision:',nprec)
        nprec=max(2,min(16,nprec))

     else if(lchar('S')) then
        ! select profile (group) to plot...

        if(icontents.eq.0) then
           icontents = 1
           call ur_cstate_sprof_contents(0)
        endif

        ! check contents...
        isize = w1%size
        if(isize.eq.0) then
           write(luntty,*) &
                ' %ur_cstate_sdatea: selected components contain no profiles.'
           call uclear
           cycle
        endif

        ! map some names to container group name...
        if(iimap.eq.0) then
           if(allocated(indmap)) deallocate(indmap)
           allocate(indmap(isize))
           call cstate_gen_indmap(indmap,isize)
           iimap=1
        endif

        ! get profile (or profile group) name...
        if(lxout(0)) then
           write(luntty,*) '(unambiguous prefix of name may be entered).'
        endif
        call ureadw('ur_cstate_sprof: enter profile name or "." to skip:', &
             dname)
        if(dname.eq.'.') cycle

        call cstate_findm(dname,luntty,indx)
        if(indx.eq.0) then
           call uclear
           cycle
        endif

        ! map name to self or to container...
        ind2 = indmap(indx)

        dname = w1%list_elems(ind2)
        dunits = w1%elem_units(ind2)
        funits = dunits

        !--------------------------------------
        ! generate scalar display...
        !   (code is very similar to display code for scalar state variables)
        !--------------------------------------

        cmt = ' '
        pname = ' '
        avg_flag=.FALSE.
        curdens =.FALSE.
        method = w1%val_field(ind2)
        if(method(1:5).eq.'STEP*') then
           pname = method(6:)
           ii = index(pname,'(')
           if(ii.gt.0) then
              pname=method(6:6+ii-2)      ! note avg_flag NOT set
              !  pname weighting not conformal with table data to be averaged
              !  but the information can be used during plotting
           else if(pname.eq.'DA') then
              pname=' '
              curdens=.TRUE.
              funits = trim(dunits)//'/m^2'
           else if(pname.eq.'DV') then
              pname=' '
              funits = trim(dunits)//'/m^3'
           else
              avg_flag=.TRUE.
              call free_wwnorm
              allocate(wwnorm(nsss_cur))  ! weighted avg possible
              cmt='   ...'//trim(pname)//' weighted volume average.'
           endif
        endif

        ! minimum, maximum display lengths
        if(dunits.eq.'character') then
           imin=6
           ifloat=0
        else if(dunits.eq.'integer') then
           imin=10
           ifloat=0
        else
           imin=nprec + 8  ! floating point...
           ifloat=1
        endif

        isize_tot = 0
        icount = 0

        ! set minimum display column widths...
        do ii=1,nsss_cur
           icountp=icount
           ilen_coln(ii)=max(imin,len(trim(sss_label(ii))))

           call psw_plist(dname,ww(ii),xlabel,xxunits,ierr, state=sss(ii))
           if(ww(ii)%size.gt.0) then
              icount = icount + 1
              if(avg_flag) then
                 call psw_plist(pname,wwnorm(ii),tmplabel,tmpunits,ierr, &
                      state=sss(ii))
              endif
           endif
           if((icount.eq.1).and.(icountp.eq.0)) then
              ifirst=ii
              group_name=ww(ii)%list_name
              if(ifloat.eq.0) then
                 disp0 = trim(ww(ii)%list_name)
              else
                 disp0 = trim(ww(ii)%list_name)//' ('//trim(xxunits)//')'
              endif
              ilen_coln(0)=len(trim(disp0))

              ! also compute intflag here
              intflag = 0
              if(xlabel(1:8).eq.'vol int:') intflag = 1
              if(xlabel(1:9).eq.'area int:') intflag = 2
              if(xlabel(1:11).eq.'simple avg:') intflag = 3
              if((intflag.eq.1).or.(intflag.eq.2)) iunits=trim(xxunits)
           endif

           if(ww(ii)%size.eq.0) then
              write(luntty,*) ' empty list for state: '//trim(sss_label(ii))
           else
              isize_tot=isize_tot + ww(ii)%size
           endif
        enddo
        if(isize_tot.eq.0) then
           write(luntty,*) ' empty list.'
           cycle
        endif

        ! form merged list...

        call cstate_ck_listdims(w1%c256val(ind2),size(idims),ndims, &
             idims,ilodims,indx_enum)
        if(indx_enum.ne.0) then
           if(pname.ne.' ') then
              write(luntty,*) ' ?ur_cstate_sprof: not implemented yet:'
              write(luntty,*) '  viewing of weighted profile with enumeration dimension:'
              write(luntty,*) '   '//enum_dims(indx_enum)
              cycle
           endif
           icount=0
           do jj=1,nreduce_opt
              if(reduce_active(jj)) icount = icount + 1
           enddo
           if(icount.gt.0) then
              write(luntty,*) &
                   ' %ur_cstate_sprof: reduction sums not implemented for'
              write(luntty,*) &
                   '  quantities with enumeration dimensions.'
           endif
        endif

        if(indx_enum.ne.0) then
           call hck(dname)
           ! enumeration-dimensioned quantity: sort on the fly
           ! first calculate a safe upper limit (imaxn) on the size
           ! of the merged set of profiles
           imaxn = enum_max(indx_enum)*nsss_cur
           do iii=1,ndims
              idim=idims(iii)
              imaxn = imaxn*maxval(dimsizes(:,idim))
           enddo

           allocate(addr_table(nsss_cur,imaxn))
           call psw_merge(ww,wmerge,ierr, addr_table=addr_table)
           jsize=wmerge%size
           totsize=jsize
           allocate(val_field(nsss_cur,jsize))

           allocate(sum_table(0:1,nsss_cur,jsize)); sum_table = 0

           do jj=1,jsize
              name1=wmerge%list_elems(jj)
              ilen_coln(0)=max(ilen_coln(0),len(trim(name1)))
              do ii=1,nsss_cur
                 if(addr_table(ii,jj).eq.0) then
                    val_field(ii,jj)=' (absent)'
                 else
                    kk=addr_table(ii,jj)
                    sum_table(0,ii,jj)=1
                    sum_table(1,ii,jj)=kk
                    call cstate_encode(dunits,0,ww(ii)%fval(kk), &
                         ' ', val_field(ii,jj), ilen_coln(ii))
                 endif
              enddo
           enddo

        else if(ndims.eq.0) then

           ! singleton profile or group of singletons (no species list
           ! or item list indexing...); contents the same for all states.

           call clone_window(ww(ifirst),wmerge)
           jsize = wmerge%size
           totsize=jsize
           allocate(val_field(nsss_cur,jsize))

           allocate(sum_table(0:1,nsss_cur,jsize)); sum_table = 0

           do jj=1,jsize
              name1=ww(ifirst)%list_elems(jj)
              call hck(name1)
              ilen_coln(0)=max(ilen_coln(0),len(trim(name1)))
              do ii=1,nsss_cur
                 if(jj.gt.ww(ii)%size) then
                    val_field(ii,jj)=' (absent)'
                 else
                    if(ww(ii)%ival(jj).eq.0) then
                       val_field(ii,jj)=' (absent)'
                    else
                       sum_table(0,ii,jj)=1
                       sum_table(1,ii,jj)=jj
                       call cstate_encode(dunits,0,ww(ii)%fval(jj),&
                            ' ', val_field(ii,jj), ilen_coln(ii))
                    endif
                 endif
              enddo
           enddo
        else
           call hck(dname)
           ! separate dimensioning for each state

           call free_window(wmerge)
           call cstate_merge_size(ndims,idims(1:ndims),ilodims(1:ndims),jsize)
           allocate(sum_table(0:jsize,nsss_cur,jsize))
           allocate(sum_key(ndims,jsize))
           allocate(merge_names(jsize))

           call cstate_merge_addr( &
                ndims,idims(1:ndims),ilodims(1:ndims),jsize,dname,dunits,0, &
                sum_table,sum_key,merge_names,totsize)

           allocate(val_field(nsss_cur,totsize))

           wmerge%size = totsize
           allocate(wmerge%list_elems(totsize))
           wmerge%list_elems(1:totsize) = merge_names(1:totsize)

           do ii=1,nsss_cur
              do kk=1,totsize
                 if(sum(sum_key(1:ndims,kk)).eq.0) then
                    ! no summation
                    jj=sum_table(1,ii,kk)
                    if(jj.eq.0) then
                       val_field(ii,kk)=' (absent)'
                    else
                       call cstate_encode(dunits, &
                            0,ww(ii)%fval(jj),' ', val_field(ii,kk), &
                            ilen_coln(ii))
                       ilen_coln(0)=max(ilen_coln(0), &
                            len(trim(wmerge%list_elems(kk))))
                    endif
                 else
                    psum=ZERO
                    pnorm=1.0d0
                    if(cmt.eq.' ') then
                       cmt='   ...summation(s) applied.'
                    endif
                    iterms=sum_table(0,ii,kk)

                    if(iterms.eq.0) then
                       val_field(ii,kk)=' (absent)'

                    else
                       do iii=1,iterms
                          jjj=sum_table(iii,ii,kk)
                          if(avg_flag) then
                             pwght=max(ZERO,wwnorm(ii)%fval(jjj))
                             pnorm=pnorm+pwght
                             psum=psum+pwght*ww(ii)%fval(jjj)
                          else
                             psum=psum+ww(ii)%fval(jjj)
                             if((intflag.eq.3).and.(iii.gt.1)) then
                                pnorm=pnorm+1.0d0  ! simple avg
                             endif
                          endif
                       enddo

                       if(pnorm.gt.ZERO) psum = psum/pnorm

                       call cstate_encode(dunits, &
                            0,psum,' ', val_field(ii,kk), ilen_coln(ii))
                       ilen_coln(0)=max(ilen_coln(0), &
                            len(trim(wmerge%list_elems(kk))))
                    endif
                 endif
              enddo
           enddo

        endif

        !  display header...

        ! display data and offer plotting options...

        call cstate_ck_grids(w1%c256val(ind2),size(igrids),ngrids,igrids)

        allocate(ctags(ngrids),xunits(ngrids),gnames(ngrids), &
             ids(ngrids,nsss_cur),igsizes(ngrids,nsss_cur))

        call cstate_grid_info(luntty,ngrids,igrids(1:ngrids), &
             gnames,ids,igsizes,ctags,xunits)

        imaxx=maxval(igsizes)

        allocate(xgrids(imaxx,ngrids,nsss_cur)); xgrids = ZERO
        allocate(xgridz(imaxx-1,ngrids,nsss_cur)); xgridz = ZERO

        iminx=imaxx
        do is=1,nsss_cur
           do ig=1,ngrids
              inx=igsizes(ig,is)
              if(inx.gt.0) then
                 call ps_get_grid_by_name(gnames(ig), ierr, &
                      state = sss(is), gdata=xgrids(1:inx,ig,is))
                 xgridz(1:inx-1,ig,is) = &
                      (xgrids(1:inx-1,ig,is)+xgrids(2:inx,ig,is))/2
                 iminx=min(iminx,inx)
              endif
           enddo
        enddo

        !  Summary data display:

        call print_summary

        !  Plotting options

        if(ngrids.le.2) then

           call f_plot
              
        else if(ngrids.eq.3) then
           write(luntty,*) 'plot 3d f(x,y,z) not implemented.'
           iexit=1

        else
           write(luntty,*) 'plot profiles of rank > 3 not implemented.'
           iexit=1
        endif

        deallocate(val_field)
        if(allocated(sum_table)) deallocate(sum_table)
        if(allocated(sum_key)) deallocate(sum_key)
        if(allocated(merge_names)) deallocate(merge_names)
        if(allocated(ctags)) deallocate(ctags,xunits,gnames,ids,igsizes)
        if(allocated(xgrids)) deallocate(xgrids,xgridz)
        if(allocated(addr_table)) deallocate(addr_table)

        call free_wwnorm

     else
        call uermsg(' ?ur_cstate_sprof: unrecognized option.',' ',0)
     endif
  enddo

  contains

    subroutine hck(name)
      character*(*), intent(in) :: name

      integer :: ihash1(nsss_cur),ihash2(nsss_cur)
      integer :: iii,iaddr

      call plasma_state_hash_addr("PROFILE",name,iaddr)

      if(iaddr.gt.0) then
         do iii=1,nsss_cur
            ihash1(iii) = sss(iii)%hash_codes(iaddr)
            ihash2(iii) = sss(iii)%hash_codes(iaddr+1)
         enddo

         call cstate_hash_compare(trim(name)//' profiles',ihash1,ihash2)
      endif
    end subroutine hck

    subroutine print_summary

      integer :: ivd2

      ivd2 = size(val_field,2)

      call cstate_table_out(disp0,xlabel,cmt, &
           ilen_coln,1,val_field,ivd2, &
           0,totsize)

    end subroutine print_summary

    subroutine f_plot
      
      !  f(x) and f(x,y) plotting...
      !  if ngrids = 1 and ctags(1)='rho':  f(rho) plotting...

      !-------------------
      character*70 :: cur_select
      character*32 :: curname,rzunits,yyunits
      logical :: iselect,loc_metric,ix_fixed,f_rho_plot
      integer :: kselect,imatch,ix,ixi,in,iopt

      integer :: imaxtarg,intarg,invar,irezone,imaxvar,inactiv

      character*32 :: fixlab,zgrid,ztest,xfix1
      character*32 :: varlab(max(totsize,nsss_cur))
      character*32 :: vlbsel(max(totsize,nsss_cur))
      character*10 :: zdigs = '0123456789'

      character*17 kylab(5)
      character*60 xfixall
      character*9 zdate

      integer :: iself(max(totsize,nsss_cur))
      integer :: jself(max(totsize,nsss_cur))
      integer :: inmax,inums(max(totsize,nsss_cur))
      integer :: inumc(max(totsize,nsss_cur))

      real*8 :: zz(max(totsize,nsss_cur))
      real*8 :: vmin1,vmax1

      logical :: inrange(nsss_cur),imrange(max(totsize,nsss_cur))
      logical :: isactiv(max(totsize,nsss_cur))

      integer :: isa(max(totsize,nsss_cur)),iva(max(totsize,nsss_cur)),iv

      real*8, dimension(:,:), allocatable :: xxplot,yyplot

      character*1 :: zgnums(8) = (/ '1', '2', '3', '4', '5', '6', '7', '8' /)
      !-------------------

      call c9date(zdate)

      cur_select = 'NONE'
      iselect = .FALSE.

      f_rho_plot = .FALSE.
      if(ngrids.eq.1) then
         ix_fixed = .TRUE.
         f_rho_plot = (ctags(1).eq.'rho') 
         inrange = .TRUE.
         imrange = .TRUE.
         igrid = 1
      else
         ix_fixed = .FALSE.
         inrange = .FALSE.
         imrange = .FALSE.
         igrid = 0
         if(allocated(vfixed)) deallocate(vfixed)
         allocate(vfixed(ngrids))
      endif

      xfixall = ' '
      kselect = 0
      imatch = 0
      curname = ' '
      isa = 0
      iva = 0
      inmax = 0

      ! x grid name & units: ctags(1); xunits
      ! data units: funits (integrated: rzunits).

      imaxvar=max(totsize,nsss_cur)
      do ii=1,imaxvar
         iself(ii)=ii
         zz(ii)=ii
      enddo

      kylab = ' '
      mnlab(2)=' '
      do
         !-------------------------------------------------------------
         ! metric flag (enable integro-differential transforms if true)
         !   for f(rho) profiles ONLY...

         if(kselect.gt.0) then
            loc_metric=f_rho_plot .AND. metric_flag(kselect)
         else
            loc_metric=f_rho_plot .AND. metric_all
         endif

         !-------------------------------------------------------------
         ! rezone options: f(rho) plots only...

         if(f_rho_plot.and.loc_metric) then
            if(pname.ne.' ') then
               irezone=3
               rzunits = funits
            else if(intflag.eq.1) then
               irezone=1
               rzunits = iunits
            else if(intflag.eq.2) then
               irezone=2
               rzunits = iunits
            endif
         else
            irezone=0
         endif

         !-------------------------------------------------------------
         ! interpolation target grids (redo: ntarg_factor may change)

         if(allocated(xgridt)) deallocate(xgridt,xgridtz,intargs)
         if(allocated(xrezon)) deallocate(xrezon,xrezonz)

         imaxtarg=(imaxx-1)*ntarg_factor + 1
         allocate(xgridt(imaxtarg,ngrids,nsss_cur)); xgridt = ZERO
         allocate(xgridtz(imaxtarg-1,ngrids,nsss_cur)); xgridtz = ZERO
         allocate(intargs(ngrids,nsss_cur))

         if(f_rho_plot.and.loc_metric) then

            !-----------------
            ! rezone & integration target grid here...
            !-----------------

            if(nrezon_rho.eq.0) then
               inum_rezone=iminx
            else
               inum_rezone=nrezon_rho
            endif
            allocate(xrezon(inum_rezone,ngrids),xrezonz(inum_rezone-1,ngrids))
            ig=1  ! assume ngrids = 1 & f(rho) for xrezon
            xrezon(1,ig)=ZERO
            do ix=2,inum_rezone
               xrezon(ix,ig)=(ix-1)*1.0d0/(inum_rezone-1)
               xrezonz(ix-1,ig)=(xrezon(ix,ig)+xrezon(ix-1,ig))/2
            enddo
         endif

         do is=1,nsss_cur
            do ig=1,ngrids
               inx=igsizes(ig,is)
               if(inx.eq.0) cycle
               ixi=1
               xgridt(1,ig,is)=xgrids(1,ig,is)
               do ix=2,inx
                  do in=1,ntarg_factor
                     ixi=ixi+1
                     xgridt(ixi,ig,is)=xgrids(ix-1,ig,is) + in* &
                          (xgrids(ix,ig,is)-xgrids(ix-1,ig,is))/ntarg_factor
                     xgridtz(ixi-1,ig,is)= &
                          0.5d0*(xgridt(ixi-1,ig,is)+xgridt(ixi,ig,is))
                  enddo
               enddo
               intargs(ig,is)=ixi
            enddo
         enddo

         if(iselect.and.ix_fixed) then

            icount = 0
            do ii=1,invar
               imrange(ii)=inrange(isa(ii))
               if(imrange(ii)) icount = icount + 1
            enddo

            if(icount.eq.0) then
               write(luntty,*) ' %CAUTION: no selected profiles in range of fixed coordinates.'
            endif
         endif

         !-------------------------------------------------------------
         ! clear plotting information
         kylab(2:5)=' '
         if(igrid.eq.0) then
            mnlab(3) = trim(curname)
         else
            if(xfixall.eq.' ') then
               mnlab(3) = trim(curname)//' vs. '//trim(ctags(igrid))
            else
               mnlab(3) = trim(curname)//' vs. '//trim(ctags(igrid))// &
                    '; '//trim(xfixall)
            endif
         endif
         inmax = 0
         if(allocated(xxplot)) deallocate(xxplot)
         if(allocated(yyplot)) deallocate(yyplot)

         !-------------------------------------------------------------
         ! interactive menu options:
         !    data-to-plot must be selected before plots are made;

         !    if #grids > 1 , "F" option for specifying plotting grid
         !         and fixed values for non-plotting grids must be selected
         !         before plots are made.

         if(lxout(0)) then
            write(luntty,*) 'Data storage descriptor: '//trim(method)
            write(luntty,*) ' '

            write(luntty,*) &
                 ' 1 -- retype summary of currently selected data items.'
            write(luntty,*) &
                 ' S -- select data to plot, currently "'// &
                 trim(cur_select)//'".'

            if(ngrids.gt.1) then
               write(luntty,*) &
                    ' F -- fix value of 1 independent coordinate; plot vs. the other(s).'
               if(.not.ix_fixed) then
                  write(luntty,*) &
                       '      (selection has NOT been made).'
               else
                  write(luntty,*) &
                       '      plot vs. '//trim(ctags(igrid))// &
                       ' with fixed values:'
                  do ig=1,ngrids
                     if(ig.ne.igrid) then
                        write(luntty,'(10x,a," = ",1pe11.4,1x,a)') &
                             trim(ctags(ig)),vfixed(ig),trim(xunits(ig))
                     endif
                  enddo
               endif
            endif

            write(luntty,*) &
                 ' Q -- quit.'

            if(iselect.and.ix_fixed) then
               if(loc_metric.and.(intflag.eq.1)) write(luntty,*) &
                    ' I -- volume integral'
               if(loc_metric.and.(intflag.eq.2)) write(luntty,*) &
                    ' I -- area integral (current profile)'
               write(luntty,*) &
                    ' P -- plot data on native grid(s).'
               write(luntty,*) &
                    ' M -- plot data interpolated (mapped) to target grid.'
               write(luntty,*) &
                    ' N -- reset interpolation or rezone grid resolutions:'
               write(luntty,*) '      interpolation target grid size: ', &
                    ntarg_factor,' * [state grid size];'
               if(loc_metric) write(luntty,*) &
                    '      rezone grid size: ',inum_rezone
               if(loc_metric) write(luntty,*) &
                    ' D -- derivative plot options (gradient, scale length).'
               write(luntty,*) &
                    ' X -- option to exclude selected curves from plot.'
            endif
         endif

         !-------------------------------------------------------------
         !  user inputs option...

         call uread('ur_cstate_sprof: f(rho) plotting option:')

         !-------------------------------------------------------------

         if(lchar('Q')) then
            iexit = 1        ! all done
            exit

         else if(lchar('1')) then
            call print_summary

         !-------------------------------------------------------------
         else if(lchar('S')) then

            !  select item(s) to plot

            if(lxout(0)) then
               write(luntty,*) ' A -- plot ALL data for a SINGLE state.'
               write(luntty,*) ' S -- plot a SINGLE data item for ALL states.'
               write(luntty,*) ' Q -- quit: do not change current selection.'
            endif
            call uread('ur_cstate_sprof: plot data selection option:')

            if(lchar('Q')) then
               continue

            else if(lchar('A')) then
               !-------------------------------------------------------------
               !  all items for a single state; it could be there are none.

               if(lxout(0)) then
                  write(luntty,*) 'choose a state number btw 1 and ', &
                       nsss_cur,':'
                  do ii=1,nsss_cur
                     write(luntty,'(4x,i2,".  ",a)') ii,trim(sss_label(ii))
                  enddo
               endif
               call ureadi('state selection number:',kselect)
               kselect=min(nsss_cur,max(1,kselect))

               fixlab = trim(sss_label(kselect))
               cur_select = 'all items in state '//trim(sss_label(kselect))
               write(luntty,*) ' selected:  '//trim(cur_select)

               isa = kselect

               invar = 0
               do jj=1,totsize
                  if(val_field(kselect,jj).ne.' (absent)') then
                     invar = invar + 1
                     iva(invar)=jj
                     varlab(invar) = wmerge%list_elems(jj)
                     write(luntty,*) '     '//trim(varlab(invar))
                     isactiv(invar) = .TRUE.
                  endif
               enddo

               if(invar.eq.0) then
                  write(luntty,*) ' '
                  write(luntty,*) ' No such items available...'
                  write(luntty,*) ' '
                  iselect = .FALSE.
                  kselect = 0
                  imatch = 0
                  cur_select = 'NONE'
                  isa = 0
                  iva = 0
               else
                  iselect = .TRUE.
                  kylab(1) = 'profile names'
                  mnlab(2) = ' '
                  write(mnlab(2),'("state label: ",a," t1(sec): ",1pe11.4)') trim(sss_label(kselect)),sss(kselect)%t1
               endif
               curname = group_name

            else if(lchar('S')) then
               !-------------------------------------------------------------
               !  a single item for all states; at least 1 exists...

               if(lxout(0)) then
                  write(luntty,*) 'enter the name of the data item to plot.'
                  write(luntty,*) '  e.g.: '//trim(wmerge%list_elems(1))
                  write(luntty,*) 'or enter the number of the item:'
                  do jj=1,totsize
                     write(luntty,'(4x,i2,".  ",a)') jj,trim(wmerge%list_elems(jj))
                  enddo
               endif

               if(curname.eq.' ') curname = wmerge%list_elems(1)
               call ureadw('data item name:',curname)

               imatch=0
               do ii=1,wmerge%size
                  if(curname.eq.wmerge%list_elems(ii)) then
                     imatch=ii
                     exit
                  endif
               enddo

               if(imatch.eq.0) then
                  do ii=1,len(trim(curname))
                     jj=index(zdigs,curname(ii:ii))
                     if(jj.lt.1) then
                        imatch=0
                        exit  ! not a positive integer
                     else
                        imatch = 10*imatch + (jj-1)
                     endif
                  enddo
               endif
               if(imatch.gt.totsize) imatch=0

               if(imatch.eq.0) then
                  call uermsg('data item name not in current list',curname,0)
               else

                  iselect = .TRUE.
                  kselect = 0
                  curname = wmerge%list_elems(imatch)
                  cur_select = trim(curname)//' in all states'
                  write(luntty,*) ' selected:  '//trim(cur_select)

                  fixlab = curname
                  kylab(1) = 'state labels'
                  mnlab(2) = ' '

                  iva = imatch
                  invar = 0
                  do ii=1,nsss_cur
                     if(val_field(ii,imatch).ne.' (absent)') then
                        invar = invar + 1
                        isa(invar) = ii
                        varlab(invar) = sss_label(ii)
                        write(luntty,*) '     '//trim(varlab(invar))
                        isactiv(invar) = .TRUE.
                     endif
                  enddo

               endif
            endif

         !-------------------------------------------------------------
         else if(lchar('F')) then

            !  select grid over which to plot; select fixed values for all
            !  other grids...

            if(ngrids.eq.1) then
               call uermsg(' %ur_cstate_sprof (f_plot): no "F" option; there is only one grid.',' ',0)
               cycle
            endif

            if(lxout(0)) then
               write(luntty,*) ' available grid coordinates (choose 1 for x axis of plot):'
               do ii=1,ngrids
                  write(luntty,'(6x,i2,2x,a)') ii,ctags(ii)
               enddo
            endif

            call ureadw('choose one of these grids by tag name or number:', &
                 zgrid)

            call uupper(zgrid)

            ig=0
            do ii=1,ngrids
               ztest=ctags(ii)
               call uupper(ztest)
               if(ztest.eq.zgrid) then
                  ig=ii
                  exit
               endif
               if(zgrid.eq.zgnums(ii)) then
                  ig=ii
                  exit
               endif
            enddo

            if(ig.eq.0) then
               call uermsg(' input does not match any available grid',zgrid,0)
               cycle
            endif

            ! OK: x axis chosen:

            igrid = ig
            ix_fixed = .TRUE.

            !-----------------------------
            ! choose fixed values for coordinates NOT plotted
            
            vfixed = ZERO
            xfixall = ' '

            inrange = .TRUE.

            do ig=1,ngrids
               if(ig.ne.igrid) then
                  do
                     icount=0
                     do is=1,nsss_cur
                        inx=igsizes(ig,is)
                        if(lxout(0).and.inrange(is)) then
                           write(luntty,'(a,1x,1pe11.4," to ",1pe11.4,1x,a)') &
                                trim(sss_label(is))//' '//trim(ctags(ig))// &
                                ' range: ',xgrids(1,ig,is),xgrids(inx,ig,is), &
                                trim(xunits(ig))
                        endif
                        if(inrange(is)) then
                           icount=icount+1
                           if(icount.eq.1) then
                              vmin1=xgrids(1,ig,is)
                              vmax1=xgrids(inx,ig,is)
                           else
                              vmin1=min(vmin1,xgrids(1,ig,is))
                              vmax1=max(vmax1,xgrids(inx,ig,is))
                           endif
                        endif
                     enddo

                     if(lxout(0)) write(luntty,*) ' '

                     call ureadr8('enter fixed '//trim(ctags(ig))//' value:', &
                          vfixed(ig))

                     if((vfixed(ig).lt.vmin1).or.(vfixed(ig).gt.vmax1)) then
                        write(luntty, &
                             '(" value ",1pe11.4," not in range ",1pe11.4," to ",1pe11.4,".")') &
                             vfixed(ig),vmin1,vmax1
                        call uclear
                        cycle
                     endif

                     xfix1=' '
                     write(xfix1,'(1x,a,"=",1pe11.4,1x,a)') trim(ctags(ig)), &
                          vfixed(ig),trim(xunits(ig))
                     xfixall = trim(xfixall)//trim(xfix1)

                     do is=1,nsss_cur
                        inx=igsizes(ig,is)
                        if((vfixed(ig).lt.xgrids(1,ig,is)).or. &
                             (vfixed(ig).gt.xgrids(inx,ig,is))) then
                           write(luntty,*) ' %fixed value exceeds coordinate range for state: '//sss_label(is)
                           inrange(is) = .FALSE.
                        endif
                     enddo

                     exit
                  enddo
               endif
            enddo

         else if(lchar('X')) then

            do
               if(lxout(0)) then
                  write(luntty,*) &
                       '  #  label                          in range?  active?'
                  do iv=1,invar
                     write(luntty,'(1x,i2,2x,a,4x,L1,10x,L1)') &
                          iv,varlab(iv),imrange(iv),isactiv(iv)
                  enddo
                  write(luntty,*) ' '
                  write(luntty,*) ' enter "Q" or "0" to exit control loop.'
               endif

               inactiv=0
               do iv=1,invar
                  if(imrange(iv).and.isactiv(iv)) inactiv=inactiv+1
               enddo

               iv=0
               call ureadi('enter item # to toggle (de)activate item:',iv)
               if(iv.eq.0) exit

               if((iv.lt.0).or.(iv.gt.invar)) then
                  call uermsi('% item # not in range',iv,0)
                  cycle
               endif

               if(imrange(iv)) then
                  if(isactiv(iv)) then
                     if(inactiv.eq.1) then
                        call uermsg('% at least one item must remain active.',&
                             ' ',0)
                        cycle
                     endif
                     inactiv=inactiv-1
                  else
                     inactiv=inactiv+1
                  endif
               endif
               isactiv(iv) = .not.isactiv(iv)
            enddo

         else if(lchar('I')) then
            !  integrated plot option...

            if(.not.loc_metric) then
               call uermsg( &
                    'Integration plot unavailable: no flux surface metric.', &
                    ' ',0)
               cycle
            else if((intflag.lt.1).or.(intflag.gt.2)) then
               call uermsg( &
                    'Integration operation not defined for this data.', &
                    ' ',0)
               cycle
            endif

            if(intflag.eq.1) kylab(5)='volume integral'
            if(intflag.eq.2) kylab(5)='area integral'

            yyunits = rzunits
            iopt = 0
            inmax = inum_rezone
            allocate(xxplot(inmax,invar),yyplot(inmax,invar))

            do iv=1,invar
               call get1('I',xxplot(1:inmax,iv),yyplot(1:inmax,iv), &
                    isa(iv),iva(iv),iopt,inums(iv))
            enddo

         else if(lchar('P')) then
            !  basic plot option...

            yyunits = funits

            allocate(xxplot(imaxx,invar),yyplot(imaxx,invar))
            iopt = 0
            do iv=1,invar
               call get1('P',xxplot(1:imaxx,iv),yyplot(1:imaxx,iv), &
                    isa(iv),iva(iv),iopt,inums(iv))
            enddo
            inmax = size(xxplot,1)

         else if(lchar('M')) then
            !  interpolated or rezoned plot...

            smflag = .FALSE.
            yyunits = funits
            if(irezone.eq.0) then
               iopt = 1
               inmax=imaxtarg
            else
               if(lxout(0)) then
                  write(luntty,*) ' mapping options:'
                  write(luntty,*) '  I: standard interpolation.'
                  if(irezone.eq.1) then
                     write(luntty,*) '  R: rezone; volume integral conserved.'
                  else if(irezone.eq.2) then
                     write(luntty,*) '  R: rezone; area integral conserved.'
                  else 
                     write(luntty,*) '  R: rezone; '//trim(pname)// &
                          ' weighted volume integral conserved.'
                  endif
                  write(luntty,*) '  S: rezone with smoothing.'
                  write(luntty,*) '  Q: quit (no plot).'
               endif

               call uread('interpolation/rezone option:')

               if(lchar('I')) then
                  iopt = 1
                  inmax=imaxtarg
               else if(lchar('R')) then
                  iopt = 2
                  inmax=inum_rezone
               else if(lchar('S')) then
                  iopt = 2
                  inmax=inum_rezone
                  smflag=.TRUE.
               else if(lchar('Q')) then
                  cycle
               else
                  call uermsg('interpolation/rezone option not recognized.', &
                       ' ',0)
                  cycle
               endif
            endif

            if(iopt.eq.1) then
               kylab(5)='interpolation'

            else if(iopt.eq.2) then
               if(irezone.eq.1) then
                  kylab(5)='volume rezone'
               else if(irezone.eq.2) then
                  kylab(5)='area rezone'
               else if(irezone.eq.3) then
                  kylab(4)='weighted rezone:'
                  kylab(5)='  '//trim(pname)
               endif
               if(smflag) kylab(3)='(smoothed)'
            endif

            allocate(xxplot(inmax,invar),yyplot(inmax,invar))
            do iv=1,invar
               call get1('P',xxplot(1:inmax,iv),yyplot(1:inmax,iv), &
                    isa(iv),iva(iv),iopt,inums(iv))
            enddo

         else if(lchar('N')) then
            !  reset interpolation target grid resolution factor

            if(lxout(0)) then
               write(luntty,*) ' Interpolation target grids for plots'
               write(luntty,*) ' are made a fixed factor N finer in resolution'
               write(luntty,*) ' than the native grids of the data.'
               write(luntty,*) ' enter a new N, between 2 and 25...:'
            endif
            call ureadi('enter new resolution factor for interpolation:', &
                 ntarg_factor)
            ntarg_factor = max(2,min(25,ntarg_factor))

            if(lxout(0)) then
               write(luntty,*) ' The rezone target grid size is controlled'
               write(luntty,*) ' separately, and is also used for volume and'
               write(luntty,*) ' area integral plots.  Enter the desired size'
               write(luntty,*) ' (between 11 and 1001), or "0" to select the'
               write(luntty,*) ' size from the state with the smallest grid.'
            endif

            call ureadi('enter new rezone grid size or "0":',nrezon_rho)
            if(nrezon_rho.le.0) then
               nrezon_rho = 0
            else
               nrezon_rho = max(11,min(1001,nrezon_rho))
            endif

         else if(lchar('D')) then
            !  derivative operator

            if(.not.loc_metric) then
               call uermsg( &
                    'Derivative plots unavailable: no flux surface metric.', &
                    ' ',0)
               cycle
            endif
            do 
               iopt = 0
               if(lxout(0)) then
                  write(luntty,*) ' derivative options:'
                  write(luntty,*) '  D -- df/drho'
                  write(luntty,*) &
                       '  G -- flux surface averaged gradient <grad(f)>.'
                  write(luntty,*) '  I -- inverse scale length min(M,|<grad(f)>/f|).'
                  write(luntty,*) '  L -- scale length min(M,|f/<grad(f)>|).'
                  write(luntty,*) '  M -- reset value of M: ',scalemax
                  write(luntty,*) '  P -- df/dPsi'
                  write(luntty,*) '  Q -- quit (no plot).'
               endif

               call uread('choose option for derivative operator:')

               if(lchar('D')) then
                  iopt=4
                  yyunits = funits
                  kylab(4)= ' '
                  kylab(5)= 'd(f)/drho'

               else if(lchar('P')) then
                  iopt=5
                  yyunits = trim(funits)//'/(Wb/rad)'
                  kylab(4)= ' '
                  kylab(5)= 'd(f)/dPsi'

               else if(lchar('G')) then
                  iopt=1
                  yyunits = trim(funits)//'/m'
                  kylab(4)='(gradient)'
                  kylab(5)='=<grad(f)>'

               else if(lchar('I')) then
                  iopt=3
                  yyunits = '1/m'
                  kylab(4)='1/(Scale Length)'
                  kylab(5)='=|<grad(f)>/f|'

               else if(lchar('L')) then
                  iopt=2
                  yyunits = 'm'
                  kylab(4)='(Scale Length)'
                  kylab(5)='=|f/<grad(f)>|'

               else if(lchar('M')) then

                  if(lxout(0)) then
                     write(luntty,*) ' M is in (m) for Scale Length;'
                     write(luntty,*) ' M is also in (1/m) for Inverse Scale Length.'
                  endif

                  call ureadr8( &
                       'enter new scale length limit M, or 0 to restore default:', &
                       scalemax)

                  if(scalemax.le.ZERO) scalemax = 1000.0d0
                  scalemax = min(1.0d10,scalemax)

               else if(lchar('Q')) then
                  exit
               else
                  call uermsg('derivative option unrecognized.',' ',0)
                  cycle
               endif

               if(iopt.eq.0) exit

               if((method(1:4).eq.'STEP').or.(method(1:5).eq.'PCLIN').or. &
                    (pname.ne.' ')) then
                  inmax=imaxx            ! finite difference on orig. grids.

               else
                  inmax=imaxtarg         ! interpolated derivative 
               endif

               allocate(xxplot(inmax,invar),yyplot(inmax,invar))
               do iv=1,invar
                  call get1('D',xxplot(1:inmax,iv),yyplot(1:inmax,iv), &
                       isa(iv),iva(iv),iopt,inums(iv))
               enddo
               exit

            enddo
            
         else
            call uermsg(' ?f_plot (ur_cstate_sprof): invalid option code.',&
                 ' ',0)
            cycle
         endif

         if(inmax.gt.0) then

            call varlab_chk(varlab,invar)  ! define ifc

            ii = 0
            do iv=1,invar
               if(isactiv(iv).and.imrange(iv)) then
                  ii=ii+1
                  if(ii.eq.1) then
                     jself(ii)=-iv
                  else
                     jself(ii)=iv
                  endif
                  inumc(ii)=inums(iv)
                  vlbsel(ii)=varlab(iv)(ifc:)
               endif
            enddo

            inactiv=ii

            if(inactiv.eq.0) then

               write(luntty,*) ' %f_plot: no data available.'
               write(luntty,*) '  This can happen if none of the selected'
               write(luntty,*) '  profiles are in range of fixed coordinate'
               write(luntty,*) '  values.'

            else

               call r8_grfmm1(xxplot,yyplot,inmax,inumc,inactiv, &
                    zz,invar,jself,inactiv, &
                    xunits,yyunits,mnlab,kylab,vlbsel)

            endif

         endif
      enddo
      deallocate(xgridt,xgridtz,intargs)

    end subroutine f_plot

    subroutine get1(ctrl,xx,yy,istate,ivar,iopt,inum)

      ! fetch data for 1d profile or profile sum

      character*1 :: ctrl  ! "P" for plot, "I" for integration plot, ...
      real*8, dimension(:) :: xx   ! x axis to use
      real*8, dimension(:) :: yy   ! fetched data

      integer, intent(in) :: istate   ! state index
      integer, intent(in) :: ivar     ! profile sum index
      integer, intent(in) :: iopt     ! ctrl sub-option

      integer, intent(out) :: inum    ! #pts in data fetched

      real*8, dimension(:), allocatable :: yyloc,wght,wsum,xfixa
      real*8, dimension(:), allocatable :: xeval,vola,surfa,yyderiv,yydsum
      real*8, dimension(:), allocatable :: psieval,psideriv
      real*8 :: ygrad,yval,ydiff,ydpsi

      integer :: is,insum,iv,id,idw,idpsi,ierr,inw,ix,i0,ict_zero
      logical :: findiff
      !---------------------------------

      xx = ZERO
      yy = ZERO
      i0 = 1

      if((ctrl.eq.'P').or.(ctrl.eq.'I')) then

         ! plot data itself or interpolation or rezone of data

         if(ctrl.eq.'I') then
            ! integration, done by rezone...
            inum = inum_rezone
            xx(1:inum) = xrezon(1:inum,igrid)
            i0 = 2

         else if(iopt.eq.2) then
            ! rezone...
            inum = inum_rezone - 1
            xx(1:inum) = xrezonz(1:inum,igrid)

         else if(method(1:4).eq.'STEP') then
            ! step function: interpolation or plot on original grid.
            if(iopt.eq.0) then
               inum = igsizes(igrid,istate) - 1
               xx(1:inum) = xgridz(1:inum,igrid,istate)
            else if(iopt.eq.1) then
               inum = intargs(igrid,istate) - 1
               xx(1:inum) = xgridtz(1:inum,igrid,istate)
            endif

         else
            ! other than step function: interpolation or plot on original grid.
            if(iopt.eq.0) then
               inum = igsizes(igrid,istate)
               xx(1:inum) = xgrids(1:inum,igrid,istate)
            else if(iopt.eq.1) then
               inum = intargs(igrid,istate)
               xx(1:inum) = xgridt(1:inum,igrid,istate)
            endif
         endif

         allocate(yyloc(inum),wght(inum),wsum(inum)); wsum = ZERO
         inw = 0

         insum = sum_table(0,istate,ivar)
         do is = 1,insum
            iv = sum_table(is,istate,ivar)
            id = ww(istate)%ival(iv)
            idw = sss(istate)%id_zonwt(id)
            if(idw.gt.0) then
               inw = inw + 1
            else
               wght = 1.0d0  ! weighting for simple sum
            endif

            if(ctrl.eq.'I') then
               ! rezone for integration
               call ps_rho_rezone(xrezon(1:inum_rezone,1), id, &
                    yyloc(2:inum_rezone), ierr, &
                    state=sss(istate), curdens=curdens, zonesmoo=smflag, &
                    nonorm=.TRUE.)

            else if(iopt.eq.2) then
               ! rezone
               call ps_rho_rezone(xrezon(1:inum_rezone,1), id, yyloc, ierr, &
                    state=sss(istate), curdens=curdens, zonesmoo=smflag)
            else
               ! interpolation
               if(ngrids.eq.1) then
                  call ps_intrp_1d(xx(1:inum), id, yyloc, ierr, &
                       state=sss(istate))
               else if(ngrids.eq.2) then
                  allocate(xfixa(inum))
                  if(igrid.eq.1) then
                     xfixa=vfixed(2)
                     call ps_intrp_2d(xx(1:inum), xfixa, id, yyloc, ierr, &
                          state=sss(istate))
                  else
                     xfixa=vfixed(1)
                     call ps_intrp_2d(xfixa, xx(1:inum), id, yyloc, ierr, &
                          state=sss(istate))
                  endif
                  deallocate(xfixa)
               endif
            endif

            if((idw.gt.0).and.(ierr.eq.0)) then
               if(ctrl.eq.'I') then
                  ! rezone for integration
                  call ps_rho_rezone(xrezon(1:inum_rezone,1), idw, &
                       wght(2:inum_rezone), ierr, &
                       state=sss(istate), curdens=curdens, zonesmoo=smflag, &
                       nonorm=.TRUE.)

               else if(iopt.eq.2) then
                  ! rezone
                  call ps_rho_rezone(xrezon(1:inum_rezone,1), &
                       idw, wght(1:inum), ierr, &
                       state=sss(istate), curdens=curdens, zonesmoo=smflag)
               else
                  ! interpolation
                  if(ngrids.eq.1) then
                     call ps_intrp_1d(xx(1:inum), idw, wght(1:inum), ierr, &
                          state=sss(istate))
                  else if(ngrids.eq.2) then
                     allocate(xfixa(inum))
                     if(igrid.eq.1) then
                        xfixa=vfixed(2)
                        call ps_intrp_2d(xx(1:inum), xfixa, idw, &
                             wght(1:inum), ierr, &
                             state=sss(istate))
                     else
                        xfixa=vfixed(1)
                        call ps_intrp_2d(xfixa, xx(1:inum), idw, &
                             wght(1:inum), ierr, &
                             state=sss(istate))
                     endif
                     deallocate(xfixa)
                  endif
               endif
            endif

            if(ierr.ne.0) then
               write(luntty,*) ' ?? ur_cstate_sprof: interpolation error.'
               yy = ZERO
               exit
            endif

            yy(i0:inum) = yy(i0:inum) + yyloc(i0:inum)*wght(i0:inum)
            wsum(i0:inum) = wsum(i0:inum) + wght(i0:inum)
         enddo

         !  renormalize weighted average if necessary

         if(inw.gt.0) then
            if(inw.ne.insum) then
               write(luntty,*) ' ?? ur_cstate_sprof: missing weights.'
               yy = ZERO
            else
               do ix=i0,inum
                  if(wsum(ix).gt.ZERO) then
                     yy(ix)=yy(ix)/wsum(ix)
                  else
                     yy(ix)=ZERO
                  endif
               enddo
            endif
         endif

         if(ctrl.eq.'I') then
            ! do the integration...
            yy(1)=ZERO
            do ix=i0,inum
               yy(ix)=yy(ix-1)+yy(ix)
            enddo
         endif

      else if(ctrl.eq.'D') then

         ! evaluate gradient or scale length;
         ! it is assumed that metric data is available; else this routine
         ! would not be called with this option.

         findiff = size(xx).le.imaxx

         if(findiff) then
            ! finite difference formula applies; evaluate function on orig.
            ! grid, possibly with weighting

            if(method(1:4).eq.'STEP') then
               inum=igsizes(igrid,istate)-2
               xx(1:inum)=xgrids(2:inum+1,igrid,istate)

               allocate(psieval(inum+1),xeval(inum+1),vola(inum+1),surfa(inum))
               xeval=xgridz(1:inum+1,igrid,istate)

            else
               inum=igsizes(igrid,istate)-1
               xx(1:inum)=xgridz(1:inum,igrid,istate)

               allocate(psieval(inum+1),xeval(inum+1),vola(inum+1),surfa(inum))
               xeval=xgrids(1:inum+1,igrid,istate)
            endif

            id = sss(istate)%id_vol
            call ps_intrp_1d(xeval,id,vola,ierr, state=sss(istate))
            if(ierr.ne.0) then
               write(luntty,*) &
                    ' ?ur_cstate_sprof (get1): error fetching metric: Vol(x)'
               return
            endif

            id = sss(istate)%id_surf
            call ps_intrp_1d(xx(1:inum),id,surfa,ierr, state=sss(istate))
            if(ierr.ne.0) then
               write(luntty,*) &
                    ' ?ur_cstate_sprof (get1): error fetching metric: Surf(x)'
               return
            endif

            idpsi = sss(istate)%id_psipol
            if(idpsi.gt.0) then
               call ps_intrp_1d(xeval,id,psieval,ierr, state=sss(istate))
               if(ierr.ne.0) then
                  write(luntty,*) &
                       ' ?ur_cstate_sprof (get1): error fetching Psipol(x)'
                  return
               endif
            else
               write(luntty,*) &
                    ' %ur_cstate_sprof (get1): no poloidal flux function'
               psieval = ZERO
            endif

            allocate(yyloc(inum+1),wght(inum+1),wsum(inum+1)); wsum = ZERO
            allocate(yydsum(inum+1)); yydsum = ZERO
            inw = 0

            insum = sum_table(0,istate,ivar)
            do is = 1,insum
               iv = sum_table(is,istate,ivar)
               id = ww(istate)%ival(iv)
               idw = sss(istate)%id_zonwt(id)
               if(idw.gt.0) then
                  inw = inw + 1
               else
                  wght = 1.0d0  ! weighting for simple sum
               endif

               call ps_intrp_1d(xeval, id, yyloc, ierr, &
                    state=sss(istate))

               if((idw.gt.0).and.(ierr.eq.0)) then

                  call ps_intrp_1d(xeval, idw, wght, ierr, &
                       state=sss(istate))

               endif
               if(ierr.ne.0) then
                  write(luntty,*) ' ?? ur_cstate_sprof: interpolation error during finite diff setup.'
                  yy = ZERO
                  exit
               endif
               
               yydsum = yydsum + yyloc*wght
               wsum = wsum + wght
            enddo

            !  renormalize weighted average if necessary

            if(inw.gt.0) then
               if(inw.ne.insum) then
                  write(luntty,*) ' ?? ur_cstate_sprof("D"): missing weights.'
                  yydsum = ZERO
               else
                  do ix=1,inum+1
                     if(wsum(ix).gt.ZERO) then
                        yydsum(ix)=yydsum(ix)/wsum(ix)
                     else
                        yydsum(ix)=ZERO
                     endif
                  enddo
               endif
            endif

            !  finite difference evaluate of gradient or scale length...

            do ix=1,inum

               yval = (yydsum(ix)+yydsum(ix+1))/2
               ygrad = (yydsum(ix+1)-yydsum(ix))*surfa(ix)/ &
                    (vola(ix+1)-vola(ix))
               ydiff = (yydsum(ix+1)-yydsum(ix))/(xeval(ix+1)-xeval(ix))
               if(idpsi.gt.0) then
                  ydpsi = (yydsum(ix+1)-yydsum(ix))/(psieval(ix+1)-psieval(ix))
               else
                  ydpsi = 0.0d0
               endif

               if(iopt.eq.1) then
                  ! return gradient
                  yy(ix)=ygrad

               else if(iopt.eq.2) then
                  ! return scale length
                  if(max(abs(yval),abs(ygrad)).eq.ZERO) then
                     yy(ix)=ZERO
                  else if(abs(yval).gt.scalemax*abs(ygrad)) then
                     yy(ix)=scalemax
                  else
                     yy(ix)=abs(yval)/abs(ygrad)
                  endif

               else if(iopt.eq.3) then
                  ! return inverse scale length
                  if(max(abs(yval),abs(ygrad)).eq.ZERO) then
                     yy(ix)=ZERO
                  else if(abs(ygrad).gt.scalemax*abs(yval)) then
                     yy(ix)=scalemax
                  else
                     yy(ix)=abs(ygrad)/abs(yval)
                  endif

               else if(iopt.eq.4) then
                  yy(ix)=ydiff

               else if(iopt.eq.5) then
                  yy(ix)=ydpsi

               endif

            enddo

         else
            ! derivative of unweighted Spline or Hermite, evaluated by
            ! interpolation

            inum=intargs(igrid,istate)-1
            xx(1:inum)=xgridtz(1:inum,igrid,istate)

            allocate(xeval(inum),vola(inum),surfa(inum))
            xeval = xx(1:inum)

            id = sss(istate)%id_vol
            call ps_intrp_1d(xeval,id,vola,ierr, state=sss(istate), &
                 ideriv=1)   ! dVol/dx
            if(ierr.ne.0) then
               write(luntty,*) &
                    " ?ur_cstate_sprof (get1): error fetching metric: Vol'(x)"
               return
            endif

            id = sss(istate)%id_surf
            call ps_intrp_1d(xeval,id,surfa,ierr, state=sss(istate))
            if(ierr.ne.0) then
               write(luntty,*) &
                    ' ?ur_cstate_sprof (get1): error fetching metric: Surf(x)'
               return
            endif

            allocate(yyloc(inum),yyderiv(inum),yydsum(inum),psideriv(inum))

            idpsi = sss(istate)%id_psipol
            if(idpsi.gt.0) then
               call ps_intrp_1d(xeval,idpsi,psideriv,ierr, state=sss(istate), &
                    ideriv=1)
               if(ierr.ne.0) then
                  write(luntty,*) &
                       ' ?ur_cstate_sprof (get1): error fetching dPsi/drho'
                  return
               endif
               !  make sure axial value is zero exactly...
               if(xeval(1).eq.ZERO) then
                  psideriv(1)=ZERO
                  ict_zero=0
               else
                  ict_zero=999999
               endif
            else
               write(luntty,*) &
                    ' %ur_cstate_sprof (get1): no poloidal flux function'
               psideriv = ZERO
               ict_zero=99
            endif

            yydsum = ZERO

            insum = sum_table(0,istate,ivar)

            do is = 1,insum
               iv = sum_table(is,istate,ivar)
               id = ww(istate)%ival(iv)
               idw = sss(istate)%id_zonwt(id)
               if(idw.gt.0) then
                  write(luntty,*) &
                       ' ?ur_cstate_sprof (get1): unexpected weight data.'
                  exit
               endif

               call ps_intrp_1d(xeval,id,yyloc,ierr, state=sss(istate))
               if(ierr.ne.0) then
                  write(luntty,*) &
                       ' ?ur_cstate_sprof (get1): f(x) interpolation error.'
                  yy = ZERO
                  exit
               endif

               call ps_intrp_1d(xeval,id,yyderiv,ierr, state=sss(istate), &
                    ideriv=1)
               if(ierr.ne.0) then
                  write(luntty,*) &
                       ' ?ur_cstate_sprof (get1): df/dx interpolation error.'
                  yy = ZERO
                  exit
               endif

               yy(1:inum)=yy(1:inum)+yyloc
               yydsum = yydsum + yyderiv
            enddo

            !  yy is the sum of f
            !  yydsum is the sum of df/dx

            do ix=1,inum
               ygrad=yydsum(ix)*surfa(ix)/vola(ix)
               yval=yy(ix)

               if(iopt.eq.1) then
                  ! return gradient
                  yy(ix)=ygrad

               else if(iopt.eq.2) then
                  ! return scale length
                  if(max(abs(yval),abs(ygrad)).eq.ZERO) then
                     yy(ix)=ZERO
                  else if(abs(yval).gt.scalemax*abs(ygrad)) then
                     yy(ix)=scalemax
                  else
                     yy(ix)=abs(yval)/abs(ygrad)
                  endif

               else if(iopt.eq.3) then
                  ! return inverse scale length
                  if(max(abs(yval),abs(ygrad)).eq.ZERO) then
                     yy(ix)=ZERO
                  else if(abs(ygrad).gt.scalemax*abs(yval)) then
                     yy(ix)=scalemax
                  else
                     yy(ix)=abs(ygrad)/abs(yval)
                  endif

               else if(iopt.eq.4) then
                  yy(ix)=yydsum(ix)

               else if(iopt.eq.5) then
                  if(psideriv(ix).eq.ZERO) then
                     yy(ix)=0
                     ict_zero=ict_zero+ix
                  else
                     yy(ix)=yydsum(ix)/psideriv(ix)
                  endif
               endif

            enddo

            if(iopt.ne.5) ict_zero = 0
            if(ict_zero.eq.999999) ict_zero = 0

            if(ict_zero.gt.1) then
               write(luntty,*) &
                    ' %ur_cstate_sprof: dPsi/drho = 0 away from axis.'
               write(luntty,*) &
                    '  df/dpsi not evaluated, where dPsi/drho = 0.'
            else if(ict_zero.eq.1) then
               write(luntty,*) &
                    ' %ur_cstate_sprof: patched on axis; dPsi -> 0.'
               yy(1) = yy(2) - (xeval(2)-xeval(1))*(yy(3)-yy(2))/ &
                    (xeval(3)-xeval(2))
            endif

         endif
      else
         write(luntty,*) ' ?? ur_cstate_sprof: get1 internal error: ctrl=',ctrl
      endif
    end subroutine get1

    subroutine free_wwnorm
      if(allocated(wwnorm)) then
         do ii=1,nsss_cur
            call free_window(wwnorm(ii))
         enddo
         deallocate(wwnorm)
      endif
    end subroutine free_wwnorm

    subroutine varlab_chk(varlab,invar)

      !  find common prefix among profile data labels
      !  (if labels are long, omit common prefix to improve readibility
      !  of multigraph key box)

      integer :: invar
      character*(*) :: varlab(invar)

      integer :: lenmax,lenmin,lenmatch
      integer :: ivar,ic,ilen,imatch

      ifc=1
      if(invar.eq.1) return

      lenmax=0
      lenmin=len(varlab(1))
      do ivar=1,invar
         ilen=len(trim(varlab(ivar)))
         lenmax=max(lenmax,ilen)
         lenmin=min(lenmin,ilen)
      enddo

      if(lenmax.le.12) return

      ! OK labels are rather long... see if there are identical prefixes
      ! that we will presume can be dropped...

      lenmatch=lenmin-1
      if(lenmatch.le.0) return

      do ivar=2,invar
         imatch=lenmatch
         do ic=1,lenmatch
            if(varlab(1)(ic:ic).ne.varlab(ivar)(ic:ic)) then
               imatch=ic-1
               exit
            endif
         enddo
         lenmatch=imatch
      enddo

      if(lenmatch.gt.3) ifc=lenmatch+1

    end subroutine varlab_chk

end subroutine ur_cstate_sprof

