program xplasma2_test

  ! Quick and dirty test program for xplasma2 module & library

  ! DMC Feb 2008: remove "*" format for floating point outputs

  use xplasma_obj_instance

  implicit NONE

  integer :: ier

  character*32 zname,zauthor
  character*75 zlabel
  integer :: id,ilun,iaxi,iscr,i,ioff
  real*8 ztime,ztime2,dmax

  character*32 elem_names(10),list_names(10)
  character*40 elem_labels(10)
  integer :: elem_ivals(10)
  real*8 :: elem_r8vals(10)

  integer :: id_list,id_listb,list_ids(10),ict(10),ict2(10)
  integer :: id_chi,id_rho,id_rho2,id_phi,ideriv,ideriv1,ideriv2,ideriv3
  integer :: inum,isize,igot,imax,itmax,itest

  integer, parameter :: inrho=21,inchi=41,inphi=35,inxtest=51
  real*8 zrho(inrho),zrho2(inrho+1),zchi(inchi),zphi(inphi)
  real*8 ztmp(max(inrho+1,inchi)),zsinx(inrho),zcosx(inrho),zsinx2(inrho+1)
  real*8 zsinx1(inrho+1),zcoeff1(inrho+1)
  real*8, parameter :: z2pi = 6.2831853071795862D+00
  real*8, parameter :: zero = 0.0d0
  real*8 :: ztol = 1.0d-8

  real*8 :: zxtest(inxtest)
  real*8 :: zans1(inxtest),zans2(inxtest),zans3(inxtest),zans4(inxtest,4)
  real*8 :: zscale,zfac,zpart1,zpart2

  integer :: id_fun,id_fun1,ix,ichi,iphi,irank,ispline,idum,icount
  integer :: id_funs,id_funsc,id_funh,id_funl
  integer :: ifuns(4)
  real*8 :: zdum

  real*8 :: x2d(9),th2d(9),vals2d(9,4),vals2da(9),valtmp2d(1,12)
  real*8 :: yy2d(9),yy2e(9)
  real*8 :: f2d(inrho,inchi)  ! exp(x)*sin(theta)
  real*8 :: f2dret(inrho,inchi),f2dcoeff(3,inrho,inchi)
  integer :: id_f2dlin,id_f2dherm,id_f2dspl
  real*8,parameter :: x2dcen=0.525d0,th2dcen=1.635d0,zd=1.0d-4

  real*8 :: x3d(27),th3d(27),ph3d(27),vals3d(27,4),vals3da(27),zsum1,zsum2
  real*8 :: yy3d(27),yy3e(27)
  real*8 :: f3d(inphi,inrho,inchi)  ! exp(x)*cos(theta)*sin(phi)
  real*8 :: f3dret(inphi,inrho,inchi),f3dcoeff(7,inphi,inrho,inchi)
  integer :: id_f3dlin,id_f3dherm,id_f3dspl
  real*8,parameter :: x3dcen=0.625d0,th3dcen=0.5d0,ph3dcen=1.235d0

  integer :: idxxc,idxxg,idyyc,idyyg,ii
  real*8 :: xx(4),yy(3),ff(4),ff1(4)

  !--------------------------------------------------
  !  pointers for pack/unpack test...
  integer, dimension(:), pointer :: ipack
  real*8, dimension(:), pointer :: r8pack
  !--------------------------------------------------

  zrho2(1)=0
  zrho2(inrho+1)=1

  do i=1,size(zrho)
     zrho(i)=(i-1)*1.0d0/(size(zrho)-1)
     if(i.gt.1) then
        zrho2(i)=(zrho(i)+zrho(i-1))/2
     endif
  enddo

  do i=1,size(zchi)
     zchi(i)=(i-1)*z2pi/(size(zchi)-1)
  enddo

  do i=1,size(zphi)
     zphi(i)=(i-1)*z2pi/(size(zphi)-1)
  enddo

  do i=1,size(xx)
     xx(i)=(i-1)*1.0d0/(size(xx)-1)
  enddo

  do i=1,size(yy)
     yy(i)=(i-1)*1.0d0/(size(yy)-1)
  enddo

  xplasma_trace_name = 'yygrid'

  zlabel=' '

  write(6,*) ' => here an error is expected (s has not been initialized):'
  call xplasma_global_info(s,ier, initLabel=zlabel)
  if(ier.ne.0) then
     call xplasma_error(s,ier,6)
  else
     write(6,*) ' zlabel = "',trim(zlabel),'"'
  endif

  write(6,*) ' '

  !----
  ! init s pointer
  call xoi_init(ier); call ckerr(s)  ! set up xplasma instance pointers

  !----
  ! init object pointed to by s...

  call xplasma_init(s,'test xplasma',ier); call ckerr(s)

  call xplasma_global_info(s,ier, initLabel=zlabel); call ckerr(s)
  write(6,*) ' zlabel = "',trim(zlabel),'"'

  call xplasma_init(s,'test xplasma #2',ier); call ckerr(s)

  call xplasma_global_info(s,ier, initLabel=zlabel); call ckerr(s)
  write(6,*) ' zlabel = "',trim(zlabel),'"'

  !-----------------------------------------------------
  !  traditional xplasma interface test

  call eqm_select("eqm_select_test",1)
  call eqm_premsg("eqm_premsg string")

  call eq_get_lunerr(ilun)
  write(6,*) ' eq_get_lunerr returned: ',ilun

  call eq_ident(zlabel,iaxi,iscr)
  write(6,*) ' eq_ident: "',trim(zlabel),'" iaxi,iscr=',iaxi,iscr

  ztime = 1.2345
  call eqm_time(ztime)
  call eq_time(ztime2)
  if(ztime.ne.ztime2) then
     write(6,*) ' ?eq_time did not return time set by eqm_time:'
     write(6,*) '  input to eqm_time: ',ztime,'; returned from eq_time:',  &
          ztime2
  else
     write(6,*) ' eqm_time/eq_time test passed OK.'
  endif
  !-----------------------------------------------------
  !  list creation and retrieval...

  write(6,*) ' '
  write(6,*) ' testing: eqm_list methods... '

  elem_names=' '
  elem_labels=' '
  elem_ivals=0
  elem_r8vals=0.0d0

  do i=1,4
     write(elem_names(i),1001) i
     write(elem_labels(i),1002) i
     elem_ivals(i)=i
     elem_r8vals(i)=elem_ivals(i)*1.0d0 + 0.1d0
  enddo

  write(6,*) ' creating: LIST_A'
  call eqm_list_create('list_a','a list soon to have 9 values', &
       4,elem_names,id_list,ier); call ckerr(s)

  call eqm_list_chval(id_list,4,elem_labels,ier); call ckerr(s)
  call eqm_list_ival(id_list,4,elem_ivals,ier); call ckerr(s)
  call eqm_list_r8val(id_list,4,elem_r8vals,ier); call ckerr(s)

  ioff=3
  do i=1,6
     write(elem_names(i),1001) i+ioff
     write(elem_labels(i),1002) i+ioff
     elem_ivals(i)=i+ioff
     elem_r8vals(i)=elem_ivals(i)*1.0d0 + 0.2d0
  enddo
     
  write(6,*) ' creating: LIST_B'
  call eqm_list_create('list_b','a supplemental list', &
       6,elem_names,id_listb,ier); call ckerr(s)

  call eqm_list_chval(id_listb,6,elem_labels,ier); call ckerr(s)
  call eqm_list_ival(id_listb,6,elem_ivals,ier); call ckerr(s)
  call eqm_list_r8val(id_listb,6,elem_r8vals,ier); call ckerr(s)

1001 format('item_',i1)
1002 format('label for item #',i1)

  write(6,*) ' merging: LIST_A and LIST_B'
  call eqm_list_merge(id_list,id_listb,.FALSE.,ier); call ckerr(s)
  if(ier.lt.0) then
     write(6,*) ' %warning -- list element was replaced.'
  endif

  write(6,*) ' '
  write(6,*) ' list readback tests...'

  call eq_nlist(inum)
  write(6,*) ' Number of known lists: ',inum
  call eq_list_maxsize(isize)
  write(6,*) ' Maximum list size: ',isize

  call eq_lists(size(list_names),list_names,list_ids,ier); call ckerr(s)

  do i=1,inum
     call eq_list_size(list_ids(i),isize)
     write(6,*) i,list_names(i),' id=',list_ids(i),' size=',isize
  enddo

  call eq_glistnum('List_A',inum)
  if(inum.eq.0) call ps_bad_exit

  call eq_list_label(inum,elem_labels(10))
  write(6,*) ' LIST_A generic label: ',trim(elem_labels(10))

  call eq_list_size(inum,isize)

  call eq_list_enames(inum,isize,elem_names,ier); call ckerr(s)
  call eq_list_chvals(inum,isize,elem_labels,ier); call ckerr(s)
  call eq_list_ivals(inum,isize,elem_ivals,ier); call ckerr(s)
  call eq_list_r8vals(inum,isize,elem_r8vals,ier); call ckerr(s)

  write(6,'(a,a,1x,i3,1x,1pe20.13)') &
       ' LIST elem 1 name & values: ',trim(elem_names(1)), &
       elem_ivals(1),elem_r8vals(1)
  write(6,*) ' LIST A elem_labels(',isize,')=',trim(elem_labels(isize))

  call xplasma_getList_names(s,inum,elem_names(1:isize),ier, &
       ivals=elem_ivals(1:isize),chvals=elem_labels(1:isize), &
       r8vals=elem_r8vals(1:isize))

  write(6,'(a,a,1x,i3,1x,1pe20.13)') &
       ' LIST elem 1 name & values: ',trim(elem_names(1)), &
       elem_ivals(1),elem_r8vals(1)
  write(6,*) ' LIST A elem_labels(',isize,')=',trim(elem_labels(isize))

  write(6,*) ' LIST_A:'
  do i=1,isize
     write(6,'(3x,i3,1x,a,1x,i3,1x,1pe15.8,1x,a,a,a)') &
          i,trim(elem_names(i)),elem_ivals(i),elem_r8vals(i),' "', &
          trim(elem_labels(i)),'"'
  enddo

  !---------------------------
  ! test removal operation-- steal it from F77_USER -- the generic legacy
  ! author name for eqm_ calls...

  ! this may be a BAD EXAMPLE of xplasma usage-- normally one module
  ! should not fetch an identity to "impersonate" another module!

  call xplasma_get_item_info(s,id_list,ier,author=zauthor); call ckerr(s)
  call xplasma_author_set(s,zauthor,ier); call ckerr(s)

  call xplasma_remove_item(s,id_list,ier)
  if(ier.ne.0) then
     write(6,*) ' %xplasma2_test: error deleting LIST_A'
  else
     write(6,*) ' %xplasma2_test: LIST_A deleted.'
  endif
  call ckerr(s)

  call xplasma_author_clear(s,zauthor,ier); call ckerr(s)

  inum=id_listb
  call eq_list_label(inum,elem_labels(10))
  write(6,*) ' LIST_B generic label: ',trim(elem_labels(10))

  call eq_list_size(inum,isize)

  call eq_list_enames(inum,isize,elem_names,ier); call ckerr(s)
  call eq_list_chvals(inum,isize,elem_labels,ier); call ckerr(s)
  call eq_list_ivals(inum,isize,elem_ivals,ier); call ckerr(s)
  call eq_list_r8vals(inum,isize,elem_r8vals,ier); call ckerr(s)

  write(6,*) ' LIST_B:'
  do i=1,isize
     write(6,'(3x,i3,1x,a,1x,i3,1x,1pe15.8,1x,a,a,a)') &
          i,trim(elem_names(i)),elem_ivals(i),elem_r8vals(i),' "', &
          trim(elem_labels(i)),'"'
  enddo

  call eqm_rho(zrho,inrho,ztol,id_rho,ier); call ckerr(s)

  call eqm_chi(zchi,0,inchi,ztol,id_chi,ier); call ckerr(s)

  call eqm_uaxis('RHO2',id_rho,0,zrho2,inrho+1,ztol,id_rho2,ier); call ckerr(s)

  call xplasma_author_set(s,'xplasma2_test',ier); call ckerr(s)

  call xplasma_create_coord(s,'xxcoord',.FALSE.,idxxc,ier,label='XX')
  call ckerr(s)

  call xplasma_create_coord(s,'yycoord',.FALSE.,idyyc,ier,label='YY')
  call ckerr(s)

  call xplasma_create_grid(s,'xxgrid',idxxc,xx,idxxg,ier,label='XX')
  call ckerr(s)

  call xplasma_create_grid(s,'yygrid',idyyc,yy,idyyg,ier,label='YY')
  call ckerr(s)

  call xplasma_create_grid(s,'Phi_grid',xplasma_phi_coord,zphi, &
       id_phi,ier); call ckerr(s)

  call xplasma_author_clear(s,'xplasma2_test',ier); call ckerr(s)

  write(6,*) ' '
  write(6,*) ' theta:'
  call ckgrid(xplasma_theta_coord)

  write(6,*) ' '
  write(6,*) ' rho:'
  call ckgrid(xplasma_rho_coord)

  call eq_ngrid(id_rho,inum)
  imax=max(inrho+1,inchi)

  call eq_grid_zc(id_rho,ztmp,imax,igot,ier); call ckerr(s)
  write(6,*) ' id_rho -> eq_grid_zc -> zone centers:'
  write(6,'(3(1x,1pe20.13))') ztmp(1:igot)

  call eq_get_aname(id_rho,zname)
  write(6,*) ' eq_get_aname(',id_rho,',...) returned zname="',trim(zname),'"'
  call eq_ganum(zname,id_rho)
  write(6,*) ' eq_ganum("',trim(zname),'",...) returned id_rho=',id_rho

  !=========================================================
  !  profile tests...

  do ix=1,inrho
     zsinx(ix)=sin(z2pi*zrho(ix))
     zcosx(ix)=cos(z2pi*zrho(ix))
  enddo
  do ix=1,inrho+1
     zsinx2(ix)=sin(z2pi*zrho2(ix))
  enddo

  call eqm_rhofun(2,id_rho,'SINX',zsinx, &
       0,zdum,0,zdum, id_fun,ier); call ckerr(s)

  call eqm_rhofun(99,id_rho2,'SINX',zsinx2, &
       0,zdum,0,zdum, id_fun,ier); call ckerr(s)

  call xplasma_getProf_1data(s,id_fun,zsinx1,ier, zcoeff=zcoeff1)
  call ckerr(s)

  call xplasma_author_set(s,'xplasma2_test',ier); call ckerr(s)

  call xplasma_create_prof(s,'SINX1',id_rho2,zsinx1,id_fun1,ier, &
       ispline=2, coeffs=zcoeff1); call ckerr(s)

  call xplasma_author_clear(s,'xplasma2_test',ier); call ckerr(s)

  do ideriv=0,2
     call xplasma_eval_prof(s,id_fun,xx,ff,ier,  ideriv1=ideriv); call ckerr(s)
     call xplasma_eval_prof(s,id_fun1,xx,ff1,ier,ideriv1=ideriv); call ckerr(s)

     do ix=1,size(xx)
        if(ff(ix).ne.ff1(ix)) then
           write(6,*) ' ?unexpected difference: ideriv=',ideriv
           write(6,*) '    ff, ff1 (',ix,'): ',ff(ix),ff1(ix)
        endif
     enddo
  enddo

  ! test: convert spline to hermite (C2 maintained)
  call xplasma_hermitize(s,id_fun1,ier);  call ckerr(s)

  do ideriv=0,1
     call xplasma_eval_prof(s,id_fun,xx,ff,ier,  ideriv1=ideriv); call ckerr(s)
     call xplasma_eval_prof(s,id_fun1,xx,ff1,ier,ideriv1=ideriv); call ckerr(s)

     zscale = maxval(abs(ff))

     do ix=1,size(xx)
        if(abs(ff(ix)-ff1(ix)).gt.(1.0d-14*zscale)) then
           write(6,*) ' ?unexpected difference: ideriv=',ideriv
           write(6,*) '    ff, ff1 (',ix,'): ',ff(ix),ff1(ix)
        endif
     enddo
  enddo

  call xplasma_author_set(s,'xplasma2_test',ier); call ckerr(s)
  call xplasma_remove_item(s,id_fun1,ier); call ckerr(s)
  call xplasma_author_clear(s,'xplasma2_test',ier); call ckerr(s)

  call eqm_rhofun(2,id_rho,'SINX_SPL',zsinx, &
       1,z2pi,1,z2pi, id_funs,ier); call ckerr(s)  ! explicit BC

  call eqm_rhofun(2,id_rho,'COSX_SPL',zcosx, &
       1,zero,1,zero, id_funsc,ier); call ckerr(s)  ! explicit BC

  call eqm_rhofun(1,id_rho,'SINX_HERM',zsinx, &
       1,z2pi,1,z2pi, id_funh,ier); call ckerr(s)  ! explicit BC

  call eqm_rhofun(0,id_rho,'SINX_LIN',zsinx, &
       0,zdum,0,zdum, id_funl,ier); call ckerr(s)

  call xplasma_prof_info(s,id_fun,ier, rank=irank); call ckerr(s)
  write(6,*) '  SINX rank: ',irank

  call xplasma_prof_info(s,id_fun,ier, splineType=ispline); call ckerr(s)
  write(6,*) '  SINX spline type: ',ispline

  call xplasma_prof_info(s,id_fun,ier, gridId1=id_rho2); call ckerr(s)
  call xplasma_get_item_info(s,id_rho2,ier,name=zname); call ckerr(s)
  write(6,*) '  SINX x axis name: ',trim(zname)

  call eq_get_fname(id_fun,zname)
  write(6,*) '  SINX name fetched: ',trim(zname)

  call eq_gfnum(zname,idum)
  write(6,*) '  SINX number twice: ',id_fun,idum

  call xplasma_get_counter(s,icount,ier); call ckerr(s)
  write(6,*) ' Xplasma profile counter: ',icount
  
  call xplasma_prof_info(s,id_funs,ier, counter=icount); call ckerr(s)
  write(6,*) ' Profile counter for "SINX_SPL": ',icount

  call xplasma_eval_prof(s,id_funs,zans1(1:inrho),ier); call ckerr(s)
  write(6,*) ' Profile retrieve test: '
  write(6,'(a,2(1x,1pe20.13))') '   2-> ',zsinx(2),zans1(2)
  write(6,'(a,2(1x,1pe20.13))') '  12-> ',zsinx(12),zans1(12)

  do ix=1,inxtest
     zxtest(ix)=(ix-1)*0.25d0/(inxtest-1)
  enddo

  ifuns(1)=id_funsc
  ifuns(2)=id_funs
  ifuns(3)=id_funh
  ifuns(4)=id_funl

  call eq_rgetff(inxtest,zxtest,4,ifuns(1:4),0,inxtest,zans4, &
       ier); call ckerr(s)
  write(6,'(a,1pe20.13,a)') '    p = ',z2pi,'  ... function interpolations:'
  write(6,*) ' i  x             cos(px)_spl   sin(px)_spl   sin(px)_herm  sin(px)_lin'
  do ix=1,inxtest
    write(6,1005) ix,zxtest(ix),zans4(ix,1),zans4(ix,2),zans4(ix,3),zans4(ix,4)
  enddo

  !------------
  !  2d & 3d hybrid  zone-spline, zone-hermite, zone-pclin tests
  !  all have just 1 dimension of cubic interpolation

  itest=0
  itmax=0
  dmax=0.0d0

  call hyb2dzon(id_funs,idxxg,zxtest,zans4(:,2),0)
  call hyb2dzon(idxxg,id_funs,zxtest,zans4(:,2),0)

  call hyb2dzon(id_funh,idxxg,zxtest,zans4(:,3),0)
  call hyb2dzon(idxxg,id_funh,zxtest,zans4(:,3),0)

  call hyb2dzon(id_funl,idxxg,zxtest,zans4(:,4),0)
  call hyb2dzon(idxxg,id_funl,zxtest,zans4(:,4),0)

  !-------------
  call hyb2dlin(id_funs,idxxg,zxtest,zans4(:,2),0)
  call hyb2dlin(idxxg,id_funs,zxtest,zans4(:,2),0)

  call hyb2dlin(id_funh,idxxg,zxtest,zans4(:,3),0)
  call hyb2dlin(idxxg,id_funh,zxtest,zans4(:,3),0)

  !-------------
  call hyb3dzon(id_funs,idxxg,idyyg,zxtest,zans4(:,2),0)
  call hyb3dzon(idxxg,id_funs,idyyg,zxtest,zans4(:,2),0)
  call hyb3dzon(idxxg,idyyg,id_funs,zxtest,zans4(:,2),0)

  call hyb3dzon(id_funh,idxxg,idyyg,zxtest,zans4(:,3),0)
  call hyb3dzon(idxxg,id_funh,idyyg,zxtest,zans4(:,3),0)
  call hyb3dzon(idxxg,idyyg,id_funh,zxtest,zans4(:,3),0)

  call hyb3dzon(id_funl,idxxg,idyyg,zxtest,zans4(:,4),0)
  call hyb3dzon(idxxg,id_funl,idyyg,zxtest,zans4(:,4),0)
  call hyb3dzon(idxxg,idyyg,id_funl,zxtest,zans4(:,4),0)

  !-------------
  call hyb3dlin(id_funs,idxxg,idyyg,zxtest,zans4(:,2),0)
  call hyb3dlin(idxxg,id_funs,idyyg,zxtest,zans4(:,2),0)
  call hyb3dlin(idxxg,idyyg,id_funs,zxtest,zans4(:,2),0)

  call hyb3dlin(id_funh,idxxg,idyyg,zxtest,zans4(:,3),0)
  call hyb3dlin(idxxg,id_funh,idyyg,zxtest,zans4(:,3),0)
  call hyb3dlin(idxxg,idyyg,id_funh,zxtest,zans4(:,3),0)

  !-------------
  call hyb3dzln(id_funs,idxxg,idyyg,zxtest,zans4(:,2),0)
  call hyb3dzln(idxxg,id_funs,idyyg,zxtest,zans4(:,2),0)
  call hyb3dzln(idxxg,idyyg,id_funs,zxtest,zans4(:,2),0)

  call hyb3dzln(id_funh,idxxg,idyyg,zxtest,zans4(:,3),0)
  call hyb3dzln(idxxg,id_funh,idyyg,zxtest,zans4(:,3),0)
  call hyb3dzln(idxxg,idyyg,id_funh,zxtest,zans4(:,3),0)

  !-------------
  call hyb3dlzn(id_funs,idxxg,idyyg,zxtest,zans4(:,2),0)
  call hyb3dlzn(idxxg,id_funs,idyyg,zxtest,zans4(:,2),0)
  call hyb3dlzn(idxxg,idyyg,id_funs,zxtest,zans4(:,2),0)

  call hyb3dlzn(id_funh,idxxg,idyyg,zxtest,zans4(:,3),0)
  call hyb3dlzn(idxxg,id_funh,idyyg,zxtest,zans4(:,3),0)
  call hyb3dlzn(idxxg,idyyg,id_funh,zxtest,zans4(:,3),0)

  write(6,*) ' 2d & 3d hybrid interpolation (no derivative)'
  write(6,'(3x,a,i4,a,1pe14.7)') &
       ' number of hybrid tests: ',itest,'; max deviation: ',dmax
  write(6,*) ' '

  if(dmax.gt.1.0d-10) then
     write(6,*) '   MAX DEVIATION:  test#: ',itmax
  endif
  !------------

  call eq_rgetff(inxtest,zxtest,4,ifuns(1:4),1,inxtest,zans4, &
       ier); call ckerr(s)
  write(6,'(a,1pe20.13,a)') '    p = ',z2pi,'  ... df/dx interpolations:'
  write(6,*) ' i  x             cos(px)_spl   sin(px)_spl   sin(px)_herm  sin(px)_lin'
  do ix=1,inxtest
    write(6,1005) ix,zxtest(ix),zans4(ix,1),zans4(ix,2),zans4(ix,3),zans4(ix,4)
  enddo
  !------------
  !  2d & 3d hybrid  zone-spline, zone-hermite, zone-pclin tests
  !  all have just 1 dimension of cubic interpolation

  itest=0
  itmax=0
  dmax=0.0d0

  call hyb2dzon(id_funs,idxxg,zxtest,zans4(:,2),1)
  call hyb2dzon(idxxg,id_funs,zxtest,zans4(:,2),1)

  call hyb2dzon(id_funh,idxxg,zxtest,zans4(:,3),1)
  call hyb2dzon(idxxg,id_funh,zxtest,zans4(:,3),1)

  call hyb2dzon(id_funl,idxxg,zxtest,zans4(:,4),1)
  call hyb2dzon(idxxg,id_funl,zxtest,zans4(:,4),1)

  !-------------
  call hyb2dlin(id_funs,idxxg,zxtest,zans4(:,2),1)
  call hyb2dlin(idxxg,id_funs,zxtest,zans4(:,2),1)

  call hyb2dlin(id_funh,idxxg,zxtest,zans4(:,3),1)
  call hyb2dlin(idxxg,id_funh,zxtest,zans4(:,3),1)

  !-------------
  call hyb3dzon(id_funs,idxxg,idyyg,zxtest,zans4(:,2),1)
  call hyb3dzon(idxxg,id_funs,idyyg,zxtest,zans4(:,2),1)
  call hyb3dzon(idxxg,idyyg,id_funs,zxtest,zans4(:,2),1)

  call hyb3dzon(id_funh,idxxg,idyyg,zxtest,zans4(:,3),1)
  call hyb3dzon(idxxg,id_funh,idyyg,zxtest,zans4(:,3),1)
  call hyb3dzon(idxxg,idyyg,id_funh,zxtest,zans4(:,3),1)

  call hyb3dzon(id_funl,idxxg,idyyg,zxtest,zans4(:,4),1)
  call hyb3dzon(idxxg,id_funl,idyyg,zxtest,zans4(:,4),1)
  call hyb3dzon(idxxg,idyyg,id_funl,zxtest,zans4(:,4),1)

  !-------------
  call hyb3dlin(id_funs,idxxg,idyyg,zxtest,zans4(:,2),1)
  call hyb3dlin(idxxg,id_funs,idyyg,zxtest,zans4(:,2),1)
  call hyb3dlin(idxxg,idyyg,id_funs,zxtest,zans4(:,2),1)

  call hyb3dlin(id_funh,idxxg,idyyg,zxtest,zans4(:,3),1)
  call hyb3dlin(idxxg,id_funh,idyyg,zxtest,zans4(:,3),1)
  call hyb3dlin(idxxg,idyyg,id_funh,zxtest,zans4(:,3),1)

  !-------------
  call hyb3dzln(id_funs,idxxg,idyyg,zxtest,zans4(:,2),1)
  call hyb3dzln(idxxg,id_funs,idyyg,zxtest,zans4(:,2),1)
  call hyb3dzln(idxxg,idyyg,id_funs,zxtest,zans4(:,2),1)

  call hyb3dzln(id_funh,idxxg,idyyg,zxtest,zans4(:,3),1)
  call hyb3dzln(idxxg,id_funh,idyyg,zxtest,zans4(:,3),1)
  call hyb3dzln(idxxg,idyyg,id_funh,zxtest,zans4(:,3),1)

  !-------------
  call hyb3dlzn(id_funs,idxxg,idyyg,zxtest,zans4(:,2),1)
  call hyb3dlzn(idxxg,id_funs,idyyg,zxtest,zans4(:,2),1)
  call hyb3dlzn(idxxg,idyyg,id_funs,zxtest,zans4(:,2),1)

  call hyb3dlzn(id_funh,idxxg,idyyg,zxtest,zans4(:,3),1)
  call hyb3dlzn(idxxg,id_funh,idyyg,zxtest,zans4(:,3),1)
  call hyb3dlzn(idxxg,idyyg,id_funh,zxtest,zans4(:,3),1)

  write(6,*) ' 2d & 3d hybrid interpolation (1st derivative)'
  write(6,'(3x,a,i4,a,1pe14.7)') &
       ' number of hybrid tests: ',itest,'; max deviation: ',dmax
  write(6,*) ' '

  if(dmax.gt.1.0d-10) then
     write(6,*) '   MAX DEVIATION:  test#: ',itmax
  endif
  !------------

  !  test using 2d evaluator for 1d profile -- answer should not change...
  !  reverse order of rho & chi... answer should not change...
  call xplasma_eval_prof(s,ifuns(2),id_chi,zxtest,id_rho,zxtest,zans1,ier, &
       ideriv2=1); call ckerr(s)

  write(6,'(2x,a,2(1x,1pe20.13))') &
       '1d 2d eval compare pt. 13: ',zans4(13,2),zans1(13)
  write(6,'(2x,a,2(1x,1pe20.13))') &
       '1d 2d eval compare pt. 37: ',zans4(37,2),zans1(37)

  call xplasma_eval_prof(s,ifuns,id_rho,zxtest,id_chi,zxtest,zans4,ier, &
       ideriv1=1); call ckerr(s)

  write(6,'(2x,a,2(1x,1pe20.13))') &
       '1d 2d eval compare pt. 13: ',zans4(13,2),zans1(13)
  write(6,'(2x,a,2(1x,1pe20.13))') &
       '1d 2d eval compare pt. 37: ',zans4(37,2),zans1(37)


  !  only splines have 2nd and 3rd derivatives...
  call xplasma_eval_prof(s,id_funs,zxtest,zans4(1:inxtest,1),ier, &
       ideriv1=0); call ckerr(s)
  call xplasma_eval_prof(s,id_funs,zxtest,zans4(1:inxtest,2),ier, &
       ideriv1=1); call ckerr(s)
  call xplasma_eval_prof(s,id_funs,zxtest,zans4(1:inxtest,3),ier, &
       ideriv1=2); call ckerr(s)
  call xplasma_eval_prof(s,id_funs,zxtest,zans4(1:inxtest,4),ier, &
       ideriv1=3); call ckerr(s)

  write(6,'(a,1pe20.13,a)') '    p = ',z2pi,'  ... function interpolations:'
  write(6,*) ' i  x         f(x)=sin(px)_spl  df/dx         d2f/dx2       d3f/dx3'
  do ix=1,inxtest
     write(6,1005) ix,zxtest(ix),zans4(ix,1),zans4(ix,2),zans4(ix,3),zans4(ix,4)
  enddo

1005 format(1x,i2,5(1x,1pe13.6))
  !------------
  !  2d & 3d hybrid  zone-spline, zone-hermite, zone-pclin tests
  !  all have just 1 dimension of cubic interpolation

  itest=0
  itmax=0
  dmax=0.0d0

  call hyb2dzon(id_funs,idxxg,zxtest,zans4(:,3),2)
  call hyb2dzon(idxxg,id_funs,zxtest,zans4(:,3),2)

  !-------------
  call hyb2dlin(id_funs,idxxg,zxtest,zans4(:,3),2)
  call hyb2dlin(idxxg,id_funs,zxtest,zans4(:,3),2)

  !-------------
  call hyb3dzon(id_funs,idxxg,idyyg,zxtest,zans4(:,3),2)
  call hyb3dzon(idxxg,id_funs,idyyg,zxtest,zans4(:,3),2)
  call hyb3dzon(idxxg,idyyg,id_funs,zxtest,zans4(:,3),2)

  !-------------
  call hyb3dlin(id_funs,idxxg,idyyg,zxtest,zans4(:,3),2)
  call hyb3dlin(idxxg,id_funs,idyyg,zxtest,zans4(:,3),2)
  call hyb3dlin(idxxg,idyyg,id_funs,zxtest,zans4(:,3),2)

  !-------------
  call hyb3dzln(id_funs,idxxg,idyyg,zxtest,zans4(:,3),2)
  call hyb3dzln(idxxg,id_funs,idyyg,zxtest,zans4(:,3),2)
  call hyb3dzln(idxxg,idyyg,id_funs,zxtest,zans4(:,3),2)

  !-------------
  call hyb3dlzn(id_funs,idxxg,idyyg,zxtest,zans4(:,3),2)
  call hyb3dlzn(idxxg,id_funs,idyyg,zxtest,zans4(:,3),2)
  call hyb3dlzn(idxxg,idyyg,id_funs,zxtest,zans4(:,3),2)

  write(6,*) ' 2d & 3d hybrid interpolation (2nd derivative)'
  write(6,'(3x,a,i4,a,1pe14.7)') &
       ' number of hybrid tests: ',itest,'; max deviation: ',dmax
  write(6,*) ' '

  if(dmax.gt.1.0d-10) then
     write(6,*) '   MAX DEVIATION:  test#: ',itmax
  endif
  !------------
  !=========================================================
  ! f(rho,chi) profile test

  do ichi=1,inchi
     do ix=1,inrho
        f2d(ix,ichi)=exp(zrho(ix))*sin(zchi(ichi))
     enddo
  enddo

  !  zchi args here are actually dummies but with right dimensionality

  call eqm_frhochi(0,id_rho,id_chi,'f2dlin',f2d,inrho, &
       0,zchi,0,zchi,id_f2dlin,ier); call ckerr(s)

  call eqm_frhochi(1,id_rho,id_chi,'f2dherm',f2d,inrho, &
       0,zchi,0,zchi,id_f2dherm,ier); call ckerr(s)

  call eqm_frhochi(2,id_rho,id_chi,'f2dspl',f2d,inrho, &
       0,zchi,0,zchi,id_f2dspl,ier); call ckerr(s)

  call xplasma_eval_prof(s,id_f2dspl,f2dret,ier, zcoeff=f2dcoeff)
  call ckerr(s)
  write(6,'(1x,a,2(1x,1pe20.13))') 'f2d[ret](3,4)   =',f2d(3,4),f2dret(3,4)
  write(6,'(1x,a,2(1x,1pe20.13))') 'f2d[ret](11,35) =',f2d(11,35),f2dret(11,35)

  call xplasma_author_set(s,'xplasma2_test',ier); call ckerr(s)

  call xplasma_create_prof(s,'f2dspl2',id_rho,id_chi,f2dret,id_fun1,ier, &
       ispline=2,coeffs=f2dcoeff); call ckerr(s)

  call xplasma_author_clear(s,'xplasma2_test',ier); call ckerr(s)

  x2d(1)=x2dcen-zd; th2d(1)=th2dcen-zd
  x2d(2)=x2dcen;    th2d(2)=th2dcen-zd
  x2d(3)=x2dcen+zd; th2d(3)=th2dcen-zd

  x2d(4)=x2dcen-zd; th2d(4)=th2dcen
  x2d(5)=x2dcen;    th2d(5)=th2dcen
  x2d(6)=x2dcen+zd; th2d(6)=th2dcen

  x2d(7)=x2dcen-zd; th2d(7)=th2dcen+zd
  x2d(8)=x2dcen;    th2d(8)=th2dcen+zd
  x2d(9)=x2dcen+zd; th2d(9)=th2dcen+zd

  ifuns(1)=id_f2dlin
  ifuns(2)=id_f2dherm
  ifuns(3)=id_f2dspl
  ifuns(4)=id_fun1

  call eq_frhochi(9,x2d,th2d,4,ifuns(1:4),9,vals2d,ier); call ckerr(s)
  do i=1,9
     if(vals2d(i,3).ne.vals2d(i,4)) then
        write(6,*) ' f2dspl/f2dspl2 mismatch: i,val,val2=', &
             i,vals2d(i,2),vals2d(i,4)
     endif
     vals2da(i)=exp(x2d(i))*sin(th2d(i))
  enddo

  write(6,1400) '   values: L H S A: ',vals2d(5,1:3),vals2da(5)

  ztmp(1:3)=(vals2d(6,1:3)-vals2d(4,1:3))/(2*zd)
  ztmp(4)=(vals2da(6)-vals2da(4))/(2*zd)
  write(6,1400) '        f.d. df/dx: ',ztmp(1:4)

  ztmp(1:3)=(vals2d(8,1:3)-vals2d(2,1:3))/(2*zd)
  ztmp(4)=(vals2da(8)-vals2da(2))/(2*zd)
  write(6,1400) '    f.d. df/dtheta: ',ztmp(1:4)

  ztmp(1:3)=(vals2d(9,1:3)-vals2d(7,1:3)-vals2d(3,1:3)+vals2d(1,1:3))/(4*zd*zd)
  ztmp(4)=(vals2da(9)-vals2da(7)-vals2da(3)+vals2da(1))/(4*zd*zd)
  write(6,1400) ' f.d. d2f/dxdtheta: ',ztmp(1:4)

1400 format(1x,a,4(1x,1pe13.6))
1401 format(1x,a,3(1x,1pe13.6))

  ict=0
  ict(2:3)=1
  ict(6)=1
  call eq_hrhochi(1,x2d(5:5),th2d(5:5),4,ifuns(1:4),ict,1,valtmp2d,ier)
  call ckerr(s)

  write(6,*) ' '
  write(6,1401) '     interp. df/dx: ',valtmp2d(1,1),valtmp2d(1,4),valtmp2d(1,7)
  write(6,1401) ' interp. df/dtheta: ',valtmp2d(1,2),valtmp2d(1,5),valtmp2d(1,8)
  write(6,1401) ' interp. d2f/dthdx: ',valtmp2d(1,3),valtmp2d(1,6),valtmp2d(1,9)
  do ii=7,9
     if(valtmp2d(1,ii).ne.valtmp2d(1,ii+3)) then
        write(6,*) ' ? f2dspl/f2dspl2 derivative mismatch: ', &
             valtmp2d(1,ii),valtmp2d(1,ii+3)
     endif
  enddo

  call xplasma_hermitize(s,id_fun1,ier); call ckerr(s)
  do ideriv2=0,1
     do ideriv1=0,1
        call xplasma_eval_prof(s,id_f2dspl, &
             id_chi,th2d, id_rho,x2d, yy2d, ier, &
             ideriv1=ideriv1, ideriv2=ideriv2); call ckerr(s)
        call xplasma_eval_prof(s,id_fun1, &
             id_chi,th2d, id_rho,x2d, yy2e, ier, &
             ideriv1=ideriv1, ideriv2=ideriv2); call ckerr(s)

        zscale = maxval(abs(yy2d))
        zfac=1.0d-14
        if(ideriv1+ideriv2.eq.2) zfac=1.0d-12

        do ix=1,size(x2d)
           if(abs(yy2d(ix)-yy2e(ix)).gt.(zfac*zscale)) then
              write(6,*) ' ?unexpected difference: ideriv1,2=',ideriv1,ideriv2
              write(6,*) '    yy2d, yy2e (',ix,'): ',yy2d(ix),yy2e(ix)
           endif
        enddo
     enddo
  enddo

  call xplasma_author_set(s,'xplasma2_test',ier); call ckerr(s)
  call xplasma_remove_item(s,id_fun1,ier); call ckerr(s)
  call xplasma_author_clear(s,'xplasma2_test',ier); call ckerr(s)

  ! 3d hybrid interpolation comparisons
  !  Zone-linear, Zone-Hermite, and Zone-Spline hybrids

  itest=0
  itmax=0
  dmax=0.0d0

  ict=0
  ict(1)=1  ! value
  call eq_hrhochi(9,x2d,th2d,3,ifuns(1:3),ict,9,vals2d,ier); call ckerr(s)

  call hyb3z2(1,id_f2dlin,x2d,th2d,vals2d(:,1),0,0)
  call hyb3z2(2,id_f2dlin,x2d,th2d,vals2d(:,1),0,0)
  call hyb3z2(3,id_f2dlin,x2d,th2d,vals2d(:,1),0,0)

  call hyb3z2(1,id_f2dherm,x2d,th2d,vals2d(:,2),0,0)
  call hyb3z2(2,id_f2dherm,x2d,th2d,vals2d(:,2),0,0)
  call hyb3z2(3,id_f2dherm,x2d,th2d,vals2d(:,2),0,0)

  call hyb3z2(1,id_f2dspl,x2d,th2d,vals2d(:,3),0,0)
  call hyb3z2(2,id_f2dspl,x2d,th2d,vals2d(:,3),0,0)
  call hyb3z2(3,id_f2dspl,x2d,th2d,vals2d(:,3),0,0)

  ict(1)=0
  ict(2)=1  ! d/drho
  call eq_hrhochi(9,x2d,th2d,3,ifuns(1:3),ict,9,vals2d,ier); call ckerr(s)

  call hyb3z2(1,id_f2dlin,x2d,th2d,vals2d(:,1),1,0)
  call hyb3z2(2,id_f2dlin,x2d,th2d,vals2d(:,1),1,0)
  call hyb3z2(3,id_f2dlin,x2d,th2d,vals2d(:,1),1,0)

  call hyb3z2(1,id_f2dherm,x2d,th2d,vals2d(:,2),1,0)
  call hyb3z2(2,id_f2dherm,x2d,th2d,vals2d(:,2),1,0)
  call hyb3z2(3,id_f2dherm,x2d,th2d,vals2d(:,2),1,0)

  call hyb3z2(1,id_f2dspl,x2d,th2d,vals2d(:,3),1,0)
  call hyb3z2(2,id_f2dspl,x2d,th2d,vals2d(:,3),1,0)
  call hyb3z2(3,id_f2dspl,x2d,th2d,vals2d(:,3),1,0)

  ict(2)=0
  ict(3)=1  ! d/dchi
  call eq_hrhochi(9,x2d,th2d,3,ifuns(1:3),ict,9,vals2d,ier); call ckerr(s)

  call hyb3z2(1,id_f2dlin,x2d,th2d,vals2d(:,1),0,1)
  call hyb3z2(2,id_f2dlin,x2d,th2d,vals2d(:,1),0,1)
  call hyb3z2(3,id_f2dlin,x2d,th2d,vals2d(:,1),0,1)

  call hyb3z2(1,id_f2dherm,x2d,th2d,vals2d(:,2),0,1)
  call hyb3z2(2,id_f2dherm,x2d,th2d,vals2d(:,2),0,1)
  call hyb3z2(3,id_f2dherm,x2d,th2d,vals2d(:,2),0,1)

  call hyb3z2(1,id_f2dspl,x2d,th2d,vals2d(:,3),0,1)
  call hyb3z2(2,id_f2dspl,x2d,th2d,vals2d(:,3),0,1)
  call hyb3z2(3,id_f2dspl,x2d,th2d,vals2d(:,3),0,1)

  ict(3)=0
  ict(6)=1  ! d2/drhodchi
  call eq_hrhochi(9,x2d,th2d,3,ifuns(1:3),ict,9,vals2d,ier); call ckerr(s)

  call hyb3z2(1,id_f2dlin,x2d,th2d,vals2d(:,1),1,1)
  call hyb3z2(2,id_f2dlin,x2d,th2d,vals2d(:,1),1,1)
  call hyb3z2(3,id_f2dlin,x2d,th2d,vals2d(:,1),1,1)

  call hyb3z2(1,id_f2dherm,x2d,th2d,vals2d(:,2),1,1)
  call hyb3z2(2,id_f2dherm,x2d,th2d,vals2d(:,2),1,1)
  call hyb3z2(3,id_f2dherm,x2d,th2d,vals2d(:,2),1,1)

  call hyb3z2(1,id_f2dspl,x2d,th2d,vals2d(:,3),1,1)
  call hyb3z2(2,id_f2dspl,x2d,th2d,vals2d(:,3),1,1)
  call hyb3z2(3,id_f2dspl,x2d,th2d,vals2d(:,3),1,1)

  ict(6)=0
  ict(4)=1  ! d2/drho2
  call eq_hrhochi(9,x2d,th2d,1,ifuns(3:3),ict,9,vals2d,ier); call ckerr(s)

  call hyb3z2(1,id_f2dspl,x2d,th2d,vals2d(:,1),2,0)
  call hyb3z2(2,id_f2dspl,x2d,th2d,vals2d(:,1),2,0)
  call hyb3z2(3,id_f2dspl,x2d,th2d,vals2d(:,1),2,0)

  ict(4)=0
  ict(5)=1  ! d2/dchi2
  call eq_hrhochi(9,x2d,th2d,1,ifuns(3:3),ict,9,vals2d,ier); call ckerr(s)

  call hyb3z2(1,id_f2dspl,x2d,th2d,vals2d(:,1),0,2)
  call hyb3z2(2,id_f2dspl,x2d,th2d,vals2d(:,1),0,2)
  call hyb3z2(3,id_f2dspl,x2d,th2d,vals2d(:,1),0,2)

  write(6,*) ' '
  write(6,*) ' 3d (zonal + 2d interpolant)'
  write(6,'(3x,a,i4,a,1pe14.7)') &
       ' number of hybrid tests: ',itest,'; max deviation: ',dmax
  write(6,*) ' '

  if(dmax.gt.1.0d-10) then
     write(6,*) '   MAX DEVIATION:  test#: ',itmax
     write(6,*) ' '
  endif

  ! 3d hybrid interpolation comparisons
  !   Linear-Hermite, and Linear-Spline hybrids

  itest=0
  itmax=0
  dmax=0.0d0

  ict=0
  ict(1)=1  ! value
  call eq_hrhochi(9,x2d,th2d,3,ifuns(1:3),ict,9,vals2d,ier); call ckerr(s)

  call hyb3lin2(1,id_f2dherm,x2d,th2d,vals2d(:,2),0,0)
  call hyb3lin2(2,id_f2dherm,x2d,th2d,vals2d(:,2),0,0)
  call hyb3lin2(3,id_f2dherm,x2d,th2d,vals2d(:,2),0,0)

  call hyb3lin2(1,id_f2dspl,x2d,th2d,vals2d(:,3),0,0)
  call hyb3lin2(2,id_f2dspl,x2d,th2d,vals2d(:,3),0,0)
  call hyb3lin2(3,id_f2dspl,x2d,th2d,vals2d(:,3),0,0)

  ict(1)=0
  ict(2)=1  ! d/drho
  call eq_hrhochi(9,x2d,th2d,3,ifuns(1:3),ict,9,vals2d,ier); call ckerr(s)

  call hyb3lin2(1,id_f2dherm,x2d,th2d,vals2d(:,2),1,0)
  call hyb3lin2(2,id_f2dherm,x2d,th2d,vals2d(:,2),1,0)
  call hyb3lin2(3,id_f2dherm,x2d,th2d,vals2d(:,2),1,0)

  call hyb3lin2(1,id_f2dspl,x2d,th2d,vals2d(:,3),1,0)
  call hyb3lin2(2,id_f2dspl,x2d,th2d,vals2d(:,3),1,0)
  call hyb3lin2(3,id_f2dspl,x2d,th2d,vals2d(:,3),1,0)

  ict(2)=0
  ict(3)=1  ! d/dchi
  call eq_hrhochi(9,x2d,th2d,3,ifuns(1:3),ict,9,vals2d,ier); call ckerr(s)

  call hyb3lin2(1,id_f2dherm,x2d,th2d,vals2d(:,2),0,1)
  call hyb3lin2(2,id_f2dherm,x2d,th2d,vals2d(:,2),0,1)
  call hyb3lin2(3,id_f2dherm,x2d,th2d,vals2d(:,2),0,1)

  call hyb3lin2(1,id_f2dspl,x2d,th2d,vals2d(:,3),0,1)
  call hyb3lin2(2,id_f2dspl,x2d,th2d,vals2d(:,3),0,1)
  call hyb3lin2(3,id_f2dspl,x2d,th2d,vals2d(:,3),0,1)

  ict(3)=0
  ict(6)=1  ! d2/drhodchi
  call eq_hrhochi(9,x2d,th2d,3,ifuns(1:3),ict,9,vals2d,ier); call ckerr(s)

  call hyb3lin2(1,id_f2dherm,x2d,th2d,vals2d(:,2),1,1)
  call hyb3lin2(2,id_f2dherm,x2d,th2d,vals2d(:,2),1,1)
  call hyb3lin2(3,id_f2dherm,x2d,th2d,vals2d(:,2),1,1)

  call hyb3lin2(1,id_f2dspl,x2d,th2d,vals2d(:,3),1,1)
  call hyb3lin2(2,id_f2dspl,x2d,th2d,vals2d(:,3),1,1)
  call hyb3lin2(3,id_f2dspl,x2d,th2d,vals2d(:,3),1,1)

  ict(6)=0
  ict(4)=1  ! d2/drho2
  call eq_hrhochi(9,x2d,th2d,1,ifuns(3:3),ict,9,vals2d,ier); call ckerr(s)

  call hyb3lin2(1,id_f2dspl,x2d,th2d,vals2d(:,1),2,0)
  call hyb3lin2(2,id_f2dspl,x2d,th2d,vals2d(:,1),2,0)
  call hyb3lin2(3,id_f2dspl,x2d,th2d,vals2d(:,1),2,0)

  ict(4)=0
  ict(5)=1  ! d2/dchi2
  call eq_hrhochi(9,x2d,th2d,1,ifuns(3:3),ict,9,vals2d,ier); call ckerr(s)

  call hyb3lin2(1,id_f2dspl,x2d,th2d,vals2d(:,1),0,2)
  call hyb3lin2(2,id_f2dspl,x2d,th2d,vals2d(:,1),0,2)
  call hyb3lin2(3,id_f2dspl,x2d,th2d,vals2d(:,1),0,2)

  write(6,*) ' 3d (piecewise linear + 2d cubic interpolant)'
  write(6,'(3x,a,i4,a,1pe14.7)') &
       ' number of hybrid tests: ',itest,'; max deviation: ',dmax
  write(6,*) ' '

  if(dmax.gt.1.0d-10) then
     write(6,*) '   MAX DEVIATION:  test#: ',itmax
     write(6,*) ' '
  endif
  !=========================================================
  !  f(phi,rho,theta) test (3d)
  !  no f77 interface

  do ichi=1,inchi
     do ix=1,inrho
        do iphi=1,inphi
           f3d(iphi,ix,ichi)=exp(zrho(ix))*cos(zchi(ichi))*sin(zphi(iphi))
        enddo
     enddo
  enddo

  call xplasma_author_set(s,'xplasma2_test',ier); call ckerr(s)

  !  linear
  call xplasma_create_prof(s,'f3dlin',id_phi,id_rho,id_chi,f3d,id_f3dlin,ier)
  call ckerr(s)

  !  Hermite w/BCs

  call xplasma_create_prof(s,'f3dherm',id_phi,id_rho,id_chi,f3d,id_f3dherm, &
       ier,  ispline=1,ibcx2a=1,zbcx2a=f3d(1:inphi,1,1:inchi), &
       ibcx2b=1,zbcx2b=f3d(1:inphi,inrho,1:inchi))
  call ckerr(s)
  
  !  Spline w/BCs

  call xplasma_create_prof(s,'f3dspl',id_phi,id_rho,id_chi,f3d,id_f3dspl, &
       ier,  ispline=2,ibcx2a=1,zbcx2a=f3d(1:inphi,1,1:inchi), &
       ibcx2b=1,zbcx2b=f3d(1:inphi,inrho,1:inchi))
  call ckerr(s)

  call xplasma_author_clear(s,'xplasma2_test',ier); call ckerr(s)

  ! sanity check... return profile verbatim on original grid...

  call xplasma_eval_prof(s,id_f3dspl,f3dret,ier, zcoeff=f3dcoeff)
  call ckerr(s)

  zsum1=0; zsum2=0
  do ichi=1,inchi
     do ix=1,inrho
        do iphi=1,inphi
           zsum1=zsum1+abs(f3d(iphi,ix,ichi))
           zsum2=zsum2+abs(f3dret(iphi,ix,ichi))
        enddo
     enddo
  enddo
  write(6,'(1x,a,2(1x,1pe20.13))') ' f3d/f3dret checksums: ',zsum1,zsum2

  call xplasma_author_set(s,'xplasma2_test',ier); call ckerr(s)

  call xplasma_create_prof(s,'f3dspl2',id_phi,id_rho,id_chi, &
       f3dret,id_fun1,ier, &
       ispline=2,coeffs=f3dcoeff); call ckerr(s)

  call xplasma_author_clear(s,'xplasma2_test',ier); call ckerr(s)

  x3d(1)=x3dcen-zd; th3d(1)=th3dcen-zd; ph3d(1)=ph3dcen-zd
  x3d(2)=x3dcen;    th3d(2)=th3dcen-zd; ph3d(2)=ph3dcen-zd
  x3d(3)=x3dcen+zd; th3d(3)=th3dcen-zd; ph3d(3)=ph3dcen-zd

  x3d(4)=x3dcen-zd; th3d(4)=th3dcen;    ph3d(4)=ph3dcen-zd
  x3d(5)=x3dcen;    th3d(5)=th3dcen;    ph3d(5)=ph3dcen-zd
  x3d(6)=x3dcen+zd; th3d(6)=th3dcen;    ph3d(6)=ph3dcen-zd

  x3d(7)=x3dcen-zd; th3d(7)=th3dcen+zd; ph3d(7)=ph3dcen-zd
  x3d(8)=x3dcen;    th3d(8)=th3dcen+zd; ph3d(8)=ph3dcen-zd
  x3d(9)=x3dcen+zd; th3d(9)=th3dcen+zd; ph3d(9)=ph3dcen-zd

  x3d(10)=x3dcen-zd; th3d(10)=th3dcen-zd; ph3d(10)=ph3dcen
  x3d(11)=x3dcen;    th3d(11)=th3dcen-zd; ph3d(11)=ph3dcen
  x3d(12)=x3dcen+zd; th3d(12)=th3dcen-zd; ph3d(12)=ph3dcen

  x3d(13)=x3dcen-zd; th3d(13)=th3dcen;    ph3d(13)=ph3dcen
  x3d(14)=x3dcen;    th3d(14)=th3dcen;    ph3d(14)=ph3dcen
  x3d(15)=x3dcen+zd; th3d(15)=th3dcen;    ph3d(15)=ph3dcen

  x3d(16)=x3dcen-zd; th3d(16)=th3dcen+zd; ph3d(16)=ph3dcen
  x3d(17)=x3dcen;    th3d(17)=th3dcen+zd; ph3d(17)=ph3dcen
  x3d(18)=x3dcen+zd; th3d(18)=th3dcen+zd; ph3d(18)=ph3dcen

  x3d(19)=x3dcen-zd; th3d(19)=th3dcen-zd; ph3d(19)=ph3dcen+zd
  x3d(20)=x3dcen;    th3d(20)=th3dcen-zd; ph3d(20)=ph3dcen+zd
  x3d(21)=x3dcen+zd; th3d(21)=th3dcen-zd; ph3d(21)=ph3dcen+zd

  x3d(22)=x3dcen-zd; th3d(22)=th3dcen;    ph3d(22)=ph3dcen+zd
  x3d(23)=x3dcen;    th3d(23)=th3dcen;    ph3d(23)=ph3dcen+zd
  x3d(24)=x3dcen+zd; th3d(24)=th3dcen;    ph3d(24)=ph3dcen+zd

  x3d(25)=x3dcen-zd; th3d(25)=th3dcen+zd; ph3d(25)=ph3dcen+zd
  x3d(26)=x3dcen;    th3d(26)=th3dcen+zd; ph3d(26)=ph3dcen+zd
  x3d(27)=x3dcen+zd; th3d(27)=th3dcen+zd; ph3d(27)=ph3dcen+zd

  ifuns(1)=id_f3dlin
  ifuns(2)=id_f3dherm
  ifuns(3)=id_f3dspl
  ifuns(4)=id_fun1

  do i=1,27
     vals3da(i)=exp(x3d(i))*cos(th3d(i))*sin(ph3d(i))
  enddo

  call xplasma_eval_prof(s,id_f3dspl,id_rho,x3d(14),id_phi,ph3d(14), &
       id_chi,th3d(14),ztmp(1),ier); call ckerr(s)
  write(6,1400) '   3d spline singleton: ',ztmp(1)

  call xplasma_eval_prof(s,ifuns(1:4),id_chi,th3d,id_phi,ph3d,id_rho,x3d, &
       vals3d,ier); call ckerr(s)

  do ii=1,27
     if(vals3d(ii,3).ne.vals3d(ii,4)) then
        write(6,*) ' ? 3d Spline clone profile eval not identical: ', &
             ii,vals3d(ii,2),vals3d(ii,4)
     endif
  enddo

  write(6,1400) '   values: L H S A: ',vals3d(14,1:3),vals3da(14)

  ztmp(1:3)=(vals3d(15,1:3)-vals3d(13,1:3))/(2*zd)
  ztmp(4)=(vals3da(15)-vals3da(13))/(2*zd)
  write(6,1400) '        f.d. df/dx: ',ztmp(1:4)

  ztmp(1:3)=(vals3d(17,1:3)-vals3d(11,1:3))/(2*zd)
  ztmp(4)=(vals3da(17)-vals3da(11))/(2*zd)
  write(6,1400) '       f.d. df/dth: ',ztmp(1:4)

  ztmp(1:3)=(vals3d(23,1:3)-vals3d(5,1:3))/(2*zd)
  ztmp(4)=(vals3da(23)-vals3da(5))/(2*zd)
  write(6,1400) '       f.d. df/dph: ',ztmp(1:4)

  ztmp(1:3)=(vals3d(24,1:3)-vals3d(6,1:3)-vals3d(22,1:3)+vals3d(4,1:3))/ &
       (4*zd*zd)
  ztmp(4)=(vals3da(24)-vals3da(6)-vals3da(22)+vals3da(4))/ &
       (4*zd*zd)
  write(6,1400) '    f.d. d2f/dxdph: ',ztmp(1:4)

  ztmp(1:3)=(vals3d(18,1:3)-vals3d(16,1:3)-vals3d(12,1:3)+vals3d(10,1:3))/ &
       (4*zd*zd)
  ztmp(4)=(vals3da(18)-vals3da(16)-vals3da(12)+vals3da(10))/ &
       (4*zd*zd)
  write(6,1400) '    f.d. d2f/dxdth: ',ztmp(1:4)

  ztmp(1:3)=(vals3d(26,1:3)-vals3d(8,1:3)-vals3d(20,1:3)+vals3d(2,1:3))/ &
       (4*zd*zd)
  ztmp(4)=(vals3da(26)-vals3da(8)-vals3da(20)+vals3da(2))/ &
       (4*zd*zd)
  write(6,1400) '   f.d. d2f/dthdph: ',ztmp(1:4)

  ztmp(1:3)=(vals3d(27,1:3)-vals3d(9,1:3)-vals3d(21,1:3)+vals3d(3,1:3) &
       -vals3d(25,1:3)+vals3d(7,1:3)+vals3d(19,1:3)-vals3d(1,1:3))/ &
       (8*zd*zd*zd)

  ztmp(4)=(vals3da(27)-vals3da(9)-vals3da(21)+vals3da(3) &
       -vals3da(25)+vals3da(7)+vals3da(19)-vals3da(1))/ &
       (8*zd*zd*zd)

  write(6,*) ' ...platform to platform variation likely in following line:'
  write(6,1400) ' f.d. d3f/dxdthdph: ',ztmp(1:4)

  write(6,*) ' '

  call xplasma_eval_prof(s,ifuns(1:4),id_chi,th3d(14),id_rho,x3d(14), &
       id_phi,ph3d(14),ztmp(1:4),ier,ideriv2=1)
  write(6,1401) '     interp. df/dx: ',ztmp(1:3)
  if(ztmp(3).ne.ztmp(4)) then
     write(6,*) ' clone 3dspl fcn ideriv2=1 eval difference: ',ztmp(2),ztmp(4)
  endif

  call xplasma_eval_prof(s,ifuns(1:4),id_chi,th3d(14),id_rho,x3d(14), &
       id_phi,ph3d(14),ztmp(1:4),ier,ideriv1=1)
  write(6,1401) '    interp. df/dth: ',ztmp(1:3)
  if(ztmp(3).ne.ztmp(4)) then
     write(6,*) ' clone 3dspl fcn ideriv1=1 eval difference: ',ztmp(2),ztmp(4)
  endif

  call xplasma_eval_prof(s,ifuns(1:4),id_chi,th3d(14),id_rho,x3d(14), &
       id_phi,ph3d(14),ztmp(1:4),ier,ideriv3=1)
  write(6,1401) '    interp. df/dph: ',ztmp(1:3)
  if(ztmp(3).ne.ztmp(4)) then
     write(6,*) ' clone 3dspl fcn ideriv3=1 eval difference: ',ztmp(2),ztmp(4)
  endif

  call xplasma_eval_prof(s,ifuns(1:3),id_chi,th3d(14),id_rho,x3d(14), &
       id_phi,ph3d(14),ztmp(1:3),ier,ideriv2=1,ideriv3=1)
  write(6,1401) ' interp. d2f/dxdph: ',ztmp(1:3)

  call xplasma_eval_prof(s,ifuns(1:3),id_chi,th3d(14),id_rho,x3d(14), &
       id_phi,ph3d(14),ztmp(1:3),ier,ideriv2=1,ideriv1=1)
  write(6,1401) ' interp. d2f/dxdth: ',ztmp(1:3)

  call xplasma_eval_prof(s,ifuns(1:3),id_chi,th3d(14),id_rho,x3d(14), &
       id_phi,ph3d(14),ztmp(1:3),ier,ideriv1=1,ideriv3=1)
  write(6,1401) 'interp. d2f/dthdph: ',ztmp(1:3)

  call xplasma_eval_prof(s,ifuns(1:3),id_chi,th3d(14),id_rho,x3d(14), &
       id_phi,ph3d(14),ztmp(1:3),ier,ideriv3=1,ideriv1=1,ideriv2=1)
  write(6,1401) 'intrp d3f/dxdthdph: ',ztmp(1:3)

  call xplasma_hermitize(s,id_fun1,ier); call ckerr(s)
  do ideriv3=0,1
     do ideriv2=0,1
        do ideriv1=0,1
           call xplasma_eval_prof(s,id_f3dspl, &
                id_chi,th3d, id_rho,x3d, id_phi, ph3d, yy3d, ier, &
                ideriv1=ideriv1, ideriv2=ideriv2, ideriv3=ideriv3)
           call ckerr(s)
           call xplasma_eval_prof(s,id_fun1, &
                id_chi,th3d, id_rho,x3d, id_phi, ph3d, yy3e, ier, &
                ideriv1=ideriv1, ideriv2=ideriv2, ideriv3=ideriv3)
           call ckerr(s)

           zscale = maxval(abs(yy3d))
           zfac=1.0d-14
           if(ideriv1+ideriv2+ideriv3.eq.1) zfac=1.0d-12
           if(ideriv1+ideriv2+ideriv3.eq.2) zfac=1.0d-11
           if(ideriv1+ideriv2+ideriv3.eq.3) zfac=1.0d-10

           do ix=1,size(x3d)
              if(abs(yy3d(ix)-yy3e(ix)).gt.(zfac*zscale)) then
                 write(6,*) ' ?unexpected difference: ideriv1,2,3=', &
                      ideriv1,ideriv2,ideriv3
                 write(6,*) '    yy3d, yy3e (',ix,'): ',yy3d(ix),yy3e(ix)
              endif
           enddo
        enddo
     enddo
  enddo

  call xplasma_author_set(s,'xplasma2_test',ier); call ckerr(s)
  call xplasma_remove_item(s,id_fun1,ier); call ckerr(s)
  call xplasma_author_clear(s,'xplasma2_test',ier); call ckerr(s)

  !=========================================================
  !  step function tests -- separate routine

  call test_stepfuns

  !=========================================================
  !  final tests:  copy and i/o -- 3 identical files should be created.
  !  if they are not identical then there is an error in the code...

  call xplasma_copy(s,s1,ier); call ckerr(s)

  call xplasma_write(s,'foo.cdf',ier); call ckerr(s)

  call xplasma_write(s1,'foo1.cdf',ier); call ckerr(s1)

  call xplasma_read(s2,'foo1.cdf',ier); call ckerr(s2)

  call xplasma_write(s2,'foo2.cdf',ier); call ckerr(s2)

  call xplasma_pack(s2, ipack, r8pack, ier); call ckerr(s2)
  call xplasma_unpack(s2, ipack, r8pack, ier); call ckerr(s2)

  call xplasma_write(s2,'foo3.cdf',ier); call ckerr(s2)

  deallocate(ipack,r8pack)

  call xplasma_init(s2,'empty s2',ier,total=.TRUE.); call ckerr(s2)
  call xplasma_init(s1,'empty s1',ier,total=.TRUE.); call ckerr(s1)

  write(6,*) ' '
  write(6,*) ' xpdebug: '
  call xpdebug(s)
  call xplasma_init(s,'empty s',ier); call ckerr(s)

  write(6,*) ' '
  write(6,*) ' all done.'

  !---------------------------

  contains

    subroutine ckerr(t)
      type (xplasma), pointer :: t
      if(ier.ne.0) then
         call xplasma_error(t,ier,6)
         if(ier.gt.0) call ps_bad_exit
      endif
    end subroutine ckerr

    !---------------------------

    subroutine ckgrid(icoord)

      integer,  intent(in) :: icoord

      !  info on a coordinate & 1 grid for the coordinate...

      integer :: inum,idg,jcoord
      character*32 zname
      logical periodic
      real*8 :: xmin,xmax
      real*8, dimension(:), allocatable :: xgrid

      call xplasma_coord_info(s,icoord,ier, &
           periodic=periodic, ngrids=inum, xmin=xmin, xmax=xmax)

      if(periodic) write(6,*) ' ...periodic coordinate.'
      write(6,'(2x,a,2(1pe20.13))') ' nominal range: ',xmin,xmax
      write(6,*) ' #grids = ',inum

      call xplasma_coord_gridId(s,icoord,inum,idg,ier); call ckerr(s)
      call xplasma_get_item_info(s,idg,ier,name=zname); call ckerr(s)
      write(6,*) '  grid#',inum,' is: ',trim(zname)

      call xplasma_grid_size(s,idg,inum,ier); call ckerr(s)
      write(6,*) '    no. of pts in grid: ', inum

      allocate(xgrid(inum))
      call xplasma_grid(s,idg,xgrid,ier); call ckerr(s)
      write(6,'(3(1x,1pe20.13))') xgrid
      deallocate(xgrid)

      call xplasma_grid_info(s,idg,ier, coord=jcoord); call ckerr(s)
      if(icoord.ne.jcoord) then
         write(6,*) ' ?ckgrid(xplasma2_test): icoord/jcoord mismatch.'
         call ps_bad_exit
      endif

    end subroutine ckgrid

    subroutine hyb2dzon(id1,id2,zxtest,zresult,ideriv)
      integer, intent(in) :: id1  ! id for 1st dimension (profile or XX)
      integer, intent(in) :: id2  ! id for 2nd dimension (XX or profile)
      real*8, intent(in), dimension(:) :: zxtest  ! profile dim test vector
      real*8, intent(in), dimension(:) :: zresult ! profile dim values expected
      integer, intent(in) :: ideriv

      ! construct zone-pclin or zone-spline or zone-hermite 2d Hybrid; evaluate
      ! and compare to expected result.

      !----------------------
      integer :: itype1,itype2,isize_xx,ii,idprof,hspline(2),idg,irank,inx
      integer :: idtmp,idt2(2),idt2a(2)
      real*8 :: result_local(size(zresult),2),zxxwk(size(zresult))
      real*8, dimension(:), allocatable :: xxzc,xxfac,xxoff,bcvals
      real*8, dimension(:,:), allocatable :: pdata
      real*8, parameter :: xxincr = 2.5d0
      real*8 :: xxoffs,dmaxp
      !----------------------

      itest = itest + 1  ! increment test counter

      isize_xx=size(xx)
      allocate(xxzc(isize_xx-1),xxfac(isize_xx-1),xxoff(isize_xx-1))
      allocate(bcvals(isize_xx-1))

      xxzc = (xx(1:isize_xx-1)+xx(2:isize_xx))/2

      xxoffs = xx(isize_xx)/(isize_xx-1)/10.0d0
      xxfac(1)=1.0d0
      xxoff(1)=-xxoffs/2
      do ii=2,isize_xx-1
         xxfac(ii) = xxfac(ii-1) + xxincr
         xxoff(ii) = xxoff(ii-1) + xxoffs
      enddo

      call xplasma_get_item_info(s,id1,ier, itype=itype1); call ckerr(s)
      call xplasma_get_item_info(s,id2,ier, itype=itype2); call ckerr(s)

      if(itype1.eq.xplasma_gridType) then
         idprof=id2
      else
         idprof=id1
      endif
      idt2(1)=idprof

      call xplasma_prof_info(s,idprof,ier, splineType=ispline, &
           rank=irank, gridId1=idg); call ckerr(s)
      if(irank.ne.1) then
         write(6,*) ' ?hyb2dzon: input profile not 1d, id=',idprof
         call ps_bad_exit
      endif

      call xplasma_grid_info(s,idg,ier, size=inx)

      call xplasma_author_set(s,'hyb2dzon',ier); call ckerr(s)

      bcvals(1)=z2pi
      if(itype1.eq.xplasma_gridType) then
         hspline(1)=-1
         hspline(2)=ispline
         allocate(pdata(isize_xx-1,inx))
         call xplasma_getprof_1data(s,idprof,pdata(1,1:inx),ier); call ckerr(s)
         do ii=2,isize_xx-1
            pdata(ii,1:inx)=xxfac(ii)*pdata(1,1:inx)
            bcvals(ii)=xxfac(ii)*bcvals(1)
         enddo

         if(ispline.gt.0) then
            call xplasma_create_2dprof(s,'_TMP_HYB2', &
                 idxxg,idg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx2a=1,zbcx2a=bcvals,ibcx2b=1,zbcx2b=bcvals)
         else
            call xplasma_create_2dprof(s,'_TMP_HYB2', &
                 idxxg,idg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)

      else
         hspline(1)=ispline
         hspline(2)=-1
         allocate(pdata(inx,isize_xx-1))
         call xplasma_getprof_1data(s,idprof,pdata(1:inx,1),ier); call ckerr(s)
         do ii=2,isize_xx-1
            pdata(1:inx,ii)=xxfac(ii)*pdata(1:inx,1)
            bcvals(ii)=xxfac(ii)*bcvals(1)
         enddo

         if(ispline.gt.0) then
            call xplasma_create_2dprof(s,'_TMP_HYB2', &
                 idg,idxxg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx1a=1,zbcx1a=bcvals,ibcx1b=1,zbcx1b=bcvals)
         else
            call xplasma_create_2dprof(s,'_TMP_HYB2', &
                 idg,idxxg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)

      endif

      idt2(2)=idtmp

      idt2a(1)=idt2(2)
      idt2a(2)=idt2(1)

      dmaxp=dmax
      do ii=1,isize_xx-1
         zxxwk=xxzc(ii)+xxoff(ii)
         call xplasma_eval_prof(s,idtmp, &
              idxxg,zxxwk, idg,zxtest, result_local(:,1),ier, &
              ideriv2=ideriv); call ckerr(s)
         dmax = max(dmax, &
              maxval(abs(result_local(:,1)/xxfac(ii) - zresult)) )
         
         call xplasma_eval_prof(s,idt2, &
              idxxg,zxxwk, idg,zxtest, result_local,ier, &
              ideriv2=ideriv); call ckerr(s)
         dmax = max(dmax, maxval(abs(result_local(:,1) - zresult)) )
         dmax = max(dmax, &
              maxval(abs(result_local(:,2)/xxfac(ii) - zresult)) )

         call xplasma_eval_prof(s,idt2a, &
              idg,zxtest, idxxg,zxxwk, result_local,ier, &
              ideriv1=ideriv); call ckerr(s)
         dmax = max(dmax, maxval(abs(result_local(:,2) - zresult)) )
         dmax = max(dmax, &
              maxval(abs(result_local(:,1)/xxfac(ii) - zresult)) )
      enddo
      if(dmax.gt.dmaxp) itmax=itest

      deallocate(xxzc,xxfac,xxoff,bcvals,pdata)

      call xplasma_remove_item(s,idtmp,ier)
      call xplasma_author_clear(s,'hyb2dzon',ier); call ckerr(s)

    end subroutine hyb2dzon

    subroutine hyb2dlin(id1,id2,zxtest,zresult,ideriv)
      integer, intent(in) :: id1  ! id for 1st dimension (profile or XX)
      integer, intent(in) :: id2  ! id for 2nd dimension (XX or profile)
      real*8, intent(in), dimension(:) :: zxtest  ! profile dim test vector
      real*8, intent(in), dimension(:) :: zresult ! profile dim values expected
      integer, intent(in) :: ideriv

      ! construct pclin-spline or pclin-hermite 2d Hybrid; evaluate
      ! and compare to expected result.

      !----------------------
      integer :: itype1,itype2,isize_xx,ii,idprof,hspline(2),idg,irank,inx
      integer :: idtmp,idt2(2),idt2a(2)
      real*8 :: result_local(size(zresult),2),zxxwk(size(zresult))
      real*8, dimension(:), allocatable :: xxzc,xxfac,xxoff,bcvals,fxfac
      real*8, dimension(:,:), allocatable :: pdata
      real*8 :: xxincr
      real*8 :: xxoffs,dmaxp
      !----------------------

      itest = itest + 1  ! increment test counter

      isize_xx=size(xx)
      allocate(xxzc(isize_xx-1),xxfac(isize_xx),xxoff(isize_xx-1))
      allocate(bcvals(isize_xx),fxfac(isize_xx-1))

      xxzc = (xx(1:isize_xx-1)+xx(2:isize_xx))/2

      xxoffs = xx(isize_xx)/(isize_xx-1)/10.0d0
      xxfac(1)=1.0d0
      xxoff(1)=-xxoffs/2
      xxincr = 2.5d0
      do ii=2,isize_xx-1
         xxfac(ii) = xxfac(ii-1) + xxincr
         xxoff(ii) = xxoff(ii-1) + xxoffs
         xxincr = xxincr - 0.5d0
      enddo
      xxfac(isize_xx)=xxfac(isize_xx-1) + xxincr
      do ii=1,isize_xx-1
         !  linear interpolation factor
         fxfac(ii) = (xxzc(ii)+xxoff(ii)-xx(ii))/(xx(ii+1)-xx(ii))
         fxfac(ii) = xxfac(ii) + fxfac(ii)*(xxfac(ii+1)-xxfac(ii))
      enddo

      call xplasma_get_item_info(s,id1,ier, itype=itype1); call ckerr(s)
      call xplasma_get_item_info(s,id2,ier, itype=itype2); call ckerr(s)

      if(itype1.eq.xplasma_gridType) then
         idprof=id2
      else
         idprof=id1
      endif
      idt2(1)=idprof

      call xplasma_prof_info(s,idprof,ier, splineType=ispline, &
           rank=irank, gridId1=idg); call ckerr(s)
      if(irank.ne.1) then
         write(6,*) ' ?hyb2dlin: input profile not 1d, id=',idprof
         call ps_bad_exit
      endif

      call xplasma_grid_info(s,idg,ier, size=inx)

      call xplasma_author_set(s,'hyb2dlin',ier); call ckerr(s)

      bcvals(1)=z2pi
      if(itype1.eq.xplasma_gridType) then
         hspline(1)=0
         hspline(2)=ispline
         allocate(pdata(isize_xx,inx))
         call xplasma_getprof_1data(s,idprof,pdata(1,1:inx),ier); call ckerr(s)
         do ii=2,isize_xx
            pdata(ii,1:inx)=xxfac(ii)*pdata(1,1:inx)
            bcvals(ii)=xxfac(ii)*bcvals(1)
         enddo

         if(ispline.gt.0) then
            call xplasma_create_2dprof(s,'_TMP_HYB2', &
                 idxxg,idg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx2a=1,zbcx2a=bcvals,ibcx2b=1,zbcx2b=bcvals)
         else
            call xplasma_create_2dprof(s,'_TMP_HYB2', &
                 idxxg,idg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)

      else
         hspline(1)=ispline
         hspline(2)=0
         allocate(pdata(inx,isize_xx))
         call xplasma_getprof_1data(s,idprof,pdata(1:inx,1),ier); call ckerr(s)
         do ii=2,isize_xx
            pdata(1:inx,ii)=xxfac(ii)*pdata(1:inx,1)
            bcvals(ii)=xxfac(ii)*bcvals(1)
         enddo

         if(ispline.gt.0) then
            call xplasma_create_2dprof(s,'_TMP_HYB2', &
                 idg,idxxg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx1a=1,zbcx1a=bcvals,ibcx1b=1,zbcx1b=bcvals)
         else
            call xplasma_create_2dprof(s,'_TMP_HYB2', &
                 idg,idxxg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)

      endif

      idt2(2)=idtmp

      idt2a(1)=idt2(2)
      idt2a(2)=idt2(1)

      dmaxp=dmax
      do ii=1,isize_xx-1
         zxxwk=xxzc(ii)+xxoff(ii)
         call xplasma_eval_prof(s,idtmp, &
              idxxg,zxxwk, idg,zxtest, result_local(:,1),ier, &
              ideriv2=ideriv); call ckerr(s)
         dmax = max(dmax, &
              maxval(abs(result_local(:,1)/fxfac(ii) - zresult)) )
         
         call xplasma_eval_prof(s,idt2, &
              idxxg,zxxwk, idg,zxtest, result_local,ier, &
              ideriv2=ideriv); call ckerr(s)
         dmax = max(dmax, maxval(abs(result_local(:,1) - zresult)) )
         dmax = max(dmax, &
              maxval(abs(result_local(:,2)/fxfac(ii) - zresult)) )

         call xplasma_eval_prof(s,idt2a, &
              idg,zxtest, idxxg,zxxwk, result_local,ier, &
              ideriv1=ideriv); call ckerr(s)
         dmax = max(dmax, maxval(abs(result_local(:,2) - zresult)) )
         dmax = max(dmax, &
              maxval(abs(result_local(:,1)/fxfac(ii) - zresult)) )
      enddo
      if(dmax.gt.dmaxp) itmax=itest

      deallocate(xxzc,xxfac,xxoff,bcvals,pdata,fxfac)

      call xplasma_remove_item(s,idtmp,ier)
      call xplasma_author_clear(s,'hyb2dlin',ier); call ckerr(s)

    end subroutine hyb2dlin

    subroutine hyb3dzon(id1,id2,id3,zxtest,zresult,ideriv)
      integer, intent(in) :: id1  ! id for 1st dimension (profile or XX)
      integer, intent(in) :: id2  ! id for 2nd dimension (XX or YY or profile)
      integer, intent(in) :: id3  ! id for 3rd dimension (YY or profile)
      real*8, intent(in), dimension(:) :: zxtest  ! profile dim test vector
      real*8, intent(in), dimension(:) :: zresult ! profile dim values expected
      integer, intent(in) :: ideriv

      ! construct zone-pclin or zone-spline or zone-hermite 2d Hybrid; evaluate
      ! and compare to expected result.

      !----------------------
      integer :: itype1,itype2,itype3,isize_xx,isize_yy
      integer :: ii,jj,idprof,hspline(3),idg,irank,inx
      integer :: idtmp,idt2(2),idt2a(2)
      real*8 :: result_local(size(zresult),2),zxxwk(size(zresult))
      real*8 :: zyywk(size(zresult))
      real*8, dimension(:), allocatable :: xxzc,xxfac,xxoff
      real*8, dimension(:), allocatable :: yyzc,yyfac,yyoff
      real*8, dimension(:,:), allocatable :: bcvals
      real*8, dimension(:,:,:), allocatable :: pdata
      real*8, parameter :: xxincr = 2.5d0
      real*8 :: xxoffs,dmaxp
      !----------------------

      itest = itest + 1  ! increment test counter

      isize_xx=size(xx)
      allocate(xxzc(isize_xx-1),xxfac(isize_xx-1),xxoff(isize_xx-1))

      isize_yy=size(yy)
      allocate(yyzc(isize_yy-1),yyfac(isize_yy-1),yyoff(isize_yy-1))

      allocate(bcvals(isize_xx-1,isize_yy-1))

      xxzc = (xx(1:isize_xx-1)+xx(2:isize_xx))/2
      yyzc = (yy(1:isize_yy-1)+yy(2:isize_yy))/2

      xxoffs = xx(isize_xx)/(isize_xx-1)/10.0d0
      xxfac(1)=1.0d0
      xxoff(1)=-xxoffs/2
      yyfac(1)=1.0d0
      yyoff(1)=-3*xxoffs/2

      do ii=2,isize_xx-1
         xxfac(ii) = xxfac(ii-1) + xxincr
         xxoff(ii) = xxoff(ii-1) + xxoffs
      enddo

      do ii=2,isize_yy-1
         yyfac(ii) = yyfac(ii-1) + xxincr
         yyoff(ii) = yyoff(ii-1) + xxoffs
      enddo

      call xplasma_get_item_info(s,id1,ier, itype=itype1); call ckerr(s)
      call xplasma_get_item_info(s,id2,ier, itype=itype2); call ckerr(s)
      call xplasma_get_item_info(s,id3,ier, itype=itype3); call ckerr(s)

      if(itype3.eq.xplasma_profType) then
         idprof=id3
      else if(itype2.eq.xplasma_profType) then
         idprof=id2
      else
         idprof=id1
      endif
      idt2(1)=idprof

      call xplasma_prof_info(s,idprof,ier, splineType=ispline, &
           rank=irank, gridId1=idg); call ckerr(s)
      if(irank.ne.1) then
         write(6,*) ' ?hyb3dzon: input profile not 1d, id=',idprof
         call ps_bad_exit
      endif

      call xplasma_grid_info(s,idg,ier, size=inx)

      call xplasma_author_set(s,'hyb3dzon',ier); call ckerr(s)

      bcvals(1,1)=z2pi
      if(itype1.eq.xplasma_profType) then
         hspline(1)=ispline
         hspline(2)=-1
         hspline(3)=-1
         allocate(pdata(inx,isize_xx-1,isize_yy-1))
         call xplasma_getprof_1data(s,idprof,pdata(1:inx,1,1),ier)
         call ckerr(s)
         do jj=1,isize_yy-1
            do ii=1,isize_xx-1
               pdata(1:inx,ii,jj)=xxfac(ii)*yyfac(jj)*pdata(1:inx,1,1)
               bcvals(ii,jj)=xxfac(ii)*yyfac(jj)*bcvals(1,1)
            enddo
         enddo

         if(ispline.gt.0) then
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idg,idxxg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx1a=1,zbcx1a=bcvals,ibcx1b=1,zbcx1b=bcvals)
         else
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idg,idxxg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)
      endif

      if(itype2.eq.xplasma_profType) then
         hspline(1)=-1
         hspline(2)=ispline
         hspline(3)=-1
         allocate(pdata(isize_xx-1,inx,isize_yy-1))
         call xplasma_getprof_1data(s,idprof,pdata(1,1:inx,1),ier)
         call ckerr(s)
         do jj=1,isize_yy-1
            do ii=1,isize_xx-1
               pdata(ii,1:inx,jj)=xxfac(ii)*yyfac(jj)*pdata(1,1:inx,1)
               bcvals(ii,jj)=xxfac(ii)*yyfac(jj)*bcvals(1,1)
            enddo
         enddo

         if(ispline.gt.0) then
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx2a=1,zbcx2a=bcvals,ibcx2b=1,zbcx2b=bcvals)
         else
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)
      endif

      if(itype3.eq.xplasma_profType) then
         hspline(1)=-1
         hspline(2)=-1
         hspline(3)=ispline
         allocate(pdata(isize_xx-1,isize_yy-1,inx))
         call xplasma_getprof_1data(s,idprof,pdata(1,1,1:inx),ier)
         call ckerr(s)
         do jj=1,isize_yy-1
            do ii=1,isize_xx-1
               pdata(ii,jj,1:inx)=xxfac(ii)*yyfac(jj)*pdata(1,1,1:inx)
               bcvals(ii,jj)=xxfac(ii)*yyfac(jj)*bcvals(1,1)
            enddo
         enddo

         if(ispline.gt.0) then
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idyyg,idg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx3a=1,zbcx3a=bcvals,ibcx3b=1,zbcx3b=bcvals)
         else
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idyyg,idg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)
      endif

      idt2(2)=idtmp

      idt2a(1)=idt2(2)
      idt2a(2)=idt2(1)

      dmaxp=dmax
      do jj=1,isize_yy-1
         zyywk=yyzc(jj)+yyoff(jj)
         do ii=1,isize_xx-1
            zxxwk=xxzc(ii)+xxoff(ii)
            call xplasma_eval_prof(s,idtmp, &
                 idxxg,zxxwk, idyyg,zyywk, idg,zxtest, result_local(:,1),ier, &
                 ideriv3=ideriv); call ckerr(s)
            dmax = max(dmax, &
                 maxval(abs(result_local(:,1)/xxfac(ii)/yyfac(jj) - zresult)) )
         
            call xplasma_eval_prof(s,idt2, &
                 idxxg,zxxwk, idyyg,zyywk, idg,zxtest, result_local,ier, &
                 ideriv3=ideriv); call ckerr(s)
            dmax = max(dmax, maxval(abs(result_local(:,1) - zresult)) )
            dmax = max(dmax, &
                 maxval(abs(result_local(:,2)/xxfac(ii)/yyfac(jj) - zresult)) )

            call xplasma_eval_prof(s,idt2a, &
                 idg,zxtest, idxxg,zxxwk, idyyg,zyywk, result_local,ier, &
                 ideriv1=ideriv); call ckerr(s)
            dmax = max(dmax, maxval(abs(result_local(:,2) - zresult)) )
            dmax = max(dmax, &
                 maxval(abs(result_local(:,1)/xxfac(ii)/yyfac(jj) - zresult)) )
         enddo
      enddo
      if(dmax.gt.dmaxp) itmax=itest

      deallocate(xxzc,xxfac,xxoff,bcvals,pdata)
      deallocate(yyzc,yyfac,yyoff)

      call xplasma_remove_item(s,idtmp,ier)
      call xplasma_author_clear(s,'hyb3dzon',ier); call ckerr(s)

    end subroutine hyb3dzon

    subroutine hyb3dlin(id1,id2,id3,zxtest,zresult,ideriv)
      integer, intent(in) :: id1  ! id for 1st dimension (profile or XX)
      integer, intent(in) :: id2  ! id for 2nd dimension (XX or YY or profile)
      integer, intent(in) :: id3  ! id for 3rd dimension (YY or profile)
      real*8, intent(in), dimension(:) :: zxtest  ! profile dim test vector
      real*8, intent(in), dimension(:) :: zresult ! profile dim values expected
      integer, intent(in) :: ideriv

      ! construct lin-hermite or lin-spline hybrid interpolant
      ! and compare evaluation to expected result.

      !----------------------
      integer :: itype1,itype2,itype3,isize_xx,isize_yy
      integer :: ii,jj,idprof,hspline(3),idg,irank,inx
      integer :: idtmp,idt2(2),idt2a(2)
      real*8 :: result_local(size(zresult),2),zxxwk(size(zresult))
      real*8 :: zyywk(size(zresult))
      real*8, dimension(:), allocatable :: xxzc,xxfac,xxoff
      real*8, dimension(:), allocatable :: yyzc,yyfac,yyoff
      real*8, dimension(:,:), allocatable :: bcvals,fffac
      real*8, dimension(:,:,:), allocatable :: pdata
      real*8, parameter :: xxincr = 2.5d0
      real*8 :: xxoffs,dmaxp,fjj,fii
      !----------------------

      itest = itest + 1  ! increment test counter

      isize_xx=size(xx)
      allocate(xxzc(isize_xx-1),xxfac(isize_xx),xxoff(isize_xx-1))

      isize_yy=size(yy)
      allocate(yyzc(isize_yy-1),yyfac(isize_yy),yyoff(isize_yy-1))

      allocate(bcvals(isize_xx,isize_yy),fffac(isize_xx-1,isize_yy-1))

      xxzc = (xx(1:isize_xx-1)+xx(2:isize_xx))/2
      yyzc = (yy(1:isize_yy-1)+yy(2:isize_yy))/2

      xxoffs = xx(isize_xx)/(isize_xx-1)/10.0d0
      xxfac(1)=1.0d0
      xxoff(1)=-xxoffs/2
      yyfac(1)=1.0d0
      yyoff(1)=-3*xxoffs/2

      do ii=2,isize_xx-1
         xxfac(ii) = xxfac(ii-1) + xxincr
         xxoff(ii) = xxoff(ii-1) + xxoffs
      enddo
      xxfac(isize_xx)=xxfac(isize_xx-1) + xxincr

      do ii=2,isize_yy-1
         yyfac(ii) = yyfac(ii-1) + xxincr
         yyoff(ii) = yyoff(ii-1) + xxoffs
      enddo
      yyfac(isize_yy)=yyfac(isize_yy-1) + xxincr

      do jj=1,isize_yy-1
         fjj = (yyzc(jj)+yyoff(jj)-yy(jj))/(yy(jj+1)-yy(jj))
         do ii=1,isize_xx-1
            fii = (xxzc(ii)+xxoff(ii)-xx(ii))/(xx(ii+1)-xx(ii))
            fffac(ii,jj) = &
                 (1-fjj)*yyfac(jj)*((1-fii)*xxfac(ii)+fii*xxfac(ii+1)) + &
                 fjj*yyfac(jj+1)*((1-fii)*xxfac(ii)+fii*xxfac(ii+1))
         enddo
      enddo

      call xplasma_get_item_info(s,id1,ier, itype=itype1); call ckerr(s)
      call xplasma_get_item_info(s,id2,ier, itype=itype2); call ckerr(s)
      call xplasma_get_item_info(s,id3,ier, itype=itype3); call ckerr(s)

      if(itype3.eq.xplasma_profType) then
         idprof=id3
      else if(itype2.eq.xplasma_profType) then
         idprof=id2
      else
         idprof=id1
      endif
      idt2(1)=idprof

      call xplasma_prof_info(s,idprof,ier, splineType=ispline, &
           rank=irank, gridId1=idg); call ckerr(s)
      if(irank.ne.1) then
         write(6,*) ' ?hyb3dlin: input profile not 1d, id=',idprof
         call ps_bad_exit
      endif

      call xplasma_grid_info(s,idg,ier, size=inx)

      call xplasma_author_set(s,'hyb3dlin',ier); call ckerr(s)

      bcvals(1,1)=z2pi
      if(itype1.eq.xplasma_profType) then
         hspline(1)=ispline
         hspline(2)=0
         hspline(3)=0
         allocate(pdata(inx,isize_xx,isize_yy))
         call xplasma_getprof_1data(s,idprof,pdata(1:inx,1,1),ier)
         call ckerr(s)
         do jj=1,isize_yy
            do ii=1,isize_xx
               pdata(1:inx,ii,jj)=xxfac(ii)*yyfac(jj)*pdata(1:inx,1,1)
               bcvals(ii,jj)=xxfac(ii)*yyfac(jj)*bcvals(1,1)
            enddo
         enddo

         if(ispline.gt.0) then
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idg,idxxg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx1a=1,zbcx1a=bcvals,ibcx1b=1,zbcx1b=bcvals)
         else
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idg,idxxg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)
      endif

      if(itype2.eq.xplasma_profType) then
         hspline(1)=0
         hspline(2)=ispline
         hspline(3)=0
         allocate(pdata(isize_xx,inx,isize_yy))
         call xplasma_getprof_1data(s,idprof,pdata(1,1:inx,1),ier)
         call ckerr(s)
         do jj=1,isize_yy
            do ii=1,isize_xx
               pdata(ii,1:inx,jj)=xxfac(ii)*yyfac(jj)*pdata(1,1:inx,1)
               bcvals(ii,jj)=xxfac(ii)*yyfac(jj)*bcvals(1,1)
            enddo
         enddo

         if(ispline.gt.0) then
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx2a=1,zbcx2a=bcvals,ibcx2b=1,zbcx2b=bcvals)
         else
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)
      endif

      if(itype3.eq.xplasma_profType) then
         hspline(1)=0
         hspline(2)=0
         hspline(3)=ispline
         allocate(pdata(isize_xx,isize_yy,inx))
         call xplasma_getprof_1data(s,idprof,pdata(1,1,1:inx),ier)
         call ckerr(s)
         do jj=1,isize_yy
            do ii=1,isize_xx
               pdata(ii,jj,1:inx)=xxfac(ii)*yyfac(jj)*pdata(1,1,1:inx)
               bcvals(ii,jj)=xxfac(ii)*yyfac(jj)*bcvals(1,1)
            enddo
         enddo

         if(ispline.gt.0) then
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idyyg,idg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx3a=1,zbcx3a=bcvals,ibcx3b=1,zbcx3b=bcvals)
         else
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idyyg,idg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)
      endif

      idt2(2)=idtmp

      idt2a(1)=idt2(2)
      idt2a(2)=idt2(1)

      dmaxp=dmax
      do jj=1,isize_yy-1
         zyywk=yyzc(jj)+yyoff(jj)
         do ii=1,isize_xx-1
            zxxwk=xxzc(ii)+xxoff(ii)
            call xplasma_eval_prof(s,idtmp, &
                 idxxg,zxxwk, idyyg,zyywk, idg,zxtest, result_local(:,1),ier, &
                 ideriv3=ideriv); call ckerr(s)
            dmax = max(dmax, &
                 maxval(abs(result_local(:,1)/fffac(ii,jj) - zresult)) )
         
            call xplasma_eval_prof(s,idt2, &
                 idxxg,zxxwk, idyyg,zyywk, idg,zxtest, result_local,ier, &
                 ideriv3=ideriv); call ckerr(s)
            dmax = max(dmax, maxval(abs(result_local(:,1) - zresult)) )
            dmax = max(dmax, &
                 maxval(abs(result_local(:,2)/fffac(ii,jj) - zresult)) )

            call xplasma_eval_prof(s,idt2a, &
                 idg,zxtest, idxxg,zxxwk, idyyg,zyywk, result_local,ier, &
                 ideriv1=ideriv); call ckerr(s)
            dmax = max(dmax, maxval(abs(result_local(:,2) - zresult)) )
            dmax = max(dmax, &
                 maxval(abs(result_local(:,1)/fffac(ii,jj) - zresult)) )
         enddo
      enddo
      if(dmax.gt.dmaxp) itmax=itest

      deallocate(xxzc,xxfac,xxoff,bcvals,pdata)
      deallocate(yyzc,yyfac,yyoff,fffac)

      call xplasma_remove_item(s,idtmp,ier)
      call xplasma_author_clear(s,'hyb3dlin',ier); call ckerr(s)

    end subroutine hyb3dlin

    subroutine hyb3dlzn(id1,id2,id3,zxtest,zresult,ideriv)
      integer, intent(in) :: id1  ! id for 1st dimension (profile or XX)
      integer, intent(in) :: id2  ! id for 2nd dimension (XX or YY or profile)
      integer, intent(in) :: id3  ! id for 3rd dimension (YY or profile)
      real*8, intent(in), dimension(:) :: zxtest  ! profile dim test vector
      real*8, intent(in), dimension(:) :: zresult ! profile dim values expected
      integer, intent(in) :: ideriv

      ! construct lin-zone-hermite or lin-zone-spline hybrid interpolant
      ! and compare evaluation to expected result.

      !----------------------
      integer :: itype1,itype2,itype3,isize_xx,isize_yy
      integer :: ii,jj,idprof,hspline(3),idg,irank,inx
      integer :: idtmp,idt2(2),idt2a(2)
      real*8 :: result_local(size(zresult),2),zxxwk(size(zresult))
      real*8 :: zyywk(size(zresult))
      real*8, dimension(:), allocatable :: xxzc,xxfac,xxoff
      real*8, dimension(:), allocatable :: yyzc,yyfac,yyoff
      real*8, dimension(:,:), allocatable :: bcvals,fffac
      real*8, dimension(:,:,:), allocatable :: pdata
      real*8, parameter :: xxincr = 2.5d0
      real*8 :: xxoffs,dmaxp,fjj,fii
      !----------------------

      itest = itest + 1  ! increment test counter

      isize_xx=size(xx)
      allocate(xxzc(isize_xx-1),xxfac(isize_xx),xxoff(isize_xx-1))

      isize_yy=size(yy)
      allocate(yyzc(isize_yy-1),yyfac(isize_yy-1),yyoff(isize_yy-1))

      allocate(bcvals(isize_xx,isize_yy-1),fffac(isize_xx-1,isize_yy-1))

      xxzc = (xx(1:isize_xx-1)+xx(2:isize_xx))/2
      yyzc = (yy(1:isize_yy-1)+yy(2:isize_yy))/2

      xxoffs = xx(isize_xx)/(isize_xx-1)/10.0d0
      xxfac(1)=1.0d0
      xxoff(1)=-xxoffs/2
      yyfac(1)=1.0d0
      yyoff(1)=-3*xxoffs/2

      do ii=2,isize_xx-1
         xxfac(ii) = xxfac(ii-1) + xxincr
         xxoff(ii) = xxoff(ii-1) + xxoffs
      enddo
      xxfac(isize_xx)=xxfac(isize_xx-1) + xxincr

      do ii=2,isize_yy-1
         yyfac(ii) = yyfac(ii-1) + xxincr
         yyoff(ii) = yyoff(ii-1) + xxoffs
      enddo

      do jj=1,isize_yy-1
         do ii=1,isize_xx-1
            fii = (xxzc(ii)+xxoff(ii)-xx(ii))/(xx(ii+1)-xx(ii))
            fffac(ii,jj) = &
                 yyfac(jj)*((1-fii)*xxfac(ii)+fii*xxfac(ii+1))
         enddo
      enddo

      call xplasma_get_item_info(s,id1,ier, itype=itype1); call ckerr(s)
      call xplasma_get_item_info(s,id2,ier, itype=itype2); call ckerr(s)
      call xplasma_get_item_info(s,id3,ier, itype=itype3); call ckerr(s)

      if(itype3.eq.xplasma_profType) then
         idprof=id3
      else if(itype2.eq.xplasma_profType) then
         idprof=id2
      else
         idprof=id1
      endif
      idt2(1)=idprof

      call xplasma_prof_info(s,idprof,ier, splineType=ispline, &
           rank=irank, gridId1=idg); call ckerr(s)
      if(irank.ne.1) then
         write(6,*) ' ?hyb3dlzn: input profile not 1d, id=',idprof
         call ps_bad_exit
      endif

      call xplasma_grid_info(s,idg,ier, size=inx)

      call xplasma_author_set(s,'hyb3dlzn',ier); call ckerr(s)

      bcvals(1,1)=z2pi
      if(itype1.eq.xplasma_profType) then
         hspline(1)=ispline
         hspline(2)=0
         hspline(3)=-1
         allocate(pdata(inx,isize_xx,isize_yy-1))
         call xplasma_getprof_1data(s,idprof,pdata(1:inx,1,1),ier)
         call ckerr(s)
         do jj=1,isize_yy-1
            do ii=1,isize_xx
               pdata(1:inx,ii,jj)=xxfac(ii)*yyfac(jj)*pdata(1:inx,1,1)
               bcvals(ii,jj)=xxfac(ii)*yyfac(jj)*bcvals(1,1)
            enddo
         enddo

         if(ispline.gt.0) then
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idg,idxxg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx1a=1,zbcx1a=bcvals,ibcx1b=1,zbcx1b=bcvals)
         else
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idg,idxxg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)
      endif

      if(itype2.eq.xplasma_profType) then
         hspline(1)=0
         hspline(2)=ispline
         hspline(3)=-1
         allocate(pdata(isize_xx,inx,isize_yy-1))
         call xplasma_getprof_1data(s,idprof,pdata(1,1:inx,1),ier)
         call ckerr(s)
         do jj=1,isize_yy-1
            do ii=1,isize_xx
               pdata(ii,1:inx,jj)=xxfac(ii)*yyfac(jj)*pdata(1,1:inx,1)
               bcvals(ii,jj)=xxfac(ii)*yyfac(jj)*bcvals(1,1)
            enddo
         enddo

         if(ispline.gt.0) then
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx2a=1,zbcx2a=bcvals,ibcx2b=1,zbcx2b=bcvals)
         else
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)
      endif

      if(itype3.eq.xplasma_profType) then
         hspline(1)=0
         hspline(2)=-1
         hspline(3)=ispline
         allocate(pdata(isize_xx,isize_yy-1,inx))
         call xplasma_getprof_1data(s,idprof,pdata(1,1,1:inx),ier)
         call ckerr(s)
         do jj=1,isize_yy-1
            do ii=1,isize_xx
               pdata(ii,jj,1:inx)=xxfac(ii)*yyfac(jj)*pdata(1,1,1:inx)
               bcvals(ii,jj)=xxfac(ii)*yyfac(jj)*bcvals(1,1)
            enddo
         enddo

         if(ispline.gt.0) then
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idyyg,idg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx3a=1,zbcx3a=bcvals,ibcx3b=1,zbcx3b=bcvals)
         else
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idyyg,idg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)
      endif

      idt2(2)=idtmp

      idt2a(1)=idt2(2)
      idt2a(2)=idt2(1)

      dmaxp=dmax
      do jj=1,isize_yy-1
         zyywk=yyzc(jj)+yyoff(jj)
         do ii=1,isize_xx-1
            zxxwk=xxzc(ii)+xxoff(ii)
            call xplasma_eval_prof(s,idtmp, &
                 idxxg,zxxwk, idyyg,zyywk, idg,zxtest, result_local(:,1),ier, &
                 ideriv3=ideriv); call ckerr(s)
            dmax = max(dmax, &
                 maxval(abs(result_local(:,1)/fffac(ii,jj) - zresult)) )
         
            call xplasma_eval_prof(s,idt2, &
                 idxxg,zxxwk, idyyg,zyywk, idg,zxtest, result_local,ier, &
                 ideriv3=ideriv); call ckerr(s)
            dmax = max(dmax, maxval(abs(result_local(:,1) - zresult)) )
            dmax = max(dmax, &
                 maxval(abs(result_local(:,2)/fffac(ii,jj) - zresult)) )

            call xplasma_eval_prof(s,idt2a, &
                 idg,zxtest, idxxg,zxxwk, idyyg,zyywk, result_local,ier, &
                 ideriv1=ideriv); call ckerr(s)
            dmax = max(dmax, maxval(abs(result_local(:,2) - zresult)) )
            dmax = max(dmax, &
                 maxval(abs(result_local(:,1)/fffac(ii,jj) - zresult)) )
         enddo
      enddo
      if(dmax.gt.dmaxp) itmax=itest

      deallocate(xxzc,xxfac,xxoff,bcvals,pdata)
      deallocate(yyzc,yyfac,yyoff,fffac)

      call xplasma_remove_item(s,idtmp,ier)
      call xplasma_author_clear(s,'hyb3dlzn',ier); call ckerr(s)

    end subroutine hyb3dlzn

    subroutine hyb3dzln(id1,id2,id3,zxtest,zresult,ideriv)
      integer, intent(in) :: id1  ! id for 1st dimension (profile or XX)
      integer, intent(in) :: id2  ! id for 2nd dimension (XX or YY or profile)
      integer, intent(in) :: id3  ! id for 3rd dimension (YY or profile)
      real*8, intent(in), dimension(:) :: zxtest  ! profile dim test vector
      real*8, intent(in), dimension(:) :: zresult ! profile dim values expected
      integer, intent(in) :: ideriv

      ! construct zone-lin-hermite or zone-lin-spline hybrid interpolant
      ! and compare evaluation to expected result.

      !----------------------
      integer :: itype1,itype2,itype3,isize_xx,isize_yy
      integer :: ii,jj,idprof,hspline(3),idg,irank,inx
      integer :: idtmp,idt2(2),idt2a(2)
      real*8 :: result_local(size(zresult),2),zxxwk(size(zresult))
      real*8 :: zyywk(size(zresult))
      real*8, dimension(:), allocatable :: xxzc,xxfac,xxoff
      real*8, dimension(:), allocatable :: yyzc,yyfac,yyoff
      real*8, dimension(:,:), allocatable :: bcvals,fffac
      real*8, dimension(:,:,:), allocatable :: pdata
      real*8, parameter :: xxincr = 2.5d0
      real*8 :: xxoffs,dmaxp,fjj,fii
      !----------------------

      itest = itest + 1  ! increment test counter

      isize_xx=size(xx)
      allocate(xxzc(isize_xx-1),xxfac(isize_xx-1),xxoff(isize_xx-1))

      isize_yy=size(yy)
      allocate(yyzc(isize_yy-1),yyfac(isize_yy),yyoff(isize_yy-1))

      allocate(bcvals(isize_xx-1,isize_yy),fffac(isize_xx-1,isize_yy-1))

      xxzc = (xx(1:isize_xx-1)+xx(2:isize_xx))/2
      yyzc = (yy(1:isize_yy-1)+yy(2:isize_yy))/2

      xxoffs = xx(isize_xx)/(isize_xx-1)/10.0d0
      xxfac(1)=1.0d0
      xxoff(1)=-xxoffs/2
      yyfac(1)=1.0d0
      yyoff(1)=-3*xxoffs/2

      do ii=2,isize_xx-1
         xxfac(ii) = xxfac(ii-1) + xxincr
         xxoff(ii) = xxoff(ii-1) + xxoffs
      enddo

      do ii=2,isize_yy-1
         yyfac(ii) = yyfac(ii-1) + xxincr
         yyoff(ii) = yyoff(ii-1) + xxoffs
      enddo
      yyfac(isize_yy)=yyfac(isize_yy-1) + xxincr

      do jj=1,isize_yy-1
         fjj = (yyzc(jj)+yyoff(jj)-yy(jj))/(yy(jj+1)-yy(jj))
         do ii=1,isize_xx-1
            fffac(ii,jj) = xxfac(ii)*((1-fjj)*yyfac(jj)+fjj*yyfac(jj+1))
         enddo
      enddo

      call xplasma_get_item_info(s,id1,ier, itype=itype1); call ckerr(s)
      call xplasma_get_item_info(s,id2,ier, itype=itype2); call ckerr(s)
      call xplasma_get_item_info(s,id3,ier, itype=itype3); call ckerr(s)

      if(itype3.eq.xplasma_profType) then
         idprof=id3
      else if(itype2.eq.xplasma_profType) then
         idprof=id2
      else
         idprof=id1
      endif
      idt2(1)=idprof

      call xplasma_prof_info(s,idprof,ier, splineType=ispline, &
           rank=irank, gridId1=idg); call ckerr(s)
      if(irank.ne.1) then
         write(6,*) ' ?hyb3dzln: input profile not 1d, id=',idprof
         call ps_bad_exit
      endif

      call xplasma_grid_info(s,idg,ier, size=inx)

      call xplasma_author_set(s,'hyb3dzln',ier); call ckerr(s)

      bcvals(1,1)=z2pi
      if(itype1.eq.xplasma_profType) then
         hspline(1)=ispline
         hspline(2)=-1
         hspline(3)=0
         allocate(pdata(inx,isize_xx-1,isize_yy))
         call xplasma_getprof_1data(s,idprof,pdata(1:inx,1,1),ier)
         call ckerr(s)
         do jj=1,isize_yy
            do ii=1,isize_xx-1
               pdata(1:inx,ii,jj)=xxfac(ii)*yyfac(jj)*pdata(1:inx,1,1)
               bcvals(ii,jj)=xxfac(ii)*yyfac(jj)*bcvals(1,1)
            enddo
         enddo

         if(ispline.gt.0) then
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idg,idxxg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx1a=1,zbcx1a=bcvals,ibcx1b=1,zbcx1b=bcvals)
         else
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idg,idxxg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)
      endif

      if(itype2.eq.xplasma_profType) then
         hspline(1)=-1
         hspline(2)=ispline
         hspline(3)=0
         allocate(pdata(isize_xx-1,inx,isize_yy))
         call xplasma_getprof_1data(s,idprof,pdata(1,1:inx,1),ier)
         call ckerr(s)
         do jj=1,isize_yy
            do ii=1,isize_xx-1
               pdata(ii,1:inx,jj)=xxfac(ii)*yyfac(jj)*pdata(1,1:inx,1)
               bcvals(ii,jj)=xxfac(ii)*yyfac(jj)*bcvals(1,1)
            enddo
         enddo

         if(ispline.gt.0) then
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx2a=1,zbcx2a=bcvals,ibcx2b=1,zbcx2b=bcvals)
         else
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idg,idyyg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)
      endif

      if(itype3.eq.xplasma_profType) then
         hspline(1)=-1
         hspline(2)=0
         hspline(3)=ispline
         allocate(pdata(isize_xx-1,isize_yy,inx))
         call xplasma_getprof_1data(s,idprof,pdata(1,1,1:inx),ier)
         call ckerr(s)
         do jj=1,isize_yy
            do ii=1,isize_xx-1
               pdata(ii,jj,1:inx)=xxfac(ii)*yyfac(jj)*pdata(1,1,1:inx)
               bcvals(ii,jj)=xxfac(ii)*yyfac(jj)*bcvals(1,1)
            enddo
         enddo

         if(ispline.gt.0) then
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idyyg,idg,pdata,idtmp,ier, &
                 hspline=hspline,ibcx3a=1,zbcx3a=bcvals,ibcx3b=1,zbcx3b=bcvals)
         else
            call xplasma_create_3dprof(s,'_TMP_HYB3', &
                 idxxg,idyyg,idg,pdata,idtmp,ier, &
                 hspline=hspline)
         endif
         call ckerr(s)
      endif

      idt2(2)=idtmp

      idt2a(1)=idt2(2)
      idt2a(2)=idt2(1)

      dmaxp=dmax
      do jj=1,isize_yy-1
         zyywk=yyzc(jj)+yyoff(jj)
         do ii=1,isize_xx-1
            zxxwk=xxzc(ii)+xxoff(ii)
            call xplasma_eval_prof(s,idtmp, &
                 idxxg,zxxwk, idyyg,zyywk, idg,zxtest, result_local(:,1),ier, &
                 ideriv3=ideriv); call ckerr(s)
            dmax = max(dmax, &
                 maxval(abs(result_local(:,1)/fffac(ii,jj) - zresult)) )
         
            call xplasma_eval_prof(s,idt2, &
                 idxxg,zxxwk, idyyg,zyywk, idg,zxtest, result_local,ier, &
                 ideriv3=ideriv); call ckerr(s)
            dmax = max(dmax, maxval(abs(result_local(:,1) - zresult)) )
            dmax = max(dmax, &
                 maxval(abs(result_local(:,2)/fffac(ii,jj) - zresult)) )

            call xplasma_eval_prof(s,idt2a, &
                 idg,zxtest, idxxg,zxxwk, idyyg,zyywk, result_local,ier, &
                 ideriv1=ideriv); call ckerr(s)
            dmax = max(dmax, maxval(abs(result_local(:,2) - zresult)) )
            dmax = max(dmax, &
                 maxval(abs(result_local(:,1)/fffac(ii,jj) - zresult)) )
         enddo
      enddo
      if(dmax.gt.dmaxp) itmax=itest

      deallocate(xxzc,xxfac,xxoff,bcvals,pdata)
      deallocate(yyzc,yyfac,yyoff,fffac)

      call xplasma_remove_item(s,idtmp,ier)
      call xplasma_author_clear(s,'hyb3dzln',ier); call ckerr(s)

    end subroutine hyb3dzln

    subroutine hyb3z2(idimz,idf,zxtest,zthtest,zresult,derivx,derivth)

      !  hybrid: bicubic or bilinear with one addl zonal dimension

      integer, intent(in) :: idimz  ! 1, 2, or 3  -- zonal dimension

      integer, intent(in) :: idf  ! id for 2d model profile

      real*8, intent(in), dimension(:) :: zxtest  ! profile dim test vector
      real*8, intent(in), dimension(:) :: zthtest ! profile dim test vector
      real*8, intent(in), dimension(:) :: zresult ! profile dim values expected

      integer, intent(in) :: derivx,derivth  ! derivative selection in x, th

      !----------------------
      integer :: isize_xx,ii,ispline,hspline(3),irank
      integer :: idtmp,idt2(2),idt2a(2)
      integer :: inrho,inchi
      real*8 :: result_local(size(zresult),2),zxxwk(size(zresult))
      real*8, dimension(:), allocatable :: xxzc,xxfac,xxoff
      real*8, dimension(:,:,:), allocatable :: pdata
      real*8 :: xxincr
      real*8 :: xxoffs,dmaxp
      !----------------------

      itest = itest + 1  ! increment test counter

      isize_xx=size(xx)
      allocate(xxzc(isize_xx-1),xxfac(isize_xx-1),xxoff(isize_xx-1))

      xxzc = (xx(1:isize_xx-1)+xx(2:isize_xx))/2

      xxincr = 2.5d0
      xxoffs = xx(isize_xx)/(isize_xx-1)/10.0d0
      xxfac(1)=1.0d0
      xxoff(1)=-xxoffs/2
      do ii=2,isize_xx-1
         xxfac(ii) = xxfac(ii-1) + xxincr
         xxoff(ii) = xxoff(ii-1) + xxoffs
         xxincr = xxincr - 0.5d0
      enddo

      idt2(1)=idf

      call xplasma_prof_info(s,idf,ier, splineType=ispline, &
           rank=irank); call ckerr(s)
      if(irank.ne.2) then
         write(6,*) ' ?hyb3z2: input profile not 1d, id=',idf
         call ps_bad_exit
      endif

      call xplasma_grid_info(s,id_rho,ier, size=inrho); call ckerr(s)
      call xplasma_grid_info(s,id_chi,ier, size=inchi); call ckerr(s)

      call xplasma_author_set(s,'hyb3z2',ier); call ckerr(s)

      if(idimz.eq.1) then
         hspline(1)=-1
         hspline(2)=ispline
         hspline(3)=ispline
         allocate(pdata(isize_xx-1,inrho,inchi))
         call xplasma_getprof_2data(s,idf,pdata(1,1:inrho,1:inchi),ier)
         call ckerr(s)

         do ii=2,isize_xx-1
            pdata(ii,1:inrho,1:inchi)=xxfac(ii)*pdata(1,1:inrho,1:inchi)
         enddo

         call xplasma_create_3dprof(s,'_TMP_HYB3A', &
              idxxg,id_rho,id_chi,pdata,idtmp,ier, &
              hspline=hspline)
         call ckerr(s)
      endif

      if(idimz.eq.2) then
         hspline(1)=ispline
         hspline(2)=-1
         hspline(3)=ispline
         allocate(pdata(inrho,isize_xx-1,inchi))
         call xplasma_getprof_2data(s,idf,pdata(1:inrho,1,1:inchi),ier)
         call ckerr(s)

         do ii=2,isize_xx-1
            pdata(1:inrho,ii,1:inchi)=xxfac(ii)*pdata(1:inrho,1,1:inchi)
         enddo

         call xplasma_create_3dprof(s,'_TMP_HYB3A', &
              id_rho,idxxg,id_chi,pdata,idtmp,ier, &
              hspline=hspline)
         call ckerr(s)
      endif

      if(idimz.eq.3) then
         hspline(1)=ispline
         hspline(2)=ispline
         hspline(3)=-1
         allocate(pdata(inrho,inchi,isize_xx-1))
         call xplasma_getprof_2data(s,idf,pdata(1:inrho,1:inchi,1),ier)
         call ckerr(s)

         do ii=2,isize_xx-1
            pdata(1:inrho,1:inchi,ii)=xxfac(ii)*pdata(1:inrho,1:inchi,1)
         enddo

         call xplasma_create_3dprof(s,'_TMP_HYB3A', &
              id_rho,id_chi,idxxg,pdata,idtmp,ier, &
              hspline=hspline)
         call ckerr(s)
      endif

      idt2(2)=idtmp

      idt2a(1)=idt2(2)
      idt2a(2)=idt2(1)

      dmaxp=dmax
      do ii=1,isize_xx-1
         zxxwk=xxzc(ii)+xxoff(ii)
         call xplasma_eval_prof(s,idtmp, &
              idxxg,zxxwk, id_rho,zxtest, id_chi,zthtest, &
              result_local(:,1),ier, &
              ideriv2=derivx, ideriv3=derivth); call ckerr(s)
         dmax = max(dmax, &
              maxval(abs(result_local(:,1)/xxfac(ii) - zresult)) )
         
         call xplasma_eval_prof(s,idt2, &
              idxxg,zxxwk, id_chi,zthtest, id_rho,zxtest, &
              result_local,ier, &
              ideriv2=derivth, ideriv3=derivx); call ckerr(s)
         dmax = max(dmax, maxval(abs(result_local(:,1) - zresult)) )
         dmax = max(dmax, &
              maxval(abs(result_local(:,2)/xxfac(ii) - zresult)) )

         call xplasma_eval_prof(s,idt2a, &
              id_rho,zxtest, id_chi,zthtest, idxxg,zxxwk, &
              result_local,ier, &
              ideriv1=derivx, ideriv2=derivth); call ckerr(s)
         dmax = max(dmax, maxval(abs(result_local(:,2) - zresult)) )
         dmax = max(dmax, &
              maxval(abs(result_local(:,1)/xxfac(ii) - zresult)) )
      enddo
      if(dmax.gt.dmaxp) itmax=itest

      deallocate(xxzc,xxfac,xxoff,pdata)

      call xplasma_remove_item(s,idtmp,ier)
      call xplasma_author_clear(s,'hyb3z2',ier); call ckerr(s)

    end subroutine hyb3z2

    subroutine hyb3lin2(idimz,idf,zxtest,zthtest,zresult,derivx,derivth)

      !  hybrid: bicubic with one addl piecewise linear dimension

      integer, intent(in) :: idimz  ! 1, 2, or 3  -- pc-lin dimension

      integer, intent(in) :: idf  ! id for 2d model profile

      real*8, intent(in), dimension(:) :: zxtest  ! profile dim test vector
      real*8, intent(in), dimension(:) :: zthtest ! profile dim test vector
      real*8, intent(in), dimension(:) :: zresult ! profile dim values expected

      integer, intent(in) :: derivx,derivth  ! derivative selection in x, th

      !----------------------
      integer :: isize_xx,ii,ispline,hspline(3),irank
      integer :: idtmp,idt2(2),idt2a(2)
      integer :: inrho,inchi
      real*8 :: result_local(size(zresult),2),zxxwk(size(zresult))
      real*8, dimension(:), allocatable :: xxzc,xxfac,xxoff,fffac
      real*8, dimension(:,:,:), allocatable :: pdata
      real*8 :: xxincr
      real*8 :: xxoffs,dmaxp
      !----------------------

      itest = itest + 1  ! increment test counter

      isize_xx=size(xx)
      allocate(xxzc(isize_xx-1),xxfac(isize_xx),xxoff(isize_xx-1))
      allocate(fffac(isize_xx-1))

      xxzc = (xx(1:isize_xx-1)+xx(2:isize_xx))/2

      xxincr = 2.5d0
      xxoffs = xx(isize_xx)/(isize_xx-1)/10.0d0
      xxfac(1)=1.0d0
      xxoff(1)=-xxoffs/2
      do ii=2,isize_xx-1
         xxfac(ii) = xxfac(ii-1) + xxincr
         xxoff(ii) = xxoff(ii-1) + xxoffs
         xxincr = xxincr - 0.5d0
      enddo
      xxfac(isize_xx) = xxfac(isize_xx-1) + xxincr

      do ii=1,isize_xx-1
         fffac(ii) = (xxzc(ii)+xxoff(ii)-xx(ii))/(xx(ii+1)-xx(ii))
         fffac(ii) = xxfac(ii) + fffac(ii)*(xxfac(ii+1)-xxfac(ii))
      enddo

      idt2(1)=idf

      call xplasma_prof_info(s,idf,ier, splineType=ispline, &
           rank=irank); call ckerr(s)
      if(irank.ne.2) then
         write(6,*) ' ?hyb3lin2: input profile not 1d, id=',idf
         call ps_bad_exit
      endif

      call xplasma_grid_info(s,id_rho,ier, size=inrho); call ckerr(s)
      call xplasma_grid_info(s,id_chi,ier, size=inchi); call ckerr(s)

      call xplasma_author_set(s,'hyb3lin2',ier); call ckerr(s)

      if(idimz.eq.1) then
         hspline(1)=0
         hspline(2)=ispline
         hspline(3)=ispline
         allocate(pdata(isize_xx,inrho,inchi))
         call xplasma_getprof_2data(s,idf,pdata(1,1:inrho,1:inchi),ier)
         call ckerr(s)

         do ii=2,isize_xx
            pdata(ii,1:inrho,1:inchi)=xxfac(ii)*pdata(1,1:inrho,1:inchi)
         enddo

         call xplasma_create_3dprof(s,'_TMP_HYB3A', &
              idxxg,id_rho,id_chi,pdata,idtmp,ier, &
              hspline=hspline)
         call ckerr(s)
      endif

      if(idimz.eq.2) then
         hspline(1)=ispline
         hspline(2)=0
         hspline(3)=ispline
         allocate(pdata(inrho,isize_xx,inchi))
         call xplasma_getprof_2data(s,idf,pdata(1:inrho,1,1:inchi),ier)
         call ckerr(s)

         do ii=2,isize_xx
            pdata(1:inrho,ii,1:inchi)=xxfac(ii)*pdata(1:inrho,1,1:inchi)
         enddo

         call xplasma_create_3dprof(s,'_TMP_HYB3A', &
              id_rho,idxxg,id_chi,pdata,idtmp,ier, &
              hspline=hspline)
         call ckerr(s)
      endif

      if(idimz.eq.3) then
         hspline(1)=ispline
         hspline(2)=ispline
         hspline(3)=0
         allocate(pdata(inrho,inchi,isize_xx))
         call xplasma_getprof_2data(s,idf,pdata(1:inrho,1:inchi,1),ier)
         call ckerr(s)

         do ii=2,isize_xx
            pdata(1:inrho,1:inchi,ii)=xxfac(ii)*pdata(1:inrho,1:inchi,1)
         enddo

         call xplasma_create_3dprof(s,'_TMP_HYB3A', &
              id_rho,id_chi,idxxg,pdata,idtmp,ier, &
              hspline=hspline)
         call ckerr(s)
      endif

      idt2(2)=idtmp

      idt2a(1)=idt2(2)
      idt2a(2)=idt2(1)

      dmaxp=dmax
      do ii=1,isize_xx-1
         zxxwk=xxzc(ii)+xxoff(ii)
         call xplasma_eval_prof(s,idtmp, &
              idxxg,zxxwk, id_rho,zxtest, id_chi,zthtest, &
              result_local(:,1),ier, &
              ideriv2=derivx, ideriv3=derivth); call ckerr(s)
         dmax = max(dmax, &
              maxval(abs(result_local(:,1)/fffac(ii) - zresult)) )
         
         call xplasma_eval_prof(s,idt2, &
              idxxg,zxxwk, id_chi,zthtest, id_rho,zxtest, &
              result_local,ier, &
              ideriv2=derivth, ideriv3=derivx); call ckerr(s)
         dmax = max(dmax, maxval(abs(result_local(:,1) - zresult)) )
         dmax = max(dmax, &
              maxval(abs(result_local(:,2)/fffac(ii) - zresult)) )

         call xplasma_eval_prof(s,idt2a, &
              id_rho,zxtest, id_chi,zthtest, idxxg,zxxwk, &
              result_local,ier, &
              ideriv1=derivx, ideriv2=derivth); call ckerr(s)
         dmax = max(dmax, maxval(abs(result_local(:,2) - zresult)) )
         dmax = max(dmax, &
              maxval(abs(result_local(:,1)/fffac(ii) - zresult)) )
      enddo
      if(dmax.gt.dmaxp) itmax=itest

      deallocate(xxzc,xxfac,xxoff,pdata,fffac)

      call xplasma_remove_item(s,idtmp,ier)
      call xplasma_author_clear(s,'hyb3lin2',ier); call ckerr(s)

    end subroutine hyb3lin2

end program xplasma2_test

subroutine test_stepfuns

  !  test step function profiles over some very simple grids

  use xplasma_obj_instance
  implicit NONE

  integer, parameter :: n1=11, n2=4, n3=3, nvec=30

  real*8 :: x1(n1),x2(n2),x3(n3)
  real*8 :: x1vec(nvec),x2vec(nvec),x3vec(nvec),xvec(nvec)
  real*8 :: ans1(nvec),ans2(nvec),ans3(nvec)

  real*8 :: fun1(n1-1),fun1ret(n1-1)
  real*8 :: fun2(n1-1,n2-1),fun2ret(n1-1,n2-1)
  real*8 :: fun3(n1-1,n2-1,n3-1),fun3ret(n1-1,n2-1,n3-1)

  integer :: i1,i2,i3,ivec,idiff,idifft
  integer :: id1g,id2g,id3g,id1c,id2c,id3c,id1f,id2f,id3f
  integer :: ii,ier

  !--------------------------------------------------
  call xplasma_author_set(s,'xplasma2_test',ier); call ckerr(s)

  do i1=1,n1
     x1(i1)=(i1-1)*1.0d0/(n1-1)
  enddo

  do i2=1,n2
     x2(i2)=(i2-1)*1.0d0/(n2-1)
  enddo

  do i3=1,n3
     x3(i3)=(i3-1)*1.0d0/(n3-1)
  enddo

  do ivec=1,nvec
     xvec(ivec) = (ivec-0.5d0)/nvec
  enddo
  call xplasma_create_coord(s,'x1coord',.FALSE.,id1c,ier,label='X1')
  call ckerr(s)

  call xplasma_create_coord(s,'x2coord',.FALSE.,id2c,ier,label='X2')
  call ckerr(s)

  call xplasma_create_coord(s,'x3coord',.FALSE.,id3c,ier,label='X3')
  call ckerr(s)

  call xplasma_create_grid(s,'x1grid',id1c,x1,id1g,ier,label='X1')
  call ckerr(s)

  call xplasma_create_grid(s,'x2grid',id2c,x2,id2g,ier,label='X2')
  call ckerr(s)

  call xplasma_create_grid(s,'x3grid',id3c,x3,id3g,ier,label='X3')
  call ckerr(s)

  ii = 0
  do i3=1,n3-1
     do i2=1,n2-1
        do i1=1,n1-1

           ii = ii + 1

           if((i2.eq.1).and.(i3.eq.1)) fun1(i1)=ii
           if(i3.eq.1) fun2(i1,i2)=ii
           fun3(i1,i2,i3)=ii

        enddo
     enddo
  enddo

  call xplasma_create_prof(s,'Fun1',id1g,fun1,id1f,ier,label='fun1')
  call ckerr(s)

  call xplasma_create_prof(s,'Fun2',id1g,id2g,fun2,id2f,ier,label='fun2')
  call ckerr(s)

  call xplasma_create_prof(s,'Fun3',id1g,id2g,id3g,fun3,id3f,ier,label='fun3')
  call ckerr(s)

  !  accurate retrieval test

  call xplasma_eval_prof(s,id1f,fun1ret,ier)
  call ckerr(s)

  call xplasma_eval_prof(s,id2f,fun2ret,ier)
  call ckerr(s)

  call xplasma_eval_prof(s,id3f,fun3ret,ier)
  call ckerr(s)

  idifft=0

  idiff=0
  do i1=1,n1-1
     if(fun1ret(i1).ne.fun1(i1)) then
        idiff=idiff+1
        write(6,*) ' fun1 diff ',i1,fun1(i1),fun1ret(i1)
     endif
  enddo
  if(idiff.gt.0) write(6,*) ' **fun1** compare: ',idiff,' differences!'
  idifft=idifft + idiff

  idiff=0
  do i2=1,n2-1
     do i1=1,n1-1
        if(fun2ret(i1,i2).ne.fun2(i1,i2)) then
           idiff=idiff+1
           write(6,*) ' fun2 diff ',i1,i2,fun2(i1,i2),fun2ret(i1,i2)
        endif
     enddo
  enddo
  if(idiff.gt.0) write(6,*) ' **fun2** compare: ',idiff,' differences!'
  idifft=idifft + idiff

  idiff=0
  do i3=1,n3-1
     do i2=1,n2-1
        do i1=1,n1-1
           if(fun3ret(i1,i2,i3).ne.fun3(i1,i2,i3)) then
              idiff=idiff+1
              write(6,*) ' fun3 diff ',i1,i2,i3,fun3(i1,i2,i3),fun3ret(i1,i2,i3)
           endif
        enddo
     enddo
  enddo
  if(idiff.gt.0) write(6,*) ' **fun3** compare: ',idiff,' differences!'
  idifft=idifft + idiff

  if(idifft.gt.0) then
     write(6,*) ' ?? unexpected differences in retrieved data'
     call ps_bad_exit
  endif

  !---------------------
  !  eval along x dimension

  x1vec=xvec
  x2vec=0.8d0
  x3vec=0.75d0

  call xplasma_eval_prof(s,id1f,x1vec,ans1,ier); call ckerr(s)

  call xplasma_eval_prof(s,id2f,id1g,x1vec,id2g,x2vec,ans2,ier); call ckerr(s)

  call xplasma_eval_prof(s,id3f,id3g,x3vec,id1g,x1vec,id2g,x2vec,ans3,ier)
  call ckerr(s)

  write(6,*) ' '
  write(6,'(a,1pe12.5,a,1pe12.5)') &
       ' variation along x1; x2=',x2vec(1),'; x3=',x3vec(1)
  write(6,*) ' '
  write(6,*) '  x1              ans1            ans2            ans3'
  do ivec=1,nvec
     write(6,'(4(1x,1pe15.8))') x1vec(ivec),ans1(ivec),ans2(ivec),ans3(ivec)
  enddo

  x1vec=0.55d0
  x2vec=xvec
  x3vec=1.00d0

  call xplasma_eval_prof(s,id1f,x1vec,ans1,ier); call ckerr(s)

  call xplasma_eval_prof(s,id2f,id1g,x1vec,id2g,x2vec,ans2,ier); call ckerr(s)

  call xplasma_eval_prof(s,id3f,id3g,x3vec,id1g,x1vec,id2g,x2vec,ans3,ier)
  call ckerr(s)

  write(6,*) ' '
  write(6,'(a,1pe12.5,a,1pe12.5)') &
       ' variation along x2; x1=',x1vec(1),'; x3=',x3vec(1)
  write(6,*) ' '
  write(6,*) '  x2              ans1            ans2            ans3'
  do ivec=1,nvec
     write(6,'(4(1x,1pe15.8))') x2vec(ivec),ans1(ivec),ans2(ivec),ans3(ivec)
  enddo

  x1vec=0.15d0
  x2vec=0.45d0
  x3vec=xvec

  call xplasma_eval_prof(s,id1f,x1vec,ans1,ier); call ckerr(s)

  call xplasma_eval_prof(s,id2f,id1g,x1vec,id2g,x2vec,ans2,ier); call ckerr(s)

  call xplasma_eval_prof(s,id3f,id3g,x3vec,id1g,x1vec,id2g,x2vec,ans3,ier)
  call ckerr(s)

  write(6,*) ' '
  write(6,'(a,1pe12.5,a,1pe12.5)') &
       ' variation along x3; x1=',x1vec(1),'; x2=',x2vec(1)
  write(6,*) ' '
  write(6,*) '  x3              ans1            ans2            ans3'
  do ivec=1,nvec
     write(6,'(4(1x,1pe15.8))') x3vec(ivec),ans1(ivec),ans2(ivec),ans3(ivec)
  enddo

  call xplasma_author_clear(s,'xplasma2_test',ier); call ckerr(s)

  contains

    subroutine ckerr(t)
      type (xplasma), pointer :: t
      if(ier.ne.0) then
         call xplasma_error(t,ier,6)
         if(ier.gt.0) call ps_bad_exit
      endif

    end subroutine ckerr

end subroutine test_stepfuns
