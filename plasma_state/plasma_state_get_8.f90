! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_get_8(ss,icdf,version_change,iout,iertot)


  !  read subset of variables and arrays from NetCDF file

  USE plasma_state_definition_mod
  USE netcdf
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to read
  INTEGER, INTENT(IN) :: icdf   ! NetCDF file ID
  LOGICAL, INTENT(IN) :: version_change  ! flag file/software version mismatch
  INTEGER, INTENT(IN) :: iout   ! message LUN
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !--------------

  INTEGER :: varid,dimid,status,ilen,jlen,idims(20),indim
  INTEGER :: ii,iflag_alloc2,iflag_loc2,isize
  CHARACTER*80 :: nc_errmsg

  !--------------


  !-----------------------------------------------
  ! NetCDF reads...

  status = nf90_inq_varid(icdf,"t1",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"t1")
  ELSE
     status = nf90_get_var(icdf,varid,ss%t1)
     IF(status.NE.NF90_NOERR) THEN
        ss%t1 = 0.0_rspec
        CALL ncerr_name(2,status,"t1")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%teflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"teflux_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"teflux_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%teflux_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%teflux_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"teflux_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Te_bdy",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"Te_bdy")
  ELSE
     status = nf90_get_var(icdf,varid,ss%Te_bdy)
     IF(status.NE.NF90_NOERR) THEN
        ss%Te_bdy = 0.0_rspec
        CALL ncerr_name(2,status,"Te_bdy")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%te_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"te_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"te_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%te_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%te_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"te_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tfinal",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"tfinal")
  ELSE
     status = nf90_get_var(icdf,varid,ss%tfinal)
     IF(status.NE.NF90_NOERR) THEN
        ss%tfinal = 0.0_rspec
        CALL ncerr_name(2,status,"tfinal")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tfrip2_log)) THEN
     status = nf90_inq_varid(icdf,"tfrip2_log",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tfrip2_log")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tfrip2_log)
        IF(status.NE.NF90_NOERR) THEN
           ss%tfrip2_log = 0.0_rspec
           CALL ncerr_name(2,status,"tfrip2_log")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tfrip_log)) THEN
     status = nf90_inq_varid(icdf,"tfrip_log",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tfrip_log")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tfrip_log)
        IF(status.NE.NF90_NOERR) THEN
           ss%tfrip_log = 0.0_rspec
           CALL ncerr_name(2,status,"tfrip_log")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"TF_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"TF_Data_Info")
  ELSE
     ilen = len(ss%TF_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%TF_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%TF_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%TF_Data_Info = " "
        CALL ncerr_name(2,status,"TF_Data_Info")
     ELSE
        CALL denull(ss%TF_Data_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%thegrilh)) THEN
     status = nf90_inq_varid(icdf,"thegrilh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"thegrilh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%thegrilh)
        IF(status.NE.NF90_NOERR) THEN
           ss%thegrilh = 0.0_rspec
           CALL ncerr_name(2,status,"thegrilh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%th_eq)) THEN
     status = nf90_inq_varid(icdf,"th_eq",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"th_eq")
     ELSE
        status = nf90_get_var(icdf,varid,ss%th_eq)
        IF(status.NE.NF90_NOERR) THEN
           ss%th_eq = 0.0_rspec
           CALL ncerr_name(2,status,"th_eq")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Ti)) THEN
     status = nf90_inq_varid(icdf,"Ti",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Ti")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Ti)
        IF(status.NE.NF90_NOERR) THEN
           ss%Ti = 0.0_rspec
           CALL ncerr_name(2,status,"Ti")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tiflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"tiflux_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tiflux_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tiflux_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%tiflux_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"tiflux_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tinit",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"tinit")
  ELSE
     status = nf90_get_var(icdf,varid,ss%tinit)
     IF(status.NE.NF90_NOERR) THEN
        ss%tinit = 0.0_rspec
        CALL ncerr_name(2,status,"tinit")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Ti_bdy",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"Ti_bdy")
  ELSE
     status = nf90_get_var(icdf,varid,ss%Ti_bdy)
     IF(status.NE.NF90_NOERR) THEN
        ss%Ti_bdy = 0.0_rspec
        CALL ncerr_name(2,status,"Ti_bdy")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ti_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"ti_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ti_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ti_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%ti_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"ti_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tmodel)) THEN
     status = nf90_inq_varid(icdf,"tmodel",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tmodel")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tmodel)
        IF(status.NE.NF90_NOERR) THEN
           ss%tmodel = 0.0_rspec
           CALL ncerr_name(2,status,"tmodel")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tmodel_bdy",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"tmodel_bdy")
  ELSE
     status = nf90_get_var(icdf,varid,ss%tmodel_bdy)
     IF(status.NE.NF90_NOERR) THEN
        ss%tmodel_bdy = 0.0_rspec
        CALL ncerr_name(2,status,"tmodel_bdy")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"tokamak_id",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"tokamak_id")
  ELSE
     ilen = len(ss%tokamak_id)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%tokamak_id = " "
     status = nf90_get_var(icdf,varid,ss%tokamak_id(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%tokamak_id = " "
        CALL ncerr_name(2,status,"tokamak_id")
     ELSE
        CALL denull(ss%tokamak_id)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Tornum_balcrit)) THEN
     status = nf90_inq_varid(icdf,"Tornum_balcrit",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Tornum_balcrit")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Tornum_balcrit)
        IF(status.NE.NF90_NOERR) THEN
           ss%Tornum_balcrit = 0
           CALL ncerr_name(2,status,"Tornum_balcrit")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Tor_mode_label)) THEN
     status = nf90_inq_varid(icdf,"Tor_mode_label",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Tor_mode_label")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Tor_mode_label)
        IF(status.NE.NF90_NOERR) THEN
           ss%Tor_mode_label = " "
           CALL ncerr_name(2,status,"Tor_mode_label")
        ELSE
           CALL denull_vec(ss%Tor_mode_label,ss%mmodes)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Tor_mode_No)) THEN
     status = nf90_inq_varid(icdf,"Tor_mode_No",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Tor_mode_No")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Tor_mode_No)
        IF(status.NE.NF90_NOERR) THEN
           ss%Tor_mode_No = 0
           CALL ncerr_name(2,status,"Tor_mode_No")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tq0_reco)) THEN
     status = nf90_inq_varid(icdf,"tq0_reco",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tq0_reco")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tq0_reco)
        IF(status.NE.NF90_NOERR) THEN
           ss%tq0_reco = 0.0_rspec
           CALL ncerr_name(2,status,"tq0_reco")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqb0_halo)) THEN
     status = nf90_inq_varid(icdf,"tqb0_halo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tqb0_halo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tqb0_halo)
        IF(status.NE.NF90_NOERR) THEN
           ss%tqb0_halo = 0.0_rspec
           CALL ncerr_name(2,status,"tqb0_halo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqbe)) THEN
     status = nf90_inq_varid(icdf,"tqbe",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tqbe")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tqbe)
        IF(status.NE.NF90_NOERR) THEN
           ss%tqbe = 0.0_rspec
           CALL ncerr_name(2,status,"tqbe")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqbi)) THEN
     status = nf90_inq_varid(icdf,"tqbi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tqbi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tqbi)
        IF(status.NE.NF90_NOERR) THEN
           ss%tqbi = 0.0_rspec
           CALL ncerr_name(2,status,"tqbi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqbjxb)) THEN
     status = nf90_inq_varid(icdf,"tqbjxb",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tqbjxb")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tqbjxb)
        IF(status.NE.NF90_NOERR) THEN
           ss%tqbjxb = 0.0_rspec
           CALL ncerr_name(2,status,"tqbjxb")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqbth)) THEN
     status = nf90_inq_varid(icdf,"tqbth",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tqbth")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tqbth)
        IF(status.NE.NF90_NOERR) THEN
           ss%tqbth = 0.0_rspec
           CALL ncerr_name(2,status,"tqbth")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqcx_halo)) THEN
     status = nf90_inq_varid(icdf,"tqcx_halo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tqcx_halo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tqcx_halo)
        IF(status.NE.NF90_NOERR) THEN
           ss%tqcx_halo = 0.0_rspec
           CALL ncerr_name(2,status,"tqcx_halo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqcx_reco)) THEN
     status = nf90_inq_varid(icdf,"tqcx_reco",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tqcx_reco")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tqcx_reco)
        IF(status.NE.NF90_NOERR) THEN
           ss%tqcx_reco = 0.0_rspec
           CALL ncerr_name(2,status,"tqcx_reco")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqioniz)) THEN
     status = nf90_inq_varid(icdf,"tqioniz",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tqioniz")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tqioniz)
        IF(status.NE.NF90_NOERR) THEN
           ss%tqioniz = 0.0_rspec
           CALL ncerr_name(2,status,"tqioniz")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqqcx)) THEN
     status = nf90_inq_varid(icdf,"tqqcx",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tqqcx")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tqqcx)
        IF(status.NE.NF90_NOERR) THEN
           ss%tqqcx = 0.0_rspec
           CALL ncerr_name(2,status,"tqqcx")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqsc_halo)) THEN
     status = nf90_inq_varid(icdf,"tqsc_halo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tqsc_halo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tqsc_halo)
        IF(status.NE.NF90_NOERR) THEN
           ss%tqsc_halo = 0.0_rspec
           CALL ncerr_name(2,status,"tqsc_halo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tqsc_reco)) THEN
     status = nf90_inq_varid(icdf,"tqsc_reco",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tqsc_reco")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tqsc_reco)
        IF(status.NE.NF90_NOERR) THEN
           ss%tqsc_reco = 0.0_rspec
           CALL ncerr_name(2,status,"tqsc_reco")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tq_anom)) THEN
     status = nf90_inq_varid(icdf,"tq_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tq_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tq_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%tq_anom = 0.0_rspec
           CALL ncerr_name(2,status,"tq_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tq_trans)) THEN
     status = nf90_inq_varid(icdf,"tq_trans",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tq_trans")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tq_trans)
        IF(status.NE.NF90_NOERR) THEN
           ss%tq_trans = 0.0_rspec
           CALL ncerr_name(2,status,"tq_trans")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tq_transcat)) THEN
     status = nf90_inq_varid(icdf,"tq_transcat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tq_transcat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tq_transcat)
        IF(status.NE.NF90_NOERR) THEN
           ss%tq_transcat = 0.0_rspec
           CALL ncerr_name(2,status,"tq_transcat")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%trace_flag)) THEN
     status = nf90_inq_varid(icdf,"trace_flag",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"trace_flag")
     ELSE
        status = nf90_get_var(icdf,varid,ss%trace_flag)
        IF(status.NE.NF90_NOERR) THEN
           ss%trace_flag = 0
           CALL ncerr_name(2,status,"trace_flag")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%trcat)) THEN
     status = nf90_inq_varid(icdf,"trcat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"trcat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%trcat)
        IF(status.NE.NF90_NOERR) THEN
           ss%trcat = " "
           CALL ncerr_name(2,status,"trcat")
        ELSE
           CALL denull_vec(ss%trcat,ss%ntrcat)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%triang)) THEN
     status = nf90_inq_varid(icdf,"triang",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"triang")
     ELSE
        status = nf90_get_var(icdf,varid,ss%triang)
        IF(status.NE.NF90_NOERR) THEN
           ss%triang = 0.0_rspec
           CALL ncerr_name(2,status,"triang")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%triangL)) THEN
     status = nf90_inq_varid(icdf,"triangL",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"triangL")
     ELSE
        status = nf90_get_var(icdf,varid,ss%triangL)
        IF(status.NE.NF90_NOERR) THEN
           ss%triangL = 0.0_rspec
           CALL ncerr_name(2,status,"triangL")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%triangU)) THEN
     status = nf90_inq_varid(icdf,"triangU",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"triangU")
     ELSE
        status = nf90_get_var(icdf,varid,ss%triangU)
        IF(status.NE.NF90_NOERR) THEN
           ss%triangU = 0.0_rspec
           CALL ncerr_name(2,status,"triangU")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%triang_miller_L)) THEN
     status = nf90_inq_varid(icdf,"triang_miller_L",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"triang_miller_L")
     ELSE
        status = nf90_get_var(icdf,varid,ss%triang_miller_L)
        IF(status.NE.NF90_NOERR) THEN
           ss%triang_miller_L = 0.0_rspec
           CALL ncerr_name(2,status,"triang_miller_L")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%triang_miller_U)) THEN
     status = nf90_inq_varid(icdf,"triang_miller_U",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"triang_miller_U")
     ELSE
        status = nf90_get_var(icdf,varid,ss%triang_miller_U)
        IF(status.NE.NF90_NOERR) THEN
           ss%triang_miller_U = 0.0_rspec
           CALL ncerr_name(2,status,"triang_miller_U")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%trmodels)) THEN
     status = nf90_inq_varid(icdf,"trmodels",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"trmodels")
     ELSE
        status = nf90_get_var(icdf,varid,ss%trmodels)
        IF(status.NE.NF90_NOERR) THEN
           ss%trmodels = " "
           CALL ncerr_name(2,status,"trmodels")
        ELSE
           CALL denull_vec(ss%trmodels,ss%ntrcat)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Ts)) THEN
     status = nf90_inq_varid(icdf,"Ts",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Ts")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Ts)
        IF(status.NE.NF90_NOERR) THEN
           ss%Ts = 0.0_rspec
           CALL ncerr_name(2,status,"Ts")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"Ts_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"Ts_Data_Info")
  ELSE
     ilen = len(ss%Ts_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%Ts_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%Ts_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%Ts_Data_Info = " "
        CALL ncerr_name(2,status,"Ts_Data_Info")
     ELSE
        CALL denull(ss%Ts_Data_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Ts_is_input)) THEN
     status = nf90_inq_varid(icdf,"Ts_is_input",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Ts_is_input")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Ts_is_input)
        IF(status.NE.NF90_NOERR) THEN
           ss%Ts_is_input = 0
           CALL ncerr_name(2,status,"Ts_is_input")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tzflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"tzflux_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tzflux_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tzflux_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%tzflux_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"tzflux_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%tz_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"tz_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"tz_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%tz_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%tz_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"tz_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%T_einj_standard)) THEN
     status = nf90_inq_varid(icdf,"T_einj_standard",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"T_einj_standard")
     ELSE
        status = nf90_get_var(icdf,varid,ss%T_einj_standard)
        IF(status.NE.NF90_NOERR) THEN
           ss%T_einj_standard = 0.0_rspec
           CALL ncerr_name(2,status,"T_einj_standard")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%T_ffull_standard)) THEN
     status = nf90_inq_varid(icdf,"T_ffull_standard",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"T_ffull_standard")
     ELSE
        status = nf90_get_var(icdf,varid,ss%T_ffull_standard)
        IF(status.NE.NF90_NOERR) THEN
           ss%T_ffull_standard = 0.0_rspec
           CALL ncerr_name(2,status,"T_ffull_standard")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%T_fhalf_standard)) THEN
     status = nf90_inq_varid(icdf,"T_fhalf_standard",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"T_fhalf_standard")
     ELSE
        status = nf90_get_var(icdf,varid,ss%T_fhalf_standard)
        IF(status.NE.NF90_NOERR) THEN
           ss%T_fhalf_standard = 0.0_rspec
           CALL ncerr_name(2,status,"T_fhalf_standard")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%upwind_pfrac_ns)) THEN
     status = nf90_inq_varid(icdf,"upwind_pfrac_ns",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"upwind_pfrac_ns")
     ELSE
        status = nf90_get_var(icdf,varid,ss%upwind_pfrac_ns)
        IF(status.NE.NF90_NOERR) THEN
           ss%upwind_pfrac_ns = 0.0_rspec
           CALL ncerr_name(2,status,"upwind_pfrac_ns")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%upwind_pfrac_omega)) THEN
     status = nf90_inq_varid(icdf,"upwind_pfrac_omega",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"upwind_pfrac_omega")
     ELSE
        status = nf90_get_var(icdf,varid,ss%upwind_pfrac_omega)
        IF(status.NE.NF90_NOERR) THEN
           ss%upwind_pfrac_omega = 0.0_rspec
           CALL ncerr_name(2,status,"upwind_pfrac_omega")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%upwind_pfrac_Te)) THEN
     status = nf90_inq_varid(icdf,"upwind_pfrac_Te",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"upwind_pfrac_Te")
     ELSE
        status = nf90_get_var(icdf,varid,ss%upwind_pfrac_Te)
        IF(status.NE.NF90_NOERR) THEN
           ss%upwind_pfrac_Te = 0.0_rspec
           CALL ncerr_name(2,status,"upwind_pfrac_Te")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%upwind_pfrac_Ti)) THEN
     status = nf90_inq_varid(icdf,"upwind_pfrac_Ti",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"upwind_pfrac_Ti")
     ELSE
        status = nf90_get_var(icdf,varid,ss%upwind_pfrac_Ti)
        IF(status.NE.NF90_NOERR) THEN
           ss%upwind_pfrac_Ti = 0.0_rspec
           CALL ncerr_name(2,status,"upwind_pfrac_Ti")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vee_trans)) THEN
     status = nf90_inq_varid(icdf,"vee_trans",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vee_trans")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vee_trans)
        IF(status.NE.NF90_NOERR) THEN
           ss%vee_trans = 0.0_rspec
           CALL ncerr_name(2,status,"vee_trans")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%vee_transcat)) THEN
     status = nf90_inq_varid(icdf,"vee_transcat",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"vee_transcat")
     ELSE
        status = nf90_get_var(icdf,varid,ss%vee_transcat)
        IF(status.NE.NF90_NOERR) THEN
           ss%vee_transcat = 0.0_rspec
           CALL ncerr_name(2,status,"vee_transcat")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velb_fusi)) THEN
     status = nf90_inq_varid(icdf,"velb_fusi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velb_fusi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velb_fusi)
        IF(status.NE.NF90_NOERR) THEN
           ss%velb_fusi = 0.0_rspec
           CALL ncerr_name(2,status,"velb_fusi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velb_nbi)) THEN
     status = nf90_inq_varid(icdf,"velb_nbi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velb_nbi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velb_nbi)
        IF(status.NE.NF90_NOERR) THEN
           ss%velb_nbi = 0.0_rspec
           CALL ncerr_name(2,status,"velb_nbi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velb_rfmi)) THEN
     status = nf90_inq_varid(icdf,"velb_rfmi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velb_rfmi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velb_rfmi)
        IF(status.NE.NF90_NOERR) THEN
           ss%velb_rfmi = 0.0_rspec
           CALL ncerr_name(2,status,"velb_rfmi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velne_nclass)) THEN
     status = nf90_inq_varid(icdf,"velne_nclass",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velne_nclass")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velne_nclass)
        IF(status.NE.NF90_NOERR) THEN
           ss%velne_nclass = 0.0_rspec
           CALL ncerr_name(2,status,"velne_nclass")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velnh_nclass)) THEN
     status = nf90_inq_varid(icdf,"velnh_nclass",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velnh_nclass")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velnh_nclass)
        IF(status.NE.NF90_NOERR) THEN
           ss%velnh_nclass = 0.0_rspec
           CALL ncerr_name(2,status,"velnh_nclass")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velni_drbm)) THEN
     status = nf90_inq_varid(icdf,"velni_drbm",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velni_drbm")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velni_drbm)
        IF(status.NE.NF90_NOERR) THEN
           ss%velni_drbm = 0.0_rspec
           CALL ncerr_name(2,status,"velni_drbm")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_anom)) THEN
     status = nf90_inq_varid(icdf,"velns_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velns_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velns_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%velns_anom = 0.0_rspec
           CALL ncerr_name(2,status,"velns_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_glf)) THEN
     status = nf90_inq_varid(icdf,"velns_glf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velns_glf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velns_glf)
        IF(status.NE.NF90_NOERR) THEN
           ss%velns_glf = 0.0_rspec
           CALL ncerr_name(2,status,"velns_glf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"velns_gtcneo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velns_gtcneo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velns_gtcneo)
        IF(status.NE.NF90_NOERR) THEN
           ss%velns_gtcneo = 0.0_rspec
           CALL ncerr_name(2,status,"velns_gtcneo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_mmm71)) THEN
     status = nf90_inq_varid(icdf,"velns_mmm71",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velns_mmm71")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velns_mmm71)
        IF(status.NE.NF90_NOERR) THEN
           ss%velns_mmm71 = 0.0_rspec
           CALL ncerr_name(2,status,"velns_mmm71")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_nclass)) THEN
     status = nf90_inq_varid(icdf,"velns_nclass",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velns_nclass")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velns_nclass)
        IF(status.NE.NF90_NOERR) THEN
           ss%velns_nclass = 0.0_rspec
           CALL ncerr_name(2,status,"velns_nclass")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_neo)) THEN
     status = nf90_inq_varid(icdf,"velns_neo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velns_neo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velns_neo)
        IF(status.NE.NF90_NOERR) THEN
           ss%velns_neo = 0.0_rspec
           CALL ncerr_name(2,status,"velns_neo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velns_tglf)) THEN
     status = nf90_inq_varid(icdf,"velns_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velns_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velns_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%velns_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"velns_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velnx_nclass)) THEN
     status = nf90_inq_varid(icdf,"velnx_nclass",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velnx_nclass")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velnx_nclass)
        IF(status.NE.NF90_NOERR) THEN
           ss%velnx_nclass = 0.0_rspec
           CALL ncerr_name(2,status,"velnx_nclass")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velpphi_anom)) THEN
     status = nf90_inq_varid(icdf,"velpphi_anom",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velpphi_anom")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velpphi_anom)
        IF(status.NE.NF90_NOERR) THEN
           ss%velpphi_anom = 0.0_rspec
           CALL ncerr_name(2,status,"velpphi_anom")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velpphi_glf)) THEN
     status = nf90_inq_varid(icdf,"velpphi_glf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velpphi_glf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velpphi_glf)
        IF(status.NE.NF90_NOERR) THEN
           ss%velpphi_glf = 0.0_rspec
           CALL ncerr_name(2,status,"velpphi_glf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velpphi_gtcneo)) THEN
     status = nf90_inq_varid(icdf,"velpphi_gtcneo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velpphi_gtcneo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velpphi_gtcneo)
        IF(status.NE.NF90_NOERR) THEN
           ss%velpphi_gtcneo = 0.0_rspec
           CALL ncerr_name(2,status,"velpphi_gtcneo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velpphi_mmm71)) THEN
     status = nf90_inq_varid(icdf,"velpphi_mmm71",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velpphi_mmm71")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velpphi_mmm71)
        IF(status.NE.NF90_NOERR) THEN
           ss%velpphi_mmm71 = 0.0_rspec
           CALL ncerr_name(2,status,"velpphi_mmm71")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%velpphi_neo)) THEN
     status = nf90_inq_varid(icdf,"velpphi_neo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"velpphi_neo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%velpphi_neo)
        IF(status.NE.NF90_NOERR) THEN
           ss%velpphi_neo = 0.0_rspec
           CALL ncerr_name(2,status,"velpphi_neo")
        ENDIF
     ENDIF
  ENDIF

  CONTAINS
     SUBROUTINE denull_vec(vstr,isize)
        INTEGER, intent(in) :: isize
        CHARACTER*(*) :: vstr(isize)
        INTEGER :: i

        do i=1,isize
           call denull(vstr(i))
        enddo
     END SUBROUTINE denull_vec

     SUBROUTINE denull(str)
        ! convert null characters to blank
        CHARACTER*(*) :: str
        INTEGER :: i

        do i=1,len(str)
           if(ichar(str(i:i)).eq.0) str(i:i)=' '
        enddo
     END SUBROUTINE denull

     SUBROUTINE ncerr_name(itype,status,name)
        INTEGER, INTENT(in) :: itype
        INTEGER, INTENT(in) :: status
        CHARACTER*(*), INTENT(in) :: name

        if(itype.eq.1) then
           write(iout,*) ' %plasma_state_get: warning: variable name = "'// &
                  trim(name)//'":'
        else
           write(iout,*) ' ?plasma_state_get: error: variable name = "'// &
                  trim(name)//'":'
           if(version_change) then
              write(iout,*) '   => version change RECOVERY: variable reset to initial value.'
           else
              iertot = iertot + 1
           endif
        endif
        call ncerr(status)
     END SUBROUTINE ncerr_name

     SUBROUTINE ncerr(status)
        INTEGER, INTENT(in) :: status

        nc_errmsg = NF90_strerror(status)
        write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
     END SUBROUTINE ncerr

END SUBROUTINE plasma_state_get_8
