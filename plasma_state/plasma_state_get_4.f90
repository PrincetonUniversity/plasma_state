! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 2.056
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_get_4(ss,icdf,version_change,iout,iertot)


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

  IF(ALLOCATED(ss%nbeami)) THEN
     status = nf90_inq_varid(icdf,"nbeami",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nbeami")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nbeami)
        IF(status.NE.NF90_NOERR) THEN
           ss%nbeami = 0.0_rspec
           CALL ncerr_name(2,status,"nbeami")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nbeami_bdy)) THEN
     status = nf90_inq_varid(icdf,"nbeami_bdy",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nbeami_bdy")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nbeami_bdy)
        IF(status.NE.NF90_NOERR) THEN
           ss%nbeami_bdy = 0.0_rspec
           CALL ncerr_name(2,status,"nbeami_bdy")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nbion)) THEN
     status = nf90_inq_varid(icdf,"nbion",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nbion")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nbion)
        IF(status.NE.NF90_NOERR) THEN
           ss%nbion = " "
           CALL ncerr_name(2,status,"nbion")
        ELSE
           CALL denull_vec(ss%nbion,ss%nbeam)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nbion_trace)) THEN
     status = nf90_inq_varid(icdf,"nbion_trace",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nbion_trace")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nbion_trace)
        IF(status.NE.NF90_NOERR) THEN
           ss%nbion_trace = " "
           CALL ncerr_name(2,status,"nbion_trace")
        ELSE
           CALL denull_vec(ss%nbion_trace,ss%nbeam)
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"NBI_Code_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"NBI_Code_Info")
  ELSE
     ilen = len(ss%NBI_Code_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%NBI_Code_Info = " "
     status = nf90_get_var(icdf,varid,ss%NBI_Code_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%NBI_Code_Info = " "
        CALL ncerr_name(2,status,"NBI_Code_Info")
     ELSE
        CALL denull(ss%NBI_Code_Info)
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"NBI_Data_Info",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"NBI_Data_Info")
  ELSE
     ilen = len(ss%NBI_Data_Info)
     status = nf90_inquire_variable(icdf,varid,ndims=indim,dimids=idims)
     IF((status.EQ.NF90_NOERR).AND.(indim.eq.1)) THEN
        status = nf90_inquire_dimension(icdf,idims(1),len=jlen)
        IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)
     ENDIF
     ss%NBI_Data_Info = " "
     status = nf90_get_var(icdf,varid,ss%NBI_Data_Info(1:ilen))
     IF(status.NE.NF90_NOERR) THEN
        ss%NBI_Data_Info = " "
        CALL ncerr_name(2,status,"NBI_Data_Info")
     ELSE
        CALL denull(ss%NBI_Data_Info)
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nbi_src_name)) THEN
     status = nf90_inq_varid(icdf,"nbi_src_name",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nbi_src_name")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nbi_src_name)
        IF(status.NE.NF90_NOERR) THEN
           ss%nbi_src_name = " "
           CALL ncerr_name(2,status,"nbi_src_name")
        ELSE
           CALL denull_vec(ss%nbi_src_name,ss%nbeam)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nbshape)) THEN
     status = nf90_inq_varid(icdf,"nbshape",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nbshape")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nbshape)
        IF(status.NE.NF90_NOERR) THEN
           ss%nbshape = " "
           CALL ncerr_name(2,status,"nbshape")
        ELSE
           CALL denull_vec(ss%nbshape,ss%nbeam)
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nebar",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"nebar")
  ELSE
     status = nf90_get_var(icdf,varid,ss%nebar)
     IF(status.NE.NF90_NOERR) THEN
        ss%nebar = 0.0_rspec
        CALL ncerr_name(2,status,"nebar")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%neflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"neflux_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"neflux_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%neflux_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%neflux_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"neflux_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Newcomb_code)) THEN
     status = nf90_inq_varid(icdf,"Newcomb_code",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Newcomb_code")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Newcomb_code)
        IF(status.NE.NF90_NOERR) THEN
           ss%Newcomb_code = " "
           CALL ncerr_name(2,status,"Newcomb_code")
        ELSE
           CALL denull_vec(ss%Newcomb_code,ss%nnewcomb)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Newcomb_mapper)) THEN
     status = nf90_inq_varid(icdf,"Newcomb_mapper",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Newcomb_mapper")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Newcomb_mapper)
        IF(status.NE.NF90_NOERR) THEN
           ss%Newcomb_mapper = " "
           CALL ncerr_name(2,status,"Newcomb_mapper")
        ELSE
           CALL denull_vec(ss%Newcomb_mapper,ss%nnewcomb)
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Newcomb_stable)) THEN
     status = nf90_inq_varid(icdf,"Newcomb_stable",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Newcomb_stable")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Newcomb_stable)
        IF(status.NE.NF90_NOERR) THEN
           ss%Newcomb_stable = 0
           CALL ncerr_name(2,status,"Newcomb_stable")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Newcomb_status)) THEN
     status = nf90_inq_varid(icdf,"Newcomb_status",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"Newcomb_status")
     ELSE
        status = nf90_get_var(icdf,varid,ss%Newcomb_status)
        IF(status.NE.NF90_NOERR) THEN
           ss%Newcomb_status = 0
           CALL ncerr_name(2,status,"Newcomb_status")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ne_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"ne_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ne_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ne_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%ne_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"ne_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nfusi)) THEN
     status = nf90_inq_varid(icdf,"nfusi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nfusi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nfusi)
        IF(status.NE.NF90_NOERR) THEN
           ss%nfusi = 0.0_rspec
           CALL ncerr_name(2,status,"nfusi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nfusi_bdy)) THEN
     status = nf90_inq_varid(icdf,"nfusi_bdy",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nfusi_bdy")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nfusi_bdy)
        IF(status.NE.NF90_NOERR) THEN
           ss%nfusi_bdy = 0.0_rspec
           CALL ncerr_name(2,status,"nfusi_bdy")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ngradb2_av)) THEN
     status = nf90_inq_varid(icdf,"ngradb2_av",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ngradb2_av")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ngradb2_av)
        IF(status.NE.NF90_NOERR) THEN
           ss%ngradb2_av = 0.0_rspec
           CALL ncerr_name(2,status,"ngradb2_av")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ni)) THEN
     status = nf90_inq_varid(icdf,"ni",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ni")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ni)
        IF(status.NE.NF90_NOERR) THEN
           ss%ni = 0.0_rspec
           CALL ncerr_name(2,status,"ni")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%niflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"niflux_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"niflux_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%niflux_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%niflux_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"niflux_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ni_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"ni_ky_tglf",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ni_ky_tglf")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ni_ky_tglf)
        IF(status.NE.NF90_NOERR) THEN
           ss%ni_ky_tglf = 0.0_rspec
           CALL ncerr_name(2,status,"ni_ky_tglf")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nmdifb",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"nmdifb")
  ELSE
     status = nf90_get_var(icdf,varid,ss%nmdifb)
     IF(status.NE.NF90_NOERR) THEN
        ss%nmdifb = 0
        CALL ncerr_name(2,status,"nmdifb")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nmini)) THEN
     status = nf90_inq_varid(icdf,"nmini",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nmini")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nmini)
        IF(status.NE.NF90_NOERR) THEN
           ss%nmini = 0.0_rspec
           CALL ncerr_name(2,status,"nmini")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nmini_bdy)) THEN
     status = nf90_inq_varid(icdf,"nmini_bdy",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nmini_bdy")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nmini_bdy)
        IF(status.NE.NF90_NOERR) THEN
           ss%nmini_bdy = 0.0_rspec
           CALL ncerr_name(2,status,"nmini_bdy")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nmodel)) THEN
     status = nf90_inq_varid(icdf,"nmodel",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nmodel")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nmodel)
        IF(status.NE.NF90_NOERR) THEN
           ss%nmodel = 0.0_rspec
           CALL ncerr_name(2,status,"nmodel")
        ENDIF
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nmodel_bdy",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"nmodel_bdy")
  ELSE
     status = nf90_get_var(icdf,varid,ss%nmodel_bdy)
     IF(status.NE.NF90_NOERR) THEN
        ss%nmodel_bdy = 0.0_rspec
        CALL ncerr_name(2,status,"nmodel_bdy")
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nmom",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr_name(1,status,"nmom")
  ELSE
     status = nf90_get_var(icdf,varid,ss%nmom)
     IF(status.NE.NF90_NOERR) THEN
        ss%nmom = 16
        CALL ncerr_name(2,status,"nmom")
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nnkparlh)) THEN
     status = nf90_inq_varid(icdf,"nnkparlh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nnkparlh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nnkparlh)
        IF(status.NE.NF90_NOERR) THEN
           ss%nnkparlh = 0
           CALL ncerr_name(2,status,"nnkparlh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nnkpollh)) THEN
     status = nf90_inq_varid(icdf,"nnkpollh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nnkpollh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nnkpollh)
        IF(status.NE.NF90_NOERR) THEN
           ss%nnkpollh = 0
           CALL ncerr_name(2,status,"nnkpollh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nnktorlh)) THEN
     status = nf90_inq_varid(icdf,"nnktorlh",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nnktorlh")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nnktorlh)
        IF(status.NE.NF90_NOERR) THEN
           ss%nnktorlh = 0
           CALL ncerr_name(2,status,"nnktorlh")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nphi)) THEN
     status = nf90_inq_varid(icdf,"nphi",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nphi")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nphi)
        IF(status.NE.NF90_NOERR) THEN
           ss%nphi = 0
           CALL ncerr_name(2,status,"nphi")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nrw)) THEN
     status = nf90_inq_varid(icdf,"nrw",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nrw")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nrw)
        IF(status.NE.NF90_NOERR) THEN
           ss%nrw = 0.0_rspec
           CALL ncerr_name(2,status,"nrw")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nrz_antgeo)) THEN
     status = nf90_inq_varid(icdf,"nrz_antgeo",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nrz_antgeo")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nrz_antgeo)
        IF(status.NE.NF90_NOERR) THEN
           ss%nrz_antgeo = 0
           CALL ncerr_name(2,status,"nrz_antgeo")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ns)) THEN
     status = nf90_inq_varid(icdf,"ns",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"ns")
     ELSE
        status = nf90_get_var(icdf,varid,ss%ns)
        IF(status.NE.NF90_NOERR) THEN
           ss%ns = 0.0_rspec
           CALL ncerr_name(2,status,"ns")
        ENDIF
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nspectrumlh_src)) THEN
     status = nf90_inq_varid(icdf,"nspectrumlh_src",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr_name(1,status,"nspectrumlh_src")
     ELSE
        status = nf90_get_var(icdf,varid,ss%nspectrumlh_src)
        IF(status.NE.NF90_NOERR) THEN
           ss%nspectrumlh_src = 0
           CALL ncerr_name(2,status,"nspectrumlh_src")
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

END SUBROUTINE plasma_state_get_4
