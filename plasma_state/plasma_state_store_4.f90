! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: 3.000
! *** do not edit *** (use code generator instead)


SUBROUTINE plasma_state_store_def_4(ss,icdf,iout,iertot)


  !  define subset of variables and arrays for NetCDF file

  USE plasma_state_definition_mod
  USE netcdf
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to read
  INTEGER, INTENT(IN) :: icdf   ! NetCDF file ID
  INTEGER, INTENT(IN) :: iout   ! message LUN
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !--------------

  INTEGER :: varid,dimid,status,ival,idims(20),indim
  CHARACTER*80 :: nc_errmsg

  !--------------
  !  define variables...


  IF(ALLOCATED(ss%nbeami)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_nbi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("nbeami","R",indim,idims, &
            "beam species density", &
            "m^-3","NBI","STATE_PROFILES", &
            "R|units=m^-3|alias=n|step  nbeami(~nrho_nbi,nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%nbeami_bdy)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_beam",idims(indim))
     CALL define_var("nbeami_bdy","R",indim,idims, &
            "beam ion densities at/beyond boundary", &
            "m^-3","NBI","STATE_DATA", &
            "R|units=m^-3  nbeami_bdy(nspec_beam)")
  ENDIF

  IF(ALLOCATED(ss%nbion)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("nbion","N",indim,idims, &
            "name of species injected by each beam.", &
            "__NONE","NBI","SHOT_CONFIGURATION", &
            "N  nbion(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%nbion_trace)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("nbion_trace","N",indim,idims, &
            "(if non-blank) beam trace element", &
            "__NONE","NBI","SHOT_CONFIGURATION", &
            "N  nbion_trace(nbeam)")
  ENDIF

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("NBI_Code_Info","C*80",indim,idims, &
         "Information: code implementing NBI component", &
         "__NONE","NBI","SIMULATION_INIT", &
         "C*80   NBI_Code_Info")

  indim = 1
  status = nf90_inq_dimid(icdf,"ps_clen_80",idims(1))
  CALL define_var("NBI_Data_Info","C*80",indim,idims, &
         "information on source of beam power & voltage data", &
         "__NONE","NBI","SIMULATION_INIT", &
         "C*80   NBI_Data_Info")

  IF(ALLOCATED(ss%nbi_src_name)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("nbi_src_name","N",indim,idims, &
            "number & name of neutral beams", &
            "__NONE","NBI","MACHINE_DESCRIPTION", &
            "L|neutral_beams nbi_src_name(nbeam)")
  ENDIF

  IF(ALLOCATED(ss%nbshape)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nbeam",idims(indim))
     CALL define_var("nbshape","N",indim,idims, &
            "shape keyword for beam source grid", &
            "__NONE","NBI","MACHINE_DESCRIPTION", &
            "N     nbshape(nbeam)")
  ENDIF

  indim = 0
  CALL define_var("nebar","R",indim,idims, &
         "line-averaged electron density", &
         "#/m^2","PLASMA","STATE_DATA", &
         "R|units=#/m^2 nebar")

  IF(ALLOCATED(ss%neflux_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("neflux_ky_tglf","R",indim,idims, &
            "electron density flux spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin neflux_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  IF(ALLOCATED(ss%Newcomb_code)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nnewcomb",idims(indim))
     CALL define_var("Newcomb_code","N",indim,idims, &
            "name(s) of Newcomb code(s) used", &
            "__NONE","LMHD","SIMULATION_INIT", &
            "L|Newcomb_codes  Newcomb_code(nnewcomb)")
  ENDIF

  IF(ALLOCATED(ss%Newcomb_mapper)) THEN
     indim = 1
     status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nnewcomb",idims(indim))
     CALL define_var("Newcomb_mapper","N",indim,idims, &
            "mapper code used for each Newcomb code", &
            "__NONE","LMHD","SIMULATION_INIT", &
            "N  Newcomb_mapper(nnewcomb)")
  ENDIF

  IF(ALLOCATED(ss%Newcomb_stable)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_mmodes",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nnewcomb",idims(indim))
     CALL define_var("Newcomb_stable","I",indim,idims, &
            "0: stable; 1: unstable;", &
            "__NONE","LMHD","STATE_DATA", &
            "I   Newcomb_stable(mmodes,nnewcomb)")
  ENDIF

  IF(ALLOCATED(ss%Newcomb_status)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nnewcomb",idims(indim))
     CALL define_var("Newcomb_status","I",indim,idims, &
            "completion status: Newcomb codes", &
            "__NONE","LMHD","STATE_DATA", &
            "I   Newcomb_status(nnewcomb)")
  ENDIF

  IF(ALLOCATED(ss%ne_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("ne_ky_tglf","R",indim,idims, &
            "electron density fluctuation spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin ne_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  IF(ALLOCATED(ss%nfusi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_fus",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("nfusi","R",indim,idims, &
            "fusion ion density", &
            "m^-3","FUS","STATE_PROFILES", &
            "R|units=m^-3|alias=n|step  nfusi(~nrho_fus,nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%nfusi_bdy)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_fusion",idims(indim))
     CALL define_var("nfusi_bdy","R",indim,idims, &
            "fusion ion densities at/beyond boundary", &
            "m^-3","IC","STATE_DATA", &
            "R|units=m^-3  nfusi_bdy(nspec_fusion)")
  ENDIF

  IF(ALLOCATED(ss%ngradb2_av)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_eq_geo",idims(indim))
     CALL define_var("ngradb2_av","R",indim,idims, &
            "<(B.grad(B)/|B|)^2>", &
            "T^2*m^-2","EQ","STATE_PROFILES", &
            "R|units=T^2*m^-2|pclin   ngradb2_av(nrho_eq_geo)")
  ENDIF

  IF(ALLOCATED(ss%ni)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     CALL define_var("ni","R",indim,idims, &
            "total thermal ion density", &
            "m^-3","PLASMA","STATE_PROFILES", &
            "R|units=m^-3|step  ni(~nrho)")
  ENDIF

  IF(ALLOCATED(ss%niflux_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("niflux_ky_tglf","R",indim,idims, &
            "ion density flux spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin niflux_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  IF(ALLOCATED(ss%ni_ky_tglf)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nrho_anomth",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nky_anomth",idims(indim))
     CALL define_var("ni_ky_tglf","R",indim,idims, &
            "ion density fluctuation spectrum", &
            "GB","ANOM","STATE_PROFILES", &
            "R|units=GB|pclin ni_ky_tglf(nrho_anomth,nky_anomth)")
  ENDIF

  indim = 0
  CALL define_var("nmdifb","I",indim,idims, &
         "type of fast ion diffusivity", &
         "__NONE","ANOM","STATE_DATA", &
         "I   nmdifb = 0")

  IF(ALLOCATED(ss%nmini)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_icrf",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",idims(indim))
     CALL define_var("nmini","R",indim,idims, &
            "minority species density", &
            "m^-3","IC","STATE_PROFILES", &
            "R|units=m^-3|alias=n|step  nmini(~nrho_icrf,nspec_rfmin)")
  ENDIF

  IF(ALLOCATED(ss%nmini_bdy)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nspec_rfmin",idims(indim))
     CALL define_var("nmini_bdy","R",indim,idims, &
            "RF minority ion densities at/beyond boundary", &
            "m^-3","FUS","STATE_DATA", &
            "R|units=m^-3  nmini_bdy(nspec_rfmin)")
  ENDIF

  IF(ALLOCATED(ss%nmodel)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_anomth",idims(indim))
     CALL define_var("nmodel","R",indim,idims, &
            "target density profile", &
            "m^-3","ANOM","STATE_PROFILES", &
            "R|units=m^-3|step  nmodel(~nrho_anomth)")
  ENDIF

  indim = 0
  CALL define_var("nmodel_bdy","R",indim,idims, &
         "model density, boundary value", &
         "m^-3","ANOM","STATE_DATA", &
         "R|units=m^-3   nmodel_bdy")

  indim = 0
  CALL define_var("nmom","I",indim,idims, &
         "number of Fourier (R,Z) moments", &
         "__NONE","EQ","SIMULATION_INIT", &
         "I  nmom = 16")

  IF(ALLOCATED(ss%nnkparlh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_nspectrumlh_src",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("nnkparlh","I",indim,idims, &
            "need description", &
            "__NONE","LH","SHOT_CONFIGURATION", &
            "I nnkparlh(nspectrumlh_src(),nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%nnkpollh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_nspectrumlh_src",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("nnkpollh","I",indim,idims, &
            "need description", &
            "__NONE","LH","SHOT_CONFIGURATION", &
            "I nnkpollh(nspectrumlh_src(),nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%nnktorlh)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_nspectrumlh_src",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("nnktorlh","I",indim,idims, &
            "need description", &
            "__NONE","LH","SHOT_CONFIGURATION", &
            "I nnktorlh(nspectrumlh_src(),nlhrf_src)")
  ENDIF

  IF(ALLOCATED(ss%nphi)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_max_num_nphi",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("nphi","I",indim,idims, &
            "n_phi wave spectrum from antenna", &
            "__NONE","IC","SHOT_CONFIGURATION", &
            "I nphi(num_nphi(),nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%nrw)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho_rw",idims(indim))
     CALL define_var("nrw","R",indim,idims, &
            "runaway electron density", &
            "m^-3","RUNAWAY","STATE_PROFILES", &
            "R|units=m^-3|step   nrw(~nrho_rw)")
  ENDIF

  IF(ALLOCATED(ss%nrz_antgeo)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nicrf_src",idims(indim))
     CALL define_var("nrz_antgeo","I",indim,idims, &
            "number of (R,Z) points, antenna geometries", &
            "__NONE","IC","MACHINE_DESCRIPTION", &
            "I|ENUM nrz_antgeo(nicrf_src)")
  ENDIF

  IF(ALLOCATED(ss%ns)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dm1_nrho",idims(indim))
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dp1_nspec_th",idims(indim))
     CALL define_var("ns","R",indim,idims, &
            "thermal specie density", &
            "m^-3","PLASMA","STATE_PROFILES", &
            "R|units=m^-3|alias=n|step  ns(~nrho,0:nspec_th)")
  ENDIF

  IF(ALLOCATED(ss%nspectrumlh_src)) THEN
     indim = 0
     indim = indim + 1
     status = nf90_inq_dimid(icdf,"dim_nlhrf_src",idims(indim))
     CALL define_var("nspectrumlh_src","I",indim,idims, &
            "number of spectrum peaks", &
            "__NONE","LH","SHOT_CONFIGURATION", &
            "I|ENUM nspectrumlh_src(nlhrf_src)")
  ENDIF

  CONTAINS


     SUBROUTINE define_var(name,dtype,indim,idims, &
          long_name,units,component,section,spec)

        CHARACTER*(*), INTENT(in) :: name
        CHARACTER*(*), INTENT(in) :: dtype
        INTEGER, INTENT(in) :: indim
        INTEGER, dimension(:), INTENT(in) :: idims
        CHARACTER*(*), INTENT(in) :: long_name
        CHARACTER*(*), INTENT(in) :: units
        CHARACTER*(*), INTENT(in) :: component
        CHARACTER*(*), INTENT(in) :: section
        CHARACTER*(*), INTENT(in) :: spec

        integer :: itype

        IF(dtype.EQ.'R') THEN
           itype = NF90_DOUBLE

        ELSE IF (dtype.EQ.'I') THEN
           itype = NF90_INT

        ELSE
           itype = NF90_CHAR

        ENDIF

        IF(indim.EQ.0) THEN
           status=nf90_def_var(icdf,name,itype, varid=varid)

        ELSE
           status=nf90_def_var(icdf,name,itype, dimids=idims(1:indim),varid=varid)

        ENDIF

        IF(status.NE.NF90_NOERR) THEN
           WRITE(iout,*) " ?plasma_state_store: error defining variable: "//trim(name)
           CALL ncerr(status)
           iertot = iertot + 1
           RETURN
        ENDIF

        ! Variable OK, now add attributes:

        CALL add_attrib("long_name",long_name)
        IF(dtype.EQ.'R') CALL add_attrib("units",units)
        CALL add_attrib("component",component)
        CALL add_attrib("section",section)
        CALL add_attrib("specification",spec)

     END SUBROUTINE define_var

     SUBROUTINE add_attrib(name,value)
        CHARACTER*(*), intent(in) :: name
        CHARACTER*(*), intent(in) :: value

        status=nf90_put_att(icdf,varid,name,value)
        IF(status.NE.NF90_NOERR) THEN
           WRITE(iout,*) " ?plasma_state_store: error defining attribute: "//trim(name)//' = '//trim(value)
           CALL ncerr(status)
           iertot = iertot + 1
        ENDIF
     END SUBROUTINE add_attrib

     SUBROUTINE ncerr(status)
        INTEGER, INTENT(in) :: status

        nc_errmsg = NF90_strerror(status)
        write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
     END SUBROUTINE ncerr

END SUBROUTINE plasma_state_store_def_4


SUBROUTINE plasma_state_store_4(ss,icdf,iout,iertot)


  !  write subset of variables and arrays from NetCDF file

  USE plasma_state_definition_mod
  USE netcdf
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to read
  INTEGER, INTENT(IN) :: icdf   ! NetCDF file ID
  INTEGER, INTENT(IN) :: iout   ! message LUN
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !--------------

  INTEGER :: varid,dimid,status,ival,idims(20),indim
  CHARACTER*80 :: nc_errmsg

  !--------------
  !  write variables...


  IF(ALLOCATED(ss%nbeami)) THEN
     status = nf90_inq_varid(icdf,"nbeami",varid)
     status = nf90_put_var(icdf,varid,ss%nbeami)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nbeami"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nbeami_bdy)) THEN
     status = nf90_inq_varid(icdf,"nbeami_bdy",varid)
     status = nf90_put_var(icdf,varid,ss%nbeami_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nbeami_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nbion)) THEN
     status = nf90_inq_varid(icdf,"nbion",varid)
     status = nf90_put_var(icdf,varid,ss%nbion)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nbion"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nbion_trace)) THEN
     status = nf90_inq_varid(icdf,"nbion_trace",varid)
     status = nf90_put_var(icdf,varid,ss%nbion_trace)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nbion_trace"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"NBI_Code_Info",varid)
  status = nf90_put_var(icdf,varid,ss%NBI_Code_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: NBI_Code_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"NBI_Data_Info",varid)
  status = nf90_put_var(icdf,varid,ss%NBI_Data_Info)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: NBI_Data_Info"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%nbi_src_name)) THEN
     status = nf90_inq_varid(icdf,"nbi_src_name",varid)
     status = nf90_put_var(icdf,varid,ss%nbi_src_name)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nbi_src_name"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nbshape)) THEN
     status = nf90_inq_varid(icdf,"nbshape",varid)
     status = nf90_put_var(icdf,varid,ss%nbshape)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nbshape"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nebar",varid)
  status = nf90_put_var(icdf,varid,ss%nebar)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: nebar"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%neflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"neflux_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%neflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: neflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Newcomb_code)) THEN
     status = nf90_inq_varid(icdf,"Newcomb_code",varid)
     status = nf90_put_var(icdf,varid,ss%Newcomb_code)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Newcomb_code"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Newcomb_mapper)) THEN
     status = nf90_inq_varid(icdf,"Newcomb_mapper",varid)
     status = nf90_put_var(icdf,varid,ss%Newcomb_mapper)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Newcomb_mapper"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Newcomb_stable)) THEN
     status = nf90_inq_varid(icdf,"Newcomb_stable",varid)
     status = nf90_put_var(icdf,varid,ss%Newcomb_stable)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Newcomb_stable"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%Newcomb_status)) THEN
     status = nf90_inq_varid(icdf,"Newcomb_status",varid)
     status = nf90_put_var(icdf,varid,ss%Newcomb_status)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: Newcomb_status"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ne_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"ne_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%ne_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ne_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nfusi)) THEN
     status = nf90_inq_varid(icdf,"nfusi",varid)
     status = nf90_put_var(icdf,varid,ss%nfusi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nfusi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nfusi_bdy)) THEN
     status = nf90_inq_varid(icdf,"nfusi_bdy",varid)
     status = nf90_put_var(icdf,varid,ss%nfusi_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nfusi_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ngradb2_av)) THEN
     status = nf90_inq_varid(icdf,"ngradb2_av",varid)
     status = nf90_put_var(icdf,varid,ss%ngradb2_av)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ngradb2_av"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ni)) THEN
     status = nf90_inq_varid(icdf,"ni",varid)
     status = nf90_put_var(icdf,varid,ss%ni)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ni"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%niflux_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"niflux_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%niflux_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: niflux_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ni_ky_tglf)) THEN
     status = nf90_inq_varid(icdf,"ni_ky_tglf",varid)
     status = nf90_put_var(icdf,varid,ss%ni_ky_tglf)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ni_ky_tglf"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nmdifb",varid)
  status = nf90_put_var(icdf,varid,ss%nmdifb)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: nmdifb"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%nmini)) THEN
     status = nf90_inq_varid(icdf,"nmini",varid)
     status = nf90_put_var(icdf,varid,ss%nmini)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nmini"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nmini_bdy)) THEN
     status = nf90_inq_varid(icdf,"nmini_bdy",varid)
     status = nf90_put_var(icdf,varid,ss%nmini_bdy)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nmini_bdy"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nmodel)) THEN
     status = nf90_inq_varid(icdf,"nmodel",varid)
     status = nf90_put_var(icdf,varid,ss%nmodel)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nmodel"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  status = nf90_inq_varid(icdf,"nmodel_bdy",varid)
  status = nf90_put_var(icdf,varid,ss%nmodel_bdy)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: nmodel_bdy"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  status = nf90_inq_varid(icdf,"nmom",varid)
  status = nf90_put_var(icdf,varid,ss%nmom)
  IF(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) " ?plasma_state_store: error storing: nmom"
     CALL ncerr(status)
     iertot = iertot + 1
  ENDIF

  IF(ALLOCATED(ss%nnkparlh)) THEN
     status = nf90_inq_varid(icdf,"nnkparlh",varid)
     status = nf90_put_var(icdf,varid,ss%nnkparlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nnkparlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nnkpollh)) THEN
     status = nf90_inq_varid(icdf,"nnkpollh",varid)
     status = nf90_put_var(icdf,varid,ss%nnkpollh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nnkpollh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nnktorlh)) THEN
     status = nf90_inq_varid(icdf,"nnktorlh",varid)
     status = nf90_put_var(icdf,varid,ss%nnktorlh)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nnktorlh"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nphi)) THEN
     status = nf90_inq_varid(icdf,"nphi",varid)
     status = nf90_put_var(icdf,varid,ss%nphi)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nphi"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nrw)) THEN
     status = nf90_inq_varid(icdf,"nrw",varid)
     status = nf90_put_var(icdf,varid,ss%nrw)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nrw"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nrz_antgeo)) THEN
     status = nf90_inq_varid(icdf,"nrz_antgeo",varid)
     status = nf90_put_var(icdf,varid,ss%nrz_antgeo)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nrz_antgeo"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%ns)) THEN
     status = nf90_inq_varid(icdf,"ns",varid)
     status = nf90_put_var(icdf,varid,ss%ns)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: ns"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  IF(ALLOCATED(ss%nspectrumlh_src)) THEN
     status = nf90_inq_varid(icdf,"nspectrumlh_src",varid)
     status = nf90_put_var(icdf,varid,ss%nspectrumlh_src)
     IF(status.NE.NF90_NOERR) THEN
        WRITE(iout,*) " ?plasma_state_store: error storing: nspectrumlh_src"
        CALL ncerr(status)
        iertot = iertot + 1
     ENDIF
  ENDIF

  CONTAINS


     SUBROUTINE ncerr(status)
        INTEGER, INTENT(in) :: status

        nc_errmsg = NF90_strerror(status)
        write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
     END SUBROUTINE ncerr

END SUBROUTINE plasma_state_store_4
